use rsdp::handler::PhysicalMapping;

use crate::{sdt::SdtHeader, AcpiError, AcpiHandler, AcpiTable, AcpiTables};
use core::{mem, slice};

/// Describes a set of regions of physical memory used to access the PCIe configuration space. An
/// entry is created for each entry in the MCFG.
pub struct PciConfig<H: AcpiHandler>(PhysicalMapping<H, Mcfg>);

impl<H> PciConfig<H>
where
    H: AcpiHandler,
{
    /// Creates a new [`PciConfig`] structure, encapsulating the relevant information about the system's
    /// PCIe configuration space.
    pub fn new(tables: &AcpiTables<H>) -> Result<Self, AcpiError> {
        Ok(Self(unsafe {
            tables
                .get_sdt::<Mcfg>(crate::sdt::Signature::MCFG)?
                .ok_or(AcpiError::TableMissing(crate::sdt::Signature::MCFG))?
        }))
    }

    /// Get the physical address of the start of the configuration space for a given PCIe device
    /// function. Returns `None` if there isn't an entry in the MCFG that manages that device.
    pub fn physical_address(&self, segment_group_no: u16, bus: u8, device: u8, function: u8) -> Option<u64> {
        // First, find the memory region that handles this segment and bus. This method is fine
        // because there should only be one region that handles each segment group + bus
        // combination.
        let region = self.0.entries().iter().find(|region| {
            region.pci_segment_group == segment_group_no
                && (region.bus_number_start..=region.bus_number_end).contains(&bus)
        })?;

        Some(
            region.base_address
                + ((u64::from(bus - region.bus_number_start) << 20)
                    | (u64::from(device) << 15)
                    | (u64::from(function) << 12)),
        )
    }

    /// Returns an iterator providing information about the system's present PCI busses.
    /// This is roughly equivalent to manually iterating the system's MCFG table.
    pub fn iter(&self) -> PciConfigEntryIterator {
        PciConfigEntryIterator { entries: self.0.entries(), index: 0 }
    }
}

/// Configuration entry describing a valid bus range for the given PCI segment group.
pub struct PciConfigEntry {
    pub segment_group: u16,
    pub bus_range: core::ops::RangeInclusive<u8>,
    pub physical_address: usize,
}

/// Iterator providing a [`PciConfigEntry`] for all of the valid bus ranges on the system.
pub struct PciConfigEntryIterator<'a> {
    entries: &'a [McfgEntry],
    index: usize,
}

impl Iterator for PciConfigEntryIterator<'_> {
    type Item = PciConfigEntry;

    fn next(&mut self) -> Option<Self::Item> {
        let entry = self.entries.get(self.index)?;
        self.index += 1;

        Some(PciConfigEntry {
            segment_group: entry.pci_segment_group,
            bus_range: entry.bus_number_start..=entry.bus_number_end,
            physical_address: entry.base_address as usize,
        })
    }
}

#[repr(C, packed)]
pub struct Mcfg {
    header: SdtHeader,
    _reserved: u64,
    // Followed by `n` entries with format `McfgEntry`
}

impl AcpiTable for Mcfg {
    const SIGNATURE: crate::Signature = crate::Signature::MCFG;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Mcfg {
    /// Creates a bare slice over the MCFG entry table.
    fn entries(&self) -> &[McfgEntry] {
        let length = self.header.length as usize - mem::size_of::<Mcfg>();

        // Intentionally round down in case length isn't an exact multiple of McfgEntry size
        // (see rust-osdev/acpi#58)
        let num_entries = length / mem::size_of::<McfgEntry>();

        unsafe {
            let pointer = (self as *const Mcfg as *const u8).add(mem::size_of::<Mcfg>()) as *const McfgEntry;
            slice::from_raw_parts(pointer, num_entries)
        }
    }
}

/// Entry type for the MCFG ACPI table.
#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
struct McfgEntry {
    base_address: u64,
    pci_segment_group: u16,
    bus_number_start: u8,
    bus_number_end: u8,
    _reserved: u32,
}
