use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};
use core::{mem, slice};

/// Describes a set of regions of physical memory used to access the PCIe configuration space. A
/// region is created for each entry in the MCFG. Given the segment group, bus, device number, and
/// function of a PCIe device, the `physical_address` method on this will give you the physical
/// address of the start of that device function's configuration space (each function has 4096
/// bytes of configuration space in PCIe).
#[cfg(feature = "allocator_api")]
pub struct PciConfigRegions<'a, A>
where
    A: core::alloc::Allocator,
{
    regions: crate::ManagedSlice<'a, McfgEntry, A>,
}

#[cfg(feature = "alloc")]
impl<'a> PciConfigRegions<'a, alloc::alloc::Global> {
    pub fn new<H>(tables: &crate::AcpiTables<H>) -> crate::AcpiResult<PciConfigRegions<'a, alloc::alloc::Global>>
    where
        H: crate::AcpiHandler,
    {
        Self::new_in(tables, alloc::alloc::Global)
    }
}

#[cfg(feature = "allocator_api")]
impl<'a, A> PciConfigRegions<'a, A>
where
    A: core::alloc::Allocator,
{
    pub fn new_in<H>(tables: &crate::AcpiTables<H>, allocator: A) -> crate::AcpiResult<PciConfigRegions<'a, A>>
    where
        H: crate::AcpiHandler,
    {
        let mcfg = tables.find_table::<Mcfg>()?;
        let mcfg_entries = mcfg.entries();

        let mut regions = crate::ManagedSlice::new_in(mcfg_entries.len(), allocator)?;
        regions.copy_from_slice(mcfg_entries);

        Ok(Self { regions })
    }

    /// Get the physical address of the start of the configuration space for a given PCIe device
    /// function. Returns `None` if there isn't an entry in the MCFG that manages that device.
    pub fn physical_address(&self, segment_group_no: u16, bus: u8, device: u8, function: u8) -> Option<u64> {
        // First, find the memory region that handles this segment and bus. This method is fine
        // because there should only be one region that handles each segment group + bus
        // combination.
        let region = self.regions.iter().find(|region| {
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
        PciConfigEntryIterator { entries: &self.regions, index: 0 }
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

/// ### Safety: Implementation properly represents a valid MCFG.
unsafe impl AcpiTable for Mcfg {
    const SIGNATURE: Signature = Signature::MCFG;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Mcfg {
    /// Returns a slice containing each of the entries in the MCFG table. Where possible, `PlatformInfo.interrupt_model` should
    /// be enumerated instead.
    pub fn entries(&self) -> &[McfgEntry] {
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

impl core::fmt::Debug for Mcfg {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        formatter.debug_struct("Mcfg").field("header", &self.header).field("entries", &self.entries()).finish()
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct McfgEntry {
    pub base_address: u64,
    pub pci_segment_group: u16,
    pub bus_number_start: u8,
    pub bus_number_end: u8,
    _reserved: u32,
}
