use crate::{sdt::SdtHeader, AcpiError, AcpiHandler, AcpiTable, AcpiTables};
use core::{mem, slice};

/// Describes a set of regions of physical memory used to access the PCIe configuration space. An
/// entry is created for each entry in the MCFG.
#[derive(Clone, Debug)]
pub struct PciConfigEntries<'a> {
    entries: &'a [McfgEntry],
}

impl<'a> PciConfigEntries<'a> {
    /// Creates a new `PciConfigEntries` structure, encapsulating the relevant information about the system's PCI configuration space.
    pub fn new<H: AcpiHandler>(tables: &'a AcpiTables<H>) -> Result<PciConfigEntries, AcpiError> {
        let mcfg = unsafe {
            tables
                .get_sdt::<Mcfg>(crate::sdt::Signature::MCFG)?
                .ok_or(AcpiError::TableMissing(crate::sdt::Signature::MCFG))?
        };

        Ok({
            let entries = mcfg.entries();
            // SAFETY: We're simply reconstructing an existing slice to elide the local lifetime (for the higher-context
            //         lifetime of the `AcpiTables<H>`, which this type is bound to via `'a`).
            PciConfigEntries { entries: unsafe { core::slice::from_raw_parts(entries.as_ptr(), entries.len()) } }
        })
    }

    /// Returns an iterator providing information about the system's present PCI busses.
    pub const fn iter(&self) -> PciConfigEntryIterator {
        PciConfigEntryIterator { entries: self.entries, index: 0 }
    }
}

/// Configuration entry describing a valid bus range for the given PCI segment group.
pub struct PciConfigEntry {
    pub segment_group: u16,
    pub bus_range: core::ops::RangeInclusive<u8>,
    pub physical_address: usize,
}

/// Iterator providing a `PciConfigEntry` for all of the valid bus ranges on the system.
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
