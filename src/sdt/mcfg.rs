use crate::{
    AcpiTable,
    sdt::{SdtHeader, Signature},
};
use core::{fmt, mem, slice};

#[repr(C, packed)]
pub struct Mcfg {
    pub header: SdtHeader,
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

        // Intentionally round down in case length isn't an exact multiple of McfgEntry size - this
        // has been observed on real hardware (see rust-osdev/acpi#58)
        let num_entries = length / mem::size_of::<McfgEntry>();

        unsafe {
            let pointer = (self as *const Mcfg as *const u8).add(mem::size_of::<Mcfg>()) as *const McfgEntry;
            slice::from_raw_parts(pointer, num_entries)
        }
    }
}

impl fmt::Debug for Mcfg {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
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
