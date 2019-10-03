use crate::{handler::PhysicalMapping, sdt::SdtHeader, Acpi, AcpiError};
use alloc::vec::Vec;
use core::{mem, slice};

/// Describes a set of regions of physical memory used to access the PCI-E configuration space. A
/// region  is created for each entry in the MCFG. Given the segment group, bus, device number, and
/// function of a PCI-E device, the `physical_address` method on this will give you the physical
/// address of the start of that device function's configuration space (each function has 4096
/// bytes of configuration space in PCI-E).
#[derive(Debug)]
pub struct PciConfigRegions {
    regions: Vec<McfgEntry>,
}

impl PciConfigRegions {
    /// Get the physical address of the start of the configuration space for a given PCI-E device
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
}

#[repr(C, packed)]
pub(crate) struct Mcfg {
    header: SdtHeader,
    _reserved: u64,
    // Followed by `n` entries with format `McfgEntry`
}

impl Mcfg {
    fn entries(&self) -> &[McfgEntry] {
        let length = self.header.length() as usize - mem::size_of::<Mcfg>();

        // intentionally round down in case length isn't an exact multiple of McfgEntry size
        let num_entries = length / mem::size_of::<McfgEntry>();

        unsafe {
            let pointer = (self as *const Mcfg as *const u8).offset(mem::size_of::<Mcfg>() as isize)
                as *const McfgEntry;
            slice::from_raw_parts(pointer, num_entries)
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
struct McfgEntry {
    base_address: u64,
    pci_segment_group: u16,
    bus_number_start: u8,
    bus_number_end: u8,
    _reserved: u32,
}

pub(crate) fn parse_mcfg(acpi: &mut Acpi, mapping: &PhysicalMapping<Mcfg>) -> Result<(), AcpiError> {
    (*mapping).header.validate(b"MCFG")?;

    acpi.pci_config_regions =
        Some(PciConfigRegions { regions: mapping.entries().iter().map(|&entry| entry).collect() });
    Ok(())
}
