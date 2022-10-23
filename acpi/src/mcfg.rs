use crate::sdt::SdtHeader;
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

#[cfg(feature = "allocator_api")]
impl<'a, A> PciConfigRegions<'a, A>
where
    A: core::alloc::Allocator,
{
    pub fn new_in<H>(tables: &crate::AcpiTables<H>, allocator: &'a A) -> crate::AcpiResult<PciConfigRegions<'a, A>>
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
}

#[repr(C, packed)]
pub struct Mcfg {
    header: SdtHeader,
    _reserved: u64,
    // Followed by `n` entries with format `McfgEntry`
}

impl crate::AcpiTable for Mcfg {
    const SIGNATURE: crate::sdt::Signature = crate::sdt::Signature::MCFG;

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

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct McfgEntry {
    base_address: u64,
    pci_segment_group: u16,
    bus_number_start: u8,
    bus_number_end: u8,
    _reserved: u32,
}
