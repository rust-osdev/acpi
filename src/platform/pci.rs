use crate::{
    AcpiError,
    AcpiHandler,
    AcpiTables,
    sdt::{
        Signature,
        mcfg::{Mcfg, McfgEntry},
    },
};
use alloc::{
    alloc::{Allocator, Global},
    vec::Vec,
};

/// Describes a set of regions of physical memory used to access the PCIe configuration space. A
/// region is created for each entry in the MCFG. Given the segment group, bus, device number, and
/// function of a PCIe device, [`PciConfigRegions::physical_address`] will give you the physical
/// address of the start of that device function's configuration space (each function has 4096
/// bytes of configuration space in PCIe).
pub struct PciConfigRegions<A: Allocator> {
    pub regions: Vec<McfgEntry, A>,
}

impl PciConfigRegions<Global> {
    pub fn new<H>(tables: &AcpiTables<H>) -> Result<PciConfigRegions<Global>, AcpiError>
    where
        H: AcpiHandler,
    {
        Self::new_in(tables, Global)
    }
}

impl<A: Allocator> PciConfigRegions<A> {
    pub fn new_in<H>(tables: &AcpiTables<H>, allocator: A) -> Result<PciConfigRegions<A>, AcpiError>
    where
        H: AcpiHandler,
    {
        let Some(mcfg) = tables.find_table::<Mcfg>() else { Err(AcpiError::TableNotFound(Signature::MCFG))? };
        let regions = mcfg.entries().to_vec_in(allocator);

        Ok(Self { regions })
    }

    /// Get the **physical** address of the start of the configuration space for a given PCIe device
    /// function. Returns `None` if there isn't an entry in the MCFG that manages that device.
    pub fn physical_address(&self, segment_group_no: u16, bus: u8, device: u8, function: u8) -> Option<u64> {
        /*
         * First, find the memory region that handles this segment and bus. This method is fine
         * because there should only be one region that handles each segment group + bus
         * combination.
         */
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
