use crate::aml::{AmlError, namespace::AmlName};
use pci_types::PciAddress;
use spinning_top::Spinlock;

#[derive(Debug)]
pub struct OpRegion {
    pub space: RegionSpace,
    pub base: u64,
    pub length: u64,
    pub parent_device_path: AmlName,
    /// The address of the device a `PCI_Config` region is in, resolved on first access.
    pub(crate) pci_address: Spinlock<Option<PciAddress>>,
}

impl OpRegion {
    pub(crate) fn new(space: RegionSpace, base: u64, length: u64, parent_device_path: AmlName) -> OpRegion {
        OpRegion { space, base, length, parent_device_path, pci_address: Spinlock::new(None) }
    }
}

impl Clone for OpRegion {
    fn clone(&self) -> Self {
        OpRegion {
            space: self.space,
            base: self.base,
            length: self.length,
            parent_device_path: self.parent_device_path.clone(),
            pci_address: Spinlock::new(*self.pci_address.lock()),
        }
    }
}

pub trait RegionHandler {
    fn read_u8(&self, region: &OpRegion) -> Result<u8, AmlError>;
    fn read_u16(&self, region: &OpRegion) -> Result<u16, AmlError>;
    fn read_u32(&self, region: &OpRegion) -> Result<u32, AmlError>;
    fn read_u64(&self, region: &OpRegion) -> Result<u64, AmlError>;

    fn write_u8(&self, region: &OpRegion, value: u8) -> Result<(), AmlError>;
    fn write_u16(&self, region: &OpRegion, value: u16) -> Result<(), AmlError>;
    fn write_u32(&self, region: &OpRegion, value: u32) -> Result<(), AmlError>;
    fn write_u64(&self, region: &OpRegion, value: u64) -> Result<(), AmlError>;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RegionSpace {
    SystemMemory,
    SystemIO,
    PciConfig,
    EmbeddedControl,
    SmBus,
    SystemCmos,
    PciBarTarget,
    Ipmi,
    GeneralPurposeIo,
    GenericSerialBus,
    Pcc,
    Oem(u8),
}

impl From<u8> for RegionSpace {
    fn from(value: u8) -> Self {
        match value {
            0 => RegionSpace::SystemMemory,
            1 => RegionSpace::SystemIO,
            2 => RegionSpace::PciConfig,
            3 => RegionSpace::EmbeddedControl,
            4 => RegionSpace::SmBus,
            5 => RegionSpace::SystemCmos,
            6 => RegionSpace::PciBarTarget,
            7 => RegionSpace::Ipmi,
            8 => RegionSpace::GeneralPurposeIo,
            9 => RegionSpace::GenericSerialBus,
            10 => RegionSpace::Pcc,
            _ => RegionSpace::Oem(value),
        }
    }
}
