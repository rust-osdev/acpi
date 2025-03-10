use crate::{AmlError, namespace::AmlName};

#[derive(Debug)]
pub struct OpRegion {
    pub space: RegionSpace,
    pub base: u64,
    pub length: u64,
    pub parent_device_path: AmlName,
}

pub trait RegionHandler {
    fn read(&self, region: &OpRegion) -> Result<(), AmlError>;
    fn write(&self, region: &OpRegion) -> Result<(), AmlError>;
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
