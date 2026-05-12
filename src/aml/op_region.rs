use crate::aml::{AmlError, namespace::AmlName};
use core::alloc::Allocator;

// PILOT-DECISION: OpRegion was holding `AmlName` (Global). Now generic over A
// so OpRegion<A> embeds AmlName<A>. RegionHandler keeps its non-generic
// trait shape; each method has its own `<A>` parameter for the error type,
// letting concrete handler impls live in callers' allocator universe.
#[derive(Clone)]
pub struct OpRegion<A: core::alloc::Allocator + Clone> {
    pub space: RegionSpace,
    pub base: u64,
    pub length: u64,
    pub parent_device_path: AmlName<A>,
}

impl<A: core::alloc::Allocator + Clone> core::fmt::Debug for OpRegion<A> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("OpRegion")
            .field("space", &self.space)
            .field("base", &self.base)
            .field("length", &self.length)
            .field("parent_device_path", &self.parent_device_path)
            .finish()
    }
}

// PILOT-DECISION: trait parameterized by A so it's object-safe — generic
// methods can't appear on a `Box<dyn Trait>`. Concrete handler impls now
// pin themselves to a specific A (typically the same A as the interpreter).
pub trait RegionHandler<A: Allocator + Clone> {
    fn read_u8(&self, region: &OpRegion<A>) -> Result<u8, AmlError<A>>;
    fn read_u16(&self, region: &OpRegion<A>) -> Result<u16, AmlError<A>>;
    fn read_u32(&self, region: &OpRegion<A>) -> Result<u32, AmlError<A>>;
    fn read_u64(&self, region: &OpRegion<A>) -> Result<u64, AmlError<A>>;

    fn write_u8(&self, region: &OpRegion<A>, value: u8) -> Result<(), AmlError<A>>;
    fn write_u16(&self, region: &OpRegion<A>, value: u16) -> Result<(), AmlError<A>>;
    fn write_u32(&self, region: &OpRegion<A>, value: u32) -> Result<(), AmlError<A>>;
    fn write_u64(&self, region: &OpRegion<A>, value: u64) -> Result<(), AmlError<A>>;
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
