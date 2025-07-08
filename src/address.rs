//! ACPI defines a Generic Address Structure (GAS), which provides a versatile way to describe register locations
//! in a wide range of address spaces.

use crate::AcpiError;

/// This is the raw form of a Generic Address Structure, and follows the layout found in the ACPI tables.
#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct RawGenericAddress {
    pub address_space: u8,
    pub bit_width: u8,
    pub bit_offset: u8,
    pub access_size: u8,
    pub address: u64,
}

impl RawGenericAddress {
    pub(crate) const fn is_empty(&self) -> bool {
        self.address_space == 0
            && self.bit_width == 0
            && self.bit_offset == 0
            && self.access_size == 0
            && self.address == 0
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum AddressSpace {
    SystemMemory,
    SystemIo,
    /// Describes a register in the configuration space of a PCI device in segment `0`, on bus `0`. The `address`
    /// field is of the format:
    /// ```ignore
    /// 64              48              32              16               0
    ///  +---------------+---------------+---------------+---------------+
    ///  |  reserved (0) |    device     |   function    |    offset     |
    ///  +---------------+---------------+---------------+---------------+
    /// ```
    PciConfigSpace,
    EmbeddedController,
    SMBus,
    SystemCmos,
    PciBarTarget,
    Ipmi,
    GeneralIo,
    GenericSerialBus,
    PlatformCommunicationsChannel,
    FunctionalFixedHardware,
    OemDefined(u8),
}

/// Specifies a standard access size. The access size of a GAS can be non-standard, and is defined
/// by the Address Space ID in such cases.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum StandardAccessSize {
    Undefined,
    ByteAccess,
    WordAccess,
    DWordAccess,
    QWordAccess,
}

impl TryFrom<u8> for StandardAccessSize {
    type Error = AcpiError;

    fn try_from(size: u8) -> Result<Self, Self::Error> {
        match size {
            0 => Ok(StandardAccessSize::Undefined),
            1 => Ok(StandardAccessSize::ByteAccess),
            2 => Ok(StandardAccessSize::WordAccess),
            3 => Ok(StandardAccessSize::DWordAccess),
            4 => Ok(StandardAccessSize::QWordAccess),
            _ => Err(AcpiError::InvalidGenericAddress),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct GenericAddress {
    pub address_space: AddressSpace,
    pub bit_width: u8,
    pub bit_offset: u8,
    pub access_size: u8,
    pub address: u64,
}

impl GenericAddress {
    pub fn from_raw(raw: RawGenericAddress) -> Result<GenericAddress, AcpiError> {
        let address_space = match raw.address_space {
            0x00 => AddressSpace::SystemMemory,
            0x01 => AddressSpace::SystemIo,
            0x02 => AddressSpace::PciConfigSpace,
            0x03 => AddressSpace::EmbeddedController,
            0x04 => AddressSpace::SMBus,
            0x05 => AddressSpace::SystemCmos,
            0x06 => AddressSpace::PciBarTarget,
            0x07 => AddressSpace::Ipmi,
            0x08 => AddressSpace::GeneralIo,
            0x09 => AddressSpace::GenericSerialBus,
            0x0a => AddressSpace::PlatformCommunicationsChannel,
            0x0b..=0x7e => return Err(AcpiError::InvalidGenericAddress),
            0x7f => AddressSpace::FunctionalFixedHardware,
            0x80..=0xbf => return Err(AcpiError::InvalidGenericAddress),
            0xc0..=0xff => AddressSpace::OemDefined(raw.address_space),
        };

        Ok(GenericAddress {
            address_space,
            bit_width: raw.bit_width,
            bit_offset: raw.bit_offset,
            access_size: raw.access_size,
            address: raw.address,
        })
    }

    pub fn standard_access_size(&self) -> Result<StandardAccessSize, AcpiError> {
        StandardAccessSize::try_from(self.access_size)
    }
}
