//! ACPI defines a Generic Address Structure (GAS), which provides a versatile way to describe register locations
//! in a wide range of address spaces.

use crate::{AcpiError, Handler, PhysicalMapping};
use log::warn;

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
    /// Describes a register in the configuration space of a PCI device in segment `0`, on bus `0`.
    /// The `address` field is of the format:
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

pub struct MappedGas<H: Handler> {
    gas: GenericAddress,
    handler: H,
    mapping: Option<PhysicalMapping<H, u8>>,
}

impl<H> MappedGas<H>
where
    H: Handler,
{
    /// Map the given `GenericAddress`, giving a `MappedGas` that can be read from and written to.
    ///
    /// ### Safety
    /// The supplied `GenericAddress` must be a valid GAS and all subsequent reads and writes must
    /// be valid.
    pub unsafe fn map_gas(gas: GenericAddress, handler: &H) -> Result<MappedGas<H>, AcpiError> {
        match gas.address_space {
            AddressSpace::SystemMemory => {
                // TODO: how to know total size needed?
                let mapping = unsafe { handler.map_physical_region(gas.address as usize, 0x1000) };
                Ok(MappedGas { gas, handler: handler.clone(), mapping: Some(mapping) })
            }
            AddressSpace::SystemIo => Ok(MappedGas { gas, handler: handler.clone(), mapping: None }),
            other => {
                warn!("Tried to map GAS of unsupported type {:?}", other);
                Err(AcpiError::LibUnimplemented)
            }
        }
    }

    pub fn read(&self) -> Result<u64, AcpiError> {
        /*
         * TODO: this is only correct for basic GASs that require a single access. Extend it to
         * support bit offsets and multiple reads etc.
         */
        let access_size_bits = gas_decode_access_bit_width(self.gas)?;
        match self.gas.address_space {
            AddressSpace::SystemMemory => {
                let mapping = self.mapping.as_ref().unwrap();
                let value = match access_size_bits {
                    8 => unsafe { core::ptr::read_volatile(mapping.virtual_start.as_ptr() as *const u8) as u64 },
                    16 => unsafe { core::ptr::read_volatile(mapping.virtual_start.as_ptr() as *const u16) as u64 },
                    32 => unsafe { core::ptr::read_volatile(mapping.virtual_start.as_ptr() as *const u32) as u64 },
                    64 => unsafe { core::ptr::read_volatile(mapping.virtual_start.as_ptr() as *const u64) },
                    _ => panic!(),
                };
                Ok(value)
            }
            AddressSpace::SystemIo => {
                let value = match access_size_bits {
                    8 => self.handler.read_io_u8(self.gas.address as u16) as u64,
                    16 => self.handler.read_io_u16(self.gas.address as u16) as u64,
                    32 => self.handler.read_io_u32(self.gas.address as u16) as u64,
                    _ => panic!(),
                };
                Ok(value)
            }
            _ => unimplemented!(),
        }
    }

    pub fn write(&self, value: u64) -> Result<(), AcpiError> {
        // TODO: see above
        let access_size_bits = gas_decode_access_bit_width(self.gas)?;
        match self.gas.address_space {
            AddressSpace::SystemMemory => {
                let mapping = self.mapping.as_ref().unwrap();
                match access_size_bits {
                    8 => unsafe {
                        core::ptr::write_volatile(mapping.virtual_start.as_ptr(), value as u8);
                    },
                    16 => unsafe {
                        core::ptr::write_volatile(mapping.virtual_start.as_ptr() as *mut u16, value as u16);
                    },
                    32 => unsafe {
                        core::ptr::write_volatile(mapping.virtual_start.as_ptr() as *mut u32, value as u32);
                    },
                    64 => unsafe { core::ptr::write_volatile(mapping.virtual_start.as_ptr() as *mut u64, value) },
                    _ => panic!(),
                }
                Ok(())
            }
            AddressSpace::SystemIo => {
                match access_size_bits {
                    8 => self.handler.write_io_u8(self.gas.address as u16, value as u8),
                    16 => self.handler.write_io_u16(self.gas.address as u16, value as u16),
                    32 => self.handler.write_io_u32(self.gas.address as u16, value as u32),
                    _ => panic!(),
                }
                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}

/// Returns the access size that should be made for a given `GenericAddress`, in bits.
fn gas_decode_access_bit_width(gas: GenericAddress) -> Result<u8, AcpiError> {
    /*
     * This is more complex than it should be - we follow ACPICA to try and work with quirky
     * firmwares.
     *
     * We should actually ignore the access sizes for normal registers (they tend to be unspecified
     * in my experience anyway) and base our accesses on the width of the register. Only if a
     * register has a bit width that cannot be accessed as a single native access do we look at the
     * access size.
     *
     * We use a third method, based on the alignment of the address, for registers that have
     * non-zero bit offsets. These are not typically encountered in normal registers - they very
     * often mean the GAS has come from APEI (ACPI Platform Error Interface), and so needs speical
     * handling.
     */
    if gas.bit_offset == 0 && [8, 16, 32, 64].contains(&gas.bit_width) {
        Ok(gas.bit_width)
    } else if gas.access_size != 0 {
        match gas.access_size {
            1 => Ok(8),
            2 => Ok(16),
            3 => Ok(32),
            4 => Ok(64),
            _ => Err(AcpiError::InvalidGenericAddress),
        }
    } else {
        // TODO: work out access size based on alignment of the address
        todo!()
    }
}
