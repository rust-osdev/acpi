use crate::{
    value::{Args, FieldAccessType, FieldFlags, FieldUpdateRule},
    AmlContext,
    AmlError,
    AmlName,
    AmlValue,
};
use bit_field::BitField;

#[derive(Clone, Debug)]
pub struct OpRegion {
    region: RegionSpace,
    base: u64,
    length: u64,
    parent_device: Option<AmlName>,
}

impl OpRegion {
    pub fn new(region: RegionSpace, base: u64, length: u64, parent_device: Option<AmlName>) -> OpRegion {
        OpRegion { region, base, length, parent_device }
    }

    /// Get the length of this op-region, in **bits**.
    pub fn length(&self) -> u64 {
        self.length
    }

    /// Read a field from this op-region. This has looser requirements than `read`, and will
    /// perform multiple standard-sized reads and mask the result as required.
    pub fn read_field(
        &self,
        offset: u64,
        length: u64,
        flags: FieldFlags,
        context: &mut AmlContext,
    ) -> Result<AmlValue, AmlError> {
        let _max_access_size = match self.region {
            RegionSpace::SystemMemory => 64,
            RegionSpace::SystemIo | RegionSpace::PciConfig => 32,
            _ => unimplemented!(),
        };
        let minimum_access_size = match flags.access_type()? {
            FieldAccessType::Any => 8,
            FieldAccessType::Byte => 8,
            FieldAccessType::Word => 16,
            FieldAccessType::DWord => 32,
            FieldAccessType::QWord => 64,
            FieldAccessType::Buffer => 8, // TODO
        };

        /*
         * Find the access size, as either the minimum access size allowed by the region, or the field length
         * rounded up to the next power-of-2, whichever is larger.
         */
        let access_size = u64::max(minimum_access_size, length.next_power_of_two());

        /*
         * TODO: we need to decide properly how to read from the region itself. Complications:
         *    - if the region has a minimum access size greater than the desired length, we need to read the
         *      minimum and mask it (reading a byte from a WordAcc region)
         *    - if the desired length is larger than we can read, we need to do multiple reads
         */
        let value = self.read(offset, access_size, context)?.get_bits(0..(length as usize));
        Ok(AmlValue::Integer(value))
    }

    pub fn write_field(
        &self,
        offset: u64,
        length: u64,
        flags: FieldFlags,
        value: AmlValue,
        context: &mut AmlContext,
    ) -> Result<(), AmlError> {
        /*
         * If the field's update rule is `Preserve`, we need to read the initial value of the field, so we can
         * overwrite the correct bits. We destructure the field to do the actual write, so we read from it if
         * needed here, otherwise the borrow-checker doesn't understand.
         */
        let mut field_value = match flags.field_update_rule()? {
            FieldUpdateRule::Preserve => self.read_field(offset, length, flags, context)?.as_integer(context)?,
            FieldUpdateRule::WriteAsOnes => 0xffffffff_ffffffff,
            FieldUpdateRule::WriteAsZeros => 0x0,
        };

        let _maximum_access_size = match self.region {
            RegionSpace::SystemMemory => 64,
            RegionSpace::SystemIo | RegionSpace::PciConfig => 32,
            _ => unimplemented!(),
        };
        let minimum_access_size = match flags.access_type()? {
            FieldAccessType::Any => 8,
            FieldAccessType::Byte => 8,
            FieldAccessType::Word => 16,
            FieldAccessType::DWord => 32,
            FieldAccessType::QWord => 64,
            FieldAccessType::Buffer => 8, // TODO
        };

        /*
         * Find the access size, as either the minimum access size allowed by the region, or the field length
         * rounded up to the next power-of-2, whichever is larger.
         */
        let access_size = u64::max(minimum_access_size, length.next_power_of_two());

        field_value.set_bits(0..(length as usize), value.as_integer(context)?);
        self.write(offset, access_size, field_value, context)
    }

    /// Perform a standard-size read from this op-region. `length` must be a supported power-of-2,
    /// and `offset` correctly aligned for that `length`. `value` must be appropriately sized.
    pub fn read(&self, offset: u64, length: u64, context: &mut AmlContext) -> Result<u64, AmlError> {
        match self.region {
            RegionSpace::SystemMemory => {
                let address = (self.base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(context.handler.read_u8(address) as u64),
                    16 => Ok(context.handler.read_u16(address) as u64),
                    32 => Ok(context.handler.read_u32(address) as u64),
                    64 => Ok(context.handler.read_u64(address)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::SystemIo => {
                let port = (self.base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(context.handler.read_io_u8(port) as u64),
                    16 => Ok(context.handler.read_io_u16(port) as u64),
                    32 => Ok(context.handler.read_io_u32(port) as u64),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::PciConfig => {
                /*
                 * First, we need to get some extra information out of objects in the parent object. Both
                 * `_SEG` and `_BBN` seem optional, with defaults that line up with legacy PCI implementations
                 * (e.g. systems with a single segment group and a single root, respectively).
                 */
                let parent_device = self.parent_device.as_ref().unwrap();
                let seg = match context.invoke_method(
                    &AmlName::from_str("_SEG").unwrap().resolve(parent_device).unwrap(),
                    Args::EMPTY,
                ) {
                    Ok(seg) => seg.as_integer(context)?.try_into().map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let bbn = match context.invoke_method(
                    &AmlName::from_str("_BBN").unwrap().resolve(parent_device).unwrap(),
                    Args::EMPTY,
                ) {
                    Ok(bbn) => bbn.as_integer(context)?.try_into().map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let adr = {
                    let adr = context.invoke_method(
                        &AmlName::from_str("_ADR").unwrap().resolve(parent_device).unwrap(),
                        Args::EMPTY,
                    )?;
                    adr.as_integer(context)?
                };

                let device = adr.get_bits(16..24) as u8;
                let function = adr.get_bits(0..8) as u8;
                let offset = (self.base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;

                match length {
                    8 => Ok(context.handler.read_pci_u8(seg, bbn, device, function, offset) as u64),
                    16 => Ok(context.handler.read_pci_u16(seg, bbn, device, function, offset) as u64),
                    32 => Ok(context.handler.read_pci_u32(seg, bbn, device, function, offset) as u64),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            // TODO
            _ => unimplemented!(),
        }
    }

    /// Perform a standard-size write to this op-region. `length` must be a supported power-of-2,
    /// and `offset` correctly aligned for that `length`. `value` must be appropriately sized.
    pub fn write(&self, offset: u64, length: u64, value: u64, context: &mut AmlContext) -> Result<(), AmlError> {
        match self.region {
            RegionSpace::SystemMemory => {
                let address = (self.base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(context.handler.write_u8(address, value as u8)),
                    16 => Ok(context.handler.write_u16(address, value as u16)),
                    32 => Ok(context.handler.write_u32(address, value as u32)),
                    64 => Ok(context.handler.write_u64(address, value)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::SystemIo => {
                let port = (self.base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(context.handler.write_io_u8(port, value as u8)),
                    16 => Ok(context.handler.write_io_u16(port, value as u16)),
                    32 => Ok(context.handler.write_io_u32(port, value as u32)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::PciConfig => {
                /*
                 * First, we need to get some extra information out of objects in the parent object. Both
                 * `_SEG` and `_BBN` seem optional, with defaults that line up with legacy PCI implementations
                 * (e.g. systems with a single segment group and a single root, respectively).
                 */
                let parent_device = self.parent_device.as_ref().unwrap();
                let seg = match context.invoke_method(
                    &AmlName::from_str("_SEG").unwrap().resolve(parent_device).unwrap(),
                    Args::EMPTY,
                ) {
                    Ok(seg) => seg.as_integer(context)?.try_into().map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let bbn = match context.invoke_method(
                    &AmlName::from_str("_BBN").unwrap().resolve(parent_device).unwrap(),
                    Args::EMPTY,
                ) {
                    Ok(bbn) => bbn.as_integer(context)?.try_into().map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let adr = {
                    let adr = context.invoke_method(
                        &AmlName::from_str("_ADR").unwrap().resolve(parent_device).unwrap(),
                        Args::EMPTY,
                    )?;
                    adr.as_integer(context)?
                };

                let device = adr.get_bits(16..24) as u8;
                let function = adr.get_bits(0..8) as u8;
                let offset = (self.base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;

                match length {
                    8 => Ok(context.handler.write_pci_u8(seg, bbn, device, function, offset, value as u8)),
                    16 => Ok(context.handler.write_pci_u16(seg, bbn, device, function, offset, value as u16)),
                    32 => Ok(context.handler.write_pci_u32(seg, bbn, device, function, offset, value as u32)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            // TODO
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RegionSpace {
    SystemMemory,
    SystemIo,
    PciConfig,
    EmbeddedControl,
    SMBus,
    SystemCmos,
    PciBarTarget,
    IPMI,
    GeneralPurposeIo,
    GenericSerialBus,
    OemDefined(u8),
}
