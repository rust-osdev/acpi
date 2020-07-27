use crate::{misc::ArgNum, AmlContext, AmlError, AmlHandle, AmlName};
use alloc::{string::String, vec::Vec};
use bit_field::BitField;
use core::convert::TryInto;

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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FieldAccessType {
    Any,
    Byte,
    Word,
    DWord,
    QWord,
    Buffer,
    Reserved,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FieldUpdateRule {
    Preserve,
    WriteAsOnes,
    WriteAsZeros,
}

// TODO: custom debug impl
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct FieldFlags(u8);

impl FieldFlags {
    pub fn new(value: u8) -> FieldFlags {
        FieldFlags(value)
    }

    pub fn access_type(&self) -> Result<FieldAccessType, AmlError> {
        match self.0.get_bits(0..4) {
            0 => Ok(FieldAccessType::Any),
            1 => Ok(FieldAccessType::Byte),
            2 => Ok(FieldAccessType::Word),
            3 => Ok(FieldAccessType::DWord),
            4 => Ok(FieldAccessType::QWord),
            5 => Ok(FieldAccessType::Buffer),
            _ => Err(AmlError::InvalidFieldFlags),
        }
    }

    pub fn lock_rule(&self) -> bool {
        self.0.get_bit(4)
    }

    pub fn field_update_rule(&self) -> Result<FieldUpdateRule, AmlError> {
        match self.0.get_bits(5..7) {
            0 => Ok(FieldUpdateRule::Preserve),
            1 => Ok(FieldUpdateRule::WriteAsOnes),
            2 => Ok(FieldUpdateRule::WriteAsZeros),
            _ => Err(AmlError::InvalidFieldFlags),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct MethodFlags(u8);

impl MethodFlags {
    pub fn new(value: u8) -> MethodFlags {
        MethodFlags(value)
    }

    pub fn arg_count(&self) -> u8 {
        self.0.get_bits(0..3)
    }

    pub fn serialize(&self) -> bool {
        self.0.get_bit(3)
    }

    pub fn sync_level(&self) -> u8 {
        self.0.get_bits(4..8)
    }
}

/// Representation of the return value of a `_STA` method, which represents the status of an object. It must be
/// evaluated, if present, before evaluating the `_INI` method for an device.
///
/// The `Default` implementation of this type is the correct value to use if a device doesn't have a `_STA` object
/// to evaluate.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct StatusObject {
    /// Whether the device is physically present. If this is `false`, `enabled` should also be `false` (i.e. a
    /// device that is not present can't be enabled). However, this is not enforced here if the firmware is doing
    /// something wrong.
    present: bool,
    /// Whether the device is enabled. Both `present` and `enabled` must be `true` for the device to decode its
    /// hardware resources.
    enabled: bool,
    show_in_ui: bool,
    functioning: bool,
    /// Only applicable for Control Method Battery Devices (`PNP0C0A`). For all other devices, ignore this value.
    battery_present: bool,
}

impl Default for StatusObject {
    fn default() -> Self {
        StatusObject { present: true, enabled: true, show_in_ui: true, functioning: true, battery_present: true }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AmlType {
    Uninitialized,
    Buffer,
    BufferField,
    /// Handle to a definition block handle. Returned by the `Load` operator.
    DdbHandle,
    DebugObject,
    Event,
    FieldUnit,
    Integer,
    Method,
    Mutex,
    ObjReference,
    OpRegion,
    Package,
    PowerResource,
    Processor,
    RawDataBuffer,
    String,
    ThermalZone,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AmlValue {
    Boolean(bool),
    Integer(u64),
    String(String),
    /// Describes an operation region. Some regions require other objects to be declared under their parent device
    /// (e.g. an `_ADR` object for a `PciConfig` region), in which case an absolute path to the object is stored in
    /// `parent_device`.
    OpRegion {
        region: RegionSpace,
        offset: u64,
        length: u64,
        parent_device: Option<AmlName>,
    },
    /// Describes a field unit within an operation region.
    Field {
        region: AmlHandle,
        flags: FieldFlags,
        offset: u64,
        length: u64,
    },
    Method {
        flags: MethodFlags,
        code: Vec<u8>,
    },
    Buffer {
        bytes: Vec<u8>,
        size: u64,
    },
    Processor {
        id: u8,
        pblk_address: u32,
        pblk_len: u8,
    },
    Mutex {
        sync_level: u8,
    },
    Package(Vec<AmlValue>),
}

impl AmlValue {
    /// Returns the AML type of this value. For `Name`, this returns the type of the inner value.
    pub fn type_of(&self) -> AmlType {
        match self {
            AmlValue::Boolean(_) => AmlType::Integer,
            AmlValue::Integer(_) => AmlType::Integer,
            AmlValue::String(_) => AmlType::String,
            AmlValue::OpRegion { .. } => AmlType::OpRegion,
            AmlValue::Field { .. } => AmlType::FieldUnit,
            AmlValue::Method { .. } => AmlType::Method,
            AmlValue::Buffer { .. } => AmlType::Buffer,
            AmlValue::Processor { .. } => AmlType::Processor,
            AmlValue::Mutex { .. } => AmlType::Mutex,
            AmlValue::Package(_) => AmlType::Package,
        }
    }

    pub fn as_bool(&self) -> Result<bool, AmlError> {
        match self {
            AmlValue::Boolean(value) => Ok(*value),
            AmlValue::Integer(value) => Ok(*value != 0),
            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }

    pub fn as_integer(&self, context: &AmlContext) -> Result<u64, AmlError> {
        match self {
            AmlValue::Integer(value) => Ok(*value),

            AmlValue::Buffer { ref bytes, .. } => {
                /*
                 * "The first 8 bytes of the buffer are converted to an integer, taking the first
                 * byte as the least significant byte of the integer. A zero-length buffer is
                 * illegal." - §19.6.140
                 *
                 * XXX: We return `0` for zero-length buffers because they literally occur in
                 * the reference implementation.
                 */
                let bytes = if bytes.len() > 8 { &bytes[0..8] } else { bytes };

                Ok(bytes.iter().rev().fold(0: u64, |mut i, &popped| {
                    i <<= 8;
                    i += popped as u64;
                    i
                }))
            }

            /*
             * Read from a field. This can return either a `Buffer` or an `Integer`, so we make sure to call
             * `as_integer` on the result.
             */
            AmlValue::Field { .. } => self.read_field(context)?.as_integer(context),

            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }

    /// Turns an `AmlValue` returned from a `_STA` method into a `StatusObject`. Should only be called for values
    /// returned from `_STA`. If you need a `StatusObject`, but the device does not have a `_STA` method, use
    /// `StatusObject::default()`.
    pub fn as_status(&self) -> Result<StatusObject, AmlError> {
        match self {
            AmlValue::Integer(value) => {
                /*
                 * Bits 5+ are reserved and are expected to be cleared.
                 */
                if value.get_bits(5..64) != 0 {
                    return Err(AmlError::InvalidStatusObject);
                }

                Ok(StatusObject {
                    present: value.get_bit(0),
                    enabled: value.get_bit(1),
                    show_in_ui: value.get_bit(2),
                    functioning: value.get_bit(3),
                    battery_present: value.get_bit(4),
                })
            }

            _ => Err(AmlError::InvalidStatusObject),
        }
    }

    /// Convert this value to a value of the same data, but with the given AML type, if possible,
    /// by converting the implicit conversions described in §19.3.5 of the spec.
    ///
    /// The implicit conversions applied are:
    ///     `Buffer` from: `Integer`, `String`, `Debug`
    ///     `BufferField` from: `Integer`, `Buffer`, `String`, `Debug`
    ///     `DdbHandle` from: `Integer`, `Debug`
    ///     `FieldUnit` from: `Integer`,`Buffer`, `String`, `Debug`
    ///     `Integer` from: `Buffer`, `BufferField`, `DdbHandle`, `FieldUnit`, `String`, `Debug`
    ///     `Package` from: `Debug`
    ///     `String` from: `Integer`, `Buffer`, `Debug`
    pub fn as_type(&self, desired_type: AmlType, context: &AmlContext) -> Result<AmlValue, AmlError> {
        // Cache the type of this object
        let our_type = self.type_of();

        // If the value is already of the correct type, just return it as is
        if our_type == desired_type {
            return Ok(self.clone());
        }

        // TODO: implement all of the rules
        match desired_type {
            AmlType::Integer => self.as_integer(context).map(|value| AmlValue::Integer(value)),
            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }

    /// Reads from a field of an opregion, returning either a `AmlValue::Integer` or an `AmlValue::Buffer`,
    /// depending on the size of the field.
    pub fn read_field(&self, context: &AmlContext) -> Result<AmlValue, AmlError> {
        if let AmlValue::Field { region, flags, offset, length } = self {
            let (region_space, region_base, region_length, parent_device) = {
                if let AmlValue::OpRegion { region, offset, length, parent_device } =
                    context.namespace.get(*region)?
                {
                    (region, offset, length, parent_device)
                } else {
                    return Err(AmlError::FieldRegionIsNotOpRegion);
                }
            };

            match region_space {
                RegionSpace::SystemMemory => {
                    let address = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                    match length {
                        8 => Ok(AmlValue::Integer(context.handler.read_u8(address) as u64)),
                        16 => Ok(AmlValue::Integer(context.handler.read_u16(address) as u64)),
                        32 => Ok(AmlValue::Integer(context.handler.read_u32(address) as u64)),
                        64 => Ok(AmlValue::Integer(context.handler.read_u64(address))),
                        _ => Err(AmlError::FieldInvalidAccessSize),
                    }
                }

                RegionSpace::SystemIo => {
                    let port = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                    match length {
                        8 => Ok(AmlValue::Integer(context.handler.read_io_u8(port) as u64)),
                        16 => Ok(AmlValue::Integer(context.handler.read_io_u16(port) as u64)),
                        32 => Ok(AmlValue::Integer(context.handler.read_io_u32(port) as u64)),
                        _ => Err(AmlError::FieldInvalidAccessSize),
                    }
                }

                // TODO
                _ => unimplemented!(),
            }
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }
}

/// A control method can take up to 7 arguments, each of which can be an `AmlValue`.
#[derive(Clone, Debug, Default)]
pub struct Args {
    pub arg_0: Option<AmlValue>,
    pub arg_1: Option<AmlValue>,
    pub arg_2: Option<AmlValue>,
    pub arg_3: Option<AmlValue>,
    pub arg_4: Option<AmlValue>,
    pub arg_5: Option<AmlValue>,
    pub arg_6: Option<AmlValue>,
}

impl Args {
    pub fn from_list(mut list: Vec<AmlValue>) -> Args {
        assert!(list.len() <= 7);
        list.reverse();
        Args {
            arg_0: list.pop(),
            arg_1: list.pop(),
            arg_2: list.pop(),
            arg_3: list.pop(),
            arg_4: list.pop(),
            arg_5: list.pop(),
            arg_6: list.pop(),
        }
    }
    /// Get an argument by its `ArgNum`.
    ///
    /// ### Panics
    /// Panics if passed an invalid argument number (valid argument numbers are `0..=6`)
    pub fn arg(&self, num: ArgNum) -> Result<&AmlValue, AmlError> {
        match num {
            0 => self.arg_0.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            1 => self.arg_1.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            2 => self.arg_2.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            3 => self.arg_3.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            4 => self.arg_4.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            5 => self.arg_5.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            6 => self.arg_6.as_ref().ok_or(AmlError::InvalidArgAccess(num)),
            _ => Err(AmlError::InvalidArgAccess(num)),
        }
    }
}
