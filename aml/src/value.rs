use crate::{misc::ArgNum, AmlContext, AmlError, AmlHandle, AmlName};
use alloc::{string::String, vec::Vec};
use bit_field::BitField;

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
    pub present: bool,
    /// Whether the device is enabled. Both `present` and `enabled` must be `true` for the device to decode its
    /// hardware resources.
    pub enabled: bool,
    pub show_in_ui: bool,
    pub functional: bool,
    /// Only applicable for Control Method Battery Devices (`PNP0C0A`). For all other devices, ignore this value.
    pub battery_present: bool,
}

impl Default for StatusObject {
    fn default() -> Self {
        StatusObject { present: true, enabled: true, show_in_ui: true, functional: true, battery_present: true }
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
                    functional: value.get_bit(3),
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
        // If the value is already of the correct type, just return it as is
        if self.type_of() == desired_type {
            return Ok(self.clone());
        }

        // TODO: implement all of the rules
        match desired_type {
            AmlType::Integer => self.as_integer(context).map(|value| AmlValue::Integer(value)),
            AmlType::FieldUnit => panic!(
                "Can't implicitly convert to FieldUnit. This must be special-cased by the caller for now :("
            ),
            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }

    /// Reads from a field of an opregion, returning either a `AmlValue::Integer` or an `AmlValue::Buffer`,
    /// depending on the size of the field.
    pub fn read_field(&self, context: &AmlContext) -> Result<AmlValue, AmlError> {
        if let AmlValue::Field { region, flags, offset, length } = self {
            /*
             * TODO: we need to decide properly how to read from the region itself. Complications:
             *    - if the region has a minimum access size greater than the desired length, we need to read the
             *      minimum and mask it (reading a byte from a WordAcc region)
             *    - if the desired length is larger than we can read, we need to do multiple reads
             */
            Ok(AmlValue::Integer(context.read_region(*region, *offset, *length)?))
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }

    pub fn write_field(&mut self, value: AmlValue, context: &mut AmlContext) -> Result<(), AmlError> {
        /*
         * TODO:
         * If we need to preserve the field's value, we'll need the contents of the field before we write it. To
         * appease the borrow-checker, this is done before we destructure the field for now, but it would be more
         * efficient if we could only do this if the field's update rule is Preserve.
         */
        let field_value = self.read_field(context)?.as_integer(context)?;

        if let AmlValue::Field { region, flags, offset, length } = self {
            let maximum_access_size = {
                if let AmlValue::OpRegion { region, .. } = context.namespace.get(*region)? {
                    match region {
                        RegionSpace::SystemMemory => 64,
                        RegionSpace::SystemIo | RegionSpace::PciConfig => 32,
                        _ => unimplemented!(),
                    }
                } else {
                    return Err(AmlError::FieldRegionIsNotOpRegion);
                }
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

            let mut value_to_write = match flags.field_update_rule()? {
                FieldUpdateRule::Preserve => field_value,
                FieldUpdateRule::WriteAsOnes => 0xffffffff_ffffffff,
                FieldUpdateRule::WriteAsZeros => 0x0,
            };
            value_to_write.set_bits(0..(*length as usize), value.as_integer(context)?);

            context.write_region(*region, *offset, access_size, value_to_write)
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
