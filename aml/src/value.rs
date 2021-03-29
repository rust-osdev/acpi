use crate::{misc::ArgNum, AmlContext, AmlError, AmlHandle, AmlName};
use alloc::{rc::Rc, string::String, vec::Vec};
use bit_field::BitField;
use core::{cmp, fmt, fmt::Debug};

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
    pub fn new(arg_count: u8, serialize: bool, sync_level: u8) -> MethodFlags {
        assert!(arg_count <= 7);
        assert!(sync_level <= 15);

        let mut value = 0;
        value.set_bits(0..3, arg_count);
        value.set_bit(3, serialize);
        value.set_bits(4..8, sync_level);
        MethodFlags(value)
    }

    pub fn from(value: u8) -> MethodFlags {
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

#[derive(Clone)]
pub enum MethodCode {
    Aml(Vec<u8>),
    Native(Rc<dyn Fn(&mut AmlContext) -> Result<AmlValue, AmlError>>),
}

impl fmt::Debug for MethodCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MethodCode::Aml(ref code) => f.debug_struct("AML method").field("code", code).finish(),
            MethodCode::Native(_) => f.debug_struct("Native method").finish(),
        }
    }
}

#[derive(Clone, Debug)]
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
        code: MethodCode,
    },
    Buffer(Vec<u8>),
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
    pub fn zero() -> AmlValue {
        AmlValue::Integer(0)
    }

    pub fn one() -> AmlValue {
        AmlValue::Integer(1)
    }

    pub fn ones() -> AmlValue {
        AmlValue::Integer(u64::max_value())
    }

    pub fn native_method<F>(arg_count: u8, serialize: bool, sync_level: u8, f: F) -> AmlValue
    where
        F: Fn(&mut AmlContext) -> Result<AmlValue, AmlError> + 'static,
    {
        let flags = MethodFlags::new(arg_count, serialize, sync_level);
        AmlValue::Method { flags, code: MethodCode::Native(Rc::new(f)) }
    }

    pub fn type_of(&self) -> AmlType {
        match self {
            AmlValue::Boolean(_) => AmlType::Integer,
            AmlValue::Integer(_) => AmlType::Integer,
            AmlValue::String(_) => AmlType::String,
            AmlValue::OpRegion { .. } => AmlType::OpRegion,
            AmlValue::Field { .. } => AmlType::FieldUnit,
            AmlValue::Method { .. } => AmlType::Method,
            AmlValue::Buffer(_) => AmlType::Buffer,
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

            AmlValue::Buffer(ref bytes) => {
                /*
                 * "The first 8 bytes of the buffer are converted to an integer, taking the first
                 * byte as the least significant byte of the integer. A zero-length buffer is
                 * illegal." - §19.6.140
                 *
                 * XXX: Buffers with length `0` appear in real tables, so we return `0` for them.
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

    pub fn as_buffer(&self, context: &AmlContext) -> Result<Vec<u8>, AmlError> {
        match self {
            AmlValue::Buffer(ref bytes) => Ok(bytes.clone()),
            // TODO: implement conversion of String and Integer to Buffer
            AmlValue::Field { .. } => self.read_field(context)?.as_buffer(context),
            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }

    pub fn as_string(&self, context: &AmlContext) -> Result<String, AmlError> {
        match self {
            AmlValue::String(ref string) => Ok(string.clone()),
            // TODO: implement conversion of Buffer to String
            AmlValue::Field { .. } => self.read_field(context)?.as_string(context),
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

            /*
             * TODO: we need to decide properly how to read from the region itself. Complications:
             *    - if the region has a minimum access size greater than the desired length, we need to read the
             *      minimum and mask it (reading a byte from a WordAcc region)
             *    - if the desired length is larger than we can read, we need to do multiple reads
             */
            Ok(AmlValue::Integer(
                context.read_region(*region, *offset, access_size)?.get_bits(0..(*length as usize)),
            ))
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }

    pub fn write_field(&mut self, value: AmlValue, context: &mut AmlContext) -> Result<(), AmlError> {
        /*
         * If the field's update rule is `Preserve`, we need to read the initial value of the field, so we can
         * overwrite the correct bits. We destructure the field to do the actual write, so we read from it if
         * needed here, otherwise the borrow-checker doesn't understand.
         */
        let field_update_rule = if let AmlValue::Field { region, flags, offset, length } = self {
            flags.field_update_rule()?
        } else {
            return Err(AmlError::IncompatibleValueConversion);
        };
        let mut field_value = match field_update_rule {
            FieldUpdateRule::Preserve => self.read_field(context)?.as_integer(context)?,
            FieldUpdateRule::WriteAsOnes => 0xffffffff_ffffffff,
            FieldUpdateRule::WriteAsZeros => 0x0,
        };

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

            field_value.set_bits(0..(*length as usize), value.as_integer(context)?);
            context.write_region(*region, *offset, access_size, field_value)
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }

    /// Logically compare two `AmlValue`s, according to the rules that govern opcodes like `DefLEqual`, `DefLLess`,
    /// etc. The type of `self` dictates the type that `other` will be converted to, and the method by which the
    /// values will be compared:
    ///    - `Integer`s are simply compared by numeric comparison
    ///    - `String`s and `Buffer`s are compared lexicographically - `other` is compared byte-wise until a byte
    ///      is discovered that is either less or greater than the corresponding byte of `self`. If the bytes are
    ///      identical, the lengths are compared. Luckily, the Rust standard library implements lexicographic
    ///      comparison of strings and `[u8]` for us already.
    pub fn cmp(&self, other: AmlValue, context: &mut AmlContext) -> Result<cmp::Ordering, AmlError> {
        let self_inner =
            if self.type_of() == AmlType::FieldUnit { self.read_field(context)? } else { self.clone() };

        match self_inner.type_of() {
            AmlType::Integer => Ok(self.as_integer(context)?.cmp(&other.as_integer(context)?)),
            AmlType::Buffer => Ok(self.as_buffer(context)?.cmp(&other.as_buffer(context)?)),
            AmlType::String => Ok(self.as_string(context)?.cmp(&other.as_string(context)?)),
            typ => Err(AmlError::TypeCannotBeCompared(typ)),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{test_utils::*, AmlError};
    use core::cmp::Ordering;

    #[test]
    fn test_object_cmp() {
        let mut context = make_test_context();

        assert_eq!(AmlValue::Integer(76).cmp(AmlValue::Integer(89), &mut context), Ok(Ordering::Less));
        assert_eq!(AmlValue::Integer(11).cmp(AmlValue::Integer(11), &mut context), Ok(Ordering::Equal));
        assert_eq!(AmlValue::Integer(8362836690).cmp(AmlValue::Integer(1), &mut context), Ok(Ordering::Greater));

        assert_eq!(
            AmlValue::Integer(4).cmp(AmlValue::Boolean(true), &mut context),
            Err(AmlError::IncompatibleValueConversion)
        );

        // TODO: test the other combinations too, as well as conversions to the correct types for the second operand
    }
}
