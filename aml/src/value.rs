use crate::{misc::ArgNum, opregion::OpRegion, AmlContext, AmlError, AmlHandle};
use alloc::{
    string::{String, ToString},
    sync::Arc,
    vec::Vec,
};
use bit_field::BitField;
use core::{cmp, fmt, fmt::Debug};
use spinning_top::Spinlock;

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
    Device,
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
    Native(Arc<dyn Fn(&mut AmlContext) -> Result<AmlValue, AmlError> + Send + Sync>),
}

impl fmt::Debug for MethodCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MethodCode::Aml(ref code) => write!(f, "AML({:x?})", code),
            MethodCode::Native(_) => write!(f, "(native method)"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AmlValue {
    Uninitialized,
    Boolean(bool),
    Integer(u64),
    String(String),
    /// Describes an operation region. Some regions require other objects to be declared under their parent device
    /// (e.g. an `_ADR` object for a `PciConfig` region), in which case an absolute path to the object is stored in
    /// `parent_device`.
    OpRegion(OpRegion),
    /// Describes a field unit within an operation region.
    Field {
        region: AmlHandle,
        flags: FieldFlags,
        offset: u64,
        length: u64,
    },
    Device,
    Method {
        flags: MethodFlags,
        code: MethodCode,
    },
    Buffer(Arc<Spinlock<Vec<u8>>>),
    BufferField {
        buffer_data: Arc<Spinlock<Vec<u8>>>,
        /// In bits.
        offset: u64,
        /// In bits.
        length: u64,
    },
    Processor {
        id: u8,
        pblk_address: u32,
        pblk_len: u8,
    },
    Mutex {
        sync_level: u8,
    },
    // TODO: I think this will need to be `Arc`ed as well, as `Index` can be used on both Buffers and Packages
    Package(Vec<AmlValue>),
    PowerResource {
        system_level: u8,
        resource_order: u16,
    },
    ThermalZone,
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
        F: (Fn(&mut AmlContext) -> Result<AmlValue, AmlError>) + 'static + Send + Sync,
    {
        let flags = MethodFlags::new(arg_count, serialize, sync_level);
        AmlValue::Method { flags, code: MethodCode::Native(Arc::new(f)) }
    }

    pub fn type_of(&self) -> AmlType {
        match self {
            AmlValue::Uninitialized => AmlType::Uninitialized,
            AmlValue::Boolean(_) => AmlType::Integer,
            AmlValue::Integer(_) => AmlType::Integer,
            AmlValue::String(_) => AmlType::String,
            AmlValue::OpRegion { .. } => AmlType::OpRegion,
            AmlValue::Field { .. } => AmlType::FieldUnit,
            AmlValue::Device => AmlType::Device,
            AmlValue::Method { .. } => AmlType::Method,
            AmlValue::Buffer(_) => AmlType::Buffer,
            AmlValue::BufferField { .. } => AmlType::BufferField,
            AmlValue::Processor { .. } => AmlType::Processor,
            AmlValue::Mutex { .. } => AmlType::Mutex,
            AmlValue::Package(_) => AmlType::Package,
            AmlValue::PowerResource { .. } => AmlType::PowerResource,
            AmlValue::ThermalZone => AmlType::ThermalZone,
        }
    }

    /// Returns the `SizeOf (x)` application result as specified in ACPI 6.2 §19.6.125
    pub fn size_of(&self) -> Result<u64, AmlError> {
        match self {
            // For a buffer, returns the size in bytes of the data
            AmlValue::Buffer(value) => Ok(value.lock().len() as u64),
            // For a string, returns the size in bytes (without NULL)
            AmlValue::String(value) => Ok(value.len() as u64),
            // For a package, returns the number of elements
            AmlValue::Package(value) => Ok(value.len() as u64),
            // TODO: For an Object Reference, the size of the object is returned
            // Other data types cause a fatal run-time error
            _ => Err(AmlError::InvalidSizeOfApplication(self.type_of())),
        }
    }

    pub fn as_bool(&self, context: &mut AmlContext) -> Result<bool, AmlError> {
        match self {
            AmlValue::Boolean(value) => Ok(*value),
            AmlValue::Integer(value) => Ok(*value != 0),
            AmlValue::Field { .. } => Ok(self.as_integer(context)? != 0),
            _ => Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::Integer }),
        }
    }

    pub fn as_integer(&self, context: &mut AmlContext) -> Result<u64, AmlError> {
        match self {
            AmlValue::Integer(value) => Ok(*value),
            AmlValue::Boolean(value) => Ok(if *value { u64::max_value() } else { 0 }),
            AmlValue::Buffer(ref bytes) => {
                /*
                 * "The first 8 bytes of the buffer are converted to an integer, taking the first
                 * byte as the least significant byte of the integer. A zero-length buffer is
                 * illegal." - §19.6.140
                 *
                 * XXX: Buffers with length `0` appear in real tables, so we return `0` for them.
                 */
                let bytes = bytes.lock();
                let bytes = if bytes.len() > 8 { &bytes[0..8] } else { &bytes[..] };

                Ok(bytes.iter().rev().fold(0u64, |mut i, &popped| {
                    i <<= 8;
                    i += popped as u64;
                    i
                }))
            }
            /*
             * Read from a field or buffer field. These can return either a `Buffer` or an `Integer`, so we make sure to call
             * `as_integer` on the result.
             */
            AmlValue::Field { .. } => self.read_field(context)?.as_integer(context),
            AmlValue::BufferField { .. } => self.read_buffer_field(context)?.as_integer(context),

            _ => Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::Integer }),
        }
    }

    pub fn as_buffer(&self, context: &mut AmlContext) -> Result<Arc<Spinlock<Vec<u8>>>, AmlError> {
        match self {
            AmlValue::Buffer(ref bytes) => Ok(bytes.clone()),
            // TODO: implement conversion of String and Integer to Buffer
            AmlValue::Field { .. } => self.read_field(context)?.as_buffer(context),
            AmlValue::BufferField { .. } => self.read_buffer_field(context)?.as_buffer(context),
            _ => Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::Buffer }),
        }
    }

    pub fn as_string(&self, context: &mut AmlContext) -> Result<String, AmlError> {
        match self {
            AmlValue::String(ref string) => Ok(string.clone()),
            // TODO: implement conversion of Buffer to String
            AmlValue::Field { .. } => self.read_field(context)?.as_string(context),
            _ => Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::String }),
        }
    }

    /// Converts an `AmlValue` to the representation that should be used when concatenating it with other values,
    /// primarily by the `DefConcat` opcode. This will always produce a `AmlValue::Integer`, `AmlValue::String`, or
    /// `AmlValue::Buffer`, with other types being converted to strings containing the name of their type.
    pub fn as_concat_type(&self) -> AmlValue {
        match self.type_of() {
            AmlType::Integer => self.clone(),
            AmlType::String => self.clone(),
            AmlType::Buffer => self.clone(),

            AmlType::Uninitialized => AmlValue::String("[Uninitialized]".to_string()),
            AmlType::BufferField => AmlValue::String("[Buffer Field]".to_string()),
            AmlType::DdbHandle => AmlValue::String("[Ddb Handle]".to_string()),
            AmlType::DebugObject => AmlValue::String("[Debug Object]".to_string()),
            AmlType::Event => AmlValue::String("[Event]".to_string()),
            AmlType::FieldUnit => AmlValue::String("[Field]".to_string()),
            AmlType::Device => AmlValue::String("[Device]".to_string()),
            AmlType::Method => AmlValue::String("[Control Method]".to_string()),
            AmlType::Mutex => AmlValue::String("[Mutex]".to_string()),
            AmlType::ObjReference => AmlValue::String("[Obj Reference]".to_string()),
            AmlType::OpRegion => AmlValue::String("[Operation Region]".to_string()),
            AmlType::Package => AmlValue::String("[Package]".to_string()),
            AmlType::Processor => AmlValue::String("[Processor]".to_string()),
            AmlType::PowerResource => AmlValue::String("[Power Resource]".to_string()),
            AmlType::RawDataBuffer => AmlValue::String("[Raw Data Buffer]".to_string()),
            AmlType::ThermalZone => AmlValue::String("[Thermal Zone]".to_string()),
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
    pub fn as_type(&self, desired_type: AmlType, context: &mut AmlContext) -> Result<AmlValue, AmlError> {
        // If the value is already of the correct type, just return it as is
        if self.type_of() == desired_type {
            return Ok(self.clone());
        }

        // TODO: implement all of the rules
        match desired_type {
            AmlType::Integer => self.as_integer(context).map(|value| AmlValue::Integer(value)),
            AmlType::Buffer => self.as_buffer(context).map(|value| AmlValue::Buffer(value)),
            AmlType::FieldUnit => panic!(
                "Can't implicitly convert to FieldUnit. This must be special-cased by the caller for now :("
            ),
            _ => Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: desired_type }),
        }
    }

    /// Reads from a field of an op-region, returning either a `AmlValue::Integer` or an `AmlValue::Buffer`,
    /// depending on the size of the field.
    pub fn read_field(&self, context: &mut AmlContext) -> Result<AmlValue, AmlError> {
        if let AmlValue::Field { region, flags, offset, length } = self {
            if let AmlValue::OpRegion(region) = context.namespace.get(*region)?.clone() {
                region.read_field(*offset, *length, *flags, context)
            } else {
                Err(AmlError::FieldRegionIsNotOpRegion)
            }
        } else {
            Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::FieldUnit })
        }
    }

    /// Write to a field of an op-region, from either a `AmlValue::Integer` or `AmlValue::Buffer`
    /// as necessary.
    pub fn write_field(&mut self, value: AmlValue, context: &mut AmlContext) -> Result<(), AmlError> {
        if let AmlValue::Field { region, flags, offset, length } = self {
            if let AmlValue::OpRegion(region) = context.namespace.get(*region)?.clone() {
                region.write_field(*offset, *length, *flags, value, context)
            } else {
                Err(AmlError::FieldRegionIsNotOpRegion)
            }
        } else {
            Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::FieldUnit })
        }
    }

    pub fn read_buffer_field(&self, _context: &AmlContext) -> Result<AmlValue, AmlError> {
        use bitvec::view::BitView;

        if let AmlValue::BufferField { buffer_data, offset, length } = self {
            let offset = *offset as usize;
            let length = *length as usize;
            let inner_data = buffer_data.lock();

            if (offset + length) > (inner_data.len() * 8) {
                return Err(AmlError::BufferFieldIndexesOutOfBounds);
            }

            let bitslice = inner_data.view_bits::<bitvec::order::Lsb0>();
            let bits = &bitslice[offset..(offset + length)];

            if length > 64 {
                let mut bitvec = bits.to_bitvec();
                bitvec.set_uninitialized(false);
                Ok(AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(bitvec.into_vec()))))
            } else if length > 32 {
                /*
                 * TODO: this is a pretty gross hack to work around a weird limitation with the `bitvec` crate on
                 * 32-bit platforms. For reasons beyond me right now, it can't operate on a `u64` on a 32-bit
                 * platform, so we manually extract two `u32`s and stick them together. In the future, we should
                 * definitely have a closer look at what `bitvec` is doing and see if we can fix this code, or
                 * replace it with a different crate. This should hold everything vaguely together until we have
                 * time to do that.
                 */
                let mut upper = 0u32;
                let mut lower = 0u32;
                lower.view_bits_mut::<bitvec::order::Lsb0>()[0..32].clone_from_bitslice(bits);
                upper.view_bits_mut::<bitvec::order::Lsb0>()[0..(length - 32)].clone_from_bitslice(&bits[32..]);
                Ok(AmlValue::Integer((upper as u64) << 32 + (lower as u64)))
            } else {
                let mut value = 0u32;
                value.view_bits_mut::<bitvec::order::Lsb0>()[0..length].clone_from_bitslice(bits);
                Ok(AmlValue::Integer(value as u64))
            }
        } else {
            Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::BufferField })
        }
    }

    pub fn write_buffer_field(&mut self, value: AmlValue, _context: &mut AmlContext) -> Result<(), AmlError> {
        use bitvec::view::BitView;

        if let AmlValue::BufferField { buffer_data, offset, length } = self {
            let offset = *offset as usize;
            let length = *length as usize;
            // TODO: check these against the size of the buffer to be written into
            let mut inner_data = buffer_data.lock();
            let bitslice = inner_data.view_bits_mut::<bitvec::order::Lsb0>();

            match value {
                AmlValue::Integer(value) => {
                    /*
                     * When an `Integer` is written into a `BufferField`, the entire contents are overwritten. If
                     * it's smaller than the length of the buffer field, it's zero-extended. If it's larger, the
                     * upper bits are truncated.
                     */
                    let bits_to_copy = cmp::min(length, 64);
                    bitslice[offset..(offset + bits_to_copy)]
                        .copy_from_bitslice(&value.to_le_bytes().view_bits()[..(bits_to_copy as usize)]);
                    // Zero extend to the end of the buffer field
                    bitslice[(offset + bits_to_copy)..(offset + length)].fill(false);
                    Ok(())
                }
                AmlValue::Boolean(value) => {
                    bitslice.set(offset, value);
                    Ok(())
                }
                AmlValue::Buffer(value) => {
                    /*
                     * When a `Buffer` is written into a `BufferField`, the entire contents are copied into the
                     * field. If the buffer is smaller than the size of the buffer field, it is zero extended. If
                     * the buffer is larger, the upper bits are truncated.
                     * XXX: this behaviour is only explicitly defined in ACPI 2.0+. While undefined in ACPI 1.0,
                     * we produce the same behaviour there.
                     */
                    let value_data = value.lock();
                    let bits_to_copy = cmp::min(length, value_data.len() * 8);
                    bitslice[offset..(offset + bits_to_copy)]
                        .copy_from_bitslice(&value_data.view_bits()[..(bits_to_copy as usize)]);
                    // Zero extend to the end of the buffer field
                    bitslice[(offset + bits_to_copy)..(offset + length)].fill(false);
                    Ok(())
                }
                _ => Err(AmlError::TypeCannotBeWrittenToBufferField(value.type_of())),
            }
        } else {
            Err(AmlError::IncompatibleValueConversion { current: self.type_of(), target: AmlType::BufferField })
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
            AmlType::Buffer => Ok(self.as_buffer(context)?.lock().cmp(&other.as_buffer(context)?.lock())),
            AmlType::String => Ok(self.as_string(context)?.cmp(&other.as_string(context)?)),
            typ => Err(AmlError::TypeCannotBeCompared(typ)),
        }
    }
}

/// A control method can take up to 7 arguments, each of which is an `AmlValue`.
#[derive(Clone, Default, Debug)]
pub struct Args(pub [Option<AmlValue>; 7]);

impl Args {
    pub const EMPTY: Self = Self([None, None, None, None, None, None, None]);

    pub fn from_list(list: Vec<AmlValue>) -> Result<Args, AmlError> {
        if list.len() > 7 {
            return Err(AmlError::TooManyArgs);
        }

        let mut args: Vec<Option<AmlValue>> = list.into_iter().map(Option::Some).collect();
        args.extend(core::iter::repeat(None).take(7 - args.len()));
        Ok(Args(args.try_into().unwrap()))
    }

    pub fn arg(&self, arg: ArgNum) -> Result<&AmlValue, AmlError> {
        if arg > 6 {
            return Err(AmlError::InvalidArgAccess(arg));
        }

        self.0[arg as usize].as_ref().ok_or(AmlError::InvalidArgAccess(arg))
    }

    pub fn store_arg(&mut self, arg: ArgNum, value: AmlValue) -> Result<(), AmlError> {
        if arg > 6 {
            return Err(AmlError::InvalidArgAccess(arg));
        }

        self.0[arg as usize] = Some(value);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;
    use core::cmp::Ordering;

    #[test]
    fn test_object_cmp() {
        let mut context = make_test_context();

        assert_eq!(AmlValue::Integer(76).cmp(AmlValue::Integer(89), &mut context), Ok(Ordering::Less));
        assert_eq!(AmlValue::Integer(11).cmp(AmlValue::Integer(11), &mut context), Ok(Ordering::Equal));
        assert_eq!(AmlValue::Integer(8362836690).cmp(AmlValue::Integer(1), &mut context), Ok(Ordering::Greater));

        // TODO: test the other combinations too, as well as conversions to the correct types for the second operand
    }
}
