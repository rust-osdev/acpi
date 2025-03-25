use crate::aml::{AmlError, Handle, Operation, op_region::OpRegion};
use alloc::{borrow::Cow, string::String, sync::Arc, vec::Vec};
use bit_field::BitField;

#[derive(Clone, Debug)]
pub enum Object {
    Uninitialized,
    Buffer(Vec<u8>),
    BufferField { buffer: Arc<Object>, offset: usize, length: usize },
    Device,
    Event,
    FieldUnit(FieldUnit),
    Integer(u64),
    Method { code: Vec<u8>, flags: MethodFlags },
    Mutex { mutex: Handle, sync_level: u8 },
    Reference { kind: ReferenceKind, inner: Arc<Object> },
    OpRegion(OpRegion),
    Package(Vec<Arc<Object>>),
    PowerResource { system_level: u8, resource_order: u16 },
    Processor { proc_id: u8, pblk_address: u32, pblk_length: u8 },
    RawDataBuffer,
    String(String),
    ThermalZone,
    Debug,
}

impl Object {
    /*
     * TODO XXX: this is a horrendous hack to emulate a clever locking solution for dynamically
     * validating borrow checking for objects at A Later Date. It is trivially easy to produce
     * undefined behaviour with this (and might be UB intrinsically).
     *
     * Options are:
     *   - Put something like an AtomicRefCell around every single object. This is too slow I
     *     think.
     *   - Utilise a global lock on the namespace that gives us some sort of token we can then
     *     magic up mutable references to objects through. Safety is ensured at type-level.
     *   - Something else cleverer.
     */
    pub fn gain_mut(&self) -> &mut Self {
        #[allow(invalid_reference_casting)]
        unsafe {
            &mut *(self as *const Self as *mut Self)
        }
    }

    pub fn as_integer(&self) -> Result<u64, AmlError> {
        if let Object::Integer(value) = self {
            Ok(*value)
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Integer, got: self.typ() })
        }
    }

    pub fn as_string(&self) -> Result<Cow<str>, AmlError> {
        if let Object::String(value) = self {
            Ok(Cow::from(value))
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::String, got: self.typ() })
        }
    }

    pub fn as_buffer(&self) -> Result<&[u8], AmlError> {
        if let Object::Buffer(bytes) = self {
            Ok(bytes)
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Buffer, got: self.typ() })
        }
    }

    pub fn to_integer(&self, allowed_bytes: usize) -> Result<u64, AmlError> {
        match self {
            Object::Integer(value) => Ok(*value),
            Object::Buffer(value) => {
                let length = usize::min(value.len(), allowed_bytes);
                let mut bytes = [0u8; 8];
                bytes[0..length].copy_from_slice(&value[0..length]);
                Ok(u64::from_le_bytes(bytes))
            }
            // TODO: how should we handle invalid inputs? What does NT do here?
            Object::String(value) => Ok(value.parse::<u64>().unwrap_or(0)),
            _ => Ok(0),
        }
    }

    pub fn to_buffer(&self, allowed_bytes: usize) -> Result<Vec<u8>, AmlError> {
        match self {
            Object::Buffer(bytes) => Ok(bytes.clone()),
            Object::Integer(value) => match allowed_bytes {
                4 => Ok((*value as u32).to_le_bytes().to_vec()),
                8 => Ok(value.to_le_bytes().to_vec()),
                _ => panic!(),
            },
            Object::String(value) => Ok(value.as_bytes().to_vec()),
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::ConvertToBuffer, typ: self.typ() }),
        }
    }

    pub fn read_buffer_field(&self, dst: &mut [u8]) -> Result<(), AmlError> {
        if let Self::BufferField { buffer, offset, length } = self {
            let buffer = match **buffer {
                Object::Buffer(ref buffer) => buffer.as_slice(),
                Object::String(ref string) => string.as_bytes(),
                _ => panic!(),
            };
            // TODO: bounds check the buffer first to avoid panicking
            copy_bits(buffer, *offset, dst, 0, *length);
            Ok(())
        } else {
            Err(AmlError::InvalidOperationOnObject { op: Operation::ReadBufferField, typ: self.typ() })
        }
    }

    pub fn write_buffer_field(&mut self, value: &[u8]) -> Result<(), AmlError> {
        // TODO: bounds check the buffer first to avoid panicking
        if let Self::BufferField { buffer, offset, length } = self {
            let buffer = match buffer.gain_mut() {
                Object::Buffer(buffer) => buffer.as_mut_slice(),
                // XXX: this unfortunately requires us to trust AML to keep the string as valid
                // UTF8... maybe there is a better way?
                Object::String(string) => unsafe { string.as_bytes_mut() },
                _ => panic!(),
            };
            copy_bits(value, 0, buffer, *offset, *length);
            Ok(())
        } else {
            Err(AmlError::InvalidOperationOnObject { op: Operation::WriteBufferField, typ: self.typ() })
        }
    }

    /// Returns the `ObjectType` of this object. Returns the type of the referenced object in the
    /// case of `Object::Reference`.
    pub fn typ(&self) -> ObjectType {
        match self {
            Object::Uninitialized => ObjectType::Uninitialized,
            Object::Buffer(_) => ObjectType::Buffer,
            Object::BufferField { .. } => ObjectType::BufferField,
            Object::Device => ObjectType::Device,
            Object::Event => ObjectType::Event,
            Object::FieldUnit(_) => ObjectType::FieldUnit,
            Object::Integer(_) => ObjectType::Integer,
            Object::Method { .. } => ObjectType::Method,
            Object::Mutex { .. } => ObjectType::Mutex,
            Object::Reference { inner, .. } => inner.typ(),
            Object::OpRegion(_) => ObjectType::OpRegion,
            Object::Package(_) => ObjectType::Package,
            Object::PowerResource { .. } => ObjectType::PowerResource,
            Object::Processor { .. } => ObjectType::Processor,
            Object::RawDataBuffer => ObjectType::RawDataBuffer,
            Object::String(_) => ObjectType::String,
            Object::ThermalZone => ObjectType::ThermalZone,
            Object::Debug => ObjectType::Debug,
        }
    }

    pub fn unwrap_reference(self: Arc<Object>) -> Arc<Object> {
        let mut object = self;
        loop {
            if let Object::Reference { ref inner, .. } = *object {
                object = inner.clone();
            } else {
                return object.clone();
            }
        }
    }

    /// Unwraps 'transparent' references (e.g. locals, arguments, and internal usage of reference-type objects), but maintain 'real'
    /// references deliberately created by AML.
    pub fn unwrap_transparent_reference(self: Arc<Self>) -> Arc<Object> {
        let mut object = self;
        loop {
            // TODO: what should this do with unresolved namestrings? It would need namespace
            // access to resolve them (and then this would probs have to move to a method on
            // `Interpreter`)?
            if let Object::Reference { kind, ref inner } = *object
                && kind == ReferenceKind::LocalOrArg
            {
                object = inner.clone();
            } else {
                return object.clone();
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct FieldUnit {
    pub kind: FieldUnitKind,
    pub flags: FieldFlags,
    pub bit_index: usize,
    pub bit_length: usize,
}

#[derive(Clone, Debug)]
pub enum FieldUnitKind {
    Normal { region: Arc<Object> },
    Bank { region: Arc<Object>, bank: Arc<Object>, bank_value: u64 },
    Index { index: Arc<Object>, data: Arc<Object> },
}

#[derive(Clone, Copy, Debug)]
pub struct FieldFlags(pub u8);

#[derive(Clone, Copy, Debug)]
pub enum FieldAccessType {
    Any,
    Byte,
    Word,
    DWord,
    QWord,
    Buffer,
}

#[derive(Clone, Copy, Debug)]
pub enum FieldUpdateRule {
    Preserve,
    WriteAsOnes,
    WriteAsZeros,
}

impl FieldFlags {
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

    pub fn access_type_bytes(&self) -> Result<usize, AmlError> {
        match self.access_type()? {
            FieldAccessType::Any => {
                // TODO: given more info about the field, we might be able to make a more efficient
                // read, since all are valid in this case
                Ok(1)
            }
            FieldAccessType::Byte | FieldAccessType::Buffer => Ok(1),
            FieldAccessType::Word => Ok(2),
            FieldAccessType::DWord => Ok(4),
            FieldAccessType::QWord => Ok(8),
        }
    }

    pub fn lock_rule(&self) -> bool {
        self.0.get_bit(4)
    }

    pub fn update_rule(&self) -> FieldUpdateRule {
        match self.0.get_bits(5..7) {
            0 => FieldUpdateRule::Preserve,
            1 => FieldUpdateRule::WriteAsOnes,
            2 => FieldUpdateRule::WriteAsZeros,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MethodFlags(pub u8);

impl MethodFlags {
    pub fn arg_count(&self) -> usize {
        self.0.get_bits(0..3) as usize
    }

    pub fn serialize(&self) -> bool {
        self.0.get_bit(3)
    }

    pub fn sync_level(&self) -> u8 {
        self.0.get_bits(4..8)
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ReferenceKind {
    RefOf,
    LocalOrArg,
    Unresolved,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum ObjectType {
    Uninitialized,
    Buffer,
    BufferField,
    Device,
    Event,
    FieldUnit,
    Integer,
    Method,
    Mutex,
    Reference,
    OpRegion,
    Package,
    PowerResource,
    Processor,
    RawDataBuffer,
    String,
    ThermalZone,
    Debug,
}

/// Helper type for decoding the result of `_STA` objects.
pub struct DeviceStatus(pub u64);

impl DeviceStatus {
    pub fn present(&self) -> bool {
        self.0.get_bit(0)
    }

    pub fn enabled(&self) -> bool {
        self.0.get_bit(1)
    }

    pub fn show_in_ui(&self) -> bool {
        self.0.get_bit(2)
    }

    pub fn functioning(&self) -> bool {
        self.0.get_bit(3)
    }

    /// This flag is only used for Battery devices (PNP0C0A), and indicates if the battery is
    /// present.
    pub fn battery_present(&self) -> bool {
        self.0.get_bit(4)
    }
}

/// Copy an arbitrary bit range of `src` to an arbitrary bit range of `dst`. This is used for
/// buffer fields. Data is zero-extended if `src` does not cover `length` bits, matching the
/// expected behaviour for buffer fields.
pub(crate) fn copy_bits(
    src: &[u8],
    mut src_index: usize,
    dst: &mut [u8],
    mut dst_index: usize,
    mut length: usize,
) {
    while length > 0 {
        let src_shift = src_index & 7;
        let mut src_bits = src.get(src_index / 8).unwrap_or(&0x00) >> src_shift;
        if src_shift > 0 && length > (8 - src_shift) {
            src_bits |= src.get(src_index / 8 + 1).unwrap_or(&0x00) << (8 - src_shift);
        }

        if length < 8 {
            src_bits &= (1 << length) - 1;
        }

        let dst_shift = dst_index & 7;
        let mut dst_mask: u16 = if length < 8 { ((1 << length) - 1) as u16 } else { 0xff as u16 } << dst_shift;
        dst[dst_index / 8] =
            (dst[dst_index / 8] & !(dst_mask as u8)) | ((src_bits << dst_shift) & (dst_mask as u8));

        if dst_shift > 0 && length > (8 - dst_shift) {
            dst_mask >>= 8;
            dst[dst_index / 8 + 1] &= !(dst_mask as u8);
            dst[dst_index / 8 + 1] |= (src_bits >> (8 - dst_shift)) & (dst_mask as u8);
        }

        if length < 8 {
            length = 0;
        } else {
            length -= 8;
            src_index += 8;
            dst_index += 8;
        }
    }
}

#[inline]
pub(crate) fn align_down(value: usize, align: usize) -> usize {
    assert!(align == 0 || align.is_power_of_two());

    if align == 0 {
        value
    } else {
        /*
         * Alignment must be a power of two.
         *
         * E.g.
         * align       =   0b00001000
         * align-1     =   0b00000111
         * !(align-1)  =   0b11111000
         * ^^^ Masks the value to the one below it with the correct align
         */
        value & !(align - 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy_bits() {
        let src = [0b1011_1111, 0b1111_0111, 0b1111_1111, 0b1111_1111, 0b1111_1111];
        let mut dst = [0b1110_0001, 0, 0, 0, 0];

        copy_bits(&src, 0, &mut dst, 2, 15);
        assert_eq!(dst, [0b1111_1101, 0b1101_1110, 0b0000_0001, 0b0000_0000, 0b0000_0000]);
    }
}
