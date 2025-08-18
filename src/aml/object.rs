use crate::aml::{AmlError, Handle, Operation, op_region::OpRegion};
use alloc::{borrow::Cow, string::String, sync::Arc, vec::Vec};
use bit_field::BitField;
use core::{cell::UnsafeCell, fmt, ops, sync::atomic::AtomicU64};

type NativeMethod = dyn Fn(&[WrappedObject]) -> Result<WrappedObject, AmlError>;

#[derive(Clone)]
pub enum Object {
    Uninitialized,
    Buffer(Vec<u8>),
    BufferField { buffer: WrappedObject, offset: usize, length: usize },
    Device,
    Event(Arc<AtomicU64>),
    FieldUnit(FieldUnit),
    Integer(u64),
    Method { code: Vec<u8>, flags: MethodFlags },
    NativeMethod { f: Arc<NativeMethod>, flags: MethodFlags },
    Mutex { mutex: Handle, sync_level: u8 },
    Reference { kind: ReferenceKind, inner: WrappedObject },
    OpRegion(OpRegion),
    Package(Vec<WrappedObject>),
    PowerResource { system_level: u8, resource_order: u16 },
    Processor { proc_id: u8, pblk_address: u32, pblk_length: u8 },
    RawDataBuffer,
    String(String),
    ThermalZone,
    Debug,
}

impl Object {
    pub fn native_method<F>(num_args: u8, f: F) -> Object
    where
        F: Fn(&[WrappedObject]) -> Result<WrappedObject, AmlError> + 'static,
    {
        let mut flags = 0;
        flags.set_bits(0..3, num_args);
        Object::NativeMethod { f: Arc::new(f), flags: MethodFlags(flags) }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Uninitialized => write!(f, "[Uninitialized]"),
            Object::Buffer(bytes) => write!(f, "Buffer({bytes:x?})"),
            Object::BufferField { offset, length, .. } => {
                write!(f, "BufferField {{ offset: {offset}, length: {length} }}")
            }
            Object::Device => write!(f, "Device"),
            Object::Event(counter) => write!(f, "Event({counter:?})"),
            // TODO: include fields
            Object::FieldUnit(_) => write!(f, "FieldUnit"),
            Object::Integer(value) => write!(f, "Integer({value})"),
            // TODO: decode flags here
            Object::Method { .. } => write!(f, "Method"),
            Object::NativeMethod { .. } => write!(f, "NativeMethod"),
            Object::Mutex { .. } => write!(f, "Mutex"),
            Object::Reference { kind, inner } => write!(f, "Reference({:?} -> {})", kind, **inner),
            Object::OpRegion(region) => write!(f, "{region:?}"),
            Object::Package(elements) => {
                write!(f, "Package {{ ")?;
                for (i, element) in elements.iter().enumerate() {
                    if i == elements.len() - 1 {
                        write!(f, "{}", **element)?;
                    } else {
                        write!(f, "{}, ", **element)?;
                    }
                }
                write!(f, " }}")?;
                Ok(())
            }
            // TODO: include fields
            Object::PowerResource { .. } => write!(f, "PowerResource"),
            // TODO: include fields
            Object::Processor { .. } => write!(f, "Processor"),
            Object::RawDataBuffer => write!(f, "RawDataBuffer"),
            Object::String(value) => write!(f, "String({value:?})"),
            Object::ThermalZone => write!(f, "ThermalZone"),
            Object::Debug => write!(f, "Debug"),
        }
    }
}

/// `ObjectToken` is used to mediate mutable access to objects from a [`WrappedObject`]. It must be
/// acquired by locking the single token provided by [`super::Interpreter`].
#[non_exhaustive]
pub struct ObjectToken {
    _dont_construct_me: (),
}

impl ObjectToken {
    /// Create an [`ObjectToken`]. This should **only** be done **once** by the main interpreter,
    /// as contructing your own token allows invalid mutable access to objects.
    pub(super) unsafe fn create_interpreter_token() -> ObjectToken {
        ObjectToken { _dont_construct_me: () }
    }
}

#[derive(Clone, Debug)]
pub struct WrappedObject(Arc<UnsafeCell<Object>>);

impl WrappedObject {
    pub fn new(object: Object) -> WrappedObject {
        #[allow(clippy::arc_with_non_send_sync)]
        WrappedObject(Arc::new(UnsafeCell::new(object)))
    }

    /// Gain a mutable reference to an [`Object`] from this [`WrappedObject`].
    ///
    /// # Safety
    /// This requires an [`ObjectToken`] which is protected by a lock on [`super::Interpreter`],
    /// which prevents mutable access to objects from multiple contexts. It does not, however,
    /// prevent the same object, referenced from multiple [`WrappedObject`]s, having multiple
    /// mutable (and therefore aliasing) references being made to it, and therefore care must be
    /// taken in the interpreter to prevent this.
    pub unsafe fn gain_mut<'r, 'a, 't>(&'a self, _token: &'t ObjectToken) -> &'r mut Object
    where
        't: 'r,
        'a: 'r,
    {
        unsafe { &mut *(self.0.get()) }
    }

    pub fn unwrap_reference(self) -> WrappedObject {
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
    pub fn unwrap_transparent_reference(self) -> WrappedObject {
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

impl ops::Deref for WrappedObject {
    type Target = Object;

    fn deref(&self) -> &Self::Target {
        /*
         * SAFETY: elided lifetime ensures reference cannot outlive at least one reference-counted
         * instance of the object. `WrappedObject::gain_mut` is unsafe, and so it is the user's
         * responsibility to ensure shared references from `Deref` do not co-exist with an
         * exclusive reference.
         */
        unsafe { &*self.0.get() }
    }
}

impl Object {
    pub fn wrap(self) -> WrappedObject {
        WrappedObject::new(self)
    }

    pub fn as_integer(&self) -> Result<u64, AmlError> {
        if let Object::Integer(value) = self {
            Ok(*value)
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Integer, got: self.typ() })
        }
    }

    pub fn as_string(&self) -> Result<Cow<'_, str>, AmlError> {
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

    pub fn write_buffer_field(&mut self, value: &[u8], token: &ObjectToken) -> Result<(), AmlError> {
        // TODO: bounds check the buffer first to avoid panicking
        if let Self::BufferField { buffer, offset, length } = self {
            let buffer = match unsafe { buffer.gain_mut(token) } {
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
            Object::Event(_) => ObjectType::Event,
            Object::FieldUnit(_) => ObjectType::FieldUnit,
            Object::Integer(_) => ObjectType::Integer,
            Object::Method { .. } => ObjectType::Method,
            Object::NativeMethod { .. } => ObjectType::Method,
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
    Normal { region: WrappedObject },
    Bank { region: WrappedObject, bank: WrappedObject, bank_value: u64 },
    Index { index: WrappedObject, data: WrappedObject },
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
        let mut dst_mask: u16 = if length < 8 { ((1 << length) - 1) as u16 } else { 0xff_u16 } << dst_shift;
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
