use crate::aml::{AmlError, Handle, IntegerSize, Operation, op_region::OpRegion};
use alloc::{alloc::Global, sync::Arc, vec::Vec};
use bit_field::BitField;
use core::{alloc::Allocator, cell::UnsafeCell, cmp::Ordering, fmt, ops, sync::atomic::AtomicU64};

// nightly's `String` is not allocator-parameterized (no
// `String<A>` type, no `String::new_in`). `AmlString<A>` is a thin newtype
// wrapping `Vec<u8, A>` with a UTF-8 invariant, providing the subset of
// `String` operations the AML interpreter actually needs. Construction sites
// always start from validated `&str` literals or copy from existing strings,
// so the UTF-8 invariant is straightforward to maintain.
pub struct AmlString<A: Allocator + Clone = Global>(Vec<u8, A>);

impl<A: Allocator + Clone> AmlString<A> {
    pub fn new_in(alloc: A) -> Self {
        Self(Vec::new_in(alloc))
    }

    pub fn from_str_in(s: &str, alloc: A) -> Self {
        let mut v = Vec::with_capacity_in(s.len(), alloc);
        v.extend_from_slice(s.as_bytes());
        Self(v)
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: every construction path either takes a `&str` (already valid
        // UTF-8) or pushes via `push_str`/`push(char)` (also guaranteed valid).
        // `as_bytes_mut` is the only way to break this and is `unsafe`.
        unsafe { core::str::from_utf8_unchecked(&self.0) }
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    /// # Safety
    /// Caller must preserve UTF-8 validity in the returned slice. Breaking
    /// this invariant makes [`as_str`] unsound on subsequent reads.
    #[inline]
    pub unsafe fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    pub fn push_str(&mut self, s: &str) {
        self.0.extend_from_slice(s.as_bytes());
    }

    pub fn push(&mut self, c: char) {
        let mut buf = [0u8; 4];
        let s = c.encode_utf8(&mut buf);
        self.0.extend_from_slice(s.as_bytes());
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn parse<F: core::str::FromStr>(&self) -> Result<F, F::Err> {
        self.as_str().parse::<F>()
    }
}

impl<A: Allocator + Clone> Clone for AmlString<A> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

// Manual PartialEq impls - derive would bound `A: PartialEq`, but Vec's
// PartialEq impl works for any allocator (compares element-wise).
impl<A: Allocator + Clone, A2: Allocator + Clone> PartialEq<AmlString<A2>> for AmlString<A> {
    fn eq(&self, other: &AmlString<A2>) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl<A: Allocator + Clone> Eq for AmlString<A> {}

impl<A: Allocator + Clone> fmt::Display for AmlString<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl<A: Allocator + Clone> fmt::Debug for AmlString<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

// Lets `write!(amlstring, "...")` work without going through Global.
impl<A: Allocator + Clone> fmt::Write for AmlString<A> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

// NativeMethod is parameterized over `A` because the closure
// produces and consumes WrappedObject<A>. The 'static bound stays on the
// constructor (`Object::native_method` below).
type NativeMethod<A> = dyn Fn(&[WrappedObject<A>]) -> Result<WrappedObject<A>, AmlError<A>>;

#[derive(Clone)]
pub enum Object<A: Allocator + Clone = Global> {
    Uninitialized,
    Buffer(Vec<u8, A>),
    BufferField { buffer: WrappedObject<A>, offset: usize, length: usize },
    Device,
    // Event's Arc is also allocator-parameterized. We could
    // keep this Arc<AtomicU64, Global> as a "small exception" for shared
    // synchronization primitives, but consistency wins - every allocation
    // goes through the same arena.
    Event(Arc<AtomicU64, A>),
    FieldUnit(FieldUnit<A>),
    Integer(u64),
    Method { code: Vec<u8, A>, flags: MethodFlags },
    NativeMethod { f: Arc<NativeMethod<A>, A>, flags: MethodFlags },
    Mutex { mutex: Handle, sync_level: u8 },
    Reference { kind: ReferenceKind, inner: WrappedObject<A> },
    OpRegion(OpRegion<A>),
    Package(Vec<WrappedObject<A>, A>),
    PowerResource { system_level: u8, resource_order: u16 },
    Processor { proc_id: u8, pblk_address: u32, pblk_length: u8 },
    RawDataBuffer,
    String(AmlString<A>),
    ThermalZone,
    Debug,
}

impl<A: Allocator + Clone> Object<A> {
    pub fn native_method<F>(num_args: u8, f: F, alloc: A) -> Object<A>
    where
        A: 'static,
        F: Fn(&[WrappedObject<A>]) -> Result<WrappedObject<A>, AmlError<A>> + 'static,
    {
        let mut flags = 0;
        flags.set_bits(0..3, num_args);
        // Arc<F, A> coerces to Arc<dyn Fn..., A> at the field assignment.
        Object::NativeMethod { f: Arc::new_in(f, alloc), flags: MethodFlags(flags) }
    }
}

impl<A: Allocator + Clone> fmt::Display for Object<A> {
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

#[derive(Clone)]
pub struct WrappedObject<A: Allocator + Clone = Global>(Arc<UnsafeCell<Object<A>>, A>);

// Manual Debug impl - derive auto-bounds `A: Debug`.
impl<A: Allocator + Clone> fmt::Debug for WrappedObject<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("WrappedObject").finish_non_exhaustive()
    }
}

impl<A: Allocator + Clone> WrappedObject<A> {
    pub fn new(object: Object<A>, alloc: A) -> WrappedObject<A> {
        #[allow(clippy::arc_with_non_send_sync)]
        WrappedObject(Arc::new_in(UnsafeCell::new(object), alloc))
    }

    /// Gain a mutable reference to an [`Object`] from this [`WrappedObject`].
    ///
    /// # Safety
    /// This requires an [`ObjectToken`] which is protected by a lock on [`super::Interpreter`],
    /// which prevents mutable access to objects from multiple contexts. It does not, however,
    /// prevent the same object, referenced from multiple [`WrappedObject`]s, having multiple
    /// mutable (and therefore aliasing) references being made to it, and therefore care must be
    /// taken in the interpreter to prevent this.
    pub unsafe fn gain_mut<'r, 'a, 't>(&'a self, _token: &'t ObjectToken) -> &'r mut Object<A>
    where
        't: 'r,
        'a: 'r,
    {
        unsafe { &mut *(self.0.get()) }
    }

    pub fn unwrap_reference(self) -> WrappedObject<A> {
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
    pub fn unwrap_transparent_reference(self) -> WrappedObject<A> {
        let mut object = self;
        loop {
            if let Object::Reference { kind, ref inner } = *object
                && (kind == ReferenceKind::Local || kind == ReferenceKind::Arg || kind == ReferenceKind::Named)
            {
                object = inner.clone();
            } else {
                return object.clone();
            }
        }
    }
}

impl<A: Allocator + Clone> ops::Deref for WrappedObject<A> {
    type Target = Object<A>;

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

impl<A: Allocator + Clone> fmt::Display for WrappedObject<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Wrapped({})", unsafe { &*self.0.get() })
    }
}

impl<A: Allocator + Clone> Object<A> {
    pub fn wrap(self, alloc: A) -> WrappedObject<A> {
        WrappedObject::new(self, alloc)
    }

    /// Unwraps an integer object. Errors if not already an integer.
    ///
    /// For casting to integer, use [`Object::to_integer`] instead.
    pub fn as_integer(&self) -> Result<u64, AmlError<A>> {
        if let Object::Integer(value) = self {
            Ok(*value)
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Integer, got: self.typ() })
        }
    }

    /// Unwraps a string object as a borrowed string slice.
    pub fn as_string(&self) -> Result<&str, AmlError<A>> {
        if let Object::String(value) = self {
            Ok(value.as_str())
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::String, got: self.typ() })
        }
    }

    pub fn as_buffer(&self) -> Result<&[u8], AmlError<A>> {
        if let Object::Buffer(bytes) = self {
            Ok(bytes)
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Buffer, got: self.typ() })
        }
    }

    /// Converts the object to an integer. Used for both implicit and explicit conversions.
    ///
    /// To avoid the cast, use [`Object::as_integer`] instead.
    pub fn to_integer(&self, integer_size: IntegerSize, alloc: A) -> Result<u64, AmlError<A>> {
        match self {
            Object::Integer(value) => Ok(*value),
            Object::Buffer(bytes) => {
                /*
                 * The spec says this should respect the revision of the current definition block.
                 * Apparently, the NT interpreter always uses the first 8 bytes of the buffer.
                 */
                let length = usize::min(bytes.len(), integer_size as usize);
                let mut to_interpret = [0u8; 8];
                to_interpret[0..length].copy_from_slice(&bytes[0..length]);
                Ok(u64::from_le_bytes(to_interpret))
            }
            Object::String(value) => {
                /*
                 * This is about the same level of effort as ACPICA puts in. The uACPI test suite
                 * has tests that this fails - namely because of support for octal, signs, strings
                 * that won't fit in a `u64` etc. We probably need to write a more robust parser
                 * 'real' parser to handle those cases.
                 */
                let value = value.as_str().trim();
                let (value, radix): (&str, u32) =
                    if let Some(value) = value.strip_prefix("0x").or_else(|| value.strip_prefix("0X")) {
                        (value.split(|c: char| !c.is_ascii_hexdigit()).next().unwrap_or(""), 16)
                    } else {
                        (value.split(|c: char| !c.is_ascii_digit()).next().unwrap_or(""), 10)
                    };
                match value.len() {
                    0 => Ok(0),
                    _ => Ok(u64::from_str_radix(value, radix).map_err(|_| {
                        AmlError::InvalidOperationOnObject { op: Operation::ToInteger, typ: ObjectType::String }
                    })?),
                }
            }
            Object::BufferField { .. } => {
                self.read_buffer_field(integer_size, alloc.clone())?.to_integer(integer_size, alloc)
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::ToInteger, typ: self.typ() })?,
        }
    }

    pub fn to_buffer(&self, integer_size: IntegerSize, alloc: A) -> Result<Vec<u8, A>, AmlError<A>> {
        match self {
            Object::Buffer(bytes) => Ok(bytes.clone()),
            Object::Integer(value) => match integer_size {
                IntegerSize::FourBytes => {
                    let bytes = (*value as u32).to_le_bytes();
                    let mut out = Vec::with_capacity_in(bytes.len(), alloc);
                    out.extend_from_slice(&bytes);
                    Ok(out)
                }
                IntegerSize::EightBytes => {
                    let bytes = value.to_le_bytes();
                    let mut out = Vec::with_capacity_in(bytes.len(), alloc);
                    out.extend_from_slice(&bytes);
                    Ok(out)
                }
            },
            Object::String(value) => {
                let src = value.as_bytes();
                let mut out = Vec::with_capacity_in(src.len(), alloc);
                out.extend_from_slice(src);
                Ok(out)
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::ConvertToBuffer, typ: self.typ() }),
        }
    }

    pub fn read_buffer_field(&self, integer_size: IntegerSize, alloc: A) -> Result<Object<A>, AmlError<A>> {
        if let Self::BufferField { buffer, offset, length } = self {
            let buffer = match **buffer {
                Object::Buffer(ref buffer) => buffer.as_slice(),
                Object::String(ref string) => string.as_bytes(),
                _ => panic!(),
            };
            if *length <= integer_size as usize {
                let mut dst = [0u8; 8];
                copy_bits(buffer, *offset, &mut dst, 0, *length);
                Ok(Object::Integer(u64::from_le_bytes(dst)))
            } else {
                let size = length.div_ceil(8);
                let mut dst = Vec::with_capacity_in(size, alloc);
                dst.resize(size, 0u8);
                copy_bits(buffer, *offset, &mut dst, 0, *length);
                Ok(Object::Buffer(dst))
            }
        } else {
            Err(AmlError::InvalidOperationOnObject { op: Operation::ReadBufferField, typ: self.typ() })
        }
    }

    pub fn write_buffer_field(&mut self, value: &[u8], token: &ObjectToken) -> Result<(), AmlError<A>> {
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

    /// Replace this object's contents with that of a `new` object, applying implicit casting rules
    /// as needed. This follows the NT interpreter's creative interpretation of implicit casts, which is
    /// effectively a byte-wise transmutation.
    pub fn replace_with_implicit_casting(&mut self, new: Object<A>) -> Result<(), AmlError<A>> {
        // Extract a &[u8] view of `new` without taking ownership (so we can keep
        // its allocator A live for the lifetime of the borrow).
        let new_bytes: &[u8] = match new {
            Object::Integer(value) => {
                // Convert to a fixed-size byte buffer first; the borrow below
                // must outlive the match arm, so we stash it in a local.
                let bytes = value.to_le_bytes();
                return apply_cast_bytes_owned(self, &bytes);
            }
            Object::String(ref value) => value.as_bytes(),
            Object::Buffer(ref value) => value.as_slice(),
            _ => return Err(AmlError::InvalidImplicitCast { from: self.typ(), to: new.typ() }),
        };
        apply_cast_bytes(self, new_bytes)?;
        return Ok(());

        fn apply_cast_bytes_owned<A2: Allocator + Clone>(
            target: &mut Object<A2>,
            bytes: &[u8],
        ) -> Result<(), AmlError<A2>> {
            apply_cast_bytes(target, bytes)
        }

        fn apply_cast_bytes<A2: Allocator + Clone>(
            target: &mut Object<A2>,
            new_bytes: &[u8],
        ) -> Result<(), AmlError<A2>> {
            match target {
                Object::Integer(value) => {
                    let bytes_to_copy = core::cmp::min(new_bytes.len(), 8);
                    let mut bytes = [0u8; 8];
                    bytes[0..bytes_to_copy].copy_from_slice(&new_bytes[0..bytes_to_copy]);
                    *value = u64::from_le_bytes(bytes);
                }
                Object::String(value) => {
                    value.clear();
                    push_utf8_lossy_until_nul(value, new_bytes);
                }
                Object::Buffer(value) => {
                    value.clear();
                    value.extend_from_slice(new_bytes);
                }
                _ => return Err(AmlError::InvalidImplicitCast { from: target.typ(), to: ObjectType::Buffer }),
            }
            Ok(())
        }

        fn push_utf8_lossy_until_nul<A2: Allocator + Clone>(target: &mut AmlString<A2>, bytes: &[u8]) {
            let mut remaining = bytes.split(|byte| *byte == b'\0').next().unwrap_or_default();

            loop {
                match core::str::from_utf8(remaining) {
                    Ok(valid) => {
                        target.push_str(valid);
                        return;
                    }
                    Err(error) => {
                        let valid_up_to = error.valid_up_to();
                        if valid_up_to > 0 {
                            // SAFETY: `valid_up_to` is the UTF-8-valid prefix reported by `from_utf8`.
                            target.push_str(unsafe { core::str::from_utf8_unchecked(&remaining[..valid_up_to]) });
                        }

                        target.push(char::REPLACEMENT_CHARACTER);
                        let Some(error_len) = error.error_len() else {
                            return;
                        };
                        remaining = &remaining[(valid_up_to + error_len)..];
                    }
                }
            }
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
            // Object::Reference { inner, .. } => inner.typ(),
            Object::Reference { .. } => ObjectType::Reference, // TODO: maybe this should
            // differentiate internal/real
            // references?
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

    /// Calculate the ordering of two objects using the AML rules
    ///
    /// This function is not intended to be used for `impl PartialOrd` because we don't want to tie
    /// the meaning of `object_a.cmp(object_b)` to those AML rules - we may want more flexibility.
    pub fn aml_cmp(&self, other: &Object<A>) -> Result<Ordering, AmlError<A>> {
        match (self, &other) {
            (Object::Integer(a), Object::Integer(b)) => Ok(a.cmp(b)),
            (Object::String(a), Object::String(b)) => Ok(a.as_str().cmp(b.as_str())),
            (Object::Buffer(a), Object::Buffer(b)) => {
                let size_cmp = a.len().cmp(&b.len());
                if size_cmp != Ordering::Equal {
                    return Ok(size_cmp);
                }
                Ok(a.cmp(b))
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::LogicalOp, typ: self.typ() }),
        }
    }
}

#[derive(Clone)]
pub struct FieldUnit<A: Allocator + Clone = Global> {
    pub kind: FieldUnitKind<A>,
    pub flags: FieldFlags,
    pub bit_index: usize,
    pub bit_length: usize,
}

impl<A: Allocator + Clone> fmt::Debug for FieldUnit<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FieldUnit")
            .field("flags", &self.flags)
            .field("bit_length", &self.bit_length)
            .finish_non_exhaustive()
    }
}

#[derive(Clone)]
pub enum FieldUnitKind<A: Allocator + Clone = Global> {
    Normal { region: WrappedObject<A> },
    Bank { region: WrappedObject<A>, bank: WrappedObject<A>, bank_value: u64 },
    Index { index: WrappedObject<A>, data: WrappedObject<A> },
}

impl<A: Allocator + Clone> fmt::Debug for FieldUnitKind<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Normal { .. } => f.write_str("Normal { .. }"),
            Self::Bank { .. } => f.write_str("Bank { .. }"),
            Self::Index { .. } => f.write_str("Index { .. }"),
        }
    }
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
    pub fn access_type<A: Allocator + Clone>(&self) -> Result<FieldAccessType, AmlError<A>> {
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

    pub fn access_type_bytes<A: Allocator + Clone>(&self) -> Result<usize, AmlError<A>> {
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
    Named,
    RefOf,
    Local,
    Arg,
    Index,
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
    use alloc::alloc::Global;

    #[test]
    fn test_copy_bits() {
        let src = [0b1011_1111, 0b1111_0111, 0b1111_1111, 0b1111_1111, 0b1111_1111];
        let mut dst = [0b1110_0001, 0, 0, 0, 0];

        copy_bits(&src, 0, &mut dst, 2, 15);
        assert_eq!(dst, [0b1111_1101, 0b1101_1110, 0b0000_0001, 0b0000_0000, 0b0000_0000]);
    }

    #[test]
    fn buffer_to_integer() {
        let buffer = Object::Buffer(Vec::from([0xab, 0xcd, 0xef, 0x01, 0xff]));
        assert_eq!(buffer.to_integer(IntegerSize::FourBytes, Global).unwrap(), 0x01efcdab);
    }
    #[test]
    fn buffer_field_to_integer() {
        const BUFFER: [u8; 5] = [0xffu8; 5];
        let buffer = Object::Buffer(Vec::from(BUFFER)).wrap(Global);
        let buffer_field = Object::BufferField { buffer, offset: 5, length: 9 };

        assert_eq!(buffer_field.to_integer(IntegerSize::FourBytes, Global).unwrap(), 0x1ff);
    }

    #[test]
    fn buffer_field_to_4_byte_integer() {
        // The ones in this buffer are strategically chosen to not make it to the final integer.
        const BUFFER: [u8; 5] = [0x0f, 0x00, 0x00, 0x00, 0xf0];
        let buffer = Object::Buffer(Vec::from(BUFFER)).wrap(Global);
        let buffer_field = Object::BufferField {
            buffer,
            offset: 4,
            length: 36, // This should be truncated to 32 bits in the conversion
        };

        assert_eq!(buffer_field.to_integer(IntegerSize::FourBytes, Global).unwrap(), 0);
    }

    #[test]
    fn buffer_field_to_8_byte_integer() {
        const BUFFER: [u8; 6] = [0x0f, 0x00, 0x00, 0x00, 0xf0, 0xff];
        let buffer = Object::Buffer(Vec::from(BUFFER)).wrap(Global);
        let buffer_field = Object::BufferField { buffer, offset: 4, length: 36 };

        assert_eq!(buffer_field.to_integer(IntegerSize::EightBytes, Global).unwrap(), 0x0000000f_00000000);
    }
}
