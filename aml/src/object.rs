use crate::op_region::OpRegion;
use alloc::{sync::Arc, vec::Vec};
use bit_field::BitField;

#[derive(Debug)]
pub enum Object {
    Uninitialized,
    Buffer(Vec<u8>),
    BufferField { buffer: Arc<Object>, offset: usize, length: usize },
    Device,
    Event,
    FieldUnit(FieldUnit),
    Integer(u64),
    Method { code: Vec<u8>, flags: MethodFlags },
    Mutex,
    Reference(Arc<Object>),
    OpRegion(OpRegion),
    Package(Vec<Arc<Object>>),
    PowerResource,
    Processor,
    RawDataBuffer,
    String(String),
    ThermalZone,
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
            Object::Mutex => ObjectType::Mutex,
            Object::Reference(object) => object.typ(),
            Object::OpRegion(_) => ObjectType::OpRegion,
            Object::Package(_) => ObjectType::Package,
            Object::PowerResource => ObjectType::PowerResource,
            Object::Processor => ObjectType::Processor,
            Object::RawDataBuffer => ObjectType::RawDataBuffer,
            Object::String(_) => ObjectType::String,
            Object::ThermalZone => ObjectType::ThermalZone,
        }
    }
}

#[derive(Debug)]
pub enum FieldUnit {
    Normal { region: Arc<Object>, bit_index: usize, bit_length: usize },
    Bank,
    Index,
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
}
