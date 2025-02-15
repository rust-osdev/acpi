use crate::{AmlError, op_region::OpRegion};
use alloc::{string::String, sync::Arc, vec::Vec};
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
    PowerResource { system_level: u8, resource_order: u16 },
    Processor { proc_id: u8, pblk_address: u32, pblk_length: u8 },
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

    pub fn read_buffer_field(&self, dst: &mut [u8]) -> Result<(), AmlError> {
        if let Self::BufferField { buffer, offset, length } = self {
            let Object::Buffer(ref buffer) = **buffer else { panic!() };
            // TODO: assert length of buffer is sufficient
            copy_bits(buffer.as_slice(), *offset, dst, 0, *length);
            Ok(())
        } else {
            Err(AmlError::InvalidOperationOnObject)
        }
    }

    pub fn write_buffer_field(&mut self, value: &[u8]) -> Result<(), AmlError> {
        if let Self::BufferField { buffer, offset, length } = self {
            let Object::Buffer(buffer) = buffer.gain_mut() else { panic!() };
            copy_bits(value, 0, buffer.as_mut_slice(), *offset, *length);
            Ok(())
        } else {
            Err(AmlError::InvalidOperationOnObject)
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
            Object::PowerResource { .. } => ObjectType::PowerResource,
            Object::Processor { .. } => ObjectType::Processor,
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

/// Copy an arbitrary bit range of `src` to an arbitrary bit range of `dst`. This is used for
/// buffer fields. Data is zero-extended if `src` does not cover `length` bits, matching the
/// expected behaviour for buffer fields.
fn copy_bits(src: &[u8], mut src_index: usize, dst: &mut [u8], mut dst_index: usize, mut length: usize) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_copy_bits() {
        let src = [0b1011_1111, 0b1111_0111, 0b1111_1111, 0b1111_1111, 0b1111_1111];
        let mut dst = [0b1110_0001, 0, 0, 0, 0];

        copy_bits(&src, 0, &mut dst, 2, 15);
        for i in 0..dst.len() {
            print!("{:08b} ", dst[i]);
        }
        assert_eq!(dst, [0b1111_1101, 0b1101_1110, 0b0000_0001, 0b0000_0000, 0b0000_0000]);
    }
}
