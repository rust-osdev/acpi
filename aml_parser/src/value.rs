use crate::AmlError;
use alloc::{collections::BTreeMap, string::String, vec::Vec};
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AmlValue {
    Integer(u64),
    String(String),
    Field { flags: FieldFlags, offset: u64, length: u64 },
}
