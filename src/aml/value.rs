use super::AmlError;
use bit_field::BitField;

#[derive(Debug)]
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

pub enum FieldAccessType {
    Any,
    Byte,
    Word,
    DWord,
    QWord,
    Buffer,
    Reserved,
}

pub enum FieldUpdateRule {
    Preserve,
    WriteAsOnes,
    WriteAsZeros,
}

#[derive(Clone, Copy, Debug)] // TODO: custom debug / get rid of completely
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

#[derive(Debug)]
pub enum AmlValue {
    Integer(u64),

    OpRegion { region: RegionSpace, offset: u64, length: u64 },

    Field { flags: FieldFlags, offset: u64, length: u64 },
}

impl AmlValue {
    pub fn as_integer(&self) -> Result<u64, AmlError> {
        match self {
            AmlValue::Integer(value) => Ok(*value),

            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }
}
