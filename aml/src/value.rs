use crate::{misc::ArgNum, namespace::AmlName, AmlError};
use alloc::{boxed::Box, string::String, vec::Vec};
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AmlType {
    Uninitialized,
    Buffer,
    BufferField,
    /// Handle to a definition block handle. Returned by the `Load` operator.
    DdbHandle,
    DebugObject,
    Device,
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
    OpRegion { region: RegionSpace, offset: u64, length: u64 },
    Field { region: AmlName, flags: FieldFlags, offset: u64, length: u64 },
    Device,
    Method { flags: MethodFlags, code: Vec<u8> },
    Buffer { bytes: Vec<u8>, size: u64 },
    Processor { id: u8, pblk_address: u32, pblk_len: u8 },
    Mutex { sync_level: u8 },
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
            AmlValue::Device => AmlType::Device,
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
            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }

    pub fn as_integer(&self) -> Result<u64, AmlError> {
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

            _ => Err(AmlError::IncompatibleValueConversion),
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
    pub fn as_type(&self, desired_type: AmlType) -> Result<AmlValue, AmlError> {
        // Cache the type of this object
        let our_type = self.type_of();

        // If the value is already of the correct type, just return it as is
        if our_type == desired_type {
            return Ok(self.clone());
        }

        // TODO: implement all of the rules
        match desired_type {
            AmlType::Integer => self.as_integer().map(|value| AmlValue::Integer(value)),
            _ => Err(AmlError::IncompatibleValueConversion),
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
    /// Get an argument by its `ArgNum`.
    ///
    /// ### Panics
    /// Panics if passed an invalid argument number (valid argument numbers are `0..=6`)
    pub fn arg(&self, num: ArgNum) -> Result<&AmlValue, AmlError> {
        match num {
            0 => self.arg_0.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            1 => self.arg_1.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            2 => self.arg_2.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            3 => self.arg_3.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            4 => self.arg_4.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            5 => self.arg_5.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            6 => self.arg_6.as_ref().ok_or(AmlError::InvalidArgumentAccess(num)),
            _ => panic!("Invalid argument number: {}", num),
        }
    }
}
