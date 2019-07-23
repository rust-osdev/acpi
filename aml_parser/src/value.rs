use crate::{
    name_object::AmlName,
    parser::Parser,
    pkg_length::PkgLength,
    term_object::term_list,
    AmlContext,
    AmlError,
};
use alloc::{boxed::Box, string::String, vec::Vec};
use bit_field::BitField;
use log::info;

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
    Name(Box<AmlValue>),
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
    pub fn as_integer(&self) -> Result<u64, AmlError> {
        match self {
            AmlValue::Integer(value) => Ok(*value),

            AmlValue::Buffer { size, ref bytes } => {
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

    /// If this value is a control method, invoke it. Returns `AmlError::IncompatibleValueConversion` if this
    /// is not a method.
    pub fn invoke(&self, context: &mut AmlContext, args: Args) -> Result<AmlValue, AmlError> {
        if let AmlValue::Method { flags, ref code } = self {
            /*
             * First, set up the state we expect to enter the method with, but clearing local
             * variables to "null" and setting the arguments.
             */
            context.current_args = Some(args);
            context.local_0 = None;
            context.local_1 = None;
            context.local_2 = None;
            context.local_3 = None;
            context.local_4 = None;
            context.local_5 = None;
            context.local_6 = None;
            context.local_7 = None;

            let result = term_list(PkgLength::from_raw_length(code, code.len() as u32)).parse(code, context);

            /*
             * Now clear the state.
             */
            context.current_args = None;
            context.local_0 = None;
            context.local_1 = None;
            context.local_2 = None;
            context.local_3 = None;
            context.local_4 = None;
            context.local_5 = None;
            context.local_6 = None;
            context.local_7 = None;

            // TODO: return the real return value
            Ok(AmlValue::Integer(0))
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }
}

/// A control method can take up to 7 arguments, each of which can be an `AmlValue`.
#[derive(Clone, Debug, Default)]
pub struct Args {
    arg_0: Option<AmlValue>,
    arg_1: Option<AmlValue>,
    arg_2: Option<AmlValue>,
    arg_3: Option<AmlValue>,
    arg_4: Option<AmlValue>,
    arg_5: Option<AmlValue>,
    arg_6: Option<AmlValue>,
}
