pub const NULL_NAME: u8 = 0x00;
pub const DUAL_NAME_PREFIX: u8 = 0x2E;
pub const MULTI_NAME_PREFIX: u8 = 0x2F;

pub const ZERO_OP: u8 = 0x00;
pub const ONE_OP: u8 = 0x01;
pub const ONES_OP: u8 = 0xff;
pub const BYTE_CONST: u8 = 0x0a;
pub const WORD_CONST: u8 = 0x0b;
pub const DWORD_CONST: u8 = 0x0c;
pub const STRING_PREFIX: u8 = 0x0d;
pub const QWORD_CONST: u8 = 0x0e;

pub const NAME_OP: u8 = 0x08;
pub const SCOPE_OP: u8 = 0x10;
pub const BUFFER_OP: u8 = 0x11;
pub const METHOD_OP: u8 = 0x14;
pub const EXT_REVISION_OP: u8 = 0x30;
pub const EXT_OP_REGION_OP: u8 = 0x80;
pub const EXT_FIELD_OP: u8 = 0x81;
pub const EXT_DEVICE_OP: u8 = 0x82;

pub const EXT_OPCODE_PREFIX: u8 = 0x5b;
