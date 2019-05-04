use crate::{parser::*, AmlError};

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
pub const PACKAGE_OP: u8 = 0x12;
pub const METHOD_OP: u8 = 0x14;
pub const EXT_REVISION_OP: u8 = 0x30;
pub const EXT_OP_REGION_OP: u8 = 0x80;
pub const EXT_FIELD_OP: u8 = 0x81;
pub const EXT_DEVICE_OP: u8 = 0x82;

pub const EXT_OPCODE_PREFIX: u8 = 0x5b;

pub(crate) fn opcode<'a>(opcode: u8) -> impl Parser<'a, ()> {
    move |stream: &'a [u8]| match stream.first() {
        None => Err((stream, AmlError::UnexpectedEndOfStream)),
        Some(&byte) if byte == opcode => Ok((&stream[1..], ())),
        Some(&byte) => Err((stream, AmlError::UnexpectedByte(byte))),
    }
}

pub(crate) fn ext_opcode<'a>(ext_opcode: u8) -> impl Parser<'a, ()> {
    opcode(EXT_OPCODE_PREFIX).then(opcode(ext_opcode)).map(|_| ())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{test_utils::*, AmlError};

    #[test]
    fn empty() {
        check_err!(opcode(NULL_NAME).parse(&[]), AmlError::UnexpectedEndOfStream, &[]);
        check_err!(ext_opcode(EXT_FIELD_OP).parse(&[]), AmlError::UnexpectedEndOfStream, &[]);
    }

    #[test]
    fn simple_opcodes() {
        check_ok!(opcode(SCOPE_OP).parse(&[SCOPE_OP]), (), &[]);
        check_ok!(opcode(NAME_OP).parse(&[NAME_OP, 0x31, 0x55, 0xf3]), (), &[0x31, 0x55, 0xf3]);
    }

    #[test]
    fn extended_opcodes() {
        check_err!(
            ext_opcode(EXT_FIELD_OP).parse(&[EXT_FIELD_OP, EXT_FIELD_OP]),
            AmlError::UnexpectedByte(EXT_FIELD_OP),
            &[EXT_FIELD_OP, EXT_FIELD_OP]
        );
        check_ok!(ext_opcode(EXT_FIELD_OP).parse(&[EXT_OPCODE_PREFIX, EXT_FIELD_OP]), (), &[]);
    }
}
