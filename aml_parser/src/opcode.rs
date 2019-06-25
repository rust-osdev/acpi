use crate::{parser::*, AmlContext, AmlError};

pub const NULL_NAME: u8 = 0x00;
pub const DUAL_NAME_PREFIX: u8 = 0x2E;
pub const MULTI_NAME_PREFIX: u8 = 0x2F;
pub const ROOT_CHAR: u8 = b'\\';
pub const PREFIX_CHAR: u8 = b'^';

pub const RESERVED_FIELD: u8 = 0x00;
pub const ACCESS_FIELD: u8 = 0x01;
pub const CONNECT_FIELD: u8 = 0x02;
pub const EXTENDED_ACCESS_FIELD: u8 = 0x03;

pub const ZERO_OP: u8 = 0x00;
pub const ONE_OP: u8 = 0x01;
pub const ONES_OP: u8 = 0xff;
pub const BYTE_CONST: u8 = 0x0a;
pub const WORD_CONST: u8 = 0x0b;
pub const DWORD_CONST: u8 = 0x0c;
pub const STRING_PREFIX: u8 = 0x0d;
pub const QWORD_CONST: u8 = 0x0e;

pub const DEF_NAME_OP: u8 = 0x08;
pub const DEF_SCOPE_OP: u8 = 0x10;
pub const DEF_BUFFER_OP: u8 = 0x11;
pub const DEF_PACKAGE_OP: u8 = 0x12;
pub const DEF_METHOD_OP: u8 = 0x14;
pub const EXT_REVISION_OP: u8 = 0x30;
pub const EXT_DEF_OP_REGION_OP: u8 = 0x80;
pub const EXT_DEF_FIELD_OP: u8 = 0x81;
pub const EXT_DEF_DEVICE_OP: u8 = 0x82;

pub const EXT_OPCODE_PREFIX: u8 = 0x5b;

pub(crate) fn opcode<'a, 'c>(opcode: u8) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| match input.first() {
        None => Err((input, context, AmlError::UnexpectedEndOfStream)),
        Some(&byte) if byte == opcode => Ok((&input[1..], context, ())),
        Some(&byte) => Err((input, context, AmlError::UnexpectedByte(byte))),
    }
}

pub(crate) fn ext_opcode<'a, 'c>(ext_opcode: u8) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    opcode(EXT_OPCODE_PREFIX).then(opcode(ext_opcode)).map(|_| ())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{test_utils::*, AmlError};

    #[test]
    fn empty() {
        let mut context = AmlContext::new();
        check_err!(
            opcode(NULL_NAME).parse(&[], &mut context),
            AmlError::UnexpectedEndOfStream,
            &[]
        );
        check_err!(
            ext_opcode(EXT_DEF_FIELD_OP).parse(&[], &mut context),
            AmlError::UnexpectedEndOfStream,
            &[]
        );
    }

    #[test]
    fn simple_opcodes() {
        let mut context = AmlContext::new();
        check_ok!(opcode(DEF_SCOPE_OP).parse(&[DEF_SCOPE_OP], &mut context), (), &[]);
        check_ok!(
            opcode(DEF_NAME_OP).parse(&[DEF_NAME_OP, 0x31, 0x55, 0xf3], &mut context),
            (),
            &[0x31, 0x55, 0xf3]
        );
    }

    #[test]
    fn extended_opcodes() {
        let mut context = AmlContext::new();
        check_err!(
            ext_opcode(EXT_DEF_FIELD_OP).parse(&[EXT_DEF_FIELD_OP, EXT_DEF_FIELD_OP], &mut context),
            AmlError::UnexpectedByte(EXT_DEF_FIELD_OP),
            &[EXT_DEF_FIELD_OP, EXT_DEF_FIELD_OP]
        );
        check_ok!(
            ext_opcode(EXT_DEF_FIELD_OP)
                .parse(&[EXT_OPCODE_PREFIX, EXT_DEF_FIELD_OP], &mut context),
            (),
            &[]
        );
    }
}
