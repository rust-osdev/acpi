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
pub const DEF_EXTERNAL_OP: u8 = 0x15;
pub const DEF_CREATE_DWORD_FIELD_OP: u8 = 0x8a;
pub const DEF_CREATE_WORD_FIELD_OP: u8 = 0x8b;
pub const DEF_CREATE_BYTE_FIELD_OP: u8 = 0x8c;
pub const DEF_CREATE_BIT_FIELD_OP: u8 = 0x8d;
pub const DEF_CREATE_QWORD_FIELD_OP: u8 = 0x8f;
pub const EXT_DEF_MUTEX_OP: u8 = 0x01;
pub const EXT_DEF_CREATE_FIELD_OP: u8 = 0x13;
pub const EXT_REVISION_OP: u8 = 0x30;
pub const EXT_DEF_FATAL_OP: u8 = 0x32;
pub const EXT_DEF_OP_REGION_OP: u8 = 0x80;
pub const EXT_DEF_FIELD_OP: u8 = 0x81;
pub const EXT_DEF_DEVICE_OP: u8 = 0x82;
pub const EXT_DEF_PROCESSOR_OP: u8 = 0x83;
pub const EXT_DEF_POWER_RES_OP: u8 = 0x84;
pub const EXT_DEF_THERMAL_ZONE_OP: u8 = 0x85;

/*
 * Type 1 opcodes
 */
pub const DEF_CONTINUE_OP: u8 = 0x9f;
pub const DEF_IF_ELSE_OP: u8 = 0xa0;
pub const DEF_ELSE_OP: u8 = 0xa1;
pub const DEF_WHILE_OP: u8 = 0xa2;
pub const DEF_NOOP_OP: u8 = 0xa3;
pub const DEF_RETURN_OP: u8 = 0xa4;
pub const DEF_BREAK_OP: u8 = 0xa5;
pub const DEF_BREAKPOINT_OP: u8 = 0xcc;

/*
 * Type 2 opcodes
 */
pub const DEF_STORE_OP: u8 = 0x70;
pub const DEF_ADD_OP: u8 = 0x72;
pub const DEF_CONCAT_OP: u8 = 0x73;
pub const DEF_INCREMENT_OP: u8 = 0x75;
pub const DEF_DECREMENT_OP: u8 = 0x76;
pub const DEF_SHIFT_LEFT: u8 = 0x79;
pub const DEF_SHIFT_RIGHT: u8 = 0x7a;
pub const DEF_AND_OP: u8 = 0x7b;
pub const DEF_CONCAT_RES_OP: u8 = 0x84;
pub const DEF_L_OR_OP: u8 = 0x91;
pub const DEF_L_NOT_OP: u8 = 0x92;
pub const DEF_L_EQUAL_OP: u8 = 0x93;
pub const DEF_L_GREATER_OP: u8 = 0x94;
pub const DEF_L_LESS_OP: u8 = 0x95;
pub const DEF_TO_INTEGER_OP: u8 = 0x99;
pub const DEF_MID_OP: u8 = 0x9e;

/*
 * Miscellaneous objects
 */
pub const EXT_DEBUG_OP: u8 = 0x31;
pub const LOCAL0_OP: u8 = 0x60;
pub const LOCAL1_OP: u8 = 0x61;
pub const LOCAL2_OP: u8 = 0x62;
pub const LOCAL3_OP: u8 = 0x63;
pub const LOCAL4_OP: u8 = 0x64;
pub const LOCAL5_OP: u8 = 0x65;
pub const LOCAL6_OP: u8 = 0x66;
pub const LOCAL7_OP: u8 = 0x67;
pub const ARG0_OP: u8 = 0x68;
pub const ARG1_OP: u8 = 0x69;
pub const ARG2_OP: u8 = 0x6a;
pub const ARG3_OP: u8 = 0x6b;
pub const ARG4_OP: u8 = 0x6c;
pub const ARG5_OP: u8 = 0x6d;
pub const ARG6_OP: u8 = 0x6e;

pub const EXT_OPCODE_PREFIX: u8 = 0x5b;

pub(crate) fn opcode<'a, 'c>(opcode: u8) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| match input.first() {
        None => Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream))),
        Some(&byte) if byte == opcode => Ok((&input[1..], context, ())),
        Some(_) => Err((input, context, Propagate::Err(AmlError::WrongParser))),
    }
}

pub(crate) fn ext_opcode<'a, 'c>(ext_opcode: u8) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    opcode(EXT_OPCODE_PREFIX).then(opcode(ext_opcode)).discard_result()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{test_utils::*, AmlError};

    #[test]
    fn empty() {
        let mut context = crate::test_utils::make_test_context();
        check_err!(opcode(NULL_NAME).parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
        check_err!(ext_opcode(EXT_DEF_FIELD_OP).parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
    }

    #[test]
    fn simple_opcodes() {
        let mut context = crate::test_utils::make_test_context();
        check_ok!(opcode(DEF_SCOPE_OP).parse(&[DEF_SCOPE_OP], &mut context), (), &[]);
        check_ok!(
            opcode(DEF_NAME_OP).parse(&[DEF_NAME_OP, 0x31, 0x55, 0xf3], &mut context),
            (),
            &[0x31, 0x55, 0xf3]
        );
    }

    #[test]
    fn extended_opcodes() {
        let mut context = crate::test_utils::make_test_context();
        check_err!(
            ext_opcode(EXT_DEF_FIELD_OP).parse(&[EXT_DEF_FIELD_OP, EXT_DEF_FIELD_OP], &mut context),
            AmlError::WrongParser,
            &[EXT_DEF_FIELD_OP, EXT_DEF_FIELD_OP]
        );
        check_ok!(
            ext_opcode(EXT_DEF_FIELD_OP).parse(&[EXT_OPCODE_PREFIX, EXT_DEF_FIELD_OP], &mut context),
            (),
            &[]
        );
    }
}
