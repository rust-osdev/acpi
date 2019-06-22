use crate::{
    name_object::name_string,
    opcode::{opcode, SCOPE_OP},
    parser::{choice, comment, comment_scope, take_n, ParseResult, Parser},
    pkg_length::{pkg_length, PkgLength},
    value::{AmlValue, FieldFlags},
    AmlNamespace,
};
use log::{debug, trace};

/// `TermList`s are usually found within explicit-length objects (so they have a `PkgLength`
/// elsewhere in the structure), so this takes a number of bytes to parse.
pub fn term_list<'a>(list_length: PkgLength) -> impl Parser<'a, ()> {
    /*
     * TermList := Nothing | <TermObj TermList>
     */
    move |mut input: &'a [u8]| -> ParseResult<'a, ()> {
        while list_length.still_parsing(input) {
            let (new_input, ()) = term_object().parse(input)?;
            input = new_input;
        }

        Ok((input, ()))
    }
}

// TODO: maybe return `AmlValue` on success
pub fn term_object<'a>() -> impl Parser<'a, ()> {
    /*
     * TermObj := NamespaceModifierObj | NamedObj | Type1Opcode | Type2Opcode
     * NamespaceModifierObj := DefAlias | DefName | DefScope
     */
    comment_scope("TermObj", choice!(def_scope()))
}

pub fn def_scope<'a>() -> impl Parser<'a, ()> {
    /*
     * DefScope := 0x10 PkgLength NameString TermList
     */
    opcode(SCOPE_OP)
        .then(comment_scope(
            "DefScope",
            pkg_length().then(name_string()).feed(move |(pkg_length, name)| {
                debug!("Scope with name: {}, length: {:?}", name, pkg_length);
                term_list(pkg_length)
            }),
        ))
        .discard_result()
}
pub fn computational_data<'a>() -> impl Parser<'a, AmlValue> {
    /*
     * ComputationalData := ByteConst | WordConst | DWordConst | QWordConst | String |
     *                      ConstObj | RevisionOp | DefBuffer
     * ByteConst := 0x0a ByteData
     * WordConst := 0x0b WordData
     * DWordConst := 0x0c DWordData
     * QWordConst := 0x0e QWordData
     * String := 0x0d AsciiCharList NullChar
     * ConstObj := ZeroOp(0x00) | OneOp(0x01) | OnesOp(0xff)
     * RevisionOp := ExtOpPrefix(0x5b) 0x30
     */
    let const_parser = |input: &'a [u8]| {
        let (new_input, op) = take().parse(input)?;

        match op {
            opcode::BYTE_CONST => {
                take().map(|value| AmlValue::Integer(value as u64)).parse(new_input)
            }
            opcode::WORD_CONST => {
                take_u16().map(|value| AmlValue::Integer(value as u64)).parse(new_input)
            }
            opcode::DWORD_CONST => {
                take_u32().map(|value| AmlValue::Integer(value as u64)).parse(new_input)
            }
            opcode::QWORD_CONST => {
                take_u64().map(|value| AmlValue::Integer(value)).parse(new_input)
            }
            // TODO: implement String
            opcode::ZERO_OP => Ok((new_input, AmlValue::Integer(0))),
            opcode::ONE_OP => Ok((new_input, AmlValue::Integer(1))),
            opcode::ONES_OP => Ok((new_input, AmlValue::Integer(u64::max_value()))),

            _ => Err((input, AmlError::UnexpectedByte(op))),
        }
    };

    comment_scope(
        "ComputationalData",
        choice!(
            ext_opcode(opcode::EXT_REVISION_OP)
                .map(|_| AmlValue::Integer(crate::AML_INTERPRETER_REVISION)),
            const_parser //TODO: parse DefBuffer here too
        ),
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_computational_data() {
        check_ok!(
            computational_data().parse(&[0x00, 0x34, 0x12]),
            AmlValue::Integer(0),
            &[0x34, 0x12]
        );
        check_ok!(
            computational_data().parse(&[0x01, 0x18, 0xf3]),
            AmlValue::Integer(1),
            &[0x18, 0xf3]
        );
        check_ok!(
            computational_data().parse(&[0xff, 0x98, 0xc3]),
            AmlValue::Integer(u64::max_value()),
            &[0x98, 0xc3]
        );
        check_ok!(
            computational_data().parse(&[0x5b, 0x30]),
            AmlValue::Integer(crate::AML_INTERPRETER_REVISION),
            &[]
        );
        check_ok!(
            computational_data().parse(&[0x0a, 0xf3, 0x35]),
            AmlValue::Integer(0xf3),
            &[0x35]
        );
        check_ok!(computational_data().parse(&[0x0b, 0xf3, 0x35]), AmlValue::Integer(0x35f3), &[]);
        check_ok!(
            computational_data().parse(&[0x0c, 0xf3, 0x35, 0x12, 0x65, 0xff, 0x00]),
            AmlValue::Integer(0x651235f3),
            &[0xff, 0x00]
        );
        check_ok!(
            computational_data()
                .parse(&[0x0e, 0xf3, 0x35, 0x12, 0x65, 0xff, 0x00, 0x67, 0xde, 0x28]),
            AmlValue::Integer(0xde6700ff651235f3),
            &[0x28]
        );
    }
}
