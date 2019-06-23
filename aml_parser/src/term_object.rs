use crate::{
    name_object::{name_seg, name_string},
    opcode::{self, ext_opcode, opcode},
    parser::{choice, comment_scope, take, take_u16, take_u32, take_u64, ParseResult, Parser},
    pkg_length::{pkg_length, PkgLength},
    value::{AmlValue, FieldFlags},
    AmlContext,
    AmlError,
};
use alloc::string::String;
use log::{debug, trace};

/// `TermList`s are usually found within explicit-length objects (so they have a `PkgLength`
/// elsewhere in the structure), so this takes a number of bytes to parse.
pub fn term_list<'a, 'c>(list_length: PkgLength) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * TermList := Nothing | <TermObj TermList>
     */
    move |mut input: &'a [u8], mut context: &'c mut AmlContext| {
        while list_length.still_parsing(input) {
            let (new_input, new_context, ()) = term_object().parse(input, context)?;
            input = new_input;
            context = new_context;
        }

        Ok((input, context, ()))
    }
}

// TODO: maybe return `AmlValue` on success
pub fn term_object<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * TermObj := NamespaceModifierObj | NamedObj | Type1Opcode | Type2Opcode
     * NamespaceModifierObj := DefAlias | DefName | DefScope
     */
    comment_scope("TermObj", choice!(def_scope(), def_op_region(), def_field()))
}

pub fn def_scope<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefScope := 0x10 PkgLength NameString TermList
     */
    opcode(opcode::SCOPE_OP)
        .then(comment_scope(
            "DefScope",
            pkg_length().then(name_string()).feed(move |(pkg_length, name)| {
                debug!("Scope with name: {}, length: {:?}", name, pkg_length);
                term_list(pkg_length)
            }),
        ))
        .discard_result()
}

pub fn def_op_region<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefOpRegion := ExtOpPrefix 0x80 NameString RegionSpace RegionOffset RegionLen
     * RegionSpace := ByteData (where 0x00      = SystemMemory
     *                                0x01      = SystemIO
     *                                0x02      = PciConfig
     *                                0x03      = EmbeddedControl
     *                                0x04      = SMBus
     *                                0x05      = SystemCMOS
     *                                0x06      = PciBarTarget
     *                                0x07      = IPMI
     *                                0x08      = GeneralPurposeIO
     *                                0x09      = GenericSerialBus
     *                                0x80-0xff = OEM Defined)
     * ByteData := 0x00 - 0xff
     * RegionOffset := TermArg => Integer
     * RegionLen := TermArg => Integer
     */
    ext_opcode(opcode::EXT_OP_REGION_OP)
        .then(comment_scope(
            "DefOpRegion",
            name_string().then(take()).then(term_arg()).then(term_arg()).map(
                |(((name, space), offset), region_len)| {
                    trace!("Op region: {}, {}, {:?}, {:?}", name, space, offset, region_len);
                    ()
                },
            ),
        ))
        .discard_result()
}

pub fn def_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefField = ExtOpPrefix 0x81 PkgLength NameString FieldFlags FieldList
     * FieldFlags := ByteData
     */
    ext_opcode(opcode::EXT_FIELD_OP)
        .then(comment_scope(
            "DefField",
            pkg_length().then(name_string()).then(take()).feed(
                |((list_length, region_name), flags)| {
                    move |mut input: &'a [u8],
                          mut context: &'c mut AmlContext|
                          -> ParseResult<'a, 'c, ()> {
                        /*
                         * FieldList := Nothing | <FieldElement FieldList>
                         */
                        // TODO: can this pattern be expressed as a combinator
                        while list_length.still_parsing(input) {
                            let (new_input, new_context, ()) =
                                field_element(&region_name, FieldFlags::new(flags))
                                    .parse(input, context)?;
                            input = new_input;
                            context = new_context;
                        }

                        Ok((input, context, ()))
                    }
                },
            ),
        ))
        .discard_result()
}

pub fn field_element<'a, 'c>(region_name: &String, flags: FieldFlags) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * FieldElement := NamedField | ReservedField | AccessField | ExtendedAccessField |
     *                 ConnectField
     * NamedField := NameSeg PkgLength
     * ReservedField := 0x00 PkgLength
     * AccessField := 0x01 AccessType AccessAttrib
     * ConnectField := <0x02 NameString> | <0x02 BufferData>
     * ExtendedAccessField := 0x03 AccessType ExtendedAccessAttrib AccessLength
     *
     * AccessType := ByteData
     * AccessAttrib := ByteData
     *
     * XXX: The spec says a ConnectField can be <0x02 BufferData>, but BufferData isn't an AML
     * object (it seems to be defined in ASL). We treat BufferData as if it was encoded like
     * DefBuffer, and this seems to work so far.
     */
    // TODO: replace these maps with `with_context` and register the fields in the namespace
    // TODO: parse ConnectField and ExtendedAccessField
    let reserved_field =
        opcode(opcode::RESERVED_FIELD).then(pkg_length()).map(|((), pkg_length)| {
            trace!("Adding reserved field with length: {}", pkg_length.raw_length);
        });

    let access_field = opcode(opcode::ACCESS_FIELD).then(take()).then(take()).map(
        |(((), access_type), access_attrib)| {
            trace!(
                "Adding access field with access_type: {}, access_attrib: {}",
                access_type,
                access_attrib
            );
        },
    );

    let named_field = name_seg().then(pkg_length()).map(|(name, length)| {
        trace!("Named field with name {} and length {}", name.as_str(), length.raw_length);
    });

    choice!(reserved_field, access_field, named_field)
}

pub fn term_arg<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * TermArg := Type2Opcode | DataObject | ArgObj | LocalObj
     */
    // TODO: this doesn't yet parse Term2Opcode, ArgObj, or LocalObj
    comment_scope("TermArg", choice!(data_object()))
}

pub fn data_object<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DataObject := DefPackage | DefVarPackage | ComputationalData
     *
     * The order of the parsers are important here, as DefPackage and DefVarPackage can be
     * accidently parsed as ComputationalDatas.
     */
    // TODO: this doesn't yet parse DefPackage or DefVarPackage
    comment_scope("DataObject", choice!(computational_data()))
}

pub fn computational_data<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
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
    let const_parser = |input: &'a [u8], context: &'c mut AmlContext| {
        let (new_input, context, op) = take().parse(input, context)?;

        match op {
            opcode::BYTE_CONST => {
                take().map(|value| AmlValue::Integer(value as u64)).parse(new_input, context)
            }
            opcode::WORD_CONST => {
                take_u16().map(|value| AmlValue::Integer(value as u64)).parse(new_input, context)
            }
            opcode::DWORD_CONST => {
                take_u32().map(|value| AmlValue::Integer(value as u64)).parse(new_input, context)
            }
            opcode::QWORD_CONST => {
                take_u64().map(|value| AmlValue::Integer(value)).parse(new_input, context)
            }
            // TODO: implement String
            opcode::ZERO_OP => Ok((new_input, context, AmlValue::Integer(0))),
            opcode::ONE_OP => Ok((new_input, context, AmlValue::Integer(1))),
            opcode::ONES_OP => Ok((new_input, context, AmlValue::Integer(u64::max_value()))),

            _ => Err((input, context, AmlError::UnexpectedByte(op))),
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
        let mut context = AmlContext::new();
        check_ok!(
            computational_data().parse(&[0x00, 0x34, 0x12], &mut context),
            AmlValue::Integer(0),
            &[0x34, 0x12]
        );
        check_ok!(
            computational_data().parse(&[0x01, 0x18, 0xf3], &mut context),
            AmlValue::Integer(1),
            &[0x18, 0xf3]
        );
        check_ok!(
            computational_data().parse(&[0xff, 0x98, 0xc3], &mut context),
            AmlValue::Integer(u64::max_value()),
            &[0x98, 0xc3]
        );
        check_ok!(
            computational_data().parse(&[0x5b, 0x30], &mut context),
            AmlValue::Integer(crate::AML_INTERPRETER_REVISION),
            &[]
        );
        check_ok!(
            computational_data().parse(&[0x0a, 0xf3, 0x35], &mut context),
            AmlValue::Integer(0xf3),
            &[0x35]
        );
        check_ok!(
            computational_data().parse(&[0x0b, 0xf3, 0x35], &mut context),
            AmlValue::Integer(0x35f3),
            &[]
        );
        check_ok!(
            computational_data().parse(&[0x0c, 0xf3, 0x35, 0x12, 0x65, 0xff, 0x00], &mut context),
            AmlValue::Integer(0x651235f3),
            &[0xff, 0x00]
        );
        check_ok!(
            computational_data()
                .parse(&[0x0e, 0xf3, 0x35, 0x12, 0x65, 0xff, 0x00, 0x67, 0xde, 0x28], &mut context),
            AmlValue::Integer(0xde6700ff651235f3),
            &[0x28]
        );
    }
}
