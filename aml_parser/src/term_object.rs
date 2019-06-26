use crate::{
    name_object::{name_seg, name_string},
    opcode::{self, ext_opcode, opcode},
    parser::{
        choice,
        comment_scope,
        comment_scope_verbose,
        make_parser_concrete,
        take,
        take_to_end_of_pkglength,
        take_u16,
        take_u32,
        take_u64,
        ParseResult,
        Parser,
    },
    pkg_length::{pkg_length, PkgLength},
    value::{AmlValue, FieldFlags},
    AmlContext,
    AmlError,
};
use alloc::{string::String, vec::Vec};
use core::str;
use log::trace;

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

pub fn term_object<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * TermObj := NamespaceModifierObj | NamedObj | Type1Opcode | Type2Opcode
     */
    comment_scope_verbose("TermObj", choice!(namespace_modifier(), named_obj()))
}

pub fn namespace_modifier<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * NamespaceModifierObj := DefAlias | DefName | DefScope
     */
    choice!(def_name(), def_scope())
}

pub fn named_obj<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * NamedObj := DefBankField | DefCreateBitField | DefCreateByteField | DefCreateDWordField |
     *             DefCreateField | DefCreateQWordField | DefCreateWordField | DefDataRegion |
     *             DefExternal | DefOpRegion | DefPowerRes | DefProcessor | DefThermalZone |
     *             DefMethod | DefMutex
     *
     * XXX: DefMethod and DefMutex (at least) are not included in any rule in the AML grammar,
     * but are defined in the NamedObj section so we assume they're part of NamedObj
     */
    comment_scope_verbose(
        "NamedObj",
        choice!(
            def_op_region(),
            def_field(),
            def_method(),
            def_device(),
}

pub fn def_name<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefName := 0x08 NameString DataRefObject
     */
    opcode(opcode::DEF_NAME_OP)
        .then(comment_scope(
            "DefName",
            name_string().then(data_ref_object()).map_with_context(
                |(name, data_ref_object), context| {
                    // TODO: add to namespace
                    trace!("Defined name: {}", name);
                    ((), context)
                },
            ),
        ))
        .discard_result()
}

pub fn def_scope<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefScope := 0x10 PkgLength NameString TermList
     */
    opcode(opcode::DEF_SCOPE_OP)
        .then(comment_scope(
            "DefScope",
            pkg_length()
                .then(name_string())
                .map_with_context(|(length, name), context| {
                    // TODO: change scope in `context`
                    trace!("Moving to scope: {:?}", name);
                    ((length, name), context)
                })
                .feed(move |(pkg_length, name)| {
                    trace!("Scope with name: {}, length: {:?}", name, pkg_length);
                    term_list(pkg_length).map(move |_| Ok(name.clone()))
                })
                .map_with_context(|name, context| {
                    // TODO: remove scope
                    trace!("Exiting scope: {}", name);
                    ((), context)
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
    ext_opcode(opcode::EXT_DEF_OP_REGION_OP)
        .then(comment_scope(
            "DefOpRegion",
            name_string().then(take()).then(term_arg()).then(term_arg()).map_with_context(
                |(((name, space), offset), region_len), context| {
                    trace!("Op region: {}, {}, {:?}, {:?}", name, space, offset, region_len);
                    // TODO: add to namespace
                    ((), context)
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
    ext_opcode(opcode::EXT_DEF_FIELD_OP)
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
    let reserved_field = opcode(opcode::RESERVED_FIELD).then(pkg_length()).map_with_context(
        |((), pkg_length), context| {
            trace!("Adding reserved field with length: {}", pkg_length.raw_length);
            // TODO: put it in the namespace
            ((), context)
        },
    );

    let access_field = opcode(opcode::ACCESS_FIELD).then(take()).then(take()).map(
        |(((), access_type), access_attrib)| {
            trace!(
                "Adding access field with access_type: {}, access_attrib: {}",
                access_type,
                access_attrib
            );
            // TODO: put it in the namespace
        },
    );

    let named_field = name_seg().then(pkg_length()).map_with_context(|(name, length), context| {
        trace!("Named field with name {} and length {}", name.as_str(), length.raw_length);
        // TODO: put it in the namespace
        ((), context)
    });

    choice!(reserved_field, access_field, named_field)
}

pub fn def_method<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefMethod := 0x14 PkgLength NameString MethodFlags TermList
     * MethodFlags := ByteData (where bits 0-2: ArgCount (0 to 7)
     *                                bit 3: SerializeFlag (0 = Not Serialized, 1 = Serialized)
     *                                bits 4-7: SyncLevel (0x00 to 0x0f))
     */
    opcode(opcode::DEF_METHOD_OP)
        .then(comment_scope(
            "DefMethod",
            pkg_length()
                .then(name_string())
                .then(take())
                .feed(|((length, name), flags)| {
                    take_to_end_of_pkglength(length)
                        .map(move |code| Ok((name.clone(), flags, code)))
                })
                .map_with_context(|(name, flags, code), context| {
                    // TODO: put it in the namespace
                    trace!(
                        "Method with name {} with flags {:#b}, length = {}",
                        name,
                        flags,
                        code.len()
                    );
                    ((), context)
                }),
        ))
        .discard_result()
}

pub fn def_device<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefDevice := ExtOpPrefix 0x82 PkgLength NameString TermList
     */
    ext_opcode(opcode::EXT_DEF_DEVICE_OP)
        .then(comment_scope(
            "DefDevice",
            pkg_length()
                .then(name_string())
                .feed(|(length, name)| {
                    term_list(length).map(move |result| Ok((name.clone(), result)))
                })
                .map_with_context(|(name, result), context| {
                    // TODO: add to namespace
                    trace!("Device with name: {}", name);
                    ((), context)
                }),
        ))
        .discard_result()
}

pub fn def_buffer<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefBuffer := 0x11 PkgLength BufferSize ByteList
     * BufferSize := TermArg => Integer
     *
     * XXX: The spec says that zero-length buffers (e.g. the PkgLength is 0) are illegal, but
     * we've encountered them in QEMU-generated tables, so we return an empty buffer in these
     * cases.
     */
    opcode(opcode::DEF_BUFFER_OP)
        .then(comment_scope(
            "DefBuffer",
            pkg_length().then(term_arg()).feed(|(pkg_length, buffer_size)| {
                take_to_end_of_pkglength(pkg_length)
                    .map(move |bytes| Ok((bytes.to_vec(), buffer_size.as_integer()?)))
            }),
        ))
        .map(|((), (bytes, buffer_size))| Ok(AmlValue::Buffer { bytes, size: buffer_size }))
}
pub fn term_arg<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * TermArg := Type2Opcode | DataObject | ArgObj | LocalObj
     */
    // TODO: this doesn't yet parse Term2Opcode, ArgObj, or LocalObj
    comment_scope_verbose("TermArg", choice!(data_object()))
}

pub fn data_ref_object<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DataRefObject := DataObject | ObjectReference | DDBHandle
     */
    comment_scope_verbose("DataRefObject", choice!(data_object()))
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
        let string_parser = |input: &'a [u8], context| -> ParseResult<'a, 'c, AmlValue> {
            /*
             * Using `position` isn't very efficient here, but is probably fine because the
             * strings are usually quite short.
             */
            let nul_position = match input.iter().position(|&c| c == b'\0') {
                Some(position) => position,
                None => return Err((input, context, AmlError::UnterminatedStringConstant)),
            };

            let string = String::from(match str::from_utf8(&input[0..nul_position]) {
                Ok(string) => string,
                Err(_) => return Err((input, context, AmlError::InvalidStringConstant)),
            });

            Ok((&input[(nul_position + 1)..], context, AmlValue::String(string)))
        };

        let (new_input, context, op) = take().parse(input, context)?;
        match op {
            opcode::BYTE_CONST => {
                take().map(|value| Ok(AmlValue::Integer(value as u64))).parse(new_input, context)
            }
            opcode::WORD_CONST => take_u16()
                .map(|value| Ok(AmlValue::Integer(value as u64)))
                .parse(new_input, context),
            opcode::DWORD_CONST => take_u32()
                .map(|value| Ok(AmlValue::Integer(value as u64)))
                .parse(new_input, context),
            opcode::QWORD_CONST => {
                take_u64().map(|value| Ok(AmlValue::Integer(value))).parse(new_input, context)
            }
            opcode::STRING_PREFIX => string_parser.parse(new_input, context),
            opcode::ZERO_OP => Ok((new_input, context, AmlValue::Integer(0))),
            opcode::ONE_OP => Ok((new_input, context, AmlValue::Integer(1))),
            opcode::ONES_OP => Ok((new_input, context, AmlValue::Integer(u64::max_value()))),

            _ => Err((input, context, AmlError::UnexpectedByte(op))),
        }
    };

    comment_scope_verbose(
        "ComputationalData",
        choice!(
            ext_opcode(opcode::EXT_REVISION_OP)
                .map(|_| Ok(AmlValue::Integer(crate::AML_INTERPRETER_REVISION))),
            const_parser,
            make_parser_concrete!(def_buffer())
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
        check_ok!(
            computational_data()
                .parse(&[0x0d, b'A', b'B', b'C', b'D', b'\0', 0xff, 0xf5], &mut context),
            AmlValue::String(String::from("ABCD")),
            &[0xff, 0xf5]
        );
    }
}
