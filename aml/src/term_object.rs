use crate::{
    expression::{def_buffer, def_package, expression_opcode},
    misc::{arg_obj, local_obj},
    name_object::{name_seg, name_string},
    namespace::{AmlName, LevelType},
    opcode::{self, ext_opcode, opcode},
    parser::{
        choice,
        comment_scope,
        take,
        take_to_end_of_pkglength,
        take_u16,
        take_u32,
        take_u64,
        try_with_context,
        ParseResult,
        Parser,
        Propagate,
    },
    pkg_length::{pkg_length, PkgLength},
    statement::statement_opcode,
    value::{AmlValue, FieldFlags, MethodCode, MethodFlags, RegionSpace},
    AmlContext,
    AmlError,
    AmlHandle,
    DebugVerbosity,
};
use alloc::{string::String, sync::Arc, vec::Vec};
use core::str;

/// `TermList`s are usually found within explicit-length objects (so they have a `PkgLength`
/// elsewhere in the structure), so this takes a number of bytes to parse.
pub fn term_list<'a, 'c>(list_length: PkgLength) -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * TermList := Nothing | <TermObj TermList>
     */
    // TODO: why does this use still_parsing, instead of just taking the whole thing and parsing it til it's empty?
    move |mut input: &'a [u8], mut context: &'c mut AmlContext| {
        while list_length.still_parsing(input) {
            // TODO: currently, we ignore the value of the expression. We may need to propagate
            // this.
            let (new_input, new_context, _) = term_object().parse(input, context)?;
            input = new_input;
            context = new_context;
        }

        Ok((input, context, ()))
    }
}

pub fn term_object<'a, 'c>() -> impl Parser<'a, 'c, Option<AmlValue>>
where
    'c: 'a,
{
    /*
     * TermObj := NamespaceModifierObj | NamedObj | StatementOpcode | ExpressionOpcode
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "TermObj",
        choice!(
            namespace_modifier().map(|()| Ok(None)),
            named_obj().map(|()| Ok(None)),
            statement_opcode().map(|()| Ok(None)),
            expression_opcode().map(|value| Ok(Some(value)))
        ),
    )
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
     * NamedObj := DefBankField | DefCreateBitField | DefCreateByteField | DefCreateWordField | DefCreateDWordField |
     *             DefCreateQWordField | DefCreateField | DefDataRegion | DefExternal | DefOpRegion | DefPowerRes |
     *             DefProcessor | DefThermalZone | DefMethod | DefMutex
     *
     * XXX: DefMethod and DefMutex (at least) are not included in any rule in the AML grammar,
     * but are defined in the NamedObj section so we assume they're part of NamedObj
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "NamedObj",
        choice!(
            def_create_bit_field(),
            def_create_byte_field(),
            def_create_word_field(),
            def_create_dword_field(),
            def_create_qword_field(),
            def_create_field(),
            def_op_region(),
            def_field(),
            def_method(),
            def_external(),
            def_device(),
            def_processor(),
            def_power_res(),
            def_thermal_zone(),
            def_mutex()
        ),
    )
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
            DebugVerbosity::Scopes,
            "DefName",
            name_string().then(data_ref_object()).map_with_context(|(name, data_ref_object), context| {
                try_with_context!(
                    context,
                    context.namespace.add_value_at_resolved_path(name, &context.current_scope, data_ref_object)
                );
                (Ok(()), context)
            }),
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
            DebugVerbosity::Scopes,
            "DefScope",
            pkg_length()
                .then(name_string())
                .map_with_context(|(length, name), context| {
                    let previous_scope = context.current_scope.clone();
                    context.current_scope = try_with_context!(context, name.resolve(&context.current_scope));

                    context.comment(
                        DebugVerbosity::Scopes,
                        &(String::from("Scope name: ") + &context.current_scope.as_string()),
                    );

                    try_with_context!(
                        context,
                        context.namespace.add_level(context.current_scope.clone(), LevelType::Scope)
                    );

                    (Ok((length, previous_scope)), context)
                })
                .feed(|(pkg_length, previous_scope)| {
                    term_list(pkg_length).map(move |_| Ok(previous_scope.clone()))
                })
                .map_with_context(|previous_scope, context| {
                    context.current_scope = previous_scope;
                    (Ok(()), context)
                }),
        ))
        .discard_result()
}

pub fn def_create_bit_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefCreateBitField := 0x8d SourceBuf BitIndex NameString
     * SourceBuf := TermArg => Buffer
     * BitIndex := TermArg => Integer
     */
    opcode(opcode::DEF_CREATE_BIT_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefCreateBitField",
            term_arg().then(term_arg()).then(name_string()).map_with_context(
                |((source, index), name), context| {
                    let source_data: Arc<spinning_top::Spinlock<Vec<u8>>> =
                        try_with_context!(context, source.as_buffer(context)).clone();
                    let index = try_with_context!(context, index.as_integer(context));

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::BufferField { buffer_data: source_data, offset: index, length: 1 }
                        )
                    );

                    (Ok(()), context)
                },
            ),
        ))
        .discard_result()
}

pub fn def_create_byte_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefCreateByteField := 0x8c SourceBuf ByteIndex NameString
     * SourceBuf := TermArg => Buffer
     * ByteIndex := TermArg => Integer
     */
    opcode(opcode::DEF_CREATE_BYTE_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefCreateByteField",
            term_arg().then(term_arg()).then(name_string()).map_with_context(
                |((source, index), name), context| {
                    let source_data: Arc<spinning_top::Spinlock<Vec<u8>>> =
                        try_with_context!(context, source.as_buffer(context)).clone();
                    let index = try_with_context!(context, index.as_integer(context));

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::BufferField { buffer_data: source_data, offset: index * 8, length: 8 }
                        )
                    );

                    (Ok(()), context)
                },
            ),
        ))
        .discard_result()
}

pub fn def_create_word_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefCreateWordField := 0x8b SourceBuf ByteIndex NameString
     * SourceBuf := TermArg => Buffer
     * ByteIndex := TermArg => Integer
     */
    opcode(opcode::DEF_CREATE_WORD_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefCreateWordField",
            term_arg().then(term_arg()).then(name_string()).map_with_context(
                |((source, index), name), context| {
                    let source_data: Arc<spinning_top::Spinlock<Vec<u8>>> =
                        try_with_context!(context, source.as_buffer(context)).clone();
                    let index = try_with_context!(context, index.as_integer(context));

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::BufferField { buffer_data: source_data, offset: index * 8, length: 16 }
                        )
                    );

                    (Ok(()), context)
                },
            ),
        ))
        .discard_result()
}

pub fn def_create_dword_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefCreateDWordField := 0x8a SourceBuf ByteIndex NameString
     * SourceBuf := TermArg => Buffer
     * ByteIndex := TermArg => Integer
     */
    opcode(opcode::DEF_CREATE_DWORD_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefCreateDWordField",
            term_arg().then(term_arg()).then(name_string()).map_with_context(
                |((source, index), name), context| {
                    let source_data: Arc<spinning_top::Spinlock<Vec<u8>>> =
                        try_with_context!(context, source.as_buffer(context)).clone();
                    let index = try_with_context!(context, index.as_integer(context));

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::BufferField { buffer_data: source_data, offset: index * 8, length: 32 }
                        )
                    );

                    (Ok(()), context)
                },
            ),
        ))
        .discard_result()
}

pub fn def_create_qword_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefCreateQWordField := 0x8f SourceBuf ByteIndex NameString
     * SourceBuf := TermArg => Buffer
     * ByteIndex := TermArg => Integer
     */
    opcode(opcode::DEF_CREATE_QWORD_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefCreateQWordField",
            term_arg().then(term_arg()).then(name_string()).map_with_context(
                |((source, index), name), context| {
                    let source_data: Arc<spinning_top::Spinlock<Vec<u8>>> =
                        try_with_context!(context, source.as_buffer(context)).clone();
                    let index = try_with_context!(context, index.as_integer(context));

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::BufferField { buffer_data: source_data, offset: index * 8, length: 64 }
                        )
                    );

                    (Ok(()), context)
                },
            ),
        ))
        .discard_result()
}

pub fn def_create_field<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefCreateField := ExtOpPrefix 0x13 SourceBuf BitIndex NumBits NameString
     * SourceBuf := TermArg => Buffer
     * BitIndex := TermArg => Integer
     * NumBits := TermArg => Integer
     */
    ext_opcode(opcode::EXT_DEF_CREATE_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefCreateField",
            term_arg().then(term_arg()).then(term_arg()).then(name_string()).map_with_context(
                |(((source, index), num_bits), name), context| {
                    let source_data: Arc<spinning_top::Spinlock<Vec<u8>>> =
                        try_with_context!(context, source.as_buffer(context)).clone();
                    let index = try_with_context!(context, index.as_integer(context));
                    let num_bits = try_with_context!(context, num_bits.as_integer(context));

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::BufferField { buffer_data: source_data, offset: index, length: num_bits }
                        )
                    );

                    (Ok(()), context)
                },
            ),
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
            DebugVerbosity::Scopes,
            "DefOpRegion",
            name_string().then(take()).then(term_arg()).then(term_arg()).map_with_context(
                |(((name, space), offset), length), context| {
                    let region = match space {
                        0x00 => RegionSpace::SystemMemory,
                        0x01 => RegionSpace::SystemIo,
                        0x02 => RegionSpace::PciConfig,
                        0x03 => RegionSpace::EmbeddedControl,
                        0x04 => RegionSpace::SMBus,
                        0x05 => RegionSpace::SystemCmos,
                        0x06 => RegionSpace::PciBarTarget,
                        0x07 => RegionSpace::IPMI,
                        0x08 => RegionSpace::GeneralPurposeIo,
                        0x09 => RegionSpace::GenericSerialBus,
                        space @ 0x80..=0xff => RegionSpace::OemDefined(space),
                        byte => return (Err(Propagate::Err(AmlError::InvalidRegionSpace(byte))), context),
                    };
                    let offset = match offset.as_integer(context) {
                        Ok(offset) => offset,
                        Err(err) => return (Err(Propagate::Err(err)), context),
                    };
                    let length = match length.as_integer(context) {
                        Ok(length) => length,
                        Err(err) => return (Err(Propagate::Err(err)), context),
                    };
                    let parent_device = match region {
                        RegionSpace::PciConfig | RegionSpace::IPMI | RegionSpace::GenericSerialBus => {
                            let resolved_path = try_with_context!(context, name.resolve(&context.current_scope));
                            Some(try_with_context!(context, resolved_path.parent()))
                        }
                        _ => None,
                    };

                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::OpRegion { region, offset, length, parent_device }
                        )
                    );
                    (Ok(()), context)
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
    let opregion_as_handle = name_string().map_with_context(|region_name, context| {
        /*
         * We search for the opregion that this field is referencing here as we already have the correct starting
         * scope. If we leave this to later, it becomes much harder as we also need to know the field's scope.
         */
        let (_, handle) =
            try_with_context!(context, context.namespace.search(&region_name, &context.current_scope));
        (Ok(handle), context)
    });

    ext_opcode(opcode::EXT_DEF_FIELD_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefField",
            pkg_length().then(opregion_as_handle).then(take()).feed(|((list_length, region_handle), flags)| {
                move |mut input: &'a [u8], mut context: &'c mut AmlContext| -> ParseResult<'a, 'c, ()> {
                    /*
                     * FieldList := Nothing | <FieldElement FieldList>
                     */
                    // TODO: can this pattern be expressed as a combinator
                    let mut current_offset = 0;
                    while list_length.still_parsing(input) {
                        let (new_input, new_context, field_length) =
                            field_element(region_handle, FieldFlags::new(flags), current_offset)
                                .parse(input, context)?;
                        input = new_input;
                        context = new_context;
                        current_offset += field_length;
                    }

                    Ok((input, context, ()))
                }
            }),
        ))
        .discard_result()
}

/// Parses a `FieldElement`. Takes the current offset within the field list, and returns the length
/// of the field element parsed.
pub fn field_element<'a, 'c>(
    region_handle: AmlHandle,
    flags: FieldFlags,
    current_offset: u64,
) -> impl Parser<'a, 'c, u64>
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
    // TODO: parse ConnectField and ExtendedAccessField

    /*
     * Reserved fields shouldn't actually be added to the namespace; they seem to show gaps in
     * the operation region that aren't used for anything.
     */
    let reserved_field =
        opcode(opcode::RESERVED_FIELD).then(pkg_length()).map(|((), length)| Ok(length.raw_length as u64));

    // TODO: work out what to do with an access field
    // let access_field = opcode(opcode::ACCESS_FIELD)
    //     .then(take())
    //     .then(take())
    //     .map_with_context(|(((), access_type), access_attrib), context| (Ok(    , context));

    let named_field = name_seg().then(pkg_length()).map_with_context(move |(name_seg, length), context| {
        try_with_context!(
            context,
            context.namespace.add_value_at_resolved_path(
                AmlName::from_name_seg(name_seg),
                &context.current_scope,
                AmlValue::Field {
                    region: region_handle,
                    flags,
                    offset: current_offset,
                    length: length.raw_length as u64,
                },
            )
        );

        (Ok(length.raw_length as u64), context)
    });

    choice!(reserved_field, named_field)
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
            DebugVerbosity::Scopes,
            "DefMethod",
            pkg_length()
                .then(name_string())
                .then(take())
                .feed(|((length, name), flags)| {
                    take_to_end_of_pkglength(length).map(move |code| Ok((name.clone(), flags, code)))
                })
                .map_with_context(|(name, flags, code), context| {
                    try_with_context!(
                        context,
                        context.namespace.add_value_at_resolved_path(
                            name,
                            &context.current_scope,
                            AmlValue::Method {
                                flags: MethodFlags::from(flags),
                                code: MethodCode::Aml(code.to_vec())
                            },
                        )
                    );
                    (Ok(()), context)
                }),
        ))
        .discard_result()
}

pub fn def_external<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefExternal = 0x15 NameString ObjectType ArgumentCount
     * ObjectType := ByteData
     * ArgumentCount := ByteData (0 to 7)
     */
    opcode(opcode::DEF_EXTERNAL_OP)
        .then(comment_scope(DebugVerbosity::Scopes, "DefExternal", name_string().then(take()).then(take())))
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
            DebugVerbosity::Scopes,
            "DefDevice",
            pkg_length()
                .then(name_string())
                .map_with_context(|(length, name), context| {
                    let resolved_name = try_with_context!(context, name.resolve(&context.current_scope));
                    try_with_context!(
                        context,
                        context.namespace.add_value(resolved_name.clone(), AmlValue::Device)
                    );
                    try_with_context!(
                        context,
                        context.namespace.add_level(resolved_name.clone(), LevelType::Device)
                    );

                    let previous_scope = context.current_scope.clone();
                    context.current_scope = resolved_name;

                    (Ok((length, previous_scope)), context)
                })
                .feed(|(length, previous_scope)| term_list(length).map(move |_| Ok(previous_scope.clone())))
                .map_with_context(|previous_scope, context| {
                    context.current_scope = previous_scope;
                    (Ok(()), context)
                }),
        ))
        .discard_result()
}

pub fn def_processor<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefProcessor := ExtOpPrefix 0x83 PkgLength NameString ProcID PblkAddress PblkLen TermList
     * ProcID := ByteData
     * PblkAddress := DWordData
     * PblkLen := ByteData
     */
    ext_opcode(opcode::EXT_DEF_PROCESSOR_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefProcessor",
            pkg_length()
                .then(name_string())
                .then(take())
                .then(take_u32())
                .then(take())
                .map_with_context(|((((pkg_length, name), proc_id), pblk_address), pblk_len), context| {
                    /*
                     * Legacy `Processor` objects contain data within themselves, and can also have sub-objects,
                     * so we add both a level for the sub-objects, and a value for the data.
                     */
                    let resolved_name = try_with_context!(context, name.resolve(&context.current_scope));
                    try_with_context!(
                        context,
                        context.namespace.add_level(resolved_name.clone(), LevelType::Processor)
                    );
                    try_with_context!(
                        context,
                        context.namespace.add_value(
                            resolved_name.clone(),
                            AmlValue::Processor { id: proc_id, pblk_address, pblk_len }
                        )
                    );
                    let previous_scope = context.current_scope.clone();
                    context.current_scope = resolved_name;

                    (Ok((previous_scope, pkg_length)), context)
                })
                .feed(move |(previous_scope, pkg_length)| {
                    term_list(pkg_length).map(move |_| Ok(previous_scope.clone()))
                })
                .map_with_context(|previous_scope, context| {
                    context.current_scope = previous_scope;
                    (Ok(()), context)
                }),
        ))
        .discard_result()
}

pub fn def_power_res<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefPowerRes := ExtOpPrefix 0x84 PkgLength NameString SystemLevel ResourceOrder TermList
     * SystemLevel := ByteData
     * ResourceOrder := WordData
     */
    ext_opcode(opcode::EXT_DEF_POWER_RES_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefPowerRes",
            pkg_length()
                .then(name_string())
                .then(take())
                .then(take_u16())
                .map_with_context(|(((pkg_length, name), system_level), resource_order), context| {
                    /*
                     * `PowerResource` objects contain data within themselves, and can also have sub-objects,
                     * so we add both a level for the sub-objects, and a value for the data.
                     */
                    let resolved_name = try_with_context!(context, name.resolve(&context.current_scope));
                    try_with_context!(
                        context,
                        context.namespace.add_level(resolved_name.clone(), LevelType::PowerResource)
                    );
                    try_with_context!(
                        context,
                        context.namespace.add_value(
                            resolved_name.clone(),
                            AmlValue::PowerResource { system_level, resource_order }
                        )
                    );
                    let previous_scope = context.current_scope.clone();
                    context.current_scope = resolved_name;

                    (Ok((previous_scope, pkg_length)), context)
                })
                .feed(move |(previous_scope, pkg_length)| {
                    term_list(pkg_length).map(move |_| Ok(previous_scope.clone()))
                })
                .map_with_context(|previous_scope, context| {
                    context.current_scope = previous_scope;
                    (Ok(()), context)
                }),
        ))
        .discard_result()
}

pub fn def_thermal_zone<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefThermalZone := ExtOpPrefix 0x85 PkgLength NameString TermList
     * TODO: we use this pattern a lot (move into scope, parse a term_list, move back out). Could we simplify into
     * just a `feed` by passing a scope into term_list?
     */
    ext_opcode(opcode::EXT_DEF_THERMAL_ZONE_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefThermalZone",
            pkg_length()
                .then(name_string())
                .map_with_context(|(pkg_length, name), context| {
                    let resolved_name = try_with_context!(context, name.resolve(&context.current_scope));
                    try_with_context!(
                        context,
                        context.namespace.add_value(resolved_name.clone(), AmlValue::ThermalZone)
                    );
                    try_with_context!(
                        context,
                        context.namespace.add_level(resolved_name.clone(), LevelType::ThermalZone)
                    );

                    let previous_scope = context.current_scope.clone();
                    context.current_scope = resolved_name;

                    (Ok((pkg_length, previous_scope)), context)
                })
                .feed(|(length, previous_scope)| term_list(length).map(move |_| Ok(previous_scope.clone())))
                .map_with_context(|previous_scope, context| {
                    context.current_scope = previous_scope;
                    (Ok(()), context)
                }),
        ))
        .discard_result()
}

pub fn def_mutex<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefMutex := ExtOpPrefix 0x01 NameString SyncFlags
     * SyncFlags := ByteData (where bits 0-3: SyncLevel
     *                              bits 4-7: Reserved)
     */
    ext_opcode(opcode::EXT_DEF_MUTEX_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefMutex",
            name_string().then(take()).map_with_context(|(name, sync_level), context| {
                try_with_context!(
                    context,
                    context.namespace.add_value_at_resolved_path(
                        name,
                        &context.current_scope,
                        AmlValue::Mutex { sync_level }
                    )
                );
                (Ok(()), context)
            }),
        ))
        .discard_result()
}

pub fn term_arg<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * TermArg := ExpressionOpcode | DataObject | ArgObj | LocalObj
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "TermArg",
        choice!(
            data_object(),
            arg_obj().map_with_context(|arg_num, context| {
                (Ok(try_with_context!(context, context.current_arg(arg_num)).clone()), context)
            }),
            local_obj().map_with_context(|local_num, context| {
                (Ok(try_with_context!(context, context.local(local_num)).clone()), context)
            }),
            expression_opcode()
        ),
    )
}

pub fn data_ref_object<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DataRefObject := DataObject | ObjectReference | DDBHandle
     */
    comment_scope(DebugVerbosity::AllScopes, "DataRefObject", choice!(data_object()))
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
    // TODO: this doesn't yet parse DefVarPackage
    comment_scope(DebugVerbosity::AllScopes, "DataObject", choice!(def_package(), computational_data()))
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
                None => return Err((input, context, Propagate::Err(AmlError::UnterminatedStringConstant))),
            };

            let string = String::from(match str::from_utf8(&input[0..nul_position]) {
                Ok(string) => string,
                Err(_) => return Err((input, context, Propagate::Err(AmlError::InvalidStringConstant))),
            });

            Ok((&input[(nul_position + 1)..], context, AmlValue::String(string)))
        };

        let (new_input, context, op) = take().parse(input, context)?;
        match op {
            opcode::BYTE_CONST => {
                take().map(|value| Ok(AmlValue::Integer(value as u64))).parse(new_input, context)
            }
            opcode::WORD_CONST => {
                take_u16().map(|value| Ok(AmlValue::Integer(value as u64))).parse(new_input, context)
            }
            opcode::DWORD_CONST => {
                take_u32().map(|value| Ok(AmlValue::Integer(value as u64))).parse(new_input, context)
            }
            opcode::QWORD_CONST => take_u64().map(|value| Ok(AmlValue::Integer(value))).parse(new_input, context),
            opcode::STRING_PREFIX => string_parser.parse(new_input, context),
            opcode::ZERO_OP => Ok((new_input, context, AmlValue::zero())),
            opcode::ONE_OP => Ok((new_input, context, AmlValue::one())),
            opcode::ONES_OP => Ok((new_input, context, AmlValue::ones())),

            _ => Err((input, context, Propagate::Err(AmlError::WrongParser))),
        }
    };

    comment_scope(
        DebugVerbosity::AllScopes,
        "ComputationalData",
        choice!(
            ext_opcode(opcode::EXT_REVISION_OP).map(|_| Ok(AmlValue::Integer(crate::AML_INTERPRETER_REVISION))),
            const_parser,
            def_buffer()
        ),
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_computational_data() {
        let mut context = make_test_context();
        check_ok_value!(
            computational_data().parse(&[0x00, 0x34, 0x12], &mut context),
            AmlValue::Integer(0),
            &[0x34, 0x12]
        );
        check_ok_value!(
            computational_data().parse(&[0x01, 0x18, 0xf3], &mut context),
            AmlValue::Integer(1),
            &[0x18, 0xf3]
        );
        check_ok_value!(
            computational_data().parse(&[0xff, 0x98, 0xc3], &mut context),
            AmlValue::Integer(u64::max_value()),
            &[0x98, 0xc3]
        );
        check_ok_value!(
            computational_data().parse(&[0x5b, 0x30], &mut context),
            AmlValue::Integer(crate::AML_INTERPRETER_REVISION),
            &[]
        );
        check_ok_value!(
            computational_data().parse(&[0x0a, 0xf3, 0x35], &mut context),
            AmlValue::Integer(0xf3),
            &[0x35]
        );
        check_ok_value!(
            computational_data().parse(&[0x0b, 0xf3, 0x35], &mut context),
            AmlValue::Integer(0x35f3),
            &[]
        );
        check_ok_value!(
            computational_data().parse(&[0x0c, 0xf3, 0x35, 0x12, 0x65, 0xff, 0x00], &mut context),
            AmlValue::Integer(0x651235f3),
            &[0xff, 0x00]
        );
        check_ok_value!(
            computational_data()
                .parse(&[0x0e, 0xf3, 0x35, 0x12, 0x65, 0xff, 0x00, 0x67, 0xde, 0x28], &mut context),
            AmlValue::Integer(0xde6700ff651235f3),
            &[0x28]
        );
        check_ok_value!(
            computational_data().parse(&[0x0d, b'A', b'B', b'C', b'D', b'\0', 0xff, 0xf5], &mut context),
            AmlValue::String(String::from("ABCD")),
            &[0xff, 0xf5]
        );
    }
}
