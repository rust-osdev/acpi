use crate::{
    name_object::{name_string, super_name, target},
    opcode::{self, opcode},
    parser::{
        choice,
        comment_scope,
        make_parser_concrete,
        n_of,
        take,
        take_to_end_of_pkglength,
        try_with_context,
        Parser,
        Propagate,
    },
    pkg_length::pkg_length,
    term_object::{data_ref_object, term_arg},
    value::{AmlValue, Args},
    AmlError,
    DebugVerbosity,
};
use alloc::{vec, vec::Vec};
use core::{cmp::Ordering, convert::TryInto};

/// Type 2 opcodes return a value and so can be used in expressions.
pub fn type2_opcode<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * Type2Opcode := DefAquire | DefAdd | DefAnd | DefBuffer | DefConcat | DefConcatRes |
     *                DefCondRefOf | DefCopyObject | DefDecrement | DefDerefOf | DefDivide |
     *                DefFindSetLeftBit | DefFindSetRightBit | DefFromBCD | DefIncrement | DefIndex |
     *                DefLAnd | DefLEqual | DefLGreater | DefLGreaterEqual | DefLLess | DefLLessEqual |
     *                DefMid | DefLNot | DefLNotEqual | DefLoadTable | DefLOr | DefMatch | DefMod |
     *                DefMultiply | DefNAnd | DefNOr | DefNot | DefObjectType | DefOr | DefPackage |
     *                DefVarPackage | DefRefOf | DefShiftLeft | DefShitRight | DefSizeOf | DefStore |
     *                DefSubtract | DefTimer | DefToBCD | DefToBuffer | DefToDecimalString |
     *                DefToHexString | DefToInteger | DefToString | DefWait | DefXOr | MethodInvocation
     */
    make_parser_concrete!(comment_scope(
        DebugVerbosity::AllScopes,
        "Type2Opcode",
        choice!(
            def_add(),
            def_and(),
            def_buffer(),
            def_l_equal(),
            def_l_greater(),
            def_l_greater_equal(),
            def_l_less(),
            def_l_less_equal(),
            def_l_not_equal(),
            def_l_or(),
            def_package(),
            def_shift_left(),
            def_shift_right(),
            def_store(),
            method_invocation() // XXX: this must always appear last. See how we have to parse it to see why.
        ),
    ))
}

pub fn def_add<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefAdd := 0x72 Operand Operand Target
     * Operand := TermArg => Integer
     */
    opcode(opcode::DEF_ADD_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefAdd",
            term_arg().then(term_arg()).then(target()).map_with_context(
                |((left_arg, right_arg), target), context| {
                    let left = try_with_context!(context, left_arg.as_integer(context));
                    let right = try_with_context!(context, right_arg.as_integer(context));
                    let result = AmlValue::Integer(left.wrapping_add(right));

                    try_with_context!(context, context.store(target, result.clone()));
                    (Ok(result), context)
                },
            ),
        ))
        .map(|((), result)| Ok(result))
}

pub fn def_and<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefAnd := 0x7b Operand Operand Target
     * Operand := TermArg => Integer
     */
    opcode(opcode::DEF_AND_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefAnd",
            term_arg().then(term_arg()).then(target()).map_with_context(
                |((left_arg, right_arg), target), context| {
                    let left = try_with_context!(context, left_arg.as_integer(context));
                    let right = try_with_context!(context, right_arg.as_integer(context));
                    let result = AmlValue::Integer(left & right);

                    try_with_context!(context, context.store(target, result.clone()));
                    (Ok(result), context)
                },
            ),
        ))
        .map(|((), result)| Ok(result))
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
     *
     * Uninitialized elements are initialized to zero.
     */
    opcode(opcode::DEF_BUFFER_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefBuffer",
            pkg_length().then(term_arg()).feed(|(pkg_length, buffer_size)| {
                take_to_end_of_pkglength(pkg_length).map_with_context(move |bytes, context| {
                    let buffer_size = try_with_context!(context, buffer_size.as_integer(context)) as usize;

                    if buffer_size < bytes.len() {
                        return (Err(Propagate::Err(AmlError::MalformedBuffer)), context);
                    }

                    let mut buffer = vec![0; buffer_size];
                    (&mut buffer[0..bytes.len()]).copy_from_slice(bytes);
                    (Ok(buffer), context)
                })
            }),
        ))
        .map(|((), buffer)| Ok(AmlValue::Buffer(buffer)))
}

fn def_l_or<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLOr := 0x91 Operand Operand
     * Operand := TermArg => Integer
     */
    opcode(opcode::DEF_L_OR_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLOr",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let left = try_with_context!(context, left_arg.as_bool());
                let right = try_with_context!(context, right_arg.as_bool());
                (Ok(AmlValue::Boolean(left || right)), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn def_l_equal<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLEqual := 0x93 Operand Operand
     * Operand := TermArg => Integer
     */
    opcode(opcode::DEF_L_EQUAL_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLEqual",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let ord = try_with_context!(context, left_arg.cmp(right_arg, context));
                (Ok(AmlValue::Boolean(ord == Ordering::Equal)), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn def_l_greater<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLGreater := 0x94 Operand Operand
     */
    opcode(opcode::DEF_L_GREATER_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLGreater",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let ord = try_with_context!(context, left_arg.cmp(right_arg, context));
                (Ok(AmlValue::Boolean(ord == Ordering::Greater)), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn def_l_greater_equal<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLGreaterEqual := LNotOp(0x92) LLessOp(0x95) Operand Operand
     */
    opcode(opcode::DEF_L_NOT_OP)
        .then(opcode(opcode::DEF_L_LESS_OP))
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLGreaterEqual",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let ord = try_with_context!(context, left_arg.cmp(right_arg, context));
                (Ok(AmlValue::Boolean(ord != Ordering::Less)), context)
            }),
        ))
        .map(|(((), ()), result)| Ok(result))
}

fn def_l_less<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLLess := 0x95 Operand Operand
     */
    opcode(opcode::DEF_L_LESS_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLLess",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let ord = try_with_context!(context, left_arg.cmp(right_arg, context));
                (Ok(AmlValue::Boolean(ord == Ordering::Less)), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn def_l_less_equal<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLLessEqual := LNotOp(0x92) LGreaterOp(0x94) Operand Operand
     */
    opcode(opcode::DEF_L_NOT_OP)
        .then(opcode(opcode::DEF_L_GREATER_OP))
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLLessEqual",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let ord = try_with_context!(context, left_arg.cmp(right_arg, context));
                (Ok(AmlValue::Boolean(ord != Ordering::Greater)), context)
            }),
        ))
        .map(|(((), ()), result)| Ok(result))
}

fn def_l_not_equal<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefLNotEqual := LNotOp(0x92) LEqualOp(0x93) Operand Operand
     */
    opcode(opcode::DEF_L_NOT_OP)
        .then(opcode(opcode::DEF_L_EQUAL_OP))
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefLNotEqual",
            term_arg().then(term_arg()).map_with_context(|(left_arg, right_arg), context| {
                let ord = try_with_context!(context, left_arg.cmp(right_arg, context));
                (Ok(AmlValue::Boolean(ord != Ordering::Equal)), context)
            }),
        ))
        .map(|(((), ()), result)| Ok(result))
}

pub fn def_package<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefPackage := 0x12 PkgLength NumElements PackageElementList
     * NumElements := ByteData
     * PackageElementList := Nothing | <PackageElement PackageElementList>
     * PackageElement := DataRefObject | NameString
     */
    opcode(opcode::DEF_PACKAGE_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefPackage",
            pkg_length().then(take()).feed(|(pkg_length, num_elements)| {
                move |mut input, mut context| {
                    let mut package_contents = Vec::new();

                    while pkg_length.still_parsing(input) {
                        let (new_input, new_context, value) = package_element().parse(input, context)?;
                        input = new_input;
                        context = new_context;

                        package_contents.push(value);
                    }

                    if package_contents.len() != num_elements as usize {
                        return Err((input, context, Propagate::Err(AmlError::MalformedPackage)));
                    }

                    Ok((input, context, AmlValue::Package(package_contents)))
                }
            }),
        ))
        .map(|((), package)| Ok(package))
}

pub fn package_element<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    choice!(data_ref_object(), name_string().map(|string| Ok(AmlValue::String(string.as_string()))))
}

fn def_shift_left<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefShiftLeft := 0x79 Operand ShiftCount Target
     * Operand := TermArg => Integer
     * ShiftCount := TermArg => Integer
     */
    opcode(opcode::DEF_SHIFT_LEFT)
        .then(comment_scope(DebugVerbosity::Scopes, "DefShiftLeft", term_arg().then(term_arg()).then(target())))
        .map_with_context(|((), ((operand, shift_count), target)), context| {
            let operand = try_with_context!(context, operand.as_integer(context));
            let shift_count = try_with_context!(context, shift_count.as_integer(context));
            let shift_count =
                try_with_context!(context, shift_count.try_into().map_err(|_| AmlError::InvalidShiftLeft));

            let result = AmlValue::Integer(try_with_context!(
                context,
                operand.checked_shl(shift_count).ok_or(AmlError::InvalidShiftLeft)
            ));

            try_with_context!(context, context.store(target, result.clone()));
            (Ok(result), context)
        })
}

fn def_shift_right<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefShiftRight := 0x7a Operand ShiftCount Target
     * Operand := TermArg => Integer
     * ShiftCount := TermArg => Integer
     */
    opcode(opcode::DEF_SHIFT_RIGHT)
        .then(comment_scope(DebugVerbosity::Scopes, "DefShiftRight", term_arg().then(term_arg()).then(target())))
        .map_with_context(|((), ((operand, shift_count), target)), context| {
            let operand = try_with_context!(context, operand.as_integer(context));
            let shift_count = try_with_context!(context, shift_count.as_integer(context));
            let shift_count =
                try_with_context!(context, shift_count.try_into().map_err(|_| AmlError::InvalidShiftRight));

            let result = AmlValue::Integer(try_with_context!(
                context,
                operand.checked_shr(shift_count).ok_or(AmlError::InvalidShiftRight)
            ));

            try_with_context!(context, context.store(target, result.clone()));
            (Ok(result), context)
        })
}

fn def_store<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefStore := 0x70 TermArg SuperName
     *
     * Implicit conversion is only applied when the destination target is a `Name` - not when we
     * are storing into a method local or argument (these stores are semantically identical to
     * CopyObject). We must also make sure to return a copy of the data that is in the destination
     * after the store (as opposed to the data we think we put into it), because some stores can
     * alter the data during the store.
     */
    opcode(opcode::DEF_STORE_OP)
        .then(comment_scope(DebugVerbosity::Scopes, "DefStore", term_arg().then(super_name())))
        .map_with_context(|((), (value, target)), context| {
            (Ok(try_with_context!(context, context.store(target, value))), context)
        })
}

fn method_invocation<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * MethodInvocation := NameString TermArgList
     *
     * MethodInvocation is the worst of the AML structures, because you're meant to figure out how much you're
     * meant to parse using the name of the method (by knowing from its definition how how many arguments it
     * takes). However, the definition of a method can in theory appear after an invocation of that method, and
     * so parsing them properly can be very difficult.
     * NOTE: We don't support the case of the definition appearing after the invocation.
     */
    comment_scope(
        DebugVerbosity::Scopes,
        "MethodInvocation",
        name_string()
            .map_with_context(move |name, context| {
                let (full_path, handle) =
                    try_with_context!(context, context.namespace.search(&name, &context.current_scope)).clone();

                /*
                 * `None` if the path is not a method and so doesn't have arguments, or `Some(the number of
                 * arguments to parse)` if it's a method.
                 */
                let num_args = if let AmlValue::Method { flags, .. } =
                    try_with_context!(context, context.namespace.get(handle))
                {
                    Some(flags.arg_count())
                } else {
                    None
                };
                (Ok((full_path, num_args)), context)
            })
            .feed(|(path, num_args)| {
                n_of(term_arg(), num_args.unwrap_or(0) as usize).map_with_context(move |arg_list, context| {
                    if num_args.is_some() {
                        let result = context.invoke_method(&path, Args::from_list(arg_list));
                        (Ok(try_with_context!(context, result)), context)
                    } else {
                        (Ok(try_with_context!(context, context.namespace.get_by_path(&path)).clone()), context)
                    }
                })
            }),
    )
}
