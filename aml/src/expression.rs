use crate::{
    name_object::{name_string, super_name, target},
    opcode::{self, opcode},
    parser::{choice, comment_scope, n_of, take, take_to_end_of_pkglength, try_with_context, Parser, Propagate},
    pkg_length::pkg_length,
    term_object::{data_ref_object, term_arg},
    value::{AmlType, AmlValue, Args},
    AmlError,
    DebugVerbosity,
};
use alloc::{
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};
use core::{cmp::Ordering, convert::TryInto, mem, ops::Deref};

pub fn expression_opcode<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * ExpressionOpcode := DefAquire | DefAdd | DefAnd | DefBuffer | DefConcat | DefConcatRes |
     *                     DefCondRefOf | DefCopyObject | DefDecrement | DefDerefOf | DefDivide |
     *                     DefFindSetLeftBit | DefFindSetRightBit | DefFromBCD | DefIncrement | DefIndex |
     *                     DefLAnd | DefLEqual | DefLGreater | DefLGreaterEqual | DefLLess | DefLLessEqual |
     *                     DefMid | DefLNot | DefLNotEqual | DefLoad | DefLoadTable | DefLOr | DefMatch | DefMod |
     *                     DefMultiply | DefNAnd | DefNOr | DefNot | DefObjectType | DefOr | DefPackage |
     *                     DefVarPackage | DefRefOf | DefShiftLeft | DefShiftRight | DefSizeOf | DefStore |
     *                     DefSubtract | DefTimer | DefToBCD | DefToBuffer | DefToDecimalString |
     *                     DefToHexString | DefToInteger | DefToString | DefWait | DefXOr | MethodInvocation
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "ExpressionOpcode",
        choice!(
            def_add(),
            def_and(),
            def_buffer(),
            def_concat(),
            def_concat_res(),
            def_increment(),
            def_decrement(),
            def_l_equal(),
            def_l_greater(),
            def_l_greater_equal(),
            def_l_less(),
            def_l_less_equal(),
            def_l_not_equal(),
            def_l_or(),
            def_mid(),
            def_package(),
            def_shift_left(),
            def_shift_right(),
            def_store(),
            def_to_integer(),
            method_invocation() // XXX: this must always appear last. See how we have to parse it to see why.
        ),
    )
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
        .map(|((), buffer)| Ok(AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(buffer)))))
}

pub fn def_concat<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefConcat := 0x73 Data Data Target
     * Data := TermArg => ComputationalData
     */
    opcode(opcode::DEF_CONCAT_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefConcat",
            term_arg().then(term_arg()).then(target()).map_with_context(|((left, right), target), context| {
                let result = match left.as_concat_type() {
                    AmlValue::Integer(left) => {
                        let right = try_with_context!(context, right.as_integer(context));

                        let mut buffer = Vec::with_capacity(mem::size_of::<u64>() * 2);
                        buffer.extend_from_slice(&left.to_le_bytes());
                        buffer.extend_from_slice(&right.to_le_bytes());

                        AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(buffer)))
                    }
                    AmlValue::Buffer(left) => {
                        let mut new: Vec<u8> = left.lock().deref().clone();
                        new.extend(try_with_context!(context, right.as_buffer(context)).lock().iter());
                        AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(new)))
                    }
                    AmlValue::String(left) => {
                        let right = match right.as_concat_type() {
                            AmlValue::String(right) => right,
                            AmlValue::Integer(_) => try_with_context!(context, right.as_string(context)),
                            AmlValue::Buffer(_) => try_with_context!(context, right.as_string(context)),
                            _ => panic!("Invalid type returned from `as_concat_type`"),
                        };
                        AmlValue::String(left + &right)
                    }
                    _ => panic!("Invalid type returned from `as_concat_type`"),
                };

                try_with_context!(context, context.store(target, result.clone()));
                (Ok(result), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

pub fn def_concat_res<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefConcatRes := 0x84 BufData BufData Target
     * BufData := TermArg => Buffer
     */
    opcode(opcode::DEF_CONCAT_RES_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefConcatRes",
            term_arg().then(term_arg()).then(target()).map_with_context(|((left, right), target), context| {
                let left = try_with_context!(context, left.as_buffer(context));
                let right = try_with_context!(context, right.as_buffer(context));

                let left_len = left.lock().len();
                let right_len = right.lock().len();

                if left_len == 1 || right_len == 1 {
                    return (Err(Propagate::Err(AmlError::ResourceDescriptorTooShort)), context);
                }

                /*
                 * `left` and `right` are buffers of resource descriptors, which we're trying to concatenate into a
                 * new, single buffer containing all of the descriptors from the source buffers. We need to strip
                 * off the end tags (2 bytes from each buffer), and then add our own end tag.
                 *
                 * XXX: either buffer may be empty (contains no tags), and so our arithmetic has to be careful.
                 */
                let result = {
                    let mut result =
                        Vec::with_capacity(left_len.saturating_sub(2) + right_len.saturating_sub(2) + 2);
                    let left_contents = left.lock();
                    let right_contents = right.lock();
                    result.extend_from_slice(if left_len == 0 { &[] } else { &left_contents[..(left_len - 2)] });
                    result.extend_from_slice(if right_len == 0 {
                        &[]
                    } else {
                        &right_contents[..(right_len - 2)]
                    });

                    /*
                     * Construct a new end tag, including a new checksum:
                     *    | Bits        | Field             | Value                     |
                     *    |-------------|-------------------|---------------------------|
                     *    | 0-2         | Length - n bytes  | 1 (for checksum)          |
                     *    | 3-6         | Small item type   | 0x0f = end tag descriptor |
                     *    | 7           | 0 = small item    | 0                         |
                     */
                    result.push(0b01111001);
                    result.push(
                        result.iter().fold(0u8, |checksum, byte| checksum.wrapping_add(*byte)).wrapping_neg(),
                    );

                    AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(result)))
                };

                try_with_context!(context, context.store(target, result.clone()));
                (Ok(result), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn def_increment<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefIncrement := 0x75 SuperName
     */
    opcode(opcode::DEF_INCREMENT_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefIncrement",
            super_name().map_with_context(|addend, context| {
                let value = try_with_context!(context, context.read_target(&addend));
                let value = try_with_context!(context, value.as_integer(context));
                let new_value = AmlValue::Integer(value + 1);
                try_with_context!(context, context.store(addend, new_value.clone()));
                (Ok(new_value), context)
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn def_decrement<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefDecrement := 0x76 SuperName
     */
    opcode(opcode::DEF_DECREMENT_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefDecrement",
            super_name().map_with_context(|minuend, context| {
                let value = try_with_context!(context, context.read_target(&minuend));
                let value = try_with_context!(context, value.as_integer(context));
                let new_value = AmlValue::Integer(value - 1);
                try_with_context!(context, context.store(minuend, new_value.clone()));
                (Ok(new_value), context)
            }),
        ))
        .map(|((), result)| Ok(result))
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

fn def_mid<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefMid := 0x9e MidObj TermArg TermArg Target
     * MidObj := TermArg => Buffer | String
     */
    opcode(opcode::DEF_MID_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefMid",
            term_arg().then(term_arg()).then(term_arg()).then(target()).map_with_context(
                |(((source, index), length), target), context| {
                    let index = try_with_context!(context, index.as_integer(context)) as usize;
                    let length = try_with_context!(context, length.as_integer(context)) as usize;

                    let result = try_with_context!(
                        context,
                        match source {
                            AmlValue::Buffer(bytes) => {
                                let foo = bytes.lock();
                                if index >= foo.len() {
                                    Ok(AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(vec![]))))
                                } else if (index + length) >= foo.len() {
                                    Ok(AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(
                                        foo[index..].to_vec(),
                                    ))))
                                } else {
                                    Ok(AmlValue::Buffer(Arc::new(spinning_top::Spinlock::new(
                                        foo[index..(index + length)].to_vec(),
                                    ))))
                                }
                            }
                            /*
                             * XXX: The spec conflates characters and bytes, so we effectively ignore unicode and do
                             * this bytewise, to hopefully match other implementations.
                             */
                            AmlValue::String(string) => {
                                if index >= string.len() {
                                    Ok(AmlValue::String(String::new()))
                                } else if (index + length) >= string.len() {
                                    Ok(AmlValue::String(string[index..].to_string()))
                                } else {
                                    Ok(AmlValue::String(string[index..(index + length)].to_string()))
                                }
                            }
                            _ => Err(AmlError::TypeCannotBeSliced(source.type_of())),
                        }
                    );

                    try_with_context!(context, context.store(target, result.clone()));
                    (Ok(result), context)
                },
            ),
        ))
        .map(|((), result)| Ok(result))
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

fn def_to_integer<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * DefToInteger := 0x99 Operand Target
     * Operand := TermArg
     */
    opcode(opcode::DEF_TO_INTEGER_OP)
        .then(comment_scope(DebugVerbosity::AllScopes, "DefToInteger", term_arg().then(target())))
        .map_with_context(|((), (operand, target)), context| {
            let result = match operand {
                AmlValue::Integer(value) => AmlValue::Integer(value),
                AmlValue::Buffer(data) => {
                    AmlValue::Integer(try_with_context!(context, AmlValue::Buffer(data).as_integer(context)))
                }
                AmlValue::String(string) => AmlValue::Integer(try_with_context!(
                    context,
                    if string.starts_with("0x") {
                        u64::from_str_radix(string.trim_start_matches("0x"), 16)
                    } else {
                        string.parse::<u64>()
                    }
                    .map_err(|_| AmlError::IncompatibleValueConversion {
                        current: AmlType::String,
                        target: AmlType::Integer,
                    })
                )),
                _ => {
                    return (
                        Err(Propagate::Err(AmlError::IncompatibleValueConversion {
                            current: operand.type_of(),
                            target: AmlType::Integer,
                        })),
                        context,
                    )
                }
            };
            try_with_context!(context, context.store(target, result.clone()));
            (Ok(result), context)
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
                        let args = try_with_context!(context, Args::from_list(arg_list));
                        let result = context.invoke_method(&path, args);
                        (Ok(try_with_context!(context, result)), context)
                    } else {
                        (Ok(try_with_context!(context, context.namespace.get_by_path(&path)).clone()), context)
                    }
                })
            }),
    )
}
