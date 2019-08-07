use crate::{
    name_object::name_string,
    opcode::{self, opcode},
    parser::{choice, comment_scope_verbose, try_with_context, ParseResult, Parser},
    term_object::term_arg,
    value::AmlValue,
    AmlError,
};
use alloc::boxed::Box;

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
    comment_scope_verbose("Type2Opcode", choice!(def_l_equal(), method_invocation()))
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
        .then(comment_scope_verbose(
            "DefLEqual",
            term_arg().then(term_arg()).map(|(left_arg, right_arg)| {
                Ok(AmlValue::Boolean(left_arg.as_integer()? == right_arg.as_integer()?))
            }),
        ))
        .map(|((), result)| Ok(result))
}

fn method_invocation<'a, 'c>() -> impl Parser<'a, 'c, AmlValue>
where
    'c: 'a,
{
    /*
     * MethodInvocation := NameString TermArgList
     *
     * MethodInvocation is the worst of the AML structures, because you're meant to figure out how
     * much you're meant to parse using the name of the method (by knowing from its definition how
     * how many arguments it takes). However, the definition of a method can in theory appear after
     * an invocation of that method, and so parsing them properly can be very difficult.
     * NOTE: We don't support the case of the definition appearing after the invocation.
     *
     * It's also not clear whether things that aren't methods can be "invoked" using
     * MethodInvocation with 0 arguments. It seems that references to DefNames can be encoded using
     * MethodInvocation, at least, and should just be looked up.
     */
    comment_scope_verbose(
        "MethodInvocation",
        name_string()
            .map_with_context(move |name, context| {
                let object =
                    try_with_context!(context, context.namespace.search(&name, &context.current_scope))
                        .clone();
                (Ok(object), context)
            })
            .feed(|object| {
                move |input: &'a [u8], context| -> ParseResult<'a, 'c, AmlValue> {
                    match object.clone() {
                        AmlValue::Name(boxed_value) => Ok((input, context, unbox(boxed_value))),

                        AmlValue::Method { flags, ref code } => {
                            // TODO: before we do this, we need to restructure the structures to allow us
                            // to execute control methods from inside other control methods
                            // TODO
                            unimplemented!()
                        }

                        _ => Err((input, context, AmlError::IncompatibleValueConversion)),
                    }
                }
            }),
    )
}

/// An unfortunate helper method to unbox an owned, boxed value. `*x` is special-cased for `Box`
/// here, but the compiler needs the type signature of the method to figure it out.
fn unbox<T>(x: Box<T>) -> T {
    *x
}
