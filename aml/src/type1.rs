use crate::{
    opcode::{self, opcode},
    parser::{choice, comment_scope, id, take_to_end_of_pkglength, ParseResult, Parser, Propagate},
    pkg_length::{pkg_length, PkgLength},
    term_object::{term_arg, term_list},
    DebugVerbosity,
};

/// Type 1 opcodes do not return a value and so can't be used in expressions.
pub fn type1_opcode<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * Type1Opcode := DefBreak | DefBreakPoint | DefContinue | DefFatal | DefIfElse | DefLoad | DefNoop |
     *                DefNotify | DefRelease | DefReset | DefReturn | DefSignal | DefSleep | DefStall |
     *                DefWhile
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "Type1Opcode",
        choice!(def_breakpoint(), def_if_else(), def_noop(), def_return()),
    )
}

fn def_breakpoint<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefBreakPoint := 0xcc
     * TODO: there is no debugger, so this doesn't do anything. If there was, this should stop execution and enter
     * the AML debugger.
     */
    opcode(opcode::DEF_BREAKPOINT_OP)
        .then(comment_scope(DebugVerbosity::AllScopes, "DefBreakPoint", id()))
        .discard_result()
}

fn def_if_else<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefIfElse := 0xa0 PkgLength Predicate TermList DefElse
     * Predicate := TermArg => Integer (0 = false, >0 = true)
     * DefElse := Nothing | <0xa1 PkgLength TermList>
     */
    opcode(opcode::DEF_IF_ELSE_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefIfElse",
            pkg_length()
                .then(term_arg())
                .feed(|(length, predicate_arg)| {
                    take_to_end_of_pkglength(length)
                        .map(move |then_branch| Ok((predicate_arg.as_bool()?, then_branch)))
                })
                .then(choice!(
                    opcode(opcode::DEF_ELSE_OP)
                        .then(comment_scope(
                            DebugVerbosity::AllScopes,
                            "DefElse",
                            pkg_length().feed(|length| take_to_end_of_pkglength(length))
                        ))
                        .map(|((), else_branch): ((), &[u8])| Ok(else_branch)),
                    |input, context| -> ParseResult<'a, 'c, &[u8]> {
                        /*
                         * This path parses an DefIfElse that doesn't have an else branch. We simply
                         * return an empty slice, so if the predicate is false, we don't execute
                         * anything.
                         */
                        Ok((input, context, &[]))
                    }
                ))
                .map_with_context(|((predicate, then_branch), else_branch), context| {
                    let branch = if predicate { then_branch } else { else_branch };

                    match term_list(PkgLength::from_raw_length(branch, branch.len() as u32).unwrap())
                        .parse(branch, context)
                    {
                        Ok((_, context, result)) => (Ok(result), context),
                        Err((_, context, err)) => (Err(err), context),
                    }
                }),
        ))
        .discard_result()
}

fn def_noop<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefNoop := 0xa3
     */
    opcode(opcode::DEF_NOOP_OP).then(comment_scope(DebugVerbosity::AllScopes, "DefNoop", id())).discard_result()
}

fn def_return<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefReturn := 0xa4 ArgObject
     * ArgObject := TermArg => DataRefObject
     */
    opcode(opcode::DEF_RETURN_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefReturn",
            term_arg().map(|return_arg| -> Result<(), Propagate> {
                /*
                 * To return a value, we want to halt execution of the method and propagate the
                 * return value all the way up to the start of the method invocation. To do this,
                 * we emit a special error that is intercepted during method invocation and turned
                 * into a valid result.
                 */
                Err(Propagate::Return(return_arg))
            }),
        ))
        .discard_result()
}
