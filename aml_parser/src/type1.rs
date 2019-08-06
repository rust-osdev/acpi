use crate::{
    opcode::{self, opcode},
    parser::{choice, comment_scope, comment_scope_verbose, take_to_end_of_pkglength, ParseResult, Parser},
    pkg_length::{pkg_length, PkgLength},
    term_object::{term_arg, term_list},
    AmlError,
};

/// Type 1 opcodes return a value and so can be used in expressions.
pub fn type1_opcode<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * Type1Opcode := DefBreak | DefBreakPoint | DefContinue | DefFatal | DefIfElse | DefLoad | DefNoop |
     *                DefNotify | DefRelease | DefReset | DefReturn | DefSignal | DefSleep | DefStall |
     *                DefWhile
     */
    comment_scope_verbose("Type1Opcode", choice!(def_if_else(), def_return()))
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
            "DefIfElse",
            pkg_length()
                .then(term_arg())
                .feed(|(length, predicate_arg)| {
                    take_to_end_of_pkglength(length)
                        .map(move |then_branch| Ok((predicate_arg.as_bool()?, then_branch)))
                })
                .then(choice!(
                    opcode(opcode::DEF_ELSE_OP)
                        .then(comment_scope_verbose(
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

                    match term_list(PkgLength::from_raw_length(branch, branch.len() as u32))
                        .parse(branch, context)
                    {
                        Ok((_, context, result)) => (Ok(result), context),
                        Err((_, context, err)) => (Err(err), context),
                    }
                }),
        ))
        .discard_result()
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
        .then(comment_scope_verbose(
            "DefReturn",
            term_arg().map(|return_arg| -> Result<(), AmlError> {
                /*
                 * To return a value, we want to halt execution of the method and propagate the
                 * return value all the way up to the start of the method invocation. To do this,
                 * we emit a special error that is intercepted during method invocation and turned
                 * into a valid result.
                 */
                Err(AmlError::Return(return_arg))
            }),
        ))
        .discard_result()
}
