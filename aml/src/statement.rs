use crate::{
    opcode::{self, ext_opcode, opcode},
    parser::{
        choice,
        comment_scope,
        extract,
        id,
        take,
        take_to_end_of_pkglength,
        take_u32,
        try_with_context,
        ParseResult,
        Parser,
        Propagate,
    },
    pkg_length::{pkg_length, PkgLength},
    term_object::{term_arg, term_list},
    AmlContext,
    AmlError,
    DebugVerbosity,
};

pub fn statement_opcode<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * StatementOpcode := DefBreak | DefBreakPoint | DefContinue | DefFatal | DefIfElse | DefLoad | DefNoop |
     *                    DefNotify | DefRelease | DefReset | DefReturn | DefSignal | DefSleep | DefStall | DefWhile
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "StatementOpcode",
        choice!(
            def_break(),
            def_breakpoint(),
            def_continue(),
            def_fatal(),
            def_if_else(),
            def_noop(),
            def_return(),
            def_while()
        ),
    )
}

fn def_break<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefBreak := 0xa5
     */
    opcode(opcode::DEF_BREAK_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefBreak",
            id().map(|()| -> Result<(), Propagate> { Err(Propagate::Break) }),
        ))
        .discard_result()
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

fn def_continue<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefContinue := 0x9f
     */
    opcode(opcode::DEF_CONTINUE_OP)
        .then(comment_scope(
            DebugVerbosity::AllScopes,
            "DefContinue",
            id().map(|()| -> Result<(), Propagate> { Err(Propagate::Continue) }),
        ))
        .discard_result()
}

fn def_fatal<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefFatal := ExtOpPrefix 0x32 FatalType FatalCode FatalArg
     * FatalType := ByteData
     * FatalCode := DWordData
     * FatalArg := TermArg => Integer
     */
    ext_opcode(opcode::EXT_DEF_FATAL_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefFatal",
            take().then(take_u32()).then(term_arg()).map_with_context(
                |((fatal_type, fatal_code), fatal_arg), context| -> (Result<(), Propagate>, &'c mut AmlContext) {
                    let fatal_arg = try_with_context!(context, fatal_arg.as_integer(context));
                    context.handler.handle_fatal_error(fatal_type, fatal_code, fatal_arg);
                    (Err(Propagate::Err(AmlError::FatalError)), context)
                },
            ),
        ))
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
                    // TODO: can this be `id().map(&[])`?
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

fn def_while<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DefWhile := 0xa2 PkgLength Predicate TermList
     * Predicate := TermArg => Integer (0 = false, >0 = true)
     *
     * Parsing this does something a little unusual - it 'extracts' the predicate when it's first parsed, which
     * allows us to reevaluate it to see if we should break out of the while yet. This is required, to make sure
     * we're observing changes to the context between the iterations of the loop.
     */
    opcode(opcode::DEF_WHILE_OP)
        .then(comment_scope(
            DebugVerbosity::Scopes,
            "DefWhile",
            pkg_length()
                .then(extract(term_arg()))
                .feed(move |(length, (first_predicate, predicate_stream))| {
                    take_to_end_of_pkglength(length)
                        .map(move |body| Ok((first_predicate.clone(), predicate_stream, body)))
                })
                .map_with_context(|(first_predicate, predicate_stream, body), mut context| {
                    if !try_with_context!(context, first_predicate.as_bool()) {
                        return (Ok(()), context);
                    }

                    loop {
                        match term_list(PkgLength::from_raw_length(body, body.len() as u32).unwrap())
                            .parse(body, context)
                        {
                            Ok((_, new_context, result)) => {
                                context = new_context;
                            }
                            Err((_, new_context, Propagate::Break)) => {
                                context = new_context;
                                break;
                            }
                            Err((_, new_context, Propagate::Continue)) => {
                                // We don't need to do anything special here - the `Propagate::Continue` bubbles
                                // up, and then we can just move on to checking the predicate for the next
                                // iteration.
                                context = new_context;
                            }
                            Err((_, context, err)) => return (Err(err), context),
                        }

                        // Reevaluate the predicate to see if we should break out of the loop yet
                        let predicate =
                            match comment_scope(DebugVerbosity::AllScopes, "WhilePredicate", term_arg())
                                .parse(predicate_stream, context)
                            {
                                Ok((_, new_context, result)) => {
                                    context = new_context;
                                    try_with_context!(context, result.as_bool())
                                }
                                Err((_, context, err)) => return (Err(err), context),
                            };

                        if !predicate {
                            break;
                        }
                    }

                    (Ok(()), context)
                }),
        ))
        .discard_result()
}
