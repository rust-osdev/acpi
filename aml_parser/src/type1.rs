use crate::parser::{choice, comment_scope_verbose, Parser};

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
    comment_scope_verbose("Type1Opcode", choice!())
}
