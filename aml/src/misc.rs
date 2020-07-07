use crate::{
    opcode::{self, ext_opcode, opcode},
    parser::{choice, comment_scope, id, Parser},
    DebugVerbosity,
};

pub fn debug_obj<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    /*
     * DebugObj := ExtOpPrefix 0x31
     */
    ext_opcode(opcode::EXT_DEBUG_OP)
}

/// Takes a value between `0` and `7`, where 0 represents `Local0` etc.
pub type LocalNum = u8;

pub fn local_obj<'a, 'c>() -> impl Parser<'a, 'c, LocalNum>
where
    'c: 'a,
{
    /*
     * LocalObj := Local0Op | Local1Op | Local2Op | Local3Op | Local4Op | Local5Op | Local6Op | Local7Op
     * Local0Op := 0x60
     * Local1Op := 0x61
     * Local2Op := 0x62
     * Local3Op := 0x63
     * Local4Op := 0x64
     * Local5Op := 0x65
     * Local6Op := 0x66
     * Local7Op := 0x67
     */
    let local_parser = |i, local_opcode| {
        opcode(local_opcode)
            .then(comment_scope(DebugVerbosity::AllScopes, "LocalObj", id()))
            .map(move |((), _)| Ok(i))
    };

    choice!(
        local_parser(0, opcode::LOCAL0_OP),
        local_parser(1, opcode::LOCAL1_OP),
        local_parser(2, opcode::LOCAL2_OP),
        local_parser(3, opcode::LOCAL3_OP),
        local_parser(4, opcode::LOCAL4_OP),
        local_parser(5, opcode::LOCAL5_OP),
        local_parser(6, opcode::LOCAL6_OP),
        local_parser(7, opcode::LOCAL7_OP)
    )
}

/// Takes a value between `0` and `6`, where 0 represents `Arg0` etc.
pub type ArgNum = u8;

pub fn arg_obj<'a, 'c>() -> impl Parser<'a, 'c, ArgNum>
where
    'c: 'a,
{
    /*
     * ArgObj := Arg0Op | Arg1Op | Arg2Op | Arg3Op | Arg4Op | Arg5Op | Arg6Op
     * Arg0Op = 0x68
     * Arg1Op = 0x69
     * Arg2Op = 0x6a
     * Arg3Op = 0x6b
     * Arg4Op = 0x6c
     * Arg5Op = 0x6d
     * Arg6Op = 0x6e
     */
    let arg_parser = |i, arg_opcode| {
        opcode(arg_opcode).then(comment_scope(DebugVerbosity::AllScopes, "ArgObj", id())).map(move |((), _)| Ok(i))
    };

    choice!(
        arg_parser(0, opcode::ARG0_OP),
        arg_parser(1, opcode::ARG1_OP),
        arg_parser(2, opcode::ARG2_OP),
        arg_parser(3, opcode::ARG3_OP),
        arg_parser(4, opcode::ARG4_OP),
        arg_parser(5, opcode::ARG5_OP),
        arg_parser(6, opcode::ARG6_OP)
    )
}
