use crate::{
    name_object::name_string,
    opcode::{opcode, SCOPE_OP},
    parser::{choice, comment, comment_scope, take_n, ParseResult, Parser},
    pkg_length::{pkg_length, PkgLength},
    AmlNamespace,
};
use log::{debug, trace};

/// `TermList`s are usually found within explicit-length objects (so they have a `PkgLength`
/// elsewhere in the structure), so this takes a number of bytes to parse.
pub fn term_list<'a>(list_length: PkgLength) -> impl Parser<'a, ()> {
    /*
     * TermList := Nothing | <TermObj TermList>
     */
    move |input: &'a [u8]| -> ParseResult<'a, ()> {
        while list_length.still_parsing(input) {
            let (input, ()) = term_object().parse(input)?;
        }
        Ok((input, ()))
    }
}

// TODO: maybe return `AmlValue` on success
pub fn term_object<'a>() -> impl Parser<'a, ()> {
    /*
     * TermObj := NamespaceModifierObj | NamedObj | Type1Opcode | Type2Opcode
     * NamespaceModifierObj := DefAlias | DefName | DefScope
     */
    comment_scope("TermObj", choice!(def_scope()))
}

pub fn def_scope<'a>() -> impl Parser<'a, ()> {
    /*
     * DefScope := 0x10 PkgLength NameString TermList
     */
    opcode(SCOPE_OP)
        .then(comment_scope(
            "DefScope",
            pkg_length().then(name_string()).feed(move |(pkg_length, name)| {
                debug!("Scope with name: {}, length: {:?}", name, pkg_length);
                term_list(pkg_length)
            }),
        ))
        .discard_result()
}
