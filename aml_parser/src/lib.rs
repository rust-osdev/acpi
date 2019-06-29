//! `aml_parser` is a pure-Rust AML (ACPI Machine Language) parser, used for parsing the DSDT and
//! SSDT tables from ACPI. This crate can be used by kernels to gather information about the
//! hardware, and invoke control methods (this is not yet supported) to query and change the state
//! of devices in a hardware-independent way.
//!
//! ### Using the library
//! To use the library, you will mostly interact with the `AmlContext` type. You should create an
//! instance of this type using `AmlContext::new()`, and then pass it tables containing AML
//! (probably from the `acpi` crate), which you've mapped into the virtual address space. This will
//! parse the table, populating the namespace with objects encoded by the AML. After this, you may
//! unmap the memory the table was mapped into - all the information needed will be extracted and
//! allocated on the heap.
//!
//! ### About the parser
//! The parser is written using a set of custom parser combinators - the code can be confusing on
//! first reading, but provides an extensible and type-safe way to write parsers. For an easy
//! introduction to parser combinators and the foundations used for this library, I suggest reading
//! [Bodil's fantastic blog post](https://bodil.lol/parser-combinators/).
//!
//! The actual combinators can be found in `parser.rs`. Various tricks are used to provide a nice
//! API and work around limitations in the type system, such as the concrete types like
//! `MapWithContext`, and the `make_parser_concrete` hack macro.
//!
//! The actual parsers are then grouped into categories based loosely on the AML grammar sections in
//! the ACPI spec. Most are written in terms of combinators, but some have to be written in a more
//! imperitive style, either because they're clearer, or because we haven't yet found good
//! combinator patterns to express the parse.

#![no_std]
#![feature(decl_macro, type_ascription, box_syntax)]

extern crate alloc;

#[cfg(test)]
extern crate std;

#[cfg(test)]
mod test_utils;

pub(crate) mod name_object;
pub(crate) mod opcode;
pub(crate) mod parser;
pub(crate) mod pkg_length;
pub(crate) mod term_object;
pub mod value;

pub use crate::value::AmlValue;

use alloc::collections::BTreeMap;
use log::error;
use name_object::AmlName;
use parser::Parser;
use pkg_length::PkgLength;

/// AML has a `RevisionOp` operator that returns the "AML interpreter revision". It's not clear
/// what this is actually used for, but this is ours.
pub const AML_INTERPRETER_REVISION: u64 = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AmlError {
    UnexpectedEndOfStream,
    UnexpectedByte(u8),
    InvalidNameSeg([u8; 4]),
    InvalidFieldFlags,
    IncompatibleValueConversion,
    UnterminatedStringConstant,
    InvalidStringConstant,
    InvalidRegionSpace(u8),
    /// Error produced when none of the parsers in a `choice!` could parse the next part of the
    /// stream. Contains the next two bytes to make debugging missing extended opcodes easier.
    NoParsersCouldParse([u8; 2]),
}

#[derive(Debug)]
pub struct AmlContext {
    namespace: BTreeMap<AmlName, AmlValue>,
    current_scope: AmlName,
}

impl AmlContext {
    pub fn new() -> AmlContext {
        AmlContext { namespace: BTreeMap::new(), current_scope: AmlName::root() }
    }

    pub fn parse_table(&mut self, stream: &[u8]) -> Result<(), AmlError> {
        if stream.len() == 0 {
            return Err(AmlError::UnexpectedEndOfStream);
        }

        let table_length = PkgLength::from_raw_length(stream, stream.len() as u32) as PkgLength;
        match term_object::term_list(table_length).parse(stream, self) {
            Ok(_) => Ok(()),
            Err((remaining, _context, err)) => {
                error!("Failed to parse AML stream. Err = {:?}", err);
                Err(err)
            }
        }
    }

    /// Resolves a given path relative to the current scope (if the given path is not absolute).
    /// The returned path can be used to index the namespace.
    pub fn resolve_path(&mut self, path: &AmlName) -> AmlName {
        // TODO: we should normalize the path by resolving prefix chars etc.

        // If the path is absolute, just return it.
        if path.is_absolute() {
            return path.clone();
        }

        // Otherwise, it's relative to the current scope so append it onto that.
        let mut new_path = self.current_scope.clone();
        new_path.0.extend_from_slice(&(path.0));
        new_path
    }

    /// Add an `AmlValue` to the namespace. `path` can either be absolute, or relative (in which
    /// case it's treated as relative to the current scope).
    pub fn add_to_namespace(&mut self, path: AmlName, value: AmlValue) {
        let resolved_path = self.resolve_path(&path);
        self.namespace.insert(resolved_path, value);
    }
}
