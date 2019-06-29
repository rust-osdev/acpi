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
use log::{error, trace};
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
