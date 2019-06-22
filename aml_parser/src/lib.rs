#![no_std]
#![feature(decl_macro)]

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

use alloc::{collections::BTreeMap, string::String};
use log::{error, trace};
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
}

pub struct AmlContext {
    namespace: BTreeMap<String, AmlValue>,
}

impl AmlContext {
    pub fn new() -> AmlContext {
        AmlContext { namespace: BTreeMap::new() }
    }

    pub fn parse_table(&mut self, stream: &[u8]) -> Result<(), AmlError> {
        if stream.len() == 0 {
            return Err(AmlError::UnexpectedEndOfStream);
        }

        match term_object::term_list(
            PkgLength::from_raw_length(stream, stream.len() as u32) as PkgLength
        )
        .parse(stream)
        {
            Ok(_) => Ok(()),
            Err((remaining, err)) => {
                error!("Failed to parse AML stream. Err = {:?}", err);
                trace!("Remaining AML: {:02x?}", remaining);
                Err(err)
            }
        }
    }
}
