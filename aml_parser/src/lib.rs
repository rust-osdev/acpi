#![no_std]
#![feature(alloc, decl_macro)]

extern crate alloc;

#[cfg(test)]
extern crate std;

#[cfg(test)]
mod test_utils;

pub(crate) mod opcode;
pub(crate) mod parser;

use alloc::{collections::BTreeMap, string::String};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AmlError {
    UnexpectedEndOfStream,
    UnexpectedByte(u8),
}

pub struct AmlNamespace {
    namespace: BTreeMap<String, AmlValue>,
}

impl AmlNamespace {
    pub fn new() -> AmlNamespace {
        AmlNamespace { namespace: BTreeMap::new() }
    }

    pub fn parse_table(&mut self, stream: &[u8]) -> Result<(), AmlError> {
        if stream.len() == 0 {
            return Err(AmlError::UnexpectedEndOfStream);
        }

        log::info!("stream length: {}, {:#x}", stream.len(), stream[0]);
        return Err(AmlError::UnexpectedEndOfStream);
    }
}
