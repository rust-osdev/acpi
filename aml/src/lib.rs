//! `aml` is a pure-Rust AML (ACPI Machine Language) parser, used for parsing the DSDT and
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
//! You can then access specific objects by name like so: e.g.
//! ```ignore
//! let my_aml_value = aml_context.lookup(&AmlName::from_str("\\_SB.PCI0.S08._ADR").unwrap());
//! ```
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

pub(crate) mod misc;
pub(crate) mod name_object;
pub(crate) mod namespace;
pub(crate) mod opcode;
pub(crate) mod parser;
pub mod pci_routing;
pub(crate) mod pkg_length;
pub mod resource;
pub(crate) mod term_object;
pub(crate) mod type1;
pub(crate) mod type2;
pub mod value;

pub use crate::{
    namespace::{AmlHandle, AmlName, Namespace},
    value::AmlValue,
};

use log::error;
use misc::{ArgNum, LocalNum};
use parser::Parser;
use pkg_length::PkgLength;
use term_object::term_list;
use value::Args;

/// AML has a `RevisionOp` operator that returns the "AML interpreter revision". It's not clear
/// what this is actually used for, but this is ours.
pub const AML_INTERPRETER_REVISION: u64 = 0;

/// Describes how much debug information the parser should emit. Set the "maximum" expected verbosity in
/// the context's `debug_verbosity` - everything will be printed that is less or equal in 'verbosity'.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum DebugVerbosity {
    /// Print no debug information
    None,
    /// Print heads and tails when entering and leaving scopes of major objects, but not more minor ones.
    Scopes,
    /// Print heads and tails when entering and leaving scopes of all objects.
    AllScopes,
    /// Print heads and tails of all objects, and extra debug information as it's parsed.
    All,
}

#[derive(Debug)]
pub struct AmlContext {
    pub namespace: Namespace,

    /*
     * AML local variables. These are used when we invoke a control method. A `None` value
     * represents a null AML object.
     */
    local_0: Option<AmlValue>,
    local_1: Option<AmlValue>,
    local_2: Option<AmlValue>,
    local_3: Option<AmlValue>,
    local_4: Option<AmlValue>,
    local_5: Option<AmlValue>,
    local_6: Option<AmlValue>,
    local_7: Option<AmlValue>,

    /// If we're currently invoking a control method, this stores the arguments that were passed to
    /// it. It's `None` if we aren't invoking a method.
    current_args: Option<Args>,

    /*
     * These track the state of the context while it's parsing an AML table.
     */
    current_scope: AmlName,
    debug_verbosity: DebugVerbosity,
}

impl AmlContext {
    pub fn new(debug_verbosity: DebugVerbosity) -> AmlContext {
        AmlContext {
            namespace: Namespace::new(),
            local_0: None,
            local_1: None,
            local_2: None,
            local_3: None,
            local_4: None,
            local_5: None,
            local_6: None,
            local_7: None,
            current_args: None,

            current_scope: AmlName::root(),
            debug_verbosity,
        }
    }

    pub fn parse_table(&mut self, stream: &[u8]) -> Result<(), AmlError> {
        if stream.len() == 0 {
            return Err(AmlError::UnexpectedEndOfStream);
        }

        let table_length = PkgLength::from_raw_length(stream, stream.len() as u32) as PkgLength;
        match term_object::term_list(table_length).parse(stream, self) {
            Ok(_) => Ok(()),
            Err((_, _, err)) => {
                error!("Failed to parse AML stream. Err = {:?}", err);
                Err(err)
            }
        }
    }

    /// Invoke a method referred to by its path in the namespace, with the given arguments.
    pub fn invoke_method(&mut self, path: &AmlName, args: Args) -> Result<AmlValue, AmlError> {
        if let AmlValue::Method { flags, code } = self.namespace.get_by_path(path)?.clone() {
            /*
             * First, set up the state we expect to enter the method with, but clearing local
             * variables to "null" and setting the arguments.
             */
            self.current_scope = path.clone();
            self.current_args = Some(args);
            self.local_0 = None;
            self.local_1 = None;
            self.local_2 = None;
            self.local_3 = None;
            self.local_4 = None;
            self.local_5 = None;
            self.local_6 = None;
            self.local_7 = None;

            log::trace!("Invoking method with {} arguments, code: {:x?}", flags.arg_count(), code);
            let return_value =
                match term_list(PkgLength::from_raw_length(&code, code.len() as u32)).parse(&code, self) {
                    // If the method doesn't return a value, we implicitly return `0`
                    Ok(_) => Ok(AmlValue::Integer(0)),
                    Err((_, _, AmlError::Return(result))) => Ok(result),
                    Err((_, _, err)) => {
                        error!("Failed to execute control method: {:?}", err);
                        Err(err)
                    }
                };

            /*
             * Now clear the state.
             */
            self.current_args = None;
            self.local_0 = None;
            self.local_1 = None;
            self.local_2 = None;
            self.local_3 = None;
            self.local_4 = None;
            self.local_5 = None;
            self.local_6 = None;
            self.local_7 = None;

            return_value
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }

    pub(crate) fn current_arg(&self, arg: ArgNum) -> Result<&AmlValue, AmlError> {
        self.current_args.as_ref().ok_or(AmlError::InvalidArgumentAccess(0xff))?.arg(arg)
    }

    /// Get the current value of a local by its local number.
    ///
    /// ### Panics
    /// Panics if an invalid local number is passed (valid local numbers are `0..=7`)
    pub(crate) fn local(&self, local: LocalNum) -> Result<&AmlValue, AmlError> {
        match local {
            0 => self.local_0.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            1 => self.local_1.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            2 => self.local_2.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            3 => self.local_3.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            4 => self.local_4.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            5 => self.local_5.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            6 => self.local_6.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            7 => self.local_7.as_ref().ok_or(AmlError::InvalidLocalAccess(local)),
            _ => panic!("Invalid local number: {}", local),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AmlError {
    /*
     * Errors produced parsing the AML stream.
     */
    UnexpectedEndOfStream,
    UnexpectedByte(u8),
    InvalidNameSeg,
    InvalidFieldFlags,
    IncompatibleValueConversion,
    UnterminatedStringConstant,
    InvalidStringConstant,
    InvalidRegionSpace(u8),
    /// Emitted by a parser when it's clear that the stream doesn't encode the object parsed by
    /// that parser (e.g. the wrong opcode starts the stream). This is handled specially by some
    /// parsers such as `or` and `choice!`.
    WrongParser,

    /*
     * Errors produced manipulating AML names.
     */
    EmptyNamesAreInvalid,
    /// Produced when trying to normalize a path that does not point to a valid level of the
    /// namespace. E.g. `\_SB.^^PCI0` goes above the root of the namespace. The contained value is the name that
    /// normalization was attempted upon.
    InvalidNormalizedName(AmlName),
    RootHasNoParent,

    /*
     * Errors produced working with the namespace.
     */
    /// Produced when a sub-level or value is added to a level that has not yet been added to the namespace. The
    /// `AmlName` is the name of the entire sub-level/value.
    LevelDoesNotExist(AmlName),
    HandleDoesNotExist(AmlHandle),
    /// Produced when two values with the same name are added to the namespace.
    NameCollision(AmlName),

    /*
     * Errors produced executing control methods.
     */
    /// Produced when a method accesses an argument it does not have (e.g. a method that takes 2
    /// arguments accesses `Arg4`). The inner value is the number of the argument accessed. If any
    /// arguments are accessed when a method is not being executed, this error is produced with an
    /// argument number of `0xff`.
    InvalidArgumentAccess(ArgNum),
    InvalidLocalAccess(LocalNum),
    /// This is not a real error, but is used to propagate return values from within the deep
    /// parsing call-stack. It should only be emitted when parsing a `DefReturn`. We use the
    /// error system here because the way errors are propagated matches how we want to handle
    /// return values.
    Return(AmlValue),

    /*
     * Errors produced parsing the PCI routing tables (_PRT objects).
     */
    PrtInvalidAddress,
    PrtInvalidPin,
    PrtInvalidSource,
    PrtInvalidGsi,
    /// Produced when the PRT doesn't contain an entry for the requested address + pin
    PrtNoEntry,

    /*
     * Errors produced parsing Resource Descriptors.
     */
    ReservedResourceType,
    ResourceDescriptorTooShort,
}
