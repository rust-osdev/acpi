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

use alloc::boxed::Box;
use log::error;
use misc::{ArgNum, LocalNum};
use name_object::Target;
use namespace::LevelType;
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

pub struct AmlContext {
    /// The `Handler` passed from the library user. This is stored as a boxed trait object simply to avoid having
    /// to add a lifetime and type parameter to `AmlContext`, as they would massively complicate the parser types.
    handler: Box<dyn Handler>,
    legacy_mode: bool,

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
    scope_indent: usize,
    debug_verbosity: DebugVerbosity,
}

impl AmlContext {
    /// Creates a new `AmlContext` - the central type in managing the AML tables. Only one of these should be
    /// created, and it should be passed the DSDT and all SSDTs defined by the hardware.
    ///
    /// ### Legacy mode
    /// If `true` is passed in `legacy_mode`, the library will try and remain compatible with a ACPI 1.0
    /// implementation. The following changes/assumptions are made:
    ///     - Two extra root namespaces are predefined: `\_PR` and `_TZ`
    ///     - Processors are expected to be defined with `DefProcessor`, instead of `DefDevice`
    ///     - Processors are expected to be found in `\_PR`, instead of `\_SB`
    ///     - Thermal zones are expected to be found in `\_TZ`, instead of `\_SB`
    pub fn new(handler: Box<dyn Handler>, legacy_mode: bool, debug_verbosity: DebugVerbosity) -> AmlContext {
        let mut context = AmlContext {
            handler,
            legacy_mode,
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
            scope_indent: 0,
            debug_verbosity,
        };

        /*
         * Add the predefined root namespaces.
         */
        context.namespace.add_level(AmlName::from_str("\\_GPE").unwrap(), LevelType::Scope).unwrap();
        context.namespace.add_level(AmlName::from_str("\\_SB").unwrap(), LevelType::Scope).unwrap();
        context.namespace.add_level(AmlName::from_str("\\_SI").unwrap(), LevelType::Scope).unwrap();
        if legacy_mode {
            context.namespace.add_level(AmlName::from_str("\\_PR").unwrap(), LevelType::Scope).unwrap();
            context.namespace.add_level(AmlName::from_str("\\_TZ").unwrap(), LevelType::Scope).unwrap();
        }

        context
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

            /*
             * Create a namespace level to store local objects created by the invocation.
             */
            self.namespace.add_level(path.clone(), LevelType::MethodLocals)?;

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
             * Locally-created objects should be destroyed on method exit (see §5.5.2.3 of the ACPI spec). We do
             * this by simply removing the method's local object layer.
             */
            // TODO: this should also remove objects created by the method outside the method's scope, if they
            // weren't statically created. This is harder.
            self.namespace.remove_level(path.clone())?;

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

    pub fn initialize_objects(&mut self) -> Result<(), AmlError> {
        use name_object::NameSeg;
        use namespace::NamespaceLevel;
        use value::StatusObject;

        /*
         * TODO:
         *    - unconditionally execute `\_SB._INI`, if it exists
         *    - traverse namespace, looking for devices
         *    - if `_STA` is present, evaluate it. If not, assume device is present and functional.
         *    - if device is present, evaluate `_INI` if it exists
         *    - if device is present, or isn't present but is functional, traverse it's children for more devices
         */
        /*
         * If `\_SB._INI` exists, we unconditionally execute it at the beginning of device initialization.
         */
        match self.invoke_method(&AmlName::from_str("\\_SB._INI").unwrap(), Args::default()) {
            Ok(_) => (),
            Err(AmlError::ValueDoesNotExist(_)) => (),
            Err(err) => return Err(err),
        }

        /*
         * Next, we traverse the namespace, looking for devices.
         *
         * XXX: we clone the namespace here, which obviously drives up heap burden quite a bit (not as much as you
         * might first expect though - we're only duplicating the level data structure, not all the objects). The
         * issue here is that we need to access the namespace during traversal (e.g. to invoke a method), which the
         * borrow checker really doesn't like. A better solution could be a iterator-like traversal system that
         * keeps track of the namespace without keeping it borrowed. This works for now.
         */
        self.namespace.clone().traverse(|path, level: &NamespaceLevel| match level.typ {
            LevelType::Device => {
                let sta = if level.values.contains_key(&NameSeg::from_str("_STA").unwrap()) {
                    self.invoke_method(&AmlName::from_str("_STA").unwrap().resolve(&path)?, Args::default())?
                        .as_status()?
                } else {
                    StatusObject::default()
                };

                // TODO: if this object is present, evaluate _INI if it exists

                // TODO: can devices contain other devices?
                Ok(true)
            }

            LevelType::Scope => Ok(true),

            // TODO: can either of these contain devices?
            LevelType::Processor => Ok(false),
            LevelType::MethodLocals => Ok(false),
        })?;

        Ok(())
    }

    pub(crate) fn current_arg(&self, arg: ArgNum) -> Result<&AmlValue, AmlError> {
        self.current_args.as_ref().ok_or(AmlError::InvalidArgAccess(0xff))?.arg(arg)
    }

    /// Get the current value of a local by its local number.
    ///
    /// ### Panics
    /// Panics if an invalid local number is passed (valid local numbers are `0..=7`)
    pub(crate) fn local(&self, local: LocalNum) -> Option<&AmlValue> {
        match local {
            0 => self.local_0.as_ref(),
            1 => self.local_1.as_ref(),
            2 => self.local_2.as_ref(),
            3 => self.local_3.as_ref(),
            4 => self.local_4.as_ref(),
            5 => self.local_5.as_ref(),
            6 => self.local_6.as_ref(),
            7 => self.local_7.as_ref(),
            _ => panic!("Invalid local number: {}", local),
        }
    }

    /// Perform a store into a `Target`. This returns a value read out of the target, if neccessary, as values can
    /// be altered during a store in some circumstances. If the target is a `Name`, this also performs required
    /// implicit conversions. Stores to other targets are semantically equivalent to a `CopyObject`.
    pub(crate) fn store(&mut self, target: Target, value: AmlValue) -> Result<AmlValue, AmlError> {
        match target {
            Target::Name(ref path) => {
                let (_, handle) = self.namespace.search(path, &self.current_scope)?;
                let desired_type = self.namespace.get(handle).unwrap().type_of();
                let converted_object = value.as_type(desired_type)?;

                *self.namespace.get_mut(handle)? = converted_object;
                Ok(self.namespace.get(handle)?.clone())
            }

            Target::Debug => {
                // TODO
                unimplemented!()
            }

            Target::Arg(arg_num) => {
                if let None = self.current_args {
                    return Err(AmlError::InvalidArgAccess(0xff));
                }

                match arg_num {
                    0 => self.current_args.as_mut().unwrap().arg_0 = Some(value.clone()),
                    1 => self.current_args.as_mut().unwrap().arg_1 = Some(value.clone()),
                    2 => self.current_args.as_mut().unwrap().arg_2 = Some(value.clone()),
                    3 => self.current_args.as_mut().unwrap().arg_3 = Some(value.clone()),
                    4 => self.current_args.as_mut().unwrap().arg_4 = Some(value.clone()),
                    5 => self.current_args.as_mut().unwrap().arg_5 = Some(value.clone()),
                    6 => self.current_args.as_mut().unwrap().arg_6 = Some(value.clone()),
                    _ => return Err(AmlError::InvalidArgAccess(arg_num)),
                }
                Ok(value)
            }

            Target::Local(local_num) => {
                match local_num {
                    0 => self.local_0 = Some(value.clone()),
                    1 => self.local_1 = Some(value.clone()),
                    2 => self.local_2 = Some(value.clone()),
                    3 => self.local_3 = Some(value.clone()),
                    4 => self.local_4 = Some(value.clone()),
                    5 => self.local_5 = Some(value.clone()),
                    6 => self.local_6 = Some(value.clone()),
                    7 => self.local_7 = Some(value.clone()),
                    _ => return Err(AmlError::InvalidLocalAccess(local_num)),
                }
                Ok(value)
            }

            Target::Null => Ok(value),
        }
    }
}

pub trait Handler {
    fn read_u8(&self, address: usize) -> u8;
    fn read_u16(&self, address: usize) -> u16;
    fn read_u32(&self, address: usize) -> u32;
    fn read_u64(&self, address: usize) -> u64;

    fn write_u8(&mut self, address: usize, value: u8);
    fn write_u16(&mut self, address: usize, value: u16);
    fn write_u32(&mut self, address: usize, value: u32);
    fn write_u64(&mut self, address: usize, value: u64);

    fn read_io_u8(&self, port: u16) -> u8;
    fn read_io_u16(&self, port: u16) -> u16;
    fn read_io_u32(&self, port: u16) -> u32;

    fn write_io_u8(&self, port: u16, value: u8);
    fn write_io_u16(&self, port: u16, value: u16);
    fn write_io_u32(&self, port: u16, value: u32);

    // TODO: PCI config space accessing functions
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
    ValueDoesNotExist(AmlName),
    /// Produced when two values with the same name are added to the namespace.
    NameCollision(AmlName),
    TriedToRemoveRootNamespace,

    /*
     * Errors produced executing control methods.
     */
    /// Produced when a method accesses an argument it does not have (e.g. a method that takes 2
    /// arguments accesses `Arg4`). The inner value is the number of the argument accessed. If any
    /// arguments are accessed when a method is not being executed, this error is produced with an
    /// argument number of `0xff`.
    InvalidArgAccess(ArgNum),
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

    /*
     * Errors produced working with AML values.
     */
    InvalidStatusObject,
    InvalidShiftLeft,
    InvalidShiftRight,
}
