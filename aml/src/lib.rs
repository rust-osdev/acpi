//! `aml` is a pure-Rust AML (ACPI Machine Language) parser, used for parsing the DSDT and
//! SSDT tables from ACPI. This crate can be used by kernels to gather information about the
//! hardware, and invoke control methods to query and change the state of devices in a
//! hardware-independent way.
//!
//! ### Using the library
//! To use the library, you should create an instance of this type using `AmlContext::new()`, and
//! then pass it tables containing AML (probably from the `acpi` crate), which you've mapped into
//! the virtual address space. This will parse the table, populating the namespace with objects
//! encoded by the AML. After this, you may unmap the memory the table was mapped into - all the
//! information needed will be extracted and allocated on the heap.
//!
//! You can then access specific objects by name like so: e.g.
//! ```ignore
//! let my_aml_value = aml_context.lookup(&AmlName::from_str("\\_SB.PCI0.S08._ADR").unwrap());
//! ```
//!
//! And invoke control methods like this: e.g.
//! ```ignore
//! let result = aml_context.invoke_method(&AmlName::from_str("\\_SB.HPET._CRS").unwrap(), value::Args::EMPTY);
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
//! `MapWithContext`.
//!
//! The actual parsers are then grouped into categories based loosely on the AML grammar sections in
//! the ACPI spec. Most are written in terms of combinators, but some have to be written in a more
//! imperitive style, either because they're clearer, or because we haven't yet found good
//! combinator patterns to express the parse.

#![no_std]
#![feature(decl_macro, type_ascription, box_syntax, bool_to_option)]

extern crate alloc;

#[cfg(test)]
extern crate std;

#[cfg(test)]
mod test_utils;

pub(crate) mod expression;
pub(crate) mod misc;
pub(crate) mod name_object;
pub(crate) mod namespace;
pub(crate) mod opcode;
pub(crate) mod parser;
pub mod pci_routing;
pub(crate) mod pkg_length;
pub mod resource;
pub(crate) mod statement;
pub(crate) mod term_object;
pub mod value;

pub use crate::{namespace::*, value::AmlValue};

use alloc::{boxed::Box, string::ToString};
use core::mem;
use log::{error, warn};
use misc::{ArgNum, LocalNum};
use name_object::Target;
use parser::{Parser, Propagate};
use pkg_length::PkgLength;
use term_object::term_list;
use value::{AmlType, Args};

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
struct MethodContext {
    /// AML local variables. These are used when we invoke a control method. A `None` value represents a null AML
    /// object.
    locals: [Option<AmlValue>; 8],
    /// If we're currently invoking a control method, this stores the arguments that were passed to
    /// it. It's `None` if we aren't invoking a method.
    args: Args,
}

impl MethodContext {
    fn new(args: Args) -> MethodContext {
        // XXX: this is required because `Option<AmlValue>` is not `Copy`, so it can't be used to initialize an
        // array, but consts can :(
        const NONE_BUT_CONST: Option<AmlValue> = None;

        MethodContext { locals: [NONE_BUT_CONST; 8], args }
    }
}

pub struct AmlContext {
    /// The `Handler` passed from the library user. This is stored as a boxed trait object simply to avoid having
    /// to add a lifetime and type parameter to `AmlContext`, as they would massively complicate the parser types.
    handler: Box<dyn Handler>,

    pub namespace: Namespace,
    method_context: Option<MethodContext>,

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
    pub fn new(handler: Box<dyn Handler>, debug_verbosity: DebugVerbosity) -> AmlContext {
        let mut context = AmlContext {
            handler,
            namespace: Namespace::new(),
            method_context: None,

            current_scope: AmlName::root(),
            scope_indent: 0,
            debug_verbosity,
        };

        context.add_predefined_objects();
        context
    }

    pub fn parse_table(&mut self, stream: &[u8]) -> Result<(), AmlError> {
        if stream.len() == 0 {
            return Err(AmlError::UnexpectedEndOfStream);
        }

        let table_length = PkgLength::from_raw_length(stream, stream.len() as u32).unwrap();
        match term_object::term_list(table_length).parse(stream, self) {
            Ok(_) => Ok(()),
            Err((_, _, Propagate::Err(err))) => {
                error!("Failed to parse AML stream. Err = {:?}", err);
                Err(err)
            }
            Err((_, _, other)) => {
                error!("AML table evaluated to unexpected result: {:?}", other);
                Err(AmlError::MalformedStream)
            }
        }
    }

    // TODO: docs
    pub fn invoke_method(&mut self, path: &AmlName, args: Args) -> Result<AmlValue, AmlError> {
        use value::MethodCode;

        match self.namespace.get_by_path(path)?.clone() {
            AmlValue::Method { flags, code } => {
                /*
                 * First, set up the state we expect to enter the method with, but clearing local
                 * variables to "null" and setting the arguments. Save the current method state and scope, so if we're
                 * already executing another control method, we resume into it correctly.
                 */
                let old_context = mem::replace(&mut self.method_context, Some(MethodContext::new(args)));
                let old_scope = mem::replace(&mut self.current_scope, path.clone());

                /*
                 * Create a namespace level to store local objects created by the invocation.
                 */
                self.namespace.add_level(path.clone(), LevelType::MethodLocals)?;

                let return_value = match code {
                    MethodCode::Aml(ref code) => {
                        match term_list(PkgLength::from_raw_length(code, code.len() as u32).unwrap())
                            .parse(code, self)
                        {
                            // If the method doesn't return a value, we implicitly return `0`
                            Ok(_) => Ok(AmlValue::Integer(0)),
                            Err((_, _, Propagate::Return(result))) => Ok(result),
                            Err((_, _, Propagate::Break)) => Err(AmlError::BreakInInvalidPosition),
                            Err((_, _, Propagate::Continue)) => Err(AmlError::ContinueInInvalidPosition),
                            Err((_, _, Propagate::Err(err))) => {
                                error!("Failed to execute control method: {:?}", err);
                                Err(err)
                            }
                        }
                    }

                    MethodCode::Native(ref method) => match (method)(self) {
                        Ok(result) => Ok(result),
                        Err(err) => {
                            error!("Failed to execute control method: {:?}", err);
                            Err(err)
                        }
                    },
                };

                /*
                 * Locally-created objects should be destroyed on method exit (see §5.5.2.3 of the ACPI spec). We do
                 * this by simply removing the method's local object layer.
                 */
                // TODO: this should also remove objects created by the method outside the method's scope, if they
                // weren't statically created. This is harder.
                self.namespace.remove_level(path.clone())?;

                /*
                 * Restore the old state.
                 */
                self.method_context = old_context;
                self.current_scope = old_scope;

                return_value
            }

            /*
             * AML can encode methods that don't require any computation simply as the value that would otherwise be
             * returned (e.g. a `_STA` object simply being an `AmlValue::Integer`, instead of a method that just
             * returns an integer).
             */
            value => Ok(value),
        }
    }

    // TODO: docs
    pub fn initialize_objects(&mut self) -> Result<(), AmlError> {
        use name_object::NameSeg;
        use value::StatusObject;

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
                let status = if level.values.contains_key(&NameSeg::from_str("_STA").unwrap()) {
                    self.invoke_method(&AmlName::from_str("_STA").unwrap().resolve(&path)?, Args::default())?
                        .as_status()?
                } else {
                    StatusObject::default()
                };

                /*
                 * If the device is present and has an `_INI` method, invoke it.
                 */
                if status.present && level.values.contains_key(&NameSeg::from_str("_INI").unwrap()) {
                    log::info!("Invoking _INI at level: {}", path);
                    self.invoke_method(&AmlName::from_str("_INI").unwrap().resolve(&path)?, Args::default())?;
                }

                /*
                 * We traverse the children of this device if it's present, or isn't present but is functional.
                 */
                Ok(status.present || status.functional)
            }

            LevelType::Scope => Ok(true),

            // TODO: can any of these contain devices?
            LevelType::Processor => Ok(false),
            LevelType::PowerResource => Ok(false),
            LevelType::ThermalZone => Ok(false),
            LevelType::MethodLocals => Ok(false),
        })?;

        Ok(())
    }

    pub(crate) fn read_target(&self, target: &Target) -> Result<&AmlValue, AmlError> {
        match target {
            Target::Null => todo!(),
            Target::Name(name) => {
                let (_, handle) = self.namespace.search(name, &self.current_scope)?;
                self.namespace.get(handle)
            }
            Target::Debug => todo!(),
            Target::Arg(arg) => self.current_arg(*arg),
            Target::Local(local) => self.local(*local),
        }
    }

    /// Get the value of an argument by its argument number. Can only be executed from inside a control method.
    pub(crate) fn current_arg(&self, arg: ArgNum) -> Result<&AmlValue, AmlError> {
        self.method_context.as_ref().ok_or(AmlError::NotExecutingControlMethod)?.args.arg(arg)
    }

    /// Get the current value of a local by its local number. Can only be executed from inside a control method.
    pub(crate) fn local(&self, local: LocalNum) -> Result<&AmlValue, AmlError> {
        if let None = self.method_context {
            return Err(AmlError::NotExecutingControlMethod);
        }
        if local > 7 {
            return Err(AmlError::InvalidLocalAccess(local));
        }

        self.method_context.as_ref().unwrap().locals[local as usize]
            .as_ref()
            .ok_or(AmlError::InvalidLocalAccess(local))
    }

    /// Perform a store into a `Target`, according to the rules specified by §19.3.5.8. This returns a value read
    /// out of the target, if neccessary, as values can be altered during a store in some circumstances.  When
    /// required, this also performs required implicit conversions, otherwise stores are semantically equivalent to
    /// a `CopyObject`.
    pub(crate) fn store(&mut self, target: Target, value: AmlValue) -> Result<AmlValue, AmlError> {
        match target {
            Target::Name(ref path) => {
                let (_, handle) = self.namespace.search(path, &self.current_scope)?;

                match self.namespace.get(handle).unwrap().type_of() {
                    AmlType::FieldUnit => {
                        let mut field = self.namespace.get(handle).unwrap().clone();
                        field.write_field(value, self)?;
                        field.read_field(self)
                    }
                    AmlType::BufferField => {
                        let mut buffer_field = self.namespace.get(handle).unwrap().clone();
                        buffer_field.write_buffer_field(value.clone(), self)?;
                        Ok(value)
                    }
                    typ => {
                        *self.namespace.get_mut(handle)? = value.as_type(typ, self)?;
                        Ok(self.namespace.get(handle)?.clone())
                    }
                }
            }

            Target::Debug => {
                // TODO
                unimplemented!()
            }

            Target::Arg(arg_num) => {
                if let None = self.method_context {
                    return Err(AmlError::NotExecutingControlMethod);
                }

                /*
                 * Stores into `Arg` objects are simply copied with no conversion applied, unless the `Arg`
                 * contains an Object Reference, in which case an automatic de-reference occurs and the object is
                 * copied to the target of the Object Reference, instead of overwriting the `Arg.`
                 */
                // TODO: implement behaviour for object references
                self.method_context.as_mut().unwrap().args.store_arg(arg_num, value.clone())?;
                Ok(value)
            }

            Target::Local(local_num) => {
                if let None = self.method_context {
                    return Err(AmlError::NotExecutingControlMethod);
                }

                /*
                 * Stores into `Local` objects are always simply copied into the destination with no conversion
                 * applied, even if it contains an Object Reference.
                 */
                self.method_context.as_mut().unwrap().locals[local_num as usize] = Some(value.clone());
                Ok(value)
            }

            Target::Null => Ok(value),
        }
    }

    /// Read from an operation-region, performing only standard-sized reads (supported powers-of-2 only. If a field
    /// is not one of these sizes, it may need to be masked, or multiple reads may need to be performed).
    pub(crate) fn read_region(&self, region_handle: AmlHandle, offset: u64, length: u64) -> Result<u64, AmlError> {
        use bit_field::BitField;
        use core::convert::TryInto;
        use value::RegionSpace;

        let (region_space, region_base, region_length, parent_device) = {
            if let AmlValue::OpRegion { region, offset, length, parent_device } =
                self.namespace.get(region_handle)?
            {
                (region, offset, length, parent_device)
            } else {
                return Err(AmlError::FieldRegionIsNotOpRegion);
            }
        };

        match region_space {
            RegionSpace::SystemMemory => {
                let address = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(self.handler.read_u8(address) as u64),
                    16 => Ok(self.handler.read_u16(address) as u64),
                    32 => Ok(self.handler.read_u32(address) as u64),
                    64 => Ok(self.handler.read_u64(address)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::SystemIo => {
                let port = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(self.handler.read_io_u8(port) as u64),
                    16 => Ok(self.handler.read_io_u16(port) as u64),
                    32 => Ok(self.handler.read_io_u32(port) as u64),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::PciConfig => {
                /*
                 * First, we need to get some extra information out of objects in the parent object. Both
                 * `_SEG` and `_BBN` seem optional, with defaults that line up with legacy PCI implementations
                 * (e.g. systems with a single segment group and a single root, respectively).
                 */
                let parent_device = parent_device.as_ref().unwrap();
                let seg = match self.namespace.search(&AmlName::from_str("_SEG").unwrap(), parent_device) {
                    Ok((_, handle)) => self
                        .namespace
                        .get(handle)?
                        .as_integer(self)?
                        .try_into()
                        .map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let bbn = match self.namespace.search(&AmlName::from_str("_BBN").unwrap(), parent_device) {
                    Ok((_, handle)) => self
                        .namespace
                        .get(handle)?
                        .as_integer(self)?
                        .try_into()
                        .map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let adr = {
                    let (_, handle) = self.namespace.search(&AmlName::from_str("_ADR").unwrap(), parent_device)?;
                    self.namespace.get(handle)?.as_integer(self)?
                };

                let device = adr.get_bits(16..24) as u8;
                let function = adr.get_bits(0..8) as u8;
                let offset = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;

                match length {
                    8 => Ok(self.handler.read_pci_u8(seg, bbn, device, function, offset) as u64),
                    16 => Ok(self.handler.read_pci_u16(seg, bbn, device, function, offset) as u64),
                    32 => Ok(self.handler.read_pci_u32(seg, bbn, device, function, offset) as u64),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            // TODO
            _ => unimplemented!(),
        }
    }

    pub(crate) fn write_region(
        &mut self,
        region_handle: AmlHandle,
        offset: u64,
        length: u64,
        value: u64,
    ) -> Result<(), AmlError> {
        use bit_field::BitField;
        use core::convert::TryInto;
        use value::RegionSpace;

        let (region_space, region_base, region_length, parent_device) = {
            if let AmlValue::OpRegion { region, offset, length, parent_device } =
                self.namespace.get(region_handle)?
            {
                (region, offset, length, parent_device)
            } else {
                return Err(AmlError::FieldRegionIsNotOpRegion);
            }
        };

        match region_space {
            RegionSpace::SystemMemory => {
                let address = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(self.handler.write_u8(address, value as u8)),
                    16 => Ok(self.handler.write_u16(address, value as u16)),
                    32 => Ok(self.handler.write_u32(address, value as u32)),
                    64 => Ok(self.handler.write_u64(address, value)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::SystemIo => {
                let port = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;
                match length {
                    8 => Ok(self.handler.write_io_u8(port, value as u8)),
                    16 => Ok(self.handler.write_io_u16(port, value as u16)),
                    32 => Ok(self.handler.write_io_u32(port, value as u32)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            RegionSpace::PciConfig => {
                /*
                 * First, we need to get some extra information out of objects in the parent object. Both
                 * `_SEG` and `_BBN` seem optional, with defaults that line up with legacy PCI implementations
                 * (e.g. systems with a single segment group and a single root, respectively).
                 */
                let parent_device = parent_device.as_ref().unwrap();
                let seg = match self.namespace.search(&AmlName::from_str("_SEG").unwrap(), parent_device) {
                    Ok((_, handle)) => self
                        .namespace
                        .get(handle)?
                        .as_integer(self)?
                        .try_into()
                        .map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let bbn = match self.namespace.search(&AmlName::from_str("_BBN").unwrap(), parent_device) {
                    Ok((_, handle)) => self
                        .namespace
                        .get(handle)?
                        .as_integer(self)?
                        .try_into()
                        .map_err(|_| AmlError::FieldInvalidAddress)?,
                    Err(AmlError::ValueDoesNotExist(_)) => 0,
                    Err(err) => return Err(err),
                };
                let adr = {
                    let (_, handle) = self.namespace.search(&AmlName::from_str("_ADR").unwrap(), parent_device)?;
                    self.namespace.get(handle)?.as_integer(self)?
                };

                let device = adr.get_bits(16..24) as u8;
                let function = adr.get_bits(0..8) as u8;
                let offset = (region_base + offset).try_into().map_err(|_| AmlError::FieldInvalidAddress)?;

                match length {
                    8 => Ok(self.handler.write_pci_u8(seg, bbn, device, function, offset, value as u8)),
                    16 => Ok(self.handler.write_pci_u16(seg, bbn, device, function, offset, value as u16)),
                    32 => Ok(self.handler.write_pci_u32(seg, bbn, device, function, offset, value as u32)),
                    _ => Err(AmlError::FieldInvalidAccessSize),
                }
            }

            // TODO
            _ => unimplemented!(),
        }
    }

    fn add_predefined_objects(&mut self) {
        /*
         * These are the scopes predefined by the spec. Some tables will try to access them without defining them
         * themselves, and so we have to pre-create them.
         */
        self.namespace.add_level(AmlName::from_str("\\_GPE").unwrap(), LevelType::Scope).unwrap();
        self.namespace.add_level(AmlName::from_str("\\_SB").unwrap(), LevelType::Scope).unwrap();
        self.namespace.add_level(AmlName::from_str("\\_SI").unwrap(), LevelType::Scope).unwrap();
        self.namespace.add_level(AmlName::from_str("\\_PR").unwrap(), LevelType::Scope).unwrap();
        self.namespace.add_level(AmlName::from_str("\\_TZ").unwrap(), LevelType::Scope).unwrap();

        /*
         * In the dark ages of ACPI 1.0, before `\_OSI`, `\_OS` was used to communicate to the firmware which OS
         * was running. This was predictably not very good, and so was replaced in ACPI 3.0 with `_OSI`, which
         * allows support for individual capabilities to be queried. `_OS` should not be used by modern firmwares,
         * but to avoid problems we follow Linux in returning `"Microsoft Windows NT"`.
         *
         * See https://www.kernel.org/doc/html/latest/firmware-guide/acpi/osi.html for more information.
         */
        self.namespace
            .add_value(AmlName::from_str("\\_OS").unwrap(), AmlValue::String("Microsoft Windows NT".to_string()))
            .unwrap();

        /*
         * `\_OSI` was introduced by ACPI 3.0 to improve the situation created by `\_OS`. Unfortunately, exactly
         * the same problem was immediately repeated by introducing capabilities reflecting that an ACPI
         * implementation is exactly the same as a particular version of Windows' (e.g. firmwares will call
         * `\_OSI("Windows 2001")`).
         *
         * We basically follow suit with whatever Linux does, as this will hopefully minimise breakage:
         *    - We always claim `Windows *` compatability
         *    - We answer 'yes' to `_OSI("Darwin")
         *    - We answer 'no' to `_OSI("Linux")`, and report that the tables are doing the wrong thing
         */
        self.namespace
            .add_value(
                AmlName::from_str("\\_OSI").unwrap(),
                AmlValue::native_method(1, false, 0, |context| {
                    Ok(match context.current_arg(0)?.as_string(context)?.as_str() {
                        "Windows 2000" => true,       // 2000
                        "Windows 2001" => true,       // XP
                        "Windows 2001 SP1" => true,   // XP SP1
                        "Windows 2001 SP2" => true,   // XP SP2
                        "Windows 2001.1" => true,     // Server 2003
                        "Windows 2001.1 SP1" => true, // Server 2003 SP1
                        "Windows 2006" => true,       // Vista
                        "Windows 2006 SP1" => true,   // Vista SP1
                        "Windows 2006 SP2" => true,   // Vista SP2
                        "Windows 2006.1" => true,     // Server 2008
                        "Windows 2009" => true,       // 7 and Server 2008 R2
                        "Windows 2012" => true,       // 8 and Server 2012
                        "Windows 2013" => true,       // 8.1 and Server 2012 R2
                        "Windows 2015" => true,       // 10
                        "Windows 2016" => true,       // 10 version 1607
                        "Windows 2017" => true,       // 10 version 1703
                        "Windows 2017.2" => true,     // 10 version 1709
                        "Windows 2018" => true,       // 10 version 1803
                        "Windows 2018.2" => true,     // 10 version 1809
                        "Windows 2019" => true,       // 10 version 1903

                        "Darwin" => true,

                        "Linux" => {
                            // TODO: should we allow users to specify that this should be true? Linux has a
                            // command line option for this.
                            warn!("ACPI evaluated `_OSI(\"Linux\")`. This is a bug. Reporting no support.");
                            false
                        }

                        "Extended Address Space Descriptor" => true,
                        // TODO: support module devices
                        "Module Device" => false,
                        "3.0 Thermal Model" => true,
                        "3.0 _SCP Extensions" => true,
                        // TODO: support processor aggregator devices
                        "Processor Aggregator Device" => false,

                        _ => false,
                    }
                    .then_some(AmlValue::ones())
                    .unwrap_or(AmlValue::zero()))
                }),
            )
            .unwrap();

        /*
         * `\_REV` evaluates to the version of the ACPI specification supported by this interpreter. Linux did this
         * correctly until 2015, but firmwares misused this to detect Linux (as even modern versions of Windows
         * return `2`), and so they switched to just returning `2` (as we'll also do). `_REV` should be considered
         * useless and deprecated (this is mirrored in newer specs, which claim `2` means "ACPI 2 or greater").
         */
        self.namespace.add_value(AmlName::from_str("\\_REV").unwrap(), AmlValue::Integer(2)).unwrap();
    }
}

/// Trait type used by [`AmlContext`] to handle reading and writing to various types of memory in the system.
pub trait Handler: Send + Sync {
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

    fn read_pci_u8(&self, segment: u16, bus: u8, device: u8, function: u8, offset: u16) -> u8;
    fn read_pci_u16(&self, segment: u16, bus: u8, device: u8, function: u8, offset: u16) -> u16;
    fn read_pci_u32(&self, segment: u16, bus: u8, device: u8, function: u8, offset: u16) -> u32;

    fn write_pci_u8(&self, segment: u16, bus: u8, device: u8, function: u8, offset: u16, value: u8);
    fn write_pci_u16(&self, segment: u16, bus: u8, device: u8, function: u8, offset: u16, value: u16);
    fn write_pci_u32(&self, segment: u16, bus: u8, device: u8, function: u8, offset: u16, value: u32);

    fn handle_fatal_error(&self, fatal_type: u8, fatal_code: u32, fatal_arg: u64) {
        panic!("Fatal error while executing AML (encountered DefFatal op). fatal_type = {:?}, fatal_code = {:?}, fatal_arg = {:?}", fatal_type, fatal_code, fatal_arg);
    }
}

/// Used when an [`AmlContext`] encounters an error.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AmlError {
    /*
     * Errors produced parsing the AML stream.
     */
    UnexpectedEndOfStream,
    UnexpectedByte(u8),
    /// Produced when the stream evaluates to something other than nothing or an error.
    MalformedStream,
    InvalidNameSeg,
    InvalidPkgLength,
    InvalidFieldFlags,
    UnterminatedStringConstant,
    InvalidStringConstant,
    InvalidRegionSpace(u8),
    /// Produced when a `DefPackage` contains a different number of elements to the package's length.
    MalformedPackage,
    /// Produced when a `DefBuffer` contains more bytes that its size.
    MalformedBuffer,
    /// Emitted by a parser when it's clear that the stream doesn't encode the object parsed by
    /// that parser (e.g. the wrong opcode starts the stream). This is handled specially by some
    /// parsers such as `or` and `choice!`.
    WrongParser,
    /// Returned when a `DefFatal` op is encountered. This is separately reported using [`Handler::handle_fatal_error`].
    FatalError,

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
    /// Produced when AML tries to do something only possible in a control method (e.g. read from an argument)
    /// when there's no control method executing.
    NotExecutingControlMethod,
    /// Produced when a method accesses an argument it does not have (e.g. a method that takes 2
    /// arguments accesses `Arg4`). The inner value is the number of the argument accessed.
    InvalidArgAccess(ArgNum),
    /// Produced when a method accesses a local that it has not stored into.
    InvalidLocalAccess(LocalNum),
    /// Tried to invoke a method with too many arguments.
    TooManyArgs,
    /// A `DefBreak` operation was performed outside of a `DefWhile` or `DefSwitch`.
    BreakInInvalidPosition,
    /// A `DefContinue` operation was performed outside of a `DefWhile`.
    ContinueInInvalidPosition,

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
    ResourceDescriptorTooLong,
    UnexpectedResourceType,

    /*
     * Errors produced working with AML values.
     */
    IncompatibleValueConversion {
        current: AmlType,
        target: AmlType,
    },
    InvalidStatusObject,
    InvalidShiftLeft,
    InvalidShiftRight,
    FieldRegionIsNotOpRegion,
    FieldInvalidAddress,
    FieldInvalidAccessSize,
    TypeCannotBeCompared(AmlType),
    /// Produced when the `Mid` operator is applied to a value of a type other than `Buffer` or `String`.
    TypeCannotBeSliced(AmlType),
    TypeCannotBeWrittenToBufferField(AmlType),
    BufferFieldIndexesOutOfBounds,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_send_sync() {
        // verify that AmlContext implements Send and Sync
        fn test_send_sync<T: Send + Sync>() {}
        test_send_sync::<AmlContext>();
    }
}
