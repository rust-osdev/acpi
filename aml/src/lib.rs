#![no_std]
#![feature(let_chains, inherent_str_constructors)]

extern crate alloc;

pub mod namespace;
pub mod object;
pub mod op_region;
pub mod pci_routing;
pub mod resource;

pub use pci_types::PciAddress;

use alloc::{
    boxed::Box,
    collections::btree_map::BTreeMap,
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};
use bit_field::BitField;
use core::mem;
use log::{info, trace};
use namespace::{AmlName, Namespace, NamespaceLevelKind};
use object::{FieldFlags, FieldUnit, FieldUnitKind, MethodFlags, Object, ObjectType, ReferenceKind};
use op_region::{OpRegion, RegionHandler, RegionSpace};
use spinning_top::Spinlock;

pub struct Interpreter<H>
where
    H: Handler,
{
    handler: H,
    pub namespace: Spinlock<Namespace>,
    context_stack: Spinlock<Vec<MethodContext>>,
    dsdt_revision: u8,
    region_handlers: Spinlock<BTreeMap<RegionSpace, Box<dyn RegionHandler>>>,
}

unsafe impl<H> Send for Interpreter<H> where H: Handler + Send {}
unsafe impl<H> Sync for Interpreter<H> where H: Handler + Send {}

/// The value returned by the `Revision` opcode.
const INTERPRETER_REVISION: u64 = 1;

impl<H> Interpreter<H>
where
    H: Handler,
{
    pub fn new(handler: H, dsdt_revision: u8) -> Interpreter<H> {
        info!("Initializing AML interpreter v{}", env!("CARGO_PKG_VERSION"));
        Interpreter {
            handler,
            namespace: Spinlock::new(Namespace::new()),
            context_stack: Spinlock::new(Vec::new()),
            dsdt_revision,
            ops_interpreted: AtomicUsize::new(0),
            region_handlers: Spinlock::new(BTreeMap::new()),
        }
    }

    pub fn load_table(&self, stream: &[u8]) -> Result<(), AmlError> {
        // TODO: probs needs to do more stuff
        let context = unsafe { MethodContext::new_from_table(stream) };
        self.do_execute_method(context)?;
        Ok(())
    }

    /// Invoke a method by its name, with the given set of arguments. If the referenced object is
    /// not a method, the object will instead be returned - this is useful for objects that can
    /// either be defined directly, or through a method (e.g. a `_CRS` object).
    pub fn invoke_method(&self, path: AmlName, args: Vec<Arc<Object>>) -> Result<Arc<Object>, AmlError> {
        info!("Invoking AML method: {}", path);

        let object = self.namespace.lock().get(path.clone())?.clone();
        match object.typ() {
            ObjectType::Method => {
                self.namespace.lock().add_level(path.clone(), NamespaceLevelKind::MethodLocals)?;
                let context = MethodContext::new_from_method(object, args, path)?;
                self.do_execute_method(context)
            }
            _ => Ok(object),
        }
    }

    pub fn invoke_method_if_present(
        &self,
        path: AmlName,
        args: Vec<Arc<Object>>,
    ) -> Result<Option<Arc<Object>>, AmlError> {
        match self.invoke_method(path.clone(), args) {
            Ok(result) => Ok(Some(result)),
            Err(AmlError::ObjectDoesNotExist(not_present)) => {
                if path == not_present {
                    Ok(None)
                } else {
                    Err(AmlError::ObjectDoesNotExist(not_present))
                }
            }
            Err(other) => Err(other),
        }
    }

    pub fn install_region_handler<RH>(&self, space: RegionSpace, handler: RH)
    where
        RH: RegionHandler + 'static,
    {
        let mut handlers = self.region_handlers.lock();
        assert!(handlers.get(&space).is_none(), "Tried to install handler for same space twice!");
        handlers.insert(space, Box::new(handler));
    }

    fn do_execute_method(&self, mut context: MethodContext) -> Result<Arc<Object>, AmlError> {
        /*
         * TODO
         *
         * This is the main loop that executes ops. Every op is handled at the top-level loop to
         * prevent pathological stack depths.
         *
         * Worked example: AddOp TermArg TermArg Target
         * - We go round the loop 4 times to interpret this.
         * - We encounter the AddOp. We add a new in-flight operation with 3 expected arguments.
         * - We go round again and find some sort of TermArg. This may create new in-flight ops
         *   which we then go round again to complete. Once we're finished, we add this to the
         *   AddOp's record. We then need to detect that all 3 arguments are ready and retire the
         *   AddOp *before* moving on.
         *
         * My thoughts are that we can go round and round a loop with two big stages. First, we
         * check if in-flight ops are ready to be executed (e.g. have collected all their
         * arguments) and execute them. This can in turn complete the next op up, so this should
         * complete as many in-flight ops as possible at a time.
         *
         * Once all possible in-flight ops have been executed, we then need to move forward in the
         * stream. Depending on the next op, this could create a new in-flight op that is then
         * pre-empted, or could parse an argument to an existing in-flight op, which may then be
         * able to be completed. This stage should not do any executing in its own right, really.
         * It's just about consuming the next however-many bytes of the stream and setting things
         * up.
         */

        loop {
            /*
             * First, see if we've gathered enough arguments to complete some in-flight operations.
             */
            while let Some(op) = context.in_flight.pop_if(|op| op.arguments.len() == op.expected_arguments) {
                match op.op {
                    Opcode::Add
                    | Opcode::Subtract
                    | Opcode::Multiply
                    | Opcode::Divide
                    | Opcode::ShiftLeft
                    | Opcode::ShiftRight
                    | Opcode::Mod
                    | Opcode::Nand
                    | Opcode::And
                    | Opcode::Or
                    | Opcode::Nor
                    | Opcode::Xor => {
                        self.do_binary_maths(&mut context, op)?;
                    }
                    Opcode::Not | Opcode::FindSetLeftBit | Opcode::FindSetRightBit => {
                        self.do_unary_maths(&mut context, op)?;
                    }
                    Opcode::Increment | Opcode::Decrement => {
                        let [Argument::Object(operand)] = &op.arguments[..] else { panic!() };
                        let Object::Integer(operand) = operand.gain_mut() else {
                            Err(AmlError::ObjectNotOfExpectedType {
                                expected: ObjectType::Integer,
                                got: operand.typ(),
                            })?
                        };

                        let new_value = match op.op {
                            Opcode::Increment => operand.wrapping_add(1),
                            Opcode::Decrement => operand.wrapping_sub(1),
                            _ => unreachable!(),
                        };

                        *operand = new_value;
                    }
                    Opcode::LAnd
                    | Opcode::LOr
                    | Opcode::LNot
                    | Opcode::LNotEqual
                    | Opcode::LLessEqual
                    | Opcode::LGreaterEqual
                    | Opcode::LEqual
                    | Opcode::LGreater
                    | Opcode::LLess => {
                        self.do_logical_op(&mut context, op)?;
                    }
                    Opcode::Mid => self.do_mid(&mut context, op)?,
                    Opcode::Concat => self.do_concat(&mut context, op)?,
                    Opcode::ConcatRes => {
                        let [Argument::Object(source1), Argument::Object(source2), target] = &op.arguments[..]
                        else {
                            panic!()
                        };
                        let source1 = source1.as_buffer()?;
                        let source2 = source2.as_buffer()?;
                        let result = {
                            let mut buffer = Vec::from(source1);
                            buffer.extend_from_slice(source2);
                            // Add a new end-tag
                            buffer.push(0x78);
                            // Don't calculate the new real checksum - just use 0
                            buffer.push(0x00);
                            Arc::new(Object::Buffer(buffer))
                        };
                        // TODO: use potentially-updated result for return value here
                        self.do_store(&mut context, target, result.clone())?;
                        context.contribute_arg(Argument::Object(result));
                    }
                    Opcode::FromBCD => self.do_from_bcd(&mut context, op)?,
                    Opcode::ToBCD => self.do_to_bcd(&mut context, op)?,
                    Opcode::Name => {
                        let [Argument::Namestring(name), Argument::Object(object)] = &op.arguments[..] else {
                            panic!()
                        };

                        let name = name.resolve(&context.current_scope)?;
                        self.namespace.lock().insert(name, object.clone())?;
                    }
                    Opcode::Fatal => {
                        let [Argument::ByteData(typ), Argument::DWordData(code), Argument::Object(arg)] =
                            &op.arguments[..]
                        else {
                            panic!()
                        };
                        let arg = arg.as_integer()?;
                        self.handler.handle_fatal_error(*typ, *code, arg);
                    }
                    Opcode::OpRegion => {
                        let [
                            Argument::Namestring(name),
                            Argument::ByteData(region_space),
                            Argument::Object(region_offset),
                            Argument::Object(region_length),
                        ] = &op.arguments[..]
                        else {
                            panic!()
                        };

                        let region = Object::OpRegion(OpRegion {
                            space: RegionSpace::from(*region_space),
                            base: region_offset.as_integer()?,
                            length: region_length.as_integer()?,
                            parent_device_path: context.current_scope.clone(),
                        });
                        });
                        self.namespace.lock().insert(name.resolve(&context.current_scope)?, Arc::new(region))?;
                    }
                    Opcode::Buffer => {
                        let [
                            Argument::TrackedPc(start_pc),
                            Argument::PkgLength(pkg_length),
                            Argument::Object(buffer_size),
                        ] = &op.arguments[..]
                        else {
                            panic!()
                        };
                        let buffer_size = buffer_size.as_integer()?;

                        let buffer_len = pkg_length - (context.current_block.pc - start_pc);
                        let mut buffer = vec![0; buffer_size as usize];
                        buffer[0..buffer_len].copy_from_slice(
                            &context.current_block.stream()
                                [context.current_block.pc..(context.current_block.pc + buffer_len)],
                        );
                        context.current_block.pc += buffer_len;

                        context.contribute_arg(Argument::Object(Arc::new(Object::Buffer(buffer))));
                    }
                    Opcode::Package => {
                        let mut elements = Vec::with_capacity(op.expected_arguments);
                        for arg in &op.arguments {
                            let Argument::Object(object) = arg else { panic!() };
                            elements.push(object.clone());
                        }

                        /*
                         * We can end up completing a package's in-flight op in two circumstances:
                         *    - If the correct number of elements are supplied, we end up here
                         *      first, and then later in the block's finishing logic.
                         *    - If less elements are supplied, we end up in the block's finishing
                         *      logic to add some `Uninitialized`s, then go round again to complete
                         *      the in-flight operation.
                         *
                         * To make these consistent, we always remove the block here, making sure
                         * we've finished it as a sanity check.
                         */
                        assert_eq!(context.current_block.kind, BlockKind::Package);
                        assert_eq!(context.peek(), Err(AmlError::RunOutOfStream));
                        context.current_block = context.block_stack.pop().unwrap();
                        context.contribute_arg(Argument::Object(Arc::new(Object::Package(elements))));
                    }
                    Opcode::If => {
                        let [
                            Argument::TrackedPc(start_pc),
                            Argument::PkgLength(then_length),
                            Argument::Object(predicate),
                        ] = &op.arguments[..]
                        else {
                            panic!()
                        };

                        let predicate = predicate.as_integer()?;
                        let remaining_then_length = then_length - (context.current_block.pc - start_pc);

                        if predicate > 0 {
                            context.start_new_block(BlockKind::IfThenBranch, remaining_then_length);
                        } else {
                            context.current_block.pc += remaining_then_length;
                            // Skip over the prolog to the else branch if present
                            const DEF_ELSE_OP: u8 = 0xa1;
                            // TODO: maybe need to handle error here
                            if context.peek()? == DEF_ELSE_OP {
                                context.next()?;
                                let _else_length = context.pkglength()?;
                            }
                        }
                    }
                    opcode @ Opcode::CreateBitField
                    | opcode @ Opcode::CreateByteField
                    | opcode @ Opcode::CreateWordField
                    | opcode @ Opcode::CreateDWordField
                    | opcode @ Opcode::CreateQWordField => {
                        let [Argument::Object(buffer), Argument::Object(index)] = &op.arguments[..] else {
                            panic!()
                        };
                        let name = context.namestring()?;
                        let index = index.as_integer()?;
                        let (offset, length) = match opcode {
                            Opcode::CreateBitField => (index, 1),
                            Opcode::CreateByteField => (index * 8, 8),
                            Opcode::CreateWordField => (index * 8, 16),
                            Opcode::CreateDWordField => (index * 8, 32),
                            Opcode::CreateQWordField => (index * 8, 64),
                            _ => unreachable!(),
                        };
                        self.namespace.lock().insert(
                            name.resolve(&context.current_scope)?,
                            Arc::new(Object::BufferField {
                                buffer: buffer.clone(),
                                offset: offset as usize,
                                length,
                            }),
                        )?;
                    }
                    Opcode::CreateField => {
                        let [Argument::Object(buffer), Argument::Object(bit_index), Argument::Object(num_bits)] =
                            &op.arguments[..]
                        else {
                            panic!()
                        };
                        let name = context.namestring()?;
                        let bit_index = bit_index.as_integer()?;
                        let num_bits = num_bits.as_integer()?;

                        self.namespace.lock().insert(
                            name.resolve(&context.current_scope)?,
                            Arc::new(Object::BufferField {
                                buffer: buffer.clone(),
                                offset: bit_index as usize,
                                length: num_bits as usize,
                            }),
                        )?;
                    }
                    Opcode::Store => {
                        let [Argument::Object(object), target] = &op.arguments[..] else { panic!() };
                        self.do_store(&mut context, &target, object.clone())?;
                    }
                    Opcode::RefOf => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        let reference =
                            Arc::new(Object::Reference { kind: ReferenceKind::RefOf, inner: object.clone() });
                        context.contribute_arg(Argument::Object(reference));
                    }
                    Opcode::CondRefOf => {
                        let [Argument::Object(object), target] = &op.arguments[..] else { panic!() };
                        let result = if let Object::Reference { kind: ReferenceKind::Unresolved, .. } = **object {
                            Object::Integer(0)
                        } else {
                            let reference =
                                Arc::new(Object::Reference { kind: ReferenceKind::RefOf, inner: object.clone() });
                            self.do_store(&mut context, target, reference)?;
                            Object::Integer(u64::MAX)
                        };
                        context.contribute_arg(Argument::Object(Arc::new(result)));
                    }
                    Opcode::Sleep => {
                        let [Argument::Object(msec)] = &op.arguments[..] else { panic!() };
                        self.handler.sleep(msec.as_integer()?);
                    }
                    Opcode::Stall => {
                        let [Argument::Object(usec)] = &op.arguments[..] else { panic!() };
                        self.handler.stall(usec.as_integer()?);
                    }
                    Opcode::InternalMethodCall => {
                        let [Argument::Object(method), Argument::Namestring(method_scope)] = &op.arguments[0..2]
                        else {
                            panic!()
                        };

                        let args = op.arguments[2..]
                            .iter()
                            .map(|arg| {
                                if let Argument::Object(arg) = arg {
                                    arg.clone()
                                } else {
                                    panic!();
                                }
                            })
                            .collect();

                        self.namespace.lock().add_level(method_scope.clone(), NamespaceLevelKind::MethodLocals)?;

                        let new_context =
                            MethodContext::new_from_method(method.clone(), args, method_scope.clone())?;
                        let old_context = mem::replace(&mut context, new_context);
                        self.context_stack.lock().push(old_context);
                    }
                    Opcode::Return => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };

                        if let Some(last) = self.context_stack.lock().pop() {
                            context = last;
                            context.contribute_arg(Argument::Object(object.clone()));
                        } else {
                            /*
                             * If this is the top-most context, this is a `Return` from the actual
                             * method.
                             */
                            return Ok(object.clone());
                        }
                    }
                    Opcode::ObjectType => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        // TODO: this should technically support scopes as well - this is less easy
                        // (they should return `0`)
                        let typ = match object.typ() {
                            ObjectType::Uninitialized => 0,
                            ObjectType::Integer => 1,
                            ObjectType::String => 2,
                            ObjectType::Buffer => 3,
                            ObjectType::Package => 4,
                            ObjectType::FieldUnit => 5,
                            ObjectType::Device => 6,
                            ObjectType::Event => 7,
                            ObjectType::Method => 8,
                            ObjectType::Mutex => 9,
                            ObjectType::OpRegion => 10,
                            ObjectType::PowerResource => 11,
                            ObjectType::Processor => 12,
                            ObjectType::ThermalZone => 13,
                            ObjectType::BufferField => 14,
                            // XXX: 15 is reserved
                            ObjectType::Debug => 16,
                            ObjectType::Reference => panic!(),
                            ObjectType::RawDataBuffer => todo!(),
                        };

                        context.contribute_arg(Argument::Object(Arc::new(Object::Integer(typ))));
                    }
                    Opcode::SizeOf => self.do_size_of(&mut context, op)?,
                    Opcode::Index => self.do_index(&mut context, op)?,
                    Opcode::BankField => {
                        let [
                            Argument::TrackedPc(start_pc),
                            Argument::PkgLength(pkg_length),
                            Argument::Namestring(region_name),
                            Argument::Namestring(bank_name),
                            Argument::Object(bank_value),
                        ] = &op.arguments[..]
                        else {
                            panic!()
                        };
                        let bank_value = bank_value.as_integer()?;
                        let field_flags = context.next()?;

                        let (region, bank) = {
                            let namespace = self.namespace.lock();
                            let (_, region) = namespace.search(region_name, &context.current_scope)?;
                            let (_, bank) = namespace.search(bank_name, &context.current_scope)?;
                            (region, bank)
                        };

                        let kind = FieldUnitKind::Bank { region, bank, bank_value };
                        self.parse_field_list(&mut context, kind, *start_pc, *pkg_length, field_flags)?;
                    }
                    Opcode::While => {
                        /*
                         * We've just evaluated the predicate for an iteration of a while loop. If
                         * false, skip over the rest of the loop, otherwise carry on.
                         */
                        let [Argument::Object(predicate)] = &op.arguments[..] else { panic!() };
                        let predicate = predicate.as_integer()?;

                        if predicate == 0 {
                            // Exit from the while loop by skipping out of the current block
                            context.current_block = context.block_stack.pop().unwrap();
                        }
                    }
                    _ => panic!("Unexpected operation has created in-flight op!"),
                }
            }

            /*
             * Now that we've retired as many in-flight operations as we have arguments for, move
             * forward in the AML stream.
             */
            let opcode = match context.opcode() {
                Ok(opcode) => opcode,
                Err(AmlError::RunOutOfStream) => {
                    /*
                     * We've reached the end of the current block. What we should do about this
                     * depends on what type of block it was.
                     */
                    match context.current_block.kind {
                        BlockKind::Table => {
                            break Ok(Arc::new(Object::Uninitialized));
                        }
                        BlockKind::Method { method_scope } => {
                            self.namespace.lock().remove_level(method_scope)?;

                            if let Some(prev_context) = self.context_stack.lock().pop() {
                                context = prev_context;
                                continue;
                            } else {
                                /*
                                 * If there is no explicit `Return` op, the result is undefined. We
                                 * just return an uninitialized object.
                                 */
                                return Ok(Arc::new(Object::Uninitialized));
                            }
                        }
                        BlockKind::Scope { old_scope } => {
                            assert!(context.block_stack.len() > 0);
                            context.current_block = context.block_stack.pop().unwrap();
                            context.current_scope = old_scope;
                            // Go round the loop again to get the next opcode for the new block
                            continue;
                        }
                        BlockKind::Package => {
                            /*
                             * We've reached the end of the package. The in-flight op may have
                             * already been completed in the case of the package specifying all of
                             * its elements, or reach the end of the block here if it does not.
                             *
                             * In the latter case, fill in the rest of the package with
                             * *distinct* uninitialized objects, and go round again to complete the
                             * in-flight op.
                             */
                            assert!(context.block_stack.len() > 0);

                            if let Some(package_op) = context.in_flight.last_mut()
                                && package_op.op == Opcode::Package
                            {
                                let num_elements_left = package_op.expected_arguments - package_op.arguments.len();
                                for _ in 0..num_elements_left {
                                    package_op.arguments.push(Argument::Object(Arc::new(Object::Uninitialized)));
                                }
                            }

                            // XXX: don't remove the package's block. Refer to completion of
                            // package ops for rationale here.
                            continue;
                        }
                        BlockKind::IfThenBranch => {
                            context.current_block = context.block_stack.pop().unwrap();

                            // Check for an else-branch, and skip over it
                            // TODO: if we run out of stream here, it might just be an IfOp at the
                            // end I think?
                            const DEF_ELSE_OP: u8 = 0xa1;
                            if context.peek()? == DEF_ELSE_OP {
                                context.next()?;
                                let start_pc = context.current_block.pc;
                                let else_length = context.pkglength()?;
                                context.current_block.pc += else_length - (context.current_block.pc - start_pc);
                            }

                            continue;
                        }
                        BlockKind::While { start_pc } => {
                            /*
                             * Go round again, and create a new in-flight op to have a look at the
                             * predicate.
                             */
                            context.current_block.pc = start_pc;
                            context.start_in_flight_op(OpInFlight::new(Opcode::While, 1));
                            continue;
                        }
                    }
                }
                Err(other_err) => return Err(other_err),
            };
            match opcode {
                Opcode::Zero => {
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(0))));
                }
                Opcode::One => {
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(1))));
                }
                Opcode::Alias => {
                    let source = context.namestring()?;
                    let alias = context.namestring()?;

                    let mut namespace = self.namespace.lock();
                    let object = namespace.get(source.resolve(&context.current_scope)?)?.clone();
                    let alias = alias.resolve(&context.current_scope)?;
                    namespace.create_alias(alias, object)?;
                }
                Opcode::Name => {
                    let name = context.namestring()?;
                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::Name,
                        vec![Argument::Namestring(name)],
                        1,
                    ));
                }
                Opcode::BytePrefix => {
                    let value = context.next()?;
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(value as u64))));
                }
                Opcode::WordPrefix => {
                    let value = context.next_u16()?;
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(value as u64))));
                }
                Opcode::DWordPrefix => {
                    let value = context.next_u32()?;
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(value as u64))));
                }
                Opcode::StringPrefix => {
                    let str_start = context.current_block.pc;
                    while context.next()? != b'\0' {}
                    // TODO: handle err
                    let str = String::from(
                        str::from_utf8(&context.current_block.stream()[str_start..(context.current_block.pc - 1)])
                            .unwrap(),
                    );
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::String(str))));
                }
                Opcode::QWordPrefix => {
                    let value = context.next_u64()?;
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(value))));
                }
                Opcode::Scope => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let name = context.namestring()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    let new_scope = name.resolve(&context.current_scope)?;
                    self.namespace.lock().add_level(new_scope.clone(), NamespaceLevelKind::Scope)?;

                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::Buffer => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::Buffer,
                        vec![Argument::TrackedPc(start_pc), Argument::PkgLength(pkg_length)],
                        1,
                    ));
                }
                Opcode::Package => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let num_elements = context.next()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    /*
                     * We now need to interpret an arbitrary number of package elements, bounded by
                     * the remaining pkglength. This may be less than `num_elements` - the
                     * remaining elements of the package are uninitialized. We utilise a
                     * combination of a block to manage the pkglength, plus an in-flight op to
                     * store interpreted arguments.
                     */
                    context.start_in_flight_op(OpInFlight::new(Opcode::Package, num_elements as usize));
                    context.start_new_block(BlockKind::Package, remaining_length);
                }
                Opcode::VarPackage => todo!(),
                Opcode::Method => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let name = context.namestring()?;
                    let flags = MethodFlags(context.next()?);

                    let code_len = pkg_length - (context.current_block.pc - start_pc);
                    let code = context.current_block.stream()
                        [context.current_block.pc..(context.current_block.pc + code_len)]
                        .to_vec();
                    context.current_block.pc += code_len;

                    let name = name.resolve(&context.current_scope)?;
                    self.namespace.lock().insert(name, Arc::new(Object::Method { code, flags }))?;
                }
                Opcode::External => {
                    let _name = context.namestring()?;
                    let _object_type = context.next()?;
                    let _arg_count = context.next()?;
                }
                Opcode::Mutex => {
                    let name = context.namestring()?;
                    let sync_level = context.next()?;

                    let name = name.resolve(&context.current_scope)?;
                    self.namespace.lock().insert(name, Arc::new(Object::Mutex { sync_level }))?;
                }
                Opcode::Event => {
                    let name = context.namestring()?;

                    let name = name.resolve(&context.current_scope)?;
                    self.namespace.lock().insert(name, Arc::new(Object::Event))?;
                }
                Opcode::LoadTable => todo!(),
                Opcode::Load => todo!(),
                Opcode::Stall => context.start_in_flight_op(OpInFlight::new(Opcode::Stall, 1)),
                Opcode::Sleep => context.start_in_flight_op(OpInFlight::new(Opcode::Sleep, 1)),
                Opcode::Acquire => todo!(),
                Opcode::Signal => todo!(),
                Opcode::Wait => todo!(),
                Opcode::Reset => todo!(),
                Opcode::Release => todo!(),
                Opcode::FromBCD | Opcode::ToBCD => context.start_in_flight_op(OpInFlight::new(opcode, 2)),
                Opcode::Revision => {
                    context.contribute_arg(Argument::Object(Arc::new(Object::Integer(INTERPRETER_REVISION))));
                }
                Opcode::Debug => {
                    context.contribute_arg(Argument::Object(Arc::new(Object::Debug)));
                }
                Opcode::Fatal => {
                    let typ = context.next()?;
                    let code = context.next_u32()?;
                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::Fatal,
                        vec![Argument::ByteData(typ), Argument::DWordData(code)],
                        1,
                    ));
                }
                Opcode::Timer => {
                    // Time has to be monotonically-increasing, in 100ns units
                    let time = self.handler.nanos_since_boot() / 100;
                    context.contribute_arg(Argument::Object(Arc::new(Object::Integer(time))));
                }
                Opcode::OpRegion => {
                    let name = context.namestring()?;
                    let region_space = context.next()?;
                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::OpRegion,
                        vec![Argument::Namestring(name), Argument::ByteData(region_space)],
                        2,
                    ));
                }
                Opcode::Field => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let region_name = context.namestring()?;
                    let field_flags = context.next()?;

                    let region = self.namespace.lock().get(region_name.resolve(&context.current_scope)?)?.clone();
                    let kind = FieldUnitKind::Normal { region };
                    self.parse_field_list(&mut context, kind, start_pc, pkg_length, field_flags)?;
                }
                Opcode::BankField => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let region_name = context.namestring()?;
                    let bank_name = context.namestring()?;

                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::BankField,
                        vec![
                            Argument::TrackedPc(start_pc),
                            Argument::PkgLength(pkg_length),
                            Argument::Namestring(region_name),
                            Argument::Namestring(bank_name),
                        ],
                        1,
                    ));
                }
                Opcode::IndexField => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let index_name = context.namestring()?;
                    let data_name = context.namestring()?;
                    let field_flags = context.next()?;

                    let (index, data) = {
                        let namespace = self.namespace.lock();
                        let (_, index) = namespace.search(&index_name, &context.current_scope)?;
                        let (_, data) = namespace.search(&data_name, &context.current_scope)?;
                        (index, data)
                    };
                    let kind = FieldUnitKind::Index { index, data };
                    self.parse_field_list(&mut context, kind, start_pc, pkg_length, field_flags)?;
                }
                Opcode::Device | Opcode::ThermalZone => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let name = context.namestring()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    let new_scope = name.resolve(&context.current_scope)?;
                    let (kind, object) = match opcode {
                        Opcode::Device => (NamespaceLevelKind::Device, Object::Device),
                        Opcode::ThermalZone => (NamespaceLevelKind::ThermalZone, Object::ThermalZone),
                        _ => unreachable!(),
                    };
                    let mut namespace = self.namespace.lock();
                    namespace.add_level(new_scope.clone(), kind)?;
                    namespace.insert(new_scope.clone(), Arc::new(object))?;

                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::Processor => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let name = context.namestring()?;
                    let proc_id = context.next()?;
                    let pblk_address = context.next_u32()?;
                    let pblk_length = context.next()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    let new_scope = name.resolve(&context.current_scope)?;
                    let object = Object::Processor { proc_id, pblk_address, pblk_length };
                    let mut namespace = self.namespace.lock();
                    namespace.add_level(new_scope.clone(), NamespaceLevelKind::Processor)?;
                    namespace.insert(new_scope.clone(), Arc::new(object))?;

                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::PowerRes => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let name = context.namestring()?;
                    let system_level = context.next()?;
                    let resource_order = context.next_u16()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    let new_scope = name.resolve(&context.current_scope)?;
                    let object = Object::PowerResource { system_level, resource_order };
                    let mut namespace = self.namespace.lock();
                    namespace.add_level(new_scope.clone(), NamespaceLevelKind::PowerResource)?;
                    namespace.insert(new_scope.clone(), Arc::new(object))?;

                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::DataRegion => todo!(),
                Opcode::Local(local) => {
                    let local = context.locals[local as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Reference {
                        kind: ReferenceKind::LocalOrArg,
                        inner: local,
                    })));
                }
                Opcode::Arg(arg) => {
                    let arg = context.args[arg as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Reference {
                        kind: ReferenceKind::LocalOrArg,
                        inner: arg,
                    })));
                }
                Opcode::Store => context.start_in_flight_op(OpInFlight::new(Opcode::Store, 2)),
                Opcode::RefOf => context.start_in_flight_op(OpInFlight::new(Opcode::RefOf, 1)),
                Opcode::CondRefOf => context.start_in_flight_op(OpInFlight::new(opcode, 2)),

                Opcode::DualNamePrefix
                | Opcode::MultiNamePrefix
                | Opcode::Digit(_)
                | Opcode::NameChar(_)
                | Opcode::RootChar
                | Opcode::ParentPrefixChar => {
                    context.current_block.pc -= 1;
                    let name = context.namestring()?;

                    match self.namespace.lock().search(&name, &context.current_scope) {
                        Ok((resolved_name, object)) => {
                            if let Object::Method { flags, .. } = *object {
                                context.start_in_flight_op(OpInFlight::new_with(
                                    Opcode::InternalMethodCall,
                                    vec![Argument::Object(object), Argument::Namestring(resolved_name)],
                                    flags.arg_count(),
                                ))
                            } else {
                                context.last_op()?.arguments.push(Argument::Object(object));
                            }
                        }
                        Err(AmlError::ObjectDoesNotExist(_)) => {
                            let allow_unresolved = context.current_block.kind == BlockKind::Package
                                || context.in_flight.last().map(|op| op.op == Opcode::CondRefOf).unwrap_or(false);
                            if allow_unresolved {
                                let reference = Object::Reference {
                                    kind: ReferenceKind::Unresolved,
                                    inner: Arc::new(Object::String(name.to_string())),
                                };
                                context.last_op()?.arguments.push(Argument::Object(Arc::new(reference)));
                            }
                        }
                        Err(other) => Err(other)?,
                    }
                }

                Opcode::Add
                | Opcode::Subtract
                | Opcode::Multiply
                | Opcode::ShiftLeft
                | Opcode::ShiftRight
                | Opcode::Mod
                | Opcode::Nand
                | Opcode::And
                | Opcode::Or
                | Opcode::Nor
                | Opcode::Xor
                | Opcode::Concat => {
                    context.start_in_flight_op(OpInFlight::new(opcode, 3));
                }

                Opcode::Divide => context.start_in_flight_op(OpInFlight::new(Opcode::Divide, 4)),
                Opcode::Increment | Opcode::Decrement => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Not => context.start_in_flight_op(OpInFlight::new(Opcode::Not, 2)),
                Opcode::FindSetLeftBit | Opcode::FindSetRightBit => {
                    context.start_in_flight_op(OpInFlight::new(opcode, 2))
                }
                Opcode::DerefOf => todo!(),
                Opcode::Notify => todo!(),
                Opcode::ConcatRes => context.start_in_flight_op(OpInFlight::new(opcode, 3)),
                Opcode::SizeOf => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Index => context.start_in_flight_op(OpInFlight::new(opcode, 3)),
                Opcode::Match => todo!(),

                Opcode::CreateBitField
                | Opcode::CreateByteField
                | Opcode::CreateWordField
                | Opcode::CreateDWordField
                | Opcode::CreateQWordField => context.start_in_flight_op(OpInFlight::new(opcode, 2)),
                Opcode::CreateField => context.start_in_flight_op(OpInFlight::new(Opcode::CreateField, 3)),

                Opcode::LAnd
                | Opcode::LOr
                | Opcode::LNot
                | Opcode::LNotEqual
                | Opcode::LLessEqual
                | Opcode::LGreaterEqual
                | Opcode::LEqual
                | Opcode::LGreater
                | Opcode::LLess => {
                    context.start_in_flight_op(OpInFlight::new(opcode, 2));
                }

                Opcode::ToBuffer
                | Opcode::ToDecimalString
                | Opcode::ToHexString
                | Opcode::ToInteger
                | Opcode::ToString => context.start_in_flight_op(OpInFlight::new(opcode, 2)),

                Opcode::ObjectType => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::CopyObject => todo!(),
                Opcode::Mid => context.start_in_flight_op(OpInFlight::new(Opcode::Mid, 4)),
                Opcode::If => {
                    let start_pc = context.current_block.pc;
                    let then_length = context.pkglength()?;
                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::If,
                        vec![Argument::TrackedPc(start_pc), Argument::PkgLength(then_length)],
                        1,
                    ));
                }
                Opcode::Else => return Err(AmlError::ElseFoundWithoutCorrespondingIf),
                Opcode::While => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);
                    context.start_new_block(
                        BlockKind::While { start_pc: context.current_block.pc },
                        remaining_length,
                    );
                    context.start_in_flight_op(OpInFlight::new(Opcode::While, 1));
                }
                Opcode::Continue => {
                    if let BlockKind::While { start_pc } = &context.current_block.kind {
                        context.current_block.pc = *start_pc;
                    } else {
                        loop {
                            let Some(block) = context.block_stack.pop() else {
                                Err(AmlError::ContinueOutsideOfWhile)?
                            };
                            if let BlockKind::While { start_pc } = block.kind {
                                context.current_block.pc = start_pc;
                                break;
                            }
                        }
                    }
                    context.start_in_flight_op(OpInFlight::new(Opcode::While, 1));
                }
                Opcode::Break => {
                    if let BlockKind::While { .. } = &context.current_block.kind {
                        context.current_block = context.block_stack.pop().unwrap();
                    } else {
                        loop {
                            let Some(block) = context.block_stack.pop() else {
                                Err(AmlError::BreakOutsideOfWhile)?
                            };
                            if let BlockKind::While { .. } = block.kind {
                                context.current_block = context.block_stack.pop().unwrap();
                                break;
                            }
                        }
                    }
                }
                Opcode::Return => context.start_in_flight_op(OpInFlight::new(Opcode::Return, 1)),
                Opcode::Noop => {}
                Opcode::Breakpoint => {
                    self.handler.breakpoint();
                }
                Opcode::Ones => {
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(u64::MAX))));
                }

                Opcode::InternalMethodCall => panic!(),
            }
        }
    }

    fn parse_field_list(
        &self,
        context: &mut MethodContext,
        kind: FieldUnitKind,
        start_pc: usize,
        pkg_length: usize,
        flags: u8,
    ) -> Result<(), AmlError> {
        const RESERVED_FIELD: u8 = 0x00;
        const ACCESS_FIELD: u8 = 0x01;
        const CONNECT_FIELD: u8 = 0x02;
        const EXTENDED_ACCESS_FIELD: u8 = 0x03;

        let mut field_offset = 0;

        while context.current_block.pc < (start_pc + pkg_length) {
            match context.next()? {
                RESERVED_FIELD => {
                    let length = context.pkglength()?;
                    field_offset += length;
                }
                ACCESS_FIELD => {
                    let access_type = context.next()?;
                    let access_attrib = context.next()?;
                    todo!()
                    // TODO
                }
                CONNECT_FIELD => {
                    // TODO: either consume a namestring or `BufferData` (it's not
                    // clear what a buffer data acc is lmao)
                    todo!("Connect field :(");
                }
                EXTENDED_ACCESS_FIELD => {
                    todo!("Extended access field :(");
                }
                _ => {
                    context.current_block.pc -= 1;
                    // TODO: this should only ever be a nameseg really...
                    let field_name = context.namestring()?;
                    let field_length = context.pkglength()?;

                    let field = Object::FieldUnit(FieldUnit {
                        kind: kind.clone(),
                        bit_index: field_offset,
                        bit_length: field_length,
                        flags: FieldFlags(flags),
                    });
                    self.namespace.lock().insert(field_name.resolve(&context.current_scope)?, Arc::new(field))?;

                    field_offset += field_length;
                }
            }
        }

        Ok(())
    }

    fn do_binary_maths(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(left), Argument::Object(right), target] = &op.arguments[0..3] else { panic!() };
        let target2 = if op.op == Opcode::Divide { Some(&op.arguments[3]) } else { None };

        let left = left.clone().unwrap_transparent_reference().as_integer()?;
        let right = right.clone().unwrap_transparent_reference().as_integer()?;

        let result = match op.op {
            Opcode::Add => left.wrapping_add(right),
            Opcode::Subtract => left.wrapping_sub(right),
            Opcode::Multiply => left.wrapping_mul(right),
            Opcode::Divide => {
                if let Some(remainder) = target2 {
                    self.do_store(context, remainder, Arc::new(Object::Integer(left.wrapping_rem(right))))?;
                }
                left.wrapping_div_euclid(right)
            }
            Opcode::ShiftLeft => left.wrapping_shl(right as u32),
            Opcode::ShiftRight => left.wrapping_shr(right as u32),
            Opcode::Mod => left.wrapping_rem(right),
            Opcode::Nand => !(left & right),
            Opcode::And => left & right,
            Opcode::Or => left | right,
            Opcode::Nor => !(left | right),
            Opcode::Xor => left ^ right,
            _ => panic!(),
        };

        let result = Arc::new(Object::Integer(result));
        // TODO: use result for arg
        self.do_store(context, target, result.clone())?;
        context.contribute_arg(Argument::Object(result));
        Ok(())
    }

    fn do_unary_maths(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(operand)] = &op.arguments[..] else { panic!() };
        let operand = operand.clone().unwrap_transparent_reference().as_integer()?;

        let result = match op.op {
            Opcode::FindSetLeftBit => {
                if operand == 0 {
                    0
                } else {
                    /*
                     * TODO: this is a particular instance where not respecting integers being
                     * 32-bit on revision 1 tables does cause properly incorrect behaviour...
                     * TODO: we can fix this now we have the DSDT revision
                     */
                    (operand.leading_zeros() + 1) as u64
                }
            }
            Opcode::FindSetRightBit => {
                if operand == 0 {
                    0
                } else {
                    (operand.trailing_zeros() + 1) as u64
                }
            }
            Opcode::Not => {
                if operand == 0 {
                    u64::MAX
                } else {
                    0
                }
            }
            _ => panic!(),
        };

        context.contribute_arg(Argument::Object(Arc::new(Object::Integer(result))));
        Ok(())
    }

    fn do_logical_op(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        if op.op == Opcode::LNot {
            let [Argument::Object(operand)] = &op.arguments[..] else { panic!() };
            let operand = operand.clone().unwrap_transparent_reference().as_integer()?;
            let result = if operand == 0 { u64::MAX } else { 0 };

            if let Some(prev_op) = context.in_flight.last_mut() {
                if prev_op.arguments.len() < prev_op.expected_arguments {
                    prev_op.arguments.push(Argument::Object(Arc::new(Object::Integer(result))));
                }
            }

            return Ok(());
        }

        let [Argument::Object(left), Argument::Object(right)] = &op.arguments[..] else { panic!() };

        /*
         * Some of these operations allow strings and buffers to be used as operands. Apparently
         * NT's interpreter just takes the first 4 bytes of the string/buffer and casts them as an
         * integer...
         */
        let left = left.clone().unwrap_transparent_reference();
        let right = right.clone().unwrap_transparent_reference();
        let (left, right) = match *left {
            Object::Integer(left) => (left, right.as_integer()?),
            Object::String(ref left) => {
                let left = {
                    let mut bytes = [0u8; 4];
                    let left_bytes = left.as_bytes();
                    (bytes[0..left_bytes.len()]).copy_from_slice(left_bytes);
                    u32::from_le_bytes(bytes) as u64
                };
                let right = {
                    let mut bytes = [0u8; 4];
                    let right = right.as_string()?;
                    let right_bytes = right.as_bytes();
                    (bytes[0..right_bytes.len()]).copy_from_slice(right_bytes);
                    u32::from_le_bytes(bytes) as u64
                };
                (left, right)
            }
            Object::Buffer(ref left) => {
                let Object::Buffer(ref right) = *right else { panic!() };
                let left = {
                    let mut bytes = [0u8; 4];
                    (bytes[0..left.len()]).copy_from_slice(left);
                    u32::from_le_bytes(bytes) as u64
                };
                let right = {
                    let mut bytes = [0u8; 4];
                    (bytes[0..right.len()]).copy_from_slice(right);
                    u32::from_le_bytes(bytes) as u64
                };
                (left, right)
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::LogicalOp, typ: left.typ() })?,
        };

        let result = match op.op {
            Opcode::LAnd => (left > 0) && (right > 0),
            Opcode::LOr => (left > 0) || (right > 0),
            Opcode::LNotEqual => left != right,
            Opcode::LLessEqual => left <= right,
            Opcode::LGreaterEqual => left >= right,
            Opcode::LEqual => left == right,
            Opcode::LGreater => left > right,
            Opcode::LLess => left < right,
            _ => panic!(),
        };
        let result = if result { Object::Integer(u64::MAX) } else { Object::Integer(0) };

        context.contribute_arg(Argument::Object(Arc::new(result)));
        Ok(())
    }

    fn do_mid(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(source), Argument::Object(index), Argument::Object(length), target] =
            &op.arguments[..]
        else {
            panic!()
        };
        let index = index.as_integer()? as usize;
        let length = length.as_integer()? as usize;

        let result = Arc::new(match **source {
            Object::String(ref string) => {
                if index >= string.len() {
                    Object::String(String::new())
                } else {
                    let upper = usize::min(index + length, index + string.len());
                    let chars = &string[index..upper];
                    Object::String(String::from(chars))
                }
            }
            Object::Buffer(ref buffer) => {
                if index >= buffer.len() {
                    Object::Buffer(vec![])
                } else {
                    let upper = usize::min(index + length, index + buffer.len());
                    let bytes = &buffer[index..upper];
                    Object::Buffer(bytes.to_vec())
                }
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::Mid, typ: source.typ() })?,
        });

        self.do_store(context, target, result.clone())?;
        context.contribute_arg(Argument::Object(result));

        Ok(())
    }

    fn do_concat(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(source1), Argument::Object(source2), target] = &op.arguments[..] else { panic!() };
        fn resolve_as_string(obj: &Object) -> String {
            match obj {
                Object::Uninitialized => "[Uninitialized Object]".to_string(),
                Object::Buffer(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
                Object::BufferField { .. } => "[Buffer Field]".to_string(),
                Object::Device => "[Device]".to_string(),
                Object::Event => "[Event]".to_string(),
                Object::FieldUnit(_) => "[Field]".to_string(),
                Object::Integer(value) => value.to_string(),
                Object::Method { .. } => "[Control Method]".to_string(),
                Object::Mutex { .. } => "[Mutex]".to_string(),
                Object::Reference { inner, .. } => resolve_as_string(&*(inner.clone().unwrap_reference())),
                Object::OpRegion(_) => "[Operation Region]".to_string(),
                Object::Package(_) => "[Package]".to_string(),
                Object::PowerResource { .. } => "[Power Resource]".to_string(),
                Object::Processor { .. } => "[Processor]".to_string(),
                // TODO: what even is one of these??
                Object::RawDataBuffer => todo!(),
                Object::String(value) => value.clone(),
                Object::ThermalZone => "[Thermal Zone]".to_string(),
                Object::Debug => "[Debug Object]".to_string(),
            }
        }
        let result = match source1.typ() {
            ObjectType::Integer => {
                let source1 = source1.as_integer()?;
                let source2 = source2.to_integer(if self.dsdt_revision >= 2 { 8 } else { 4 })?;
                let mut buffer = Vec::new();
                if self.dsdt_revision >= 2 {
                    buffer.extend_from_slice(&source1.to_le_bytes());
                    buffer.extend_from_slice(&source2.to_le_bytes());
                } else {
                    buffer.extend_from_slice(&(source1 as u32).to_le_bytes());
                    buffer.extend_from_slice(&(source2 as u32).to_le_bytes());
                }
                Arc::new(Object::Buffer(buffer))
            }
            ObjectType::Buffer => {
                let mut buffer = source1.as_buffer()?.to_vec();
                buffer.extend(source2.to_buffer(if self.dsdt_revision >= 2 { 8 } else { 4 })?);
                Arc::new(Object::Buffer(buffer))
            }
            ObjectType::String | _ => {
                let source1 = resolve_as_string(&source1);
                let source2 = resolve_as_string(&source2);
                Arc::new(Object::String(source1 + &source2))
            }
        };
        // TODO: use result of store
        self.do_store(context, target, result.clone())?;
        context.contribute_arg(Argument::Object(result));
        Ok(())
    }

    fn do_from_bcd(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(value)] = &op.arguments[..] else { panic!() };
        let mut value = value.clone().unwrap_transparent_reference().as_integer()?;

        let mut result = 0;
        let mut i = 1;
        while value > 0 {
            result += (value & 0x0f) * i;
            i *= 10;
            value >>= 4;
        }

        context.contribute_arg(Argument::Object(Arc::new(Object::Integer(result))));
        Ok(())
    }

    fn do_to_bcd(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(value)] = &op.arguments[..] else { panic!() };
        let mut value = value.clone().unwrap_transparent_reference().as_integer()?;

        let mut result = 0;
        let mut i = 0;
        while value > 0 {
            result |= (value % 10) << (4 * i);
            value /= 10;
            i += 1;
        }

        context.contribute_arg(Argument::Object(Arc::new(Object::Integer(result))));
        Ok(())
    }

    fn do_size_of(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
        let object = object.clone().unwrap_reference();

        let result = match *object {
            Object::Buffer(ref buffer) => buffer.len(),
            Object::String(ref str) => str.len(),
            Object::Package(ref package) => package.len(),
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::SizeOf, typ: object.typ() })?,
        };

        context.contribute_arg(Argument::Object(Arc::new(Object::Integer(result as u64))));
        Ok(())
    }

    fn do_index(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(object), Argument::Object(index_value), target] = &op.arguments[..] else {
            panic!()
        };
        let index_value = index_value.as_integer()?;

        let result = Arc::new(match **object {
            Object::Buffer(ref buffer) => {
                if index_value as usize >= buffer.len() {
                    Err(AmlError::IndexOutOfBounds)?
                }

                Object::Reference {
                    kind: ReferenceKind::RefOf,
                    inner: Arc::new(Object::BufferField {
                        buffer: object.clone(),
                        offset: index_value as usize * 8,
                        length: 8,
                    }),
                }
            }
            Object::String(ref string) => {
                if index_value as usize >= string.len() {
                    Err(AmlError::IndexOutOfBounds)?
                }

                Object::Reference {
                    kind: ReferenceKind::RefOf,
                    inner: Arc::new(Object::BufferField {
                        buffer: object.clone(),
                        offset: index_value as usize * 8,
                        length: 8,
                    }),
                }
            }
            Object::Package(ref package) => {
                let Some(element) = package.get(index_value as usize) else { Err(AmlError::IndexOutOfBounds)? };
                Object::Reference { kind: ReferenceKind::RefOf, inner: element.clone() }
            }
            _ => Err(AmlError::IndexOutOfBounds)?,
        });

        self.do_store(context, target, result.clone())?;
        context.contribute_arg(Argument::Object(result));
        Ok(())
    }
    fn do_store(
        &self,
        context: &mut MethodContext,
        target: &Argument,
        object: Arc<Object>,
    ) -> Result<(), AmlError> {
        // TODO: find the destination (need to handle references, debug objects, etc.)
        // TODO: convert object to be of the type of destination, in line with 19.3.5 of the spec
        // TODO: write the object to the destination, including e.g. field writes that then lead to
        // literally god knows what.
        let object = object.unwrap_transparent_reference();
        match target {
            Argument::Object(target) => match target.gain_mut() {
                Object::Integer(target) => match object.gain_mut() {
                    Object::Integer(value) => {
                        *target = *value;
                    }
                    Object::BufferField { .. } => {
                        let mut buffer = [0u8; 8];
                        object.gain_mut().read_buffer_field(&mut buffer)?;
                        let value = u64::from_le_bytes(buffer);
                        *target = value;
                    }
                    Object::FieldUnit(field) => {
                        // TODO: not sure if we should convert buffers to integers if needed here?
                        *target = self.do_field_read(field)?.as_integer()?;
                    }
                    _ => panic!("Store to integer from unsupported object: {:?}", object),
                },
                Object::BufferField { .. } => match object.gain_mut() {
                    Object::Integer(value) => {
                        target.gain_mut().write_buffer_field(&value.to_le_bytes())?;
                    }
                    Object::Buffer(value) => {
                        target.gain_mut().write_buffer_field(&value.as_slice())?;
                    }
                    _ => panic!(),
                },
                Object::Debug => {
                    self.handler.handle_debug(&*object);
                }
                _ => panic!("Stores to objects like {:?} are not yet supported", target),
            },

            Argument::Namestring(_) => {}
            Argument::ByteData(_) | Argument::DWordData(_) | Argument::TrackedPc(_) | Argument::PkgLength(_) => {
                panic!()
            }
        }
        Ok(())
    }

    /// Do a read from a field by performing one or more well-formed accesses to the underlying
    /// operation regions, and then shifting and masking the resulting value as appropriate. Will
    /// return either an `Integer` or `Buffer` as appropriate, guided by the size of the field
    /// and expected integer size (as per the DSDT revision).
    fn do_field_read(&self, field: &FieldUnit) -> Result<Arc<Object>, AmlError> {
        let needs_buffer = if self.dsdt_revision >= 2 { field.bit_length > 64 } else { field.bit_length > 32 };
        let access_width_bits = field.flags.access_type_bytes()? * 8;

        trace!("AML field read. Field = {:?}", field);

        // TODO: if the field needs to be locked, acquire/release a global mutex?

        enum Output {
            Integer([u8; 8]),
            Buffer(Vec<u8>),
        }
        let mut output = if needs_buffer {
            Output::Buffer(vec![0; field.bit_length.next_multiple_of(8)])
        } else {
            Output::Integer([0; 8])
        };
        let output_bytes = match &mut output {
            Output::Buffer(bytes) => bytes.as_mut_slice(),
            Output::Integer(value) => value,
        };

        match field.kind {
            FieldUnitKind::Normal { ref region } => {
                let Object::OpRegion(ref region) = **region else { panic!() };

                /*
                 * TODO: it might be worth having a fast path here for reads that don't do weird
                 * unaligned accesses, which I'm guessing might be relatively common on real
                 * hardware? Eg. single native read + mask
                 */

                /*
                 * Break the field read into native reads that respect the region's access width.
                 * Copy each potentially-unaligned part into the destination's bit range.
                 */
                let native_accesses_needed = (field.bit_length + (field.bit_index % access_width_bits))
                    .next_multiple_of(access_width_bits)
                    / access_width_bits;
                let mut read_so_far = 0;
                for i in 0..native_accesses_needed {
                    let aligned_offset =
                        object::align_down(field.bit_index + i * access_width_bits, access_width_bits);
                    let raw = self.do_native_region_read(region, aligned_offset / 8, access_width_bits / 8)?;
                    let src_index = if i == 0 { field.bit_index % access_width_bits } else { 0 };
                    let remaining_length = field.bit_length - read_so_far;
                    let length = if i == 0 {
                        usize::min(remaining_length, access_width_bits - (field.bit_index % access_width_bits))
                    } else {
                        usize::min(remaining_length, access_width_bits)
                    };

                    trace!(
                        "Extracting bits {}..{} from native read to bits {}..{}",
                        src_index,
                        src_index + length,
                        read_so_far,
                        read_so_far + length,
                    );
                    object::copy_bits(&raw.to_le_bytes(), src_index, output_bytes, read_so_far, length);

                    read_so_far += length;
                }

                match output {
                    Output::Buffer(bytes) => Ok(Arc::new(Object::Buffer(bytes))),
                    Output::Integer(value) => Ok(Arc::new(Object::Integer(u64::from_le_bytes(value)))),
                }
            }
            FieldUnitKind::Bank { ref region, ref bank, bank_value } => todo!(),
            FieldUnitKind::Index { ref index, ref data } => todo!(),
        }
    }

    /// Performs an actual read from an operation region. `offset` and `length` must respect the
    /// access requirements of the field being read, and are supplied in **bytes**. This may call
    /// AML methods if required, and may invoke user-supplied handlers.
    fn do_native_region_read(&self, region: &OpRegion, offset: usize, length: usize) -> Result<u64, AmlError> {
        trace!("Native field read. Region = {:?}, offset = {:#x}, length={:#x}", region, offset, length);

        match region.space {
            RegionSpace::SystemMemory => Ok({
                let address = region.base as usize + offset;
                match length {
                    1 => self.handler.read_u8(address) as u64,
                    2 => self.handler.read_u16(address) as u64,
                    4 => self.handler.read_u32(address) as u64,
                    8 => self.handler.read_u64(address) as u64,
                    _ => panic!(),
                }
            }),
            RegionSpace::SystemIO => Ok({
                let address = region.base as u16 + offset as u16;
                match length {
                    1 => self.handler.read_io_u8(address) as u64,
                    2 => self.handler.read_io_u16(address) as u64,
                    4 => self.handler.read_io_u32(address) as u64,
                    _ => panic!(),
                }
            }),
            RegionSpace::PciConfig => {
                /*
                 * TODO: it's not ideal to do these reads for every native access. See if we can
                 * cache them somewhere?
                 */
                let seg = match self.invoke_method_if_present(
                    AmlName::from_str("_SEG").unwrap().resolve(&region.parent_device_path)?,
                    vec![],
                )? {
                    Some(value) => value.as_integer()?,
                    None => 0,
                };
                let bus = match self.invoke_method_if_present(
                    AmlName::from_str("_BBR").unwrap().resolve(&region.parent_device_path)?,
                    vec![],
                )? {
                    Some(value) => value.as_integer()?,
                    None => 0,
                };
                let (device, function) = {
                    let adr = self.invoke_method_if_present(
                        AmlName::from_str("_ADR").unwrap().resolve(&region.parent_device_path)?,
                        vec![],
                    )?;
                    let adr = match adr {
                        Some(adr) => adr.as_integer()?,
                        None => 0,
                    };
                    (adr.get_bits(16..32), adr.get_bits(0..16))
                };

                let address = PciAddress::new(seg as u16, bus as u8, device as u8, function as u8);
                match length {
                    1 => Ok(self.handler.read_pci_u8(address, offset as u16) as u64),
                    2 => Ok(self.handler.read_pci_u16(address, offset as u16) as u64),
                    4 => Ok(self.handler.read_pci_u32(address, offset as u16) as u64),
                    _ => panic!(),
                }
            }

            RegionSpace::EmbeddedControl
            | RegionSpace::SmBus
            | RegionSpace::SystemCmos
            | RegionSpace::PciBarTarget
            | RegionSpace::Ipmi
            | RegionSpace::GeneralPurposeIo
            | RegionSpace::GenericSerialBus
            | RegionSpace::Pcc
            | RegionSpace::Oem(_) => {
                if let Some(handler) = self.region_handlers.lock().get(&region.space) {
                    todo!("Utilise handler");
                } else {
                    // TODO: panic or normal error here??
                    panic!("Unhandled region space that needs handler!");
                }
            }
        }
    }
}

/// A `MethodContext` represents a piece of running AML code - either a real method, or the
/// top-level of an AML table.
///
/// ### Safety
/// `MethodContext` does not keep the lifetime of the underlying AML stream, which for tables is
/// borrowed from the underlying physical mapping. This is because the interpreter needs to
/// preempt method contexts that execute other methods, and these contexts may have disparate
/// lifetimes. This is made safe in the case of methods by the context holding a reference to the
/// method object, but must be handled manually for AML tables.
struct MethodContext {
    current_block: Block,
    block_stack: Vec<Block>,
    in_flight: Vec<OpInFlight>,
    args: [Arc<Object>; 8],
    locals: [Arc<Object>; 8],
    current_scope: AmlName,

    _method: Option<Arc<Object>>,
}

#[derive(Debug)]
struct OpInFlight {
    op: Opcode,
    expected_arguments: usize,
    arguments: Vec<Argument>,
}

#[derive(Debug)]
enum Argument {
    Object(Arc<Object>),
    Namestring(AmlName),
    ByteData(u8),
    DWordData(u32),
    TrackedPc(usize),
    PkgLength(usize),
}

struct Block {
    stream: *const [u8],
    pc: usize,
    kind: BlockKind,
}

impl Block {
    fn stream(&self) -> &[u8] {
        unsafe { &*self.stream }
    }
}

#[derive(PartialEq, Debug)]
pub enum BlockKind {
    Table,
    Method {
        method_scope: AmlName,
    },
    Scope {
        old_scope: AmlName,
    },
    Package,
    /// Used for executing the then-branch of an `DefIfElse`. After finishing, it will check for
    /// and skip over an else-branch, if present.
    IfThenBranch,
    While {
        start_pc: usize,
    },
}

impl OpInFlight {
    pub fn new(op: Opcode, expected_arguments: usize) -> OpInFlight {
        OpInFlight { op, expected_arguments, arguments: Vec::new() }
    }

    pub fn new_with(op: Opcode, arguments: Vec<Argument>, more: usize) -> OpInFlight {
        OpInFlight { op, expected_arguments: arguments.len() + more, arguments }
    }
}

impl MethodContext {
    unsafe fn new_from_table(stream: &[u8]) -> MethodContext {
        let block = Block { stream: stream as *const [u8], pc: 0, kind: BlockKind::Table };
        MethodContext {
            current_block: block,
            block_stack: Vec::new(),
            in_flight: Vec::new(),
            args: core::array::from_fn(|_| Arc::new(Object::Uninitialized)),
            locals: core::array::from_fn(|_| Arc::new(Object::Uninitialized)),
            current_scope: AmlName::root(),
            _method: None,
        }
    }

    fn new_from_method(
        method: Arc<Object>,
        args: Vec<Arc<Object>>,
        scope: AmlName,
    ) -> Result<MethodContext, AmlError> {
        if let Object::Method { code, flags } = &*method {
            if args.len() != flags.arg_count() {
                return Err(AmlError::MethodArgCountIncorrect);
            }
            let block = Block {
                stream: code as &[u8] as *const [u8],
                pc: 0,
                kind: BlockKind::Method { method_scope: scope.clone() },
            };
            let args = core::array::from_fn(|i| {
                if let Some(arg) = args.get(i) { arg.clone() } else { Arc::new(Object::Uninitialized) }
            });
            let context = MethodContext {
                current_block: block,
                block_stack: Vec::new(),
                in_flight: Vec::new(),
                args,
                locals: core::array::from_fn(|_| Arc::new(Object::Uninitialized)),
                current_scope: scope,
                _method: Some(method.clone()),
            };
            Ok(context)
        } else {
            Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Method, got: method.typ() })
        }
    }

    fn last_op(&mut self) -> Result<&mut OpInFlight, AmlError> {
        match self.in_flight.last_mut() {
            Some(op) => Ok(op),
            None => Err(AmlError::NoCurrentOp),
        }
    }

    fn contribute_arg(&mut self, arg: Argument) {
        if let Some(in_flight) = self.in_flight.last_mut() {
            if in_flight.arguments.len() < in_flight.expected_arguments {
                in_flight.arguments.push(arg);
            }
        }
    }

    fn start_in_flight_op(&mut self, op: OpInFlight) {
        self.in_flight.push(op);
    }

    fn start_new_block(&mut self, kind: BlockKind, length: usize) {
        let block = Block {
            stream: &self.current_block.stream()[..(self.current_block.pc + length)] as *const [u8],
            pc: self.current_block.pc,
            kind,
        };
        self.current_block.pc += length;
        self.block_stack.push(mem::replace(&mut self.current_block, block));
    }

    fn opcode(&mut self) -> Result<Opcode, AmlError> {
        let opcode: u16 = match self.next()? {
            0x5b => {
                let ext = self.next()?;
                (0x5b << 8) as u16 | ext as u16
            }
            other => other as u16,
        };

        Ok(match opcode {
            0x00 => Opcode::Zero,
            0x01 => Opcode::One,
            0x06 => Opcode::Alias,
            0x08 => Opcode::Name,
            0x0a => Opcode::BytePrefix,
            0x0b => Opcode::WordPrefix,
            0x0c => Opcode::DWordPrefix,
            0x0d => Opcode::StringPrefix,
            0x0e => Opcode::QWordPrefix,
            0x10 => Opcode::Scope,
            0x11 => Opcode::Buffer,
            0x12 => Opcode::Package,
            0x13 => Opcode::VarPackage,
            0x14 => Opcode::Method,
            0x15 => Opcode::External,
            0x2e => Opcode::DualNamePrefix,
            0x2f => Opcode::MultiNamePrefix,
            0x30..=0x39 => Opcode::Digit(opcode as u8),    // b'0'..=b'9'
            0x41..=0x5a => Opcode::NameChar(opcode as u8), // b'A'..=b'Z'
            0x5b01 => Opcode::Mutex,
            0x5b02 => Opcode::Event,
            0x5b12 => Opcode::CondRefOf,
            0x5b13 => Opcode::CreateField,
            0x5b1f => Opcode::LoadTable,
            0x5b20 => Opcode::Load,
            0x5b21 => Opcode::Stall,
            0x5b22 => Opcode::Sleep,
            0x5b23 => Opcode::Acquire,
            0x5b24 => Opcode::Signal,
            0x5b25 => Opcode::Wait,
            0x5b26 => Opcode::Reset,
            0x5b27 => Opcode::Release,
            0x5b28 => Opcode::FromBCD,
            0x5b29 => Opcode::ToBCD,
            0x5b30 => Opcode::Revision,
            0x5b31 => Opcode::Debug,
            0x5b32 => Opcode::Fatal,
            0x5b33 => Opcode::Timer,
            0x5b80 => Opcode::OpRegion,
            0x5b81 => Opcode::Field,
            0x5b82 => Opcode::Device,
            0x5b83 => Opcode::Processor,
            0x5b84 => Opcode::PowerRes,
            0x5b85 => Opcode::ThermalZone,
            0x5b86 => Opcode::IndexField,
            0x5b87 => Opcode::BankField,
            0x5b88 => Opcode::DataRegion,
            0x5c => Opcode::RootChar,
            0x5e => Opcode::ParentPrefixChar,
            0x5f => Opcode::NameChar(b'_'),
            0x60..=0x67 => Opcode::Local(opcode as u8 - 0x60),
            0x68..=0x6e => Opcode::Arg(opcode as u8 - 0x68),
            0x70 => Opcode::Store,
            0x71 => Opcode::RefOf,
            0x72 => Opcode::Add,
            0x73 => Opcode::Concat,
            0x74 => Opcode::Subtract,
            0x75 => Opcode::Increment,
            0x76 => Opcode::Decrement,
            0x77 => Opcode::Multiply,
            0x78 => Opcode::Divide,
            0x79 => Opcode::ShiftLeft,
            0x7a => Opcode::ShiftRight,
            0x7b => Opcode::And,
            0x7c => Opcode::Nand,
            0x7d => Opcode::Or,
            0x7e => Opcode::Nor,
            0x7f => Opcode::Xor,
            0x80 => Opcode::Not,
            0x81 => Opcode::FindSetLeftBit,
            0x82 => Opcode::FindSetRightBit,
            0x83 => Opcode::DerefOf,
            0x84 => Opcode::ConcatRes,
            0x85 => Opcode::Mod,
            0x86 => Opcode::Notify,
            0x87 => Opcode::SizeOf,
            0x88 => Opcode::Index,
            0x89 => Opcode::Match,
            0x8a => Opcode::CreateDWordField,
            0x8b => Opcode::CreateWordField,
            0x8c => Opcode::CreateByteField,
            0x8d => Opcode::CreateBitField,
            0x8e => Opcode::ObjectType,
            0x8f => Opcode::CreateQWordField,
            0x90 => Opcode::LAnd,
            0x91 => Opcode::LOr,
            /*
             * `0x92` is a bit strange. It can be an opcode in its own right (`LNotOp`), but when
             * followed by `0x93..=0x95`, it instead serves as a negating prefix to encode
             * `LNotEqualOp`, `LLessEqualOp`, and `LGreaterEqualOp`.
             */
            0x92 => match self.peek() {
                Ok(0x93) => {
                    self.current_block.pc += 1;
                    Opcode::LNotEqual
                }
                Ok(0x94) => {
                    self.current_block.pc += 1;
                    Opcode::LLessEqual
                }
                Ok(0x95) => {
                    self.current_block.pc += 1;
                    Opcode::LGreaterEqual
                }
                _ => Opcode::LNot,
            },
            0x93 => Opcode::LEqual,
            0x94 => Opcode::LGreater,
            0x95 => Opcode::LLess,
            0x96 => Opcode::ToBuffer,
            0x97 => Opcode::ToDecimalString,
            0x98 => Opcode::ToHexString,
            0x99 => Opcode::ToInteger,
            0x9c => Opcode::ToString,
            0x9d => Opcode::CopyObject,
            0x9e => Opcode::Mid,
            0x9f => Opcode::Continue,
            0xa0 => Opcode::If,
            0xa1 => Opcode::Else,
            0xa2 => Opcode::While,
            0xa3 => Opcode::Noop,
            0xa4 => Opcode::Return,
            0xa5 => Opcode::Break,
            0xcc => Opcode::Breakpoint,
            0xff => Opcode::Ones,

            _ => Err(AmlError::IllegalOpcode(opcode))?,
        })
    }

    fn pkglength(&mut self) -> Result<usize, AmlError> {
        let lead_byte = self.next()?;
        let byte_count = lead_byte.get_bits(6..8);
        assert!(byte_count < 4);

        if byte_count == 0 {
            Ok(lead_byte.get_bits(0..6) as usize)
        } else {
            let mut length = lead_byte.get_bits(0..4) as usize;
            for i in 0..byte_count {
                length |= (self.next()? as usize) << (4 + i * 8);
            }
            Ok(length)
        }
    }

    fn namestring(&mut self) -> Result<AmlName, AmlError> {
        use namespace::{NameComponent, NameSeg};

        /*
         * The NameString grammar is actually a little finicky and annoying.
         *
         * NameString := <RootChar NamePath> | <PrefixPath NamePath>
         * PrefixPath := Nothing | <'^' PrefixPath>
         * NamePath := NameSeg | DualNamePath | MultiNamePath | NullName
         * DualNamePath := DualNamePrefix NameSeg NameSeg
         * MultiNamePath := MultiNamePrefix SegCount NameSeg(SegCount)
         */
        const NULL_NAME: u8 = 0x00;
        const DUAL_NAME_PREFIX: u8 = 0x2e;
        const MULTI_NAME_PREFIX: u8 = 0x2f;

        let mut components = vec![];

        match self.peek()? {
            b'\\' => {
                self.next()?;
                components.push(NameComponent::Root);
            }
            b'^' => {
                components.push(NameComponent::Prefix);
                self.next()?;
                while self.peek()? == b'^' {
                    self.next()?;
                    components.push(NameComponent::Prefix);
                }
            }
            _ => (),
        }

        let next = self.next()?;
        match next {
            NULL_NAME => {}
            DUAL_NAME_PREFIX => {
                for _ in 0..2 {
                    let name_seg = [self.next()?, self.next()?, self.next()?, self.next()?];
                    components.push(NameComponent::Segment(NameSeg::from_bytes(name_seg)?));
                }
            }
            MULTI_NAME_PREFIX => {
                let count = self.next()?;
                for _ in 0..count {
                    let name_seg = [self.next()?, self.next()?, self.next()?, self.next()?];
                    components.push(NameComponent::Segment(NameSeg::from_bytes(name_seg)?));
                }
            }
            first_char => {
                if !namespace::is_lead_name_char(first_char) {
                    self.current_block.pc -= 1;
                }
                let name_seg = [first_char, self.next()?, self.next()?, self.next()?];
                components.push(namespace::NameComponent::Segment(namespace::NameSeg::from_bytes(name_seg)?));
            }
        }

        Ok(AmlName::from_components(components))
    }

    fn next(&mut self) -> Result<u8, AmlError> {
        if self.current_block.pc >= self.current_block.stream.len() {
            return Err(AmlError::RunOutOfStream);
        }

        let byte = self.current_block.stream()[self.current_block.pc];
        self.current_block.pc += 1;

        Ok(byte)
    }

    fn next_u16(&mut self) -> Result<u16, AmlError> {
        Ok(u16::from_le_bytes([self.next()?, self.next()?]))
    }

    fn next_u32(&mut self) -> Result<u32, AmlError> {
        Ok(u32::from_le_bytes([self.next()?, self.next()?, self.next()?, self.next()?]))
    }

    fn next_u64(&mut self) -> Result<u64, AmlError> {
        Ok(u64::from_le_bytes([
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
            self.next()?,
        ]))
    }

    fn peek(&self) -> Result<u8, AmlError> {
        if self.current_block.pc >= self.current_block.stream.len() {
            return Err(AmlError::RunOutOfStream);
        }

        Ok(self.current_block.stream()[self.current_block.pc])
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Opcode {
    Zero,
    One,
    Alias,
    Name,
    BytePrefix,
    WordPrefix,
    DWordPrefix,
    StringPrefix,
    QWordPrefix,
    Scope,
    Buffer,
    Package,
    VarPackage,
    Method,
    External,
    DualNamePrefix,
    MultiNamePrefix,
    Digit(u8),
    NameChar(u8),
    Mutex,
    Event,
    CondRefOf,
    CreateField,
    LoadTable,
    Load,
    Stall,
    Sleep,
    Acquire,
    Signal,
    Wait,
    Reset,
    Release,
    FromBCD,
    ToBCD,
    Revision,
    Debug,
    Fatal,
    Timer,
    OpRegion,
    Field,
    Device,
    Processor,
    PowerRes,
    ThermalZone,
    IndexField,
    BankField,
    DataRegion,
    RootChar,
    ParentPrefixChar,
    Local(u8),
    Arg(u8),
    Store,
    RefOf,
    Add,
    Concat,
    Subtract,
    Increment,
    Decrement,
    Multiply,
    Divide,
    ShiftLeft,
    ShiftRight,
    And,
    Nand,
    Or,
    Nor,
    Xor,
    Not,
    FindSetLeftBit,
    FindSetRightBit,
    DerefOf,
    ConcatRes,
    Mod,
    Notify,
    SizeOf,
    Index,
    Match,
    CreateDWordField,
    CreateWordField,
    CreateByteField,
    CreateBitField,
    ObjectType,
    CreateQWordField,
    LAnd,
    LOr,
    LNot,
    LNotEqual,
    LLessEqual,
    LGreaterEqual,
    LEqual,
    LGreater,
    LLess,
    ToBuffer,
    ToDecimalString,
    ToHexString,
    ToInteger,
    ToString,
    CopyObject,
    Mid,
    Continue,
    If,
    Else,
    While,
    Noop,
    Return,
    Break,
    Breakpoint,
    Ones,

    /*
     * Internal opcodes are not produced from the bytecode, but are used to track special in-flight
     * ops etc.
     */
    InternalMethodCall,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Operation {
    Mid,
    SizeOf,
    Acquire,
    Release,
    ConvertToBuffer,

    ReadBufferField,
    WriteBufferField,
    LogicalOp,
    DecodePrt,
    ParseResource,
}

/*
 * TODO: not sure if we should use a better error reporting system or just keep a giant enum?
 */
#[derive(Clone, PartialEq, Debug)]
pub enum AmlError {
    RunOutOfStream,
    IllegalOpcode(u16),
    InvalidFieldFlags,

    InvalidName(Option<AmlName>),

    InvalidNameSeg([u8; 4]),
    InvalidNormalizedName(AmlName),
    RootHasNoParent,
    EmptyNamesAreInvalid,
    LevelDoesNotExist(AmlName),
    NameCollision(AmlName),
    ObjectDoesNotExist(AmlName),

    NoCurrentOp,
    ElseFoundWithoutCorrespondingIf,
    ContinueOutsideOfWhile,
    BreakOutsideOfWhile,

    MethodArgCountIncorrect,

    InvalidOperationOnObject { op: Operation, typ: ObjectType },
    IndexOutOfBounds,
    ObjectNotOfExpectedType { expected: ObjectType, got: ObjectType },

    InvalidResourceDescriptor,
    UnexpectedResourceType,

    PrtInvalidAddress,
    PrtInvalidPin,
    PrtInvalidGsi,
    PrtInvalidSource,
    PrtNoEntry,
}

/// This trait represents the interface from the `Interpreter` to the hosting kernel, and allows
/// AML to interact with the underlying hardware.
///
/// ### Implementation notes
/// Reads and writes to PCI devices must succeed for devices that are not detected during
/// enumeration of the PCI bus / do not exist.
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

    fn read_pci_u8(&self, address: PciAddress, offset: u16) -> u8;
    fn read_pci_u16(&self, address: PciAddress, offset: u16) -> u16;
    fn read_pci_u32(&self, address: PciAddress, offset: u16) -> u32;

    fn write_pci_u8(&self, address: PciAddress, offset: u16, value: u8);
    fn write_pci_u16(&self, address: PciAddress, offset: u16, value: u16);
    fn write_pci_u32(&self, address: PciAddress, offset: u16, value: u32);

    /// Returns a monotonically-increasing value of nanoseconds.
    fn nanos_since_boot(&self) -> u64;

    /// Stall for at least the given number of **microseconds**. An implementation should not relinquish control of
    /// the processor during the stall, and for this reason, firmwares should not stall for periods of more than
    /// 100 microseconds.
    fn stall(&self, microseconds: u64);

    /// Sleep for at least the given number of **milliseconds**. An implementation may round to the closest sleep
    /// time supported, and should relinquish the processor.
    fn sleep(&self, milliseconds: u64);

    fn breakpoint(&self) {}

    fn handle_debug(&self, _object: &Object) {}

    fn handle_fatal_error(&self, fatal_type: u8, fatal_code: u32, fatal_arg: u64) {
        panic!(
            "Fatal error while executing AML (encountered DefFatalOp). fatal_type = {}, fatal_code = {}, fatal_arg = {}",
            fatal_type, fatal_code, fatal_arg
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::str::FromStr;

    struct TestHandler;
    #[rustfmt::skip]
    impl Handler for TestHandler {
        fn read_u8(&self, _address: usize) -> u8 {0}
        fn read_u16(&self, _address: usize) -> u16 {0}
        fn read_u32(&self, _address: usize) -> u32 {0}
        fn read_u64(&self, _address: usize) -> u64 {0}
        fn write_u8(&mut self, _address: usize, _value: u8) {}
        fn write_u16(&mut self, _address: usize, _value: u16) {}
        fn write_u32(&mut self, _address: usize, _value: u32) {}
        fn write_u64(&mut self, _address: usize, _value: u64) {}
        fn read_io_u8(&self, _port: u16) -> u8 {0}
        fn read_io_u16(&self, _port: u16) -> u16 {0}
        fn read_io_u32(&self, _port: u16) -> u32 {0}
        fn write_io_u8(&self, _port: u16, _value: u8) {}
        fn write_io_u16(&self, _port: u16, _value: u16) {}
        fn write_io_u32(&self, _port: u16, _value: u32) {}
        fn read_pci_u8(&self, _address: PciAddress, _offset: u16) -> u8 {0}
        fn read_pci_u16(&self, _address: PciAddress, _offset: u16) -> u16 {0}
        fn read_pci_u32(&self, _address: PciAddress, _offset: u16) -> u32 {0}
        fn write_pci_u8(&self, _address: PciAddress, _offset: u16, _value: u8) {}
        fn write_pci_u16(&self, _address: PciAddress, _offset: u16, _value: u16) {}
        fn write_pci_u32(&self, _address: PciAddress, _offset: u16, _value: u32) {}
        fn nanos_since_boot(&self) -> u64 {0}
        fn stall(&self, _microseconds: u64) {}
        fn sleep(&self, _milliseconds: u64) {}
    }

    #[test]
    fn add_op() {
        let interpreter = Interpreter::new(TestHandler, 2);
        // AddOp 0x0e 0x06 => Local2
        interpreter.load_table(&[0x72, 0x0b, 0x0e, 0x00, 0x0a, 0x06, 0x62]).unwrap();
        // AddOp 0x0e (AddOp 0x01 0x03 => Local1) => Local1
        interpreter.load_table(&[0x72, 0x0a, 0x0e, 0x72, 0x0a, 0x01, 0x0a, 0x03, 0x61, 0x61]).unwrap();
    }

    #[test]
    fn names() {
        assert_eq!(
            unsafe { MethodContext::new_from_table(b"\\\x2eABC_DEF_\0") }.namestring(),
            Ok(AmlName::from_str("\\ABC.DEF").unwrap())
        );
    }
}
