#![no_std]
#![feature(let_chains, vec_pop_if)]

#[cfg(test)]
extern crate std;

extern crate alloc;

pub mod namespace;
pub mod object;
pub mod op_region;

use alloc::{boxed::Box, sync::Arc, vec, vec::Vec};
use bit_field::BitField;
use core::{mem, str};
use namespace::{AmlName, Namespace, NamespaceLevelKind};
use object::{MethodFlags, Object, ObjectType};
use op_region::{OpRegion, RegionSpace};
use spinning_top::Spinlock;

pub struct Interpreter {
    handler: Box<dyn Handler>,
    pub namespace: Spinlock<Namespace>,
    context_stack: Spinlock<Vec<MethodContext>>,
}

impl Interpreter {
    pub fn new<H>(handler: H) -> Interpreter
    where
        H: Handler + 'static,
    {
        Interpreter {
            namespace: Spinlock::new(Namespace::new()),
            handler: Box::new(handler),
            context_stack: Spinlock::new(Vec::new()),
        }
    }

    pub fn load_table(&self, stream: &[u8]) -> Result<(), AmlError> {
        // TODO: probs needs to do more stuff
        self.execute_method(stream)
    }

    pub fn execute_method(&self, stream: &[u8]) -> Result<(), AmlError> {
        let mut context = unsafe { MethodContext::new_from_table(stream) };

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
                    Opcode::Add => {
                        let [Argument::Object(left), Argument::Object(right), Argument::Object(target)] =
                            &op.arguments[..]
                        else {
                            panic!()
                        };
                        let Object::Integer(left) = **left else { panic!() };
                        let Object::Integer(right) = **right else { panic!() };

                        *target.gain_mut() = Object::Integer(left + right);

                        // TODO: this is probs a slightly scuffed way of working out if the
                        // prev op wants our result
                        if let Some(prev_op) = context.in_flight.last_mut() {
                            if prev_op.arguments.len() < prev_op.expected_arguments {
                                prev_op.arguments.push(Argument::Object(Arc::new(Object::Integer(left + right))));
                            }
                        }
                    }
                    Opcode::Increment | Opcode::Decrement => {
                        let [Argument::Object(operand)] = &op.arguments[..] else { panic!() };
                        let Object::Integer(operand) = operand.gain_mut() else { panic!() };

                        let new_value = match op.op {
                            Opcode::Increment => operand.wrapping_add(1),
                            Opcode::Decrement => operand.wrapping_sub(1),
                            _ => unreachable!(),
                        };

                        *operand = new_value;
                    }
                    Opcode::Name => {
                        let [Argument::Namestring(name), Argument::Object(object)] = &op.arguments[..] else {
                            panic!()
                        };

                        let name = name.resolve(&context.current_scope)?;
                        self.namespace.lock().insert(name, object.clone())?;
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
                        let Object::Integer(region_offset) = **region_offset else { panic!() };
                        let Object::Integer(region_length) = **region_length else { panic!() };

                        let region_space = RegionSpace::from(*region_space);

                        let region = Object::OpRegion(OpRegion {
                            space: region_space,
                            base: region_offset,
                            length: region_length,
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
                        let Object::Integer(buffer_size) = **buffer_size else { panic!() };

                        let buffer_len = pkg_length - (context.current_block.pc - start_pc);
                        let mut buffer = vec![0; buffer_size as usize];
                        buffer[0..buffer_len].copy_from_slice(
                            &context.current_block.stream()
                                [context.current_block.pc..(context.current_block.pc + buffer_len)],
                        );
                        context.current_block.pc += buffer_len;

                        if let Some(prev_op) = context.in_flight.last_mut() {
                            if prev_op.arguments.len() < prev_op.expected_arguments {
                                prev_op.arguments.push(Argument::Object(Arc::new(Object::Buffer(buffer))));
                            }
                        }
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

                        if let Some(prev_op) = context.in_flight.last_mut() {
                            if prev_op.arguments.len() < prev_op.expected_arguments {
                                prev_op.arguments.push(Argument::Object(Arc::new(Object::Package(elements))));
                            } else {
                                panic!("Random package floating around?");
                            }
                        }
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

                        let Object::Integer(predicate) = **predicate else { panic!() };
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
                        let Object::Integer(index) = **index else { panic!() };
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
                        let Object::Integer(bit_index) = **bit_index else { panic!() };
                        let Object::Integer(num_bits) = **num_bits else { panic!() };
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
                    Opcode::Sleep => {
                        let [Argument::Object(msec)] = &op.arguments[..] else { panic!() };
                        let Object::Integer(msec) = **msec else { panic!() };
                        self.handler.sleep(msec);
                    }
                    Opcode::Stall => {
                        let [Argument::Object(usec)] = &op.arguments[..] else { panic!() };
                        let Object::Integer(usec) = **usec else { panic!() };
                        self.handler.stall(usec);
                    }
                    Opcode::InternalMethodCall => {
                        let Argument::Object(method) = &op.arguments[0] else { panic!() };

                        let args = op.arguments[1..]
                            .iter()
                            .map(|arg| {
                                if let Argument::Object(arg) = arg {
                                    arg.clone()
                                } else {
                                    panic!();
                                }
                            })
                            .collect();

                        let new_context = MethodContext::new_from_method(method.clone(), args)?;
                        let old_context = mem::replace(&mut context, new_context);
                        self.context_stack.lock().push(old_context);
                    }
                    Opcode::Return => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        context = self.context_stack.lock().pop().unwrap();

                        if let Some(prev_op) = context.in_flight.last_mut() {
                            if prev_op.arguments.len() < prev_op.expected_arguments {
                                prev_op.arguments.push(Argument::Object(object.clone()));
                            }
                        }
                    }
                    Opcode::ObjectType => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        // TODO: this should technically support scopes as well - this is less easy
                        // (they should return `0`)
                        // TODO: calling this on the debug object should should return `16`
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
                            ObjectType::Reference => panic!(),
                            ObjectType::RawDataBuffer => todo!(),
                        };

                        if let Some(prev_op) = context.in_flight.last_mut() {
                            if prev_op.arguments.len() < prev_op.expected_arguments {
                                prev_op.arguments.push(Argument::Object(Arc::new(Object::Integer(typ))));
                            }
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
                            break Ok(());
                        }
                        BlockKind::Method => {
                            // TODO: not sure how to handle no explicit return. Result is undefined
                            // but we might still need to handle sticking it in an in-flight op?
                            context = self.context_stack.lock().pop().unwrap();
                            continue;
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
                Opcode::Alias => todo!(),
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
                Opcode::Mutex => todo!(),
                Opcode::Event => todo!(),
                Opcode::CondRefOf => todo!(),
                Opcode::LoadTable => todo!(),
                Opcode::Load => todo!(),
                Opcode::Stall => context.start_in_flight_op(OpInFlight::new(Opcode::Stall, 1)),
                Opcode::Sleep => context.start_in_flight_op(OpInFlight::new(Opcode::Sleep, 1)),
                Opcode::Acquire => todo!(),
                Opcode::Signal => todo!(),
                Opcode::Wait => todo!(),
                Opcode::Reset => todo!(),
                Opcode::Release => todo!(),
                Opcode::FromBCD => todo!(),
                Opcode::ToBCD => todo!(),
                Opcode::Revision => todo!(),
                Opcode::Debug => todo!(),
                Opcode::Fatal => todo!(),
                Opcode::Timer => todo!(),
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
                    let flags = context.next()?;

                    let region = self.namespace.lock().get(region_name.resolve(&context.current_scope)?)?.clone();

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
                                // TODO: do something with the flags, and also detect when this
                                // should be a bank or index field
                                let field = Object::FieldUnit(object::FieldUnit::Normal {
                                    region: region.clone(),
                                    bit_index: field_offset,
                                    bit_length: field_length,
                                });
                                self.namespace
                                    .lock()
                                    .insert(field_name.resolve(&context.current_scope)?, Arc::new(field))?;
                            }
                        }
                    }
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
                Opcode::IndexField => todo!(),
                Opcode::BankField => todo!(),
                Opcode::DataRegion => todo!(),
                Opcode::Local(local) => {
                    let local = context.locals[local as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Reference(local))));
                }
                Opcode::Arg(arg) => {
                    let arg = context.args[arg as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Reference(arg))));
                }
                Opcode::Store => context.start_in_flight_op(OpInFlight::new(Opcode::Store, 2)),
                Opcode::RefOf => todo!(),

                Opcode::DualNamePrefix
                | Opcode::MultiNamePrefix
                | Opcode::Digit(_)
                | Opcode::NameChar(_)
                | Opcode::RootChar
                | Opcode::ParentPrefixChar => {
                    context.current_block.pc -= 1;
                    let name = context.namestring()?;

                    let (_, object) = self.namespace.lock().search(&name, &context.current_scope)?;
                    if let Object::Method { flags, .. } = *object {
                        context.start_in_flight_op(OpInFlight::new_with(
                            Opcode::InternalMethodCall,
                            vec![Argument::Object(object)],
                            flags.arg_count(),
                        ))
                    } else {
                        context.last_op()?.arguments.push(Argument::Object(object));
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
                Opcode::ConcatRes => todo!(),
                Opcode::Notify => todo!(),
                Opcode::SizeOf => todo!(),
                Opcode::Index => todo!(),
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
                Opcode::Continue => todo!(),
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
                Opcode::While => todo!(),
                Opcode::Noop => {}
                Opcode::Return => context.start_in_flight_op(OpInFlight::new(Opcode::Return, 1)),
                Opcode::Break => todo!(),
                Opcode::Breakpoint => todo!(),
                Opcode::Ones => {
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(u64::MAX))));
                }

                Opcode::InternalMethodCall => panic!(),
            }
        }
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
                    _ => panic!(),
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
                _ => panic!("Stores to objects like {:?} are not yet supported", target),
            },
            Argument::Namestring(_) => {}
            Argument::UnresolvedObjectPath(_) => {
                // TODO: do we need to attempt to allow this somehow??
                todo!("Is this allowed here?");
            }

            Argument::ByteData(_) | Argument::TrackedPc(_) | Argument::PkgLength(_) => panic!(),
        }
        Ok(())
    }
}

/// A `MethodContext` represents a piece of running AML code - either a real method, or the
/// top-level of an AML table.
///
/// ### Safety
/// `MethodContext` does not keep the lifetime of the underlying AML stream, which for tables is
/// borrowed from the underlying physical mapping. This is because the interpreter needs to
/// pre-empt method contexts that execute other methods, storing pre-empted contexts.
///
/// This is made safe in the case of methods by the context holding a reference to the method
/// object, but must be handled manually for AML tables.
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
    TrackedPc(usize),
    PkgLength(usize),
}

struct Block {
    stream: *const [u8],
    pc: usize,
    kind: BlockKind,
}

// TODO: we might need to impl Send + Sync for Block?

impl Block {
    fn stream(&self) -> &[u8] {
        unsafe { &*self.stream }
    }
}

#[derive(PartialEq, Debug)]
pub enum BlockKind {
    Table,
    Method,
    Scope {
        old_scope: AmlName,
    },
    Package,
    /// Used for executing the then-branch of an `DefIfElse`. After finishing, it will check for
    /// and skip over an else-branch, if present.
    IfThenBranch,
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

    fn new_from_method(method: Arc<Object>, args: Vec<Arc<Object>>) -> Result<MethodContext, AmlError> {
        if let Object::Method { code, flags } = &*method {
            if args.len() != flags.arg_count() {
                return Err(AmlError::MethodArgCountIncorrect);
            }
            let block = Block { stream: code as &[u8] as *const [u8], pc: 0, kind: BlockKind::Method };
            let args = core::array::from_fn(|i| {
                if let Some(arg) = args.get(i) { arg.clone() } else { Arc::new(Object::Uninitialized) }
            });
            let context = MethodContext {
                current_block: block,
                block_stack: Vec::new(),
                in_flight: Vec::new(),
                args,
                locals: core::array::from_fn(|_| Arc::new(Object::Uninitialized)),
                current_scope: AmlName::root(),
                _method: Some(method.clone()),
            };
            Ok(context)
        } else {
            panic!()
        }
    }

    fn last_op(&mut self) -> Result<&mut OpInFlight, AmlError> {
        match self.in_flight.last_mut() {
            Some(op) => Ok(op),
            None => Err(AmlError::NoCurrentOp),
        }
    }

    fn start_in_flight_op(&mut self, op: OpInFlight) {
        println!("Starting in-flight op of type: {:?}", op);
        self.in_flight.push(op);
    }

    fn start_new_block(&mut self, kind: BlockKind, length: usize) {
        println!("Starting new block at pc={}, length={}, kind={:?}", self.current_block.pc, length, kind);
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

/*
 * TODO: not sure if we should use a better error reporting system or just keep a giant enum?
 */
#[derive(Clone, PartialEq, Debug)]
pub enum AmlError {
    RunOutOfStream,
    IllegalOpcode(u16),

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

    MethodArgCountIncorrect,

    InvalidOperationOnObject,
}

/// This trait represents the interface from the `Interpreter` to the hosting kernel, and allows
/// AML to interact with the underlying hardware.
// TODO: maybe use `pci_types::PciAddress` to simplify PCI address passing here
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

    /// Stall for at least the given number of **microseconds**. An implementation should not relinquish control of
    /// the processor during the stall, and for this reason, firmwares should not stall for periods of more than
    /// 100 microseconds.
    fn stall(&self, microseconds: u64);

    /// Sleep for at least the given number of **milliseconds**. An implementation may round to the closest sleep
    /// time supported, and should relinquish the processor.
    fn sleep(&self, milliseconds: u64);

    fn handle_fatal_error(&self, fatal_type: u8, fatal_code: u32, fatal_arg: u64) {
        panic!(
            "Fatal error while executing AML (encountered DefFatalOp). fatal_type = {:?}, fatal_code = {:?}, fatal_arg = {:?}",
            fatal_type, fatal_code, fatal_arg
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

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
        fn read_pci_u8(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16) -> u8 {0}
        fn read_pci_u16(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16) -> u16 {0}
        fn read_pci_u32(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16) -> u32 {0}
        fn write_pci_u8(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16, _value: u8) {}
        fn write_pci_u16(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16, _value: u16) {}
        fn write_pci_u32(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16, _value: u32) {}
        fn stall(&self, _microseconds: u64) {}
        fn sleep(&self, _milliseconds: u64) {}
    }

    #[test]
    fn add_op() {
        let interpreter = Interpreter::new(TestHandler);
        // AddOp 0x0e 0x06 => Local2
        interpreter.execute_method(&[0x72, 0x0b, 0x0e, 0x00, 0x0a, 0x06, 0x62]).unwrap();
        // AddOp 0x0e (AddOp 0x01 0x03 => Local1) => Local1
        interpreter.execute_method(&[0x72, 0x0a, 0x0e, 0x72, 0x0a, 0x01, 0x0a, 0x03, 0x61, 0x61]).unwrap();
    }

    #[test]
    fn names() {
        assert_eq!(
            unsafe { MethodContext::new_from_table(b"\\\x2eABC_DEF_\0") }.namestring(),
            Ok(AmlName::from_str("\\ABC.DEF").unwrap())
        );
        assert_eq!(
            unsafe { MethodContext::new_from_table(b"\x2eABC_DEF_^_GHI") }.namestring(),
            Ok(AmlName::from_str("ABC.DEF.^_GHI").unwrap())
        );
    }
}
