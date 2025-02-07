#![no_std]
#![feature(let_chains, vec_pop_if)]

#[cfg(test)]
extern crate std;

extern crate alloc;

pub mod namespace;
pub mod object;

use alloc::{boxed::Box, sync::Arc, vec, vec::Vec};
use bit_field::BitField;
use core::{mem, str};
use namespace::{AmlName, Namespace, NamespaceLevelKind};
use object::Object;
use spinning_top::Spinlock;

pub struct Interpreter {
    handler: Box<dyn Handler>,
    pub namespace: Spinlock<Namespace>,
}

impl Interpreter {
    pub fn new<H>(handler: H) -> Interpreter
    where
        H: Handler + 'static,
    {
        Interpreter { namespace: Spinlock::new(Namespace::new()), handler: Box::new(handler) }
    }

    pub fn load_table(&self, stream: &[u8]) -> Result<(), AmlError> {
        // TODO: probs needs to do more stuff
        self.execute_method(stream)
    }

    pub fn execute_method(&self, stream: &[u8]) -> Result<(), AmlError> {
        let mut context = MethodContext::new(stream);

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
                        let [left, right, target] = &op.arguments[..] else { panic!() };
                        if let Argument::Object(left) = left
                            && let Argument::Object(right) = right
                        {
                            let Object::Integer(left) = **left else { panic!() };
                            let Object::Integer(right) = **right else { panic!() };

                            if let Argument::Object(target) = target {
                                *target.gain_mut() = Object::Integer(left + right);
                            } else {
                                panic!("Unexpected target type in AddOp");
                            }
                            // TODO: this is probs a slightly scuffed way of working out if the
                            // prev op wants our result
                            if let Some(prev_op) = context.in_flight.last_mut() {
                                if prev_op.arguments.len() < prev_op.expected_arguments {
                                    prev_op
                                        .arguments
                                        .push(Argument::Object(Arc::new(Object::Integer(left + right))));
                                }
                            }
                            // println!("Result: {}", left + right);
                        } else {
                            panic!("Bad operands");
                        }
                    }
                    Opcode::Name => {
                        let [name, object] = &op.arguments[..] else { panic!() };
                        let Argument::Namestring(name) = name else { panic!() };
                        let Argument::Object(object) = object else { panic!() };
                        // println!("Making Name with name {} and object: {:?}", name, object);

                        let name = name.resolve(&context.current_scope)?;
                        self.namespace.lock().insert(name, object.clone())?;
                    }
                    Opcode::Package => {
                        // Nothing to do here. The package is created by the block ending.
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
                        BlockKind::Normal => {
                            break Ok(());
                        }
                        BlockKind::Scope { old_scope } => {
                            assert!(context.block_stack.len() > 0);
                            context.current_block = context.block_stack.pop().unwrap();
                            context.current_scope = old_scope;
                            // Go round the loop again to get the next opcode for the new block
                            continue;
                        }
                        BlockKind::Package => {
                            assert!(context.block_stack.len() > 0);

                            // Pop the in-flight operation off and make sure it's the package
                            let package_op = context.in_flight.pop().unwrap();
                            assert_eq!(package_op.op, Opcode::Package);

                            let mut elements = Vec::with_capacity(package_op.expected_arguments);
                            for arg in &package_op.arguments {
                                let Argument::Object(object) = arg else { panic!() };
                                elements.push(object.clone());
                            }
                            for _ in package_op.arguments.len()..package_op.expected_arguments {
                                // Each uninitialized element must be a distinct object
                                elements.push(Arc::new(Object::Uninitialized));
                            }

                            // Add the created package to the last in-flight op's arguments
                            if let Some(prev_op) = context.in_flight.last_mut() {
                                if prev_op.arguments.len() < prev_op.expected_arguments {
                                    prev_op.arguments.push(Argument::Object(Arc::new(Object::Package(elements))));
                                } else {
                                    panic!("Random package floating around?");
                                }
                            }

                            // End the block, go round the loop again
                            context.current_block = context.block_stack.pop().unwrap();
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
                    let str =
                        str::from_utf8(&context.current_block.stream[str_start..(context.current_block.pc - 1)])
                            .unwrap();
                    context
                        .last_op()?
                        .arguments
                        .push(Argument::Object(Arc::new(Object::String(String::from(str)))));
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

                    // println!("Scope. length = {}({}), name = {:?}", pkg_length, remaining_length, new_scope);
                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::Buffer => todo!(),
                Opcode::Package => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let num_elements = context.next()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);
                    println!("Package. num elements = {}", num_elements);

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
                Opcode::Method => todo!(),
                Opcode::External => todo!(),
                Opcode::DualNamePrefix => todo!(),
                Opcode::MultiNamePrefix => todo!(),
                Opcode::Digit(_) => todo!(),
                Opcode::NameChar(_) => todo!(),
                Opcode::Mutex => todo!(),
                Opcode::Event => todo!(),
                Opcode::CondRefOf => todo!(),
                Opcode::CreateField => todo!(),
                Opcode::LoadTable => todo!(),
                Opcode::Load => todo!(),
                Opcode::Stall => todo!(),
                Opcode::Sleep => todo!(),
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
                Opcode::OpRegion => todo!(),
                Opcode::Field => todo!(),
                Opcode::Device => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let name = context.namestring()?;

                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    let new_scope = name.resolve(&context.current_scope)?;
                    self.namespace.lock().add_level(new_scope.clone(), NamespaceLevelKind::Device)?;

                    // println!("Device. length = {}({}), name = {:?}", pkg_length, remaining_length, new_scope);
                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::PowerRes => todo!(),
                Opcode::ThermalZone => todo!(),
                Opcode::IndexField => todo!(),
                Opcode::BankField => todo!(),
                Opcode::DataRegion => todo!(),
                Opcode::RootChar => todo!(),
                Opcode::ParentPrefixChar => todo!(),
                Opcode::Local(local) => {
                    let local = context.locals[local as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Reference(local))));
                }
                Opcode::Arg(arg) => todo!(),
                Opcode::Store => todo!(),
                Opcode::RefOf => todo!(),
                Opcode::Add => {
                    context.start_in_flight_op(OpInFlight::new(Opcode::Add, 3));
                }
                Opcode::Concat => todo!(),
                Opcode::Subtract => todo!(),
                Opcode::Increment => todo!(),
                Opcode::Decrement => todo!(),
                Opcode::Multiply => todo!(),
                Opcode::Divide => todo!(),
                Opcode::ShiftLeft => todo!(),
                Opcode::ShiftRight => todo!(),
                Opcode::And => todo!(),
                Opcode::Nand => todo!(),
                Opcode::Or => todo!(),
                Opcode::Nor => todo!(),
                Opcode::Xor => todo!(),
                Opcode::Not => todo!(),
                Opcode::FindSetLeftBit => todo!(),
                Opcode::FindSetRightBit => todo!(),
                Opcode::DerefOf => todo!(),
                Opcode::ConcatRes => todo!(),
                Opcode::Mod => todo!(),
                Opcode::Notify => todo!(),
                Opcode::SizeOf => todo!(),
                Opcode::Index => todo!(),
                Opcode::Match => todo!(),
                Opcode::CreateDWordField => todo!(),
                Opcode::CreateWordField => todo!(),
                Opcode::CreateByteField => todo!(),
                Opcode::CreateBitField => todo!(),
                Opcode::ObjectType => todo!(),
                Opcode::CreateQWordField => todo!(),
                Opcode::LAnd => todo!(),
                Opcode::LOr => todo!(),
                Opcode::LNot => todo!(),
                Opcode::LNotEqual => todo!(),
                Opcode::LLessEqual => todo!(),
                Opcode::LGreaterEqual => todo!(),
                Opcode::LEqual => todo!(),
                Opcode::LGreater => todo!(),
                Opcode::LLess => todo!(),
                Opcode::ToBuffer => todo!(),
                Opcode::ToDecimalString => todo!(),
                Opcode::ToHexString => todo!(),
                Opcode::ToInteger => todo!(),
                Opcode::ToString => todo!(),
                Opcode::CopyObject => todo!(),
                Opcode::Mid => todo!(),
                Opcode::Continue => todo!(),
                Opcode::If => todo!(),
                Opcode::Else => todo!(),
                Opcode::While => todo!(),
                Opcode::Noop => {}
                Opcode::Return => todo!(),
                Opcode::Break => todo!(),
                Opcode::Breakpoint => todo!(),
                Opcode::Ones => {
                    context.last_op()?.arguments.push(Argument::Object(Arc::new(Object::Integer(u64::MAX))));
                }
            }
        }
    }
}

pub struct MethodContext<'a> {
    current_block: Block<'a>,
    block_stack: Vec<Block<'a>>,
    in_flight: Vec<OpInFlight>,
    locals: [Arc<Object>; 8],
    current_scope: AmlName,
}

#[derive(Debug)]
pub struct OpInFlight {
    op: Opcode,
    expected_arguments: usize,
    arguments: Vec<Argument>,
}

#[derive(Debug)]
pub enum Argument {
    Object(Arc<Object>),
    Namestring(AmlName),
}

pub struct Block<'a> {
    stream: &'a [u8],
    pc: usize,
    kind: BlockKind,
}

#[derive(Debug)]
pub enum BlockKind {
    Normal,
    Scope { old_scope: AmlName },
    Package,
}

impl OpInFlight {
    pub fn new(op: Opcode, expected_arguments: usize) -> OpInFlight {
        OpInFlight { op, expected_arguments, arguments: Vec::new() }
    }

    pub fn new_with(op: Opcode, arguments: Vec<Argument>, more: usize) -> OpInFlight {
        OpInFlight { op, expected_arguments: arguments.len() + more, arguments }
    }
}

impl<'a> MethodContext<'a> {
    pub fn new(stream: &'a [u8]) -> MethodContext<'a> {
        let block = Block { stream, pc: 0, kind: BlockKind::Normal };
        MethodContext {
            current_block: block,
            block_stack: Vec::new(),
            in_flight: Vec::new(),
            locals: core::array::from_fn(|_| Arc::new(Object::Uninitialized)),
            current_scope: AmlName::root(),
        }
    }

    pub fn last_op(&mut self) -> Result<&mut OpInFlight, AmlError> {
        match self.in_flight.last_mut() {
            Some(op) => Ok(op),
            None => Err(AmlError::NoCurrentOp),
        }
    }

    pub fn start_in_flight_op(&mut self, op: OpInFlight) {
        // println!("Starting in-flight op of type: {:?}", op);
        self.in_flight.push(op);
    }

    pub fn start_new_block(&mut self, kind: BlockKind, length: usize) {
        // println!("Starting new block at pc={}, length={}, kind={:?}", self.current_block.pc, length, kind);
        let block = Block {
            stream: &self.current_block.stream[..(self.current_block.pc + length)],
            pc: self.current_block.pc,
            kind,
        };
        self.current_block.pc += length;
        self.block_stack.push(mem::replace(&mut self.current_block, block));
    }

    pub fn opcode(&mut self) -> Result<Opcode, AmlError> {
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

            _ => Err(AmlError::IllegalOpcode)?,
        })
    }

    pub fn pkglength(&mut self) -> Result<usize, AmlError> {
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

    pub fn namestring(&mut self) -> Result<AmlName, AmlError> {
        use namespace::{NameComponent, NameSeg};

        const NULL_NAME: u8 = 0x00;
        const DUAL_NAME_PREFIX: u8 = 0x2e;
        const MULTI_NAME_PREFIX: u8 = 0x2f;

        let mut components = vec![];
        loop {
            let next = match self.next() {
                Ok(next) => next,
                Err(AmlError::RunOutOfStream) => break,
                Err(other) => Err(other)?,
            };
            match next {
                b'\\' => {
                    if !components.is_empty() {
                        return Err(AmlError::InvalidName(None));
                    }
                    components.push(NameComponent::Root);
                }
                b'^' => components.push(NameComponent::Prefix),
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
                        break;
                    }
                    let name_seg = [first_char, self.next()?, self.next()?, self.next()?];
                    components.push(namespace::NameComponent::Segment(namespace::NameSeg::from_bytes(name_seg)?));
                }
            }
        }

        Ok(AmlName::from_components(components))
    }

    pub fn next(&mut self) -> Result<u8, AmlError> {
        if self.current_block.pc >= self.current_block.stream.len() {
            return Err(AmlError::RunOutOfStream);
        }

        let byte = self.current_block.stream[self.current_block.pc];
        self.current_block.pc += 1;

        Ok(byte)
    }

    pub fn next_u16(&mut self) -> Result<u16, AmlError> {
        Ok(u16::from_le_bytes([self.next()?, self.next()?]))
    }

    pub fn next_u32(&mut self) -> Result<u32, AmlError> {
        Ok(u32::from_le_bytes([self.next()?, self.next()?, self.next()?, self.next()?]))
    }

    pub fn next_u64(&mut self) -> Result<u64, AmlError> {
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

    pub fn peek(&self) -> Result<u8, AmlError> {
        if self.current_block.pc >= self.current_block.stream.len() {
            return Err(AmlError::RunOutOfStream);
        }

        Ok(self.current_block.stream[self.current_block.pc])
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Opcode {
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
}

/*
 * TODO: not sure if we should use a better error reporting system or just keep a giant enum?
 */
#[derive(Clone, PartialEq, Debug)]
pub enum AmlError {
    RunOutOfStream,
    IllegalOpcode,

    InvalidName(Option<AmlName>),

    InvalidNameSeg,
    InvalidNormalizedName(AmlName),
    RootHasNoParent,
    EmptyNamesAreInvalid,
    LevelDoesNotExist(AmlName),
    NameCollision(AmlName),
    ObjectDoesNotExist(AmlName),

    NoCurrentOp,
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
            MethodContext::new(b"\\\x2eABC_DEF_").namestring(),
            Ok(AmlName::from_str("\\ABC.DEF").unwrap())
        );
        assert_eq!(
            MethodContext::new(b"\x2eABC_DEF_^_GHI").namestring(),
            Ok(AmlName::from_str("ABC.DEF.^_GHI").unwrap())
        );
    }
}
