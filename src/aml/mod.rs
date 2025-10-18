/*
 * TODO:
 *  - Field reads supporting custom handlers
 *  - Locked fields
 *  - Bank and index fields
 *  - Run `_REG` on supported op region handlers
 *  - Count operations performed and time
 *  - Correct DefStore / DefCopyObject behaviour
 *  - Load and LoadTable
 *  - DefDataRegion
 *  - Notify
 *  - DefMatch
 *
 *  - Method recursion depth?
 *  - Loop timeouts
 *  - Fuzzing and guarantee panic-free interpretation
 */

pub mod namespace;
pub mod object;
pub mod op_region;
pub mod pci_routing;
pub mod resource;

use crate::{
    AcpiError,
    AmlTable,
    Handle,
    Handler,
    PhysicalMapping,
    platform::AcpiPlatform,
    registers::{FixedRegisters, Pm1ControlBit},
    sdt::{SdtHeader, facs::Facs, fadt::Fadt},
};
use alloc::{
    boxed::Box,
    collections::btree_map::BTreeMap,
    string::{String, ToString},
    sync::Arc,
    vec,
    vec::Vec,
};
use bit_field::BitField;
use core::{
    mem,
    slice,
    str::FromStr,
    sync::atomic::{AtomicU64, Ordering},
};
use log::{info, trace, warn};
use namespace::{AmlName, Namespace, NamespaceLevelKind};
use object::{
    DeviceStatus,
    FieldFlags,
    FieldUnit,
    FieldUnitKind,
    FieldUpdateRule,
    MethodFlags,
    Object,
    ObjectToken,
    ObjectType,
    ReferenceKind,
    WrappedObject,
};
use op_region::{OpRegion, RegionHandler, RegionSpace};
use pci_types::PciAddress;
use spinning_top::Spinlock;

/// `Interpreter` implements a virtual machine for the dynamic AML bytecode. It can be used by a
/// host operating system to load tables containing AML bytecode (generally the DSDT and SSDTs) and
/// will then manage the AML namespace and all objects created during the life of the system.
pub struct Interpreter<H>
where
    H: Handler,
{
    handler: H,
    pub namespace: Spinlock<Namespace>,
    pub object_token: Spinlock<ObjectToken>,
    context_stack: Spinlock<Vec<MethodContext>>,
    dsdt_revision: u8,
    region_handlers: Spinlock<BTreeMap<RegionSpace, Box<dyn RegionHandler>>>,

    global_lock_mutex: Handle,
    registers: Arc<FixedRegisters<H>>,
    facs: Option<PhysicalMapping<H, Facs>>,
}

unsafe impl<H> Send for Interpreter<H> where H: Handler + Send {}
unsafe impl<H> Sync for Interpreter<H> where H: Handler + Send {}

/// The value returned by the `Revision` opcode.
const INTERPRETER_REVISION: u64 = 1;

impl<H> Interpreter<H>
where
    H: Handler,
{
    /// Construct a new `Interpreter`. This does not load any tables - if you have an `AcpiTables`
    /// already, use [`Interpreter::new_from_tables`] instead.
    pub fn new(
        handler: H,
        dsdt_revision: u8,
        registers: Arc<FixedRegisters<H>>,
        facs: Option<PhysicalMapping<H, Facs>>,
    ) -> Interpreter<H> {
        info!("Initializing AML interpreter v{}", env!("CARGO_PKG_VERSION"));

        let global_lock_mutex = handler.create_mutex();

        Interpreter {
            handler,
            namespace: Spinlock::new(Namespace::new(global_lock_mutex)),
            object_token: Spinlock::new(unsafe { ObjectToken::create_interpreter_token() }),
            context_stack: Spinlock::new(Vec::new()),
            dsdt_revision,
            region_handlers: Spinlock::new(BTreeMap::new()),
            global_lock_mutex,
            registers,
            facs,
        }
    }

    /// Construct a new `Interpreter` with the given set of ACPI tables. This will automatically
    /// load the DSDT and any SSDTs in the supplied [`AcpiTables`].
    pub fn new_from_platform(platform: &AcpiPlatform<H>) -> Result<Interpreter<H>, AcpiError> {
        fn load_table(interpreter: &Interpreter<impl Handler>, table: AmlTable) -> Result<(), AcpiError> {
            let mapping = unsafe {
                interpreter.handler.map_physical_region::<SdtHeader>(table.phys_address, table.length as usize)
            };
            let stream = unsafe {
                slice::from_raw_parts(
                    mapping.virtual_start.as_ptr().byte_add(mem::size_of::<SdtHeader>()) as *const u8,
                    table.length as usize - mem::size_of::<SdtHeader>(),
                )
            };
            interpreter.load_table(stream).map_err(AcpiError::Aml)?;
            Ok(())
        }

        let registers = platform.registers.clone();
        let facs = {
            platform.tables.find_table::<Fadt>().and_then(|fadt| fadt.facs_address().ok()).map(
                |facs_address| unsafe {
                    platform.handler.map_physical_region(facs_address, mem::size_of::<Facs>())
                },
            )
        };

        let dsdt = platform.tables.dsdt()?;
        let interpreter = Interpreter::new(platform.handler.clone(), dsdt.revision, registers, facs);
        load_table(&interpreter, dsdt)?;

        for ssdt in platform.tables.ssdts() {
            load_table(&interpreter, ssdt)?;
        }

        Ok(interpreter)
    }

    /// Load the supplied byte stream as an AML table. This should be only the encoded AML stream -
    /// not the header at the start of a table. If you've used [`Interpreter::new_from_tables`],
    /// you'll likely not need to load any tables manually.
    pub fn load_table(&self, stream: &[u8]) -> Result<(), AmlError> {
        let context = unsafe { MethodContext::new_from_table(stream) };
        self.do_execute_method(context)?;
        Ok(())
    }

    /// Evaluate an object at the given path in the namespace. If the object is a method, this
    /// invokes the method with the given set of arguments.
    pub fn evaluate(&self, path: AmlName, args: Vec<WrappedObject>) -> Result<WrappedObject, AmlError> {
        trace!("Invoking AML method: {}", path);

        let object = self.namespace.lock().get(path.clone())?.clone();
        match &*object {
            Object::Method { .. } => {
                self.namespace.lock().add_level(path.clone(), NamespaceLevelKind::MethodLocals)?;
                let context = MethodContext::new_from_method(object, args, path)?;
                self.do_execute_method(context)
            }
            Object::NativeMethod { f, .. } => f(&args),
            _ => Ok(object),
        }
    }

    pub fn evaluate_if_present(
        &self,
        path: AmlName,
        args: Vec<WrappedObject>,
    ) -> Result<Option<WrappedObject>, AmlError> {
        match self.evaluate(path.clone(), args) {
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

    /// Initialize the namespace - this should be called after all tables have been loaded and
    /// operation region handlers registered. Specifically, it will call relevant `_STA`, `_INI`,
    /// and `_REG` methods.
    pub fn initialize_namespace(&self) {
        /*
         * This should match the initialization order of ACPICA and uACPI.
         */
        if let Err(err) = self.evaluate_if_present(AmlName::from_str("\\_INI").unwrap(), vec![]) {
            warn!("Invoking \\_INI failed: {:?}", err);
        }
        if let Err(err) = self.evaluate_if_present(AmlName::from_str("\\_SB._INI").unwrap(), vec![]) {
            warn!("Invoking \\_SB._INI failed: {:?}", err);
        }

        // TODO: run all _REGs for globally-installed handlers (this might need more bookkeeping)

        /*
         * We can now initialize each device in the namespace. For each device, we evaluate `_STA`,
         * which indicates if the device is present and functional. If this method does not exist,
         * we assume the device should be initialized.
         *
         * We then evaluate `_INI` for the device. This can dynamically populate objects such as
         * `_ADR`, `_CID`, `_HID`, `_SUN`, and `_UID`, and so is necessary before further
         * operation.
         */
        let mut num_devices_initialized = 0;
        /*
         * TODO
         * We clone a copy of the namespace here to traverse while executing all the `_STA` and
         * `_INI` objects. Avoiding this would be good, but is not easy, as we need
         * potentially-mutable access while executing all of the methods.
         */
        let mut namespace = self.namespace.lock().clone();
        let init_status = namespace.traverse(|path, level| {
            match level.kind {
                NamespaceLevelKind::Device
                | NamespaceLevelKind::Processor
                | NamespaceLevelKind::ThermalZone
                | NamespaceLevelKind::PowerResource => {
                    let should_initialize = match self
                        .evaluate_if_present(AmlName::from_str("_STA").unwrap().resolve(path)?, vec![])
                    {
                        Ok(Some(result)) => {
                            let Object::Integer(result) = *result else { panic!() };
                            let status = DeviceStatus(result);
                            status.present() && status.functioning()
                        }
                        Ok(None) => true,
                        Err(err) => {
                            warn!("Failed to evaluate _STA for device {}: {:?}", path, err);
                            false
                        }
                    };

                    if should_initialize {
                        num_devices_initialized += 1;
                        if let Err(err) =
                            self.evaluate_if_present(AmlName::from_str("_INI").unwrap().resolve(path)?, vec![])
                        {
                            warn!("Failed to evaluate _INI for device {}: {:?}", path, err);
                        }
                        Ok(true)
                    } else {
                        /*
                         * If this device should not be initialized, don't initialize it's children.
                         */
                        Ok(false)
                    }
                }
                _ => Ok(true),
            }
        });
        if let Err(err) = init_status {
            warn!("Error while traversing namespace for devices: {:?}", err);
        }

        info!("Initialized {} devices", num_devices_initialized);
    }

    pub fn acquire_global_lock(&self, timeout: u16) -> Result<(), AmlError> {
        self.handler.acquire(self.global_lock_mutex, timeout)?;

        // Now we've acquired the AML-side mutex, acquire the hardware side
        // TODO: count the number of times we have to go round this loop / enforce a timeout?
        loop {
            if self.try_do_acquire_firmware_lock() {
                break Ok(());
            } else {
                /*
                 * The lock is owned by the firmware. We have set the pending bit - we now need to
                 * wait for the firmware to signal it has released the lock.
                 *
                 * TODO: this should wait for an interrupt from the firmware. That needs more infra
                 * so for now let's just spin round and try and acquire it again...
                 */
                self.handler.release(self.global_lock_mutex);
                continue;
            }
        }
    }

    /// Attempt to acquire the firmware lock, setting the owned bit if the lock is free. If the
    /// lock is not free, sets the pending bit to instruct the firmware to alert us when we can
    /// attempt to take ownership of the lock again. Returns `true` if we now have ownership of the
    /// lock, and `false` if we need to wait for firmware to release it.
    fn try_do_acquire_firmware_lock(&self) -> bool {
        let Some(facs) = &self.facs else { return true };
        loop {
            let global_lock = facs.global_lock.load(Ordering::Relaxed);
            let is_owned = global_lock.get_bit(1);

            /*
             * Compute the new value: either the lock is already owned, and we need to set the
             * pending bit and wait, or we can acquire ownership of the lock now. Either way, we
             * unconditionally set the owned bit and set the pending bit if the lock is already
             * owned.
             */
            let mut new_value = global_lock;
            new_value.set_bit(0, is_owned);
            new_value.set_bit(1, true);

            if facs
                .global_lock
                .compare_exchange(global_lock, new_value, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                break !is_owned;
            }
        }
    }

    pub fn release_global_lock(&self) -> Result<(), AmlError> {
        let is_pending = self.do_release_firmware_lock();
        if is_pending {
            self.registers.pm1_control_registers.set_bit(Pm1ControlBit::GlobalLockRelease, true).unwrap();
        }
        Ok(())
    }

    /// Atomically release the owned and pending bits of the global lock. Returns whether the
    /// pending bit was set (this means the firmware is waiting to acquire the lock, and should be
    /// informed we're finished with it).
    fn do_release_firmware_lock(&self) -> bool {
        let Some(facs) = &self.facs else { return false };
        loop {
            let global_lock = facs.global_lock.load(Ordering::Relaxed);
            let is_pending = global_lock.get_bit(0);
            let mut new_value = global_lock;
            new_value.set_bit(0, false);
            new_value.set_bit(1, false);

            if facs
                .global_lock
                .compare_exchange(global_lock, new_value, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
            {
                break is_pending;
            }
        }
    }

    fn do_execute_method(&self, mut context: MethodContext) -> Result<WrappedObject, AmlError> {
        /*
         * This is the main loop that executes operations. Every op is handled at the top-level of
         * the loop to prevent pathological stack growth from nested operations.
         *
         * The loop has three main stages:
         *   1) Check if any in-flight operations are ready to be executed (i.e. have collected all
         *      their arguments). An operation completing may contribute the last required argument
         *      of the one above, so this is repeated for as many operations as are ready to be
         *      retired.
         *   2) Look at the next opcode in the stream. If we've run out of opcodes in the current
         *      block, run logic to determine where in the stream we should move to next. Special
         *      logic at this level handles things like moving in/out of package definitions, and
         *      performing control flow.
         *   3) When the next opcode is determined, use it to interpret the next portion of the
         *      stream. If that is data, the correct number of bytes can be consumed and
         *      contributed to the current in-flight operation. If it's an opcode, a new in-flight
         *      operation is started, and we go round the loop again.
         *
         * This scheme is what allows the interpreter to use a loop that somewhat resembles a
         * traditional fast bytecode VM, but also provides enough flexibility to handle the
         * quirkier parts of the AML grammar, particularly the left-to-right encoding of operands.
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
                        let token = self.object_token.lock();
                        let Object::Integer(operand) = (unsafe { operand.gain_mut(&token) }) else {
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
                        context.retire_op(op);
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
                    Opcode::ToBuffer => self.do_to_buffer(&mut context, op)?,
                    Opcode::ToInteger => self.do_to_integer(&mut context, op)?,
                    Opcode::ToString => self.do_to_string(&mut context, op)?,
                    Opcode::ToDecimalString | Opcode::ToHexString => {
                        self.do_to_dec_hex_string(&mut context, op)?
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
                            Object::Buffer(buffer).wrap()
                        };
                        // TODO: use potentially-updated result for return value here
                        self.do_store(target, result.clone())?;
                        context.contribute_arg(Argument::Object(result));
                        context.retire_op(op);
                    }
                    Opcode::Reset => {
                        let [Argument::Object(sync_object)] = &op.arguments[..] else {
                            panic!();
                        };
                        let sync_object = sync_object.clone().unwrap_reference();

                        if let Object::Event(ref counter) = *sync_object {
                            counter.store(0, Ordering::Release);
                        } else {
                            return Err(AmlError::InvalidOperationOnObject {
                                op: Operation::ResetEvent,
                                typ: sync_object.typ(),
                            });
                        }
                    }
                    Opcode::Signal => {
                        let [Argument::Object(sync_object)] = &op.arguments[..] else {
                            panic!();
                        };
                        let sync_object = sync_object.clone().unwrap_reference();

                        if let Object::Event(ref counter) = *sync_object {
                            counter.fetch_add(1, Ordering::AcqRel);
                        } else {
                            return Err(AmlError::InvalidOperationOnObject {
                                op: Operation::SignalEvent,
                                typ: sync_object.typ(),
                            });
                        }
                    }
                    Opcode::Wait => {
                        let [Argument::Object(sync_object), Argument::Object(timeout)] = &op.arguments[..] else {
                            panic!();
                        };
                        let sync_object = sync_object.clone().unwrap_reference();
                        let timeout = u64::min(timeout.as_integer()?, 0xffff);

                        if let Object::Event(ref counter) = *sync_object {
                            /*
                             * `Wait` returns a non-zero value if a timeout occurs and the event
                             * was not signaled, and zero if it was. Timeout is specified in
                             * milliseconds, should relinquish processor control (we use
                             * `Handler::sleep` to do so) and a value of `0xffff` specifies that
                             * the operation should wait indefinitely.
                             */
                            let mut remaining_sleep = timeout;
                            let mut timed_out = true;

                            'signaled: while remaining_sleep > 0 {
                                loop {
                                    /*
                                     * Try to decrement the counter. If it's zero after a load, we
                                     * haven't been signalled and should wait for a bit. If it's
                                     * non-zero, we were signalled and should stop waiting.
                                     */
                                    let value = counter.load(Ordering::Acquire);
                                    if value == 0 {
                                        break;
                                    }
                                    if counter
                                        .compare_exchange(value, value - 1, Ordering::AcqRel, Ordering::Acquire)
                                        .is_ok()
                                    {
                                        timed_out = false;
                                        break 'signaled;
                                    }
                                }

                                let to_sleep = u64::min(timeout, 10);
                                if timeout < 0xffff {
                                    remaining_sleep -= to_sleep
                                }
                                self.handler.sleep(to_sleep);
                            }

                            context.contribute_arg(Argument::Object(
                                Object::Integer(if timed_out { u64::MAX } else { 0 }).wrap(),
                            ));
                        } else {
                            return Err(AmlError::InvalidOperationOnObject {
                                op: Operation::WaitEvent,
                                typ: sync_object.typ(),
                            });
                        }
                    }
                    Opcode::FromBCD => self.do_from_bcd(&mut context, op)?,
                    Opcode::ToBCD => self.do_to_bcd(&mut context, op)?,
                    Opcode::Name => {
                        let [Argument::Namestring(name), Argument::Object(object)] = &op.arguments[..] else {
                            panic!()
                        };

                        let name = name.resolve(&context.current_scope)?;
                        self.namespace.lock().insert(name, object.clone())?;
                        context.retire_op(op);
                    }
                    Opcode::Fatal => {
                        let [Argument::ByteData(typ), Argument::DWordData(code), Argument::Object(arg)] =
                            &op.arguments[..]
                        else {
                            panic!()
                        };
                        let arg = arg.as_integer()?;
                        self.handler.handle_fatal_error(*typ, *code, arg);
                        context.retire_op(op);
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

                        let region_offset = region_offset.clone().unwrap_transparent_reference();
                        let region_length = region_length.clone().unwrap_transparent_reference();

                        let region = Object::OpRegion(OpRegion {
                            space: RegionSpace::from(*region_space),
                            base: region_offset.as_integer()?,
                            length: region_length.as_integer()?,
                            parent_device_path: context.current_scope.clone(),
                        });
                        self.namespace.lock().insert(name.resolve(&context.current_scope)?, region.wrap())?;
                        context.retire_op(op);
                    }
                    Opcode::DataRegion => {
                        let [
                            Argument::Namestring(name),
                            Argument::Object(signature),
                            Argument::Object(oem_id),
                            Argument::Object(oem_table_id),
                        ] = &op.arguments[..]
                        else {
                            panic!()
                        };
                        let _signature = signature.as_string()?;
                        let _oem_id = oem_id.as_string()?;
                        let _oem_table_id = oem_table_id.as_string()?;

                        // TODO: once this is integrated into the rest of the crate, load the table
                        log::warn!(
                            "DefDataRegion encountered in AML! We don't actually support these - produced region will be incorrect"
                        );

                        let region = Object::OpRegion(OpRegion {
                            space: RegionSpace::SystemMemory,
                            base: 0,
                            length: 0,
                            parent_device_path: context.current_scope.clone(),
                        });
                        self.namespace.lock().insert(name.resolve(&context.current_scope)?, region.wrap())?;
                        context.retire_op(op);
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
                        let buffer_size = buffer_size.clone().unwrap_transparent_reference().as_integer()?;

                        let buffer_len = pkg_length - (context.current_block.pc - start_pc);
                        let mut buffer = vec![0; buffer_size as usize];
                        buffer[0..buffer_len].copy_from_slice(
                            &context.current_block.stream()
                                [context.current_block.pc..(context.current_block.pc + buffer_len)],
                        );
                        context.current_block.pc += buffer_len;

                        context.contribute_arg(Argument::Object(Object::Buffer(buffer).wrap()));
                        context.retire_op(op);
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
                        context.contribute_arg(Argument::Object(Object::Package(elements).wrap()));
                        context.retire_op(op);
                    }
                    Opcode::VarPackage => {
                        let Argument::Object(total_elements) = &op.arguments[0] else { panic!() };
                        let total_elements =
                            total_elements.clone().unwrap_transparent_reference().as_integer()? as usize;

                        let mut elements = Vec::with_capacity(total_elements);
                        for arg in &op.arguments[1..] {
                            let Argument::Object(object) = arg else { panic!() };
                            elements.push(object.clone());
                        }

                        /*
                         * As above, we always remove the block here after the in-flight op has
                         * been retired.
                         */
                        assert_eq!(context.current_block.kind, BlockKind::VarPackage);
                        assert_eq!(context.peek(), Err(AmlError::RunOutOfStream));
                        context.current_block = context.block_stack.pop().unwrap();
                        context.contribute_arg(Argument::Object(Object::Package(elements).wrap()));
                        context.retire_op(op);
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

                            /*
                             * Skip over the prolog to the else branch if present. Also handle if
                             * there are no more bytes to peek - the `If` op could be the last op
                             * in a block.
                             */
                            const DEF_ELSE_OP: u8 = 0xa1;
                            match context.peek() {
                                Ok(DEF_ELSE_OP) => {
                                    context.next()?;
                                    let _else_length = context.pkglength()?;
                                }
                                Ok(_) => (),
                                Err(AmlError::RunOutOfStream) => (),
                                Err(other) => Err(other)?,
                            }
                        }
                        context.retire_op(op);
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
                            Object::BufferField { buffer: buffer.clone(), offset: offset as usize, length }.wrap(),
                        )?;
                        context.retire_op(op);
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
                            Object::BufferField {
                                buffer: buffer.clone(),
                                offset: bit_index as usize,
                                length: num_bits as usize,
                            }
                            .wrap(),
                        )?;
                        context.retire_op(op);
                    }
                    Opcode::Store => {
                        let [Argument::Object(object), target] = &op.arguments[..] else { panic!() };
                        self.do_store(target, object.clone())?;
                        context.retire_op(op);
                    }
                    Opcode::RefOf => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        let reference =
                            Object::Reference { kind: ReferenceKind::RefOf, inner: object.clone() }.wrap();
                        context.contribute_arg(Argument::Object(reference));
                        context.retire_op(op);
                    }
                    Opcode::CondRefOf => {
                        let [Argument::Object(object), target] = &op.arguments[..] else { panic!() };
                        let result = if let Object::Reference { kind: ReferenceKind::Unresolved, .. } = **object {
                            Object::Integer(0)
                        } else {
                            let reference =
                                Object::Reference { kind: ReferenceKind::RefOf, inner: object.clone() }.wrap();
                            self.do_store(target, reference)?;
                            Object::Integer(u64::MAX)
                        };
                        context.contribute_arg(Argument::Object(result.wrap()));
                        context.retire_op(op);
                    }
                    Opcode::DerefOf => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        let result = if object.typ() == ObjectType::Reference {
                            object.clone().unwrap_reference()
                        } else if object.typ() == ObjectType::String {
                            let path = AmlName::from_str(&object.as_string().unwrap())?
                                .resolve(&context.current_scope)?;
                            self.namespace.lock().get(path)?.clone()
                        } else {
                            return Err(AmlError::ObjectNotOfExpectedType {
                                expected: ObjectType::Reference,
                                got: object.typ(),
                            });
                        };
                        context.contribute_arg(Argument::Object(result));
                        context.retire_op(op);
                    }
                    Opcode::Sleep => {
                        let [Argument::Object(msec)] = &op.arguments[..] else { panic!() };
                        self.handler.sleep(msec.as_integer()?);
                        context.retire_op(op);
                    }
                    Opcode::Stall => {
                        let [Argument::Object(usec)] = &op.arguments[..] else { panic!() };
                        self.handler.stall(usec.as_integer()?);
                        context.retire_op(op);
                    }
                    Opcode::Acquire => {
                        let [Argument::Object(mutex)] = &op.arguments[..] else { panic!() };
                        let Object::Mutex { mutex, sync_level: _ } = **mutex else {
                            Err(AmlError::InvalidOperationOnObject { op: Operation::Acquire, typ: mutex.typ() })?
                        };
                        let timeout = context.next_u16()?;

                        // TODO: should we do something with the sync level??
                        if mutex == self.global_lock_mutex {
                            self.acquire_global_lock(timeout)?;
                        } else {
                            self.handler.acquire(mutex, timeout)?;
                        }

                        context.retire_op(op);
                    }
                    Opcode::Release => {
                        let [Argument::Object(mutex)] = &op.arguments[..] else { panic!() };
                        let Object::Mutex { mutex, sync_level: _ } = **mutex else {
                            Err(AmlError::InvalidOperationOnObject { op: Operation::Release, typ: mutex.typ() })?
                        };

                        // TODO: should we do something with the sync level??
                        if mutex == self.global_lock_mutex {
                            self.release_global_lock()?;
                        } else {
                            self.handler.release(mutex);
                        }

                        context.retire_op(op);
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

                        if let Object::Method { .. } = **method {
                            self.namespace
                                .lock()
                                .add_level(method_scope.clone(), NamespaceLevelKind::MethodLocals)?;

                            let new_context =
                                MethodContext::new_from_method(method.clone(), args, method_scope.clone())?;
                            let old_context = mem::replace(&mut context, new_context);
                            self.context_stack.lock().push(old_context);
                            context.retire_op(op);
                        } else if let Object::NativeMethod { ref f, .. } = **method {
                            let result = f(&args)?;
                            context.contribute_arg(Argument::Object(result));
                        } else {
                            panic!();
                        }
                    }
                    Opcode::Return => {
                        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
                        let object = object.clone().unwrap_transparent_reference();

                        if let Some(last) = self.context_stack.lock().pop() {
                            context = last;
                            context.contribute_arg(Argument::Object(object.clone()));
                            context.retire_op(op);
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

                        context.contribute_arg(Argument::Object(Object::Integer(typ).wrap()));
                        context.retire_op(op);
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
                        context.retire_op(op);
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
                            context.retire_op(op);
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
                            break Ok(Object::Uninitialized.wrap());
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
                                return Ok(Object::Uninitialized.wrap());
                            }
                        }
                        BlockKind::Scope { old_scope } => {
                            assert!(!context.block_stack.is_empty());
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
                            assert!(!context.block_stack.is_empty());

                            if let Some(package_op) = context.in_flight.last_mut()
                                && package_op.op == Opcode::Package
                            {
                                let num_elements_left = package_op.expected_arguments - package_op.arguments.len();
                                for _ in 0..num_elements_left {
                                    package_op.arguments.push(Argument::Object(Object::Uninitialized.wrap()));
                                }
                            }

                            // XXX: don't remove the package's block. Refer to completion of
                            // package ops for rationale here.
                            continue;
                        }
                        BlockKind::VarPackage => {
                            assert!(!context.block_stack.is_empty());

                            if let Some(package_op) = context.in_flight.last_mut()
                                && package_op.op == Opcode::VarPackage
                            {
                                let num_elements_left = {
                                    let Argument::Object(total_elements) = &package_op.arguments[0] else {
                                        panic!()
                                    };
                                    let total_elements =
                                        total_elements.clone().unwrap_transparent_reference().as_integer()?
                                            as usize;

                                    // Update the expected number of arguments to terminate the in-flight op
                                    package_op.expected_arguments = package_op.arguments.len();
                                    total_elements - (package_op.arguments.len() - 1)
                                };

                                for _ in 0..num_elements_left {
                                    package_op.arguments.push(Argument::Object(Object::Uninitialized.wrap()));
                                }
                            }

                            // As above, leave the package's block.
                            continue;
                        }
                        BlockKind::IfThenBranch => {
                            context.current_block = context.block_stack.pop().unwrap();

                            /*
                             * Check for an else-branch, and skip over it. We need to handle the
                             * case here where there isn't a next byte - that just means the `If`
                             * is the last op in a block.
                             */
                            const DEF_ELSE_OP: u8 = 0xa1;
                            match context.peek() {
                                Ok(DEF_ELSE_OP) => {
                                    context.next()?;
                                    let start_pc = context.current_block.pc;
                                    let else_length = context.pkglength()?;
                                    context.current_block.pc +=
                                        else_length - (context.current_block.pc - start_pc);
                                }
                                Ok(_) => (),
                                Err(AmlError::RunOutOfStream) => (),
                                Err(other) => Err(other)?,
                            };

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
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(0).wrap()));
                }
                Opcode::One => {
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(1).wrap()));
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
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(value as u64).wrap()));
                }
                Opcode::WordPrefix => {
                    let value = context.next_u16()?;
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(value as u64).wrap()));
                }
                Opcode::DWordPrefix => {
                    let value = context.next_u32()?;
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(value as u64).wrap()));
                }
                Opcode::StringPrefix => {
                    let str_start = context.current_block.pc;
                    while context.next()? != b'\0' {}
                    // TODO: handle err
                    let str = String::from(
                        str::from_utf8(&context.current_block.stream()[str_start..(context.current_block.pc - 1)])
                            .unwrap(),
                    );
                    context.last_op()?.arguments.push(Argument::Object(Object::String(str).wrap()));
                }
                Opcode::QWordPrefix => {
                    let value = context.next_u64()?;
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(value).wrap()));
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
                Opcode::VarPackage => {
                    let start_pc = context.current_block.pc;
                    let pkg_length = context.pkglength()?;
                    let remaining_length = pkg_length - (context.current_block.pc - start_pc);

                    /*
                     * For variable packages, we're first going to parse a `TermArg` that encodes,
                     * dynamically, how many elements the package will have. We then accept as many
                     * elements as remain in the block, and we'll sort out how many are supposed to
                     * be in the package later.
                     */
                    context.start_in_flight_op(OpInFlight::new(Opcode::VarPackage, usize::MAX));
                    context.start_new_block(BlockKind::VarPackage, remaining_length);
                }
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
                    self.namespace.lock().insert(name, Object::Method { code, flags }.wrap())?;
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
                    let mutex = self.handler.create_mutex();
                    self.namespace.lock().insert(name, Object::Mutex { mutex, sync_level }.wrap())?;
                }
                Opcode::Event => {
                    let name = context.namestring()?;

                    let name = name.resolve(&context.current_scope)?;
                    self.namespace.lock().insert(name, Object::Event(Arc::new(AtomicU64::new(0))).wrap())?;
                }
                Opcode::LoadTable => todo!(),
                Opcode::Load => todo!(),
                Opcode::Stall => context.start_in_flight_op(OpInFlight::new(Opcode::Stall, 1)),
                Opcode::Sleep => context.start_in_flight_op(OpInFlight::new(Opcode::Sleep, 1)),
                Opcode::Acquire => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Release => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Signal => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Wait => context.start_in_flight_op(OpInFlight::new(opcode, 2)),
                Opcode::Reset => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Notify => todo!(),
                Opcode::FromBCD | Opcode::ToBCD => context.start_in_flight_op(OpInFlight::new(opcode, 2)),
                Opcode::Revision => {
                    context.contribute_arg(Argument::Object(Object::Integer(INTERPRETER_REVISION).wrap()));
                }
                Opcode::Debug => {
                    context.contribute_arg(Argument::Object(Object::Debug.wrap()));
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
                    context.contribute_arg(Argument::Object(Object::Integer(time).wrap()));
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
                Opcode::DataRegion => {
                    let name = context.namestring()?;
                    context.start_in_flight_op(OpInFlight::new_with(
                        Opcode::DataRegion,
                        vec![Argument::Namestring(name)],
                        3,
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
                    namespace.insert(new_scope.clone(), object.wrap())?;

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
                    namespace.insert(new_scope.clone(), object.wrap())?;

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
                    namespace.insert(new_scope.clone(), object.wrap())?;

                    let old_scope = mem::replace(&mut context.current_scope, new_scope);
                    context.start_new_block(BlockKind::Scope { old_scope }, remaining_length);
                }
                Opcode::Local(local) => {
                    let local = context.locals[local as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(
                        Object::Reference { kind: ReferenceKind::LocalOrArg, inner: local }.wrap(),
                    ));
                }
                Opcode::Arg(arg) => {
                    let arg = context.args[arg as usize].clone();
                    context.last_op()?.arguments.push(Argument::Object(
                        Object::Reference { kind: ReferenceKind::LocalOrArg, inner: arg }.wrap(),
                    ));
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

                    /*
                     * The desired behaviour when we encounter a name at the top-level differs
                     * depending on the context we're in.
                     *    - Generally, we want to attempt to evaluate names to objects that should have
                     *      already been defined. There are generally no forward definitions in AML.
                     *    - In `CondRefOf`, we need to handle a name not referring to any object. For
                     *      this, we emit an `Unresolved` reference.
                     *    - In package definitions, all objects referred to by name should be referred
                     *      to by a string. This is not well defined by the specification, but matches
                     *      expected behaviour of other interpreters, and is most useful for downstream
                     *      users.
                     *    - In variable-length package definitions, the first 'element' is the
                     *      length of the package, and should be resolved to an object. The
                     *      remaining elements should be treated the same as in a package definition.
                     */
                    enum ResolveBehaviour {
                        ResolveToObject,
                        ResolveIfExists,
                        PackageElement,
                    }
                    let behaviour = if context.current_block.kind == BlockKind::Package {
                        ResolveBehaviour::PackageElement
                    } else if context.current_block.kind == BlockKind::VarPackage {
                        if context.last_op()?.arguments.is_empty() {
                            ResolveBehaviour::ResolveToObject
                        } else {
                            ResolveBehaviour::PackageElement
                        }
                    } else if context.in_flight.last().map(|op| op.op == Opcode::CondRefOf).unwrap_or(false) {
                        ResolveBehaviour::ResolveIfExists
                    } else {
                        ResolveBehaviour::ResolveToObject
                    };

                    match behaviour {
                        ResolveBehaviour::ResolveToObject => {
                            let object = self.namespace.lock().search(&name, &context.current_scope);
                            match object {
                                Ok((resolved_name, object)) => {
                                    if let Object::Method { flags, .. } | Object::NativeMethod { flags, .. } =
                                        *object
                                    {
                                        context.start_in_flight_op(OpInFlight::new_with(
                                            Opcode::InternalMethodCall,
                                            vec![Argument::Object(object), Argument::Namestring(resolved_name)],
                                            flags.arg_count(),
                                        ))
                                    } else if let Object::FieldUnit(ref field) = *object {
                                        let value = self.do_field_read(field)?;
                                        context.last_op()?.arguments.push(Argument::Object(value));
                                    } else {
                                        context.last_op()?.arguments.push(Argument::Object(object));
                                    }
                                }
                                Err(err) => Err(err)?,
                            }
                        }
                        ResolveBehaviour::ResolveIfExists => {
                            let object = self.namespace.lock().search(&name, &context.current_scope);
                            match object {
                                Ok((_, object)) => {
                                    let reference =
                                        Object::Reference { kind: ReferenceKind::RefOf, inner: object.clone() };
                                    context.last_op()?.arguments.push(Argument::Object(reference.wrap()));
                                }
                                Err(AmlError::ObjectDoesNotExist(_)) => {
                                    let reference = Object::Reference {
                                        kind: ReferenceKind::Unresolved,
                                        inner: Object::String(name.to_string()).wrap(),
                                    };
                                    context.last_op()?.arguments.push(Argument::Object(reference.wrap()));
                                }
                                Err(other) => Err(other)?,
                            }
                        }
                        ResolveBehaviour::PackageElement => {
                            context
                                .last_op()?
                                .arguments
                                .push(Argument::Object(Object::String(name.to_string()).wrap()));
                        }
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
                Opcode::DerefOf => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::ConcatRes => context.start_in_flight_op(OpInFlight::new(opcode, 3)),
                Opcode::SizeOf => context.start_in_flight_op(OpInFlight::new(opcode, 1)),
                Opcode::Index => context.start_in_flight_op(OpInFlight::new(opcode, 3)),
                /*
                 * TODO
                 * Match is a difficult opcode to parse, as it interleaves dynamic arguments and
                 * random bytes that need to be extracted as you go. I think we'll need to use 1+
                 * internal in-flight ops to parse the static bytedatas as we go, and then retire
                 * the real op at the end.
                 */
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

                Opcode::ToBuffer | Opcode::ToDecimalString | Opcode::ToHexString | Opcode::ToInteger => {
                    context.start_in_flight_op(OpInFlight::new(opcode, 2))
                }
                Opcode::ToString => context.start_in_flight_op(OpInFlight::new(opcode, 3)),

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
                    context.last_op()?.arguments.push(Argument::Object(Object::Integer(u64::MAX).wrap()));
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
                    /*
                     * These aren't actually fields themselves, but are created by `AccessAs` AML
                     * elements. They change the access type and attributes for remaining fields in
                     * the list.
                     */
                    let _access_type = context.next()?;
                    let _access_attrib = context.next()?;
                    todo!()
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
                    self.namespace.lock().insert(field_name.resolve(&context.current_scope)?, field.wrap())?;

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
                    self.do_store(remainder, Object::Integer(left.wrapping_rem(right)).wrap())?;
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

        let result = Object::Integer(result).wrap();
        let result = self.do_store(target, result)?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
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
                     * This is a particularly important place to respect the integer width as set
                     * by the DSDT revision.
                     */
                    if self.dsdt_revision >= 2 {
                        (operand.leading_zeros() + 1) as u64
                    } else {
                        ((operand as u32).leading_zeros() + 1) as u64
                    }
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

        context.contribute_arg(Argument::Object(Object::Integer(result).wrap()));
        context.retire_op(op);
        Ok(())
    }

    fn do_logical_op(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        if op.op == Opcode::LNot {
            let [Argument::Object(operand)] = &op.arguments[..] else { panic!() };
            let operand = operand.clone().unwrap_transparent_reference().as_integer()?;
            let result = if operand == 0 { u64::MAX } else { 0 };

            context.contribute_arg(Argument::Object(Object::Integer(result).wrap()));
            context.retire_op(op);
            return Ok(());
        }

        let [Argument::Object(left), Argument::Object(right)] = &op.arguments[..] else { panic!() };
        let left = left.clone().unwrap_transparent_reference();
        let right = right.clone().unwrap_transparent_reference();

        /*
         * Some of these operations allow strings and buffers to be used as operands. Apparently
         * NT's interpreter just takes the first 4 bytes of the string/buffer and casts them as an
         * integer...
         */
        let (left, right) = match *left {
            Object::Integer(left) => (left, right.as_integer()?),
            Object::String(ref left) => {
                let left = {
                    let mut bytes = [0u8; 4];
                    let left_bytes = left.as_bytes();
                    let bytes_to_use = usize::min(4, left_bytes.len());
                    (bytes[0..bytes_to_use]).copy_from_slice(&left_bytes[0..bytes_to_use]);
                    u32::from_le_bytes(bytes) as u64
                };
                let right = {
                    let mut bytes = [0u8; 4];
                    let right = right.as_string()?;
                    let right_bytes = right.as_bytes();
                    let bytes_to_use = usize::min(4, right_bytes.len());
                    (bytes[0..bytes_to_use]).copy_from_slice(&right_bytes[0..bytes_to_use]);
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

        context.contribute_arg(Argument::Object(result.wrap()));
        context.retire_op(op);
        Ok(())
    }

    fn do_to_buffer(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(operand), target] = &op.arguments[..] else { panic!() };
        let operand = operand.clone().unwrap_transparent_reference();

        let result = match *operand {
            Object::Buffer(ref bytes) => Object::Buffer(bytes.clone()),
            Object::Integer(value) => {
                if self.dsdt_revision >= 2 {
                    Object::Buffer(value.to_le_bytes().to_vec())
                } else {
                    Object::Buffer((value as u32).to_le_bytes().to_vec())
                }
            }
            Object::String(ref value) => {
                // XXX: an empty string is converted to an empty buffer, *without* the null-terminator
                if value.is_empty() {
                    Object::Buffer(vec![])
                } else {
                    let mut bytes = value.as_bytes().to_vec();
                    bytes.push(b'\0');
                    Object::Buffer(bytes)
                }
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::ToBuffer, typ: operand.typ() })?,
        }
        .wrap();

        let result = self.do_store(target, result)?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
        Ok(())
    }

    fn do_to_integer(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(operand), target] = &op.arguments[..] else { panic!() };
        let operand = operand.clone().unwrap_transparent_reference();

        let result = match *operand {
            Object::Integer(value) => Object::Integer(value),
            Object::Buffer(ref bytes) => {
                /*
                 * The spec says this should respect the revision of the current definition block.
                 * Apparently, the NT interpreter always uses the first 8 bytes of the buffer.
                 */
                let mut to_interpret = [0u8; 8];
                (to_interpret[0..usize::min(bytes.len(), 8)]).copy_from_slice(bytes);
                Object::Integer(u64::from_le_bytes(to_interpret))
            }
            Object::String(ref value) => {
                /*
                 * This is about the same level of effort as ACPICA puts in. The uACPI test suite
                 * has tests that this fails - namely because of support for octal, signs, strings
                 * that won't fit in a `u64` etc. We probably need to write a more robust parser
                 * 'real' parser to handle those cases.
                 */
                if let Some(value) = value.strip_prefix("0x") {
                    let parsed = u64::from_str_radix(value, 16).map_err(|_| {
                        AmlError::InvalidOperationOnObject { op: Operation::ToInteger, typ: ObjectType::String }
                    })?;
                    Object::Integer(parsed)
                } else {
                    let parsed = str::parse::<u64>(value).map_err(|_| AmlError::InvalidOperationOnObject {
                        op: Operation::ToInteger,
                        typ: ObjectType::String,
                    })?;
                    Object::Integer(parsed)
                }
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::ToBuffer, typ: operand.typ() })?,
        }
        .wrap();

        let result = self.do_store(target, result)?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
        Ok(())
    }

    fn do_to_string(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(source), Argument::Object(length), target] = &op.arguments[..] else { panic!() };
        let source = source.clone().unwrap_transparent_reference();
        let source = source.as_buffer()?;
        let length = length.clone().unwrap_transparent_reference().as_integer()? as usize;

        let result = if source.is_empty() {
            Object::String(String::new())
        } else {
            let mut buffer = source.split_inclusive(|b| *b == b'\0').next().unwrap();
            if length < usize::MAX {
                buffer = &buffer[0..usize::min(length, buffer.len())];
            }
            let string = str::from_utf8(buffer).map_err(|_| AmlError::InvalidOperationOnObject {
                op: Operation::ToString,
                typ: ObjectType::Buffer,
            })?;
            Object::String(string.to_string())
        }
        .wrap();

        let result = self.do_store(target, result)?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
        Ok(())
    }

    /// Perform a `ToDecimalString` or `ToHexString` operation
    fn do_to_dec_hex_string(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(operand), target] = &op.arguments[..] else { panic!() };
        let operand = operand.clone().unwrap_transparent_reference();

        let result = match *operand {
            Object::String(ref value) => Object::String(value.clone()),
            Object::Integer(value) => match op.op {
                Opcode::ToDecimalString => Object::String(value.to_string()),
                Opcode::ToHexString => Object::String(alloc::format!("{value:#x}")),
                _ => panic!(),
            },
            Object::Buffer(ref bytes) => {
                if bytes.is_empty() {
                    Object::String(String::new())
                } else {
                    let mut string = String::new();
                    for byte in bytes {
                        let as_str = match op.op {
                            Opcode::ToDecimalString => alloc::format!("{byte},"),
                            Opcode::ToHexString => alloc::format!("{byte:#04X},"),
                            _ => panic!(),
                        };
                        string.push_str(&as_str);
                    }
                    // Remove last comma, if present
                    if !string.is_empty() {
                        string.pop();
                    }
                    Object::String(string)
                }
            }
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::ToDecOrHexString, typ: operand.typ() })?,
        }
        .wrap();

        let result = self.do_store(target, result)?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
        Ok(())
    }

    fn do_mid(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(source), Argument::Object(index), Argument::Object(length), target] =
            &op.arguments[..]
        else {
            panic!()
        };
        let index = index.clone().unwrap_transparent_reference().as_integer()? as usize;
        let length = length.clone().unwrap_transparent_reference().as_integer()? as usize;

        let result = match **source {
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
        }
        .wrap();

        self.do_store(target, result.clone())?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
        Ok(())
    }

    fn do_concat(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(source1), Argument::Object(source2), target] = &op.arguments[..] else { panic!() };
        let source1 = source1.clone().unwrap_transparent_reference();
        let source2 = source2.clone().unwrap_transparent_reference();

        fn resolve_as_string(obj: &Object) -> String {
            match obj {
                Object::Uninitialized => "[Uninitialized Object]".to_string(),
                Object::Buffer(bytes) => String::from_utf8_lossy(bytes).into_owned(),
                Object::BufferField { .. } => "[Buffer Field]".to_string(),
                Object::Device => "[Device]".to_string(),
                Object::Event(_) => "[Event]".to_string(),
                Object::FieldUnit(_) => "[Field]".to_string(),
                Object::Integer(value) => value.to_string(),
                Object::Method { .. } | Object::NativeMethod { .. } => "[Control Method]".to_string(),
                Object::Mutex { .. } => "[Mutex]".to_string(),
                Object::Reference { inner, .. } => resolve_as_string(&(inner.clone().unwrap_reference())),
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
                Object::Buffer(buffer).wrap()
            }
            ObjectType::Buffer => {
                let mut buffer = source1.as_buffer()?.to_vec();
                buffer.extend(source2.to_buffer(if self.dsdt_revision >= 2 { 8 } else { 4 })?);
                Object::Buffer(buffer).wrap()
            }
            _ => {
                let source1 = resolve_as_string(&source1);
                let source2 = resolve_as_string(&source2);
                Object::String(source1 + &source2).wrap()
            }
        };

        let result = self.do_store(target, result)?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
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

        context.contribute_arg(Argument::Object(Object::Integer(result).wrap()));
        context.retire_op(op);
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

        context.contribute_arg(Argument::Object(Object::Integer(result).wrap()));
        context.retire_op(op);
        Ok(())
    }

    fn do_size_of(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(object)] = &op.arguments[..] else { panic!() };
        let object = object.clone().unwrap_transparent_reference();

        let result = match *object {
            Object::Buffer(ref buffer) => buffer.len(),
            Object::String(ref str) => str.len(),
            Object::Package(ref package) => package.len(),
            _ => Err(AmlError::InvalidOperationOnObject { op: Operation::SizeOf, typ: object.typ() })?,
        };

        context.contribute_arg(Argument::Object(Object::Integer(result as u64).wrap()));
        context.retire_op(op);
        Ok(())
    }

    fn do_index(&self, context: &mut MethodContext, op: OpInFlight) -> Result<(), AmlError> {
        let [Argument::Object(object), Argument::Object(index_value), target] = &op.arguments[..] else {
            panic!()
        };
        let object = object.clone().unwrap_transparent_reference();
        let index_value = index_value.clone().unwrap_transparent_reference().as_integer()?;

        let result = match *object {
            Object::Buffer(ref buffer) => {
                if index_value as usize >= buffer.len() {
                    Err(AmlError::IndexOutOfBounds)?
                }

                Object::Reference {
                    kind: ReferenceKind::RefOf,
                    inner: Object::BufferField {
                        buffer: object.clone(),
                        offset: index_value as usize * 8,
                        length: 8,
                    }
                    .wrap(),
                }
            }
            Object::String(ref string) => {
                if index_value as usize >= string.len() {
                    Err(AmlError::IndexOutOfBounds)?
                }

                Object::Reference {
                    kind: ReferenceKind::RefOf,
                    inner: Object::BufferField {
                        buffer: object.clone(),
                        offset: index_value as usize * 8,
                        length: 8,
                    }
                    .wrap(),
                }
            }
            Object::Package(ref package) => {
                let Some(element) = package.get(index_value as usize) else { Err(AmlError::IndexOutOfBounds)? };
                Object::Reference { kind: ReferenceKind::RefOf, inner: element.clone() }
            }
            _ => Err(AmlError::IndexOutOfBounds)?,
        }
        .wrap();

        self.do_store(target, result.clone())?;
        context.contribute_arg(Argument::Object(result));
        context.retire_op(op);
        Ok(())
    }

    fn do_store(&self, target: &Argument, object: WrappedObject) -> Result<WrappedObject, AmlError> {
        let object = object.unwrap_transparent_reference();
        let token = self.object_token.lock();

        /*
         * TODO: stores should do more implicit conversion to the type of the destination in some
         * cases, in line with section 19.3.5 of the spec
         *
         * TODO: stores to fields with `BufferAcc` can actually return a value of the store that
         * differs from what was written into the field. This is used for complex field types with
         * a write-then-read pattern. The return value is then used as the 'result' of the storing
         * expression.
         */
        let to_return = object.clone();

        match target {
            Argument::Object(target) => match unsafe { target.gain_mut(&token) } {
                Object::Integer(target) => match unsafe { object.gain_mut(&token) } {
                    Object::Integer(value) => {
                        *target = *value;
                    }
                    Object::BufferField { .. } => {
                        let mut buffer = [0u8; 8];
                        unsafe { object.gain_mut(&token) }.read_buffer_field(&mut buffer)?;
                        let value = u64::from_le_bytes(buffer);
                        *target = value;
                    }
                    Object::FieldUnit(field) => {
                        // TODO: not sure if we should convert buffers to integers if needed here?
                        *target = self.do_field_read(field)?.as_integer()?;
                    }
                    _ => {
                        let as_integer = object.to_integer(if self.dsdt_revision >= 2 { 8 } else { 4 })?;
                        *target = as_integer;
                    }
                },
                Object::BufferField { .. } => match unsafe { object.gain_mut(&token) } {
                    Object::Integer(value) => {
                        unsafe { target.gain_mut(&token) }.write_buffer_field(&value.to_le_bytes(), &token)?;
                    }
                    Object::Buffer(value) => {
                        unsafe { target.gain_mut(&token) }.write_buffer_field(value.as_slice(), &token)?;
                    }
                    _ => panic!(),
                },
                Object::FieldUnit(field) => self.do_field_write(field, object)?,
                Object::Reference { kind, inner } => {
                    match kind {
                        ReferenceKind::RefOf => todo!(),
                        ReferenceKind::LocalOrArg => {
                            if let Object::Reference { kind: _inner_kind, inner: inner_inner } = &**inner {
                                // TODO: this should store into the reference, potentially doing an
                                // implicit cast
                                unsafe {
                                    *inner_inner.gain_mut(&token) = object.gain_mut(&token).clone();
                                }
                            } else {
                                // Overwrite the value
                                unsafe {
                                    *inner.gain_mut(&token) = object.gain_mut(&token).clone();
                                }
                            }
                        }
                        ReferenceKind::Unresolved => todo!(),
                    }
                }
                Object::Debug => {
                    self.handler.handle_debug(&object);
                }
                _ => panic!("Stores to objects like {:?} are not yet supported", target),
            },

            Argument::Namestring(_) => todo!(),
            Argument::ByteData(_) | Argument::DWordData(_) | Argument::TrackedPc(_) | Argument::PkgLength(_) => {
                panic!()
            }
        }

        Ok(to_return)
    }

    /// Do a read from a field by performing one or more well-formed accesses to the underlying
    /// operation regions, and then shifting and masking the resulting value as appropriate. Will
    /// return either an `Integer` or `Buffer` as appropriate, guided by the size of the field
    /// and expected integer size (as per the DSDT revision).
    fn do_field_read(&self, field: &FieldUnit) -> Result<WrappedObject, AmlError> {
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

        let read_region = match field.kind {
            FieldUnitKind::Normal { ref region } => region,
            // FieldUnitKind::Bank { ref region, ref bank, bank_value } => {
            FieldUnitKind::Bank { .. } => {
                // TODO: put the bank_value in the bank
                todo!();
            }
            // FieldUnitKind::Index { ref index, ref data } => {
            FieldUnitKind::Index { .. } => {
                // TODO: configure the correct index
                todo!();
            }
        };
        let Object::OpRegion(ref read_region) = **read_region else { panic!() };

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
            let aligned_offset = object::align_down(field.bit_index + i * access_width_bits, access_width_bits);
            let raw = self.do_native_region_read(read_region, aligned_offset / 8, access_width_bits / 8)?;
            let src_index = if i == 0 { field.bit_index % access_width_bits } else { 0 };
            let remaining_length = field.bit_length - read_so_far;
            let length = if i == 0 {
                usize::min(remaining_length, access_width_bits - (field.bit_index % access_width_bits))
            } else {
                usize::min(remaining_length, access_width_bits)
            };

            object::copy_bits(&raw.to_le_bytes(), src_index, output_bytes, read_so_far, length);
            read_so_far += length;
        }

        match output {
            Output::Buffer(bytes) => Ok(Object::Buffer(bytes).wrap()),
            Output::Integer(value) => Ok(Object::Integer(u64::from_le_bytes(value)).wrap()),
        }
    }

    fn do_field_write(&self, field: &FieldUnit, value: WrappedObject) -> Result<(), AmlError> {
        trace!("AML field write. Field = {:?}. Value = {:?}", field, value);

        let value_bytes = match &*value {
            Object::Integer(value) => &value.to_le_bytes() as &[u8],
            Object::Buffer(bytes) => bytes,
            _ => Err(AmlError::ObjectNotOfExpectedType { expected: ObjectType::Integer, got: value.typ() })?,
        };
        let access_width_bits = field.flags.access_type_bytes()? * 8;

        let write_region = match field.kind {
            FieldUnitKind::Normal { ref region } => region,
            // FieldUnitKind::Bank { ref region, ref bank, bank_value } => {
            FieldUnitKind::Bank { .. } => {
                // TODO: put the bank_value in the bank
                todo!();
            }
            // FieldUnitKind::Index { ref index, ref data } => {
            FieldUnitKind::Index { .. } => {
                // TODO: configure the correct index
                todo!();
            }
        };
        let Object::OpRegion(ref write_region) = **write_region else { panic!() };

        // TODO: if the region wants locking, do that

        // TODO: maybe also a fast path for writes

        let native_accesses_needed = (field.bit_length + (field.bit_index % access_width_bits))
            .next_multiple_of(access_width_bits)
            / access_width_bits;
        let mut written_so_far = 0;

        for i in 0..native_accesses_needed {
            let aligned_offset = object::align_down(field.bit_index + i * access_width_bits, access_width_bits);
            let dst_index = if i == 0 { field.bit_index % access_width_bits } else { 0 };

            /*
             * If we're not going to write a whole native access, respect the field's
             * update rule. If we're meant to preserve the surrounding bits, we need to do
             * a read first.
             */
            let mut bytes = if dst_index > 0 || (field.bit_length - written_so_far) < access_width_bits {
                match field.flags.update_rule() {
                    FieldUpdateRule::Preserve => self
                        .do_native_region_read(write_region, aligned_offset / 8, access_width_bits / 8)?
                        .to_le_bytes(),
                    FieldUpdateRule::WriteAsOnes => [0xff; 8],
                    FieldUpdateRule::WriteAsZeros => [0; 8],
                }
            } else {
                [0; 8]
            };

            let remaining_length = field.bit_length - written_so_far;
            let length = if i == 0 {
                usize::min(remaining_length, access_width_bits - (field.bit_index % access_width_bits))
            } else {
                usize::min(remaining_length, access_width_bits)
            };

            object::copy_bits(value_bytes, written_so_far, &mut bytes, dst_index, length);
            self.do_native_region_write(
                write_region,
                aligned_offset / 8,
                access_width_bits / 8,
                u64::from_le_bytes(bytes),
            )?;
            written_so_far += length;
        }

        Ok(())
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
                    8 => self.handler.read_u64(address),
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
                let address = self.pci_address_for_device(&region.parent_device_path)?;
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
                if let Some(_handler) = self.region_handlers.lock().get(&region.space) {
                    warn!("Custom region handlers aren't actually supported yet.");
                    Err(AmlError::LibUnimplemented)
                } else {
                    Err(AmlError::NoHandlerForRegionAccess(region.space))
                }
            }
        }
    }

    /// Performs an actual write to an operation region. `offset` and `length` must respect the
    /// access requirements of the field being read, and are supplied in **bytes**. This may call
    /// AML methods if required, and may invoke user-supplied handlers.
    fn do_native_region_write(
        &self,
        region: &OpRegion,
        offset: usize,
        length: usize,
        value: u64,
    ) -> Result<(), AmlError> {
        trace!(
            "Native field write. Region = {:?}, offset = {:#x}, length={:#x}, value={:#x}",
            region, offset, length, value
        );

        match region.space {
            RegionSpace::SystemMemory => {
                let address = region.base as usize + offset;
                match length {
                    1 => self.handler.write_u8(address, value as u8),
                    2 => self.handler.write_u16(address, value as u16),
                    4 => self.handler.write_u32(address, value as u32),
                    8 => self.handler.write_u64(address, value),
                    _ => panic!(),
                }
                Ok(())
            }
            RegionSpace::SystemIO => {
                let address = region.base as u16 + offset as u16;
                match length {
                    1 => self.handler.write_io_u8(address, value as u8),
                    2 => self.handler.write_io_u16(address, value as u16),
                    4 => self.handler.write_io_u32(address, value as u32),
                    _ => panic!(),
                }
                Ok(())
            }
            RegionSpace::PciConfig => {
                let address = self.pci_address_for_device(&region.parent_device_path)?;
                match length {
                    1 => self.handler.write_pci_u8(address, offset as u16, value as u8),
                    2 => self.handler.write_pci_u16(address, offset as u16, value as u16),
                    4 => self.handler.write_pci_u32(address, offset as u16, value as u32),
                    _ => panic!(),
                }
                Ok(())
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
                if let Some(_handler) = self.region_handlers.lock().get(&region.space) {
                    warn!("Custom region handlers aren't actually supported yet.");
                    Err(AmlError::LibUnimplemented)
                } else {
                    Err(AmlError::NoHandlerForRegionAccess(region.space))
                }
            }
        }
    }

    fn pci_address_for_device(&self, path: &AmlName) -> Result<PciAddress, AmlError> {
        /*
         * TODO: it's not ideal to do these reads for every native access. See if we can
         * cache them somewhere?
         */
        let seg = match self.evaluate_if_present(AmlName::from_str("_SEG").unwrap().resolve(path)?, vec![])? {
            Some(value) => value.as_integer()?,
            None => 0,
        };
        let bus = match self.evaluate_if_present(AmlName::from_str("_BBR").unwrap().resolve(path)?, vec![])? {
            Some(value) => value.as_integer()?,
            None => 0,
        };
        let (device, function) = {
            let adr = self.evaluate_if_present(AmlName::from_str("_ADR").unwrap().resolve(path)?, vec![])?;
            let adr = match adr {
                Some(adr) => adr.as_integer()?,
                None => 0,
            };
            (adr.get_bits(16..32), adr.get_bits(0..16))
        };
        Ok(PciAddress::new(seg as u16, bus as u8, device as u8, function as u8))
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
    args: [WrappedObject; 8],
    locals: [WrappedObject; 8],
    current_scope: AmlName,

    _method: Option<WrappedObject>,
}

#[derive(Debug)]
struct OpInFlight {
    op: Opcode,
    expected_arguments: usize,
    arguments: Vec<Argument>,
}

#[derive(Debug)]
enum Argument {
    Object(WrappedObject),
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
    VarPackage,
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
            args: core::array::from_fn(|_| Object::Uninitialized.wrap()),
            locals: core::array::from_fn(|_| Object::Uninitialized.wrap()),
            current_scope: AmlName::root(),
            _method: None,
        }
    }

    fn new_from_method(
        method: WrappedObject,
        args: Vec<WrappedObject>,
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
                if let Some(arg) = args.get(i) { arg.clone() } else { Object::Uninitialized.wrap() }
            });
            let context = MethodContext {
                current_block: block,
                block_stack: Vec::new(),
                in_flight: Vec::new(),
                args,
                locals: core::array::from_fn(|_| Object::Uninitialized.wrap()),
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
        if let Some(in_flight) = self.in_flight.last_mut()
            && in_flight.arguments.len() < in_flight.expected_arguments
        {
            in_flight.arguments.push(arg);
        }
    }

    fn start_in_flight_op(&mut self, op: OpInFlight) {
        trace!(
            "START OP: {:?}, args: {:?}, with {} more needed",
            op.op,
            op.arguments,
            op.expected_arguments - op.arguments.len()
        );
        self.in_flight.push(op);
    }

    fn retire_op(&mut self, op: OpInFlight) {
        trace!("RETIRE OP: {:?}, args: {:?}", op.op, op.arguments);
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

    ToBuffer,
    ToInteger,
    ToString,
    ToDecOrHexString,

    ReadBufferField,
    WriteBufferField,
    LogicalOp,
    DecodePrt,
    ParseResource,

    ResetEvent,
    SignalEvent,
    WaitEvent,
}

#[derive(Clone, PartialEq, Debug)]
#[non_exhaustive]
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

    InvalidOperationOnObject {
        op: Operation,
        typ: ObjectType,
    },
    IndexOutOfBounds,
    ObjectNotOfExpectedType {
        expected: ObjectType,
        got: ObjectType,
    },

    InvalidResourceDescriptor,
    UnexpectedResourceType,

    NoHandlerForRegionAccess(RegionSpace),
    MutexAcquireTimeout,

    PrtInvalidAddress,
    PrtInvalidPin,
    PrtInvalidGsi,
    PrtInvalidSource,
    PrtNoEntry,

    /// This is emitted to signal that the library does not support the requested behaviour. This
    /// should eventually never be emitted.
    LibUnimplemented,
}
