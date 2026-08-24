//! Test the ACPI Global Lock mechanism.

// These tests make use of `serial_test` as the test infra creates one firmware lock and then uses
// it for all tests - which causes these tests of the locking mechanism to conflict with each other
// when run in parallel.

use acpi::{Handle, Handler, PhysicalMapping, aml::AmlError};
use aml_test_tools::new_interpreter;
use lock_api::RawReentrantMutex;
use parking_lot::{RawMutex, RawThreadId};
use pci_types::PciAddress;
use serial_test::serial;
use std::{
    sync::{
        Arc,
        Barrier,
        atomic::{AtomicBool, Ordering},
    },
    thread,
    time::Duration,
};

mod test_infra;

#[test]
#[serial]
fn uncontended_acquire_release() {
    let interpreter = new_interpreter(LockHandler::new());

    interpreter.acquire_global_lock(0).expect("Failed to acquire lock");
    interpreter.release_global_lock().expect("Failed to release lock");
}

#[test]
#[serial]
fn single_thread_acquire_release() {
    let interpreter = new_interpreter(LockHandler::new());

    interpreter.acquire_global_lock(0).expect("Failed to acquire lock (1)");
    interpreter.acquire_global_lock(0).expect("Failed to acquire lock (2)");
    interpreter.release_global_lock().expect("Failed to release lock (2)");
    interpreter.release_global_lock().expect("Failed to release lock (1)");
}

#[test]
#[serial]
fn multi_thread_acquire_release() {
    // Steps in order (threads are labeled A & B):
    // 1: A acquires lock
    // 2: B attempts to acquire lock, fails
    // 3: A waits for B to complete #2.
    // 4: A releases lock
    // 5: B waits for A to release lock and acquires lock successfully.

    let handler = LockHandler::new();
    let interpreter = new_interpreter(handler.clone());

    let barrier = Barrier::new(2);

    let success = AtomicBool::new(false);

    // 1: A acquires the lock
    interpreter.acquire_global_lock(0).expect("1: A failed to acquire lock");
    thread::scope(|s| {
        s.spawn(|| {
            let interpreter = new_interpreter(handler);

            // 2: B attempts to acquire lock, fails.
            let e = interpreter.acquire_global_lock(0).expect_err("2: B managed to acquire lock!");
            assert!(matches!(e, AmlError::MutexAcquireTimeout));

            // 3: A waits for B to complete #2
            barrier.wait();
            // 4: Occurs in thread A (the main thread)

            // 5: B attempts to acquire lock, waiting as needed.
            interpreter.acquire_global_lock(10000).expect("5: B failed to acquire lock");

            success.store(true, Ordering::Relaxed);

            interpreter.release_global_lock().expect("B: Failed to release global lock");
        });

        // 3: A waits for B to complete #2
        barrier.wait();

        // 4: A releases the global lock
        interpreter.release_global_lock().expect("Failed to release global lock");
    });

    // Check that B acquired lock successfully
    assert!(success.load(Ordering::Relaxed));
}

#[test]
#[serial]
fn uacpi_global_lock_test() {
    // This test is adapted from the uACPI test file `tests/test-cases/global-lock.asl`
    const AML: &str = r#"
DefinitionBlock ("", "DSDT", 2, "uTEST", "TESTTABL", 0xF0F0F0F0)
{
    Method (CHEK, 1, Serialized, 15)
    {
        If (Arg0 != 0) {
            Debug = "Failed to acquire the global lock!"
            Return (1)
        }

        Return (0)
    }

    Method (MAIN, 0, Serialized)
    {
        Local0 = 0

        Local0 += CHEK(Acquire (_GL, 0xFFFF))
        Local0 += CHEK(Acquire (_GL, 0xFFFF))
        Local0 += CHEK(Acquire (_GL, 0xFFFF))
        Local0 += CHEK(Acquire (_GL, 0xFFFF))

        Release(_GL)
        Release(_GL)
        Release(_GL)
        Release(_GL)

        Return (Local0)
    }
}
"#;
    let handler = LockHandler::new();
    test_infra::run_aml_test(AML, handler);
}

#[derive(Clone)]
struct LockHandler {
    // Use RawReentrantMutex for two reasons:
    // 1. We need a raw mutex because we don't want to hold onto the RAII MutexGuard - we'll control
    //    locking and unlocking manually
    // 2. We need to handle reentrancy, so RawMutex by itself is insufficient.
    mutex: Arc<RawReentrantMutex<RawMutex, RawThreadId>>,
}

impl LockHandler {
    const MUTEX_HANDLE: Handle = Handle(1);

    pub fn new() -> Self {
        Self { mutex: Arc::new(RawReentrantMutex::INIT) }
    }
}

impl Handler for LockHandler {
    fn create_mutex(&self) -> Handle {
        // Don't add complexity for a simple test handler - we only need one mutex.
        Self::MUTEX_HANDLE
    }

    fn acquire(&self, mutex: Handle, timeout: u16) -> Result<(), AmlError> {
        assert_eq!(mutex, Self::MUTEX_HANDLE);

        match timeout {
            0 => self.mutex.try_lock(),
            0xffff => {
                self.mutex.lock();
                true
            }
            _ => self.mutex.try_lock_for(Duration::from_millis(timeout as u64)),
        }
        .ok_or(AmlError::MutexAcquireTimeout)
    }

    fn release(&self, mutex: Handle) {
        assert_eq!(mutex, Self::MUTEX_HANDLE);
        assert!(self.mutex.is_owned_by_current_thread());

        // Safety: We've just checked that it's this thread that owns the mutex, so it's safe to
        // unlock.
        unsafe {
            self.mutex.unlock();
        }
    }

    unsafe fn map_physical_region<T>(&self, _physical_address: usize, _size: usize) -> PhysicalMapping<Self, T> {
        unimplemented!()
    }

    fn unmap_physical_region<T>(_region: &PhysicalMapping<Self, T>) {
        // Do nothing
    }

    fn read_u8(&self, _address: usize) -> u8 {
        unimplemented!()
    }

    fn read_u16(&self, _address: usize) -> u16 {
        unimplemented!()
    }

    fn read_u32(&self, _address: usize) -> u32 {
        unimplemented!()
    }

    fn read_u64(&self, _address: usize) -> u64 {
        unimplemented!()
    }

    fn write_u8(&self, _address: usize, _value: u8) {
        unimplemented!()
    }

    fn write_u16(&self, _address: usize, _value: u16) {
        unimplemented!()
    }

    fn write_u32(&self, _address: usize, _value: u32) {
        unimplemented!()
    }

    fn write_u64(&self, _address: usize, _value: u64) {
        unimplemented!()
    }

    fn read_io_u8(&self, _port: u16) -> u8 {
        unimplemented!()
    }

    fn read_io_u16(&self, _port: u16) -> u16 {
        unimplemented!()
    }

    fn read_io_u32(&self, _port: u16) -> u32 {
        unimplemented!()
    }

    fn write_io_u8(&self, _port: u16, _value: u8) {
        unimplemented!()
    }

    fn write_io_u16(&self, _port: u16, _value: u16) {
        unimplemented!()
    }

    fn write_io_u32(&self, _port: u16, _value: u32) {
        unimplemented!()
    }

    fn read_pci_u8(&self, _address: PciAddress, _offset: u16) -> u8 {
        unimplemented!()
    }

    fn read_pci_u16(&self, _address: PciAddress, _offset: u16) -> u16 {
        unimplemented!()
    }

    fn read_pci_u32(&self, _address: PciAddress, _offset: u16) -> u32 {
        unimplemented!()
    }

    fn write_pci_u8(&self, _address: PciAddress, _offset: u16, _value: u8) {
        unimplemented!()
    }

    fn write_pci_u16(&self, _address: PciAddress, _offset: u16, _value: u16) {
        unimplemented!()
    }

    fn write_pci_u32(&self, _address: PciAddress, _offset: u16, _value: u32) {
        unimplemented!()
    }

    fn nanos_since_boot(&self) -> u64 {
        unimplemented!()
    }

    fn stall(&self, _microseconds: u64) {
        unimplemented!()
    }

    fn sleep(&self, _milliseconds: u64) {
        unimplemented!()
    }
}
