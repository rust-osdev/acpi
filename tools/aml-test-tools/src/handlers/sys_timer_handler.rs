//! A wrapper around another [`Handler`] that checks for the correct sequence of commands in a test.

use acpi::{Handle, Handler, RawPhysicalMapping, aml::AmlError};
use pci_types::PciAddress;
use std::{
    thread::sleep,
    time::{Duration, Instant},
};

/// A wrapper around another [`Handler`] that enables the handler to use the system's timing
/// capabilities.
///
/// All commands are forwarded except [`sleep`], [`stall`] and [`nanos_since_boot`].
#[derive(Clone, Debug)]
pub struct SystemTimerHandler<H>
where
    H: Handler + Clone,
{
    next_handler: H,
    start_time: Instant,
    scale_factor: u64,
}

impl<H> SystemTimerHandler<H>
where
    H: Handler + Clone,
{
    /// Construct a new `SystemTimerHandler`
    ///
    /// * `next_handler` is the handler to forward non-timing method calls to.
    /// * `scale_factor` effectively accelerates time by the given integer multiple. For example, a
    ///   10-second wait with a scale factor of 5 would lead to a 2-second wait. Note: Large
    ///   multiples combined with small waits may lead to reduced accuracy or zero-length waits.
    pub fn new(next_handler: H, scale_factor: u64) -> Self {
        Self { next_handler, start_time: Instant::now(), scale_factor }
    }
}

impl<H> Handler for SystemTimerHandler<H>
where
    H: Handler + Clone,
{
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> RawPhysicalMapping<T> {
        unsafe { self.next_handler.map_physical_region::<T>(physical_address, size) }
    }

    unsafe fn unmap_physical_region<T>(&self, region: RawPhysicalMapping<T>) {
        unsafe {
            self.next_handler.unmap_physical_region(region);
        }
    }

    fn read_u8(&self, address: usize) -> u8 {
        self.next_handler.read_u8(address)
    }

    fn read_u16(&self, address: usize) -> u16 {
        self.next_handler.read_u16(address)
    }

    fn read_u32(&self, address: usize) -> u32 {
        self.next_handler.read_u32(address)
    }

    fn read_u64(&self, address: usize) -> u64 {
        self.next_handler.read_u64(address)
    }

    fn write_u8(&self, address: usize, value: u8) {
        self.next_handler.write_u8(address, value);
    }

    fn write_u16(&self, address: usize, value: u16) {
        self.next_handler.write_u16(address, value);
    }

    fn write_u32(&self, address: usize, value: u32) {
        self.next_handler.write_u32(address, value);
    }

    fn write_u64(&self, address: usize, value: u64) {
        self.next_handler.write_u64(address, value);
    }

    fn read_io_u8(&self, port: u16) -> u8 {
        self.next_handler.read_io_u8(port)
    }

    fn read_io_u16(&self, port: u16) -> u16 {
        self.next_handler.read_io_u16(port)
    }

    fn read_io_u32(&self, port: u16) -> u32 {
        self.next_handler.read_io_u32(port)
    }

    fn write_io_u8(&self, port: u16, value: u8) {
        self.next_handler.write_io_u8(port, value);
    }

    fn write_io_u16(&self, port: u16, value: u16) {
        self.next_handler.write_io_u16(port, value);
    }

    fn write_io_u32(&self, port: u16, value: u32) {
        self.next_handler.write_io_u32(port, value);
    }

    fn read_pci_u8(&self, address: PciAddress, offset: u16) -> u8 {
        self.next_handler.read_pci_u8(address, offset)
    }

    fn read_pci_u16(&self, address: PciAddress, offset: u16) -> u16 {
        self.next_handler.read_pci_u16(address, offset)
    }

    fn read_pci_u32(&self, address: PciAddress, offset: u16) -> u32 {
        self.next_handler.read_pci_u32(address, offset)
    }

    fn write_pci_u8(&self, address: PciAddress, offset: u16, value: u8) {
        self.next_handler.write_pci_u8(address, offset, value);
    }

    fn write_pci_u16(&self, address: PciAddress, offset: u16, value: u16) {
        self.next_handler.write_pci_u16(address, offset, value);
    }

    fn write_pci_u32(&self, address: PciAddress, offset: u16, value: u32) {
        self.next_handler.write_pci_u32(address, offset, value);
    }

    fn nanos_since_boot(&self) -> u64 {
        ((Instant::now() - self.start_time).as_nanos() as u64) * self.scale_factor
    }

    fn stall(&self, microseconds: u64) {
        // There's no `std` equivalent to stall, and sleep is probably OK for a test environment.
        sleep(Duration::from_micros(microseconds / self.scale_factor));
    }

    fn sleep(&self, milliseconds: u64) {
        sleep(Duration::from_millis(milliseconds / self.scale_factor));
    }

    fn create_mutex(&self) -> Handle {
        self.next_handler.create_mutex()
    }

    fn acquire(&self, mutex: Handle, timeout: u16) -> Result<(), AmlError> {
        self.next_handler.acquire(mutex, timeout)
    }

    fn release(&self, mutex: Handle) {
        self.next_handler.release(mutex);
    }
}
