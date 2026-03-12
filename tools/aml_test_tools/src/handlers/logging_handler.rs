//! A [`Handler`] that logs all calls, then forwards them to an inner handler.

use acpi::{Handle, Handler, PhysicalMapping, aml::object::Object};
use core::mem::ManuallyDrop;
use log::info;
use pci_types::PciAddress;

/// A [`Handler`] wrapper that logs every call to `info!` and then forwards it to an inner handler.
///
/// Use this Handler to have a logging style consistent with the [`acpi`] crate's tests.
#[derive(Clone)]
pub struct LoggingHandler<H> {
    next_handler: H,
}

impl<H> LoggingHandler<H>
where
    H: Handler,
{
    pub fn new(next_handler: H) -> Self {
        Self { next_handler }
    }
}

impl<H> Handler for LoggingHandler<H>
where
    H: Handler,
{
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T> {
        info!("map_physical_region(physical_address={:#x}, size={:#x})", physical_address, size);

        let inner_mapping = unsafe { self.next_handler.map_physical_region::<T>(physical_address, size) };
        let inner_mapping = ManuallyDrop::new(inner_mapping);

        PhysicalMapping {
            physical_start: inner_mapping.physical_start,
            virtual_start: inner_mapping.virtual_start,
            region_length: inner_mapping.region_length,
            mapped_length: inner_mapping.mapped_length,
            handler: self.clone(),
        }
    }

    fn unmap_physical_region<T>(region: &PhysicalMapping<Self, T>) {
        info!("unmap_physical_region(physical_start={:#x})", region.physical_start);

        // Convert `PhysicalMapping<LoggingHandler<H>, T>` -> `PhysicalMapping<H, T>` and delegate.
        // Prevent the temporary mapping from being dropped (and thus calling `H::unmap_physical_region` twice).
        let inner_region = ManuallyDrop::new(PhysicalMapping::<H, T> {
            physical_start: region.physical_start,
            virtual_start: region.virtual_start,
            region_length: region.region_length,
            mapped_length: region.mapped_length,
            handler: region.handler.next_handler.clone(),
        });

        H::unmap_physical_region(&inner_region);
    }

    fn read_u8(&self, address: usize) -> u8 {
        let value = self.next_handler.read_u8(address);
        info!("read_u8(address={:#x}) -> {:#x}", address, value);
        value
    }

    fn read_u16(&self, address: usize) -> u16 {
        let value = self.next_handler.read_u16(address);
        info!("read_u16(address={:#x}) -> {:#x}", address, value);
        value
    }

    fn read_u32(&self, address: usize) -> u32 {
        let value = self.next_handler.read_u32(address);
        info!("read_u32(address={:#x}) -> {:#x}", address, value);
        value
    }

    fn read_u64(&self, address: usize) -> u64 {
        let value = self.next_handler.read_u64(address);
        info!("read_u64(address={:#x}) -> {:#x}", address, value);
        value
    }

    fn write_u8(&self, address: usize, value: u8) {
        info!("write_u8(address={:#x}, value={:#x})", address, value);
        self.next_handler.write_u8(address, value);
    }

    fn write_u16(&self, address: usize, value: u16) {
        info!("write_u16(address={:#x}, value={:#x})", address, value);
        self.next_handler.write_u16(address, value);
    }

    fn write_u32(&self, address: usize, value: u32) {
        info!("write_u32(address={:#x}, value={:#x})", address, value);
        self.next_handler.write_u32(address, value);
    }

    fn write_u64(&self, address: usize, value: u64) {
        info!("write_u64(address={:#x}, value={:#x})", address, value);
        self.next_handler.write_u64(address, value);
    }

    fn read_io_u8(&self, port: u16) -> u8 {
        let value = self.next_handler.read_io_u8(port);
        info!("read_io_u8(port={:#x}) -> {:#x}", port, value);
        value
    }

    fn read_io_u16(&self, port: u16) -> u16 {
        let value = self.next_handler.read_io_u16(port);
        info!("read_io_u16(port={:#x}) -> {:#x}", port, value);
        value
    }

    fn read_io_u32(&self, port: u16) -> u32 {
        let value = self.next_handler.read_io_u32(port);
        info!("read_io_u32(port={:#x}) -> {:#x}", port, value);
        value
    }

    fn write_io_u8(&self, port: u16, value: u8) {
        info!("write_io_u8(port={:#x}, value={:#x})", port, value);
        self.next_handler.write_io_u8(port, value);
    }

    fn write_io_u16(&self, port: u16, value: u16) {
        info!("write_io_u16(port={:#x}, value={:#x})", port, value);
        self.next_handler.write_io_u16(port, value);
    }

    fn write_io_u32(&self, port: u16, value: u32) {
        info!("write_io_u32(port={:#x}, value={:#x})", port, value);
        self.next_handler.write_io_u32(port, value);
    }

    fn read_pci_u8(&self, address: PciAddress, offset: u16) -> u8 {
        let value = self.next_handler.read_pci_u8(address, offset);
        info!("read_pci_u8(address={:?}, offset={:#x}) -> {:#x}", address, offset, value);
        value
    }

    fn read_pci_u16(&self, address: PciAddress, offset: u16) -> u16 {
        let value = self.next_handler.read_pci_u16(address, offset);
        info!("read_pci_u16(address={:?}, offset={:#x}) -> {:#x}", address, offset, value);
        value
    }

    fn read_pci_u32(&self, address: PciAddress, offset: u16) -> u32 {
        let value = self.next_handler.read_pci_u32(address, offset);
        info!("read_pci_u32(address={:?}, offset={:#x}) -> {:#x}", address, offset, value);
        value
    }

    fn write_pci_u8(&self, address: PciAddress, offset: u16, value: u8) {
        info!("write_pci_u8(address={:?}, offset={:#x}, value={:#x})", address, offset, value);
        self.next_handler.write_pci_u8(address, offset, value);
    }

    fn write_pci_u16(&self, address: PciAddress, offset: u16, value: u16) {
        info!("write_pci_u16(address={:?}, offset={:#x}, value={:#x})", address, offset, value);
        self.next_handler.write_pci_u16(address, offset, value);
    }

    fn write_pci_u32(&self, address: PciAddress, offset: u16, value: u32) {
        info!("write_pci_u32(address={:?}, offset={:#x}, value={:#x})", address, offset, value);
        self.next_handler.write_pci_u32(address, offset, value);
    }

    fn nanos_since_boot(&self) -> u64 {
        info!("nanos_since_boot()");
        self.next_handler.nanos_since_boot()
    }

    fn stall(&self, microseconds: u64) {
        info!("stall(microseconds={})", microseconds);
        self.next_handler.stall(microseconds);
    }

    fn sleep(&self, milliseconds: u64) {
        info!("sleep(milliseconds={})", milliseconds);
        self.next_handler.sleep(milliseconds);
    }

    fn create_mutex(&self) -> Handle {
        info!("create_mutex()");
        self.next_handler.create_mutex()
    }

    fn acquire(&self, mutex: Handle, timeout: u16) -> Result<(), acpi::aml::AmlError> {
        info!("acquire(mutex={:?}, timeout={})", mutex, timeout);
        self.next_handler.acquire(mutex, timeout)
    }

    fn release(&self, mutex: Handle) {
        info!("release(mutex={:?})", mutex);
        self.next_handler.release(mutex);
    }

    fn handle_debug(&self, object: &Object) {
        info!("Debug store: {}", object);
        self.next_handler.handle_debug(object);
    }
}
