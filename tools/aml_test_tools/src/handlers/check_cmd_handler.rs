//! A wrapper around another [`Handler`] that checks for the correct sequence of commands in a test.

use acpi::{Handle, Handler, PhysicalMapping, aml::AmlError};
use pci_types::PciAddress;
use std::{
    mem::ManuallyDrop,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering::Relaxed},
    },
};

/// The commands that may be received by an ACPI [`handler`](Handler).
///
/// They are written as an enum to allow a list of commands to be stored in a [`Vec`] or similar.
/// A `Vec` of these is used by [`CheckCommandHandler`]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AcpiCommands {
    MapPhysicalRegion(usize, usize),
    UnmapPhysicalRegion(usize),
    ReadU8(usize),
    ReadU16(usize),
    ReadU32(usize),
    ReadU64(usize),
    WriteU8(usize, u8),
    WriteU16(usize, u16),
    WriteU32(usize, u32),
    WriteU64(usize, u64),
    ReadIoU8(u16),
    ReadIoU16(u16),
    ReadIoU32(u16),
    WriteIoU8(u16, u8),
    WriteIoU16(u16, u16),
    WriteIoU32(u16, u32),
    ReadPciU8(PciAddress, u16),
    ReadPciU16(PciAddress, u16),
    ReadPciU32(PciAddress, u16),
    WritePciU8(PciAddress, u16, u8),
    WritePciU16(PciAddress, u16, u16),
    WritePciU32(PciAddress, u16, u32),
    NanosSinceBoot,
    Stall(u64),
    Sleep(u64),
    CreateMutex,
    Acquire(Handle, u16),
    Release(Handle),
}

/// A wrapper around another [`Handler`] that checks the correct sequence of commands are being
/// generated, and that they have the expected parameters.
#[derive(Clone, Debug)]
pub struct CheckCommandHandler<H>
where
    H: Handler + Clone,
{
    commands: Vec<AcpiCommands>,
    next_command_idx: Arc<AtomicUsize>,
    next_handler: H,
}

impl<H> CheckCommandHandler<H>
where
    H: Handler + Clone,
{
    pub fn new(commands: Vec<AcpiCommands>, next_handler: H) -> Self {
        Self { commands, next_command_idx: Arc::new(AtomicUsize::new(0)), next_handler }
    }

    fn check_command(&self, command: AcpiCommands) {
        let next_command_idx = self.next_command_idx.fetch_add(1, Relaxed);
        let next_command = self.commands.get(next_command_idx);

        if next_command.is_none() {
            panic!("More commands attempted than expected");
        };

        assert_eq!(*next_command.unwrap(), command);
    }
}

impl<H> Drop for CheckCommandHandler<H>
where
    H: Handler + Clone,
{
    fn drop(&mut self) {
        // Don't do this if the test has already failed, to avoid a double-panic.
        if !std::thread::panicking() {
            assert_eq!(
                self.next_command_idx.load(std::sync::atomic::Ordering::Relaxed),
                self.commands.len(),
                "Not all commands were executed"
            );
        }
    }
}

impl<H> Handler for CheckCommandHandler<H>
where
    H: Handler + Clone,
{
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T> {
        self.check_command(AcpiCommands::MapPhysicalRegion(physical_address, size));

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
        // This function can be called during a panic, and it's pretty unlikely this command will
        // be in the expected commands list...
        //
        // Also stop checking if we're at or past the end of the command list. This stops any
        // confusion about whether we're in Drop or not.
        if !std::thread::panicking()
            && region.handler.commands.len() > region.handler.next_command_idx.load(Relaxed)
        {
            region.handler.check_command(AcpiCommands::UnmapPhysicalRegion(region.physical_start));
        }

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
        self.check_command(AcpiCommands::ReadU8(address));
        self.next_handler.read_u8(address)
    }

    fn read_u16(&self, address: usize) -> u16 {
        self.check_command(AcpiCommands::ReadU16(address));
        self.next_handler.read_u16(address)
    }

    fn read_u32(&self, address: usize) -> u32 {
        self.check_command(AcpiCommands::ReadU32(address));
        self.next_handler.read_u32(address)
    }

    fn read_u64(&self, address: usize) -> u64 {
        self.check_command(AcpiCommands::ReadU64(address));
        self.next_handler.read_u64(address)
    }

    fn write_u8(&self, address: usize, value: u8) {
        self.check_command(AcpiCommands::WriteU8(address, value));
        self.next_handler.write_u8(address, value);
    }

    fn write_u16(&self, address: usize, value: u16) {
        self.check_command(AcpiCommands::WriteU16(address, value));
        self.next_handler.write_u16(address, value);
    }

    fn write_u32(&self, address: usize, value: u32) {
        self.check_command(AcpiCommands::WriteU32(address, value));
        self.next_handler.write_u32(address, value);
    }

    fn write_u64(&self, address: usize, value: u64) {
        self.check_command(AcpiCommands::WriteU64(address, value));
        self.next_handler.write_u64(address, value);
    }

    fn read_io_u8(&self, port: u16) -> u8 {
        self.check_command(AcpiCommands::ReadIoU8(port));
        self.next_handler.read_io_u8(port)
    }

    fn read_io_u16(&self, port: u16) -> u16 {
        self.check_command(AcpiCommands::ReadIoU16(port));
        self.next_handler.read_io_u16(port)
    }

    fn read_io_u32(&self, port: u16) -> u32 {
        self.check_command(AcpiCommands::ReadIoU32(port));
        self.next_handler.read_io_u32(port)
    }

    fn write_io_u8(&self, port: u16, value: u8) {
        self.check_command(AcpiCommands::WriteIoU8(port, value));
        self.next_handler.write_io_u8(port, value);
    }

    fn write_io_u16(&self, port: u16, value: u16) {
        self.check_command(AcpiCommands::WriteIoU16(port, value));
        self.next_handler.write_io_u16(port, value);
    }

    fn write_io_u32(&self, port: u16, value: u32) {
        self.check_command(AcpiCommands::WriteIoU32(port, value));
        self.next_handler.write_io_u32(port, value);
    }

    fn read_pci_u8(&self, address: PciAddress, offset: u16) -> u8 {
        self.check_command(AcpiCommands::ReadPciU8(address, offset));
        self.next_handler.read_pci_u8(address, offset)
    }

    fn read_pci_u16(&self, address: PciAddress, offset: u16) -> u16 {
        self.check_command(AcpiCommands::ReadPciU16(address, offset));
        self.next_handler.read_pci_u16(address, offset)
    }

    fn read_pci_u32(&self, address: PciAddress, offset: u16) -> u32 {
        self.check_command(AcpiCommands::ReadPciU32(address, offset));
        self.next_handler.read_pci_u32(address, offset)
    }

    fn write_pci_u8(&self, address: PciAddress, offset: u16, value: u8) {
        self.check_command(AcpiCommands::WritePciU8(address, offset, value));
        self.next_handler.write_pci_u8(address, offset, value);
    }

    fn write_pci_u16(&self, address: PciAddress, offset: u16, value: u16) {
        self.check_command(AcpiCommands::WritePciU16(address, offset, value));
        self.next_handler.write_pci_u16(address, offset, value);
    }

    fn write_pci_u32(&self, address: PciAddress, offset: u16, value: u32) {
        self.check_command(AcpiCommands::WritePciU32(address, offset, value));
        self.next_handler.write_pci_u32(address, offset, value);
    }

    fn nanos_since_boot(&self) -> u64 {
        self.check_command(AcpiCommands::NanosSinceBoot);
        self.next_handler.nanos_since_boot()
    }

    fn stall(&self, microseconds: u64) {
        self.check_command(AcpiCommands::Stall(microseconds));
        self.next_handler.stall(microseconds);
    }

    fn sleep(&self, milliseconds: u64) {
        self.check_command(AcpiCommands::Sleep(milliseconds));
        self.next_handler.sleep(milliseconds);
    }

    fn create_mutex(&self) -> Handle {
        self.check_command(AcpiCommands::CreateMutex);
        self.next_handler.create_mutex()
    }

    fn acquire(&self, mutex: Handle, timeout: u16) -> Result<(), AmlError> {
        self.check_command(AcpiCommands::Acquire(mutex, timeout));
        self.next_handler.acquire(mutex, timeout)
    }

    fn release(&self, mutex: Handle) {
        self.check_command(AcpiCommands::Release(mutex));
        self.next_handler.release(mutex);
    }
}

#[cfg(test)]
mod test {
    use crate::handlers::null_handler::NullHandler;
    use super::*;

    #[test]
    fn handler_basic_functions() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::WriteIoU16(3, 4)];

        let handler = CheckCommandHandler::new(test_commands, NullHandler {});
        handler.read_io_u8(2);
        handler.write_io_u16(3, 4);
    }

    #[test]
    #[should_panic]
    fn handler_fails_for_wrong_command() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::WriteIoU16(3, 4)];

        let handler = CheckCommandHandler::new(test_commands, NullHandler {});
        handler.read_io_u8(3);
        // We shouldn't actually make it to this command, but it makes sure the handler doesn't panic for having too few
        // commands sent to it.
        handler.write_io_u16(3, 4);
    }

    #[test]
    #[should_panic]
    fn handler_fails_for_too_many_commands() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::WriteIoU16(3, 4)];

        let handler = CheckCommandHandler::new(test_commands, NullHandler {});
        handler.read_io_u8(2);
        handler.write_io_u16(3, 4);
        handler.read_io_u8(5);
    }

    #[test]
    #[should_panic]
    fn handler_fails_for_too_few_commands() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::WriteIoU16(3, 4)];

        let handler = CheckCommandHandler::new(test_commands, NullHandler {});
        handler.read_io_u8(2);
    }

    #[test]
    #[should_panic]
    fn check_handler_fails_gracefully_for_no_commands() {
        let test_commands: Vec<AcpiCommands> = vec![];
        let handler = CheckCommandHandler::new(test_commands, NullHandler {});
        handler.read_io_u8(2);
    }
}