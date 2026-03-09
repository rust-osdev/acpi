use acpi::{aml::AmlError, Handle, Handler, PhysicalMapping};
use pci_types::PciAddress;
use std::sync::{atomic::AtomicUsize, Arc};

/// Commands that may be received by a [handler](Handler) which return a value from the handler.
///
/// Commands that do not return a value are represented by [`Skip`](AcpiCommands::Skip).
///
/// A [`Vec`] of these is used by [`ListedResponseHandler`]
// Some variants are unused for the time being, until more tests are written.
#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum AcpiCommands {
    /// A stand-in for all commands that don't return a value.
    Skip(),
    ReadU8(u8),
    ReadU16(u16),
    ReadU32(u32),
    ReadU64(u64),
    ReadIoU8(u8),
    ReadIoU16(u16),
    ReadIoU32(u32),
    ReadPciU8(u8),
    ReadPciU16(u16),
    ReadPciU32(u32),
    NanosSinceBoot(u64),
}

/// A basic Handler that returns an expected result from a provided sequence of commands.
///
/// If the command is unexpected, this handler will panic.
#[derive(Clone, Debug)]
pub struct ListedResponseHandler {
    commands: Vec<AcpiCommands>,
    next_command_idx: Arc<AtomicUsize>,
}

impl ListedResponseHandler {
    pub fn new(commands: Vec<AcpiCommands>) -> Self {
        Self { commands, next_command_idx: Arc::new(AtomicUsize::new(0)) }
    }

    fn get_next_command(&self) -> AcpiCommands {
        let next_command_idx = self.next_command_idx.load(std::sync::atomic::Ordering::Relaxed);
        let next_command = self.commands.get(next_command_idx);

        if next_command.is_none() {
            panic!("More commands attempted than expected");
        };

        self.next_command_idx.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        *next_command.unwrap()
    }
}

macro_rules! check_and_get_result {
    ($self:ident, $expected_cmd:ident) => {
        match $self.get_next_command() {
            AcpiCommands::$expected_cmd(x) => x,
            _ => panic!("Unexpected command"),
        }
    };
}

macro_rules! check_is_skipped {
    ($self:ident) => {
        match $self.get_next_command() {
            AcpiCommands::Skip() => (),
            _ => panic!("Unexpected command"),
        }
    };
}
impl Drop for ListedResponseHandler {
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

impl Handler for ListedResponseHandler {
    unsafe fn map_physical_region<T>(&self, _physical_address: usize, _size: usize) -> PhysicalMapping<Self, T> {
        // This isn't implemented in `aml_tester` either
        todo!()
    }

    fn unmap_physical_region<T>(_region: &PhysicalMapping<Self, T>) {}

    fn read_u8(&self, _address: usize) -> u8 {
        check_and_get_result!(self, ReadU8)
    }

    fn read_u16(&self, _address: usize) -> u16 {
        check_and_get_result!(self, ReadU16)
    }

    fn read_u32(&self, _address: usize) -> u32 {
        check_and_get_result!(self, ReadU32)
    }

    fn read_u64(&self, _address: usize) -> u64 {
        check_and_get_result!(self, ReadU64)
    }

    fn write_u8(&self, _address: usize, _value: u8) {
        check_is_skipped!(self)
    }

    fn write_u16(&self, _address: usize, _value: u16) {
        check_is_skipped!(self)
    }

    fn write_u32(&self, _address: usize, _value: u32) {
        check_is_skipped!(self)
    }

    fn write_u64(&self, _address: usize, _value: u64) {
        check_is_skipped!(self)
    }

    fn read_io_u8(&self, _port: u16) -> u8 {
        check_and_get_result!(self, ReadIoU8)
    }

    fn read_io_u16(&self, _port: u16) -> u16 {
        check_and_get_result!(self, ReadIoU16)
    }

    fn read_io_u32(&self, _port: u16) -> u32 {
        check_and_get_result!(self, ReadIoU32)
    }

    fn write_io_u8(&self, _port: u16, _value: u8) {
        check_is_skipped!(self)
    }

    fn write_io_u16(&self, _port: u16, _value: u16) {
        check_is_skipped!(self)
    }

    fn write_io_u32(&self, _port: u16, _value: u32) {
        check_is_skipped!(self)
    }

    fn read_pci_u8(&self, _address: PciAddress, _offset: u16) -> u8 {
        check_and_get_result!(self, ReadPciU8)
    }

    fn read_pci_u16(&self, _address: PciAddress, _offset: u16) -> u16 {
        check_and_get_result!(self, ReadPciU16)
    }

    fn read_pci_u32(&self, _address: PciAddress, _offset: u16) -> u32 {
        check_and_get_result!(self, ReadPciU32)
    }

    fn write_pci_u8(&self, _address: PciAddress, _offset: u16, _value: u8) {
        check_is_skipped!(self)
    }

    fn write_pci_u16(&self, _address: PciAddress, _offset: u16, _value: u16) {
        check_is_skipped!(self)
    }

    fn write_pci_u32(&self, _address: PciAddress, _offset: u16, _value: u32) {
        check_is_skipped!(self)
    }

    fn nanos_since_boot(&self) -> u64 {
        check_and_get_result!(self, NanosSinceBoot)
    }

    fn stall(&self, _microseconds: u64) {
        check_is_skipped!(self)
    }

    fn sleep(&self, _milliseconds: u64) {
        check_is_skipped!(self)
    }

    fn create_mutex(&self) -> Handle {
        check_is_skipped!(self);
        Handle(1)
    }

    fn acquire(&self, _mutex: Handle, _timeout: u16) -> Result<(), AmlError> {
        check_is_skipped!(self);
        Ok(())
    }

    fn release(&self, _mutex: Handle) {
        check_is_skipped!(self)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn handler_basic_functions() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::Skip()];

        let handler = ListedResponseHandler::new(test_commands);
        handler.read_io_u8(2);
        handler.write_io_u16(3, 4);
    }

    #[test]
    #[should_panic]
    fn handler_fails_for_wrong_command() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::ReadIoU8(3)];

        let handler = ListedResponseHandler::new(test_commands);
        handler.read_io_u8(3);
        // We shouldn't actually make it to this command, but it makes sure the handler doesn't panic for having too few
        // commands sent to it.
        handler.write_io_u16(3, 4);
    }

    #[test]
    #[should_panic]
    fn handler_fails_for_too_many_commands() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::Skip()];

        let handler = ListedResponseHandler::new(test_commands);
        handler.read_io_u8(2);
        handler.write_io_u16(3, 4);
        handler.read_io_u8(5);
    }

    #[test]
    #[should_panic]
    fn handler_fails_for_too_few_commands() {
        let test_commands = vec![AcpiCommands::ReadIoU8(2), AcpiCommands::Skip()];

        let handler = ListedResponseHandler::new(test_commands);
        handler.read_io_u8(2);
    }

    #[test]
    #[should_panic]
    fn check_handler_fails_gracefully_for_no_commands() {
        let test_commands: Vec<AcpiCommands> = vec![];
        let handler = ListedResponseHandler::new(test_commands);
        handler.read_io_u8(2);
    }
}
