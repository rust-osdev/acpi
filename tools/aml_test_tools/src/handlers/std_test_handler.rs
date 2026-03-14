//! Rather than defining a [`Handler`], this module defines useful functions to streamline the most-
//! used case of a [`CheckCommandHandler`] wrapping a [`ListedResponseHandler`].

use pci_types::PciAddress;
use crate::handlers::{
    check_cmd_handler::{AcpiCommands as Check, CheckCommandHandler},
    listed_response_handler::{AcpiCommands as Response, ListedResponseHandler},
};
use acpi::{Handle, Handler};

/// Simplifies the construction of a standard test [`Handler`].
///
/// By combining the commands and responses into a single tuple, it hopefully makes test files
/// easier to read and write.
pub type Command = (Check, Response);

/// Construct a "standard handler".
///
/// This is the handler that I expect to be used most often in tests. (A [`CheckCommandHandler`]
/// wrapping a [`ListedResponseHandler`]).
pub fn construct_std_handler(commands: Vec<Command>) -> impl Handler {
    let (c, r): (Vec<Check>, Vec<Response>) = commands.into_iter().unzip();

    CheckCommandHandler::new(c, ListedResponseHandler::new(r))
}

/// A simple helper to generate a [`Command`] for [`Handler::map_physical_region`].
pub const fn map_physical_region(physical_address: usize, size: usize) -> Command {
    (Check::MapPhysicalRegion(physical_address, size), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::unmap_physical_region`].
pub const fn unmap_physical_region(region: usize) -> Command {
    (Check::UnmapPhysicalRegion(region), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::read_u8`].
pub const fn read_u8(address: usize, response: u8) -> Command {
    (Check::ReadU8(address), Response::ReadU8(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_u16`].
pub const fn read_u16(address: usize, response: u16) -> Command {
    (Check::ReadU16(address), Response::ReadU16(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_u32`].
pub const fn read_u32(address: usize, response: u32) -> Command {
    (Check::ReadU32(address), Response::ReadU32(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_u64`].
pub const fn read_u64(address: usize, response: u64) -> Command {
    (Check::ReadU64(address), Response::ReadU64(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::write_u8`].
pub const fn write_u8(address: usize, value: u8) -> Command {
    (Check::WriteU8(address, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_u16`].
pub const fn write_u16(address: usize, value: u16) -> Command {
    (Check::WriteU16(address, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_u32`].
pub const fn write_u32(address: usize, value: u32) -> Command {
    (Check::WriteU32(address, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_u64`].
pub const fn write_u64(address: usize, value: u64) -> Command {
    (Check::WriteU64(address, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::read_io_u8`].
pub const fn read_io_u8(port: u16, response: u8) -> Command {
    (Check::ReadIoU8(port), Response::ReadIoU8(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_io_u16`].
pub const fn read_io_u16(port: u16, response: u16) -> Command {
    (Check::ReadIoU16(port), Response::ReadIoU16(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_io_u32`].
pub const fn read_io_u32(port: u16, response: u32) -> Command {
    (Check::ReadIoU32(port), Response::ReadIoU32(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::write_io_u8`].
pub const fn write_io_u8(port: u16, value: u8) -> Command {
    (Check::WriteIoU8(port, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_io_u16`].
pub const fn write_io_u16(port: u16, value: u16) -> Command {
    (Check::WriteIoU16(port, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_io_u32`].
pub const fn write_io_u32(port: u16, value: u32) -> Command {
    (Check::WriteIoU32(port, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::read_pci_u8`].
pub const fn read_pci_u8(address: PciAddress, offset: u16, response: u8) -> Command {
    (Check::ReadPciU8(address, offset), Response::ReadPciU8(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_pci_u16`].
pub const fn read_pci_u16(address: PciAddress, offset: u16, response: u16) -> Command {
    (Check::ReadPciU16(address, offset), Response::ReadPciU16(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::read_pci_u32`].
pub const fn read_pci_u32(address: PciAddress, offset: u16, response: u32) -> Command {
    (Check::ReadPciU32(address, offset), Response::ReadPciU32(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::write_pci_u8`].
pub const fn write_pci_u8(address: PciAddress, offset: u16, value: u8) -> Command {
    (Check::WritePciU8(address, offset, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_pci_u16`].
pub const fn write_pci_u16(address: PciAddress, offset: u16, value: u16) -> Command {
    (Check::WritePciU16(address, offset, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::write_pci_u32`].
pub const fn write_pci_u32(address: PciAddress, offset: u16, value: u32) -> Command {
    (Check::WritePciU32(address, offset, value), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::nanos_since_boot`].
pub const fn nanos_since_boot(response: u64) -> Command {
    (Check::NanosSinceBoot, Response::NanosSinceBoot(response))
}

/// A simple helper to generate a [`Command`] for [`Handler::stall`].
pub const fn stall(microseconds: u64) -> Command {
    (Check::Stall(microseconds), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::sleep`].
pub const fn sleep(milliseconds: u64) -> Command {
    (Check::Sleep(milliseconds), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::create_mutex`].
pub const fn create_mutex() -> Command {
    (Check::CreateMutex, Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::acquire`].
pub const fn acquire(mutex: Handle, timeout: u16) -> Command {
    (Check::Acquire(mutex, timeout), Response::Skip())
}

/// A simple helper to generate a [`Command`] for [`Handler::release`].
pub const fn release(mutex: Handle) -> Command {
    (Check::Release(mutex), Response::Skip())
}