use acpi::{Handle, Handler, PhysicalMapping, aml::AmlError};
use pci_types::PciAddress;

#[derive(Clone)]
pub struct NullHandler;

impl Handler for NullHandler {
    unsafe fn map_physical_region<T>(&self, _physical_address: usize, _size: usize) -> PhysicalMapping<Self, T> {
        // This isn't implemented in `aml_tester` either
        todo!()
    }

    fn unmap_physical_region<T>(_region: &PhysicalMapping<Self, T>) {}

    fn read_u8(&self, _address: usize) -> u8 {
        0
    }

    fn read_u16(&self, _address: usize) -> u16 {
        0
    }

    fn read_u32(&self, _address: usize) -> u32 {
        0
    }

    fn read_u64(&self, _address: usize) -> u64 {
        0
    }

    fn write_u8(&self, _address: usize, _value: u8) {}

    fn write_u16(&self, _address: usize, _value: u16) {}

    fn write_u32(&self, _address: usize, _value: u32) {}

    fn write_u64(&self, _address: usize, _value: u64) {}

    fn read_io_u8(&self, _port: u16) -> u8 {
        0
    }

    fn read_io_u16(&self, _port: u16) -> u16 {
        0
    }

    fn read_io_u32(&self, _port: u16) -> u32 {
        0
    }

    fn write_io_u8(&self, _port: u16, _value: u8) {}

    fn write_io_u16(&self, _port: u16, _value: u16) {}

    fn write_io_u32(&self, _port: u16, _value: u32) {}

    fn read_pci_u8(&self, _address: PciAddress, _offset: u16) -> u8 {
        0
    }

    fn read_pci_u16(&self, _address: PciAddress, _offset: u16) -> u16 {
        0
    }

    fn read_pci_u32(&self, _address: PciAddress, _offset: u16) -> u32 {
        0
    }

    fn write_pci_u8(&self, _address: PciAddress, _offset: u16, _value: u8) {}

    fn write_pci_u16(&self, _address: PciAddress, _offset: u16, _value: u16) {}

    fn write_pci_u32(&self, _address: PciAddress, _offset: u16, _value: u32) {}

    fn nanos_since_boot(&self) -> u64 {
        1000
    }

    fn stall(&self, _microseconds: u64) {}

    fn sleep(&self, _milliseconds: u64) {}

    fn create_mutex(&self) -> Handle {
        Handle(0)
    }

    fn acquire(&self, _mutex: Handle, _timeout: u16) -> Result<(), AmlError> {
        Ok(())
    }

    fn release(&self, _mutex: Handle) {}
}
