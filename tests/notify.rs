use acpi::{
    Handle,
    Handler,
    RawPhysicalMapping,
    aml::{AmlError, namespace::AmlName},
};
use aml_test_tools::handlers::null_handler::NullHandler;
use pci_types::PciAddress;
use std::{
    str::FromStr,
    sync::{Arc, Mutex},
};

mod test_infra;

#[test]
fn test_notify() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "NOTIFY", 0x00000002) {
    Device (LID0) {
        Name (_HID, EisaId ("PNP0C0D"))
    }
    Method (MAIN, 0, NotSerialized) {
        Notify (LID0, 0x80)
        Local0 = 0x80
        Notify (\LID0, Local0)
        Return (0)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}

/// A [`NullHandler`] that records every notification it receives.
#[derive(Clone, Default)]
struct NotifyRecorder {
    notifications: Arc<Mutex<Vec<(AmlName, u64)>>>,
}

impl Handler for NotifyRecorder {
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> RawPhysicalMapping<T> {
        unsafe { NullHandler.map_physical_region(physical_address, size) }
    }

    unsafe fn unmap_physical_region<T>(&self, region: RawPhysicalMapping<T>) {
        unsafe { NullHandler.unmap_physical_region(region) }
    }

    fn read_u8(&self, address: usize) -> u8 {
        NullHandler.read_u8(address)
    }

    fn read_u16(&self, address: usize) -> u16 {
        NullHandler.read_u16(address)
    }

    fn read_u32(&self, address: usize) -> u32 {
        NullHandler.read_u32(address)
    }

    fn read_u64(&self, address: usize) -> u64 {
        NullHandler.read_u64(address)
    }

    fn write_u8(&self, address: usize, value: u8) {
        NullHandler.write_u8(address, value)
    }

    fn write_u16(&self, address: usize, value: u16) {
        NullHandler.write_u16(address, value)
    }

    fn write_u32(&self, address: usize, value: u32) {
        NullHandler.write_u32(address, value)
    }

    fn write_u64(&self, address: usize, value: u64) {
        NullHandler.write_u64(address, value)
    }

    fn read_io_u8(&self, port: u16) -> u8 {
        NullHandler.read_io_u8(port)
    }

    fn read_io_u16(&self, port: u16) -> u16 {
        NullHandler.read_io_u16(port)
    }

    fn read_io_u32(&self, port: u16) -> u32 {
        NullHandler.read_io_u32(port)
    }

    fn write_io_u8(&self, port: u16, value: u8) {
        NullHandler.write_io_u8(port, value)
    }

    fn write_io_u16(&self, port: u16, value: u16) {
        NullHandler.write_io_u16(port, value)
    }

    fn write_io_u32(&self, port: u16, value: u32) {
        NullHandler.write_io_u32(port, value)
    }

    fn read_pci_u8(&self, address: PciAddress, offset: u16) -> u8 {
        NullHandler.read_pci_u8(address, offset)
    }

    fn read_pci_u16(&self, address: PciAddress, offset: u16) -> u16 {
        NullHandler.read_pci_u16(address, offset)
    }

    fn read_pci_u32(&self, address: PciAddress, offset: u16) -> u32 {
        NullHandler.read_pci_u32(address, offset)
    }

    fn write_pci_u8(&self, address: PciAddress, offset: u16, value: u8) {
        NullHandler.write_pci_u8(address, offset, value)
    }

    fn write_pci_u16(&self, address: PciAddress, offset: u16, value: u16) {
        NullHandler.write_pci_u16(address, offset, value)
    }

    fn write_pci_u32(&self, address: PciAddress, offset: u16, value: u32) {
        NullHandler.write_pci_u32(address, offset, value)
    }

    fn nanos_since_boot(&self) -> u64 {
        NullHandler.nanos_since_boot()
    }

    fn stall(&self, microseconds: u64) {
        NullHandler.stall(microseconds)
    }

    fn sleep(&self, milliseconds: u64) {
        NullHandler.sleep(milliseconds)
    }

    fn create_mutex(&self) -> Handle {
        NullHandler.create_mutex()
    }

    fn acquire(&self, mutex: Handle, timeout: u16) -> Result<(), AmlError> {
        NullHandler.acquire(mutex, timeout)
    }

    fn release(&self, mutex: Handle) {
        NullHandler.release(mutex)
    }

    fn handle_notify(&self, object: &AmlName, value: u64) {
        self.notifications.lock().unwrap().push((object.clone(), value));
    }
}

#[test]
fn test_notify_delivers_resolved_path() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "NOTIFY", 0x00000002) {
    Scope (\_SB) {
        Device (CPUS) {
            Name (_HID, "ACPI0010")
            Device (C001) {
                Name (_HID, "ACPI0007")
            }
            Method (NTFY, 0, NotSerialized) {
                // Relative, resolved against the scope of the running method (\_SB.CPUS)
                Notify (C001, 0x01)
                // Absolute
                Notify (\_SB.CPUS.C001, 0x02)
                // Relative with a parent prefix and a computed value
                Local0 = 0x80
                Notify (^C001, Local0)
            }
            Method (NTFA, 2, NotSerialized) {
                // The device arrives as an argument
                Notify (Arg0, Arg1)
            }
            Method (NTFR, 0, NotSerialized) {
                // The device is held by a local as a reference
                Local0 = RefOf (C001)
                Notify (Local0, 0x04)
            }
        }
    }
    Method (MAIN, 0, NotSerialized) {
        \_SB.CPUS.NTFY ()
        \_SB.CPUS.NTFA (\_SB.CPUS.C001, 0x03)
        \_SB.CPUS.NTFR ()
        Return (0)
    }
}
"#;

    let handler = NotifyRecorder::default();
    let notifications = handler.notifications.clone();
    test_infra::run_aml_test(AML, handler);

    let path = AmlName::from_str("\\_SB_.CPUS.C001").unwrap();
    let expected: Vec<_> = [0x01, 0x02, 0x80, 0x03, 0x04].into_iter().map(|value| (path.clone(), value)).collect();
    assert_eq!(*notifications.lock().unwrap(), expected);
}
