use aml_test_tools::handlers::std_test_handler::{
    Command,
    construct_std_handler,
    create_mutex,
    read_pci_u8,
    read_u8,
    write_pci_u8,
    write_u8,
};
use pci_types::PciAddress;
use std::{sync::mpsc, thread, time::Duration};

mod test_infra;

#[test]
fn test_region_in_top_level() {
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "OPREG", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        DATA, 8
    }

    Method(RDWR, 0, NotSerialized) {
        DATA = 0xA5
        Return (DATA)
    }

    Method(MAIN, 0, NotSerialized) {
        Local0 = RDWR()
        // Remember that returning zero indicates success.
        Return (Local0 != 0xA5)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // DATA = 0xA5
        write_u8(0x40000, 0xA5),
        read_u8(0x40000, 0xA5),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_region_in_device() {
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "OPREG", 1) {
    Device(TEST) {
        Name (_HID, EisaId ("PNP0C01")) // Arbitrary choice.

        OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
        Field(MEM, ByteAcc, NoLock, Preserve) {
            DATA, 8
        }

        Method(RDWR, 0, NotSerialized) {
            DATA = 0xA5
            Return (DATA)
        }
    }

    Method(MAIN, 0, NotSerialized) {
        Local0 = ^TEST.RDWR()
        // Remember that returning zero indicates success.
        Return (Local0 != 0xA5)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // DATA = 0xA5
        write_u8(0x40000, 0xA5),
        read_u8(0x40000, 0xA5),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_region_in_method() {
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "OPREG", 1) {

    // `iasl` is not a fan of this function - it generates a bunch of warnings, which for testing
    // purposes can be ignored.
    Method(RDWR, 1, NotSerialized) {
        OperationRegion(MEM, SystemMemory, Arg0, 0x1000)
        Field(MEM, ByteAcc, NoLock, Preserve) {
            DATA, 8
        }

        DATA = 0xA5
        Return (DATA)
    }

    Method(MAIN, 0, NotSerialized) {
        Local0 = RDWR(0x40000)
        // Remember that returning zero indicates success.
        if (Local0 != 0xA5) {
            Return (One)
        }

        Local0 = RDWR(0x50000)
        Return (Local0 != 0xA5)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // Base set to 0x40000
        write_u8(0x40000, 0xA5),
        read_u8(0x40000, 0xA5),
        // Base set to 0x50000
        write_u8(0x50000, 0xA5),
        read_u8(0x50000, 0xA5),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
#[ignore]
fn test_buffer_field_implicit_conv() {
    // This test covers https://github.com/rust-osdev/acpi/issues/273
    const AML: &str = r#"DefinitionBlock ("", "SSDT", 2, "AMD", "AmdTable", 0x00000002)
{
    Name (ADAT, Buffer (0x0010)
    {
        /* 0000 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // ........
        /* 0008 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // ........

    })
    OperationRegion (A053, SystemMemory, (DerefOf (ADAT [(0x04)])), 0x1000) // <--- this line failed
    Field (A053, ByteAcc, NoLock, Preserve)
    {
        Offset (0x18),
        A, 8
    }
}"#;

    const EXPECTED_COMMANDS: &[Command] = &[create_mutex()];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_region_in_pci_device() {
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "OPREG", 1) {
    Device(TEST) {
        Name (_ADR, 0x00020001) // Arbitrary choice.
        Name (_BBN, 3)
        Method (_SEG, 0, NotSerialized) { Return (4) }

        OperationRegion(MEM, PCI_Config, 0x40, 0x20)
        Field(MEM, ByteAcc, NoLock, Preserve) {
        Offset (0x10),
            DATA, 8
        }

        Method(RDWR, 0, NotSerialized) {
            DATA = 0xA5
            Return (DATA)
        }
    }

    Method(MAIN, 0, NotSerialized) {
        Local0 = ^TEST.RDWR()
        // Remember that returning zero indicates success.
        Return (Local0 != 0xA5)
    }
}
"#;

    let address: PciAddress = PciAddress::new(4, 3, 2, 1);
    let expected_commands: &[Command] = &[
        create_mutex(),
        // DATA = 0xA5
        write_pci_u8(address, 0x50, 0xA5),
        read_pci_u8(address, 0x50, 0xA5),
    ];

    let handler = construct_std_handler(expected_commands.to_vec());
    test_infra::run_aml_test(AML, handler);
}

/// Regression test: storing to a field in a `PCI_Config` region must not deadlock when the
/// device's address is computed by a method. Accessing a `PCI_Config` region evaluates the
/// device's `_SEG`/`_BBN`/`_ADR` objects, which are allowed to be methods. A method that
/// stores to an implicit target (e.g. arithmetic without one) re-enters `do_store`, which used
/// to deadlock on the object token. The test runs on a separate thread with a timeout, so a
/// regression fails instead of hanging the whole test harness.
#[test]
fn test_region_in_pci_device_with_method_adr() {
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "OPREG", 1) {
    Device(TEST) {
        Name (_BBN, 3)
        Name (_SEG, 4)
        // The arithmetic must be on named objects, or iasl constant-folds it away.
        Name (DEVN, 0x1E)
        Name (FUNC, 7)
        Method (_ADR, 0, NotSerialized) {
            Return (Add (ShiftLeft (DEVN, 16), FUNC))
        }

        OperationRegion(MEM, PCI_Config, 0x40, 0x20)
        Field(MEM, ByteAcc, NoLock, Preserve) {
        Offset (0x10),
            DATA, 8
        }

        Method(RDWR, 0, NotSerialized) {
            DATA = 0xA5
            Return (DATA)
        }
    }

    Method(MAIN, 0, NotSerialized) {
        Local0 = ^TEST.RDWR()
        // Remember that returning zero indicates success.
        Return (Local0 != 0xA5)
    }
}
"#;

    let address: PciAddress = PciAddress::new(4, 3, 0x1e, 7);
    let expected_commands: Vec<Command> = vec![
        create_mutex(),
        // DATA = 0xA5
        write_pci_u8(address, 0x50, 0xA5),
        read_pci_u8(address, 0x50, 0xA5),
    ];

    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let handler = construct_std_handler(expected_commands);
        test_infra::run_aml_test(AML, handler);
        let _ = tx.send(());
    });

    rx.recv_timeout(Duration::from_secs(30))
        .expect("interpreter deadlocked evaluating a PCI address method during a field store");
}
