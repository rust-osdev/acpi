// Test operations on "normal" fields - those that are not Index or Bank fields.

use aml_test_tools::handlers::std_test_handler::{
    Command,
    construct_std_handler,
    create_mutex,
    read_io_u8,
    read_io_u16,
    read_u16,
    write_io_u8,
    write_io_u16,
    write_u16,
};

mod test_infra;

#[test]
fn test_basic_store_and_load() {
    const AML: &str = r#"DefinitionBlock("%FN%", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, WordAcc, NoLock, Preserve) {
        A, 16,
        B, 16
    }

    Method(MAIN, 0, NotSerialized) {
        A = 0xA5A5
        B = A
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // A = 0xA5A5
        write_u16(0x40000, 0xA5A5),
        // B = A
        read_u16(0x40000, 0xA5A5),
        write_u16(0x40002, 0xA5A5),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_narrow_access_store_and_load() {
    const AML: &str = r#"DefinitionBlock("%FN%", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
    OperationRegion(MEM, SystemIO, 0x40, 0x10)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        A, 16,
        B, 16
    }

    Method(MAIN, 0, NotSerialized) {
        A = 0xA55A
        B = A
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // A = 0xA55A
        write_io_u8(0x40, 0x5A),
        write_io_u8(0x41, 0xA5),
        // B = A
        read_io_u8(0x40, 0x5A),
        read_io_u8(0x41, 0xA5),
        write_io_u8(0x42, 0x5A),
        write_io_u8(0x43, 0xA5),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_unaligned_field_store() {
    const AML: &str = r#"DefinitionBlock("%FN%", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
    OperationRegion(MEM, SystemIO, 0x40, 0x10)
    Field(MEM, WordAcc, NoLock, Preserve) {
        A, 7,
        B, 8
    }

    Method(MAIN, 0, NotSerialized) {
        A = 4
        B = A

        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        read_io_u16(0x40, 0),
        write_io_u16(0x40, 0x04),
        read_io_u16(0x40, 4),
        read_io_u16(0x40, 4),
        write_io_u16(0x40, 0x204),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}
