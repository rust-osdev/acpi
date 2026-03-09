// Test operations on "normal" fields - those that are not Index or Bank fields.

use aml_test_tools::handlers::{
    check_cmd_handler::AcpiCommands as CheckCommands,
    listed_response_handler::AcpiCommands as Results,
    std_test_handler::{Command, construct_std_handler},
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
        (CheckCommands::CreateMutex, Results::Skip()),

        // A = 0xA5A5
        (CheckCommands::WriteU16(0x40000, 0xA5A5), Results::Skip()),

        // B = A
        (CheckCommands::ReadU16(0x40000), Results::ReadU16(0xA5A5)),
        (CheckCommands::WriteU16(0x40002, 0xA5A5), Results::Skip()),
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
        (CheckCommands::CreateMutex, Results::Skip()),

        // A = 0xA55A
        (CheckCommands::WriteIoU8(0x40, 0x5A), Results::Skip()),
        (CheckCommands::WriteIoU8(0x41, 0xA5), Results::Skip()),

        // B = A
        (CheckCommands::ReadIoU8(0x40), Results::ReadIoU8(0x5A)),
        (CheckCommands::ReadIoU8(0x41), Results::ReadIoU8(0xA5)),
        (CheckCommands::WriteIoU8(0x42, 0x5A), Results::Skip()),
        (CheckCommands::WriteIoU8(0x43, 0xA5), Results::Skip()),
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
        (CheckCommands::CreateMutex, Results::Skip()),
        (CheckCommands::ReadIoU16(0x40), Results::ReadIoU16(0)),
        (CheckCommands::WriteIoU16(0x40, 0x04), Results::Skip()),
        (CheckCommands::ReadIoU16(0x40), Results::ReadIoU16(4)),
        (CheckCommands::ReadIoU16(0x40), Results::ReadIoU16(4)),
        (CheckCommands::WriteIoU16(0x40, 0x204), Results::Skip()),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}
