use aml_test_tools::handlers::{
    check_cmd_handler::{AcpiCommands as CheckCommands, CheckCommandHandler},
    listed_response_handler::AcpiCommands as Results,
    null_handler::NullHandler,
    std_test_handler::{Command, construct_std_handler},
};

mod test_infra;

#[test]
#[ignore]
fn test_basic() {
    const AML: &str = r#"DefinitionBlock("%FN%", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
    // Adapted from part of the DSDT on a Dell XPS 13 laptop.
    OperationRegion (RTCO, SystemIO, 0x72, 0x02)
    Field (RTCO, ByteAcc, NoLock, Preserve)
    {
        CIND,   8,
        CDAT,   8
    }
    IndexField (CIND, CDAT, AnyAcc, NoLock, Preserve)
    {
        Offset (0x5A),
        TEST,   16
    }

    TEST++
}"#;

    const EXPECTED_COMMANDS: &[CheckCommands] = &[];
    let handler = CheckCommandHandler::new(EXPECTED_COMMANDS.to_vec(), NullHandler {});

    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_fields() {
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
