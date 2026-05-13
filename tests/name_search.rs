use aml_test_tools::handlers::null_handler::NullHandler;

mod test_infra;

#[test]
fn test_name_search_1() {
    const AML: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "NAMING", 1) {
    Scope (\) {
        Device (AMW0) {
            Name (_HID, EisaId ("PNP0C14"))

            OperationRegion (\RAMX, SystemMemory, 0x1000, 0x0100)
            Field (RAMX, ByteAcc, NoLock, Preserve)
            {
                WFUN,   32,
            }
        }
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_name_search_2() {
    const AML: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "NAMING", 1) {
    Scope (\) {
        OperationRegion (RAMX, SystemMemory, 0x1000, 0x0100)
        Device (AMW0) {
            Name (_HID, EisaId ("PNP0C14"))

            Field (\RAMX, ByteAcc, NoLock, Preserve)
            {
                WFUN,   32,
            }
        }
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}
