use aml_test_tools::handlers::null_handler::NullHandler;

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
