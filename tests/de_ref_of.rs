use aml_test_tools::handlers::null_handler::NullHandler;

mod test_infra;

#[test]
fn test_deref_of_buffer_field() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "DerefOf", 0x00000002) {
    Scope (\_SB) {
        Name (ADAT, Buffer (0x0010) {
            /* 0000 */  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            /* 0008 */  0x00, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        })
    }

    Method(MAIN, 0, NotSerialized) {
        Local0 = (DerefOf(\_SB.ADAT[0x09]))
        // This relies on subtraction rather than equality as logical ops on BufferFields don't work
        // yet.
        return (Local0 - 0xaa)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}
