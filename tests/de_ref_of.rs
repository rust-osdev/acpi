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
        // TODO: Use logical ops for clarity, when available.
        return (Local0 - 0xaa)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_deref_of_direct_buffer_field() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "DerefOf", 0x00000002) {
    Name (ADAT, Buffer (0x01) { 0xaa })
    CreateByteField (ADAT, 0x00, BF00)

    Method(MAIN, 0, NotSerialized) {
        Return (DerefOf(BF00) - 0xaa)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_deref_of_local_buffer_field_after_store() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "DerefOf", 0x00000002) {
    Method(MAIN, 0, NotSerialized) {
        Local0 = Buffer (0x01) { 0x00 }
        CreateByteField (Local0, 0x00, BF00)
        BF00 = 0xaa
        Return (DerefOf(BF00) - 0xaa)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_deref_of_named_buffer_field_stores_value() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "DerefOf", 0x00000002) {
    Name (ADAT, Buffer (0x01) { 0xaa })
    CreateByteField (ADAT, 0x00, BF00)

    Method(MAIN, 0, NotSerialized) {
        Local0 = DerefOf(BF00)
        Return (Local0 - 0xaa)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_deref_of_named_string_path() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "DerefOf", 0x00000002) {
    Name (TGT, 0xaa)
    Name (STR, "\\TGT")

    Method(MAIN, 0, NotSerialized) {
        Return (DerefOf(STR) - 0xaa)
    }
}
"#;

    let handler = NullHandler {};
    test_infra::run_aml_test(AML, handler);
}
