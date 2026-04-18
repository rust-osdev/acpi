//! These examples have been adapted from the Readme of the [uACPI](https://github.com/uACPI/uACPI)
//! project at commit 1ca45f3.
//!
//! At present most of these tests probably won't work, but it'll help to guide us towards better
//! compatibility.
//!
//! The comments demonstrate some of the differences between the NT "real world" interpreter and
//! the ACPI reference standard.

mod test_infra;

use aml_test_tools::{handlers::null_handler::NullHandler};

#[test]
#[ignore] // Fails with ObjectNotOfExpectedType { expected: Reference, got: Integer }
fn expressions_with_package() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Method (TEST) {
        Local0 = 10
        Local1 = Package { Local0 * 5 }
        Return (DerefOf(Local1[0]))
    }

    // ACPICA: AE_SUPPORT, Expressions within package elements are not supported
    // Windows, uACPI: Local0 = 50
    Local0 = TEST()
}
    "#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
fn package_outside_of_control_method() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    // ACPICA: internal error
    // Windows, uACPI: ok
    Local0 = Package { 1 }

    // I don't have a good way of testing this, but if it completes without errors it's probably OK.
    Debug = Local0[0]
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
#[ignore] // Local0 is set to 123 - we follow the ACPICA way, not the Windows way.
fn reference_rebind_semantics() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Method (MAIN, 0, NotSerialized) {
        Local0 = 123
        Local1 = RefOf(Local0)

        // ACPICA: Local1 = 321, Local0 = 123
        // Windows, uACPI: Local1 = reference->Local0, Local0 = 321
        Local1 = 321

        Return (Local0 == 123)
    }
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
#[ignore] // ParseFail(ObjectNotOfExpectedType { expected: Integer, got: Integer } (a referencing failure)
fn increment_decrement() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Local0 = 123
    Local1 = RefOf(Local0)

    // ACPICA: error
    // Windows, uACPI: Local0 = 124
    Local1++
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
#[ignore] // ParseFail(ObjectNotOfExpectedType { expected: Reference, got: Integer })
fn multilevel_references() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Local0 = 123
    Local1 = RefOf(Local0)
    Local2 = RefOf(Local1)

    // ACPICA: Local3 = reference->Local0
    // Windows, uACPI: Local3 = 123
    Local3 = DerefOf(Local2)
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
#[ignore] // "Stores to objects like WrappedObject(UnsafeCell { .. }) are not yet supported"
fn implicit_case_semantics() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Name (TEST, "BAR")

    // ACPICA: TEST = "00000000004F4F46"
    // Windows, uACPI: TEST = "FOO"
    TEST = 0x4F4F46
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
#[ignore] // "Stores to objects like WrappedObject(UnsafeCell { .. }) are not yet supported"
fn buffer_size_mutability() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Name (TEST, "XXXX")
    Name (VAL, "")

    // ACPICA: TEST = "LONGSTRING"
    // Windows, UACPI: TEST = "LONG"
    TEST = "LONGSTRING"

    // ACPICA: VAL = "FOO"
    // Windows, UACPI: VAL = ""
    VAL = "FOO"
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
fn ref_to_local() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Method (TEST) {
        Local0 = 123

        // Use-after-free in ACPICA, perfectly fine in uACPI
        Return (RefOf(Local0))
    }

    Method (FOO) {
        Name (TEST, 123)

        // Use-after-free in ACPICA, object lifetime prolonged in uACPI (node is still removed from the namespace)
        Return (RefOf(TEST))
    }

    Method (MAIN) {
        FOO ()
        Return (0)
    }
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}

#[test]
#[ignore] // CopyObject not yet implemented
fn copy_object_to_self() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Method (TEST) {
        CopyObject(123, TEST)
        Return (1)
    }

    // Segfault in ACPICA, prints 1 in uACPI
    Debug = TEST()

    // Unreachable in ACPICA, prints 123 in uACPI
    Debug = TEST
}"#;

    let handler = NullHandler;
    test_infra::run_aml_test(ASL, handler);
}
