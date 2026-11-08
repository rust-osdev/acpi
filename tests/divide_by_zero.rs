mod test_infra;
use acpi::aml::AmlError;
use aml_test_tools::{RunTestResult, TestFailureReason, handlers::null_handler::NullHandler};
use std::assert_matches;

#[test]
fn test_divide_by_zero() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "DIVZERO", 0x00000002) {
    Method(MAIN, 0, NotSerialized) {
        Local0 = 0
        Local1 = 17 / Local0
        Return (0)
    }
}
"#;

    let r = test_infra::run_aml_test_with_result(AML, NullHandler);
    assert_matches!(r, RunTestResult::Failed(_, TestFailureReason::ParseFail(AmlError::DivideByZero)));
}

#[test]
fn test_mod_by_zero() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 2, "RSACPI", "MODZERO", 0x00000002) {
    Method(MAIN, 0, NotSerialized) {
        Local0 = 0
        Local1 = 17 % Local0
        Return (0)
    }
}
"#;

    let r = test_infra::run_aml_test_with_result(AML, NullHandler);
    assert_matches!(r, RunTestResult::Failed(_, TestFailureReason::ParseFail(AmlError::DivideByZero)));
}
