mod test_infra;
use acpi::aml::AmlError;
use aml_test_tools::{RunTestResult, TestFailureReason, handlers::null_handler::NullHandler};
use std::assert_matches;

#[test]
fn infinite_method_recursion() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Name(X, 0)
    Method(INF) {
        X++
        INF()
    }
    INF()
}"#;

    let r = test_infra::run_aml_test_with_result(ASL, NullHandler);
    assert_matches!(r, RunTestResult::Failed(_, TestFailureReason::ParseFail(AmlError::MethodStackExceeded)));
}
