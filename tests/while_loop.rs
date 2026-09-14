mod test_infra;
use acpi::aml::AmlError;
use aml_test_tools::{
    RunTestResult,
    TestFailureReason,
    handlers::{null_handler::NullHandler, sys_timer_handler::SystemTimerHandler},
};
use std::assert_matches;

#[test]
fn infinite_while_loop() {
    const ASL: &str = r#"
DefinitionBlock("", "DSDT", 1, "RSACPI", "UACPI", 1) {
    Name(X, 0)
    While (1) {
        X++
    }
}"#;

    let handler = SystemTimerHandler::new(NullHandler, 100);
    let r = test_infra::run_aml_test_with_result(ASL, handler);
    assert_matches!(r, RunTestResult::Failed(_, TestFailureReason::ParseFail(AmlError::LoopTimeout)));
}
