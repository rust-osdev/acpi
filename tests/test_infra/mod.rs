use acpi::Handler;
use aml_test_tools::{RunTestResult, TestResult, handlers::logging_handler::LoggingHandler, new_interpreter, run_test_for_string, run_test_for_opcodes};

// The following two functions are very similar in structure, but whilst there are only two of them
// it's not worth adding complexity to make them DRY.

/// Run a test against an ASL string.
///
/// The string `asl` represents a compile-able ASL string, so needs to include the `DefinitionBlock`
/// statement.
#[allow(dead_code)]
pub fn run_aml_test(asl: &'static str, handler: impl Handler) {
    // Tests calling `run_aml_test` don't do much else, and we usually want logging, so initialize it here.
    let _ = pretty_env_logger::try_init();

    let logged_handler = LoggingHandler::new(handler);
    let interpreter = new_interpreter(logged_handler);

    let result = run_test_for_string(asl, interpreter, &None);
    assert!(matches!(result, RunTestResult::Pass(_)), "Test failed with: {:?}", TestResult::from(&result));
}

/// Run a test against a sequence of AML opcodes.
///
/// The provided opcodes are wrapped in a `MAIN` method and also have a table header prepended. The
/// `MAIN` function is executed as part of the test, so `opcodes` only needs to include the actual
/// opcodes to execute.
#[allow(dead_code)]
pub fn run_opcodes_test(opcodes: &[u8], handler: impl Handler) {
    let _ = pretty_env_logger::try_init();

    let logged_handler = LoggingHandler::new(handler);
    let interpreter = new_interpreter(logged_handler);

    let result = run_test_for_opcodes(opcodes, interpreter, &None);
    assert!(matches!(result, RunTestResult::Pass(_)), "Test failed with: {:?}", TestResult::from(&result));
}
