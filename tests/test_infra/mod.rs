use acpi::Handler;
use aml_test_tools::{
    RunTestResult,
    TestResult,
    handlers::logging_handler::LoggingHandler,
    new_interpreter,
    run_test_for_string,
};

pub fn run_aml_test(asl: &'static str, handler: impl Handler) {
    // Tests calling `run_aml_test` don't do much else, and we usually want logging, so initialize it here.
    let _ = pretty_env_logger::try_init();

    let logged_handler = LoggingHandler::new(handler);
    let interpreter = new_interpreter(logged_handler);

    let result = run_test_for_string(asl, interpreter);
    assert!(matches!(result, RunTestResult::Pass(_)), "Test failed with: {:?}", TestResult::from(&result));
}
