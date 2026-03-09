use acpi::Handler;
use aml_test_tools::{new_interpreter, run_test_for_string, TestResult};
use aml_test_tools::handlers::logging_handler::LoggingHandler;

pub fn run_aml_test(asl: &'static str, handler: impl Handler) {
    // Tests calling `run_aml_test` don't do much else, and we usually want logging, so initialize it here.
    let _ = pretty_env_logger::try_init();
    
    let logged_handler = LoggingHandler::new(handler);
    let mut interpreter = new_interpreter(logged_handler);

    assert_eq!(run_test_for_string(asl, &mut interpreter), TestResult::Pass);
}
