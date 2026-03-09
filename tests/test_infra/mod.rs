use acpi::Handler;
use aml_test_tools::run_test_for_string;

pub fn run_aml_test(asl: &'static str, handler: impl Handler) {
    // Tests calling `run_aml_test` don't do much else, and we usually want logging, so initialize it here.
    let _ = pretty_env_logger::try_init();

    run_test_for_string(asl, handler);
}
