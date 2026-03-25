//! A wrapper around the `aml_tester` crate that allows it to run the uACPI test suite.
//!
//! Simply forwards all arguments sent from the uACPI test runner to `aml_tester`, *except* for when
//! the resource-tests are requested (which we don't support).
//!
//! Why make an adapter instead of just running `aml_tester` directly?
//! * It allows uACPI to change their testing system without affecting users of `aml_tester`.
//! * We can write a separate binary or other tool for running "resource-tests" without needing to
//!   complicate `aml_tester`.
//!
//! Usage:
//! * Set the environment variable `AML_TESTER_PATH` to the path of the `aml_tester` binary.
//! * Run the uACPI test suite but setting `uacpi_test_adapter` to the test runner.
//!
//! e.g.: from the uACPI root directory:
//! ```sh
//! AML_TESTER_PATH=../acpi/target/debug/aml_tester python3 tests/run_tests.py --test-runner ../acpi/target/debug/uacpi_test_adapter
//! ```
//! > Adjust the paths as needed!
//!
//! Notes:
//!
//! You may prefer to manually run aml_tester with individual ASL files from the uACPI test suite,
//! as you'll get better formatting. However, that would require you to manually enter the expected
//! results.
use std::{
    env,
    process::{Command, ExitCode},
};

fn main() -> ExitCode {
    // We don't support the resource tests, so just claim success to allow the main test suite to
    // run.
    if env::args_os().skip(1).any(|arg| arg == "resource-tests") {
        return ExitCode::SUCCESS;
    }

    let Some(tester_path) = env::var_os("AML_TESTER_PATH") else {
        eprintln!("AML_TESTER_PATH is not set");
        return ExitCode::FAILURE;
    };

    let status = match Command::new(tester_path).args(env::args_os().skip(1)).status() {
        Ok(status) => status,
        Err(err) => {
            eprintln!("Failed to execute AML_TESTER_PATH: {err}");
            return ExitCode::FAILURE;
        }
    };

    match status.code() {
        Some(code) => ExitCode::from(code as u8),
        None => ExitCode::FAILURE,
    }
}
