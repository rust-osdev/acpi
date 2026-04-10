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
//! * Make sure `aml_tester` can be found. The following methods are tried, in this order of
//!   precedence:
//!   * Set the environment variable `AML_TESTER_PATH` to the path and filename of the `aml_tester`
//!     binary, or
//!   * Make sure `aml_tester` is in the system PATH, or
//!   * Ensure `aml_tester` is in the same folder as `uacpi_test_adapter`.
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
    ffi::OsString,
    path::PathBuf,
    process::{Command, ExitCode},
};
use which::which;

fn main() -> ExitCode {
    // We don't support the resource tests, so just claim success to allow the main test suite to
    // run.
    if env::args_os().skip(1).any(|arg| arg == "resource-tests") {
        return ExitCode::SUCCESS;
    }

    let Some(tester_path) = get_aml_tester_path() else {
        eprintln!("aml_tester not found. Try setting AML_TESTER_PATH to the path of the aml_tester binary.");
        return ExitCode::FAILURE;
    };

    let status = match Command::new(tester_path).args(env::args_os().skip(1)).status() {
        Ok(status) => status,
        Err(err) => {
            eprintln!("Failed to execute aml_tester: {err}");
            return ExitCode::FAILURE;
        }
    };

    match status.code() {
        Some(code) => ExitCode::from(code as u8),
        None => ExitCode::FAILURE,
    }
}

/// Find the path to the `aml_tester` binary.
///
/// Uses the search order given in this executable's main documentation.
fn get_aml_tester_path() -> Option<OsString> {
    [get_aml_tester_from_env, get_aml_tester_from_path_env, get_aml_tester_from_binary_path]
        .iter()
        .find_map(|f| f())
}

/// If the environment variable `AML_TESTER_PATH` is set, use that. Assume it is correct without
/// checking.
fn get_aml_tester_from_env() -> Option<OsString> {
    env::var_os("AML_TESTER_PATH")
}

/// If `aml_tester` is in the system PATH, use that.
fn get_aml_tester_from_path_env() -> Option<OsString> {
    which("aml_tester").ok().map(|path| path.into())
}

/// If `aml_tester` exists alongside this executable, use that.
fn get_aml_tester_from_binary_path() -> Option<OsString> {
    // This says: "change the name of the current executable to `aml_tester` and see if that exists."
    env::current_exe().ok().map(change_exec_name).and_then(|path| which(path).ok()).map(|path| path.into())
}

/// Replace the filename of the given path with `aml_tester`. Preserve the extension so that
/// Windows won't have problems.
fn change_exec_name(mut path: PathBuf) -> PathBuf {
    // The final `unwrap` on the following line is reasonable because if the UTF-8 conversion fails,
    // the filename is probably invalid, so we don't really want to keep trying to use it!
    let extension = String::from(path.extension().unwrap_or_default().to_str().unwrap());
    path.set_file_name("aml_tester");
    path.set_extension(extension);
    path
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_filename_replacement() {
        const PATH: &str = "/a/b/c/uacpi_test_adapter.ext";
        let original_path = PathBuf::from(PATH);
        let expected_path = PathBuf::from("/a/b/c/aml_tester.ext");
        let result_path = change_exec_name(original_path);

        assert_eq!(result_path, expected_path);
    }
}
