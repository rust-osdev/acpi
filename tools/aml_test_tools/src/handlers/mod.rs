//! A collection of [`handlers`](acpi::Handler) that might be useful when testing AML.
//!
//! These are all used by the [`acpi`] integration tests, or by `aml_tester`.

pub mod check_cmd_handler;
pub mod listed_response_handler;
pub mod logging_handler;
pub mod null_handler;
pub mod std_test_handler;
