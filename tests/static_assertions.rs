use aml_test_tools::handlers::null_handler::NullHandler;
use acpi::aml::{Interpreter, object::WrappedObject};
use static_assertions::assert_impl_all;

assert_impl_all!(Interpreter<NullHandler>: Send, Sync);
assert_impl_all!(WrappedObject: Send, Sync);
