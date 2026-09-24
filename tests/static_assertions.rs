#![feature(negative_impls)]

use acpi::{PhysicalMapping, aml::Interpreter};
use aml_test_tools::handlers::null_handler::NullHandler;
use static_assertions::{assert_impl_all, assert_not_impl_any};
use acpi::registers::FixedRegisters;

struct NoSend {}
impl !Send for NoSend {}
impl !Sync for NoSend {}

struct SendOnly {}
unsafe impl Send for SendOnly {}
impl !Sync for SendOnly {}

// If all other types are correctly labelled with Send and/or Sync, then Interpreter should
// naturally become Send/Sync.
assert_impl_all!(Interpreter<NullHandler>: Send, Sync);

// The Send/Sync-ness of PhysicalMapping depend directly and purely on the wrapped type.
assert_impl_all!(PhysicalMapping<NullHandler, u8>: Send, Sync);
assert_not_impl_any!(PhysicalMapping<NullHandler, NoSend>: Send, Sync);
assert_impl_all!(PhysicalMapping<NullHandler, SendOnly>: Send);
assert_not_impl_any!(PhysicalMapping<NullHandler, SendOnly>: Sync);

// This was explicitly requested in [#324](https://github.com/rust-osdev/acpi/issues/324)
assert_impl_all!(FixedRegisters<NullHandler>: Send, Sync);
