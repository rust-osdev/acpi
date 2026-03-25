# Tests in the ACPI crate.

There are 4 categories of tests:

1. Unit tests in the main crate source.
2. Integration tests in the `tests` directory - these all have the extension `.rs`.
3. Raw ASL files that can be checked using `aml_tester`
4. Support for running tests from the `uacpi` project.

## Unit tests

These are standard Rust unit tests. Run them using `cargo test` (which will also run the integration tests). There are
relatively few unit tests at present - feel free to send PRs with more!

## Integration tests

These are the `.rs` files in the `tests` directory. They largely consist of snippets of ASL code that are parsed and
then executed using the `aml_test_tools` sub-crate. Most of the tests specify the expected output from the provided
`Handler`, which provides reasonable confidence the parser is working correctly.

The `aml_test_tools` sub-crate provides various utilities for writing this style of test.

Like the unit tests, they can be run using `cargo test`.

## Raw ASL files

Various ASL files are provided in the `tests` directory. They can be parsed and executed by running 
`cargo run_tests -p tests` from the root of the repository. This cargo alias runs the `aml_tester` tool.

The `aml_tester` tool provides a command-line way to run ASL files - similar to `acpiexec`.

## Running tests from the `uacpi` project

> See the documentation for `uacpi_test_adapter` for more detailed information. Note that a large proportion of the
> tests in the `uacpi` project do not yet pass in this crate.
 
The `uacpi` project has a fairly extensive test suite. It makes sense for us to be able to check against their test
suite as well as our own. This can be done by:

1. Checking out the [`uacpi` repository.](https://github.com/uACPI/uACPI)
2. Navigating to the root of that repo.
3. Running something like: 
   ```shell
   AML_TESTER_PATH=../acpi/target/debug/aml_tester python3 tests/run_tests.py --test-runner ../acpi/target/debug/uacpi_test_adapter`
   ```
   Adjusting the paths as necessary. (And editing as needed for PowerShell)

Note that, at present, several of the uACPI tests run indefinitely. You may want to skip these! The easiest way to do so
is probably just to delete them... The relevant tests are:

* `global-lock.asl`
* `hanging-while.asl`
* `infinite-recursion.asl`
* `mutex-1.asl`
* `mutex-2.asl`
* `mutex-3.asl`
