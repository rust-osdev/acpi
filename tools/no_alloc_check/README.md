# no_alloc_check test tool

This executable is used to check that the crate is usable in a no-alloc environment. It was born
out of a comment on [issue 311](https://github.com/rust-osdev/acpi/issues/311) that it didn't work.

The main `acpi` crate is compiled with no alloc support in one of the workflow builds. This
executable is also built in the workflow as a sanity check that the crate can be *linked to* in a
no-alloc environment.

This executable should build, but actually attempting to run it will result in an access
violation. To demonstrate that the crate can be used in a no-alloc environment it is enough that it
builds successfully.

## Acknowledgements

The code was based on the [example by zulinx86](https://zenn.dev/zulinx86/articles/rust-nostd-101)

## Exclusion from the workspace

This crate is excluded from the workspace because it requires the profile setting `panic = abort`.

We do not want to apply that profile to the whole workspace, but the panic profile setting cannot
be applied to individual crates in a workspace.
