# Acpi
![Build Status](https://github.com/rust-osdev/acpi/actions/workflows/build.yml/badge.svg)
[![Version](https://img.shields.io/crates/v/rsdp.svg?style=rounded-square)](https://crates.io/crates/rsdp/)
[![Version](https://img.shields.io/crates/v/acpi.svg?style=rounded-square)](https://crates.io/crates/acpi/)
[![Version](https://img.shields.io/crates/v/aml.svg?style=rounded-square)](https://crates.io/crates/aml/)

### [Documentation (`rsdp`)](https://docs.rs/rsdp)
### [Documentation (`acpi`)](https://docs.rs/acpi)
### [Documentation (`aml`)](https://docs.rs/aml)

A library to parse ACPI tables and AML, written in pure Rust. Designed to be easy to use from Rust bootloaders and kernels. The library is split into three crates:
- `rsdp` parses the RSDP and can locate it on BIOS platforms. It does not depend on `alloc`, so is suitable to use from bootloaders without heap alloctors. All of its
  functionality is reexported by `acpi`.
- `acpi` parses the static tables (useful but not feature-complete). It can be used from environments that have allocators, and ones that don't (but with reduced functionality).
- `aml` parses the AML tables (can be useful, far from feature-complete).

There is also the `acpi-dumper` utility to easily dump a platform's ACPI tables (this currently only works on Linux).

## Contributing
Contributions are more than welcome! You can:
- Write code - the ACPI spec is huge and there are bound to be things we don't support yet!
- Improve our documentation!
- Use the crates within your kernel and file bug reports and feature requests!

Useful resources for contributing are:
- [The ACPI specification](https://uefi.org/specifications)
- [OSDev Wiki](https://wiki.osdev.org/ACPI)

You can run the AML test suite with `cargo run --bin aml_tester -- -p tests`.
You can run fuzz the AML parser with `cd aml && cargo fuzz run fuzz_target_1` (you may need to `cargo install cargo-fuzz`).

## Licence
This project is dual-licenced under:
- Apache Licence, Version 2.0 ([LICENCE-APACHE](LICENCE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENCE-MIT](LICENCE-MIT) or http://opensource.org/licenses/MIT)

Unless you explicitly state otherwise, any contribution submitted for inclusion in this work by you,
as defined in the Apache-2.0 licence, shall be dual licenced as above, without additional terms or
conditions.
