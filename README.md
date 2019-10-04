# Acpi
[![Build Status](https://travis-ci.org/rust-osdev/acpi.svg?branch=master)](https://travis-ci.org/rust-osdev/acpi)
[![Version](https://img.shields.io/crates/v/acpi.svg?style=rounded-square)](https://crates.io/crates/acpi/)

### [Documentation (`acpi`)](https://docs.rs/acpi)
### [Documentation (`aml`)](https://docs.rs/aml)

A library to parse ACPI tables and AML, written in pure Rust. Designed to be easy to use from Rust bootloaders and kernels. The library is split into two crates:
- `acpi` parses the static tables (useful but not feature-complete)
- `aml` parses the AML tables (can be useful, far from feature-complete)

## Contributing
Contributions are more than welcome! You can:
- Write code - the ACPI spec is huge and there are bound to be things we don't support yet!
- Documentation
- Using the crates within your kernel and file bug reports and feature requests!

Useful resources for contributing are:
- [The ACPI specification](https://uefi.org/specifications)
- [OSDev Wiki](https://wiki.osdev.org/ACPI)

## Licence
Acpi is dual-licenced under:
- Apache Licence, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

Unless you explicitly state otherwise, any contribution submitted for inclusion in this work by you,
as defined in the Apache-2.0 licence, shall be dual licenced as above, without additional terms or
conditions.
