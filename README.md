# Acpi
[![Build Status](https://travis-ci.org/rust-osdev/acpi.svg?branch=master)](https://travis-ci.org/rust-osdev/acpi)
[![Version](https://img.shields.io/crates/v/acpi.svg?style=rounded-square)](https://crates.io/crates/acpi/)
[![Docs](https://docs.rs/acpi/badge.svg)](https://docs.rs/acpi)

A library to parse ACPI tables and AML, written in Rust. Designed to be easy to use from inside a
kernel written in Rust, and fully tested.

Acpi is still far from feature-complete, but can be used to parse the static tables and so can be
used to set up the APIC and HPET.

## Contributing
Contributions are more than welcome! You can:
- Request features and give feedback - the API is still in its infancy and we're flexible to
supporting the needs of projects using the library.
- Write code - the ACPI spec is huge and there are bound to be things we don't support yet!
- Add documentation!
- Test it! If you're writing a kernel and test on real hardware, we'd like to know about any discrepancies
you find (many firmwares aren't compliant, and we'd like to deal with as many common issues as possible).

## Resources
- [The ACPI specification](http://www.uefi.org/sites/default/files/resources/ACPI%206_2_A_Sept29.pdf)
- [OSDev Wiki](https://wiki.osdev.org/ACPI)

## Licence
Acpi is dual-licenced under:
- Apache Licence, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

Unless you explicitly state otherwise, any contribution submitted for inclusion in this work by you,
as defined in the Apache-2.0 licence, shall be dual licenced as above, without additional terms or
conditions.
