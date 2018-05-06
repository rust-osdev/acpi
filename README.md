# Acpi
[![Build Status](https://travis-ci.org/rust-osdev/acpi.svg?branch=master)](https://travis-ci.org/rust-osdev/acpi)
[![Version](https://img.shields.io/crates/v/acpi.svg?style=rounded-square)](https://crates.io/crates/acpi/)

A library to parse ACPI tables and AML, written in Rust. Designed to be easy to use from inside a
kernel written in Rust, and fully tested.
**Acpi is currently very early in development, will be highly unstable and is next to useless for
actually parsing ACPI or AML.**

## Using
`acpi` uses the nightly channel, and currently builds on `rustc 1.27.0-nightly (7925ff4e 2018-04-19)`.
If `acpi` fails to build on a later nightly, please file an issue!

## Contributing
Contributions are more than welcome! You can:
- Write code - the ACPI spec is huge and there are bound to be things we don't support yet!

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
