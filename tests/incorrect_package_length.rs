mod test_infra;

use crate::test_infra::run_opcodes_test;
use aml_test_tools::handlers::null_handler::NullHandler;

#[test]
fn package_with_overlong_length_does_not_consume_following_term() {
    // Name (AAAA, Package (2) { 0xA1, 0xA2 }) but the package length is four bytes too long.
    // The next term is Name (BBBB, Package (2) { 0xB1, 0xB2 }).
    let opcodes = [
        0x08, b'A', b'A', b'A', b'A', 0x12, 0x0A, 0x02, 0x0A, 0xA1, 0x0A, 0xA2, 0x08, b'B', b'B', b'B', b'B',
        0x12, 0x06, 0x02, 0x0A, 0xB1, 0x0A, 0xB2, 0xA4, 0x92, 0x93, 0x87, b'B', b'B', b'B', b'B', 0x0A, 0x02,
    ];

    // Return zero only if the following Name term was parsed and contains two elements.
    run_opcodes_test(&opcodes, NullHandler);
}
