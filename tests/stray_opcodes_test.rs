mod test_infra;

use crate::test_infra::run_opcodes_test;
use aml_test_tools::handlers::null_handler::NullHandler;

#[test]
fn test_stray_opcode() {
    // Make sure that a stray `One` opcode is ignored (see issue #289)
    // The first opcode is `One`, the remainder are `Return (0)`.
    let opcodes: Vec<u8> = vec![0x01, 0xA4, 0x0A, 0x00];

    let handler = NullHandler;
    run_opcodes_test(&opcodes, handler);
}
