// Test operations on "Bank" fields

use aml_test_tools::handlers::std_test_handler::{
    Command,
    construct_std_handler,
    create_mutex,
    read_u16,
    write_u8,
    write_u16,
};

mod test_infra;

#[test]
fn test_basic_bank_store_and_load() {
    // This is a straightforward test of banked fields.
    // Internally: Apart from setting the bank index beforehand, the field read/write is identical
    // to normal fields. So this test is probably sufficient testing of banked fields.
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "BNKFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        INDX, 8
    }

    BankField (MEM, INDX, 0, WordAcc, NoLock, Preserve) {
        OFFSET (0x02), // Prevent aliasing INDX
        A, 16,
        B, 16,
        C, 16
    }

    BankField (MEM, INDX, 1, WordAcc, NoLock, Preserve) {
        OFFSET (0x02),
        D, 16,
        E, 16,
        F, 16
    }

    Method(MAIN, 0, NotSerialized) {
        C = 0xA55A
        D = C
        E = 0x5AA5
        A = E
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // C = 0xA55A
        write_u8(0x40000, 0),       // Select bank 0.
        write_u16(0x40006, 0xA55A), // Write the value to C.
        // D = C
        write_u8(0x40000, 0),       // Select bank 0.
        read_u16(0x40006, 0xA55A),  // Read the value from C.
        write_u8(0x40000, 1),       // Select bank 1.
        write_u16(0x40002, 0xA55A), // Write the value to D.
        // E = 0x5AA5
        write_u8(0x40000, 1),       // Select bank 1.
        write_u16(0x40004, 0x5AA5), // Write the value to E.
        // A = E
        write_u8(0x40000, 1),       // Select bank 1.
        read_u16(0x40004, 0x5AA5),  // Read from E
        write_u8(0x40000, 0),       // Select bank 0.
        write_u16(0x40002, 0x5AA5), // Write the value to A.
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}
