// Test operations on "Index" fields

use aml_test_tools::handlers::std_test_handler::{
    Command,
    construct_std_handler,
    create_mutex,
    read_u8,
    read_u16,
    write_u8,
    write_u16,
};

mod test_infra;

#[test]
fn test_basic_index_store_and_load_8_bit() {
    // In this test, the data register has the same width as the fields and all fields are correctly
    // aligned. We should see single reads and writes for each store operation.
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        OFFSET(0x10),
        INDX, 8,
        DATA, 8
    }

    IndexField (INDX, DATA, ByteAcc, NoLock, Preserve) {
        A, 8,
        B, 8,
        C, 8
    }

    Method(MAIN, 0, NotSerialized) {
        C = 0xA5
        A = 0x5A
        B = A
        C = A
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // C = 0xA5
        write_u8(0x40010, 0x02), // Set index to point at C
        write_u8(0x40011, 0xA5), // Set C = 0xA5
        // A = 0x5A
        write_u8(0x40010, 0x00), // Set index to point at A
        write_u8(0x40011, 0x5A), // Set A = 0x5A
        // Read A
        write_u8(0x40010, 0x00), // etc.
        read_u8(0x40011, 0x5A),
        // B = A
        write_u8(0x40010, 0x01),
        write_u8(0x40011, 0x5A),
        // Read A
        write_u8(0x40010, 0x00),
        read_u8(0x40011, 0x5A),
        // C = A
        write_u8(0x40010, 0x02),
        write_u8(0x40011, 0x5A),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_basic_index_store_and_load_16_bit() {
    // In this test, the data register has the same width as the fields and all fields are correctly
    // aligned. We should see single reads and writes for each store operation.
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, WordAcc, NoLock, Preserve) {
        OFFSET(0x20),
        INDX, 16,
        DATA, 16
    }

    IndexField (INDX, DATA, WordAcc, NoLock, Preserve) {
        A, 16,
        B, 16,
        C, 16
    }

    Method(MAIN, 0, NotSerialized) {
        C = 0xA5B6
        A = 0x5A6B
        B = A
        C = A
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // C = 0xA5B6
        write_u16(0x40020, 0x04),   // Set index to point at C
        write_u16(0x40022, 0xA5B6), // Set C = 0xA5B6
        // A = 0x5A6B
        write_u16(0x40020, 0x00),   // Set index to point at A
        write_u16(0x40022, 0x5A6B), // Set A = 0x5A6B
        // Read A
        write_u16(0x40020, 0x00), // etc.
        read_u16(0x40022, 0x5A6B),
        // B = A
        write_u16(0x40020, 0x02),
        write_u16(0x40022, 0x5A6B),
        // Read A
        write_u16(0x40020, 0x00),
        read_u16(0x40022, 0x5A6B),
        // C = A
        write_u16(0x40020, 0x04),
        write_u16(0x40022, 0x5A6B),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_index_multiple_aligned_reads() {
    // In this test, the data register is narrower than the fields, so multiple data register
    // reads are needed to access each field.
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        OFFSET(0x04),
        INDX, 8,
        DATA, 8
    }

    IndexField (INDX, DATA, ByteAcc, NoLock, Preserve) {
        A, 16,
        B, 16,
        C, 16
    }

    Method(MAIN, 0, NotSerialized) {
        C = 0xA5B6
        B = C
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // C = 0xA5B6
        write_u8(0x40004, 0x04), // Set index to point at C (low byte)
        write_u8(0x40005, 0xB6), // Set low C = 0xB6
        write_u8(0x40004, 0x05), // Set index to point at C (high byte)
        write_u8(0x40005, 0xA5), // Set low C = 0xA5
        // B = C. Read C
        write_u8(0x40004, 0x04),
        read_u8(0x40005, 0xB6),
        write_u8(0x40004, 0x05),
        read_u8(0x40005, 0xA5),
        // Write B
        write_u8(0x40004, 0x02),
        write_u8(0x40005, 0xB6),
        write_u8(0x40004, 0x03),
        write_u8(0x40005, 0xA5),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_index_narrower_than_data() {
    // In this test, the access width of the index field is smaller than the data register. Even
    // though it looks as though individual 16-bit reads/writes would be OK, actually multiple
    // 8-bit reads/writes are needed to access each field. (Not intuitive, but matches ACPICA)
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, WordAcc, NoLock, Preserve) {
        OFFSET(0x06),
        INDX, 16,
        DATA, 16
    }

    IndexField (INDX, DATA, ByteAcc, NoLock, Preserve) {
        A, 16,
        B, 16,
        C, 16
    }

    Method(MAIN, 0, NotSerialized) {
        C = 0xA5B6
        B = C
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // C = 0xA5B6
        write_u16(0x40006, 0x04), // Set index to point at C, low byte
        write_u8(0x40008, 0xB6),  // Set C (low) = 0xB6
        write_u16(0x40006, 0x05), // Set index to point at C, high byte
        write_u8(0x40008, 0xA5),  // Set C (high) = 0xA5
        // Read C
        write_u16(0x40006, 0x04), // etc.
        read_u8(0x40008, 0xB6),
        write_u16(0x40006, 0x05),
        read_u8(0x40008, 0xA5),
        // B = C
        write_u16(0x40006, 0x02),
        write_u8(0x40008, 0xB6),
        write_u16(0x40006, 0x03),
        write_u8(0x40008, 0xA5),
    ];

    //let handler = NullHandler;
    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}

#[test]
fn test_index_misaligned_field() {
    // Check that we can successfully update non-aligned index fields.
    const AML: &str = r#"DefinitionBlock("", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        OFFSET(0x08),
        INDX, 8,
        DATA, 8
    }

    IndexField (INDX, DATA, ByteAcc, NoLock, Preserve) {
        SKIP, 4, // Nybbles make it easier to model mentally!
        A, 8,
        B, 8
    }

    Method(MAIN, 0, NotSerialized) {
        B = 0xB6
        A = B
        Return (0)
    }
}
"#;

    const EXPECTED_COMMANDS: &[Command] = &[
        create_mutex(),
        // B = 0xB6
        // Set index to point at B, low nybble. Which is actually the byte containing A's high
        // nybble as well.
        write_u8(0x40008, 0x01),
        // Read that byte so as to be able to preserve A's high nybble. Pretend that A's high nybble
        // is 0xA.
        read_u8(0x40009, 0x0A),
        // Write back A's high nybble and B's low nybble. This is as much as we test Preserve
        // behaviour in the index field tests - it's assumed that if it works for normal fields, it
        // works for index fields as well.
        write_u8(0x40009, 0x6A),
        // Set the index to point at B, high nybble.
        write_u8(0x40008, 0x02),
        // Read that byte, which also contains an unused nybble. (Which we've just set to zero)
        read_u8(0x40009, 0x00),
        // Write B's high nybble, preserving the unused zero nybble.
        write_u8(0x40009, 0x0B),
        // A = B. Start by reading B
        write_u8(0x40008, 0x01),
        read_u8(0x40009, 0x60),
        write_u8(0x40008, 0x02),
        read_u8(0x40009, 0x0B),
        // Set A, remembering that there are some reads needed to preserve the other nybbles.
        write_u8(0x40008, 0x00),
        read_u8(0x40009, 0x00),
        write_u8(0x40009, 0x60),
        write_u8(0x40008, 0x01),
        read_u8(0x40009, 0x60),
        write_u8(0x40009, 0x6B),
    ];

    let handler = construct_std_handler(EXPECTED_COMMANDS.to_vec());
    test_infra::run_aml_test(AML, handler);
}
