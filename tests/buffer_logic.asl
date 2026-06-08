// Check correct logic operations on Buffers - see issue #297
// These tests have been checked against uACPI commit 71f981d
DefinitionBlock("", "DSDT", 1, "RSACPI", "Buffers", 1) {
    // Buffers longer than integer size to check all bytes are compared, not just the first integer-size bytes.
    Name(BUF0, Buffer() { 0x01, 0x02, 0x03, 0x04, 0x05 })
    Name(BUF1, Buffer() { 0x01, 0x02, 0x03, 0x04, 0x05 })
    Name(BUF2, Buffer() { 0x01, 0x02, 0x03, 0x04, 0x06 })

    // Buffers integer size and less
    NAME(BUF3, Buffer() { 0x01, 0x02, 0x03, 0x04 })
    NAME(BUF4, Buffer() { 0x01, 0x02, 0x03, 0x05 })
    NAME(BUF5, Buffer() { 0x01, 0x02, 0x03 })

    // Zero buffer for checking and/or operators
    NAME(ZBUF, Buffer() { 0x00 })

    // Very long zero buffer to check only 4 bytes are used for and/or operators
    NAME(LZBF, Buffer() { 0x00, 0x00, 0x00, 0x00, 0x01 })

    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (!Arg0) {
            FCNT++
            Printf("Failure on line %o", ToDecimalString(Arg1))
        }
    }

    Method(MAIN) {
        CHEK(BUF0 == BUF1, __LINE__)
        CHEK(!(BUF0 != BUF1), __LINE__)

        CHEK(BUF3 != BUF1, __LINE__)
        CHEK(!(BUF3 == BUF1), __LINE__)

        CHEK(BUF3 != BUF4, __LINE__)
        CHEK(!(BUF3 == BUF4), __LINE__)

        CHEK(BUF5 != BUF3, __LINE__)
        CHEK(!(BUF5 == BUF3), __LINE__)

        CHEK(BUF1 > BUF3, __LINE__)
        CHEK(!(BUF3 > BUF1), __LINE__)

        CHEK(BUF4 > BUF3, __LINE__)
        CHEK(!(BUF3 > BUF4), __LINE__)

        CHEK(BUF3 < BUF4, __LINE__)
        CHEK(!(BUF4 < BUF3), __LINE__)

        CHEK(BUF3 < BUF1, __LINE__)
        CHEK(!(BUF1 < BUF3), __LINE__)

        CHEK(BUF3 >= BUF3, __LINE__)
        CHEK(!(BUF3 >= BUF4), __LINE__)

        CHEK(BUF4 >= BUF3, __LINE__)
        CHEK(!(BUF3 >= BUF4), __LINE__)

        CHEK(BUF3 <= BUF3, __LINE__)
        CHEK(!(BUF4 <= BUF3), __LINE__)

        CHEK(BUF3 <= BUF4, __LINE__)
        CHEK(!(BUF4 <= BUF3), __LINE__)

        CHEK(BUF3 && BUF3, __LINE__)
        CHEK(!(ZBUF && BUF3), __LINE__)
        CHEK(!(BUF3 && ZBUF), __LINE__)
        CHEK(!(ZBUF && ZBUF), __LINE__)

        CHEK(BUF3 || BUF3, __LINE__)
        CHEK(ZBUF || BUF3, __LINE__)
        CHEK(BUF3 || ZBUF, __LINE__)
        CHEK(!(ZBUF || ZBUF), __LINE__)

        CHEK(!(LZBF && LZBF), __LINE__)

        Return(FCNT)
    }
}
