DefinitionBlock("buffer_fields.aml", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
    Name(X, Buffer (16) { 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff })
    CreateBitField(X, 3, BIT3)
    CreateField(X, 0, 3, BITS)
    CreateByteField(X, 1, BYTE)
    CreateWordField(X, 2, WORD)
    CreateDWordField(X, 4, DWRD)
    CreateQWordField(X, 8, QWRD)

    // `X` should end up as [0xff, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x00, 0x00]
    BIT3 = 1
    BITS = 7
    BYTE = 0x01
    WORD = 0x0302
    DWRD = 0x07060504
    // Last two bytes should be cleared because of zero-extension of this store
    // We do this as a buffer store a) to test them b) because `iasl` doesn't support 64-bit integer constants...
    QWRD = Buffer() { 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d }

    // `Y` should end up as `Integer(0x07060504)` (`Integer(117835012)` in decimal)
    Name(Y, 4)
    Y = DWRD

    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            FCNT++
        }
    }

    Method (T1) {
        // Fields up to 64 bits wide read as integers
        CHEK(WORD, 0x0302)
        CHEK(DWRD, 0x07060504)
    }

    Method (GETD, 1) {
        CreateDWordField(X, Arg0, FLD)
        Return (FLD)
    }

    Method (GETF, 2) {
        Local0 = Arg0
        Local1 = Arg1
        CreateField(X, Local0, Local1, FLD)
        Return (FLD)
    }

    Method (T2) {
        // The index and size of a field from an argument or a local
        CHEK(GETD(4), 0x07060504)
        CHEK(GETF(32, 16), 0x0504)
    }

    Method (MAIN) {
        T1()
        T2()
        Return (FCNT)
    }
}
