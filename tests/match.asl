DefinitionBlock("", "DSDT", 1, "RSACPI", "MATCH", 1) {
    Name (AAAA, Package (4) { 0x01, 0x01, 0x02, 0x03 })
    Name (BBBB, Package (4) { 0x01, 0x01, 0x01, 0x01 })

    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            FCNT++
        }
    }

    Method (MAIN) {
        // Test all operations in first position.
        CHEK(2, Match (AAAA, MEQ, 2, MTR, 0, 0))
        CHEK(Ones, Match (AAAA, MLT, 1, MTR, 0, 0))
        CHEK(0, Match (AAAA, MLE, 1, MTR, 0, 0))
        CHEK(3, Match (AAAA, MGT, 2, MTR, 0, 0))
        CHEK(2, Match (AAAA, MGE, 2, MTR, 0, 0))

        // Test all operations in second position.
        CHEK(2, Match (AAAA, MTR, 0, MEQ, 2, 0))
        CHEK(Ones, Match (AAAA, MTR, 0, MLT, 1, 0))
        CHEK(0, Match (AAAA, MTR, 0, MLE, 1, 0))
        CHEK(3, Match (AAAA, MTR, 0, MGT, 2, 0))
        CHEK(2, Match (AAAA, MTR, 0, MGE, 2, 0))

        // Check offset works
        CHEK(2, Match (BBBB, MEQ, 1, MTR, 0, 2))
        CHEK(Ones, Match (BBBB, MEQ, 2, MTR, 0, 0))

        Return (FCNT)
    }
}
