DefinitionBlock("incdec.aml", "DSDT", 1, "RSACPI", "INCDEC", 1) {
    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            FCNT++
            Printf("Wrong result, %o != %o", ToHexString(Arg0), ToHexString(Arg1))
        }
    }

    // Plain integers
    Method (T1) {
        Name(X, 0)
        X++
        X++
        X++


        Name(Y, 0)
        Y = X
        X--

        CHEK(Y, 3)
        CHEK(X, 2)
    }

    // Basic buffer fields
    Method (T2) {
        Name(BUF, Buffer (2) { 0x00, 0x00 })
        CreateWordField(BUF, 0, X)
        X++

        CHEK(ToInteger(X), 1)
    }

    Method (MAIN) {
        T1()
        T2()

        Return (FCNT)
    }
}
