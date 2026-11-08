DefinitionBlock("divide.aml", "DSDT", 1, "RSACPI", "DIVIDE", 1) {
    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            FCNT++
            Printf("Wrong result, %o != %o", ToHexString(Arg0), ToHexString(Arg1))
        }
    }

    Method (MAIN) {
        // Variables, not constants: iasl would compute a constant division itself.
        Local0 = 17

        // Divide (Dividend, Divisor, Remainder, Result)
        Divide (Local0, 5, Local1, Local2)
        CHEK(Local1, 2)
        CHEK(Local2, 3)

        CHEK(Local0 / 5, 3)
        CHEK(Local0 % 5, 2)

        Return (FCNT)
    }
}
