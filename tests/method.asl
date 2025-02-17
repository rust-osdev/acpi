DefinitionBlock("method.aml", "DSDT", 1, "RSACPI", "METHOD", 1) {
    Name(X, 5)

    Method(FOO, 0, NotSerialized) {
        If (X > 1) {
            Noop
        } Else {
            Return (0x55)
        }
        Return (0x3f)
    }

    Name(Y, 0)
    Y = FOO()
}
