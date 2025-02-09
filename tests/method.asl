DefinitionBlock("method.aml", "DSDT", 1, "RSACPI", "METHOD", 1) {
    Method(FOO, 0, NotSerialized) {
        Return (0xff)
    }

    Name(X, 0)
    X = FOO()
}
