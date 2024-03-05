DefinitionBlock("inc.aml", "DSDT", 1, "RSACPI", "INCDEC", 1) {
    Name(X, 0)
    X++
    X++
    X++
    Name(Y, 0)
    Y = X
    X--
}
