// Check for issue 294 (logical-not broken)
DefinitionBlock("", "DSDT", 1, "RSACPI", "LOGICNOT", 1) {
    Method (MAIN, 0, NotSerialized) {
        Return (!1)
    }
}
