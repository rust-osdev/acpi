DefinitionBlock("", "DSDT", 1, "RSACPI", "METHOD", 1) {
    Name (AAAA, Package (1) { 0xaa })
    Method (MAIN) {
        Local0 = Match (AAAA, MEQ, 2, MTR, 0, 0)
    }
}