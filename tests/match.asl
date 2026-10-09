DefinitionBlock("", "DSDT", 1, "RSACPI", "MATCH", 1) {
    Name (AAAA, Package (4) { 0x00, 0x01, 0x02, 0x03 })
    Method (MAIN) {
        Local0 = Match (AAAA, MEQ, 2, MTR, 0, 0)

        Return (!(Local0 == 2))
    }
}
