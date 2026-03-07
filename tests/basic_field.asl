DefinitionBlock("basic_field.aml", "DSDT", 1, "RSACPI", "BASFLD", 1) {
    OperationRegion(MEM, SystemIO, 0x40, 0x50)
    Field(MEM, WordAcc, NoLock, Preserve) {
        A, 7,
        B, 8
    }

    Method(MAIN, 0, NotSerialized) {
        A = 4
        B = A

        Return (0)
    }
}