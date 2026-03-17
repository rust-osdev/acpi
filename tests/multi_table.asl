/* This file is a test for `aml_tester` rather than of the parser itself.
 * Can `aml_tester` cope with multiple tables?
 */
DefinitionBlock("", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, ByteAcc, NoLock, Preserve) {
        A, 8
    }
}
DefinitionBlock("", "SSDT", 1, "RSACPI", "BUFFLD", 1) {
    OperationRegion(MEMB, SystemMemory, 0x50000, 0x1000)
    Field(MEMB, ByteAcc, NoLock, Preserve) {
        B, 8
    }
}
