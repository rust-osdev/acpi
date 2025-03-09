DefinitionBlock("fields.aml", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
    OperationRegion(MEM, SystemMemory, 0x40000, 0x1000)
    Field(MEM, WordAcc, NoLock, Preserve) {
        , 7,
        A, 15,
        , 8,
        B, 1,
        , 1,
        C, 35
    }

    OperationRegion(GIO0, SystemIO, 0x125, 0x100)

    Field(GIO0, ByteAcc, NoLock, Preserve) {
        GLB1, 1,
        GLB2, 1,
        Offset(1),
        BNK1, 4,

        Offset(2),
        IDX0, 8,
        DAT0, 8
    }

    BankField(GIO0, BNK1, 0, ByteAcc, NoLock, Preserve) {
        Offset(0x30),
        FET0, 1,
        FET1, 1
    }

    BankField(GIO0, BNK1, 1, ByteAcc, NoLock, Preserve) {
        Offset(0x30),
        BLVL, 7,
        BAC, 1
    }

    IndexField(IDX0, DAT0, ByteAcc, NoLock, Preserve) {
        FET2, 1,
        FET3, 1,
        Offset(0x2f),
        , 7,
        FET4, 1
    }

    Name(RESA, 0)
    Name(RESB, 0)
    Name(RESC, 0)
    Method(MAIN, 0, NotSerialized) {
        RESA = A
        RESB = B
        RESC = C
    }
}
