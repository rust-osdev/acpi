DefinitionBlock("package.aml", "DSDT", 1, "RSACPI", "PACKGE", 1) {
    Name(FOO, Package (15) {
        0x01,
        0x02,
        0x03,
        "Hello, World!",
        0x05,
        0x06,
        0x07,
        0x08,
        0x09,
        0x0a,
    })

    Name(BAR, Package (5) {
        Package { 0x01, 0x02, 0x03 },
        Package { 0x04, 0x05, 0x06 },
        Package { 0x07, 0x08, 0x09 },
        Package { 0x0a, 0x0b, 0x0c },
        Package { 0x0d, 0x0e, 0x0f },
    })

    Name(LEN, 5)
    Name(BAL, Package(LEN) {
        1,
        2,
        3,
        4,
    })

    LEN = 10
    Name(BAZ, Package (LEN) {
        4,
        11,
        16,
    })
}
