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
}
