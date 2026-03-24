DefinitionBlock("", "DSDT", 1, "RSACPI", "PACKGE", 1) {
    // Checks fix for issue 282 - SizeOf on a reference to a package doesn't work.
    Name(FOO, Package (3) {
        Package (0x01) {
                0x1,
            },

        Package (0x01) {
            0x02,
        },

        Package (0x01) {
            0x03,
        }
    })

    Method (MAIN, 0, NotSerialized) {
        Local0 = Sizeof (FOO)    // Should be 3
        Local1 = Sizeof (FOO[0]) // Should be 1. This was the failing line.
        Return ((Local0 + Local1) - 4)
    }
}
