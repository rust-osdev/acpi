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
        Local0 = Sizeof (FOO)
        Local1 = Sizeof (FOO[0]) // This was what failed in issue 282.
        Return ((Local0 != 3) || (Local1 != 1))
    }
}
