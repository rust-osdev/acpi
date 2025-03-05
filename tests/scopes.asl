DefinitionBlock("scopes.aml", "DSDT", 1, "RSACPI", "SCOPES", 1) {
    Scope(_SB) {
        Name(X, 320)
        Name(Y, Zero)

        Device(PCI0) {
            Name(Y, 15)
            Name (_HID, EisaId ("PNP0A03"))
            Scope(\) {
                Name(Z, 413)
            }
        }

        Device(FOO) {
            Name (_HID, EisaId ("PNP0A03"))
            Alias (\_SB.PCI0.Y, MY_Y)
        }
    }
}
