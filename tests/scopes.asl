DefinitionBlock("scopes.aml", "DSDT", 1, "RSACPI", "SCOPES", 1) {
	Scope(_SB) {
		Name(X, 320)
		Device(PCI0) {
			Name(Y, 15)
			Name (_HID, EisaId ("PNP0A03"))
			Scope(\) {
				Name(Z, 413)
			}
		}
	}
}
