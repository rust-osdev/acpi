DefinitionBlock("object_type.aml", "DSDT", 1, "RSACPI", "OBJTYP", 1) {
	Name(INT, 723)
	Name(STR, "Hello, World!")
	Name(BUFF, Buffer { 7, 2, 3, 5, 92, 6 })
	// TODO: more types

	Device(TYPS) {
		// This is just so it compiles
		Name (_HID, EisaId ("PNP0A03"))

		Name(INT, 0)	// Should be `1`
		Name(STR, 0)	// Should be `2`
		Name(BUFF, 0)	// Should be `3`

		INT = ObjectType(\INT)
		STR = ObjectType(\STR)
		BUFF = ObjectType(\BUFF)
	}
}
