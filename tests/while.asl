DefinitionBlock("while.aml", "DSDT", 1, "RSACPI", "WHILE", 1) {
	Name(X, 0)
	While (X < 5) {
		X += 1
	}

	// Test `DefBreak` - Y should only make it to 5
	Name(Y, 0)
	While (Y < 10) {
		If (Y >= 5) {
			Break
		}

		Y += 1
	}
}
