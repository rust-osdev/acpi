DefinitionBlock("while.aml", "DSDT", 1, "RSACPI", "WHILE", 1) {
	Name(X, 0)
	While (X < 5) {
		X++
	}

	// Test `DefBreak` - Y should only make it to 5
	Name(Y, 0)
	While (Y < 10) {
		If (Y >= 5) {
			Break
		}

		Y++
	}

	// Test `DefContinue` - Z should remain at zero
	Name(CNT, 0)
	Name(Z, 0)
	While (CNT < 5) {
		CNT++
		Continue
		Z++
	}
}
