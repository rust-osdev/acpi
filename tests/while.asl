DefinitionBlock("while.aml", "DSDT", 1, "RSACPI", "WHILE", 1) {
	Name(X, 0)
	While (X < 5) {
		X += 1
	}
}
