DefinitionBlock("external.aml", "DSDT", 1, "RSACPI", "EXTRL", 1) {
	Scope(_SB) {
		External(FOO, MethodObj, IntObj, {IntObj})
	}

	Name(X, Zero)
	Method(BAR) {
		Store(\_SB.FOO(4), X)
	}
}
