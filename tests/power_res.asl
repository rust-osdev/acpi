DefinitionBlock("power_res.aml", "DSDT", 1, "RSACPI", "PWRRES", 1) {
	Scope(_SB) {
		PowerResource(PIDE, 0, 1) {
			Name(X, Zero)
			Method(_STA) {
				Return (X)
			}
			Method(_ON) {
				Store(One, X)
			}
			Method(_OFF) {
				Store(Zero, X)
			}
		}
	}
}

