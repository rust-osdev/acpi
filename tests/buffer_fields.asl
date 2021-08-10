DefinitionBlock("buffer_fields.aml", "DSDT", 1, "RSACPI", "BUFFLD", 1) {
	Name(X, Buffer (16) { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f })
	CreateBitField(X, 3, BIT3)
	CreateField(X, 0, 3, BITS)
	CreateByteField(X, 1, BYTE)
	CreateWordField(X, 2, WORD)
	CreateDWordField(X, 4, DWRD)
	CreateQWordField(X, 8, QWRD)
}
