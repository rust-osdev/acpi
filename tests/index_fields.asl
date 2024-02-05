DefinitionBlock("index_fields.aml", "DSDT", 1, "RSACPI", "IDXFLD", 1) {
   OperationRegion (GIO0, SystemIO, 0x125, 0x100)

   Field (GIO0, ByteAcc, NoLock, WriteAsZeros) {
      IDX0, 8,
      DAT0, 8,
      IDX1, 8,
      DAT1, 16
   }

   IndexField (IDX0, DAT0, ByteAcc, NoLock, Preserve) {
      Offset(0x10),
      , 7,              // Skip to offset 0x10 bit 7
      REG0, 2,          // Covers bit 7 in 0x10 and bit 0 in 0x11
   }

   IndexField (IDX1, DAT1, WordAcc, NoLock, Preserve) {
      Offset(0x07),     // Offset 0x06, bits 8..
      , 7,              // Skip to offset 0x06, bit 15
      REG1, 2,          // Covers bit 15 in 0x06 and bit 0 in 0x08
   }

   // Store 0b11 to REG0 (index 0x10, bit 0 + index 0x11, bit 1)
   Store (3, REG0)

   // Store 0b11 to REG1 (index 0x06, bit 15 + index 0x08, bit 0)
   Store (3, REG1)
}
