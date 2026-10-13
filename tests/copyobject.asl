// These tests are based on the following table:
// [Object Storing and Copying Rules](https://uefi.org/specs/ACPI/6.6/19_ASL_Reference.html#object-storing-and-copying-rules)
//
// Note: Tests are in a fairly arbitrary order of increasing "mental complexity". Which row of the above table they
// relate to is given in brackets.

// TODO: Check that a suitable error occurs for copying non-integer/buffer to fields. (as described in row 3 of the
// table)
DefinitionBlock ("", "DSDT", 2, "RSCAPI", "CPYOBJCT", 0xF0F0F0F0) {
    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            Printf("Wrong result, %o != %o", ToHexString(Arg0), ToHexString(Arg1))
            FCNT++
        }
    }

    // Straightforward tests with a named object (row 4 of the table)
    Method (T1) {
        Name(TST, "Hello")
        CopyObject(1, TST)
        CHEK(TST, 1)

        CopyObject("World", TST)
        CHEK(TST, "World")
    }

    // Arguments are rebound unless they are a RefOf. (row 1 of the table)
    Method (T2) {
        Name(TST, "String")
        T2IN(TST)
        CHEK(TST, "String")
    }

    Method (T2IN, 1) {
        CopyObject("Something else", Arg0)
    }

    // Arguments are not rebound if they are a RefOf (row 1 of the table)
    Method (T3) {
        Name(TST, "String")
        T3IN(RefOf(TST))
        CHEK(TST, "Something else")
    }

    Method (T3IN, 1) {
        CopyObject("Something else", Arg0)
    }

    // When doing CopyObject to Arg, multiple RefOfs are skipped over before rebinding (row 1 of the table)
    //
    // TODO: uACPI and Windows pass this test, but ACPICA does not.
    Method (T4) {
        Name(TST, "String")
        Local0 = RefOf(TST)
        T4IN(RefOf(Local0))
        CHEK(TST, "Something else")
    }

    Method (T4IN, 1) {
        CopyObject("Something else", Arg0)
    }

    // Local is overwritten (row 2 of the table)
    Method (T5) {
        Local0 = "String"
        CopyObject(1, Local0)
        CHEK(Local0, 1)
    }

    // Local is overwritten, even if it contains a reference (row 2 of the table)
    Method (T6) {
        Local0 = "String"
        Local1 = RefOf(Local0)
        CopyObject(1, Local1)
        CHEK(Local0, "String")
        CHEK(Local1, 1)
    }

    Name(BUF, Buffer (12) { 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c })

    CreateDwordField(BUF, 0, DWRD)
    CreateWordField(BUF, 1, WORD)

    // Copy to buffer fields works (row 3 of the table)
    // NB: Field Units cannot be properly tested in the ASL / aml-tester tests as reads there always return 0. They
    // could be tested in the Rust integration tests if needed.
    Method (T7) {
        CopyObject(0xFFFFFFFF, WORD)

        CHEK(WORD, 0xFFFF)
        CHEK(DWRD, 0x04FFFF01)
    }

    Method (MAIN) {
        T1()
        T2()
        T3()
        T4()
        T5()
        T6()
        T7()

        Return (FCNT)
    }
}
