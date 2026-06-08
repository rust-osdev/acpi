// Check correct logic operations on strings - see issue #297
// These tests have been checked against uACPI commit 71f981d
DefinitionBlock("", "DSDT", 1, "RSACPI", "Strings", 1) {
    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (!Arg0) {
            FCNT++
            Printf("Failure on line %o", ToDecimalString(Arg1))
        }
    }

    Method(MAIN) {
        CHEK("Hello" != "Hell", __LINE__)
        CHEK(!("Hello" == "Hell"), __LINE__)

        CHEK("Hella" != "Hello", __LINE__)
        CHEK(!("Hello" != "Hello"), __LINE__)

        CHEK("Hello" == "Hello", __LINE__)
        CHEK(!("Hello" == "Hella"), __LINE__)

        CHEK("AAAAB" > "AAAAA", __LINE__)
        CHEK(!("AAAAA" > "AAAAB"), __LINE__)

        CHEK("AAAAA" < "AAAAB", __LINE__)
        CHEK(!("AAAAB" < "AAAAA"), __LINE__)

        CHEK("AAAAA" >= "AAAAA", __LINE__)
        CHEK(!("AAAAA" >= "AAAAB"), __LINE__)

        CHEK("AAAAB" >= "AAAAA", __LINE__)
        CHEK(!("AAAAA" >= "AAAAB"), __LINE__)

        CHEK("AAAAA" <= "AAAAA", __LINE__)
        CHEK(!("AAAAB" <= "AAAAA"), __LINE__)

        CHEK("AAAAA" <= "AAAAB", __LINE__)
        CHEK(!("AAAAB" <= "AAAAA"), __LINE__)

        // Strings are interpreted as integers.
        CHEK("1" && "2", __LINE__)
        CHEK(!("" && "1"), __LINE__)
        CHEK(!("1" && ""), __LINE__)
        CHEK(!("" && ""), __LINE__)

        CHEK("1" || "2", __LINE__)
        CHEK("" || "1", __LINE__)
        CHEK("1" || "", __LINE__)
        CHEK(!("" || ""), __LINE__)

        Return(FCNT)
    }
}
