// Check that store handles simple references correctly
//
// Tests T1 - T4 are very basic, to ensure any trivial errors in `do_store` are caught.
//
// These tests don't check any conversions - it's assumed that references and conversions are orthogonal.
DefinitionBlock ("", "DSDT", 1, "RSACPI", "TESTTABL", 0xF0F0F0F0)
{
    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            FCNT++
        }
    }

    Method (T1) {
        Name(V1, 1)
        V1 = 2
        CHEK(V1, 2)
    }

    Method (T2) {
        Name(V1, 1)
        Alias(V1, V2)
        V2 = 2
        CHEK(V1, 2)
    }

    Method (T3) {
        Local1 = 1
        Local2 = Local1
        Local2 = 2
        CHEK(Local1, 1)
    }

    Method (T4) {
        Local1 = 1
        Local2 = RefOf(Local1)
        Local2 = 2
        CHEK(Local1, 2)
    }

    Method (INR5, 1) {
        Arg0 = 5
    }

    Method (T5) {
        Local1 = 1
        INR5(Local1)
        CHEK (Local1, 1)
    }

    Method (T6) {
        Local1 = 1
        INR5(RefOf(Local1))
        CHEK (Local1, 5)
    }

    Method (T7, 1) {
        Local1 = 1
        Arg0 = RefOf(Local1)
        Arg0 = 2
        CHEK (Local1, 2)
    }

    // Test 8 is adapted from uACPI's `references-3.asl`. To quote that test:
    // "This test seems bogus but it's actually correct, it produces the same output on NT."
    Method (INR8, 1, NotSerialized)
    {
        Local0 = RefOf(Arg0)

        // WHY? in little-endian ASCII
        Local0 = 0x3F594857
    }

    Method (T8)
    {
        Local0 = "MyST"
        INR8(Local0)
        CHEK(Local0, "WHY?")
    }

    // This is the same as `T8` but with an extra function call to see if the Windows behaviour is
    // limited to one level of the stack - but it is not, multiple calls behave the same as a 
    // single call.
    Method (T8A) {
        Local0 = "MyST"
        IN8A(Local0)
        CHEK(Local0, "WHY?")
    }

    Method (IN8A, 1, NotSerialized) {
        INR8(Arg0)
    }

    // Test 8 not withstanding, non-string "pass by value" argument types show the expected behavior.
    Method (INR9, 1) {
        Local0 = RefOf(Arg0)
        Local0 = 9
    }

    Method (T9) {
       Local0 = 1
       INR9(Local0)
       CHEK(Local0, 1)
    }

    Method (MAIN, 0, NotSerialized) {
        T1()
        T2()
        T3()
        T4()
        T5()
        T6()
        T7(0)
        T8()
        T8A()
        T9()

        Return (FCNT)
    }
}
