// Check that store handles simple references correctly
//
// Tests with a name that begins with a T are written by us for this crate. Tests with names that begin with a U have
// been adapted from the uACPI test case named in the associated comment.
//
// Tests T1 - T4 are very basic, to ensure any trivial errors in `do_store` are caught.
//
// These tests don't check any conversions - it's assumed that references and conversions are orthogonal.
DefinitionBlock ("", "DSDT", 2, "RSACPI", "TESTTABL", 0xF0F0F0F0)
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

    // uACPI equivalent: references-0.asl
    Method (U1) {
        Local0 = "MyString"
        INR5(Local0) // Arg0 = 5
        CHEK (Local0, "MyString")
    }

    // uACPI equivalent: references-3.asl. To quote that test:
    // "This test seems bogus but it's actually correct, it produces the same output on NT."
    Method (U2)
    {
        Local0 = "MyST"
        U2IN(Local0) // Local0 = RefOf(Arg0)
        CHEK(Local0, "WHY?")
    }

    Method (U2IN, 1, NotSerialized)
    {
        Local0 = RefOf(Arg0)

        // WHY? in little-endian ASCII
        Local0 = 0x3F594857
    }

    // This is the same as `U2` but with an extra function call to see if the Windows behaviour is
    // limited to one level of the stack - but it is not, multiple calls behave the same as a 
    // single call.
    Method (U2A) {
        Local0 = "MyST"
        IN2A(Local0)
        CHEK(Local0, "WHY?")
    }

    Method (IN2A, 1, NotSerialized) {
        U2IN(Arg0)
    }

    // uACPI test equivalent: references-4
    // Test U2 not withstanding, non-string "pass by value" argument types show the expected behavior.
    Method (U3) {
       Local0 = 1
       U3IN(Local0)
       CHEK(Local0, 1)
    }

    Method (U3IN, 1) {
        Local0 = RefOf(Arg0)
        Local0 = 9
    }

    // uACPI equivalent: references-8
    METHOD(U4) {
        Local0 = "MyString"
        U4IN(RefOf(Local0))
        CHEK(Local0, 0xDEADBEEF)
    }

    Method (U4IN, 1, NotSerialized)
    {
        Store(0xDEADC0DE, Arg0)
        Store(0xDEADBEEF, Arg0)
    }

    // uACPI test equivalent: references-9
    Method (U5, 0, NotSerialized)
    {
        Local0 = 0xDEADC0DEDEADBEEF
        U5IN(RefOf(Local0))
        CHEK (Local0, 0x676E6F6C79726576)
    }

    Method (U5IN, 1, NotSerialized)
    {
        Local0 = RefOf(Arg0)
        Local0 = "verylongstringbiggerthanint"
    }

    // uACPI test equivalent: references-10
    Method (U6, 0, NotSerialized)
    {
        Local0 = 0x1000
        U6IN(RefOf(Local0), 10)
        CHEK (Local0, 0x100A)
    }

    Method (U6IN, 2, NotSerialized)
    {
        Local0 = RefOf(Arg0)
        Local0++

        Debug = Arg1
        Debug = DerefOf(Local0)
        If (Arg1--) {
            U6IN(RefOf(Arg0), Arg1)
        }
    }

    Method (MAIN, 0, NotSerialized) {
        T1()
        T2()
        T3()
        T4()
        T5()
        T6()
        T7(0)

        // uACPI equivalents given in the comments:
        U1() // references-0
        U2() // references-3
        U2A() // An extra test with another layer of indirection
        U3() // references-4
        U4() // references-8
        U5() // references-9
        U6() // references-10

        Return (FCNT)
    }
}
