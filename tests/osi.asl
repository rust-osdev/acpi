// Check a variety of _OSI parameters and formats. These are a fairly random selection.
//
// Most of these call _OSI by reference, but also check that a native string works
//
// Adapted from the uACPI test of the same name.
DefinitionBlock ("", "DSDT", 2, "RSCAPI", "OSI", 0xF0F0F0F0)
{
    Method (CHEK, 2) {
        Local0 = _OSI(Arg0)

        If (Local0 != Arg1) {
            Printf("_OSI(%o) failed (expected %o, got %o)", Arg0, Arg1, Local0)
            Return (1)
        }

        Return (0)
    }

    Method (MAIN, 0, NotSerialized)
    {
        Local0 = 0

        If (!CondRefOf(_OSI)) {
            Debug = "No _OSI method!"
            Return (1111111)
        }

        if (_OSI("Windows 2000") != Ones) {
            Local0 += 1
        }

        Local0 += CHEK("Windows 2000", Ones)
        Local0 += CHEK("Windows 2001", Ones)
        Local0 += CHEK("Windows 2001 SP1", Ones)
        Local0 += CHEK("Windows 2001.1", Ones)
        Local0 += CHEK("Windows 2001 SP2", Ones)
        Local0 += CHEK("Windows 2001.1 SP1", Ones)
        Local0 += CHEK("Windows 2006.1", Ones)
        Local0 += CHEK("Windows 2006 SP1", Ones)
        Local0 += CHEK("Windows 2006 SP2", Ones)
        Local0 += CHEK("Windows 2009", Ones)
        Local0 += CHEK("Windows 2012", Ones)
        Local0 += CHEK("Windows 2013", Ones)
        Local0 += CHEK("Windows 2015", Ones)
        Local0 += CHEK("Windows 2016", Ones)
        Local0 += CHEK("Windows 2017", Ones)
        Local0 += CHEK("Windows 2017.2", Ones)
        Local0 += CHEK("Windows 2018", Ones)
        Local0 += CHEK("Windows 2018.2", Ones)
        Local0 += CHEK("Windows 2019", Ones)

        Local0 += CHEK("Processor Device", Zero)
        Local0 += CHEK("", Zero)
        Local0 += CHEK("Windows 99999", Zero)
        Local0 += CHEK("Windows 2014", Zero)
        Local0 += CHEK("Linux", Zero)

        Return (Local0)
    }
}
