DefinitionBlock("thermal_zone.aml", "DSDT", 1, "RSACPI", "THERMZ", 1) {
    Scope(_SB) {
        Device(EC0) {
            Name(_HID, EISAID("PNP0C09"))
            OperationRegion(EC0, EmbeddedControl, 0, 0xFF)
            Field(EC0, ByteAcc, Lock, Preserve) {
                MODE, 1, // thermal policy (quiet/perform)
                FAN, 1, // fan power (on/off)
                , 6, // reserved
                TMP, 16, // current temp
                AC0, 16, // active cooling temp (fan high)
                , 16, // reserved
                PSV, 16, // passive cooling temp
                HOT, 16, // critical S4 temp
                CRT, 16 // critical temp
            }
        }

        Device(CPU0) {
            Name(_HID, "ACPI0007")
            Name(_UID, 1) // unique number for this processor
        }

        ThermalZone(TZ0) {
            Method(_TMP) { Return (\_SB.EC0.TMP )} // get current temp
            Method(_AC0) { Return (\_SB.EC0.AC0) } // fan high temp
            Name(_AL0, Package(){\_SB.EC0.FAN}) // fan is act cool dev
            Method(_PSV) { Return (\_SB.EC0.PSV) } // passive cooling temp
            Name(_PSL, Package (){\_SB.CPU0}) // passive cooling devices
            Method(_HOT) { Return (\_SB.EC0.HOT) } // get critical S4 temp
            Method(_CRT) { Return (\_SB.EC0.CRT) } // get critical temp
            Method(_SCP, 1) { Store (Arg0, \_SB.EC0.MODE) } // set cooling mode
            Name(_TC1, 4) // bogus example constant
            Name(_TC2, 3) // bogus example constant
            Name(_TSP, 150) // passive sampling = 15 sec
            Name(_TZP, 0) // polling not required
            Name (_STR, Unicode ("System thermal zone"))
        }
    }
}
