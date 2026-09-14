DefinitionBlock("while.aml", "DSDT", 1, "RSACPI", "WHILE", 1) {
    Name(FCNT, 0)

    Method (CHEK, 2) {
        If (Arg0 != Arg1) {
            FCNT++
        }
    }

    Method(T1) {
        Name(X, 0)
        While (X < 5) {
            X++
        }

        CHEK(X, 5)
    }

    Method(T2) {
        // Test `DefBreak` - Y should only make it to 5
        Name(Y, 0)
        While (Y < 10) {
            If (Y >= 5) {
                Break
            }

            Y++
        }
        CHEK(Y, 5)
    }

    Method(T3) {
        // Test `DefContinue` - Z should remain at zero
        Name(CNT, 0)
        Name(Z, 0)
        While (CNT < 5) {
            CNT++
            Continue
            Z++
        }
        CHEK(Z, 0)
        CHEK(CNT, 5)
    }

    Method(T4) {
        // Test `Decrement` in the predicate - common pattern
        Local0 = 5
        While (Local0--) {
            Continue
        }
        CHEK(Local0, 0)
    }

    Method(MAIN) {
        T1()
        T2()
        T3()
        T4()

        Return(FCNT)
    }
}
