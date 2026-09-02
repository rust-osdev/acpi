mod test_infra;

use crate::test_infra::{evaluate, run_aml_test};
use aml_test_tools::handlers::null_handler::NullHandler;

// regression test for 31b3057, ensure ConcatenateRestTemplate does
// not concatenate end tags
#[test]
fn concatenate_res_template_strip_end_tags() {
    const AML: &str = r#"
DefinitionBlock ("", "SSDT", 1, "LENOVO", "Platform", 0x00001000) {
    Scope (\_SB) {
        Device (TPD0) {
            Name (_ADR, One)
            Name (_CID, "PNP0C50")
            Name (_UID, 0x03)
            Name (_S0W, 0x04)
            Method (_HID, 0, NotSerialized)
            {
                Return ("ELAN0630")
            }
            Name (EIC0, ResourceTemplate ()
            {
                I2cSerialBusV2 (0x0015, ControllerInitiated, 0x00061A80,
                    AddressingMode7Bit, "",
                    0x00, ResourceConsumer, , Exclusive,
                    )
            })
            Name (TINT, ResourceTemplate ()
            {
                GpioInt (Level, ActiveLow, ExclusiveAndWake, PullDefault, 0x0000,
                    "", 0x00, ResourceConsumer, ,
                    )
                    {   // Pin list
                        0x001D
                    }
            })
            Method (_CRS, 0, NotSerialized)
            {
                Return (ConcatenateResTemplate (EIC0, TINT))
            }

        }
    }
}
"#;

    let interpreter = run_aml_test(AML, NullHandler {});
    let crs = evaluate(&interpreter, "\\_SB.TPD0._CRS");
    let buffer = crs.as_buffer().unwrap();

    assert_eq!(1, buffer.iter().filter(|b| **b == 0x79).count());

    // additional regression test for 9748050 where the end tag value was
    // incorrectly 0x78 vs 0x78|1
    assert_eq!(0, buffer.iter().filter(|b| **b == 0x78).count());
}
