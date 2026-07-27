//! Tests for `NameString`s used as package elements, which are references to the named object and
//! are resolved lazily, as they may refer to objects that don't exist yet.

mod test_infra;

use crate::test_infra::{evaluate, run_aml_test};
use acpi::aml::{
    namespace::AmlName,
    object::Object,
    pci_routing::{PciRoutingTable, Pin},
};
use aml_test_tools::handlers::null_handler::NullHandler;
use std::str::FromStr;

/// A cut-down version of the `gpio-leds` `_DSD` from the AMD Rhino, which refers to the device the
/// package is declared in with a relative name.
#[test]
fn relative_name_in_package_refers_to_declaration_scope() {
    const AML: &str = r#"
DefinitionBlock("", "DSDT", 2, "COREBT", "RHINO", 1) {
    Scope (\_SB) {
        Device (LEDS) {
            Name (_HID, "PRP0001")
            Name (_UID, 0x05)
            Name (_DDN, "gpio-leds APU HeartBeat")
            Name (_DSD, Package () {
                ToUUID("daffd814-6eba-4d8c-8a91-bc9bbf4aa301"),
                Package () {
                    Package () {"compatible", Package () {"gpio-leds"}},
                },
                ToUUID("dbb8e3e6-5886-4ba6-8795-1319f52a966b"),
                Package () {
                    Package () {"led-0", "LED0"},
                }
            })
            Name (LED0, Package () {
                ToUUID("daffd814-6eba-4d8c-8a91-bc9bbf4aa301"),
                Package () {
                    Package () {"label", "heartbeat"},
                    Package () {"default-state", "on"},
                    Package () {"linux,default-trigger", "heartbeat"},
                    Package () {"gpios", Package () {^LEDS, 0, 0, 1}},
                    Package () {"retain-state-suspended", 1},
                }
            })
        }
    }
}
"#;

    let interpreter = run_aml_test(AML, NullHandler {});
    let led = evaluate(&interpreter, "\\_SB.LEDS.LED0");

    let Object::Package(top_level) = led else { panic!("LED0 is not a package") };
    let Object::Package(ref properties) = *top_level[1] else { panic!("LED0 properties are not a package") };
    let Object::Package(ref gpio_property) = *properties[3] else { panic!("GPIO property is not a package") };
    let Object::Package(ref gpio_ref) = *gpio_property[1] else { panic!("GPIO reference is not a package") };

    let Object::NamePath { ref name, ref scope } = *gpio_ref[0] else {
        panic!("GPIO reference is not a name: {}", *gpio_ref[0]);
    };

    // `^LEDS`, declared in `\_SB.LEDS`, refers to the device itself.
    assert_eq!(name.resolve(scope), Ok(AmlName::from_str("\\_SB.LEDS").unwrap()));
}

/// Names in packages may refer to objects that don't exist when the package is created, and must be
/// resolved against the scope the package was declared in, not the one it is used from.
#[test]
fn forward_reference_in_package_uses_declaration_scope() {
    const AML: &str = r#"
DefinitionBlock("", "DSDT", 2, "RSACPI", "PKGREF", 1) {
    Scope (\_SB) {
        Device (OWNR) {
            Name (_HID, "TEST0001")
            Name (PKG0, Package () { ^LATE })
        }

        Device (LATE) {
            Name (_HID, "TEST0002")
        }

        Device (USER) {
            Name (_HID, "TEST0003")

            Device (SUB0) {
                Name (_HID, "TEST0004")

                Method (GET0, 0, NotSerialized) {
                    Return (DerefOf (\_SB.OWNR.PKG0[0]))
                }

                Method (TYP0, 0, NotSerialized) {
                    Return (ObjectType (\_SB.OWNR.PKG0[0]))
                }

                Method (SIZ0, 0, NotSerialized) {
                    Return (SizeOf (\_SB.OWNR.PKG0[0]))
                }
            }
        }
    }
}
"#;

    let interpreter = run_aml_test(AML, NullHandler {});

    let Object::Package(elements) = evaluate(&interpreter, "\\_SB.OWNR.PKG0") else {
        panic!("PKG0 is not a package")
    };
    assert!(matches!(*elements[0], Object::NamePath { .. }));

    // `^LATE` is declared after the package that refers to it, and is used from another scope.
    assert!(matches!(evaluate(&interpreter, "\\_SB.USER.SUB0.GET0"), Object::Device));
    // `ObjectType` dereferences its operand, so this is a device (`6`), not a reference.
    assert!(matches!(evaluate(&interpreter, "\\_SB.USER.SUB0.TYP0"), Object::Integer(6)));
    // `SizeOf` also dereferences, and a device has no size.
    assert!(interpreter.evaluate(AmlName::from_str("\\_SB.USER.SUB0.SIZ0").unwrap(), vec![]).is_err());
}

/// Names in `_PRT` `Source` fields are package elements, and so are references to the link device.
#[test]
fn prt_source_name_is_resolved_to_a_link_object() {
    const AML: &str = r#"
DefinitionBlock("", "DSDT", 2, "RSACPI", "PRTTST", 1) {
    Scope (\_SB) {
        Device (LNKA) {
            Name (_HID, EisaId ("PNP0C0F"))
            Name (_UID, 1)
            Name (_CRS, ResourceTemplate () { IRQ (Level, ActiveLow, Shared) {11} })
        }

        Device (LNKB) {
            Name (_HID, EisaId ("PNP0C0F"))
            Name (_UID, 2)
            Name (_CRS, ResourceTemplate () { IRQ (Level, ActiveLow, Shared) {10} })
        }

        Device (PCI0) {
            Name (_HID, EisaId ("PNP0A03"))
            Name (_PRT, Package () {
                Package () { 0x0001FFFF, 0, \_SB.LNKA, 0 },
                Package () { 0x0002FFFF, 1, LNKB, 0 },
                Package () { 0x0003FFFF, 2, 0, 19 },
            })
        }
    }
}
"#;

    let interpreter = run_aml_test(AML, NullHandler {});
    let table =
        PciRoutingTable::from_prt_path(AmlName::from_str("\\_SB.PCI0._PRT").unwrap(), &interpreter).unwrap();

    // An absolute name, a relative name found by the namespace search rules, and a GSI.
    // The IRQ of a link object is decoded from its `_CRS` as a mask.
    assert_eq!(table.route(1, 0, Pin::IntA, &interpreter).unwrap().irq, 1 << 11);
    assert_eq!(table.route(2, 0, Pin::IntB, &interpreter).unwrap().irq, 1 << 10);
    assert_eq!(table.route(3, 0, Pin::IntC, &interpreter).unwrap().irq, 19);
}

/// `DerefOf` of a string is a namespace lookup, and must not recurse into the object it finds -
/// otherwise firmware can make us loop forever.
#[test]
fn deref_of_self_referential_string_terminates() {
    const AML: &str = r#"
DefinitionBlock("", "DSDT", 2, "RSACPI", "DRFTST", 1) {
    Scope (\_SB) {
        Name (FOO, "\\_SB.FOO")

        Method (GET0, 0, NotSerialized) {
            Return (DerefOf (FOO))
        }
    }
}
"#;

    let interpreter = run_aml_test(AML, NullHandler {});
    let Object::String(value) = evaluate(&interpreter, "\\_SB.GET0") else { panic!("FOO is not a string") };
    assert_eq!(value, "\\_SB.FOO");
}
