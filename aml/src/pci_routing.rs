use crate::{AmlError, AmlValue};
use bit_field::BitField;
use core::convert::TryInto;
use log::info;

// TODO: maybe make this an enum with a variant for each _PRT type
pub struct PciRoute {
    device: u16,
    function: u16,
    pin: u8,
    // TODO: complete this
}

/// A `PciRoutingTable` is used to interpret the data in a `_PRT` object, which provides a mapping
/// from PCI interrupt pins to the inputs of the interrupt controller. One of these objects must be
/// present under each PCI root bridge, and consists of a package of packages, each of which describes the
/// mapping of a single PCI interrupt pin.
pub struct PciRoutingTable {}

impl PciRoutingTable {
    /// Construct a `PciRoutingTable` from a `_PRT` object. Returns `AmlError::IncompatibleValueConversion` if
    /// the value passed is not a package, or if any of the values within it are not packages.
    pub fn from_prt(prt: &AmlValue) -> Result<PciRoutingTable, AmlError> {
        if let AmlValue::Package(ref inner_values) = prt {
            for value in inner_values {
                if let AmlValue::Package(ref pin_package) = value {
                    /*
                     * Each inner package has the following structure:
                     *   | Field      | Type      | Description                                               |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     *   | Address    | Dword     | Address of the device. Same format as _ADR objects (high  |
                     *   |            |           | word = #device, low word = #function)                     |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     *   | Pin        | Byte      | The PCI pin (0 = INTA, 1 = INTB, 2 = INTC, 3 = INTC)      |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     *   | Source     | Byte or   | Name of the device that allocates the interrupt to which  |
                     *   |            | NamePath  | the above pin is connected. Can be fully qualified,       |
                     *   |            |           | relative, or a simple NameSeg that utilizes namespace     |
                     *   |            |           | search rules. Instead, if this is a byte value of 0, the  |
                     *   |            |           | interrupt is allocated out of the GSI pool, and Source    |
                     *   |            |           | Index should be utilised.                                 |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     *   | Source     | Dword     | Index that indicates which resource descriptor in the     |
                     *   | Index      |           | resource template of the device pointed to in the Source  |
                     *   |            |           | field this interrupt is allocated from. If the Source     |
                     *   |            |           | is zero, then this field is the GSI number to which the   |
                     *   |            |           | pin is connected.                                         |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     */
                    info!("PRT pin: {:?}", pin_package);

                    let address = pin_package[0].as_integer()?;
                    let device = address.get_bits(16..32);
                    let function = address.get_bits(16..32);
                    let pin: u8 = pin_package[1].as_integer()?.try_into().expect("Invalid pin value!");
                    info!("PRT pin: {:#x}, {:#x}, pin = {}", device, function, pin);
                } else {
                    return Err(AmlError::IncompatibleValueConversion);
                }
            }

            Ok(PciRoutingTable {})
        } else {
            Err(AmlError::IncompatibleValueConversion)
        }
    }
}
