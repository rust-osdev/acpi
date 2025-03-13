use crate::{
    AmlError,
    Handler,
    Interpreter,
    Operation,
    namespace::AmlName,
    object::{Object, ReferenceKind},
    resource::{self, InterruptPolarity, InterruptTrigger, Resource},
};
use alloc::{vec, vec::Vec};
use bit_field::BitField;
use core::str::FromStr;

pub use crate::resource::IrqDescriptor;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Pin {
    IntA,
    IntB,
    IntC,
    IntD,
}

#[derive(Debug)]
pub enum PciRouteType {
    /// The interrupt is hard-coded to a specific GSI
    Gsi(u32),

    /// The interrupt is linked to a link object. This object will have `_PRS`, `_CRS` fields and a `_SRS` method
    /// that can be used to allocate the interrupt. Note that some platforms (e.g. QEMU's q35 chipset) use link
    /// objects but do not support changing the interrupt that it's linked to (i.e. `_SRS` doesn't do anything).
    /*
     * The actual object itself will just be a `Device`, and we need paths to its children objects to do
     * anything useful, so we just store the resolved name here.
     */
    LinkObject(AmlName),
}

#[derive(Debug)]
pub struct PciRoute {
    device: u16,
    function: u16,
    pin: Pin,
    route_type: PciRouteType,
}

/// A `PciRoutingTable` is used to interpret the data in a `_PRT` object, which provides a mapping
/// from PCI interrupt pins to the inputs of the interrupt controller. One of these objects must be
/// present under each PCI root bridge, and consists of a package of packages, each of which describes the
/// mapping of a single PCI interrupt pin.
#[derive(Debug)]
pub struct PciRoutingTable {
    entries: Vec<PciRoute>,
}

impl PciRoutingTable {
    /// Construct a `PciRoutingTable` from a path to a `_PRT` object. Returns
    /// `AmlError::InvalidOperationOnObject` if the value passed is not a package, or if any of the
    /// values within it are not packages. Returns the various `AmlError::Prt*` errors if the
    /// internal structure of the entries is invalid.
    pub fn from_prt_path(
        prt_path: AmlName,
        interpreter: &Interpreter<impl Handler>,
    ) -> Result<PciRoutingTable, AmlError> {
        let mut entries = Vec::new();

        let prt = interpreter.invoke_method(prt_path.clone(), vec![])?;

        if let Object::Package(ref inner_values) = *prt {
            for value in inner_values {
                if let Object::Package(ref pin_package) = **value {
                    /*
                     * Each inner package has the following structure:
                     *   | Field      | Type      | Description                                               |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     *   | Address    | Dword     | Address of the device. Same format as _ADR objects (high  |
                     *   |            |           | word = #device, low word = #function)                     |
                     *   | -----------|-----------|-----------------------------------------------------------|
                     *   | Pin        | Byte      | The PCI pin (0 = INTA, 1 = INTB, 2 = INTC, 3 = INTD)      |
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
                    let Object::Integer(address) = *pin_package[0] else {
                        return Err(AmlError::PrtInvalidAddress);
                    };
                    let device = address.get_bits(16..32).try_into().map_err(|_| AmlError::PrtInvalidAddress)?;
                    let function = address.get_bits(0..16).try_into().map_err(|_| AmlError::PrtInvalidAddress)?;
                    let pin = match *pin_package[1] {
                        Object::Integer(0) => Pin::IntA,
                        Object::Integer(1) => Pin::IntB,
                        Object::Integer(2) => Pin::IntC,
                        Object::Integer(3) => Pin::IntD,
                        _ => return Err(AmlError::PrtInvalidPin),
                    };

                    match *pin_package[2] {
                        Object::Integer(0) => {
                            /*
                             * The Source Index field contains the GSI number that this interrupt is attached
                             * to.
                             */
                            let Object::Integer(gsi) = *pin_package[3] else {
                                return Err(AmlError::PrtInvalidGsi);
                            };
                            entries.push(PciRoute {
                                device,
                                function,
                                pin,
                                route_type: PciRouteType::Gsi(gsi as u32),
                            });
                        }
                        Object::Reference { kind: ReferenceKind::Unresolved, ref inner } => {
                            let Object::String(ref name) = **inner else { panic!() };
                            let link_object_name = interpreter
                                .namespace
                                .lock()
                                .search_for_level(&AmlName::from_str(&name)?, &prt_path)?;
                            entries.push(PciRoute {
                                device,
                                function,
                                pin,
                                route_type: PciRouteType::LinkObject(link_object_name),
                            });
                        }
                        _ => return Err(AmlError::PrtInvalidSource),
                    }
                } else {
                    return Err(AmlError::InvalidOperationOnObject { op: Operation::DecodePrt, typ: value.typ() });
                }
            }

            Ok(PciRoutingTable { entries })
        } else {
            return Err(AmlError::InvalidOperationOnObject { op: Operation::DecodePrt, typ: prt.typ() });
        }
    }

    /// Get the interrupt input that a given PCI interrupt pin is wired to. Returns `AmlError::PrtNoEntry` if the
    /// PRT doesn't contain an entry for the given address + pin.
    pub fn route(
        &self,
        device: u16,
        function: u16,
        pin: Pin,
        interpreter: &Interpreter<impl Handler>,
    ) -> Result<IrqDescriptor, AmlError> {
        let entry = self
            .entries
            .iter()
            .find(|entry| {
                entry.device == device
                    && (entry.function == 0xffff || entry.function == function)
                    && entry.pin == pin
            })
            .ok_or(AmlError::PrtNoEntry)?;

        match entry.route_type {
            PciRouteType::Gsi(gsi) => Ok(IrqDescriptor {
                is_consumer: true,
                trigger: InterruptTrigger::Level,
                polarity: InterruptPolarity::ActiveLow,
                is_shared: true,
                is_wake_capable: false,
                irq: gsi,
            }),
            PciRouteType::LinkObject(ref name) => {
                let path = AmlName::from_str("_CRS").unwrap().resolve(name)?;
                let link_crs = interpreter.invoke_method(path, vec![])?;

                let resources = resource::resource_descriptor_list(link_crs)?;
                match resources.as_slice() {
                    [Resource::Irq(descriptor)] => Ok(descriptor.clone()),
                    _ => Err(AmlError::UnexpectedResourceType),
                }
            }
        }
    }
}
