use alloc::vec;
use core::str::FromStr;

use crate::{
    Handler,
    aml::{
        AmlError,
        BaseInterpreter,
        namespace::AmlName,
        object::{Object, WrappedObject},
        op_region::RegionHandler,
    },
};

/// See the [the docs for the `\_PIC` method](https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html#pic-method).
#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub enum InterruptModelUsed {
    /// 0 - PIC mode
    PicMode,
    /// 1 - APIC mode
    ApicMode,
    /// 2 - SAPIC mode
    SapicMode,
    /// 4 – GIC model
    GicModel = 4,
    /// 5 – LPIC model
    LpicModel = 5,
    /// 6 – RINTC model
    RintcModel = 6,
}

impl From<InterruptModelUsed> for Object {
    fn from(value: InterruptModelUsed) -> Self {
        Self::Integer(match value {
            InterruptModelUsed::PicMode => 0,
            InterruptModelUsed::ApicMode => 1,
            InterruptModelUsed::SapicMode => 2,
            InterruptModelUsed::GicModel => 4,
            InterruptModelUsed::LpicModel => 5,
            InterruptModelUsed::RintcModel => 6,
        })
    }
}

impl<H, R> BaseInterpreter<H, R>
where
    H: Handler,
    R: RegionHandler + ?Sized,
{
    /// Calls the [`\_PIC` method](https://uefi.org/specs/ACPI/6.6/05_ACPI_Software_Programming_Model.html#pic-method).
    /// The method is optional, so if it doesn't exist this function returns success. Returns `true` is the method was called, `false` if it doesn't exist.
    pub fn set_interrupt_model_used(&self, model: InterruptModelUsed) -> Result<bool, AmlError> {
        Ok(self
            .evaluate_if_present(
                AmlName::from_str(r#"\_PIC"#).expect("valid name"),
                vec![WrappedObject::new(model.into())],
            )?
            .is_some())
    }
}
