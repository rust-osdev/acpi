use alloc::vec;
use core::str::FromStr;

use crate::{
    Handler,
    aml::{
        AmlError,
        Interpreter,
        namespace::AmlName,
        object::{Object, WrappedObject},
    },
};

/// See <https://uefi.org/htmlspecs/ACPI_Spec_6_4_html/05_ACPI_Software_Programming_Model/ACPI_Software_Programming_Model.html?highlight=_pic#pic-method>.
#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub enum InterruptModelUsed {
    /// 0 - PIC mode
    PicMode,
    /// 1 - APIC mode
    ApicMode,
    /// 2 - SAPIC mode
    SapicMode,
}

impl From<InterruptModelUsed> for Object {
    fn from(value: InterruptModelUsed) -> Self {
        Self::Integer(match value {
            InterruptModelUsed::PicMode => 0,
            InterruptModelUsed::ApicMode => 1,
            InterruptModelUsed::SapicMode => 2,
        })
    }
}

impl<H> Interpreter<H>
where
    H: Handler,
{
    /// Calls the [`\_PIC` method](https://uefi.org/htmlspecs/ACPI_Spec_6_4_html/05_ACPI_Software_Programming_Model/ACPI_Software_Programming_Model.html?highlight=_pic#pic-method).
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
