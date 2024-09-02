pub mod interrupt;
pub mod processor;

use crate::{
    address::GenericAddress,
    fadt::Fadt,
    madt::Madt,
    AcpiError,
    AcpiHandler,
    AcpiResult,
    AcpiTables,
    PowerProfile,
};
use core::alloc::Allocator;
use interrupt::InterruptModel;
use processor::ProcessorInfo;

/// Information about the ACPI Power Management Timer (ACPI PM Timer).
#[derive(Debug)]
pub struct PmTimer {
    /// A generic address to the register block of ACPI PM Timer.
    pub base: GenericAddress,
    /// This field is `true` if the hardware supports 32-bit timer, and `false` if the hardware supports 24-bit timer.
    pub supports_32bit: bool,
}

impl PmTimer {
    pub fn new(fadt: &Fadt) -> Result<Option<PmTimer>, AcpiError> {
        match fadt.pm_timer_block()? {
            Some(base) => Ok(Some(PmTimer { base, supports_32bit: { fadt.flags }.pm_timer_is_32_bit() })),
            None => Ok(None),
        }
    }
}

/// `PlatformInfo` allows the collection of some basic information about the platform from some of the fixed-size
/// tables in a nice way. It requires access to the `FADT` and `MADT`. It is the easiest way to get information
/// about the processors and interrupt controllers on a platform.
#[derive(Debug)]
pub struct PlatformInfo<'a, A>
where
    A: Allocator,
{
    pub power_profile: PowerProfile,
    pub interrupt_model: InterruptModel<'a, A>,
    /// On `x86_64` platforms that support the APIC, the processor topology must also be inferred from the
    /// interrupt model. That information is stored here, if present.
    pub processor_info: Option<ProcessorInfo<'a, A>>,
    pub pm_timer: Option<PmTimer>,
    /*
     * TODO: we could provide a nice view of the hardware register blocks in the FADT here.
     */
}

#[cfg(feature = "alloc")]
impl<'a> PlatformInfo<'a, alloc::alloc::Global> {
    pub fn new<H>(tables: &AcpiTables<H>) -> AcpiResult<Self>
    where
        H: AcpiHandler,
    {
        Self::new_in(tables, alloc::alloc::Global)
    }
}

impl<'a, A> PlatformInfo<'a, A>
where
    A: Allocator + Clone,
{
    pub fn new_in<H>(tables: &AcpiTables<H>, allocator: A) -> AcpiResult<Self>
    where
        H: AcpiHandler,
    {
        let fadt = tables.find_table::<Fadt>()?;
        let power_profile = fadt.power_profile();

        let madt = tables.find_table::<Madt>();
        let (interrupt_model, processor_info) = match madt {
            Ok(madt) => madt.parse_interrupt_model_in(allocator)?,
            Err(_) => (InterruptModel::Unknown, None),
        };
        let pm_timer = PmTimer::new(&fadt)?;

        Ok(PlatformInfo { power_profile, interrupt_model, processor_info, pm_timer })
    }
}
