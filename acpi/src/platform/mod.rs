pub mod interrupt;

use core::alloc::Allocator;

use crate::{address::GenericAddress, fadt::Fadt, madt::Madt, AcpiError, AcpiHandler, AcpiTables, PowerProfile};
use interrupt::InterruptModel;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcessorState {
    /// A processor in this state is unusable, and you must not attempt to bring it up.
    Disabled,

    /// A processor waiting for a SIPI (Startup Inter-processor Interrupt) is currently not active,
    /// but may be brought up.
    WaitingForSipi,

    /// A Running processor is currently brought up and running code.
    Running,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Processor {
    /// Corresponds to the `_UID` object of the processor's `Device`, or the `ProcessorId` field of the `Processor`
    /// object, in AML.
    pub processor_uid: u32,
    /// The ID of the local APIC of the processor. Will be less than `256` if the APIC is being used, but can be
    /// greater than this if the X2APIC is being used.
    pub local_apic_id: u32,

    /// The state of this processor. Check that the processor is not `Disabled` before attempting to bring it up!
    pub state: ProcessorState,

    /// Whether this processor is the Bootstrap Processor (BSP), or an Application Processor (AP).
    /// When the bootloader is entered, the BSP is the only processor running code. To run code on
    /// more than one processor, you need to "bring up" the APs.
    pub is_ap: bool,
}

#[derive(Debug)]

pub struct ProcessorInfo<'a, A>
where
    A: core::alloc::Allocator,
{
    pub boot_processor: Processor,
    /// Application processors should be brought up in the order they're defined in this list.
    pub application_processors: crate::ManagedSlice<'a, Processor, A>,
}

impl<'a, A> ProcessorInfo<'a, A>
where
    A: Allocator,
{
    pub(crate) fn new(
        boot_processor: Processor,
        application_processors: crate::ManagedSlice<'a, Processor, A>,
    ) -> Self {
        Self { boot_processor, application_processors }
    }
}

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

impl<'a, A> PlatformInfo<'a, A>
where
    A: Allocator,
{
    pub fn new<H>(tables: &AcpiTables<H>, allocator: &'a A) -> crate::AcpiResult<PlatformInfo<'a, A>>
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
