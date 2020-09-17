use crate::{fadt::Fadt, madt::Madt, AcpiError, AcpiHandler, AcpiTables, PhysicalMapping, PowerProfile};
use alloc::vec::Vec;

#[derive(Debug)]
pub struct IoApic {
    pub id: u8,
    pub address: u32,
    pub global_system_interrupt_base: u32,
}

#[derive(Debug)]
pub struct NmiLine {
    pub processor: NmiProcessor,
    pub line: LocalInterruptLine,
}

#[derive(Debug)]
pub enum LocalInterruptLine {
    Lint0,
    Lint1,
}

#[derive(Debug)]
pub enum NmiProcessor {
    All,
    /// Refers to a processor with the given UID. This is stored as a `u32`, but should be casted to `u8` when the
    /// DSDT uses the deprecated `DefProcessor` operator to define processor UIDs.
    ProcessorUid(u32),
}

#[derive(Debug)]
pub enum Polarity {
    SameAsBus,
    ActiveHigh,
    ActiveLow,
}

#[derive(Debug)]
pub enum TriggerMode {
    SameAsBus,
    Edge,
    Level,
}

/// Describes a difference in the mapping of an ISA interrupt to how it's mapped in other interrupt
/// models. For example, if a device is connected to ISA IRQ 0 and IOAPIC input 2, an override will
/// appear mapping source 0 to GSI 2. Currently these will only be created for ISA interrupt
/// sources.
#[derive(Debug)]
pub struct InterruptSourceOverride {
    pub isa_source: u8,
    pub global_system_interrupt: u32,
    pub polarity: Polarity,
    pub trigger_mode: TriggerMode,
}

/// Describes a Global System Interrupt that should be enabled as non-maskable. Any source that is
/// non-maskable can not be used by devices.
#[derive(Debug)]
pub struct NmiSource {
    pub global_system_interrupt: u32,
    pub polarity: Polarity,
    pub trigger_mode: TriggerMode,
}

#[derive(Debug)]
pub struct Apic {
    pub local_apic_address: u64,
    pub io_apics: Vec<IoApic>,
    pub local_apic_nmi_lines: Vec<NmiLine>,
    pub interrupt_source_overrides: Vec<InterruptSourceOverride>,
    pub nmi_sources: Vec<NmiSource>,

    /// If this field is set, you must remap and mask all the lines of the legacy PIC, even if
    /// you choose to use the APIC. It's recommended that you do this even if ACPI does not
    /// require you to.
    pub also_has_legacy_pics: bool,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum InterruptModel {
    /// This model is only chosen when the MADT does not describe another interrupt model. On `x86_64` platforms,
    /// this probably means only the legacy i8259 PIC is present.
    Unknown,

    /// Describes an interrupt controller based around the Advanced Programmable Interrupt
    /// Controllers. These are likely to be found on x86 and x86_64 systems and are made up of a
    /// Local APIC for each core and one or more I/O APICs to handle external interrupts.
    Apic(Apic),
}

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
    pub processor_uid: u8,
    pub local_apic_id: u8,

    /// The state of this processor. Always check that the processor is not `Disabled` before
    /// attempting to bring it up!
    pub state: ProcessorState,

    /// Whether this processor is the Bootstrap Processor (BSP), or an Application Processor (AP).
    /// When the bootloader is entered, the BSP is the only processor running code. To run code on
    /// more than one processor, you need to "bring up" the APs.
    pub is_ap: bool,
}

pub struct ProcessorInfo {
    pub boot_processor: Processor,
    /// Application processors should be brought up in the order they're defined in this list.
    pub application_processors: Vec<Processor>,
}

/// `PlatformInfo` allows the collection of some basic information about the platform from some of the fixed-size
/// tables in a nice way. It requires access to the `FADT` and `MADT`. It is the easiest way to get information
/// about the processors and interrupt controllers on a platform.
pub struct PlatformInfo {
    pub power_profile: PowerProfile,
    pub interrupt_model: InterruptModel,
    /// On `x86_64` platforms that support the APIC, the processor topology must also be inferred from the
    /// interrupt model. That information is stored here, if present.
    pub processor_info: Option<ProcessorInfo>,
    /*
     * TODO: we could provide a nice view of the hardware register blocks in the FADT here.
     */
}

impl PlatformInfo {
    pub fn new<H>(tables: &AcpiTables<H>, handler: &mut H) -> Result<PlatformInfo, AcpiError>
    where
        H: AcpiHandler,
    {
        let fadt = unsafe {
            tables
                .get_sdt::<Fadt>(handler, crate::sdt::Signature::FADT)?
                .ok_or(AcpiError::TableMissing(crate::sdt::Signature::FADT))?
        };
        let power_profile = fadt.power_profile();

        let madt = unsafe { tables.get_sdt::<Madt>(handler, crate::sdt::Signature::MADT)? };
        let (interrupt_model, processor_info) = match madt {
            Some(madt) => madt.parse_interrupt_model()?,
            None => (InterruptModel::Unknown, None),
        };

        Ok(PlatformInfo { power_profile, interrupt_model, processor_info })
    }
}
