use crate::ManagedSlice;
use core::alloc::Allocator;

#[derive(Debug, Clone, Copy)]
pub struct IoApic {
    pub id: u8,
    /// The physical address at which to access this I/O APIC.
    pub address: u32,
    /// The global system interrupt number where this I/O APIC's inputs start.
    pub global_system_interrupt_base: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct NmiLine {
    pub processor: NmiProcessor,
    pub line: LocalInterruptLine,
}

/// Indicates which local interrupt line will be utilized by an external interrupt. Specifically,
/// these lines directly correspond to their requisite LVT entries in a processor's APIC.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalInterruptLine {
    Lint0,
    Lint1,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NmiProcessor {
    All,
    ProcessorUid(u32),
}

/// Polarity indicates what signal mode the interrupt line needs to be in to be considered 'active'.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Polarity {
    SameAsBus,
    ActiveHigh,
    ActiveLow,
}

/// Trigger mode of an interrupt, describing how the interrupt is triggered.
///
/// When an interrupt is `Edge` triggered, it is triggered exactly once, when the interrupt
/// signal goes from its opposite polarity to its active polarity.
///
/// For `Level` triggered interrupts, a continuous signal is emitted so long as the interrupt
/// is in its active polarity.
///
/// `SameAsBus`-triggered interrupts will utilize the same interrupt triggering as the system bus
/// they communicate across.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriggerMode {
    SameAsBus,
    Edge,
    Level,
}

/// Describes a difference in the mapping of an ISA interrupt to how it's mapped in other interrupt
/// models. For example, if a device is connected to ISA IRQ 0 and IOAPIC input 2, an override will
/// appear mapping source 0 to GSI 2. Currently these will only be created for ISA interrupt
/// sources.
#[derive(Debug, Clone, Copy)]
pub struct InterruptSourceOverride {
    pub isa_source: u8,
    pub global_system_interrupt: u32,
    pub polarity: Polarity,
    pub trigger_mode: TriggerMode,
}

/// Describes a Global System Interrupt that should be enabled as non-maskable. Any source that is
/// non-maskable can not be used by devices.
#[derive(Debug, Clone, Copy)]
pub struct NmiSource {
    pub global_system_interrupt: u32,
    pub polarity: Polarity,
    pub trigger_mode: TriggerMode,
}

#[derive(Debug, Clone)]
pub struct Apic<'a, A>
where
    A: Allocator,
{
    pub local_apic_address: u64,
    pub io_apics: ManagedSlice<'a, IoApic, A>,
    pub local_apic_nmi_lines: ManagedSlice<'a, NmiLine, A>,
    pub interrupt_source_overrides: ManagedSlice<'a, InterruptSourceOverride, A>,
    pub nmi_sources: ManagedSlice<'a, NmiSource, A>,

    /// If this field is set, you must remap and mask all the lines of the legacy PIC, even if
    /// you choose to use the APIC. It's recommended that you do this even if ACPI does not
    /// require you to.
    pub also_has_legacy_pics: bool,
}

impl<'a, A> Apic<'a, A>
where
    A: Allocator,
{
    pub(crate) fn new(
        local_apic_address: u64,
        io_apics: ManagedSlice<'a, IoApic, A>,
        local_apic_nmi_lines: ManagedSlice<'a, NmiLine, A>,
        interrupt_source_overrides: ManagedSlice<'a, InterruptSourceOverride, A>,
        nmi_sources: ManagedSlice<'a, NmiSource, A>,
        also_has_legacy_pics: bool,
    ) -> Self {
        Self {
            local_apic_address,
            io_apics,
            local_apic_nmi_lines,
            interrupt_source_overrides,
            nmi_sources,
            also_has_legacy_pics,
        }
    }
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum InterruptModel<'a, A>
where
    A: Allocator,
{
    /// This model is only chosen when the MADT does not describe another interrupt model. On `x86_64` platforms,
    /// this probably means only the legacy i8259 PIC is present.
    Unknown,

    /// Describes an interrupt controller based around the Advanced Programmable Interrupt Controller (any of APIC,
    /// XAPIC, or X2APIC). These are likely to be found on x86 and x86_64 systems and are made up of a Local APIC
    /// for each core and one or more I/O APICs to handle external interrupts.
    Apic(Apic<'a, A>),
}
