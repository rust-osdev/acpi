use alloc::vec::Vec;

#[derive(Debug)]
pub struct IoApic {
    pub id: u8,
    pub address: u32,
    pub global_system_interrupt_base: u32,
}

#[derive(Debug)]
pub enum LocalInterruptLine {
    Lint0,
    Lint1,
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
    pub local_apic_nmi_lines: Vec<LocalInterruptLine>,
    pub interrupt_source_overrides: Vec<InterruptSourceOverride>,
    pub nmi_sources: Vec<NmiSource>,

    /// If this field is set, you must remap and mask all the lines of the legacy PIC, even if
    /// you choose to use the APIC. It's recommended that you do this even if ACPI does not
    /// require you to.
    pub also_has_legacy_pics: bool,
}

#[derive(Debug)]
pub enum InterruptModel {
    /// This model is only chosen when a newer one can not be found and the system supports the
    /// legacy dual-8259 PIC.
    Pic,

    /// Describes an interrupt controller based around the Advanced Programmable Interrupt
    /// Controllers. These are likely to be found on x86 and x86_64 systems and are made up of a
    /// Local APIC for each core and one or more I/O APICs to handle external interrupts.
    Apic(Apic),
}
