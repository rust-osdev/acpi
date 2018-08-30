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
pub enum InterruptModel {
    /// This model is only chosen when a newer one can not be found and the system supports the
    /// legacy dual-8259 PIC.
    Pic,

    /// Describes an interrupt controller based around the Advanced Programmable Interrupt
    /// Controllers. These are likely to be found on x86 and x86_64 systems and are made up of a
    /// Local APIC for each core and one or more I/O APICs to handle external interrupts.
    Apic {
        local_apic_address: u64,
        io_apics: Vec<IoApic>,
        local_apic_nmi_line: LocalInterruptLine,

        /// If this field is set, you must remap and mask all the lines of the legacy PIC, even if
        /// you choose to use the APIC. It's recommended that you do this even if ACPI does not
        /// require you to.
        also_has_legacy_pics: bool,
    },
}
