use super::{Processor, ProcessorInfo, ProcessorState};
use crate::{
    AcpiError,
    AcpiTables,
    Handler,
    MadtError,
    sdt::{
        Signature,
        madt::{Madt, MadtEntry, parse_mps_inti_flags},
    },
};
use alloc::{alloc::Global, vec::Vec};
use bit_field::BitField;
use core::{alloc::Allocator, pin::Pin};

pub use crate::sdt::madt::{Polarity, TriggerMode};

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum InterruptModel<A: Allocator = Global> {
    /// This model is only chosen when the MADT does not describe another interrupt model. On `x86_64` platforms,
    /// this probably means only the legacy i8259 PIC is present.
    Unknown,

    /// Describes an interrupt controller based around the Advanced Programmable Interrupt Controller (any of APIC,
    /// XAPIC, or X2APIC). These are likely to be found on x86 and x86_64 systems and are made up of a Local APIC
    /// for each core and one or more I/O APICs to handle external interrupts.
    Apic(Apic<A>),
}

impl InterruptModel<Global> {
    pub fn new<H: Handler>(
        tables: &AcpiTables<H>,
    ) -> Result<(InterruptModel<Global>, Option<ProcessorInfo<Global>>), AcpiError> {
        Self::new_in(tables, Global)
    }
}

impl<A: Allocator + Clone> InterruptModel<A> {
    pub fn new_in<H: Handler>(
        tables: &AcpiTables<H>,
        allocator: A,
    ) -> Result<(InterruptModel<A>, Option<ProcessorInfo<A>>), AcpiError> {
        let Some(madt) = tables.find_table::<Madt>() else { Err(AcpiError::TableNotFound(Signature::MADT))? };

        /*
         * We first do a pass through the MADT to determine which interrupt model is being used.
         */
        for entry in madt.get().entries() {
            match entry {
                MadtEntry::LocalApic(_)
                | MadtEntry::LocalX2Apic(_)
                | MadtEntry::IoApic(_)
                | MadtEntry::InterruptSourceOverride(_)
                | MadtEntry::LocalApicNmi(_)
                | MadtEntry::X2ApicNmi(_)
                | MadtEntry::LocalApicAddressOverride(_) => {
                    return Self::from_apic_model_in(madt.get(), allocator);
                }

                MadtEntry::IoSapic(_) | MadtEntry::LocalSapic(_) | MadtEntry::PlatformInterruptSource(_) => {}

                MadtEntry::Gicc(_)
                | MadtEntry::Gicd(_)
                | MadtEntry::GicMsiFrame(_)
                | MadtEntry::GicRedistributor(_)
                | MadtEntry::GicInterruptTranslationService(_) => {}

                MadtEntry::NmiSource(_) => (),
                MadtEntry::MultiprocessorWakeup(_) => (),
            }
        }

        Ok((InterruptModel::Unknown, None))
    }

    fn from_apic_model_in(
        madt: Pin<&Madt>,
        allocator: A,
    ) -> Result<(InterruptModel<A>, Option<ProcessorInfo<A>>), AcpiError> {
        let mut local_apic_address = madt.local_apic_address as u64;
        let mut io_apic_count = 0;
        let mut iso_count = 0;
        let mut nmi_source_count = 0;
        let mut local_nmi_line_count = 0;
        let mut processor_count = 0usize;

        // Do a pass over the entries so we know how much space we should reserve in the vectors
        for entry in madt.entries() {
            match entry {
                MadtEntry::IoApic(_) => io_apic_count += 1,
                MadtEntry::InterruptSourceOverride(_) => iso_count += 1,
                MadtEntry::NmiSource(_) => nmi_source_count += 1,
                MadtEntry::LocalApicNmi(_) => local_nmi_line_count += 1,
                MadtEntry::X2ApicNmi(_) => local_nmi_line_count += 1,
                MadtEntry::LocalApic(_) => processor_count += 1,
                MadtEntry::LocalX2Apic(_) => processor_count += 1,
                _ => (),
            }
        }

        let mut io_apics = Vec::with_capacity_in(io_apic_count, allocator.clone());
        let mut interrupt_source_overrides = Vec::with_capacity_in(iso_count, allocator.clone());
        let mut nmi_sources = Vec::with_capacity_in(nmi_source_count, allocator.clone());
        let mut local_apic_nmi_lines = Vec::with_capacity_in(local_nmi_line_count, allocator.clone());
        let mut application_processors = Vec::with_capacity_in(processor_count.saturating_sub(1), allocator); // Subtract one for the BSP
        let mut boot_processor = None;

        for entry in madt.entries() {
            match entry {
                MadtEntry::LocalApic(entry) => {
                    /*
                     * The first processor is the BSP. Subsequent ones are APs. If we haven't found
                     * the BSP yet, this must be it.
                     */
                    let is_ap = boot_processor.is_some();
                    let is_disabled = !{ entry.flags }.get_bit(0);

                    let state = match (is_ap, is_disabled) {
                        (_, true) => ProcessorState::Disabled,
                        (true, false) => ProcessorState::WaitingForSipi,
                        (false, false) => ProcessorState::Running,
                    };

                    let processor = Processor {
                        processor_uid: entry.processor_id as u32,
                        local_apic_id: entry.apic_id as u32,
                        state,
                        is_ap,
                    };

                    if is_ap {
                        application_processors.push(processor);
                    } else {
                        boot_processor = Some(processor);
                    }
                }

                MadtEntry::LocalX2Apic(entry) => {
                    let is_ap = boot_processor.is_some();
                    let is_disabled = !{ entry.flags }.get_bit(0);

                    let state = match (is_ap, is_disabled) {
                        (_, true) => ProcessorState::Disabled,
                        (true, false) => ProcessorState::WaitingForSipi,
                        (false, false) => ProcessorState::Running,
                    };

                    let processor = Processor {
                        processor_uid: entry.processor_uid,
                        local_apic_id: entry.x2apic_id,
                        state,
                        is_ap,
                    };

                    if is_ap {
                        application_processors.push(processor);
                    } else {
                        boot_processor = Some(processor);
                    }
                }

                MadtEntry::IoApic(entry) => {
                    io_apics.push(IoApic {
                        id: entry.io_apic_id,
                        address: entry.io_apic_address,
                        global_system_interrupt_base: entry.global_system_interrupt_base,
                    });
                }

                MadtEntry::InterruptSourceOverride(entry) => {
                    if entry.bus != 0 {
                        return Err(AcpiError::InvalidMadt(MadtError::InterruptOverrideEntryHasInvalidBus));
                    }

                    let (polarity, trigger_mode) = parse_mps_inti_flags(entry.flags)?;

                    interrupt_source_overrides.push(InterruptSourceOverride {
                        isa_source: entry.irq,
                        global_system_interrupt: entry.global_system_interrupt,
                        polarity,
                        trigger_mode,
                    });
                }

                MadtEntry::NmiSource(entry) => {
                    let (polarity, trigger_mode) = parse_mps_inti_flags(entry.flags)?;

                    nmi_sources.push(NmiSource {
                        global_system_interrupt: entry.global_system_interrupt,
                        polarity,
                        trigger_mode,
                    });
                }

                MadtEntry::LocalApicNmi(entry) => {
                    local_apic_nmi_lines.push(NmiLine {
                        processor: if entry.processor_id == 0xff {
                            NmiProcessor::All
                        } else {
                            NmiProcessor::ProcessorUid(entry.processor_id as u32)
                        },
                        line: match entry.nmi_line {
                            0 => LocalInterruptLine::Lint0,
                            1 => LocalInterruptLine::Lint1,
                            _ => return Err(AcpiError::InvalidMadt(MadtError::InvalidLocalNmiLine)),
                        },
                    });
                }

                MadtEntry::X2ApicNmi(entry) => {
                    local_apic_nmi_lines.push(NmiLine {
                        processor: if entry.processor_uid == 0xffffffff {
                            NmiProcessor::All
                        } else {
                            NmiProcessor::ProcessorUid(entry.processor_uid)
                        },
                        line: match entry.nmi_line {
                            0 => LocalInterruptLine::Lint0,
                            1 => LocalInterruptLine::Lint1,
                            _ => return Err(AcpiError::InvalidMadt(MadtError::InvalidLocalNmiLine)),
                        },
                    });
                }

                MadtEntry::LocalApicAddressOverride(entry) => {
                    local_apic_address = entry.local_apic_address;
                }

                MadtEntry::MultiprocessorWakeup(_) => {}

                _ => {
                    return Err(AcpiError::InvalidMadt(MadtError::UnexpectedEntry));
                }
            }
        }

        Ok((
            InterruptModel::Apic(Apic::new(
                local_apic_address,
                io_apics,
                local_apic_nmi_lines,
                interrupt_source_overrides,
                nmi_sources,
                madt.supports_8259(),
            )),
            Some(ProcessorInfo::new_in(boot_processor.unwrap(), application_processors)),
        ))
    }
}

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
pub struct Apic<A: Allocator = Global> {
    pub local_apic_address: u64,
    pub io_apics: Vec<IoApic, A>,
    pub local_apic_nmi_lines: Vec<NmiLine, A>,
    pub interrupt_source_overrides: Vec<InterruptSourceOverride, A>,
    pub nmi_sources: Vec<NmiSource, A>,

    /// If this field is set, you must remap and mask all the lines of the legacy PIC, even if
    /// you choose to use the APIC. It's recommended that you do this even if ACPI does not
    /// require you to.
    pub also_has_legacy_pics: bool,
}

impl<A: Allocator> Apic<A> {
    pub(crate) fn new(
        local_apic_address: u64,
        io_apics: Vec<IoApic, A>,
        local_apic_nmi_lines: Vec<NmiLine, A>,
        interrupt_source_overrides: Vec<InterruptSourceOverride, A>,
        nmi_sources: Vec<NmiSource, A>,
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
