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

    /// Describes an interrupt controller based around the ARM Generic Interrupt Controller (GIC) — versions 1
    /// through 4. Found on `aarch64` systems. Composed of a single Distributor (GICD), one CPU Interface (GICC)
    /// per core, optional Redistributor discovery ranges (GICv3+), optional Interrupt Translation Service blocks
    /// (GICv3+, for LPI / MSI), and optional MSI frames (GICv2m, for legacy SPI-based MSI).
    Gic(Gic<A>),
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
                | MadtEntry::GicInterruptTranslationService(_) => {
                    return Self::from_gic_model_in(madt.get(), allocator);
                }

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
                        hw_id: entry.apic_id as u64,
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
                        hw_id: entry.x2apic_id as u64,
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
                    let (polarity, trigger_mode) = parse_mps_inti_flags(entry.flags)?;
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
                        polarity,
                        trigger_mode,
                    });
                }

                MadtEntry::X2ApicNmi(entry) => {
                    let (polarity, trigger_mode) = parse_mps_inti_flags(entry.flags)?;
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
                        polarity,
                        trigger_mode,
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

    fn from_gic_model_in(
        madt: Pin<&Madt>,
        allocator: A,
    ) -> Result<(InterruptModel<A>, Option<ProcessorInfo<A>>), AcpiError> {
        // Revision drives optional GICC fields like TRBE Interrupt
        // (added in ACPI 6.6).  Captured once so each entry can do
        // its own version-gated reads consistently.
        let revision = madt.header.revision;

        // First pass: count each entry kind so we can `with_capacity_in`
        // exactly and avoid any growth-time reallocation.
        let mut gicd_count = 0usize;
        let mut gicc_count = 0usize;
        let mut gicr_count = 0usize;
        let mut gic_msi_count = 0usize;
        let mut gic_its_count = 0usize;
        for entry in madt.entries() {
            match entry {
                MadtEntry::Gicd(_) => gicd_count += 1,
                MadtEntry::Gicc(_) => gicc_count += 1,
                MadtEntry::GicRedistributor(_) => gicr_count += 1,
                MadtEntry::GicMsiFrame(_) => gic_msi_count += 1,
                MadtEntry::GicInterruptTranslationService(_) => gic_its_count += 1,
                _ => (),
            }
        }

        let mut distributors = Vec::with_capacity_in(gicd_count, allocator.clone());
        let mut cpu_interfaces = Vec::with_capacity_in(gicc_count, allocator.clone());
        let mut redistributor_ranges = Vec::with_capacity_in(gicr_count, allocator.clone());
        let mut msi_frames = Vec::with_capacity_in(gic_msi_count, allocator.clone());
        let mut its_blocks = Vec::with_capacity_in(gic_its_count, allocator.clone());

        // `application_processors` is reserved for AP entries only;
        // the BSP slot is filled in by `boot_processor`.  Real
        // hardware tags exactly one GICC as the BSP via the boot
        // bring-up convention (first GICC in MADT order, by ACPI 6.x
        // spec text), but we don't have a reliable in-table flag —
        // mirror the APIC path's "first one wins" convention.
        let mut application_processors = Vec::with_capacity_in(gicc_count.saturating_sub(1), allocator);
        let mut boot_processor: Option<Processor> = None;

        for entry in madt.entries() {
            match entry {
                MadtEntry::Gicd(g) => distributors.push(GicDistributor {
                    gic_id: g.gic_id,
                    physical_base_address: g.physical_base_address,
                    system_vector_base: g.system_vector_base,
                    gic_version: g.gic_version,
                    // (All u32/u64/u8 — Copy through field access in
                    // packed structs is fine; only references / method
                    // dispatch need an aligned copy first.)
                }),

                MadtEntry::Gicc(g) => {
                    // `GiccEntry` is `#[repr(C, packed)]` — copy
                    // every used field to an aligned local before
                    // any method dispatch / reference taking.
                    let processor_uid = g.processor_uid;
                    let mpidr = g.mpidr;
                    let flags = g.flags;
                    let cpu_interface_number = g.cpu_interface_number;
                    let gic_registers_address = g.gic_registers_address;
                    let gicr_base_address = g.gicr_base_address;
                    let performance_interrupt_gsiv = g.performance_interrupt_gsiv;
                    let vgic_maintenance_interrupt = g.vgic_maintenance_interrupt;
                    let spe_overflow_interrupt = g.spe_overflow_interrupt;
                    let trbe_interrupt_field = g.trbe_interrupt;

                    let is_disabled = flags & 1 == 0;
                    let is_ap = boot_processor.is_some();
                    let state = match (is_ap, is_disabled) {
                        (_, true) => ProcessorState::Disabled,
                        (true, false) => ProcessorState::WaitingForSipi,
                        (false, false) => ProcessorState::Running,
                    };

                    let processor = Processor { processor_uid, hw_id: mpidr, state, is_ap };
                    if is_ap {
                        application_processors.push(processor);
                    } else {
                        boot_processor = Some(processor);
                    }

                    // SAFETY: `ExtendedField::access` is unsafe iff a
                    // bogus revision is passed; we feed it the MADT's
                    // own header revision, which is authoritative.
                    let trbe_interrupt = unsafe { trbe_interrupt_field.access(revision) };

                    cpu_interfaces.push(GicCpuInterface {
                        cpu_interface_number,
                        processor_uid,
                        mpidr,
                        gic_registers_address,
                        gicr_base_address,
                        performance_interrupt_gsiv,
                        vgic_maintenance_interrupt,
                        spe_overflow_interrupt,
                        trbe_interrupt,
                        flags,
                    });
                }

                MadtEntry::GicRedistributor(r) => redistributor_ranges.push(GicRedistributorRange {
                    discovery_range_base_address: r.discovery_range_base_address,
                    discovery_range_length: r.discovery_range_length,
                }),

                MadtEntry::GicMsiFrame(m) => msi_frames.push(GicMsiFrame {
                    frame_id: m.frame_id,
                    physical_base_address: m.physical_base_address,
                    flags: m.flags,
                    spi_count: m.spi_count,
                    spi_base: m.spi_base,
                }),

                MadtEntry::GicInterruptTranslationService(t) => {
                    its_blocks.push(GicIts { id: t.id, physical_base_address: t.physical_base_address })
                }

                // GIC systems may also carry `MultiprocessorWakeup`,
                // `NmiSource`, etc. — these are either x86-specific
                // (handled above by the dispatch refusing to pick GIC
                // when they appear) or future arch additions.  Skip.
                _ => (),
            }
        }

        let processor_info = boot_processor.map(|bsp| ProcessorInfo::new_in(bsp, application_processors));

        Ok((
            InterruptModel::Gic(Gic::new(
                distributors,
                cpu_interfaces,
                redistributor_ranges,
                msi_frames,
                its_blocks,
            )),
            processor_info,
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

/// Per-CPU NMI line annotation derived from MADT
/// [`LocalApicNmiEntry`](crate::sdt::madt::LocalApicNmiEntry) /
/// [`X2ApicNmiEntry`](crate::sdt::madt::X2ApicNmiEntry).  Targets one
/// of the LAPIC LINT pins on a specific CPU (or all CPUs) for NMI
/// delivery, and carries the MPS INTI polarity / trigger-mode that
/// the receiver should program into its LVT entry.
#[derive(Debug, Clone, Copy)]
pub struct NmiLine {
    pub processor: NmiProcessor,
    pub line: LocalInterruptLine,
    /// Decoded from the MADT entry's `flags` field via the same MPS
    /// INTI parsing used for [`InterruptSourceOverride`] /
    /// [`NmiSource`].  `Polarity::SameAsBus` and
    /// `TriggerMode::SameAsBus` are the spec's "use the bus default"
    /// sentinels — for a LAPIC LINT pin that means edge / active-high,
    /// which is the conventional default firmware programs.
    pub polarity: Polarity,
    pub trigger_mode: TriggerMode,
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

// ============================================================================
// GIC (aarch64) parsed types
// ============================================================================

/// Parsed view of the aarch64 GIC interrupt model.  Mirrors the
/// [`Apic`] shape: per-block `Vec<_, A>` lists, allocated in the
/// caller-supplied arena.
#[derive(Debug, Clone)]
pub struct Gic<A: Allocator = Global> {
    /// GIC distributors (GICD entries).  Modern systems have exactly
    /// one; multi-cluster designs may have one per cluster.
    pub distributors: Vec<GicDistributor, A>,

    /// Per-CPU GIC CPU Interface descriptions, one entry per
    /// `GiccEntry` in the MADT.  These also carry per-core metadata
    /// the runtime needs (GICR base on GICv3+, performance interrupt
    /// GSIV, vGIC maintenance interrupt, MPIDR for join).
    pub cpu_interfaces: Vec<GicCpuInterface, A>,

    /// GICR discovery ranges (GICv3+ only).  Each entry describes a
    /// contiguous physical-address range to scan for redistributors
    /// belonging to enumerated CPUs.  Empty on GICv1/v2 (per-CPU
    /// GICR base is reported inline on each [`GicCpuInterface`]).
    pub redistributor_ranges: Vec<GicRedistributorRange, A>,

    /// GICv2m MSI frames.  Each frame translates writes to its
    /// `MSIxxxxxxxx` register into a contiguous SPI range
    /// (`spi_base..spi_base + spi_count`).  Empty on GICv3+ systems
    /// that route MSIs through ITS instead.
    pub msi_frames: Vec<GicMsiFrame, A>,

    /// GIC Interrupt Translation Services (GICv3+ only).  Each ITS
    /// translates device-side MSI writes (DeviceID + EventID) into
    /// LPIs delivered through the GICRs.
    pub its_blocks: Vec<GicIts, A>,
}

impl<A: Allocator> Gic<A> {
    pub(crate) fn new(
        distributors: Vec<GicDistributor, A>,
        cpu_interfaces: Vec<GicCpuInterface, A>,
        redistributor_ranges: Vec<GicRedistributorRange, A>,
        msi_frames: Vec<GicMsiFrame, A>,
        its_blocks: Vec<GicIts, A>,
    ) -> Self {
        Self { distributors, cpu_interfaces, redistributor_ranges, msi_frames, its_blocks }
    }
}

/// One GIC distributor.  Modern systems have a single GICD covering
/// all CPUs; the `gic_version` byte selects v1/v2 vs v3/v4 register
/// layout (or `0` = "fall back to hardware discovery").
#[derive(Debug, Clone, Copy)]
pub struct GicDistributor {
    pub gic_id: u32,
    pub physical_base_address: u64,
    pub system_vector_base: u32,
    /// `0x01`/`0x02` = GICv1/v2 (4 KiB register window).
    /// `0x03`/`0x04` = GICv3/v4 (64 KiB register window).
    /// `0x00` = unspecified, fall back to hardware discovery.
    pub gic_version: u8,
}

/// One GIC CPU Interface entry — i.e. one core's worth of GIC-side
/// metadata.  On GICv3+ the per-CPU runtime accesses go through
/// system registers (`ICC_*_EL1`); `gic_registers_address` and
/// `gicr_base_address` are kept here for catalog completeness.
#[derive(Debug, Clone, Copy)]
pub struct GicCpuInterface {
    pub cpu_interface_number: u32,
    pub processor_uid: u32,
    pub mpidr: u64,
    /// GICv2 CPU-interface MMIO base.  Zero on GICv3+.
    pub gic_registers_address: u64,
    /// GICv3+ redistributor (GICR) base for this CPU.  Zero on
    /// GICv2 (use the discovery ranges in [`Gic::redistributor_ranges`]
    /// in combination with `gic_registers_address` instead).
    pub gicr_base_address: u64,
    /// Performance Monitor Interrupt GSIV (PPI).  Zero if absent.
    pub performance_interrupt_gsiv: u32,
    /// vGIC Maintenance Interrupt GSIV (PPI).  Zero if absent.
    pub vgic_maintenance_interrupt: u32,
    /// SPE Overflow Interrupt (ACPI 6.3+).  Zero if absent / older
    /// firmware.
    pub spe_overflow_interrupt: u16,
    /// TRBE Interrupt (ACPI 6.6+, MADT revision ≥ 6).  `None` on
    /// older firmware where the field wasn't yet defined.
    pub trbe_interrupt: Option<u16>,
    /// Raw GICC flags field (`enabled`, `performance_interrupt_mode`,
    /// `vgic_signaled_via_pmu`, etc.).  Bit 0 = enabled.
    pub flags: u32,
}

impl GicCpuInterface {
    /// Convenience: is this CPU firmware-enabled? (GICC flags bit 0.)
    #[inline]
    pub fn is_enabled(&self) -> bool {
        self.flags & 1 != 0
    }
}

/// One GICR discovery range (GICv3+).
#[derive(Debug, Clone, Copy)]
pub struct GicRedistributorRange {
    pub discovery_range_base_address: u64,
    pub discovery_range_length: u32,
}

/// One GICv2m MSI frame.
#[derive(Debug, Clone, Copy)]
pub struct GicMsiFrame {
    pub frame_id: u32,
    pub physical_base_address: u64,
    pub flags: u32,
    pub spi_count: u16,
    pub spi_base: u16,
}

/// One GIC ITS block (GICv3+).
#[derive(Debug, Clone, Copy)]
pub struct GicIts {
    pub id: u32,
    pub physical_base_address: u64,
}
