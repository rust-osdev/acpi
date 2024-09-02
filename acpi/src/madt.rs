use crate::{
    sdt::{ExtendedField, SdtHeader, Signature}, AcpiTable
};
use bit_field::BitField;
use core::{marker::PhantomData, mem};

#[cfg(feature = "allocator_api")]
use crate::{
    platform::{
        interrupt::{InterruptModel, Polarity, TriggerMode},
        processor::ProcessorInfo,
    },
    AcpiResult,
};

#[derive(Debug)]
pub enum MadtError {
    UnexpectedEntry,
    InterruptOverrideEntryHasInvalidBus,
    InvalidLocalNmiLine,
    MpsIntiInvalidPolarity,
    MpsIntiInvalidTriggerMode,
}

/// Represents the MADT - this contains the MADT header fields. You can then iterate over a `Madt`
/// to read each entry from it.
///
/// In modern versions of ACPI, the MADT can detail one of four interrupt models:
///     * The ancient dual-i8259 legacy PIC model
///     * The Advanced Programmable Interrupt Controller (APIC) model
///     * The Streamlined Advanced Programmable Interrupt Controller (SAPIC) model (for Itanium systems)
///     * The Generic Interrupt Controller (GIC) model (for ARM systems)
#[repr(C, packed)]
#[derive(Debug, Clone, Copy)]
pub struct Madt {
    pub header: SdtHeader,
    pub local_apic_address: u32,
    pub flags: u32,
}

/// ### Safety: Implementation properly represents a valid MADT.
unsafe impl AcpiTable for Madt {
    const SIGNATURE: Signature = Signature::MADT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

use core::fmt;
impl fmt::Display for Madt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "MADT: {:#x?}", self.header)?;
        for entry in self.entries() {
            write!(f, "\n{:#x?}", entry)?
        }
        Ok(())
    }
}

impl Madt {
    #[cfg(feature = "allocator_api")]
    pub fn parse_interrupt_model_in<'a, A>(
        &self,
        allocator: A,
    ) -> AcpiResult<(InterruptModel<'a, A>, Option<ProcessorInfo<'a, A>>)>
    where
        A: core::alloc::Allocator + Clone,
    {
        /*
         * We first do a pass through the MADT to determine which interrupt model is being used.
         */
        for entry in self.entries() {
            match entry {
                MadtEntry::LocalApic(_) |
                MadtEntry::LocalX2Apic(_) |
                MadtEntry::IoApic(_) |
                MadtEntry::InterruptSourceOverride(_) |
                MadtEntry::NmiSource(_) |   // TODO: is this one used by more than one model?
                MadtEntry::LocalApicNmi(_) |
                MadtEntry::X2ApicNmi(_) |
                MadtEntry::LocalApicAddressOverride(_) => {
                    return self.parse_apic_model_in(allocator);
                }

                MadtEntry::IoSapic(_) |
                MadtEntry::LocalSapic(_) |
                MadtEntry::PlatformInterruptSource(_) => {
                    unimplemented!();
                }

                MadtEntry::Gicc(_) |
                MadtEntry::Gicd(_) |
                MadtEntry::GicMsiFrame(_) |
                MadtEntry::GicRedistributor(_) |
                MadtEntry::GicInterruptTranslationService(_) => {
                    return self.parse_gic_model_in(allocator);
                }

                MadtEntry::MultiprocessorWakeup(_) => ()
            }
        }

        Ok((InterruptModel::Unknown, None))
    }

    #[cfg(feature = "allocator_api")]
    fn parse_apic_model_in<'a, A>(
        &self,
        allocator: A,
    ) -> AcpiResult<(InterruptModel<'a, A>, Option<ProcessorInfo<'a, A>>)>
    where
        A: core::alloc::Allocator + Clone,
    {
        use crate::{
            platform::{
                interrupt::{
                    Apic,
                    InterruptSourceOverride,
                    IoApic,
                    LocalInterruptLine,
                    NmiLine,
                    NmiProcessor,
                    NmiSource,
                },
                processor::{
                    X86Processor,
                    ProcessorState,
                }, 
            },
            AcpiError,
        };

        let mut local_apic_address = self.local_apic_address as u64;
        let mut io_apic_count = 0;
        let mut iso_count = 0;
        let mut nmi_source_count = 0;
        let mut local_nmi_line_count = 0;
        let mut processor_count = 0usize;

        // Do a pass over the entries so we know how much space we should reserve in the vectors
        for entry in self.entries() {
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

        let mut io_apics = crate::ManagedSlice::new_in(io_apic_count, allocator.clone())?;
        let mut interrupt_source_overrides = crate::ManagedSlice::new_in(iso_count, allocator.clone())?;
        let mut nmi_sources = crate::ManagedSlice::new_in(nmi_source_count, allocator.clone())?;
        let mut local_apic_nmi_lines = crate::ManagedSlice::new_in(local_nmi_line_count, allocator.clone())?;
        let mut application_processors =
            crate::ManagedSlice::new_in(processor_count, allocator)?; // Subtract one for the BSP
        let mut found_bsp = false;

        io_apic_count = 0;
        iso_count = 0;
        nmi_source_count = 0;
        local_nmi_line_count = 0;
        processor_count = 1;

        for entry in self.entries() {
            match entry {
                MadtEntry::LocalApic(entry) => {
                    /*
                     * The first processor is the BSP. Subsequent ones are APs. If we haven't found
                     * the BSP yet, this must be it.
                     */
                    let is_ap = found_bsp;
                    let is_disabled = !{ entry.flags }.get_bit(0);

                    let state = match (is_ap, is_disabled) {
                        (_, true) => ProcessorState::Disabled,
                        (true, false) => ProcessorState::WaitingForSipi,
                        (false, false) => ProcessorState::Running,
                    };

                    let processor = X86Processor {
                        processor_uid: entry.processor_id as u32,
                        local_apic_id: entry.apic_id as u32,
                        state,
                        is_ap,
                    };

                    if is_ap {
                        application_processors[processor_count] = processor;
                        processor_count += 1;
                    } else {
                        application_processors[0] = processor;
                        found_bsp = true;
                    }
                }

                MadtEntry::LocalX2Apic(entry) => {
                    let is_ap = found_bsp;
                    let is_disabled = !{ entry.flags }.get_bit(0);

                    let state = match (is_ap, is_disabled) {
                        (_, true) => ProcessorState::Disabled,
                        (true, false) => ProcessorState::WaitingForSipi,
                        (false, false) => ProcessorState::Running,
                    };

                    let processor = X86Processor {
                        processor_uid: entry.processor_uid,
                        local_apic_id: entry.x2apic_id,
                        state,
                        is_ap,
                    };

                    if is_ap {
                        application_processors[processor_count] = processor;
                        processor_count += 1;
                    } else {
                        application_processors[0] = processor;
                        found_bsp = true;
                    }
                }

                MadtEntry::IoApic(entry) => {
                    io_apics[io_apic_count] = IoApic {
                        id: entry.io_apic_id,
                        address: entry.io_apic_address,
                        global_system_interrupt_base: entry.global_system_interrupt_base,
                    };
                    io_apic_count += 1;
                }

                MadtEntry::InterruptSourceOverride(entry) => {
                    if entry.bus != 0 {
                        return Err(AcpiError::InvalidMadt(MadtError::InterruptOverrideEntryHasInvalidBus));
                    }

                    let (polarity, trigger_mode) = parse_mps_inti_flags(entry.flags)?;

                    interrupt_source_overrides[iso_count] = InterruptSourceOverride {
                        isa_source: entry.irq,
                        global_system_interrupt: entry.global_system_interrupt,
                        polarity,
                        trigger_mode,
                    };
                    iso_count += 1;
                }

                MadtEntry::NmiSource(entry) => {
                    let (polarity, trigger_mode) = parse_mps_inti_flags(entry.flags)?;

                    nmi_sources[nmi_source_count] = NmiSource {
                        global_system_interrupt: entry.global_system_interrupt,
                        polarity,
                        trigger_mode,
                    };
                    nmi_source_count += 1;
                }

                MadtEntry::LocalApicNmi(entry) => {
                    local_apic_nmi_lines[local_nmi_line_count] = NmiLine {
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
                    };
                    local_nmi_line_count += 1;
                }

                MadtEntry::X2ApicNmi(entry) => {
                    local_apic_nmi_lines[local_nmi_line_count] = NmiLine {
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
                    };
                    local_nmi_line_count += 1;
                }

                MadtEntry::LocalApicAddressOverride(entry) => {
                    local_apic_address = entry.local_apic_address;
                }

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
                self.supports_8259(),
            )),
            Some(ProcessorInfo::new_x86(application_processors)),
        ))
    }

    fn parse_gic_model_in<'a, A>(
        &self,
        allocator: A,
    ) -> AcpiResult<(InterruptModel<'a, A>, Option<ProcessorInfo<'a, A>>)>
    where
        A: core::alloc::Allocator + Clone,
    {
        use crate::{
            platform::{
                interrupt::{
                    Gic,
                    Gicd,
                    Gicc,
                    GicMsiFrame,
                    Gicr,
                    GicIts,
                },
                processor::Arm64Processor,
            },
            AcpiError,
        };

        // need to count the number of each type of entry

        let mut gicc_count = 0;
        let mut gicd_count = 0;
        let mut gic_msi_frame_count = 0;
        let mut gicr_count = 0;
        let mut gic_its_count = 0;
        let mut processor_count = 0;

        for entry in self.entries() {
            match entry {
                MadtEntry::Gicc(_) => {gicc_count += 1; processor_count += 1;},
                MadtEntry::Gicd(_) => gicd_count += 1,
                MadtEntry::GicMsiFrame(_) => gic_msi_frame_count += 1,
                MadtEntry::GicRedistributor(_) => gicr_count += 1,
                MadtEntry::GicInterruptTranslationService(_) => gic_its_count += 1,
                _ => (),
            }
        }

        let mut gicc: crate::ManagedSlice<Gicc, A> = crate::ManagedSlice::new_in(gicc_count, allocator.clone())?;
        let mut gicd: crate::ManagedSlice<Gicd, A> = crate::ManagedSlice::new_in(gicd_count, allocator.clone())?;
        let mut gic_msi_frame: crate::ManagedSlice<GicMsiFrame, A> = crate::ManagedSlice::new_in(gic_msi_frame_count, allocator.clone())?;
        let mut gicr: crate::ManagedSlice<Gicr, A> = crate::ManagedSlice::new_in(gicr_count, allocator.clone())?;
        let mut gic_its: crate::ManagedSlice<GicIts, A> = crate::ManagedSlice::new_in(gic_its_count, allocator.clone())?;
        let mut processors: crate::ManagedSlice<Arm64Processor, A> = crate::ManagedSlice::new_in(processor_count, allocator.clone())?;
        // let mut boot_processor = None;

        gicc_count = 0;
        gicd_count = 0;
        gic_msi_frame_count = 0;
        gicr_count = 0;
        gic_its_count = 0;
        processor_count = 0;

        for entry in self.entries() {
            match entry {
                MadtEntry::Gicc(entry) => {
                    // TODO the ProcessorInfo struct is not implemented for aaarch64 yet.
                    // TODO need to implement the ProcessorInfo.
                    gicc[gicc_count] = Gicc {
                        cpu_interface_number: entry.cpu_interface_number,
                        acpi_processor_uid: entry.processor_uid,
                        flags: entry.flags,
                        parking_version: entry.parking_protocol_version,
                        performance_interrupt_gsiv: entry.performance_interrupt_gsiv,
                        parked_address: entry.parked_address,
                        physical_base_address: entry.gic_registers_address,
                        gicv: entry.gic_virtual_registers_address,
                        gich: entry.gic_hypervisor_registers_address,
                        vgic_maintenance_interrupt: entry.vgic_maintenance_interrupt,
                        gicr_base_address: entry.gicr_base_address,
                        mpidr: entry.mpidr,
                        processor_power_efficiency_class: entry.processor_power_efficiency_class,
                        spe_overflow_interrupt: entry.spe_overflow_interrupt,
                    };
                    processors[processor_count] = Arm64Processor {
                        processor_uid: entry.processor_uid as u32,
                        mpidr: entry.mpidr,
                        gicc_base_address: entry.gic_registers_address,
                    };
                    gicc_count += 1;
                    processor_count += 1;
                }
                MadtEntry::Gicd(entry) => {
                    gicd[gicd_count] = Gicd {
                        id: entry.gic_id,
                        physical_base_address: entry.physical_base_address,
                        gic_version: entry.gic_version,
                    };
                    gicd_count += 1;
                }
                MadtEntry::GicMsiFrame(entry) => {
                    gic_msi_frame[gic_msi_frame_count] = GicMsiFrame {
                        id: entry.frame_id,
                        physical_base_address: entry.physical_base_address,
                        spi_count_base_select: { entry.flags }.get_bit(0),
                        spi_count: entry.spi_count,
                        spi_base: entry.spi_base,
                    };
                    gic_msi_frame_count += 1;
                }
                MadtEntry::GicRedistributor(entry) => {
                    gicr[gicr_count] = Gicr {
                        base_address: entry.discovery_range_base_address,
                        length: entry.discovery_range_length,
                    };
                    gicr_count += 1;
                }
                MadtEntry::GicInterruptTranslationService(entry) => {
                    gic_its[gic_its_count] = GicIts {
                        id: entry.id,
                        physical_base_address: entry.physical_base_address,
                    };
                    gic_its_count += 1;
                }
                _ => {
                    return Err(AcpiError::InvalidMadt(MadtError::UnexpectedEntry));
                },
            }
        }

        Ok((InterruptModel::Gic(Gic::new(
            gicc,
            gicd,
            gic_msi_frame,
            gicr,
            gic_its,
        )), Some(ProcessorInfo::new_arm64(processors))))
    }

    pub fn entries(&self) -> MadtEntryIter {
        MadtEntryIter {
            pointer: unsafe { (self as *const Madt as *const u8).add(mem::size_of::<Madt>()) },
            remaining_length: self.header.length - mem::size_of::<Madt>() as u32,
            _phantom: PhantomData,
        }
    }

    pub fn supports_8259(&self) -> bool {
        { self.flags }.get_bit(0)
    }
}

#[derive(Debug)]
pub struct MadtEntryIter<'a> {
    pointer: *const u8,
    /*
     * The iterator can only have at most `u32::MAX` remaining bytes, because the length of the
     * whole SDT can only be at most `u32::MAX`.
     */
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>,
}

#[derive(Debug)]
pub enum MadtEntry<'a> {
    LocalApic(&'a LocalApicEntry),
    IoApic(&'a IoApicEntry),
    InterruptSourceOverride(&'a InterruptSourceOverrideEntry),
    NmiSource(&'a NmiSourceEntry),
    LocalApicNmi(&'a LocalApicNmiEntry),
    LocalApicAddressOverride(&'a LocalApicAddressOverrideEntry),
    IoSapic(&'a IoSapicEntry),
    LocalSapic(&'a LocalSapicEntry),
    PlatformInterruptSource(&'a PlatformInterruptSourceEntry),
    LocalX2Apic(&'a LocalX2ApicEntry),
    X2ApicNmi(&'a X2ApicNmiEntry),
    Gicc(&'a GiccEntry),
    Gicd(&'a GicdEntry),
    GicMsiFrame(&'a GicMsiFrameEntry),
    GicRedistributor(&'a GicRedistributorEntry),
    GicInterruptTranslationService(&'a GicInterruptTranslationServiceEntry),
    MultiprocessorWakeup(&'a MultiprocessorWakeupEntry),
}

impl<'a> Iterator for MadtEntryIter<'a> {
    type Item = MadtEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.remaining_length > 0 {
            let entry_pointer = self.pointer;
            let header = unsafe { *(self.pointer as *const EntryHeader) };

            self.pointer = unsafe { self.pointer.offset(header.length as isize) };
            self.remaining_length -= header.length as u32;

            macro_rules! construct_entry {
                ($entry_type:expr,
                 $entry_pointer:expr,
                 $(($value:expr => $variant:path as $type:ty)),*
                ) => {
                    match $entry_type {
                        $(
                            $value => {
                                return Some($variant(unsafe {
                                    &*($entry_pointer as *const $type)
                                }))
                            }
                         )*

                        /*
                         * These entry types are reserved by the ACPI standard. We should skip them
                         * if they appear in a real MADT.
                         */
                        0x11..=0x7f => {}

                        /*
                         * These entry types are reserved for OEM use. Atm, we just skip them too.
                         * TODO: work out if we should ever do anything else here
                         */
                        0x80..=0xff => {}
                    }
                }
            }

            #[rustfmt::skip]
            construct_entry!(
                header.entry_type,
                entry_pointer,
                (0x0 => MadtEntry::LocalApic as LocalApicEntry),
                (0x1 => MadtEntry::IoApic as IoApicEntry),
                (0x2 => MadtEntry::InterruptSourceOverride as InterruptSourceOverrideEntry),
                (0x3 => MadtEntry::NmiSource as NmiSourceEntry),
                (0x4 => MadtEntry::LocalApicNmi as LocalApicNmiEntry),
                (0x5 => MadtEntry::LocalApicAddressOverride as LocalApicAddressOverrideEntry),
                (0x6 => MadtEntry::IoSapic as IoSapicEntry),
                (0x7 => MadtEntry::LocalSapic as LocalSapicEntry),
                (0x8 => MadtEntry::PlatformInterruptSource as PlatformInterruptSourceEntry),
                (0x9 => MadtEntry::LocalX2Apic as LocalX2ApicEntry),
                (0xa => MadtEntry::X2ApicNmi as X2ApicNmiEntry),
                (0xb => MadtEntry::Gicc as GiccEntry),
                (0xc => MadtEntry::Gicd as GicdEntry),
                (0xd => MadtEntry::GicMsiFrame as GicMsiFrameEntry),
                (0xe => MadtEntry::GicRedistributor as GicRedistributorEntry),
                (0xf => MadtEntry::GicInterruptTranslationService as GicInterruptTranslationServiceEntry),
                (0x10 => MadtEntry::MultiprocessorWakeup as MultiprocessorWakeupEntry)
            );
        }

        None
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct EntryHeader {
    pub entry_type: u8,
    pub length: u8,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalApicEntry {
    pub header: EntryHeader,
    pub processor_id: u8,
    pub apic_id: u8,
    pub flags: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct IoApicEntry {
    pub header: EntryHeader,
    pub io_apic_id: u8,
    _reserved: u8,
    pub io_apic_address: u32,
    pub global_system_interrupt_base: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct InterruptSourceOverrideEntry {
    pub header: EntryHeader,
    pub bus: u8, // 0 - ISA bus
    pub irq: u8, // This is bus-relative
    pub global_system_interrupt: u32,
    pub flags: u16,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct NmiSourceEntry {
    pub header: EntryHeader,
    pub flags: u16,
    pub global_system_interrupt: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalApicNmiEntry {
    pub header: EntryHeader,
    pub processor_id: u8,
    pub flags: u16,
    pub nmi_line: u8, // Describes which LINTn is the NMI connected to
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalApicAddressOverrideEntry {
    pub header: EntryHeader,
    _reserved: u16,
    pub local_apic_address: u64,
}

/// If this entry is present, the system has an I/O SAPIC, which must be used instead of the I/O
/// APIC.
#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct IoSapicEntry {
    pub header: EntryHeader,
    pub io_apic_id: u8,
    _reserved: u8,
    pub global_system_interrupt_base: u32,
    pub io_sapic_address: u64,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalSapicEntry {
    pub header: EntryHeader,
    pub processor_id: u8,
    pub local_sapic_id: u8,
    pub local_sapic_eid: u8,
    _reserved: [u8; 3],
    pub flags: u32,
    pub processor_uid: u32,

    /// This string can be used to associate this local SAPIC to a processor defined in the
    /// namespace when the `_UID` object is a string. It is a null-terminated ASCII string, and so
    /// this field will be `'\0'` if the string is not present, otherwise it extends from the
    /// address of this field.
    processor_uid_string: u8,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct PlatformInterruptSourceEntry {
    pub header: EntryHeader,
    pub flags: u16,
    pub interrupt_type: u8,
    pub processor_id: u8,
    pub processor_eid: u8,
    pub io_sapic_vector: u8,
    pub global_system_interrupt: u32,
    pub platform_interrupt_source_flags: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalX2ApicEntry {
    pub header: EntryHeader,
    _reserved: u16,
    pub x2apic_id: u32,
    pub flags: u32,
    pub processor_uid: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct X2ApicNmiEntry {
    pub header: EntryHeader,
    pub flags: u16,
    pub processor_uid: u32,
    pub nmi_line: u8,
    _reserved: [u8; 3],
}

/// This field will appear for ARM processors that support ACPI and use the Generic Interrupt
/// Controller. In the GICC interrupt model, each logical process has a Processor Device object in
/// the namespace, and uses this structure to convey its GIC information.
#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GiccEntry {
    pub header: EntryHeader,
    _reserved1: u16,
    pub cpu_interface_number: u32,
    pub processor_uid: u32,
    pub flags: u32,
    pub parking_protocol_version: u32,
    pub performance_interrupt_gsiv: u32,
    pub parked_address: u64,
    pub gic_registers_address: u64,
    pub gic_virtual_registers_address: u64,
    pub gic_hypervisor_registers_address: u64,
    pub vgic_maintenance_interrupt: u32,
    pub gicr_base_address: u64,
    pub mpidr: u64,
    pub processor_power_efficiency_class: u8,
    _reserved2: u8,
    /// SPE overflow Interrupt.
    ///
    /// ACPI 6.3 defined this field. It is zero in prior versions or
    /// if this processor does not support SPE.
    pub spe_overflow_interrupt: u16,
    pub trbe_interrupt: ExtendedField<u16, 6>,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GicdEntry {
    pub header: EntryHeader,
    _reserved1: u16,
    pub gic_id: u32,
    pub physical_base_address: u64,
    pub system_vector_base: u32,

    /// The GIC version
    ///     0x00: Fall back to hardware discovery
    ///     0x01: GICv1
    ///     0x02: GICv2
    ///     0x03: GICv3
    ///     0x04: GICv4
    ///     0x05-0xff: Reserved for future use
    pub gic_version: u8,
    _reserved2: [u8; 3],
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GicMsiFrameEntry {
    pub header: EntryHeader,
    _reserved: u16,
    pub frame_id: u32,
    pub physical_base_address: u64,
    pub flags: u32,
    pub spi_count: u16,
    pub spi_base: u16,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GicRedistributorEntry {
    pub header: EntryHeader,
    _reserved: u16,
    pub discovery_range_base_address: u64,
    pub discovery_range_length: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GicInterruptTranslationServiceEntry {
    pub header: EntryHeader,
    _reserved1: u16,
    pub id: u32,
    pub physical_base_address: u64,
    _reserved2: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct MultiprocessorWakeupEntry {
    pub header: EntryHeader,
    pub mailbox_version: u16,
    _reserved: u32,
    pub mailbox_address: u64,
}

#[cfg(feature = "allocator_api")]
fn parse_mps_inti_flags(flags: u16) -> crate::AcpiResult<(Polarity, TriggerMode)> {
    let polarity = match flags.get_bits(0..2) {
        0b00 => Polarity::SameAsBus,
        0b01 => Polarity::ActiveHigh,
        0b11 => Polarity::ActiveLow,
        _ => return Err(crate::AcpiError::InvalidMadt(MadtError::MpsIntiInvalidPolarity)),
    };

    let trigger_mode = match flags.get_bits(2..4) {
        0b00 => TriggerMode::SameAsBus,
        0b01 => TriggerMode::Edge,
        0b11 => TriggerMode::Level,
        _ => return Err(crate::AcpiError::InvalidMadt(MadtError::MpsIntiInvalidTriggerMode)),
    };

    Ok((polarity, trigger_mode))
}
