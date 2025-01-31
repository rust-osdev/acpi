use crate::{
    sdt::{ExtendedField, SdtHeader, Signature},
    AcpiError,
    AcpiTable,
};
use bit_field::BitField;
use core::{
    marker::{PhantomData, PhantomPinned},
    mem,
    pin::Pin,
};

#[cfg(feature = "allocator_api")]
use crate::{
    platform::{
        interrupt::{InterruptModel, Polarity, TriggerMode},
        ProcessorInfo,
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
    WakeupApsTimeout,
}

/// Represents the MADT - this contains the MADT header fields. You can then iterate over a `Madt`
/// to read each entry from it.
///
/// In modern versions of ACPI, the MADT can detail one of four interrupt models:
/// - The ancient dual-i8259 legacy PIC model
/// - The Advanced Programmable Interrupt Controller (APIC) model
/// - The Streamlined Advanced Programmable Interrupt Controller (SAPIC) model (for Itanium systems)
/// - The Generic Interrupt Controller (GIC) model (for ARM systems)
///
/// The MADT is a variable-sized structure consisting of a static header and then a variable number of entries.
/// This type only contains the static portion, and then uses pointer arithmetic to parse the following entries.
/// To make this sound, this type is `!Unpin` - this prevents you from getting anything other than a `Pin<&Madt>`
/// out of a `PhysicalMapping`, thereby preventing a `Madt` from being moved before [`Madt::entries`] is called.
#[repr(C, packed)]
#[derive(Debug)]
pub struct Madt {
    pub header: SdtHeader,
    pub local_apic_address: u32,
    pub flags: u32,
    _pinned: PhantomPinned,
}

/// ### Safety: Implementation properly represents a valid MADT.
unsafe impl AcpiTable for Madt {
    const SIGNATURE: Signature = Signature::MADT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Madt {
    pub fn get_mpwk_mailbox_addr(self: Pin<&Self>) -> Result<u64, AcpiError> {
        for entry in self.entries() {
            if let MadtEntry::MultiprocessorWakeup(entry) = entry {
                return Ok(entry.mailbox_address);
            }
        }
        Err(AcpiError::InvalidMadt(MadtError::UnexpectedEntry))
    }

    #[cfg(feature = "allocator_api")]
    pub fn parse_interrupt_model_in<'a, A>(
        self: Pin<&Self>,
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
                    unimplemented!();
                }

                MadtEntry::MultiprocessorWakeup(_) => ()
            }
        }

        Ok((InterruptModel::Unknown, None))
    }

    #[cfg(feature = "allocator_api")]
    fn parse_apic_model_in<'a, A>(
        self: Pin<&Self>,
        allocator: A,
    ) -> AcpiResult<(InterruptModel<'a, A>, Option<ProcessorInfo<'a, A>>)>
    where
        A: core::alloc::Allocator + Clone,
    {
        use crate::platform::{
            interrupt::{
                Apic,
                InterruptSourceOverride,
                IoApic,
                LocalInterruptLine,
                NmiLine,
                NmiProcessor,
                NmiSource,
            },
            Processor,
            ProcessorState,
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
            crate::ManagedSlice::new_in(processor_count.saturating_sub(1), allocator)?; // Subtract one for the BSP
        let mut boot_processor = None;

        io_apic_count = 0;
        iso_count = 0;
        nmi_source_count = 0;
        local_nmi_line_count = 0;
        processor_count = 0;

        for entry in self.entries() {
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
                        application_processors[processor_count] = processor;
                        processor_count += 1;
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
                        application_processors[processor_count] = processor;
                        processor_count += 1;
                    } else {
                        boot_processor = Some(processor);
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
                self.supports_8259(),
            )),
            Some(ProcessorInfo::new(boot_processor.unwrap(), application_processors)),
        ))
    }

    pub fn entries(self: Pin<&Self>) -> MadtEntryIter<'_> {
        let ptr = unsafe { Pin::into_inner_unchecked(self) as *const Madt as *const u8 };
        MadtEntryIter {
            pointer: unsafe { ptr.add(mem::size_of::<Madt>()) },
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

#[derive(Debug, PartialEq, Eq)]
pub enum MpProtectedModeWakeupCommand {
    Noop = 0,
    Wakeup = 1,
    Sleep = 2,
    AcceptPages = 3,
}

impl From<u16> for MpProtectedModeWakeupCommand {
    fn from(value: u16) -> Self {
        match value {
            0 => MpProtectedModeWakeupCommand::Noop,
            1 => MpProtectedModeWakeupCommand::Wakeup,
            2 => MpProtectedModeWakeupCommand::Sleep,
            3 => MpProtectedModeWakeupCommand::AcceptPages,
            _ => panic!("Invalid value for MpProtectedModeWakeupCommand"),
        }
    }
}

#[repr(C)]
pub struct MultiprocessorWakeupMailbox {
    pub command: u16,
    _reserved: u16,
    pub apic_id: u32,
    pub wakeup_vector: u64,
    pub reserved_for_os: [u64; 254],
    reserved_for_firmware: [u64; 256],
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
