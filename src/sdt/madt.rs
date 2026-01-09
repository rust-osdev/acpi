use crate::{
    AcpiError,
    AcpiTable,
    sdt::{ExtendedField, SdtHeader, Signature},
};
use bit_field::BitField;
use core::{
    marker::{PhantomData, PhantomPinned},
    mem,
    pin::Pin,
};
use log::warn;

#[derive(Clone, Copy, Debug)]
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
/// To make this sound, this type is `!Unpin` - this prevents `Madt` being moved, which would leave
/// the entries behind.
#[repr(C, packed)]
#[derive(Debug)]
pub struct Madt {
    pub header: SdtHeader,
    pub local_apic_address: u32,
    pub flags: u32,
    _pinned: PhantomPinned,
}

unsafe impl AcpiTable for Madt {
    const SIGNATURE: Signature = Signature::MADT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Madt {
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

    pub fn get_mpwk_mailbox_addr(self: Pin<&Self>) -> Result<u64, AcpiError> {
        for entry in self.entries() {
            if let MadtEntry::MultiprocessorWakeup(entry) = entry {
                return Ok(entry.mailbox_address);
            }
        }
        Err(AcpiError::InvalidMadt(MadtError::UnexpectedEntry))
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

            if header.length as u32 > self.remaining_length {
                warn!(
                    "Invalid entry of type {} in MADT - extending past length of table. Ignoring",
                    header.entry_type
                );
                return None;
            }

            self.pointer = unsafe { self.pointer.byte_offset(header.length as isize) };
            self.remaining_length = self.remaining_length.saturating_sub(header.length as u32);

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
    pub processor_uid_string: u8,
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
    pub reserved_for_firmware: [u64; 256],
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

pub fn parse_mps_inti_flags(flags: u16) -> Result<(Polarity, TriggerMode), AcpiError> {
    let polarity = match flags.get_bits(0..2) {
        0b00 => Polarity::SameAsBus,
        0b01 => Polarity::ActiveHigh,
        0b11 => Polarity::ActiveLow,
        _ => return Err(AcpiError::InvalidMadt(MadtError::MpsIntiInvalidPolarity)),
    };

    let trigger_mode = match flags.get_bits(2..4) {
        0b00 => TriggerMode::SameAsBus,
        0b01 => TriggerMode::Edge,
        0b11 => TriggerMode::Level,
        _ => return Err(AcpiError::InvalidMadt(MadtError::MpsIntiInvalidTriggerMode)),
    };

    Ok((polarity, trigger_mode))
}
