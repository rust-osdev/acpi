use core::marker::PhantomData;
use core::mem;
use sdt::SdtHeader;
use {Acpi, AcpiError, AcpiHandler, PhysicalMapping};

/// Represents the MADT - this contains the MADT header fields. You can then iterate over a `Madt`
/// to read each entry from it.
#[repr(C, packed)]
pub(crate) struct Madt {
    header: SdtHeader,
    local_apic_address: u32,
    flags: u32,
}

impl Madt {
    fn entries(&self) -> MadtEntryIter {
        MadtEntryIter {
            pointer: unsafe {
                (self as *const Madt as *const u8).offset(mem::size_of::<Madt>() as isize)
            },
            remaining_length: self.header.length() - mem::size_of::<Madt>() as u32,
            _phantom: PhantomData,
        }
    }
}

struct MadtEntryIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    _phantom: PhantomData<&'a u8>,
}

enum MadtEntry<'a> {
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
}

impl<'a> Iterator for MadtEntryIter<'a> {
    type Item = MadtEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.remaining_length > 0 {
            let entry_pointer = self.pointer;
            let header = unsafe { *(self.pointer as *const EntryHeader) };

            self.pointer = unsafe { self.pointer.offset(header.length as isize) };
            self.remaining_length -= header.length as u32;
            info!(
                "Found MADT entry of {} bytes (id={}), {} bytes remaining",
                header.length, header.entry_type, self.remaining_length
            );

            match header.entry_type {
                0x0 => {
                    return Some(MadtEntry::LocalApic(unsafe {
                        &*(entry_pointer as *const LocalApicEntry)
                    }))
                }

                0x1 => {
                    return Some(MadtEntry::IoApic(unsafe {
                        &*(entry_pointer as *const IoApicEntry)
                    }))
                }

                0x2 => {
                    return Some(MadtEntry::InterruptSourceOverride(unsafe {
                        &*(entry_pointer as *const InterruptSourceOverrideEntry)
                    }))
                }

                0x3 => {
                    return Some(MadtEntry::NmiSource(unsafe {
                        &*(entry_pointer as *const NmiSourceEntry)
                    }))
                }

                0x4 => {
                    return Some(MadtEntry::LocalApicNmi(unsafe {
                        &*(entry_pointer as *const LocalApicNmiEntry)
                    }))
                }

                0x5 => {
                    return Some(MadtEntry::LocalApicAddressOverride(unsafe {
                        &*(entry_pointer as *const LocalApicAddressOverrideEntry)
                    }))
                }

                0x6 => {
                    return Some(MadtEntry::IoSapic(unsafe {
                        &*(entry_pointer as *const IoSapicEntry)
                    }))
                }

                0x7 => {
                    return Some(MadtEntry::LocalSapic(unsafe {
                        &*(entry_pointer as *const LocalSapicEntry)
                    }))
                }

                0x8 => {
                    return Some(MadtEntry::PlatformInterruptSource(unsafe {
                        &*(entry_pointer as *const PlatformInterruptSourceEntry)
                    }))
                }

                0x9 => {
                    return Some(MadtEntry::LocalX2Apic(unsafe {
                        &*(entry_pointer as *const LocalX2ApicEntry)
                    }))
                }

                0xa => {
                    return Some(MadtEntry::X2ApicNmi(unsafe {
                        &*(entry_pointer as *const X2ApicNmiEntry)
                    }))
                }

                0xb => {
                    return Some(MadtEntry::Gicc(unsafe {
                        &*(entry_pointer as *const GiccEntry)
                    }))
                }

                0xc => {
                    return Some(MadtEntry::Gicd(unsafe {
                        &*(entry_pointer as *const GicdEntry)
                    }))
                }

                0xd => {
                    return Some(MadtEntry::GicMsiFrame(unsafe {
                        &*(entry_pointer as *const GicMsiFrameEntry)
                    }))
                }

                0xe => {
                    return Some(MadtEntry::GicRedistributor(unsafe {
                        &*(entry_pointer as *const GicRedistributorEntry)
                    }))
                }

                0xf => {
                    return Some(MadtEntry::GicInterruptTranslationService(unsafe {
                        &*(entry_pointer as *const GicInterruptTranslationServiceEntry)
                    }))
                }

                /*
                 * These entry types are reserved by the ACPI standard. We should skip them if they
                 * appear in a real MADT.
                 */
                0x10..0x7f => {}

                /*
                 * These entry types are reserved for OEM use. Atm, we just skip them too.
                 * TODO: work out if we should ever do anything else here
                 */
                0x80..0xff => {}

                // TODO: remove when support for exhaustive integer patterns is merged
                _ => unreachable!(),
            }
        }

        None
    }
}

#[derive(Clone, Copy)]
#[repr(C, packed)]
struct EntryHeader {
    entry_type: u8,
    length: u8,
}

#[repr(C, packed)]
struct LocalApicEntry {
    header: EntryHeader,
    processor_id: u8,
    apic_id: u8,
    flags: u32,
}

#[repr(C, packed)]
struct IoApicEntry {
    header: EntryHeader,
    io_apic_id: u8,
    _reserved: u8,
    io_apic_address: u32,
    global_system_interrupt_base: u32,
}

#[repr(C, packed)]
struct InterruptSourceOverrideEntry {
    header: EntryHeader,
    bus: u8, // 0 - ISA bus
    irq: u8, // This is bus-relative
    global_system_interrupt: u32,
    flags: u16,
}

#[repr(C, packed)]
struct NmiSourceEntry {
    header: EntryHeader,
    flags: u16,
    global_system_interrupt: u32,
}

#[repr(C, packed)]
struct LocalApicNmiEntry {
    header: EntryHeader,
    processor_id: u8,
    flags: u16,
    nmi_line: u8, // Describes which LINTn is the NMI connected to
}

#[repr(C, packed)]
struct LocalApicAddressOverrideEntry {
    header: EntryHeader,
    _reserved: u16,
    local_apic_address: u64,
}

/// If this entry is present, the system has an I/O SAPIC, which must be used instead of the I/O APIC.
#[repr(C, packed)]
struct IoSapicEntry {
    header: EntryHeader,
    io_apic_id: u8,
    _reserved: u8,
    global_system_interrupt_base: u32,
    io_sapic_address: u64,
}

#[repr(C, packed)]
struct LocalSapicEntry {
    header: EntryHeader,
    processor_id: u8,
    local_sapic_id: u8,
    local_sapic_eid: u8,
    _reserved: [u8; 3],
    flags: u32,
    processor_uid: u32,

    /// This string can be used to associate this local SAPIC to a processor defined in the
    /// namespace when the `_UID` object is a string. It is a null-terminated ASCII string, and so
    /// this field will be `'\0'` if the string is not present, otherwise it extends from the
    /// address of this field.
    processor_uid_string: u8,
}

#[repr(C, packed)]
struct PlatformInterruptSourceEntry {
    header: EntryHeader,
    flags: u16,
    interrupt_type: u8,
    processor_id: u8,
    processor_eid: u8,
    io_sapic_vector: u8,
    global_system_interrupt: u32,
    platform_interrupt_source_flags: u32,
}

#[repr(C, packed)]
struct LocalX2ApicEntry {
    header: EntryHeader,
    _reserved: u16,
    x2apic_id: u32,
    flags: u32,
    processor_uid: u32,
}

#[repr(C, packed)]
struct X2ApicNmiEntry {
    header: EntryHeader,
    flags: u16,
    processor_uid: u32,
    nmi_line: u8,
    _reserved: [u8; 3],
}

/// This field will appear for ARM processors that support ACPI and use the Generic Interrupt
/// Controller. In the GICC interrupt model, each logical process has a Processor Device object in
/// the namespace, and uses this structure to convey its GIC information.
#[repr(C, packed)]
struct GiccEntry {
    header: EntryHeader,
    _reserved1: u16,
    cpu_interface_number: u32,
    processor_uid: u32,
    flags: u32,
    parking_protocol_version: u32,
    performance_interrupt_gsiv: u32,
    parked_address: u64,
    gic_registers_address: u64,
    gic_control_block_address: u64,
    vgic_maintenance_interrupt: u32,
    gicr_base_address: u64,
    mpidr: u64,
    processor_power_efficiency_class: u8,
    _reserved2: [u8; 3],
}

#[repr(C, packed)]
struct GicdEntry {
    header: EntryHeader,
    _reserved1: u16,
    gic_id: u32,
    physical_base_address: u64,
    system_vector_base: u32,

    /// The GIC version
    ///     0x00: Fall back to hardware discovery
    ///     0x01: GICv1
    ///     0x02: GICv2
    ///     0x03: GICv3
    ///     0x04: GICv4
    ///     0x05-0xff: Reserved for future use
    gic_version: u8,
    _reserved2: [u8; 3],
}

#[repr(C, packed)]
struct GicMsiFrameEntry {
    header: EntryHeader,
    _reserved: u16,
    frame_id: u32,
    physical_base_address: u64,
    flags: u32,
    spi_count: u16,
    spi_base: u16,
}

#[repr(C, packed)]
struct GicRedistributorEntry {
    header: EntryHeader,
    _reserved: u16,
    discovery_range_base_address: u64,
    discovery_range_length: u32,
}

#[repr(C, packed)]
struct GicInterruptTranslationServiceEntry {
    header: EntryHeader,
    _reserved1: u16,
    id: u32,
    physical_base_address: u64,
    _reserved2: u32,
}

pub(crate) fn parse_madt<'a, 'h, H>(
    acpi: &'a mut Acpi<'h, H>,
    mapping: &PhysicalMapping<Madt>,
) -> Result<(), AcpiError>
where
    'h: 'a,
    H: AcpiHandler + 'a,
{
    (*mapping).header.validate(b"APIC")?;

    for entry in (*mapping).entries() {
        info!("Found MADT entry");
        // TODO: parse entries
    }

    Ok(())
}
