use core::{marker::{PhantomData, PhantomPinned}, mem, pin::Pin};
use core::alloc::Allocator;

use bitflags::bitflags;

use crate::{platform::{MemoryInfo, ProcessorTopology}, sdt::{SdtHeader, Signature}, AcpiResult, AcpiTable};

/// System Resource Affinity Table (SRAT).
/// 
/// This optional table provides information that allows OSPM to associate the following types of
/// devices with system locality / proximity domains and clock domains:
/// - processors,
/// - memory ranges (including those provided by hot-added memory devices), and
/// - generic initiators (e.g. heterogeneous processors and accelerators, GPUs, and I/O devices
///   with integrated compute or DMA engines).
#[repr(C, packed)]
pub struct Srat {
    header: SdtHeader,
    _reserved_1: u32,
    _reserved_2: u64,
    _pinned: PhantomPinned,
}

/// ### Safety: Implementation properly represents a valid SRAT.
unsafe impl AcpiTable for Srat {
    const SIGNATURE: Signature = Signature::SRAT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Srat {
    #[cfg(feature = "allocator_api")]
    pub fn parse_topology_in<'a, A>(
        self: Pin<&Self>,
        allocator: A
    ) -> AcpiResult<(ProcessorTopology<'a, A>, MemoryInfo<'a, A>)>
    where
        A: Allocator + Clone,
    {
        for entry in self.entries() {
            match entry {
                SratEntry::LocalApic(_) |
                SratEntry::LocalX2Apic(_) => {
                    return self.parse_apic_topology_in(allocator);
                }
                SratEntry::Gicc(_) |
                SratEntry::GicIts(_) => {
                    unimplemented!();
                }
                SratEntry::Memory(_) |
                SratEntry::GenericInitiator(_) => {}
            };
        }

        unreachable!();
    }

    #[cfg(feature = "allocator_api")]
    pub fn parse_apic_topology_in<'a, A>(
        self: Pin<&Self>,
        allocator: A
    ) -> AcpiResult<(ProcessorTopology<'a, A>, MemoryInfo<'a, A>)>
    where
        A: Allocator + Clone,
    {
        use crate::{platform::{MemoryRange, ProcessorAffinity}, ManagedSlice};

        let mut processor_count = 0;
        let mut memory_range_count = 0;

        for entry in self.entries() {
            match entry {
                SratEntry::LocalApic(_) |
                SratEntry::LocalX2Apic(_) => processor_count += 1,
                SratEntry::Memory(_) => memory_range_count += 1,
                _ => (),
            }
        }

        let mut processor_affinities = ManagedSlice::new_in(processor_count, allocator.clone())?;
        let mut memory_ranges = ManagedSlice::new_in(memory_range_count, allocator.clone())?;

        processor_count = 0;
        memory_range_count = 0;

        for entry in self.entries() {
            match entry {
                SratEntry::LocalApic(entry) => {
                    let local_apic_id = entry.apic_id as u32;
                    let mut proximity_domain = entry.proximity_domain_low as u32;
                    for i in 0..3 {
                        let shift = 8 * (3 - i);
                        proximity_domain += (entry.proximity_domain_high[i] as u32) << shift;
                    }
                    let flags = entry.flags;
                    let is_enabled = flags.contains(LocalApicAffinityFlags::ENABLED);
                    let processor_affinity = ProcessorAffinity { local_apic_id, proximity_domain, is_enabled };
                    processor_affinities[processor_count] = processor_affinity;
                    processor_count += 1;
                }
                SratEntry::LocalX2Apic(entry) => {
                    let local_apic_id = entry.x2apic_id;
                    let proximity_domain = entry.proximity_domain;
                    let flags = entry.flags;
                    let is_enabled = flags.contains(LocalX2ApicAffinityFlags::ENABLED);
                    let processor_affinity = ProcessorAffinity { local_apic_id, proximity_domain, is_enabled };
                    processor_affinities[processor_count] = processor_affinity;
                    processor_count += 1;
                }
                SratEntry::Memory(entry) => {
                    let flags = entry.flags;
                    let base_address = entry.base_address_low as u64 + ((entry.base_address_high as u64) << 32);
                    let length = entry.length_low as u64 + ((entry.length_high as u64) << 32);
                    let proximity_domain = Some(entry.proximity_domain);
                    let hot_pluggable = flags.contains(MemoryAffinityFlags::HOT_PLUGGABLE);
                    let non_volatile = flags.contains(MemoryAffinityFlags::NON_VOLATILE);
                    let is_enabled = flags.contains(MemoryAffinityFlags::ENABLED);
                    let memory_range = MemoryRange {
                        base_address,
                        length,
                        proximity_domain,
                        hot_pluggable,
                        non_volatile,
                        is_enabled,
                    };
                    memory_ranges[memory_range_count] = memory_range;
                    memory_range_count += 1;
                }
                // TODO: parse information of generic initiators
                SratEntry::GenericInitiator(_) => {}
                _ => {}
            }
        }

        let processor_topology = ProcessorTopology { processor_affinities };
        let memory_info = MemoryInfo { memory_ranges };
        Ok((processor_topology, memory_info))
    }

    fn entries(self: Pin<&Self>) -> SratEntryIter {
        let ptr = unsafe { Pin::into_inner_unchecked(self) as *const Srat as *const u8 };
        SratEntryIter {
            pointer: unsafe { ptr.add(mem::size_of::<Srat>()) },
            remaining_length: self.header.length - mem::size_of::<Srat>() as u32,
            _phantom: PhantomData,
        }
    }
}

struct SratEntryIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> Iterator for SratEntryIter<'a> {
    type Item = SratEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length > 0 {
            let entry_pointer = self.pointer;
            let entry_header = unsafe { *(self.pointer as *const AffinityEntryHeader) };

            self.pointer = unsafe { self.pointer.offset(entry_header.length as isize) };
            self.remaining_length -= entry_header.length as u32;

            match entry_header.r#type {
                AffinityEntryType::LocalApic => Some(SratEntry::LocalApic(unsafe { &*(entry_pointer as *const LocalApicAffinityEntry) })),
                AffinityEntryType::Memory => Some(SratEntry::Memory(unsafe { &*(entry_pointer as *const MemoryAffinityEntry) })),
                AffinityEntryType::LocalX2Apic => Some(SratEntry::LocalX2Apic(unsafe { &*(entry_pointer as *const LocalX2ApicAffinityEntry) })),
                AffinityEntryType::Gicc => Some(SratEntry::Gicc(unsafe { &*(entry_pointer as *const GiccAffinityEntry) })),
                AffinityEntryType::GicIts => Some(SratEntry::GicIts(unsafe { &*(entry_pointer as *const GicItsAffinityEntry) })),
                AffinityEntryType::GenericInitiator => Some(SratEntry::GenericInitiator(unsafe { &*(entry_pointer as *const GenericInitiatorAffinityEntry) })),
            }
        } else {
            None
        }
    }
}

enum SratEntry <'a>{
    LocalApic(&'a LocalApicAffinityEntry),
    Memory(&'a MemoryAffinityEntry),
    LocalX2Apic(&'a LocalX2ApicAffinityEntry),
    // TODO: remove the attributes after we can parse these entries.
    #[allow(unused)]
    Gicc(&'a GiccAffinityEntry),
    #[allow(unused)]
    GicIts(&'a GicItsAffinityEntry),
    #[allow(unused)]
    GenericInitiator(&'a GenericInitiatorAffinityEntry),
}

#[derive(Clone, Copy)]
#[repr(C, packed)]
struct AffinityEntryHeader {
    r#type: AffinityEntryType,
    length: u8,
}

#[allow(unused)]
#[derive(Clone, Copy)]
#[repr(u8)]
enum AffinityEntryType {
    LocalApic = 0,
    Memory = 1,
    LocalX2Apic = 2,
    Gicc = 3,
    GicIts = 4,
    GenericInitiator = 5,
}

#[repr(C, packed)]
struct LocalApicAffinityEntry {
    header: AffinityEntryHeader,
    proximity_domain_low: u8,
    apic_id: u8,
    flags: LocalApicAffinityFlags,
    local_sapic_eid: u8,
    proximity_domain_high: [u8; 3],
    clock_domain: u32,
}

bitflags! {
    #[derive(Clone, Copy)]
    struct LocalApicAffinityFlags: u32 {
        const ENABLED = 1;
    }
}

#[repr(C, packed)]
struct MemoryAffinityEntry {
    header: AffinityEntryHeader,
    proximity_domain: u32,
    _reserved_1: u16,
    base_address_low: u32,
    base_address_high: u32,
    length_low: u32,
    length_high: u32,
    _reserved_2: u32,
    flags: MemoryAffinityFlags,
    _reserved_3: u64,
}

bitflags! {
    #[derive(Clone, Copy)]
    struct MemoryAffinityFlags: u32 {
        const ENABLED = 1;
        const HOT_PLUGGABLE = 1 << 1;
        const NON_VOLATILE = 1 << 2;
    }
}

#[repr(C, packed)]
struct LocalX2ApicAffinityEntry {
    header: AffinityEntryHeader,
    _reserved_1: u16,
    proximity_domain: u32,
    x2apic_id: u32,
    flags: LocalX2ApicAffinityFlags,
    clock_domain: u32,
    _reserved_2: u32,
}

type LocalX2ApicAffinityFlags = LocalApicAffinityFlags;

#[repr(C, packed)]
struct GiccAffinityEntry {
    header: AffinityEntryHeader,
    proximity_domain: u32,
    acpi_processor_uid: u32,
    flags: GiccAffinityFlags,
    clock_domain: u32,
}

type GiccAffinityFlags = LocalApicAffinityFlags;

#[repr(C, packed)]
struct GicItsAffinityEntry {
    header: AffinityEntryHeader,
    proximity_domain: u32,
    _reserved: u16,
    its_id: u32,
}

#[repr(C, packed)]
struct GenericInitiatorAffinityEntry {
    header: AffinityEntryHeader,
    _reserved_1: u8,
    device_handle_type: DeviceHandleType,
    proximity_domain: u32,
    device_handle: DeviceHandle,
    flags: GenericInitiatorAffinityFlags,
    _reserved_2: u32,
}

#[allow(unused)]
#[repr(u8)]
#[non_exhaustive]
enum DeviceHandleType {
    Acpi = 0,
    Pci = 1,
}

// TODO: remove this attribute after we can parse generic initiator affinity entry.
#[allow(unused)]
#[repr(C)]
enum DeviceHandle {
    Acpi(AcpiDeviceHandle),
    Pci(PciDeviceHandle),
}

#[repr(C, packed)]
struct AcpiDeviceHandle {
    acpi_hid: u64,
    acpi_uid: u32,
    _reserved: u32,
}

#[repr(C, packed)]
struct PciDeviceHandle {
    pci_segment: u16,
    pci_bdf_number: u16,
    _reserved: [u32; 3],
}

bitflags! {
    #[derive(Clone, Copy)]
    struct GenericInitiatorAffinityFlags: u32 {
        const ENABLED = 1;
        const ARCHITECTURAL_TRANSACTIONS = 1 << 1;
    }
}
