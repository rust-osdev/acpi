use crate::{
    AcpiTable,
    sdt::{SdtHeader, Signature},
};
use bit_field::BitField;
use core::{
    marker::{PhantomData, PhantomPinned},
    mem,
    pin::Pin,
};
use log::warn;

/// Represents the SRAT (System Resource Affinity Table). This is a variable length table that
/// allows devices (processors, memory ranges, and generic 'initiators') to be associated with
/// system locality / proximity domains and clock domains.
#[derive(Debug)]
#[repr(C, packed)]
pub struct Srat {
    pub header: SdtHeader,
    _reserved0: u32,
    _reserved1: u64,
    _pinned: PhantomPinned,
}

unsafe impl AcpiTable for Srat {
    const SIGNATURE: Signature = Signature::SRAT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Srat {
    pub fn entries(self: Pin<&Self>) -> SratEntryIter<'_> {
        let ptr = unsafe { Pin::into_inner_unchecked(self) as *const Srat as *const u8 };
        SratEntryIter {
            pointer: unsafe { ptr.add(mem::size_of::<Srat>()) },
            remaining_length: self.header.length - mem::size_of::<Srat>() as u32,
            _phantom: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct SratEntryIter<'a> {
    pointer: *const u8,
    /*
     * The iterator can only have at most `u32::MAX` remaining bytes, because the length of the
     * whole SDT can only be at most `u32::MAX`.
     */
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>,
}

#[derive(Debug)]
pub enum SratEntry<'a> {
    LocalApicAffinity(&'a LocalApicAffinity),
    MemoryAffinity(&'a MemoryAffinity),
    LocalApicX2Affinity(&'a LocalApicX2Affinity),
    GiccAffinity(&'a GiccAffinity),
    GicItsAffinity(&'a GicItsAffinity),
    GicInitiatorAffinity(&'a GicInitiatorAffinity),
}

impl<'a> Iterator for SratEntryIter<'a> {
    type Item = SratEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.remaining_length > 0 {
            let entry_pointer = self.pointer;
            let header = unsafe { *(self.pointer as *const EntryHeader) };

            if header.length as u32 > self.remaining_length {
                warn!("Invalid entry of type {} in SRAT - extending past length of table. Ignoring", header.typ);
                return None;
            }

            self.pointer = unsafe { self.pointer.byte_offset(header.length as isize) };
            self.remaining_length = self.remaining_length.saturating_sub(header.length as u32);

            match header.typ {
                0 => {
                    return Some(SratEntry::LocalApicAffinity(unsafe {
                        &*(entry_pointer as *const LocalApicAffinity)
                    }));
                }
                1 => {
                    return Some(SratEntry::MemoryAffinity(unsafe { &*(entry_pointer as *const MemoryAffinity) }));
                }
                2 => {
                    return Some(SratEntry::LocalApicX2Affinity(unsafe {
                        &*(entry_pointer as *const LocalApicX2Affinity)
                    }));
                }
                3 => return Some(SratEntry::GiccAffinity(unsafe { &*(entry_pointer as *const GiccAffinity) })),
                4 => {
                    return Some(SratEntry::GicItsAffinity(unsafe { &*(entry_pointer as *const GicItsAffinity) }));
                }
                5 => {
                    return Some(SratEntry::GicInitiatorAffinity(unsafe {
                        &*(entry_pointer as *const GicInitiatorAffinity)
                    }));
                }
                other => warn!("Unrecognised entry in SRAT of type {}", other),
            }
        }

        None
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct EntryHeader {
    pub typ: u8,
    pub length: u16,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalApicAffinity {
    pub header: EntryHeader,
    pub proximity_domain_low: u8,
    pub apic_id: u8,
    pub flags: LocalApicAffinityFlags,
    pub local_sapic_eid: u8,
    pub proximity_domain_high: [u8; 3],
    pub clock_domain: u32,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct LocalApicAffinityFlags: u32 {
        const ENABLED = 1;
    }
}

impl LocalApicAffinity {
    pub fn proximity_domain(&self) -> u32 {
        u32::from_le_bytes([
            self.proximity_domain_low,
            self.proximity_domain_high[0],
            self.proximity_domain_high[1],
            self.proximity_domain_high[2],
        ])
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct MemoryAffinity {
    pub header: EntryHeader,
    pub proximity_domain: u32,
    _reserved0: u16,
    pub base_address_low: u32,
    pub base_address_high: u32,
    pub length_low: u32,
    pub length_high: u32,
    _reserved1: u32,
    pub flags: MemoryAffinityFlags,
    _reserved2: u64,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct MemoryAffinityFlags: u32 {
        const ENABLED = 1;
        const HOT_PLUGGABLE = 1 << 1;
        const NON_VOLATILE = 1 << 2;
    }
}

impl MemoryAffinity {
    pub fn base_address(&self) -> u64 {
        let mut address = self.base_address_low as u64;
        address.set_bits(32..64, self.base_address_high as u64);
        address
    }

    pub fn length(&self) -> u64 {
        let mut length = self.length_low as u64;
        length.set_bits(32..64, self.base_address_high as u64);
        length
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct LocalApicX2Affinity {
    pub header: EntryHeader,
    _reserved0: u16,
    pub proximity_domain: u32,
    pub x2apic_id: u32,
    pub flags: LocalApicAffinityFlags,
    pub clock_domain: u32,
    _reserved1: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GiccAffinity {
    pub header: EntryHeader,
    pub proximity_domain: u32,
    pub acpi_processor_uid: u32,
    pub flags: u32,
    pub clock_domain: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GicItsAffinity {
    pub header: EntryHeader,
    pub proximity_domain: u32,
    _reserved0: u16,
    pub its_id: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct GicInitiatorAffinity {
    pub header: EntryHeader,
    _reserved0: u8,
    pub device_handle_type: u8,
    pub proximity_domain: u32,
    pub device_handle: [u8; 16],
    pub flags: u32,
    _reserved1: u32,
}
