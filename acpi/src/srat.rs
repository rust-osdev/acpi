use core::marker::PhantomData;

use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct Srat {
    header: SdtHeader,
    _reserved: u32,
    _reserved2: u64,
    // Followed by `n` structs with Static Resource Allocation Structure
    // A list of static resource allocation structures for the platform.
    // See Processor Local APIC/SAPIC Affinity Structure, Memory Affinity Structure,
    // Processor Local x2APIC Affinity Structure, and GICC Affinity Structure.
}

/// ### Safety: Implementation properly represents a valid SRAT.
unsafe impl AcpiTable for Srat {
    const SIGNATURE: Signature = Signature::SRAT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

use core::fmt;
impl fmt::Display for Srat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SRAT: {:?}", self.header)?;
        for entry in self.entries() {
            write!(f, "\n{:#x?}", entry)?
        }
        Ok(())
    }
}

impl Srat {
    /// Returns an iterator over the memory ranges specified in the System Resource Affinity Table (SRAT).
    ///
    /// The iterator yields tuples of `(base_address, length)`, where `base_address` is the starting address of a memory range
    /// and `length` is the size of the memory range.
    ///
    /// However, the mem ranges here are not actually the range of System RAM,
    /// and the final available memory segment needs to be obtained through the Memory Map of UEFI Boot Services.
    pub fn memory_ranges(&self) -> impl Iterator<Item = (u64, u64)> + '_ {
        self.entries().filter_map(|entry| match entry {
            AffinityStruct::MemoryAffinity(memory_affinity) => Some((
                memory_affinity.base_address_lo as u64 | ((memory_affinity.base_address_hi as u64) << 32),
                memory_affinity.length_lo as u64 | ((memory_affinity.length_hi as u64) << 32),
            )),
            _ => None,
        })
    }

    /// Returns an iterator over the affinity structures in the System Resource Affinity Table (SRAT).
    /// including Local APIC/SAPIC Affinity Structure, Memory Affinity Structure, Processor Local x2APIC Affinity Structure,
    /// GICC Affinity Structure, GIC ITS Affinity Structure, and Generic Initiator Affinity Structure.
    pub fn entries(&self) -> AffinityStructIter {
        let pointer = unsafe { (self as *const Srat).add(1) as *const u8 };
        let remaining_length = self.header.length as u32 - core::mem::size_of::<Srat>() as u32;

        AffinityStructIter { pointer, remaining_length, _phantom: PhantomData }
    }
}

#[derive(Debug)]
pub struct AffinityStructIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>,
}

#[derive(Debug)]
pub enum AffinityStruct<'a> {
    LocalApicAffinity(&'a LocalApicAffinity),
    MemoryAffinity(&'a MemoryAffinity),
    LocalX2ApicAffinity(&'a LocalX2ApicAffinity),
    GiccAffinity(&'a GiccAffinity),
    GicItsAffinity(&'a GicItsAffinity),
    GenericInitiatorAffinity(&'a GenericInitiatorAffinity),
}

impl<'a> Iterator for AffinityStructIter<'a> {
    type Item = AffinityStruct<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length <= 0 {
            return None;
        }

        let struct_header = unsafe { &*(self.pointer as *const StructHeader) };
        let struct_length = struct_header.length as u32;
        let struct_type = struct_header.struct_type;

        let affinity_struct = match struct_type {
            0 => AffinityStruct::LocalApicAffinity(unsafe { &*(self.pointer as *const LocalApicAffinity) }),
            1 => AffinityStruct::MemoryAffinity(unsafe { &*(self.pointer as *const MemoryAffinity) }),
            2 => AffinityStruct::LocalX2ApicAffinity(unsafe { &*(self.pointer as *const LocalX2ApicAffinity) }),
            3 => AffinityStruct::GiccAffinity(unsafe { &*(self.pointer as *const GiccAffinity) }),
            4 => AffinityStruct::GicItsAffinity(unsafe { &*(self.pointer as *const GicItsAffinity) }),
            5 => AffinityStruct::GenericInitiatorAffinity(unsafe {
                &*(self.pointer as *const GenericInitiatorAffinity)
            }),
            _ => return None,
        };

        self.pointer = unsafe { self.pointer.add(struct_length as usize) };
        self.remaining_length -= struct_length;

        Some(affinity_struct)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct StructHeader {
    /// Type of the structure
    /// 0: Processor Local APIC/SAPIC Affinity Structure
    /// 1: Memory Affinity Structure
    /// 2: Processor Local x2APIC Affinity Structure
    /// 3: GICC Affinity Structure
    /// 4: GIC ITS Affinity Structure
    /// 5: Generic Initiator Affinity Structure
    pub struct_type: u8,
    pub length: u8,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct MemoryAffinity {
    pub struct_header: StructHeader,
    pub proximity_domain: u32,
    pub _reserved: u16,
    pub base_address_lo: u32,
    pub base_address_hi: u32,
    pub length_lo: u32,
    pub length_hi: u32,
    pub _reserved2: u32,
    /// bit_offset: field
    ///          0: Enabled
    ///          1: Hot-pluggable
    ///          2: Non-volatile
    pub flags: u32,
    pub _reserved3: [u8; 8],
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct GiccAffinity {
    pub struct_header: StructHeader,
    pub proximity_domain: u32,
    pub acpi_processor_uid: u32,
    /// bit_offset: field
    ///          0: Enabled
    pub flags: u32,
    pub clock_domain: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct LocalApicAffinity {
    pub struct_header: StructHeader,
    pub proximity_domain_lo: u8,
    pub apic_id: u8,
    /// bit_offset: field
    ///         0: Enabled
    pub flags: u32,
    pub local_sapic_eid: u8,
    pub proximity_domain_hi: [u8; 3],
    pub clock_domain: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct LocalX2ApicAffinity {
    pub struct_header: StructHeader,
    pub _reserved: u16,
    pub proximity_domain: u32,
    pub x2apic_id: u32,
    /// bit_offset: field
    ///         0: Enabled
    pub flags: u32,
    pub clock_domain: u32,
    pub _reserved2: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct GicItsAffinity {
    pub struct_header: StructHeader,
    pub proximity_domain: u32,
    pub _reserved: u16,
    pub its_id: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct GenericInitiatorAffinity {
    pub struct_header: StructHeader,
    pub _reserved: u8,
    /// 0: ACPI Device Handle
    /// 1: PCI Device Handle
    pub device_handle_type: u8,
    pub proximity_domain: u32,
    pub device_handle: [u8; 16],
    pub flags: u32,
    pub _reserved2: u32,
}
