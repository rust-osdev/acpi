use crate::{
    AcpiTables,
    Handler,
    sdt::{
        slit::{DistanceMatrix, Slit},
        srat::{LocalApicAffinityFlags, MemoryAffinityFlags, Srat, SratEntry},
    },
};
use alloc::{alloc::Global, vec::Vec};
use core::alloc::Allocator;

/// Information about the setup of NUMA (Non-Uniform Memory Architecture) resources within the
/// sytem.
#[derive(Clone, Debug)]
pub struct NumaInfo<A: Allocator = Global> {
    pub processor_affinity: Vec<ProcessorAffinity, A>,
    pub memory_affinity: Vec<MemoryAffinity, A>,
    pub num_proximity_domains: usize,
    pub distance_matrix: Vec<u8, A>,
}

impl NumaInfo<Global> {
    pub fn new(tables: &AcpiTables<impl Handler>) -> NumaInfo<Global> {
        Self::new_in(tables, Global)
    }
}

impl<A: Allocator + Clone> NumaInfo<A> {
    pub fn new_in(tables: &AcpiTables<impl Handler>, allocator: A) -> NumaInfo<A> {
        let mut processor_affinity = Vec::new_in(allocator.clone());
        let mut memory_affinity = Vec::new_in(allocator.clone());

        if let Some(srat) = tables.find_table::<Srat>() {
            for entry in srat.get().entries() {
                match entry {
                    SratEntry::LocalApicAffinity(entry) => processor_affinity.push(ProcessorAffinity {
                        hw_id: entry.apic_id as u64,
                        proximity_domain: entry.proximity_domain(),
                        is_enabled: { entry.flags }.contains(LocalApicAffinityFlags::ENABLED),
                    }),
                    SratEntry::LocalApicX2Affinity(entry) => processor_affinity.push(ProcessorAffinity {
                        hw_id: entry.x2apic_id as u64,
                        proximity_domain: entry.proximity_domain,
                        is_enabled: { entry.flags }.contains(LocalApicAffinityFlags::ENABLED),
                    }),
                    SratEntry::GiccAffinity(entry) => processor_affinity.push(ProcessorAffinity {
                        // SRAT spec: GICC affinity identifies the CPU by
                        // its ACPI processor UID (matches
                        // `MadtEntry::Gicc::processor_uid`), *not* MPIDR.
                        // Cross-arch consumers join against the SRAT-side
                        // identifier appropriate for their arch — see
                        // [`ProcessorAffinity::hw_id`] docs.
                        hw_id: entry.acpi_processor_uid as u64,
                        proximity_domain: entry.proximity_domain,
                        // GICC affinity flags bit 0 = ENABLED, identical
                        // layout to the APIC affinity flags.
                        is_enabled: entry.flags & 1 != 0,
                    }),
                    SratEntry::MemoryAffinity(entry) => memory_affinity.push(MemoryAffinity {
                        base_address: entry.base_address(),
                        length: entry.length(),
                        proximity_domain: entry.proximity_domain,
                        is_enabled: { entry.flags }.contains(MemoryAffinityFlags::ENABLED),
                        is_hot_pluggable: { entry.flags }.contains(MemoryAffinityFlags::HOT_PLUGGABLE),
                        is_non_volatile: { entry.flags }.contains(MemoryAffinityFlags::NON_VOLATILE),
                    }),
                    _ => (),
                }
            }
        }

        let (num_proximity_domains, distance_matrix) = if let Some(slit) = tables.find_table::<Slit>() {
            (slit.get().num_proximity_domains as usize, slit.get().matrix_raw().to_vec_in(allocator.clone()))
        } else {
            (0, Vec::new_in(allocator.clone()))
        };

        NumaInfo { processor_affinity, memory_affinity, num_proximity_domains, distance_matrix }
    }

    pub fn distance_matrix(&self) -> DistanceMatrix<'_> {
        DistanceMatrix { num_proximity_domains: self.num_proximity_domains as u64, matrix: &self.distance_matrix }
    }
}

#[derive(Clone, Debug)]
pub struct ProcessorAffinity {
    /// The SRAT-side processor identifier — i.e., the value the
    /// firmware put into the SRAT entry to identify the CPU.
    /// Per-arch semantics (these mirror the SRAT subtable kinds the
    /// spec defines):
    ///
    /// - **x86 / x86_64** (Local APIC / X2APIC affinity, SRAT types
    ///   0 and 2): the APIC ID.  This matches the MADT
    ///   [`Processor::hw_id`](super::Processor::hw_id) directly, so
    ///   the SRAT→MADT join is by equal `hw_id`.
    /// - **aarch64** (GICC affinity, SRAT type 3): the ACPI
    ///   processor UID — *not* MPIDR.  This matches the MADT
    ///   [`Processor::processor_uid`](super::Processor::processor_uid),
    ///   not its `hw_id`.  Consumers joining cross-table on
    ///   aarch64 must use `processor_uid`.
    /// - **riscv64** (RINTC affinity, SRAT type 7, future): the
    ///   hartid.
    ///
    /// Widened to `u64` to cover MPIDR / hartid cleanly even though
    /// every SRAT subtable currently defined uses 32 bits or fewer.
    pub hw_id: u64,
    pub proximity_domain: u32,
    pub is_enabled: bool,
}

#[derive(Clone, Debug)]
pub struct MemoryAffinity {
    pub base_address: u64,
    pub length: u64,
    pub proximity_domain: u32,
    pub is_enabled: bool,
    pub is_hot_pluggable: bool,
    pub is_non_volatile: bool,
}
