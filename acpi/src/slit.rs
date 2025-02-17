use core::{alloc::Allocator, marker::PhantomPinned, pin::Pin};

use crate::{platform::SystemLocalityInfo, sdt::{SdtHeader, Signature}, AcpiResult, AcpiTable};

/// System Locality Information Table (SLIT)
/// 
/// This optional table provides a matrix that describes the relative distance 
/// (memory latency) between all System Localities, which are also referred to 
/// as Proximity Domains. The value of each Entry[i,j] in the SLIT table, where 
/// i represents a row of a matrix and j represents a column of a matrix, 
/// indicates the relative distances from System Locality / Proximity Domain i 
/// to every other System Locality j in the system (including itself).
#[repr(C, packed)]
pub struct Slit {
    header: SdtHeader,
    nr_system_localities: u64,
    _pinned: PhantomPinned,
}

unsafe impl AcpiTable for Slit {
    const SIGNATURE: Signature = Signature::SLIT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Slit {
    #[cfg(feature = "allocator_api")]
    pub fn parse_system_locality_in<'a, A>(
        self: Pin<&Self>,
        allocator: A,
    ) -> AcpiResult<SystemLocalityInfo<'a, A>>
    where 
        A: Allocator + Clone,
    {
        use crate::ManagedSlice;

        let (mut row, mut column) = (0, 0);
        let mut distance_matrix = ManagedSlice::new_in(self.nr_system_localities as usize, allocator.clone())?;
        for entry in self.entries() {
            if column == 0 {
                distance_matrix[row] = ManagedSlice::new_in(self.nr_system_localities as usize, allocator.clone())?;
            }
            distance_matrix[row][column] = entry;
            column += 1;
            if column == self.nr_system_localities as usize {
                row += 1;
                column = 0;
            }
        }

        Ok(SystemLocalityInfo::new(self.nr_system_localities, distance_matrix))
    }

    fn entries(self: Pin<&Self>) -> SlitEntryIter {
        let ptr = unsafe { Pin::into_inner_unchecked(self) as *const Slit as *const u8 };
        SlitEntryIter {
            pointer: unsafe { ptr.add(size_of::<Slit>()) },
            remaining_length: self.header.length - size_of::<Slit>() as u32,
        }
    }   
}

struct SlitEntryIter {
    pointer: *const u8,
    remaining_length: u32,
}

impl Iterator for SlitEntryIter {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length > 0 {
            let entry_pointer = self.pointer;
            self.pointer = unsafe { self.pointer.add(size_of::<Self::Item>()) };
            self.remaining_length -= size_of::<Self::Item>() as u32;
            Some(unsafe { *entry_pointer })
        } else {
            None
        }
    }
}

