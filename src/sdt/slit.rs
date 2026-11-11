use crate::{
    AcpiTable,
    sdt::{SdtHeader, Signature},
};
use core::{marker::PhantomPinned, mem, pin::Pin, slice};
use log::warn;

/// The SLIT (System Locality Information Table) provides a matrix of relative distances between
/// all proximity domains. It encodes a list of N*N entries, where each entry is `u8` and the
/// relative distance between proximity domains `(i, j)` is the entry at `i*N+j`. The distance
/// between a proximity domain and itself is normalised to a value of `10`.
#[derive(Debug)]
#[repr(C, packed)]
pub struct Slit {
    pub header: SdtHeader,
    pub num_proximity_domains: u64,
    _pinned: PhantomPinned,
}

unsafe impl AcpiTable for Slit {
    const SIGNATURE: Signature = Signature::SLIT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Slit {
    pub fn matrix(self: Pin<&Self>) -> DistanceMatrix<'_> {
        DistanceMatrix { num_proximity_domains: self.num_proximity_domains, matrix: self.matrix_raw() }
    }

    pub fn matrix_raw(self: Pin<&Self>) -> &[u8] {
        let mut num_entries = self.num_proximity_domains * self.num_proximity_domains;
        if (mem::size_of::<Slit>() + num_entries as usize * mem::size_of::<u8>()) > self.header.length as usize {
            warn!("SLIT too short for given number of proximity domains! Returning empty matrix");
            num_entries = 0;
        }

        unsafe {
            let ptr = Pin::into_inner_unchecked(self) as *const Slit as *const u8;
            slice::from_raw_parts(ptr.byte_add(mem::size_of::<Slit>()), num_entries as usize)
        }
    }
}

pub struct DistanceMatrix<'a> {
    pub num_proximity_domains: u64,
    pub matrix: &'a [u8],
}

impl DistanceMatrix<'_> {
    pub fn distance(&self, i: u32, j: u32) -> Option<u8> {
        if (i as u64) < self.num_proximity_domains && (j as u64) < self.num_proximity_domains {
            Some(self.matrix[i as usize * self.num_proximity_domains as usize + j as usize])
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A two-locality SLIT whose distances differ by direction, so a transposed lookup shows.
    fn table() -> [u8; 44 + 4] {
        let mut bytes = [0u8; 44 + 4];
        bytes[0..4].copy_from_slice(b"SLIT");
        bytes[4..8].copy_from_slice(&(44u32 + 4).to_le_bytes());
        bytes[36..44].copy_from_slice(&2u64.to_le_bytes());
        bytes[44..48].copy_from_slice(&[10, 20, 21, 10]);
        bytes
    }

    #[test]
    fn matrix_covers_every_locality_pair() {
        let bytes = table();
        let slit = unsafe { Pin::new_unchecked(&*(bytes.as_ptr() as *const Slit)) };
        assert_eq!(slit.matrix_raw(), &[10, 20, 21, 10]);
    }

    #[test]
    fn distance_is_indexed_from_then_to() {
        let bytes = table();
        let slit = unsafe { Pin::new_unchecked(&*(bytes.as_ptr() as *const Slit)) };
        let matrix = slit.matrix();
        assert_eq!(matrix.distance(0, 0), Some(10));
        assert_eq!(matrix.distance(0, 1), Some(20));
        assert_eq!(matrix.distance(1, 0), Some(21));
        assert_eq!(matrix.distance(2, 0), None);
        assert_eq!(matrix.distance(0, 2), None);
    }
}
