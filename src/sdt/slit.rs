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
    pub fn matrix(self: Pin<&Self>) -> &[u8] {
        let mut num_entries = self.num_proximity_domains * self.num_proximity_domains;
        if (mem::size_of::<Slit>() + num_entries as usize * num_entries as usize * mem::size_of::<u8>())
            > self.header.length as usize
        {
            warn!("SLIT too short for given number of proximity domains! Returning empty matrix");
            num_entries = 0;
        }

        unsafe {
            let ptr = Pin::into_inner_unchecked(self) as *const Slit as *const u8;
            slice::from_raw_parts(ptr.byte_add(mem::size_of::<Slit>()), num_entries as usize)
        }
    }

    pub fn entry(self: Pin<&Self>, i: u32, j: u32) -> Option<u8> {
        if i as u64 <= self.num_proximity_domains && j as u64 <= self.num_proximity_domains {
            let matrix = self.matrix();
            Some(matrix[i as usize + j as usize * self.num_proximity_domains as usize])
        } else {
            None
        }
    }
}
