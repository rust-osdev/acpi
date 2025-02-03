use crate::{
    address::{AddressSpace, GenericAddress, RawGenericAddress},
    sdt::{SdtHeader, Signature},
    AcpiError,
    AcpiHandler,
    AcpiResult,
    AcpiTable,
    PhysicalMapping,
};
use bitflags::bitflags;
use core::{marker::PhantomData, ptr::NonNull};

/// Generic subspace structures (type 0).
pub mod generic;
use generic::*;

/// HW-reduced subspace structures (types 1 and 2).
pub mod reduced;
use reduced::*;

/// Extended PCC subspace structures (types 3 and 4).
pub mod extended;
use extended::*;

/// HW-registers-based subspace structures (type 5).
pub mod register_based;
use register_based::*;

bitflags! {
    /// Global flags for the [PCCT](Pcct).
    ///
    /// See section "14.1.1. Platform Communications Channel Global
    /// Flags" of the ACPI spec for more information.
    #[derive(Clone, Copy, Debug)]
    pub struct PcctFlags: u32 {
        /// If set, the platform is capable of generating an
        /// interrupt to indicate completion of a command.
        const PLATFORM_INTERRUPT = 0b1;
    }
}

/// Platform Communications Channel Table (PCCT).
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct Pcct {
    header: SdtHeader,
    /// Global flags for the PCCT.
    pub flags: PcctFlags,
    _rsvd: [u8; 8],
}

unsafe impl AcpiTable for Pcct {
    const SIGNATURE: Signature = Signature::PCCT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Pcct {
    /// Returns an iterator over the PCC subspaces.
    pub const fn entries(&self) -> PcctEntryIter {
        // SAFETY: this is the layout specified by the ACPI spec.
        // (14.1. Platform Communications Channel Table)
        let ptr = unsafe { core::ptr::from_ref(self).add(1).cast() };
        let len = (self.header.length as usize).saturating_sub(size_of::<Self>());
        PcctEntryIter { ptr, num: 0, len, _phantom: PhantomData }
    }

    /// Gets the appropriate subspace for the given address and its
    /// position in the table. Returns an error if the address does
    /// not belong to the PCC address space.
    pub fn get_subspace(&self, addr: GenericAddress) -> AcpiResult<(u8, PcctEntry)> {
        if addr.address_space != AddressSpace::PlatformCommunicationsChannel {
            return Err(AcpiError::InvalidGenericAddress);
        }
        let idx = addr.access_size;
        let entry = self.entries().nth(idx as usize).ok_or(AcpiError::InvalidGenericAddress)?;
        Ok((idx, entry))
    }
}

/// An iterator over the PCC subspaces in the [PCCT](Pcct).
#[derive(Clone, Debug)]
pub struct PcctEntryIter<'a> {
    ptr: *const u8,
    len: usize,
    num: usize,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> Iterator for PcctEntryIter<'a> {
    type Item = PcctEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // Stop after we exceed the total length or we iterate over
        // the maximum number of entries.
        if self.len == 0 || self.num >= 256 {
            return None;
        }

        // SAFETY: we keep track of each entry we parse, so the
        // pointer must be valid.
        let entry = unsafe { Self::Item::from_ptr(self.ptr)? };
        let entry_len = entry.header().len as usize;

        self.num += 1;
        self.ptr = self.ptr.wrapping_add(entry_len);
        self.len = self.len.saturating_sub(entry_len);
        Some(entry)
    }
}

/// Common header for all PCC subspaces.
///
/// See section "14.1.2. Platform Communications Channel Subspace
/// Structures" of the ACPI spec for more information.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(C, packed)]
pub struct PccSubspaceHeader {
    pub stype: u8,
    pub len: u8,
}

/// Information about the command complete check register for a PCC
/// subspace.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccCmdCompleteCheck {
    /// Location of the register.
    pub addr: GenericAddress,
    /// Mask to determine whether a command is complete, using the
    /// command complete check register. A command is complete if the
    /// value of the register when combined through a logical AND with
    /// this mask, yields a non-zero value
    pub mask: u64,
}

/// Information about the Doorbell register a PCC subspace.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccDoorbell {
    /// The address of the doorbell
    pub addr: GenericAddress,
    /// Mask of bits to preserve when writing the doorbell register.
    pub preserve: u64,
    /// Mask of bits to set when writing the doorbell register.
    pub write: u64,
}

/// A generic shared memory region for a PCC subspace.
///
/// The region is formed by a header (`T`) and a variable-length area
/// [`PccShmem::shmem`].
#[derive(Debug)]
pub struct PccShmem<H: AcpiHandler, T: PccShmemHdr> {
    /// Mapping for the shared memory region header.
    pub header: PhysicalMapping<H, T>,
    /// Communication space. The pointer is only valid while the
    /// header mapping is alive.
    pub shmem: NonNull<[u8]>,
}

impl<H: AcpiHandler, T: PccShmemHdr> PccShmem<H, T> {
    /// Verify that the signature for the shared memory region matches.
    pub fn verify_signature(&self, subspace_num: u8) -> AcpiResult<()> {
        if self.header.signature() != 0x50434300 | (subspace_num as u32) {
            return Err(AcpiError::PccInvalidShmemSignature(self.header.signature()));
        }
        Ok(())
    }
}

/// Trait implemented by all PCC subspaces. It allows access to the
/// subspace's doorbell and shared memory region.
pub trait PccSubspace {
    /// The header for this PCC subspace's shared memory region.
    type ShmemHdr: PccShmemHdr;

    /// Map the shared memory region for this subspace.
    ///
    /// Each PCC subspace has an address and length that points to a
    /// shared memory region for communications ([`PccShmem`]). This
    /// region is formed by a header (whose format is determined by
    /// the type of the subspace - [`Self::ShmemHdr`]) and a
    /// variable-length region (whose format is determined by the
    /// subsystem using this subspace).
    fn map_shmem<H: AcpiHandler>(&self, h: &H) -> Option<PccShmem<H, Self::ShmemHdr>>;

    /// Get information about the doorbell for this subspace, if set.
    fn doorbell(&self) -> Option<AcpiResult<PccDoorbell>>;
}

/// Trait implemented by all shared memory region headers.
pub trait PccShmemHdr {
    /// The signature for this header.
    fn signature(&self) -> u32;
}

/// Prepare an enum with all possible PCC subspace entries, plus some
/// catch-all implementations.
macro_rules! pcct_subentries {
    ([
        $(
            ($ty:ty, [$($id:expr),+], $variant:ident, $shhdr:ty)
        ),+ $(,)?
    ]) => {
        /// An entry in the PCCT, corresponding to a subspace of a
        /// specific type.
        #[derive(Clone, Copy, Debug)]
        pub enum PcctEntry<'a> {
            $(
                $variant(&'a $ty),
            )+
        }

        impl PcctEntry<'_> {
            /// The PCC subspace header for this entry.
            pub const fn header(&self) -> &PccSubspaceHeader {
                match self {
                    $(
                        Self::$variant(v) => &v.header,
                    )+
                }
            }

            /// Information about the doorbell for this subspace, if
            /// set.
            pub fn doorbell(&self) -> Option<AcpiResult<PccDoorbell>> {
                match self {
                    $(
                        Self::$variant(v) => v.doorbell(),
                    )+
                }
            }

            /// Read a PCC subspace entry based on its type. Returns
            /// [`None`] if an unknown entry type is found.
            ///
            /// # Safety
            ///
            /// The caller must provide a pointer to the beginning
            /// of a PCC subspace structure.
            const unsafe fn from_ptr(ptr: *const u8) -> Option<Self> {
                // SAFETY: the caller must ensure we are given a
                // valid pointer. The header is packed, so it has no
                // alignment requirements.
                let header = unsafe { &*ptr.cast::<PccSubspaceHeader>() };
                match header.stype {
                    $(
                        $(
                            // SAFETY: we checked the entry type above.
                            $id => Some(Self::$variant(unsafe { &*ptr.cast() })),
                        )+
                    )+
                    _ => None,
                }
            }
        }

        $(
            impl PccSubspace for $ty {
                type ShmemHdr = $shhdr;

                fn doorbell(&self) -> Option<AcpiResult<PccDoorbell>> {
                    if self.doorbell_reg.is_empty() {
                        return None;
                    }
                    Some(GenericAddress::from_raw(self.doorbell_reg).map(|addr| PccDoorbell {
                        addr,
                        write: self.doorbell_write,
                        preserve: self.doorbell_preserve,
                    }))
                }

                fn map_shmem<H: AcpiHandler>(
                    &self,
                    h: &H,
                ) -> Option<PccShmem<H, Self::ShmemHdr>> {
                    if self.mem_len == 0 {
                        return None;
                    }

                    // SAFETY: we trust the base address and length
                    // provided by the PCCT.
                    let header = unsafe {
                        h.map_physical_region::<$shhdr>(
                            self.base_address as usize,
                            self.mem_len as usize,
                        )
                    };

                    // SAFETY: this is the layout specified in the
                    // ACPI spec. The area right after the header is
                    // the communication space.
                    // The length takes into account the size of the
                    // header itself.
                    let shmem_raw = unsafe {
                        header.virtual_start().add(1).cast::<u8>()
                    };
                    let shmem_len = self.mem_len as usize - size_of::<$shhdr>();
                    let shmem = NonNull::slice_from_raw_parts(shmem_raw, shmem_len);
                    Some(PccShmem { header, shmem })
                }
            }
        )*
    };
}

pcct_subentries!([
    (PccGenericSubspace, [0], Generic, PccGenericShmem),
    (PccHwReducedSubspace1, [1], HwReduced1, PccGenericShmem),
    (PccHwReducedSubspace2, [2], HwReduced2, PccGenericShmem),
    (PccExtendedSubspace, [3, 4], Extended, PccExtendedShmem),
    (PccHwRegisterBasedSubspace, [5], HwRegisterBased, PccReducedShmem),
]);

#[cfg(test)]
mod test {
    use super::*;
    use core::mem::offset_of;
}
