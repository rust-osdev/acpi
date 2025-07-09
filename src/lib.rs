//! `acpi` is a Rust library for interacting with the Advanced Configuration and Power Interface, a
//! complex framework for power management and device discovery and configuration. ACPI is used on
//! modern x64, ARM, RISC-V, and other platforms. An operating system needs to interact with ACPI
//! to correctly set up a platform's interrupt controllers, perform power management, and fully
//! support many other platform capabilities.
//!
//! This crate provides a limited API that can be used without an allocator, for example for use
//! from a bootloader. This API will allow you to search for the RSDP, enumerate over the available
//! tables, and interact with the tables using their raw structures. All other functionality is
//! behind an `alloc` feature (enabled by default) and requires an allocator.
//!
//! With an allocator, this crate also provides higher-level interfaces to the static tables, as
//! well as a dynamic interpreter for AML - the bytecode format encoded in the DSDT and SSDT
//! tables.
//!
//! ### Usage
//! To use the library, you will need to provide an implementation of the [`AcpiHandler`] trait,
//! which allows the library to make requests such as mapping a particular region of physical
//! memory into the virtual address space.
//!
//! Next, you'll need to get the physical address of either the RSDP, or the RSDT/XSDT. The method
//! for doing this depends on the platform you're running on and how you were booted. If you know
//! the system was booted via the BIOS, you can use [`Rsdp::search_for_rsdp_bios`]. UEFI provides a
//! separate mechanism for getting the address of the RSDP.
//!
//! You then need to construct an instance of [`AcpiTables`], which can be done in a few ways
//! depending on how much information you have:
//! * Use [`AcpiTables::from_rsdp`] if you have the physical address of the RSDP
//! * Use [`AcpiTables::from_rsdt`] if you have the physical address of the RSDT/XSDT
//!
//! Once you have an `AcpiTables`, you can search for relevant tables, or use the higher-level
//! interfaces, such as [`PlatformInfo`], [`PciConfigRegions`], or [`HpetInfo`].

#![no_std]
#![feature(allocator_api, let_chains)]

#[cfg_attr(test, macro_use)]
#[cfg(test)]
extern crate std;

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod address;
#[cfg(feature = "aml")]
pub mod aml;
#[cfg(feature = "alloc")]
pub mod platform;
pub mod rsdp;
pub mod sdt;

pub use sdt::{fadt::PowerProfile, hpet::HpetInfo, madt::MadtError};

use crate::sdt::{SdtHeader, Signature};
use core::{
    fmt,
    mem,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};
use log::warn;
use rsdp::Rsdp;

/// `AcpiTables` should be constructed after finding the RSDP or RSDT/XSDT and allows enumeration
/// of the system's ACPI tables.
pub struct AcpiTables<H: RegionMapper> {
    rsdt_mapping: PhysicalMapping<H, SdtHeader>,
    pub rsdp_revision: u8,
    handler: H,
}

unsafe impl<H> Send for AcpiTables<H> where H: RegionMapper + Send {}
unsafe impl<H> Sync for AcpiTables<H> where H: RegionMapper + Send {}

impl<H> AcpiTables<H>
where
    H: RegionMapper,
{
    /// Construct an `AcpiTables` from the **physical** address of the RSDP.
    ///
    /// # Safety
    /// The address of the RSDP must be valid.
    pub unsafe fn from_rsdp(handler: H, rsdp_address: usize) -> Result<AcpiTables<H>, AcpiError> {
        let rsdp_mapping = unsafe { handler.map_physical_region::<Rsdp>(rsdp_address, mem::size_of::<Rsdp>()) };

        /*
         * If the address given does not have a correct RSDP signature, the user has probably given
         * us an invalid address, and we should not continue. We're more lenient with other errors
         * as it's probably a real RSDP and the firmware developers are just lazy.
         */
        match rsdp_mapping.validate() {
            Ok(()) => (),
            Err(AcpiError::RsdpIncorrectSignature) => return Err(AcpiError::RsdpIncorrectSignature),
            Err(AcpiError::RsdpInvalidOemId) | Err(AcpiError::RsdpInvalidChecksum) => {
                warn!("RSDP has invalid checksum or OEM ID. Continuing.");
            }
            Err(_) => (),
        }

        let rsdp_revision = rsdp_mapping.revision();
        let rsdt_address = if rsdp_revision == 0 {
            // We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
            rsdp_mapping.rsdt_address() as usize
        } else {
            /*
             * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
             * to 32 bits on x86.
             */
            rsdp_mapping.xsdt_address() as usize
        };

        unsafe { Self::from_rsdt(handler, rsdp_revision, rsdt_address) }
    }

    /// Construct an `AcpiTables` from the **physical** address of the RSDT/XSDT, and the revision
    /// found in the RSDP.
    ///
    /// # Safety
    /// The address of the RSDT must be valid.
    pub unsafe fn from_rsdt(
        handler: H,
        rsdp_revision: u8,
        rsdt_address: usize,
    ) -> Result<AcpiTables<H>, AcpiError> {
        let rsdt_mapping =
            unsafe { handler.map_physical_region::<SdtHeader>(rsdt_address, mem::size_of::<SdtHeader>()) };
        let rsdt_length = rsdt_mapping.length;
        let rsdt_mapping = unsafe { handler.map_physical_region::<SdtHeader>(rsdt_address, rsdt_length as usize) };
        Ok(Self { rsdt_mapping, rsdp_revision, handler })
    }

    /// Iterate over the **physical** addresses of the SDTs.
    pub fn table_entries(&self) -> impl Iterator<Item = usize> {
        let entry_size = if self.rsdp_revision == 0 { 4 } else { 8 };
        let mut table_entries_ptr =
            unsafe { self.rsdt_mapping.virtual_start.as_ptr().byte_add(mem::size_of::<SdtHeader>()) }.cast::<u8>();
        let mut num_entries = (self.rsdt_mapping.region_length - mem::size_of::<SdtHeader>()) / entry_size;

        core::iter::from_fn(move || {
            if num_entries > 0 {
                unsafe {
                    let entry = if entry_size == 4 {
                        *table_entries_ptr.cast::<u32>() as usize
                    } else {
                        *table_entries_ptr.cast::<u64>() as usize
                    };
                    table_entries_ptr = table_entries_ptr.byte_add(entry_size);
                    num_entries -= 1;

                    Some(entry)
                }
            } else {
                None
            }
        })
    }

    /// Iterate over the headers of each SDT, along with their **physical** addresses.
    pub fn table_headers(&self) -> impl Iterator<Item = (usize, SdtHeader)> {
        self.table_entries().map(|table_phys_address| {
            let mapping = unsafe {
                self.handler.map_physical_region::<SdtHeader>(table_phys_address, mem::size_of::<SdtHeader>())
            };
            (table_phys_address, *mapping)
        })
    }

    /// Find all tables with the signature `T::SIGNATURE`.
    pub fn find_tables<T>(&self) -> impl Iterator<Item = PhysicalMapping<H, T>>
    where
        T: AcpiTable,
    {
        self.table_entries().filter_map(|table_phys_address| {
            let header_mapping = unsafe {
                self.handler.map_physical_region::<SdtHeader>(table_phys_address, mem::size_of::<SdtHeader>())
            };
            if header_mapping.signature == T::SIGNATURE {
                // Extend the mapping to the entire table
                let length = header_mapping.length;
                drop(header_mapping);
                Some(unsafe { self.handler.map_physical_region::<T>(table_phys_address, length as usize) })
            } else {
                None
            }
        })
    }

    /// Find the first table with the signature `T::SIGNATURE`.
    pub fn find_table<T>(&self) -> Option<PhysicalMapping<H, T>>
    where
        T: AcpiTable,
    {
        self.find_tables().next()
    }

    pub fn dsdt(&self) -> Result<AmlTable, AcpiError> {
        let Some(fadt) = self.find_table::<sdt::fadt::Fadt>() else {
            Err(AcpiError::TableNotFound(Signature::FADT))?
        };
        let phys_address = fadt.dsdt_address()?;
        let header =
            unsafe { self.handler.map_physical_region::<SdtHeader>(phys_address, mem::size_of::<SdtHeader>()) };
        Ok(AmlTable { phys_address, length: header.length, revision: header.revision })
    }

    pub fn ssdts(&self) -> impl Iterator<Item = AmlTable> {
        self.table_headers().filter_map(|(phys_address, header)| {
            if header.signature == Signature::SSDT {
                Some(AmlTable { phys_address, length: header.length, revision: header.revision })
            } else {
                None
            }
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct AmlTable {
    /// The physical address of the start of the table. Add `mem::size_of::<SdtHeader>()` to this
    /// to get the physical address of the start of the AML stream.
    pub phys_address: usize,
    /// The length of the table, including the header.
    pub length: u32,
    pub revision: u8,
}

/// All types representing ACPI tables should implement this trait.
///
/// ### Safety
/// The table's memory is naively interpreted, so you must be careful in providing a type that
/// correctly represents the table's structure. Regardless of the provided type's size, the region mapped will
/// be the size specified in the SDT's header. If a table's definition may be larger than a valid
/// SDT's size, [`ExtendedField`](sdt::ExtendedField) should be used to define fields that may or
/// may not exist.
pub unsafe trait AcpiTable {
    const SIGNATURE: Signature;

    fn header(&self) -> &SdtHeader;

    fn validate(&self) -> Result<(), AcpiError> {
        unsafe { self.header().validate(Self::SIGNATURE) }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum AcpiError {
    NoValidRsdp,
    RsdpIncorrectSignature,
    RsdpInvalidOemId,
    RsdpInvalidChecksum,

    SdtInvalidSignature(Signature),
    SdtInvalidOemId(Signature),
    SdtInvalidTableId(Signature),
    SdtInvalidChecksum(Signature),
    SdtInvalidCreatorId(Signature),

    TableNotFound(Signature),
    InvalidFacsAddress,
    InvalidDsdtAddress,
    InvalidMadt(MadtError),
    InvalidGenericAddress,

    #[cfg(feature = "alloc")]
    Aml(aml::AmlError),
}

/// Describes a physical mapping created by [`RegionMapper::map_physical_region`] and unmapped by
/// [`RegionMapper::unmap_physical_region`]. The region mapped must be at least `size_of::<T>()`
/// bytes, but may be bigger.
pub struct PhysicalMapping<M, T>
where
    M: RegionMapper,
{
    /// The physical address of the mapped structure. The actual mapping may start at a lower address
    /// if the requested physical address is not well-aligned.
    pub physical_start: usize,
    /// The virtual address of the mapped structure. It must be a valid, non-null pointer to the
    /// start of the requested structure. The actual virtual mapping may start at a lower address
    /// if the requested address is not well-aligned.
    pub virtual_start: NonNull<T>,
    /// The size of the requested region, in bytes. Can be equal or larger to `size_of::<T>()`. If a
    /// larger region has been mapped, this should still be the requested size.
    pub region_length: usize,
    /// The total size of the produced mapping. This may be the same as `region_length`, or larger to
    /// meet requirements of the mapping implementation.
    pub mapped_length: usize,
    /// The [`RegionMapper`] that was used to produce the mapping. When this mapping is dropped, this
    /// mapper will be used to unmap the region.
    pub mapper: M,
}

impl<M, T> PhysicalMapping<M, T>
where
    M: RegionMapper,
{
    /// Get a pinned reference to the inner `T`. This is generally only useful if `T` is `!Unpin`,
    /// otherwise the mapping can simply be dereferenced to access the inner type.
    pub fn get(&self) -> Pin<&T> {
        unsafe { Pin::new_unchecked(self.virtual_start.as_ref()) }
    }
}

impl<R, T> fmt::Debug for PhysicalMapping<R, T>
where
    R: RegionMapper,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PhysicalMapping")
            .field("physical_start", &self.physical_start)
            .field("virtual_start", &self.virtual_start)
            .field("region_length", &self.region_length)
            .field("mapped_length", &self.mapped_length)
            .field("mapper", &())
            .finish()
    }
}

unsafe impl<M: RegionMapper + Send, T: Send> Send for PhysicalMapping<M, T> {}

impl<M, T> Deref for PhysicalMapping<M, T>
where
    T: Unpin,
    M: RegionMapper,
{
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.virtual_start.as_ref() }
    }
}

impl<M, T> DerefMut for PhysicalMapping<M, T>
where
    T: Unpin,
    M: RegionMapper,
{
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.virtual_start.as_mut() }
    }
}

impl<M, T> Drop for PhysicalMapping<M, T>
where
    M: RegionMapper,
{
    fn drop(&mut self) {
        M::unmap_physical_region(self)
    }
}

/// An implementation of this trait must be provided to allow `acpi` to map regions of physical memory to
/// access ACPI tables and other structures. This should be cheaply clonable (e.g. a reference, `Arc`,
/// marker struct, etc.) as a copy of the mapper is stored in each `PhysicalMapping` to facilitate unmapping.
pub trait RegionMapper: Clone {
    /// Given a physical address and a size, map a region of physical memory that contains `T` (note: the passed
    /// size may be larger than `size_of::<T>()`). The address is not neccessarily page-aligned, so the
    /// implementation may need to map more than `size` bytes. The virtual address the region is mapped to does not
    /// matter, as long as it is accessible to `acpi`. Refer to the fields on [`PhysicalMapping`] to understand how
    /// to produce one properly.
    ///
    /// ## Safety
    ///
    /// - `physical_address` must point to a valid `T` in physical memory.
    /// - `size` must be at least `size_of::<T>()`.
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T>;

    /// Unmap the given physical mapping. This is called when a `PhysicalMapping` is dropped, you should **not** manually call this.
    ///
    /// Note: A reference to the `RegionMapper` used to construct `region` can be acquired by calling [`PhysicalMapping::mapper`].
    fn unmap_physical_region<T>(region: &PhysicalMapping<Self, T>);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(dead_code)]
    fn test_physical_mapping_send_sync() {
        fn test_send_sync<T: Send>() {}
        fn caller<M: RegionMapper + Send, T: Send>() {
            test_send_sync::<PhysicalMapping<M, T>>();
        }
    }
}
