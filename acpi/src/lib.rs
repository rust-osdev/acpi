//! A library for parsing ACPI tables. This crate can be used by bootloaders and kernels for architectures that
//! support ACPI. This crate is not feature-complete, but can parse lots of the more common tables. Parsing the
//! ACPI tables is required for correctly setting up the APICs, HPET, and provides useful information about power
//! management and many other platform capabilities.
//!
//! This crate is designed to find and parse the static tables ACPI provides. It should be used in conjunction with
//! the `aml` crate, which is the (much less complete) AML parser used to parse the DSDT and SSDTs. These crates
//! are separate because some kernels may want to detect the static tables, but delay AML parsing to a later stage.
//!
//! This crate can be used in three configurations, depending on the environment it's being used from:
//!    - **Without allocator support** - this can be achieved by disabling the `allocator_api` and `alloc`
//!      features. The core parts of the library will still be usable, but with generally reduced functionality
//!      and ease-of-use.
//!    - **With a custom allocator** - by disabling just the `alloc` feature, you can use the `new_in` functions to
//!      access increased functionality with your own allocator. This allows `acpi` to be integrated more closely
//!      with environments that already provide a custom allocator, for example to gracefully handle allocation
//!      errors.
//!    - **With the globally-set allocator** - the `alloc` feature provides `new` functions that simply use the
//!      global allocator. This is the easiest option, and the one the majority of users will want. It is the
//!      default configuration of the crate.
//!
//! ### Usage
//! To use the library, you will need to provide an implementation of the [`AcpiHandler`] trait, which allows the
//! library to make requests such as mapping a particular region of physical memory into the virtual address space.
//!
//! You then need to construct an instance of [`AcpiTables`], which can be done in a few ways depending on how much
//! information you have:
//! * Use [`AcpiTables::from_rsdp`] if you have the physical address of the RSDP
//! * Use [`AcpiTables::from_rsdt`] if you have the physical address of the RSDT/XSDT
//! * Use [`AcpiTables::search_for_rsdp_bios`] if you don't have the address of either, but **you know you are
//!   running on BIOS, not UEFI**
//!
//! `AcpiTables` stores the addresses of all of the tables detected on a platform. The SDTs are parsed by this
//! library, or can be accessed directly with `from_sdt`, while the `DSDT` and any `SSDTs` should be parsed with
//! `aml`.
//!
//! To gather information out of the static tables, a few of the types you should take a look at are:
//!    - [`PlatformInfo`] parses the FADT and MADT to create a nice view of the processor topology and interrupt
//!      controllers on `x86_64`, and the interrupt controllers on other platforms.
//!      [`AcpiTables::platform_info`] is a convenience method for constructing a `PlatformInfo`.
//!    - [`HpetInfo`] parses the HPET table and tells you how to configure the High Precision Event Timer.
//!    - [`PciConfigRegions`] parses the MCFG and tells you how PCIe configuration space is mapped into physical
//!      memory.

/*
 * Contributing notes (you may find these useful if you're new to contributing to the library):
 *    - Accessing packed fields without UB: Lots of the structures defined by ACPI are defined with `repr(packed)`
 *      to prevent padding being introduced, which would make the structure's layout incorrect. In Rust, this
 *      creates a problem as references to these fields could be unaligned, which is undefined behaviour. For the
 *      majority of these fields, this problem can be easily avoided by telling the compiler to make a copy of the
 *      field's contents: this is the perhaps unfamiliar pattern of e.g. `!{ entry.flags }.get_bit(0)` we use
 *      around the codebase.
 */

#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]
#![cfg_attr(feature = "allocator_api", feature(allocator_api))]

#[cfg_attr(test, macro_use)]
#[cfg(test)]
extern crate std;

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod address;
pub mod bgrt;
pub mod fadt;
pub mod handler;
pub mod hpet;
pub mod madt;
pub mod mcfg;
pub mod rsdp;
pub mod sdt;
pub mod spcr;

#[cfg(feature = "allocator_api")]
mod managed_slice;
#[cfg(feature = "allocator_api")]
pub use managed_slice::*;

#[cfg(feature = "allocator_api")]
pub mod platform;
#[cfg(feature = "allocator_api")]
pub use crate::platform::{interrupt::InterruptModel, PlatformInfo};

#[cfg(feature = "allocator_api")]
pub use crate::mcfg::PciConfigRegions;

pub use fadt::PowerProfile;
pub use handler::{AcpiHandler, PhysicalMapping};
pub use hpet::HpetInfo;
pub use madt::MadtError;

use crate::sdt::{SdtHeader, Signature};
use core::mem;
use rsdp::Rsdp;

/// Result type used by error-returning functions.
pub type AcpiResult<T> = core::result::Result<T, AcpiError>;

/// All types representing ACPI tables should implement this trait.
///
/// ### Safety
///
/// The table's memory is naively interpreted, so you must be careful in providing a type that
/// correctly represents the table's structure. Regardless of the provided type's size, the region mapped will
/// be the size specified in the SDT's header. Providing a table impl that is larger than this, *may* lead to
/// page-faults, aliasing references, or derefencing uninitialized memory (the latter two being UB).
/// This isn't forbidden, however, because some tables rely on the impl being larger than a provided SDT in some
/// versions of ACPI (the [`ExtendedField`](crate::sdt::ExtendedField) type will be useful if you need to do
/// this. See our [`Fadt`](crate::fadt::Fadt) type for an example of this).
pub unsafe trait AcpiTable {
    const SIGNATURE: Signature;

    fn header(&self) -> &sdt::SdtHeader;

    fn validate(&self) -> AcpiResult<()> {
        self.header().validate(Self::SIGNATURE)
    }
}

/// Error type used by functions that return an `AcpiResult<T>`.
#[derive(Debug)]
pub enum AcpiError {
    NoValidRsdp,
    RsdpIncorrectSignature,
    RsdpInvalidOemId,
    RsdpInvalidChecksum,

    SdtInvalidSignature(Signature),
    SdtInvalidOemId(Signature),
    SdtInvalidTableId(Signature),
    SdtInvalidChecksum(Signature),

    TableMissing(Signature),
    InvalidFacsAddress,
    InvalidDsdtAddress,
    InvalidMadt(MadtError),
    InvalidGenericAddress,

    AllocError,
}

macro_rules! read_root_table {
    ($signature_name:ident, $address:ident, $acpi_handler:ident) => {{
        #[repr(transparent)]
        struct RootTable {
            header: SdtHeader,
        }

        unsafe impl AcpiTable for RootTable {
            const SIGNATURE: Signature = Signature::$signature_name;

            fn header(&self) -> &SdtHeader {
                &self.header
            }
        }

        // Map and validate root table
        // SAFETY: Addresses from a validated RSDP are also guaranteed to be valid.
        let table_mapping = unsafe { read_table::<_, RootTable>($acpi_handler.clone(), $address) }?;

        // Convert `table_mapping` to header mapping for storage
        // Avoid requesting table unmap twice (from both original and converted `table_mapping`s)
        let table_mapping = mem::ManuallyDrop::new(table_mapping);
        // SAFETY: `SdtHeader` is equivalent to `Sdt` memory-wise
        let table_mapping = unsafe {
            PhysicalMapping::new(
                table_mapping.physical_start(),
                table_mapping.virtual_start().cast::<SdtHeader>(),
                table_mapping.region_length(),
                table_mapping.mapped_length(),
                $acpi_handler.clone(),
            )
        };

        table_mapping
    }};
}

/// Type capable of enumerating the existing ACPI tables on the system.
///
///
/// ### Implementation Note
///
/// When using the `allocator_api`±`alloc` features, [`PlatformInfo::new()`] or [`PlatformInfo::new_in()`] provide
/// a much cleaner API for enumerating ACPI structures once an `AcpiTables` has been constructed.
#[derive(Debug)]
pub struct AcpiTables<H: AcpiHandler> {
    mapping: PhysicalMapping<H, SdtHeader>,
    revision: u8,
    handler: H,
}

impl<H> AcpiTables<H>
where
    H: AcpiHandler,
{
    /// Create an `AcpiTables` if you have the physical address of the RSDP.
    ///
    /// ### Safety
    ///
    /// Caller must ensure the provided address is valid to read as an RSDP.
    pub unsafe fn from_rsdp(handler: H, address: usize) -> AcpiResult<Self> {
        let rsdp_mapping = unsafe { handler.map_physical_region::<Rsdp>(address, mem::size_of::<Rsdp>()) };
        rsdp_mapping.validate()?;

        // Safety: RSDP has been validated.
        unsafe { Self::from_validated_rsdp(handler, rsdp_mapping) }
    }

    /// Search for the RSDP on a BIOS platform. This accesses BIOS-specific memory locations and will probably not
    /// work on UEFI platforms. See [`Rsdp::search_for_on_bios`] for details.
    /// details.
    ///
    /// ### Safety
    ///
    /// The caller must ensure that this function is called on BIOS platforms.
    pub unsafe fn search_for_rsdp_bios(handler: H) -> AcpiResult<Self> {
        let rsdp_mapping = unsafe { Rsdp::search_for_on_bios(handler.clone())? };
        // Safety: RSDP has been validated from `Rsdp::search_for_on_bios`
        unsafe { Self::from_validated_rsdp(handler, rsdp_mapping) }
    }

    /// Create an `AcpiTables` if you have a `PhysicalMapping` of the RSDP that you know is correct. This is called
    /// from `from_rsdp` after validation, but can also be used if you've searched for the RSDP manually on a BIOS
    /// system.
    ///
    /// ### Safety
    ///
    /// Caller must ensure that the provided mapping is a fully validated RSDP.
    pub unsafe fn from_validated_rsdp(handler: H, rsdp_mapping: PhysicalMapping<H, Rsdp>) -> AcpiResult<Self> {
        let revision = rsdp_mapping.revision();
        let root_table_mapping = if revision == 0 {
            /*
             * We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
             */
            let table_phys_start = rsdp_mapping.rsdt_address() as usize;
            drop(rsdp_mapping);
            read_root_table!(RSDT, table_phys_start, handler)
        } else {
            /*
             * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
             * to 32 bits on x86.
             */
            let table_phys_start = rsdp_mapping.xsdt_address() as usize;
            drop(rsdp_mapping);
            read_root_table!(XSDT, table_phys_start, handler)
        };

        Ok(Self { mapping: root_table_mapping, revision, handler })
    }

    /// Create an `AcpiTables` if you have the physical address of the RSDT/XSDT.
    ///
    /// ### Safety
    ///
    /// Caller must ensure the provided address is valid RSDT/XSDT address.
    pub unsafe fn from_rsdt(handler: H, revision: u8, address: usize) -> AcpiResult<Self> {
        let root_table_mapping = if revision == 0 {
            /*
             * We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
             */

            read_root_table!(RSDT, address, handler)
        } else {
            /*
             * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
             * to 32 bits on x86.
             */

            read_root_table!(XSDT, address, handler)
        };

        Ok(Self { mapping: root_table_mapping, revision, handler })
    }

    /// The ACPI revision of the tables enumerated by this structure.
    #[inline]
    pub const fn revision(&self) -> u8 {
        self.revision
    }

    /// Constructs a [`TablesPhysPtrsIter`] over this table.
    fn tables_phys_ptrs(&self) -> TablesPhysPtrsIter<'_> {
        // SAFETY: The virtual address of the array of pointers follows the virtual address of the table in memory.
        let ptrs_virt_start = unsafe { self.mapping.virtual_start().as_ptr().add(1).cast::<u8>() };
        let ptrs_bytes_len = self.mapping.region_length() - mem::size_of::<SdtHeader>();
        // SAFETY: `ptrs_virt_start` points to an array of `ptrs_bytes_len` bytes that lives as long as `self`.
        let ptrs_bytes = unsafe { core::slice::from_raw_parts(ptrs_virt_start, ptrs_bytes_len) };
        let ptr_size = if self.revision == 0 {
            4 // RSDT entry size
        } else {
            8 // XSDT entry size
        };

        ptrs_bytes.chunks(ptr_size).map(|ptr_bytes_src| {
            // Construct a native pointer using as many bytes as required from `ptr_bytes_src` (note that ACPI is
            // little-endian)

            let mut ptr_bytes_dst = [0; mem::size_of::<usize>()];
            let common_ptr_size = usize::min(mem::size_of::<usize>(), ptr_bytes_src.len());
            ptr_bytes_dst[..common_ptr_size].copy_from_slice(&ptr_bytes_src[..common_ptr_size]);

            usize::from_le_bytes(ptr_bytes_dst) as *const SdtHeader
        })
    }

    /// Searches through the ACPI table headers and attempts to locate the table with a matching `T::SIGNATURE`.
    pub fn find_table<T: AcpiTable>(&self) -> AcpiResult<PhysicalMapping<H, T>> {
        self.tables_phys_ptrs()
            .find_map(|table_phys_ptr| {
                // SAFETY: Table guarantees its contained addresses to be valid.
                match unsafe { read_table(self.handler.clone(), table_phys_ptr as usize) } {
                    Ok(table_mapping) => Some(table_mapping),
                    Err(AcpiError::SdtInvalidSignature(_)) => None,
                    Err(e) => {
                        log::warn!(
                            "Found invalid {} table at physical address {:p}: {:?}",
                            T::SIGNATURE,
                            table_phys_ptr,
                            e
                        );

                        None
                    }
                }
            })
            .ok_or(AcpiError::TableMissing(T::SIGNATURE))
    }

    /// Iterates through all of the table headers.
    pub fn headers(&self) -> SdtHeaderIterator<'_, H> {
        SdtHeaderIterator { tables_phys_ptrs: self.tables_phys_ptrs(), handler: self.handler.clone() }
    }

    /// Finds and returns the DSDT AML table, if it exists.
    pub fn dsdt(&self) -> AcpiResult<AmlTable> {
        self.find_table::<fadt::Fadt>().and_then(|fadt| {
            #[repr(transparent)]
            struct Dsdt {
                header: SdtHeader,
            }

            // Safety: Implementation properly represents a valid DSDT.
            unsafe impl AcpiTable for Dsdt {
                const SIGNATURE: Signature = Signature::DSDT;

                fn header(&self) -> &SdtHeader {
                    &self.header
                }
            }

            let dsdt_address = fadt.dsdt_address()?;
            let dsdt = unsafe { read_table::<H, Dsdt>(self.handler.clone(), dsdt_address)? };

            Ok(AmlTable::new(dsdt_address, dsdt.header().length))
        })
    }

    /// Iterates through all of the SSDT tables.
    pub fn ssdts(&self) -> SsdtIterator<H> {
        SsdtIterator { tables_phys_ptrs: self.tables_phys_ptrs(), handler: self.handler.clone() }
    }

    /// Convenience method for contructing a [`PlatformInfo`]. This is one of the first things you should usually do
    /// with an `AcpiTables`, and allows to collect helpful information about the platform from the ACPI tables.
    ///
    /// Like [`platform_info_in`](Self::platform_info_in), but uses the global allocator.
    #[cfg(feature = "alloc")]
    pub fn platform_info(&self) -> AcpiResult<PlatformInfo<alloc::alloc::Global>> {
        PlatformInfo::new(self)
    }

    /// Convenience method for contructing a [`PlatformInfo`]. This is one of the first things you should usually do
    /// with an `AcpiTables`, and allows to collect helpful information about the platform from the ACPI tables.
    #[cfg(feature = "allocator_api")]
    pub fn platform_info_in<A>(&self, allocator: A) -> AcpiResult<PlatformInfo<A>>
    where
        A: core::alloc::Allocator + Clone,
    {
        PlatformInfo::new_in(self, allocator)
    }
}

#[derive(Debug)]
pub struct Sdt {
    /// Physical address of the start of the SDT, including the header.
    pub physical_address: usize,
    /// Length of the table in bytes.
    pub length: u32,
    /// Whether this SDT has been validated. This is set to `true` the first time it is mapped and validated.
    pub validated: bool,
}

/// An iterator over the physical table addresses in an RSDT or XSDT.
type TablesPhysPtrsIter<'t> = core::iter::Map<core::slice::Chunks<'t, u8>, fn(&[u8]) -> *const SdtHeader>;

#[derive(Debug)]
pub struct AmlTable {
    /// Physical address of the start of the AML stream (excluding the table header).
    pub address: usize,
    /// Length (in bytes) of the AML stream.
    pub length: u32,
}

impl AmlTable {
    /// Create an `AmlTable` from the address and length of the table **including the SDT header**.
    pub(crate) fn new(address: usize, length: u32) -> AmlTable {
        AmlTable {
            address: address + mem::size_of::<SdtHeader>(),
            length: length - mem::size_of::<SdtHeader>() as u32,
        }
    }
}

/// ### Safety
///
/// Caller must ensure the provided address is valid for being read as an `SdtHeader`.
unsafe fn read_table<H: AcpiHandler, T: AcpiTable>(
    handler: H,
    address: usize,
) -> AcpiResult<PhysicalMapping<H, T>> {
    // Attempt to peek at the SDT header to correctly enumerate the entire table.

    // SAFETY: `address` needs to be valid for the size of `SdtHeader`, or the ACPI tables are malformed (not a
    // software issue).
    let header_mapping = unsafe { handler.map_physical_region::<SdtHeader>(address, mem::size_of::<SdtHeader>()) };

    SdtHeader::validate_lazy(header_mapping, handler)
}

/// Iterator that steps through all of the tables, and returns only the SSDTs as `AmlTable`s.
pub struct SsdtIterator<'t, H>
where
    H: AcpiHandler,
{
    tables_phys_ptrs: TablesPhysPtrsIter<'t>,
    handler: H,
}

impl<H> Iterator for SsdtIterator<'_, H>
where
    H: AcpiHandler,
{
    type Item = AmlTable;

    fn next(&mut self) -> Option<Self::Item> {
        #[repr(transparent)]
        struct Ssdt {
            header: SdtHeader,
        }

        // SAFETY: Implementation properly represents a valid SSDT.
        unsafe impl AcpiTable for Ssdt {
            const SIGNATURE: Signature = Signature::SSDT;

            fn header(&self) -> &SdtHeader {
                &self.header
            }
        }

        // Borrow single field for closure to avoid immutable reference to `self` that inhibits `find_map`
        let handler = &self.handler;

        // Consume iterator until next valid SSDT and return the latter
        self.tables_phys_ptrs.find_map(|table_phys_ptr| {
            // SAFETY: Table guarantees its contained addresses to be valid.
            match unsafe { read_table::<_, Ssdt>(handler.clone(), table_phys_ptr as usize) } {
                Ok(ssdt_mapping) => Some(AmlTable::new(ssdt_mapping.physical_start(), ssdt_mapping.header.length)),
                Err(AcpiError::SdtInvalidSignature(_)) => None,
                Err(e) => {
                    log::warn!("Found invalid SSDT at physical address {:p}: {:?}", table_phys_ptr, e);

                    None
                }
            }
        })
    }
}

pub struct SdtHeaderIterator<'t, H>
where
    H: AcpiHandler,
{
    tables_phys_ptrs: TablesPhysPtrsIter<'t>,
    handler: H,
}

impl<H> Iterator for SdtHeaderIterator<'_, H>
where
    H: AcpiHandler,
{
    type Item = SdtHeader;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let table_phys_ptr = self.tables_phys_ptrs.next()?;
            // SAFETY: `address` needs to be valid for the size of `SdtHeader`, or the ACPI tables are malformed (not a
            // software issue).
            let header_mapping = unsafe {
                self.handler.map_physical_region::<SdtHeader>(table_phys_ptr as usize, mem::size_of::<SdtHeader>())
            };
            let r = header_mapping.validate(header_mapping.signature);
            if r.is_err() {
                log::warn!("Found invalid SDT at physical address {:p}: {:?}", table_phys_ptr, r);
                continue;
            }
            return Some(*header_mapping);
        }
    }
}
