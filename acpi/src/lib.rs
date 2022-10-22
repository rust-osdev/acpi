//! A library for parsing ACPI tables. This crate can be used by bootloaders and kernels for architectures that
//! support ACPI. This crate is not feature-complete, but can parse lots of the more common tables. Parsing the
//! ACPI tables is required for correctly setting up the APICs, HPET, and provides useful information about power
//! management and many other platform capabilities.
//!
//! This crate is designed to find and parse the static tables ACPI provides. It should be used in conjunction with
//! the `aml` crate, which is the (much less complete) AML parser used to parse the DSDT and SSDTs. These crates
//! are separate because some kernels may want to detect the static tables, but delay AML parsing to a later stage.
//!
//! This crate requires `alloc` to make heap allocations. If you are trying to find the RSDP in an environment that
//! does not have a heap (e.g. a bootloader), you can use the `rsdp` crate. The types from that crate are
//! compatible with `acpi`.
//!
//! ### Usage
//! To use the library, you will need to provide an implementation of the `AcpiHandler` trait, which allows the
//! library to make requests such as mapping a particular region of physical memory into the virtual address space.
//!
//! You then need to construct an instance of `AcpiTables`, which can be done in a few ways depending on how much
//! information you have:
//! * Use `AcpiTables::from_rsdp` if you have the physical address of the RSDP
//! * Use `AcpiTables::from_rsdt` if you have the physical address of the RSDT/XSDT
//! * Use `AcpiTables::search_for_rsdp_bios` if you don't have the address of either, but **you know you are
//! running on BIOS, not UEFI**
//! * Use `AcpiTables::from_tables_direct` if you are using the library in an unusual setting, such as in usermode,
//!   and have a custom method to enumerate and access the tables.
//!
//! `AcpiTables` stores the addresses of all of the tables detected on a platform. The SDTs are parsed by this
//! library, or can be accessed directly with `from_sdt`, while the `DSDT` and any `SSDTs` should be parsed with
//! `aml`.
//!
//! To gather information out of the static tables, a few of the types you should take a look at are:
//!    - [`PlatformInfo`](crate::platform::PlatformInfo) parses the FADT and MADT to create a nice view of the
//!      processor topology and interrupt controllers on `x86_64`, and the interrupt controllers on other platforms.
//!      `AcpiTables::platform_info` is a convenience method for constructing a `PlatformInfo`.
//!    - [`HpetInfo`](crate::hpet::HpetInfo) parses the HPET table and tells you how to configure the High
//!      Precision Event Timer.
//!    - [`PciConfigRegions`](crate::mcfg::PciConfigRegions) parses the MCFG and tells you how PCIe configuration
//!      space is mapped into physical memory.

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

extern crate alloc;
#[cfg_attr(test, macro_use)]
#[cfg(test)]
extern crate std;

pub mod bgrt;
pub mod fadt;
pub mod hpet;
pub mod madt;
pub mod mcfg;
pub mod platform;
pub mod sdt;

pub use crate::{
    fadt::PowerProfile,
    hpet::HpetInfo,
    madt::MadtError,
    mcfg::PciConfigRegions,
    platform::{interrupt::InterruptModel, PlatformInfo},
};
pub use rsdp::{
    handler::{AcpiHandler, PhysicalMapping},
    RsdpError,
};

use crate::sdt::{SdtHeader, Signature};
use core::mem;
use rsdp::Rsdp;

#[derive(Debug)]
pub enum AcpiError {
    Rsdp(RsdpError),

    SdtInvalidSignature(Signature),
    SdtInvalidOemId(Signature),
    SdtInvalidTableId(Signature),
    SdtInvalidChecksum(Signature),

    TableMissing(Signature),
    InvalidFacsAddress,
    InvalidDsdtAddress,
    InvalidMadt(MadtError),
    InvalidGenericAddress,
}

pub type AcpiResult<T> = core::result::Result<T, AcpiError>;

/// All types representing ACPI tables should implement this trait.
pub trait AcpiTable {
    const SIGNATURE: Signature;

    fn header(&self) -> &sdt::SdtHeader;

    fn validate(&self) -> AcpiResult<()> {
        self.header().validate(Self::SIGNATURE)
    }
}

pub struct AcpiTables<H: AcpiHandler> {
    mapping: PhysicalMapping<H, SdtHeader>,
    revision: u8,
    handler: H,
}

impl<H: AcpiHandler> AcpiTables<H> {
    /// Create an `AcpiTables` if you have the physical address of the RSDP.
    pub fn from_rsdp(handler: H, address: usize) -> AcpiResult<Self> {
        let rsdp_mapping = unsafe { handler.map_physical_region::<Rsdp>(address, mem::size_of::<Rsdp>()) };
        rsdp_mapping.validate().map_err(AcpiError::Rsdp)?;

        // SAFETY: `RSDP` has been validated.
        unsafe { Self::from_validated_rsdp(handler, rsdp_mapping) }
    }

    /// Create an `RsdpReader` if you have a `PhysicalMapping` of the RSDP that you know is correct. This is called
    /// from `from_address` after validation, but can also be used if you've searched for the RSDP manually on a BIOS
    /// system.
    ///
    /// SAFETY: Caller must ensure that the provided mapping is a fully validated RSDP.
    pub unsafe fn from_validated_rsdp(handler: H, rsdp_mapping: PhysicalMapping<H, Rsdp>) -> AcpiResult<Self> {
        let revision = rsdp_mapping.revision();

        if revision == 0 {
            /*
             * We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
             */

            // SAFETY: Addresses from a validated `RSDP` are also guaranteed to be valid.
            let rsdt_mapping: PhysicalMapping<H, SdtHeader> = unsafe {
                handler.map_physical_region::<SdtHeader>(
                    rsdp_mapping.rsdt_address() as usize,
                    core::mem::size_of::<SdtHeader>(),
                )
            };
            rsdt_mapping.validate(Signature::RSDT)?;

            Ok(Self { mapping: rsdt_mapping, revision, handler })
        } else {
            /*
             * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
             * to 32 bits on x86.
             */

            // SAFETY: Addresses from a validated `RSDP` are also guaranteed to be valid.
            let xsdt_mapping: PhysicalMapping<H, SdtHeader> = unsafe {
                handler.map_physical_region::<SdtHeader>(
                    rsdp_mapping.xsdt_address() as usize,
                    core::mem::size_of::<SdtHeader>(),
                )
            };
            xsdt_mapping.validate(Signature::XSDT)?;

            Ok(Self { mapping: xsdt_mapping, revision, handler })
        }
    }

    /// Search for the RSDP on a BIOS platform. This accesses BIOS-specific memory locations and will probably not
    /// work on UEFI platforms. See [Rsdp::search_for_rsdp_bios](rsdp_search::Rsdp::search_for_rsdp_bios) for
    /// details.
    pub unsafe fn search_for_rsdp_bios(handler: H) -> AcpiResult<Self> {
        let rsdp_mapping = unsafe { Rsdp::search_for_on_bios(handler.clone()) }.map_err(AcpiError::Rsdp)?;
        // SAFETY: RSDP has been validated from `Rsdp::search_for_on_bios`
        unsafe { Self::from_validated_rsdp(handler, rsdp_mapping) }
    }

    pub fn find_table<T: AcpiTable>(&self) -> AcpiResult<PhysicalMapping<H, T>> {
        use core::mem::size_of;

        if self.revision == 0 {
            let num_tables = ((self.mapping.length as usize) - size_of::<SdtHeader>()) / size_of::<u32>();
            // SAFETY: Table pointer is known-good for these offsets and types.
            let tables_base = unsafe { self.mapping.virtual_start().as_ptr().add(1).cast::<u32>() };

            for offset in 0..num_tables {
                // SAFETY: See above safety message.
                let sdt_header_address = unsafe { tables_base.add(offset).read_unaligned() } as usize;

                // SAFETY: `RSDT` guarantees its contained addresses to be valid.
                let table_result = unsafe { read_table(self.handler.clone(), sdt_header_address) };
                if table_result.is_ok() {
                    return table_result;
                }
            }
        } else {
            let num_tables = ((self.mapping.length as usize) - size_of::<SdtHeader>()) / size_of::<u64>();
            // SAFETY: Table pointer is known-good for these offsets and types.
            let tables_base = unsafe { self.mapping.virtual_start().as_ptr().add(1).cast::<u64>() };

            for offset in 0..num_tables {
                // SAFETY: See above safety message.
                let sdt_header_address = unsafe { tables_base.add(offset).read_unaligned() } as usize;

                // SAFETY: `RSDT` guarantees its contained addresses to be valid.
                let table_result = unsafe { read_table(self.handler.clone(), sdt_header_address) };
                if table_result.is_ok() {
                    return table_result;
                }
            }
        }

        Err(AcpiError::TableMissing(T::SIGNATURE))
    }

    /// Convenience method for contructing a [`PlatformInfo`](crate::platform::PlatformInfo). This is one of the
    /// first things you should usually do with an `AcpiTables`, and allows to collect helpful information about
    /// the platform from the ACPI tables.
    pub fn platform_info(&self) -> AcpiResult<PlatformInfo> {
        PlatformInfo::new(self)
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

// SAFETY: Caller must ensure the provided address is valid for being read as an `SdtHeader`.
unsafe fn read_table<H: AcpiHandler, T: AcpiTable>(
    handler: H,
    address: usize,
) -> AcpiResult<PhysicalMapping<H, T>> {
    // Attempt to peek at the SDT header to correctly enumerate the entire table.
    // SAFETY: `address` needs to be valid for the size of `SdtHeader`, or the ACPI tables are malformed (not a software issue).
    let mapping = unsafe { handler.map_physical_region::<SdtHeader>(address, core::mem::size_of::<SdtHeader>()) };
    mapping.validate(T::SIGNATURE)?;

    // If possible (if the existing mapping covers enough memory), resuse the existing physical mapping.
    // This allows allocators/memory managers that map in chunks larger than `size_of::<SdtHeader>()` to be used more efficiently.
    if mapping.mapped_length() >= (mapping.length as usize) {
        // SAFETY: Pointer is known non-null.
        let virtual_start =
            unsafe { core::ptr::NonNull::new_unchecked(mapping.virtual_start().as_ptr() as *mut _) };

        // SAFETY: Mapping is known-good and validated.
        Ok(unsafe {
            PhysicalMapping::new(
                mapping.physical_start(),
                virtual_start,
                mapping.region_length(),
                mapping.mapped_length(),
                handler.clone(),
            )
        })
    } else {
        let sdt_length = mapping.length;
        // Drop the old mapping here, to ensure it's unmapped in software before requesting an overlapping mapping.
        drop(mapping);

        // SAFETY: Address and length are already known-good.
        Ok(unsafe { handler.map_physical_region(address, sdt_length as usize) })
    }
}
