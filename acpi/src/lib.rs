//! A library for parsing ACPI tables. This crate can be used by bootloaders and kernels for
//! architectures that support ACPI. The crate is far from feature-complete, but can still be used
//! for finding and parsing the static tables, which is enough to set up hardware such as the APIC
//! and HPET on x86_64.
//!
//! The crate is designed for use in conjunction with the `aml` crate, which is the (much
//! less complete) AML parser used to parse the DSDT and SSDTs. These crates are separate because
//! some kernels may want to detect the static tables, but delay AML parsing to a later stage.
//!
//! ### Usage
//! To use the library, you will need to provide an implementation of the `AcpiHandler` trait,
//! which allows the library to make requests such as mapping a particular region of physical
//! memory into the virtual address space.
//!
//! You should then call one of the entry points, based on how much information you have:
//! * Call `parse_rsdp` if you have the physical address of the RSDP
//! * Call `parse_rsdt` if you have the physical address of the RSDT / XSDT
//! * Call `search_for_rsdp_bios` if you don't have the address of either structure, but **you know
//! you're running on BIOS, not UEFI**
//!
//! All of these methods return an instance of `Acpi`. This struct contains all the information
//! gathered from the static tables, and can be queried to set up hardware etc.

#![no_std]
#![feature(const_generics, unsafe_block_in_unsafe_fn)]

extern crate alloc;
#[cfg_attr(test, macro_use)]
#[cfg(test)]
extern crate std;

mod fadt;
pub mod handler;
mod hpet;
mod madt;
mod mcfg;
pub mod platform;
mod rsdp;
mod sdt;

pub use crate::{
    fadt::PowerProfile,
    handler::{AcpiHandler, PhysicalMapping},
    hpet::HpetInfo,
    madt::MadtError,
    mcfg::PciConfigRegions,
    platform::{InterruptModel, PlatformInfo},
};

use crate::{
    rsdp::Rsdp,
    sdt::{SdtHeader, Signature},
};
use alloc::{collections::BTreeMap, vec::Vec};
use core::mem;
use log::trace;

#[derive(Debug)]
pub enum AcpiError {
    RsdpIncorrectSignature,
    RsdpInvalidOemId,
    RsdpInvalidChecksum,
    NoValidRsdp,

    SdtInvalidSignature(Signature),
    SdtInvalidOemId(Signature),
    SdtInvalidTableId(Signature),
    SdtInvalidChecksum(Signature),

    TableMissing(Signature),
    InvalidDsdtAddress,
    InvalidMadt(MadtError),
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub(crate) struct GenericAddress {
    address_space: u8,
    bit_width: u8,
    bit_offset: u8,
    access_size: u8,
    address: u64,
}

pub struct AcpiTables<H>
where
    H: AcpiHandler,
{
    /// The revision of ACPI that the system uses, as inferred from the revision of the RSDT/XSDT.
    pub revision: u8,
    pub sdts: BTreeMap<sdt::Signature, Sdt>,
    pub dsdt: Option<AmlTable>,
    pub ssdts: Vec<AmlTable>,
    handler: H,
}

impl<H> AcpiTables<H>
where
    H: AcpiHandler,
{
    /// Create an `AcpiTables` if you have the physical address of the RSDP.
    pub unsafe fn from_rsdp(handler: H, rsdp_address: usize) -> Result<AcpiTables<H>, AcpiError> {
        let rsdp_mapping = unsafe { handler.map_physical_region::<Rsdp>(rsdp_address, mem::size_of::<Rsdp>()) };
        rsdp_mapping.validate()?;

        Self::from_validated_rsdp(handler, rsdp_mapping)
    }

    /// Create an `AcpiTables` if you have a `PhysicalMapping` of the RSDP that you know is correct. This is called
    /// from `from_rsdp` after validation, and also from the RSDP search routines since they need to validate the
    /// RSDP anyways.
    fn from_validated_rsdp(
        handler: H,
        rsdp_mapping: PhysicalMapping<H, Rsdp>,
    ) -> Result<AcpiTables<H>, AcpiError> {
        let revision = rsdp_mapping.revision();

        if revision == 0 {
            /*
             * We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
             */
            let rsdt_address = rsdp_mapping.rsdt_address();
            unsafe { Self::from_rsdt(handler, revision, rsdt_address as usize) }
        } else {
            /*
             * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
             * to 32 bits on x86.
             */
            let xsdt_address = rsdp_mapping.xsdt_address();
            unsafe { Self::from_rsdt(handler, revision, xsdt_address as usize) }
        }
    }

    /// Create an `AcpiTables` if you have the physical address of the RSDT. This is useful, for example, if your chosen
    /// bootloader reads the RSDP and passes you the address of the RSDT. You also need to supply the correct ACPI
    /// revision - if `0`, a RSDT is expected, while a `XSDT` is expected for greater revisions.
    pub unsafe fn from_rsdt(handler: H, revision: u8, rsdt_address: usize) -> Result<AcpiTables<H>, AcpiError> {
        let mut result = AcpiTables { revision, sdts: BTreeMap::new(), dsdt: None, ssdts: Vec::new(), handler };

        let header = sdt::peek_at_sdt_header(&result.handler, rsdt_address);
        let mapping =
            unsafe { result.handler.map_physical_region::<SdtHeader>(rsdt_address, header.length as usize) };

        if revision == 0 {
            /*
             * ACPI Version 1.0. It's a RSDT!
             */
            mapping.validate(sdt::Signature::RSDT)?;

            let num_tables = (mapping.length as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u32>();
            let tables_base =
                ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u32;

            for i in 0..num_tables {
                result.process_sdt(unsafe { *tables_base.offset(i as isize) as usize })?;
            }
        } else {
            /*
             * ACPI Version 2.0+. It's a XSDT!
             */
            mapping.validate(sdt::Signature::XSDT)?;

            let num_tables = (mapping.length as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u64>();
            let tables_base =
                ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u64;

            for i in 0..num_tables {
                result.process_sdt(unsafe { *tables_base.offset(i as isize) as usize })?;
            }
        }

        Ok(result)
    }

    /// Construct an `AcpiTables` from a custom set of "discovered" tables. This is provided to allow the library
    /// to be used from unconventional settings (e.g. in userspace), for example with a `AcpiHandler` that detects
    /// accesses to specific physical addresses, and provides the correct data.
    pub fn from_tables_direct(
        handler: H,
        revision: u8,
        sdts: BTreeMap<sdt::Signature, Sdt>,
        dsdt: Option<AmlTable>,
        ssdts: Vec<AmlTable>,
    ) -> AcpiTables<H> {
        AcpiTables { revision, sdts, dsdt, ssdts, handler }
    }

    fn process_sdt(&mut self, physical_address: usize) -> Result<(), AcpiError> {
        let header = sdt::peek_at_sdt_header(&self.handler, physical_address);
        trace!("Found ACPI table with signature {:?} and length {:?}", header.signature, header.length);

        match header.signature {
            Signature::FADT => {
                use fadt::Fadt;

                /*
                 * For whatever reason, they chose to put the DSDT inside the FADT, instead of just listing it
                 * as another SDT. We extract it here to provide a nicer public API.
                 */
                let fadt_mapping =
                    unsafe { self.handler.map_physical_region::<Fadt>(physical_address, mem::size_of::<Fadt>()) };
                fadt_mapping.validate()?;

                let dsdt_address = fadt_mapping.dsdt_address()?;
                let dsdt_header = sdt::peek_at_sdt_header(&self.handler, dsdt_address);
                self.dsdt = Some(AmlTable::new(dsdt_address, dsdt_header.length));

                /*
                 * We've already validated the FADT to get the DSDT out, so it doesn't need to be done again.
                 */
                self.sdts
                    .insert(Signature::FADT, Sdt { physical_address, length: header.length, validated: true });
            }
            Signature::SSDT => {
                self.ssdts.push(AmlTable::new(physical_address, header.length));
            }
            signature => {
                self.sdts.insert(signature, Sdt { physical_address, length: header.length, validated: false });
            }
        }

        Ok(())
    }

    pub unsafe fn get_sdt<T>(&self, signature: sdt::Signature) -> Result<Option<PhysicalMapping<H, T>>, AcpiError>
    where
        T: AcpiTable,
        let sdt = match self.sdts.get(&signature) {
            Some(sdt) => sdt,
            None => return Ok(None),
        };
        let mapping =
            unsafe { self.handler.map_physical_region::<SdtHeader>(sdt.physical_address, sdt.length as usize) };

        if !sdt.validated {
            mapping.validate(signature)?;
        }

        Ok(Some(mapping.coerce_type()))
    }
}

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
