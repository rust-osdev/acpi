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
#![feature(nll, exclusive_range_pattern, const_generics)]

extern crate alloc;
#[cfg_attr(test, macro_use)]
#[cfg(test)]
extern crate std;

mod fadt;
pub mod handler;
mod hpet;
pub mod interrupt;
mod madt;
mod mcfg;
mod rsdp;
mod rsdp_search;
mod sdt;

pub use crate::{
    handler::{AcpiHandler, PhysicalMapping},
    hpet::HpetInfo,
    interrupt::InterruptModel,
    madt::MadtError,
    mcfg::PciConfigRegions,
    rsdp_search::search_for_rsdp_bios,
};

use crate::{rsdp::Rsdp, sdt::SdtHeader};
use alloc::vec::Vec;
use core::mem;

#[derive(Debug)]
// TODO: manually implement Debug to print signatures correctly etc.
pub enum AcpiError {
    RsdpIncorrectSignature,
    RsdpInvalidOemId,
    RsdpInvalidChecksum,
    NoValidRsdp,

    SdtInvalidSignature([u8; 4]),
    SdtInvalidOemId([u8; 4]),
    SdtInvalidTableId([u8; 4]),
    SdtInvalidChecksum([u8; 4]),

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcessorState {
    /// A processor in this state is unusable, and you must not attempt to bring it up.
    Disabled,

    /// A processor waiting for a SIPI (Startup Inter-processor Interrupt) is currently not active,
    /// but may be brought up.
    WaitingForSipi,

    /// A Running processor is currently brought up and running code.
    Running,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Processor {
    pub processor_uid: u8,
    pub local_apic_id: u8,

    /// The state of this processor. Always check that the processor is not `Disabled` before
    /// attempting to bring it up!
    pub state: ProcessorState,

    /// Whether this processor is the Bootstrap Processor (BSP), or an Application Processor (AP).
    /// When the bootloader is entered, the BSP is the only processor running code. To run code on
    /// more than one processor, you need to "bring up" the APs.
    pub is_ap: bool,
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

#[derive(Debug)]
pub struct Acpi {
    pub acpi_revision: u8,

    /// The boot processor. Until you bring up any APs, this is the only processor running code.
    pub boot_processor: Option<Processor>,

    /// Application processes. These are not brought up until you do so, and must be brought up in
    /// the order they appear in this list.
    pub application_processors: Vec<Processor>,

    /// ACPI theoretically allows for more than one interrupt model to be supported by the same
    /// hardware. For simplicity and because hardware practically will only support one model, we
    /// just error in cases that the tables detail more than one.
    pub interrupt_model: Option<InterruptModel>,
    pub hpet: Option<HpetInfo>,

    /// Info about the DSDT, if we find it.
    pub dsdt: Option<AmlTable>,

    /// Info about any SSDTs, if there are any.
    pub ssdts: Vec<AmlTable>,

    /// Info about the PCI-E configuration memory regions, collected from the MCFG.
    pub pci_config_regions: Option<PciConfigRegions>,
}

/// This is the entry point of `acpi` if you have the **physical** address of the RSDP. It maps
/// the RSDP, works out what version of ACPI the hardware supports, and passes the physical
/// address of the RSDT/XSDT to `parse_rsdt`.
pub fn parse_rsdp<H>(handler: &mut H, rsdp_address: usize) -> Result<Acpi, AcpiError>
where
    H: AcpiHandler,
{
    let rsdp_mapping = handler.map_physical_region::<Rsdp>(rsdp_address, mem::size_of::<Rsdp>());
    (*rsdp_mapping).validate()?;

    parse_validated_rsdp(handler, rsdp_mapping)
}

fn parse_validated_rsdp<H>(handler: &mut H, rsdp_mapping: PhysicalMapping<Rsdp>) -> Result<Acpi, AcpiError>
where
    H: AcpiHandler,
{
    let revision = (*rsdp_mapping).revision();

    if revision == 0 {
        /*
         * We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
         */
        let rsdt_address = (*rsdp_mapping).rsdt_address();
        handler.unmap_physical_region(rsdp_mapping);
        parse_rsdt(handler, revision, rsdt_address as usize)
    } else {
        /*
         * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
         * to 32 bits on x86.
         */
        let xsdt_address = (*rsdp_mapping).xsdt_address();
        handler.unmap_physical_region(rsdp_mapping);
        parse_rsdt(handler, revision, xsdt_address as usize)
    }
}

/// This is the entry point of `acpi` if you already have the **physical** address of the
/// RSDT/XSDT; it parses all the SDTs in the RSDT/XSDT, calling the relevant handlers in the
/// implementation's `AcpiHandler`.
///
/// If the given revision is 0, an address to the RSDT is expected. Otherwise, an address to
/// the XSDT is expected.
pub fn parse_rsdt<H>(handler: &mut H, revision: u8, physical_address: usize) -> Result<Acpi, AcpiError>
where
    H: AcpiHandler,
{
    let mut acpi = Acpi {
        acpi_revision: revision,
        boot_processor: None,
        application_processors: Vec::new(),
        interrupt_model: None,
        hpet: None,
        dsdt: None,
        ssdts: Vec::new(),
        pci_config_regions: None,
    };

    let header = sdt::peek_at_sdt_header(handler, physical_address);
    let mapping = handler.map_physical_region::<SdtHeader>(physical_address, header.length() as usize);

    if revision == 0 {
        /*
         * ACPI Version 1.0. It's a RSDT!
         */
        (*mapping).validate(b"RSDT")?;

        let num_tables = ((*mapping).length() as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u32>();
        let tables_base = ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u32;

        for i in 0..num_tables {
            sdt::dispatch_sdt(&mut acpi, handler, unsafe { *tables_base.offset(i as isize) } as usize)?;
        }
    } else {
        /*
         * ACPI Version 2.0+. It's a XSDT!
         */
        (*mapping).validate(b"XSDT")?;

        let num_tables = ((*mapping).length() as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u64>();
        let tables_base = ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u64;

        for i in 0..num_tables {
            sdt::dispatch_sdt(&mut acpi, handler, unsafe { *tables_base.offset(i as isize) } as usize)?;
        }
    }

    handler.unmap_physical_region(mapping);
    Ok(acpi)
}
