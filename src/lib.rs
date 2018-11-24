//! A library for parsing ACPI tables, and interacting with AML. This crate can be used in kernels,
//! but also aims to be usable from user-space for microkernel designs. `acpi` is still far from
//! feature-complete and is of alpha-quality, but can still be used for parsing the static tables,
//! and setting up hardware like the APIC and HPET. The AML parser and interpreter are not yet
//! usable.
//!
//! # Usage
//! To use the library, you will need to provide an implementation of the `AcpiHandler` trait, which
//! allows us to make requests such as mapping physical memory into the virtual address space. You
//! can then either provide the physical address of the RSDP to `parse_rsdp` (GRUB can provide this
//! to you with the correct Multiboot tag), provide the physical address of the RSDT or XSDT to
//! `parse_rsdt`, or do a manual search for the RSDP using `search_for_rsdp_bios` (note that this
//! **only** works on legacy BIOS systems - UEFI systems do not follow the same conventions and may
//! put the RSDP anywhere in memory).
//!
//! You can then use the provided `Acpi` struct to query information about the system. For example,
//! the `InterruptModel` will detail how the interrupt controller, usually the APIC on x86_64
//! systems, should be set up.

#![no_std]
#![feature(
    nll,
    alloc,
    exclusive_range_pattern,
    range_contains,
    exhaustive_integer_patterns
)]

#[cfg_attr(test, macro_use)]
#[cfg(test)]
extern crate std;

#[macro_use]
extern crate log;
extern crate alloc;
extern crate bit_field;

mod aml;
mod fadt;
mod hpet;
pub mod interrupt;
mod madt;
mod rsdp;
mod rsdp_search;
mod sdt;

pub use aml::AmlError;
pub use madt::MadtError;
pub use rsdp_search::search_for_rsdp_bios;

use alloc::{collections::BTreeMap, string::String, vec::Vec};
use aml::AmlValue;
use core::mem;
use core::ops::Deref;
use core::ptr::NonNull;
use interrupt::InterruptModel;
use rsdp::Rsdp;
use sdt::SdtHeader;

/// Main error type for the `acpi` crate. Describes many of the main errors that can occur when
/// parsing the ACPI tables.
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

    InvalidAmlTable([u8; 4], AmlError),

    InvalidMadt(MadtError),
}

#[repr(C, packed)]
pub(crate) struct GenericAddress {
    address_space: u8,
    bit_width: u8,
    bit_offset: u8,
    access_size: u8,
    address: u64,
}

/// The state of a processor, as describes in the MADT. This can be used to establish whether a
/// processor can be used, and who is responsible for bringing it up.
#[derive(Clone, Copy, Debug)]
pub enum ProcessorState {
    /// A processor in this state is unusable, and you must not attempt to bring it up.
    Disabled,

    /// A processor waiting for a SIPI (Startup Inter-processor Interrupt) is currently not active,
    /// but may be brought up.
    WaitingForSipi,

    /// A Running processor is currently brought up and running code.
    Running,
}

/// A processor, as described in the MADT. Each processor has a local APIC, and is either the
/// Bootstrap Processor (the processor brought up to run the bootloader), or an Application
/// Processor that the OS is responsible for bringing up.
#[derive(Clone, Copy, Debug)]
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

impl Processor {
    pub(crate) fn new(
        processor_uid: u8,
        local_apic_id: u8,
        state: ProcessorState,
        is_ap: bool,
    ) -> Processor {
        Processor {
            processor_uid,
            local_apic_id,
            state,
            is_ap,
        }
    }
}

/// Describes a physical mapping created by `AcpiHandler::map_physical_region` and unmapped by
/// `AcpiHandler::unmap_physical_region`. The region mapped must be at least `size_of::<T>()`
/// bytes, but may be bigger.
pub struct PhysicalMapping<T> {
    pub physical_start: usize,
    pub virtual_start: NonNull<T>,
    pub region_length: usize, // Can be equal or larger than size_of::<T>()
    pub mapped_length: usize, // Differs from `region_length` if padding is added for alignment
}

impl<T> Deref for PhysicalMapping<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.virtual_start.as_ref() }
    }
}

/// An implementation of this trait must be provided to allow `acpi` to access platform-specific
/// functionality, such as mapping regions of physical memory. You are free to implement these
/// however you please, as long as they conform to the documentation of each function.
pub trait AcpiHandler {
    /// Given a starting physical address and a size, map a region of physical memory that contains
    /// a `T` (but may be bigger than `size_of::<T>()`). The address doesn't have to be page-aligned,
    /// so the implementation may have to add padding to either end. The given size must be greater
    /// or equal to the size of a `T`. The virtual address the memory is mapped to does not matter,
    /// as long as it is accessible from `acpi`.
    fn map_physical_region<T>(
        &mut self,
        physical_address: usize,
        size: usize,
    ) -> PhysicalMapping<T>;

    /// Unmap the given physical mapping. Safe because we consume the mapping, and so it can't be
    /// used after being passed to this function.
    fn unmap_physical_region<T>(&mut self, region: PhysicalMapping<T>);
}

/// This structure is built up as the crate parses the ACPI tables, and passed back to the kernel.
/// It can be used to query information about the system such as the interrupt model, or values in
/// the AML namespace.
#[derive(Debug)]
pub struct Acpi {
    acpi_revision: u8,
    namespace: BTreeMap<String, AmlValue>,
    boot_processor: Option<Processor>,
    application_processors: Vec<Processor>,

    /// ACPI theoretically allows for more than one interrupt model to be supported by the same
    /// hardware. For simplicity and because hardware practically will only support one model, we
    /// just error in cases that the tables detail more than one.
    interrupt_model: Option<InterruptModel>,
}

impl Acpi {
    /// A description of the boot processor. Until you bring any more up, this is the only processor
    /// running code, even on SMP systems.
    pub fn boot_processor(&self) -> &Option<Processor> {
        &self.boot_processor
    }

    /// Descriptions of each of the application processors. These are not brought up until you do
    /// so. The application processors must be brought up in the order that they appear in this
    /// list.
    pub fn application_processors(&self) -> &Vec<Processor> {
        &self.application_processors
    }

    /// The interrupt model supported by this system.
    pub fn interrupt_model(&self) -> &Option<InterruptModel> {
        &self.interrupt_model
    }
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

fn parse_validated_rsdp<H>(
    handler: &mut H,
    rsdp_mapping: PhysicalMapping<Rsdp>,
) -> Result<Acpi, AcpiError>
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
pub fn parse_rsdt<H>(
    handler: &mut H,
    revision: u8,
    physical_address: usize,
) -> Result<Acpi, AcpiError>
where
    H: AcpiHandler,
{
    let mut acpi = Acpi {
        acpi_revision: revision,
        namespace: BTreeMap::new(),
        boot_processor: None,
        application_processors: Vec::new(),
        interrupt_model: None,
    };

    let header = sdt::peek_at_sdt_header(handler, physical_address);
    let mapping =
        handler.map_physical_region::<SdtHeader>(physical_address, header.length() as usize);

    if revision == 0 {
        /*
         * ACPI Version 1.0. It's a RSDT!
         */
        (*mapping).validate(b"RSDT")?;

        let num_tables =
            ((*mapping).length() as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u32>();
        let tables_base =
            ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u32;

        for i in 0..num_tables {
            sdt::dispatch_sdt(
                &mut acpi,
                handler,
                unsafe { *tables_base.offset(i as isize) } as usize,
            )?;
        }
    } else {
        /*
         * ACPI Version 2.0+. It's a XSDT!
         */
        (*mapping).validate(b"XSDT")?;

        let num_tables =
            ((*mapping).length() as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u64>();
        let tables_base =
            ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u64;

        for i in 0..num_tables {
            sdt::dispatch_sdt(
                &mut acpi,
                handler,
                unsafe { *tables_base.offset(i as isize) } as usize,
            )?;
        }
    }

    info!("Parsed namespace: {:#?}", acpi.namespace);

    handler.unmap_physical_region(mapping);
    Ok(acpi)
}
