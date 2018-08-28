#![no_std]
#![feature(nll)]
#![feature(alloc)]
#![feature(exclusive_range_pattern, range_contains)]

#[cfg(test)]
#[macro_use]
extern crate std;

#[macro_use]
extern crate log;
extern crate alloc;
extern crate bit_field;

mod aml;
mod fadt;
mod hpet;
mod rsdp;
mod sdt;
mod rsdp_search;

use alloc::{collections::BTreeMap, string::String};
use aml::{AmlError, AmlValue};
use core::mem;
use core::ops::Deref;
use core::ptr::NonNull;
use rsdp::Rsdp;
use sdt::SdtHeader;
use rsdp_search::*;

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
}

#[repr(C, packed)]
pub struct GenericAddress {
    address_space: u8,
    bit_width: u8,
    bit_offset: u8,
    access_size: u8,
    address: u64,
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

/// The kernel must provide an implementation of this trait for `acpi` to interface with. It has
/// utility methods `acpi` uses to for e.g. mapping physical memory, but also an interface for
/// `acpi` to tell the kernel about the tables it's parsing, such as how the kernel should
/// configure the APIC or PCI routing.
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

/// This struct manages the internal state of `acpi`. It is not visible to the user of the library.
pub(crate) struct Acpi<'a, H>
where
    H: AcpiHandler + 'a,
{
    handler: &'a mut H,
    acpi_revision: u8,
    namespace: BTreeMap<String, AmlValue>,
}

/// This is the entry point of `acpi` if you have no information except that the machine is running
/// BIOS and not UEFI. It maps the RSDP, works out what version of ACPI the hardware supports, and
/// passes the physical address of the RSDT/XSDT to `parse_rsdt`.
///
/// # Unsafety
///
/// This function is unsafe because it may read from protected memory if the computer is using UEFI.
/// Only use this function if you are sure the computer is using BIOS.
pub unsafe fn search_for_rsdp_bios<H>(handler: &mut H) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let ebda_start = find_ebda_start(handler);

    // The areas that will be searched for the RSDP
    let areas = [
        // Main bios area below 1 mb
        // In practice (from my [Restioson's] testing, at least), the RSDP is more often here than
        // the in EBDA. Also, if we cannot find the EBDA, then we don't want to search the largest
        // possible EBDA first.
        RSDP_BIOS_AREA_START..=RSDP_BIOS_AREA_END,

        if let Some(ebda_start) = ebda_start {
            // First kb of EBDA
            ebda_start..=ebda_start + 1024
        } else {
            // We don't know where the EBDA starts, so just search the largest possible EBDA
            EBDA_EARLIEST_START..=EBDA_END
        }
    ];


    // On x86 it is more efficient to map 4096 bytes at a time because of how paging works
    let mut area_mapping = handler.map_physical_region::<[[u8; 8]; 4096 / 8]>(
        areas[0].clone().next().unwrap(),
        4096,
    );

    // Signature is always on a 16 byte boundary so only search there
    for address in areas.iter().flat_map(|i| i.clone()).step_by(16) {

        let mapping_start = area_mapping.physical_start as usize;
        if !(mapping_start..mapping_start + 4096).contains(&address) {
            let mut new_mapping = handler.map_physical_region::<[[u8; 8]; 4096 / 8]>(
                address,
                4096,
            );

            // Avoid "variable moved in previous iteration of loop"
            let old_mapping = mem::replace(&mut area_mapping, new_mapping);
            handler.unmap_physical_region(old_mapping);
        }

        let index = (address - area_mapping.physical_start as usize) / 8;
        let signature = (*area_mapping)[index];

        if signature != *RSDP_SIGNATURE {
            continue;
        }

        let rsdp_mapping = handler.map_physical_region::<Rsdp>(address, mem::size_of::<Rsdp>());

        // This would need to be updated if any more RSDP parsing errors are added
        // (but I [Restioson <restiosondev@gmail.com>] doubt any more will).
        match (*rsdp_mapping).validate() {
            Ok(_) => (),
            Err(e @ AcpiError::RsdpIncorrectSignature)
            | Err(e @ AcpiError::RsdpInvalidOemId)
            | Err(e @ AcpiError::RsdpInvalidChecksum) => {
                warn!("Invalid RSDP found at 0x{:x}: {:?}", address, e);
                continue;
            },
            // TODO perhaps use a custom error type for the RSDP validation
            // (RsdpValidationError for example) to make this type safe
            Err(_) => unreachable!(),
        }

        handler.unmap_physical_region(area_mapping);
        return parse_validated_rsdp(handler, rsdp_mapping);
    }

    Err(AcpiError::NoValidRsdp)
}

/// This is the entry point of `acpi` if you have the **physical** address of the RSDP. It maps
/// the RSDP, works out what version of ACPI the hardware supports, and passes the physical
/// address of the RSDT/XSDT to `parse_rsdt`.
pub fn parse_rsdp<H>(handler: &mut H, rsdp_address: usize) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let rsdp_mapping = handler.map_physical_region::<Rsdp>(rsdp_address, mem::size_of::<Rsdp>());
    (*rsdp_mapping).validate()?;

    parse_validated_rsdp(handler, rsdp_mapping)
}

fn parse_validated_rsdp<H>(
    handler: &mut H,
    rsdp_mapping: PhysicalMapping<Rsdp>
) -> Result<(), AcpiError>
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
) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let mut acpi = Acpi {
        handler,
        acpi_revision: revision,
        namespace: BTreeMap::new(),
    };

    let header = sdt::peek_at_sdt_header(acpi.handler, physical_address);
    let mapping = acpi
        .handler
        .map_physical_region::<SdtHeader>(physical_address, header.length() as usize);

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
            sdt::dispatch_sdt(&mut acpi, unsafe { *tables_base.offset(i as isize) }
                as usize)?;
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
            sdt::dispatch_sdt(&mut acpi, unsafe { *tables_base.offset(i as isize) }
                as usize)?;
        }
    }

    info!("Parsed namespace: {:#?}", acpi.namespace);

    acpi.handler.unmap_physical_region(mapping);
    Ok(())
}

#[cfg(test)]
mod tests {
    use GenericAddress;

    impl GenericAddress {
        pub(crate) fn make_testcase() -> GenericAddress {
            GenericAddress {
                address_space: 0 as u8,
                bit_width: 0 as u8,
                bit_offset: 0 as u8,
                access_size: 0 as u8,
                address: 0 as u64,
            }
        }
    }
}
