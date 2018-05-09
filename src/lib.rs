#![no_std]

#[cfg(test)]
#[macro_use]
extern crate std;

#[macro_use]
extern crate log;

#[cfg(test)]
mod constructed_tables_test;
mod fadt;
mod rsdp;
mod sdt;
mod madt;

use core::mem;
use core::ops::Deref;
use core::ptr::NonNull;
use rsdp::Rsdp;
use sdt::SdtHeader;

#[derive(Debug)]
pub enum AcpiError {
    RsdpIncorrectSignature,
    RsdpInvalidOemId,
    RsdpInvalidChecksum,

    SdtInvalidSignature,
    SdtInvalidOemId,
    SdtInvalidTableId,
    SdtInvalidChecksum,

    FadtIncorrectSignature,
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
    /// Given a starting physical address, map a region of physical memory that contains a `T`
    /// somewhere in the virtual address space. The address doesn't have to be page-aligned, so
    /// the implementation may have to add padding to either end. The supplied size must be large
    /// enough to hold a `T`, but may also be larger.
    fn map_physical_region<T>(&mut self, physical_address: usize) -> PhysicalMapping<T>;

    /// Unmap the given physical mapping. Safe because we consume the mapping, and so it can't be
    /// used after being passed to this function.
    fn unmap_physical_region<T>(&mut self, region: PhysicalMapping<T>);
}

/// This is the entry point of `acpi` if you have the **physical** address of the RSDP. It maps
/// the RSDP, works out what version of ACPI the hardware supports, and passes the physical
/// address of the RSDT/XSDT to `parse_rsdt`.
pub fn parse_rsdp<H>(handler: &mut H, rsdp_address: usize) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let rsdp_mapping = handler.map_physical_region::<Rsdp>(rsdp_address);
    (*rsdp_mapping).validate()?;
    let revision = (*rsdp_mapping).revision();

    if revision == 0 {
        /*
         * We're running on ACPI Version 1.0. We should use the 32-bit RSDT address.
         */
        let rsdt_address = (*rsdp_mapping).rsdt_address();
        handler.unmap_physical_region(rsdp_mapping);
        parse_rsdt(handler, revision, rsdt_address as usize)?;
    } else {
        /*
         * We're running on ACPI Version 2.0+. We should use the 64-bit XSDT address, truncated
         * to 32 bits on x86.
         */
        let xsdt_address = (*rsdp_mapping).xsdt_address();
        handler.unmap_physical_region(rsdp_mapping);
        parse_rsdt(handler, revision, xsdt_address as usize)?;
    }

    Ok(())
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
    let mapping = handler.map_physical_region::<SdtHeader>(physical_address);

    // TODO: extend the mapping to header.length
    // TODO: validate the signature and checksum (XXX: if it's a RSDT, the signature will be
    // "RSDT", whereas a XSDT will have a signature of "XSDT"

    if revision == 0 {
        /*
         * ACPI Version 1.0. It's a RSDT!
         */
        let num_tables =
            ((*mapping).length() as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u32>();
        let tables_base =
            ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u32;

        for i in 0..num_tables {
            sdt::dispatch_sdt(handler, unsafe { *tables_base.offset(i as isize) } as usize)?;
        }
    } else {
        /*
         * ACPI Version 2.0+. It's a XSDT!
         */
        let num_tables =
            ((*mapping).length() as usize - mem::size_of::<SdtHeader>()) / mem::size_of::<u64>();
        let tables_base =
            ((mapping.virtual_start.as_ptr() as usize) + mem::size_of::<SdtHeader>()) as *const u64;

        for i in 0..num_tables {
            sdt::dispatch_sdt(handler, unsafe { *tables_base.offset(i as isize) } as usize)?;
        }
    }

    handler.unmap_physical_region(mapping);
    Ok(())
}
