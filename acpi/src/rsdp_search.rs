use crate::{parse_validated_rsdp, rsdp::Rsdp, Acpi, AcpiError, AcpiHandler};
use core::{mem, ops::RangeInclusive};
use log::warn;

/// The pointer to the EBDA (Extended Bios Data Area) start segment pointer
const EBDA_START_SEGMENT_PTR: usize = 0x40e;
/// The earliest (lowest) memory address an EBDA (Extended Bios Data Area) can start
const EBDA_EARLIEST_START: usize = 0x80000;
/// The end of the EBDA (Extended Bios Data Area)
const EBDA_END: usize = 0x9ffff;
/// The start of the main bios area below 1mb in which to search for the RSDP
/// (Root System Description Pointer)
const RSDP_BIOS_AREA_START: usize = 0xe0000;
/// The end of the main bios area below 1mb in which to search for the RSDP
/// (Root System Description Pointer)
const RSDP_BIOS_AREA_END: usize = 0xfffff;
/// The RSDP (Root System Description Pointer)'s signature, "RSD PTR " (note trailing space)
const RSDP_SIGNATURE: &'static [u8; 8] = b"RSD PTR ";

/// Find the begining of the EBDA (Extended Bios Data Area) and return `None` if the ptr at
/// `0x40e` is invalid.
pub fn find_search_areas<H>(handler: &mut H) -> [RangeInclusive<usize>; 2]
where
    H: AcpiHandler,
{
    // Read base segment from BIOS area. This is not always given by the bios, so it needs to be
    // checked. We left shift 4 because it is a segment ptr.
    let ebda_start_mapping =
        handler.map_physical_region::<u16>(EBDA_START_SEGMENT_PTR, mem::size_of::<u16>());
    let ebda_start = (*ebda_start_mapping as usize) << 4;
    handler.unmap_physical_region(ebda_start_mapping);

    [
        // Main bios area below 1 mb
        // In practice (from my [Restioson's] testing, at least), the RSDP is more often here than
        // the in EBDA. Also, if we cannot find the EBDA, then we don't want to search the largest
        // possible EBDA first.
        RSDP_BIOS_AREA_START..=RSDP_BIOS_AREA_END,
        // Check if base segment ptr is in valid range for EBDA base
        if (EBDA_EARLIEST_START..EBDA_END).contains(&ebda_start) {
            // First kb of EBDA
            ebda_start..=ebda_start + 1024
        } else {
            // We don't know where the EBDA starts, so just search the largest possible EBDA
            EBDA_EARLIEST_START..=EBDA_END
        },
    ]
}

/// This is the entry point of `acpi` if you have no information except that the machine is running
/// BIOS and not UEFI. It maps the RSDP, works out what version of ACPI the hardware supports, and
/// passes the physical address of the RSDT/XSDT to `parse_rsdt`.
///
/// # Unsafety
///
/// This function is unsafe because it may read from protected memory if the computer is using UEFI.
/// Only use this function if you are sure the computer is using BIOS.
pub unsafe fn search_for_rsdp_bios<H>(handler: &mut H) -> Result<Acpi, AcpiError>
where
    H: AcpiHandler,
{
    // The areas that will be searched for the RSDP
    let areas = find_search_areas(handler);

    // On x86 it is more efficient to map 4096 bytes at a time because of how paging works
    let mut area_mapping = handler.map_physical_region::<[[u8; 8]; 0x1000 / 8]>(
        areas[0].clone().next().unwrap() & !0xfff, // Get frame addr
        0x1000,
    );

    // Signature is always on a 16 byte boundary so only search there
    for address in areas.iter().flat_map(|i| i.clone()).step_by(16) {
        let mut mapping_start = area_mapping.physical_start as usize;
        if !(mapping_start..mapping_start + 0x1000).contains(&address) {
            handler.unmap_physical_region(area_mapping);
            area_mapping = handler.map_physical_region::<[[u8; 8]; 0x1000 / 8]>(
                address & !0xfff, // Get frame addr
                0x1000,
            );

            // Update if mapping remapped
            mapping_start = area_mapping.physical_start as usize;
        }

        let index = (address - mapping_start) / 8;
        let signature = (*area_mapping)[index];

        if signature != *RSDP_SIGNATURE {
            continue;
        }

        let rsdp_mapping = handler.map_physical_region::<Rsdp>(address, mem::size_of::<Rsdp>());

        if let Err(e) = (*rsdp_mapping).validate() {
            warn!("Invalid RSDP found at 0x{:x}: {:?}", address, e);
            continue;
        }

        handler.unmap_physical_region(area_mapping);
        return parse_validated_rsdp(handler, rsdp_mapping);
    }

    Err(AcpiError::NoValidRsdp)
}
