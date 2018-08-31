use core::mem;
use AcpiHandler;

/// The pointer to the EBDA (Extended Bios Data Area) start segment pointer
pub const EBDA_START_SEGMENT_PTR: usize = 0x40e;
/// The earliest (lowest) memory address an EBDA (Extended Bios Data Area) can start
pub const EBDA_EARLIEST_START: usize = 0x80000;
/// The end of the EBDA (Extended Bios Data Area)
pub const EBDA_END: usize = 0x9ffff;
/// The start of the main bios area below 1mb in which to search for the RSDP
/// (Root System Description Pointer)
pub const RSDP_BIOS_AREA_START: usize = 0xe0000;
/// The end of the main bios area below 1mb in which to search for the RSDP
/// (Root System Description Pointer)
pub const RSDP_BIOS_AREA_END: usize = 0xfffff;
/// The RSDP (Root System Description Pointer)'s signature, "RSD PTR " (note trailing space)
pub const RSDP_SIGNATURE: &'static [u8; 8] = b"RSD PTR ";

/// Find the begining of the EBDA (Extended Bios Data Area) and return `None` if the ptr at
/// `0x40e` is invalid.
pub fn find_ebda_start<H>(handler: &mut H) -> Option<usize>
where
    H: AcpiHandler,
{
    // Read base segment from BIOS area. This is not always given by the bios, so it needs to be
    // checked. We left shift 4 because it is a segment ptr.
    let base_mapping =
        handler.map_physical_region::<u16>(EBDA_START_SEGMENT_PTR, mem::size_of::<u16>());
    let base = (*base_mapping as usize) << 4;
    handler.unmap_physical_region(base_mapping);

    // Check if base segment ptr is in valid range valid
    if (EBDA_EARLIEST_START..EBDA_END).contains(&base) {
        debug!("EBDA address is {:#x}", base);
        Some(base)
    } else {
        warn!(
            "EBDA address at {:#x} out of range ({:#x}), falling back to {:#x}",
            EBDA_START_SEGMENT_PTR, base, EBDA_EARLIEST_START
        );

        None
    }
}
