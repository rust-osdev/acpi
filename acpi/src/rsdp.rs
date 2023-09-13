use crate::{AcpiError, AcpiHandler, AcpiResult, PhysicalMapping};
use core::{mem, ops::Range, slice, str};

/// The size in bytes of the ACPI 1.0 RSDP.
const RSDP_V1_LENGTH: usize = 20;
/// The total size in bytes of the RSDP fields introduced in ACPI 2.0.
const RSDP_V2_EXT_LENGTH: usize = mem::size_of::<Rsdp>() - RSDP_V1_LENGTH;

/// The first structure found in ACPI. It just tells us where the RSDT is.
///
/// On BIOS systems, it is either found in the first 1KiB of the Extended Bios Data Area, or between `0x000e0000`
/// and `0x000fffff`. The signature is always on a 16 byte boundary. On (U)EFI, it may not be located in these
/// locations, and so an address should be found in the EFI configuration table instead.
///
/// The recommended way of locating the RSDP is to let the bootloader do it - Multiboot2 can pass a
/// tag with the physical address of it. If this is not possible, a manual scan can be done.
///
/// If `revision > 0`, (the hardware ACPI version is Version 2.0 or greater), the RSDP contains
/// some new fields. For ACPI Version 1.0, these fields are not valid and should not be accessed.
/// For ACPI Version 2.0+, `xsdt_address` should be used (truncated to `u32` on x86) instead of
/// `rsdt_address`.
#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct Rsdp {
    signature: [u8; 8],
    checksum: u8,
    oem_id: [u8; 6],
    revision: u8,
    rsdt_address: u32,

    /*
     * These fields are only valid for ACPI Version 2.0 and greater
     */
    length: u32,
    xsdt_address: u64,
    ext_checksum: u8,
    reserved: [u8; 3],
}

impl Rsdp {
    /// This searches for a RSDP on BIOS systems.
    ///
    /// ### Safety
    /// This function probes memory in three locations:
    ///    - It reads a word from `40:0e` to locate the EBDA.
    ///    - The first 1KiB of the EBDA (Extended BIOS Data Area).
    ///    - The BIOS memory area at `0xe0000..=0xfffff`.
    ///
    /// This should be fine on all BIOS systems. However, UEFI platforms are free to put the RSDP wherever they
    /// please, so this won't always find the RSDP. Further, prodding these memory locations may have unintended
    /// side-effects. On UEFI systems, the RSDP should be found in the Configuration Table, using two GUIDs:
    ///     - ACPI v1.0 structures use `eb9d2d30-2d88-11d3-9a16-0090273fc14d`.
    ///     - ACPI v2.0 or later structures use `8868e871-e4f1-11d3-bc22-0080c73c8881`.
    /// You should search the entire table for the v2.0 GUID before searching for the v1.0 one.
    pub unsafe fn search_for_on_bios<H>(handler: H) -> AcpiResult<PhysicalMapping<H, Rsdp>>
    where
        H: AcpiHandler,
    {
        let rsdp_address = find_search_areas(handler.clone()).iter().find_map(|area| {
            // Map the search area for the RSDP followed by `RSDP_V2_EXT_LENGTH` bytes so an ACPI 1.0 RSDP at the
            // end of the area can be read as an `Rsdp` (which always has the size of an ACPI 2.0 RSDP)
            let mapping = unsafe {
                handler.map_physical_region::<u8>(area.start, area.end - area.start + RSDP_V2_EXT_LENGTH)
            };

            let extended_area_bytes =
                unsafe { slice::from_raw_parts(mapping.virtual_start().as_ptr(), mapping.region_length()) };

            // Search `Rsdp`-sized windows at 16-byte boundaries relative to the base of the area (which is also
            // aligned to 16 bytes due to the implementation of `find_search_areas`)
            extended_area_bytes.windows(mem::size_of::<Rsdp>()).step_by(16).find_map(|maybe_rsdp_bytes_slice| {
                let maybe_rsdp_virt_ptr = maybe_rsdp_bytes_slice.as_ptr().cast::<Rsdp>();
                let maybe_rsdp_phys_start = maybe_rsdp_virt_ptr as usize
                    - mapping.virtual_start().as_ptr() as usize
                    + mapping.physical_start();
                // SAFETY: `maybe_rsdp_virt_ptr` points to an aligned, readable `Rsdp`-sized value, and the `Rsdp`
                // struct's fields are always initialized.
                let maybe_rsdp = unsafe { &*maybe_rsdp_virt_ptr };

                match maybe_rsdp.validate() {
                    Ok(()) => Some(maybe_rsdp_phys_start),
                    Err(AcpiError::RsdpIncorrectSignature) => None,
                    Err(err) => {
                        log::warn!("Invalid RSDP found at {:#x}: {:?}", maybe_rsdp_phys_start, err);
                        None
                    }
                }
            })
        });

        match rsdp_address {
            Some(address) => {
                let rsdp_mapping = unsafe { handler.map_physical_region::<Rsdp>(address, mem::size_of::<Rsdp>()) };
                Ok(rsdp_mapping)
            }
            None => Err(AcpiError::NoValidRsdp),
        }
    }

    /// Checks that:
    ///     1) The signature is correct
    ///     2) The checksum is correct
    ///     3) For Version 2.0+, that the extension checksum is correct
    pub fn validate(&self) -> AcpiResult<()> {
        // Check the signature
        if self.signature != RSDP_SIGNATURE {
            return Err(AcpiError::RsdpIncorrectSignature);
        }

        // Check the OEM id is valid UTF8 (allows use of unwrap)
        if str::from_utf8(&self.oem_id).is_err() {
            return Err(AcpiError::RsdpInvalidOemId);
        }

        /*
         * `self.length` doesn't exist on ACPI version 1.0, so we mustn't rely on it. Instead,
         * check for version 1.0 and use a hard-coded length instead.
         */
        let length = if self.revision > 0 {
            // For Version 2.0+, include the number of bytes specified by `length`
            self.length as usize
        } else {
            RSDP_V1_LENGTH
        };

        let bytes = unsafe { slice::from_raw_parts(self as *const Rsdp as *const u8, length) };
        let sum = bytes.iter().fold(0u8, |sum, &byte| sum.wrapping_add(byte));

        if sum != 0 {
            return Err(AcpiError::RsdpInvalidChecksum);
        }

        Ok(())
    }

    pub fn signature(&self) -> [u8; 8] {
        self.signature
    }

    pub fn checksum(&self) -> u8 {
        self.checksum
    }

    pub fn oem_id(&self) -> &str {
        str::from_utf8(&self.oem_id).unwrap()
    }

    pub fn revision(&self) -> u8 {
        self.revision
    }

    pub fn rsdt_address(&self) -> u32 {
        self.rsdt_address
    }

    pub fn length(&self) -> u32 {
        assert!(self.revision > 0, "Tried to read extended RSDP field with ACPI Version 1.0");
        self.length
    }

    pub fn xsdt_address(&self) -> u64 {
        assert!(self.revision > 0, "Tried to read extended RSDP field with ACPI Version 1.0");
        self.xsdt_address
    }

    pub fn ext_checksum(&self) -> u8 {
        assert!(self.revision > 0, "Tried to read extended RSDP field with ACPI Version 1.0");
        self.ext_checksum
    }
}

/// Find the areas we should search for the RSDP in.
fn find_search_areas<H>(handler: H) -> [Range<usize>; 2]
where
    H: AcpiHandler,
{
    /*
     * Read the base address of the EBDA from its location in the BDA (BIOS Data Area). Not all BIOSs fill this out
     * unfortunately, so we might not get a sensible result. We shift it left 4, as it's a segment address.
     */
    let ebda_start_mapping =
        unsafe { handler.map_physical_region::<u16>(EBDA_START_SEGMENT_PTR, mem::size_of::<u16>()) };
    let ebda_start = (*ebda_start_mapping as usize) << 4;

    [
        /*
         * The main BIOS area below 1MiB. In practice, from my [Restioson's] testing, the RSDP is more often here
         * than the EBDA. We also don't want to search the entire possibele EBDA range, if we've failed to find it
         * from the BDA.
         */
        RSDP_BIOS_AREA_START..(RSDP_BIOS_AREA_END + 1),
        // Check if base segment ptr is in valid range for EBDA base
        if (EBDA_EARLIEST_START..EBDA_END).contains(&ebda_start) {
            // First KiB of EBDA
            ebda_start..ebda_start + 1024
        } else {
            // We don't know where the EBDA starts, so just search the largest possible EBDA
            EBDA_EARLIEST_START..(EBDA_END + 1)
        },
    ]
}

/// This (usually!) contains the base address of the EBDA (Extended Bios Data Area), shifted right by 4
const EBDA_START_SEGMENT_PTR: usize = 0x40e;
/// The earliest (lowest) memory address an EBDA (Extended Bios Data Area) can start
const EBDA_EARLIEST_START: usize = 0x80000;
/// The end of the EBDA (Extended Bios Data Area)
const EBDA_END: usize = 0x9ffff;
/// The start of the main BIOS area below 1MiB in which to search for the RSDP (Root System Description Pointer)
const RSDP_BIOS_AREA_START: usize = 0xe0000;
/// The end of the main BIOS area below 1MiB in which to search for the RSDP (Root System Description Pointer)
const RSDP_BIOS_AREA_END: usize = 0xfffff;
/// The RSDP (Root System Description Pointer)'s signature, "RSD PTR " (note trailing space)
const RSDP_SIGNATURE: [u8; 8] = *b"RSD PTR ";
