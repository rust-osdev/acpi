use super::AcpiError;
use core::{mem, str};

/// The first structure found in ACPI. It just tells us where the RSDT is.
///
/// On BIOS systems, it is either found in the first 1KB of the Extended Bios Data Area, or between
/// 0x000E0000 and 0x000FFFFF. The signature is always on a 16 byte boundary. On (U)EFI, it may not
/// be located in these locations, and so an address should be found in the EFI_SYSTEM_TABLE
/// instead.
///
/// The recommended way of locating the RSDP is to let the bootloader do it - Multiboot2 can pass a
/// tag with the physical address of it. If this is not possible, a manual scan can be done.
///
/// If `revision > 0`, (the hardware ACPI version is Version 2.0 or greater), the RSDP contains
/// some new fields. For ACPI Version 1.0, these fields are not valid and should not be accessed.
/// For ACPI Version 2.0+, `xsdt_address` should be used (truncated to `u32` on x86) instead of
/// `rsdt_address`.
#[repr(C, packed)]
pub(crate) struct Rsdp {
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
    /// Checks that:
    ///     1) The signature is correct
    ///     2) The checksum is correct
    ///     3) For Version 2.0+, that the extension checksum is correct
    pub(crate) fn validate(&self) -> Result<(), AcpiError> {
        // Check the signature
        if &self.signature != b"RSD PTR " {
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
            // For Version 2.0+, check ALL the fields
            mem::size_of::<Self>()
        } else {
            // For Version 1, only check fields up to v1 length only
            mem::size_of::<[u8; 8]>()
                + mem::size_of::<u8>()
                + mem::size_of::<[u8; 6]>()
                + mem::size_of::<u8>()
                + mem::size_of::<u32>()
        };

        let self_ptr = self as *const Rsdp as *const u8;
        let mut sum: u8 = 0;
        for i in 0..length {
            sum = sum.wrapping_add(unsafe { *(self_ptr.offset(i as isize)) });
        }

        if sum != 0 {
            return Err(AcpiError::RsdpInvalidChecksum);
        }

        Ok(())
    }

    pub(crate) fn oem_id<'a>(&'a self) -> &'a str {
        str::from_utf8(&self.oem_id).unwrap()
    }

    pub(crate) fn revision(&self) -> u8 {
        self.revision
    }

    pub(crate) fn rsdt_address(&self) -> u32 {
        self.rsdt_address
    }

    pub(crate) fn xsdt_address(&self) -> u64 {
        assert!(self.revision > 0, "Tried to read extended RSDP field with ACPI Version 1.0");
        self.xsdt_address
    }
}
