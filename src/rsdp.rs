use core::{str,mem};
use super::AcpiError;

/// The first structure found in ACPI. It just tells us where the RSDT is.
///
/// On BIOS systems, it is either found in the first 1KB of the Extended Bios Data Area, or between
/// 0x000E0000 and 0x000FFFFF. The signature is always on a 16 byte boundary. On (U)EFI, it may not
/// be located in these locations, and so an address should be found in the EFI_SYSTEM_TABLE instead.
///
/// The recommended way of locating the RSDP is to let the bootloader do it - Multiboot2 can pass a
/// tag with the physical address of it. If this is not possible, a manual scan can be done.
///
/// If `revision > 0`, (the hardware ACPI version is Version 2.0 or greater), the RSDP contains
/// some new fields. For ACPI Version 1.0, these fields are not valid and should not be accessed.
#[repr(C, packed)]
pub(crate) struct Rsdp
{
    signature       : [u8; 8],
    checksum        : u8,
    oem_id          : [u8; 6],
    revision        : u8,
    rsdt_address    : u32,

    /*
     * These fields are only valid for ACPI Version 2.0 and greater
     */
    length          : u32,
    xsdt_address    : u64,
    ext_checksum    : u8,
    reserved        : [u8; 3],
}

impl Rsdp
{
    /// Checks that:
    ///     1) The signature is correct
    ///     2) The checksum is correct
    ///     3) For Version 2.0+, that the extension checksum is correct
    pub(crate) fn validate(&self) -> Result<(), AcpiError>
    {
        // FIXME: Check what version of ACPI this is. This works for Version 1.0 (revision=0), but
        // for subsequent versions, we also need to check the checksum for the extension fields.
        // In fact, should we rewrite this to be clearer what fields we're testing for each
        // checksum?

        // Check the signature
        if &self.signature != b"RSD PTR "
        {
            return Err(AcpiError::RsdpIncorrectSignature);
        }

        // Check the OEM id is valid UTF8 (allows use of unwrap)
        if str::from_utf8(&self.oem_id).is_err()
        {
            return Err(AcpiError::RsdpInvalidOemId);
        }

        // Sum all bytes in the structure
        let mut sum : usize = 0;
        for i in 0..mem::size_of::<Rsdp>()
        {
            sum += unsafe { *(self as *const Rsdp as *const u8).offset(i as isize) } as usize;
        }

        // Check that the lowest byte is 0
        if sum & 0b1111_1111 != 0
        {
            return Err(AcpiError::RsdpInvalidChecksum);
        }

        Ok(())
    }

    pub(crate) fn oem_id<'a>(&'a self) -> &'a str
    {
        str::from_utf8(&self.oem_id).unwrap()
    }

    pub(crate) fn revision(&self) -> u8
    {
        self.revision
    }

    pub(crate) fn rsdt_address(&self) -> u32
    {
        self.rsdt_address
    }

    pub(crate) fn xsdt_address(&self) -> u64
    {
        assert!(self.revision > 0, "Tried to read extended RSDP field with ACPI Version 1.0");
        self.xsdt_address
    }
}
