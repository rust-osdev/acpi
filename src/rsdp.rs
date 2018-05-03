use core::mem;

/// The first structure found in ACPI. It just tells us where the RSDT is.
///
/// On BIOS systems, it is either found in the first 1KB of the Extended Bios Data Area, or between
/// 0x000E0000 and 0x000FFFFF. The signature is always on a 16 byte boundary. On (U)EFI, it may not
/// be located in these locations, and so an address should be found in the EFI_SYSTEM_TABLE instead.
///
/// The recommended way of locating the RSDP is to let the bootloader do it - Multiboot2 can pass a
/// tag with the physical address of it. If this is not possible, a manual scan can be done.
#[repr(C, packed)]
pub struct Rsdp
{
    pub(crate) signature    : [u8; 8],
    pub(crate) checksum     : u8,
    pub(crate) oem_id       : [u8; 6],
    pub(crate) revision     : u8,
    pub(crate) rsdt_address : u32,
}

impl Rsdp
{
    /// Checks that:
    ///     1) The signature is correct
    ///     2) The checksum is correct
    ///     3) For Version 2.0+, that the extension checksum is correct
    pub(crate) fn validate(&self) -> Result<(), &str>
    {
        // FIXME: Check what version of ACPI this is. This works for Version 1.0 (revision=0), but
        // for subsequent versions, we also need to check the checksum for the extension fields.
        // In fact, should we rewrite this to be clearer what fields we're testing for each
        // checksum?

        // Check the signature
        if &self.signature != b"RSD PTR "
        {
            return Err("RSDP has incorrect signature");
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
            return Err("RSDP has incorrect checksum");
        }

        Ok(())
    }
}
