use core::str;

/// All SDTs share the same header, and are `length` bytes long. The signature tells us which SDT
/// this is.
#[repr(C, packed)]
pub struct SdtHeader
{
    signature           : [u8; 4],
    length              : u32,
    revision            : u8,
    checksum            : u8,
    oem_id              : [u8; 6],
    oem_table_id        : [u8; 8],
    oem_revision        : u32,
    creator_id          : u32,
    creator_revision    : u32,
}

impl SdtHeader
{
    /// Check that:
    ///     a) The signature is valid UTF8
    ///     b) The checksum of the SDT.
    ///
    /// This assumes that the whole SDT is mapped.
    fn validate(&self) -> Result<(), &str>
    {
        // Check the signature
        if str::from_utf8(&self.signature).is_err()
        {
            return Err("SDT signature is not valid UTF8");
        }

        // Sum all bytes in the SDT (not just the header)
        let mut sum : usize = 0;
        for i in 0..self.length
        {
            sum += unsafe { *(self as *const SdtHeader as *const u8).offset(i as isize) } as usize;
        }

        // Check that the lowest byte is 0
        if sum & 0b1111_1111 != 0
        {
            return Err("SDT has incorrect checksum");
        }

        Ok(())
    }
}
