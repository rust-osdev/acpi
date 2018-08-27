use super::AcpiError;
use core::str;

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

        // Check the fields present in all versions against `checksum`
        let mut sum: usize = 0;
        sum += self
            .signature
            .iter()
            .map(|&b| usize::from(b))
            .sum::<usize>();
        sum += self.checksum as usize;
        sum += self.oem_id.iter().map(|&b| usize::from(b)).sum::<usize>();
        sum += self.revision as usize;
        sum += self.rsdt_address as usize;

        // Check that the lowest byte is 0
        if sum & 0b1111_1111 != 0 {
            return Err(AcpiError::RsdpInvalidChecksum);
        }

        // For Version 2.0+, check the extension checksum too
        if self.revision > 0 {
            let mut sum: usize = 0;
            sum += self.length as usize;
            sum += self.xsdt_address as usize;
            sum += self.ext_checksum as usize;
            sum += self.reserved.iter().map(|&b| usize::from(b)).sum::<usize>();

            if sum & 0b1111_1111 != 0 {
                return Err(AcpiError::RsdpInvalidChecksum);
            }
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
        assert!(
            self.revision > 0,
            "Tried to read extended RSDP field with ACPI Version 1.0"
        );
        self.xsdt_address
    }
}

#[cfg(test)]
mod tests {
    use rsdp::Rsdp;

    impl Rsdp {
        /// Create a test RSDP. Checksums are passed as `Option<u8>`; if `None` is passed, the
        /// correct checksum is calculated and used.
        pub fn make_testcase(
            signature: [u8; 8],
            checksum: Option<u8>,
            oem_id: [u8; 6],
            revision: u8,
            rsdt_address: u32,
            length: u32,
            xsdt_address: u64,
            ext_checksum: Option<u8>,
            reserved: [u8; 3],
        ) -> Rsdp {
            let checksum = checksum.unwrap_or(
                ((0isize
                    - signature.iter().map(|&b| isize::from(b)).sum::<isize>()
                    - oem_id.iter().map(|&b| isize::from(b)).sum::<isize>()
                    - revision as isize
                    - rsdt_address as isize)
                    & 0b1111_1111) as u8,
            );

            let ext_checksum = ext_checksum.unwrap_or(
                ((0isize
                    - length as isize
                    - xsdt_address as isize
                    - reserved.iter().map(|&b| isize::from(b)).sum::<isize>())
                    & 0b1111_1111) as u8,
            );

            Rsdp {
                signature,
                checksum,
                oem_id,
                revision,
                rsdt_address,
                length,
                xsdt_address,
                ext_checksum,
                reserved,
            }
        }
    }
}
