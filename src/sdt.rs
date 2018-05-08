use core::str;
use {AcpiError, AcpiHandler};

/// All SDTs share the same header, and are `length` bytes long. The signature tells us which SDT
/// this is.
#[repr(C, packed)]
pub struct SdtHeader {
    signature: [u8; 4],
    length: u32,
    revision: u8,
    checksum: u8,
    oem_id: [u8; 6],
    oem_table_id: [u8; 8],
    oem_revision: u32,
    creator_id: u32,
    creator_revision: u32,
}

impl SdtHeader {
    /// Check that:
    ///     a) The signature matches the one given
    ///     b) The checksum of the SDT
    ///
    /// This assumes that the whole SDT is mapped.
    fn validate(&self, signature: &[u8; 4]) -> Result<(), AcpiError> {
        // Check the signature
        if &self.signature == signature {
            return Err(AcpiError::SdtInvalidSignature);
        }

        // Check the OEM id
        if str::from_utf8(&self.oem_id).is_err() {
            return Err(AcpiError::SdtInvalidOemId);
        }

        // Check the OEM table id
        if str::from_utf8(&self.oem_table_id).is_err() {
            return Err(AcpiError::SdtInvalidTableId);
        }

        // Sum all bytes in the SDT (not just the header)
        let mut sum: usize = 0;
        for i in 0..self.length {
            sum += unsafe { *(self as *const SdtHeader as *const u8).offset(i as isize) } as usize;
        }

        // Check that the lowest byte is 0
        if sum & 0b1111_1111 != 0 {
            return Err(AcpiError::SdtInvalidChecksum);
        }

        Ok(())
    }

    pub fn signature<'a>(&'a self) -> &'a str {
        // Safe to unwrap because we check signature is valid UTF8 in `validate`
        str::from_utf8(&self.signature).unwrap()
    }

    pub fn length(&self) -> u32 {
        self.length
    }

    pub fn revision(&self) -> u8 {
        self.revision
    }

    pub fn oem_id<'a>(&'a self) -> &'a str {
        // Safe to unwrap because checked in `validate`
        str::from_utf8(&self.oem_id).unwrap()
    }

    pub fn oem_table_id<'a>(&'a self) -> &'a str {
        // Safe to unwrap because checked in `validate`
        str::from_utf8(&self.oem_table_id).unwrap()
    }

    #[cfg(test)]
    pub(crate) fn make_testcase(
        signature: [u8; 4],
        length: u32,
        revision: u8,
        checksum: u8,
        oem_id: [u8; 6],
        oem_table_id: [u8; 8],
        oem_revision: u32,
        creator_id: u32,
        creator_revision: u32,
    ) -> SdtHeader {
        SdtHeader {
            signature,
            length,
            revision,
            checksum,
            oem_id,
            oem_table_id,
            oem_revision,
            creator_id,
            creator_revision,
        }
    }
}

/// This takes the physical address of an SDT, maps it correctly and dispatches it to whatever
/// function parses that table.
pub(crate) fn dispatch_sdt<H>(handler: &mut H, physical_address: usize) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let header_mapping = handler.map_physical_region::<SdtHeader>(physical_address);
    {
        let signature = (*header_mapping).signature();
        let length = (*header_mapping).length();

        /*
         * For a recognised signature, a new physical mapping should be created with the correct type
         * and length, and then the dispatched to the correct function to actually parse the table.
         */
        match signature {
            _ => {
                /*
                 * We don't recognise this signature. Early on, this probably just means we don't
                 * have support yet, but later on maybe this should become an actual error
                 */
                warn!("Unsupported SDT signature: {}. Skipping.", signature);
            }
        }
    }

    handler.unmap_physical_region(header_mapping);
    Ok(())
}
