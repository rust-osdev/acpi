use core::{mem, str};
use fadt::Fadt;
use hpet::Hpet;
use {Acpi, AcpiError, AcpiHandler};

/// All SDTs share the same header, and are `length` bytes long. The signature tells us which SDT
/// this is.
///
/// The ACPI Spec (Version 6.2) defines the following SDT signatures:
///     "APIC" - Multiple APIC Descriptor Table (MADT)
///     "BGRT" - Boot Graphics Resource Table
///     "BERT" - Boot Error Record Table
///     "CPEP" - Corrected Platform Error Polling Table
///     "DSDT" - Differentiated System Descriptor Table
///     "ECDT" - Embedded Controller Boot Resources Table
///     "EINJ" - Error Injection Table
///     "ERST" - Error Record Serialization Table
///     "FACP" - Fixed ACPI Description Table (FADT)
///     "FACS" - Firmware ACPI Control Structure
///     "FPDT" - Firmware Performance Data Table
///     "GTDT" - Generic Timer Description Table
///     "HEST" - Hardware Error Source Table
///     "HMAT" - Heterogeneous Memory Attributes Table
///     "MSCT" - Maximum System Characteristics Table
///     "MPST" - Memory Power State Table
///     "NFIT" - NVDIMM Firmware Interface Table
///     "OEMx" - Various OEM-specific tables
///     "PDTT" - Platform Debug Trigger Table
///     "PMTT" - Platform Memory Topology Table
///     "PPTT" - Processor Properties Topology Table
///     "PSDT" - Persistent System Description Table
///     "RASF" - ACPI RAS Feature Table
///     "RSDT" - Root System Descriptor Table
///     "SBST" - Smart Battery Specification Table
///     "SLIT" - System Locality Information Table
///     "SRAT" - System Resource Affinity Table
///     "SSDT" - Secondary System Description Table
///     "XSDT" - eXtended System Descriptor Table
#[derive(Clone, Copy)]
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
    pub fn validate(&self, signature: &[u8; 4]) -> Result<(), AcpiError> {
        // Check the signature
        if &self.signature != signature {
            return Err(AcpiError::SdtInvalidSignature(*signature));
        }

        // Check the OEM id
        if str::from_utf8(&self.oem_id).is_err() {
            return Err(AcpiError::SdtInvalidOemId(*signature));
        }

        // Check the OEM table id
        if str::from_utf8(&self.oem_table_id).is_err() {
            return Err(AcpiError::SdtInvalidTableId(*signature));
        }

        // Validate the checksum
        let self_ptr = self as *const SdtHeader as *const u8;
        let mut sum: u8 = 0;
        for i in 0..self.length {
            sum = sum.wrapping_add(unsafe { *(self_ptr.offset(i as isize)) } as u8);
        }

        if sum > 0 {
            return Err(AcpiError::SdtInvalidChecksum(*signature));
        }

        Ok(())
    }

    pub fn raw_signature(&self) -> [u8; 4] {
        self.signature
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

/// Takes the physical address of an SDT, and maps, clones and unmaps its header. Useful for
/// finding out how big it is to map it correctly later.
pub(crate) fn peek_at_sdt_header<H>(handler: &mut H, physical_address: usize) -> SdtHeader
where
    H: AcpiHandler,
{
    let mapping =
        handler.map_physical_region::<SdtHeader>(physical_address, mem::size_of::<SdtHeader>());
    let header = (*mapping).clone();
    handler.unmap_physical_region(mapping);

    header
}

/// This takes the physical address of an SDT, maps it correctly and dispatches it to whatever
/// function parses that table.
pub(crate) fn dispatch_sdt<'a, 'h, H>(
    acpi: &'a mut Acpi<'h, H>,
    physical_address: usize,
) -> Result<(), AcpiError>
where
    'h: 'a,
    H: AcpiHandler + 'a,
{
    let header = peek_at_sdt_header(acpi.handler, physical_address);
    info!(
        "Dispatching SDT with signature {:?} and length {:?}",
        header.signature(),
        header.length()
    );

    /*
     * For a recognised signature, a new physical mapping should be created with the correct type
     * and length, and then the dispatched to the correct function to actually parse the table.
     */
    match header.signature() {
        "FACP" => {
            let fadt_mapping = acpi
                .handler
                .map_physical_region::<Fadt>(physical_address, mem::size_of::<Fadt>());
            ::fadt::parse_fadt(acpi, &fadt_mapping)?;
            acpi.handler.unmap_physical_region(fadt_mapping);
        }

        "HPET" => {
            let hpet_mapping = acpi
                .handler
                .map_physical_region::<Hpet>(physical_address, mem::size_of::<Hpet>());
            ::hpet::parse_hpet(&hpet_mapping)?;
            acpi.handler.unmap_physical_region(hpet_mapping);
        }

        signature => {
            /*
             * We don't recognise this signature. Early on, this probably just means we don't
             * have support yet, but later on maybe this should become an actual error
             */
            warn!("Unsupported SDT signature: {}. Skipping.", signature);
        }
    }

    Ok(())
}
