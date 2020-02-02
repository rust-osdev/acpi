use crate::{fadt::Fadt, hpet::HpetTable, madt::Madt, mcfg::Mcfg, Acpi, AcpiError, AcpiHandler, AmlTable};
use core::{fmt, mem, mem::MaybeUninit, str};
use log::{trace, warn};

pub const ACPI_VERSION_2_0: u8 = 20;

/// Represents a field which may or may not be present within an ACPI structure, depending on the version of ACPI
/// that a system supports. If the field is not present, it is not safe to treat the data as initialised.
#[repr(C, packed)]
pub struct ExtendedField<T: Copy, const MIN_VERSION: u8>(MaybeUninit<T>);

impl<T: Copy, const MIN_VERSION: u8> ExtendedField<T, MIN_VERSION> {
    /// Access the field if it's present for the given ACPI version. You should get this version from another ACPI
    /// structure, such as the RSDT/XSDT.
    ///
    /// ### Safety
    /// If a bogus ACPI version is passed, this function may access uninitialised data, which is unsafe.
    pub unsafe fn access(&self, version: u8) -> Option<T> {
        if version >= MIN_VERSION {
            Some(self.0.assume_init())
        } else {
            None
        }
    }
}

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
    pub signature: Signature,
    pub length: u32,
    pub revision: u8,
    pub checksum: u8,
    pub oem_id: [u8; 6],
    pub oem_table_id: [u8; 8],
    pub oem_revision: u32,
    pub creator_id: u32,
    pub creator_revision: u32,
}

impl SdtHeader {
    /// Check that:
    ///     a) The signature matches the one given
    ///     b) The checksum of the SDT
    ///
    /// This assumes that the whole SDT is mapped.
    pub fn validate(&self, signature: Signature) -> Result<(), AcpiError> {
        // Check the signature
        if self.signature != signature {
            return Err(AcpiError::SdtInvalidSignature(signature));
        }

        // Check the OEM id
        if str::from_utf8(&self.oem_id).is_err() {
            return Err(AcpiError::SdtInvalidOemId(signature));
        }

        // Check the OEM table id
        if str::from_utf8(&self.oem_table_id).is_err() {
            return Err(AcpiError::SdtInvalidTableId(signature));
        }

        // Validate the checksum
        let self_ptr = self as *const SdtHeader as *const u8;
        let mut sum: u8 = 0;
        for i in 0..self.length {
            sum = sum.wrapping_add(unsafe { *(self_ptr.offset(i as isize)) } as u8);
        }

        if sum > 0 {
            return Err(AcpiError::SdtInvalidChecksum(signature));
        }

        Ok(())
    }

    pub fn oem_id<'a>(&'a self) -> &'a str {
        // Safe to unwrap because checked in `validate`
        str::from_utf8(&self.oem_id).unwrap()
    }

    pub fn oem_table_id<'a>(&'a self) -> &'a str {
        // Safe to unwrap because checked in `validate`
        str::from_utf8(&self.oem_table_id).unwrap()
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Signature([u8; 4]);

impl Signature {
    pub const RSDT: Signature = Signature(*b"RSDT");
    pub const XSDT: Signature = Signature(*b"XSDT");
    pub const FADT: Signature = Signature(*b"FACP");
    pub const HPET: Signature = Signature(*b"HPET");
    pub const MADT: Signature = Signature(*b"APIC");
    pub const MCFG: Signature = Signature(*b"MCFG");
    pub const SSDT: Signature = Signature(*b"SSDT");

    pub fn as_str(&self) -> &str {
        str::from_utf8(&self.0).unwrap()
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.as_str())
    }
}

/// Takes the physical address of an SDT, and maps, clones and unmaps its header. Useful for
/// finding out how big it is to map it correctly later.
pub(crate) fn peek_at_sdt_header<H>(handler: &mut H, physical_address: usize) -> SdtHeader
where
    H: AcpiHandler,
{
    let mapping = handler.map_physical_region::<SdtHeader>(physical_address, mem::size_of::<SdtHeader>());
    let header = (*mapping).clone();
    handler.unmap_physical_region(mapping);

    header
}

/// This takes the physical address of an SDT, maps it correctly and dispatches it to whatever
/// function parses that table.
pub(crate) fn dispatch_sdt<H>(acpi: &mut Acpi, handler: &mut H, physical_address: usize) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let header = peek_at_sdt_header(handler, physical_address);
    trace!("Found ACPI table with signature {:?} and length {:?}", header.signature, header.length);

    /*
     * For a recognised signature, a new physical mapping should be created with the correct type
     * and length, and then the dispatched to the correct function to actually parse the table.
     */
    match header.signature {
        Signature::FADT => {
            let fadt_mapping = handler.map_physical_region::<Fadt>(physical_address, mem::size_of::<Fadt>());
            crate::fadt::parse_fadt(acpi, handler, &fadt_mapping)?;
            handler.unmap_physical_region(fadt_mapping);
        }

        Signature::HPET => {
            let hpet_mapping =
                handler.map_physical_region::<HpetTable>(physical_address, mem::size_of::<HpetTable>());
            crate::hpet::parse_hpet(acpi, &hpet_mapping)?;
            handler.unmap_physical_region(hpet_mapping);
        }

        Signature::MADT => {
            let madt_mapping = handler.map_physical_region::<Madt>(physical_address, header.length as usize);
            crate::madt::parse_madt(acpi, handler, &madt_mapping)?;
            handler.unmap_physical_region(madt_mapping);
        }

        Signature::MCFG => {
            let mcfg_mapping = handler.map_physical_region::<Mcfg>(physical_address, header.length as usize);
            crate::mcfg::parse_mcfg(acpi, &mcfg_mapping)?;
            handler.unmap_physical_region(mcfg_mapping);
        }

        Signature::SSDT => acpi.ssdts.push(AmlTable::new(physical_address, header.length)),

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
