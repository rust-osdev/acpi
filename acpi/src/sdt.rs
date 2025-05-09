use crate::{AcpiError, AcpiHandler, AcpiResult, AcpiTable, PhysicalMapping};
use core::{fmt, mem::MaybeUninit, str};

/// Represents a field which may or may not be present within an ACPI structure, depending on the version of ACPI
/// that a system supports. If the field is not present, it is not safe to treat the data as initialised.
#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ExtendedField<T: Copy, const MIN_REVISION: u8>(MaybeUninit<T>);

impl<T: Copy, const MIN_REVISION: u8> ExtendedField<T, MIN_REVISION> {
    /// Access the field if it's present for the given revision of the table.
    ///
    /// ### Safety
    /// If a bogus ACPI version is passed, this function may access uninitialised data.
    pub unsafe fn access(&self, revision: u8) -> Option<T> {
        if revision >= MIN_REVISION {
            Some(unsafe { self.0.assume_init() })
        } else {
            None
        }
    }
}

/// All SDTs share the same header, and are `length` bytes long.
/// The [`Signature`] tells us which SDT this is.
#[derive(Debug, Clone, Copy)]
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
    /// Whether values of header fields are permitted.
    fn validate_header_fields(&self, signature: Signature) -> AcpiResult<()> {
        // Check the signature
        if self.signature != signature || str::from_utf8(&self.signature.0).is_err() {
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

        Ok(())
    }

    /// Whether table is valid according to checksum.
    fn validate_checksum(&self, signature: Signature) -> AcpiResult<()> {
        // SAFETY: Entire table is mapped.
        let table_bytes =
            unsafe { core::slice::from_raw_parts((self as *const SdtHeader).cast::<u8>(), self.length as usize) };
        let sum = table_bytes.iter().fold(0u8, |sum, &byte| sum.wrapping_add(byte));

        if sum == 0 {
            Ok(())
        } else {
            Err(AcpiError::SdtInvalidChecksum(signature))
        }
    }

    /// Checks that:
    ///
    /// 1. The signature matches the one given.
    /// 2. The values of various fields in the header are allowed.
    /// 3. The checksum of the SDT is valid.
    ///
    /// This assumes that the whole SDT is mapped.
    pub fn validate(&self, signature: Signature) -> AcpiResult<()> {
        self.validate_header_fields(signature)?;
        self.validate_checksum(signature)?;

        Ok(())
    }

    /// Validates header, proceeding with checking entire table and returning a [`PhysicalMapping`] to it if
    /// successful.
    ///
    /// The same checks are performed as [`SdtHeader::validate`], but `header_mapping` does not have to map the
    /// entire table when calling. This is useful to avoid completely mapping a table that will be immediately
    /// unmapped if it does not have a particular signature or has an invalid header.
    pub(crate) fn validate_lazy<H: AcpiHandler, T: AcpiTable>(
        header_mapping: PhysicalMapping<H, Self>,
        handler: H,
    ) -> AcpiResult<PhysicalMapping<H, T>> {
        header_mapping.validate_header_fields(T::SIGNATURE)?;

        // Reuse `header_mapping` to access the rest of the table if the latter is already mapped entirely
        let table_length = header_mapping.length as usize;
        let table_mapping = if header_mapping.mapped_length() >= table_length {
            // Avoid requesting table unmap twice (from both `header_mapping` and `table_mapping`)
            let header_mapping = core::mem::ManuallyDrop::new(header_mapping);

            // SAFETY: `header_mapping` maps entire table.
            unsafe {
                PhysicalMapping::new(
                    header_mapping.physical_start(),
                    header_mapping.virtual_start().cast::<T>(),
                    table_length,
                    header_mapping.mapped_length(),
                    handler,
                )
            }
        } else {
            // Unmap header as soon as possible
            let table_phys_start = header_mapping.physical_start();
            drop(header_mapping);

            // SAFETY: `table_phys_start` is the physical address of the header and the rest of the table.
            unsafe { handler.map_physical_region(table_phys_start, table_length) }
        };

        // This is usually redundant compared to simply calling `validate_checksum` but respects custom
        // `AcpiTable::validate` implementations.
        table_mapping.get().validate()?;

        Ok(table_mapping)
    }

    pub fn oem_id(&self) -> &str {
        // Safe to unwrap because checked in `validate`
        str::from_utf8(&self.oem_id).unwrap()
    }

    pub fn oem_table_id(&self) -> &str {
        // Safe to unwrap because checked in `validate`
        str::from_utf8(&self.oem_table_id).unwrap()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Signature([u8; 4]);

impl Signature {
    // The ACPI Spec (Version 6.4) defines the following SDT signatures:

    /// Root System Description Table
    pub const RSDT: Signature = Signature(*b"RSDT");
    /// Extended System Description Table
    pub const XSDT: Signature = Signature(*b"XSDT");
    /// Secondary System Description Table
    pub const SSDT: Signature = Signature(*b"SSDT");
    /// Multiple APIC Description Table (MADT)
    pub const MADT: Signature = Signature(*b"APIC");
    /// Boot Error Record Table
    pub const BERT: Signature = Signature(*b"BERT");
    /// Boot Graphics Resource Table
    pub const BGRT: Signature = Signature(*b"BGRT");
    /// Corrected Platform Error Polling Table
    pub const CPEP: Signature = Signature(*b"CPEP");
    /// Differentiated System Description Table
    pub const DSDT: Signature = Signature(*b"DSDT");
    /// Embedded Controller Boot Resources Table
    pub const ECDT: Signature = Signature(*b"ECDT");
    /// Error Injection Table
    pub const EINJ: Signature = Signature(*b"EINJ");
    /// Error Record Serialization Table
    pub const ERST: Signature = Signature(*b"ERST");
    /// Fixed ACPI Description Table (FADT)
    pub const FADT: Signature = Signature(*b"FACP");
    /// Firmware ACPI Control Structure
    pub const FACS: Signature = Signature(*b"FACS");
    /// Firmware Performance Data Table
    pub const FPDT: Signature = Signature(*b"FPDT");
    /// Generic Timer Description Table
    pub const GTDT: Signature = Signature(*b"GTDT");
    /// Hardware Error Source Table
    pub const HEST: Signature = Signature(*b"HEST");
    /// Maximum System Characteristics Table
    pub const MSCT: Signature = Signature(*b"MSCT");
    /// Memory Power State Table
    pub const MPST: Signature = Signature(*b"MPST");
    /// NVDIMM Firmware Interface Table
    pub const NFIT: Signature = Signature(*b"NFIT");
    /// Platform Communications Channel Table
    pub const PCCT: Signature = Signature(*b"PCCT");
    /// Platform Health Assessment Table
    pub const PHAT: Signature = Signature(*b"PHAT");
    /// Platform Memory Topology Table
    pub const PMTT: Signature = Signature(*b"PMTT");
    /// Persistent System Description Table
    pub const PSDT: Signature = Signature(*b"PSDT");
    /// ACPI RAS Feature Table
    pub const RASF: Signature = Signature(*b"RASF");
    /// Smart Battery Specification Table
    pub const SBST: Signature = Signature(*b"SBST");
    /// Secure Devices Table
    pub const SDEV: Signature = Signature(*b"SDEV");
    /// System Locality Distance Information Table
    pub const SLIT: Signature = Signature(*b"SLIT");
    /// System Resource Affinity Table
    pub const SRAT: Signature = Signature(*b"SRAT");
    // OEMx - OEM Specific Information Tables (Any table with a signature beginning with "OEM" falls into this definition)

    // Acpi reserves the following signatures and the specifications for them can be found [here](https://uefi.org/acpi):

    /// ARM Error Source Table
    pub const AEST: Signature = Signature(*b"AEST");
    /// BIOS Data ACPI Table
    pub const BDAT: Signature = Signature(*b"BDAT");
    /// Component Distance Information Table
    pub const CDIT: Signature = Signature(*b"CDIT");
    /// CXL Early Discovery Table
    pub const CEDT: Signature = Signature(*b"CEDT");
    /// Component Resource Attribute Table
    pub const CRAT: Signature = Signature(*b"CRAT");
    /// Core System Resource Table
    pub const CSRT: Signature = Signature(*b"CSRT");
    /// Debug Port Table
    pub const DBGP: Signature = Signature(*b"DBGP");
    /// Debug Port Table 2 (note: ACPI 6.4 defines this as "DBPG2" but this is incorrect)
    pub const DBG2: Signature = Signature(*b"DBG2");
    /// DMA Remapping Table
    pub const DMAR: Signature = Signature(*b"DMAR");
    /// Dynamic Root of Trust for Measurement Table
    pub const DRTM: Signature = Signature(*b"DRTM");
    /// Event Timer Description Table (obsolete, superseeded by HPET)
    pub const ETDT: Signature = Signature(*b"ETDT");
    /// IA-PC High Precision Event Timer Table
    pub const HPET: Signature = Signature(*b"HPET");
    /// iSCSI Boot Firmware Table
    pub const IBFT: Signature = Signature(*b"IBFT");
    /// I/O Remapping Table
    pub const IORT: Signature = Signature(*b"IORT");
    /// I/O Virtualization Reporting Structure
    pub const IVRS: Signature = Signature(*b"IVRS");
    /// Low Power Idle Table
    pub const LPIT: Signature = Signature(*b"LPIT");
    /// PCI Express Memory-mapped Configuration Space base address description table
    pub const MCFG: Signature = Signature(*b"MCFG");
    /// Management Controller Host Interface table
    pub const MCHI: Signature = Signature(*b"MCHI");
    /// ARM Memory Partitioning And Monitoring table
    pub const MPAM: Signature = Signature(*b"MPAM");
    /// Microsoft Data Management Table
    pub const MSDM: Signature = Signature(*b"MSDM");
    /// Platform Runtime Mechanism Table
    pub const PRMT: Signature = Signature(*b"PRMT");
    /// Regulatory Graphics Resource Table
    pub const RGRT: Signature = Signature(*b"RGRT");
    /// Software Delegated Exceptions Interface table
    pub const SDEI: Signature = Signature(*b"SDEI");
    /// Microsoft Software Licensing table
    pub const SLIC: Signature = Signature(*b"SLIC");
    /// Microsoft Serial Port Console Redirection table
    pub const SPCR: Signature = Signature(*b"SPCR");
    /// Server Platform Management Interface table
    pub const SPMI: Signature = Signature(*b"SPMI");
    /// _STA Override table
    pub const STAO: Signature = Signature(*b"STAO");
    /// Storage Volume Key Data table (Intel TDX only)
    pub const SVKL: Signature = Signature(*b"SVKL");
    /// Trusted Computing Platform Alliance Capabilities Table
    pub const TCPA: Signature = Signature(*b"TCPA");
    /// Trusted Platform Module 2 Table
    pub const TPM2: Signature = Signature(*b"TPM2");
    /// Unified Extensible Firmware Interface Specification table
    pub const UEFI: Signature = Signature(*b"UEFI");
    /// Windows ACPI Emulated Devices Table
    pub const WAET: Signature = Signature(*b"WAET");
    /// Watch Dog Action Table
    pub const WDAT: Signature = Signature(*b"WDAT");
    /// Watchdog Resource Table
    pub const WDRT: Signature = Signature(*b"WDRT");
    /// Windows Platform Binary Table
    pub const WPBT: Signature = Signature(*b"WPBT");
    /// Windows Security Mitigations Table
    pub const WSMT: Signature = Signature(*b"WSMT");
    /// Xen Project
    pub const XENV: Signature = Signature(*b"XENV");

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
