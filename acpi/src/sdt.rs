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

/// All SDTs share the same header, and are `length` bytes long. The signature tells us which SDT
/// this is.
///
/// The ACPI Spec (Version 6.4) defines the following SDT signatures:
///
/// * APIC - Multiple APIC Description Table (MADT)
/// * BERT - Boot Error Record Table
/// * BGRT - Boot Graphics Resource Table
/// * CPEP - Corrected Platform Error Polling Table
/// * DSDT - Differentiated System Description Table (DSDT)
/// * ECDT - Embedded Controller Boot Resources Table
/// * EINJ - Error Injection Table
/// * ERST - Error Record Serialization Table
/// * FACP - Fixed ACPI Description Table (FADT)
/// * FACS - Firmware ACPI Control Structure
/// * FPDT - Firmware Performance Data Table
/// * GTDT - Generic Timer Description Table
/// * HEST - Hardware Error Source Table
/// * MSCT - Maximum System Characteristics Table
/// * MPST - Memory Power StateTable
/// * NFIT - NVDIMM Firmware Interface Table
/// * OEMx - OEM Specific Information Tables
/// * PCCT - Platform Communications Channel Table
/// * PHAT - Platform Health Assessment Table
/// * PMTT - Platform Memory Topology Table
/// * PSDT - Persistent System Description Table
/// * RASF - ACPI RAS Feature Table
/// * RSDT - Root System Description Table
/// * SBST - Smart Battery Specification Table
/// * SDEV - Secure DEVices Table
/// * SLIT - System Locality Distance Information Table
/// * SRAT - System Resource Affinity Table
/// * SSDT - Secondary System Description Table
/// * XSDT - Extended System Description Table
///
/// Acpi reserves the following signatures and the specifications for them can be found [here](https://uefi.org/acpi):
///
/// * AEST - ARM Error Source Table
/// * BDAT - BIOS Data ACPI Table
/// * CDIT - Component Distance Information Table
/// * CEDT - CXL Early Discovery Table
/// * CRAT - Component Resource Attribute Table
/// * CSRT - Core System Resource Table
/// * DBGP - Debug Port Table
/// * DBG2 - Debug Port Table 2 (note: ACPI 6.4 defines this as "DBPG2" but this is incorrect)
/// * DMAR - DMA Remapping Table
/// * DRTM -Dynamic Root of Trust for Measurement Table
/// * ETDT - Event Timer Description Table (obsolete, superseeded by HPET)
/// * HPET - IA-PC High Precision Event Timer Table
/// * IBFT - iSCSI Boot Firmware Table
/// * IORT - I/O Remapping Table
/// * IVRS - I/O Virtualization Reporting Structure
/// * LPIT - Low Power Idle Table
/// * MCFG - PCI Express Memory-mapped Configuration Space base address description table
/// * MCHI - Management Controller Host Interface table
/// * MPAM - ARM Memory Partitioning And Monitoring table
/// * MSDM - Microsoft Data Management Table
/// * PRMT - Platform Runtime Mechanism Table
/// * RGRT - Regulatory Graphics Resource Table
/// * SDEI - Software Delegated Exceptions Interface table
/// * SLIC - Microsoft Software Licensing table
/// * SPCR - Microsoft Serial Port Console Redirection table
/// * SPMI - Server Platform Management Interface table
/// * STAO - _STA Override table
/// * SVKL - Storage Volume Key Data table (Intel TDX only)
/// * TCPA - Trusted Computing Platform Alliance Capabilities Table
/// * TPM2 - Trusted Platform Module 2 Table
/// * UEFI - Unified Extensible Firmware Interface Specification table
/// * WAET - Windows ACPI Emulated Devices Table
/// * WDAT - Watch Dog Action Table
/// * WDRT - Watchdog Resource Table
/// * WPBT - Windows Platform Binary Table
/// * WSMT - Windows Security Mitigations Table
/// * XENV - Xen Project
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
    pub const RSDT: Signature = Signature(*b"RSDT");
    pub const XSDT: Signature = Signature(*b"XSDT");
    pub const FADT: Signature = Signature(*b"FACP");
    pub const HPET: Signature = Signature(*b"HPET");
    pub const MADT: Signature = Signature(*b"APIC");
    pub const MCFG: Signature = Signature(*b"MCFG");
    pub const SSDT: Signature = Signature(*b"SSDT");
    pub const BERT: Signature = Signature(*b"BERT");
    pub const BGRT: Signature = Signature(*b"BGRT");
    pub const CPEP: Signature = Signature(*b"CPEP");
    pub const DSDT: Signature = Signature(*b"DSDT");
    pub const ECDT: Signature = Signature(*b"ECDT");
    pub const EINJ: Signature = Signature(*b"EINJ");
    pub const ERST: Signature = Signature(*b"ERST");
    pub const FACS: Signature = Signature(*b"FACS");
    pub const FPDT: Signature = Signature(*b"FPDT");
    pub const GTDT: Signature = Signature(*b"GTDT");
    pub const HEST: Signature = Signature(*b"HEST");
    pub const MSCT: Signature = Signature(*b"MSCT");
    pub const MPST: Signature = Signature(*b"MPST");
    pub const NFIT: Signature = Signature(*b"NFIT");
    pub const PCCT: Signature = Signature(*b"PCCT");
    pub const PHAT: Signature = Signature(*b"PHAT");
    pub const PMTT: Signature = Signature(*b"PMTT");
    pub const PSDT: Signature = Signature(*b"PSDT");
    pub const RASF: Signature = Signature(*b"RASF");
    pub const SBST: Signature = Signature(*b"SBST");
    pub const SDEV: Signature = Signature(*b"SDEV");
    pub const SLIT: Signature = Signature(*b"SLIT");
    pub const SRAT: Signature = Signature(*b"SRAT");
    pub const AEST: Signature = Signature(*b"AEST");
    pub const BDAT: Signature = Signature(*b"BDAT");
    pub const CDIT: Signature = Signature(*b"CDIT");
    pub const CEDT: Signature = Signature(*b"CEDT");
    pub const CRAT: Signature = Signature(*b"CRAT");
    pub const CSRT: Signature = Signature(*b"CSRT");
    pub const DBGP: Signature = Signature(*b"DBGP");
    pub const DBG2: Signature = Signature(*b"DBG2");
    pub const DMAR: Signature = Signature(*b"DMAR");
    pub const DRTM: Signature = Signature(*b"DRTM");
    pub const ETDT: Signature = Signature(*b"ETDT");
    pub const IBFT: Signature = Signature(*b"IBFT");
    pub const IORT: Signature = Signature(*b"IORT");
    pub const IVRS: Signature = Signature(*b"IVRS");
    pub const LPIT: Signature = Signature(*b"LPIT");
    pub const MCHI: Signature = Signature(*b"MCHI");
    pub const MPAM: Signature = Signature(*b"MPAM");
    pub const MSDM: Signature = Signature(*b"MSDM");
    pub const PRMT: Signature = Signature(*b"PRMT");
    pub const RGRT: Signature = Signature(*b"RGRT");
    pub const SDEI: Signature = Signature(*b"SDEI");
    pub const SLIC: Signature = Signature(*b"SLIC");
    pub const SPCR: Signature = Signature(*b"SPCR");
    pub const SPMI: Signature = Signature(*b"SPMI");
    pub const STAO: Signature = Signature(*b"STAO");
    pub const SVKL: Signature = Signature(*b"SVKL");
    pub const TCPA: Signature = Signature(*b"TCPA");
    pub const TPM2: Signature = Signature(*b"TPM2");
    pub const UEFI: Signature = Signature(*b"UEFI");
    pub const WAET: Signature = Signature(*b"WAET");
    pub const WDAT: Signature = Signature(*b"WDAT");
    pub const WDRT: Signature = Signature(*b"WDRT");
    pub const WPBT: Signature = Signature(*b"WPBT");
    pub const WSMT: Signature = Signature(*b"WSMT");
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
