//! A basic interpreter to extract ACPI tables from byte-slices.

use acpi::sdt::SdtHeader;
use std::mem::transmute;

const AML_TABLE_HEADER_LENGTH: usize = 36;

/// An ACPI table separated into header and content.
///
/// This is not provided in the main crate as it is unnecessary - but it is useful for testing.
pub struct TestAcpiTable {
    header: SdtHeader,
    content: Vec<u8>,
}

impl TryFrom<&[u8]> for TestAcpiTable {
    type Error = &'static str;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        if bytes.len() < AML_TABLE_HEADER_LENGTH {
            return Err("Buffer shorter than table header");
        }

        let mut header_bytes: [u8; AML_TABLE_HEADER_LENGTH] = [0; AML_TABLE_HEADER_LENGTH];
        header_bytes.copy_from_slice(&bytes[..AML_TABLE_HEADER_LENGTH]);
        let header: SdtHeader = unsafe { transmute(header_bytes) };

        if header.length < AML_TABLE_HEADER_LENGTH as u32 {
            return Err("AML table header reported length too short");
        }

        let content = bytes[AML_TABLE_HEADER_LENGTH..header.length as usize].to_vec();

        Ok(Self { header, content })
    }
}

/// Provide accessor functions for the two fields - don't provide write access so that `content`
/// and `header` can't get out of sync with each other.
impl TestAcpiTable {
    pub fn content(&self) -> &[u8] {
        &self.content
    }

    pub fn header(&self) -> &SdtHeader {
        &self.header
    }
}

/// Construct a Vec of AML tables from a slice of bytes.
pub fn bytes_to_tables(bytes: &[u8]) -> Result<Vec<TestAcpiTable>, &'static str> {
    let mut tables = Vec::new();
    let mut offset = 0;

    while offset < bytes.len() {
        let table = TestAcpiTable::try_from(&bytes[offset..])?;
        offset += table.header.length as usize;
        tables.push(table);
    }

    Ok(tables)
}
