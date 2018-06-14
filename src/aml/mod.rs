mod opcodes;
mod parser;
mod stream;
mod value;

pub use self::value::AmlValue;

use self::parser::AmlParser;
use self::stream::AmlStream;
use alloc::String;
use core::{mem, slice};
use sdt::SdtHeader;
use {Acpi, AcpiError, AcpiHandler, PhysicalMapping};

/// Represents a table containing AML. For ACPI Version 2+, this is just the DSDT and SSDTs.
/// Version 1.0 may also have a PSDT.
#[repr(C, packed)]
pub struct AmlTable {
    header: SdtHeader,
    // ...
}

#[derive(Debug)]
pub enum AmlError {
    EndOfStream,
    UnexpectedByte(u8),
    IncompatibleValueConversion,
    InvalidPath(String),
    InvalidFieldFlags,
    InvalidNameSeg([u8; 4]),
}

impl AmlTable {
    /// Get the AML stream encoded in this table so it can be safely accessed
    pub fn stream<'a>(&'a self) -> AmlStream<'a> {
        assert!(self.header.length() as usize > mem::size_of::<SdtHeader>());
        let stream_length = self.header.length() as usize - mem::size_of::<SdtHeader>();
        let stream_ptr =
            ((self as *const AmlTable as usize) + mem::size_of::<SdtHeader>()) as *const u8;

        unsafe { AmlStream::new(slice::from_raw_parts(stream_ptr, stream_length)) }
    }
}

pub(crate) fn parse_aml_table<'a, 'h, H>(
    acpi: &'a mut Acpi<'h, H>,
    mapping: &PhysicalMapping<AmlTable>,
    signature: &[u8; 4],
) -> Result<(), AcpiError>
where
    'h: 'a,
    H: AcpiHandler + 'a,
{
    (*mapping).header.validate(signature)?;

    match AmlParser::parse(acpi, "\\", (*mapping).stream()) {
        Ok(_) => Ok(()),
        Err(error) => Err(AcpiError::InvalidAmlTable(*signature, error)),
    }
}
