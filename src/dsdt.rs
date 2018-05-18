use core::{mem, slice};
use sdt::SdtHeader;
use {AcpiError, PhysicalMapping};

/// Represents the Differentiated Definition Block (DSDT). This table contains the standard header,
/// then an AML-encoded definition block.
#[repr(C, packed)]
pub struct Dsdt {
    header: SdtHeader,
}

impl Dsdt {
    /// Get the AML stream encoded in this table so it can be safely accessed
    pub fn stream(&self) -> &[u8] {
        assert!(self.header.length() as usize > mem::size_of::<SdtHeader>());
        let stream_length = self.header.length() as usize - mem::size_of::<SdtHeader>();
        let stream_ptr =
            ((self as *const Dsdt as usize) + mem::size_of::<SdtHeader>()) as *const u8;
        unsafe { slice::from_raw_parts(stream_ptr, stream_length) }
    }
}

pub fn parse_dsdt(mapping: &PhysicalMapping<Dsdt>) -> Result<(), AcpiError> {
    (*mapping).header.validate(b"DSDT")?;

    let stream = (*mapping).stream();
    // TODO: pass off to the AML parser
    unimplemented!();
}
