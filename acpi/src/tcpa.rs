use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};

#[repr(C, packed)]
#[derive(Debug)]
pub struct Tcpa {
    pub header: SdtHeader,
    platform_class: u16,
    pub log_area_minimum_len: u32,
    pub log_area_start_addr: u64,
}

unsafe impl AcpiTable for Tcpa {
    const SIGNATURE: Signature = Signature::TCPA;

    fn header(&self) -> &crate::sdt::SdtHeader {
        &self.header
    }
}
