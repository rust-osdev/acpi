use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};

#[repr(C, packed)]
#[derive(Debug)]
pub struct Tpm2 {
    pub header: SdtHeader,
    platform_class: u16,
    _reserved: [u8; 2],
    addr_of_crb_control_area_or_fifo_base_addr: u64,
    start_method: u32,
    start_method_parameters: [u8; 16],
    log_area_minimum_len: u32,
    log_area_start_addr: u64,
}

unsafe impl AcpiTable for Tpm2 {
    const SIGNATURE: Signature = Signature::TPM2;

    fn header(&self) -> &crate::sdt::SdtHeader {
        &self.header
    }
}

impl Tpm2 {
    pub fn platform_class(&self) -> Option<PlatformClass> {
        match self.platform_class {
            0 => Some(PlatformClass::Client),
            1 => Some(PlatformClass::Server),
            _ => None,
        }
    }

    pub fn start_method(&self) -> Option<StartMethod> {
        match self.start_method {
            2 => Some(StartMethod::AcpiStartMethod),
            6 => Some(StartMethod::Mmio),
            7 => Some(StartMethod::CommandResponseBufferInterface),
            8 => Some(StartMethod::CommandResponseBufferInterfaceWithAcpiStartMethod),
            11 => Some(StartMethod::CommandResponseBufferWithArmSecureMonitorOrHypervisorCall),
            12 => Some(StartMethod::FifoOverI2c),
            13 => Some(StartMethod::CommandResponseBufferInterfaceWithAmdMailboxSpecificNotification),
            15 => Some(StartMethod::CommandResponseBufferInterfaceWithAmdFirmwareFrameworkA),
            _ => None,
        }
    }
}

#[derive(Debug)]
#[repr(u16)]
pub enum PlatformClass {
    Client,
    Server,
}

#[derive(Debug)]
#[repr(u32)]
pub enum StartMethod {
    AcpiStartMethod = 2,
    Mmio = 6,
    CommandResponseBufferInterface = 7,
    CommandResponseBufferInterfaceWithAcpiStartMethod = 8,
    CommandResponseBufferWithArmSecureMonitorOrHypervisorCall = 11,
    FifoOverI2c = 12,
    CommandResponseBufferInterfaceWithAmdMailboxSpecificNotification = 13,
    CommandResponseBufferInterfaceWithAmdFirmwareFrameworkA = 15,
}
