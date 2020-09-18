use crate::{sdt::SdtHeader, AcpiError, AcpiHandler, AcpiTable, AcpiTables, GenericAddress};
use bit_field::BitField;

#[derive(Debug)]
pub enum PageProtection {
    None,
    /// Access to the adjacent 3KB to the base address will not generate a fault.
    Protected4K,
    /// Access to the adjacent 64KB to the base address will not generate a fault.
    Protected64K,
    Other,
}

/// Information about the High Precision Event Timer
#[derive(Debug)]
pub struct HpetInfo {
    pub event_timer_block_id: u32,
    pub base_address: usize,
    pub hpet_number: u8,
    /// The minimum number of clock ticks that can be set without losing interrupts (for timers in Periodic Mode)
    pub clock_tick_unit: u16,
    pub page_protection: PageProtection,
}

impl HpetInfo {
    pub fn new<H>(tables: &AcpiTables<H>) -> Result<HpetInfo, AcpiError>
    where
        H: AcpiHandler,
    {
        let hpet = unsafe {
            tables
                .get_sdt::<HpetTable>(crate::sdt::Signature::HPET)?
                .ok_or(AcpiError::TableMissing(crate::sdt::Signature::HPET))?
        };

        // Make sure the HPET's in system memory
        assert_eq!(hpet.base_address.address_space, 0);

        Ok(HpetInfo {
            event_timer_block_id: hpet.event_timer_block_id,
            base_address: hpet.base_address.address as usize,
            hpet_number: hpet.hpet_number,
            clock_tick_unit: hpet.clock_tick_unit,
            page_protection: match hpet.page_protection_oem.get_bits(0..5) {
                0 => PageProtection::None,
                1 => PageProtection::Protected4K,
                2 => PageProtection::Protected64K,
                3..=15 => PageProtection::Other,
                _ => unreachable!(),
            },
        })
    }
}

#[repr(C, packed)]
pub(crate) struct HpetTable {
    header: SdtHeader,
    event_timer_block_id: u32,
    base_address: GenericAddress,
    hpet_number: u8,
    clock_tick_unit: u16,
    page_protection_oem: u8,
}

impl AcpiTable for HpetTable {
    fn header(&self) -> &SdtHeader {
        &self.header
    }
}
