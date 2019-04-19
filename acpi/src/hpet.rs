use crate::{sdt::SdtHeader, AcpiError, GenericAddress, PhysicalMapping};

#[repr(C, packed)]
pub struct Hpet {
    header: SdtHeader,

    event_timer_block_id: u32,
    base_address: GenericAddress,
    hpet_number: u8,
    clock_tick_unit: u16,
    page_protection_oem: u8,
}

pub fn parse_hpet(mapping: &PhysicalMapping<Hpet>) -> Result<(), AcpiError> {
    (*mapping).header.validate(b"HPET")?;

    Ok(())
}
