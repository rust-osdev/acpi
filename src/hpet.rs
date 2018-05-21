use sdt::SdtHeader;
use {AcpiError, GenericAddress, PhysicalMapping};

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

#[cfg(test)]
mod tests {
    use hpet::Hpet;
    use GenericAddress;
    use sdt::SdtHeader;
    use std::mem;

    impl Hpet {
        fn make_testcase(
            oem_id: [u8; 6],
            oem_table_id: [u8; 8],
            oem_revision: u32,
            creator_id: u32,
            creator_revision: u32,
        ) -> Hpet {
            Hpet {
                header: SdtHeader::make_testcase(
                    *b"HPET",
                    mem::size_of::<Hpet>() as u32,
                    1,
                    3, //checksum
                    oem_id,
                    oem_table_id,
                    oem_revision,
                    creator_id,
                    creator_revision,
                ),
                event_timer_block_id: 0,
                base_address: GenericAddress::make_testcase(),
                hpet_number: 0,
                clock_tick_unit: 0,
                page_protection_oem: 0,
            }
        }
    }
}
