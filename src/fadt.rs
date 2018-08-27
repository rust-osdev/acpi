use aml::{parse_aml_table, AmlTable};
use sdt;
use sdt::SdtHeader;
use {Acpi, AcpiError, AcpiHandler, GenericAddress, PhysicalMapping};

/// Represents the Fixed ACPI Description Table (FADT). This table contains various fixed hardware
/// details, such as the addresses of the hardware register blocks. It also contains a pointer to
/// the Differentiated Definition Block (DSDT).
///
/// In cases where the FADT contains both a 32-bit and 64-bit field for the same address, we should
/// always prefer the 64-bit one. Only if it's zero or the CPU will not allow us to access that
/// address should the 32-bit one be used.
#[repr(C, packed)]
pub struct Fadt {
    header: SdtHeader,

    firmware_ctrl: u32,
    dsdt_address: u32,

    // used in acpi 1.0; compatibility only, should be zero
    reserved: u8,

    preferred_pm_profile: u8,
    sci_interrupt: u16,
    smi_cmd_port: u32,
    acpi_enable: u8,
    acpi_disable: u8,
    s4bios_req: u8,
    pstate_control: u8,
    pm1a_event_block: u32,
    pm1b_event_block: u32,
    pm1a_control_block: u32,
    pm1b_control_block: u32,
    pm2_control_block: u32,
    pm_timer_block: u32,
    gpe0_block: u32,
    gpe1_block: u32,
    pm1_event_length: u8,
    pm1_control_length: u8,
    pm2_control_length: u8,
    pm_timer_length: u8,
    gpe0_block_length: u8,
    gpe1_block_length: u8,
    gpe1_base: u8,
    c_state_control: u8,
    worst_c2_latency: u16,
    worst_c3_latency: u16,
    flush_size: u16,
    flush_stride: u16,
    duty_offset: u8,
    duty_width: u8,
    day_alarm: u8,
    month_alarm: u8,
    century: u8,
    iapc_boot_arch: u16,
    reserved2: u8, // must be 0
    flags: u32,
    reset_reg: GenericAddress,
    reset_value: u8,
    arm_boot_arch: u16,
    fadt_minor_version: u8,
    x_firmware_control: u64,
    x_dsdt_address: u64,
    x_pm1a_event_block: GenericAddress,
    x_pm1b_event_block: GenericAddress,
    x_pm1a_control_block: GenericAddress,
    x_pm1b_control_block: GenericAddress,
    x_pm2_control_block: GenericAddress,
    x_pm_timer_block: GenericAddress,
    x_gpe0_block: GenericAddress,
    x_gpe1_block: GenericAddress,
    sleep_control_reg: GenericAddress,
    sleep_status_reg: GenericAddress,
    hypervisor_vendor_id: u64,
}

pub(crate) fn parse_fadt<'a, 'h, H>(
    acpi: &'a mut Acpi<'h, H>,
    mapping: &PhysicalMapping<Fadt>,
) -> Result<(), AcpiError>
where
    'h: 'a,
    H: AcpiHandler + 'a,
{
    (*mapping).header.validate(b"FACP")?;

    let dsdt_physical_address: usize = if (*mapping).x_dsdt_address != 0 {
        (*mapping).x_dsdt_address as usize
    } else {
        (*mapping).dsdt_address as usize
    };

    // Parse the DSDT
    let dsdt_header = sdt::peek_at_sdt_header(acpi.handler, dsdt_physical_address);
    let dsdt_mapping = acpi
        .handler
        .map_physical_region::<AmlTable>(dsdt_physical_address, dsdt_header.length() as usize);
    if let Err(error) = parse_aml_table(acpi, &dsdt_mapping, b"DSDT") {
        error!("Failed to parse DSDT: {:?}. At this stage, this is expected, but should be fatal in the future", error);
    }
    acpi.handler.unmap_physical_region(dsdt_mapping);

    Ok(())
}

#[cfg(test)]
mod tests {
    use fadt::Fadt;
    use sdt::SdtHeader;
    use std::mem;
    use GenericAddress;

    impl Fadt {
        fn make_testcase(
            oem_id: [u8; 6],
            oem_table_id: [u8; 8],
            oem_revision: u32,
            creator_id: u32,
            creator_revision: u32,
        ) -> Fadt {
            Fadt {
                header: SdtHeader::make_testcase(
                    *b"FACP",
                    mem::size_of::<Fadt>() as u32,
                    6,
                    5, //checksum
                    oem_id,
                    oem_table_id,
                    oem_revision,
                    creator_id,
                    creator_revision,
                ),
                firmware_ctrl: 0xDEADBEEF as u32,
                dsdt_address: 0xDEADBEEF as u32,

                // used in acpi 1.0; compatibility only, should be zero
                reserved: 0 as u8,

                preferred_pm_profile: 0 as u8,
                sci_interrupt: 0 as u16,
                smi_cmd_port: 0 as u32,
                acpi_enable: 0 as u8,
                acpi_disable: 0 as u8,
                s4bios_req: 0 as u8,
                pstate_control: 0 as u8,
                pm1a_event_block: 0xDEADBEEF as u32,
                pm1b_event_block: 0xDEADBEEF as u32,
                pm1a_control_block: 0xDEADBEEF as u32,
                pm1b_control_block: 0xDEADBEEF as u32,
                pm2_control_block: 0xDEADBEEF as u32,
                pm_timer_block: 0xDEADBEEF as u32,
                gpe0_block: 0xDEADBEEF as u32,
                gpe1_block: 0xDEADBEEF as u32,
                pm1_event_length: 4 as u8,
                pm1_control_length: 2 as u8,
                pm2_control_length: 0 as u8,
                pm_timer_length: 0 as u8,
                gpe0_block_length: 2 as u8,
                gpe1_block_length: 2 as u8,
                gpe1_base: 0 as u8,
                c_state_control: 0 as u8,
                worst_c2_latency: 0 as u16,
                worst_c3_latency: 0 as u16,
                flush_size: 0 as u16,
                flush_stride: 0 as u16,
                duty_offset: 0 as u8,
                duty_width: 0 as u8,
                day_alarm: 0 as u8,
                month_alarm: 0 as u8,
                century: 0 as u8,
                iapc_boot_arch: 0 as u16,
                reserved2: 0 as u8,
                flags: 0 as u32,
                reset_reg: GenericAddress::make_testcase(),
                reset_value: 0 as u8,
                arm_boot_arch: 0 as u16,
                fadt_minor_version: 2 as u8,
                x_firmware_control: 0 as u64,
                x_dsdt_address: 0 as u64,
                x_pm1a_event_block: GenericAddress::make_testcase(),
                x_pm1b_event_block: GenericAddress::make_testcase(),
                x_pm1a_control_block: GenericAddress::make_testcase(),
                x_pm1b_control_block: GenericAddress::make_testcase(),
                x_pm2_control_block: GenericAddress::make_testcase(),
                x_pm_timer_block: GenericAddress::make_testcase(),
                x_gpe0_block: GenericAddress::make_testcase(),
                x_gpe1_block: GenericAddress::make_testcase(),
                sleep_control_reg: GenericAddress::make_testcase(),
                sleep_status_reg: GenericAddress::make_testcase(),
                hypervisor_vendor_id: 0 as u64,
            }
        }
    }
}
