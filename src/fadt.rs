use sdt::SdtHeader;
#[cfg(test)]
use std::mem;
use {AcpiError, PhysicalMapping};

#[repr(C, packed)]
struct GenericAddress {
    address_space: u8,
    bit_width: u8,
    bit_offset: u8,
    access_size: u8,
    address: u64,
}

impl GenericAddress {
    #[cfg(test)]
    pub(crate) fn make_testcase() -> GenericAddress {
        GenericAddress {
            address_space: 0 as u8,
            bit_width: 0 as u8,
            bit_offset: 0 as u8,
            access_size: 0 as u8,
            address: 0 as u64,
        }
    }
}

#[repr(C, packed)]
pub struct Fadt {
    // header
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

impl Fadt {
    pub fn validate(&self) -> Result<(), AcpiError> {
        self.header.validate(b"FACP")
    }

    #[cfg(test)]
    pub(crate) fn make_testcase(
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

pub fn parse_fadt(mapping: &PhysicalMapping<Fadt>) -> Result<(), AcpiError> {
    (*mapping).validate()?;

    Ok(())
}
