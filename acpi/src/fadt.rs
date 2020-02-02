use crate::{
    sdt::{ExtendedField, SdtHeader, ACPI_VERSION_2_0},
    Acpi,
    AcpiError,
    AcpiHandler,
    AmlTable,
    GenericAddress,
    PhysicalMapping,
};

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
    _reserved: u8,

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
    _reserved2: u8, // must be 0
    flags: u32,
    reset_reg: GenericAddress,
    reset_value: u8,
    arm_boot_arch: u16,
    fadt_minor_version: u8,
    x_firmware_ctrl: ExtendedField<u64, ACPI_VERSION_2_0>,
    x_dsdt_address: ExtendedField<u64, ACPI_VERSION_2_0>,
    x_pm1a_event_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_pm1b_event_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_pm1a_control_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_pm1b_control_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_pm2_control_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_pm_timer_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_gpe0_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    x_gpe1_block: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    sleep_control_reg: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    sleep_status_reg: ExtendedField<GenericAddress, ACPI_VERSION_2_0>,
    hypervisor_vendor_id: ExtendedField<u64, ACPI_VERSION_2_0>,
}

pub(crate) fn parse_fadt<H>(
    acpi: &mut Acpi,
    handler: &mut H,
    mapping: &PhysicalMapping<Fadt>,
) -> Result<(), AcpiError>
where
    H: AcpiHandler,
{
    let fadt = &*mapping;
    fadt.header.validate(crate::sdt::Signature::FADT)?;

    let dsdt_address = unsafe {
        fadt.x_dsdt_address
            .access(fadt.header.revision)
            .filter(|&p| p != 0)
            .or(Some(fadt.dsdt_address as u64))
            .filter(|p| *p != 0)
            .map(|p| p as usize)
    };

    acpi.dsdt = dsdt_address.map(|address| {
        let dsdt_header = crate::sdt::peek_at_sdt_header(handler, address);
        AmlTable::new(address, dsdt_header.length)
    });

    Ok(())
}
