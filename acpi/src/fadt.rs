use crate::{
    sdt::{ExtendedField, SdtHeader},
    AcpiError,
    AcpiTable,
    AcpiTables,
    GenericAddress,
};
use bit_field::BitField;
use rsdp::handler::AcpiHandler;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PowerProfile {
    Unspecified,
    Desktop,
    Mobile,
    Workstation,
    EnterpriseServer,
    SohoServer,
    AppliancePc,
    PerformanceServer,
    Tablet,
    Reserved(u8),
}

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
    x_firmware_ctrl: ExtendedField<u64, 2>,
    x_dsdt_address: ExtendedField<u64, 2>,
    x_pm1a_event_block: ExtendedField<GenericAddress, 2>,
    x_pm1b_event_block: ExtendedField<GenericAddress, 2>,
    x_pm1a_control_block: ExtendedField<GenericAddress, 2>,
    x_pm1b_control_block: ExtendedField<GenericAddress, 2>,
    x_pm2_control_block: ExtendedField<GenericAddress, 2>,
    x_pm_timer_block: ExtendedField<GenericAddress, 2>,
    x_gpe0_block: ExtendedField<GenericAddress, 2>,
    x_gpe1_block: ExtendedField<GenericAddress, 2>,
    sleep_control_reg: ExtendedField<GenericAddress, 2>,
    sleep_status_reg: ExtendedField<GenericAddress, 2>,
    hypervisor_vendor_id: ExtendedField<u64, 2>,
}

impl AcpiTable for Fadt {
    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Fadt {
    pub fn validate(&self) -> Result<(), AcpiError> {
        self.header.validate(crate::sdt::Signature::FADT)
    }

    pub fn dsdt_address(&self) -> Result<usize, AcpiError> {
        unsafe {
            self.x_dsdt_address
                .access(self.header.revision)
                .filter(|&p| p != 0)
                .or(Some(self.dsdt_address as u64))
                .filter(|p| *p != 0)
                .map(|p| p as usize)
                .ok_or(AcpiError::InvalidDsdtAddress)
        }
    }

    pub fn power_profile(&self) -> PowerProfile {
        match self.preferred_pm_profile {
            0 => PowerProfile::Unspecified,
            1 => PowerProfile::Desktop,
            2 => PowerProfile::Mobile,
            3 => PowerProfile::Workstation,
            4 => PowerProfile::EnterpriseServer,
            5 => PowerProfile::SohoServer,
            6 => PowerProfile::AppliancePc,
            7 => PowerProfile::PerformanceServer,
            8 => PowerProfile::Tablet,
            other => PowerProfile::Reserved(other),
        }
    }
}

/// Information about the ACPI Power Management Timer (ACPI PM Timer).
pub struct PmTimer {
    /// An I/O space address to the register of ACPI PM Timer.
    pub io_base: u32,
    /// This field is true if the hardware supports 32-bit timer, and false if the hardware
    /// supports 24-bit timer.
    pub supports_32bit: bool,
}
impl PmTimer {
    /// Creates a new instance of `PmTimer`.
    pub fn new<H>(tables: &AcpiTables<H>) -> Result<PmTimer, AcpiError>
    where
        H: AcpiHandler,
    {
        let fadt = unsafe {
            tables
                .get_sdt::<Fadt>(crate::sdt::Signature::FADT)?
                .ok_or(AcpiError::TableMissing(crate::sdt::Signature::FADT))?
        };

        Ok(PmTimer { io_base: fadt.pm_timer_block, supports_32bit: fadt.flags.get_bit(8) })
    }
}
