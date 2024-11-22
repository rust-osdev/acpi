use crate::{
    address::{GenericAddress, RawGenericAddress},
    AcpiResult,
    AcpiTable,
    SdtHeader,
    Signature,
};
use core::{
    num::{NonZeroU32, NonZeroU8},
    ptr,
    slice,
    str::{self, Utf8Error},
};

/// Serial Port Console Redirection (SPCR) Table.
///
/// The table provides information about the configuration and use of the
/// serial port or non-legacy UART interface. On a system where the BIOS or
/// system firmware uses the serial port for console input/output, this table
/// should be used to convey information about the settings.
///
/// For more information, see [the official documentation](https://learn.microsoft.com/en-us/windows-hardware/drivers/serports/serial-port-console-redirection-table).
#[repr(C, packed)]
#[derive(Debug)]
pub struct Spcr {
    pub header: SdtHeader,
    interface_type: u8,
    _reserved: [u8; 3],
    base_address: RawGenericAddress,
    interrupt_type: u8,
    irq: u8,
    global_system_interrupt: u32,
    /// The baud rate the BIOS used for redirection.
    configured_baud_rate: u8,
    pub parity: u8,
    pub stop_bits: u8,
    flow_control: u8,
    terminal_type: u8,
    /// Language which the BIOS was redirecting. Must be 0.
    pub language: u8,
    pci_device_id: u16,
    pci_vendor_id: u16,
    pci_bus_number: u8,
    pci_device_number: u8,
    pci_function_number: u8,
    pub pci_flags: u32,
    /// PCI segment number. systems with fewer than 255 PCI buses, this number
    /// will be 0.
    pub pci_segment: u8,
    uart_clock_freq: u32,
    precise_baud_rate: u32,
    namespace_string_length: u16,
    namespace_string_offset: u16,
}

unsafe impl AcpiTable for Spcr {
    const SIGNATURE: Signature = Signature::SPCR;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Spcr {
    /// Gets the type of the register interface.
    pub fn interface_type(&self) -> SpcrInterfaceType {
        SpcrInterfaceType::from(self.interface_type)
    }

    /// The base address of the Serial Port register set, if if console
    /// redirection is enabled.
    pub fn base_address(&self) -> Option<AcpiResult<GenericAddress>> {
        (!self.base_address.is_empty()).then(|| GenericAddress::from_raw(self.base_address))
    }

    fn configured_baud_rate(&self) -> Option<NonZeroU32> {
        match self.configured_baud_rate {
            3 => unsafe { Some(NonZeroU32::new_unchecked(9600)) },
            4 => unsafe { Some(NonZeroU32::new_unchecked(19200)) },
            6 => unsafe { Some(NonZeroU32::new_unchecked(57600)) },
            7 => unsafe { Some(NonZeroU32::new_unchecked(115200)) },
            _ => None,
        }
    }

    /// The baud rate the BIOS used for redirection, if configured.
    pub fn baud_rate(&self) -> Option<NonZeroU32> {
        NonZeroU32::new(self.precise_baud_rate).or_else(|| self.configured_baud_rate())
    }

    /// Flow control flags for the UART.
    pub fn flow_control(&self) -> SpcrFlowControl {
        SpcrFlowControl::from_bits_truncate(self.flow_control)
    }

    /// Interrupt type(s) used by the UART.
    pub fn interrupt_type(&self) -> SpcrInterruptType {
        SpcrInterruptType::from_bits_truncate(self.interrupt_type)
    }

    /// The PC-AT-compatible IRQ used by the UART, if the UART supports it.
    /// Support is indicated by the [`interrupt_type`](Self::interrupt_type).
    pub fn irq(&self) -> Option<u8> {
        self.interrupt_type().contains(SpcrInterruptType::DUAL_8259).then_some(self.irq)
    }

    /// The Global System Interrupt (GSIV) used by the UART, if the UART
    /// supports it. Support is indicated by the
    /// [`interrupt_type`](Self::interrupt_type).
    pub fn global_system_interrupt(&self) -> Option<u32> {
        if self.interrupt_type().difference(SpcrInterruptType::DUAL_8259).is_empty() {
            return None;
        }
        Some(self.global_system_interrupt)
    }

    /// The terminal protocol the BIOS was using for console redirection.
    pub fn terminal_type(&self) -> SpcrTerminalType {
        SpcrTerminalType::from_bits_truncate(self.terminal_type)
    }

    /// If the UART is a PCI device, returns its Device ID.
    pub fn pci_device_id(&self) -> Option<u16> {
        (self.pci_device_id != 0xffff).then_some(self.pci_device_id)
    }

    /// If the UART is a PCI device, returns its Vendor ID.
    pub fn pci_vendor_id(&self) -> Option<u16> {
        (self.pci_vendor_id != 0xffff).then_some(self.pci_vendor_id)
    }

    /// If the UART is a PCI device, returns its bus number.
    pub fn pci_bus_number(&self) -> Option<NonZeroU8> {
        NonZeroU8::new(self.pci_bus_number)
    }

    /// If the UART is a PCI device, returns its device number.
    pub fn pci_device_number(&self) -> Option<NonZeroU8> {
        NonZeroU8::new(self.pci_device_number)
    }

    /// If the UART is a PCI device, returns its function number.
    pub fn pci_function_number(&self) -> Option<NonZeroU8> {
        NonZeroU8::new(self.pci_function_number)
    }

    /// The UART clock frequency in Hz, if it can be determined.
    pub const fn uart_clock_frequency(&self) -> Option<NonZeroU32> {
        if self.header.revision <= 2 {
            return None;
        }
        NonZeroU32::new(self.uart_clock_freq)
    }

    /// An ASCII string to uniquely identify this device. This string consists
    /// of a fully qualified reference to the object that represents this
    /// device in the ACPI namespace. If no namespace device exists,
    /// the namespace string must only contain a single '.'.
    pub fn namespace_string(&self) -> Result<&str, Utf8Error> {
        let start = ptr::from_ref(self).cast::<u8>();
        let bytes = unsafe {
            let str_start = start.add(self.namespace_string_offset as usize);
            slice::from_raw_parts(str_start, self.namespace_string_length as usize)
        };
        str::from_utf8(bytes)
    }
}

bitflags::bitflags! {
    /// Interrupt type(s) used by an UART.
    #[derive(Clone, Copy, Debug)]
    pub struct SpcrInterruptType: u8 {
        /// PC-AT-compatible dual-8259 IRQ interrupt.
        const DUAL_8259 = 1 << 0;
        /// I/O APIC interrupt (Global System Interrupt).
        const IO_APIC = 1 << 1;
        /// I/O SAPIC interrupt (Global System Interrupt).
        const IO_SAPIC = 1 << 2;
        /// ARMH GIC interrupt (Global System Interrupt).
        const ARMH_GIC = 1 << 3;
        /// RISC-V PLIC/APLIC interrupt (Global System Interrupt).
        const RISCV_PLIC = 1 << 4;
    }
}

bitflags::bitflags! {
    /// The terminal protocol the BIOS uses for console redirection.
    #[derive(Clone, Copy, Debug)]
    pub struct SpcrTerminalType: u8 {
        const VT1000 = 1 << 0;
        const EXTENDED_VT1000 = 1 << 1;
        const VT_UTF8 = 1 << 2;
        const ANSI = 1 << 3;
    }
}

bitflags::bitflags! {
    /// Flow control flags for the UART.
    #[derive(Clone, Copy, Debug)]
    pub struct SpcrFlowControl: u8 {
        /// DCD required for transmit
        const DCD = 1 << 0;
        /// RTS/CTS hardware flow control
        const RTS_CTS = 1 << 1;
        /// XON/XOFF software control
        const XON_XOFF = 1 << 2;
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum SpcrInterfaceType {
    /// Full 16550 interface
    Full16550,
    /// Full 16450 interface (must also accept writing to the 16550 FCR register).
    Full16450,
    /// MAX311xE SPI UART
    MAX311xE,
    /// Arm PL011 UART
    ArmPL011,
    /// MSM8x60 (e.g. 8960)
    MSM8x60,
    /// Nvidia 16550
    Nvidia16550,
    /// TI OMAP
    TiOmap,
    /// APM88xxxx
    APM88xxxx,
    /// MSM8974
    Msm8974,
    /// SAM5250
    Sam5250,
    /// Intel USIF
    IntelUSIF,
    /// i.MX 6
    Imx6,
    /// (deprecated) Arm SBSA (2.x only) Generic UART supporting only 32-bit accesses
    ArmSBSAGeneric32bit,
    /// Arm SBSA Generic UART
    ArmSBSAGeneric,
    /// Arm DCC
    ArmDCC,
    /// VCM2835
    Bcm2835,
    /// SDM845 with clock rate of 1.8432 MHz
    Sdm845_18432,
    /// 16550-compatible with parameters defined in Generic Address Structure
    Generic16550,
    /// SDM845 with clock rate of 7.372 MHz
    Sdm845_7372,
    /// Intel LPSS
    IntelLPSS,
    /// RISC-V SBI console (any supported SBI mechanism)
    RiscVSbi,
    /// Unknown interface
    Unknown(u8),
}

impl From<u8> for SpcrInterfaceType {
    fn from(val: u8) -> Self {
        match val {
            0x00 => Self::Full16550,
            0x01 => Self::Full16450,
            0x02 => Self::MAX311xE,
            0x03 => Self::ArmPL011,
            0x04 => Self::MSM8x60,
            0x05 => Self::Nvidia16550,
            0x06 => Self::TiOmap,
            0x08 => Self::APM88xxxx,
            0x09 => Self::Msm8974,
            0x0A => Self::Sam5250,
            0x0B => Self::IntelUSIF,
            0x0C => Self::Imx6,
            0x0D => Self::ArmSBSAGeneric32bit,
            0x0E => Self::ArmSBSAGeneric,
            0x0F => Self::ArmDCC,
            0x10 => Self::Bcm2835,
            0x11 => Self::Sdm845_18432,
            0x12 => Self::Generic16550,
            0x13 => Self::Sdm845_7372,
            0x14 => Self::IntelLPSS,
            0x15 => Self::RiscVSbi,
            _ => Self::Unknown(val),
        }
    }
}
