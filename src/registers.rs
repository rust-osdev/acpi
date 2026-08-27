use crate::{AcpiError, Handler, address::MappedGas, sdt::fadt::Fadt};
use bit_field::BitField;
use bitflags::{Flags, bitflags};

pub struct FixedRegisters<H: Handler> {
    pub pm1_event_registers: Pm1EventRegisterBlock<H>,
    pub pm1_control_registers: Pm1ControlRegisterBlock<H>,
}

impl<H> FixedRegisters<H>
where
    H: Handler,
{
    pub fn new(fadt: &Fadt, handler: H) -> Result<FixedRegisters<H>, AcpiError> {
        let pm1_event_registers = {
            let pm1a = unsafe { MappedGas::map_gas(fadt.pm1a_event_block()?, &handler)? };
            let pm1b = match fadt.pm1b_event_block()? {
                Some(gas) => Some(unsafe { MappedGas::map_gas(gas, &handler)? }),
                None => None,
            };
            Pm1EventRegisterBlock { pm1_event_length: fadt.pm1_event_length as usize, pm1a, pm1b }
        };
        let pm1_control_registers = {
            let pm1a = unsafe { MappedGas::map_gas(fadt.pm1a_control_block()?, &handler)? };
            let pm1b = match fadt.pm1b_control_block()? {
                Some(gas) => Some(unsafe { MappedGas::map_gas(gas, &handler)? }),
                None => None,
            };
            Pm1ControlRegisterBlock { pm1a, pm1b }
        };

        Ok(FixedRegisters { pm1_event_registers, pm1_control_registers })
    }
}

/// The PM1 register grouping contains two register blocks that control fixed events. It is split
/// into two to allow its functionality to be split between two hardware components. `PM1a` and
/// `PM1b` are effectively mirrors of each other - reads are made from both of them and logically
/// ORed, and writes are made to both of them.
///
/// The register grouping contains two registers - a `STS` status register that can be read to
/// determine if an event has fired (and written to clear), and an `EN` enabling register to
/// control whether an event should fire.
pub struct Pm1EventRegisterBlock<H: Handler> {
    pub pm1_event_length: usize,
    pub pm1a: MappedGas<H>,
    pub pm1b: Option<MappedGas<H>>,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Pm1EventFlags: u16 {
        const TIMER = 1 << 0;
        const GLOBAL_ENABLE = 1 << 5;
        const POWER_BUTTON = 1 << 8;
        const SLEEP_BUTTON = 1 << 9;
        const RTC = 1 << 10;
        const PCIE_WAKE = 1 << 14;
        const WAKE = 1 << 15;
    }
}

impl<H> Pm1EventRegisterBlock<H>
where
    H: Handler,
{
    /// Sets the specified flags and unsets the other flags.
    /// When an event happens, the corresponding status bit is always set.
    /// These flags control whether an *interrupt* is triggered as a result of that status bit being set.
    /// Note that global enable (GBL_EN) must be set for any SCI interrupts to be fired.
    /// For most flags you set to enable and clear to disable.
    /// But for PCIe wake the flag is set to disable and clear to enable.
    pub fn set_enable_flags(&self, events: Pm1EventFlags) {
        let enable_offset = self.pm1_event_length as u64 / 2;
        let bits = events.known_bits();
        self.pm1a.write_u16(enable_offset, bits);
        if let Some(pm1b) = &self.pm1b {
            pm1b.write_u16(enable_offset, bits);
        }
    }

    pub fn pending_events(&self) -> Pm1EventFlags {
        let bits = {
            let mut bits = self.pm1a.read_u16(0);
            if let Some(pm1b) = &self.pm1b {
                bits |= pm1b.read_u16(0);
            }
            bits
        };
        Pm1EventFlags::from_bits_retain(bits)
    }

    pub fn clear_events(&self, events: Pm1EventFlags) {
        let bits = events.known_bits();
        self.pm1a.write_u16(0, bits);
        if let Some(pm1b) = &self.pm1b {
            pm1b.write_u16(0, bits);
        }
    }
}

pub struct Pm1ControlRegisterBlock<H: Handler> {
    pub pm1a: MappedGas<H>,
    pub pm1b: Option<MappedGas<H>>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Pm1ControlBit {
    /// Determines whether the power management event produces an SCI or SMI interrupt. This is
    /// controlled by the firmware - OSPM should always preserve this bit.
    SciEnable = 0,
    /// When this bit is set, bus master requests can cause any processor in the C3 state to
    /// transistion to C0.
    BusMasterWake = 1,
    /// A write to this bit generates an SMI, passing control to the platform runtime firmware. It
    /// should be written when the global lock is released and the pending bit in the FACS is set.
    GlobalLockRelease = 2,
    /*
     * Bits 3..10 are reserved. Bits 10..13 are SLP_TYPx - this field is set separately and
     * contains the desired hardware sleep state the system enters when `SleepEnable` is set.
     */
    SleepEnable = 13,
}

impl<H> Pm1ControlRegisterBlock<H>
where
    H: Handler,
{
    pub fn read_bit(&self, bit: Pm1ControlBit) -> Result<bool, AcpiError> {
        let control_bit = match bit {
            Pm1ControlBit::SciEnable => 0,
            Pm1ControlBit::BusMasterWake => 1,
            Pm1ControlBit::GlobalLockRelease => 2,
            Pm1ControlBit::SleepEnable => 13,
        };

        let pm1a = self.pm1a.read()?;
        let pm1b = if let Some(ref pm1b) = self.pm1b { pm1b.read()? } else { 0 };
        let pm1 = pm1a | pm1b;
        Ok(pm1.get_bit(control_bit))
    }

    pub fn set_bit(&self, bit: Pm1ControlBit, set: bool) -> Result<(), AcpiError> {
        let control_bit = match bit {
            Pm1ControlBit::SciEnable => 0,
            Pm1ControlBit::BusMasterWake => 1,
            Pm1ControlBit::GlobalLockRelease => 2,
            Pm1ControlBit::SleepEnable => 13,
        };

        let mut pm1a = self.pm1a.read()?;
        pm1a.set_bit(control_bit, set);
        self.pm1a.write(pm1a)?;

        if let Some(pm1b) = &self.pm1b {
            let mut value = pm1b.read()?;
            value.set_bit(control_bit, set);
            pm1b.write(value)?;
        }

        Ok(())
    }

    pub fn set_sleep_typ(&self, value: u8) -> Result<(), AcpiError> {
        let mut pm1a = self.pm1a.read()?;
        pm1a.set_bits(10..13, value as u64);
        self.pm1a.write(pm1a)?;

        if let Some(pm1b) = &self.pm1b {
            let mut pm1b_value = pm1b.read()?;
            pm1b_value.set_bits(10..13, value as u64);
            pm1b.write(pm1b_value)?;
        }

        Ok(())
    }
}
