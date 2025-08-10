use crate::{AcpiError, Handler, address::MappedGas, sdt::fadt::Fadt};
use bit_field::BitField;

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
    pm1_event_length: usize,
    pm1a: MappedGas<H>,
    pm1b: Option<MappedGas<H>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum Pm1Event {
    Timer = 0,
    GlobalLock = 5,
    PowerButton = 8,
    SleepButton = 9,
    Rtc = 10,
    PciEWake = 14,
    Wake = 15,
}

impl<H> Pm1EventRegisterBlock<H>
where
    H: Handler,
{
    pub fn set_event_enabled(&self, event: Pm1Event, enabled: bool) -> Result<(), AcpiError> {
        let enable_offset = self.pm1_event_length * 8 / 2;
        let event_bit = match event {
            Pm1Event::Timer => 0,
            Pm1Event::GlobalLock => 5,
            Pm1Event::PowerButton => 8,
            Pm1Event::SleepButton => 9,
            Pm1Event::Rtc => 10,
            Pm1Event::PciEWake => 14,
            Pm1Event::Wake => 15,
        };

        let mut pm1a = self.pm1a.read()?;
        pm1a.set_bit(enable_offset + event_bit, enabled);
        self.pm1a.write(pm1a)?;

        if let Some(pm1b) = &self.pm1b {
            let mut value = pm1b.read()?;
            value.set_bit(enable_offset + event_bit, enabled);
            pm1b.write(value)?;
        }

        Ok(())
    }

    pub fn read(&self) -> Result<u64, AcpiError> {
        let pm1_len = self.pm1_event_length * 8;

        let pm1a = self.pm1a.read()?.get_bits(0..pm1_len);
        let pm1b = if let Some(pm1b) = &self.pm1b { pm1b.read()?.get_bits(0..pm1_len) } else { 0 };

        Ok(pm1a | pm1b)
    }
}

pub struct Pm1ControlRegisterBlock<H: Handler> {
    pm1a: MappedGas<H>,
    pm1b: Option<MappedGas<H>>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Pm1ControlBit {
    /// Selects whether the power management event produces an SCI or SMI interrupt. This is
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
