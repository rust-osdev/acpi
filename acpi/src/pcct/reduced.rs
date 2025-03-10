use super::{PccPlatformInterruptAck, PccSubspaceHeader, RawGenericAddress};
use crate::{address::GenericAddress, AcpiResult};
use bitflags::bitflags;

/// HW-reduced PCC communications subspace (type 1).
///
/// See section "14.1.4. HW-Reduced Communications Subspace Structure
/// (type 1)" of the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct PccHwReducedSubspace1 {
    /// PCC subspace header.
    pub header: PccSubspaceHeader,
    /// GSIV of the interrupt used for the PCC platform interrupt for
    /// this subspace.
    pub plat_interrupt: u32,
    /// Flags for the platform interrupt for this subspace.
    pub plat_interrupt_flags: PccPlatformInterruptFlags,
    _rsvd: u8,
    pub(super) base_address: u64,
    pub(super) mem_len: u64,
    pub(super) doorbell_reg: RawGenericAddress,
    pub(super) doorbell_preserve: u64,
    pub(super) doorbell_write: u64,
    /// Expected latency to process a command, in microseconds.
    pub nominal_latency: u32,
    /// The maximum number of periodic requests that the subspace
    /// channel can support, reported in commands per minute. 0
    /// indicates no limitation.
    pub max_periodic_access_rate: u32,
    /// The minimum amount of time that OSPM must wait after
    /// the completion of a command before issuing the next command,
    /// in microseconds.
    pub min_request_turnaround_time: u16,
}

/// HW-reduced PCC communications subspace (type 2).
///
/// See section "14.1.5. HW-Reduced Communications Subspace Structure
/// (type 2)" of the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct PccHwReducedSubspace2 {
    /// PCC subspace header.
    pub header: PccSubspaceHeader,
    /// GSIV of the interrupt used for the PCC platform interrupt for
    /// this subspace.
    pub plat_interrupt: u32,
    /// Flags for the platform interrupt for this subspace.
    pub plat_interrupt_flags: PccPlatformInterruptFlags,
    _rsvd: u8,
    pub(super) base_address: u64,
    pub(super) mem_len: u64,
    pub(super) doorbell_reg: RawGenericAddress,
    pub(super) doorbell_preserve: u64,
    pub(super) doorbell_write: u64,
    /// Expected latency to process a command, in microseconds.
    pub nominal_latency: u32,
    /// The maximum number of periodic requests that the subspace
    /// channel can support, reported in commands per minute. 0
    /// indicates no limitation.
    pub max_periodic_access_rate: u32,
    /// The minimum amount of time that OSPM must wait after
    /// the completion of a command before issuing the next command,
    /// in microseconds.
    pub min_request_turnaround_time: u16,
    pub(super) plat_interrupt_ack_reg: RawGenericAddress,
    pub(super) plat_interrupt_ack_preserve: u64,
    pub(super) plat_interrupt_ack_write: u64,
}

impl PccHwReducedSubspace2 {
    /// Get information about the platform interrupt ACK mechanism.
    pub fn platform_interrupt_ack(&self) -> AcpiResult<PccPlatformInterruptAck> {
        let addr = GenericAddress::from_raw(self.plat_interrupt_ack_reg)?;
        Ok(PccPlatformInterruptAck {
            addr,
            preserve: self.plat_interrupt_ack_preserve,
            write: self.plat_interrupt_ack_write,
        })
    }
}

bitflags! {
    /// Interrupt flags for [`PccHwReducedSubspace1`] and
    /// [`PccHwReducedSubspace2`].
    #[derive(Clone, Copy, Debug, Eq, PartialEq)]
    pub struct PccPlatformInterruptFlags: u8 {
        /// * 1: Interrupt is Active low.
        /// * 0: Interrupt is Active high.
        const INTERRUPT_POLARITY = 1 << 0;
        /// * 1: Interrupt is Edge triggered.
        /// * 0: Interrupt is Level triggered.
        const INTERRUPT_MODE = 1 << 1;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use core::mem::offset_of;

    #[test]
    fn pcc_hw_reduced_subspace_1_offsets() {
        assert_eq!(offset_of!(PccHwReducedSubspace1, plat_interrupt), 2);
        assert_eq!(offset_of!(PccHwReducedSubspace1, plat_interrupt_flags), 6);
        assert_eq!(offset_of!(PccHwReducedSubspace1, base_address), 8);
        assert_eq!(offset_of!(PccHwReducedSubspace1, mem_len), 16);
        assert_eq!(offset_of!(PccHwReducedSubspace1, doorbell_reg), 24);
        assert_eq!(offset_of!(PccHwReducedSubspace1, doorbell_preserve), 36);
        assert_eq!(offset_of!(PccHwReducedSubspace1, doorbell_write), 44);
        assert_eq!(offset_of!(PccHwReducedSubspace1, nominal_latency), 52);
        assert_eq!(offset_of!(PccHwReducedSubspace1, max_periodic_access_rate), 56);
        assert_eq!(offset_of!(PccHwReducedSubspace1, min_request_turnaround_time), 60);
    }

    #[test]
    fn pcc_hw_reduced_subspace_2_offsets() {
        assert_eq!(offset_of!(PccHwReducedSubspace2, plat_interrupt), 2);
        assert_eq!(offset_of!(PccHwReducedSubspace2, plat_interrupt_flags), 6);
        assert_eq!(offset_of!(PccHwReducedSubspace2, base_address), 8);
        assert_eq!(offset_of!(PccHwReducedSubspace2, mem_len), 16);
        assert_eq!(offset_of!(PccHwReducedSubspace2, doorbell_reg), 24);
        assert_eq!(offset_of!(PccHwReducedSubspace2, doorbell_preserve), 36);
        assert_eq!(offset_of!(PccHwReducedSubspace2, doorbell_write), 44);
        assert_eq!(offset_of!(PccHwReducedSubspace2, nominal_latency), 52);
        assert_eq!(offset_of!(PccHwReducedSubspace2, max_periodic_access_rate), 56);
        assert_eq!(offset_of!(PccHwReducedSubspace2, min_request_turnaround_time), 60);
        assert_eq!(offset_of!(PccHwReducedSubspace2, plat_interrupt_ack_reg), 62);
        assert_eq!(offset_of!(PccHwReducedSubspace2, plat_interrupt_ack_preserve), 74);
        assert_eq!(offset_of!(PccHwReducedSubspace2, plat_interrupt_ack_write), 82);
    }
}
