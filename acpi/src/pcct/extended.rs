use super::{PccCmdCompleteCheck, PccPlatformInterruptFlags, PccShmemHdr, PccSubspaceHeader, RawGenericAddress};
use crate::{address::GenericAddress, AcpiResult};
use bitflags::bitflags;

/// Extended PCC communication subspace.
///
/// The subspace might be a master or slave subspace, depending on
/// its type (3 and 4 respectively). The type can be inspected in the
/// subspace header or via this type's methods
/// ([`is_master()`](Self::is_master) and [`is_slave()`](Self::is_slave)).
///
/// * Master subspaces are used by the OSPM to communicate with the
///   platform.
/// * Slave subspaces are used by the platform to send asynchronous
///   notifications to the OSPM.
///
/// See section "14.1.6. Extended PCC subspaces (types 3 and 4)" of
/// the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct PccExtendedSubspace {
    /// PCC subspace header.
    pub header: PccSubspaceHeader,
    /// GSIV of the interrupt used for the PCC platform interrupt for
    /// this subspace.
    pub plat_interrupt: u32,
    /// Flags for the platform interrupt for this subspace.
    pub plat_interrupt_flags: PccPlatformInterruptFlags,
    _rsvd: u8,
    pub(super) base_address: u64,
    pub(super) mem_len: u32,
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
    pub min_request_turnaround_time: u32,
    plat_interrupt_ack_reg: RawGenericAddress,
    plat_interrupt_ack_preserve: u64,
    plat_interrupt_ack_write: u64,
    _rsvd2: [u8; 8],
    cmd_complete_check_reg: RawGenericAddress,
    cmd_complete_check_mask: u64,
    cmd_complete_update_reg: RawGenericAddress,
    cmd_complete_update_preserve: u64,
    cmd_complete_update_write: u64,
    err_status_reg: RawGenericAddress,
    err_status_mask: u64,
}

impl PccExtendedSubspace {
    /// Returns `true` if this is a master subspace.
    pub fn is_master(&self) -> bool {
        self.header.stype == 3
    }

    /// Returns `true` if this is a slave subspace.
    pub fn is_slave(&self) -> bool {
        self.header.stype == 4
    }

    /// Get information about the platform interrupt ACK mechanism.
    pub fn platform_interrupt_ack(&self) -> AcpiResult<PccPlatformInterruptAck> {
        let addr = GenericAddress::from_raw(self.plat_interrupt_ack_reg)?;
        Ok(PccPlatformInterruptAck {
            addr,
            preserve: self.plat_interrupt_ack_preserve,
            write: self.plat_interrupt_ack_write,
        })
    }

    /// Get information about the command complete check register.
    pub fn cmd_complete_check(&self) -> AcpiResult<PccCmdCompleteCheck> {
        let addr = GenericAddress::from_raw(self.cmd_complete_check_reg)?;
        Ok(PccCmdCompleteCheck { addr, mask: self.cmd_complete_check_mask })
    }

    /// Get information about the command complete update register.
    pub fn cmd_complete_update(&self) -> AcpiResult<PccCmdCompleteUpdate> {
        let addr = GenericAddress::from_raw(self.cmd_complete_update_reg)?;
        Ok(PccCmdCompleteUpdate {
            addr,
            preserve: self.cmd_complete_update_preserve,
            write: self.cmd_complete_update_write,
        })
    }

    /// Get information about the error status register.
    pub fn error_status(&self) -> AcpiResult<PccErrorStatus> {
        let addr = GenericAddress::from_raw(self.err_status_reg)?;
        Ok(PccErrorStatus { addr, mask: self.err_status_mask })
    }
}

/// Information about the command complete update register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccCmdCompleteUpdate {
    /// Location of the register.
    pub addr: GenericAddress,
    /// Mask of bits to preserve in the command complete update
    /// register, when updating command complete in this subspace.
    pub preserve: u64,
    /// Mask of bits to set in the command complete update register,
    /// when updating command complete in this subspace. For master
    /// subspaces the mask must indicate how to clear the command
    /// complete bit. For slave subspaces, the mask must indicate how
    /// to set the command complete bit.
    pub write: u64,
}

/// Platform interrupt ACK information.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccPlatformInterruptAck {
    /// Location of the register.
    pub addr: GenericAddress,
    /// Bits to preserve when writing the register.
    pub preserve: u64,
    /// Bits to set when writing the register.
    pub write: u64,
}

/// Information about the error status register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccErrorStatus {
    /// Location of the register.
    pub addr: GenericAddress,
    /// The mask contained here can be combined through a logical AND
    /// with content of the Error status register to ascertain whether
    /// an error occurred in the transmission of the command through
    /// the subspace. The logical NOT of this mask is be used to clear
    /// the error. The inverted mask is combined through a logical AND
    /// with the content of the Error status register, and the result
    /// is written back into said register. This field is ignored for
    /// slave channels.
    pub mask: u64,
}

/// Shared memory region header for [`PccExtendedSubspace`].
///
/// See section "14.3. Extended PCC Subspace Shared Memory Region" of
/// the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccExtendedShmem {
    /// The PCC signature. The signature of a subspace is computed by
    /// a bitwise-or of the value `0x50434300` with the subspace ID.
    /// For example, subspace 3 has the signature `0x50434303`.
    pub signature: u32,
    /// Flags for doorbell behavior on this shared region.
    pub flags: PccExtendedShmemFlags,
    /// Length of payload being transmitted including command field.
    pub len: u32,
    /// Command being sent over the subspace.
    pub cmd: u32,
}

impl PccShmemHdr for PccExtendedShmem {
    fn signature(&self) -> u32 {
        self.signature
    }
}

bitflags! {
    /// Flags for [`PccExtendedShmem`].
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct PccExtendedShmemFlags: u32 {
        /// **For master subspaces** this field indicates to the
        /// platform that it must generate an interrupt when the
        /// command has completed. - Setting this bit to 1 when
        /// sending a command, requests that completion of the command
        /// is signaled via the platform interrupt. - Setting it to 0
        /// when sending a command, requests that no interrupt is
        /// asserted when the command is completed.
        ///
        /// **For slave subspaces**, if the doorbell field of the
        /// slave subspace is non zero, and this flag is set, the
        /// OSPM must access the doorbell once it has processed the
        /// notification. This bit is ignored by the platform if the
        /// [Platform Interrupt field](PcctFlags::INTERRUPT) of the
        /// [PCC flags](Pcct::flags) is set to zero.
        const NOTIFY_ON_COMPLETION = 1;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use core::mem::offset_of;

    #[test]
    fn pcc_extended_subspace_offsets() {
        assert_eq!(offset_of!(PccExtendedSubspace, plat_interrupt), 2);
        assert_eq!(offset_of!(PccExtendedSubspace, plat_interrupt_flags), 6);
        assert_eq!(offset_of!(PccExtendedSubspace, base_address), 8);
        assert_eq!(offset_of!(PccExtendedSubspace, mem_len), 16);
        assert_eq!(offset_of!(PccExtendedSubspace, doorbell_reg), 20);
        assert_eq!(offset_of!(PccExtendedSubspace, doorbell_preserve), 32);
        assert_eq!(offset_of!(PccExtendedSubspace, doorbell_write), 40);
        assert_eq!(offset_of!(PccExtendedSubspace, nominal_latency), 48);
        assert_eq!(offset_of!(PccExtendedSubspace, max_periodic_access_rate), 52);
        assert_eq!(offset_of!(PccExtendedSubspace, min_request_turnaround_time), 56);
        assert_eq!(offset_of!(PccExtendedSubspace, plat_interrupt_ack_reg), 60);
        assert_eq!(offset_of!(PccExtendedSubspace, plat_interrupt_ack_preserve), 72);
        assert_eq!(offset_of!(PccExtendedSubspace, plat_interrupt_ack_write), 80);
        assert_eq!(offset_of!(PccExtendedSubspace, cmd_complete_check_reg), 96);
        assert_eq!(offset_of!(PccExtendedSubspace, cmd_complete_check_mask), 108);
        assert_eq!(offset_of!(PccExtendedSubspace, cmd_complete_update_reg), 116);
        assert_eq!(offset_of!(PccExtendedSubspace, cmd_complete_update_preserve), 128);
        assert_eq!(offset_of!(PccExtendedSubspace, cmd_complete_update_write), 136);
        assert_eq!(offset_of!(PccExtendedSubspace, err_status_reg), 144);
        assert_eq!(offset_of!(PccExtendedSubspace, err_status_mask), 156);
    }
}
