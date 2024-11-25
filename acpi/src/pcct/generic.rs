use super::{PccShmemHdr, PccSubspaceHeader, RawGenericAddress};
use bitflags::bitflags;

/// Generic PCC communication subspace.
///
/// See section "14.1.3. Generic Communications Subspace Structure
/// (type 0)" of the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct PccGenericSubspace {
    /// PCC subspace header.
    pub header: PccSubspaceHeader,
    _rsvd: [u8; 6],
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

/// Shared memory region header for [`PccGenericSubspace`].
///
/// See section "14.2. Generic Communications Channel Shared Memory
/// Region" of the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccGenericShmem {
    /// The PCC signature. The signature of a subspace is computed b
    /// a bitwise-or of the value `0x50434300` with the subspace ID.
    /// For example, subspace 3 has the signature `0x50434303`.
    pub signature: u32,
    /// Commands for the platform to perform.
    pub cmd: PccGenericShmemCmd,
    /// Command processing status.
    pub status: PccGenericShmemStatus,
}

impl PccShmemHdr for PccGenericShmem {
    fn signature(&self) -> u32 {
        self.signature
    }
}

/// Command field for [`PccGenericShmem`].
///
/// For [`PccGenericSubspace`],
/// [`PccHwReducedSubspace1`](super::PccHwReducedSubspace1) and
/// [`PccHwReducedSubspace2`](super::PccHwReducedSubspace2), this
/// 16-bit field is used to select one of the defined commands for
/// the platform to perform. OSPM is esponsible for populating this
/// field before each command invocation.
///
/// See section "14.2.1. Generic Communications Channel Command
/// Field" of the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccGenericShmemCmd {
    /// Command code to execute. Command codes are application
    /// specific and defined by the consumer of this interface.
    pub cmd: u8,
    /// Additional bitfields for the command field.
    pub flags: PccShmemCmdFlags,
}

bitflags! {
    /// Flags for [`PccGenericShmemCmd`].
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct PccShmemCmdFlags: u8 {
        /// If set, the platform should generate a Doorbell interrupt
        /// at the completion of this command. The interrupt is an
        /// SCI for a [`PccGenericSubspace`], or as described by
        /// the [Doorbell Interrupt field](PccHwReducedSubspace1::plat_interrupt)
        /// for [`PccHwReducedSubspace1`] and
        /// [`PccHwReducedSubspace2`]. If the
        /// [Doorbell bit](PcctFlags::PLATFORM_INTERRUPT) is not set
        /// in the [PCC global flags](Pcct::flags), this bit must be
        /// cleared.
        const NOTIFY_ON_COMPLETION = 1 << 7;
    }
}

bitflags! {
    /// Status flags for [`PccGenericShmem`].
    ///
    /// See section "14.2.2. Generic Communications Channel Status
    /// Field" of the ACPI spec for more information.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct PccGenericShmemStatus: u16 {
        /// If set, the platform has completed processing the last
        /// command.
        const CMD_COMPLETE = 0b1;
        /// If set, the platform has issued a Platform Interrupt to
        /// this subspace. OSPM must check the
        /// [`CMD_COMPLETE`](Self::CMD_COMPLETE) and
        /// [`PLATFORM_NOTIFICATION`](Self::PLATFORM_NOTIFICATION)
        /// fields to determine the cause of the Interrupt.
        const PLATFORM_INTERRUPT = 1 << 1;
        /// If set, an error occurred executing the last command.
        const ERROR = 1 << 2;
        /// If set, indicates the platform is issuing an asynchronous
        /// notification to OSPM.
        const PLATFORM_NOTIFICATION = 1 << 3;
    }
}
#[cfg(test)]
mod test {
    use super::*;
    use core::mem::offset_of;

    #[test]
    fn pcc_generic_subspace_offsets() {
        assert_eq!(offset_of!(PccGenericSubspace, base_address), 8);
        assert_eq!(offset_of!(PccGenericSubspace, mem_len), 16);
        assert_eq!(offset_of!(PccGenericSubspace, doorbell_reg), 24);
        assert_eq!(offset_of!(PccGenericSubspace, doorbell_preserve), 36);
        assert_eq!(offset_of!(PccGenericSubspace, doorbell_write), 44);
        assert_eq!(offset_of!(PccGenericSubspace, nominal_latency), 52);
        assert_eq!(offset_of!(PccGenericSubspace, max_periodic_access_rate), 56);
        assert_eq!(offset_of!(PccGenericSubspace, min_request_turnaround_time), 60);
    }
}
