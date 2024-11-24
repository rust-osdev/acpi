use crate::{address::GenericAddress, AcpiResult};

use super::{PccCmdCompleteCheck, PccErrorStatus, PccShmemHdr, PccSubspaceHeader, RawGenericAddress};

/// See section "14.1.7. HW Registers based Communications Subspace
/// Structure (Type 5)" of the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug)]
pub struct PccHwRegisterBasedSubspace {
    /// PCC subspace header.
    pub header: PccSubspaceHeader,
    /// Must be 0x0001 (Version 1 of this PCC definition).
    pub version: u16,
    pub(super) base_address: u64,
    pub(super) mem_len: u64,
    pub(super) doorbell_reg: RawGenericAddress,
    pub(super) doorbell_preserve: u64,
    pub(super) doorbell_write: u64,
    cmd_complete_check_reg: RawGenericAddress,
    cmd_complete_check_mask: u64,
    err_status_reg: RawGenericAddress,
    err_status_mask: u64,
    /// Expected latency to process a command, in microseconds.
    pub nominal_latency: u32,
    /// The minimum amount of time that OSPM must wait after
    /// the completion of a command before issuing the next command,
    /// in microseconds.
    pub min_request_turnaround_time: u16,
}

impl PccHwRegisterBasedSubspace {
    /// Get information about the command complete check register.
    pub fn cmd_complete_check(&self) -> AcpiResult<PccCmdCompleteCheck> {
        let addr = GenericAddress::from_raw(self.cmd_complete_check_reg)?;
        Ok(PccCmdCompleteCheck { addr, mask: self.cmd_complete_check_mask })
    }

    /// Get information about the error status register.
    pub fn error_status(&self) -> AcpiResult<PccErrorStatus> {
        let addr = GenericAddress::from_raw(self.err_status_reg)?;
        Ok(PccErrorStatus { addr, mask: self.err_status_mask })
    }
}

/// Shared memory region header for [`PccHwRegisterBasedSubspace`].
///
/// See section "14.4. Reduced PCC Subspace Shared Memory Region" of
/// the ACPI spec for more information.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PccReducedShmem {
    /// The PCC signature. The signature of a subspace is computed b
    /// a bitwise-or of the value `0x50434300` with the subspace ID.
    /// For example, subspace 3 has the signature `0x50434303`.
    pub signature: u32,
}

impl PccShmemHdr for PccReducedShmem {
    fn signature(&self) -> u32 {
        self.signature
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use core::mem::offset_of;

    #[test]
    fn pcc_hw_register_based_subspace_offsets() {
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, base_address), 4);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, mem_len), 12);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, doorbell_reg), 20);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, doorbell_preserve), 32);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, doorbell_write), 40);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, cmd_complete_check_reg), 48);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, cmd_complete_check_mask), 60);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, err_status_reg), 68);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, err_status_mask), 80);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, nominal_latency), 88);
        assert_eq!(offset_of!(PccHwRegisterBasedSubspace, min_request_turnaround_time), 92);
    }
}
