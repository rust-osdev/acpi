use crate::ManagedSlice;
use core::alloc::Allocator;

/// Processor trait
pub trait Processor {
    /// The OS associates this GICC Structure with a processor device object in the namespace 
    /// when the _UID child object of the processor device evaluates to a numeric value that matches the numeric value in this field.
    fn processor_uid(&self) -> u32;
}

impl Processor for Arm64Processor {
    fn processor_uid(&self) -> u32 {
        self.processor_uid
    }
}

impl Processor for X86Processor {
    fn processor_uid(&self) -> u32 {
        self.processor_uid
    }
}

/// Arm64 processor
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Arm64Processor {
    /// The OS associates this GICC Structure with a processor device object in the namespace 
    /// when the _UID child object of the processor device evaluates to a numeric value that matches the numeric value in this field.
    pub processor_uid: u32,
    /// This fields follows the MPIDR formatting of ARM architecture.
    pub mpidr: u64,
    /// On GICv1/v2 systems and GICv3/4 systems in GICv2 compatibility mode, this field holds the 64-bit physical address at which the processor can access this GIC CPU Interface.
    /// If provided here, the “Local Interrupt Controller Address” field in the MADT must be ignored by the OSPM.
    pub gicc_base_address: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProcessorState {
    /// A processor in this state is unusable, and you must not attempt to bring it up.
    Disabled,

    /// A processor waiting for a SIPI (Startup Inter-processor Interrupt) is currently not active,
    /// but may be brought up.
    WaitingForSipi,

    /// A Running processor is currently brought up and running code.
    Running,
}

/// x86_64 processor
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct X86Processor {
    /// Corresponds to the `_UID` object of the processor's `Device`, or the `ProcessorId` field of the `Processor`
    /// object, in AML.
    pub processor_uid: u32,
    /// The ID of the local APIC of the processor. Will be less than `256` if the APIC is being used, but can be
    /// greater than this if the X2APIC is being used.
    pub local_apic_id: u32,

    /// The state of this processor. Check that the processor is not `Disabled` before attempting to bring it up!
    pub state: ProcessorState,

    /// Whether this processor is the Bootstrap Processor (BSP), or an Application Processor (AP).
    /// When the bootloader is entered, the BSP is the only processor running code. To run code on
    /// more than one processor, you need to "bring up" the APs.
    pub is_ap: bool,
}

#[derive(Debug)]
pub enum ProcessorInfo<'a, A>
where
    A: Allocator,
{
    /// in x86_64, the BSP is the first processor.
    /// Application processors should be brought up in the order they're defined in this list.
    X86Processors(ManagedSlice<'a, X86Processor, A>),

    Arm64Processors(ManagedSlice<'a, Arm64Processor, A>),
}

impl<'a, A> ProcessorInfo<'a, A>
where
    A: Allocator,
{
    pub fn new_x86(processors: ManagedSlice<'a, X86Processor, A>) -> Self {
        ProcessorInfo::X86Processors(processors)
    }

    pub fn new_arm64(processors: ManagedSlice<'a, Arm64Processor, A>) -> Self {
        ProcessorInfo::Arm64Processors(processors)
    }
}