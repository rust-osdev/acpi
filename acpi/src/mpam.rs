use core::{marker::PhantomData, mem::size_of};

use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct Mpam {
    header: SdtHeader,
    // An array of MPAM node structures that describes MSCs in the system.
}

/// ### Safety: This is safe to implement.
unsafe impl AcpiTable for Mpam {
    const SIGNATURE: Signature = Signature::MPAM;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

use core::fmt;
impl fmt::Display for Mpam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MAPM: {:#x?}", self.header)?;
        let mut i = 100;
        for node in self.nodes() {
            write!(f, "\n{:#x?}", node)?;
            i -= 1;
            if i == 0 {
                break;
            }
        }
        Ok(())
    }
}

impl Mpam {
    /// Returns an iterator over the MPAM node structures.
    pub fn nodes(&self) -> MscNodeIter {
        let pointer = unsafe { (self as *const Mpam).add(1) as *const u8 };
        let remaining_length = self.header.length as u32 - size_of::<Mpam>() as u32;

        MscNodeIter {
            pointer,
            remaining_length,
            _phantom: PhantomData
        }
    }

    pub fn mmio_ranges(&self) -> impl Iterator<Item = (u64, u32)> + '_ {
        self.nodes().filter_map(|node| {
            return if node.if_type == 0x00 {
                Some((node.base_address, node.mmio_size))
            } else {
                None
            };
        })
    }
}

pub struct MscNodeIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>
}

impl Iterator for MscNodeIter<'_> {
    type Item = MscNode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length <= 0 {
            return None;
        }

        let node = unsafe { &*(self.pointer as *const MscNode) };
        let node_length = node.length as u32;
        
        self.pointer = unsafe { self.pointer.add(node_length as usize) };
        self.remaining_length -= node_length;

        Some(*node)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct MscNode {
    // MPAM node structure
    pub length: u16,
    pub if_type: u8,
    pub reserved: u8,
    pub id: u32,
    pub base_address: u64,
    pub mmio_size: u32,
    
    pub overflow_interrupt: u32,
    pub overflow_interrupt_flags: u32,
    pub reserved2: u32,
    pub overflow_interrupt_affinity: u32,
    
    pub error_interrupt: u32,
    pub error_interrupt_flags: u32,
    pub reserved3: u32,
    pub error_interrupt_affinity: u32,

    pub max_nrdy_usec: u32,
    pub hw_id: u64,
    pub instance_id: u32,
    pub resource_node_num: u32,
    // Resource node list
    // after the resource node list, there is resource specific data, if the length after the resource node list is not 0
}

impl fmt::Display for MscNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x?}", self)?;
        for node in self.resource_nodes() {
            write!(f, "\n\t{}", node)?
        }
        Ok(())
    }
}

impl MscNode {
    pub fn resource_nodes(&self) -> ResourceNodeIter {
        let ptr = unsafe {
            let ptr = self as *const MscNode;
            ptr.add(1) as *const u8
        };
        let remaining_length = self.length as u32 - size_of::<MscNode>() as u32;
        ResourceNodeIter {
            pointer: ptr,
            remaining_length,
            remaining_nodes: self.resource_node_num,
            _phantom: PhantomData
        }
    }
}

pub struct ResourceNodeIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    remaining_nodes: u32,
    _phantom: PhantomData<&'a ()>
}

impl Iterator for ResourceNodeIter<'_> {
    type Item = ResourceNode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length <= 0 || self.remaining_nodes <= 0 {
            return None;
        }

        let node = unsafe { &*(self.pointer as *const ResourceNode) };
        let node_length = size_of::<ResourceNode>() + (node.func_dep_num as usize) * size_of::<FunctionDependency>();
        
        self.pointer = unsafe { self.pointer.add(node_length) };
        self.remaining_length -= node_length as u32;
        self.remaining_nodes -= 1;

        Some(*node)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct ResourceNode {
    // Resource node structure
    pub id: u32,
    pub ris_id: u32,
    pub reserved: u16,
    pub locator_type: u8,
    pub locator: (u64, u32),
    pub func_dep_num: u32,
    // Function dependency list [FunctionDependency]
}

impl fmt::Display for ResourceNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x?}", self)?;
        for dep in self.function_dependencies() {
            write!(f, "\n\t{:#x?}", dep)?
        }
        Ok(())
    }
}

impl ResourceNode {
    pub fn function_dependencies(&self) -> &[FunctionDependency] {
        let ptr = self as *const ResourceNode;
        let ptr = unsafe { ptr.add(1) };
        let ptr = ptr as *const FunctionDependency;
        unsafe {
            core::slice::from_raw_parts(ptr, self.func_dep_num as usize)
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct FunctionDependency {
    // Function dependency structure
    pub producer: u32,
    pub reserved: u32,
}