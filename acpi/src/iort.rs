///Apart from the basic header, the table contains a number of IORT Nodes. 
/// Each node represents a component, which can be an SMMU, 
/// an ITS Group, a root complex, or a component that is described in the namespace.
use core::marker::PhantomData;
use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};

pub enum IortError {
    
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct Iort {
    header: SdtHeader,
    node_number: u32,
    node_array_offset: u32,
    reserved: u32,
    // After offset_to_node_array, there are node_number IORT Nodes
}

/// ### Safety: This is safe to implement.
/// The IORT table is safe to implement because it is a valid ACPI table.
unsafe impl AcpiTable for Iort {
    const SIGNATURE: Signature = Signature::IORT;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

use core::fmt;
impl fmt::Display for Iort {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IORT: {:?}", self.header)?;
        for node in self.nodes() {
            write!(f, "\n{:#x?}", node)?
        }
        Ok(())
    }
}

impl Iort {
    pub fn nodes(&self) -> IortNodeIter {
        let pointer = unsafe { (self as *const Iort).add(1) as *const u8 };
        let remaining_length = self.header.length as u32 - core::mem::size_of::<Iort>() as u32;

        IortNodeIter {
            pointer,
            remaining_length,
            _phantom: PhantomData
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct IortNodeHeader {
    node_type: u8,
    length: u16,
    revision: u8,
    identifier: u32,
    pub id_mapping_num: u32,
    pub id_mapping_array_offset: u32,
    // Between this and id_mapping_array, there are data fields that are specific to each node type
    // After offset_to_id_mapping_array, there are id_mapping_num ID Mappings
}

#[derive(Debug)]
pub enum IortNode<'a> {
    Its(&'a ItsNode),
    NamedComponent(&'a NamedComponentNode),
    RootComplex(&'a RootComplexNode),
    SmmuV12(&'a SmmuV12Node),
    SmmuV3(&'a SmmuV3Node),
    Pmcg(&'a PmcgNode),
    MemoryRange(&'a MemoryRangeNode),
}

impl IortNode<'_> {
    pub fn id_mapping_array(&self) -> Option<&[IortIdMapping]> {
        let node_header = unsafe{ *(self as *const IortNode as *const IortNodeHeader) };
        let id_mapping_num = node_header.id_mapping_num;
        let id_mapping_array_offset = node_header.id_mapping_array_offset;
        
        if id_mapping_num == 0 {
            return None;
        } else {
            unsafe {
                let ptr = (self as *const IortNode as *const u8).add(id_mapping_array_offset as usize);
                Some(core::slice::from_raw_parts(ptr as *const IortIdMapping, id_mapping_num as usize))
            }
        }
    }
}

pub struct IortNodeIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>
}

impl<'a> Iterator for IortNodeIter<'a> {
    type Item = IortNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length <= 0 {
            return None;
        }

        let node = unsafe { &*(self.pointer as *const IortNodeHeader) };
        let node_length = node.length as u32;

        let node = match node.node_type {
            0 => IortNode::Its(unsafe { &*(self.pointer as *const ItsNode) }),
            1 => IortNode::NamedComponent(unsafe { &*(self.pointer as *const NamedComponentNode) }),
            2 => IortNode::RootComplex(unsafe { &*(self.pointer as *const RootComplexNode) }),
            3 => IortNode::SmmuV12(unsafe { &*(self.pointer as *const SmmuV12Node) }),
            4 => IortNode::SmmuV3(unsafe { &*(self.pointer as *const SmmuV3Node) }),
            5 => IortNode::Pmcg(unsafe { &*(self.pointer as *const PmcgNode) }),
            6 => IortNode::MemoryRange(unsafe { &*(self.pointer as *const MemoryRangeNode) }),
            _ => return None,
        };

        self.pointer = unsafe { self.pointer.add(node_length as usize) };
        self.remaining_length -= node_length as u32;

        Some(node)
    }
}


#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct ItsNode {
    header: IortNodeHeader,
    its_number: u32,
    // This is followed by the ITS Identifer Array of length 4 * its_number
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct NamedComponentNode {
    header: IortNodeHeader,
    node_flags: u32,
    mem_access_properties: u64,
    device_mem_address_size_limit: u8,
    // This is followed by a ASCII Null terminated string 
    // with the full path to the entry in the namespace for this object
    // and a padding to 32-bit word-aligned.
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct RootComplexNode {
    header: IortNodeHeader,
    mem_access_properties: u64,
    ats_attribute: u32,
    pci_segment_number: u32,
    mem_address_size_limit: u8,
    pasid_capabilities: u16,
    reserved: u8,
    flags: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct SmmuV12Node {
    header: IortNodeHeader,
    base_address: u64,
    span: u64,
    model: u32,
    flags: u32,
    global_interrupt_array_offset: u32,
    context_interrupt_array_num: u32,
    context_interrupt_array_offset: u32,
    pmu_interrupt_array_num: u32,
    pmu_interrupt_array_offset: u32,
    // 后面是几个中断数组
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct SmmuV3Node {
    header: IortNodeHeader,
    base_address: u64,
    flags: u32,
    reserved: u32,
    vatos_address: u64,
    model: u32,
    event: u32,
    pri: u32,
    gerr: u32,
    sync: u32,
    proximity_domain: u32,
    device_id_mapping_index: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct PmcgNode {
    header: IortNodeHeader,
    pg0_base_address: u64,
    overflow_interrupt: u32,
    node_reference: u32,
    pg1_base_address: u64,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct MemoryRangeNode {
    header: IortNodeHeader,
    flags: u32,
    mem_range_discriptor_array_num: u32,
    mem_range_discriptor_array_offset: u32,
    // After the base address of the node is offset, there are multiple MemoryRangeDescriptor
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct MemoryRangeDescriptor {
    physical_range_offset: u64,
    physical_range_length: u64,
    reserved: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct IortIdMapping {
    input_base: u32,
    id_count: u32,
    output_base: u32,
    output_reference: u32,
    flags: u32,
}