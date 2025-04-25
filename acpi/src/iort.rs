use crate::{
    sdt::{SdtHeader, Signature},
    AcpiTable,
};
///Apart from the basic header, the table contains a number of IORT Nodes.
/// Each node represents a component, which can be an SMMU,
/// an ITS Group, a root complex, or a component that is described in the namespace.
use core::marker::PhantomData;

pub enum IortError {
    InvalidNodeType,
    InvalidLength,
    RevisionNotSupported,
    OffsetOutOfRange,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct Iort {
    header: SdtHeader,
    pub node_number: u32,
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
            write!(f, "\n{}", node)?;
        }
        Ok(())
    }
}

impl Iort {
    pub fn nodes(&self) -> IortNodeIter {
        let node_offset = self.node_array_offset as usize;
        let pointer = unsafe { (self as *const Iort as *const u8).add(node_offset) };
        let remaining_length = self.header.length - node_offset as u32;

        IortNodeIter { pointer, remaining_length, _phantom: PhantomData }
    }

    /// Returns the IORT node at the given offset.
    /// Used to find the node pointed to by the output_reference field in the id_mapping
    /// If the type field is invalid, return None.
    pub fn get_node(&self, offset: usize) -> Option<IortNode> {
        let node = unsafe { *((self as *const Iort as *const u8).add(offset) as *const IortNodeHeader) };
        match node.node_type {
            0 => Some(IortNode::Its(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const ItsNode)
            })),
            1 => Some(IortNode::NamedComponent(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const NamedComponentNode)
            })),
            2 => Some(IortNode::RootComplex(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const RootComplexNode)
            })),
            3 => Some(IortNode::SmmuV12(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const SmmuV12Node)
            })),
            4 => Some(IortNode::SmmuV3(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const SmmuV3Node)
            })),
            5 => Some(IortNode::Pmcg(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const PmcgNode)
            })),
            6 => Some(IortNode::MemoryRange(unsafe {
                &*((self as *const Iort as *const u8).add(offset) as *const MemoryRangeNode)
            })),
            _ => None,
        }
    }

    pub fn smmuv3_bases(&self) -> impl Iterator<Item = u64> + '_ {
        self.nodes().filter_map(|node| match node {
            IortNode::SmmuV3(smmu_v3) => Some(smmu_v3.base_address),
            _ => None,
        })
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct IortNodeHeader {
    pub node_type: u8,
    pub length: u16,
    pub revision: u8,
    pub identifier: u32,
    pub id_mapping_num: u32,
    pub id_mapping_array_offset: u32,
    // Between this and id_mapping_array, there are data fields that are specific to each node type
    // After offset_to_id_mapping_array, there are id_mapping_num ID Mappings
}

impl IortNodeHeader {
    pub fn validate(&self) -> Result<(), IortError> {
        if self.node_type > 6 {
            return Err(IortError::InvalidNodeType);
        }
        if self.length < core::mem::size_of::<IortNodeHeader>() as u16 {
            return Err(IortError::InvalidLength);
        }
        if self.id_mapping_array_offset < self.length as u32 {
            return Err(IortError::OffsetOutOfRange);
        }
        // let revision_valid = match self.node_type {
        //     0 => self.revision == 1,
        //     1 => self.revision == 4,
        //     2 => self.revision == 4,
        //     3 => self.revision == 3,
        //     4 => self.revision == 5,
        //     5 => self.revision == 2,
        //     6 => self.revision == 3,
        //     _ => false,
        // };
        // if !revision_valid {
        //     return Err(IortError::RevisionNotSupported);
        // }
        Ok(())
    }

    pub fn id_mapping_array(&self) -> &[IortIdMapping] {
        let id_mapping_num = self.id_mapping_num;
        let id_mapping_array_offset = self.id_mapping_array_offset;

        unsafe {
            let ptr = (self as *const IortNodeHeader as *const u8).add(id_mapping_array_offset as usize);
            core::slice::from_raw_parts(ptr as *const IortIdMapping, id_mapping_num as usize)
        }
    }
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

impl fmt::Display for IortNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = match self {
            IortNode::Its(node) => write!(f, "{}", node),
            IortNode::NamedComponent(node) => write!(f, "{}", node),
            IortNode::RootComplex(node) => write!(f, "{}", node),
            IortNode::SmmuV12(node) => write!(f, "{}", node),
            IortNode::SmmuV3(node) => write!(f, "{}", node),
            IortNode::Pmcg(node) => write!(f, "{}", node),
            IortNode::MemoryRange(node) => write!(f, "{}", node),
        };
        for id in self.id_mapping_array() {
            write!(f, "\n{:#x?}", id)?
        }
        Ok(())
    }
}

impl IortNode<'_> {
    pub fn id_mapping_array(&self) -> &[IortIdMapping] {
        self.header().id_mapping_array()
    }

    pub fn header(&self) -> &IortNodeHeader {
        match self {
            IortNode::Its(node) => &node.header,
            IortNode::NamedComponent(node) => &node.header,
            IortNode::RootComplex(node) => &node.header,
            IortNode::SmmuV12(node) => &node.header,
            IortNode::SmmuV3(node) => &node.header,
            IortNode::Pmcg(node) => &node.header,
            IortNode::MemoryRange(node) => &node.header,
        }
    }
}

pub struct IortNodeIter<'a> {
    pointer: *const u8,
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> Iterator for IortNodeIter<'a> {
    type Item = IortNode<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length == 0 {
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
        self.remaining_length -= node_length;

        Some(node)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct ItsNode {
    pub header: IortNodeHeader,
    pub its_number: u32,
    // This is followed by the ITS Identifer Array of length 4 * its_number
}

impl fmt::Display for ItsNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)?;
        write!(f, "\n\tIdentifiers:")?;
        for id in self.its_identifiers() {
            write!(f, " {} ", id)?
        }
        Ok(())
    }
}

impl ItsNode {
    pub fn its_identifiers(&self) -> &[u32] {
        let its_number = self.its_number;
        let its_identifiers_offset = core::mem::size_of::<ItsNode>() as u32;

        unsafe {
            let ptr = (self as *const ItsNode as *const u8).add(its_identifiers_offset as usize);
            core::slice::from_raw_parts(ptr as *const u32, its_number as usize)
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct NamedComponentNode {
    pub header: IortNodeHeader,
    pub node_flags: u32,
    pub mem_access_properties: u64,
    pub device_mem_address_size_limit: u8,
    // This is followed by a ASCII Null terminated string
    // with the full path to the entry in the namespace for this object
    // and a padding to 32-bit word-aligned.
}

impl fmt::Display for NamedComponentNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)?;
        write!(f, "\n\tName: {}", self.name())
    }
}

impl NamedComponentNode {
    pub fn name(&self) -> &str {
        let name_offset = core::mem::size_of::<NamedComponentNode>() as u32;
        let name_ptr = unsafe { (self as *const NamedComponentNode as *const u8).add(name_offset as usize) };

        let mut length = 0;
        while unsafe { *name_ptr.add(length as usize) } != 0 {
            length += 1;
        }

        unsafe { core::str::from_utf8_unchecked(core::slice::from_raw_parts(name_ptr, length as usize)) }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct RootComplexNode {
    pub header: IortNodeHeader,
    pub mem_access_properties: u64,
    pub ats_attribute: u32,
    pub pci_segment_number: u32,
    pub mem_address_size_limit: u8,
    pub pasid_capabilities: u16,
    pub reserved: u8,
    pub flags: u32,
}

impl fmt::Display for RootComplexNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct SmmuV12Node {
    pub header: IortNodeHeader,
    pub base_address: u64,
    pub span: u64,
    pub model: u32,
    pub flags: u32,
    pub global_interrupt_array_offset: u32,
    pub context_interrupt_array_num: u32,
    pub context_interrupt_array_offset: u32,
    pub pmu_interrupt_array_num: u32,
    pub pmu_interrupt_array_offset: u32,
    // Followed by several interrupt arrays
}

impl fmt::Display for SmmuV12Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct SmmuV3Node {
    pub header: IortNodeHeader,
    pub base_address: u64,
    pub flags: u32,
    pub reserved: u32,
    pub vatos_address: u64,
    pub model: u32,
    pub event: u32,
    pub pri: u32,
    pub gerr: u32,
    pub sync: u32,
    pub proximity_domain: u32,
    pub device_id_mapping_index: u32,
}

impl fmt::Display for SmmuV3Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)
    }
}

impl SmmuV3Node {
    /// ID mappings of an SMMUv3 node can only have ITS group nodes as output references.
    /// All other output references are illegal and forbidden
    pub fn output_its_nodes<'a>(&'a self, iort: &'a Iort) -> impl Iterator<Item = &'a ItsNode> {
        self.header.id_mapping_array().iter().map(|id_mapping| {
            let offset = id_mapping.output_reference as usize;
            iort.get_node(offset).and_then(|node| match node {
                IortNode::Its(its) => Some(its),
                _ => {log::error!("Invalid output reference in SMMUv3 ID mapping, type: {}", node.header().node_type); None},
            }).unwrap()
        })
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct PmcgNode {
    pub header: IortNodeHeader,
    pub pg0_base_address: u64,
    pub overflow_interrupt: u32,
    pub node_reference: u32,
    pub pg1_base_address: u64,
}

impl fmt::Display for PmcgNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(C, packed)]
pub struct MemoryRangeNode {
    pub header: IortNodeHeader,
    pub flags: u32,
    pub mem_range_discriptor_array_num: u32,
    pub mem_range_discriptor_array_offset: u32,
    // After the base address of the node is offset, there are multiple MemoryRangeDescriptor
}

impl fmt::Display for MemoryRangeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x?}", self)?;
        for descriptor in self.memory_range_descriptors() {
            write!(f, "\n\t{:#x?}", descriptor)?
        }
        Ok(())
    }
}

impl MemoryRangeNode {
    pub fn memory_range_descriptors(&self) -> &[MemoryRangeDescriptor] {
        let mem_range_discriptor_array_num = self.mem_range_discriptor_array_num;
        let mem_range_discriptor_array_offset = self.mem_range_discriptor_array_offset;

        unsafe {
            let ptr =
                (self as *const MemoryRangeNode as *const u8).add(mem_range_discriptor_array_offset as usize);
            core::slice::from_raw_parts(
                ptr as *const MemoryRangeDescriptor,
                mem_range_discriptor_array_num as usize,
            )
        }
    }
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
    pub input_base: u32,
    pub id_count: u32,
    pub output_base: u32,
    /// A reference to the output IORT Node. This field contains the
    /// address offset of the IORT Node relative to the start of the IORT.
    pub output_reference: u32,
    pub flags: u32,
}

#[cfg(test)]
mod tests {
    use core::{ops::Deref, ptr::NonNull};
    use std::{io::Read, vec::Vec};

    use crate::{read_table, AcpiHandler, PhysicalMapping};

    use super::*;

    #[derive(Clone)]
    struct AcpiIO;

    impl AcpiHandler for AcpiIO {
        unsafe fn map_physical_region<T>(
            &self,
            physical_address: usize,
            size: usize,
        ) -> crate::PhysicalMapping<Self, T> {
            unsafe {
                PhysicalMapping::new(
                    physical_address,
                    NonNull::new_unchecked(physical_address as *mut T),
                    size,
                    size,
                    AcpiIO,
                )
            }
        }

        fn unmap_physical_region<T>(_region: &crate::PhysicalMapping<Self, T>) {}
    }

    #[test]
    fn test_iort() {
        let file = std::fs::File::open("../example/acpi-dsl/iort.aml").unwrap();
        let buffer = file.bytes().map(|b| b.unwrap()).collect::<Vec<u8>>();
        let iort = unsafe { read_table::<AcpiIO, Iort>(AcpiIO, buffer.as_ptr() as usize).unwrap() };
        let iort = iort.deref();

        let output = std::fs::File::create("iort.txt").unwrap();
        use crate::std::io::Write;
        let mut writer = std::io::BufWriter::new(output);
        write!(writer, "{}", iort).unwrap();

        iort.nodes().for_each(|node| match node {
            IortNode::SmmuV3(smmu_v3) => {
                let base_addr = smmu_v3.base_address;
                println!("smmu: {:#x?}", base_addr);
                smmu_v3.header.id_mapping_array().iter().for_each(|id_mapping| {
                    let input_base = id_mapping.input_base;
                    let id_count = id_mapping.id_count;
                    let output_base = id_mapping.output_base;
                    let output_reference = id_mapping.output_reference;
                    let output_node = iort.get_node(output_reference as usize).unwrap();
                    println!("    output_reference: {:#x?}", output_reference);
                    println!(
                        "\tinput_base: {:#x}\n\tid_count: {:#x}\n\toutput_base: {:#x}",
                        input_base, id_count, output_base
                    );
                    match output_node {
                        IortNode::Its(its) => {
                            println!("\tITS Groups: {:x?}", its.its_identifiers());
                        }
                        _ => {}
                    }
                });

            }
            _ => {}
        });
    }
}