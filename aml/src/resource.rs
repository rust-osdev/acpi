use crate::{value::AmlValue, AmlError};
use alloc::vec::Vec;
use bit_field::BitField;
use byteorder::{ByteOrder, LittleEndian};

#[derive(Debug, PartialEq)]
pub enum Resource {
    Irq(IrqDescriptor),
    WordAddressSpace(AddressSpaceDescriptor),
    MemoryRange(MemoryRangeDescriptor),
    IOPort(IOPortDescriptor),
    Dma(DMADescriptor)
}


/// Parse a `ResourceDescriptor` into a list of resources. Returns `AmlError::IncompatibleValueConversion` if the passed value is not a
/// `Buffer`.
pub fn resource_descriptor_list(descriptor: &AmlValue) -> Result<Vec<Resource>, AmlError> {
    if let AmlValue::Buffer { bytes, size: _ } = descriptor {
        let mut descriptors = Vec::new();
        let mut bytes = bytes.as_slice();

        while bytes.len() > 0 {
            let (descriptor, remaining_bytes) = resource_descriptor_inner(bytes)?;

            if let Some(descriptor) = descriptor {
                descriptors.push(descriptor);
                bytes = remaining_bytes;
            } else {
                break;
            }
        }

        Ok(descriptors)
    } else {
        Err(AmlError::IncompatibleValueConversion)
    }
}

/// Parse a `ResourceDescriptor`. Returns `AmlError::IncompatibleValueConversion` if the passed value is not a
/// `Buffer`.
pub fn resource_descriptor(descriptor: &AmlValue) -> Result<Resource, AmlError> {
    if let AmlValue::Buffer { bytes, size: _ } = descriptor {
        let (descriptor, _) = resource_descriptor_inner(bytes)?;
        let descriptor = descriptor.expect("found unexpected End Tag Descriptor");
        Ok(descriptor)
    } else {
        Err(AmlError::IncompatibleValueConversion)
    }
}

fn resource_descriptor_inner(bytes: &[u8]) -> Result<(Option<Resource>, &[u8]), AmlError> {
    /*
    * If bit 7 of Byte 0 is set, it's a large descriptor. If not, it's a small descriptor.
    */
    if bytes[0].get_bit(7) {
        /*
        * We're parsing a large item. The descriptor type is encoded in Bits 0-6 of Byte 0. Valid types:
        *      0x00: Reserved
        *      0x01: 24-bit Memory Range Descriptor
        *      0x02: Generic Register Descriptor
        *      0x03: Reserved
        *      0x04: Vendor-defined Descriptor
        *      0x05: 32-bit Memory Range Descriptor
        *      0x06: 32-bit Fixed Memory Range Descriptor
        *      0x07: Address Space Resource Descriptor
        *      0x08: Word Address Space Descriptor
        *      0x09: Extended Interrupt Descriptor
        *      0x0a: QWord Address Space Descriptor
        *      0x0b: Extended Address Space Descriptor
        *      0x0c: GPIO Connection Descriptor
        *      0x0d: Pin Function Descriptor
        *      0x0e: GenericSerialBus Connection Descriptor
        *      0x0f: Pin Configuration Descriptor
        *      0x10: Pin Group Descriptor
        *      0x11: Pin Group Function Descriptor
        *      0x12: Pin Group Configuration Descriptor
        *      0x13-0x7f: Reserved
        *
        * Byte 1 contains bits 0-7 of the length, and Byte 2 contains bits 8-15 of the length. Subsequent
        * bytes contain the actual data items.
        */
        let descriptor_type = bytes[0].get_bits(0..7);
        let length = LittleEndian::read_u16(&bytes[1..=2]) as usize;

        let descriptor = match descriptor_type {
            0x01 => unimplemented!("24-bit Memory Range Descriptor"),
            0x02 => unimplemented!("Generic Register Descriptor"),
            0x03 => unimplemented!("0x03 Reserved"),
            0x04 => unimplemented!("Vendor-defined Descriptor"),
            0x05 => unimplemented!("32-bit Memory Range Descriptor"),
            0x06 => fixed_memory_descriptor(&bytes[0..=length]),
            0x07 => address_space_descriptor::<u32>(&bytes[0..=length]),
            0x08 => address_space_descriptor::<u16>(&bytes[0..=length]),
            0x09 => extended_interrupt_descriptor(&bytes[0..=length]),
            0x0a => address_space_descriptor::<u64>(&bytes[0..=length]),
            0x0b => unimplemented!("Extended Address Space Descriptor"),
            0x0c => unimplemented!("GPIO Connection Descriptor"),
            0x0d => unimplemented!("Pin Function Descriptor"),
            0x0e => unimplemented!("GenericSerialBus Connection Descriptor"),
            0x0f => unimplemented!("Pin Configuration Descriptor"),
            0x10 => unimplemented!("Pin Group Descriptor"),
            0x11 => unimplemented!("Pin Group Function Descriptor"),
            0x12 => unimplemented!("Pin Group Configuration Descriptor"),

            0x00 | 0x13..=0x7f => Err(AmlError::ReservedResourceType),
            0x80..=0xff => unreachable!(),
        }?;
        
        Ok((Some(descriptor), &bytes[length+3..]))
    } else {
        /*
        * We're parsing a small descriptor. Byte 0 has the format:
        *    | Bits        | Field             |
        *    |-------------|-------------------|
        *    | 0-2         | Length - n bytes  |
        *    | 3-6         | Small item type   |
        *    | 7           | 0 = small item    |
        *
        * The valid types are:
        *      0x00-0x03: Reserved
        *      0x04: IRQ Format Descriptor
        *      0x05: DMA Format Descriptor
        *      0x06: Start Dependent Functions Descriptor
        *      0x07: End Dependent Functions Descriptor
        *      0x08: IO Port Descriptor
        *      0x09: Fixed Location IO Port Descriptor
        *      0x0A: Fixed DMA Descriptor
        *      0x0B-0x0D: Reserved
        *      0x0E: Vendor Defined Descriptor
        *      0x0F: End Tag Descriptor
        */
        let descriptor_type = bytes[0].get_bits(3..=6);
        let length: usize = bytes[0].get_bits(0..=2) as usize;

        let descriptor = match descriptor_type {
            0x00..=0x03 => Err(AmlError::ReservedResourceType),
            0x04 => irq_format_descriptor(&bytes[0..=length]),
            0x05 => dma_format_descriptor(&bytes[0..=length]),
            0x06 => unimplemented!("Start Dependent Functions Descriptor"),
            0x07 => unimplemented!("End Dependent Functions Descriptor"),
            0x08 => io_port_descriptor(&bytes[0..=length]),
            0x09 => unimplemented!("Fixed Location IO Port Descriptor"),
            0x0A => unimplemented!("Fixed DMA Descriptor"),
            0x0B..=0x0D => Err(AmlError::ReservedResourceType),
            0x0E => unimplemented!("Vendor Defined Descriptor"),
            0x0F => return Ok((None, &[])),
            0x10..=0xFF => unreachable!()
        }?;

        Ok((Some(descriptor), &bytes[length+1..]))
    }
}

#[derive(Debug, PartialEq)]
pub enum InterruptTrigger {
    Edge,
    Level,
}

#[derive(Debug, PartialEq)]
pub enum InterruptPolarity {
    ActiveHigh,
    ActiveLow,
}

#[derive(Debug, PartialEq)]
pub enum AddressSpaceResourceType {
    MemoryRange,
    IORange,
    BusNumberRange
}

#[derive(Debug, PartialEq)]
pub enum AddressSpaceDecodeType {
    Additive,
    Subtractive
}


#[derive(Debug, PartialEq)]
pub struct AddressSpaceDescriptor {
    resource_type: AddressSpaceResourceType,
    is_maximum_address_fixed: bool,
    is_minimum_address_fixed: bool,
    decode_type: AddressSpaceDecodeType,

    granularity: u64,
    address_range: (u64, u64),
}

#[derive(Debug, PartialEq)]
pub enum MemoryRangeDescriptor {
    FixedLocation { 
        is_writable: bool,
        base_address: u32,
        range_length: u32
    }
}

fn fixed_memory_descriptor(bytes: &[u8]) -> Result<Resource, AmlError> {
    /*
     * -- 32-bit Fixed Memory Descriptor ---
     * Offset     Field Name                              Definition      
     * Byte 0     32-bit Fixed Memory Range Descriptor    Value = 0x86 (10000110B) – Type = 1, Large item name = 0x06
     * Byte 1     Length, bits [7:0]                      Value = 0x09 (9)
     * Byte 2     Length, bits [15:8]                     Value = 0x00
     * Byte 3     Information                             This field provides extra information about this memory. 
     *                                                    Bit [7:1]   Ignored
     *                                                    Bit [0]     Write status, _RW
     *                                                        1  writeable (read/write)
     *                                                        0  non-writeable (read-only))
     * Byte 4     Range base address, _BAS bits [7:0]     Address bits [7:0] of the base memory address for which the card may be configured. 
     * Byte 5     Range base address, _BASbits [15:8]     Address bits [15:8] of the base memory address for which the card may be configured.
     * Byte 6     Range base address, _BASbits [23:16]    Address bits [23:16] of the base memory address for which the card may be configured. 
     * Byte 7     Range base address, _BASbits [31:24]    Address bits [31:24] of the base memory address for which the card may be configured.
     * Byte 8     Range length, _LEN bits [7:0]           This field contains bits [7:0] of the memory range length. The range length provides the length of the memory range in 1-byte blocks.
     * Byte 9     Range length, _LEN bits[15:8]           This field contains bits [15:8] of the memory range length. The range length provides the length of the memory range in 1-byte blocks.
     * Byte 10    Range length, _LEN bits [23:16]         This field contains bits [23:16] of the memory range length. The range length provides the length of the memory range in 1-byte blocks.
     * Byte 11    Range length, _LEN bits [31:24]         This field contains bits [31:24] of the memory range length. The range length provides the length of the memory range in 1-byte blocks.
     */
    if bytes.len() < 12 {
        return Err(AmlError::ResourceDescriptorTooShort);
    }

    let information = bytes[3];
    let is_writable = information.get_bit(0);

    let base_address = 
        ((bytes[4] as u32) << 0) | 
        ((bytes[5] as u32) << 8) | 
        ((bytes[6] as u32) << 16) | 
        ((bytes[7] as u32) << 24);

    let range_length = 
        ((bytes[8] as u32) << 0) | 
        ((bytes[9] as u32) << 8) | 
        ((bytes[10] as u32) << 16) | 
        ((bytes[11] as u32) << 24);

    Ok(Resource::MemoryRange(MemoryRangeDescriptor::FixedLocation {
        is_writable,
        base_address,
        range_length
    }))
}

fn address_space_descriptor<T>(bytes: &[u8]) -> Result<Resource, AmlError> {
    if bytes.len() < 14 {
        return Err(AmlError::ResourceDescriptorTooShort);
    }

    let resource_type = match bytes[3] {
        0 => AddressSpaceResourceType::MemoryRange,
        1 => AddressSpaceResourceType::IORange,
        2 => AddressSpaceResourceType::BusNumberRange,
        3..=191 => return Err(AmlError::ReservedResourceType),
        192..=255 => unimplemented!()
    };

    let general_flags = bytes[4];
    let is_maximum_address_fixed = general_flags.get_bit(3);
    let is_minimum_address_fixed = general_flags.get_bit(2);
    let decode_type = if general_flags.get_bit(1) {
        AddressSpaceDecodeType::Subtractive
    } else {
        AddressSpaceDecodeType::Additive
    };

    let size = core::mem::size_of::<T>();
    let location = 6;

    let granularity = LittleEndian::read_uint(&bytes[location..], size);
    let location = location + size;

    let address_range_min = LittleEndian::read_uint(&bytes[location..], size);
    let address_range_max = LittleEndian::read_uint(&bytes[location+size..], size);
    let address_range = (address_range_min, address_range_max);

    Ok(Resource::WordAddressSpace(AddressSpaceDescriptor {
        resource_type,
        is_maximum_address_fixed,
        is_minimum_address_fixed,
        decode_type,
        granularity,
        address_range
    }))
}

#[derive(Debug, PartialEq)]
pub struct IrqDescriptor {
    pub is_consumer: bool,
    pub trigger: InterruptTrigger,
    pub polarity: InterruptPolarity,
    pub is_shared: bool,
    pub is_wake_capable: bool,
    /*
     * NOTE: We currently only support the cases where a descriptor only contains a single interrupt
     * number.
     */
    pub irq: u32,
}

fn irq_format_descriptor(bytes: &[u8]) -> Result<Resource, AmlError> {

    // We do `- 1` here so the arms below match specification. It doesn't 
    // count byte 0 for some reason.
    match bytes.len() - 1 { 
        0..=1 => Err(AmlError::ResourceDescriptorTooShort),
        2 => {
            let irq = LittleEndian::read_u16(&bytes[1..=2]);

            Ok(Resource::Irq(IrqDescriptor {
                irq: irq as u32,
                is_wake_capable: false,
                is_shared: false,
                polarity: InterruptPolarity::ActiveHigh,
                trigger: InterruptTrigger::Edge,

                is_consumer: false // Is this correct?
            }))
        },
        3 => {
            let irq = LittleEndian::read_u16(&bytes[1..=2]);

            let information = bytes[3];
            let is_wake_capable = information.get_bit(5);
            let is_shared = information.get_bit(4);
            let polarity = match information.get_bit(3) {
                false => InterruptPolarity::ActiveHigh,
                true => InterruptPolarity::ActiveLow
            };
            let trigger = match information.get_bit(0) {
                false => InterruptTrigger::Level,
                true => InterruptTrigger::Edge
            };

            Ok(Resource::Irq(IrqDescriptor {
                irq: irq as u32,
                is_wake_capable,
                is_shared,
                polarity,
                trigger,

                is_consumer: false // Is this correct?
            }))
        },
        _ => Err(AmlError::ResourceDescriptorTooLong)
    }
}

#[derive(Debug, PartialEq)]
pub enum DMASupportedSpeed {
    CompatibilityMode,
    TypeA, // as described by the EISA
    TypeB,
    TypeF
}

#[derive(Debug, PartialEq)]
pub enum DMATransferTypePreference {
    _8BitOnly,
    _8And16Bit,
    _16Bit
}

#[derive(Debug, PartialEq)]
pub struct DMADescriptor {
    channel_mask: u8,
    supported_speeds: DMASupportedSpeed,
    is_bus_master: bool,
    transfer_type_preference: DMATransferTypePreference
}

pub fn dma_format_descriptor(bytes: &[u8]) -> Result<Resource, AmlError> {
    if bytes.len() < 3 {
        return Err(AmlError::ResourceDescriptorTooShort);
    }

    if bytes.len() > 3 {
        return Err(AmlError::ResourceDescriptorTooLong);
    }

    let channel_mask = bytes[1];
    let options = bytes[2];
    let supported_speeds = match options.get_bits(5..=6) {
        0 => DMASupportedSpeed::CompatibilityMode,
        1 => DMASupportedSpeed::TypeA,
        2 => DMASupportedSpeed::TypeB,
        3 => DMASupportedSpeed::TypeF,
        _ => unreachable!()
    };
    let is_bus_master = options.get_bit(2);
    let transfer_type_preference = match options.get_bits(0..=1) {
        0 => DMATransferTypePreference::_8BitOnly,
        1 => DMATransferTypePreference::_8And16Bit,
        2 => DMATransferTypePreference::_16Bit,
        3 => unimplemented!("Reserved DMA transfer type preference"),
        _ => unreachable!()
    };

    Ok(Resource::Dma(DMADescriptor {
        channel_mask,
        supported_speeds,
        is_bus_master,
        transfer_type_preference
    }))
}

#[derive(Debug, PartialEq)]
pub struct IOPortDescriptor {
    decodes_full_address: bool,
    memory_range: (u16, u16),
    base_alignment: u8,
    range_length: u8
}

fn io_port_descriptor(bytes: &[u8]) -> Result<Resource, AmlError> {
    if bytes.len() < 8 {
        return Err(AmlError::ResourceDescriptorTooShort);
    }

    if bytes.len() > 8 {
        return Err(AmlError::ResourceDescriptorTooLong);
    }

    let information = bytes[1];
    let decodes_full_address = information.get_bit(0);

    let memory_range_min = LittleEndian::read_u16(&bytes[2..=3]);
    let memory_range_max = LittleEndian::read_u16(&bytes[4..=5]);
    let memory_range = (memory_range_min, memory_range_max);

    let base_alignment = bytes[6];
    let range_length = bytes[7];

    Ok(Resource::IOPort(IOPortDescriptor {
        decodes_full_address,
        memory_range,
        base_alignment,
        range_length
    }))
}

fn extended_interrupt_descriptor(bytes: &[u8]) -> Result<Resource, AmlError> {
    /*
     * --- Extended Interrupt Descriptor ---
     * Byte 3 contains the Interrupt Vector Flags:
     *      Bit 0: 1 if device consumes the resource, 0 if it produces it
     *      Bit 1: 1 if edge-triggered, 0 if level-triggered
     *      Bit 2: 1 = active-high, 0 = active-low
     *      Bit 3: 1 if interrupt is shared with other devices
     *      Bit 4: 1 if this interrupt is capable of waking the system, 0 if it is not
     * Byte 4 contains the number of interrupt numbers that follow. When this descriptor is
     * returned from `_CRS` or send to `_SRS`, this field must be 1.
     *
     * From Byte 5 onwards, there are `n` interrupt numbers, each of which is encoded as a
     * 4-byte little-endian number.
     *
     * NOTE: We only support the case where there is a single interrupt number.
     */
    if bytes.len() < 9 {
        return Err(AmlError::ResourceDescriptorTooShort);
    }

    let number_of_interrupts = bytes[4] as usize;
    assert_eq!(number_of_interrupts, 1);
    let irq = LittleEndian::read_u32(&[bytes[5], bytes[6], bytes[7], bytes[8]]);

    Ok(Resource::Irq(IrqDescriptor {
        is_consumer: bytes[3].get_bit(0),
        trigger: if bytes[3].get_bit(1) { InterruptTrigger::Edge } else { InterruptTrigger::Level },
        polarity: if bytes[3].get_bit(2) { InterruptPolarity::ActiveLow } else { InterruptPolarity::ActiveHigh },
        is_shared: bytes[3].get_bit(3),
        is_wake_capable: bytes[3].get_bit(4),
        irq,
    }))
}

#[test]
fn test_parses_keyboard_crs() {
    let bytes: Vec<u8> = [71, 1, 96, 0, 96, 0, 1, 1, 71, 1, 100, 0, 100, 0, 1, 1, 34, 2, 0, 121, 0].to_vec();
    let size: u64 = bytes.len() as u64;
    let value: AmlValue = AmlValue::Buffer { bytes, size };

    let resources = resource_descriptor_list(&value).unwrap();

    assert_eq!(resources, Vec::from([
        Resource::IOPort(IOPortDescriptor { decodes_full_address: true, memory_range: (96, 96), base_alignment: 1, range_length: 1 }), 
        Resource::IOPort(IOPortDescriptor { decodes_full_address: true, memory_range: (100, 100), base_alignment: 1, range_length: 1 }), 
        Resource::Irq(IrqDescriptor { is_consumer: false, trigger: InterruptTrigger::Edge, polarity: InterruptPolarity::ActiveHigh, is_shared: false, is_wake_capable: false, irq: 2 })
    ]));
}

#[test]
fn test_pci_crs() {
    let bytes: Vec<u8> = [
        136, 13, 0, 2, 12, 0, 0, 0, 0, 0, 255, 0, 0, 0, 0, 1, 71, 1, 248, 12, 248, 12, 1, 8, 136, 13, 0, 1, 12, 3, 0, 0, 0, 0, 247, 12, 0, 0, 248, 12, 136, 13, 0, 1, 12, 3, 0, 0, 0, 13, 255, 255, 0, 0, 0, 243, 135, 23, 0, 0, 12, 3, 0, 0, 0, 0, 0, 0, 10, 0, 255, 255, 11, 0, 0, 0, 0, 0, 0, 0, 2, 0, 135, 23, 0, 0, 12, 1, 0, 0, 0, 0, 0, 0, 0, 8, 255, 255, 191, 254, 0, 0, 0, 0, 0, 0, 192, 246, 138, 43, 0, 0, 12, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 255, 255, 255, 127, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 0, 0, 0, 0, 121, 0].to_vec();
    let size: u64 = bytes.len() as u64;
    let value: AmlValue = AmlValue::Buffer { bytes, size };

    let resources = resource_descriptor_list(&value).unwrap();

    assert_eq!(resources, Vec::from([
        Resource::WordAddressSpace(AddressSpaceDescriptor { resource_type: AddressSpaceResourceType::BusNumberRange, is_maximum_address_fixed: true, is_minimum_address_fixed: true, decode_type: AddressSpaceDecodeType::Additive, granularity: 0, address_range: (0, 255) }), 
        Resource::IOPort(IOPortDescriptor { decodes_full_address: true, memory_range: (3320, 3320), base_alignment: 1, range_length: 8 }), 
        Resource::WordAddressSpace(AddressSpaceDescriptor { resource_type: AddressSpaceResourceType::IORange, is_maximum_address_fixed: true, is_minimum_address_fixed: true, decode_type: AddressSpaceDecodeType::Additive, granularity: 0, address_range: (0, 3319) }), 
        Resource::WordAddressSpace(AddressSpaceDescriptor { resource_type: AddressSpaceResourceType::IORange, is_maximum_address_fixed: true, is_minimum_address_fixed: true, decode_type: AddressSpaceDecodeType::Additive, granularity: 0, address_range: (3328, 65535) }), 
        Resource::WordAddressSpace(AddressSpaceDescriptor { resource_type: AddressSpaceResourceType::MemoryRange, is_maximum_address_fixed: true, is_minimum_address_fixed: true, decode_type: AddressSpaceDecodeType::Additive, granularity: 0, address_range: (655360, 786431) }), 
        Resource::WordAddressSpace(AddressSpaceDescriptor { resource_type: AddressSpaceResourceType::MemoryRange, is_maximum_address_fixed: true, is_minimum_address_fixed: true, decode_type: AddressSpaceDecodeType::Additive, granularity: 0, address_range: (134217728, 4273995775) }), 
        Resource::WordAddressSpace(AddressSpaceDescriptor { resource_type: AddressSpaceResourceType::MemoryRange, is_maximum_address_fixed: true, is_minimum_address_fixed: true, decode_type: AddressSpaceDecodeType::Additive, granularity: 0, address_range: (4294967296, 6442450943) })
    ]));
}

#[test]
fn test_fdc_crs() {
    let bytes: Vec<u8> = [71, 1, 242, 3, 242, 3, 0, 4, 71, 1, 247, 3, 247, 3, 0, 1, 34, 64, 0, 42, 4, 0, 121, 0].to_vec();
    let size: u64 = bytes.len() as u64;
    let value: AmlValue = AmlValue::Buffer { bytes, size };

    let resources = resource_descriptor_list(&value).unwrap();

    assert_eq!(resources, Vec::from([
        Resource::IOPort(IOPortDescriptor { decodes_full_address: true, memory_range: (1010, 1010), base_alignment: 0, range_length: 4 }), 
        Resource::IOPort(IOPortDescriptor { decodes_full_address: true, memory_range: (1015, 1015), base_alignment: 0, range_length: 1 }), 
        Resource::Irq(IrqDescriptor { is_consumer: false, trigger: InterruptTrigger::Edge, polarity: InterruptPolarity::ActiveHigh, is_shared: false, is_wake_capable: false, irq: 64 }), 
        Resource::Dma(DMADescriptor { channel_mask: 4, supported_speeds: DMASupportedSpeed::CompatibilityMode, is_bus_master: false, transfer_type_preference: DMATransferTypePreference::_8BitOnly })
    ]));
}
