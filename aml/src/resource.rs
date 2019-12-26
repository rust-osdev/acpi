use crate::{value::AmlValue, AmlError};
use bit_field::BitField;
use byteorder::{ByteOrder, LittleEndian};

#[derive(Debug)]
pub enum Resource {
    Irq(IrqDescriptor),
}

/// Parse a `ResourceDescriptor`. Returns `AmlError::IncompatibleValueConversion` if the passed value is not a
/// `Buffer`.
pub(crate) fn resource_descriptor(descriptor: &AmlValue) -> Result<Resource, AmlError> {
    if let AmlValue::Buffer { bytes, size } = descriptor {
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
            let length = LittleEndian::read_u16(&[bytes[1], bytes[2]]);

            match descriptor_type {
                0x01 => unimplemented!(),
                0x02 => unimplemented!(),
                0x03 => unimplemented!(),
                0x04 => unimplemented!(),
                0x05 => unimplemented!(),
                0x06 => unimplemented!(),
                0x07 => unimplemented!(),
                0x08 => unimplemented!(),
                0x09 => extended_interrupt_descriptor(bytes),
                0x0a => unimplemented!(),
                0x0b => unimplemented!(),
                0x0c => unimplemented!(),
                0x0d => unimplemented!(),
                0x0e => unimplemented!(),
                0x0f => unimplemented!(),
                0x10 => unimplemented!(),
                0x11 => unimplemented!(),
                0x12 => unimplemented!(),

                0x00 | 0x13..=0x7f => Err(AmlError::ReservedResourceType),
                0x80..=0xff => unreachable!(),
            }
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
            unimplemented!()
        }
    } else {
        Err(AmlError::IncompatibleValueConversion)
    }
}

#[derive(Debug)]
pub enum InterruptTrigger {
    Edge,
    Level,
}

#[derive(Debug)]
pub enum InterruptPolarity {
    ActiveHigh,
    ActiveLow,
}

#[derive(Debug)]
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
