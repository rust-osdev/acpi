use crate::{parser::Propagate, AmlContext, AmlValue, Handler};
use alloc::boxed::Box;

struct TestHandler;

impl Handler for TestHandler {
    fn read_u8(&self, _address: usize) -> u8 {
        unimplemented!()
    }
    fn read_u16(&self, _address: usize) -> u16 {
        unimplemented!()
    }
    fn read_u32(&self, _address: usize) -> u32 {
        unimplemented!()
    }
    fn read_u64(&self, _address: usize) -> u64 {
        unimplemented!()
    }

    fn write_u8(&mut self, _address: usize, _value: u8) {
        unimplemented!()
    }
    fn write_u16(&mut self, _address: usize, _value: u16) {
        unimplemented!()
    }
    fn write_u32(&mut self, _address: usize, _value: u32) {
        unimplemented!()
    }
    fn write_u64(&mut self, _address: usize, _value: u64) {
        unimplemented!()
    }

    fn read_io_u8(&self, _port: u16) -> u8 {
        unimplemented!()
    }
    fn read_io_u16(&self, _port: u16) -> u16 {
        unimplemented!()
    }
    fn read_io_u32(&self, _port: u16) -> u32 {
        unimplemented!()
    }

    fn write_io_u8(&self, _port: u16, _value: u8) {
        unimplemented!()
    }
    fn write_io_u16(&self, _port: u16, _value: u16) {
        unimplemented!()
    }
    fn write_io_u32(&self, _port: u16, _value: u32) {
        unimplemented!()
    }

    fn read_pci_u8(&self, _segment: u16, _bus: u8, device: u8, _function: u8, _offset: u16) -> u8 {
        unimplemented!()
    }
    fn read_pci_u16(&self, _segment: u16, _bus: u8, device: u8, _function: u8, _offset: u16) -> u16 {
        unimplemented!()
    }
    fn read_pci_u32(&self, _segment: u16, _bus: u8, device: u8, _function: u8, _offset: u16) -> u32 {
        unimplemented!()
    }
    fn write_pci_u8(&self, _segment: u16, _bus: u8, device: u8, _function: u8, _offset: u16, _value: u8) {
        unimplemented!()
    }
    fn write_pci_u16(&self, _segment: u16, _bus: u8, device: u8, _function: u8, _offset: u16, _value: u16) {
        unimplemented!()
    }
    fn write_pci_u32(&self, _segment: u16, _bus: u8, device: u8, _function: u8, _offset: u16, _value: u32) {
        unimplemented!()
    }
}

pub(crate) fn make_test_context() -> AmlContext {
    AmlContext::new(Box::new(TestHandler), crate::DebugVerbosity::None)
}

pub(crate) macro check_err($parse: expr, $error: pat, $remains: expr) {
    match $parse {
        Ok((remains, _, result)) => panic!("Expected Err, got {:#?}. Remaining = {:#x?}", result, remains),
        Err((remains, _, Propagate::Err($error))) if *remains == *$remains => (),
        Err((remains, _, Propagate::Err($error))) => {
            panic!("Correct error, incorrect stream returned: {:#x?}", remains)
        }
        Err((_, _, err)) => panic!("Got wrong error: {:?}", err),
    }
}

pub(crate) macro check_ok($parse: expr, $expected: expr, $remains: expr) {
    match $parse {
        Ok((remains, _, ref result)) if remains == *$remains && result == &$expected => (),
        Ok((remains, _, ref result)) if result == &$expected => {
            panic!("Correct result, incorrect slice returned: {:x?}", remains)
        }
        Ok((_, _, ref result)) => panic!("Successfully parsed Ok, but it was wrong: {:#?}", result),
        Err((_, _, err)) => panic!("Expected Ok, got {:#?}", err),
    }
}

pub(crate) macro check_ok_value($parse: expr, $expected: expr, $remains: expr) {
    match $parse {
        Ok((remains, _, ref result)) if remains == *$remains && crudely_cmp_values(result, &$expected) => (),
        Ok((remains, _, ref result)) if crudely_cmp_values(result, &$expected) => {
            panic!("Correct result, incorrect slice returned: {:x?}", remains)
        }
        Ok((_, _, ref result)) => panic!("Successfully parsed Ok, but it was wrong: {:#?}", result),
        Err((_, _, err)) => panic!("Expected Ok, got {:#?}", err),
    }
}

/// This is a bad (but good for testing) way of comparing `AmlValue`s, which tests that they're exactly the same if
/// it can, and gives up if it can't. It's useful in tests to be able to see if you're getting the `AmlValue` that
/// you're expecting.
///
/// NOTE: almost all of the `AmlValue` variants are `Eq`, and so for a long time, `AmlValue` implemented `Eq`.
/// However, this is a footgun as, in the real interpreter, you rarely want to directly compare values, as you need
/// to apply the AML value conversion rules to compare them correctly. This is therefore only useful for artificial
/// testing scenarios.
pub(crate) fn crudely_cmp_values(a: &AmlValue, b: &AmlValue) -> bool {
    use crate::value::MethodCode;

    match a {
        AmlValue::Boolean(a) => match b {
            AmlValue::Boolean(b) => a == b,
            _ => false,
        },
        AmlValue::Integer(a) => match b {
            AmlValue::Integer(b) => a == b,
            _ => false,
        },
        AmlValue::String(ref a) => match b {
            AmlValue::String(ref b) => a == b,
            _ => false,
        },
        AmlValue::OpRegion { region, offset, length, parent_device } => match b {
            AmlValue::OpRegion {
                region: b_region,
                offset: b_offset,
                length: b_length,
                parent_device: b_parent_device,
            } => {
                region == b_region && offset == b_offset && length == b_length && parent_device == b_parent_device
            }
            _ => false,
        },
        AmlValue::Field { region, flags, offset, length } => match b {
            AmlValue::Field { region: b_region, flags: b_flags, offset: b_offset, length: b_length } => {
                region == b_region && flags == b_flags && offset == b_offset && length == b_length
            }
            _ => false,
        },
        AmlValue::Device => match b {
            AmlValue::Device => true,
            _ => false,
        },
        AmlValue::Method { flags, code } => match b {
            AmlValue::Method { flags: b_flags, code: b_code } => {
                if flags != b_flags {
                    return false;
                }

                match (code, b_code) {
                    (MethodCode::Aml(a), MethodCode::Aml(b)) => a == b,
                    (MethodCode::Aml(_), MethodCode::Native(_)) => false,
                    (MethodCode::Native(_), MethodCode::Aml(_)) => false,
                    (MethodCode::Native(_), MethodCode::Native(_)) => panic!("Can't compare two native methods"),
                }
            }
            _ => false,
        },
        AmlValue::Buffer(a) => match b {
            AmlValue::Buffer(b) => *a.lock() == *b.lock(),
            _ => false,
        },
        AmlValue::BufferField { buffer_data, offset, length } => match b {
            AmlValue::BufferField { buffer_data: b_buffer_data, offset: b_offset, length: b_length } => {
                alloc::sync::Arc::as_ptr(buffer_data) == alloc::sync::Arc::as_ptr(b_buffer_data)
                    && offset == b_offset
                    && length == b_length
            }
            _ => false,
        },
        AmlValue::Processor { id, pblk_address, pblk_len } => match b {
            AmlValue::Processor { id: b_id, pblk_address: b_pblk_address, pblk_len: b_pblk_len } => {
                id == b_id && pblk_address == b_pblk_address && pblk_len == b_pblk_len
            }
            _ => false,
        },
        AmlValue::Mutex { sync_level } => match b {
            AmlValue::Mutex { sync_level: b_sync_level } => sync_level == b_sync_level,
            _ => false,
        },
        AmlValue::Package(a) => match b {
            AmlValue::Package(b) => {
                for (a, b) in a.iter().zip(b) {
                    if crudely_cmp_values(a, b) == false {
                        return false;
                    }
                }

                true
            }
            _ => false,
        },
        AmlValue::PowerResource { system_level, resource_order } => match b {
            AmlValue::PowerResource { system_level: b_system_level, resource_order: b_resource_order } => {
                system_level == b_system_level && resource_order == b_resource_order
            }
            _ => false,
        },
        AmlValue::ThermalZone => match b {
            AmlValue::ThermalZone => true,
            _ => false,
        },
    }
}
