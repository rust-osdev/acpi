use crate::{AmlContext, Handler};
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
    AmlContext::new(Box::new(TestHandler), false, crate::DebugVerbosity::None)
}

pub(crate) macro check_err($parse: expr, $error: pat, $remains: expr) {
    match $parse {
        Ok((remains, _, result)) => panic!("Expected Err, got {:#?}. Remaining = {:#x?}", result, remains),
        Err((remains, _, $error)) if *remains == *$remains => (),
        Err((remains, _, $error)) => panic!("Correct error, incorrect stream returned: {:#x?}", remains),
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
