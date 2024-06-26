#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate aml;

use std::sync::atomic::{AtomicBool, Ordering};

static INITIALIZED: AtomicBool = AtomicBool::new(false);

fuzz_target!(|data: &[u8]| {
    if let Ok(false) = INITIALIZED.compare_exchange(false, true, Ordering::Relaxed, Ordering::Relaxed) {
        simplelog::SimpleLogger::init(simplelog::LevelFilter::Trace, simplelog::Config::default()).unwrap();
    }

    let mut context = aml::AmlContext::new(Box::new(Handler), aml::DebugVerbosity::None);
    let _ = context.parse_table(data);
});

struct Handler;

impl aml::Handler for Handler {
    fn read_u8(&self, _address: usize) -> u8 {
        0
    }
    fn read_u16(&self, _address: usize) -> u16 {
        0
    }
    fn read_u32(&self, _address: usize) -> u32 {
        0
    }
    fn read_u64(&self, _address: usize) -> u64 {
        0
    }

    fn write_u8(&mut self, _address: usize, _value: u8) {}
    fn write_u16(&mut self, _address: usize, _value: u16) {}
    fn write_u32(&mut self, _address: usize, _value: u32) {}
    fn write_u64(&mut self, _address: usize, _value: u64) {}

    fn read_io_u8(&self, _port: u16) -> u8 {
        0
    }
    fn read_io_u16(&self, _port: u16) -> u16 {
        0
    }
    fn read_io_u32(&self, _port: u16) -> u32 {
        0
    }

    fn write_io_u8(&self, _port: u16, _value: u8) {}
    fn write_io_u16(&self, _port: u16, _value: u16) {}
    fn write_io_u32(&self, _port: u16, _value: u32) {}

    fn read_pci_u8(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16) -> u8 {
        0
    }
    fn read_pci_u16(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16) -> u16 {
        0
    }
    fn read_pci_u32(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16) -> u32 {
        0
    }
    fn write_pci_u8(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16, _value: u8) {}
    fn write_pci_u16(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16, _value: u16) {}
    fn write_pci_u32(&self, _segment: u16, _bus: u8, _device: u8, _function: u8, _offset: u16, _value: u32) {}

    fn stall(&self, _microseconds: u64) {}
    fn sleep(&self, _milliseconds: u64) {}
}
