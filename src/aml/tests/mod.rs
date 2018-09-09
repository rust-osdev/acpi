mod seabios_dsdt;

use log::{Log, Metadata, Record};
use std::string::ToString;
use {AcpiHandler, PhysicalMapping};

struct TestHandler;

impl AcpiHandler for TestHandler {
    fn map_physical_region<T>(
        &mut self,
        physical_address: usize,
        size: usize,
    ) -> PhysicalMapping<T> {
        unimplemented!();
    }

    fn unmap_physical_region<T>(&mut self, region: PhysicalMapping<T>) {
        unimplemented!();
    }
}

struct TestLogger;

impl Log for TestLogger {
    fn enabled(&self, _: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        println!("[{:<5}] {}", record.level().to_string(), record.args());
    }

    fn flush(&self) {}
}
