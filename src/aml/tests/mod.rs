use aml::parser::AmlParser;
use aml::stream::AmlStream;
use log::{Log, Metadata, Record};
use std::string::ToString;
use std::{collections::BTreeMap, vec::Vec};
use {Acpi, AcpiHandler, PhysicalMapping};

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

impl TestLogger {
    fn init() {
        ::log::set_boxed_logger(box TestLogger).unwrap();
        ::log::set_max_level(::log::Level::Trace.to_level_filter());
    }
}

impl Log for TestLogger {
    fn enabled(&self, _: &Metadata) -> bool {
        true
    }

    fn log(&self, record: &Record) {
        println!("[{:<5}] {}", record.level().to_string(), record.args());
    }

    fn flush(&self) {}
}

#[test]
pub fn parse_seabios_dsdt() {
    const DATA: &[u8] = include!("seabios_dsdt.hex");

    TestLogger::init();

    let mut acpi = Acpi {
        acpi_revision: 0,
        namespace: BTreeMap::new(),
        boot_processor: None,
        application_processors: Vec::new(),
        interrupt_model: None,
    };

    let mut handler = TestHandler;
    let stream = unsafe { AmlStream::new(DATA) };

    match AmlParser::parse(&mut acpi, &mut handler, "\\", stream) {
        Ok(_) => (),
        Err(error) => panic!("{:?}", error),
    }
}
