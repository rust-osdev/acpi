/*
 * This is a small program that is meant for testing the AML parser on artificial
 * AML. We want to:
 *      - scan a directory for ASL files
 *      - compile them using `iasl` into AML files (these should be gitignored), but only if the ASL file has a
 *        newer timestamp than the AML file (or just compile if there isn't a corresponding AML file)
 *      - Run the AML parser on each AML file, printing test output like `cargo test` does in a nice table for
 *        each AML file
 *      - For failing tests, print out a nice summary of the errors for each file
 */

use acpi::{
    address::MappedGas,
    aml::{namespace::AmlName, object::Object, AmlError, Interpreter},
    Handle,
    PhysicalMapping,
};
use clap::{Arg, ArgAction, ArgGroup};
use log::{error, info};
use pci_types::PciAddress;
use std::{
    collections::HashSet,
    ffi::OsStr,
    fs::{self, File},
    io::{Read, Write},
    path::{Path, PathBuf},
    process::Command,
    ptr::NonNull,
    str::FromStr,
    sync::Arc,
};

enum CompilationOutcome {
    Ignored,
    IsAml(PathBuf),
    Newer(PathBuf),
    NotCompiled(PathBuf),
    Failed(PathBuf),
    Succeeded(PathBuf),
}

fn main() -> std::io::Result<()> {
    let mut cmd = clap::Command::new("aml_tester")
        .version("v0.1.0")
        .author("Isaac Woods")
        .about("Compiles and tests ASL files")
        .arg(Arg::new("no_compile").long("no-compile").action(ArgAction::SetTrue).help("Don't compile ASL to AML"))
        .arg(
            Arg::new("combined")
                .long("combined")
                .action(ArgAction::SetTrue)
                .help("Don't clear the namespace between tests"),
        )
        .arg(Arg::new("path").short('p').long("path").required(false).action(ArgAction::Set).value_name("DIR"))
        .arg(Arg::new("files").action(ArgAction::Append).value_name("FILE.{asl,aml}"))
        .group(ArgGroup::new("files_list").args(["path", "files"]).required(true));
    if std::env::args().count() <= 1 {
        cmd.print_help()?;
        return Ok(());
    }
    log::set_logger(&Logger).unwrap();
    log::set_max_level(log::LevelFilter::Trace);

    let matches = cmd.get_matches();

    // Make sure we have the ability to compile ASL -> AML, if user wants it
    let user_wants_compile = !matches.get_flag("no_compile");
    let can_compile = user_wants_compile &&
        // Test if `iasl` is installed, so we can give a good error later if it's not
        match Command::new("iasl").arg("-v").status() {
            Ok(exit_status) if exit_status.success() => true,
            Ok(exit_status) => {
                panic!("`iasl` exited with unsuccessful status: {:?}", exit_status);
            },
            Err(_) => false,
    };

    let tests = find_tests(&matches)?;
    let compiled_files: Vec<CompilationOutcome> =
        tests.iter().map(|name| resolve_and_compile(name, can_compile).unwrap()).collect();

    // Check if compilation should have happened but did not
    if user_wants_compile
        && compiled_files.iter().any(|outcome| matches!(outcome, CompilationOutcome::NotCompiled(_)))
    {
        panic!(
            "`iasl` is not installed, but we want to compile some ASL files! Pass --no-compile, or install `iasl`"
        );
    }
    // Report compilation results
    if user_wants_compile {
        let (passed, failed) = compiled_files.iter().fold((0, 0), |(passed, failed), outcome| match outcome {
            CompilationOutcome::Succeeded(_) => (passed + 1, failed),
            CompilationOutcome::Failed(_) => (passed, failed + 1),
            _ => (passed, failed),
        });
        if passed + failed > 0 {
            println!(
                "Compiled {} ASL files: {}{} passed{}, {}{} failed{}",
                passed + failed,
                termion::color::Fg(termion::color::Green),
                passed,
                termion::style::Reset,
                termion::color::Fg(termion::color::Red),
                failed,
                termion::style::Reset
            );
            println!();
        }
    }

    #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
    enum TestResult {
        /// The test passed.
        Pass,
        /// The test ASL failed compilation by `iasl`.
        CompileFail,
        /// Our interpreter failed to parse the resulting AML.
        ParseFail,
        // TODO: should we do this??
        NotCompiled,
    }

    // Make a list of the files we have processed, and skip them if we see them again
    let mut dedup_list: HashSet<PathBuf> = HashSet::new();
    let mut summaries: HashSet<(PathBuf, TestResult)> = HashSet::new();
    // Filter down to the final list of AML files
    let aml_files = compiled_files
        .iter()
        .filter_map(|outcome| match outcome {
            CompilationOutcome::IsAml(path) => Some(path.clone()),
            CompilationOutcome::Newer(path) => Some(path.clone()),
            CompilationOutcome::Succeeded(path) => Some(path.clone()),
            CompilationOutcome::Failed(path) => {
                summaries.insert((path.clone(), TestResult::CompileFail));
                None
            }
            CompilationOutcome::NotCompiled(path) => {
                summaries.insert((path.clone(), TestResult::NotCompiled));
                None
            }
            CompilationOutcome::Ignored => None,
        })
        .filter(|path| {
            if dedup_list.contains(path) {
                false
            } else {
                dedup_list.insert(path.clone());
                true
            }
        })
        .collect::<Vec<_>>();

    let combined_test = matches.get_flag("combined");
    let mut interpreter = new_interpreter();

    let (passed, failed) = aml_files.into_iter().fold((0, 0), |(passed, failed), file_entry| {
        print!("Testing AML file: {:?}... ", file_entry);
        std::io::stdout().flush().unwrap();

        let Ok(mut file) = File::open(&file_entry) else {
            summaries.insert((file_entry, TestResult::CompileFail));
            return (passed, failed + 1);
        };
        let mut contents = Vec::new();
        file.read_to_end(&mut contents).unwrap();

        if !combined_test {
            interpreter = new_interpreter();
        }

        const AML_TABLE_HEADER_LENGTH: usize = 36;
        let stream = &contents[AML_TABLE_HEADER_LENGTH..];

        match run_test(stream, &mut interpreter) {
            Ok(()) => {
                println!("{}OK{}", termion::color::Fg(termion::color::Green), termion::style::Reset);
                println!("Namespace: {}", interpreter.namespace.lock());
                summaries.insert((file_entry, TestResult::Pass));
                (passed + 1, failed)
            }

            Err(err) => {
                println!("{}Failed ({:?}){}", termion::color::Fg(termion::color::Red), err, termion::style::Reset);
                println!("Namespace: {}", interpreter.namespace.lock());
                summaries.insert((file_entry, TestResult::ParseFail));
                (passed, failed + 1)
            }
        }
    });

    // Print summaries
    println!("Summary:");
    for (file, status) in summaries.iter() {
        let status = match status {
            TestResult::Pass => {
                format!("{}OK{}", termion::color::Fg(termion::color::Green), termion::style::Reset)
            }
            TestResult::CompileFail => {
                format!("{}COMPILE FAIL{}", termion::color::Fg(termion::color::Red), termion::style::Reset)
            }
            TestResult::ParseFail => {
                format!("{}PARSE FAIL{}", termion::color::Fg(termion::color::Red), termion::style::Reset)
            }
            TestResult::NotCompiled => {
                format!("{}NOT COMPILED{}", termion::color::Fg(termion::color::Red), termion::style::Reset)
            }
        };
        println!("\t{:<50}: {}", file.to_str().unwrap(), status);
    }
    println!(
        "\nTest results: {}{} passed{}, {}{} failed{}",
        termion::color::Fg(termion::color::Green),
        passed,
        termion::style::Reset,
        termion::color::Fg(termion::color::Red),
        failed,
        termion::style::Reset
    );
    Ok(())
}

fn new_interpreter() -> Interpreter<Handler> {
    let fake_registers = Arc::new(acpi::registers::FixedRegisters {
        pm1_event_registers: acpi::registers::Pm1EventRegisterBlock {
            pm1_event_length: 8,
            pm1a: unsafe {
                MappedGas::map_gas(
                    acpi::address::GenericAddress {
                        address_space: acpi::address::AddressSpace::SystemIo,
                        bit_width: 32,
                        bit_offset: 0,
                        access_size: 1,
                        address: 0x400,
                    },
                    &Handler,
                )
                .unwrap()
            },
            pm1b: None,
        },
        pm1_control_registers: acpi::registers::Pm1ControlRegisterBlock {
            pm1a: unsafe {
                MappedGas::map_gas(
                    acpi::address::GenericAddress {
                        address_space: acpi::address::AddressSpace::SystemIo,
                        bit_width: 32,
                        bit_offset: 0,
                        access_size: 1,
                        address: 0x600,
                    },
                    &Handler,
                )
                .unwrap()
            },
            pm1b: None,
        },
    });
    let fake_facs = PhysicalMapping {
        physical_start: 0x0,
        virtual_start: NonNull::new(0x8000_0000_0000_0000 as *mut acpi::sdt::facs::Facs).unwrap(),
        region_length: 32,
        mapped_length: 32,
        handler: Handler,
    };
    Interpreter::new(Handler, 2, fake_registers, Some(fake_facs))
}

fn run_test(stream: &[u8], interpreter: &mut Interpreter<Handler>) -> Result<(), AmlError> {
    interpreter.load_table(stream)?;

    if let Some(result) = interpreter.evaluate_if_present(AmlName::from_str("\\MAIN").unwrap(), vec![])? {
        match *result {
            Object::Integer(0) => Ok(()),
            Object::Integer(other) => {
                error!("Test _MAIN returned non-zero exit code: {}", other);
                // TODO: wrong error - this should probs return a more complex err type
                Err(AmlError::NoCurrentOp)
            }
            _ => {
                error!("Test _MAIN returned unexpected object type: {}", *result);
                // TODO: wrong error
                Err(AmlError::NoCurrentOp)
            }
        }
    } else {
        Ok(())
    }
}

fn find_tests(matches: &clap::ArgMatches) -> std::io::Result<Vec<PathBuf>> {
    // Get an initial list of files - may not work correctly on non-UTF8 OsString
    let files: Vec<PathBuf> = if matches.contains_id("path") {
        let dir_path = Path::new(matches.get_one::<String>("path").unwrap());

        if std::fs::metadata(&dir_path).unwrap().is_dir() {
            println!("Running tests in directory: {:?}", dir_path);
            fs::read_dir(dir_path)?
                .filter_map(|entry| if entry.is_ok() { Some(entry.unwrap().path().to_path_buf()) } else { None })
                .collect()
        } else {
            println!("Running single test: {:?}", dir_path);
            vec![dir_path.to_path_buf()]
        }
    } else {
        matches.get_many::<PathBuf>("files").unwrap_or_default().cloned().collect()
    };

    // Make sure all files exist, propagate error if it occurs
    files.iter().fold(Ok(()), |result: std::io::Result<()>, path| {
        if !path.is_file() {
            println!("Not a regular file: {}", path.display());
            path.metadata()?;
        }
        result
    })?;

    Ok(files)
}

/// Determine what to do with this file - ignore, compile and parse, or just parse.
/// If ".aml" does not exist, or if ".asl" is newer, compiles the file.
/// If the ".aml" file is newer, indicate it is ready to parse.
fn resolve_and_compile(path: &PathBuf, can_compile: bool) -> std::io::Result<CompilationOutcome> {
    // If this file is aml and it exists, it's ready for parsing
    // metadata() will error if the file does not exist
    if path.extension() == Some(OsStr::new("aml")) && path.metadata()?.is_file() {
        return Ok(CompilationOutcome::IsAml(path.clone()));
    }

    // If this file is not asl, it's not interesting. Error if the file does not exist.
    if path.extension() != Some(OsStr::new("asl")) || !path.metadata()?.is_file() {
        return Ok(CompilationOutcome::Ignored);
    }

    let aml_path = path.with_extension("aml");

    if aml_path.is_file() {
        let asl_last_modified = path.metadata()?.modified()?;
        let aml_last_modified = aml_path.metadata()?.modified()?;
        // If the aml is more recent than the asl, use the existing aml
        // Otherwise continue to compilation
        if asl_last_modified <= aml_last_modified {
            return Ok(CompilationOutcome::Newer(aml_path));
        }
    }

    if !can_compile {
        return Ok(CompilationOutcome::NotCompiled(path.clone()));
    }

    // Compile the ASL file using `iasl`
    println!("Compiling file: {}", path.display());
    let output = Command::new("iasl").arg(path).output()?;

    if !output.status.success() {
        println!(
            "Failed to compile ASL file: {}. Output from iasl:\n {}",
            path.display(),
            String::from_utf8_lossy(&output.stderr)
        );
        Ok(CompilationOutcome::Failed(path.clone()))
    } else {
        Ok(CompilationOutcome::Succeeded(aml_path))
    }
}

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, _: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        println!("[{}] {}", record.level(), record.args());
    }

    fn flush(&self) {
        std::io::stdout().flush().unwrap();
    }
}

#[derive(Clone)]
struct Handler;

impl acpi::Handler for Handler {
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T> {
        todo!()
    }

    fn unmap_physical_region<T>(_region: &PhysicalMapping<Self, T>) {}

    fn read_u8(&self, address: usize) -> u8 {
        println!("read_u8 {address:#x}");
        0
    }
    fn read_u16(&self, address: usize) -> u16 {
        println!("read_u16 {address:#x}");
        0
    }
    fn read_u32(&self, address: usize) -> u32 {
        println!("read_u32 {address:#x}");
        0
    }
    fn read_u64(&self, address: usize) -> u64 {
        println!("read_u64 {address:#x}");
        0
    }

    fn write_u8(&self, address: usize, value: u8) {
        println!("write_u8 {address:#x}<-{value:#x}");
    }
    fn write_u16(&self, address: usize, value: u16) {
        println!("write_u16 {address:#x}<-{value:#x}");
    }
    fn write_u32(&self, address: usize, value: u32) {
        println!("write_u32 {address:#x}<-{value:#x}");
    }
    fn write_u64(&self, address: usize, value: u64) {
        println!("write_u64 {address:#x}<-{value:#x}");
    }

    fn read_io_u8(&self, port: u16) -> u8 {
        println!("read_io_u8 {port:#x}");
        0
    }
    fn read_io_u16(&self, port: u16) -> u16 {
        println!("read_io_u16 {port:#x}");
        0
    }
    fn read_io_u32(&self, port: u16) -> u32 {
        println!("read_io_u32 {port:#x}");
        0
    }

    fn write_io_u8(&self, port: u16, value: u8) {
        println!("write_io_u8 {port:#x}<-{value:#x}");
    }
    fn write_io_u16(&self, port: u16, value: u16) {
        println!("write_io_u16 {port:#x}<-{value:#x}");
    }
    fn write_io_u32(&self, port: u16, value: u32) {
        println!("write_io_u32 {port:#x}<-{value:#x}");
    }

    fn read_pci_u8(&self, address: PciAddress, _offset: u16) -> u8 {
        println!("read_pci_u8 ({address})");
        0
    }
    fn read_pci_u16(&self, address: PciAddress, _offset: u16) -> u16 {
        println!("read_pci_u16 ({address})");
        0
    }
    fn read_pci_u32(&self, address: PciAddress, _offset: u16) -> u32 {
        println!("read_pci_u32 ({address})");
        0
    }

    fn write_pci_u8(&self, address: PciAddress, _offset: u16, value: u8) {
        println!("write_pci_u8 ({address})<-{value}");
    }
    fn write_pci_u16(&self, address: PciAddress, _offset: u16, value: u16) {
        println!("write_pci_u16 ({address})<-{value}");
    }
    fn write_pci_u32(&self, address: PciAddress, _offset: u16, value: u32) {
        println!("write_pci_u32 ({address})<-{value}");
    }

    fn handle_debug(&self, object: &acpi::aml::object::Object) {
        info!("Debug store: {}", object);
    }

    fn nanos_since_boot(&self) -> u64 {
        0
    }

    fn stall(&self, microseconds: u64) {
        println!("Stalling for {}us", microseconds);
    }
    fn sleep(&self, milliseconds: u64) {
        println!("Sleeping for {}ms", milliseconds);
    }

    fn create_mutex(&self) -> Handle {
        Handle(0)
    }
    fn acquire(&self, _mutex: Handle, _timeout: u16) -> Result<(), AmlError> {
        Ok(())
    }
    fn release(&self, _mutex: Handle) {}
}
