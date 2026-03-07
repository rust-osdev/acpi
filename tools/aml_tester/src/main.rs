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
use aml_test_tools::{new_interpreter, resolve_and_compile, CompilationOutcome, TestResult};
use clap::{Arg, ArgAction, ArgGroup};
use colored::Colorize;
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
    log::set_max_level(log::LevelFilter::Info);

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
                "Compiled {} ASL files: {}, {}",
                passed + failed,
                format!("{} passed", passed).green(),
                format!("{} failed", failed).red(),
            );
            println!();
        }
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
    let mut interpreter = new_interpreter(Handler {});

    let (passed, failed) = aml_files.into_iter().fold((0, 0), |(passed, failed), file_entry| {
        print!("Testing AML file: {:?}... ", file_entry);
        std::io::stdout().flush().unwrap();

        if !combined_test {
            interpreter = new_interpreter(Handler {});
        }

        let result =aml_test_tools::run_test_for_file(&file_entry, &mut interpreter);
        let updates = match result {
            TestResult::Pass => {
                println!("{}", "OK".green());
                (passed + 1, failed)
            },
            TestResult::CompileFail | TestResult::ParseFail | TestResult::NotCompiled => {
                println!("{}", format!("Failed ({:?})", result).red());
                (passed, failed + 1)
            }
        };

        println!("Namespace: {}", interpreter.namespace.lock());
        summaries.insert((file_entry, TestResult::Pass));

        updates
    });

    // Print summaries
    println!("Summary:");
    for (file, status) in summaries.iter() {
        let status = match status {
            TestResult::Pass => {
                format!("{}", "OK".green())
            }
            TestResult::CompileFail => {
                format!("{}", "COMPILE FAIL".red())
            }
            TestResult::ParseFail => {
                format!("{}", "PARSE FAIL".red())
            }
            TestResult::NotCompiled => {
                format!("{}", "NOT COMPILED".red())
            }
        };
        println!("\t{:<50}: {}", file.to_str().unwrap(), status);
    }
    println!("\nTest results: {}, {}", format!("{} passed", passed).green(), format!("{} failed", failed).red());
    Ok(())
}

fn find_tests(matches: &clap::ArgMatches) -> std::io::Result<Vec<PathBuf>> {
    // Get an initial list of files - may not work correctly on non-UTF8 OsString
    let files: Vec<PathBuf> = if matches.contains_id("path") {
        let dir_path = Path::new(matches.get_one::<String>("path").unwrap());

        if fs::metadata(dir_path)?.is_dir() {
            println!("Running tests in directory: {:?}", dir_path);
            fs::read_dir(dir_path)?
                .filter_map(|entry| if let Ok(entry) = entry { Some(entry.path().to_path_buf()) } else { None })
                .collect()
        } else {
            println!("Running single test: {:?}", dir_path);
            vec![dir_path.to_path_buf()]
        }
    } else {
        matches.get_many::<String>("files").unwrap_or_default().map(PathBuf::from).collect()
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
    unsafe fn map_physical_region<T>(&self, _physical_address: usize, _size: usize) -> PhysicalMapping<Self, T> {
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

    fn handle_debug(&self, object: &Object) {
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
