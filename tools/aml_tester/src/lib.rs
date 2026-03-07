pub use acpi::aml::{AmlError, Interpreter};
use acpi::{
    address::MappedGas,
    aml::{namespace::AmlName, object::Object},
    Handler,
    PhysicalMapping,
};
use log::error;
use std::{
    ffi::OsStr,
    fs::File,
    io::Read,
    path::PathBuf,
    process::Command,
    ptr::NonNull,
    str::FromStr,
    sync::Arc,
};

pub enum CompilationOutcome {
    Ignored,
    IsAml(PathBuf),
    Newer(PathBuf),
    NotCompiled(PathBuf),
    Failed(PathBuf),
    Succeeded(PathBuf),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum TestResult {
    /// The test passed.
    Pass,
    /// The test ASL failed compilation by `iasl`.
    CompileFail,
    /// Our interpreter failed to parse the resulting AML.
    ParseFail,
    // TODO: should we do this??
    NotCompiled,
}

/// Determine what to do with this file - ignore, compile and parse, or just parse.
/// If ".aml" does not exist, or if ".asl" is newer, compiles the file.
/// If the ".aml" file is newer, indicate it is ready to parse.
pub fn resolve_and_compile(path: &PathBuf, can_compile: bool) -> std::io::Result<CompilationOutcome> {
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

pub fn new_interpreter<T>(handler: T) -> Interpreter<T>
where
    T: Handler + Clone,
{
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
                    &handler,
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
                    &handler,
                )
                .unwrap()
            },
            pm1b: None,
        },
    });

    // This PhysicalMapping is dropped when the interpreter is dropped, and if you use logging in
    // the handler object you'll see a call to Handler::unmap_physical_region without any
    // corresponding call to Interpreter::map_physical_region.
    let fake_facs = PhysicalMapping {
        physical_start: 0x0,
        virtual_start: NonNull::new(0x8000_0000_0000_0000 as *mut acpi::sdt::facs::Facs).unwrap(),
        region_length: 32,
        mapped_length: 32,
        handler: handler.clone(),
    };
    Interpreter::new(handler, 2, fake_registers, Some(fake_facs))
}

pub fn run_test_for_file(file: &PathBuf, interpreter: &mut Interpreter<impl Handler + Clone>) -> TestResult {
    let Ok(mut file) = File::open(&file) else {
        return TestResult::CompileFail;
    };
    let mut contents = Vec::new();
    file.read_to_end(&mut contents).unwrap();

    const AML_TABLE_HEADER_LENGTH: usize = 36;
    let stream = &contents[AML_TABLE_HEADER_LENGTH..];

    match run_test(stream, interpreter) {
        Ok(()) => TestResult::Pass,
        Err(e) => {
            error!("Error running test: {:?}", e);
            TestResult::ParseFail
        }
    }
}

pub fn run_test(stream: &[u8], interpreter: &mut Interpreter<impl Handler + Clone>) -> Result<(), AmlError> {
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
