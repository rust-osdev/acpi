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

use aml::AmlContext;
use clap::{App, Arg};
use std::{
    ffi::OsStr,
    fs::{self, File},
    io::{Read, Write},
    ops::Not,
    path::Path,
    process::Command,
};

fn main() -> std::io::Result<()> {
    log::set_logger(&Logger).unwrap();
    log::set_max_level(log::LevelFilter::Trace);

    let matches = App::new("aml_tester")
        .version("v0.1.0")
        .author("Isaac Woods")
        .about("Compiles and tests ASL files")
        .arg(Arg::with_name("path").short("p").long("path").required(true).takes_value(true))
        .arg(Arg::with_name("no_compile").long("no-compile"))
        .get_matches();

    let dir_path = Path::new(matches.value_of("path").unwrap());
    println!("Running tests in directory: {:?}", dir_path);

    if !matches.is_present("no_compile") {
        compile_asl_files(dir_path)?;
    }

    /*
     * Now, we find all the AML files in the directory, and try to compile them with the `aml`
     * parser.
     */
    let aml_files = fs::read_dir(dir_path)?
        .filter(|entry| entry.is_ok() && entry.as_ref().unwrap().path().extension() == Some(OsStr::new("aml")))
        .map(|entry| entry.unwrap());

    let (passed, failed) = aml_files.fold((0, 0), |(passed, failed), file_entry| {
        print!("Testing AML file: {:?}... ", file_entry.path());
        std::io::stdout().flush().unwrap();

        let mut file = File::open(file_entry.path()).unwrap();
        let mut contents = Vec::new();
        file.read_to_end(&mut contents).unwrap();

        const AML_TABLE_HEADER_LENGTH: usize = 36;
        let mut context = AmlContext::new();

        match context.parse_table(&contents[AML_TABLE_HEADER_LENGTH..]) {
            Ok(()) => {
                println!("{}OK{}", termion::color::Fg(termion::color::Green), termion::style::Reset);
                println!("Namespace: {:#?}", context.namespace);
                (passed + 1, failed)
            }

            Err(err) => {
                println!("{}Failed ({:?}){}", termion::color::Fg(termion::color::Red), err, termion::style::Reset);
                println!("Namespace: {:#?}", context.namespace);
                (passed, failed + 1)
            }
        }
    });

    println!("Test results: {} passed, {} failed", passed, failed);
    Ok(())
}

fn compile_asl_files(dir_path: &Path) -> std::io::Result<()> {
    let mut asl_files = fs::read_dir(dir_path)?
        .filter(|entry| entry.is_ok() && entry.as_ref().unwrap().path().extension() == Some(OsStr::new("asl")))
        .map(|file| file.unwrap())
        .peekable();

    if !asl_files.peek().is_none() {
        // Test if `iasl` is installed, so we can give a good error if it's not
        if Command::new("iasl").arg("-v").status().unwrap().success().not() {
            panic!("`iasl` is not installed, but we want to compile some ASL files! Pass --no-compile, or install `iasl`");
        }
    }

    for file in asl_files {
        let aml_path = file.path().with_extension(OsStr::new("aml"));

        /*
         * Check if an AML path with a matching last-modified date exists. If it
         * does, we don't need to compile the ASL file again.
         */
        if aml_path.is_file() {
            let asl_last_modified = file.metadata()?.modified()?;
            let aml_last_modified = aml_path.metadata()?.modified()?;

            if asl_last_modified <= aml_last_modified {
                continue;
            }
        }

        // Compile the ASL file using `iasl`
        println!("Compiling file: {}", file.path().to_str().unwrap());
        let output = Command::new("iasl").arg(file.path()).output()?;

        if !output.status.success() {
            println!(
                "Failed to compile ASL file: {}. Output from iasl: {}",
                file.path().to_str().unwrap(),
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }

    Ok(())
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
