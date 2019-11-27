/*
 * This is a small program that is meant for testing the AML parser on artificial
 * AML. We want to:
 *      - scan a directory for ASL files
 *      - compile them using `iasl` into AML files (these should be gitignored), but only if the ASL file
 *        has a newer timestamp than the AML file (or just compile if there isn't a corresponding AML file)
 *      - Run the AML parser on each AML file, printing test output like `cargo test` does in a nice table
 *        for each AML file
 *      - For failing tests, print out a nice summary of the errors for each file
 */

use aml::AmlContext;
use clap::{App, Arg};
use std::{
    ffi::OsStr,
    fs::{self, File},
    io::Read,
    path::Path,
    process::Command,
};

fn main() -> std::io::Result<()> {
    let matches = App::new("aml_tester")
        .version("v0.1.0")
        .author("Isaac Woods")
        .about("Compiles and tests ASL files")
        .arg(Arg::with_name("path").short("p").long("path").required(true).takes_value(true))
        .get_matches();

    println!("Running tests in directory: {:?}", matches.value_of("path"));
    let dir_path = Path::new(matches.value_of("path").unwrap());

    compile_asl_files(dir_path)?;

    /*
     * Now, we find all the AML files in the directory, and try to compile them with the `aml`
     * parser.
     */
    let aml_files = fs::read_dir(dir_path)?
        .filter(|entry| {
            entry.is_ok() && entry.as_ref().unwrap().path().extension() == Some(OsStr::new("aml"))
        })
        .map(|entry| entry.unwrap());
    for file_entry in aml_files {
        println!("Testing AML file: {:?}", file_entry.path());

        let mut file = File::open(file_entry.path())?;
        let mut contents = Vec::new();
        file.read_to_end(&mut contents)?;

        const AML_TABLE_HEADER_LENGTH: usize = 36;
        let mut context = AmlContext::new();
        let result = context.parse_table(&contents[AML_TABLE_HEADER_LENGTH..]);

        println!("Result: {:?}", result);
    }

    Ok(())
}

fn compile_asl_files(dir_path: &Path) -> std::io::Result<()> {
    let asl_files = fs::read_dir(dir_path)?
        .filter(|entry| {
            entry.is_ok() && entry.as_ref().unwrap().path().extension() == Some(OsStr::new("asl"))
        })
        .map(|file| file.unwrap());

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
