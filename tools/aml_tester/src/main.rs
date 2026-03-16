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

use acpi::Handler;
use aml_test_tools::{
    handlers::{logging_handler::LoggingHandler, null_handler::NullHandler},
    new_interpreter,
    resolve_and_compile,
    CompilationOutcome,
    RunTestResult,
    TestFailureReason,
    TestResult,
};
use clap::{Arg, ArgAction, ArgGroup};
use colored::Colorize;
use std::{
    collections::HashSet,
    fs::{self},
    io::Write,
    path::{Path, PathBuf},
    process::Command,
};
use std::process::ExitCode;

/// The result of a test, with all other information (error codes etc.) stripped away. This value
/// can then be stored in a Set - the test results with more info cannot.
#[derive(Eq, Hash, PartialEq)]
enum FinalTestResult {
    Passed,
    Failed,
    NotCompiled,
    CompileFailed,
}

impl From<TestResult> for FinalTestResult {
    fn from(outcome: TestResult) -> Self {
        match outcome {
            TestResult::Pass => FinalTestResult::Passed,
            TestResult::Failed(e) => match e {
                TestFailureReason::CompileFail | TestFailureReason::FilesystemErr => {
                    FinalTestResult::CompileFailed
                }
                _ => FinalTestResult::Failed,
            },
            TestResult::Panicked => FinalTestResult::Failed,
        }
    }
}

fn main() -> ExitCode {
    pretty_env_logger::init();

    let mut cmd = clap::Command::new("aml_tester")
        .version("v0.1.0")
        .author("Isaac Woods")
        .about(
            "Compiles ASL files and checks that they can be parsed by the ACPI crate.
If the ASL contains a MAIN method, it will be executed.",
        )
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
        cmd.print_help().unwrap();
        return ExitCode::SUCCESS;
    }
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

    let tests = find_tests(&matches).unwrap();
    let compiled_files: Vec<CompilationOutcome> =
        tests.iter().map(|name| resolve_and_compile(name, can_compile)).collect();

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

    let mut passed = 0u32;
    let mut failed = 0u32;

    // Make a list of the files we have processed, and skip them if we see them again
    let mut dedup_list: HashSet<PathBuf> = HashSet::new();
    let mut summaries: HashSet<(PathBuf, FinalTestResult)> = HashSet::new();
    // Filter down to the final list of AML files
    let aml_files = compiled_files
        .iter()
        .filter_map(|outcome| match outcome {
            CompilationOutcome::IsAml(path)
            | CompilationOutcome::Newer(path)
            | CompilationOutcome::Succeeded(path) => Some(path.clone()),
            CompilationOutcome::Failed(path)
            | CompilationOutcome::NotCompiled(path)
            | CompilationOutcome::FilesystemErr(path) => {
                summaries.insert((path.clone(), FinalTestResult::NotCompiled));
                failed += 1;
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
    let mut interpreter = new_interpreter(new_handler());

    for file_entry in aml_files {
        print!("Testing AML file: {:?}... ", file_entry);
        std::io::stdout().flush().unwrap();

        let result = aml_test_tools::run_test_for_file(&file_entry, interpreter);
        let simple_result: FinalTestResult = TestResult::from(&result).into();

        let interpreter_returned = match result {
            RunTestResult::Pass(i) => {
                println!("{}", "OK".green());
                passed += 1;
                Some(i)
            }
            RunTestResult::Failed(i, e) => {
                println!("{}", format!("Failed ({:?})", e).red());
                failed += 1;
                Some(i)
            }
            RunTestResult::Panicked => {
                println!("{}", "Panicked".red());
                failed += 1;
                if combined_test {
                    // We can't continue to use the old Interpreter, and the user specifically wants
                    // to. Creating a new one most likely invalidates the test they were trying to
                    // perform.
                    panic!("Interpreter panicked during combined test, unable to continue.");
                }
                None
            }
        };

        interpreter = match interpreter_returned {
            _ if !combined_test => new_interpreter(new_handler()),
            None => new_interpreter(new_handler()),
            Some(i) => i,
        };

        println!("Namespace: {}", interpreter.namespace.lock());
        summaries.insert((file_entry, simple_result));
    }

    // Print summaries
    println!("Summary:");
    for (file, status) in summaries.iter() {
        let status = match status {
            FinalTestResult::Passed => {
                format!("{}", "OK".green())
            }
            FinalTestResult::CompileFailed => {
                format!("{}", "COMPILE FAIL".red())
            }
            FinalTestResult::Failed => {
                format!("{}", "PARSE FAIL".red())
            }
            FinalTestResult::NotCompiled => {
                format!("{}", "NOT COMPILED".red())
            }
        };
        println!("\t{:<50}: {}", file.to_str().unwrap(), status);
    }
    println!("\nTest results: {}, {}", format!("{} passed", passed).green(), format!("{} failed", failed).red());
    if failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
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

fn new_handler() -> impl Handler {
    LoggingHandler::new(NullHandler {})
}
