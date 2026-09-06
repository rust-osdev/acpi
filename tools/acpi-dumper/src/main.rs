#[cfg(target_os = "linux")]
fn main() {
    use pico_args::Arguments;
    use std::{
        fs::{self, Permissions},
        os::unix::fs::PermissionsExt,
        path::PathBuf,
        process::{self, Command},
    };

    // Find output folder from command line arguments
    let mut args = Arguments::from_env();
    let name: String = match args.value_from_str("--name") {
        Ok(Some(name)) => {
            println!("output directory is: dumps/{}", name);
            name
        }
        _ => {
            eprintln!(
                "usage: acpi-dumper --name [NAME]\n\
                 the acpi tables will be dumped to the dumps/[name] directory"
            );
            process::exit(-1);
        }
    };

    // Check lshw is installed
    match Command::new("lshw").arg("-version").output() {
        Ok(output) => {
            println!(
                "found lshw version: {}",
                String::from_utf8(output.stderr)
                    .expect("lshw returned invalid utf-8")
                    .rsplit_terminator(' ')
                    .next()
                    .unwrap_or("unknown")
                    .trim_end()
            );
        }
        Err(e) => {
            eprintln!("error: lshw failed to execute ({}) - please check that it is installed", e);
            process::exit(-1);
        }
    };

    if unsafe { libc::geteuid() } != 0 {
        eprintln!("error: please re-run with root privileges");
        process::exit(-1);
    }

    // Create output folder
    let mut out_path = PathBuf::from("./dumps");
    out_path.push(name);
    fs::create_dir(&out_path).expect("failed to create output directory");
    fs::set_permissions(&out_path, Permissions::from_mode(0o777))
        .expect("failed to set permissions for output directory");

    println!("dumping acpi tables...");

    // Loop over the ACPI tables, dump their contents
    for file in fs::read_dir("/sys/firmware/acpi/tables")
        .expect("failed to access acpi tables, check permissions")
        .filter_map(|entry| entry.ok()) // Ignore failures
        .filter(|entry| entry.metadata().unwrap().is_file())
    {
        let table_signature = file.file_name().into_string().expect("invalid utf-8 file name in acpi tables");

        // All tables have a 4 letter signature (apart from SSDTs)
        if table_signature.len() == 4 || &table_signature[..4] != "SSDT" {
            // Read table
            let data = fs::read(file.path()).expect("failed to read acpi table");
            out_path.push(table_signature + ".bin");

            // Dump table to disk
            fs::write(&out_path, data).expect("failed to write acpi table");

            // Allow anyone to read or write the file
            fs::set_permissions(&out_path, Permissions::from_mode(0o666))
                .expect("failed to set permissions of dumped acpi table");
            out_path.pop();
        }
    }

    println!("done!");

    println!("capturing lshw output...");
    let lshw_info = Command::new("lshw").output().expect("failed to get hardware information from lshw");

    // Write lshw output to disk
    let lshw_info = String::from_utf8(lshw_info.stdout).expect("lshw returned invalid utf-8");
    out_path.push("lshw.txt");
    fs::write(&out_path, lshw_info).expect("failed to write lshw dump");
    fs::set_permissions(&out_path, Permissions::from_mode(0o666))
        .expect("failed to set permissions of dumped acpi table");
    out_path.pop();
    println!("done!");
}

#[cfg(not(target_os = "linux"))]
fn main() {
    std::compile_error!("acpi-dumper currently only supports linux");
}
