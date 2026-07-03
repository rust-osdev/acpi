//! A simple executable to check that the `acpi` crate can be used in a no-alloc environment.
//!
//! This was born out of a comment in [issue 311](https://github.com/rust-osdev/acpi/issues/311)
//! that it didn't work.
//!
//! This executable should build, but actually attempting to run it will result in an access
//! violation. It is enough that it builds successfully.
//!
//! Heavily adapted from the
//! [example by zulinx86](https://zenn.dev/zulinx86/articles/rust-nostd-101)

#![no_std]
#![no_main]

mod null_handler;

use core::{arch::asm, panic::PanicInfo};

use null_handler::NullHandler;

fn sys_exit(status: i32) -> ! {
    unsafe {
        asm!(
        "syscall",
        in("rax") 60,
        in("rdi") status,
        options(noreturn)
        );
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn _start() -> ! {
    use ::acpi::{AcpiTables, sdt::madt::Madt};

    // Clearly actually executing this will result in an access violation. The point is to check that it compiles.
    let tables = unsafe { AcpiTables::from_rsdp(NullHandler {}, 0xd1e as usize) }
        .unwrap_or_else(|x| panic!("Failed to parse RSDP table: {:?}", x));

    let mut _madt = tables.find_table::<Madt>().expect("Failed to find MADT");

    _madt.get().entries().for_each(|_entry| {});

    // Just in case it doesn't die already!
    sys_exit(0);
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}
