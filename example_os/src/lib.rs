#![no_std]
#![feature(lang_items)]
#![feature(const_fn, unique)]
#![feature(alloc)]
#![feature(asm)]
#![feature(naked_functions)]
#![feature(abi_x86_interrupt)]
#![feature(const_unique_new, const_atomic_usize_new)]
#![feature(allocator_api)]
#![feature(global_allocator)]
#![feature(ptr_internals)]

#[macro_use]
extern crate alloc;
extern crate multiboot2;
extern crate rlibc;
extern crate spin;
extern crate volatile;
#[macro_use]
extern crate bitflags;
extern crate x86_64;
#[macro_use]
extern crate once;
extern crate linked_list_allocator;
#[macro_use]
extern crate lazy_static;
extern crate bit_field;

#[macro_use]
mod vga_buffer;
mod interrupts;
mod memory;
mod serial;

use linked_list_allocator::LockedHeap;
use serial::SerialPort;

pub const HEAP_START: usize = 0o_000_001_000_000_0000;
pub const HEAP_SIZE: usize = 100 * 1024; // 100 KiB

#[global_allocator]
static HEAP_ALLOCATOR: LockedHeap = LockedHeap::empty();

#[no_mangle]
pub extern "C" fn rust_main(multiboot_address: usize) {
    vga_buffer::clear_screen();
    println!("Hello World{}", "!");

    SerialPort::init();
    SerialPort::send_str("Hello from QEMU");

    let boot_info = unsafe { multiboot2::load(multiboot_address) };
    let mut memory_controller = memory::init(boot_info);

    unsafe {
        HEAP_ALLOCATOR.lock().init(HEAP_START, HEAP_SIZE);
    }

    interrupts::init(&mut memory_controller);

    println!("It did not crash!");
    // We now tell QEMU to quit
    unsafe { asm!("out 0xf4, al" : : "{al}"(0x00) : : "intel", "volatile"); }
    loop {}
}

#[cfg(not(test))]
#[lang = "eh_personality"]
#[no_mangle]
pub extern "C" fn eh_personality() {}

#[cfg(not(test))]
#[lang = "panic_fmt"]
#[no_mangle]
pub extern "C" fn panic_fmt(fmt: core::fmt::Arguments, file: &'static str, line: u32) -> ! {
    println!("\n\nPANIC in {} at line {}:", file, line);
    println!("    {}", fmt);
    loop {}
}

#[lang = "oom"]
fn oom() -> ! {
    panic!("OOM");
}

#[no_mangle]
pub extern "C" fn _Unwind_Resume() -> ! {
    loop {}
}
