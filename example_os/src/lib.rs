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
extern crate acpi;
extern crate bit_field;

#[macro_use]
mod serial;
mod interrupts;
mod memory;

use acpi::{AcpiHandler, PhysicalMapping as AcpiPhysicalMapping};
use alloc::heap::{GlobalAlloc, Layout};
use core::mem;
use core::ptr::NonNull;
use linked_list_allocator::LockedHeap;
use memory::{
    paging::{EntryFlags, Page}, Frame, MemoryController,
};
use serial::SerialPort;

pub const HEAP_START: usize = 0o_000_001_000_000_0000;
pub const HEAP_SIZE: usize = 100 * 1024; // 100 KiB

#[global_allocator]
static HEAP_ALLOCATOR: LockedHeap = LockedHeap::empty();

struct ExampleAcpiHandler<'a> {
    memory_controller: &'a mut MemoryController,
}

impl<'a> AcpiHandler for ExampleAcpiHandler<'a> {
    /*
     * This is not the right way to manage physical mappings! It allocates random portions of the
     * heap, remaps them to point to arbitrary physical memory and never frees anything! A real OS
     * could use something like a bump allocator to quickly manage short-lived physical mappings,
     * or even something like a bitmap or hole-tracking allocator. It should be, however,
     * completely sound.
     */
    fn map_physical_region<T>(&mut self, physical_address: usize) -> AcpiPhysicalMapping<T> {
        let &mut MemoryController {
            ref mut active_table,
            ref mut frame_allocator,
            ..
        } = self.memory_controller;

        SerialPort::write(&format!(
            "Mapping region starting: {:#x}\n",
            physical_address
        ));
        let start_frame = Frame::containing_address(physical_address);
        let end_frame = Frame::containing_address(physical_address + mem::size_of::<T>());
        let region_size = end_frame.end_address() - start_frame.start_address();

        let layout = Layout::from_size_align(region_size, PAGE_SIZE).unwrap();
        let ptr = unsafe { HEAP_ALLOCATOR.alloc(layout.clone()) };
        let start_page = Page::containing_address(ptr as usize);
        let end_page = Page::containing_address(ptr as usize + region_size - 1);

        for i in 0..(region_size / PAGE_SIZE + 1) {
            active_table.unmap(start_page + i, frame_allocator);
            active_table.map_to(
                start_page + i,
                start_frame + i,
                EntryFlags::PRESENT,
                frame_allocator,
            );
        }

        let virtual_start = ptr as usize + (physical_address - start_frame.start_address());
        AcpiPhysicalMapping {
            physical_start: start_frame.start_address(),
            virtual_start: unsafe { NonNull::new_unchecked(virtual_start as *mut _) },
            region_length: mem::size_of::<T>(),
            mapped_length: region_size,
        }
    }

    fn unmap_physical_region<T>(&mut self, _region: AcpiPhysicalMapping<T>) {
        /*
         * TODO: we just pretend to free the physical mapping. You should probably actually unmap
         * the pages and let them be used again. This also leaks heap memory.
         */
    }
}

#[no_mangle]
pub extern "C" fn rust_main(multiboot_address: usize) {
    SerialPort::init();

    let boot_info = unsafe { multiboot2::load(multiboot_address) };
    let mut memory_controller = memory::init(&boot_info);

    unsafe {
        HEAP_ALLOCATOR.lock().init(HEAP_START, HEAP_SIZE);
    }

    interrupts::init(&mut memory_controller);

    // TODO: rather than expecting a V1 tag, you should correctly check for both tags and pass the
    // correct ACPI revision to parse_rsdt
    let rsdp_tag = boot_info.rsdp_v1_tag().expect("Failed to get RSDP V1 tag");
    rsdp_tag.validate().expect("Failed to validate RSDP tag");

    let mut handler = ExampleAcpiHandler {
        memory_controller: &mut memory_controller,
    };
    match acpi::parse_rsdt(&mut handler, rsdp_tag.revision(), rsdp_tag.rsdt_address()) {
        Ok(_) => (),

        Err(err) => {
            panic!("Acpi gave error: {:?}", err);
        }
    }

    SerialPort::write("Passed");
    qemu_quit();
}

pub fn qemu_quit() -> ! {
    unsafe {
        asm!("out 0xf4, al" : : "{al}"(0x00) : : "intel", "volatile");
    }
    unreachable!();
}

#[cfg(not(test))]
#[lang = "eh_personality"]
#[no_mangle]
pub extern "C" fn eh_personality() {}

#[cfg(not(test))]
#[lang = "panic_fmt"]
#[no_mangle]
pub extern "C" fn panic_fmt(fmt: core::fmt::Arguments, file: &'static str, line: u32) -> ! {
    SerialPort::write(&format!("\n\nPANIC in {} at line {}:", file, line));
    SerialPort::write(&format!("    {}", fmt));
    qemu_quit();
}

#[lang = "oom"]
#[no_mangle]
pub extern "C" fn rust_oom() -> ! {
    panic!("OOM");
}

#[no_mangle]
pub extern "C" fn _Unwind_Resume() -> ! {
    qemu_quit();
}
