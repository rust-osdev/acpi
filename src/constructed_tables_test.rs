/// These tests cover ideal sets of ACPI tables, which we construct on the fly. Eventually, this
/// should cover all compliant implementations, but does not guarantee we'll be able to parse a
/// particular hardware's tables, obviously.

use std::mem;
use std::ptr::NonNull;
use std::boxed::Box;
use super::{AcpiHandler, AcpiInfo, PhysicalMapping, rsdp::Rsdp};

const OEM_ID : &[u8; 6] = b"RUST  ";

/*
 * We use fake physical addresses to track what is being requested. When a particular table or
 * resource is requested, we just allocate it on the heap and return the "virtual address"
 * (a pointer onto the heap).
 */
const RSDP_ADDRESS : usize = 0x0;
const RSDT_ADDRESS : usize = 0x1;

struct TestHandler;

impl AcpiHandler for TestHandler
{
    fn map_physical_region<T>(&mut self, physical_address : usize) -> PhysicalMapping<T>
    {
        match physical_address
        {
            RSDP_ADDRESS =>
            {
                let rsdp = Rsdp::make_testcase(*b"RSD PTR ",
                                               None,
                                               *OEM_ID,
                                               0,
                                               RSDT_ADDRESS as u32,
                                               0,
                                               0x0,
                                               None,
                                               [0, 0, 0]
                                              );

                PhysicalMapping
                {
                    physical_start  : RSDP_ADDRESS,
                    virtual_start   : unsafe
                                      {
                                          NonNull::<T>::new_unchecked(Box::into_raw(Box::new(rsdp)) as *mut T)
                                      },
                    region_length   : mem::size_of::<Rsdp>(),
                    mapped_length   : mem::size_of::<Rsdp>(),
                }
            },

            _ => panic!("ACPI requested invalid physical address: {:#x}", physical_address),
        }
    }

    fn unmap_physical_region<T>(&mut self, region : PhysicalMapping<T>)
    {
        match region.physical_start
        {
            RSDP_ADDRESS =>
            {
                let _ = unsafe { Box::from_raw(region.virtual_start.as_ptr()) };
            },

            address => panic!("ACPI tried to unmap a region not created by test harness: {:#x}", address),
        }
    }
}

#[test]
fn test_constructed_tables()
{
    let mut test_handler = TestHandler;
    match AcpiInfo::parse_rsdp(&mut test_handler, RSDP_ADDRESS)
    {
        Ok(info) =>
        {
            assert_eq!(info.revision, 0);
        },

        Err(err) =>
        {
            panic!("Failed to parse ACPI: {:#?}", err);
        },
    }
}
