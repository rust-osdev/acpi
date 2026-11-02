use crate::{
    AcpiTable,
    sdt::{SdtHeader, Signature},
};
use core::{
    marker::{PhantomData, PhantomPinned},
    mem,
    pin::Pin,
    ptr,
    slice,
    str::Utf8Error,
};
use log::warn;

#[repr(C, packed)]
#[derive(Debug)]
pub struct Dmar {
    pub header: SdtHeader,
    pub host_address_width: u8,
    pub flags: u8,
    _reserved: [u8; 10],
    _pinned: PhantomPinned,
}

unsafe impl AcpiTable for Dmar {
    const SIGNATURE: Signature = Signature::DMAR;

    fn header(&self) -> &SdtHeader {
        &self.header
    }
}

impl Dmar {
    pub fn entries(self: Pin<&Self>) -> DmarEntryIter<'_> {
        let ptr = unsafe { Pin::into_inner_unchecked(self) as *const Dmar as *const u8 };
        DmarEntryIter {
            pointer: unsafe { ptr.add(mem::size_of::<Dmar>()) },
            remaining_length: self.header.length.saturating_sub(mem::size_of::<Dmar>() as u32),
            _phantom: PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct DmarEntryIter<'a> {
    pointer: *const u8,
    /*
     * The iterator can only have at most `u32::MAX` remaining bytes, because the length of the
     * whole SDT can only be at most `u32::MAX`.
     */
    remaining_length: u32,
    _phantom: PhantomData<&'a ()>,
}

#[derive(Debug)]
pub enum DmarEntry<'a> {
    Drhd(&'a DrhdEntry),
    Rmrr(&'a RmrrEntry),
    Atsr(&'a AtsrEntry),
    Rhsa(&'a RhsaEntry),
    Andd(&'a AnddEntry),
    Satc(&'a SatcEntry),
    Sidp(&'a SidpEntry),
}

impl<'a> Iterator for DmarEntryIter<'a> {
    type Item = DmarEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.remaining_length > 0 {
            if (self.remaining_length as usize) < size_of::<DmarEntryHeader>() {
                warn!("Invalid DMAR entry. Too short for DMAR entry header. Ignoring");
                return None;
            }

            let entry_pointer = self.pointer;
            let header = unsafe { *(self.pointer as *const DmarEntryHeader) };

            if (header.length as usize) < size_of::<DmarEntryHeader>() {
                let entry_type = header.entry_type;
                warn!(
                    "Invalid entry of type {} in DMAR - header length shorter than header itself. Ignoring",
                    entry_type
                );
                return None;
            }
            if header.length as u32 > self.remaining_length {
                let entry_type = header.entry_type;
                warn!("Invalid entry of type {} in DMAR - extending past length of table. Ignoring", entry_type);
                return None;
            }

            self.pointer = unsafe { self.pointer.byte_offset(header.length as isize) };
            self.remaining_length = self.remaining_length.saturating_sub(header.length as u32);

            macro_rules! construct_entry {
                ($entry_type:expr,
                 $entry_pointer:expr,
                 $(($value:expr => $variant:path as $type:ty)),*
                ) => {
                    match $entry_type {
                        $(
                            $value => {
                                if (header.length as usize) < size_of::<$type>() {
                                    warn!(
                                        "Invalid entry of type {} in DMAR - shorter than {}. Ignoring",
                                        $value,
                                        stringify!($type)
                                    );
                                    return None;
                                }
                                return Some($variant(unsafe {
                                    &*($entry_pointer as *const $type)
                                }))
                            }
                         )*

                        /*
                         * These entry types are reserved by the ACPI standard. We should skip them
                         * if they appear in a real DMAR.
                         */
                        0x7..=0xffff => {}
                    }
                }
            }

            #[rustfmt::skip]
            construct_entry!(
                header.entry_type,
                entry_pointer,
                (0x0 => DmarEntry::Drhd as DrhdEntry),
                (0x1 => DmarEntry::Rmrr as RmrrEntry),
                (0x2 => DmarEntry::Atsr as AtsrEntry),
                (0x3 => DmarEntry::Rhsa as RhsaEntry),
                (0x4 => DmarEntry::Andd as AnddEntry),
                (0x5 => DmarEntry::Satc as SatcEntry),
                (0x6 => DmarEntry::Sidp as SidpEntry)
            );
        }

        None
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct DmarEntryHeader {
    pub entry_type: u16,
    pub length: u16,
}

macro_rules! device_scopes {
    ($type:ty) => {
        impl $type {
            pub fn device_scopes(&self) -> DeviceScopeIter<'_> {
                let ptr = self as *const $type as *const u8;
                DeviceScopeIter {
                    pointer: unsafe { ptr.add(mem::size_of::<$type>()) },
                    remaining_length: self.header.length.saturating_sub(mem::size_of::<$type>() as u16),
                    _phantom: PhantomData,
                }
            }
        }
    };
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct DrhdEntry {
    pub header: DmarEntryHeader,
    pub flags: u8,
    pub size: u8,
    pub segment_number: u16,
    pub register_base_address: u64,
}

device_scopes!(DrhdEntry);

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct RmrrEntry {
    pub header: DmarEntryHeader,
    _reserved: u16,
    pub segment_number: u16,
    pub reserved_memory_region_base_address: u64,
    pub reserved_memory_region_limit_address: u64,
}

device_scopes!(RmrrEntry);

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct AtsrEntry {
    pub header: DmarEntryHeader,
    pub flags: u8,
    _reserved: u8,
    pub segment_number: u16,
}

device_scopes!(AtsrEntry);

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct RhsaEntry {
    pub header: DmarEntryHeader,
    _reserved: u32,
    pub register_base_address: u64,
    pub proximity_domain: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct AnddEntry {
    pub header: DmarEntryHeader,
    _reserved: [u8; 3],
    pub acpi_device_number: u8,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DmarAnddAcpiObjectNameStringError {
    Unterminated,
    InvalidBounds,
    Utf8(Utf8Error),
}

impl AnddEntry {
    pub fn acpi_object_name(&self) -> Result<&str, DmarAnddAcpiObjectNameStringError> {
        // string length is implicit
        let name_offset = size_of::<AnddEntry>();
        let name_length = (self.header.length as usize)
            .checked_sub(name_offset)
            .ok_or(DmarAnddAcpiObjectNameStringError::InvalidBounds)?;

        let start = ptr::from_ref(self).cast::<u8>();
        let bytes = unsafe {
            let str_start = start.add(name_offset);
            slice::from_raw_parts(str_start, name_length)
        };
        // check for null-termination, also means there has to be at least one byte
        if *bytes.last().ok_or(DmarAnddAcpiObjectNameStringError::Unterminated)? != 0x00 {
            return Err(DmarAnddAcpiObjectNameStringError::Unterminated);
        }
        // parse string without null-terminator
        str::from_utf8(&bytes[..bytes.len() - 1]).map_err(DmarAnddAcpiObjectNameStringError::Utf8)
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct SatcEntry {
    pub header: DmarEntryHeader,
    pub flags: u8,
    _reserved: u8,
    pub segment_number: u16,
}

device_scopes!(SatcEntry);

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct SidpEntry {
    pub header: DmarEntryHeader,
    _reserved: u16,
    pub segment_number: u16,
}

device_scopes!(SidpEntry);

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct DeviceScopePathComponent {
    pub device: u8,
    pub function: u8,
}

#[derive(Clone, Copy, Debug)]
#[repr(C, packed)]
pub struct DeviceScope {
    pub scope_type: u8,
    pub length: u8,
    pub flags: u8,
    _reserved: u8,
    pub enumeration_id: u8,
    pub start_bus_number: u8,
}

impl DeviceScope {
    pub fn path(&self) -> Result<&[DeviceScopePathComponent], DeviceScopePathComponentError> {
        let path_offset = size_of::<DeviceScope>();
        let path_length = self.length as usize - path_offset;

        if !path_length.is_multiple_of(2) {
            return Err(DeviceScopePathComponentError::OddByteNumber);
        }

        let start = unsafe { ptr::from_ref(self).cast::<u8>().add(path_offset) };
        let array: &[DeviceScopePathComponent] =
            unsafe { slice::from_raw_parts(start.cast::<DeviceScopePathComponent>(), path_length / 2) };
        Ok(array)
    }
}

#[derive(Debug)]
pub struct DeviceScopeIter<'a> {
    pointer: *const u8,
    remaining_length: u16,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> Iterator for DeviceScopeIter<'a> {
    type Item = &'a DeviceScope;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_length == 0 {
            return None;
        }

        if (self.remaining_length as usize) < size_of::<DeviceScope>() {
            warn!("Invalid Device Scope. Too short for header. Ignoring");
            return None;
        }

        let header = unsafe { (self.pointer as *const DeviceScope).as_ref() }.unwrap();

        if (header.length as usize) < size_of::<DeviceScope>() {
            let scope_type = header.scope_type;
            warn!("Invalid device scope of type {} in DMAR - shorter than scope header. Ignoring", scope_type);
            return None;
        }
        if header.length as u16 > self.remaining_length {
            let scope_type = header.scope_type;
            warn!(
                "Invalid device scope of type {} in DMAR - extending past length of table. Ignoring",
                scope_type
            );
            return None;
        }

        self.pointer = unsafe { self.pointer.byte_offset(header.length as isize) };
        self.remaining_length = self.remaining_length.saturating_sub(header.length as u16);

        Some(header)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DeviceScopePathComponentError {
    OddByteNumber,
}
