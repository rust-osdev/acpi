use crate::sdt::Signature;
use core::sync::atomic::AtomicU32;

#[repr(C)]
pub struct Facs {
    pub signature: Signature,
    pub length: u32,
    pub hardware_signature: u32,
    pub firmware_waking_vector: u32,
    pub global_lock: AtomicU32,
    pub flags: u32,
    pub x_firmware_waking_vector: u64,
    pub version: u8,
    _reserved0: [u8; 3],
    pub ospm_flags: u32,
    reserved1: [u8; 24],
}
