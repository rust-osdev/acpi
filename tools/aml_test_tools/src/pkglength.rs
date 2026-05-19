//! pkglength encoding.

use std::{vec, vec::Vec};

/// Encode a pkglength field.
///
/// This is a variable length field used to define the length of other variable-length items in
/// ACPI - see [the spec](https://uefi.org/specs/ACPI/6.6/20_AML_Specification.html#package-length-encoding)
/// for details.
///
/// This is less straightforward than it could be, because pkglength needs to include the length of
/// the pkglength field that gets output. This function adds that extra length.
///
/// Panics if length >= 2^28. Otherwise, returns the encoded pkglength in a vec, LSB-first.
pub fn encode(data_length: u32) -> Vec<u8> {
    let extra_length = match data_length {
        0..63 => 1,
        63..0xFFE => 2,
        0xFFE..0xFFFFD => 3,
        _ => 4,
    };
    let result = encode_raw(data_length + extra_length);

    // Check the table above is correct.
    assert_eq!(result.len(), extra_length as usize);

    result
}

/// Encode a pkglength field, without taking into account the extra length of the pkglength field.
///
/// Most callers should use [`encode`] instead.
///
/// Panics if length >= 2^28. Otherwise, returns the encoded pkglength in a vec, LSB-first.
pub fn encode_raw(mut length: u32) -> Vec<u8> {
    assert_eq!(length & 0xF0000000, 0);

    if length < 64 {
        vec![length as u8]
    } else {
        let mut result = vec![(length & 0xF) as u8];
        length >>= 4;

        while length != 0 {
            result.push((length & 0xff) as u8);
            length >>= 8;
        }

        let num_bytes = result.len() as u8 - 1;
        result[0] |= num_bytes << 6;

        result
    }
}
