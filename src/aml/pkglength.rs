//! pkglength encoding and decoding.

use alloc::{vec, vec::Vec};
use bit_field::BitField;
use core::fmt::{Display, Formatter};

/// Indicates an attempt to encode a pkglength that is too long (>= 2 ^ 28).
#[derive(Clone, Debug)]
pub struct PkglengthTooLongError {}

impl Display for PkglengthTooLongError {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "pkglength too long")
    }
}

impl core::error::Error for PkglengthTooLongError {}

/// Encode a pkglength field.
///
/// This is a variable length field used to define the length of other variable-length items in
/// ACPI - see [the spec](https://uefi.org/specs/ACPI/6.6/20_AML_Specification.html#package-length-encoding)
/// for details.
///
/// This is less straightforward than it could be, because pkglength needs to include the length of
/// the pkglength field that gets output. This function adds that extra length.
///
/// Returns an error if length >= 2^28. Otherwise, returns the encoded pkglength in a vec, LSB-first.
pub fn encode(data_length: u32) -> Result<Vec<u8>, PkglengthTooLongError> {
    let extra_length = match data_length {
        0..63 => 1,
        63..0xFFE => 2,
        0xFFE..0xFFFFD => 3,
        _ => 4,
    };
    let result = encode_raw(data_length + extra_length);
    result.inspect(|result| {
        assert_eq!(result.len(), extra_length as usize);
    })
}

/// Encode a pkglength field, without taking into account the extra length of the pkglength field.
///
/// Most callers should use [`encode`] instead.
///
/// Returns an error if length >= 2^28. Otherwise, returns the encoded pkglength in a vec, LSB-first.
pub fn encode_raw(mut length: u32) -> Result<Vec<u8>, PkglengthTooLongError> {
    if length & 0xF0000000 != 0 {
        // Must be less than 2 ^ 28
        return Err(PkglengthTooLongError {});
    }

    if length < 64 {
        Ok(vec![length as u8])
    } else {
        let mut result = vec![(length & 0xF) as u8];
        length >>= 4;

        while length != 0 {
            result.push((length & 0xff) as u8);
            length >>= 8;
        }

        let num_bytes = result.len() as u8 - 1;
        result[0] |= num_bytes << 6;

        Ok(result)
    }
}

/// Decode a pkglength field from a stream
///
/// If the stream returns an error, that error is returned. Otherwise, the decoded length is
/// returned.
///
/// `stream_next` must return the next byte in the stream when called (or an error, which is passed
/// through)
pub fn decode_stream<T>(mut stream_next: impl FnMut() -> Result<u8, T>) -> Result<usize, T> {
    let lead_byte = stream_next()?;
    let byte_count = lead_byte.get_bits(6..8);
    assert!(byte_count < 4);

    if byte_count == 0 {
        Ok(lead_byte.get_bits(0..6) as usize)
    } else {
        let mut length = lead_byte.get_bits(0..4) as usize;
        for i in 0..byte_count {
            length |= (stream_next()? as usize) << (4 + i * 8);
        }
        Ok(length)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_round_trip() {
        let length: u32 = 0x12345;
        let encoded = encode_raw(length).unwrap();
        let mut i = encoded.iter();
        let decoded = decode_stream(|| i.next().copied().ok_or(())).unwrap();
        assert_eq!(decoded, length as usize);
    }

    #[test]
    fn less_than_64() {
        let length: u32 = 0x12;
        let encoded = encode_raw(length).unwrap();
        assert_eq!(encoded, vec![0x12]);

        let mut i = encoded.iter();
        let decoded = decode_stream(|| i.next().copied().ok_or(())).unwrap();
        assert_eq!(decoded, 0x12);
    }

    #[test]
    fn encodes_zero() {
        let encoded = encode_raw(0).unwrap();
        assert_eq!(encoded, vec![0]);
    }

    fn round_trip_length(length: u32) -> usize {
        let encoded = encode(length).unwrap();
        let mut i = encoded.iter();
        decode_stream(|| i.next().copied().ok_or(())).unwrap()
    }

    #[test]
    fn extra_length_correct() {
        assert_eq!(round_trip_length(62), 63); // An increase of a single byte
        // 63 bytes of payload requires two bytes of pkglength
        assert_eq!(round_trip_length(63), 65);

        // A random test:
        assert_eq!(round_trip_length(0x12345), 0x12348);

        // Simply check that there's no errors around the boundaries - this tells us the maths to
        // calculate `extra_length` is correct.
        for i in 0xFF9..0x1004 {
            encode(i).unwrap();
        }
        for i in 0xFFFF9..0x100004 {
            encode(i).unwrap();
        }
    }
}
