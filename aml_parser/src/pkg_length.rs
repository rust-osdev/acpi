use crate::{
    parser::{take, take_n, Parser},
    AmlError,
};
use bit_field::BitField;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct PkgLength {
    pub raw_length: u32,
    /// The distance from the end of the structure this `PkgLength` refers to, and the end of the
    /// stream.
    pub end_offset: u32,
}

impl PkgLength {
    pub fn from_raw_length(stream: &[u8], raw_length: u32) -> PkgLength {
        PkgLength { raw_length, end_offset: stream.len() as u32 - raw_length }
    }

    /// Returns `true` if the given stream is still within the structure this `PkgLength` refers
    /// to.
    pub fn still_parsing(&self, stream: &[u8]) -> bool {
        stream.len() as u32 > self.end_offset
    }
}

pub fn pkg_length<'a>() -> impl Parser<'a, PkgLength> {
    move |input: &'a [u8]| {
        let (new_input, raw_length) = raw_pkg_length().parse(input)?;

        /*
         * NOTE: we use the original input here, because `raw_length` includes the length of the
         * `PkgLength`.
         */
        Ok((new_input, PkgLength::from_raw_length(input, raw_length)))
    }
}

/// Parses a `PkgLength` and returns the *raw length*. If you want an instance of `PkgLength`, use
/// `pkg_length` instead.
pub fn raw_pkg_length<'a>() -> impl Parser<'a, u32> {
    /*
     * PkgLength := PkgLeadByte |
     * <PkgLeadByte ByteData> |
     * <PkgLeadByte ByteData ByteData> |
     * <PkgLeadByte ByteData ByteData ByteData>
     *
     * The length encoded by the PkgLength includes the number of bytes used to encode it.
     */
    move |input: &'a [u8]| {
        let (new_input, lead_byte) = take().parse(input)?;
        let byte_count = lead_byte.get_bits(6..8);

        if byte_count == 0 {
            let length = u32::from(lead_byte.get_bits(0..6));
            return Ok((new_input, length));
        }

        let (new_input, length): (&[u8], u32) = match take_n(byte_count as usize).parse(new_input) {
            Ok((new_input, bytes)) => {
                let initial_length = u32::from(lead_byte.get_bits(0..4));
                (
                    new_input,
                    bytes.iter().enumerate().fold(initial_length, |length, (i, &byte)| {
                        length + (u32::from(byte) << (4 + i * 8))
                    }),
                )
            }

            /*
             * The stream was too short. We return an error, making sure to return the
             * *original* stream (that we haven't consumed any of).
             */
            Err((_, err)) => return Err((input, AmlError::UnexpectedEndOfStream)),
        };

        Ok((new_input, length))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{test_utils::*, AmlError};

    fn test_correct_pkglength(stream: &[u8], expected_raw_length: u32, expected_leftover: &[u8]) {
        check_ok!(
            pkg_length().parse(stream),
            PkgLength::from_raw_length(stream, expected_raw_length),
            &expected_leftover
        );
    }

    #[test]
    fn test_raw_pkg_length() {
        check_ok!(raw_pkg_length().parse(&[0b01000101, 0x14]), 325, &[]);
        check_ok!(raw_pkg_length().parse(&[0b01000111, 0x14, 0x46]), 327, &[0x46]);
        check_ok!(raw_pkg_length().parse(&[0b10000111, 0x14, 0x46]), 287047, &[]);
    }

    #[test]
    fn test_pkg_length() {
        check_err!(pkg_length().parse(&[]), AmlError::UnexpectedEndOfStream, &[]);
        test_correct_pkglength(&[0x00], 0, &[]);
        test_correct_pkglength(
            &[0x05, 0xf5, 0x7f, 0x3e, 0x54, 0x03],
            5,
            &[0xf5, 0x7f, 0x3e, 0x54, 0x03],
        );
        check_err!(
            pkg_length().parse(&[0b11000000, 0xff, 0x4f]),
            AmlError::UnexpectedEndOfStream,
            &[0b11000000, 0xff, 0x4f]
        );
    }

    #[test]
    #[should_panic]
    fn not_enough_stream() {
        /*
         * TODO: Ideally, this shouldn't panic the parser, but return a `UnexpectedEndOfStream`.
         */
        test_correct_pkglength(&[0x05, 0xf5], 5, &[0xf5]);
    }
}
