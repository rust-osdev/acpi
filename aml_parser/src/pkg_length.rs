use crate::{
    parser::{take, take_n, Parser},
    AmlError,
};
use bit_field::BitField;
use log::trace;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct PkgLength(u32);

pub fn pkg_length<'a>() -> impl Parser<'a, PkgLength> {
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
            return Ok((new_input, PkgLength(length)));
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

        Ok((new_input, PkgLength(length)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{test_utils::*, AmlError};

    #[test]
    fn test_pkg_length() {
        check_err!(pkg_length().parse(&[]), AmlError::UnexpectedEndOfStream, &[]);
        check_ok!(pkg_length().parse(&[0x00]), PkgLength(0), &[]);
        check_ok!(pkg_length().parse(&[0x05, 0xf5, 0x7f]), PkgLength(5), &[0xf5, 0x7f]);
        check_ok!(pkg_length().parse(&[0b01000101, 0x14]), PkgLength(325), &[]);
        check_ok!(pkg_length().parse(&[0b01000111, 0x14, 0x46]), PkgLength(327), &[0x46]);
        check_ok!(pkg_length().parse(&[0b10000111, 0x14, 0x46]), PkgLength(287047), &[]);
        check_err!(
            pkg_length().parse(&[0b11000000, 0xff, 0x4f]),
            AmlError::UnexpectedEndOfStream,
            &[0b11000000, 0xff, 0x4f]
        );
    }
}
