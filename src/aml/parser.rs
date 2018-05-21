use super::{opcodes, AmlError};
use alloc::String;
use bit_field::BitField;
use core::str;
use {Acpi, AcpiHandler};

pub struct AmlStream<'a> {
    data: &'a [u8],
    offset: u32, // TODO: PkgLength can't be longer than u32, but can a whole AML stream?
}

impl<'a> AmlStream<'a> {
    pub unsafe fn new(data: &'a [u8]) -> AmlStream<'a> {
        AmlStream { data, offset: 0 }
    }

    pub fn peek(&mut self) -> Result<u8, AmlError> {
        if (self.offset + 1) >= self.len() {
            Err(AmlError::EndOfStream)
        } else {
            Ok(self.data[self.offset as usize])
        }
    }

    pub fn next(&mut self) -> Result<u8, AmlError> {
        let byte = self.peek()?;
        self.offset += 1;
        Ok(byte)
    }

    pub fn len(&self) -> u32 {
        self.data.len() as u32
    }

    /// This gets the current offset into the stream
    pub fn offset(&self) -> u32 {
        self.offset
    }
}

pub(crate) struct AmlParser<'s, 'a, 'h, H>
where
    'h: 'a,
    H: AcpiHandler + 'h,
{
    acpi: &'a mut Acpi<'h, H>,
    scope: String,
    stream: AmlStream<'s>,
}

impl<'s, 'a, 'h, H> AmlParser<'s, 'a, 'h, H>
where
    'h: 'a,
    H: AcpiHandler + 'h,
{
    pub(crate) fn parse(
        acpi: &'a mut Acpi<'h, H>,
        scope: &str,
        stream: AmlStream<'s>,
    ) -> Result<(), AmlError> {
        let mut parser = AmlParser {
            acpi,
            scope: String::from(scope),
            stream,
        };

        let end_offset = parser.stream.len() as u32;
        parser.parse_term_list(end_offset)
    }

    fn consume<F>(&mut self, predicate: F) -> Result<u8, AmlError>
    where
        F: Fn(u8) -> bool,
    {
        let byte = self.stream.next()?;

        match predicate(byte) {
            true => Ok(byte),
            false => Err(AmlError::UnexpectedByte(byte)),
        }
    }

    fn consume_opcode(&mut self, opcode: u8) -> Result<(), AmlError> {
        self.consume(matches_char(opcode))?;
        Ok(())
    }

    fn consume_ext_opcode(&mut self, ext_opcode: u8) -> Result<(), AmlError> {
        self.consume(matches_char(opcodes::EXT_OPCODE_PREFIX))?;
        self.consume(matches_char(ext_opcode))?;
        Ok(())
    }

    fn parse_term_list(&mut self, end_offset: u32) -> Result<(), AmlError> {
        /*
         * TermList := Nothing | <TermObj TermList>
         *
         * TermLists are always within an explicit-length structure, so we can just parse to the
         * end of the PkgLength. TODO: check this is true
         */

        /*
         * We parse until we reach the offset marked as the end of the structure - `end_offset`
         */
        while self.stream.offset() <= end_offset {
            self.parse_term_object()?;
        }

        Ok(())
    }

    fn parse_term_object(&mut self) -> Result<(), AmlError> {
        trace!("Parsing term object");
        /*
         * TermObj := NameSpaceModifierObj | NamedObj | Type1Opcode | Type2Opcode
         * NameSpaceModifierObj := DefAlias | DefName | DefScope
         */
        match self.stream.peek()? {
            opcodes::SCOPE_OP => {
                self.parse_scope_op()?;
            }

            byte => return Err(AmlError::UnexpectedByte(byte)),
        }

        Ok(())
    }

    fn parse_scope_op(&mut self) -> Result<(), AmlError> {
        trace!("Parsing scope op");
        /*
         * DefScope := 0x10 PkgLength NameString TermList
         */
        self.consume_opcode(opcodes::SCOPE_OP)?;
        let scope_end_offset = self.parse_pkg_length()?;
        // let pkg_length_handle = self.stream.start_explicit_length_parse(pkg_length);

        let name_string = self.parse_name_string()?;
        let term_list = self.parse_term_list(scope_end_offset)?;
        Ok(())
    }

    /// Parse a PkgLength. Returns the offset into the stream to stop parsing whatever object the
    /// PkgLength describes at.
    fn parse_pkg_length(&mut self) -> Result<u32, AmlError> {
        /*
         * PkgLength := PkgLeadByte |
         *              <PkgLeadByte ByteData> |
         *              <PkgLeadByte ByteData ByteData> |
         *              <PkgLeadByte ByteData ByteData ByteData>
         */
        let lead_byte = self.stream.next()?;
        let byte_data_count = lead_byte.get_bits(6..8);

        if byte_data_count == 0 {
            return Ok(u32::from(lead_byte.get_bits(0..6)));
        }

        let mut length = u32::from(lead_byte.get_bits(0..4));
        for i in 0..byte_data_count {
            length += u32::from(self.stream.next()?) << (4 + i * 8);
        }

        // Minus `byte_data_count + 1` to not include the PkgLength in the remaining bytes
        let end_offset = self.stream.offset() + length - byte_data_count as u32 - 1;
        trace!(
            "Parsed PkgLength with length {}, so ends at {}(current offset={})",
            length,
            end_offset,
            self.stream.offset()
        );
        Ok(end_offset)
    }

    fn parse_name_string(&mut self) -> Result<String, AmlError> {
        /*
         * NameString := <RootChar('\') NamePath> | <PrefixPath NamePath>
         * PrefixPath := Nothing | <'^' PrefixPath>
         */
        match self.stream.peek()? {
            b'\\' => {
                /*
                 * NameString := RootChar NamePath
                 */
                self.stream.next()?;
                Ok(String::from("\\") + &self.parse_name_path()?)
            }

            b'^' => {
                unimplemented!();
            }

            _ => self.parse_name_path(),
        }
    }

    fn parse_name_path(&mut self) -> Result<String, AmlError> {
        /*
         * NamePath := NameSeg | DualNamePath | MultiNamePath | NullPath
         * DualNamePath := DualNamePrefix NameSeg NameSeg
         * MultiNamePath := MultiNamePrefix SegCount{ByteData} NameSeg(..SegCount)
         */
        match self.stream.peek()? {
            opcodes::NULL_NAME => {
                self.stream.next()?;
                Ok(String::from(""))
            }

            opcodes::DUAL_NAME_PREFIX => {
                self.stream.next()?;
                let first = self.parse_name_seg()?;
                let second = self.parse_name_seg()?;

                Ok(
                    String::from(str::from_utf8(&first).unwrap())
                        + str::from_utf8(&second).unwrap(),
                )
            }

            opcodes::MULTI_NAME_PREFIX => {
                // TODO
                unimplemented!();
            }

            _ => Ok(String::from(
                str::from_utf8(&self.parse_name_seg()?).unwrap(),
            )),
        }
    }

    fn parse_name_seg(&mut self) -> Result<[u8; 4], AmlError> {
        /*
         * NameSeg := <LeadNameChar NameChar NameChar NameChar>
         */
        Ok([
            self.consume(is_lead_name_char)?,
            self.consume(is_name_char)?,
            self.consume(is_name_char)?,
            self.consume(is_name_char)?,
        ])
    }
}

fn matches_char(byte: u8) -> impl Fn(u8) -> bool {
    move |x| x == byte
}

fn is_lead_name_char(byte: u8) -> bool {
    (byte >= b'A' && byte <= b'Z') || byte == b'_'
}

fn is_digit_char(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}

fn is_name_char(byte: u8) -> bool {
    is_lead_name_char(byte) || is_digit_char(byte)
}
