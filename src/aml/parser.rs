use super::stream::AmlStream;
use super::value::{AmlValue, FieldFlags, MethodFlags, RegionSpace};
use super::{opcodes, AmlError};
use alloc::string::String;
use bit_field::BitField;
use core::str;
use {Acpi, AcpiHandler};

/// This is used internally by the parser to keep track of what we know about a field before we can
/// add it to the namespace.
struct FieldInfo {
    pub name: String,
    pub length: u64,
}

/// This is used internally by the parser. Often, we're only interested in offset of the end of the
/// current explicit-length structure, so we know when to stop parsing it. However, various constants
/// (e.g. the size of fields) are also encoded as PkgLengths, and so sometimes we want to access
/// the raw data as well.
struct PkgLength {
    pub raw_length: u32,
    pub end_offset: u32,
}

pub(crate) struct AmlParser<'s, 'a, 'h, H>
where
    H: AcpiHandler + 'h,
{
    acpi: &'a mut Acpi,
    handler: &'h mut H,
    scope: String,
    stream: AmlStream<'s>,
}

/// This macro takes a parser and one or more parsing functions and tries to parse the next part of
/// the stream with each one. If a parsing function fails, it rolls back the stream and tries the
/// next one. If none of the functions can parse the next part of the stream, we error on the
/// unexpected byte.
macro parse_any_of($parser: expr, $($function: path),+) {
    $(if let Some(value) = $parser.attempt_parse($function)? {
        Ok(value)
    } else)+ {
        warn!("Didn't parse any of!");
        Err(AmlError::UnexpectedByte($parser.stream.peek()?))
    }
}

/// This macro wraps parselets that check if we're parsing the thing we're attempting to. It should
/// be used within `attempt_parse` calls, and converts `AmlError::UnexpectedByte` errors into
/// `AmlError::NeedsBacktrack`s.
macro check_attempt($parselet: expr) {
    let parse_result = $parselet;
    if let Err(AmlError::UnexpectedByte(_)) = parse_result {
        Err(AmlError::NeedsBacktrack)
    } else {
        parse_result
    }?
}

impl<'s, 'a, 'h, H> AmlParser<'s, 'a, 'h, H>
where
    H: AcpiHandler,
{
    pub(crate) fn parse(
        acpi: &'a mut Acpi,
        handler: &'h mut H,
        scope: &str,
        stream: AmlStream<'s>,
    ) -> Result<(), AmlError> {
        let mut parser = AmlParser {
            acpi,
            handler,
            scope: String::from(scope),
            stream,
        };

        let end_offset = parser.stream.len() as u32;
        parser.parse_term_list(end_offset)
    }

    /// Consume the next byte in the stream and check if it fulfils the given predicate. If it
    /// does, returns `Ok(the consumed char)`. If it doesn't, returns
    /// `Err(AmlError::UnexpectedByte(the consumed char)`. If there's an error consuming the char,
    /// it will forward the error on from that.
    fn consume_byte<F>(&mut self, predicate: F) -> Result<u8, AmlError>
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
        self.consume_byte(matches_byte(opcode))?;
        Ok(())
    }

    fn consume_ext_opcode(&mut self, ext_opcode: u8) -> Result<(), AmlError> {
        self.consume_byte(matches_byte(opcodes::EXT_OPCODE_PREFIX))?;
        self.consume_byte(matches_byte(ext_opcode))?;
        Ok(())
    }

    /// Try to parse the next part of the stream with the given parsing function. This returns any
    /// `AmlError` as an `Err`, except `AmlError::UnexpectedByte`, to which it will return
    /// `Ok(None)`. A successful parse gives `Ok(Some(...))`. On failure, this also reverts any
    /// changes made to the stream, so it's as if the parsing function never run.
    fn attempt_parse<T, F>(&mut self, parsing_function: F) -> Result<Option<T>, AmlError>
    where
        F: Fn(&mut Self) -> Result<T, AmlError>,
    {
        let stream = self.stream.clone();

        match parsing_function(self) {
            Ok(result) => Ok(Some(result)),

            Err(AmlError::NeedsBacktrack) => {
                self.stream = stream;
                Ok(None)
            }

            Err(error) => Err(error),
        }
    }

    fn parse_term_list(&mut self, end_offset: u32) -> Result<(), AmlError> {
        /*
         * TermList := Nothing | <TermObj TermList>
         *
         * Because TermLists don't have PkgLengths, we pass the offset to stop at from whatever
         * explicit-length object we were parsing before.
         */
        trace!("--> TermList");
        while self.stream.offset() <= end_offset {
            self.parse_term_object()?;
        }

        trace!("<-- TermList");
        Ok(())
    }

    fn parse_term_object(&mut self) -> Result<(), AmlError> {
        /*
         * TermObj := NameSpaceModifierObj | NamedObj | Type1Opcode | Type2Opcode
         * NameSpaceModifierObj := DefAlias | DefName | DefScope
         * NamedObj := DefBankField | DefCreateBitField | DefCreateByteField | DefCreateDWordField |
         *             DefCreateField | DefCreateQWordField | DefCreateWordField | DefDataRegion |
         *             DefExternal | DefOpRegion | DefPowerRes | DefProcessor | DefThermalZone |
         *             DefMethod | DefDevice
         */
        trace!("--> TermObj");
        let result = parse_any_of!(
            self,
            AmlParser::parse_def_name,
            AmlParser::parse_def_scope,
            AmlParser::parse_def_op_region,
            AmlParser::parse_def_field,
            AmlParser::parse_def_method,
            AmlParser::parse_def_device // AmlParser::parse_type1_opcode    TODO: reenable when we can parse them
        )?;
        trace!("<-- TermObj");
        Ok(result)
    }

    fn parse_def_name(&mut self) -> Result<(), AmlError> {
        /*
         * DefName := 0x08 NameString DataRefObject
         */
        check_attempt!(self.consume_opcode(opcodes::NAME_OP));
        trace!("--> DefName");
        let name = self.parse_name_string()?;
        let data_ref_object = self.parse_data_ref_object()?;

        // TODO: insert into namespace

        trace!(
            "<-- DefName(name = {}, data_ref_object = {:?})",
            name,
            data_ref_object
        );
        Ok(())
    }

    fn parse_def_scope(&mut self) -> Result<(), AmlError> {
        /*
         * DefScope := 0x10 PkgLength NameString TermList
         */
        check_attempt!(self.consume_opcode(opcodes::SCOPE_OP));
        trace!("--> DefScope");
        let scope_end_offset = self.parse_pkg_length()?.end_offset;

        let name_string = self.parse_name_string()?;
        let containing_scope = self.scope.clone();

        self.scope = name_string;
        let term_list = self.parse_term_list(scope_end_offset)?;
        self.scope = containing_scope;

        trace!("<-- DefScope");
        Ok(())
    }

    fn parse_def_op_region(&mut self) -> Result<(), AmlError> {
        /*
         * DefOpRegion := ExtOpPrefix 0x80 NameString RegionSpace RegionOffset RegionLen
         * RegionSpace := ByteData (where 0x00      = SystemMemory
         *                                0x01      = SystemIO
         *                                0x02      = PciConfig
         *                                0x03      = EmbeddedControl
         *                                0x04      = SMBus
         *                                0x05      = SystemCMOS
         *                                0x06      = PciBarTarget
         *                                0x07      = IPMI
         *                                0x08      = GeneralPurposeIO
         *                                0x09      = GenericSerialBus
         *                                0x80-0xff = OEM Defined)
         * ByteData := 0x00 - 0xff
         * RegionOffset := TermArg => Integer
         * RegionLen := TermArg => Integer
         */
        check_attempt!(self.consume_ext_opcode(opcodes::EXT_OP_REGION_OP));
        trace!("--> DefOpRegion");

        let name = self.parse_name_string()?;
        let region_space = match self.stream.next()? {
            0x00 => RegionSpace::SystemMemory,
            0x01 => RegionSpace::SystemIo,
            0x02 => RegionSpace::PciConfig,
            0x03 => RegionSpace::EmbeddedControl,
            0x04 => RegionSpace::SMBus,
            0x05 => RegionSpace::SystemCmos,
            0x06 => RegionSpace::PciBarTarget,
            0x07 => RegionSpace::IPMI,
            0x08 => RegionSpace::GeneralPurposeIo,
            0x09 => RegionSpace::GenericSerialBus,
            space @ 0x80..=0xff => RegionSpace::OemDefined(space),
            byte => return Err(AmlError::UnexpectedByte(byte)),
        };
        let offset = self.parse_term_arg()?.as_integer()?;
        let length = self.parse_term_arg()?.as_integer()?;

        // Insert it into the namespace
        let namespace_path = self.resolve_path(&name)?;
        self.acpi.namespace.insert(
            namespace_path,
            AmlValue::OpRegion {
                region: region_space,
                offset,
                length,
            },
        );

        trace!(
            "<-- DefOpRegion(name = {}, space = {:?}, offset = {}, length = {})",
            name,
            region_space,
            offset,
            length
        );
        Ok(())
    }

    fn parse_def_field(&mut self) -> Result<(), AmlError> {
        /*
         * DefField = ExtOpPrefix 0x81 PkgLength NameString FieldFlags FieldList
         * FieldList := Nothing | <FieldElement FieldList>
         * FieldElement := NamedField | ReservedField | AccessField | ExtendedAccessField |
         *                 ConnectField
         * ReservedField := 0x00 PkgLength
         * AccessField := 0x01 AccessType AccessAttrib
         * AccessType := ByteData
         * AccessAttrib := ByteData
         * ConnectField := <0x02 NameString> | <0x02 BufferData>
         */
        check_attempt!(self.consume_ext_opcode(opcodes::EXT_FIELD_OP));
        trace!("--> DefField");
        let end_offset = self.parse_pkg_length()?.end_offset;
        trace!("end offset: {}", end_offset);
        let name = self.parse_name_string()?;
        trace!("name: {}", name);
        let flags = FieldFlags::new(self.stream.next()?);
        trace!("Field flags: {:?}", flags);

        while self.stream.offset() < end_offset {
            // TODO: parse other field types
            let info = parse_any_of!(self, AmlParser::parse_named_field)?;

            // TODO: add field name to this (info.name)?
            let namespace_path = self.resolve_path(&name)?;
            self.acpi.namespace.insert(
                namespace_path,
                AmlValue::Field {
                    flags,
                    offset: 0, // TODO: calculate offset
                    length: info.length,
                },
            );
        }

        trace!("<-- DefField");
        Ok(())
    }

    fn parse_named_field(&mut self) -> Result<FieldInfo, AmlError> {
        /*
         * NamedField := NameSeg PkgLength
         *
         * This encodes the size of the field using a PkgLength - it doesn't mark the length of an
         * explicit-length structure!
         */
        trace!("--> NamedField");
        let name = String::from(name_seg_to_string(&self.parse_name_seg()?)?);
        let length = self.parse_pkg_length()?.raw_length as u64;
        info!("Adding named field called {:?} with size {}", name, length);

        trace!("<-- NamedField");
        Ok(FieldInfo { name, length })
    }

    fn parse_def_method(&mut self) -> Result<(), AmlError> {
        /*
         * DefMethod := 0x14 PkgLength NameString MethodFlags TermList
         * MethodFlags := ByteData
         */
        check_attempt!(self.consume_opcode(opcodes::METHOD_OP));
        trace!("--> DefMethod");
        let end_offset = self.parse_pkg_length()?.end_offset;
        let name = self.parse_name_string()?;
        let flags = MethodFlags::new(self.stream.next()?);

        /*
         * The next item of the method is a TermList, but we don't parse it! Instead, we extract
         * the slice of bytes to the end offset, and keep it. When we want to execute the method,
         * we interpret that TermList.
         *
         * TODO: do we want to store all the AML for the control methods on the kernel heap like
         * this, or keep the mappings for the DSDT and SSDTs around and these be references to
         * slices? Both have advantages
         */
        let term_list = self
            .stream
            .take_n(end_offset - self.stream.offset())?
            .to_vec();

        trace!("Parsed method called {}", name);
        self.acpi.namespace.insert(
            name,
            AmlValue::Method {
                flags,
                code: term_list,
            },
        );

        trace!("<-- DefMethod");
        Ok(())
    }

    fn parse_def_device(&mut self) -> Result<(), AmlError> {
        /*
         * DefDevice := ExtOpPrefix 0x82 PkgLength NameString TermList
         */
        check_attempt!(self.consume_ext_opcode(opcodes::EXT_DEVICE_OP));
        trace!("--> DefDevice");
        let end_offset = self.parse_pkg_length()?.end_offset;
        trace!("end offset: {}", end_offset); // TODO: remove
        let name = self.parse_name_string()?;
        trace!("name: {}", name);
        self.parse_term_list(end_offset);
        // TODO: think about how to handle TermList namespacing (pass a name root?)
        // TODO: add to namespace

        trace!("<-- DefDevice");
        Ok(())
    }

    fn parse_def_buffer(&mut self) -> Result<AmlValue, AmlError> {
        trace!("--> DefBuffer");
        unimplemented!(); // TODO
    }

    fn parse_term_arg(&mut self) -> Result<AmlValue, AmlError> {
        /*
         * TermArg := Type2Opcode | DataObject | ArgObj | LocalObj
         */
        trace!("--> TermArg");
        let result = parse_any_of!(self, AmlParser::parse_data_object)?;
        trace!("<-- TermArg");
        Ok(result)
    }

    fn parse_data_ref_object(&mut self) -> Result<AmlValue, AmlError> {
        /*
         * DataRefObject := DataObject | ObjectReference | DDBHandle
         * DataObject := ComputationalData | DefPackage | DefVarPackage
         */
        trace!("--> DataRefObject");
        let result = parse_any_of!(self, AmlParser::parse_data_object)?;
        trace!("<-- DataRefObject");
        Ok(result)
    }

    fn parse_data_object(&mut self) -> Result<AmlValue, AmlError> {
        /*
         * DataObject := ComputationalData | DefPackage | DefVarPackage
         */
        trace!("--> DataObject");
        let result = parse_any_of!(self, AmlParser::parse_computational_data)?;
        trace!("<-- DataObject");
        Ok(result)
    }

    fn parse_computational_data(&mut self) -> Result<AmlValue, AmlError> {
        /*
         * ComputationalData := ByteConst | WordConst | DWordConst | QWordConst | String |
         *                      ConstObj | RevisionOp | DefBuffer
         * ByteConst := 0x0a ByteData
         * WordConst := 0x0b WordData
         * DWordConst := 0x0c DWordData
         * QWordConst := 0x0e QWordData
         * String := 0x0d AsciiCharList NullChar
         * ConstObj := ZeroOp(0x00) | OneOp(0x01) | OnesOp(0xff)
         * RevisionOp := ExtOpPrefix(0x5B) 0x30
         */
        trace!("--> ComputationalData");
        match self.stream.peek()? {
            opcodes::BYTE_CONST => {
                self.consume_opcode(opcodes::BYTE_CONST)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(self.stream.next()? as u64))
            }

            opcodes::WORD_CONST => {
                self.consume_opcode(opcodes::WORD_CONST)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(self.stream.next_u16()? as u64))
            }

            opcodes::DWORD_CONST => {
                self.consume_opcode(opcodes::DWORD_CONST)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(self.stream.next_u32()? as u64))
            }

            opcodes::QWORD_CONST => {
                self.consume_opcode(opcodes::QWORD_CONST)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(self.stream.next_u64()? as u64))
            }

            opcodes::STRING_PREFIX => {
                self.consume_opcode(opcodes::STRING_PREFIX)?;
                trace!("<-- ComputationalData");
                unimplemented!(); // TODO
            }

            opcodes::ZERO_OP => {
                self.consume_opcode(opcodes::ZERO_OP)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(0))
            }

            opcodes::ONE_OP => {
                self.consume_opcode(opcodes::ONE_OP)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(1))
            }

            opcodes::ONES_OP => {
                self.consume_opcode(opcodes::ONES_OP)?;
                trace!("<-- ComputationalData");
                Ok(AmlValue::Integer(u64::max_value()))
            }

            opcodes::EXT_OPCODE_PREFIX if self.stream.lookahead(1)? == opcodes::EXT_REVISION_OP => {
                unimplemented!(); // TODO
            }

            _ => self.parse_def_buffer(),
        }
    }

    fn parse_type1_opcode(&mut self) -> Result<(), AmlError> {
        /*
         * Type1Opcode := DefBreak | DefBreakPoint | DefContinue | DefFatal | DefIfElse | DefLoad |
         *                DefNoop | DefNotify | DefRelease | DefReset | DefReturn | DefSignal |
         *                DefSleep | DefStall | DefUnload | DefWhile
         */
        unimplemented!(); // TODO
    }

    /// Parse a PkgLength. Returns the offset into the stream to stop parsing whatever object the
    /// PkgLength describes at.
    fn parse_pkg_length(&mut self) -> Result<PkgLength, AmlError> {
        /*
         * PkgLength := PkgLeadByte |
         *              <PkgLeadByte ByteData> |
         *              <PkgLeadByte ByteData ByteData> |
         *              <PkgLeadByte ByteData ByteData ByteData>
         *
         * The length encoded by the PkgLength includes the number of bytes used to encode it.
         */
        trace!("--> PkgLength");
        let current_offset = self.stream.offset();

        let lead_byte = self.stream.next()?;
        let byte_data_count = lead_byte.get_bits(6..8);

        if byte_data_count == 0 {
            let length = u32::from(lead_byte.get_bits(0..6));
            trace!("<-- PkgLength");
            return Ok(PkgLength {
                raw_length: length,
                end_offset: current_offset + length,
            });
        }

        let mut length = u32::from(lead_byte.get_bits(0..4));
        for i in 0..byte_data_count {
            length += u32::from(self.stream.next()?) << (4 + i * 8);
        }

        trace!("<-- PkgLength");
        Ok(PkgLength {
            raw_length: length,
            end_offset: current_offset + length,
        })
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
            self.consume_byte(is_lead_name_char)?,
            self.consume_byte(is_name_char)?,
            self.consume_byte(is_name_char)?,
            self.consume_byte(is_name_char)?,
        ])
    }

    /// Resolve a given path and the current scope to an absolute path in the namespace.
    fn resolve_path(&mut self, mut path: &str) -> Result<String, AmlError> {
        /*
         * TODO: how should we handle '.' as they appear in paths?
         */
        let original_path = path.clone();

        // If the scope to resolve is from the root of the namespace, or the current scope is
        // nothing, just return the given scope
        if self.scope == "" || path.starts_with("\\") {
            return Ok(String::from(path));
        }

        // "^"s at the start of a path specify to go up one level from the current scope, to its
        // parent object
        let mut namespace_object = self.scope.clone();
        while path.starts_with("^") {
            path = &path[1..];

            if namespace_object.pop() == None {
                return Err(AmlError::InvalidPath(String::from(original_path)));
            }
        }

        Ok(namespace_object + &path)
    }
}

fn matches_byte(byte: u8) -> impl Fn(u8) -> bool {
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

fn name_seg_to_string<'a>(seg: &'a [u8; 4]) -> Result<&'a str, AmlError> {
    match str::from_utf8(seg) {
        Ok(seg_str) => Ok(seg_str),
        Err(_) => Err(AmlError::InvalidNameSeg(*seg)),
    }
}
