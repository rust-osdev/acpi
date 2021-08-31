use crate::{
    misc::{arg_obj, debug_obj, local_obj, ArgNum, LocalNum},
    namespace::{AmlName, NameComponent},
    opcode::{opcode, DUAL_NAME_PREFIX, MULTI_NAME_PREFIX, NULL_NAME, PREFIX_CHAR, ROOT_CHAR},
    parser::{choice, comment_scope, consume, n_of, take, take_while, Parser, Propagate},
    AmlContext,
    AmlError,
    DebugVerbosity,
};
use alloc::vec::Vec;
use core::{fmt, str};

/// Produced by the `Target`, `SimpleName`, and `SuperName` parsers
#[derive(Clone, Debug)]
pub enum Target {
    Null,
    Name(AmlName),
    Debug,
    Arg(ArgNum),
    Local(LocalNum),
}

pub fn target<'a, 'c>() -> impl Parser<'a, 'c, Target>
where
    'c: 'a,
{
    /*
     * Target := SuperName | NullName
     * NullName := 0x00
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "Target",
        choice!(null_name().map(|_| Ok(Target::Null)), super_name()),
    )
}

pub fn super_name<'a, 'c>() -> impl Parser<'a, 'c, Target>
where
    'c: 'a,
{
    /*
     * SuperName := SimpleName | DebugObj | ReferenceTypeOpcode
     * TODO: this doesn't cover ReferenceTypeOpcode yet
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "SuperName",
        choice!(debug_obj().map(|()| Ok(Target::Debug)), simple_name()),
    )
}

pub fn simple_name<'a, 'c>() -> impl Parser<'a, 'c, Target>
where
    'c: 'a,
{
    /*
     * SimpleName := NameString | ArgObj | LocalObj
     */
    comment_scope(
        DebugVerbosity::AllScopes,
        "SimpleName",
        choice!(
            arg_obj().map(|arg_num| Ok(Target::Arg(arg_num))),
            local_obj().map(|local_num| Ok(Target::Local(local_num))),
            name_string().map(move |name| Ok(Target::Name(name)))
        ),
    )
}

pub fn name_string<'a, 'c>() -> impl Parser<'a, 'c, AmlName>
where
    'c: 'a,
{
    /*
     * NameString := <RootChar('\') NamePath> | <PrefixPath NamePath>
     * PrefixPath := Nothing | <'^' PrefixPath>
     */
    let root_name_string = opcode(ROOT_CHAR).then(name_path()).map(|((), ref name_path)| {
        let mut name = alloc::vec![NameComponent::Root];
        name.extend_from_slice(name_path);
        Ok(AmlName::from_components(name))
    });

    let prefix_path =
        take_while(opcode(PREFIX_CHAR)).then(name_path()).map(|(num_prefix_chars, ref name_path)| {
            let mut name = alloc::vec![NameComponent::Prefix; num_prefix_chars];
            name.extend_from_slice(name_path);
            Ok(AmlName::from_components(name))
        });

    // TODO: combinator to select a parser based on a peeked byte?
    comment_scope(DebugVerbosity::AllScopes, "NameString", move |input: &'a [u8], context| {
        let first_char = match input.first() {
            Some(&c) => c,
            None => return Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream))),
        };

        match first_char {
            ROOT_CHAR => root_name_string.parse(input, context),
            PREFIX_CHAR => prefix_path.parse(input, context),
            _ => name_path()
                .map(|path| {
                    if path.len() == 0 {
                        return Err(Propagate::Err(AmlError::EmptyNamesAreInvalid));
                    }

                    Ok(AmlName::from_components(path))
                })
                .parse(input, context),
        }
    })
}

pub fn name_path<'a, 'c>() -> impl Parser<'a, 'c, Vec<NameComponent>>
where
    'c: 'a,
{
    /*
     * NamePath := NullName | DualNamePath | MultiNamePath | NameSeg
     */
    choice!(
        null_name(),
        dual_name_path(),
        multi_name_path(),
        name_seg().map(|seg| Ok(alloc::vec![NameComponent::Segment(seg)]))
    )
}

pub fn null_name<'a, 'c>() -> impl Parser<'a, 'c, Vec<NameComponent>>
where
    'c: 'a,
{
    /*
     * NullName := 0x00
     */
    opcode(NULL_NAME).map(|_| Ok(Vec::with_capacity(0)))
}

pub fn dual_name_path<'a, 'c>() -> impl Parser<'a, 'c, Vec<NameComponent>>
where
    'c: 'a,
{
    /*
     * DualNamePath := 0x2e NameSeg NameSeg
     */
    opcode(DUAL_NAME_PREFIX).then(name_seg()).then(name_seg()).map(|(((), first), second)| {
        Ok(alloc::vec![NameComponent::Segment(first), NameComponent::Segment(second)])
    })
}

pub fn multi_name_path<'a, 'c>() -> impl Parser<'a, 'c, Vec<NameComponent>>
where
    'c: 'a,
{
    /*
     * MultiNamePath := 0x2f ByteData{SegCount} NameSeg(SegCount)
     */
    move |input, context| {
        let (new_input, context, ((), seg_count)) =
            opcode(MULTI_NAME_PREFIX).then(take()).parse(input, context)?;
        match n_of(name_seg(), usize::from(seg_count)).parse(new_input, context) {
            Ok((new_input, context, name_segs)) => {
                Ok((new_input, context, name_segs.iter().map(|&seg| NameComponent::Segment(seg)).collect()))
            }
            // Correct returned input to the one we haven't touched
            Err((_, context, err)) => Err((input, context, err)),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NameSeg(pub(crate) [u8; 4]);

impl NameSeg {
    pub(crate) fn from_str(string: &str) -> Result<NameSeg, AmlError> {
        // Each NameSeg can only have four chars, and must have at least one
        if string.len() < 1 || string.len() > 4 {
            return Err(AmlError::InvalidNameSeg);
        }

        // We pre-fill the array with '_', so it will already be correct if the length is < 4
        let mut seg = [b'_'; 4];
        let bytes = string.as_bytes();

        // Manually do the first one, because we have to check it's a LeadNameChar
        if !is_lead_name_char(bytes[0]) {
            return Err(AmlError::InvalidNameSeg);
        }
        seg[0] = bytes[0];

        // Copy the rest of the chars, checking that they're NameChars
        for i in 1..bytes.len() {
            if !is_name_char(bytes[i]) {
                return Err(AmlError::InvalidNameSeg);
            }
            seg[i] = bytes[i];
        }

        Ok(NameSeg(seg))
    }

    /// Turn a `NameSeg` into a `&str`. Returns it in a `ParseResult` so it's easy to use from
    /// inside parsers.
    pub fn as_str(&self) -> &str {
        /*
         * This is safe, because we always check that all the bytes are valid ASCII, so every
         * `NameSeg` will be valid UTF8.
         */
        unsafe { str::from_utf8_unchecked(&self.0) }
    }
}

// A list of ASCII codes is pretty much never useful, so we always just show it as a string
impl fmt::Debug for NameSeg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

pub fn name_seg<'a, 'c>() -> impl Parser<'a, 'c, NameSeg>
where
    'c: 'a,
{
    /*
     * NameSeg := <LeadNameChar NameChar NameChar NameChar>
     */
    // TODO: can we write this better?
    move |input, context: &'c mut AmlContext| {
        let (input, context, char_1) = consume(is_lead_name_char).parse(input, context)?;
        let (input, context, char_2) = consume(is_name_char).parse(input, context)?;
        let (input, context, char_3) = consume(is_name_char).parse(input, context)?;
        let (input, context, char_4) = consume(is_name_char).parse(input, context)?;
        Ok((input, context, NameSeg([char_1, char_2, char_3, char_4])))
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::Parser, test_utils::*, AmlError};

    #[test]
    fn test_name_seg() {
        let mut context = crate::test_utils::make_test_context();

        check_ok!(
            name_seg().parse(&[b'A', b'F', b'3', b'Z'], &mut context),
            NameSeg([b'A', b'F', b'3', b'Z']),
            &[]
        );
        check_ok!(
            name_seg().parse(&[b'A', b'F', b'3', b'Z', 0xff], &mut context),
            NameSeg([b'A', b'F', b'3', b'Z']),
            &[0xff]
        );
        check_err!(
            name_seg().parse(&[0xff, b'E', b'A', b'7'], &mut context),
            AmlError::UnexpectedByte(0xff),
            &[0xff, b'E', b'A', b'7']
        );
        check_err!(name_seg().parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
    }

    #[test]
    fn test_name_path() {
        let mut context = crate::test_utils::make_test_context();

        check_err!(name_path().parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
        check_ok!(name_path().parse(&[0x00], &mut context), alloc::vec![], &[]);
        check_ok!(name_path().parse(&[0x00, 0x00], &mut context), alloc::vec![], &[0x00]);
        check_err!(name_path().parse(&[0x2e, b'A'], &mut context), AmlError::UnexpectedEndOfStream, &[0x2e, b'A']);
        check_ok!(
            name_path().parse(&[0x2e, b'A', b'B', b'C', b'D', b'E', b'_', b'F', b'G'], &mut context),
            alloc::vec![
                NameComponent::Segment(NameSeg([b'A', b'B', b'C', b'D'])),
                NameComponent::Segment(NameSeg([b'E', b'_', b'F', b'G']))
            ],
            &[]
        );
    }

    #[test]
    fn test_prefix_path() {
        let mut context = crate::test_utils::make_test_context();

        check_ok!(
            name_string().parse(&[b'^', b'A', b'B', b'C', b'D'], &mut context),
            AmlName::from_str("^ABCD").unwrap(),
            &[]
        );
        check_ok!(
            name_string().parse(&[b'^', b'^', b'^', b'A', b'B', b'C', b'D'], &mut context),
            AmlName::from_str("^^^ABCD").unwrap(),
            &[]
        );
    }
}
