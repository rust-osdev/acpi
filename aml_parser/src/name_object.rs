use crate::{
    opcode::{opcode, DUAL_NAME_PREFIX, MULTI_NAME_PREFIX, NULL_NAME, PREFIX_CHAR, ROOT_CHAR},
    parser::{choice, comment_scope_verbose, consume, n_of, take, Parser},
    AmlContext,
    AmlError,
};
use alloc::{
    string::{String, ToString},
    vec::Vec,
};
use core::{fmt, ops::Add, str};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct AmlName(pub(crate) Vec<NameComponent>);

impl AmlName {
    pub fn root() -> AmlName {
        AmlName(alloc::vec![NameComponent::Root])
    }

    pub fn from_name_seg(seg: NameSeg) -> AmlName {
        AmlName(alloc::vec![NameComponent::Segment(seg)])
    }

    /// Convert a string representation of an AML name into an `AmlName`. Returns `None` if the
    /// passed string is not a valid AML path.
    pub fn from_str(mut string: &str) -> Option<AmlName> {
        if string.len() == 0 {
            return None;
        }

        let mut components = Vec::new();

        // If it starts with a \, make it an absolute name
        if string.starts_with('\\') {
            components.push(NameComponent::Root);
            string = &string[1..];
        }

        if string.len() > 0 {
            // Divide the rest of it into segments, and parse those
            for mut part in string.split('.') {
                // Handle prefix chars
                while part.starts_with('^') {
                    components.push(NameComponent::Prefix);
                    part = &part[1..];
                }

                components.push(NameComponent::Segment(NameSeg::from_str(part)?));
            }
        }

        Some(AmlName(components))
    }

    pub fn as_string(&self) -> String {
        self.0
            .iter()
            .fold(String::new(), |name, component| match component {
                NameComponent::Root => name + "\\",
                NameComponent::Prefix => name + "^",
                NameComponent::Segment(seg) => name + seg.as_str() + ".",
            })
            .trim_end_matches('.')
            .to_string()
    }

    pub fn is_absolute(&self) -> bool {
        self.0.first() == Some(&NameComponent::Root)
    }

    /// Returns `true` if this path is at the root of the namespace (`\`).
    pub fn is_at_root(&self) -> bool {
        self.0 == &[NameComponent::Root]
    }

    /// Special rules apply when searching for certain paths (specifically, those that are made up
    /// of a single name segment). Returns `true` if those rules apply.
    pub fn search_rules_apply(&self) -> bool {
        if self.0.len() != 1 {
            return false;
        }

        match self.0[0] {
            NameComponent::Segment(_) => true,
            _ => false,
        }
    }

    /// Normalize an AML path, resolving prefix chars. Returns `None` if the path normalizes to an
    /// invalid path (e.g. `\^_FOO`)
    pub fn normalize(self) -> Option<AmlName> {
        // TODO: currently, this doesn't do anything. Work out a nice way of handling prefix chars.
        Some(self)
    }

    /// Get the parent of this `AmlName`. For example, the parent of `\_SB.PCI0._PRT` is `\_SB.PCI0`. The root
    /// path has no parent, and so returns `None`.
    pub fn parent(&self) -> Option<AmlName> {
        // Firstly, normalize the path so we don't have to deal with prefix chars
        let mut normalized_self = self.clone().normalize()?;

        match normalized_self.0.last() {
            None => None,
            Some(NameComponent::Root) => None,
            Some(NameComponent::Segment(_)) => {
                normalized_self.0.pop();
                Some(normalized_self)
            }
            Some(NameComponent::Prefix) => unreachable!(), // Prefix chars are removed by normalization
        }
    }
}

impl Add for AmlName {
    type Output = AmlName;

    fn add(mut self, other: Self) -> Self {
        self.0.extend_from_slice(&(other.0));
        self
    }
}

impl fmt::Display for AmlName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum NameComponent {
    Root,
    Prefix,
    Segment(NameSeg),
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
        Ok(AmlName(name))
    });

    // TODO: combinator to select a parser based on a peeked byte?
    comment_scope_verbose("NameString", move |input: &'a [u8], context| {
        let first_char = match input.first() {
            Some(&c) => c,
            None => return Err((input, context, AmlError::UnexpectedEndOfStream)),
        };

        // TODO: parse <PrefixPath NamePath> where there are actually PrefixChars
        match first_char {
            ROOT_CHAR => root_name_string.parse(input, context),
            PREFIX_CHAR => unimplemented!(),
            _ => name_path().map(|path| Ok(AmlName(path))).parse(input, context),
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
     *
     * This doesn't actually allocate because the `Vec`'s capacity is zero.
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
pub struct NameSeg([u8; 4]);

impl NameSeg {
    pub(crate) fn from_str(string: &str) -> Option<NameSeg> {
        // Each NameSeg can only have four chars, and must have at least one
        if string.len() < 1 || string.len() > 4 {
            return None;
        }

        // We pre-fill the array with '_', so it will already be correct if the length is < 4
        let mut seg = [b'_'; 4];
        let bytes = string.as_bytes();

        // Manually do the first one, because we have to check it's a LeadNameChar
        if !is_lead_name_char(bytes[0]) {
            return None;
        }
        seg[0] = bytes[0];

        // Copy the rest of the chars, checking that they're NameChars
        for i in 1..bytes.len() {
            if !is_name_char(bytes[i]) {
                return None;
            }
            seg[i] = bytes[i];
        }

        Some(NameSeg(seg))
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
    use crate::{parser::Parser, test_utils::*, AmlContext, AmlError};

    #[test]
    fn test_name_seg() {
        let mut context = AmlContext::new();

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
        let mut context = AmlContext::new();

        check_err!(name_path().parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
        check_ok!(name_path().parse(&[0x00], &mut context), alloc::vec![], &[]);
        check_ok!(name_path().parse(&[0x00, 0x00], &mut context), alloc::vec![], &[0x00]);
        check_err!(
            name_path().parse(&[0x2e, b'A'], &mut context),
            AmlError::UnexpectedEndOfStream,
            &[0x2e, b'A']
        );
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
    fn test_aml_name_from_str() {
        assert_eq!(AmlName::from_str(""), None);
        assert_eq!(AmlName::from_str("\\"), Some(AmlName::root()));
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0"),
            Some(AmlName(alloc::vec![
                NameComponent::Root,
                NameComponent::Segment(NameSeg([b'_', b'S', b'B', b'_'])),
                NameComponent::Segment(NameSeg([b'P', b'C', b'I', b'0']))
            ]))
        );
        assert_eq!(
            AmlName::from_str("\\_SB.^^^PCI0"),
            Some(AmlName(alloc::vec![
                NameComponent::Root,
                NameComponent::Segment(NameSeg([b'_', b'S', b'B', b'_'])),
                NameComponent::Prefix,
                NameComponent::Prefix,
                NameComponent::Prefix,
                NameComponent::Segment(NameSeg([b'P', b'C', b'I', b'0']))
            ]))
        );
    }

    #[test]
    fn test_is_at_root() {
        assert_eq!(AmlName::root().is_at_root(), true);
        assert_eq!(AmlName::from_str("\\_SB").unwrap().is_at_root(), false);
        assert_eq!(AmlName::from_str("\\_SB.PCI0.^VGA").unwrap().is_at_root(), false);
    }

    #[test]
    fn test_search_rules_apply() {
        assert_eq!(AmlName::root().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("\\_SB").unwrap().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("^VGA").unwrap().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("_SB.PCI0.VGA").unwrap().search_rules_apply(), false);
        assert_eq!(AmlName::from_str("VGA").unwrap().search_rules_apply(), true);
        assert_eq!(AmlName::from_str("_SB").unwrap().search_rules_apply(), true);
    }

    #[test]
    fn test_aml_name_parent() {
        assert_eq!(AmlName::from_str("\\").unwrap().parent(), None);
        assert_eq!(AmlName::from_str("\\_SB").unwrap().parent(), Some(AmlName::root()));
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0").unwrap().parent(),
            Some(AmlName::from_str("\\_SB").unwrap())
        );
        assert_eq!(
            AmlName::from_str("\\_SB.PCI0").unwrap().parent().unwrap().parent(),
            Some(AmlName::root())
        );
    }
}
