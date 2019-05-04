use crate::AmlError;
use core::marker::PhantomData;
use log::trace;

pub type ParseResult<'a, R> = Result<(&'a [u8], R), (&'a [u8], AmlError)>;

pub trait Parser<'a, R>: Sized {
    fn parse(&self, input: &'a [u8]) -> ParseResult<'a, R>;

    fn map<F, A>(self, map_fn: F) -> Map<'a, Self, F, R, A>
    where
        F: Fn(R) -> A,
    {
        Map { parser: self, map_fn, _phantom: PhantomData }
    }

    /// Try parsing with `self`. If it fails, try parsing with `other`, returning the result of the
    /// first of the two parsers to succeed. To `or` multiple parsers ergonomically, see the
    /// `choice!` macro.
    fn or<OtherParser>(self, other: OtherParser) -> Or<'a, Self, OtherParser, R>
    where
        OtherParser: Parser<'a, R>,
    {
        Or { p1: self, p2: other, _phantom: PhantomData }
    }
}

impl<'a, F, R> Parser<'a, R> for F
where
    F: Fn(&'a [u8]) -> ParseResult<'a, R>,
{
    fn parse(&self, input: &'a [u8]) -> ParseResult<'a, R> {
        self(input)
    }
}

pub fn take<'a>() -> impl Parser<'a, u8> {
    move |input: &'a [u8]| match input.first() {
        Some(&byte) => Ok((&input[1..], byte)),
        None => Err((input, AmlError::UnexpectedEndOfStream)),
    }
}

pub fn take_n<'a>(n: usize) -> impl Parser<'a, &'a [u8]> {
    move |input: &'a [u8]| {
        if input.len() < n {
            return Err((input, AmlError::UnexpectedEndOfStream));
        }

        let (result, new_input) = input.split_at(n);
        Ok((new_input, result))
    }
}

pub fn consume<'a, F>(condition: F) -> impl Parser<'a, u8>
where
    F: Fn(u8) -> bool,
{
    move |input: &'a [u8]| match input.first() {
        Some(&byte) if condition(byte) => Ok((&input[1..], byte)),
        Some(&byte) => Err((input, AmlError::UnexpectedByte(byte))),
        None => Err((input, AmlError::UnexpectedEndOfStream)),
    }
}

pub fn pair<'a, P1, P2, R1, R2>(a: P1, b: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        a.parse(input).and_then(|(next_input, result_a)| {
            b.parse(next_input).map(|(final_input, result_b)| (final_input, (result_a, result_b)))
        })
    }
}

// TODO: can we make this formattable with stuff from the parse result?
pub fn comment<'a, P, R>(parser: P, comment: &'static str) -> impl Parser<'a, R>
where
    P: Parser<'a, R>,
{
    move |input| {
        trace!("{}", comment);
        parser.parse(input)
    }
}

pub struct Or<'a, P1, P2, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    p1: P1,
    p2: P2,
    _phantom: PhantomData<&'a R>,
}

impl<'a, P1, P2, R> Parser<'a, R> for Or<'a, P1, P2, R>
where
    P1: Parser<'a, R>,
    P2: Parser<'a, R>,
{
    fn parse(&self, input: &'a [u8]) -> ParseResult<'a, R> {
        match self.p1.parse(input) {
            Ok(result) => return Ok(result),
            Err(_) => (),
        }

        self.p2.parse(input)
    }
}

pub struct Map<'a, P, F, R, A>
where
    P: Parser<'a, R>,
    F: Fn(R) -> A,
{
    parser: P,
    map_fn: F,
    _phantom: PhantomData<&'a (R, A)>,
}

impl<'a, P, F, R, A> Parser<'a, A> for Map<'a, P, F, R, A>
where
    P: Parser<'a, R>,
    F: Fn(R) -> A,
{
    fn parse(&self, input: &'a [u8]) -> ParseResult<'a, A> {
        self.parser.parse(input).map(|(new_input, result)| (new_input, (self.map_fn)(result)))
    }
}

/// Takes a number of parsers, and tries to apply each one to the input in order. Returns the
/// result of the first one that succeeds, or fails if all of them fail.
pub macro choice {
    ($first_parser: expr) => {
        $first_parser
    },

    ($first_parser: expr, $($other_parser: expr),*) => {
        $first_parser
        $(
            .or($other_parser)
         )*
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_take_n() {
        check_err!(take_n(1).parse(&[]), AmlError::UnexpectedEndOfStream, &[]);
        check_err!(take_n(2).parse(&[0xf5]), AmlError::UnexpectedEndOfStream, &[0xf5]);

        check_ok!(take_n(1).parse(&[0xff]), &[0xff], &[]);
        check_ok!(take_n(1).parse(&[0xff, 0xf8]), &[0xff], &[0xf8]);
        check_ok!(take_n(2).parse(&[0xff, 0xf8]), &[0xff, 0xf8], &[]);
    }
}
