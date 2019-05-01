use crate::AmlError;
use core::marker::PhantomData;
use log::trace;

pub type ParseResult<'a, R> = Result<(&'a [u8], R), (&'a [u8], AmlError)>;

pub trait Parser<'a, R>: Sized {
    fn parse(&self, input: &'a [u8]) -> ParseResult<'a, R>;

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

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| parser.parse(input).map(|(next_input, result)| (next_input, map_fn(result)))
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

/// Takes a number of parsers, and tries to apply each one to the input in order. Returns the
/// result of the first one that succeedes, or fails if all of them fail.
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
