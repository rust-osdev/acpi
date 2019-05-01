use crate::AmlError;

pub type ParseResult<'a, R> = Result<(&'a [u8], R), (&'a [u8], AmlError)>;

pub trait Parser<'a, R>: Sized {
    fn parse(&self, input: &'a [u8]) -> ParseResult<'a, R>;
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
