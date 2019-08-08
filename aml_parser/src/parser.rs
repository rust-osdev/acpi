use crate::{pkg_length::PkgLength, AmlContext, AmlError};
use alloc::vec::Vec;
use core::marker::PhantomData;

pub type ParseResult<'a, 'c, R> =
    Result<(&'a [u8], &'c mut AmlContext, R), (&'a [u8], &'c mut AmlContext, AmlError)>;

pub trait Parser<'a, 'c, R>: Sized
where
    'c: 'a,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, R>;

    fn map<F, A>(self, map_fn: F) -> Map<'a, 'c, Self, F, R, A>
    where
        F: Fn(R) -> Result<A, AmlError>,
    {
        Map { parser: self, map_fn, _phantom: PhantomData }
    }

    fn map_with_context<F, A>(self, map_fn: F) -> MapWithContext<'a, 'c, Self, F, R, A>
    where
        F: Fn(R, &'c mut AmlContext) -> (Result<A, AmlError>, &'c mut AmlContext),
    {
        MapWithContext { parser: self, map_fn, _phantom: PhantomData }
    }

    fn discard_result(self) -> DiscardResult<'a, 'c, Self, R> {
        DiscardResult { parser: self, _phantom: PhantomData }
    }

    /// Try parsing with `self`. If it succeeds, return its result. If it returns `AmlError::WrongParser`, try
    /// parsing with `other`, returning the result of that parser in all cases. Other errors from the first
    /// parser are propagated without attempting the second parser. To chain more than two parsers using
    /// `or`, see the `choice!` macro.
    fn or<OtherParser>(self, other: OtherParser) -> Or<'a, 'c, Self, OtherParser, R>
    where
        OtherParser: Parser<'a, 'c, R>,
    {
        Or { p1: self, p2: other, _phantom: PhantomData }
    }

    fn then<NextParser, NextR>(self, next: NextParser) -> Then<'a, 'c, Self, NextParser, R, NextR>
    where
        NextParser: Parser<'a, 'c, NextR>,
    {
        Then { p1: self, p2: next, _phantom: PhantomData }
    }

    /// `feed` takes a function that takes the result of this parser (`self`) and creates another
    /// parser, which is then used to parse the next part of the stream. This sounds convoluted,
    /// but is useful for when the next parser's behaviour depends on a property of the result of
    /// the first (e.g. the first parser might parse a length `n`, and the second parser then
    /// consumes `n` bytes).
    fn feed<F, P2, R2>(self, producer_fn: F) -> Feed<'a, 'c, Self, P2, F, R, R2>
    where
        P2: Parser<'a, 'c, R2>,
        F: Fn(R) -> P2,
    {
        Feed { parser: self, producer_fn, _phantom: PhantomData }
    }
}

impl<'a, 'c, F, R> Parser<'a, 'c, R> for F
where
    'c: 'a,
    F: Fn(&'a [u8], &'c mut AmlContext) -> ParseResult<'a, 'c, R>,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, R> {
        self(input, context)
    }
}

/// The identity parser - returns the stream and context unchanged. Useful for producing parsers
/// that produce a result without parsing anything by doing: `id().map(|()| Ok(foo))`.
pub fn id<'a, 'c>() -> impl Parser<'a, 'c, ()>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| Ok((input, context, ()))
}

pub fn take<'a, 'c>() -> impl Parser<'a, 'c, u8>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| match input.first() {
        Some(&byte) => Ok((&input[1..], context, byte)),
        None => Err((input, context, AmlError::UnexpectedEndOfStream)),
    }
}

pub fn take_u16<'a, 'c>() -> impl Parser<'a, 'c, u16>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| {
        if input.len() < 2 {
            return Err((input, context, AmlError::UnexpectedEndOfStream));
        }

        Ok((&input[2..], context, input[0] as u16 + ((input[1] as u16) << 8)))
    }
}

pub fn take_u32<'a, 'c>() -> impl Parser<'a, 'c, u32>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| {
        if input.len() < 4 {
            return Err((input, context, AmlError::UnexpectedEndOfStream));
        }

        Ok((
            &input[4..],
            context,
            input[0] as u32
                + ((input[1] as u32) << 8)
                + ((input[2] as u32) << 16)
                + ((input[3] as u32) << 24),
        ))
    }
}

pub fn take_u64<'a, 'c>() -> impl Parser<'a, 'c, u64>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| {
        if input.len() < 8 {
            return Err((input, context, AmlError::UnexpectedEndOfStream));
        }

        Ok((
            &input[8..],
            context,
            input[0] as u64
                + ((input[1] as u64) << 8)
                + ((input[2] as u64) << 16)
                + ((input[3] as u64) << 24)
                + ((input[4] as u64) << 32)
                + ((input[5] as u64) << 40)
                + ((input[6] as u64) << 48)
                + ((input[7] as u64) << 56),
        ))
    }
}

pub fn take_n<'a, 'c>(n: u32) -> impl Parser<'a, 'c, &'a [u8]>
where
    'c: 'a,
{
    move |input: &'a [u8], context| {
        if (input.len() as u32) < n {
            return Err((input, context, AmlError::UnexpectedEndOfStream));
        }

        let (result, new_input) = input.split_at(n as usize);
        Ok((new_input, context, result))
    }
}

pub fn take_to_end_of_pkglength<'a, 'c>(length: PkgLength) -> impl Parser<'a, 'c, &'a [u8]>
where
    'c: 'a,
{
    move |input: &'a [u8], context| {
        let bytes_to_take = (input.len() as u32) - length.end_offset;
        take_n(bytes_to_take).parse(input, context)
    }
}

// TODO: can we use const generics (e.g. [R; N]) to avoid allocating?
pub fn n_of<'a, 'c, P, R>(parser: P, n: usize) -> impl Parser<'a, 'c, Vec<R>>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
{
    // TODO: can we write this more nicely?
    move |mut input, mut context| {
        let mut results = Vec::with_capacity(n);

        for _ in 0..n {
            let (new_input, new_context, result) = match parser.parse(input, context) {
                Ok((input, context, result)) => (input, context, result),
                Err((_, context, err)) => return Err((input, context, err)),
            };
            results.push(result);
            input = new_input;
            context = new_context;
        }

        Ok((input, context, results))
    }
}

pub fn consume<'a, 'c, F>(condition: F) -> impl Parser<'a, 'c, u8>
where
    'c: 'a,
    F: Fn(u8) -> bool,
{
    move |input: &'a [u8], context: &'c mut AmlContext| match input.first() {
        Some(&byte) if condition(byte) => Ok((&input[1..], context, byte)),
        Some(&byte) => Err((input, context, AmlError::UnexpectedByte(byte))),
        None => Err((input, context, AmlError::UnexpectedEndOfStream)),
    }
}

pub fn comment_scope<'a, 'c, P, R>(scope_name: &'a str, parser: P) -> impl Parser<'a, 'c, R>
where
    'c: 'a,
    R: core::fmt::Debug,
    P: Parser<'a, 'c, R>,
{
    move |input, context| {
        #[cfg(feature = "debug_parser")]
        log::trace!("--> {}", scope_name);

        // Return if the parse fails, so we don't print the tail. Makes it easier to debug.
        let (new_input, context, result) = parser.parse(input, context)?;

        #[cfg(feature = "debug_parser")]
        log::trace!("<-- {}", scope_name);

        Ok((new_input, context, result))
    }
}

pub fn comment_scope_verbose<'a, 'c, P, R>(scope_name: &'a str, parser: P) -> impl Parser<'a, 'c, R>
where
    'c: 'a,
    R: core::fmt::Debug,
    P: Parser<'a, 'c, R>,
{
    move |input, context| {
        #[cfg(feature = "debug_parser_verbose")]
        log::trace!("--> {}", scope_name);

        // Return if the parse fails, so we don't print the tail. Makes it easier to debug.
        let (new_input, context, result) = parser.parse(input, context)?;

        #[cfg(feature = "debug_parser_verbose")]
        log::trace!("<-- {}", scope_name);

        Ok((new_input, context, result))
    }
}

pub struct Or<'a, 'c, P1, P2, R>
where
    'c: 'a,
    P1: Parser<'a, 'c, R>,
    P2: Parser<'a, 'c, R>,
{
    p1: P1,
    p2: P2,
    _phantom: PhantomData<(&'a R, &'c ())>,
}

impl<'a, 'c, P1, P2, R> Parser<'a, 'c, R> for Or<'a, 'c, P1, P2, R>
where
    'c: 'a,
    P1: Parser<'a, 'c, R>,
    P2: Parser<'a, 'c, R>,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, R> {
        match self.p1.parse(input, context) {
            Ok(parse_result) => Ok(parse_result),
            Err((_, context, AmlError::WrongParser)) => self.p2.parse(input, context),
            Err((_, context, err)) => Err((input, context, err)),
        }
    }
}

pub struct Map<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R) -> Result<A, AmlError>,
{
    parser: P,
    map_fn: F,
    _phantom: PhantomData<(&'a (R, A), &'c ())>,
}

impl<'a, 'c, P, F, R, A> Parser<'a, 'c, A> for Map<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R) -> Result<A, AmlError>,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, A> {
        match self.parser.parse(input, context) {
            Ok((new_input, context, result)) => match (self.map_fn)(result) {
                Ok(result_value) => Ok((new_input, context, result_value)),
                Err(err) => Err((input, context, err)),
            },
            Err(result) => Err(result),
        }
    }
}

pub struct MapWithContext<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R, &'c mut AmlContext) -> (Result<A, AmlError>, &'c mut AmlContext),
{
    parser: P,
    map_fn: F,
    _phantom: PhantomData<(&'a (R, A), &'c ())>,
}

impl<'a, 'c, P, F, R, A> Parser<'a, 'c, A> for MapWithContext<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R, &'c mut AmlContext) -> (Result<A, AmlError>, &'c mut AmlContext),
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, A> {
        match self.parser.parse(input, context) {
            Ok((new_input, context, result)) => match (self.map_fn)(result, context) {
                (Ok(result_value), context) => Ok((new_input, context, result_value)),
                (Err(err), context) => Err((input, context, err)),
            },
            Err(result) => Err(result),
        }
    }
}

pub struct DiscardResult<'a, 'c, P, R>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
{
    parser: P,
    _phantom: PhantomData<(&'a R, &'c ())>,
}

impl<'a, 'c, P, R> Parser<'a, 'c, ()> for DiscardResult<'a, 'c, P, R>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, ()> {
        self.parser.parse(input, context).map(|(new_input, new_context, _)| (new_input, new_context, ()))
    }
}

pub struct Then<'a, 'c, P1, P2, R1, R2>
where
    'c: 'a,
    P1: Parser<'a, 'c, R1>,
    P2: Parser<'a, 'c, R2>,
{
    p1: P1,
    p2: P2,
    _phantom: PhantomData<(&'a (R1, R2), &'c ())>,
}

impl<'a, 'c, P1, P2, R1, R2> Parser<'a, 'c, (R1, R2)> for Then<'a, 'c, P1, P2, R1, R2>
where
    'c: 'a,
    P1: Parser<'a, 'c, R1>,
    P2: Parser<'a, 'c, R2>,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, (R1, R2)> {
        self.p1.parse(input, context).and_then(|(next_input, context, result_a)| {
            self.p2
                .parse(next_input, context)
                .map(|(final_input, context, result_b)| (final_input, context, (result_a, result_b)))
        })
    }
}

pub struct Feed<'a, 'c, P1, P2, F, R1, R2>
where
    'c: 'a,
    P1: Parser<'a, 'c, R1>,
    P2: Parser<'a, 'c, R2>,
    F: Fn(R1) -> P2,
{
    parser: P1,
    producer_fn: F,
    _phantom: PhantomData<(&'a (R1, R2), &'c ())>,
}

impl<'a, 'c, P1, P2, F, R1, R2> Parser<'a, 'c, R2> for Feed<'a, 'c, P1, P2, F, R1, R2>
where
    'c: 'a,
    P1: Parser<'a, 'c, R1>,
    P2: Parser<'a, 'c, R2>,
    F: Fn(R1) -> P2,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, R2> {
        let (input, context, first_result) = self.parser.parse(input, context)?;

        // We can now produce the second parser, and parse using that.
        let second_parser = (self.producer_fn)(first_result);
        second_parser.parse(input, context)
    }
}

/// This is a helper parser used in the `choice` macro to emit `AmlError::WrongParser`
/// unconditionally. It should not be used directly.
pub(crate) fn no_parsers_could_parse<'a, 'c, R>() -> impl Parser<'a, 'c, R>
where
    'c: 'a,
{
    |input: &'a [u8], context| Err((input, context, AmlError::WrongParser))
}

/// Takes a number of parsers, and tries to apply each one to the input in order. Returns the
/// result of the first one that succeeds, or fails if all of them fail.
pub(crate) macro choice {
    () => {
        no_parsers_could_parse()
    },

    ($first_parser: expr) => {
        $first_parser
        .or(no_parsers_could_parse())
    },

    ($first_parser: expr, $($other_parser: expr),*) => {
        $first_parser
        $(
            .or($other_parser)
         )*
        .or(no_parsers_could_parse())
    }
}

/// This encapsulates an unfortunate hack we sometimes need to use, where the type checker gets
/// caught in an infinite loop of parser types. This occurs when an object can indirectly contain
/// itself, and so the parser type will contain its own type. This works by breaking the cycle of
/// `impl Parser` chains that build up, by effectively creating a "concrete" closure type.
///
/// You can try using this hack if you are writing a parser and end up with an error of the form:
/// `error[E0275]: overflow evaluating the requirement 'impl Parser<{a type}>'
///     help: consider adding a a '#![recursion_limit="128"] attribute to your crate`
/// Note: Increasing the recursion limit will not fix the issue, as the cycle will just continue
/// until you either hit the new recursion limit or `rustc` overflows its stack.
pub(crate) macro make_parser_concrete($parser: expr) {
    |input, context| ($parser).parse(input, context)
}

/// Helper macro for use within `map_with_context` as an alternative to "trying" an expression.
///
/// ### Example
/// Problem: `expr?` won't work because the expected return type is `(Result<R, AmlError>, &mut AmlContext)`
/// Solution: use `try_with_context!(context, expr)` instead.
pub(crate) macro try_with_context($context: expr, $expr: expr) {
    match $expr {
        Ok(result) => result,
        Err(err) => return (Err(err), $context),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_take_n() {
        let mut context = AmlContext::new();
        check_err!(take_n(1).parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
        check_err!(take_n(2).parse(&[0xf5], &mut context), AmlError::UnexpectedEndOfStream, &[0xf5]);

        check_ok!(take_n(1).parse(&[0xff], &mut context), &[0xff], &[]);
        check_ok!(take_n(1).parse(&[0xff, 0xf8], &mut context), &[0xff], &[0xf8]);
        check_ok!(take_n(2).parse(&[0xff, 0xf8], &mut context), &[0xff, 0xf8], &[]);
    }

    #[test]
    fn test_take_ux() {
        let mut context = AmlContext::new();
        check_err!(take_u16().parse(&[0x34], &mut context), AmlError::UnexpectedEndOfStream, &[0x34]);
        check_ok!(take_u16().parse(&[0x34, 0x12], &mut context), 0x1234, &[]);

        check_err!(
            take_u32().parse(&[0x34, 0x12], &mut context),
            AmlError::UnexpectedEndOfStream,
            &[0x34, 0x12]
        );
        check_ok!(take_u32().parse(&[0x34, 0x12, 0xf4, 0xc3, 0x3e], &mut context), 0xc3f41234, &[0x3e]);

        check_err!(take_u64().parse(&[0x34], &mut context), AmlError::UnexpectedEndOfStream, &[0x34]);
        check_ok!(
            take_u64().parse(&[0x34, 0x12, 0x35, 0x76, 0xd4, 0x43, 0xa3, 0xb6, 0xff, 0x00], &mut context),
            0xb6a343d476351234,
            &[0xff, 0x00]
        );
    }
}
