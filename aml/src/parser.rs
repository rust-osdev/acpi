use crate::{pkg_length::PkgLength, AmlContext, AmlError, AmlValue, DebugVerbosity};
use alloc::vec::Vec;
use core::{convert::TryInto, marker::PhantomData};
use log::trace;

/// This is the number of spaces added to indent a scope when printing parser debug messages.
pub const INDENT_PER_SCOPE: usize = 2;

impl AmlContext {
    /// This is used by the parser to provide debug comments about the current object, which are indented to the
    /// correct level for the current object. We most often need to print these comments from `map_with_context`s,
    /// so it's most convenient to have this method on `AmlContext`.
    pub(crate) fn comment(&self, verbosity: DebugVerbosity, message: &str) {
        if verbosity <= self.debug_verbosity {
            log::trace!("{:indent$}{}", "", message, indent = self.scope_indent);
        }
    }
}

#[derive(Debug)]
pub enum Propagate {
    Err(AmlError),
    Return(AmlValue),
    Break,
    Continue,
}

impl From<AmlError> for Propagate {
    fn from(error: AmlError) -> Self {
        Self::Err(error)
    }
}

pub type ParseResult<'a, 'c, R> =
    Result<(&'a [u8], &'c mut AmlContext, R), (&'a [u8], &'c mut AmlContext, Propagate)>;

pub trait Parser<'a, 'c, R>: Sized
where
    'c: 'a,
{
    fn parse(&self, input: &'a [u8], context: &'c mut AmlContext) -> ParseResult<'a, 'c, R>;

    fn map<F, A>(self, map_fn: F) -> Map<'a, 'c, Self, F, R, A>
    where
        F: Fn(R) -> Result<A, Propagate>,
    {
        Map { parser: self, map_fn, _phantom: PhantomData }
    }

    fn map_with_context<F, A>(self, map_fn: F) -> MapWithContext<'a, 'c, Self, F, R, A>
    where
        F: Fn(R, &'c mut AmlContext) -> (Result<A, Propagate>, &'c mut AmlContext),
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
        None => Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream))),
    }
}

pub fn take_u16<'a, 'c>() -> impl Parser<'a, 'c, u16>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| {
        if input.len() < 2 {
            return Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream)));
        }

        Ok((&input[2..], context, u16::from_le_bytes(input[0..2].try_into().unwrap())))
    }
}

pub fn take_u32<'a, 'c>() -> impl Parser<'a, 'c, u32>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| {
        if input.len() < 4 {
            return Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream)));
        }

        Ok((&input[4..], context, u32::from_le_bytes(input[0..4].try_into().unwrap())))
    }
}

pub fn take_u64<'a, 'c>() -> impl Parser<'a, 'c, u64>
where
    'c: 'a,
{
    move |input: &'a [u8], context: &'c mut AmlContext| {
        if input.len() < 8 {
            return Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream)));
        }

        Ok((&input[8..], context, u64::from_le_bytes(input[0..8].try_into().unwrap())))
    }
}

pub fn take_n<'a, 'c>(n: u32) -> impl Parser<'a, 'c, &'a [u8]>
where
    'c: 'a,
{
    move |input: &'a [u8], context| {
        if (input.len() as u32) < n {
            return Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream)));
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
        /*
         * TODO: fuzzing manages to find PkgLengths that correctly parse during construction, but later crash here.
         * I would've thought we would pick up all invalid lengths there, so have a look at why this is needed.
         */
        let bytes_to_take = match (input.len() as u32).checked_sub(length.end_offset) {
            Some(bytes_to_take) => bytes_to_take,
            None => return Err((input, context, Propagate::Err(AmlError::InvalidPkgLength))),
        };
        take_n(bytes_to_take).parse(input, context)
    }
}

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
                Err((_, context, propagate)) => return Err((input, context, propagate)),
            };
            results.push(result);
            input = new_input;
            context = new_context;
        }

        Ok((input, context, results))
    }
}

pub fn take_while<'a, 'c, P, R>(parser: P) -> impl Parser<'a, 'c, usize>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
{
    move |mut input: &'a [u8], mut context: &'c mut AmlContext| {
        let mut num_passed = 0;
        loop {
            match parser.parse(input, context) {
                Ok((new_input, new_context, _)) => {
                    input = new_input;
                    context = new_context;
                    num_passed += 1;
                }
                Err((_, context, Propagate::Err(AmlError::WrongParser))) => {
                    return Ok((input, context, num_passed))
                }
                Err((_, context, err)) => return Err((input, context, err)),
            }
        }
    }
}

pub fn consume<'a, 'c, F>(condition: F) -> impl Parser<'a, 'c, u8>
where
    'c: 'a,
    F: Fn(u8) -> bool,
{
    move |input: &'a [u8], context: &'c mut AmlContext| match input.first() {
        Some(&byte) if condition(byte) => Ok((&input[1..], context, byte)),
        Some(&byte) => Err((input, context, Propagate::Err(AmlError::UnexpectedByte(byte)))),
        None => Err((input, context, Propagate::Err(AmlError::UnexpectedEndOfStream))),
    }
}

pub fn comment_scope<'a, 'c, P, R>(
    verbosity: DebugVerbosity,
    scope_name: &'a str,
    parser: P,
) -> impl Parser<'a, 'c, R>
where
    'c: 'a,
    R: core::fmt::Debug,
    P: Parser<'a, 'c, R>,
{
    move |input, context: &'c mut AmlContext| {
        if verbosity <= context.debug_verbosity {
            trace!("{:indent$}--> {}", "", scope_name, indent = context.scope_indent);
            context.scope_indent += INDENT_PER_SCOPE;
        }

        // Return if the parse fails, so we don't print the tail. Makes it easier to debug.
        let (new_input, context, result) = parser.parse(input, context)?;

        if verbosity <= context.debug_verbosity {
            context.scope_indent -= INDENT_PER_SCOPE;
            trace!("{:indent$}<-- {}", "", scope_name, indent = context.scope_indent);
        }

        Ok((new_input, context, result))
    }
}

/// `extract` observes another parser consuming part of the stream, and returns the result of the parser, and the
/// section of the stream that was parsed by the parser. This is useful for re-parsing that section of the stream,
/// which allows the result of a piece of AML to be reevaluated with a new context, for example.
///
/// Note that reparsing the stream is not idempotent - the context is changed by this parse.
pub fn extract<'a, 'c, P, R>(parser: P) -> impl Parser<'a, 'c, (R, &'a [u8])>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
{
    move |input, context: &'c mut AmlContext| {
        let before = input;
        let (after, context, result) = parser.parse(input, context)?;
        let bytes_parsed = before.len() - after.len();
        let parsed = &before[..bytes_parsed];

        Ok((after, context, (result, parsed)))
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
            Err((_, context, Propagate::Err(AmlError::WrongParser))) => self.p2.parse(input, context),
            Err((_, context, err)) => Err((input, context, err)),
        }
    }
}

pub struct Map<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R) -> Result<A, Propagate>,
{
    parser: P,
    map_fn: F,
    _phantom: PhantomData<(&'a (R, A), &'c ())>,
}

impl<'a, 'c, P, F, R, A> Parser<'a, 'c, A> for Map<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R) -> Result<A, Propagate>,
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
    F: Fn(R, &'c mut AmlContext) -> (Result<A, Propagate>, &'c mut AmlContext),
{
    parser: P,
    map_fn: F,
    _phantom: PhantomData<(&'a (R, A), &'c ())>,
}

impl<'a, 'c, P, F, R, A> Parser<'a, 'c, A> for MapWithContext<'a, 'c, P, F, R, A>
where
    'c: 'a,
    P: Parser<'a, 'c, R>,
    F: Fn(R, &'c mut AmlContext) -> (Result<A, Propagate>, &'c mut AmlContext),
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

/// Takes a number of parsers, and tries to apply each one to the input in order. Returns the
/// result of the first one that succeeds, or fails if all of them fail.
pub(crate) macro choice {
    () => {
        id().map(|()| Err(AmlError::WrongParser))
    },

    /*
     * The nice way of writing this would be something like:
     * ```
     * $first_parser
     * $(
     *     .or($other_parser)
     *  )*
     * .or(id().map(|()| Err(AmlError::WrongParser)))
     * ```
     * This problem with this is that it generates enormous types that very easily break `rustc`'s type
     * limit, so writing large parsers with choice required some gymnastics, which sucks for everyone involved.
     *
     * Instead, we manually call each parser sequentially, checking its result to see if we should return, or try
     * the next parser. This generates worse code at the macro callsite, but is much easier for the compiler to
     * type-check (and so reduces the cost of pulling us in as a dependency as well as improving ergonomics).
     */
    ($($parser: expr),+) => {
        move |input, context| {
            $(
                let context = match ($parser).parse(input, context) {
                    Ok(parse_result) => return Ok(parse_result),
                    Err((_, new_context, Propagate::Err(AmlError::WrongParser))) => new_context,
                    Err((_, context, propagate)) => return Err((input, context, propagate)),
                };
             )+
            Err((input, context, Propagate::Err(AmlError::WrongParser)))
        }
    }
}

/// Helper macro for use within `map_with_context` as an alternative to "trying" an expression.
///
/// ### Example
/// Problem: `expr?` won't work because the expected return type is `(Result<R, AmlError>, &mut AmlContext)`
/// Solution: use `try_with_context!(context, expr)` instead.
pub(crate) macro try_with_context($context: expr, $expr: expr) {
    match $expr {
        Ok(result) => result,
        Err(err) => return (Err(Propagate::Err(err)), $context),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::*;

    #[test]
    fn test_take_n() {
        let mut context = make_test_context();
        check_err!(take_n(1).parse(&[], &mut context), AmlError::UnexpectedEndOfStream, &[]);
        check_err!(take_n(2).parse(&[0xf5], &mut context), AmlError::UnexpectedEndOfStream, &[0xf5]);

        check_ok!(take_n(1).parse(&[0xff], &mut context), &[0xff], &[]);
        check_ok!(take_n(1).parse(&[0xff, 0xf8], &mut context), &[0xff], &[0xf8]);
        check_ok!(take_n(2).parse(&[0xff, 0xf8], &mut context), &[0xff, 0xf8], &[]);
    }

    #[test]
    fn test_take_ux() {
        let mut context = make_test_context();
        check_err!(take_u16().parse(&[0x34], &mut context), AmlError::UnexpectedEndOfStream, &[0x34]);
        check_ok!(take_u16().parse(&[0x34, 0x12], &mut context), 0x1234, &[]);

        check_err!(take_u32().parse(&[0x34, 0x12], &mut context), AmlError::UnexpectedEndOfStream, &[0x34, 0x12]);
        check_ok!(take_u32().parse(&[0x34, 0x12, 0xf4, 0xc3, 0x3e], &mut context), 0xc3f41234, &[0x3e]);

        check_err!(take_u64().parse(&[0x34], &mut context), AmlError::UnexpectedEndOfStream, &[0x34]);
        check_ok!(
            take_u64().parse(&[0x34, 0x12, 0x35, 0x76, 0xd4, 0x43, 0xa3, 0xb6, 0xff, 0x00], &mut context),
            0xb6a343d476351234,
            &[0xff, 0x00]
        );
    }
}
