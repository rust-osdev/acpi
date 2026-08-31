//! Allocator-aware string storage for AML objects.
//!
//! `alloc::string::String` is not parameterised over an allocator - there is no
//! `String<A>` and no `String::new_in` - so an allocator-aware [`Object`] cannot
//! store its strings in one. [`AmlString`] is a thin newtype over `Vec<u8, A>`
//! that upholds a UTF-8 invariant and provides the subset of `String` the
//! interpreter actually uses.
//!
//! [`Object`]: super::object::Object

use alloc::{alloc::Global, vec::Vec};
use core::{alloc::Allocator, fmt};

/// An allocator-aware, UTF-8 string.
///
/// Every construction path either starts from a `&str` (already valid UTF-8) or
/// appends through [`push_str`](Self::push_str) / [`push`](Self::push), so the
/// invariant holds by construction. [`as_bytes_mut`](Self::as_bytes_mut) is the
/// only way to break it, and is `unsafe` for that reason.
pub struct AmlString<A: Allocator + Clone = Global>(Vec<u8, A>);

impl<A: Allocator + Clone> AmlString<A> {
    pub fn new_in(alloc: A) -> Self {
        Self(Vec::new_in(alloc))
    }

    pub fn from_str_in(s: &str, alloc: A) -> Self {
        let mut bytes = Vec::with_capacity_in(s.len(), alloc);
        bytes.extend_from_slice(s.as_bytes());
        Self(bytes)
    }

    /// Build a string from bytes that are not necessarily valid UTF-8, replacing
    /// each invalid sequence with `U+FFFD`.
    ///
    /// This is the allocator-aware counterpart of `String::from_utf8_lossy`,
    /// which would otherwise allocate through `Global`.
    pub fn from_utf8_lossy_in(bytes: &[u8], alloc: A) -> Self {
        let mut string = Self(Vec::with_capacity_in(bytes.len(), alloc));
        let mut rest = bytes;

        while !rest.is_empty() {
            match core::str::from_utf8(rest) {
                Ok(valid) => {
                    string.push_str(valid);
                    break;
                }
                Err(error) => {
                    let (valid, after) = rest.split_at(error.valid_up_to());
                    // SAFETY: `valid_up_to` is by definition the length of the
                    // longest valid UTF-8 prefix of `rest`.
                    string.push_str(unsafe { core::str::from_utf8_unchecked(valid) });
                    string.push(char::REPLACEMENT_CHARACTER);

                    match error.error_len() {
                        // An invalid sequence of `len` bytes: skip past it.
                        Some(len) => rest = &after[len..],
                        // An unexpected end of input: nothing valid remains.
                        None => break,
                    }
                }
            }
        }

        string
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: the UTF-8 invariant is maintained by every safe constructor
        // and mutator on this type.
        unsafe { core::str::from_utf8_unchecked(&self.0) }
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    /// # Safety
    /// The caller must leave the returned slice as valid UTF-8. Breaking that
    /// makes subsequent [`as_str`](Self::as_str) calls unsound.
    #[inline]
    pub unsafe fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    pub fn push_str(&mut self, s: &str) {
        self.0.extend_from_slice(s.as_bytes());
    }

    pub fn push(&mut self, c: char) {
        let mut buf = [0u8; 4];
        self.0.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
    }

    pub fn clear(&mut self) {
        self.0.clear();
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn parse<F: core::str::FromStr>(&self) -> Result<F, F::Err> {
        self.as_str().parse::<F>()
    }
}

impl<A: Allocator + Clone> Clone for AmlString<A> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

/*
 * These are written out rather than derived because `derive` would bound
 * `A: PartialEq`, while `Vec`'s own comparison works across differing
 * allocators. Comparing two `AmlString`s with different allocators is
 * meaningful, so the impl is generic over both.
 */
impl<A: Allocator + Clone, A2: Allocator + Clone> PartialEq<AmlString<A2>> for AmlString<A> {
    fn eq(&self, other: &AmlString<A2>) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl<A: Allocator + Clone> Eq for AmlString<A> {}

impl<A: Allocator + Clone> PartialEq<str> for AmlString<A> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<A: Allocator + Clone> PartialEq<&str> for AmlString<A> {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl<A: Allocator + Clone> fmt::Display for AmlString<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl<A: Allocator + Clone> fmt::Debug for AmlString<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

/// Lets `write!(string, "...")` append without routing through `Global`.
impl<A: Allocator + Clone> fmt::Write for AmlString<A> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.push_str(s);
        Ok(())
    }
}
