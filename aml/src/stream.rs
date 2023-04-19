use core::{convert::TryInto, mem};

/// A container for the buffer that provides debugging info
#[derive(Copy, Clone)]
pub struct AmlStream<'a> {
    /// The address of the original data, used for debugging only
    stream_start: usize,
    /// The slice of data we are currently processing
    buf: &'a [u8],
}

/// Slice operations for the stream - if there is an error, the returned stream will be empty
impl<'a> AmlStream<'a> {
    pub fn from_slice(slice: &'a [u8]) -> Self {
        Self { stream_start: slice.as_ptr() as usize, buf: slice }
    }

    pub fn empty() -> Self {
        Self { stream_start: 0, buf: &[] }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn data(&self) -> &[u8] {
        self.buf
    }

    pub fn take(&self, start: usize, end: usize) -> Self {
        if start > self.len() || end > self.len() || start > end {
            return Self::empty();
        }
        Self { stream_start: self.stream_start, buf: &self.buf[start..end] }
    }

    pub fn take_n(&self, index: usize) -> Self {
        self.take(0, index)
    }

    pub fn take_to_end(&self, index: usize) -> Self {
        self.take(index, self.len())
    }

    pub fn split_at(&self, mid: usize) -> (Self, Self) {
        (self.take(0, mid), self.take_to_end(mid))
    }

    pub fn first(&self) -> Option<&u8> {
        self.buf.first()
    }

    pub fn get_u16(&self) -> Option<u16> {
        if self.len() < mem::size_of::<u16>() {
            return None;
        }
        Some(u16::from_le_bytes(self.buf[..mem::size_of::<u16>()].try_into().ok()?))
    }

    pub fn get_u32(&self) -> Option<u32> {
        if self.len() < mem::size_of::<u32>() {
            return None;
        }
        Some(u32::from_le_bytes(self.buf[..mem::size_of::<u32>()].try_into().ok()?))
    }

    pub fn get_u64(&self) -> Option<u64> {
        if self.len() < mem::size_of::<u64>() {
            return None;
        }
        Some(u64::from_le_bytes(self.buf[..mem::size_of::<u64>()].try_into().ok()?))
    }
}

impl core::fmt::Debug for AmlStream<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.stream_start == 0 && self.len() == 0 {
            return write!(f, "AmlStream::empty()");
        }
        const PRINT_ALL_LEN: usize = 12;
        const PRINT_SUMMARY_LEN: usize = 8;
        let print_all = self.len() <= PRINT_ALL_LEN;
        let summary = self.take_n(PRINT_SUMMARY_LEN);
        let val = if print_all { self } else { &summary };
        write!(f, "[")?;
        for b in val.data() {
            write!(f, "{:02X},", b)?;
        }
        if !print_all {
            write!(f, "..], len={:#X}, ", self.len())?;
        } else {
            write!(f, "], ")?;
        }
        const HEADER_LEN: usize = 36;
        let pos = self.buf.as_ptr() as usize - self.stream_start;
        write!(f, "pos={:#X}, from header={:#X}, ", pos, pos + HEADER_LEN)?;
        write!(f, "start={:#X}", self.stream_start)
    }
}

// We ignore the stream_start for comparisons,
// as this is only used for tests
impl PartialEq<Self> for AmlStream<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.data() == other.data()
    }
}

impl PartialEq<&[u8]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8]) -> bool {
        self.data() == *other
    }
}

impl<const N: usize> PartialEq<&[u8; N]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8; N]) -> bool {
        self.data() == *other
    }
}

#[cfg(test)]
mod tests {
    use num::Integer;

    use super::*;

    #[test]
    fn test_stream_cmp() {
        let v: alloc::vec::Vec<u8> = alloc::vec![0, 1, 2, 3, 4];

        // test non-const buf compare
        assert_eq!(AmlStream::from_slice(&[]), &v[..0]);
        assert_ne!(AmlStream::from_slice(&[]), &v[..1]);
        assert_ne!(AmlStream::from_slice(&[0]), &v[..0]);
        assert_eq!(AmlStream::from_slice(&[0]), &v[..1]);
        assert_eq!(AmlStream::from_slice(&[0, 1]), &v[..2]);
        // ensure compare is still non-const even if the compiler gets smarter
        assert_eq!(
            AmlStream::from_slice(&[0, 2, 4]),
            &v.iter().filter_map(|i| if i.is_even() { Some(*i) } else { None }).collect::<alloc::vec::Vec<u8>>()[..]
        );
        assert_eq!(AmlStream::from_slice(&[0, 1, 2, 3, 4]), &v[..]);
        assert_ne!(AmlStream::from_slice(&[0, 0, 0, 0, 0]), &v[..]);
        assert_ne!(AmlStream::from_slice(&[0, 1, 2, 3, 4, 5]), &v[..]);

        // check various sizes of const buf compare
        assert_eq!(AmlStream::from_slice(&[]), &[]);
        assert_ne!(AmlStream::from_slice(&[]), &[0]);

        assert_ne!(AmlStream::from_slice(&[0]), &[]);
        assert_eq!(AmlStream::from_slice(&[0]), &[0]);

        assert_eq!(AmlStream::from_slice(&[0, 1]), &[0, 1]);
        assert_ne!(AmlStream::from_slice(&[0, 1]), &[0, 2]);

        assert_ne!(AmlStream::from_slice(&[0, 1]), &[0, 1, 2]);
        assert_eq!(AmlStream::from_slice(&[0, 1, 2]), &[0, 1, 2]);
        assert_ne!(AmlStream::from_slice(&[0, 1, 2]), &[0, 2, 1]);

        assert_ne!(AmlStream::from_slice(&[0, 1, 2]), &[0, 1, 2, 3]);
        assert_eq!(AmlStream::from_slice(&[0, 1, 2, 3]), &[0, 1, 2, 3]);
        assert_ne!(AmlStream::from_slice(&[0, 1, 2, 3]), &[3, 2, 1, 0]);

        // Check comparison of streams
        assert_eq!(AmlStream::from_slice(&[]), AmlStream::empty());
        assert_ne!(AmlStream::from_slice(&[0]), AmlStream::empty());
        assert_eq!(AmlStream::from_slice(&[0, 1, 2]), AmlStream::from_slice(&[0, 1, 2]));
        assert_ne!(AmlStream::from_slice(&[0, 1, 2]), AmlStream::from_slice(&[1, 2, 3]));
    }

    #[test]
    fn test_take() {
        let empty = AmlStream::empty();
        let len_zero = AmlStream::from_slice(&[]);
        let len_one = AmlStream::from_slice(&[0]);
        let len_four = AmlStream::from_slice(&[0, 1, 2, 3]);

        // Test first
        assert_eq!(empty.first(), None);
        assert_eq!(len_zero.first(), None);
        assert_eq!(len_one.first(), Some(&0));
        assert_eq!(len_four.first(), Some(&0));

        // Test take
        assert_eq!(len_zero.take(0, 0), AmlStream::empty());
        assert_eq!(len_one.take(0, 0), AmlStream::empty());
        assert_eq!(len_four.take(0, 0), AmlStream::empty());
        assert_eq!(len_zero.take(0, 0), AmlStream::empty());
        assert_eq!(len_one.take(0, 1), AmlStream::from_slice(&[0]));
        assert_eq!(len_four.take(0, 1), AmlStream::from_slice(&[0]));
        assert_eq!(len_four.take(1, 1), AmlStream::empty());
        assert_eq!(len_one.take(1, 2), AmlStream::empty());
        assert_eq!(len_four.take(1, 2), AmlStream::from_slice(&[1]));
        assert_eq!(len_four.take(2, 3), AmlStream::from_slice(&[2]));
        assert_eq!(len_four.take(3, 4), AmlStream::from_slice(&[3]));
        assert_eq!(len_four.take(3, 5), AmlStream::empty());
        assert_eq!(len_four.take(0, 2), AmlStream::from_slice(&[0, 1]));
        assert_eq!(len_four.take(1, 3), AmlStream::from_slice(&[1, 2]));
        assert_eq!(len_four.take(2, 4), AmlStream::from_slice(&[2, 3]));
        assert_eq!(len_four.take(1, 4), AmlStream::from_slice(&[1, 2, 3]));
        assert_eq!(len_four.take(0, 4), AmlStream::from_slice(&[0, 1, 2, 3]));
        assert_eq!(len_four.take(0, 5), AmlStream::empty());

        // Test take_n
        assert_eq!(len_zero.take_n(0), AmlStream::empty());
        assert_eq!(len_one.take_n(0), AmlStream::empty());
        assert_eq!(len_four.take_n(0), AmlStream::empty());
        assert_eq!(len_zero.take_n(1), AmlStream::empty());
        assert_eq!(len_four.take_n(5), AmlStream::empty());
        assert_eq!(len_one.take_n(1), AmlStream::from_slice(&[0]));
        assert_eq!(len_four.take_n(1), AmlStream::from_slice(&[0]));
        assert_eq!(len_one.take_n(2), AmlStream::empty());
        assert_eq!(len_four.take_n(2), AmlStream::from_slice(&[0, 1]));
        assert_eq!(len_four.take_n(3), AmlStream::from_slice(&[0, 1, 2]));
        assert_eq!(len_four.take_n(4), AmlStream::from_slice(&[0, 1, 2, 3]));
        assert_eq!(len_four.take_n(5), AmlStream::empty());

        // Test take_to_end
        assert_eq!(len_zero.take_to_end(0), AmlStream::empty());
        assert_eq!(len_one.take_to_end(0), AmlStream::from_slice(&[0]));
        assert_eq!(len_one.take_to_end(1), AmlStream::empty());
        assert_eq!(len_four.take_to_end(0), AmlStream::from_slice(&[0, 1, 2, 3]));
        assert_eq!(len_four.take_to_end(1), AmlStream::from_slice(&[1, 2, 3]));
        assert_eq!(len_four.take_to_end(2), AmlStream::from_slice(&[2, 3]));
        assert_eq!(len_four.take_to_end(3), AmlStream::from_slice(&[3]));
        assert_eq!(len_four.take_to_end(4), AmlStream::empty());
        assert_eq!(len_four.take_to_end(5), AmlStream::empty());

        // Test split_at
        assert_eq!(len_zero.split_at(0), (AmlStream::empty(), AmlStream::empty()));
        assert_eq!(len_zero.split_at(1), (AmlStream::empty(), AmlStream::empty()));
        assert_eq!(len_one.split_at(0), (AmlStream::empty(), AmlStream::from_slice(&[0])));
        assert_eq!(len_one.split_at(1), (AmlStream::from_slice(&[0]), AmlStream::empty()));
        assert_eq!(len_four.split_at(0), (AmlStream::empty(), AmlStream::from_slice(&[0, 1, 2, 3])));
        assert_eq!(len_four.split_at(1), (AmlStream::from_slice(&[0]), AmlStream::from_slice(&[1, 2, 3])));
        assert_eq!(len_four.split_at(2), (AmlStream::from_slice(&[0, 1]), AmlStream::from_slice(&[2, 3])));
        assert_eq!(len_four.split_at(3), (AmlStream::from_slice(&[0, 1, 2]), AmlStream::from_slice(&[3])));
        assert_eq!(len_four.split_at(4), (AmlStream::from_slice(&[0, 1, 2, 3]), AmlStream::empty()));
        assert_eq!(len_four.split_at(5), (AmlStream::empty(), AmlStream::empty()));
    }

    #[test]
    fn test_get_as() {
        let empty = AmlStream::empty();
        let n_8 = AmlStream::from_slice(&[4]);
        let n_16 = AmlStream::from_slice(&[3, 4]);
        let n_32 = AmlStream::from_slice(&[0xa, 0xb, 0xc, 0xd]);
        let n_64 = AmlStream::from_slice(&[1, 2, 3, 4, 5, 6, 7, 8]);

        // test get_u16
        assert_eq!(empty.get_u16(), None);
        assert_eq!(n_8.get_u16(), None);
        assert_eq!(n_16.get_u16(), Some(0x0403));
        assert_eq!(n_32.get_u16(), Some(0x0b0a));
        assert_eq!(n_64.get_u16(), Some(0x0201));

        // test get_u32
        assert_eq!(empty.get_u32(), None);
        assert_eq!(n_8.get_u32(), None);
        assert_eq!(n_16.get_u32(), None);
        assert_eq!(n_32.get_u32(), Some(0x0d0c0b0a));
        assert_eq!(n_64.get_u32(), Some(0x04030201));

        // test get_u64
        assert_eq!(empty.get_u64(), None);
        assert_eq!(n_8.get_u64(), None);
        assert_eq!(n_16.get_u64(), None);
        assert_eq!(n_32.get_u64(), None);
        assert_eq!(n_64.get_u64(), Some(0x0807060504030201));
    }

    #[test]
    fn test_debug() {
        let empty = AmlStream::empty();
        let len_zero = AmlStream::from_slice(&[]);
        let len_one = AmlStream::from_slice(&[0xa]);
        let len_eight = AmlStream::from_slice(&[0xa, 0xb, 0xa, 0xc, 0xd, 0xb, 0xa, 0xc]);
        let len_twelve = AmlStream::from_slice(&[0xa, 0xb, 0xa, 0xc, 0xd, 0xb, 0xa, 0xc, 0xf, 0xe, 0xd, 0xc]);
        let len_thirteen = AmlStream::from_slice(&[0xf, 0xa, 0xb, 0xa, 0xc, 0xd, 0xb, 0xa, 0xc, 0xf, 0xe, 0xd, 0xc]);

        // test Debug formatting
        assert_eq!(alloc::format!("{:?}", empty), "AmlStream::empty()");
        assert_eq!(alloc::format!("{empty:?}"), "AmlStream::empty()");
        assert_eq!(
            alloc::format!("{:?}", len_zero),
            alloc::format!("[], pos={:#X}, from header={:#X}, start={:#X}", 0, 36, len_zero.stream_start)
        );
        assert_eq!(
            alloc::format!("{:?}", len_one),
            alloc::format!("[0A,], pos={:#X}, from header={:#X}, start={:#X}", 0, 36, len_one.stream_start)
        );
        assert_eq!(
            alloc::format!("{:?}", len_eight),
            alloc::format!(
                "[0A,0B,0A,0C,0D,0B,0A,0C,], pos={:#X}, from header={:#X}, start={:#X}",
                0,
                36,
                len_eight.stream_start
            )
        );
        assert_eq!(
            alloc::format!("{:?}", len_twelve),
            alloc::format!(
                "[0A,0B,0A,0C,0D,0B,0A,0C,0F,0E,0D,0C,], pos={:#X}, from header={:#X}, start={:#X}",
                0,
                36,
                len_twelve.stream_start
            )
        );
        assert_eq!(
            alloc::format!("{:?}", len_thirteen),
            alloc::format!(
                "[0F,0A,0B,0A,0C,0D,0B,0A,..], len={:#X}, pos={:#X}, from header={:#X}, start={:#X}",
                13,
                0,
                36,
                len_thirteen.stream_start
            )
        );
        assert_eq!(
            alloc::format!("{:?}", len_thirteen.take_n(12)),
            alloc::format!(
                "[0F,0A,0B,0A,0C,0D,0B,0A,0C,0F,0E,0D,], pos={:#X}, from header={:#X}, start={:#X}",
                0,
                36,
                len_thirteen.stream_start
            )
        );
        assert_eq!(
            alloc::format!("{:?}", len_thirteen.take_to_end(1)),
            alloc::format!(
                "[0A,0B,0A,0C,0D,0B,0A,0C,0F,0E,0D,0C,], pos={:#X}, from header={:#X}, start={:#X}",
                1,
                37,
                len_thirteen.stream_start
            )
        );
    }
}
