use core::{mem, convert::TryInto};

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
        Self {
            stream_start: slice.as_ptr() as usize,
            buf: slice,
        }
    }

    pub fn empty() -> Self {
        Self::from_slice(&[])
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }
    
    pub fn slice(&self, start: usize, end: usize) -> Self {
        if start > self.len() || end > self.len() || start > end {
            return Self::empty();
        }
        Self {
            stream_start: self.stream_start,
            buf: &self.buf[start..end]
        }
    }

    pub fn content(&self) -> &[u8] {
        self.buf
    }

    pub fn slice_from_start(&self, index: usize) -> Self {
        self.slice(0, index)
    }

    pub fn slice_to_end(&self, index: usize) -> Self {
        self.slice(index, self.len())
    }

    pub fn first(&self) -> Option<&u8> {
        if self.buf.len() < 1 {
            return None;
        }
        self.buf.first()
    }

    pub fn get(&self, index: usize) -> Option<&u8> {
        self.buf.get(index)
    }

    pub fn get_u16(&self, index: usize) -> Option<u16> {
        if self.len() - index < mem::size_of::<u16>() {
            return None;
        }
        Some(u16::from_le_bytes(self.buf[index..index + mem::size_of::<u16>()].try_into().ok()?))
    }

    pub fn get_u32(&self, index: usize) -> Option<u32> {
        if self.len() - index < mem::size_of::<u32>() {
            return None;
        }
        Some(u32::from_le_bytes(self.buf[index..index + mem::size_of::<u32>()].try_into().ok()?))
    }

    pub fn get_u64(&self, index: usize) -> Option<u64> {
        if self.len() - index < mem::size_of::<u64>() {
            return None;
        }
        Some(u64::from_le_bytes(self.buf[index..index + mem::size_of::<u64>()].try_into().ok()?))
    }

    pub fn split_at(&self, mid: usize) -> (Self, Self) {
        (self.slice(0, mid), self.slice_to_end(mid))
    }
}

impl core::fmt::Debug for AmlStream<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        const PRINT_ALL_LEN: usize = 12;
        const PRINT_SUMMARY_LEN: usize = 8;
        let print_all = self.len() <= PRINT_ALL_LEN;
        let summary = if print_all {
            self.clone()
        } else {
            self.slice_from_start(PRINT_SUMMARY_LEN)
        };
        write!(f, "[")?;
        for b in summary.content() {
            write!(f, "{:2X},", b)?;
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

impl PartialEq<Self> for AmlStream<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.content() == other.content()
    }
}

impl PartialEq<[u8]> for AmlStream<'_> {
    fn eq(&self, other: &[u8]) -> bool {
        self.content() == other
    }
}

impl PartialEq<&[u8]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8]) -> bool {
        self.content() == *other
    }
}

impl PartialEq<[u8; 0]> for AmlStream<'_> {
    fn eq(&self, _other: &[u8; 0]) -> bool {
        self.len() == 0
    }
}

impl PartialEq<&[u8; 0]> for AmlStream<'_> {
    fn eq(&self, _other: &&[u8; 0]) -> bool {
        self.len() == 0
    }
}

impl PartialEq<[u8; 1]> for AmlStream<'_> {
    fn eq(&self, other: &[u8; 1]) -> bool {
        self.content() == other
    }
}

impl PartialEq<&[u8; 1]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8; 1]) -> bool {
        self.content() == *other
    }
}

impl PartialEq<[u8; 2]> for AmlStream<'_> {
    fn eq(&self, other: &[u8; 2]) -> bool {
        self.content() == other
    }
}

impl PartialEq<&[u8; 2]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8; 2]) -> bool {
        self.content() == *other
    }
}

impl PartialEq<[u8; 3]> for AmlStream<'_> {
    fn eq(&self, other: &[u8; 3]) -> bool {
        self.content() == other
    }
}

impl PartialEq<&[u8; 3]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8; 3]) -> bool {
        self.content() == *other
    }
}

impl PartialEq<[u8; 4]> for AmlStream<'_> {
    fn eq(&self, other: &[u8; 4]) -> bool {
        self.content() == other
    }
}

impl PartialEq<&[u8; 4]> for AmlStream<'_> {
    fn eq(&self, other: &&[u8; 4]) -> bool {
        self.content() == *other
    }
}
