use super::AmlError;

#[derive(Clone)]
pub struct AmlStream<'a> {
    data: &'a [u8],
    offset: u32, // TODO: PkgLength can't be longer than u32, but can a whole AML stream?
}

impl<'a> AmlStream<'a> {
    pub unsafe fn new(data: &'a [u8]) -> AmlStream<'a> {
        AmlStream { data, offset: 0 }
    }

    pub fn peek(&self) -> Result<u8, AmlError> {
        if (self.offset + 1) >= self.len() {
            Err(AmlError::EndOfStream)
        } else {
            Ok(self.data[self.offset as usize])
        }
    }

    pub fn next(&mut self) -> Result<u8, AmlError> {
        let byte = self.peek()?;
        self.offset += 1;
        Ok(byte)
    }

    pub fn next_u16(&mut self) -> Result<u16, AmlError> {
        let first_byte = self.next()?;
        let second_byte = self.next()?;
        Ok(first_byte as u16 + ((second_byte as u16) << 8))
    }

    pub fn next_u32(&mut self) -> Result<u32, AmlError> {
        let first_byte = self.next()?;
        let second_byte = self.next()?;
        let third_byte = self.next()?;
        Ok(first_byte as u32 + ((second_byte as u32) << 8) + ((third_byte as u32) << 16))
    }

    pub fn next_u64(&mut self) -> Result<u64, AmlError> {
        let first_byte = self.next()?;
        let second_byte = self.next()?;
        let third_byte = self.next()?;
        let forth_byte = self.next()?;
        Ok(first_byte as u64
            + ((second_byte as u64) << 8)
            + ((third_byte as u64) << 16)
            + ((forth_byte as u64) << 24))
    }

    pub fn lookahead(&self, amount: u32) -> Result<u8, AmlError> {
        match self.offset.checked_add(amount) {
            Some(offset) => Ok(self.data[offset as usize]),
            None => Err(AmlError::EndOfStream),
        }
    }

    pub fn len(&self) -> u32 {
        self.data.len() as u32
    }

    /// This gets the current offset into the stream
    pub fn offset(&self) -> u32 {
        self.offset
    }
}
