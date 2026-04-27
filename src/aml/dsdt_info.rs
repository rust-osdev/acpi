#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntegerSize {
    FourBytes = 4,
    EightBytes = 8,
}

#[derive(Debug, Clone)]
pub struct DsdtInfo {
    #[allow(dead_code)]
    pub revision: u8,
    pub integer_size: IntegerSize,
}

impl DsdtInfo {
    pub fn from_revision(revision: u8) -> DsdtInfo {
        DsdtInfo {
            integer_size: if revision >= 2 { IntegerSize::EightBytes } else { IntegerSize::FourBytes },
            revision,
        }
    }
}
