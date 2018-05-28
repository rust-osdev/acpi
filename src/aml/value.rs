use super::AmlError;

#[derive(Debug)]
pub enum RegionSpace {
    SystemMemory,
    SystemIo,
    PciConfig,
    EmbeddedControl,
    SMBus,
    SystemCmos,
    PciBarTarget,
    IPMI,
    GeneralPurposeIo,
    GenericSerialBus,
    OemDefined(u8),
}

#[derive(Debug)]
pub enum AmlValue {
    Integer(u64),

    OpRegion {
        region: RegionSpace,
        offset: u64,
        length: u64,
    },
}

impl AmlValue {
    pub fn as_integer(&self) -> Result<u64, AmlError> {
        match self {
            AmlValue::Integer(value) => Ok(*value),

            _ => Err(AmlError::IncompatibleValueConversion),
        }
    }
}
