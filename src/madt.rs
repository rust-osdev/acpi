use bit_field::BitField;
use core::mem;
use sdt::SdtHeader;
use {AcpiError, PhysicalMapping};

#[repr(C, packed)]
pub struct Madt {
    // header
    header: SdtHeader,

    local_interrupt_controller_address: u32,
    flags: u32,
    // TODO ?
}

impl Madt {
    pub fn validate(&self) -> Result<(), AcpiError> {
        self.header.validate(b"APIC")
    }

    pub fn length(&self) -> u32 {
        self.header.length()
    }

    #[cfg(test)]
    fn make_testcase(
        size: u32,
        checksum: u8,
        oem_id: [u8; 6],
        oem_table_id: [u8; 8],
        oem_revision: u32,
        creator_id: u32,
        creator_revision: u32,
    ) -> Madt {
        Madt {
            header: SdtHeader::make_testcase(
                *b"APIC",
                size,
                45,
                checksum, // checksum
                oem_id,
                oem_table_id,
                oem_revision,
                creator_id,
                creator_revision,
            ),
            local_interrupt_controller_address: 0xDEADBEEF as u32,
            flags: 0 as u32,
        }
    }
}

pub fn parse_madt(mapping: &PhysicalMapping<Madt>) -> Result<(), AcpiError> {
    (*mapping).validate()?;

    let mut i: usize = mem::size_of::<Madt>();
    while i < (*mapping).length() as usize {
        let struct_type: u8 =
            unsafe { *((mapping.virtual_start.as_ptr() as *const u8).add(i) as *const u8) } as u8;
        let length: u8 = unsafe {
            *((mapping.virtual_start.as_ptr() as *const u8).add(i + 1) as *const u8)
        } as u8;
        match struct_type {
            MADT_PROCESSOR_LOCAL_APIC => unsafe {
                (*((mapping.virtual_start.as_ptr() as *const u8).add(i)
                    as *const MadtProcessorLocalAPIC))
                    .validate()?;
            },
            MADT_IO_APIC => unsafe {
                (*((mapping.virtual_start.as_ptr() as *const u8).add(i) as *const MadtIOAPIC))
                    .validate()?;
            },
            MADT_INTERRUPT_SOURCE_OVERRIDE => unsafe {
                (*((mapping.virtual_start.as_ptr() as *const u8).add(i)
                    as *const MadtInterruptSourceOverride))
                    .validate()?;
            },

            MADT_LOCAL_APIC_NMI => unsafe {
                (*((mapping.virtual_start.as_ptr() as *const u8).add(i)
                    as *const MadtLocalAPICNMI))
                    .validate()?;
            },

            _ => {
                unimplemented!();
            }
        }
        i = i + (length as usize);
    }

    Ok(())
}

const MADT_PROCESSOR_LOCAL_APIC: u8 = 0;
const MADT_PROCESSOR_LOCAL_APIC_LENGTH: u8 = 8;

#[repr(C, packed)]
struct MadtProcessorLocalAPIC {
    struct_type: u8, // 0
    length: u8,      // 8
    acpi_processor_uid: u8,
    apic_id: u8,
    flags: u32,
}

impl MadtProcessorLocalAPIC {
    fn validate(&self) -> Result<(), AcpiError> {
        // TODO change the errors
        // Also is it also needed to check struct type and length ??
        if self.struct_type != 0 {
            return Err(AcpiError::Error);
        }
        if self.length != 8 {
            return Err(AcpiError::Error);
        }
        if self.flags.get_bits(1..31) != 0 {
            return Err(AcpiError::Error);
        }
        Ok(())
    }

    fn length() -> u32 {
        8 as u32
    }
    #[cfg(test)]
    fn make_testcase(proc_uid: u8, apic_id: u8) -> MadtProcessorLocalAPIC {
        let mut test = MadtProcessorLocalAPIC {
            struct_type: MADT_PROCESSOR_LOCAL_APIC,
            length: MADT_PROCESSOR_LOCAL_APIC_LENGTH,
            acpi_processor_uid: proc_uid,
            apic_id: apic_id,
            flags: 0,
        };
        test.flags.set_bit(0, true);
        test
    }
}

const MADT_IO_APIC: u8 = 1;
const MADT_IO_APIC_LENGTH: u8 = 12;

#[repr(C, packed)]
struct MadtIOAPIC {
    struct_type: u8, // 1
    length: u8,      // 12
    io_apic_id: u8,
    reserved: u8, // 0
    io_apic_address: u32,
    global_system_interrupt_base: u32,
}

impl MadtIOAPIC {
    fn validate(&self) -> Result<(), AcpiError> {
        if self.struct_type != 1 {
            return Err(AcpiError::Error);
        }

        if self.length != 12 {
            return Err(AcpiError::Error);
        }

        if self.reserved != 0 {
            return Err(AcpiError::Error);
        }

        Ok(())
    }

    fn length() -> u32 {
        12 as u32
    }

    #[cfg(test)]
    fn make_testcase(apic_id: u8, global_base: u32) -> MadtIOAPIC {
        MadtIOAPIC {
            struct_type: MADT_IO_APIC,
            length: MADT_IO_APIC_LENGTH,
            io_apic_id: apic_id,
            reserved: 0,
            io_apic_address: 0xDEADBEEF,
            global_system_interrupt_base: global_base,
        }
    }
}

const MADT_INTERRUPT_SOURCE_OVERRIDE: u8 = 2;
const MADT_INTERRUPT_SOURCE_OVERRIDE_LENGTH: u8 = 10;

#[repr(C, packed)]
struct MadtInterruptSourceOverride {
    struct_type: u8, // 2
    length: u8,      // 10
    bus: u8,         // 0
    source: u8,
    global_system_interrupt: u32,
    flags: u16,
}

impl MadtInterruptSourceOverride {
    fn validate(&self) -> Result<(), AcpiError> {
        if self.struct_type != 2 {
            return Err(AcpiError::Error);
        }

        if self.length != 10 {
            return Err(AcpiError::Error);
        }

        if self.bus != 0 {
            return Err(AcpiError::Error);
        }

        if self.flags.get_bits(4..15) != 0 {
            return Err(AcpiError::Error);
        }

        Ok(())
    }

    fn length() -> u32 {
        10 as u32
    }

    #[cfg(test)]
    fn make_testcase(irq: u8) -> MadtInterruptSourceOverride {
        MadtInterruptSourceOverride {
            struct_type: MADT_INTERRUPT_SOURCE_OVERRIDE,
            length: MADT_INTERRUPT_SOURCE_OVERRIDE_LENGTH,
            bus: 0,
            source: irq,
            global_system_interrupt: 0xDEADBEEF,
            flags: 0,
        }
    }
}

#[repr(C, packed)]
struct MadtNMISource {
    struct_type: u8, // 3
    length: u8,      //8
    flags: u16,
    global_system_interrupt: u32,
}

const MADT_LOCAL_APIC_NMI: u8 = 4;
const MADT_LOCAL_APIC_NMI_LENGTH: u8 = 6;

#[repr(C, packed)]
struct MadtLocalAPICNMI {
    struct_type: u8, // 4
    length: u8,      // 6
    acpi_processor_uid: u8,
    flags: u16,
    local_apic_lint: u8,
}

impl MadtLocalAPICNMI {
    fn validate(&self) -> Result<(), AcpiError> {
        if self.struct_type != 4 {
            return Err(AcpiError::Error);
        }

        if self.length != 6 {
            return Err(AcpiError::Error);
        }

        if self.flags.get_bits(4..15) != 0 {
            return Err(AcpiError::Error);
        }

        Ok(())
    }

    fn length() -> u32 {
        6 as u32
    }

    #[cfg(test)]
    fn make_testcase(uid: u8) -> MadtLocalAPICNMI {
        MadtLocalAPICNMI {
            struct_type: MADT_LOCAL_APIC_NMI,
            length: MADT_LOCAL_APIC_NMI_LENGTH,
            acpi_processor_uid: uid,
            flags: 0,
            local_apic_lint: 0,
        }
    }
}

#[repr(C, packed)]
struct MadtLocalAPICAddressOverride {
    struct_type: u8, // 5
    length: u8,      // 12
    reserved: u16,   //0
    local_apic_address: u64,
}

#[repr(C, packed)]
struct MadtIOSAPIC {
    struct_type: u8, // 6
    length: u8,      // 16
    io_apic_id: u8,
    reserved: u8, // 0
    global_system_interrupt_base: u32,
    io_sapic_address: u64,
}

#[repr(C, packed)]
struct MadtLocalSAPIC {
    struct_type: u8, // 7
    length: u8,      // depends
    acpi_processor_id: u8,
    local_sapic_id: u8,
    local_sapic_eid: u8,
    reserved: [u8; 3], // 0
    flags: u32,
    acpi_processor_uid_value: u32,
    acpi_processor_uid_string: u8, // TODO depends of length
}

#[repr(C, packed)]
struct MadtPlatformInterruptSource {
    struct_type: u8, // 8
    length: u8,      // 16
    flags: u16,
    interrupt_type: u8, // 1, 2, or 3 only
    processor_id: u8,
    processor_eid: u8,
    io_sapic_vector: u8,
    global_system_interrupt: u32,
    platform_interrupt_source_flags: u32,
}

#[repr(C, packed)]
struct MadtProcessorLocalx2APIC {
    struct_type: u8, // 9
    length: u8,      // 16
    reserved: u16,   // 0
    x2apic_id: u32,
    flags: u32,
    acpi_processor_uid: u32,
}

#[repr(C, packed)]
struct MadtLocalx2APICNMI {
    struct_type: u8, // 0x0A
    length: u8,      //12
    flags: u16,
    acpi_processor_uid: u32,
    local_x2apic_lint: u8,
    reserved: [u8; 3], // 0
}

#[repr(C, packed)]
struct MadtGICC {
    struct_type: u8, // 0x0B
    length: u8,      // 80
    reserved: u16,   // 0
    cpu_interface_number: u32,
    acpi_processor_uid: u32,
    flags: u32,
    parking_protocol_version: u32,
    performance_interrupt_gsiv: u32,
    parked_address: u64,
    physical_base_address: u64,
    gicv: u64,
    gich: u64,
    vgic_maintenance_interrupt: u32,
    gicr_base_address: u64,
    mpidr: u64,
    processor_power_efficiency_class: u8,
    reserved2: [u8; 3], // 0
}

#[repr(C, packed)]
struct MadtGICD {
    struct_type: u8, // 0x0C
    length: u8,      //24
    reserved: u16,   // 0
    gic_id: u32,
    physical_base_address: u64,
    system_vector_base: u32,

    /*
     * gic_version
     * 0x00: No version specified
     * 0x01: GICv1
     * 0x02: GICv2
     * 0x03: GICv3
     * 0x04: GICv4
     * 0x05-0xFF: reserved for future use
     */
    gic_version: u8,
    reserved2: [u8; 3],
}

#[repr(C, packed)]
struct MadtGICMSIFrame {
    struct_type: u8, // 0x0D
    length: u8,      // 24
    reserved: u16,   // 0
    gic_msi_frame_id: u32,
    physical_base_address: u64,
    flags: u32,
    spi_count: u16,
    spi_base: u16,
}

#[repr(C, packed)]
struct MadtGICR {
    struct_type: u8, // 0x0E
    length: u8,      // 16
    reserved: u16,   // 0
    discovery_range_base_address: u64,
    discovery_range_length: u32,
}

#[repr(C, packed)]
struct MadtGICITS {
    struct_type: u8, // 0x0F
    length: u8,      // 20
    reserved: u16,   // 0
    gic_its_id: u32,
    physical_base_address: u64,
    reserved2: u32, // 0
}

#[cfg(test)]
#[repr(C, packed)]
pub struct TestMadt {
    madt: Madt,
    proc_local_apic_0: MadtProcessorLocalAPIC,
    proc_local_apic_1: MadtProcessorLocalAPIC,
    io_apic: MadtIOAPIC,
    int_source_override_0: MadtInterruptSourceOverride,
    int_source_override_1: MadtInterruptSourceOverride,
    int_source_override_2: MadtInterruptSourceOverride,
    nmi: MadtLocalAPICNMI,
}
#[cfg(test)]
impl TestMadt {
    pub(crate) fn make_testcase(oem_id: &[u8; 6]) -> TestMadt {
        let mut test = TestMadt {
            madt: Madt::make_testcase(
                mem::size_of::<TestMadt>() as u32,
                0,
                *oem_id,
                *b"OEMMADT ",
                0xDEADBEEF,
                0xDEADBEEF,
                0xDEADBEEF,
            ),
            proc_local_apic_0: MadtProcessorLocalAPIC::make_testcase(0, 0),
            proc_local_apic_1: MadtProcessorLocalAPIC::make_testcase(1, 1),
            io_apic: MadtIOAPIC::make_testcase(0, 0),
            int_source_override_0: MadtInterruptSourceOverride::make_testcase(3),
            int_source_override_1: MadtInterruptSourceOverride::make_testcase(6),
            int_source_override_2: MadtInterruptSourceOverride::make_testcase(9),
            nmi: MadtLocalAPICNMI::make_testcase(0xFF),
        };
        let mut sum: usize = 0;
        for i in 0..test.madt.header.length() {
            sum += unsafe { *(&test as *const TestMadt as *const u8).offset(i as isize) } as usize;
        }
        test.madt.header.set_right_checksum();
        test
    }
}
