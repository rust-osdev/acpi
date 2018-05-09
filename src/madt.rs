use sdt::SdtHeader;
use AcpiError;

#[repr(C, packed)]
struct Madt {
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
}

#[repr(C, packed)]
struct MadtProcessorLocalAPIC {
    struct_type: u8, // 0
    length: u8,      // 8
    acpi_processor_uid: u8,
    apic_id: u8,
    flags: u32,
}

#[repr(C, packed)]
struct MadtIOApic {
    struct_type: u8, // 1
    length: u8,      // 12
    io_apic_id: u8,
    reserved: u8, // 0
    io_apic_address: u32,
    global_system_interrupt_base: u32,
}

#[repr(C, packed)]
struct MadtInterruptSourceOverride {
    struct_type: u8, // 2
    length: u8,      // 10
    bus: u8,         // 0
    source: u8,
    global_system_interrupt: u32,
    flags: u16,
}

#[repr(C, packed)]
struct MadtNMISource {
    struct_type: u8, // 3
    length: u8,      //8
    flags: u16,
    global_system_interrupt: u32,
}

#[repr(C, packed)]
struct MadtLocalAPICNMI {
    struct_type: u8, // 4
    length: u8,      // 6
    acpi_processor_uid: u8,
    flags: u16,
    local_apic_lint: u8,
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
