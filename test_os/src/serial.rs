pub struct SerialPort;

/*
 * NOTE: This isn't particularly good code. If you want a serial implementation, I don't recommend
 * using this.
 */
impl SerialPort {
    pub fn init() {
        /*
         * This initialises COM1
         */
        unsafe {
            asm!("out dx, al" : : "{dx}"(0x3f8), "{al}"(0x00) : : "intel", "volatile");
            asm!("out dx, al" : : "{dx}"(0x3fb), "{al}"(0x80) : : "intel", "volatile");
            asm!("out dx, al" : : "{dx}"(0x3f8), "{al}"(0x03) : : "intel", "volatile");
            asm!("out dx, al" : : "{dx}"(0x3f9), "{al}"(0x00) : : "intel", "volatile");
            asm!("out dx, al" : : "{dx}"(0x3fb), "{al}"(0x03) : : "intel", "volatile");
            asm!("out dx, al" : : "{dx}"(0x3fa), "{al}"(0xc7) : : "intel", "volatile");
            asm!("out dx, al" : : "{dx}"(0x3fc), "{al}"(0x0b) : : "intel", "volatile");
        }

        /*
         * We output a newline here to make cutting off the BIOS output easier.
         */
        SerialPort::write_byte(b'\n');
    }

    fn ready_to_write() -> bool {
        let result: u8;
        unsafe {
            asm!("in al, dx" : "={al}"(result) : "{dx}"(0x3fd) : : "intel", "volatile");
        }
        (result & 0x20) != 0
    }

    fn write_byte(byte: u8) {
        while !SerialPort::ready_to_write() {
            // XXX: required to stop the loop from being optimized away
            unsafe {
                asm!("" :::: "volatile");
            }
        }

        unsafe {
            asm!("out dx, al" : : "{dx}"(0x3f8), "{al}"(byte) : : "intel", "volatile");
        }
    }

    pub fn write(s: &str) {
        for byte in s.bytes() {
            // XXX: Real serial recievers generally expect carriage returns. Because we're
            // parsing the output on a POSIX system, we won't bother. In a real implementation, you
            // should detect '\n' here and actually emit '\n\r'.
            SerialPort::write_byte(byte);
        }
    }
}
