arch ?= x86_64
target := $(arch)-test-os
kernel := build/kernel-$(arch).bin
iso := build/os-$(arch).iso
linker_script := src/$(arch)/linker.ld
grub_cfg := src/$(arch)/grub.cfg
rust_lib := target/$(target)/debug/libtest_os.a
grub_mkrescue ?= grub2-mkrescue

asm_sources := $(wildcard src/$(arch)/*.asm)
asm_objs := $(patsubst src/$(arch)/%.asm, build/$(arch)/%.o, $(asm_sources))

.PHONY: test clean cargo build_dir

test: $(iso)
	@# Debug exit on QEMU gives exit code 1, so we override it
	@# We also cut the first 16 lines of the serial output, as it's just BIOS stuff
	qemu-system-x86_64 --no-reboot \
					    -nographic \
					    -display none \
					    -device isa-debug-exit,iobase=0xf4,iosize=0x04 \
					    -cdrom build/os-$(arch).iso > serial.txt
	@echo BEGIN SERIAL OUTPUT
	@cat serial.txt
	@echo END SERIAL OUTPUT
	@cat serial.txt | tail -n +16 > serial_tail.txt
	@echo BEGIN KERNEL SERIAL OUTPUT
	@cat serial_tail.txt
	@echo END KERNEL SERIAL OUTPUT
	@# Now we test whether the output file has the string "Passed" in it
	@grep -Fxq "Passed" serial_tail.txt

$(iso): $(kernel) $(grub_cfg)
	@mkdir -p build/isofiles/boot/grub
	@cp $(kernel) build/isofiles/boot/kernel.bin
	@cp $(grub_cfg) build/isofiles/boot/grub
	@$(grub_mkrescue) -o $(iso) build/isofiles
	@rm -r build/isofiles

$(kernel): build_dir cargo $(rust_lib) $(asm_objs) $(linker_script)
	@ld -n --gc-sections -T $(linker_script) -o $(kernel) $(asm_objs) $(rust_lib)

cargo:
	@RUST_TARGET_PATH="$(shell pwd)" cargo xbuild --target $(target)

build_dir:
	@mkdir -p build

build/$(arch)/%.o: src/$(arch)/%.asm
	@mkdir -p $(shell dirname $@)
	@nasm -felf64 $< -o $@

clean:
	@rm -rf build
	@rm -rf target
	@rm -f Cargo.lock
	@rm -f serial.txt serial_tail.txt
