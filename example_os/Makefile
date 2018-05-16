arch ?= x86_64
target := $(arch)-example-os
kernel := build/kernel-$(arch).bin
iso := build/os-$(arch).iso
linker_script := src/$(arch)/linker.ld
grub_cfg := src/$(arch)/grub.cfg
rust_lib := target/$(target)/debug/libexample_os.a

asm_sources := $(wildcard src/$(arch)/*.asm)
asm_objs := $(patsubst src/$(arch)/%.asm, build/$(arch)/%.o, $(asm_sources))

.PHONY: test clean cargo build_dir

test: $(iso)
	@# Debug exit on QEMU gives exit code 1, so we override it
	@# We also cut the first 16 lines of the serial output, as it's just BIOS stuff
	@qemu-system-x86_64 --no-reboot \
					    -nographic \
					    -display none \
					    -device isa-debug-exit,iobase=0xf4,iosize=0x04 \
					    -cdrom build/os-$(arch).iso \
	| tail -n +16 > serial.txt || true

$(iso): $(kernel) $(grub_cfg)
	@mkdir -p build/isofiles/boot/grub
	@cp $(kernel) build/isofiles/boot/kernel.bin
	@cp $(grub_cfg) build/isofiles/boot/grub
	@grub2-mkrescue -o $(iso) build/isofiles 2> /dev/null
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
	@rm Cargo.lock
