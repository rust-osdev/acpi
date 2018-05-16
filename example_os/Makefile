arch ?= x86_64
target := $(arch)-example-os
kernel := build/kernel-$(arch).bin
iso := build/os-$(arch).iso
linker_script := src/$(arch)/linker.ld
grub_cfg := src/$(arch)/grub.cfg
rust_lib := target/$(target)/debug/libexample_os.a

asm_sources := $(wildcard src/$(arch)/*.asm)
asm_objs := $(patsubst src/$(arch)/%.asm, build/$(arch)/%.o, $(asm_sources))

.PHONY: clean cargo build_dir

$(iso): $(kernel) $(grub_cfg)
	mkdir -p build/isofiles/boot/grub
	cp $(kernel) build/isofiles/boot/kernel.bin
	cp $(grub_cfg) build/isofiles/boot/grub
	grub2-mkrescue -o $(iso) build/isofiles 2> /dev/null
	rm -r build/isofiles

$(kernel): build_dir cargo $(rust_lib) $(asm_objs) $(linker_script)
	ld -n --gc-sections -T $(linker_script) -o $(kernel) $(asm_objs) $(rust_lib)

cargo:
	RUST_TARGET_PATH="$(shell pwd)" cargo xbuild --target $(target)

build_dir:
	mkdir -p build

build/$(arch)/%.o: src/$(arch)/%.asm
	mkdir -p $(shell dirname $@)
	nasm -felf64 $< -o $@

clean:
	rm -rf build
	rm -rf target
	rm Cargo.lock
