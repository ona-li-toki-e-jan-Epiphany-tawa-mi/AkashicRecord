################################################################################
# This file is part of yOS.                                                    #
#                                                                              #
# yOS is free software: you can redistribute it and/or modify it under the     #
# terms of the GNU General Public License as published by the Free Software    #
# Foundation, either version 3 of the License, or (at your option) any later   #
# version.                                                                     #
#                                                                              #
# yOS is distributed in the hope that it will be useful, but WITHOUT ANY       #
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    #
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more        #
# details.                                                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# yOS. If not, see <https://www.gnu.org/licenses/>.                            #
################################################################################

# The file cargo tosses the outputted object file to.
YTTERBIUM_OBJECT_FILE := target/x86_64-unknown-none/release/deps/ytterbium-*.o
# The assembly code to shove at the start of the kernal to accept control from
# Ferrite.
KERNAL_ENTRANCE := kernal-entrance.asm

KERNAL_ENTRANCE_OBJECT_FILE := kernal-entrance.o
OUTPUT_FILE                 := ytterbium.bin



# Builds the kernal.
.PHONY: all
all:
#   Release mode as debug symbols/other won't help anything in kernal land.
	cargo rustc --release -- --emit=obj
#   Compile kernal entrance assembly code.
	fasm ${KERNAL_ENTRANCE} ${KERNAL_ENTRANCE_OBJECT_FILE}
#   Creates a raw binary from the kernal and kernal entrance object files that
#   can be joined with the Ferrite binary. It will be loaded right after Ferrite
#   at 0x7D00. The order of the object files here is very important; the kernal
#   entrance MUST come first.
	ld --output=${OUTPUT_FILE} -Ttext 0x7D00 ${KERNAL_ENTRANCE_OBJECT_FILE} ${YTTERBIUM_OBJECT_FILE} --oformat binary



# Shows the disassembly of the finished kernal binary.
.PHONY: view-disassembly
view-disassembly:
	objdump --disassemble-all --disassembler-options=intel,x86-64 --target=binary --architecture=i386 ${OUTPUT_FILE}

.PHONY: clean
clean:
	-rm *.bin *.o
