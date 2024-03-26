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

FERRITE_FILE  := ferrite.asm
INCLUDE_FILES := 16-bit-utilities.inc long-mode-tools.inc
OUTPUT_FILE   := ferrite.bin


# Builds the Ferrite bootloader.
# Currently nothing is put after Ferrite, so there is nothing to load.
${OUTPUT_FILE}: ${FERRITE_FILE} ${INCLUDE_FILES}
	fasm $< $@ -d YTTERBIUM_SECTOR_COUNT=0



# Boots the bootloader inside QEmu.
.PHONY: boot
boot:
	qemu-system-x86_64 -drive file=${OUTPUT_FILE},format=raw,index=0,media=disk

.PHONY: clean
clean:
	-rm *.bin
