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

FERRITE_DIRECTORY   := ferrite
YTTERBIUM_DIRECTORY := ytterbium

FERRITE_BINARY   := ${FERRITE_DIRECTORY}/ferrite.bin
YTTERBIUM_BINARY := ${YTTERBIUM_DIRECTORY}/ytterbium.bin

OUTPUT_FILE := yOS.bin



# Builds the whole operating system.
.PHONY: all
all:
	make -C ${FERRITE_DIRECTORY}
	make -C ${YTTERBIUM_DIRECTORY}
	make ${OUTPUT_FILE}

${OUTPUT_FILE}: ${FERRITE_BINARY} ${YTTERBIUM_BINARY}
	cat $^ > $@



# Boots the system inside QEmu.
.PHONY: boot
boot:
	qemu-system-x86_64 -drive file=${OUTPUT_FILE},format=raw,index=0,media=disk

.PHONY: clean
clean:
	-rm *.bin
	-make -C ${FERRITE_DIRECTORY} clean
	-make -C ${YTTERBIUM_DIRECTORY} clean
