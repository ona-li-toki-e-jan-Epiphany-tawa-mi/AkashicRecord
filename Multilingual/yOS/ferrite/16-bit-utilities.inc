;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is part of yOS.                                                  ;;
;;                                                                            ;;
;; yOS is free software: you can redistribute it and/or modify it under the   ;;
;; terms of the GNU General Public License as published by the Free Software  ;;
;; Foundation, either version 3 of the License, or (at your option) any later ;;
;; version.                                                                   ;;
;;                                                                            ;;
;; yOS is distributed in the hope that it will be useful, but WITHOUT ANY     ;;
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS  ;;
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more      ;;
;; details.                                                                   ;;
;;                                                                            ;;
;; You should have received a copy of the GNU General Public License along    ;;
;; with yOS. If not, see <https://www.gnu.org/licenses/>.                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some utility functions for 16-bit real mode.
;; Written in FASM.
;;
;; TODO make read_sectors_16 macro more convienient to use.
;;


;; https://www.ctyme.com/intr/int-10.htm
VIDEO_INTERRUPT equ 0x10
;; Interrupt 0x10 mode to print characters to the screen using teletype.
VIDEO_TELETYPE_MODE equ 0x0E

;; Prints a null-terminated string out using BIOS teletype.
;; Parameters:.
;;    ds:si - the address of the string.
print_16:
   pusha
   mov     ah, VIDEO_TELETYPE_MODE                ; Set video interrupt to teletype mode.
   mov     bx, 0x0007                             ; bh - page number. bl - color (light grey).
.print_characters:
   lodsb                                          ; al = [ds:si++]
   test    al, al
   jz      .end                                   ; exit on null terminator.
   int     VIDEO_INTERRUPT                        ; print character.
   jmp     .print_characters
.end:
   popa
   ret

;; Prints a string out using teletype.
;; Parameters:
;;    (word) string - the address of the string.
macro m_print_16 string {
   mov     si, string
   call    print_16
}



;; https://www.ctyme.com/intr/int-13.htm
DISK_INTERRUPT equ 0x13
;; Interrupt 0x13 mode to read sectors from disk.
DISK_READ_SECTORS_MODE equ 0x02
;; Interrupt 0x13 mode to reset a disk.
DISK_RESET_DISK_SYSTEM_MODE equ 0x00

;; The number of times to attempt reading sectros from a disk.
READ_SECTORS_ATTEMPTS equ 3

;; Attempts to read sectors from a disk into memory using the BIOS.
;; Parameters:
;;    dl - the drive to read from
;;    ch = cylinder[7..0] - the cylinder to read from.
;;    cl[7..6] = cylinder [9..8] - ^
;;    dh - the head to read from.
;;    cl[5..0] - the sector to start reading from.
;;    es:bx - where to write to.
;;    al - the number of sectors to read.
;; Returns:
;;    cf - whether the sectors were read. (0 = success, 1 = failure)
read_sectors_16:
   pusha
   mov     si, (READ_SECTORS_ATTEMPTS - 1)        ; set remaining attempts.
.try:
   mov     ah, DISK_READ_SECTORS_MODE
   int     DISK_INTERRUPT
   jnc     .end                                   ; exit if succeded.
   dec     si                                     ; decrement remaining attempts.
   jc      .end                                   ; exit if attempts were used up.
   mov     ah, DISK_RESET_DISK_SYSTEM_MODE        ; reset disk for retry.
   int     DISK_INTERRUPT
   jnc     .try                                   ; retry if successfully reset disk system.
.end:
   popa
   ret

;; The bits of the cx register that the cylinder number occupies for
;; read_sectors_16.
CYLINDER_NUMBER_BIT_MASK equ 0xFFC0
;; The bits of the cx register that the starting sector number occupies for
;; read_sectors_16.
STARTING_SECTOR_NUMBER_BIT_MASK equ 0x003F

;; Attempts to read sectors from a disk into memory using the BIOS.
;; Parameters:
;;    (byte) drive - the drive to read from.
;;    (word) cylinder[15..8,7..6] - the cylinder to read from. The upper 8 bits
;;                                  form the lower 8 bits of the cylinder
;;                                  number. The lower 2 bits form the upper 2
;;                                  bits of the cylinder number.
;;    (byte) head - the head to read from.
;;    (byte) starting_sector - the sector to start reading from.
;;    (byte) sector_count - the number of sectors to read.
;;    (word) location - where to write to.
;; Returns:
;;    cf - whether the sectors were read. (0 = success, 1 = failure)
macro m_read_sectors_16 drive, cylinder, head, starting_sector, sector_count, location {
   mov     dl, drive
   mov     cx, (cylinder and CYLINDER_NUMBER_BIT_MASK) or (starting_sector and STARTING_SECTOR_NUMBER_BIT_MASK)
   mov     dh, head
   mov     bx, location
   mov     al, sector_count
   call    read_sectors_16
}
