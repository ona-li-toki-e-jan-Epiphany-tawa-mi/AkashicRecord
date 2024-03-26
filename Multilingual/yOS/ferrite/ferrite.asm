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
;; This is the boot sector for yOS, which is the first bit of code that the
;; computer will run when trying to boot yOS.
;;
;; It's, Ferrite's, job is to load the kernal, Ytterbium, into memory, elavate
;; the CPU from 16-bit mode to 32-bit mode, and then pass control to Ytterbium.
;;
;; Compilation:
;;    Compile with FASM.
;;
;;    In the resulting OS image of yOS, this boot sector's binary must be the
;;    first bytes, and directly thereafter, the binary for Ytterbium. Ytterbium
;;    must be padded up to 512-byte sectors so that they can be loaded by
;;    ferrite.
;;
;;    Ytterbium's size cannot exceed 480.5 KiB, as it is placed into memory
;;    directly after Ferrite. See the following memory map for more info.
;;
;;    Ferrite will jump directly to the start of the kernal's binary. Ensure
;;    there is something there for it to run!
;;
;;    Pass the boot-sector how many sectors Ytterbium takes up with the command
;;    line option "-d YTTERBIUM_SECTOR_COUNT=<# of sectors>".
;;
;; Resulting memory map after Ferrite is finished (more info at:
;; <https://wiki.osdev.org/Memory_Map_(x86)>):
;;    TODO update map with new bootloader size.
;;    /-------------------------|---------------------|----------------------------------\
;;    | Start      - End        | Size                | Description                      |
;;    |-------------------------|---------------------|----------------------------------|
;;    | 0x00000000 - 0x000003FF | 1 KiB               | Real mode interrupt vector table |
;;    | 0x00000400 - 0x000004FF | 256 B               | BIOS data area                   |
;;    | 0x00000500 - 0x000034FF | 12.29 KiB           | Page tables                      |
;;    | 0x00003500 - 0x00007BFF | 18.19 KiB           | Stack                            |
;;    | 0x00007C00 - 0x00007DFF | 512 B               | Ferrite bootloader               |
;;    | 0x00007E00 - 0x0007FFFF | 480.5 KiB           | Ytterbium kernal                 |
;;    | 0x00080000 - 0x0009FFFF | 128 KiB             | Extended BIOS data area          |
;;    | 0x000A0000 - 0x000BFFFF | 128 KiB             | Video display memory             |
;;    | 0x000C0000 - 0x000C7FFF | 32 KiB (typically)  | Video BIOS                       |
;;    | 0x000C8000 - 0x000EFFFF | 160 KiB (typically) | BIOS Expansions                  |
;;    | 0x000F0000 - 0x000FFFFF | 64 KiB              | Motherboard BIOS                 |
;;    | 0x00100000 - ...        | Variable            | Free space                       |
;;    \-------------------------|---------------------|----------------------------------/
;;
;; TODO make elavate to 64 bit long mode instead.
;;



;; Produces a free-standing binary.
format binary

;; The location we expect the BIOS to put the boot sector.
BOOT_SECTOR_LOCATION equ 0x7C00
org BOOT_SECTOR_LOCATION

BYTES_PER_SECTOR equ 512
postpone {
   ;; The number of sectors that need to be loaded to load the rest of Ferrite.
   ;; NOTE: All sectors must be padded to 512 bytes for this to work.
   FERRITE_EXTRA_SECTORS = ($ - $$) / 512 - 1
}

;; The location where the rest of ferrite will be loaded.
FERRITE_EXTRA_SECTORS_LOCATION equ BOOT_SECTOR_LOCATION + BYTES_PER_SECTOR
;; The location where the kernal will be loaded.
;; Set to directly after this boot-sector.
KERNAL_LOCATION equ FERRITE_EXTRA_SECTORS_LOCATION + FERRITE_EXTRA_SECTORS * BYTES_PER_SECTOR



NULL           equ 0
LINE_FEED      equ 0x0A
CARRIGE_RETURN equ 0x0D



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTOR 1 BEGIN. (BOOT SECTOR)                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;
;; ENTER 16-BIT MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;
use16                                             ; CPU starts in 16-bit mode.

;; Runs first as this is the first instruction in the boot-sector.
__start_16:
   jmp     0x0000:__main_16                       ; ensure cs == 0x0000



;; Runs an infinite loop of halting the processor for if something goes horribly
;; wrong.
halt_and_catch_fire:
   cli
   hlt
   jmp     halt_and_catch_fire



;; Marcros: m_print_16, m_read_sectors_16.
include '16-bit-utilities.inc'



;; How many bytes a page table occupies.
PAGE_TABLE_SIZE equ 4096
;; How many bytes a page table entry occupies.
PAGE_TABLE_ENTRY_SIZE equ 8
;; How many entries a page table holds.
ENTRIES_PER_PAGE_TABLE equ PAGE_TABLE_SIZE / PAGE_TABLE_ENTRY_SIZE
;; How many bytes a page occupies.
PAGE_SIZE = 4096
;; The locations of each page table, placed one after the other in memory. See
;; memory map at top of file for more info.
PAGE_MAP_LEVEL_4_TABLE       equ 0x00000500
PAGE_DIRECTORY_POINTER_TABLE equ PAGE_MAP_LEVEL_4_TABLE + PAGE_TABLE_SIZE
PAGE_DIRECTORY_TABLE         equ PAGE_DIRECTORY_POINTER_TABLE + PAGE_TABLE_SIZE
PAGE_TABLE                   equ PAGE_DIRECTORY_TABLE + PAGE_TABLE_SIZE
;; The bit (bit 1) to set in a page table entry to mark it as present, meaning that
;; the page is present in the physical memory.
PAGE_TABLE_ENTRY_PRESENT_FLAG equ 1 shl 0
;; The bit (bit 2) to set in a page table entry to mark it as writable.
PAGE_TABLE_ENTRY_READ_WRITE_FLAG equ 1 shl 1

;; The bit (bit 5) to set in control register 4 to enable physical address
;; extensions.
CR4_PHYSICAL_ADDRESS_EXTENSION_ENABLE equ 1 shl 4

;; The model-specific register that allows entering long-mode.
;; More info at <https://en.wikipedia.org/wiki/Model-specific_register>.
EXTENDED_FEATURE_ENABLE_REGISTER_MSR equ 0xC0000080
;; The bit (bit 9) to set in the extended feature enable register to enable
;; long mode.
EFER_LONG_MODE_ENABLE equ 1 shl 8

;; The bit (bit 32) to set in control register 0 to enable paging.
;; More info on cr0 at <https://en.wikipedia.org/wiki/Control_register#CR0>.
CR0_PAGING_MODE_ENABLE equ 1 shl 31
;; The bit (bit 1) to set in control register 0 to enable protected mode.
CR0_PROTECTED_MODE_ENABLE equ 1 shl 0

;; Runs on startup from BIOS.
__main_16:
   mov     BYTE [boot_drive], dl                  ; BIOS puts boot drive number in dl.
   ; Initializes segment registers.
   xor     ax, ax
   cli                                            ; disable interrupts to ensure segment registers and stack pointer are
                                                  ; set properly.
   mov     ds, ax
   mov     es, ax
   mov     ss, ax
   mov     sp, BOOT_SECTOR_LOCATION               ; initialize stack to free space below boot sector. See memory map at
                                                  ; top of file for more info.
   sti

   m_print_16 startup_message

   ; Reads in the rest of Ferrite.
   ; Reads from boot drive, cylinder 0, head 0, starting at sector 2.
   m_read_sectors_16 [boot_drive], 0x0000, 0, 2, FERRITE_EXTRA_SECTORS, FERRITE_EXTRA_SECTORS_LOCATION
   jnc     .read_ferrite                          ; checks for read error.
   m_print_16 disk_error_message
   jmp     halt_and_catch_fire
.read_ferrite:

   ; Reads in the Ytterbium kernal, if there is anything to load.
   if YTTERBIUM_SECTOR_COUNT > 0
      ; Reads from boot drive, cylinder 0, head 0, starting at the first sector after ferrite.
      m_read_sectors_16 [boot_drive], 0x0000, 0, 2 + FERRITE_EXTRA_SECTORS, YTTERBIUM_SECTOR_COUNT, KERNAL_LOCATION
      jnc     @f                                  ; checks for read error.
      m_print_16 disk_error_message
      jmp     halt_and_catch_fire
   @@:
   end if

   m_print_16 loaded_message
   m_print_16 elavating_to_64_bit_mode_message

   ; Checks to see if the processor supports long mode.
   call    supports_long_mode
   jnc     .long_mode_supported                   ; carry flag is set if not supported.
   m_print_16 long_mode_unsupported_message
   jmp     halt_and_catch_fire
.long_mode_supported:

   ; Tries to set the A20 address line, which we need to access the full memory
   ; addressing space.
   call    enable_A20_line_16
   ;jnc    .enabled_A20_line
   call    is_A20_line_enabled_16
   jnc     .enabled_A20_line
   m_print_16 A20_enable_error_message
   jmp     halt_and_catch_fire                    ; carry flag is set if failed.
.enabled_A20_line:

   ;m_print_16 loaded_message

   jmp     halt_and_catch_fire

   ;m_print_16 loaded_message
   ;jmp    halt_and_catch_fire


   ; Clears the memory for the page tables. Assumes that the page tables are in
   ; memory one after the other.
   ;mov     edi, PAGE_MAP_LEVEL_4_TABLE            ; tell rep stosd where the page tables are located.
   ;xor     eax, eax
   ;mov     ecx, PAGE_TABLE_SIZE                   ; tell rep stosd to write 4*4096 bytes, the memory occupied by the
                                                   ; page tables.
   ;rep     stosd                                  ; write null bytes to the page table memory.

   ; Sets up the page tables where (with all entries present and writable):
   ;    PML4[0] -> PDPT[0] -> PDT[0] -> PT
   ;mov     DWORD [PAGE_MAP_LEVEL_4_TABLE],       PAGE_DIRECTORY_POINTER_TABLE or PAGE_TABLE_ENTRY_PRESENT_FLAG or PAGE_TABLE_ENTRY_READ_WRITE_FLAG
   ;mov     DWORD [PAGE_DIRECTORY_POINTER_TABLE], PAGE_DIRECTORY_TABLE         or PAGE_TABLE_ENTRY_PRESENT_FLAG or PAGE_TABLE_ENTRY_READ_WRITE_FLAG
   ;mov     DWORD [PAGE_DIRECTORY_TABLE],         PAGE_TABLE                   or PAGE_TABLE_ENTRY_PRESENT_FLAG or PAGE_TABLE_ENTRY_READ_WRITE_FLAG
   ; Sets up the first final page table where (with all entries present and
   ; writable):
   ;    PT -> 0x00000000 - 0x00100000
   ; This identity maps the first 1 MB of memory for the 64-bit part of this
   ; bootloader and kernal to use. ebx is set to the start of memory with the
   ; relavent flags set.
   ;mov     ebx, PAGE_TABLE_ENTRY_PRESENT_FLAG or PAGE_TABLE_ENTRY_READ_WRITE_FLAG
   ;mov     ecx, ENTRIES_PER_PAGE_TABLE            ; set loop to run for each entry in the table.
   ;mov     edi, PAGE_TABLE                        ; start writing at the start of the table.
.fill_first_page_table_entry:
   ;mov     DWORD [edi], ebx                       ; write entry into table.
   ;add     ebx, PAGE_SIZE                         ; set next entry to cover next region of memory.
   ;add     edi, PAGE_TABLE_ENTRY_SIZE             ; move to address of next entry.
   ;loop    .fill_first_page_table_entry

   ; Tells the CPU where the first page table is located.
   ;mov     eax, PAGE_MAP_LEVEL_4_TABLE
   ;mov     cr3, eax
   ; Enables PAE, which is the method of paging that long mode uses.
   ;mov     eax, cr4
   ;or      eax, CR4_PHYSICAL_ADDRESS_EXTENSION_ENABLE
   ;mov     cr4, eax

   ; Enables long mode on the CPU for when we switch over.
   ;mov     ecx, EXTENDED_FEATURE_ENABLE_REGISTER_MSR ; select the EFER MSR.
   ;rdmsr                                             ; read it's value into eax.
   ;or      eax, EFER_LONG_MODE_ENABLE                ; set the flag to enable long mode.
   ;wrmsr                                             ; and write the new value back.

   ; Enables paging and protected mode, which are required for 64-bit long mode.
   ; This puts us in 32-bit compatibility long mode. More info at:
   ; <https://en.wikipedia.org/wiki/Long_mode>
   ;mov     eax, cr0
   ;or      eax, CR0_PAGING_MODE_ENABLE or CR0_PROTECTED_MODE_ENABLE
   ;mov     cr0, eax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENTER 32-BIT COMPATILITY LONG MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;use32

   ;lgdt    [gdt_descriptor]

   ; Doing a long jump will update the code segment (cs) register with the GDT
   ; offset for the code segment, as the orignal 16 value is now meaningless.
   ;jmp     gdt.code.index:__64_bit_mode_start_64



;data
   ;; The drive this boot sector was loaded from.
   boot_drive db 0

   startup_message db "Ferrite bootloader v0.1.0", LINE_FEED, CARRIGE_RETURN
                   db "Loading Ytterbium and rest of Ferrite...", LINE_FEED, CARRIGE_RETURN, NULL
   disk_error_message db "ERROR: Failed to read from disk!", LINE_FEED, CARRIGE_RETURN, NULL
   loaded_message db "Loading complete.", LINE_FEED, CARRIGE_RETURN, NULL
   elavating_to_64_bit_mode_message db "Elavating to 64-bit long mode...", LINE_FEED, CARRIGE_RETURN, NULL
   long_mode_unsupported_message db "ERROR: CPU does not support long mode!", LINE_FEED, CARRIGE_RETURN, NULL
   A20_enable_error_message db "ERROR: Unable to enable the A20 line!", LINE_FEED, CARRIGE_RETURN, NULL



   ;; Padding to 512 bytes, including the boot sector magic number.
   ;; NOTE: If this doesn't compile, the boot sector is using too much space.
   db 510 - ($ - $$) dup NULL
   dw 0xAA55



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTOR 1 END (BOOT SECTOR) / REMAINING SECTORS BEGIN.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Functions: supports_long_mode, enable_A20_line_16.
include 'long-mode-tools.inc'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENTER 64-BIT LONG MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
use64



VGA_MEMORY equ 0x000B8000
;; The number of bytes per line in VGA's default 80x25 text mode.
VGA_BYTES_PER_LINE = 80 * 2
;; Character attributes for a white, non-blinking character on a black background.
VGA_WHITE_ON_BLACK equ 0x0f

;; Prints a null-terminated string as VGA text to the specified memory location.
;; TODO see if this can be simplified with movsb.
;; NOTE: This will simply write the string without any concern of where or why;
;; use with caution! It also cannot handle newlines.
;; Parameters:
;;    rbx - the address of the string.
;;    rdx - the address to start writing to.
;write_vga_string_bytes_64:
;.print_characters:
;   mov     al, [rbx]                              ; set character to write.
;   test    al, al
;   jz      .end                                   ; exit on null-terminator.
;   mov     ah, VGA_WHITE_ON_BLACK                 ; set character attributes.
;   mov     WORD [rdx], ax                         ; write character to VGA memory.
;   inc     rbx                                    ; move to next character in string.
;   add     rdx, 2                                 ; move cursor to next character cell inside VGA memory.
;   jmp     .print_characters
;.end:
;   ret

;; Prints a null-terminated string as VGA text to the specified memory location.
;; NOTE: This will simply write the string without any concern of where or why;
;; use with caution! It also cannot handle newlines.
;; Parameters:
;;    (qword) string - the address of the string.
;;    (qword) location - the address to start writing to.
;macro m_write_vga_string_bytes_64 string, location {
;   mov     rbx, string
;   mov     rdx, location
;   call    write_vga_string_bytes_64
;}



;; Runs once the CPU is elavated to 64-bit long mode.
__64_bit_mode_start_64:
   cli                                            ; interrupts need to be disabled as we have not set up a 64-bit IDT.
   ; Updates the segment registers to their index in the GDT as their original
   ; 16-bit values are now meaningless.
   mov     ax, gdt.data.index
   mov     ds, ax
   mov     es, ax
   mov     fs, ax
   mov     gs, ax
   mov     ss, ax

   ;; TODO 64 bit mode worky please fix.

   ;write_vga_string_bytes_64 successful_elavation_message, VGA_MEMORY
   jmp     halt_and_catch_fire
   ;; TODO jump to kernal.



;data
   successful_elavation_message db "Successfully elavated to 64-bit mode.", NULL
   starting_kernal_message db "Starting Ytterbium... brace for impact!", NULL



   ;; The bit (bit 8) to set in the access byte to mark the segment as present
   ;; in physical memory.
   GDT_PRESENT_ACCESS_FLAG = 1 shl 7
   ;; The bit (bit 5) to set in the access byte to mark the segment as a code or
   ;; data segment.
   GDT_CODE_OR_DATA_SEGMENT_ACCESS_FLAG = 1 shl 4
   ;; The bit (bit 4) to set in the access byte to mark the segment as a code
   ;; segment.
   GDT_CODE_ACCESS_FLAG = 1 shl 3
   ;; The bit (bit 2) to set in the access byte to mark a code segment as
   ;; readable.
   GDT_READABLE_ACCESS_FLAG = 1 shl 1
   ;; The bit (bit 2) to set in the access byte to mark a data segment as
   ;; writable.
   GDT_WRITABLE_ACCESS_FLAG = 1 shl 1

   ;; The bit (bit 4) to set in the flags to multiply the segment's limit by
   ;; 16^3.
   GDT_GRANULARITY_FLAG = 1 shl 3
   ;; The bit (bit 3) to set in the flags to make a data segment's default data
   ;; unit to 32 bits.
   GDT_32_BIT_DEFAULT_FLAG = 1 shl 2
   ;; The bit (bit 2) to set in the flags to mark a code segment as 64-bit code.
   GDT_64_BIT_MODE_FLAG = 1 shl 1

   ;; A memory segment descriptor in the GDT. More info at:
   ;; <https://wiki.osdev.org/Global_Descriptor_Table>.
   ;; Parameters:
   ;;    (32 bits) base - the starting address of the segment.
   ;;    (19 bits) limit - the size of the segment.
   ;;    (byte) access - flags for the access byte.
   ;;    (4 bits) flags - misc. flags.
   macro segment_descriptor base, limit, access, flags {
      ; limit[15..0].
      dw limit and 0x0000FFFF
      ; base[15..0].
      dw base and 0x0000FFFF
      ; base[23..16].
      db (base and 0x00FF0000) shr 16
      ; access byte.
      db access
      ; flags, limit[19..16].
      db ((flags and 1111b) shl 4) or ((limit shr 16) and 1111b)
      ; base[32..24].
      db (base and 0xFF000000) shr 24
   }

   ;; The global descriptor table for describing the 64-bit mode memory layout.
   ;; This GDT defines a simple, flat memory layout. Paging will be used to
   ;; control memory protections.
   gdt:
      ;; Mandatory null descriptor for error checking.
      gdt.null_descriptor:
         segment_descriptor 0, 0, 0, 0
      ;; Descriptor for code memory segment.
      .code:
         ; base =  0x00000000, limit = 0xFFFFF000.
         ; Access byte flags:
         ; - present - segment is present in memory.
         ; - privilege (unset) - ring 0.
         ; - descriptor type - code or data segment.
         ; - code - code segment.
         ; - conforming (unset) - requires sufficient priviliges to access
         ;   segment.
         ; - readable - allows reading constants from segment.
         ; - accessed (unset) - set when the CPU accesses the segment.
         ; Flags:
         ; - granularity - limit *= 16 * 16 * 16.
         ; - 32-bit default (unset) - N/A for 64-bit code segment.
         ; - 64-bit segment - segment works in 64-bit mode.
         ; - AVL (unset) - general purpose.
         .access_byte = GDT_PRESENT_ACCESS_FLAG or GDT_CODE_OR_DATA_SEGMENT_ACCESS_FLAG or GDT_CODE_ACCESS_FLAG or GDT_READABLE_ACCESS_FLAG
         .flags       = GDT_GRANULARITY_FLAG or GDT_64_BIT_MODE_FLAG
         segment_descriptor 0x00000000, 0x000FFFFF, .access_byte, .flags
      ;; Offset of the code descriptor from the start of the GDT.
      ;; Needed when indexing for the code segment.
      gdt.code.index = .code - gdt
      ;; Desciptor for data memory segment.
      .data:
         ; base =  0x00000000, limit = 0xFFFFF000.
         ; Access byte flags:
         ; - present - segment is present in memory.
         ; - privilege (unset) - ring 0.
         ; - descriptor type - code or data segment.
         ; - code (unset) - data segment.
         ; - expand down (unset) - segment grows up in memory
         ; - writable - allows writing to segment.
         ; - accessed (unset) - set when the CPU accesses the segment.
         ; Flags:
         ; - granularity - limit *= 16 * 16 * 16.
         ; - 32-bit default - sets default data unit to 32 bits.
         ; - 64-bit segment (unset) - N/A for data segment.
         ; - AVL (unset) - general purpose.
         .access_byte = GDT_PRESENT_ACCESS_FLAG or GDT_CODE_OR_DATA_SEGMENT_ACCESS_FLAG or GDT_WRITABLE_ACCESS_FLAG
         .flags       = GDT_GRANULARITY_FLAG or GDT_32_BIT_DEFAULT_FLAG
         segment_descriptor 0x00000000, 0x000FFFFF, .access_byte, .flags
         ;; Offset of the data descriptor from the start of the GDT.
         ;; Needed when indexing for the data segment.
      gdt.data.index = .data - gdt
      gdt.size = $ - gdt

   ;; Desciptor to describe the GDT to the CPU.
   gdt_descriptor:
      dw gdt.size - 1 ; Size of the GDT. Expects number one less than the true
                      ; true size of the GDT.
      dq gdt          ; The address of the GDT.



   ;; Pads final sector to 512 bytes for calculating the number of sectors to
   ;; load.
   db (512 - ($ - $$) mod 512) dup NULL
