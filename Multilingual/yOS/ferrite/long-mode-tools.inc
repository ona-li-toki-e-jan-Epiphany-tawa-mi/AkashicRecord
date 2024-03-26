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
;; A set of utility functions to help with elavating a x86 CPU into long mode.
;; Written in FASM.
;;



;; The bit (bit 22) in the CPU EFLAGS register that indicates whether the CPUID
;; instruction is supported by the CPU.
EFLAGS_REGISTER_ID_BIT equ 1 shl 21
;; A CPUID function that gets the highest supported extended function. More info
;; at: <https://en.wikipedia.org/wiki/CPUID#EAX=80000000h:_Get_Highest_Extended_
;; Function_Implemented>.
CPUID_GET_HIGHEST_EXTENDED_FUNCTION_FUNCTION equ 0x80000000
;; A CPUID function that returns extended CPU feature flags. More info at:
;; <https://en.wikipedia.org/wiki/CPUID#EAX=80000001h:_Extended_Processor_Info_a
;; nd_Feature_Bits>.
CPUID_EXTENDED_PROCESSOR_INFO_AND_FEATURE_BITS_FUNCTION equ 0x80000001
;; The bit (bit 30) that is set in edx if long mode is supported when calling
;; CPUID with the extended processor info and feature bits function.
CPUID_EPIAFBF_LONG_MODE_SUPPORTED_FLAG equ 1 shl 29

;; Checks whether the CPU supports long mode.
;; Returns:
;;    cf - 0 = supported, 1 = unsupported.
supports_long_mode:
   ; Checks to to see if the CPU supports CPUID, which we can use for checking
   ; for long mode support.
   call    __supports_CPUID
   jc      .end                                        ; if not, return unsupported.
   ; Checks to see if we can check for long mode using CPUID (part of the
   ; extended functions of CPUID.
   call    __supports_CPUID_extended_functions
   jc      .end                                        ; if not, return unsupported.
   ; Now that we can check if long mode is supported, we check if long mode is
   ; supported.
   mov     eax, CPUID_EXTENDED_PROCESSOR_INFO_AND_FEATURE_BITS_FUNCTION
   cpuid
   test    edx, CPUID_EPIAFBF_LONG_MODE_SUPPORTED_FLAG ; see if the long mode bit was set.
   jnz .end                                            ; if not,
   stc                                                 ; return unsupported.
.end:
   ret

;; Checks whether the CPU supports the CPUID instruction.
;; Returns:
;;    cf - 0 = supported, 1 = unsupported.
__supports_CPUID:
   clc                                            ; clear carry flag as we will use it as a return value.
   ; Loads CPU flags into eax.
   pushfd
   pop     eax
   ; Flips the ID bit. If the CPU dosen't unflip this bit then CPUID is not
   ; supported.
   mov     ecx, eax                               ; save flags for later comparison.
   xor     eax, EFLAGS_REGISTER_ID_BIT
   ; Writes the modfied flags to the CPU and then reads them back.
   push    eax
   popfd
   pushfd
   pop     eax
   ; Restores original CPU flags.
   push    ecx
   popfd
   ; Checks if the CPU unflipped the ID bit.
   cmp     eax, ecx
   jne     .end
   stc                                            ; if not, tell caller CPUID isn't supported.
.end:
   ret

;; Checks whether the CPU supports the extended functions of the CPUID
;; instruction.
;; Returns:
;;    cf - 0 = supported, 1 = unsupported.
__supports_CPUID_extended_functions:
   clc                                            ; clear carry flag as we will use it as a return value.
   ; Test CPUID for whether it supports extended functions.
   mov     eax, CPUID_GET_HIGHEST_EXTENDED_FUNCTION_FUNCTION
   cpuid
   ; If the return value of CPUID is less then the value of the first extended
   ; function, then extended functions are not supported.
   cmp     eax, CPUID_GET_HIGHEST_EXTENDED_FUNCTION_FUNCTION
   jae     .end
   stc                                            ; return unsupported.
.end:
   ret



;; Tests if the A20 address line is enabled. More info at:
;; <https://wiki.osdev.org/A20_Line>.
;; Returns:
;;    cf - 0 = enabled, 1 = disabled.
is_A20_line_enabled_16:
   cli                                            ; disable interrupts to prevent weirdness with changing segment
                                                  ; registers.
   clc                                            ; clear carry flag as we will use it as a return value.
   ; Saves state of segment registers that we will use to the stack.
   push    es
   push    ds
   ; Sets es:di to 0x0000:7DFE, the location of the boot sector magic number.
   ; There is nothing special about this address, we just need one to test with.
   xor     ax, ax
   mov     es, ax
   mov     di, 0x7DFE
   ; Sets ds:si to 0xFFFF:7E0E, 1 MiB higher than 0x0000:7DFE. If the A20 line
   ; is disabled, this will wrap around to the location specified by
   ; 0x0000:7DFE. If enabled, this will actually point to the specified address.
   not     ax
   mov     ds, ax
   mov     si, 0x7E0E
   ; Saves the value at the addresses we will use to test for the A20 line's
   ; state.
   mov     al, [es:di]
   push    ax
   mov     al, [ds:si]
   push    ax
   ; Writes some random, but different, values to both locations.
   mov     BYTE [es:di], 0x00
   mov     BYTE [ds:si], 0xFF
   cmp     BYTE [es:di], 0xFF                     ; and test if the address at es:si wrapped around and overwrote the
                                                  ; value at es:di.
   ; Restores the original values at the address used for testing.
   pop     ax
   mov     BYTE [ds:si], al
   pop     ax
   mov     BYTE [es:di], al
   ; If the value was ovewritten, set the carry flag as a return value.
   jne     .end
   stc
.end:
   ; Restores segment registers' states and interrupt flag.
   pop     ds
   pop     es
   sti
   ret



;; Some of the methods to set A20 work slowly and need to be read with a
;; timeout. This is handled by retrying the A20 test several times, the
;; exact number specified here.
A20_ENABLE_STATE_READ_ATTEMPTS equ 10000

;; https://www.ctyme.com/intr/int-15.htm
SYSTEM_INTERRUPT equ 0x15
;; Interrupt 0x15 mode to enable the A20 gate.
SYSTEM_ENABLE_A20_GATE equ 0x2401
;; Interrupt 0x15 mode to see if the BIOS supports enabling the A20 line.
SYSTEM_QUERY_A20_GATE_SUPPORT equ 0x2403

;; The data port of the keyboard controller. More info at:
;; <https://wiki.osdev.org/%228042%22_PS/2_Controller>
KEYBOARD_CONTROLLER_DATA_PORT = 0x60
;; The command and status register of the keyboard controller.
KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT equ 0x64
;; The bit (bit 1) that must be set in the status register of the keyboard
;; controller before trying to read from the data port.
KCSCRP_OUTPUT_BUFFER_STATUS_FLAG equ 1 shl 0
;; The bit (bit 2) that must be clear in the status register of the keyboard
;; controller before trying to write to the keyboard controller ports.
KCSCRP_INPUT_BUFFER_STATUS_FLAG equ 1 shl 1
;; The keyboard controller command to disable the first PS/2 port (the keyboard.)
KCSCRP_DISABLE_FIRST_PS2_PORT equ 0xAD
;; The keyboard controller command to enable the first PS/2 port (the keyboard.)
KCSCRP_ENABLE_FIRST_PS2_PORT equ 0xAE
;; The keyboard controller command to read from the output port.
KCSCRP_READ_CONTROLLER_OUTPUT_PORT equ 0xD0
;; The keyboard controller command to write to the output port.
KCSCRP_WRITE_CONTROLLER_OUTPUT_PORT equ 0xD1
;; The bit (bit 2) in the keyboard controller's output port that enables the A20
;; line.
KCSCRP_OUTPUT_PORT_A20_ENABLE equ 1 shl 1

;; A port that can be used in some processors to enable the A20 line. More info
;; at: <https://wiki.osdev.org/A20_Line>
FAST_A20_GATE_PORT equ 0x0092
;; The bit (bit 2) to set to enable the A20 line via the fast A20 port.
FAGP_A20_LINE_ENABLE equ 1 shl 1
;; A bit mask to apply to the value for the fast A20 port to prevent signaling a
;; fast reset (makes sure bit 0 is set to 0.)
FAGP_NO_FAST_RESET_BIT_MASK equ 0xFE

;; Attempts to enable the A20 line via a couple methods. As you can tell, this
;; is a massive pain in the arse <https://www.win.tue.nl/~aeb/linux/kbd/A20.html>.
;; Returns:
;;    cf - 0 = success, 1 = failure.
enable_A20_line_16:
   ; Returns if A20 is already enabled.
   call    is_A20_line_enabled_16
   jnc     .end
.bios_method:
   ; Tries to enable A20 with the BIOS.
   mov     ax, SYSTEM_QUERY_A20_GATE_SUPPORT
   int     SYSTEM_INTERRUPT                       ; query BIOS for A20 support.
   jc      .keyboard_method                       ; if not supported, try keyboard.
   cmp     ah, 0
   jne     .keyboard_method                       ; if not supported, try keyboard.
   mov     ax, SYSTEM_ENABLE_A20_GATE
   int     SYSTEM_INTERRUPT                       ; attempt to enable A20 with BIOS.
   jc      .keyboard_method                       ; if failed, try keyboard.
   cmp     ah, 0
   jne     .keyboard_method                       ; if failed, try keyboard.
   call    is_A20_line_enabled_16
   jnc     .end                                   ; if BIOS successfully enabled A20, exit.
.keyboard_method:
   ; Tries to enable A20 by sending a signal to the keyboard controller
   cli                                            ; disable interrupts to prevent any weirdness when working with the
                                                  ; keyboard controller.
   ;; Disables the keyboard.
   call    __await_keyboard_controller_writeable
   mov     al, KCSCRP_DISABLE_FIRST_PS2_PORT
   out     KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT, al
   ;; Tells keyboard controller we would like to to read from it's output port.
   call    __await_keyboard_controller_writeable
   mov     al, KCSCRP_READ_CONTROLLER_OUTPUT_PORT
   out     KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT, al
   ;; Reads the resulting output port status.
   call    __await_keyboard_controller_readable
   in      al, KEYBOARD_CONTROLLER_DATA_PORT
   push    eax                                    ; and save it on the stack.
   ;; Tells the keyboard controll we would like to write to the output port.
   call    __await_keyboard_controller_writeable
   mov     al, KCSCRP_WRITE_CONTROLLER_OUTPUT_PORT
   out     KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT, al
   ;; Writes out the signal to enable the A20 line.
   call    __await_keyboard_controller_writeable
   pop     eax                                    ; get saved output port state.
   or      al, KCSCRP_OUTPUT_PORT_A20_ENABLE      ; set flag to enable A20.
   out     KEYBOARD_CONTROLLER_DATA_PORT, al
   ;; Re-enables the keyboard.
   call    __await_keyboard_controller_writeable
   mov     al, KCSCRP_ENABLE_FIRST_PS2_PORT
   out     KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT, al
   ;; And checks if it worked.
   call    __await_keyboard_controller_writeable  ; wait until controller I/O is done.
   sti
   call    __test_line_A20_with_timeout
   jnc     .end                                   ; if successfully enabled A20 via the controller, exit.
   ;mov     si, loaded_message
   ;call    _print_16
.fast_A20_method:
   ; Tries to enable A20 using the fast A20 port.
   in      al, FAST_A20_GATE_PORT                 ; read in current state of the fast A20  port.
   test    al, FAGP_A20_LINE_ENABLE
   jnz     .fast_A20_no_read                      ; if A20 line is already set, we do NOT want to write to the port.
   or      al, FAGP_A20_LINE_ENABLE               ; set the flag to enable the A20 line.
   and     al, FAGP_NO_FAST_RESET_BIT_MASK        ; prevents a fast reset.
   out     FAST_A20_GATE_PORT, al                 ; and write the new value back.
.fast_A20_no_read:
   call    __test_line_A20_with_timeout
   jnc     .end                                   ; if successfully enabled A20 via the port, exit.
   ;mov     si, loaded_message
   ;call    _print_16
.port_0xEE_method:
   ; Apparently, on some computers, reading from port 0xEE can enable A20.
   in      al, 0xEE
   call    __test_line_A20_with_timeout
   jnc     .end                                   ; if successfully enabled A20 via the port, exit.
   ;mov     si, loaded_message
   ;call    _print_16
.otherwise:
   ; Assuming we are unable to enable the A20 line via the previous methods,
   ; the carry flag will be set from the previous method invoking
   ; _is_A20_line_enabled_16, which we can just return.
.end:
   ret

;; Some of the methods to set A20 work slowly and need to be read with a
;; timeout. This is handled by retrying the A20 test several times.
;; Returns:
;;    cf - 0 = success, 1 = failure.
__test_line_A20_with_timeout:
   mov     bx, A20_ENABLE_STATE_READ_ATTEMPTS - 1 ; _is_A20_line_enabled_16 doesn't use bx.
.try:
   call    is_A20_line_enabled_16
   jnc     .end                                   ; if A20 was successfully enabled, exit.
   dec     bx
   jnc     .try                                   ; retry if we have more attempts.
.end:
   ret

;; Blocks execution until the keyboard controller is ready to recieve data on
;; the data and command ports.
__await_keyboard_controller_writeable:
   in      al, KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT ; read controller status.
   test    al, KCSCRP_INPUT_BUFFER_STATUS_FLAG                  ; check if writable.
   jnz     __await_keyboard_controller_writeable                ; if not, check again.
   ret

;; Blocks execution until the keyboard controller is ready to send data on the
;; data port.
__await_keyboard_controller_readable:
   in      al, KEYBOARD_CONTROLLER_STATUS_COMMAND_REGISTER_PORT ; read controller status.
   test    al, KCSCRP_OUTPUT_BUFFER_STATUS_FLAG                 ; check if readable.
   jz      __await_keyboard_controller_readable                 ; if not, check again.
   ret
