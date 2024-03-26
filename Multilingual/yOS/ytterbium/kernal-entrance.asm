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
;; When the Ferrite bootloader is finished loading Ytterbium and makes the jump
;; into kernal memory, there is no guarantee that it's destination is the main
;; function in our Rust code. The Rust compiler may put it in any order.
;;
;; To solve this, this small assembly file is placed at the very begining of the
;; kernal, and thus acts as a guaranteed entry point, and jumps to where ever
;; main function might be located.
;;
;; Compilation:
;;    Compile with FASM to an object file and link with the kernal.
;;

format ELF64
extrn _kernal_start_64                            ; kernal entry label.

section '.text' executable                        ; section required for the jump to appear at the start of the kernal
                                                  ; binary.
   jmp     _kernal_start_64
