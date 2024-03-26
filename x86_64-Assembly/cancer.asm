section .data
	ask db "What is your name little one? (20 letters max plz)",0xA
	ask_length equ $-ask
	
	enterrr db "",0xA
	enterrr_length equ $-enterrr
	
	helloMsg db "Hello, "
	helloMsg_length equ $-helloMsg
	
section .bss
	name resb 20
	
section .text
	global main
	
main:
	mov rsi, ask
	mov rdx, ask_length
	call _printMsg
	
	call _readName
	
	mov rax, 1
	
	_rerun:
		push rax
		
		mov rsi, helloMsg
		mov rdx, helloMsg_length
		call _printMsg
		
		mov rsi, name
		mov rdx, 20
		call _printMsg
		
		mov rsi, enterrr
		mov rdx, enterrr_length
		call _printMsg
		
		; Grabs increment value and tests it for exiting.
		pop rax
		cmp rax, 20
		je _continue
		
		; Increments counter and reruns loop.
		add rax, 1
		jmp _rerun
	_continue:
	
	; Exit stuff.
	mov rax, 60
	mov rdi, 0
	syscall
	
; Prints a message to standard out. rsi and rdx must be set with the address and message length beforehand.
_printMsg:
	mov rax, 1
	mov rdi, 1
	syscall
	
	ret
	
; Reads a name, pretty epic.
_readName:
	mov rax, 0
	mov rdi, 0
	mov rsi, name
	mov rdx, 20
	syscall
	
	ret
		