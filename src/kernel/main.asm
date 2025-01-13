org 0x7C00
bits 16

; cmd run command: qemu-system-i386 -fda build/main_floppy.img

; NASM macros
%define ENDL 0x0d, 0x0a

start:
	jmp main
	
;
; Prints a string to the screen
; Params:
; - ds:si points to string
;
puts:
	; save registers we will modify
	push si
	push ax	
	
.loop:
	lodsb			; loads byte from ds:si into al and increments si
	or al, al		; Zero Flag will be set if null (0)
	jz .done
	
	mov ah, 0x0e	; set interrupt instruction(?)
	mov bh, 0		; page number
	int 0x10		; call BIOS interrupt
	
	
	jmp .loop

.done:
	pop ax
	pop si
	ret

main:
	; setup data segments. code segment (cs) is setup by BIOS. cannot directly write to segment registers (ds/es)
	mov ax, 0
	mov ds, ax
	mov es, ax
	
	; setup stack
	mov ss, ax
	mov sp, 0x7c00	; stack grows downward
	
	; print message
	mov si, msg_hello
	call puts
	
	hlt

.halt:
	jmp .halt
	
msg_hello: db 'Hello World!', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
