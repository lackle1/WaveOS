org 0x0
bits 16

%define ENDL 0x0D, 0x0A

main:
    mov si, msg_hi
    call puts

    cli
    hlt

puts:
	; save registers we will modify
	push si
	push ax	
	
.loop:
	lodsb								; loads byte from ds:si into al and increments si
	test al, al							; Zero Flag will be set if null (0)
	jz .done
	
	mov ah, 0Eh							; set interrupt instruction(?)
	mov bh, 0							; page number
	int 10h								; call BIOS interrupt
	
	
	jmp .loop

.done:
	pop ax
	pop si
	ret

msg_hi: db 'Neo... Wake up Neo.', ENDL, 0