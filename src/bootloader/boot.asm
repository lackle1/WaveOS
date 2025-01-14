org 0x7C00
bits 16

; NASM macros
%define ENDL 0x0d, 0x0a

;
; FAT12 header
;
jmp short start
nop

bpb_oem:						db 'MSWIN4.1'
bpb_bytes_per_sector:			dw 512
bpb_sectors_per_cluster:		db 1
bpb_reserved_sectors:			dw 1
bpd_fat_count:					db 2
bpd_dir_entries_count:			dw 0E0h					; conventional assembly format
bpd_total_sectors:				dw 2880					; 512 * 2880 = 1.44MB
bpd_media_ddescriptor_type:		db 0F0h					; denotes 3.5" floppy disk
bpd_sectors_per_fat:			dw 9
bpd_sectors_per_track:			dw 18
bpd_heads:					dw 2
bpd_hidden_sectors:				dd 0
bpd_large_sector_count:			dd 0

; extended boot record
ebr_drive_number:				db 0					; 0x00 floppy, 0x80 hdd, useless
								db 0					; reserved byte
ebr_signature:					db 29h
ebr_volume_id:					db 12h, 34h, 56h, 78h	; serial number, value doesn't matter
ebr_volume_label:				db 'LIGHTWAVE  '		; 11 bytes, padded with spaces
ebr_system_id:					db 'FAT12   '			; 8 bytes, padded with spaces

;
; Code goes here
;

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

main:
	; setup data segments. code segment (cs) is setup by BIOS. cannot directly write to segment registers (ds/es)
	mov ax, 0
	mov ds, ax
	mov es, ax
	
	; setup stack
	mov ss, ax
	mov sp, 0x7C00						; stack grows downward
	
	; read something from floppy disk
	; BIOS should set dl to drive number
	mov [ebr_drive_number], dl

	mov ax, 1							; LBA = 1, second sector on disk
	mov cl, 1							; 1 sector to read
	mov bx, 0x7E00						; data should be after the bootloader
	call disk_read

	mov si, msg_success
	call puts

	; print message
	mov si, msg_hello					; moves the address of "msg_hello"
	call puts
	
	cli
	hlt

.halt:
	cli									; disable BIOS interrupts, so CPU cannot get out of "halt"
	hlt

;
; Error handlers
;
floppy_error:
	mov si, msg_read_failed
	call puts
	hlt

wait_key_and_reboot:
	mov ah, 0
	int 16h								; wait for key-press
	jmp 0FFFFh:0						; jump to beginning of BIOS, should reboot

;
; Disk routines
;

;
; Converts an LBA address to a CHS address
; Parameters:
;	- ax: LBA address
; Returns:
;	- cx [bits 0-5]: sector
;	- cx [bits 6-15]: cylinder
;	- dh: head
lba_to_chs:
	
	push ax	; try pushing cx as well
	push dx

	xor dx, dx 							; dx = 0, less space than mov, and probably faster
	div word [bpd_sectors_per_track]	; ax = LBA / SectorsPerTrack
										; dx = LBA % SectorsPerTrack

	inc dx								; dx = (LBA % SectorsPerTrack) + 1 = sector
	mov cx, dx							; cx = sector

	xor dx, dx							; dx = 0
	div word [bpd_heads]				; ax = (LBA / SectorsPerTrack) / Heads = cylinder
										; dx = (LBA / SectorsPerTrack) % Heads = head

	mov dh, dl							; dh = head
	mov ch, al							; ch = cylinder
	shl ah, 6
	or cl, ah

	pop ax
	mov dl, al							; cannot push 8-bit registers
	pop ax
	ret

;
; Reads from disk
; Parameters:
;	- ax: LBA adddress
;	- cl: number of sectors to read (up to 128)
;	- dl: drive number
;	- es:bx: memory address for where read data will be stored
;
disk_read:

	push ax								; save registers we will modify
	push bx
	push cx
	push dx
	push di

	push cx								; number of sectors to read (modified in conversion)
	call lba_to_chs
	pop ax								; al = number of sectors to read

	mov ah, 02h							; read sectors from drive
	mov di, 3							; retry count

.retry:
	pusha								; don't know what BIOS will modify
	stc									; in case BIOS doesn't set carry flag
	int 13h								; disk services interrupt
	jnc .done							; carry flag cleared = success

	; read failed
	popa
	call disk_reset
	dec di								; decrement counter
	test di, di							; check if zero
	jnz .retry

.fail:
	; all attempts are exhausted
	jmp floppy_error

.done:
	popa

	pop di								; restore registers we modified
	pop dx
	pop cx
	pop bx
	pop ax

	ret

disk_reset:
	pusha
	mov ah, 0							; reset disk system
	stc
	int 13h
	jc floppy_error
	popa
	ret
	
msg_hello: db 'Hello World!', ENDL, 0
msg_success: db 'Woohoo!', ENDL, 0
msg_read_failed: db 'Could not readd floppy disk', ENDL, 0

times 510-($-$$) db 0
dw 0AA55h
