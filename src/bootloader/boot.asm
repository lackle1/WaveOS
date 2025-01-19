org 0x7C00												; without this, program would think labels like 'msg_success' are 0x07C00 bytes lower than it actually is
bits 16

; NASM macros
%define ENDL 0x0D, 0x0A

;
; FAT12 header
;
jmp short main
nop

; BIOS Parameter Block
bpb_oem:						db 'MSWIN4.1'
bpb_bytes_per_sector:			dw 512
bpb_sectors_per_cluster:		db 1
bpb_reserved_sectors:			dw 1
bpb_fat_count:					db 2
bpb_dir_entries_count:			dw 0E0h					; 224 (= 14 sectors, as entries are 32 bytes)
bpb_total_sectors:				dw 2880					; 512 * 2880 = 1.44MB
bpb_media_descriptor_type:		db 0F0h					; denotes 3.5" floppy disk
bpb_sectors_per_fat:			dw 9
bpb_sectors_per_track:			dw 18
bpb_heads:						dw 2
bpb_hidden_sectors:				dd 0
bpb_large_sector_count:			dd 0

; Extended Boot Record
ebr_drive_number:				db 0					; 0x00 floppy, 0x80 hdd, useless
								db 0					; reserved byte
ebr_signature:					db 29h
ebr_volume_id:					db 12h, 34h, 56h, 78h	; serial number, value doesn't matter
ebr_volume_label:				db 'LIGHTWAVE  '		; 11 bytes, padded with spaces
ebr_system_id:					db 'FAT12   '			; 8 bytes, padded with spaces

;
; Code goes here
;

main:
	; setup data segments. code segment (cs) is setup by BIOS. cannot directly write to segment registers (ds/es)
	mov ax, 0
	mov ds, ax
	mov es, ax
	
	; setup stack
	mov ss, ax
	mov sp, 0x7C00						; stack grows downward
	
	; some BIOSes might start us at 07C0:0000 instead of 0000:7C00. If that happens, put some magical code here:

	mov [ebr_drive_number], dl			; BIOS should set dl to drive number

	; print message
	mov si, msg_loading
	call puts

	;
	; read FAT root directory
	;

	; compute root directory LBA = reserved sectors + (fat_count * sectors_per_fat)
	; note this can be hardcoded
	mov ax, [bpb_sectors_per_fat]
	mov bl, [bpb_fat_count]
	xor bh, bh
	mul bx								; ax = (fat_count * sectors_per_fat)
	add ax, [bpb_reserved_sectors]		; ax = LBA of root directory
	push ax

	; compute size of root directory = (32 * number_of_entries) / bytes_per_sector
	mov ax, [bpb_dir_entries_count]		; reading whole directory, even empty entries
	shl ax, 5							; ax *= 5
	xor dx, dx
	div word [bpb_bytes_per_sector]		; ax = size of root directory

	test dx, dx							; test if division remainder != 0
	jz .root_dir_after
	inc ax								; if remainder != 0, we have a sector partially filled, so we must add 1

.root_dir_after:
	
	; read root directory
	mov cl, al							; cl = sectors to read (size of root directory)
	pop ax								; ax = LBA address
	mov dl, [ebr_drive_number]
	mov bx, buffer						; es:bx = buffer
	call disk_read

	; search for kernel.bin
	xor bx, bx							; counts how many entries have been checked
	mov di, buffer						; points to current entry

.search_kernel:
	mov si, file_kernel_bin
	mov cx, 11							; compare 11 chars
	push di								; save di
	repe cmpsb							; cmpsb: compare two bytes at ds:si and es:di. si and di are incremented. comparison works by subtraction and setting flags (zero flag)
										; repe: repeats while equal (zero flag is set), or until cx = 0. cx is decremented on each iteration.
	pop di
	je .found_kernel

	add di, 32							; move on to next directory entry
	inc bx								; +1 to number of entries searched
	cmp bx, [bpb_dir_entries_count]		; check if all entries have been searched
	jl .search_kernel					; jump if bx < dir_entries_count

	; kernel not found
	jmp kernel_not_found_error

.found_kernel:
	
	; di should point to the kernel.bin directory entry
	mov ax, [di + 26]					; first cluster field (offst 26)
	mov [kernel_cluster], ax			; cannot mov memory to memory

	; load FAT from disk into memory
	mov ax, [bpb_reserved_sectors]		; LBA address
	mov cl, [bpb_sectors_per_fat]		; number of sectors to read
	mov dl, [ebr_drive_number]
	mov bx, buffer						; address to write data
	call disk_read

	; read kernel and process FAT chain
	mov bx, KERNEL_LOAD_SEGMENT
	mov es, bx
	mov bx, KERNEL_LOAD_OFFSET

.load_kernel_loop:

	; read next cluster
	mov ax, [kernel_cluster]

	; harcoded value, not very nice :(
	add ax, 31							; first sector = (kernel_cluster - 2) * sectors_per_cluster(=1) + start_sector(=33) = kernel_cluster + 31
	mov cl, 1
	mov dl, [ebr_drive_number]
	call disk_read

	add bx, bpb_bytes_per_sector

	; compute location of next sector
	mov ax, [kernel_cluster]
	mov cx, 3							; 'mul' and 'div' do not allow immediate values as operands
	mul cx
	mov cx, 2
	div cx								; ax = index of FAT entry, dx = cluster % 2

	mov si, buffer
	add si, ax
	mov ax, [ds:si]						; ax = entry stored at FAT index

	test dx, dx
	jz .even

.even:
	and ax, 0xFFF						; zeroes 4 rightmost bits
	jmp .next_cluster_after

.odd:
	shr ax, 4

.next_cluster_after:
	cmp ax, 0xFF8
	jae .read_finish

	mov [kernel_cluster], ax
	jmp .load_kernel_loop

.read_finish:

	; jump to the kernel
	mov dl, [ebr_drive_number]

	mov ax, KERNEL_LOAD_SEGMENT
	mov ds, ax
	mov es, ax

	jmp KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

	jmp wait_key_and_reboot				; should never happen

.halt:
	cli									; disable BIOS interrupts, so CPU cannot get out of "halt"
	hlt

;
; Error handlers
;
read_error:
	mov si, msg_read_failed
	call puts
	hlt

kernel_not_found_error:
	mov si, msg_kernel_not_found
	call puts
	jmp wait_key_and_reboot

wait_key_and_reboot:
	mov ah, 0
	int 16h								; wait for key-press
	jmp 0FFFFh:0						; jump to beginning of BIOS, should reboot

;
; Prints string to the screen
; Parameters:
;	- ds:si points to string
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
	
	push ax
	push dx

	xor dx, dx 							; dx = 0, less space than mov, and probably faster. must be 0 because dividend = dx:ax
	div word [bpb_sectors_per_track]	; ax = LBA / SectorsPerTrack
										; dx = LBA % SectorsPerTrack

	inc dx								; dx = (LBA % SectorsPerTrack) + 1 = sector
	mov cx, dx							; cx = sector

	xor dx, dx							; dx = 0
	div word [bpb_heads]				; ax = (LBA / SectorsPerTrack) / Heads = cylinder
										; dx = (LBA / SectorsPerTrack) % Heads = head

	mov dh, dl							; dh = head
	mov ch, al							; ch = cylinder (lower 8 bits)
	shl ah, 6							; shift cylinder 6 bits left, so upper 2 bits are on very left of ah
	or cl, ah							; copy upper 2 bits of ah into cl, while keeping the 6-bit sector number stored there

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

	mov ah, 02h							; "read sectors from drive" function code
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
	jmp read_error

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
	mov ah, 0							; "reset disk system" function code
	stc
	int 13h
	jc read_error
	popa
	ret
	
msg_loading: db 'Loading. . .', ENDL, 0
msg_read_failed: db 'Failed to read!', ENDL, 0
msg_kernel_not_found: db 'kernel.bin file not found!', ENDL, 0

file_kernel_bin: db 'KERNEL  BIN'
kernel_cluster: dw 0

KERNEL_LOAD_SEGMENT equ 0x2000
KERNEL_LOAD_OFFSET equ 0

times 510-($-$$) db 0
dw 0AA55h

buffer: