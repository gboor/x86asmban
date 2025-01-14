[map all myfile.map]

CPU 386		; Architecture we're using
BITS 16		; MBRs are 16-bit code
org 7C00h	; The location the BIOS loads the MBR

; Calculate the full size of the code from start of segment $$ to the end label at the bottom (end - $$)
; Divide that by 512 to get the number of sectors this MBR takes
; Division in NASM operands just truncates the output, so check if there is a rest value, if so add 1
; The > 0 operator returns 1 if true, 0 if false, which we add
; Subtract 1 to get the number of sectors we actually need to load - as the first sector is pre-loaded
%define SECTORS_TO_LOAD ((end - $$) / 512) + ((end - $$) % 512 > 0) - 1

main:
	; Perform this jump to make sure cs is set to 0x0000 and ip is set to 0x7c00(ish) - in line with our org.
	; BIOS behaviour on this is unpredictable
	jmp 0x0000:setcs

	; Some error messages
	disk_err db "Error reading disk", 0

setcs:
	; Base setup

	cld			; Set the direction flag forward, used for movs, stos, etc.
				; Setting this forward will increment cx on operations instead of decrementing it.

	xor ax, ax		; Set ES=DS=0 - this can be changed later for rep operations
	mov ds, ax
	mov es, ax

	mov ss, ax		; Stack segment is 0
	mov sp, 0x7c00		; This sets the stack (with segment 0, so ss:sp = 0000:7c00) to grow below the
				; bootloader. Nothing is relocated or loaded there, so this should be fine.


	; Store dl (or whole dx, as dl alone cannot be stored), as it contains the current disk index being read
	push dx

	; Set up the screen for displaying error messages and load the remaining sectors
	; These calls could be defined as callable functions, but since they are only used once, it doesn't
	; make that much sense to add that overhead and complexity.

	; Clear the screen
	mov ah, 0x07	; Function 0x07: scroll window
	mov al, 0x00	; Clear entire window
	mov bh, 0x07	; White on black
	mov cx, 0x00	; Specifies top left of screen as (0,0)
	mov dh, 0x18	; 18h = 24 rows of chars
	mov dl, 0x4f	; 4fh = 79 cols of chars
	int 0x10	; Video interrupt


	; Move cursor to position 0,0
	mov ah, 0x02	; Function 0x02: set cursor position
	mov dh, 0x00	; Row 0
	mov dl, 0x00	; Col 0
	mov bh, 0x00	; Page 0
	int 0x10	; Video interrupt


	; Load the calculated numbers of sector from the disk
	; For this function call, dl should be set to the disk number to read from. The BIOS sets dl to
	; the disk number of the MBR. It is pushed at the start, so restored here.

	pop dx

	mov ax, SECTORS_TO_LOAD	; Number of sectors to read
	mov bx, next_sector	; The memory address to read to
	xor ch, ch		; Cylinder 0
	mov cl, 0x02		; Start at sector 2 (sector 1 is the first part of the MBR)
	xor dh, dh		; Head 0
	mov ah, 0x02		; Read mode

	int 0x13		; Read interrupt
	jc .disk_error		; Check carry bit for error

	; After a succesful load, the layout in memory will be the same as the layout in our ASM files,
	; so we can jump to a defined label without worrying about offsets and exact addresses.
	; The first phase is finished. Jump to the game label, defined in game.asm.
	jmp game

.disk_error:
	push disk_err	; Print a disk error message
	call print
	cli		; Halt all interrupts and operations
	hlt


; The print function prints a string onto the screen in text mode
print:
	push bp		; Basic stack setup
	mov bp, sp
	pusha		; Push everything - this function doesn't
			; return anything

	mov si, [bp+4]	; Grab the pointer to the data
	mov bh, 0	; Page 0
	mov bl, 0	; Foreground color, irrelevant - in text mode
	mov ah, 0x0E	; Function 0x0E: print character in TTY
.char:
	mov al, [si]	; Get current char from pointer position
	inc si		; Keep incrementing si until a null char
	or al, 0
	je .return
	int 0x10	; Video interrupt
	jmp .char
.return:
	popa		; Restore registers
	mov sp, bp	; Restore stack
	pop bp
	ret 2		; Remove param from stack on return


; This is the end of the first sector of MBR code. MBRs MUST have a special signature at the end, or won't be booted.
; The signature MUST be in the last 2 bytes of the sector. So we pad the MBR binary with 0s and add the signature
; in the last 2 bytes.

; Padding bytes until the partition table.
; $$ = address of start of section, $ = current position, $-$$ = length of section thus far.
; We use 510, so the last 2 bytes can be specifically set (total sector size = 512).
times 510-($-$$) db 0

; MBR signature
db 55h, 0AAh

; This is the start of the next sector and this label is used by the disk read to load the next sectors here.
; We include game.asm, which contains the game code
next_sector:
	%include "game.asm"

; The end label signifies the last address in the entire MBR, including the game. We use it in the define above
; to figure out how large our game is and how many sectors to load.
end:

; Pad to the size of a 1.44MB floppy
times 1474560-($-$$) db 0
