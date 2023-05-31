; The structure of the levels binary is as follows:
;
; num_levels word
; offset_level_1 word
; offset_level_2 word
; ...
; offset_level_n word
;
; Per level:
;
; width (in sprites)	byte
; height (in sprites)	byte
; player_x		byte
; player_y		byte
; num_boxes		byte
; level_data		byte * width * height
; box_coordinates (x,y)	byte * num_boxes * 2


; Define a data struct for the level header
struc LEVEL_HEADER
	.width     resb 1	; Width of the level in sprites
	.height    resb 1	; Height of the level in sprites
	.player_x  resb 1	; X coordinate of the player start position
	.player_y  resb 1	; Y coordinate of  the player start postion
	.num_boxes resb 1	; Number of boxes
	.data      resb 1	; Start of the data block, this is longer,
				; but we only need the offset
				; The box coordinates are after this part,
				; but we cannot index them here, as this is
				; variable length
endstruc

; Define a structure for the loaded level
; This one is very similar to the LEVEL_HEADER, except that some values are
; words, not bytes, to make handling them easier with x86 asm being 16 bit
; (you cannot push an 8-bit value, for instance).
; The coordinates of the player and boxes are still bytes. Even if we need
; to convert them later, this makes them easier to compare word to word.
; This struct also represents the LIVE state of the level, meaning that
; values for player_x, player_y and box_coords will change as it is played.
; The LEVEL_HEADER structure represents the base state of the level, to
; initially load it.
struc LEVEL
	.idx        resw 1			; The current level index
	.width      resw 1			; Loaded level width
	.height     resw 1			; Loaded level height
	.offset_x   resw 1			; Pixel offset for centering x
	.offset_y   resw 1			; Pixel offset for centering y
	.player_x   resb 1			; Player x coordinate
	.player_y   resb 1			; Player y coordinate
	.num_boxes  resw 1			; Number of boxes
	.data_ptr   resw 1			; Pointer to the level data
	.box_coords resb MAX_BOX_COUNT * 2	; An array of box coordinates
endstruc

; Set up a reserved block of memory to hold the loaded level
current_level:
istruc LEVEL
	at LEVEL.idx,        dw 0
	at LEVEL.width,      dw 0
	at LEVEL.height,     dw 0
	at LEVEL.offset_x,   dw 0
	at LEVEL.offset_y,   dw 0
	at LEVEL.player_x,   db 0
	at LEVEL.player_y,   db 0
	at LEVEL.num_boxes,  dw 0
	at LEVEL.data_ptr,   dw 0
	at LEVEL.box_coords, db 0
iend

; Macro for easy access to the current level fields
%define CUR_LEVEL(field) current_level + LEVEL. %+ field

; This function gets the pointer to a level by its index and sets its
; various properties in current_level
load_level:
	push bp			; Basic stack setup
	mov bp, sp
	pusha			; Push everything - this function doesn't
				; return anything

	mov bx, [bp+4]		; [bp+4] is the first param (the level index)
				; We use bx here, because only bx and bp can
				; be used as an index register
	cmp bx, [LEVELS]	; The first WORD at LEVELS is the number of
				; levels
	jb .load		; If param < num_levels, jump to load
				; This is an unsigned compare, so any
				; values that are signed will register as
				; much larger, meaning it will only go
				; to load if bx >= 0 && bx < num_levels
	xor bx, bx		; If the index is not correct, set bx to 0,
				; defaulting to the first level
.load:
	; Store the (corrected) level index
	mov [CUR_LEVEL(idx)], bx

	; Calculate the data offset
	inc bx			; bx = bx + 1, to calculate the offset beyond num_levels
	shl bx, 1		; Fast version of bx = bx * 2; words are 2 bytes
	add bx, LEVELS		; bx now points to the offset of the chosen level
	mov bx, [bx]		; Put the actual offset itself in bx
	add bx, LEVELS		; Add the level offset again to get the absolute
				; offset of the of the level in memory

	; Read the actual level into the current_level
	xor ax, ax			; Clear all of ax
	mov al, [bx]			; Read the width byte
	mov [CUR_LEVEL(width)], ax	; Store as a word

	; Calculate the offset_x to center the level
	; (SCREEN_WIDTH - LEVEL_WIDTH in pixels) / 2
	imul ax, SPRITE_SIZE		; Get level width in pixels
	mov cx, SCREEN_WIDTH
	sub cx, ax			; SCREEN_WIDTH - LEVEL_WIDTH
	shr cx, 1			; Quick division by 2
	mov [CUR_LEVEL(offset_x)], cx

	; Same for height
	xor ax, ax
	mov al, [bx+1]
	mov [CUR_LEVEL(height)], ax

	; And for offset_y
	imul ax, SPRITE_SIZE
	mov cx, SCREEN_HEIGHT
	sub cx, ax
	shr cx, 1
	mov [CUR_LEVEL(offset_y)], cx

	; And for num_boxes
	xor ax, ax
	mov al, [bx+4]
	mov [CUR_LEVEL(num_boxes)], ax

	; Player coords are bytes, so move them as-is using a word
	mov ax, [bx+2]
	mov [CUR_LEVEL(player_x)], ax

	; Set bx to the level data pointer and store it
	add bx, LEVEL_HEADER.data
	mov [CUR_LEVEL(data_ptr)], bx

	; Move bx beyond the data section (width * height bytes)
	mov ax, [CUR_LEVEL(width)]
	imul ax, [CUR_LEVEL(height)]
	add bx, ax

	; bx is now at the box coordinate section, which contains a byte for x and y
	; for each box
	mov si, bx			; Set origin register to point to the data
	mov di, CUR_LEVEL(box_coords)	; Set output register to point to the level
	mov cx, [CUR_LEVEL(num_boxes)]	; loop for num_boxes
	shl cx, 1			; x2 for x and y coordinate

	; Set segments at 0x0000, as the memory addresses we're using are calculated
	; from that segment
	xor ax, ax
	mov ds, ax
	mov es, ax

	; Copy the box bytes
	rep movsb

	popa			; Restore registers
	mov sp, bp		; Restore stack
	pop bp
	ret 2			; Remove param from stack on return (2 bytes)


; Load the binary levels with a label
LEVELS:
	incbin "res/levels.bin"
