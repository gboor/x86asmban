; This file contains the game entrypoint.
; Start with the constants, which define a bunch of values used in various places
%include "constants.asm"

; Then we include sprites.asm and levels.asm, which contain the binary sprites and levels
; and some NASM macros that will make using the sprites and levels a lot easier. It must
; be included before the macros can be used.
%include "sprites.asm"
%include "levels.asm"

; The animation code, which contains functions and structs for handling animations
%include "animation.asm"

; Audio code, for playing audio
%include "audio.asm"

; Timer code for handling the timer interrupt, which in turn handles animation and audio
%include "timer.asm"

; Set the video and buffer segments
; 0xA000 is the raw VGA memory segment.
; 0x7000 is the last bit of conventional memory. We are making the assumption here that
; the MBR itself will never be so big that it needs all the memory up to that point.
%define VIDEO_SEGMENT 0xA000
%define BUFFER_SEGMENT 0x7000

; This is the entrypoint of the game, which is jumped to from mbr.asm after reading the disk
game:
	; Switch to video mode 0x13, giving us 320x200 pixels with 256 colors in a palette
	xor ah, ah		; Function 0x00: set video mode
	mov al, 0x13		; Video mode
	int 0x10		; Video interrupt

	; Load the palette
	mov ah, 0x10		; Function 0x10: set/Get palette registers
	mov al, 0x12		; Subfunction 0x12: set a block of palette registers
	xor bx, bx		; First color register to set: 0
	mov cx, 256		; Number of color registers to set
	mov dx, SPRITE(palette)	; Memory address of the palette. This function reads from es:dx.
				; The macro SPRITE is defined in sprites.asm.
				; This function assumes 3x cx bytes in the order r,g,b.
				; All colors are 18 bit in this mode, with 6 bits per color.
	int 0x10		; Video interrupt

	; Initialize the timer, which handles audio and animation
	call init_timer

.init_level:
	; Init the level in the idx (initializes to 0x0000 - the first level)
	push word [CUR_LEVEL(idx)]	; Level index
	call load_level			; Function defined in levels.asm

	; Reset player orientation to facing up
	mov word [CUR_ANIM(base_ptr)], PLAYER(up)

	; Clear the screen (previous level bits may still be visible)
	call clear_screen

.game_loop:
	call draw_game

	; If we are in an animation, do not check keyboard, just loop
	cmp word [CUR_ANIM(state)], ANIM_STATE_RUNNING
	je .game_loop

	; Check if there is an animation that was completed
	cmp word [CUR_ANIM(state)], ANIM_STATE_COMPLETED
	jne .post_anim_completed

	; Stop the animation
	call stop_anim

	; If we have a completed animation, apply the animation
	; coordinates to the player and box and stop the animation
	mov ax, [CUR_ANIM(direction_x)]
	add [CUR_LEVEL(player_x)], al
	mov dx, [CUR_ANIM(direction_y)]
	add [CUR_LEVEL(player_y)], dl

	; Check if a box was animated
	mov bx, [CUR_ANIM(box_ptr)]
	test bx, bx
	jz .post_anim_completed

	; Update the box
	add [bx], al
	add [bx+1], dl

	; We moved a box, check if the level is finished
	call is_finished
	jnc .victory		; This level is finished, move to victory mode

.post_anim_completed:
	; Check for keyboard input
	call get_input
	jc .game_loop		; No input

	; Scancode is in ah, ASCII code is in al
	; Special check for the R key, which will reload the level
	cmp al, 'r'
	je .init_level

	; And for N, which moves us to the next level
	cmp al, 'n'
	je .next_level

	; Check for movement
	push ax
	call process_movement
	jc .game_loop		; No movement was made

	; Put the player coordinates (x,y) into cx
	mov cx, [CUR_LEVEL(player_x)]	; This will copy the WORD at
					; player_x into cx, meaning it
					; will also include the y coord

	; Calculate the new coordinate - offset is in ax (ah/al)
	add ch, ah
	add cl, al

	; Now we need to check if this move is LEGAL
	; that means; it's not a wall, or 2 boxes

	; By calling convention, the caller must store
	; ax, cx and dx if they need to be preserved.
	; We need to preserve cx and ax
	push ax
	push cx

	; First find out if there's a box in this position
	push cx
	call get_box_index

	; Restore original cx and ax
	pop cx
	pop ax

	; Now bx is the box index in mem, CF is set if no box was found
	jc .no_box

	; We have a box, so apply the coordinate changes once more
	; That way, we can perform the legal test for the box and
	; if that is fine, the move is fine and we will push the box
	mov dx, cx			; Store the box coordinates in dx
	add dh, ah
	add dl, al

	; Store original ax, bx, cx
	push ax
	push bx
	push cx

	; Check AGAIN if we have a box in this new spot. If we do,
	; this move is illegal, as you cannot move 2 boxes
	push dx
	call get_box_index

	; Restore ax, bx and cx - we do this here so the stack does not grow infinitely
	pop cx
	pop bx
	pop ax

	; If there is a box, the move is illegal, back to start
	jnc .game_loop

	; Check if the new coordinate is traversable, store ax (function does not use cx)
	push ax

	push dx
	call is_traversable

	; Restore ax
	pop ax

	; The move is illegal
	jc .game_loop

	; Skip the traversable check for player, as it must be legal
	jmp .move_player

.no_box:
	; Store ax
	push ax

	; Check if the player coords (in cx) are traversable
	push cx
	call is_traversable

	; Restore ax
	pop ax

	jc .game_loop			; The move is illegal

.move_player:
	; Start an animation
	; ax contains the direction word for the player to move
	; in and bx is the memory address of the box we are pushing
	; or 0 if there is no box
	push ax
	push bx
	call start_anim

	; Start the walk sound
	push SOUND_WALK
	push SOUND_WALK_SIZE
	call start_audio

	jmp .game_loop

.victory:
	; Stop the animation
	call stop_anim

	; Change player to victory pose
	mov word [CUR_ANIM(base_ptr)], PLAYER(victory)

	; Redraw the game with last box opened and player in victory pose
	call draw_game

	; Play the victory sound
	push SOUND_VICTORY
	push SOUND_VICTORY_SIZE
	call start_audio
	
	; Wait for the victory sound to finish before moving on
.victory_wait:
	cmp word [CUR_AUDIO(playing)], 1
	je .victory_wait

.next_level:
	; Move to the next level
	inc word [CUR_LEVEL(idx)]
	jmp .init_level

	cli
	hlt


; Process movement input. Returns changed coords in ax
; ax is 0 and CF is 1 if no movement was made
; Params:
;  keycode (scancode + ascii)
process_movement:
	push bp				; Basic stack setup
	mov bp, sp

	; Clear ax for return
	xor ax, ax

	; The scancode of the pressed key is now in [bp+4]
	; Check for various key presses we're interested in
	cmp byte [bp+5], 0x48	; Up arrow
	je .move_up
	cmp byte [bp+5], 0x4B	; Left arrow
	je .move_left
	cmp byte [bp+5], 0x4D	; Right arrow
	je .move_right
	cmp byte [bp+5], 0x50	; Down arrow
	je .move_down

	; al contains the ASCII code of the key pressed, in [bp+6]
	; so also check WASD - first lower-case it
	or byte [bp+4], 0x20	; Turns an upper-case ASCII code into a
				; lower case one
	cmp byte [bp+4], 'w'
	je .move_up
	cmp byte [bp+4], 'a'
	je .move_left
	cmp byte [bp+4], 's'
	je .move_down
	cmp byte [bp+4], 'd'
	je .move_right

	; Not a key we want
	stc			; Set CF to 1
	jmp .end_process_input

.move_up:
	; y coord change is in ah
	dec ah
	mov word [CUR_ANIM(base_ptr)], PLAYER(up)	; Player faces up
	jmp .end_process_input
.move_down:
	inc ah
	mov word [CUR_ANIM(base_ptr)], PLAYER(down)	; Player faces down
	jmp .end_process_input
.move_left:
	; x coord change is in al
	dec al
	mov word [CUR_ANIM(base_ptr)], PLAYER(left)	; Player faces left
	jmp .end_process_input
.move_right:
	inc al
	mov word [CUR_ANIM(base_ptr)], PLAYER(right)	; Player faces right

.end_process_input:
	mov sp, bp			; Restore stack
	pop bp
	ret


; Checks for keyboard input
; Returns the pressed key in ax, if any
; Sets CF to 1 if no key was pressed
get_input:
	push bp				; Basic stack setup
	mov bp, sp

	; Assume worst case, set the carry flag
	stc

	; Check for keyboard input - this sets the ZF to 1 if no keystroke is present
	mov ah, 0x01		; Sub function 0x01 - check for keystroke
	int 0x16		; Keyboard interrupt
	jz .end_get_input	; If nothing in the buffer, stop

	; Clear the carry flag, we have at least one key
	clc

.get_key:
	; Read keystroke and clear from buffer
	; Even though the above function also stores the pressed key in ax,
	; it does not clear the buffer, so we must call this one to clear it
	xor ah, ah		; Sub function 0x00 - read keystroke
	int 0x16		; Keyoard interrupt
	push ax			; Store the read key so we can re-use ah in the next call

	; Check if there is another keystroke - we only want the last one so pressed
	; keys don't pile up during animations
	mov ah, 0x01		; Sub function 0x01 - check for keystroke
	int 0x16		; If nothing in the buffer, stop

	pop ax			; Restore read key

	jnz .get_key		; Get another key if there are more

.end_get_input:
	mov sp, bp			; Restore stack
	pop bp

	ret


; Clear screen by filling it with a pre-defined background color
clear_screen:
	; Store registers
	pusha

	; Store es value
	push es

	mov cx, SCREEN_WORDS		; Size of video buffer in words
	mov ax, BUFFER_SEGMENT		; Segment of video buffer
	mov es, ax
	xor di, di			; Start at 0
	mov ah, [SPRITE(background)]	; Color to fill with
	mov al, [SPRITE(background)]	; Again, so both bytes in the word have the same color
	rep stosw			; Fill buffer -> stores value in ax cx times at es:di

	; Restore es
	pop es

	; Restore registers
	popa


; Draw buffer onto the screen
draw_buffer:
	; Store registers and segments
	pusha
	push es
	push ds

	; Copy the buffer
	mov cx, SCREEN_WORDS		; The number of words to copy
	mov ax, BUFFER_SEGMENT		; The originating memory
	mov ds, ax
	mov ax, VIDEO_SEGMENT		; The target
	mov es, ax
	xor di, di			; Placement 0x0000
	xor si, si			; for both origin and target
	rep movsw			; Copy

	; Restore segments
	pop ds
	pop es

	; Restore registers
	popa
	ret


; Wait for screen sync
wait_sync:
	; Store existing values
	push dx
	push ax

	mov dx, 0x03DA

	; Wait for the end of a sync
	; This triggers if the function
	; is called DURING a sync
.wait_end:
	in al, dx
	test al, 8
	jnz .wait_end

	; Wait for the start of a sync
.wait_start:
	in al, dx
	test al, 8
	jz .wait_start

	; Restore registers
	pop ax
	pop dx
	ret


; Checks if the level is finished - that is, are all boxes
; on goals. CF is 1 if not finished and 0 if it is.
is_finished:
	push bp				; Basic stack setup
	mov bp, sp

	; Loop all boxes
	mov cx, [CUR_LEVEL(num_boxes)]	; Count register for loop
	mov bx, CUR_LEVEL(box_coords)	; Index of the first box

.box_loop:
	; Get the tile for the current box
	push word [bx]
	call get_tile

	; Check the tile (stored in al)
	cmp al, '.'
	je .box_on_goal

	; The given box is NOT on a goal, set CF
	stc
	jmp .end_is_finished

.box_on_goal:
	; Next box
	add bx, 2

	; Continue
	loop .box_loop
	
	; All boxes are on goals

.end_is_finished:
	mov sp, bp			; Restore stack
	pop bp
	ret


; Checks if a given coordinate is a traversable space
; that means; a floor or a goal. Sets CF to 1 if not traversable
; Params:
;  coordinates: 2 coordinate bytes for x and y
is_traversable:
	push bp				; Basic stack setup
	mov bp, sp

	; Get the tile in al
	push word [bp+4]
	call get_tile

	; Check if it's a floor or a goal
	cmp al, ' '
	je .end_is_traversable
	cmp al, '.'
	je .end_is_traversable

	; It is not traversable
	stc
	jmp .end_is_traversable

.end_is_traversable:
	mov sp, bp			; Restore stack
	pop bp
	ret 2				; Remove param from stack on return (2 bytes)


; Gets a tile at a given coordinate
; Params:
;  coordinate: 2 coordinate bytes for x and y
get_tile:
	push bp				; Basic stack setup
	mov bp, sp

	; By calling convention, the callee needs to
	; preserve bx, di and si. We are only using
	; bx in this function
	push bx

	; The level tile offset is y * level_width + x
	mov ax, [bp+4]			; The given coordinate. y is now in ah
					; and x in al
	xor bx, bx
	mov bl, ah			; Move y coordinate into bx
	imul bx, [CUR_LEVEL(width)]	; Multiply by width
	xor ah, ah			; Make ax contain only the x coordinate
	add bx, ax			; Add it - cx is now the tile index

	; Add the memory offset
	add bx, [CUR_LEVEL(data_ptr)]	; The tile is now at [bx]

	; Return the tile in al
	xor ax, ax
	mov al, [bx]

	pop bx				; Restore register
	mov sp, bp			; Restore stack
	pop bp
	ret 2				; Remove param from stack on return (2 bytes)


; Checks if the given coordinates (in 1 word) have a box on it
; This function returns the box memory address in bx (as it can be indexed)
; bx is 0 and CF is 1 if no box was found
; Params:
;  coordinates: 2 coordinate bytes for x and y
get_box_index:
	push bp			; Basic stack setup
	mov bp, sp

	; Point bx at the boxes
	mov bx, CUR_LEVEL(box_coords)

	; Loop through the number of boxes
	mov cx, [CUR_LEVEL(num_boxes)]
.box_loop:
	; Read current box
	mov ax, [bx]

	; Check if the current box is on the given coordinate
	cmp [bp+4], ax
	je .end_get_box_index

	add bx, 2		; Next box (2 bytes per box)
	loop .box_loop

	; Box was not found, return 0 and set the CF
	xor bx, bx
	stc
	jmp .end_get_box_index

.end_get_box_index:
	mov sp, bp		; Restore stack
	pop bp
	ret 2			; Remove param from stack on return (2 bytes)


; Draws the level currently pointed to by current_level
; Params:
;  segment word: the segment in mem to draw the level to
;                e.g. 0xA000 for VGA memory
draw_level:
	push bp			; Basic stack setup
	mov bp, sp
	pusha			; Push everything - this function doesn't
				; return anything

	; Set es to the passed segment
	mov es, [bp+4]

	; Reset source segment - source data is read from segment 0x0000
	xor ax, ax
	mov ds, ax

	; Use the following registers:
	; ax = x coordinate
	; bx = pointer to data (since it can be indexed)
	; cx = y coordinate
	; dx = multiplier for the pixel coordinates (SPRITE_SIZE * tile x|y)

	; Set bx to point to the level data
	mov bx, [CUR_LEVEL(data_ptr)]

	; Start from coord 0,0
	xor ax, ax
	xor cx, cx

.loop_y:
	.loop_x:
		; Check what byte we have
		cmp byte [bx], '-'	; Empty space
		je .post_draw

		; We are drawing something, calculate the coordinates
		; First store the original values
		push ax
		push cx

		; Multiply ax by SPRITE_SIZE
		; We use imul here, as it supports hard-coded operands
		; and does not require ax as its result
		imul ax, SPRITE_SIZE
		imul cx, SPRITE_SIZE

		; Add the offsets
		add ax, [CUR_LEVEL(offset_x)]
		add cx, [CUR_LEVEL(offset_y)]

		; Push the coordinates
		push ax
		push cx

		; Push the width and height
		push SPRITE_SIZE
		push SPRITE_SIZE

		; Do not use transparency
		push 0

		; Check what to draw
		cmp byte [bx], '#'	; Wall
		je .draw_wall
		cmp byte [bx], '.'	; Goal
		je .draw_goal
		jmp .draw_floor		; Anything else is a floor.
					; If it's not, something is corrupt in the data
		
	.draw_wall:
		push SPRITE(wall)
		jmp .draw
	.draw_goal:
		push SPRITE(goal)
		jmp .draw
	.draw_floor:
		push SPRITE(floor)
	.draw:
		; All variables are pushed, do the actual draw
		call draw_sprite

		; Restore original ax and cx
		pop cx
		pop ax

	.post_draw:
		; Next tile
		inc bx

		; Move the offset to the next coord for x
		inc ax

		; Check if the loop is over
		cmp ax, [CUR_LEVEL(width)]
		je .end_loop_x

		; Loop
		jmp .loop_x

	.end_loop_x:
		; Move the offset to the next coord for y
		inc cx

		; Check if the loop is over
		cmp cx, [CUR_LEVEL(height)]
		je .end_loop_y

		; Start from the first x coord again
		xor ax, ax

		; Loop
		jmp .loop_y

.end_loop_y:

	; Now we draw the boxes - on top of the existing floors
	; The current coordinates are in CUR_LEVEL(box_coords)
	; There are CUR_LEVEL(num_boxes) boxes
	mov cx, [CUR_LEVEL(num_boxes)]	; Loop for num_boxes
	mov bx, CUR_LEVEL(box_coords)	; Set bx to the box coords
.box_loop:
	; We need to read the x/y bytes, but handle them as words
	xor ax, ax
	mov al, [bx]			; Box x, total value is now in word ax

	; Calculate and push x coordinate
	imul ax, SPRITE_SIZE
	add ax, [CUR_LEVEL(offset_x)]

	; Same for y coordinate
	xor dx, dx
	mov dl, [bx+1]
	imul dx, SPRITE_SIZE
	add dx, [CUR_LEVEL(offset_y)]

	; Check if this box is being animated
	cmp bx, [CUR_ANIM(box_ptr)]
	jne .box_push_params

	; Add the animation offsets
	add ax, [CUR_ANIM(offset_x)]
	add dx, [CUR_ANIM(offset_y)]

.box_push_params:
	; Push offsets sizes and transparency
	push ax
	push dx
	push SPRITE_SIZE
	push SPRITE_SIZE
	push 0			; No transparency

	; Check if this box is on a goal ([bx] is the coord word)
	push word [bx]
	call get_tile
	cmp al, '.'
	je .box_on_target

	; Box is not on target
	push SPRITE(box)
	jmp .draw_box

.box_on_target:
	; Box is on a target
	push SPRITE(box_target)

.draw_box:
	call draw_sprite

	; Next box coord (2 bytes)
	add bx, 2

	; Repeat
	loop .box_loop


	; Finally, draw the character
	; The offset of the character is a bit different, as its height can differ
	; and the bottom should align with the sprites. As such, the y-offset should be
	; y * SPRITE_SIZE - (PLAYER_HEIGHT - SPRITE_SIZE)
	; Same byte-to-word conversion as above for boxes
	mov al, [CUR_LEVEL(player_x)]
	imul ax, SPRITE_SIZE
	add ax, [CUR_LEVEL(offset_x)]
	add ax, [CUR_ANIM(offset_x)]
	push ax

	mov al, [CUR_LEVEL(player_y)]
	imul ax, SPRITE_SIZE
	sub ax, PLAYER_OFFSET		; Player might be a bit taller,
					; subtract offset
	add ax, [CUR_LEVEL(offset_y)]
	add ax, [CUR_ANIM(offset_y)]
	push ax

	; Get the current frame of the animation
	mov ax, [CUR_ANIM(frame)]
	imul ax, PLAYER_BYTES		; Multiply with byte count to get the offset
	add ax, [CUR_ANIM(base_ptr)]	; Points to the current direction sprite

	; Push the rest and draw
	push SPRITE_SIZE
	push PLAYER_HEIGHT
	push 1			; Use transparency
	push ax
	call draw_sprite

	; End of draw - clean up
	popa			; Restore registers
	mov sp, bp		; Restore stack
	pop bp
	ret 2			; Remove param from stack on return (2 bytes)


; Draws the sprite from ds:[sprite] -> es:[coord]
; Params:
;  x		x coordinate			[bp+14]
;  y		y coordinate			[bp+12]
;  width	sprite width			[bp+10]
;  height	sprite height			[bp+8]
;  transparent	use transparency 1/0		[bp+6]
;  sprite	memory address of sprite data	[bp+4]
draw_sprite:
	push bp			; Basic stack setup
	mov bp, sp
	pusha			; Push everything - this function doesn't
				; return anything

	mov si, [bp+4]		; Sprite address

	; Calculate the offset of the coordinates in mem
	mov ax, [bp+12]		; The y coordinate
	imul ax, SCREEN_WIDTH   ; Multiply by screen width to get the offset,
				; which will fit in a 16 bit register, so this is fine

	add ax, [bp+14]		; Add the x coordinate
	mov di, ax		; Store it in di for the rep movsb operation

	xor bx, bx		; Start at pixel 0

.draw_loop:
	mov cx, [bp+10]				; Set the sprite width in the count register

	cmp word [bp+6], 1			; Check if we have transparency
	je .draw_transparent			; If so, go to that subroutine

	; Normal draw
	rep movsb				; Move cx bytes from ds:si -> es:di, with es:di pointing at
						; the screen buffer and ds:si pointing at the raw image data
	jmp .end_draw_line			; Line draw done

.draw_transparent:
	; This draw mode is a bit different, as we need to check each byte for transparency
	lodsb					; Load byte at ds:si into al, also increments si
	cmp al, [SPRITE(transparent)]		; Check if it's transparent
	jne .draw_byte				; If not, jump to the draw byte

	inc di					; Else skip to the next byte (since we don't stosb)
	loop .draw_transparent			; Next byte, also decrements cx
	jmp .end_draw_line			; Jump to end if loop is complete

.draw_byte:
	stosb					; Write byte in al to es:di, also increments di
	loop .draw_transparent			; Next byte, also decrements cx

.end_draw_line:
	add di, SCREEN_WIDTH-SPRITE_SIZE	; Add a full row of screen to move to the next line,
						; minus the sprite width
	inc bx					; Next pixel
	cmp bx, [bp+8]				; Check if we drew all the lines
	je .end_draw_loop			; If so, exit
	jmp .draw_loop				; If not, repeat

.end_draw_loop:
	popa			; Restore registers
	mov sp, bp		; Restore stack
	pop bp
	ret 12			; Remove param from stack on return (12 bytes)


; Draws the whole game to buffer, then buffer to screen
draw_game:
	; Draw the level onto the buffer
	push BUFFER_SEGMENT
	call draw_level

	; Wait for sync
	call wait_sync

	; Draw the buffer
	call draw_buffer

	ret
