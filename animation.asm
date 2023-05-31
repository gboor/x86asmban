; Different animation states
%define ANIM_STATE_NONE      0
%define ANIM_STATE_RUNNING   1
%define ANIM_STATE_COMPLETED 2

; Define a structure for the running animation
struc ANIMATION
	.state       resw 1	; 0 = none, 1 = running, 2 = finished
	.ticks       resw 1	; The number of ticks elapsed in this animation
	.frame       resw 1	; The frame we're displaying
	.offset_x    resw 1	; The X pixel offset we are at in the movement
	.offset_y    resw 1	; The Y pixel offset we are at in the movement
	.direction_x resw 1	; The direction for x, -1, 0 or 1
	.direction_y resw 1	; The direction for y, -1, 0 or 1
	.base_ptr    resw 1	; Pointer to the base image to use depending on direction
	.box_ptr     resw 1	; Pointer to a box, if we're pusing one, or 0 if not
endstruc

current_anim:
istruc ANIMATION
	at ANIMATION.state,       dw ANIM_STATE_NONE
	at ANIMATION.ticks,       dw 0
	at ANIMATION.frame,       dw 0
	at ANIMATION.offset_x,    dw 0
	at ANIMATION.offset_y,    dw 0
	at ANIMATION.direction_x, dw 0
	at ANIMATION.direction_y, dw 0
	at ANIMATION.base_ptr,    dw PLAYER(up)	; Start facing up
	at ANIMATION.box_ptr,     dw 0
iend

; Macro for easy access to the current animation fields
%define CUR_ANIM(field) current_anim + ANIMATION. %+ field


; The timer isr that triggers every ~55ms
anim_timer_isr:
	push bp				; Basic stack setup
	mov bp, sp

	; Store registers
	push ax
	push bx

	; If we're not animating, do nothing
	cmp word [CUR_ANIM(state)], ANIM_STATE_RUNNING
	jne .end_anim_timer_isr

	; Increase tick count
	mov ax, [CUR_ANIM(ticks)]
	inc ax

	; If the tick count >= SPRITE_SIZE, this animation is finished
	cmp ax, SPRITE_SIZE
	jg .completed

	; Set the new tick count
	mov [CUR_ANIM(ticks)], ax

	; Change the frame every 4 pixels
	; We start on frame 1 in start_anim, change it to 2 at 4,
	; then back to 1 at 8, again to 2 at 12
	; First check if the tick count is divisible by 4
	mov bl, 4
	div bl				; Divides ax by bl and stores remainder in ah
	test ah, ah
	jz .change_frame		; ah is 0, so we're on 4, 8, or 12
	jmp .after_change_frame

.change_frame:
	; We know that the tick count was divisible by 4 and al contains
	; the quotient following the div call. This means al is 1 (frame 4),
	; 2 (frame 8), or 3 (frame 12)
	test al, 1			; This tests if al is odd or even
	sete al				; Sets al to 1 if it's odd (frame 4 and 12),
					; or 0 if even (frame 8)
	inc ax				; Sets ax to the frame we want (since ah is 0)
	mov [CUR_ANIM(frame)], ax

.after_change_frame:

	; Increase the offset by the direction
	mov ax, [CUR_ANIM(direction_x)]
	add [CUR_ANIM(offset_x)], ax
	mov ax, [CUR_ANIM(direction_y)]
	add [CUR_ANIM(offset_y)], ax

	jmp .end_anim_timer_isr

.completed:
	; Set the flag for this animation as completed
	mov word [CUR_ANIM(state)], ANIM_STATE_COMPLETED

.end_anim_timer_isr:
	; Restore registers
	pop bx
	pop ax

	mov sp, bp			; Restore stack
	pop bp

	ret


; Start an animation
; Params:
;  coordinates - the coordinate word used for the direction
;                consists of 2 bytes with y in hi and x in lo
;  box         - memory address of a box, if we're pushing one,
;                otherwise 0
start_anim:
	push bp				; Basic stack setup
	mov bp, sp

	; Store ax
	push ax

	; Convert x coordinate from byte -> word
	xor ax, ax
	mov al, [bp+6]
	cbw				; This turns the byte in al into
					; a word in ax, while keeping the
					; same sign
	mov [CUR_ANIM(direction_x)], ax

	; Convert y coordinate
	mov al, [bp+7]
	cbw
	mov [CUR_ANIM(direction_y)], ax

	; Start at 0 ticks and frame 2 (makes the smoothest animation)
	mov word [CUR_ANIM(ticks)], 0
	mov word [CUR_ANIM(frame)], 2

	; Set the box pointer
	mov ax, [bp+4]
	mov [CUR_ANIM(box_ptr)], ax

	; Turn on animation
	mov word [CUR_ANIM(state)], ANIM_STATE_RUNNING

	; Restore ax
	pop ax

	mov sp, bp			; Restore stack
	pop bp
	ret 4


; Stop an animation
; Returns 1 in the carry flag if there is no completed animation
stop_anim:
	; Reset offsets
	mov word [CUR_ANIM(offset_x)], 0
	mov word [CUR_ANIM(offset_y)], 0

	; Set frame to waiting frame
	mov word [CUR_ANIM(frame)], 0

	; Turn off the state
	mov word [CUR_ANIM(state)], ANIM_STATE_NONE

	ret
