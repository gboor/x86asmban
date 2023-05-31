; Offset of int 0x1C - 4 bytes per entry
XTIMER_IVT_OFFSET equ 0x1C * 4

; Timer modifier for ~8khz
COUNTER equ (0x1234DC / 8000) & 0xFFFE

; Define a structure for the running audio file
struc AUDIO
	.ptr         resw 1	; Pointer to the audio data
	.offset      resw 1     ; Offset in the audio data
	.size        resw 1     ; Size of the audio data
	.playing     resw 1	; 0 = false, 1 = true
endstruc

current_audio:
istruc AUDIO
	at AUDIO.ptr,     dw 0
	at AUDIO.offset,  dw 0
	at AUDIO.size,    dw 0
	at AUDIO.playing, dw 0
iend

; Macro for easy access to the current audio fields
%define CUR_AUDIO(field) current_audio + AUDIO. %+ field


; The timer isr that triggers according to the audio frequency
audio_timer_isr:
	push bp				; Basic stack setup
	mov bp, sp

	; Store locally used registers
	push ax
	push bx
	push si

	; If we're not playing, do nothing
	cmp word [CUR_AUDIO(playing)], 1
	jne .end_audio_timer_isr

        mov     bx, [CUR_AUDIO(ptr)]   ; Load our data pointers
        mov     si, [CUR_AUDIO(offset)]
        cmp     si, [CUR_AUDIO(size)]	; Check if audio is finished
        jae     .nosnd

	mov     ah, [bx+si]     ; If not, load up the value
	;shr     ah, 1           ; Make it a 7 bit value
	mov     al, 0xb0        ; And program PIT channel 2 to
	out     0x43, al        ; deliver a pulse that many
	mov     al, ah          ; microseconds long
	out     0x42, al
	xor     al, al
	out     0x42, al

        inc     si              ; Update pointer
        mov     [CUR_AUDIO(offset)], si
        jmp     .end_audio_timer_isr         ; ... and jump to end

	;; If we get here, we're past the end of the sound.
.nosnd:
	; Stop playing audio
	in al, 0x61		; Read port 0x61, this is the keyboard controller, which also controls the speaker for some reason
	and al, 0xFC		; Clear bits 0 and 1 - bit 0 enables the connection of the PIT timer to the speaker and bit 1 turns on speaker data
	out 0x61, al		; Write new value

	; Set playing to 0
	mov word [CUR_AUDIO(playing)], 0

.end_audio_timer_isr:
	; Restore registers
	pop si
	pop bx
	pop ax

	mov sp, bp			; Restore stack
	pop bp

	ret


; Start playing audio
; Params:
;  address     - The memory address of the audio data
;  length      - The length of the audio data
start_audio:
	push bp				; Basic stack setup
	mov bp, sp

	; Store ax
	push ax

	; Load the sound
	mov ax, [bp+6]
	mov [CUR_AUDIO(ptr)], ax		; Pointer to the audio data
	mov ax, [bp+4]
	mov [CUR_AUDIO(size)], ax		; Length for the size comparison
	mov word [CUR_AUDIO(offset)], 0		; Start at offset 0
	mov word [CUR_AUDIO(playing)], 1	; Set playing flag to true

	; Start playing audio
        in al, 0x61		; Read port 0x61, this is the keyboard controller, which also controls the speaker for some reason
        or al, 3		; Set bits 0 and 1 - bit 0 enables the connection of the PIT timer to the speaker and bit 1 turns on speaker data
        out 0x61, al		; Write the new value

	; Restore ax
	pop ax

	mov sp, bp			; Restore stack
	pop bp
	ret 4


; Load the binary audio files with a label
SOUND_WALK:
	incbin "res/walk.raw"
SOUND_WALK_END:
SOUND_WALK_SIZE equ SOUND_WALK_END - SOUND_WALK

SOUND_VICTORY:
	incbin "res/victory.raw"
SOUND_VICTORY_END:
SOUND_VICTORY_SIZE equ SOUND_VICTORY_END - SOUND_VICTORY
