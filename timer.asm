; Offset of int 0x1C - 4 bytes per entry
%define TIMER_IVT_OFFSET 0x1C * 4

; Timer modifier for ~8khz audio
%define COUNTER (0x1234DC / 8000) ; & 0xFFFE

; Number of audio ticks per animation frame
; This is pure trial & error, 100 seems to give a nice speed
%define ANIM_AT_TICK 100

; Tick counter
timer_tick_counter: dw 0


; Timer init function. Hooks int 0x1C and sets proper interval for timer 0
init_timer:
	push bp			; Basic stack setup
	mov bp, sp
	pusha			; Store all registers

	; Stop all interrupts while we change an entry
	cli

	; IVT in real mode is at 0x0000:0x0000
	; Each entry is 2 words; offset and address, with address first,
	; then offset. We are using offset 0 in this entire MBR and the
	; address is the defined timer_isr below.
	mov word [TIMER_IVT_OFFSET], timer_isr
	mov word [TIMER_IVT_OFFSET+2], 0x0000

        ; Reprogram PIT Channel 0 to fire IRQ0 at 8kHz for audio
        mov al, 0x36		; Settings for the timer register;
				; Corresponds to 00110110
				; 00 = channel 0
				; 11 = access mode lobyte/hibyte
				; 011 = mode 3 (square wave generator)
				; 0 = 16 bit binary mode
        out 0x43, al		; 0x43 is the timer Mode/Command register
        mov ax, COUNTER		; Load the divider/modifier we want
        out 0x40, al		; Port 0x40 is channel 0 - lobyte first
        mov al, ah
        out 0x40, al		; Hibyte second

	; Restart interrupts
	sti

	popa			; Restore registers
	mov sp, bp		; Restore stack
	pop bp
	ret


; The timer isr that triggers every tick (around 8000 times per second with current settings)
timer_isr:
	; Store locally used registers
	push ds
	push ax

	; Reset ds - other interrupts might have changed it
	xor ax, ax
	mov ds, ax

	; Always call the audio handler
	call audio_timer_isr

	; Increase the tick counter
	inc word [timer_tick_counter]
	cmp word [timer_tick_counter], ANIM_AT_TICK
	jl .no_animation

	; Trigger the next animation frame
	call anim_timer_isr

	; Reset tick counter
	mov word [timer_tick_counter], 0

.no_animation:
	; Restore registers
	pop ax
	pop ds

	iret
