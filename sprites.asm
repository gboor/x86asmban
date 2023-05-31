; The size of a level sprite in bytes
%define SPRITE_BYTES SPRITE_SIZE * SPRITE_SIZE

; The size of a level sprite in words
%define SPRITE_WORDS SPRITE_BYTES / 2

; Size of the screen in bytes
%define SCREEN_BYTES SCREEN_WIDTH * SCREEN_HEIGHT

; Size of the screen buffer in words
%define SCREEN_WORDS SCREEN_BYTES / 2

; The size of a character sprite in bytes
%define PLAYER_BYTES SPRITE_SIZE * PLAYER_HEIGHT

; The offset of the player sprite is the player height minus the sprite size
; This offset is used to make sure the bottom of the player aligns with the
; bottom of the underlying sprite, while the top can be higher if the player
; sprite is taller than the other sprites
%define PLAYER_OFFSET PLAYER_HEIGHT - SPRITE_SIZE

; A macro for easy level sprite access
; Example; this macro turns "SPRITE(wall)" into "SPRITES + SPRITE_DATA.wall,"
; which gives us the exact address in memory of the start of the wall sprite.
; SPRITES is defined below as a pointer to the entire sprite data in memory,
; SPRITE_DATA has the correct offsets.
%define SPRITE(field) SPRITES + SPRITE_DATA. %+ field

; Same for the character, with the level sprites, color indexes and palette already skipped
%define PLAYER(field) SPRITES + (3 * 256) + 2 + (5 * SPRITE_BYTES) + PLAYER_DATA. %+ field

; Define a data struct for the sprite data
struc SPRITE_DATA
	.palette     resb 3 * 256	; 256 RGB colors with 1 byte per color
	.transparent resb 1		; The index of the transparent color
	.background  resb 1		; The index of the background color
	.floor       resb SPRITE_BYTES	; First sprite is open floor
	.wall        resb SPRITE_BYTES	; Second sprite is a wall
	.goal        resb SPRITE_BYTES	; Third sprite is a target
	.box         resb SPRITE_BYTES	; Fourth sprite is a box
	.box_target  resb SPRITE_BYTES	; Fifth sprite is a box on a target
endstruc

; Define a structure for the character data
struc PLAYER_DATA
	.up      resb PLAYER_BYTES * 3	; 3 frames facing up
	.down    resb PLAYER_BYTES * 3	; 3 frames facing down
	.right   resb PLAYER_BYTES * 3	; 3 frames facing right
	.left    resb PLAYER_BYTES * 3	; 3 frames facing left
	.victory resb PLAYER_BYTES      ; 1 frame victory pose
endstruc

; Load the binary sprites with a label
; This contains palette, level and character sprites
SPRITES:
	incbin "res/sprites.bin"
