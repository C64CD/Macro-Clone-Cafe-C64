;
; MACRO CLONE CAFE
;

; Code and graphics by TMR
; Music by Odie


; A quick, written-from-scratch and expanded copy of the C64 demos
; Electric Cafe by Ash & Dave and Macrojackmix by Pulse Productions
; which supports Commodore's SFX Sound Expander (with thanks to
; Graham/Oxyron for his documentation)
; Coded for C64CrapDebunk.Wordpress.com

; Released at Slipstock 2016

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/
; Compression is handled with Exomizer 2 which can be downloaded at
; http://hem.bredband.net/magli143/exo/

; build.bat will call both to create an assembled file and then the
; crunched release version.


; Memory Map

; $1000 - $1fff		program code/data/scrolling messages
; $2000 - $23ff		character set
; $2400 - $27ff		sprites
; $3000 - $fbff		sample data


; Set this to $00 for 4-bit volume samples or $01 for SFX Sound Sampler
sfx_sampler	= $01


; Select an output filename
		!to "macro_cafe.prg",cbm


; Pull in the binary data
		* = $2000
char_data	!binary "binary\plain_font_8x8.chr"

		* = $2400
		!binary "binary\gas_mask.spr"

		* = $3000
sample_data	!binary "binary\md_riff_6800hz.raw"
sample_end


; Raster split positions
raster_1_pos	= $00
raster_2_pos	= $3c
raster_3_pos	= $eb

; Constants
sample_freq	= $8d
dir_timer_value	= $0104

; Label assignments
raster_num	= $40

irq_store_a	= $41
irq_store_x	= $42
irq_store_y	= $43
irq_store_1	= $44

nmi_store_a	= $45
nmi_store_y	= $46

sample_pos	= $48		; two bytes used

scroll_1_pos	= $4a		; two bytes used
scroll_1_timer	= $4c
scroll_2_pos	= $4d		; two bytes used
scroll_2_x	= $4f
scroll_2_speed	= $50

bar_timer	= $51

sine_at_1	= $52
sprt_dir_flag	= $53
sprt_dir_timer	= $54		; two bytes used
sprt_pls_timer	= $56

scroll_1_line	= $0400
scroll_2_line	= $07c0


; Entry point for the code
		* = $1000

; Stop interrupts, disable the ROMS and set up NMI and IRQ interrupt pointers
code_start	sei

		ldx #$50
		lda #$00
clear_zp	sta $00,x
		inx
		bne clear_zp

		lda #$35
		sta $01

		lda #<nmi_int
		sta $fffa
		lda #>nmi_int
		sta $fffb

		lda #<irq_int
		sta $fffe
		lda #>irq_int
		sta $ffff

; Set the VIC-II up for a raster IRQ interrupt
		lda #$7f
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d

		lda #raster_1_pos
		sta $d012

		lda #$1b
		sta $d011
		lda #$01
		sta $d019
		sta $d01a

; Set the NMI interrupt up for the sample player
		lda #$81
		sta $dd0d
		lda #$00
		sta $dd0e
		sta $dd06

; Interrupt speed, controls the sample playback rate
		lda #sample_freq
		sta $dd04
		lda #$00
		sta $dd05

; Conditiional assembly - convert 8-bit samples down to 4-bit
!if sfx_sampler=$00 {
		lda #$34
		sta $01

		jsr sample_reset
		ldy #$00
sample_reduce	lda (sample_pos),y
		lsr
		lsr
		lsr
		lsr
		sta (sample_pos),y

		inc sample_pos+$00
		bne *+$04
		inc sample_pos+$01

		lda sample_pos+$00
		cmp #<sample_end
		bne sample_reduce
		lda sample_pos+$01
		cmp #>sample_end
		bne sample_reduce

		lda #$35
		sta $01
}

; Initialise some of our own labels
		lda #$01
		sta raster_num

		jsr sample_reset

; Set up the screen RAM
		ldx #$00
screen_clear	lda #$20
		sta $0400,x
		sta $0500,x
		sta $0600,x
		sta $06e8,x
		lda #$0e
		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $dae8,x
		inx
		bne screen_clear

		ldx #$00
		lda #$03
scroll_col_set	lda #$0f
		sta $d800,x
		lda #$0d
		sta $dbc0,x
		inx
		cpx #$28
		bne scroll_col_set

		ldx #$00
screen_build	lda screen_data+$00,x
		sta $0450,x
		sta $04c8,x
		sta $0540,x
		sta $05b8,x
		sta $0630,x
		sta $06a8,x
		sta $0720,x
		lda screen_data+$01,x
		sta $0478,x
		sta $04f0,x
		sta $0568,x
		sta $05e0,x
		sta $0658,x
		sta $06d0,x
		sta $0748,x
		lda screen_data+$02,x
		sta $04a0,x
		sta $0518,x
		sta $0590,x
		sta $0608,x
		sta $0680,x
		sta $06f8,x
		sta $0770,x
		inx
		cpx #$28
		bne screen_build

; Reset the SID chip just in case it wasn't already
		ldx #$00
		txa
sid_reset	sta $d400,x
		inx
		cpx #$19
		bne sid_reset

; Reset the scrolling messages
		jsr scroll_1_reset
		lda #$30
		sta scroll_1_timer
		jsr scroll_2_reset
		lda #$03
		sta scroll_2_speed

; Initialise the hardware sprites
		lda #$ff
		sta $d015

		ldx #$00
		lda #$90
set_sprite_dp	sta $07f8,x
		clc
		adc #$01
		inx
		cpx #$08
		bne set_sprite_dp

; Set some of our labels
		lda #<dir_timer_value
		sta sprt_dir_timer+$00
		lda #>dir_timer_value
		sta sprt_dir_timer+$01

; Restart the interrupts
		cli

; Start the NMI interrupt
		lda #$01
		sta $dd0e

; Infinite loop - all of the code is executing on the interrupt
		jmp *


; IRQ interrupt handler
irq_int		sta irq_store_a
		stx irq_store_x
		sty irq_store_y

		lda $d019
		and #$01
		sta $d019
		bne int_go
		jmp irq_exit

; An interrupt has triggered
int_go		lda raster_num
		cmp #$02
		bne *+$05
		jmp irq_rout2

		cmp #$03
		bne *+$05
		jmp irq_rout3


; Raster split 1
irq_rout1	lda #$04
		sta $d020
		sta $d021
		lda #$0e
		sta $d022
		lda #$0f
		sta $d023

		lda #$07
		sta $d016
		lda #$18
		sta $d018

		lda #$ff
		sta $d01b

; Position the sprites
		ldx #$00
		ldy #$00
set_sprite_x	lda sprite_x_pos,x
		sta $d000,y
		iny
		iny
		inx
		cpx #$09
		bne set_sprite_x

		ldx sine_at_1
		inx
		stx sine_at_1
		lda sprite_sinus,x
		sta $d001
		sta $d003
		clc
		adc #$15
		sta $d005
		sta $d007
		clc
		adc #$15
		sta $d009
		sta $d00b
		clc
		adc #$15
		sta $d00d
		sta $d00f

		ldx #$00
		lda sprt_pls_timer
		lsr
		lsr
		tay
		lda sprite_pulse,y
set_sprite_col	sta $d027,x
		inx
		cpx #$08
		bne set_sprite_col

; Set interrupt handler for split 2
		lda #$02
		sta raster_num
		lda #raster_2_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 2
irq_rout2	lda #$17
		sta $d016

		lda #$00
		sta $d01b

; Move the upper scrolling message
		ldx scroll_1_timer
		inx
		stx scroll_1_timer
		cpx #$26
		bcs scr_1_xb

; Move the text line
		ldx #$00
scroll_1_mvr_2	lda scroll_1_line+$01,x
		sta scroll_1_line+$00,x
		inx
		cpx #$25
		bcc scroll_1_mvr_2

; Copy a new character to the scroller
		ldy #$00
scroll_1_mread	lda (scroll_1_pos),y
		bne scroll_1_okay
		jsr scroll_1_reset
		jmp scroll_1_mread

scroll_1_okay	sta scroll_1_line+$25

; Nudge the scroller onto the next character
		inc scroll_1_pos+$00
		bne *+$04
		inc scroll_1_pos+$01

scr_1_xb

; Move the lower scrolling message
		lda scroll_2_speed
		sta irq_store_1

scroll_2_loop	ldx scroll_2_x
		inx
		cpx #$08
		bne scr_2_xb

; Move the text line
		ldx #$00
scroll_2_mvr_2	lda scroll_2_line+$01,x
		sta scroll_2_line+$00,x
		inx
		cpx #$26
		bcc scroll_2_mvr_2

; Copy a new character to the scroller
		ldy #$00
scroll_2_mread	lda (scroll_2_pos),y
		bne scroll_2_okay
		jsr scroll_2_reset
		jmp scroll_2_mread

scroll_2_okay	cmp #$80
		bcc scroll_2_okay_2
		and #$0f
		sta scroll_2_speed
		lda #$20

scroll_2_okay_2	sta scroll_2_line+$26

; Nudge the scroller onto the next character
		inc scroll_2_pos+$00
		bne *+$04
		inc scroll_2_pos+$01

		ldx #$00
scr_2_xb	stx scroll_2_x

		dec irq_store_1
		bne scroll_2_loop

; Set interrupt handler for split 3
		lda #$03
		sta raster_num
		lda #raster_3_pos
		sta $d012

; Exit IRQ interrupt
		jmp irq_exit


; Raster split 3
irq_rout3	lda scroll_2_x
		and #$07
		eor #$07
		sta $d016

		lda #$ff
		sta $d01b

; Move the background bars
		ldx bar_timer
		inx
		cpx #$02
		beq *+$05
		jmp bt_xb

		ldy char_data+$d8

!set byte_count=$00
!do {
		lda char_data+$d9+byte_count
		sta char_data+$d8+byte_count

		!set byte_count=byte_count+$01
} until byte_count=$17

		sty char_data+$ef


		ldx #$00
bt_xb		stx bar_timer

; Update the sprite position
		jsr sprite_update

; Set interrupt handler for split 1
		lda #$01
		sta raster_num
		lda #raster_1_pos
		sta $d012

; Restore registers and exit IRQ interrupt
irq_exit	lda irq_store_a
		ldx irq_store_x
		ldy irq_store_y
		rti

nmi_int		sta nmi_store_a
		sty nmi_store_y

; Read sample data
		dec $01
		ldy #$00
		lda (sample_pos),y
		inc $01

!if sfx_sampler=$00 {
		sta $d418
} else {
		sta $df00
}

; Update the counter
		inc sample_pos+$00
		bne *+$04
		inc sample_pos+$01

; Check for the sample's end for looping
		lda sample_pos+$01
		cmp #>sample_end
		bne nmi_exit

		jsr sample_reset

; Acknowledge the interrupt, restore the registers and exit
nmi_exit	lda $dd0d

		lda nmi_store_a
		ldy nmi_store_y
		rti

; Subroutine to reset the sample player
sample_reset	lda #<sample_data
		sta sample_pos+$00
		lda #>sample_data
		sta sample_pos+$01
		rts

; Subroutines to reset the scrolling messages
scroll_1_reset	lda #<scroll_text_1
		sta scroll_1_pos+$00
		lda #>scroll_text_1
		sta scroll_1_pos+$01
		rts

scroll_2_reset	lda #<scroll_text_2
		sta scroll_2_pos+$00
		lda #>scroll_text_2
		sta scroll_2_pos+$01
		rts


; Update the sprite colour effect
sprite_update	ldx sprt_pls_timer
		inx
		cpx #$20
		bne *+$04
		ldx #$1f
		stx sprt_pls_timer

		lda sine_at_1
		and #$7f
		bne *+$06
		lda #$00
		sta sprt_pls_timer

; Update the sprite X positions
		lda sprt_dir_flag
		beq su_right

su_left		jsr sprite_left
		jmp su_skip_1

su_right	jsr sprite_right

; Position the other sprites to match what 0 and 1 are doing
su_skip_1	lda sprite_x_pos+$00
		sta sprite_x_pos+$02
		sta sprite_x_pos+$04
		sta sprite_x_pos+$06

		lda sprite_x_pos+$01
		sta sprite_x_pos+$03
		sta sprite_x_pos+$05
		sta sprite_x_pos+$07

; Decrease the change-of-direction timer and see if it's hit zero
		ldx sprt_dir_timer+$00
		dex
		cpx #$ff
		bne *+$04
		dec sprt_dir_timer+$01
		stx sprt_dir_timer+$00

		lda sprt_dir_timer+$00
		ora sprt_dir_timer+$01
		bne su_skip_2

; Change direction and reset the counter
		lda sprt_dir_flag
		eor #$01
		sta sprt_dir_flag

		lda #$00
		sta sprt_pls_timer

		lda #<dir_timer_value
		sta sprt_dir_timer+$00
		lda #>dir_timer_value
		sta sprt_dir_timer+$01

su_skip_2	rts

; Move the sprite object left
sprite_left	lda sprite_x_pos+$00
		sec
		sbc #$01
		sta sprite_x_pos+$00
		cmp #$ff
		bne sl_skip_1
		lda sprite_x_pos+$08
		eor #$55
		sta sprite_x_pos+$08

sl_skip_1	lda sprite_x_pos+$01
		sec
		sbc #$01
		sta sprite_x_pos+$01
		cmp #$ff
		bne sl_skip_2
		lda sprite_x_pos+$08
		eor #$aa
		sta sprite_x_pos+$08

sl_skip_2	rts

; Move the sprite object right
sprite_right	lda sprite_x_pos+$00
		clc
		adc #$01
		sta sprite_x_pos+$00
		bne sr_skip_1
		lda sprite_x_pos+$08
		eor #$55
		sta sprite_x_pos+$08

sr_skip_1	lda sprite_x_pos+$01
		clc
		adc #$01
		sta sprite_x_pos+$01
		bne sr_skip_2
		lda sprite_x_pos+$08
		eor #$aa
		sta sprite_x_pos+$08

sr_skip_2	rts


; Screen data (everything is built from this table)
screen_data	!byte $1b,$1c,$1d,$1b,$1c,$1d,$1b,$1c
		!byte $1d,$1b,$1c,$1d,$1b,$1c,$1d,$1b
		!byte $1c,$1d,$1b,$1c,$1d,$1b,$1c,$1d
		!byte $1b,$1c,$1d,$1b,$1c,$1d,$1b,$1c
		!byte $1d,$1b,$1c,$1d,$1b,$1c,$1d,$1b
		!byte $1c,$1d

; Scrolling message colours
scroll_pulse_1	!byte $0f,$07,$0f,$07,$07,$07,$01,$07
		!byte $01,$01,$01,$07,$01,$07,$07,$07
		!byte $0f,$07

scroll_pulse_2	!byte $03,$0d,$03,$0d,$0d,$0d,$01,$0d
		!byte $01,$01,$01,$0d,$01,$0d,$0d,$0d
		!byte $03,$0d

; Sprite bounce data
sprite_sinus
		!byte $ad,$aa,$a7,$a4,$a1,$9e,$9b,$98
		!byte $95,$92,$8f,$8c,$89,$86,$83,$81
		!byte $7e,$7b,$78,$75,$73,$70,$6d,$6b
		!byte $68,$66,$63,$61,$5f,$5c,$5a,$58
		!byte $56,$53,$51,$4f,$4d,$4c,$4a,$48
		!byte $46,$45,$43,$41,$40,$3f,$3d,$3c
		!byte $3b,$3a,$39,$38,$37,$36,$35,$34
		!byte $34,$33,$33,$32,$32,$32,$32,$32

		!byte $32,$32,$32,$32,$32,$32,$33,$33
		!byte $34,$35,$35,$36,$37,$38,$39,$3a
		!byte $3b,$3c,$3e,$3f,$40,$42,$43,$45
		!byte $47,$48,$4a,$4c,$4e,$50,$52,$54
		!byte $56,$58,$5b,$5d,$5f,$62,$64,$67
		!byte $69,$6c,$6e,$71,$74,$76,$79,$7c
		!byte $7f,$81,$84,$87,$8a,$8d,$90,$93
		!byte $96,$99,$9c,$9f,$a2,$a5,$a8,$ab

		!byte $ad,$aa,$a7,$a4,$a1,$9e,$9b,$98
		!byte $95,$92,$8f,$8c,$89,$86,$83,$81
		!byte $7e,$7b,$78,$75,$73,$70,$6d,$6b
		!byte $68,$66,$63,$61,$5f,$5c,$5a,$58
		!byte $56,$53,$51,$4f,$4d,$4c,$4a,$48
		!byte $46,$45,$43,$41,$40,$3f,$3d,$3c
		!byte $3b,$3a,$39,$38,$37,$36,$35,$34
		!byte $34,$33,$33,$32,$32,$32,$32,$32

		!byte $32,$32,$32,$32,$32,$32,$33,$33
		!byte $34,$35,$35,$36,$37,$38,$39,$3a
		!byte $3b,$3c,$3e,$3f,$40,$42,$43,$45
		!byte $47,$48,$4a,$4c,$4e,$50,$52,$54
		!byte $56,$58,$5b,$5d,$5f,$62,$64,$67
		!byte $69,$6c,$6e,$71,$74,$76,$79,$7c
		!byte $7f,$81,$84,$87,$8a,$8d,$90,$93
		!byte $96,$99,$9c,$9f,$a2,$a5,$a8,$ab


sprite_x_pos	!byte $1e,$36,$1e,$36,$1e,$36,$1e,$36
		!byte $00

sprite_pulse	!byte $01,$07,$0f,$0a,$08,$02,$09,$00

; Text for the upper scrolling message - arranged into 38 character chunks
scroll_text_1	!scr "     c64cd sends out greetings to     "
		!scr "harlow cracking service    rob hubbard"
		!scr "  happy demomaker      stoat and tim  "
		!scr "    yak    the gps    ash and dave    "
		!scr "          pulse  productions          "
		!scr "                                      "

		!scr "      anti-greetings to c64hater      "
		!scr "                                      "

		!byte $00

; Text for the lower scrolling message - values from $81 to $88 set the
; scrolling speed
scroll_text_2	!scr $82,"here we go again with a new",$81,"c64cd",$82
		!scr "demo called..."
		!scr "    "

		!scr $81,"--- macro clone cafe ---"
		!scr "    "

		!scr $82,"a mash up of ash and dave's electric cafe and "
		!scr "macrojackmix by pulse productions."
		!scr "            "

		!scr "released at"
		!scr "    "

		!scr $81,"slipstock 2016"
		!scr "    "

		!scr $83,"code, graphics and sample wrangling by t.m.r using "
		!scr "a loop created especially for this release by odie "
		!scr "(ta very much, matey!)"
		!scr "            "

		!scr $84,"some quick technical gubbins...   "

!if sfx_sampler=1 {
		!scr "this is the 8 bit sample build; the data is stored that "
		!scr "way in memory and being played back on  the sfx sound "
		!scr "sampler's dac."
} else {
		!scr "this is the 4 bit sample build;  at start up the data goes "
		!scr "through a quick and dirty translation (well okay, four "
		!scr "lsr commands) from 8 bit and is then played on the volume "
		!scr "register."
}
		!scr "            "

		!scr "the sample is playing at around 6.8khz using an nmi interrupt "
		!scr "(in keeping with how electric cafe is doing things) and "
		!scr "the data is stored between 3000 and fc00 hex with the playback "
		!scr "routine switching out the i/o when reading.   the code, "
		!scr "graphics and associated data are all stored before that "
		!scr "point, but there's quite a bit of space left over despite "
		!scr "things not being particularly optimised!"
		!scr "            "

		!scr $83,"as ever i don't have much to say despite having left "
		!scr "quite a bit of free memory for scrolltext, but this thing "
		!scr "is being finished off with about an hour before the "
		!scr "competition deadline - after i'm done writing the code will "
		!scr "be assembled and crunched for a couple of final tests and "
		!scr "then bundled into a disk image ready for submission..."
		!scr "            "

		!scr "speaking of the competition, one thing i need to do is give "
		!scr "a cheery wave to everyone at ",$81,"slipstock",$83," since "
		!scr "some of them might actually be sober enough to see it - "
		!scr "behave you lot, the last thing we need is a chunk of the "
		!scr "uk demo scene arrested for being drunk and disorderly!"
		!scr "            "

		!scr $84,"and that's me pretty much done for this time;  enjoy "
		!scr "the sample loop, watch the sprites bounce around merrily "
		!scr "and this was t.m.r signing off on the 12th of march 2016 - "
		!scr "remember kids, don't try this at home without an "
		!scr "irresponsible adult!"
		!scr "                "

		!byte $00
