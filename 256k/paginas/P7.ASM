//	icl "../base/sys_equates.m65"


l_pag7=[.len pagina7]

;.proc pagina7,$0700
.proc pagina7,$2000
kem_buffer =$b000
INICIO
	.by 0
	.by [l_pag7+127]/128
	.wo INICIO,FIN7
	jsr KEM7
	ldx $0230
	ldy $0231
	stx $CFFC
	sty $CFFD
	ldx #<DLS7
	ldy #>DLS7
	lda #$00
	stx $0230
	stx $D402
	sty $0231
	sty $d403
	;sta $41
	jsr INJK7
	ldx #$90
	ldy #$00
	jsr PAUSA7
	jmp $0400
KEM7        
	ldx #$C0
	ldy #$00
	sei
	sty $D40E
	stx $CC
	sty $CB
LOOP7       
	lda ($CB),Y
	dec $D301
	sta ($CB),Y
	inc $D301
	iny
	bne LOOP7
	inc $CC
	beq EXIT7
	lda $CC
	cmp #$D0
	bne LOOP7
	lda #$D8
	sta $CC
	bne LOOP7
EXIT7       
	dec $D301
	cli
	sec
	lsr $D40E
	rts
INJK7       
	ldx #$17
?INJK7      
	lda FINAL7,X
	sta $ED3D,X
	dex
	bpl ?INJK7
	lda #$12
	sta $EBE2
	lda #$60
	sta $FDFC
	lda #$04
	sta $2A
	lda #$80
	sta $2B
	lda #$0C
	sta $02FC
	ldx $62
	lda #$01
	sta $FE93,X
	lda #$00
	sta $FE91,X
	ldx #$10
	jmp $FCE6
FINAL7      
	.by $8F,$06,$D2,$8F,$31,$00
	.by $8F,$0F,$D2,$18,$EB,$27
	.by $AA,$8F,$04,$D2,$AF,$32
	.by $02,$8F,$0F,$D2,$18,$60
DLS7        
	.by $70,$70,$70,$70,$70,$70
	.by $70,$70,$70,$70,$70,$70
	.by $70,$46
	.wo show7
	.by $70,$06,$41
	.wo dls7
SHOW7
TITLOP7
	.sb '--aca va el titulo--'
TITLO2P7
	.sb '--aca va el titulo--'
PAUSA7      
	stx $021C
	sty $021D
?PAUSA7     
	lda $021C
	ora $021D
	bne ?PAUSA7
	rts
FIN7        
	jmp ($0304)
	.by 'programas para computadores'
	.by ' atari xe - xl, 2021 by dog'
	.by 'dark & parche negro soft - '
	.by 'CHILE, sistemas injektor. w'
	.by 'ww.dogdark.cl'
.endp