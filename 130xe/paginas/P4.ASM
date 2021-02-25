//	icl "../base/sys_equates.m65"

;
pagina4_init=$03FD
//	org pagina4_init
.proc pagina4, pagina4_init
GENDAT = $47
	.BYTE $55,$55,$FE
INICIO
	lda $D301
	and #$02
	beq ERROR4
	ldx #$0B
MOVE4
	lda SIOV4,X
	sta $0300,X
	dex
	bpl MOVE4
	jsr $E459
	bmi ERROR4
	clc
	rts
ERROR4      
	ldx $CFFC
	ldy $CFFD
	stx $0230
	sty $0231
	ldy #$00
LER4        
	lda TABLA4,Y
	sta ($58),Y
	iny
	cpy #$1B
	bne LER4
	lda #$3C
	sta $D302
LOOP4       
	bne LOOP4
TABLA4      
	.sb +128,'ERROR'
	.SB ' ¡¡¡ CA'
	.sb 'RGUE N'
	.sb 'UEVAME'
	.sb 'NTE'
SIOV4       
	.by $60,$00,$52,$40
	.wo $CC00
	.by $23,$00
	;.wo $03bc		
	.wo l_load
	.by $00,$80
FIN
.endp