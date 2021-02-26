;load_init =$CBFE
;load_init =$CC00
load_init =$06fe
;
l_load = [[.len loader]+2]

.proc	loader,load_init
;BAFER = $0700
bafer	= pfin+11
BUFAUX = $0600
?RUTINA = BUFAUX+JUMP-RUTINA
;
	;ORG $CC00
	.by $55,$55
    ldy #$00
    STY $0244
    INY
    STY $09
    JSR RECUPERO
    JMP START
?INICIO
	JSR GBYTE
	STA MEMORY+1
	JSR GBYTE
	STA MEMORY+2
	AND MEMORY+1
	CMP #$FF
	BEQ ?INICIO
	RTS
NBYTES
    .BY 252   ;$FC
FLAGY
    .BY 0
FINISH
    .BY 0,0
pfalso
	.by 0
LSIOV
    .BY $60,$00,$52,$40
    .WO BAFER
    .BY $23,$00
    .WO $0100
    .BY $00,$80
DLIST
    .BY $70,$70,$70,$42
    .WO MENSAJE
    .BY $70,$70,$70,$70,$70,$06
    .by $70,$70,$70,$70,$70,$70
    .by $06,$70,$06,$70,$70,$70
    .BY $70,$42
DLERR
    .WO DERROR
    .BY $41
    .WO DLIST
MENSAJE
	.SB " PROGRAMAS PARA COMPUTADORES ATARI 2021 "
TITLO
    .SB "                    "
;PRIMER TITULO
TITGE
	.SB "                    "
NAME
    .SB "  NOMBRE DEL JUEGO  "
DERROR
    .SB "     Cargara dentro de "
CONTADOR
    .SB "    Bloques.     "
MERR
    .SB " Un momento porfavor, buscando respaldo "
TIEMPO
    LDA #$40
    STA $D40E
    LDX #$E4
    LDY #$5F
    LDA #$06
    JSR $E45C
    RTS
LNEW
    LDX #$04
XNEW
    LDA $02C4,X
    STA PFIN+1,X
    DEX
    BPL XNEW
    LDA $0230
    STA PFIN+6
    LDA $0231
    STA PFIN+7
    LDA $022F
    STA PFIN+8
    LDA $02F4
    STA PFIN+9
    LDA $02F3
    STA PFIN+10
    RTS
NEWL
    LDX #$04
YNEW
    LDA PFIN+1,X
    STA $02C4,X
    DEX
    BPL YNEW
    LDA PFIN+6
    STA $0230
    LDA PFIN+7
    STA $0231
    LDA PFIN+8
    STA $022F
    LDA PFIN+9
    STA $02F4
    LDA PFIN+10
    STA $02F3
    RTS
NEWDL
    LDX # <DLIST
    LDA # >DLIST
    STX $0230
    STX $D402
    STA $0231
    STA $D403
    lda #$02
    sta $d018
	sta $d01a
    LDA #$22
    STA $022F
    STA $D400
    LDA #$E0
    STA $02F4
    STA $D409
    LDA #$02
    STA $02F3
    STA $D401
;    LDX #$04
;COLORLOOP
;    LDA TABLA,X
;    STA $02C4,X
;    STA $D016,X
;    DEX
;    BPL COLORLOOP
;    LDX #<COLORDAT
;    LDY #>COLORDAT
;    LDA #$06
;    JSR $E45C
    
;    LDX #$DB
;    LDY #$CD
;    LDA #$C0
;    STX $0200
;    STY $0201
;    STA $D40E
    RTS
;COLORDAT
;    LDA #$00
;    STA NOSEPO
;    LDA $02C6
;    STA NOSEPO2
;    JMP $E45F
;?COLORDAT
;    PHA
;    TXA
;    PHA
;    LDX NOSEPO
;    LDA NOSEPO1,X
;    STA $D40A
;    STA $D01A
;    INC NOSEPO
;    PLA
;    TAX
;    PLA
;    RTI
;NOSEPO
;    .BY $00
;NOSEPO1
;    .BY $52,$72,$B4,$EA,$32
;NOSEPO2
;    .BY $FF,$FF
;TABLA
;    .BY $28,$CA,$00,$44,$00
ERROR
    LDX #<MERR		;$F3
    LDA #>MERR		;$CC
    STX DLERR		;$CC4E
    STA DLERR+1		;$CC4F
    JSR VUELTA		;$CE0E
	JMP GRAB
VUELTA
    LDA #$3C
    STA $D302
    CLC
    LDA $14
    ADC #$1E
SEARCH
	CMP $14
	BNE SEARCH
    LDA #$34
    STA $D302
    CLC
    LDA $14
    ADC #$14
SPEED
    CMP $14
    BNE SPEED
SIGUE
    LDX #$80
    STX $14
BUSCA
    LDA $D20A
    STA $D017
    LDA $D20F
    AND #$10
    BEQ SIGUE
    LDX $14
    BNE BUSCA
    JMP NEWDL
GBYTE
    CPY NBYTES
    BEQ GRAB
    TYA
    EOR BAFER+3,Y
    INY
    RTS
GRAB
    LDA $D40B
    BNE GRAB
    LDA PFIN
	beq serabye
;    BEQ BYE
    JSR LNEW
    JSR NEWDL
?GRAB
    LDX #$0B
MSIO
    LDA LSIOV,X
    STA $0300,X
    DEX
    BPL MSIO
    JSR $E459
    BMI ERROR
    LDA BAFER+2
    CMP PFIN
    BCC ERROR
    BEQ RETURN
    JMP ?GRAB
RETURN
	LDX #<DERROR	;$CB
    LDA #>DERROR	;$CC
    STX DLERR		;$CC4E
    STA DLERR+1		;$CC4F
    LDA BAFER+255
    STA NBYTES
    LDX #$02
C01
    LDA CONTADOR,X
    CMP #$10
    BNE C02
    LDA #$19
    STA CONTADOR,X
    DEX
    BPL C01
C02
    DEC CONTADOR,X
    JSR NEWL
    DEC PFIN
    LDY #$00
    STY $4D
    JMP GBYTE
serabye
	lda pfalso
	cmp #$00
	beq bye
	dec pfalso
	jmp ?grab
BYE
    JSR TIEMPO
    LDA #$4C
    STA RUTINA+10		;$CF46
    LDA #$E0
    STA JUMP+1			;$CF50
    LDX #$00
    TXS
    JMP RINIT			;$CF2E
START
    LDY NBYTES
    JSR ?INICIO
    LDA MEMORY+1
    LDY MEMORY+2
    STA $02E0
?START
	STY $02E1
	LDY #$00
LOOP
	JSR ?INICIO
    JSR GBYTE
    STA FINISH
    JSR GBYTE
    STA FINISH+1
MBTM
    JSR GBYTE
MEMORY
    STA $FFFF
    LDA MEMORY+1
    CMP FINISH
    BNE OK
    LDA MEMORY+2
    CMP FINISH+1
    BEQ VERFIN
OK
    INC MEMORY+1
    BNE NIM
    INC MEMORY+2
NIM
    JMP MBTM
VERFIN
    LDA $02E2
    ORA $02E3
    BEQ LOOP
    LDX #$F0
    TXS
    STY FLAGY
    JSR TIEMPO
    JSR NEWL
    JSR RINIT
    JSR LNEW
    JSR VUELTA
    LDY FLAGY
    LDX #$00
    TXS
    STX $02E2
    STX $02E3
    JMP LOOP
RINIT
    LDX #PFIN-RUTINA-1
MVRUT
    LDA RUTINA,X
    STA BUFAUX,X
    DEX
    BPL MVRUT
    JMP BUFAUX
RUTINA
    LDA #$3C
    STA $D302
    LDA #$FF
    STA $D301
?RUTINA
	JSR BUFAUX+19	;0813
	LDA #$FE
	STA $D301
    RTS
JUMP
    JMP ($02E2)
PFIN
    .BY $00,$00,$00,$00,$00,$00
    .BY $00,$00,$00,$00,$00
RECUPERO
	LDX #<BUFAUX	;$00
	LDY #>BUFAUX	;$08
	STX $0C
	STY $0D
	LDX $CFFC
	LDY $CFFD
	STX $0230
	STY $0231
    JSR LNEW
    LDX #$13
RECUPERO2
;RECUPERO EL TITULO
    LDA FINRECUPERO,X
    STA NAME,X
    DEX
    BPL RECUPERO2
    LDX #$02
RECUPERO3
;RECUPERO LOS BLOQUES EN ATASCII
    LDA FINRECUPERO+20,X
    STA CONTADOR,X
    DEX
    BPL RECUPERO3
    lda finrecupero+24		;recupero bytes falso para 256k
    sta pfalso
    LDX #$03
    LDA FINRECUPERO+23		;RECUPERO LOS BLOQUES EN BYTE
    STX $41
    STA PFIN
    LDY #$7F
    LDA #$00
RECUPERO4
    STA $0400,Y
    DEY
    BPL RECUPERO4
    JSR NEWDL
    JMP NEWL
FINRECUPERO		;$CFA4 $CFBB
NME
    .BY '....................'
BLQ
    .BYTE '...'
.endp