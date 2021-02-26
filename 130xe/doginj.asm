;	OPT NO LIST
;
; GRABADOR SISTEMA injektor con irg
;
;
	icl 'base/sys_equates.m65'
@LEN =  LEN+2
@LBAF = LEN+4
PPILA = LEN+5
PCRSR = $CB
ORG =   PCRSR
SVMSC = $58
POSXY = $54
LENGHT = $4000
BAFER = $4000
FR0 =   $D4
CIX =   $F2
AFP =   $D800
IFP =   $D9AA
FPI =   $D9D2
FASC =  $D8E6
ZFR0 =  $DA44
FDIV =  $DB28
FMUL =  $DADB
FMOVE = $DDB6
INBUFF = $F3
LBUFF = $0580
LLOAD = PAG7-LOAD
LAUTO = PAG4-PAG7
BL4 =   LAUTO/128
LAST =  LAUTO-128*BL4
GENDAT = $47
;
	org $2000
	icl 'paginas/kem.asm'
	icl 'paginas/injektor.asm'
LOAD	
	icl "paginas/PLOADER.ASM"
TITLO	= [[loader.titlo - loader] + load]
TITGE	= [[loader.titge - loader] + load]
NME
    .BY '....................'
BLQ
    .BYTE '...'
PFIN
	.BY 0
PAG7
	icl "paginas/p7.asm"
TITLOP7		= [[pagina7.TITLOP7 - pagina7] + pag7]	
TITLO2P7	= [[pagina7.TITLO2P7 - pagina7] + pag7]
PAG4
	icl "paginas/p4.asm"
; -------------------------
; DEFINICION DEL DISPLAY
; PARA DIRECTORIO
; -------------------------
?DIR
    .BYTE $70,$70,$70,$70,$70,$70,$70,$70
    .BYTE $46
    .WORD ???DIR
    .BYTE $70,$02,$02,$02,$02,$02,$02,$02
    .BYTE $02,$02,$41
    .WORD ?DIR
DLS
:3	.BY $70
    .by $46
    .WO SHOW
    .by $70
:10	.by $02
:3	.by $70
	.by $06,$70,$06
	.by $41
    .WO DLS
SHOW
:20	.sb " "
	.sb +32,"QRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRE"
	.sb "|  DOGDARK INJEKTOR 130XE V1.1 - 2021  |"
	.sb +32,"ARRRRRRRRRRRRRRRWRRRRRRRRRRRRRRRRRRRRRRD"
	.sb "|TITULO GENERAL | "
GENERAL
	.sb "******************** |"
	.sb "|TITULO JUEGO   | "
CRSR
	.sb "******************** |"
	.sb "|FUENTE         | "
FILE
	.sb "******************** |"
	.sb "|BYTES LEIDOS   | "
BYTES
	.sb "*****                |"
	.sb "|BLOQUES        | "
BLOQUES
	.sb "***                  |"
	.sb "|COPIAS         | "
TOTALCOPIAS
	.sb "*                    |"
	.sb +32,"ZRRRRRRRRRRRRRRRXRRRRRRRRRRRRRRRRRRRRRRC"
NAMEG
	.sb "--aca va el titulo--"
NAME
	.sb "--aca va el titulo--"   
???DIR
    .SB "     DIRECTORIO     "
??DIR
:10	.SB "                                        "
RY
	.BYTE 0,0
LEN
    .BYTE 0,0
CONT
    .BYTE 0,0
STARTF
    .BYTE 0,0
FINISH
    .BYTE 0,0
@BL4
    .BYTE 0
?FILE
    .BYTE 'D:'
??FILE
    .BYTE '                    '
BBLQS
    .by '000',$9B
ALL
    .BYTE 'D:*.*',$9B
DNHP
    .BYTE $60,$00,$50,$80
    .WORD ??DIR
    .BYTE $35,$00,$00,$01,$00,$80
MODIFICOROM
	;46
	.BY $12,$12,$B6,$C1,$B9,$B6
	.BY $B3,$C4,$BD,$12,$3B,$40
	.BY $3C,$37,$3D,$46,$41,$44
	.BY $12,$12
BAKBYT
    .SB "00000"
BAKBLQ
    .SB "000"
;************************************************
;ANTIPIRATEO
;************************************************
ROMCEANDO
	LDX #19
ROMCEANDO1
	CLC
	LDA MODIFICOROM,X
	ADC #46
	STA SHOW,X
	STA TITLO,X
	sta TITLOP7,x
	DEX
	BPL ROMCEANDO1
	RTS
PIRATEO
	LDA SHOW+10
	CMP #$73
	BEQ PIRATEO2
	JMP PIRERROR
PIRATEO2
	LDA SHOW+4
	CMP #$E4
	BEQ PIRATEO3
	JMP PIRERROR
PIRATEO3
	RTS
PDLS
	.BYTE $70,$70,$70,$42
	.WORD SHP
	.BYTE $41
	.WORD PDLS
SHP 
	.SB "ERROR.....                              "
PIRERROR
	LDX # <PDLS
	LDY # >PDLS
	STX $230
	STY $231
PIRLOOP
	JMP PIRLOOP
RESTORE
    LDY #$13
?RESTORE
    LDA #$20
    STA ??FILE,Y
    LDA #$00
    STA NAME,Y
    sta NAMEG,y
    STA FILE,Y
    STA TITLO,Y
    sta TITLOP7,y
    sta TITLO2P7,y
    STA CRSR,Y
    STA FILE,Y
    sta general,y
	DEY
    BPL ?RESTORE
    LDA #$3F
    STA CRSR
    STA FILE
    sta general
    LDA #$10
    sta TOTALCOPIAS
    LDY #$04
RESNUM
    STA BYTES,Y
    DEY
    BPL RESNUM
    STA BLOQUES
    STA BLOQUES+1
    STA BLOQUES+2
    LDA #$FF
    STA $D301
    RTS
ASCINT
    CMP #$20
    BCC ADD64
    CMP #$60
    BCC SUB32
    CMP #$80
    BCC REMAIN
    CMP #$A0
    BCC ADD64
    CMP #$E0
    BCC SUB32
    BCS REMAIN
ADD64
    CLC
    ADC #$40
    BCC REMAIN
SUB32
    SEC
    SBC #$20
REMAIN
    RTS
SETCOPIAS
	JSR OPENK
?SETCOPIAS
	LDX #<?COPIAS
	LDY #>?COPIAS
	LDA #$10
	STX $0228
	STY $0229
	STA $021A
	LDX #$10
	JSR $E456
	PHA
	LDA #$00
	STA $021A
	LDA TOTALCOPIAS
	AND #$7F
	STA TOTALCOPIAS
	PLA
	CMP #$2D
	BEQ MCOPIAS
	CMP #$3D
	BEQ ?MCOPIAS
	CMP #$9B
	BNE ?SETCOPIAS
	JMP CLOSE
MCOPIAS
	LDA TOTALCOPIAS
	CMP #$19
	BEQ ?SETCOPIAS
	INC TOTALCOPIAS
	BNE ?SETCOPIAS
?MCOPIAS
	LDA TOTALCOPIAS
	CMP #$10
	BEQ ?SETCOPIAS
	DEC TOTALCOPIAS
	BNE ?SETCOPIAS
?COPIAS
	LDA TOTALCOPIAS
	EOR #$80
	STA TOTALCOPIAS
	LDA #$10
	STA $021A
	RTS
CLS
    LDX # <??DIR
    LDY # >??DIR
    STX PCRSR
    STY PCRSR+1
    LDY #$00
    LDX #$00
?CLS
    LDA #$00
    STA (PCRSR),Y
    INY
    BNE ??CLS
    INX
    INC PCRSR+1
??CLS
    CPY #$68	;$68
    BNE ?CLS
    CPX #$01
    BNE ?CLS
    RTS
OPEN
    LDX #$10
    LDA #$03
    STA $0342,X
    LDA # <??FILE
    STA $0344,X
    LDA # >??FILE
    STA $0345,X
    LDA #$04
    STA $034A,X
    LDA #$80
    STA $034B,X
    JSR $E456
    DEY
    BNE DIR
    RTS
CLOSE
    LDX #$10
    LDA #$0C
    STA $0342,X
    JMP $E456
DIR
    JSR CLOSE
    JSR CLS
    LDX # <?DIR
    LDY # >?DIR
    STX $0230
    STY $0231
    LDX # <??DIR
    LDY # >??DIR
    STX PCRSR
    STY PCRSR+1
    LDX #$10
    LDA #$03
    STA $0342,X
    LDA # <ALL
    STA $0344,X
    LDA # >ALL
    STA $0345,X
    LDA #$06
    STA $034A,X
    LDA #$00
    STA $034B,X
    JSR $E456
    LDA #$07
    STA $0342,X
    LDA #$00
    STA $0348,X
    STA $0349,X
    STA RY
    STA RY+1
LEDIR
    JSR $E456
    BMI ?EXIT
    CMP #$9b
    BEQ EXIT
    JSR ASCINT
    LDY RY
    STA (PCRSR),Y
    INC RY
    BNE F0
    INC PCRSR+1
    INC RY+1
F0
    LDY RY+1
    CPY #$01
    BNE F1
    LDY RY
    CPY #$68	;$68
    BCC F1
    JSR PAUSE
    INC RY
F1
    JMP LEDIR
EXIT
    INC RY
    INC RY
    INC RY
    JMP LEDIR
?EXIT
    JSR CLOSE
    JSR PAUSE
    JSR CLS
    PLA
    PLA
    JMP START
PAUSE
    LDA $d01f
    CMP #$06
    BNE PAUSE
    JSR CLS
    LDA #$00
    STA RY
    STA RY+1
    LDA # <??DIR
    STA PCRSR
    LDA # >??DIR
    STA PCRSR+1
    LDX #$10
    RTS
FLSH
    LDY RY
    LDA (PCRSR),Y
    EOR #$3f
    STA (PCRSR),Y
    LDA #$10
    STA $021A
    RTS
OPENK
    LDA #$ff
    STA $02fc
    LDX #$10
    LDA #$03
    STA $0342,X
    STA $0345,X
    LDA #$26
    STA $0344,X
    LDA #$04
    STA $034A,X
    JSR $E456
    LDA #$07
    STA $0342,X
    LDA #$00
    STA $0348,X
    STA $0349,X
    STA RY
    RTS
RUTLEE
    LDX # <FLSH
    LDY # >FLSH
    LDA #$10
    STX $0228
    STY $0229
    STA $021A
    JSR OPENK
GETEC
    JSR $E456
    CMP #$7E
    BNE C0
    LDY RY
    BEQ GETEC
    LDA #$00
    STA (PCRSR),Y
    LDA #$3f		;$3F
    DEY
    STA (PCRSR),Y
    DEC RY
    JMP GETEC
C0
    CMP #$9b	;$9B
    BEQ C2
    JSR ASCINT
    LDY RY
    STA (PCRSR),Y
    CPY #$14		;#14
    BEQ C1
    INC RY
C1
    JMP GETEC
C2
    JSR CLOSE
    LDA #$00
    STA $021A
    LDY RY
    STA (PCRSR),Y
    RTS
FGET
    LDA #$DF
    STA $D301
    LDA #$00
    STA LEN
    STA LEN+1
LOPFGET
    LDX #$10
    LDA #$07
    STA $0342,X
    LDA # <BAFER
    STA $0344,X
    LDA # >BAFER
    STA $0345,X
    LDA # <LENGHT
    STA $0348,X
    LDA # >LENGHT
    STA $0349,X
??FGET
    JSR $E456
    CLC
    LDA LEN
    ADC $0348,X
    STA LEN
    LDA LEN+1
    ADC $0349,X
    STA LEN+1
    CLC
    LDA $D301
    ADC #$04
    STA $D301
    LDA $0349,X
    CMP # >LENGHT
    BEQ LOPFGET
    CPY #$88	;$88
    BEQ ?FGET
    JSR CLOSE
    JSR CLS
    LDX #$00
    TXS
    JMP START
?FGET
    JSR ZFR0
    LDA #$fc	;$FC
    STA FR0
    JSR IFP
    JSR FMOVE
    LDA LEN
    STA FR0
    LDA LEN+1
    STA FR0+1
    JSR IFP
    JSR PONBYTES
    JSR FDIV
    JSR PONBLOQUES
    JSR FPI
    LDA FR0
    PHA
    DEC FR0
    JSR IFP
    JSR FMOVE
    LDA #$fc	;$FC
    STA FR0
    LDA #$00
    STA FR0+1
    JSR IFP
    JSR FMUL
    JSR FPI
    SEC
    LDA LEN
    SBC FR0
    STA CONT+1
    INC CONT+1
    PLA
    STA CONT
    LDX #$10
    RTS
PONBYTES
    JSR NBYTES
    STY RY
    LDY #$04
?PONBYTES
    LDA LBUFF,X
    AND #$5F
    STA BYTES,Y
    DEY
    DEX
    DEC RY
    BPL ?PONBYTES
    RTS
PONBLOQUES
    JSR NBYTES
    STY RY
    LDY #$02
?PONBLOQUES
    LDA LBUFF,X
    AND #$5F
    STA BLOQUES,Y
    DEY
    DEX
    DEC RY
    BPL ?PONBLOQUES
    LDA BLOQUES+2
    CMP #$19
    BEQ ??PP0
    INC BLOQUES+2
PP0
    LDY #$02
MVBLQ
    LDA BLOQUES,Y
    ORA #$20
    STA BBLQS,Y
    DEY
    BPL MVBLQ
    LDX # <BBLQS
    LDY # >BBLQS
    LDA #$00
    STX INBUFF
    STY INBUFF+1
    STA CIX
    JMP AFP
??PP0
    LDA #$10
    STA BLOQUES+2
    LDA BLOQUES+1
    CMP #$19
    BEQ ???PP0
    INC BLOQUES+1
    JMP PP0
???PP0
    LDA #$10
    STA BLOQUES+1
    INC BLOQUES
    JMP PP0
NBYTES
    JSR FASC
    LDX #$00
    LDY #$00
    LDA LBUFF
    CMP #$30	;'0
    BNE PL0
    INX
PL0
    LDA LBUFF,X
    CMP #$80
    BCS PL1
    CMP #$2E		;'.
    BEQ PL2
    INX
    INY
    JMP PL0
PL1
    RTS
PL2
	DEX
    LDA LBUFF,X
    ORA #$80
    STA LBUFF,X
    DEY
    RTS
OPENC
	LDA $D40B
    BNE OPENC
    LDA #$FE
    STA $d301		;$02FC
	jsr preinj
	jmp ?piratas
PONDATA
    LDA BLOQUES
    STA BLQ
    LDA BLOQUES+1
    STA BLQ+1
    LDA BLOQUES+2
    STA BLQ+2
    LDY #$13
?PONDATA
	LDA NAME,Y
	STA NME,Y
	DEY
	BPL ?PONDATA   
    RTS
INITSIOV
    LDY #$0B
?INITSIOV
    LDA DNHP,Y
    STA $0300,Y
    DEY
    BPL ?INITSIOV
    LDA #$00
    STA $4d		;$4D
    RTS
AUTORUN
    LDX # <PAG7
    LDY # >PAG7
    LDA #$02
    STX MVPG7+1
    STY MVPG7+2
    STA @BL4
FALTA
    JSR INITSIOV
    LDX #<??DIR
    LDY #>??DIR
    STX $0304
    STY $0305
    LDX #$83	; $83
    LDY #$00	; $00
    STX $0308
    STY $0309
    LDY #$00
    TYA
CLBUF
    STA ??DIR,Y
    INY
    CPY #$83	;$83
    BNE CLBUF
    LDA #$55
    STA ??DIR
    STA ??DIR+1
    LDX #$FC
    LDY #$7f	;$7F
    DEC @BL4
    BPL NOFIN
    LDX #$FA
    LDY #LAST
    STY ??DIR+130
NOFIN
    STX ??DIR+2
MVPG7
    LDA PAG7,Y
    STA ??DIR+3,Y
    DEY
    BPL MVPG7
    JSR $E459
    ldx #$10
    ldy #$00
    jsr time
    CLC
    LDA MVPG7+1
    ADC #$80
    STA MVPG7+1
    LDA MVPG7+2
    ADC #$00
    STA MVPG7+2
    LDA @BL4
    BPL FALTA
    RTS
GAUTO
    JSR AUTORUN
    JSR INITSIOV
    LDX #$83
    LDY #$00
?GAUTO
    STX $0308
    STY $0309
    LDX # <PAG4
    LDY # >PAG4
    STX $0304
    STY $0305
    JSR $E459
    jsr injektor
    JSR INITSIOV
;    LDX #$f0
;    ldy #$00
;    jsr time
    ldx #<lload		;$bc
    ldy #>lload		;$03
    STX $0308
    STY $0309
    LDX # <LOAD
    LDY # >LOAD
    STX $0304
    STY $0305
    JSR $E459
    ldx #$10
    ldy #$00
    jsr time
    RTS
time
	stx $021c
	sty $021d
?time
	lda $021c
	ora $021d
	bne ?time
	rts
REST
    LDY #$04
??REST
    LDA BYTES,Y
    STA BAKBYT,Y
    DEY
    BPL ??REST
    LDY #$02
???REST
    LDA BLOQUES,Y
    STA BAKBLQ,Y
    DEY
    BPL ???REST
    RTS
?REST
    LDY #$04
????REST
    LDA BAKBYT,Y
    STA BYTES,Y
    DEY
    BPL ????REST
    LDY #$02
?????REST
    LDA BAKBLQ,Y
    STA BLOQUES,Y
    DEY
    BPL ?????REST
    LDA CONT
    STA PFIN
    RTS
EXNHPUT
    PLA
    PLA
    PLA
    PLA
    RTS
NHPUT
    LDA #$55
    STA ??DIR
    STA ??DIR+1
    LDA #$fc	;$FC
    STA ??DIR+255
    lda #$de
    sta $d301
    LDX # <BAFER
    LDY # >BAFER
    STX M+1
    STY M+2
    LDX #$00
    LDY #$00
    STY $02E2
    JSR GRABACION
    JMP ?MVBF
GRABACION
    LDA PFIN
    STA ??DIR+2
    BEQ EXNHPUT
    CMP #$01
    BNE RETURN
    LDA CONT+1
    STA ??DIR+255
RETURN
    RTS
?MVBF
    JSR GBYTE
    STA STARTF
    JSR GBYTE
    STA STARTF+1
    AND STARTF
    CMP #$FF
    BEQ ?MVBF
    JSR GBYTE
    STA FINISH
    JSR GBYTE
    STA FINISH+1
NHLOP
    JSR GBYTE
    LDA STARTF
    CMP #$E3
    BNE ?NHLOP
    LDA STARTF+1
    CMP #$02
    BNE ?NHLOP
    STA $02E2
?NHLOP
    LDA STARTF
    CMP FINISH
    BNE NHCONT
    LDA STARTF+1
    CMP FINISH+1
    BEQ ?MVBF
NHCONT
    INC STARTF
    BNE NOHI
    INC STARTF+1
NOHI
    JMP NHLOP
GBYTE
	CPY ??DIR+255
    BEQ EGRAB
    TYA
M
    EOR BAFER,X
    STA ??DIR+3,Y
    TYA
    EOR ??DIR+3,Y
    INY
    INX
    BNE EXNHPIT
    INC M+2
    BPL EXNHPIT
    PHA
    CLC
    LDA $D301
    ADC #$04
    STA $D301
    LDA # >BAFER
    STA M+2
    PLA
EXNHPIT
    RTS
EGRAB
    DEC PFIN
    TXA
    PHA
    JSR INITSIOV
    JSR $E459
    LDX #$10
    ldy #$00
    jsr time
    ldx #$02
DECBL01
    LDA BLOQUES,X
    CMP #$10
    BNE DECBL02
    LDA #$19
    STA BLOQUES,X
    DEX
    BPL DECBL01
DECBL02
    DEC BLOQUES,X
    PLA
    TAX
	LDA $02E2
    BNE SLOWB
SIGUE
    JSR GRABACION
    LDY #$00
    JMP GBYTE
SLOWB
    TXA
    PHA
    LDX #$5e	;$015e
    LDY #$01
    STX $021C
    STY $021D
IRG
    LDA $021D
    BNE IRG
    LDA $021C
    BNE IRG
    LDA #$00
    STA $02E2
    PLA
    TAX
    JMP SIGUE
DOS
    JMP ($0C)
@START
	JSR DOS
START
    LDX # <DLS
    LDY # >DLS
    STX $0230
    STY $0231
    LDA #$02
    sta 710
    sta 712
    JSR RESTORE
    jsr ROMCEANDO
;ingreso titulo general
	LDX # <general
    LDY # >general
    STX PCRSR
    STY PCRSR+1
    JSR RUTLEE
    TYA
    BEQ NOTITLEG
    LSR 
    STA RY+1
    LDA #$0a
    SEC
    SBC RY+1
    STA RY+1
    LDX #$00
    LDY RY+1
WRITEG
    LDA GENERAL,X
	STA NAMEG,Y
	STA TITGE,Y
    INY
    INX
    CPX RY
    BNE WRITEG
NOTITLEG
;ingreso titulo juego
    LDX # <CRSR
    LDY # >CRSR
    STX PCRSR
    STY PCRSR+1
    JSR RUTLEE
    TYA
    BEQ NOTITLE
    LSR 
    STA RY+1
    LDA #$0a
    SEC
    SBC RY+1
    STA RY+1
    LDX #$00
    LDY RY+1
WRITE
    LDA CRSR,X
    STA NAME,Y
    sta TITLO2P7,y
    INY
    INX
    CPX RY
    BNE WRITE
NOTITLE
    LDX # <FILE
    LDY # >FILE
    STX PCRSR
    STY PCRSR+1
    JSR RUTLEE
    LDY #$13
CONV
    LDA FILE,Y
    BEQ ?REMAIN
    AND #$7F
    CMP #$40
    BCC ADD32
    CMP #$60
    BCC SUB64
    BCS ?REMAIN
ADD32
    CLC
    ADC #32
    BCC OKLET
SUB64
    SEC
    SBC #64
?REMAIN
    LDA #$9B
OKLET
    STA ??FILE,Y
    DEY
    BPL CONV
    JSR OPEN
    JSR FGET
    JSR CLOSE
    JSR PONDATA
    JSR REST
OTRACOPIA
    JSR SETCOPIAS
    JSR ?REST
    JSR OPENC
    JSR GAUTO
?OTRACOPIA
	JSR ?REST
    JSR NHPUT
    LDX #$fe
    LDY #$80
    stx $13
    sty $14
??otracopia
	lda $13
	ora $14
	bne ??otracopia
	lda totalcopias
	and #$0f
	beq ???otracopia
	dec totalcopias
	bpl ?OTRACOPIA
???otracopia
	ldx #$3c
	ldy #$80
	lda #$03
	sta $d20f
	stx $d302
	;sty $4d
WAIT
    LDA $d01f
    CMP #$07
    BEQ WAIT
    CMP #$06
    BEQ OTRACOPIA
    CMP #$03
    BNE WAIT
    JMP START
PIRATAS
    JSR CLOSE
    LDX # <OPENK
    LDY # >OPENK
	jsr close
    JSR kkem
    jsr ?piratas
    jmp start
?piratas
    LDX # <@START
    LDY # >@START
    LDA #$03
    STX $02
    STY $03
    STA $09
    LDY #$FF
    STY $08
    INY   
    STY $0244
    rts

	RUN PIRATAS