kkem
    SEI
    LDA $D40E
    PHA
    lda #$00
    sta $D40E
    tay
kloop
    LDA $C000,Y
    DEC $D301
    STA $C000,Y
    INC $D301
    INY
    BNE kloop
    INC kloop+2
    inc kloop+8
kmove
	lda kloop+2   
    BEQ @kexit
    CMP #$D0
    BNE kloop
    LDA #$D8
    STA kloop+2
    sta kloop+8
    BNE kloop
@kexit
    PLA
    STA $D40E
    CLI
    dec $d301
    RTS