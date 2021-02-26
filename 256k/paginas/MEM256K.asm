; SAVE #D:MEM256K.ASM
;RECONOCEDOR DE BANCOS
;BY VITOCO
;El acceso a la memoria extendida del 130XE y del 800XL modificado
;se realiza a través de bancos de memoria de 16K cada uno, accesibles
;a través de una zona fija de memoria ($4000-$7FFF) que es reemplazada
;por el banco. Para activar un banco específico, se debe modificar
;algunos bits en el registro de hardware denominado PORTB (dirección
;de memoria 54017). La combinación de bits requerida para cada banco
;es la siguiente:
;       | POKE  | Banco real | D=0	 V=0 E=0		 B=0 R=1
; Banco | 54017 | 130XE 256K |  7   6   5   4   3   2   1   0
;-------|-------|------------|---------------------------------
;   0   |  177  |  RAM   RAM |  1   0   1   1   0   0   0   1
;   1   |  161  |   0     4  |  1   0   1   0   0   0   0   1
;   2   |  165  |   1     5  |  1   0   1   0   0   1   0   1
;   3   |  169  |   2     6  |  1   0   1   0   1   0   0   1
;   4   |  173  |   3     7  |  1   0   1   0   1   1   0   1
;   5   |  193  |         8  |  1   1   0   0   0   0   0   1
;   6   |  197  |         9  |  1   1   0   0   0   1   0   1
;   7   |  201  |        10  |  1   1   0   0   1   0   0   1
;   8   |  205  |        11  |  1   1   0   0   1   1   0   1
;   9   |  225  |        12  |  1   1   1   0   0   0   0   1
;  10   |  229  |        13  |  1   1   1   0   0   1   0   1
;  11   |  233  |        14  |  1   1   1   0   1   0   0   1
;  12   |  237  |        15  |  1   1   1   0   1   1   0   1
;-------|-------|------------|---------------------------------
;  13   |  129  |         0  |  1   0   0   0   0   0   0   1
;  14   |  133  |         1  |  1   0   0   0   0   1   0   1
;  15   |  137  |         2  |  1   0   0   0   1   0   0   1
;  16   |  141  |         3  |  1   0   0   0   1   1   0   1
;
MAX = 16
BANKOS
	.WORD MAX
MEMORY
	.BYTE $00,$00,$00
B	
;		  ;$B1,$A1,$A5,$A9,$AD
;	.BYTE 177,$161,165,169,173
;		  ;$C1,$C5,$C9,$CD,$E1
;	.BYTE 193,197,201,205,225
;		  ;$E5,$E9,$ED,$81,$85
;	.BYTE 229,233,237,129,133
		  ;$89,$8D
;	.BYTE 137,141
;
;
	.BYTE $B2
	.BYTE $A2,$A6,$AA,$AE
	.BYTE $C2,$C6,$CA,$CE
	.BYTE $E2,$E6,$EA,$EE
	.BYTE $82,$86,$8A,$8E
LIMPIO.MEMORY
	LDA #$00
	STA MEMORY
	STA MEMORY+1
	STA MEMORY+2
	RTS
MEMORIA
	LDY #MAX
BUSCO1
	LDA B,Y
	STA portb
	STA 22222
	DEY 
	BPL BUSCO1
	LDY #1
BUSCO2
	LDA B,Y
	STA portb
	CMP 22222
	BNE DISTINTO
	INY
	CPY #MAX+1
	BNE BUSCO2
DISTINTO
	LDA B
	STA portb
	STY BANKOS
	JSR LIMPIO.MEMORY
	LDX BANKOS
	DEX
DISTINTO2
;SACO CALCULO DE MEMORIA
;DISPONIBLE SEGUN BANCOS
;ENCONTRADOS
	CLC
	LDA MEMORY
	ADC #$00
	STA MEMORY
	LDA MEMORY+1
	ADC #$40
	STA MEMORY+1
	LDA MEMORY+2
	ADC #$00
	STA MEMORY+2
	DEX 
	BPL DISTINTO2
	RTS