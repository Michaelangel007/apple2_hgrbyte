; ca65
.feature c_comments
.linecont +
.feature labels_without_colons
.feature leading_dot_in_identifiers
.PC02 ; 65C02

; HGR Byte Inspector
; Michael Pohoreski
; Version 16
; Ctrl-IJKL to move
; 0..9, A..F enter nibble into cursor byte
;
; Keys:
;
;   ESC   Quit
;   G     Toggle fullscreen
;
;   I     Move cursor up
;   J     Move cursor left
;   K     Move cursor right
;   L     Move cursor down
;
;   ^I    Move cursor to col 0
;   ^J    Move cursor to col 39
;   ^K    Move cursor to row 0
;   ^L    Move cursor to row 191
;   RET   Center cursor
;
;   0..9  "Append" hex nibble to cursor byte
;   A..F
;
;   !     Toggle bit 0
;   @     Toggle bit 1
;   #     Toggle bit 2
;   $     Toggle bit 3
;   %     Toggle bit 4
;   ^     Toggle bit 5
;   &     Toggle bit 6
;   *     Toggle bit 7 (high bit)
;   SPC   Toggle high bit of byte (bit 7)
;   (     Set   byte to $FF (Shift-9)
;   )     Clear byte to $00 (Shift-0)
;   `     Flip all bits
;   ~     Flip all bits
;
;   ,     Shift  byte left  (with zero)
;   .     Shift  byte right (with one )
;   <     Shift  byte left  (with zero)
;   >     Shift  byte right (with one )
;   [     Rotate byte left
;   ]     Rotate byte right
;
;   -     Save cursor byte to temporary
;   =     Set cursor byte from temporary


; Force APPLE 'text' to have high bit on
; Will display as NORMAL characters
.macro APPLE text
    .repeat .strlen(text), I
        .byte   .strat(text, I) | $80
    .endrep
.endmacro


; Force ASCII 'text' to be control chars: $00..$1F
; Will display as INVERSE characters
.macro CTRL text
    .repeat .strlen(text), I
        .byte   .strat(text, I) & $1F
    .endrep
.endmacro


; Force ASCII 'text' to be control chars: $00..$3F
; Will display as INVERSE characters
.macro INV text
    .repeat .strlen(text), I
        .byte   .strat(text, I) & $3F
    .endrep
.endmacro


        GBASL       = $26
        GBASH       = $27
        CH          = $24   ; text cursor column
        CV          = $25   ; text cursor row
        ROW_20      = $0550 ; VTAB 21 TEXT address
        ROW_21      = $06D0 ; VTAB 22 TEXT address
        ROW_22      = $0750 ; VTAB 23 TEXT address
        ROW_23      = $07D0 ; VTAB 24 TEXT address

        HPOSN       = $F411 ; A=row, Y,X=col update GBASL GBASH
        TABV        = $FB5B ; A=row,   ALL STA $25, JMP $FC22
        VTAB        = $FC22 ; $25=row  //e LDA $25, STA$28
        HOME        = $FC58
        COUT        = $FDED
        PR_HEX      = $FDD3
        PRBYTE      = $FDDA

        KEYBOARD    = $C000
        KEYSTROBE   = $C010
        TXTCLR      = $C050 ; Mode Graphics
        MIXCLR      = $C052 ; Full  screen
        MIXSET      = $C053 ; Split screen
        PAGE1       = $C054
        HIRES       = $C057 ; Mode HGR

        lastkey     = $F9
        cursor_row  = $E2   ; used by Applesoft HGR.row
        cursor_org  = $FA   ; When cursor is saved
        cursor_tmp  = $FB   ; Flashing cursor byte
        cursor_val  = $FC   ; Current byte under cursor
        flags       = $FD   ;
        temp        = $FE
        cursor_col  = $FF
        FLAG_FULL   = $01   ; == $1 -> $C052 MIXCLR
        ;                   ; == $0 -> $C053 MIXSET
        HGRPAGE     = $E6   ; used by Applesoft HGR.page

              __MAIN= $0900
        .word __MAIN         ; 2 byte BLOAD address
        .word __END - __MAIN ; 2 byte BLOAD size
        .org  __MAIN         ; .org must come after header else offsets are wrong

HgrByte:
        LDA #16             ; Version
        BIT PAGE1           ; Page 1
        BIT TXTCLR          ; not text, but graphics
        BIT MIXSET          ; Split screen text/graphics
        BIT HIRES           ; HGR, no GR

        LDX #0              ; also used by PrintFooter
        STX flags
        STX cursor_col
        STX cursor_row
        STX cursor_org
        STX GBASL
        LDA #$20
        STA GBASH
        STA HGRPAGE
PrintFooter:
        LDA TextFooter, X
        BEQ _Center         ; Default to center of screen
        STA ROW_21,X        ; 4 line text window, 2nd row
        INX
        BNE PrintFooter     ; (almost) always

; Funcs that JMP to GetByte before GotKey()
; Funcs that JMP to PutByte after  GotKey()
_Screen:                    ; Toggle mixed/full screen
        LDA flags           ; A = %????_????
        AND #FLAG_FULL      ; A = %0000_000f
        TAX                 ; f=0    f=1
        STA MIXCLR,X        ; C052   C053
        LDA flags           ; FULL   MIX
        EOR #FLAG_FULL      ; mode is based on old leading edge
        STA flags           ; not new trailing edge
        BRA FlashByte
_HighBit:
        EOR #$80
        BRA PutByte

_MoveL: DEC cursor_col
        BPL GetByte
_MoveR: INC cursor_col
        LDA cursor_col
        CMP #40
        BCC GetByte         ; intentional fall into _EdgeR
_EdgeR: LDA #39
        .byte $2C           ; BIT $ABS skip next instruction
_EdgeL: LDA #0
        STA cursor_col
        BPL GetByte         ; always

_MoveU: DEC cursor_row
        LDA cursor_row
        CMP #$FF
        BNE GetByte         ; INTENTIONAL fall into if Y < 0
_MoveD: INC cursor_row
        LDA cursor_row
        CMP #192
        BCC GetByte         ; intentional fall into
_EdgeD: LDA #$BF
        .byte $2C
_EdgeU: LDA #00
        STA cursor_row
        ADC #1
        BNE GetByte         ; always


_Center:
        LDA #40/2
        STA cursor_col
        LDA #192/2
        STA cursor_row      ; intentional fall into GetByte

GetByte:                    ; Cursor position changed
        LDY #0              ; Update pointer to screen
        LDX #0
        LDA cursor_row
        JSR HPOSN           ; A=row, Y,X=col X->E0 Y->E1
        JSR GetCursorByte
PutByte:
        STA cursor_val      ; current value
        STA cursor_tmp      ; flashing cursor
        JSR DrawStatus
FlashByte:
        JSR FlashCursorByte
GetKey:
        LDA KEYBOARD
        BPL FlashByte
        STA KEYSTROBE
        AND #$7F            ; Force ASCII
        STA lastkey
        CMP #'0'            ; key < '0'
        BCC @TestHex 
        CMP #'9'+1          ; key < '9'+1
        BCC _Nibble
@TestHex
        CMP #'A'
        BCC @LoadKeys
        CMP #'F'+1
        BCC _Nibble
@LoadKeys
        LDX #nKeys
FindKey:
        DEX
        BMI FlashByte
        CMP aKeys, X
        BNE FindKey
GotKey:
                            ; If code doesn't fit withing +/-127 bytes
                            ;        LDA aFuncHi, X
        LDA #>HgrByte       ; FuncAddressHi
        PHA
        LDA aFuncLo, X      ; FuncAddressLo
        PHA
        LDA cursor_val      ; Last displayed byte may be cursor_tmp
        JSR SetCursorByte   ; restore current val in case cursor moves
                            ; Also pre-load for ROL/R SHL/R
        LDX #0              ; for Toggle Bit #
_Exit:  RTS                 ; And call function assigned to key
                            ; To BRUN under DOS.3 change above RTS to
                            ;       RTS
                            ;_Exit: JMP $3D0 ; DOS 3.3 Warmstart vector

                            ; Functions starting with _ are invoked via keys
.out .sprintf( "_ByteFF: %X", * )
_ByteFF:
        LDA #$FF
        BNE PutByte         ; always
.out .sprintf( "_Byte00: %X", * )
_Byte00:
        LDA #$00
        BEQ PutByte         ; always
.out .sprintf( "_FlipBy: %X", * )
_FlipByte:
        LDX #$FF
        BNE TogBit
_Rol:   CMP #$80            ; C=a A=%abcd_efgh
_SHL1:  ROL                 ; C=b A=%bcde_fgha C<-bit7, bit0<-C
        BRA PutByte         ; force branch always

_SHR1:  ORA #$01            ; Force C=1 via ROR (SHR, OR #$80)
                            ; Intentional fall into _Ror
                            ; Using LSR instead of ROR to save a byte
                            ; 8 Byte version
                            ;   CLC
                            ;   ROR
                            ;   BCC PutByte
                            ;   ORA #$80
                            ;   BCS PutByte
_Ror:   LSR                 ; C=h A=%0abc_defg 0->bit7, bit0->C
        BCC PutByte
        ORA #$80
        BCS PutByte         ; always

_ShiftL:ASL                 ; C=a A=%bcde_fgh0
        .byte $A2           ; skip next LSR instruction; LDX dummy imm val
_ShiftR:LSR                 ; C=h A=%0abc_defg
        BRA PutByte

.out .sprintf( "_Bits# : %X", * )
_Bit7:  INX                 ; intentional fall into
_Bit6:  INX
_Bit5:  INX
_Bit4:  INX
_Bit3:  INX
_Bit2:  INX
_Bit1:  INX
_Bit0:  INX
        TAY             ; save cursor_val
        SEC             ;
        LDA #0          ; X=? A=0 C=1
_TogBit:ROL
        DEX
        BNE _TogBit
        TAX
        TYA             ; load cursor_val
; common _Bit# code
TogBit: STX temp
        EOR temp
GotoPutByte:
        BRA PutByte         ; code is too far to directly branch
.out .sprintf( "_Save  : %X", * )
_SaveByte:
        STA cursor_org      ; intentional fall into
_LoadByte:
.out .sprintf( "_Load  : %X", * )
        LDA cursor_org
        CLC
        BCC GotoPutByte     ; always
.out .sprintf( "_Nibble: %X", * )
_Nibble:
        LDY #3
@CursorSHL:
        ASL cursor_val
        DEY
        BPL @CursorSHL

        LDA lastkey
        CMP #$41
        BCC @Digit
        SBC #7              ; $41 - 6 - (C=1) = $3A
@Digit:
        AND #$0F            ; Key to Hex
        CLC
        ADC cursor_val
        BCC GotoPutByte     ; always

TextFooter:
    ;     "X=## Y=## $=####:## %%%%%%%%~%%%%%%%%"
    APPLE "            SAVE:?? 76543210 "
    INV                                "12345678" ;1-8 INVERSE
    .byte $00

DrawStatus:
        LDA #0              ; Cursor.X = 0
        STA CH
        LDA #20             ; Cursor.Y = Top of 4 line split TEXT/HGR window
        JSR TABV            ; update CV
PrintStatus:
        LDA #'X'+$80        ; X=## Y=## $=####:##
        JSR COUT
        LDA cursor_col
        JSR PR_HEX
        LDA #' '+$80
        JSR COUT
        LDA #'Y'+$80
        JSR COUT
        LDA cursor_row
        JSR PR_HEX
        LDA #' '+$80
        JSR COUT
        LDA #'$'+$80
        JSR COUT
        LDA GBASH
        JSR PR_HEX
        LDA GBASL
        CLC
        ADC cursor_col
        JSR PRBYTE
        LDA #':'+$80
        JSR COUT
        LDA cursor_val
        JSR PRBYTE
        LDA #' '+$80
        JSR COUT

ReverseByte:
        LDX #8
        LDA cursor_val
        STA temp
ReverseBit:
        ASL temp
        ROR
        DEX
        BNE ReverseBit
        STA temp

        PHA                 ; save reverse byte

        LDX #8
PrintBitsNormal:
        LDA temp
        JSR Bit2Asc
        ROR temp
        DEX
        BNE PrintBitsNormal

        LDA #'~'+$80
        JSR COUT

        LDX #8
        LDA cursor_val
PrintBitsReverse:
        TAY
        JSR Bit2Asc
        TYA
        LSR
        DEX
        BNE PrintBitsReverse

        LDA #'$'+$80
        JSR COUT
        PLA                 ; restore reverse byte
        JSR PRBYTE

        LDA cursor_org      ; X=0
        PHA
        LSR
        LSR
        LSR
        LSR
        JSR NibToInvTxt
        PLA
; Display nibble as inverse text
; 0 -> '0' $30
; 9 -> '9' $39
; A -> ^A  $01
; F -> ^F  $06
NibToInvTxt:
        AND #$F
        ORA #'0'+$00        ; ASCII: +$80
        CMP #'9'+$01        ; ASCII: +$81
        BCC PrintSave
        SBC #$39            ; C=1, $3A ':' -> $01 'A', $3F '?' -> $06
PrintSave:
        STA ROW_21+17,X
        INX
        RTS

Bit2Asc:
        AND #$01            ; 0 -> B0
        BEQ FlipBit
        LDA #$81            ; 1 -> 31
FlipBit:
        EOR #$B0
        JMP COUT

; A = byte
GetCursorByte:
        LDY cursor_col
        LDA (GBASL),Y
        STA cursor_val
SetCursorByte:
        LDY cursor_col
        STA (GBASL),Y
        RTS

FlashCursorByte:
        LDA cursor_tmp
        EOR #$FF
        STA cursor_tmp
        JMP SetCursorByte

; Keys are searched in reverse order
; Sorted by least used to most used
aKeys:
        CTRL  "["           ; _Exit     ESC  Quit
        .byte "G"           ; _Screen   G    Toggle fullscreen

        CTRL  "H"           ; _MoveL    <-   Ctrl-H $08
        CTRL  "U"           ; _MoveR    ->   Ctrl-U $15

        CTRL  "I"           ; _EdgeU    ^I   Ctrl-I $09 Move cursor to row 0
        CTRL  "J"           ; _EdgeL    ^J   Ctrl-J $0A Move cursor to col 0
        CTRL  "K"           ; _EdgeD    ^K   Ctrl-K $0B Move cursor to row 191
        CTRL  "L"           ; _EdgeR    ^L   Ctrl-L $0C Move cursor to col 39
        CTRL  "M"           ; _Center   Center cursor

        .byte "I"           ; _moveU    Move cursor Up
        .byte "J"           ; _moveL    Move cursor Left
        .byte "K"           ; _MoveD    Move cursor Down
        .byte "L"           ; _MoveR    Move cursor Right

        .byte "!"           ; _Bit0     Toggle bit 0
        .byte "@"           ; _Bit1     Toggle bit 1
        .byte "#"           ; _Bit2     Toggle bit 2
        .byte "$"           ; _Bit3     Toggle bit 3
        .byte "%"           ; _Bit4     Toggle bit 4
        .byte "^"           ; _Bit5     Toggle bit 5
        .byte "&"           ; _Bit6     Toggle bit 6
        .byte "*"           ; _Bit7     Toggle bit 7

        .byte " "           ; _HighBit  Toggle high bit of byte (bit 7)
        .byte "("           ; _ByteFF   Set byte to FF (Shift-9)
        .byte ")"           ; _Byte00   Set byte to 00 (Shift-0)
        .byte "`"           ; _FlipByte Toggle all bits
        .byte "~"           ; _FlipByte

        .byte ","           ; _ShiftL (with zero)
        .byte "<"           ; _ShiftL (with one )
        .byte "."           ; _ShiftR (with zero)
        .byte ">"           ; _ShiftR (with one )
        .byte "["           ; _Rol
        .byte "]"           ; _Ror

        .byte "-"           ; _SaveByte Save cursor byte
        .byte "="           ; _LoadByte Load cursor byte
eKeys:
nKeys   = eKeys - aKeys     ;

; Table of function pointers
; *Note*: Must match aKeys order!
aFuncLo:
        .byte <_Exit    -1  ; ESC
        .byte <_Screen  -1  ; G

        .byte <_MoveL   -1  ; <-
        .byte <_MoveR   -1  ; ->

        .byte <_EdgeU   -1  ; ^I
        .byte <_EdgeL   -1  ; ^J
        .byte <_EdgeD   -1  ; ^K
        .byte <_EdgeR   -1  ; ^L
        .byte <_Center  -1  ; RET

        .byte <_MoveU   -1  ; 'I'
        .byte <_MoveL   -1  ; 'J'
        .byte <_MoveD   -1  ; 'K'
        .byte <_MoveR   -1  ; 'L'

        .byte <_Bit0    -1  ; '1'
        .byte <_Bit1    -1  ; '2'
        .byte <_Bit2    -1  ; '3'
        .byte <_Bit3    -1  ; '4'
        .byte <_Bit4    -1  ; '5'
        .byte <_Bit5    -1  ; '6'
        .byte <_Bit6    -1  ; '7'
        .byte <_Bit7    -1  ; '8'

        .byte <_HighBit -1  ; SPC
        .byte <_ByteFF  -1  ; '('
        .byte <_Byte00  -1  ; ')'
        .byte <_FlipByte-1  ; '`'
        .byte <_FlipByte-1  ; '~' not a dup mistake

        .byte <_ShiftL  -1  ; `,`
        .byte <_SHL1    -1  ; `<`
        .byte <_ShiftR  -1  ; `.`
        .byte <_SHR1    -1  ; `>`
        .byte <_Rol     -1  ; '['
        .byte <_Ror     -1  ; ']

        .byte <_SaveByte-1  ; '='
        .byte <_LoadByte-1  ; '-'
__END:

