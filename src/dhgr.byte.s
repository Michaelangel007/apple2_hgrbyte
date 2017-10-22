; Merlin32

CONFIG_DHGR = 1

; DHGR Byte Inspector
; https://github.com/Michaelangel007/apple2_hgrbyte/
;
; Michael Pohoreski
; Version 21
;
; TL:DR;
;   IJKL to move
;   Ctrl-IJKL to move to edges
;   0..9, A..F enter nibble into cursor byte
;
; Keys:
;
;   ESC   Quit
;   G     Toggle fullscreen
;
;   i     Move cursor up
;   j     Move cursor left
;   k     Move cursor right
;   l     Move cursor down
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
;
; TODO:
;   Z = clear aux mem
;   X = swap aux/main mem
;   M = mark begin/end -- show width,height,x,y
;
; These aren't needed
;   S = save
;   L = load

CH          = $24   ; 40-col text cursor column
CV          = $25   ; 40-col text cursor row
CH80        = $57B  ; 80-col text cursor column

GBASL       = $26   ; 16-bit pointer to start of D/HGR scanline
GBASH       = $27

    DO CONFIG_DHGR
WIDTH       = 80    ;
    ELSE
WIDTH       = 40    ; HGR
    FIN

HPOSN       = $F411 ; A=row, Y,X=col update GBASL GBASH
TABV        = $FB5B ; A=row,   ALL STA $25, JMP $FC22

HOME        = $FC58
SETTXT      = $FB39

; In 80-col mode
;   CWSL = $36 -> C307 Output
;   KSWL = $38 -> C305 Input
;COUT80      = $C307
;CR          = $FC62
;CROUT       = $FD8E ; Output CR

COUT        = $FDED ; Char Output aka putch()
PR_HEX      = $FDD3
PRBYTE      = $FDDA

; When Store80 ON, Page 2=ON, read/writes touch aux mem
SW_STORE80  = $C000 ; STA $+0 OFF, $+1 ON
SW_STORE81  = $C001 ; $C055 = page 2

SW_SET40COL = $C00C ; 40-col mode
SW_SET80COL = $C00D ; 80-col mode

SW_AUXRDOFF = $C002 ; $+0 = Read  Main $0200..$BFFF, $+1 = Read  Aux $0200..$BFFF
SW_AUXWROFF = $C004 ; $+0 = Write Main $0200..$BFFF, $+1 = Write Aux $0200..$BFFF

SW_ALTCHAR0 = $C00E ; Mouse text off

KEYBOARD    = $C000 ; LDA
KEYSTROBE   = $C010
TXTCLR      = $C050 ; Mode Graphics
MIXCLR      = $C052 ; Full  screen
MIXSET      = $C053 ; Split screen
PAGE1       = $C054
HIRES       = $C057 ; Mode HGR
SW_DHGR     = $C05E ; Mode DHGR
SW_SHGR     = $C05F ; Mode SHGR

cursor_row  = $E2   ; used by Applesoft HGR.row
aux_ptr     = $F7   ; copy of GBAS but points to HGR2 $40xx..$5Fxx
lastkey     = $F9
cursor_org  = $FA   ; When cursor is saved
cursor_tmp  = $FB   ; Flashing cursor byte
cursor_val  = $FC   ; Current byte under cursor

FLAG_FULL   = $01   ; == $1 -> $C052 MIXCLR
;                   ; == $0 -> $C053 MIXSET
flags       = $FD   ;

temp        = $FE
cursor_col  = $FF   ; x2 for DHGR
HGRPAGE     = $E6   ; used by Applesoft HGR.page
                    ; $20=MAIN, $40=AUX

MOV_SRC     = $003C ; A1L
MOV_END     = $003E ; A2L
MOV_DST     = $0042 ; A4L
AUXMOVE     = $C311 ; C=0 Aux->Main, C=1 Main->Aux
MOVE        = $FE2C ; Main<->Main, *MUST* set Y=0 prior!

__MAIN = $900

        ORG __MAIN

DhgrByte
        LDA #21             ; Version - copy HGR1 to aux, HGR2 to HGR1
        JSR Init_Exit       ; FEATURE: Set to 00 if you don't want to copy AUX $2000 to MAIN $4000

        BIT PAGE1           ; Page 1
        BIT TXTCLR          ; not text, but graphics
        BIT MIXSET          ; Split screen text/graphics
        BIT HIRES           ; HGR, no GR

    DO CONFIG_DHGR
        STA SW_DHGR         ; $C05E DHGR, not HGR
        STA SW_SET80COL     ; $C00D 80-col on
        STA SW_STORE80      ; Turn off for app
    FIN

        BRA _Center

; Funcs that JMP to GetByte before GotKey()
; Funcs that JMP to PutByte after  GotKey()
_Screen                     ; Toggle mixed/full screen
        LDA flags           ; A = %????_????
        AND #FLAG_FULL      ; A = %0000_000f
        TAX                 ; f=0    f=1
        STA MIXCLR,X        ; C052   C053
        LDA flags           ; FULL   MIX
        EOR #FLAG_FULL      ; mode is based on old leading edge
        STA flags           ; not new trailing edge
        BRA FlashByte
_HighBit
        EOR #$80
        BRA PutByte

_MoveL  DEC cursor_col
        BPL GetByte
_MoveR  INC cursor_col
        LDA cursor_col
        CMP #WIDTH
        BCC GetByte         ; intentional fall into _EdgeR
_EdgeR  LDA #WIDTH-1
        DB  $2C             ; BIT $ABS skip next instruction
_EdgeL  LDA #0
        STA cursor_col
        BPL GetByte         ; always

_MoveU  DEC cursor_row
        LDA cursor_row
        CMP #$FF
        BNE GetByte         ; INTENTIONAL fall into if Y < 0
_MoveD  INC cursor_row
        LDA cursor_row
        CMP #192
        BCC GetByte         ; intentional fall into
_EdgeD  LDA #$BF
        DB  $2C             ; BIT $ABS skip next instruction
_EdgeU  LDA #00
        STA cursor_row
        ADC #1
        BNE GetByte         ; always


_Center
        LDA #WIDTH/2
        STA cursor_col
        LDA #192/2
        STA cursor_row      ; intentional fall into GetByte

GetByte                     ; Cursor position changed
        LDY #0              ; Update pointer to screen
        LDX #0
        LDA cursor_row
        JSR HPOSN           ; A=row, Y,X=col X->E0 Y->E1

    DO CONFIG_DHGR
; NOTE $4000 is our shadow copy of AUX ram
        LDA GBASL
        STA aux_ptr+0
        LDA GBASH
        AND #$1F
        ORA #$40
        STA aux_ptr+1
    FIN

        JSR GetCursorByte
PutByte
        STA cursor_val      ; current value
        STA cursor_tmp      ; flashing cursor
        JSR DrawStatus
FlashByte
        JSR FlashCursorByte
GetKey
        LDA KEYBOARD
        BPL FlashByte
        STA KEYSTROBE
        AND #$7F            ; Force ASCII
        STA lastkey
        CMP #'0'            ; key < '0'
        BCC TestHex
        CMP #'9'+1          ; key < '9'+1
        BCC Nibble
TestHex
        CMP #'A'
        BCC LoadKeys
        CMP #'F'+1
        BCC Nibble
LoadKeys
        LDX #nKeys
FindKey
        DEX
        BMI FlashByte
        CMP aKeys, X
        BNE FindKey
GotKey
                            ; If code doesn't fit within +/-127 bytes
                            ;        LDA aFuncHi, X
        LDA #>__MAIN        ; FuncAddressHi
        PHA
        LDA aFuncLo, X      ; FuncAddressLo
        PHA
        LDA cursor_val      ; Last displayed byte may be cursor_tmp
        JSR SetCursorByte   ; restore current val in case cursor moves
                            ; Also pre-load for ROL/R SHL/R
        LDX #0              ; for Toggle Bit #
        RTS                 ; And call function assigned to key
                            ; To BRUN under DOS.3 change above RTS to
                            ;       RTS
                            ;_Exit JMP $3D0 ; DOS 3.3 Warmstart vector

                            ; Functions starting with _ are invoked via keys
_Exit
        LDA #$80            ; "Reverse" current DHGR mem config so it can be saved
        JMP Init_Exit       ; Exit to TEXT80
_ByteFF
        LDA #$FF
        BNE PutByte         ; always
_Byte00
        LDA #$00
        BEQ PutByte         ; always
_FlipByte
        LDX #$FF
        BNE PutBits
_Rol    CMP #$80            ; C=a A=%abcd_efgh
_SHL1   ROL                 ; C=b A=%bcde_fgha C<-bit7, bit0<-C
        BRA PutByte         ; force branch always

_SHR1   ORA #$01            ; Force C=1 via ROR (SHR, OR #$80)
                            ; Intentional fall into _Ror
                            ; Using LSR instead of ROR to save a byte
                            ; 8 Byte version
                            ;   CLC
                            ;   ROR
                            ;   BCC PutByte
                            ;   ORA #$80
                            ;   BCS PutByte
_Ror    LSR                 ; C=h A=%0abc_defg 0->bit7, bit0->C
        BCC PutByte
        ORA #$80
        BCS PutByte         ; always

_ShiftL ASL                 ; C=a A=%bcde_fgh0
        DB  $A2             ; skip next LSR instruction; LDX dummy imm val
_ShiftR LSR                 ; C=h A=%0abc_defg
        BRA PutByte

_Bit7   INX                 ; intentional fall into
_Bit6   INX
_Bit5   INX
_Bit4   INX
_Bit3   INX
_Bit2   INX
_Bit1   INX
_Bit0   INX
        TAY             ; save cursor_val
        SEC             ;
        LDA #0          ; X=? A=0 C=1
TogBit  ROL
        DEX
        BNE TogBit
        TAX
        TYA             ; load cursor_val
; common _Bit# code
PutBits STX temp
        EOR temp
GotoPutByte
        BRA PutByte         ; code is too far to directly branch

_SaveByte
        STA cursor_org      ; intentional fall into
_LoadByte

; *******************
; NOTE: Above code must fit on 1 page!
; IF _LoadByte overflows the initial page then
;     LDA aFuncLo PHA RTS
; will have unpredictable results!
; ********************
        ERR *-1/$9FF        ; Merlin: Error if PC > $9FF

        LDA cursor_org
        CLC
        BCC GotoPutByte     ; always
Nibble
        LDY #3
CursorSHL
        ASL cursor_val
        DEY
        BPL CursorSHL

        LDA lastkey
        CMP #$41
        BCC Digit
        SBC #7              ; $41 - 6 - (C=1) = $3A
Digit
        AND #$0F            ; Key to Hex
        CLC
        ADC cursor_val
        BCC GotoPutByte     ; always

; ------------------------------------------------------------------------
DrawStatus
        LDA #20             ; Cursor.Y = Top of 4 line split TEXT/HGR window
        JSR VTAB_COL0       ; Cursor.X = 0

        LDA #'X'+$80        ; X=## Y=## $=####:##
        JSR COUT

    DO CONFIG_DHGR
        LDY #0              ; src = &char[0][8]
        LDA cursor_col
        CLC
        ROR
        BCS HaveMainMem
HaveAuxMem
        LDY #8              ; src = &char[1][8]
HaveMainMem

        LDX #0              ; dst = 0
CopyMemType
        LDA sMemType,Y
        STA sTextFooter,X
        INY                 ; src++
        INX                 ; dst++
        CPX #8
        BNE CopyMemType
    FIN

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
        LDA cursor_col
    DO CONFIG_DHGR
        LSR                 ; Aux/Main is interleaved
    FIN
        CLC
        ADC GBASL
        JSR PRBYTE
        LDA #':'+$80
        JSR COUT
        LDA cursor_val
        JSR PRBYTE
        LDA #' '+$80
        JSR COUT

ReverseByte
        LDX #8
        LDA cursor_val
        STA temp
ReverseBit
        ASL temp
        ROR
        DEX
        BNE ReverseBit
        STA temp

        PHA                 ; save reverse byte

        LDX #8
PrintBitsNormal
        LDA temp
        JSR Bit2Asc
        ROR temp
        DEX
        BNE PrintBitsNormal

        LDA #'~'+$80
        JSR COUT

        LDX #8
        LDA cursor_val
PrintBitsReverse
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
        JSR NibToInvTxt

        LDA #21             ; Cursor.Y = 21
        JSR VTAB_COL0

        LDX #0
PrintFooter1
        LDA sTextFooter, X
        BEQ _DoneStatus
        JSR COUT            ; 4 line text window, 2nd row
        INX
        BNE PrintFooter1    ; (almost) always
_DoneStatus

; Draw pixel grouping
        LDA #22
        JSR VTAB_COL0
        LDA #29
        JSR HTAB

; (x & 4) * 8
        LDA cursor_col      ; src = sPixelFooter
        AND #3
        ASL
        ASL
        ASL
        TAX                 ; src += 8*col
        LDY #8              ; char [4][8]
PrintFooter2
        LDA sPixelFooter,X
        JSR COUT
        INX
        DEY                 ; len--
        BNE PrintFooter2
        RTS

; Display nibble as inverse text
; 0 -> '0' $30
; 9 -> '9' $39
; A -> ^A  $01
; F -> ^F  $06
NibToInvTxt
        AND #$F
        ORA #'0'+$00        ; ASCII: +$80
        CMP #'9'+$01        ; ASCII: +$81
        BCC PrintSave
        SBC #$39            ; C=1, $3A ':' -> $01 'A', $3F '?' -> $06
PrintSave
        STA sTextFooter+17,X ; VTAB 21:HTAB 17 and 18, Update Saved Byte
        INX
        RTS

Bit2Asc
        AND #$01            ; 0 -> B0
        BEQ FlipBit
        LDA #$81            ; 1 -> 31
FlipBit
        EOR #$B0
        JMP COUT

; OUT:
;   A = byte
; USES:
;   GBAS    = HGR Page 1 ptr
;   aux_ptr = HGR Page 2 AUX shadow copy
GetCursorByte               ; Main Aux

    ; Col & 1
    ;   Even = Read Aux
    ;   Odd  = Read Main
    DO CONFIG_DHGR
        LDA cursor_col
        CLC
        ROR                 ; Aux/Main is interleaved
        TAY                 ; y = byte column
        BCS _get_main
_get_aux
        STA SW_STORE80
        LDA (aux_ptr),Y
        DB $2C              ; OPTIMIZAITON: BRA _get_val -> BIT $abs - BIT $26B1
_get_main
    ELSE
        LDY cursor_col
    FIN
        LDA (GBASL),Y
_get_val
        STA cursor_val
        RTS

FlashCursorByte
        LDA cursor_tmp
        EOR #$FF
        STA cursor_tmp
; OPTIMIZATION: Intentionally fall-into SetCursorByte to save 3-byte JMP

; In:
;   A = byte
SetCursorByte
    ; Col & 1
    ;   Even = Read Aux
    ;   Odd  = Read Main
    DO CONFIG_DHGR
        TAX                 ; push byte

        LDA cursor_col
        CLC
        ROR
        TAY
        TXA                 ; pop byte
        BCS _set_main

        STA SW_STORE80
        STA (aux_ptr),Y     ; Shadow copy to HGR2
        STA SW_AUXWROFF+1   ; Write AUX
_set_main
    ELSE
        LDY cursor_col
    FIN
        STA (GBASL),Y       ; Write to AUX or MAIN
        STA SW_AUXWROFF     ; Write MAIN
        RTS



; ========================================================================
; Common code -- called by Init, Exit
Init_Exit
        STA SW_SET80COL

    DO CONFIG_DHGR
        STA SW_AUXRDOFF
        STA SW_AUXWROFF

        STA SW_STORE80
        STA SW_ALTCHAR0 ; Turn off mouse-text

        LDX #$20
        LDY #$40

; Q. Why is this needed?
; A. After the program has initially run memory is in "reverse" order
;      MAIN = 2000
;      AUX  = 2000
;    In order to allow the user to BSAVE linear memory we need to end up:
;       MAIN 2000 -> MAIN 4000
;       AUX  2000 -> MAIN 2000
;
; A=pos Convert DHGR format: MAIN 2000->AUX  2000, MAIN 4000->MAIN 2000
; A=neg Reverse DHGR format: MAIN 2000->MAIN 4000, AUX  2000->MAIN 2000
        PHA
        PLA             ; Force flags to be updated
        BMI OnExit

; HGR12 to AUX/MAIN
OnInit
        ; Copy MAIN $2000 -> AUX $2000
        STX MOV_SRC+1   ; Src Hi
        STY MOV_END+1   ; End Hi
        STX MOV_DST+1   ; Dst Hi
        CLC             ; C=0 don't do MOVE
        JSR SetDst00
        SEC             ; C=1 Main to Aux
        JSR AUXMOVE

        ; Copy MAIN $4000 -> MAIN $2000
        LDA #$60
        STY MOV_SRC+1   ; Src Hi
        STA MOV_END+1   ; End Hi
        STX MOV_DST+1   ; Dst Hi
        SEC             ; C=1 also do MOVE
        JSR SetDst00

        ; Copy AUX $2000 -> MAIN $4000
        LDY #$40
        STX MOV_SRC+1   ; Src Hi
        STY MOV_END+1   ; End Hi
        STY MOV_DST+1   ; Dst Hi
        CLC             ; C=0 Aux to Main
        JSR SetDst00    ; C=0 don't do MOVE
        JSR AUXMOVE
        BRA DoneCopy

; "Unlinearize" interleaved AUX/MAIN memory
; so it can be saved in a linear format
OnExit

        ; Copy MAIN $2000 -> MAIN $4000
        STX MOV_SRC+1   ; Src Hi
        STY MOV_END+1   ; End Hi
        STY MOV_DST+1   ; Dst Hi
        SEC             ; C=1 also do MOVE
        JSR SetDst00

        ; Copy AUX $2000 -> MAIN $2000
        LDY #$40
        STX MOV_SRC+1   ; Src Hi
        STY MOV_END+1   ; End Hi
        STX MOV_DST+1   ; Dst Hi
        CLC             ; C=0 Aux to Main
        JSR SetDst00
        JSR AUXMOVE

        STA SW_SET40COL ; $C00C
        STA SW_DHGR+1   ; $C05F DHGR off
        STA SW_STORE81  ; Enable Page 2 switching
DoneCopy
    FIN

        JSR SETTXT
        JSR HOME

        LDX #$20
        STX HGRPAGE

        LDA #0              ; also used by PrintFooter
        STA flags
        STA cursor_col
        STA cursor_row
        STA cursor_org

; OPTIMIZATION: Intentionally fall into HTAB

; ------------------------------------------------------------------------
; Move cursor to row=A,col=0
; A = Row
VTAB_COL0
        JSR TABV            ; update CV
HTAB00
        LDA #0
HTAB
        STA CH
        STA CH80

        RTS

; ------------------------------------------------------------------------
; Set src,dst,end low byte pointers to zero
; C=1 Also do MOVE
SetDst00
        STZ MOV_SRC+0   ; Src Lo
        STZ MOV_DST+0   ; Dst Hi
        STZ MOV_END+0   ; End Lo
        BCC DoneDst00
        LDY #0
        JMP MOVE
DoneDst00
        RTS


; ========================================================================
sTextFooter
    ;                1         2         3
    ;      0123456789012345678901234567890123456789
    ;     "X=## Y=## $=####:## %%%%%%%%~%%%%%%%%$00"
    ASC   "---/----    SAVE:?? 76543210 "
    INV                                "12345678" ;1-8 INVERSE
;    DB '1' & $3F
;    DB '2' & $3F
;    DB '3' & $3F
;    DB '4' & $3F
;    DB '5' & $3F
;    DB '6' & $3F
;    DB '7' & $3F
;    DB '8' & $3F
    DB $00

; char [2][8]
sMemType
    ASC "AUX/----"
    ASC "---/MAIN"

; These alternative text groupings
; don't look as good in DHGR 80-col text mode
;   "()"
;   "<>"
;   "{}"
;   "[]
; The brackets look the best
;
; char [4][8]
;        12345678123456781234567812345678
; Show which pixel group the bits belong to
sPixelFooter
    ASC "[11]222 "
    ASC         "2[33]44 "
    ASC                 "44[55]6 "
    ASC                         "666[77] "


; ------------------------------------------------------------------------
; Keys are searched in reverse order
; Sorted by least used to most used
aKeys
        DB    '[' & $1F     ; _Exit     ESC  Quit
        ASC   'g'           ; _Screen   G    Toggle fullscreen
        ASC   'G'           ; _Screen   G    Toggle fullscreen

        DB    'H' & $1F     ; _MoveL    <-   Ctrl-H $08
        DB    'U' & $1F     ; _MoveR    ->   Ctrl-U $15

        DB    'I' & $1F     ; _EdgeU    ^I   Ctrl-I $09 Move cursor to row 0
        DB    'J' & $1F     ; _EdgeL    ^J   Ctrl-J $0A Move cursor to col 0
        DB    'K' & $1F     ; _EdgeD    ^K   Ctrl-K $0B Move cursor to row 191
        DB    "L" & $1F     ; _EdgeR    ^L   Ctrl-L $0C Move cursor to col 39
        DB    "M" & $1F     ; _Center   Center cursor

        ASC   'i'           ; _moveU    Move cursor Up
        ASC   'j'           ; _moveL    Move cursor Left
        ASC   'k'           ; _MoveD    Move cursor Down
        ASC   'l'           ; _MoveR    Move cursor Right

        ASC   'I'           ; _moveU    Move cursor Up
        ASC   'J'           ; _moveL    Move cursor Left
        ASC   'K'           ; _MoveD    Move cursor Down
        ASC   'L'           ; _MoveR    Move cursor Right

        ASC   '!'           ; _Bit0     Toggle bit 0
        ASC   '@'           ; _Bit1     Toggle bit 1
        ASC   '#'           ; _Bit2     Toggle bit 2
        ASC   '$'           ; _Bit3     Toggle bit 3
        ASC   '%'           ; _Bit4     Toggle bit 4
        ASC   '^'           ; _Bit5     Toggle bit 5
        ASC   '&'           ; _Bit6     Toggle bit 6
        ASC   '*'           ; _Bit7     Toggle bit 7

    DO CONFIG_DHGR
        ASC   ' '           ; _HighBit  Toggle high bit of byte (bit 7) is useless in DHGR mode
    ELSE
        ASC   ' '           ; _HighBit  Toggle high bit of byte (bit 7)
    FIN
        ASC   '('           ; _ByteFF   Set byte to FF (Shift-9)
        ASC   ')'           ; _Byte00   Set byte to 00 (Shift-0)
        ASC   '`'           ; _FlipByte Toggle all bits
        ASC   '~'           ; _FlipByte

        ASC   ','           ; _ShiftL (with zero)
        ASC   '<'           ; _ShiftL (with one )
        ASC   '.'           ; _ShiftR (with zero)
        ASC   '>'           ; _ShiftR (with one )
        ASC   '['           ; _Rol
        ASC   ']'           ; _Ror

        ASC   '-'           ; _SaveByte Save cursor byte
        ASC   '='           ; _LoadByte Load cursor byte
eKeys
nKeys   = eKeys - aKeys     ;

; Table of function pointers
; *Note*: Must match aKeys order!
aFuncLo
        DB    <_Exit    -1  ; ESC
        DB    <_Screen  -1  ; g
        DB    <_Screen  -1  ; G

        DB    <_MoveL   -1  ; <-
        DB    <_MoveR   -1  ; ->

        DB    <_EdgeU   -1  ; ^I
        DB    <_EdgeL   -1  ; ^J
        DB    <_EdgeD   -1  ; ^K
        DB    <_EdgeR   -1  ; ^L
        DB    <_Center  -1  ; RET

        DB    <_MoveU   -1  ; 'i'
        DB    <_MoveL   -1  ; 'j'
        DB    <_MoveD   -1  ; 'k'
        DB    <_MoveR   -1  ; 'l'

        DB    <_MoveU   -1  ; 'I'
        DB    <_MoveL   -1  ; 'J'
        DB    <_MoveD   -1  ; 'K'
        DB    <_MoveR   -1  ; 'L'

        DB    <_Bit0    -1  ; '1'
        DB    <_Bit1    -1  ; '2'
        DB    <_Bit2    -1  ; '3'
        DB    <_Bit3    -1  ; '4'
        DB    <_Bit4    -1  ; '5'
        DB    <_Bit5    -1  ; '6'
        DB    <_Bit6    -1  ; '7'
        DB    <_Bit7    -1  ; '8'

    DO CONFIG_DHGR
        DB    <_HighBit -1  ; SPC
    ELSE
        DB    <_HighBit -1  ; SPC
    FIN
        DB    <_ByteFF  -1  ; '('
        DB    <_Byte00  -1  ; ')'
        DB    <_FlipByte-1  ; '`'
        DB    <_FlipByte-1  ; '~' not a dup mistake

        DB    <_ShiftL  -1  ; `,`
        DB    <_SHL1    -1  ; `<`
        DB    <_ShiftR  -1  ; `.`
        DB    <_SHR1    -1  ; `>`
        DB    <_Rol     -1  ; '['
        DB    <_Ror     -1  ; ']

        DB    <_SaveByte-1  ; '='
        DB    <_LoadByte-1  ; '-'
__END   ; Needed for DOS 3.3

