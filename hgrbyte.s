        __MAIN = $08F4
        .include "dos33.inc"

; HGR Byte Inspector
; Michael Pohoreski
; Version 1 - Barebones but it works
;
; Keys:
;
;   ESC to Quit, else use arrow keys to move the cursor
;   <-
;   ->
;   Up Arrow
;   Down Arrow
;
; Can resume with 910G$

        GBASL       = $26
        GBASH       = $27
        val         = $F0
        col         = $F1
        row         = $E2   ; used by Applesoft HGR.row
        HGRPAGE     = $E6   ; used by Applesoft HGR.page

        CH          = $24   ; text cursor column
        CV          = $25   ; text cursor row

        HPOSN       = $F411 ; A=row, Y,X=col update GBASL GBASH
        VTAB        = $FC22 ; A=row
        HOME        = $FC58
        COUT        = $FDED
        PR_HEX      = $FDD3
        PRBYTE      = $FDDA

        KEYBOARD    = $C000
        KEYSTROBE   = $C010

        .org $8F4
HgrByteInspector:
        JSR HOME
        BIT $C050
        BIT $C053
        BIT $C057
        LDA #0
        STA val
        STA col
        STA row
        STA GBASL
        LDA #$20
        STA GBASH
        STA HGRPAGE
Cursor:
        LDY #0          ; Update pointer to sceeen
        LDX #0
        LDA row
        JSR HPOSN       ; A=row, Y,X=col X->E0 Y->E1
GetKey:
        LDY col
        LDA (GBASL),Y
        STA val
        JSR XorByte
        LDA KEYBOARD
        BPL GetKey
        STA KEYSTROBE
        LDX #nKeys-1
FindKey:
        CMP aKeys, X
        BEQ GotKey
        DEX
        BPL FindKey
BadKey:
        BMI Cursor
GotKey:
        LDA #>GotKey
        PHA
        LDA Func, X
        PHA
Done:   RTS
MoveL:  DEC col
        BPL Cursor
MoveR:  INC col
        LDA col
        CMP #40
        BCS MoveL
        BCC Cursor
MoveU:  DEC row
        BPL Cursor
MoveD:  INC row
        LDA row
        CMP #192
        BCS MoveU
        BCC Cursor

aKeys:
        .byte $88       ; <-
        .byte $95       ; ->
        .byte $8B       ; Up arrow
        .byte $8A       ; Down arrow
        .byte $9B       ; Esc
eKeys:
nKeys   = eKeys - aKeys ;

Func:
        .byte <MoveL -1
        .byte <MoveR -1
        .byte <MoveU -1
        .byte <MoveD -1
        .byte <Done  -1

TogByte:
        .byte $2C       ; BIT $abs
XorByte:
        LDA val
        LDY col
        EOR #$FF
        STA (GBASL),Y
Row14:  LDA #0
        STA CH
        LDA #20         ; $14 = 20
        STA CV
        JSR VTAB
PrintStatus:
        LDA #'X'+$80    ;X=## Y=## $=####:##
        JSR COUT
        LDA col
        JSR PR_HEX
        LDA #' '+$80
        JSR COUT
        LDA #'Y'+$80
        JSR COUT
        LDA row
        JSR PR_HEX
        LDA #' '+$80
        JSR COUT
        LDA #'$'+$80
        JSR COUT
        LDA GBASH
        JSR PR_HEX
        LDA GBASL
        JSR PRBYTE
        LDA #':'+$80
        JSR COUT
        LDA val
        JSR PRBYTE
        LDA val
        LDY col
        STA (GBASL),Y
        RTS
__END:

