# Apple 2 DHGR/HGR BYTE Inspector

![hgr byte inspector screenshot](screenshots/hgrbyte.png?raw=true)
![dhgr byte inspector screenshot](screenshots/dhgrbyte.png?raw=true)
![view screenshot](screenshots/viewdhgr.png?raw=true)


# Instructions

1. Start AppleWin
2. F3 to select Slot 6, Drive 1
3. Open: `hgrbyte.dsk`
4. Select BASIC.SYSTEM
5. `BLOAD TUT.DHGR`
6. `-DHGR.BYTE`


# History

Ver 32.
- Fixed bug: Restore cursor position after when using sprite copy

Ver 31.
- Added `:` to save a sprite to memory from the DHGR screen
- Added `;` to copy a sprite from to the DHGR screen
- Added `SPACE` to toggle sprite top left or bottom right corners
- Added `M` to reset sprite bounding box

Ver 30.
- Added beep for illegal keys
- Added sprite info
- Added version number in bottom right

Ver 28.
- Fixed hex input

Ver 27.
- Ripped out ROM output routines and replaced with native code for speed
- Updated the status bar so that the saved bits and reverse bits are displayed
- Added x/2 byte column output
- Shortened memory bank indicator "AUX/MAIN" to "A/M"


# Keys:

```
  ESC   Quit
  g     Toggle fullscreen
  G     Toggle fullscreen

  i     Move cursor up
  j     Move cursor left
  k     Move cursor right
  l     Move cursor down

  I     Move cursor up
  J     Move cursor left
  K     Move cursor right
  L     Move cursor down

  ^I    Move cursor to col 0
  ^J    Move cursor to col 39
  ^K    Move cursor to row 0
  ^L    Move cursor to row 191
  RET   Center cursor

  0..9  "Append" hex nibble to cursor byte
  A..F

  !     Toggle bit 0
  @     Toggle bit 1
  #     Toggle bit 2
  $     Toggle bit 3
  %     Toggle bit 4
  ^     Toggle bit 5
  &     Toggle bit 6
  *     Toggle bit 7 (high bit)
  SPC   Toggle high bit of byte (bit 7)
  (     Set   byte to $FF (Shift-9)
  )     Clear byte to $00 (Shift-0)
  `     Flip all bits
  ~     Flip all bits

  ,     Shift  byte left  (with zero)
  .     Shift  byte right (with one )
  <     Shift  byte left  (with zero)
  >     Shift  byte right (with one )
  [     Rotate byte left
  ]     Rotate byte right

  -     Copy byte  (Save cursor byte to temporary)
  =     Paste byte (Set cursor byte from temporary)
```

# DHGR Colors

```Basic
0 DIM M(16)
1 FOR I=0 TO 7:READ M(I*2):M(I*2+1)=M(I*2)+8:NEXT:GOTO 9
2 POKE A,X:RETURN
3 B3=C *2:IF B3 > 15 THEN B3 = B3 - 15
4 B2=B3*2:IF B2 > 15 THEN B2 = B2 - 15
5 B1=B2*2:IF B1 > 15 THEN B1 = B1 - 15
6 B0=B1*2:IF B0 > 15 THEN B0 = B0 - 15
7 RETURN
9 P=8192:PRINT CHR$(12);CHR$(21):REM 40-COL
10 TEXT:HOME:HGR2:HGR
20 FOR Y=0 TO 7
30 Z=P + Y*1024
40 A=Z+P+1:GOSUB 110
50 A=Z+128:GOSUB 210
60 A=Z+P+256:GOSUB 110
70 A=Z+384+1:GOSUB 210
80 NEXT
90 POKE 3*P-2,0:POKE 3*P-1,32:REM SQUIRT PREVIEW:DHGR
100 VTAB 22:PRINT "BSAVE PAL.DHGR,A$2000,L$4000":END
110 FOR C=0 TO 15:REM Aux/Main/Aux/Main
120 GOSUB 3:IF  C=0 THEN 190
130 A = A-P: X=M(B0):GOSUB 2
140 A = A+P: X=M(B1):GOSUB 2
150 A=A+1
160 A = A-P: X=M(B2):GOSUB 2
170 A = A+P: X=M(B3):GOSUB 2
180 A=A+1:PRINT
190 NEXT:RETURN
210 FOR C=0 TO 15:REM Main/Aux/Main/Aux
220 GOSUB 3:IF C=0 THEN 290
230 A = A+P: X=M(B0):GOSUB 2
240 A=A+1
250 A = A-P: X=M(B1):GOSUB 2
260 A = A+P: X=M(B2):GOSUB 2
270 A=A+1
280 A = A-P: X=M(B3):GOSUB 2
290 NEXT:RETURN
900 REM DHGR: 16 colors
900 REM $00,$04,$44,$4C,$22,$2A,$66,$6E
901 REM $11,$19,$55,$5D,$33,$3B,$77,$7F
902 DATA 0,68
903 DATA 34,102
904 DATA 17,85
905 DATA 51,119
```

Memory order is:

* $2000 -> AUX  $2000
* $4000 -> MAIN $2000

To view without the editor you'll need a [DHGR Viewer](src/dhgr.view.s)

Also included is `squirt` -- a File Manager.


# Tutankhamun

Source image from BrutalDeluxe's [LZ4 page](https://www.brutaldeluxe.fr/products/crossdevtools/lz4/index.html)

Converted to dhgr via Sheldon's [tohgr](http://wsxyz.net/tohgr.html)

```
tohgr.mac -dhgr tut.png
```

![Tutankhamun original](pics/tut.png?raw=true)

![Tutankhamun converted](pics/tut_dhgr.png?raw=true)




# Assembler

* [Merlin32](https://www.brutaldeluxe.fr/products/crossdevtools/merlin/)


# License

* [WTFPL](http://www.wtfpl.net/)

