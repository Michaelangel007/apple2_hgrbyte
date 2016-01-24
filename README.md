#Apple 2 HGR BYTE Inspector

Keys:

  ESC   Quit
  G     Toggle fullscreen

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

  -     Save cursor byte to temporary
  =     Set cursor byte from temporary


