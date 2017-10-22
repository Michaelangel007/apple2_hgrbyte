    .word __MAIN         ; 2 byte BLOAD address
    .word __END - __MAIN ; 2 byte BLOAD size
    .org  __MAIN         ; .org must come after header else offsets are wrong

; Can't save filenames with * or - in them, hangs DOS3.3
; SAVE * COPY FOO.BAS CON
; SAVE -- COPY FOO.BAS CON

