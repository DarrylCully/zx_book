random ld hl,(seed) ;pointer
    ld a,h
    and 31  ;keep it in first 8k of ROM
    ld h,a
    ld a,(hl)   ;get random number from locations
    inc hl      ;increment pointer
    ld (seed),hl
    ret 

seed defw 0