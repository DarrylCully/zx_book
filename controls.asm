ENTRY_POINT equ 32768


    org ENTRY_POINT
    call 0xdaf;cls
    ld hl,23560
    ld (hl),0

loop:
    ld a,(hl)
    cp 0
    jr z, loop
    ret




     end ENTRY_POINT