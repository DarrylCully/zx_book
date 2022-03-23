ENTRY_POINT equ 32768


    org ENTRY_POINT
    call 0xdaf;cls

    ld hl,15616
    ld de,60000
    ld bc,768
font1 
    ld a,(hl)
    rlca
    or (hl)
    ld (de),a
    inc hl
    inc de
    dec bc
    ld a,b
    or c
    jr nz,font1
    ld hl,60000-256
    ld (23606),hl
    ret

    
    
    end ENTRY_POINT