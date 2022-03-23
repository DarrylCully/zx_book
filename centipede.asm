ENTRY_POINT equ 32768


    org ENTRY_POINT
    call 0xdaf;cls

main:
    ld a,2
    call 5633
    ld de,string
    ld bc,eostr-string
    call 8252
    jp main

string defb 'Darryl is cool'
        defb 13
eostr equ $



    end ENTRY_POINT