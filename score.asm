ENTRY_POINT equ 32768

    org ENTRY_POINT
    call 0xdaf;cls
    ld bc,(score)
    call 6683
    ;call 11563
    ;call 11747


    score dw 19000



    end ENTRY_POINT