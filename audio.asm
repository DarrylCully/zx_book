;Frequencies    Hertz
; Middle C        261.63
; C sharp         277.18
; D               293.66
; D sharp         311.13
; E               329.63
; F               349.23
; F sharp         369.99
; G               392.00
; G sharp         415.30
; A               440.00
; A sharp         466.16
; B               493.88

;DE = Duration = Frequency * Seconds 
;HL = Pitch = 437500 / Frequency - 30.125
playsound_cmiddle_0_5:
    ld hl, 1642
    ld de, 130
    call 949
    ret

playsound_cmiddle_0_25:
    ld hl, 1642
    ld de, 65
    call 949
    ret

playsound_csharp_0_25:
    ld hl,1548
    ld de,69
    call 949
    ret 

playsound_csharp_0_5:
    ld hl, 1548
    ld de, 139
    call 949
    ret

playsound_d_0_5:
    ld hl, 1460
    ld de, 147
    call 949
    ret

playsound_d_0_25:
    ld hl, 1460
    ld de, 74
    call 949
    ret

playsound_dsharp_0_5:
    ld hl,1376
    ld de,156
    call 949
    ret 

playsound_dsharp_0_25:
    ld hl,1376
    ld de,78
    call 949
    ret 

playsound_e_0_5:
    ld hl,1297
    ld de,165
    call 949
    ret 

playsound_e_0_25:
    ld hl,1297
    ld de,82
    call 949
    ret 

playsound_f_0_5:
    ld hl,1223
    ld de,175
    call 949
    ret 

playsound_f_0_25:
    ld hl,1223
    ld de,87
    call 949
    ret 

playsound_fsharp_0_5:
    ld hl,1152
    ld de,185
    call 949
    ret 

playsound_fsharp_0_25:
    ld hl,1152
    ld de,92
    call 949
    ret 

playsound_g_0_5:
    ld hl,1086
    ld de,196
    call 949
    ret 

playsound_g_0_25:
    ld hl,1086
    ld de,98
    call 949
    ret   

playsound_gsharp_0_5:
    ld hl, 1023
    ld de, 208
    call 949
    ret

playsound_gsharp_0_25:
    ld hl, 1023
    ld de, 104
    call 949
    ret

playsound_a_0_5:
    ld hl,964
    ld de,220
    call 949
    ret 

playsound_a_0_25:
    ld hl,964
    ld de,110
    call 949
    ret 

playsound_asharp_0_5:
    ld hl,908
    ld de,233
    call 949
    ret 

playsound_asharp_0_25:
    ld hl,908
    ld de,116
    call 949
    ret 

playsound_b_0_5:
    ld hl,856
    ld de,247
    call 949
    ret

playsound_b_0_25:
    ld hl,856
    ld de,123
    call 949
    ret  



pitch_bend_lp:
    ld hl,500
    ld b,250
    push bc
    push hl
    ld de,1
    call 949
    pop hl
    inc hl
    pop bc
    djnz pitch_bend_lp
    ret