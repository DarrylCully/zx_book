;Input ports
; 32766 B, N, M, Symbol Shift, Space
; 49150 H, J, K, L, Enter
; 57342 Y, U, I, O, P
; 61438 6, 7, 8, 9, 0 = Opens Port 1
; 63486 5, 4, 3, 2, 1 = Opens Port 2
; 64510 T, R, E, W, Q
; 65022 G, F, D, S, A
; 65278 V, C, X, Z, Caps Shift
;

ENTRY_POINT equ 32768


    org ENTRY_POINT
    

    ld a,71
    ld (23693),a
    xor a
    call 8859

    

    ld hl,blocks
    ld (23675),hl

    call 0xdaf;cls

    ld hl,21+14*256
    ld (plx),hl
    
    call basexy
    call splayr

    ld hl,500
    ld b,250


mloop equ $
    call basexy
    call wspace

    ld bc,654
    in a, (c)
    rra
    push af
    call nc, mpl
    pop af
    rra
    push af
    call nc,mpr
    pop af
    rra
    push af
    call nc,mpd
    pop af
    rra
    call nc,mpu

    call basexy
    call splayr
    call pitch_bend_lp

    halt
    
    jp mloop

mpl ld hl,ply
    ld a,(hl)
    and a
    ret z
    dec (hl)
    ret

mpr ld hl,ply
    ld a,(hl)
    cp 31
    ret z
    inc (hl)
    ret 

mpu ld hl,plx
    ld a,(hl)
    cp 4
    ret z
    dec (hl)
    ret 

mpd ld hl,plx
    ld a,(hl)
    cp 21
    ret z
    inc (hl)
    ret 

basexy ld a,22
        rst 16
        ld a,(plx)
        rst 16
        ld a,(ply)
        rst 16
        ret 

splayr ld a,69
        ld (23695),a
        ld a,144
        rst 16
        ret 
wspace ld a,71
        ld (23695),a
        ld a,32
        rst 16
        ret 

plx defb 0
ply defb 0

blocks defb 16,16,56,56,124,124,254,254

ktest ld c,a
    and 7
    inc a
    ld b,a
    srl c
    srl c
    srl c
    ld a,5
    sub c
    ld c,a
    ld a,254
ktest0 rrca
    djnz ktest0
    in a,(254)
ktest1 rra
    dec c
    jp nz,ktest1
    ret

pitch_bend_lp:
    push bc
    push hl
    ld de,1
    call 949
    pop hl
    inc hl
    pop bc
    djnz pitch_bend_lp
    ret

;     ld bc,(score)
;     call 6683

 
;      ld a,2 ;2 = upper screen
;      call 5633 ; open channel
;      ld a,21 ;row 21 = bottom of screen
;      ld (xcoord),a ;set initial x coordinate
;      out (254),a
;      ld a,2
     
; main:    
;     call setxy ;set up our x/y coords
;      ld a,144;asterisk to draw
;      rst 16 ;display it
;      call delay ;delay for a second
;      call setxy ; setuup x/y coords
;     ld a,32 ;ASCII code for space
;      rst 16 ;delete asterisk
;      ld hl,xcoord ;vertical position
;      dec (hl) ;move it one line up
;      ld a,(hl)
;      cp 255
;      jr nz,main
;      ret 

    

;  delay ld b,10
;  delay0:
;         halt
;         djnz delay0
;         ret 

;  setxy: 
;         ld a,22
;         rst 16
;         ld a,(xcoord)
;         rst 16
;         ld a,(ycoord)
;         rst 16
;         ret 

;  xcoord defb 0
;  ycoord defb 15
;  udgs defb 60,126,219,153
;       defb 255,255,219,219

; score dw 190


;      ld a,2 ; 2 = upper screen.
;         call 5633 ; open channel.
;         ld a,21 ; row 21 = bottom of screen.
;         ld (xcoord),a ; set initial x coordinate.
; loop    call setxy ; set up our x/y coords.
;         ld a,'*' ; want an asterisk here.
;     rst 16 ; display it.
;     call delay ; want a delay.
;     call setxy ; set up our x/y coords.
;     ld a,32 ; ASCII code for space.
;     rst 16 ; delete old asterisk.
;     ld hl,xcoord ; vertical position.
;     dec (hl) ; move it up one line.
;     ld a,(hl) ; where is it now?
;     cp 255 ; past top of screen yet?
;     jr nz,loop ; no, carry on.
;     ret
; delay ld b,10 ; length of delay.
; delay0 halt ; wait for an interrupt.
;  djnz delay0 ; loop.
;  ret ; return.
; setxy ld a,22 ; ASCII control code for AT.
;  rst 16 ; print it.
;  ld a,(xcoord) ; vertical position.
;  rst 16 ; print it.
;  ld a,(ycoord) ; y coordinate.
;  rst 16 ; print it.
;  ret
; xcoord defb 0
; ycoord defb 15

    ; ld de,string ; address of string
    ; ld bc,eostr-string ; length of string
    ; call 8252 ; print string
    ; jp main

; string defb 22,10,11,'!'
;        ; defb 13
; eostr equ $



    end ENTRY_POINT