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
    

    ld a,71 ;white ink(7) black paper (0) bright (64)
    ld (23693),a ;set screen colours
   ; xor a ; load acc to zero
    call 0xdaf      ;cls // 3503
    call 8859 ;set border colours

    ; ld a,2
    ; out (254),a
    ; ld a,2
    ; call 8859

    

    ld hl,blocks    ;address of udgs
    ld (23675),hl   ;pointer to udgs

    

    ld hl,21+14*256 ;load hl pair with start coords
    ld (plx),hl     ;set player coords

    ld b,10
    ld hl,segmnt
segint  ld (hl),1
        inc hl
        ld (hl),0
        inc hl
        ld (hl),b
        inc hl
        djnz segint
    
    call basexy     ;set x and y pos of player
    call splayr     ;show player base symbol

    ;ld hl,500       ; audio
    ;ld b,250        ; audio

    ld a,68         ; green ink (4) black paper (0) bright (64)
    ld (23695),a    ; set temporary colours
    ld b,50         ; start with a few

;     ld hl,mushlp    ;alien data structures
;     ld b,55         ;number of aliens

; alienspawnloop  call show
;                 djnz alienspawnloop
;                 ret 

; show    ld a,(hl)     ;fetch alien status
;         cp 255        ;is aliens switched off
;         jr z,next     ;yes, dont display him
;         push hl
;         inc hl
;         ld d,(hl)
;         inc hl
;         ld e,(hl)
;         call disply
;         pop hl
; next ld de,3
;     add hl,de
;     ret

mushlp  ld a,22     ;control code for AT character   
        rst 16      
        call random ;get a random number
        and 15      ;want a vertical range 0 to 15
        rst 16      
        call random ;get random number
        and 31      ;want horizontal range 0 to 31
        rst 16      
        ld a,145    ;UDG B 
        rst 16      ;draw 
        djnz mushlp ;repeat until all are displayed

mloop equ $
    call basexy     ;set x and y pos of player
    call wspace     ;display space over player

    ld bc,65022       ;load keyboard port
    in a, (c)       ; see which keys are pressed
    rra             ; outermost bit = key 1
    push af         ;remember value
    call nc, mpl    ;being pressed, move player left
    pop af          ;restore acc
    rra             ;next bit along (val 2) = key 2
    push af         
    call nc,mpd
    pop af
    rra
    push af
    call nc,mpr
    pop af

    ld bc,64510
    in a,(c)
    rra
    push af
    ;call nc,mpu
    pop af
    rra
    push af
    call nc,mpu
    pop af

    call basexy     ;set x/y pos of player
    call splayr     ;show player
    ;call pitch_bend_lp

    ld ix,segmnt
    ld b,10
censeg push bc
    ld a,(ix)
    inc a
    call nz, proseg
    pop bc
    ld de,3
    add ix,de
    djnz censeg

    halt            ;delay
    
    jp mloop

mpl ld hl,ply   ; y is horizontal coord
    ld a,(hl)   ; whats current val
    and a       ;is it 0
    ret z       ; yes, dont go further

    ;simple collision detection
    ld bc,(plx)
    dec b
    call atadd
    cp 68
    ret z

    dec (hl)    ;decrement y by 1
    ret

mpr ld hl,ply
    ld a,(hl)
    cp 31
    ret z

    ;collision detection
    ld bc,(plx)
    inc b
    call atadd
    cp 68
    ret z

    inc (hl)
    ret 

mpu ld hl,plx
    ld a,(hl)
    cp 4
    ret z

    ld bc,(plx)
    dec c
    call atadd
    cp 68
    ret z

    dec (hl)
    ret 

mpd ld hl,plx
    ld a,(hl)
    cp 21
    ret z

    ld bc,(plx)
    inc c
    call atadd
    cp 68
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

segxy ld a,22
      rst 16
      ld a,(ix+1)
      rst 16
      ld a,(ix+2)
      rst 16
      ret 

proseg ld a,(ix)
       inc a
       ret z
       call segxy
       call wspace
       call segmov
       ld a,(ix)
       inc a
       ret z
       call segxy
       ld a,2
       ld (23695),a
       ld a,146
       rst 16
       ret 

segmov ld a,(ix+1)
       ld c,a
       ld a,(ix+2)
       ld b,a
       ld a,(ix)
       and a
       jr z,segml

segmr  ld a,(ix+2)
       cp 31
       jr z,segmd
       inc a
       ld b,a
       call atadd
       cp 68
       jr z,segmd
       inc (ix+2)
       ret

segml  ld a,(ix+2)
       and a
       jr z,segmd
       dec a
       ld b,a
       call atadd
       cp 68
       jr z,segmd
       dec (ix+2)
       ret 

segmd   ld a,(ix)
        xor 1
        ld (ix),a
        ld a,(ix+1)
        cp 21
        jr z,segmt

        inc (ix+1)
        ret


segmt xor a
      ld (ix+1),a
      ret

plx defb 0
ply defb 0

blocks defb 16,16,56,56,124,124,254,254
        defb 24,126,255,255,60,60,60,60
        defb 24,126,126,255,255,126,126,24

;table for segments
;format: 3 bytes per entry, 10 segments
; byte 1: 255=segment off, 0=left, 1=right
; byte 2: x(vertical) coord
; byte 3: y (horizontal) coord
segmnt  defb 0, 0, 0    ;1
        defb 0, 0, 0    ;2
        defb 0, 0, 0    ;3
        defb 0, 0, 0    ;4
        defb 0, 0, 0    ;5
        defb 0, 0, 0    ;6
        defb 0, 0, 0    ;7
        defb 0, 0, 0    ;8
        defb 0, 0, 0    ;9
        defb 0, 0, 0    ;10


;keyboard port method to allow most keys to work
; ktest ld c,a
;     and 7
;     inc a
;     ld b,a
;     srl c
;     srl c
;     srl c
;     ld a,5
;     sub c
;     ld c,a
;     ld a,254
; ktest0 rrca
;     djnz ktest0
;     in a,(254)
; ktest1 rra
;     dec c
;     jp nz,ktest1
;     ret


random ld hl,(seed) ;pointer
    ld a,h
    and 31  ;keep it in first 8k of ROM
    ld h,a
    ld a,(hl)   ;get random number from locations
    inc hl      ;increment pointer
    ld (seed),hl
    ret 

seed defw 0


atadd ld a,c
     rrca
     rrca
     rrca
     ld e,a
     and 3
     add a,88
     ld d,a
     ld a,e
     and 224
     ld e,a
     ld a,b
     add a,e
     ld e,a
     ld a,(de)
     ret

; pitch_bend_lp:
;     push bc
;     push hl
;     ld de,1
;     call 949
;     pop hl
;     inc hl
;     pop bc
;     djnz pitch_bend_lp
;     ret

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