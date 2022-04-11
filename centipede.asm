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
    
    numseg equ 8 ; number of centipede segments

    ld a,67 ;white ink(7) black paper (0) bright (64)
    ld (23693),a ;set screen colours
   ; xor a ; load acc to zero
    call 0xdaf      ;cls // 3503
    call 8859 ;set border colours

    ; ld a,2
    ; out (254),a
;     ld a,3
;     call 8859


    

    ld hl,blocks    ;address of udgs
    ld (23675),hl   ;pointer to udgs

;     ld de,0
;     call setposition
;     ld bc,(score)
;     call 0x1a1b

    
    xor a ;zeroise accumlator
    ld (dead),a ;set flag to say player is alive


    ld hl,21+14*256 ;load hl pair with start coords
    ld (plx),hl     ;set player coords
    ld hl,255+255*256 ;player's bullets default
    ld (pbx),hl ;set bullet coords

;     ld b,10     ;number of segments to initialise
;     ld hl,segmnt    ;segment table
; segint  ld (hl),1   ;start off moving right
;         inc hl      ;
;         ld (hl),8   ;start at top
;         inc hl      ;
;         ld (hl),b   ;use b register as y coordinate
;         inc hl      ;
;         djnz segint ;repeat until all initialised

;         ld bc,basexy
;         ;call sprite
    
;         ;call basexy     ;set x and y pos of player
;         ;call splayr     ;show player base symbol

;         ;ld hl,500       ; audio
;         ;ld b,250        ; audio

         ld a,68         ; green ink (4) black paper (0) bright (64)
         ld (23695),a    ; set temporary colours
        ld b,5         ; start with a few

        ;call panels

        ;call shwnum
        ;call uscor0
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

;Alien data table, 6 bytes per alien
; ix = graphic type
;ix + 1 = direction, 0 = up, 1 = right, 2 = down, 3 = left
;ix + 2 = current x coord
;ix + 3 = current y coord
;ix + 4 = min x or y coord, depends on direction
;ix + 5 = max x or y coord, depends on dir

; altab   defb 0, 0, 0, 0, 0, 0
;         defb 0, 0, 0, 0, 0, 0
;         defb 0, 0, 0, 0, 0, 0

;         ld a,(ix+1)
;         rra
;         jr nc, movav

;         rra
        
;         jr nc, movar

; moval   ld a,(ix+3)
;         sub 2
;         ld (ix+3),a
;         cp (ix+4)
;         jr z,movax
;         jr c,movax
;         ret     

; movar   ld a,(ix+3)
;         add a,2
;         ld (ix+3),a
;         cp (ix+5)
;         jr nc,movax
;         ret 

; movav   rra
;         jr c,movad

; movau   ld a,(ix+2)
;         sub 2
;         ld (ix+2),a
;         cp (ix+4)
;         jr z,movax
;         ret     

; movad   ld a,(ix+2)
;         add a,2
;         ld (ix+2),a
;         cp (ix+5)
;         jr nc,movax
;         ret     

; movax   ld a,(ix+1)
;         xor 2
;         ld (ix+1),a
;         ret

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

        halt
        ld ix,entab     ;point to odd spaceships
        call mship      ;move spaceships
        call colc16
        halt            
        ld ix,entab+4   ;point to even spaceships
        call mship      ;move even spaceships
        call colc16
        call gwave      ;generate fresh wave

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
        rra
        call nc, fire ;it's being pressed, move up

        ; create bullet
        call bchk   ;check bullet position

        call dbull ;delete bullets
        call moveb ;move bullets
        call bchk   ;check new bullets position
        call pbull ;print bullets at new position

        

        call basexy     ;set x/y pos of player
        call splayr     ;show player

  
        ;call pitch_bend_lp

; ;centipede segments
;         ld ix,segmnt    ;table of segment data
;         ld b,10         ;number of segments in table
; censeg  push bc         ;
;         ld a,(ix)       ;is segment switched on
;         inc a           ;255 = off, increments to zero
;         call nz, proseg ;it's active. process segment
;         pop bc
;         ld de,3         ;3 bytes per segment
;         add ix,de       ;get next segment in ix registers
;         djnz censeg     ;repeat all segments

        halt            ;delay

        ld a,(dead) ;was the player killed by a segment
        and a
        ret nz  ;player is killed - lose a life
        
        jp mloop

;move player left
mpl     ld hl,ply   ; y is horizontal coord
        ld a,(hl)   ; whats current val
        and a       ;is it  at the left edge = 0
        ret z       ; yes, dont go further

        ;simple collision detection
        ld bc,(plx) ;current coords
        dec b       ;look 1 square to the left
        call atadd  ;get address of attribute at this position
        cp 68       ;mushrooms are bright (64) + green (4)
        ret z       ;theres a mushroom - we cant move there

        dec (hl)    ;decrement y by 1
        ret

;move player right
mpr     ld hl,ply   ; y is horizontal coord
        ld a,(hl)   ;current val
        cp 31       ;is it at the right edge = 31
        ret z       ;yes we cant go left

        ;collision detection
        ld bc,(plx) ;current coords
        inc b       ;look 1 square to the right
        call atadd  ;get address of attribute at this position
        cp 68       ;
        ret z       ;

        inc (hl)    ;increment
        ret 

;move player down
mpu     ld hl,plx
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

;move player down
mpd     ld hl,plx
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

;fire a missile
fire    ld a,(pbx)  ;bullet vertical coord
        inc a       ;255 is default val, increments to zero
        ret nz      ;bullet on screen, can't fire again
        ld hl,(plx) ;player coords
        dec l       ; 1 square higher
        ld (pbx),hl ; set bullet coords
        ret 

bchk    ld a,(pbx)  ;bullet vertical
        inc a       ;is it at 255?
        ret z       ;yes, no bullet on screen
        ld bc,(pbx) ;get coords
        call atadd  ;find attribute here
        cp 68       ;
        jr z,alnhit  ;hit a mushroom
        ret 

; hmush   ld a,22     ;AT control code
;         rst 16      ;
;         ld a,(pbx)  ;bullet vertical
;         rst 16
;         ld a,(pby)  ;bullet horizontal
;         rst 16
;         call wspace ;set ink colour to white

alnhit  ld a,22
        rst 16
        ld a,(pbx)
        rst 16
        ld a,(pby)
        rst 16
        call wspace

kilbul  ld a,255    ;x coord of 255 = swith bullet off
        ld (pbx),a  ;destroy bullet
        ret 

moveb   ld a,(pbx)  ;bullet vertical
        inc a       ;is it at 255
        ret z       ;yes, no bullet on screen
        sub 2       ;1 row up
        ld (pbx),a
        ret 

;;;;;;;DOUBLE SCREEN BUFFER;;;;;;;
;         ld hl,49152
;         ld de,16384
;         ld bc,6912
;         ldir
        

; SEG1    equ 16514
; SEG2    equ 18434
; SEG3    equ 20482

; P0      equ 0
; P1      equ 256
; P2      equ 512
; P3      equ 768
; P4      equ 1024
; P5      equ 1280
; P6      equ 1536
; P7      equ 1792

; C0      equ 0
; C1      equ 32
; C2      equ 64
; C3      equ 96
; C4      equ 128
; C5      equ 160
; C6      equ 192
; C7      equ 224

; xfer    ld (stptr)
;;;;;;ALIEN;;;;;;
;move enemy ships
        
mship   ld b,4          ;number to process
mship0  push bc         ;store count
        ld a,(ix)       ;get pointer low
        ld l,a          ;put into l
        ld h,(ix+1)     ;get high byte
        or h            ;check pointer is set up
        and a           ;is it
        call nz,mship1  ;yes, process it
        ld de,8         ;skip to next but one entry
        add ix,de       ;point to next enemy
        pop bc          ;restore count
        djnz mship0     ;repeat for all enemies
        ret 

mship1  push hl         ;store pointer to coordinate
        call dship      ;delete this ship
        pop hl          ;restore coord
        ld a,(hl)       ;fetch next coord
        inc hl          ;move pointer on
        ld (ix),l       ;new pointer low bute
        ld (ix+1),h     ;pointer high byte
        ld (ix+2),a     ;set x coord
        ld a,(ix+3)     ;fetch horizontal position
        sub 2           ;move left 2 pixels
        ld (ix+3),a     ;set new position
        cp 240
        jp c,dship
        
        xor a
        ld (ix),a
        ld (ix+1),a
        ld hl,numenm
        dec (hl)
        xor a
        ld (ix),a
        ld (ix+1),a
        ld hl,numenm
        dec (hl)
        ret 

gwave   ld hl,shipc
        dec (hl)
        ld a,(hl)
        cp 128
        jr z,gwave2
        ret nc
        and 7
        ret nz
        ld ix,entab
        ld de,2
        ld b,8
gwave0  ld a,(ix)
        ld h,(ix+1)
        or h
        jr z,gwave1
        add ix,de
        djnz gwave0
        ret 
gwave2  ld hl,numenm
        ld a,(hl)
        inc a
        and 3
        ld (hl),a
        ret
gwave1  ld hl,numenm
        inc (hl)
        ld a,(wavnum)
        ld hl,wavlst
        rlca
        rlca
        ld e,a
        ld d,0
        add hl,de
        ld a,(shipc)
        and 8
        rrca
        rrca
        ld e,a
        ld d,0
        add hl,de
        ld e,(hl)
        inc hl
        ld d,(hl)
        ld (ix),e
        ld (ix+1),d
        ld a,(de)
        ld (ix+2),a
        ld (ix+3),240



dship   ld hl,shipg
        ld b,(ix+3)
        ld c,(ix+2)
        ld (xcoord),bc
        jp sprite
shipc   defb 128
numenm  defb 0


coord0  defb 40,40,40,40,40,40,40,40
        defb 40,40,40,40,40,40,40,40
        defb 42,44,46,48,50,52,54,56
        defb 58,60,62,64,66,68,70,72
        defb 72,72,72,72,72,72,72,72
        defb 72,72,72,72,72,72,72,72
        defb 70,68,66,64,62,60,58,56
        defb 54,52,50,48,46,44,42,40
        defb 40,40,40,40,40,40,40,40
        defb 40,40,40,40,40,40,40,40
        defb 38,36,34,32,30,28,26,24
        defb 22,20,18,16,14,12,10,8
        defb 6,4,2,0,2,4,6,8
        defb 10,12,14,16,18,20,22,24
        defb 26,28,30,32,34,36,38,40
coord1  defb 136,136,136,136,136,136,136,136
        defb 136,136,136,136,136,136,136,136
        defb 134,132,130,128,126,124,122,120
        defb 118,116,114,112,110,108,106,104
        defb 104,104,104,104,104,104,104,104
        defb 106,108,110,112,114,116,118,120
        defb 122,124,126,128,130,132,134,136
        defb 136,136,136,136,136,136,136,136
        defb 136,136,136,136,136,136,136,136
        defb 138,140,142,144,146,148,150,152
        defb 154,156,158,160,162,164,166,168
        defb 170,172,174,176,174,172,170,168
        defb 166,164,162,160,158,156,154,152
        defb 150,148,146,144,142,140,138,136


wavlst  defw coord0, coord0, coord1, coord1
        defw coord1, coord0, coord0, coord1

wavnum  defb 0

shipg   defb 248,252,48,24,24,48,12,96,24,48,31,243,127,247,255,247
        defb 255,247,127,247,31,243,24,48,12,96,24,48,48,24,248,252

xcoord  defb 0
ycoord  defb 0


entab   defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0




;;;;;ALIEN END;;;;;;


;set up the x and y coords for the player's gunbase position,
; this routine is called prior to display an deletion of gunbase
basexy  ld a,22 ;AT code
        rst 16
        ld a,(plx)  ;player vertical coord
        rst 16      ; set vertical position of player
        ld a,(ply)  ;player horizontal coord
        rst 16      ;set horizontal position of player
        ret 

; show player at current print position
splayr  ld a,69     ;cyan ink(5) on black paper(0), bright(64)
        ld (23695),a    ;set temporary screen colours
        ld a,144        ;ASCII for UDG 'A'
        rst 16          ;draw player
        ret 

pbull   ld a,(pbx)      ;bullet vertical
        inc a           ;is it 255
        ret z           ; yes, no bullet on screen
        call bullxy     ;
        ld a,16         ;INK control char
        rst 16
        ld a,6          ;6 = yellow
        rst 16
        ld a,147        ;UDG 'D'
        rst 16
        ret 

dbull   ld a,(pbx)      ;bullet vertical
        inc a
        ret z
        call bullxy     ;set up bullet coords

wspace  ld a,71         ;
        ld (23695),a
        ld a,32         ;space character
        rst 16          ;display space
        ret 


bullxy  ld a,22         ;AT code
        rst 16
        ld a,(pbx)      ;player bullet vertical coord
        rst 16          ;set player vertical position
        ld a,(pby)      ;bullet horizontal position
        rst 16          ;set horizontal coord
        ret

; segxy   ld a,22     ;ASCII code for AT character
;         rst 16      ; display AT code
;         ld a,(ix+1) ;get segment x coord
;         rst 16      ; display coordinate code
;         ld a,(ix+2) ;get segment y coord
;         rst 16      ;display coord code
;         ret 

; proseg  call segcol ;segment collision detection
;         ld a,(ix)   ;check if segment was switched off
;         inc a       ;by collision detection routine
;         ret z       ;it was so segment is now dead
;         call segxy  ;set up segment coords
;         call wspace ;display a space, white ink on black
;         call segmov ;move segment
;         call segcol ;new segment position collision check
;         ld a,(ix)   ;check if segment was switched off
;         inc a       ;by collision detection routine
;         ret z       ;it was, so segment is dead
;         call segxy  ;set up segment coords
;         ld a,2      ;attribute code 2 = red segment
;         ld (23695),a    ;set temp attributes
;         ld a,146    ;UDG 'C'
;         rst 16 
;         ret 

; segmov  ld a,(ix+1) ;x coord
;         ld c,a      ;GP x area
;         ld a,(ix+2) ;y coord
;         ld b,a      ;GP y area
;         ld a,(ix)   ;status flag
;         and a       ;is the segment heading left
;         jr z,segml  ;going left - jump to that bit of code

; segmr   ld a,(ix+2) ;y coord
;         cp 31       ;already at edge of screen
;         jr z,segmd  ;yes - move segment down
;         inc a       ;look right
;         ld b,a      ;set up GP y coord
;         call atadd  ;find attribute address
;         cp 68       ;mushroom
;         jr z,segmd  ;mushroom to right, move down
;         inc (ix+2)  ;no obstacle so move right
;         ret

; segml   ld a,(ix+2) ;y coord
;         and a
;         jr z,segmd
;         dec a
;         ld b,a
;         call atadd
;         cp 68
;         jr z,segmd
;         dec (ix+2)
;         ret 

; segmd   ld a,(ix)   ;segment direction
;         xor 1       ;reverse it
;         ld (ix),a   ;store new direction
;         ld a,(ix+1) ;y coord
;         cp 21       ;already at bottom of screen
;         jr z,segmt  ;yes, move segment to top

;         inc (ix+1) ;havent reached bottom so move down
;         ret

;check (l,h) for collisioons with (c,b), strict enforcement without cutting off corners
colx16  ld a,l      ;x coord
        sub c       ;subtract x
        add a,15    ;add max distance
        cp 31       ;within x range
        ret nc      ;no - they've missed
        ld a,h      ; y coord
        sub b       ;sub y
        add a,15    ;add max distance
        cp 31       ;with y range
        ret         ;carry flag set if there's a collision

; check (l,h) for collisions wiht (c,b), cutting corners
colc16  ld a,(plx)          ;x coord
        sub c           ;sub x
        jr nc,colc1a    ;result is positive
        neg             ;make negative positive
colc1a  cp 16           ;within x range
        ret nc          ;no - missed
        ld e,a          ;store difference
        
        ld a,(ply)          ;y coord
        sub b           ;sub y
        jr nc,colc1b    ;result is positive
        neg             ;make negative
colc1b  cp 16           ;within y range
        ret nc          ;no = missed

        add a,e         ;add x difference
        cp 26           ;only 5 corners pixels touching
        ret             ;carry set if there's a collision


; segmt xor a         ;same as ld, 0 but saves 1 byte
;       ld (ix+1),a   ;new x coord = top of screen
;       ret

; ;segment collision detection
; ;checks for collision with player and player bullets
; segcol ld a,(ply)   ;bullet y pos
;        cp (ix+2)    ;is it identical to segment y coord
;        jr nz,bulcol ;y coords differ, try bullet instead
;        ld a,(plx)   ;player x coord
;        cp (ix+1)    ;same as segment
;        jr nz,bulcol ;x coords differ, try bullet instead

;collision with player
killpl ld (dead),a  ;set flag to say that player is now dead
       ret 

;check for collision with players bullet
bulcol ld a,(pbx)   ;bullet x coords
       inc a        ;at default value
       ret z        ;yes, no bullet to check for
       cp (ix+1)    ;is bullet x coord same as segment x coord
       ret nz       ; no so no collision
       ld a,(pby)   ;bullet y position
       cp (ix+2)    ;is it identical to segment y coord
       ret nz       ;no - no collision
       
       ; collision between bullet
       call dbull   ;delete bullet
       ld a,22      ;AT code
       rst 16       
       ld a,(pbx)   ;player bullet vertical coord
       inc a        ;1 line down
       rst 16       ;set vertical pos of mushroom
       ld a,(pby)   ;bullets horizontal position
       rst 16       ;set the horizontal coord
       ld a,16      ;ASCII code for ink control
       rst 16       ;
       ld a,4       ;4 = colour green
       rst 16       ;we want all mushrooms in this colur
       ld a,145     ;UDG 'B'
       rst 16       ;put mushroom on screen
       call kilbul  ;kill bullet
       ;ld (ix),a    ;kill segment
       ;ld hl,s
       ;ld hl,numseg ;number of segments
       dec (hl)     ;decrement it
       ret

plx defb 0          ;player x coord
ply defb 0          ;player y coord
pbx defb 255        ;player bullet coord
pby defb 255
dead defb 0         ;flag for player dead, if not 0

tmp0 defw 0
tmp1 defb 0




blocks  defb 16,16,56,56,124,124,254,254 ;player base
        defb 24,126,255,255,60,60,60,60 ;mushroom
        defb 24,126,126,255,255,126,126,24  ;segment 
        defb 0,102,102,102,102,102,102,0    ;player bullet
        defb 10, 10, 24, 24, 56, 56, 24, 0 ; test

; ;table for segments
; ;format: 3 bytes per entry, 10 segments
; ; byte 1: 255=segment off, 0=left, 1=right
; ; byte 2: x(vertical) coord
; ; byte 3: y (horizontal) coord
; segmnt  defb 0, 10, 0    ;1
;         defb 0, 10, 0    ;2
;         defb 0, 10, 0    ;3
;         defb 0, 10, 0    ;4
;         defb 0, 10, 0    ;5
;         defb 0, 10, 0    ;6
;         defb 0, 10, 0    ;7
;         defb 0, 10, 0    ;8
;         defb 0, 10, 0    ;9
;         defb 0, 10, 0    ;10


; //////////Screen address lookup table//////////

;     xor a           ;clear carry flag and accumulator
;     ld d,a          ;empty de high byte
;     ld a,(plx)   ;x pos
;     rla             ;shift left to multiply by 2
;     ld e,a          ;place this in low byte of de pair
;     rl d            ;shift top bit into de high byte
;     ld hl,addtab    ;table of screen addresses
;     add hl,de       ;point to table entry
;     ld e,(hl)       ;low byte of screen address
;     inc hl          ;point to high byte
;     ld d,(hl)       ;high byte of screen address
;     ld a,(ply)   ;horizontal position
;     rra             ;divide by 2
;     rra             ;and again for four
;     rra             ;shift again to divide by eight 
;     and 31          ;mask away rubbish shifted into rightmost bits
;     add a,e         ;add to address for start of line
;     ld e,a          ;new value of e register
;     ret             ;return with screen address in de


; addtab  defw 16384
;         defw 16640
;         defw 16896


;//////////calculating screen address (METHOD 2)
scadd   ld a,(xcoord)   ;fetch vertical coord
        ld e,a          ;store in e

       ;find line within cell 
        and 7           ;line 0-7 within character square
        add a,64        ;64 * 256 = 16384 = start of screen display
        ld d,a          ;line * 256
        
        ;find which third of the screen we're in
        ld a,e          ;restor the vertical
        and 192         ;segment 0, 1 or 2 multiplied by 64
        rrca            ;divide this by 8
        rrca            
        rrca            
        add a,d         ;segment 0-2 multiplied by 8
        ld d,a          ;add to d give segment start address
        
        ;find character cell within segment
        ld a,e          ;8 character squares per segment
        rlca            ; divide x by 8 and multiply by 4
        rlca            ;net calculation; multiply by 4
        and 224         ;mask off bits we don't want
        ld e,a          ;vertical coord calculation done
        
        ;Add the horizontal element
        ld a,(ycoord)   ;y coord
        rrca            ;only need to divide by 8
        rrca            
        rrca            
        and 31          ;squares 0 - 31 across screen
        add a,e         ;add to total so far
        ld e,a          ;de = address of screen
        ret

; Shifting
sprit7  xor 7           ;complement the last 3 bits
        inc a           ;add one for luck
sprit3  rl d            ;rotate left
        rl c            ;into middle byte
        rl e            ;and finally into left character cell
        dec a           ;count shifts we've done
        jr nz,sprit3    ;return until all shifts complete

    ;Line of sprite image is now in e + c + d, we need it in form c + d + e
        ld a,e          ;left edge of image is currently in e
        ld e,d          ;put right edge there instead
        ld d,c          ;middle bit goes in d
        ld c,a          ;left edge back into c
        jr sprit0       ;we've done the switch so transfer to screen

sprite  ld a,(xcoord)   ;draws sprite(hl)
        ld (tmp1),a     ;store vertical
        call scadd      ;calculate screen address
        ld a,16         ;height of sprite in pixels
sprit1  ex af,af'       ;store loop counter
        push de         ;store screen address
        ld c,(hl)       ;first sprite graphic
        inc hl          ;increment pointer to sprite data
        ld d,(hl)       ;next bit of sprite image
        inc hl          ;point to next row of sprite data
        ld (tmp0),hl    ;store in tmp0 for later
        ld e,0          ;blank right byte for now
        ld a,b          ;b holds y position
        and 7           ;how are we straddling character cells
        jr z,sprit0     ;we're not straddling them so don't bother shifting
        cp 5            ;5 or more shifts are needed
        jr nc,sprit7    ;yes shift from left as it#s quicker
        and a           ;carry flag is set so clear
sprit2  rr c            ;rotate left byte right
        rr d            ;through middle byte
        rr e            ;into right byte
        dec a           ;one less shift to do
        jr nz,sprit2    ;return until all shifts complete
sprit0  pop hl          ;pop screen address from stack
        ld a,(hl)       ;what's there already
        xor c           ;merge in image data
        ld (hl),a       ;place onto screen
        inc l           ;next character cell to the right please
        ld a,(hl)       ;what's there already
        xor d           ;merge with middle bit of image
        ld (hl),a       ;put back onto screen
        inc hl          ;next bit of screen area
        ld a,(hl)       ;what's there
        xor e           ;right edge of sprite image data
        ld (hl),a       ;plonk it on screen
        ld a,(tmp1)     ;temporary vertical coord
        inc a           ;next line down
        ld (tmp1),a     ;store new position
        and 63          ;are we moving to next third of screen
        jr z,sprit4     ;yes so find next segment
        and 7           ;moving into character cell below
        jr z,sprit5     ;tes find next row
        dec hl          ;left 2 bytes
        dec l           ;not straddling 256-byte boundary here
        inc h           ;next row of this character cell
sprit6  ex de,hl        ;screen address in de
        ld hl,(tmp0)    ;restore graphic address
        ex af,af'       ;restore loop counter
        dec a           ;decrement it
        jp nz,sprit1    ;not reached bottom of sprite yet to repeat
        ret             ;job done
sprit4  ld de,30        ;next segment is 30 bytes on
        add hl,de       ;add to screen address
        jp sprit6       ;repeat
sprit5  ld de,63774     ;minus 1762
        add hl,de       ;subtract 1762 from physical screen address
        jp sprit6       ;rejoin loop




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

; calculate the address of attribute for character at (dispx,dispy)
atadd   ld a,c  ;vertical coord
        rrca    ;multiply by 32
        rrca    ;shift right with carry 3 times is
        rrca    ;faster than shifting left 5 times
        ld e,a
        and 3
        add a,88    ;88 x 256 = address of attributes
        ld d,a
        ld a,e
        and 224
        ld e,a
        ld a,b  ;horizontal position
        add a,e
        ld e,a  ;de = address of attributes
        ld a,(de)   ;return with attribute in accumulator
        ret


; ;scoring
; shwnum  ld a,48 ;(or 32) ;
;         ld de,10000
;         call shwdg
;         ld de,1000
;         call shwdg
;         ld de,100
;         call shwdg
;         ld de,10
;         call shwdg
;         or 16
;         ld de,1
; shwdg   and 48
; shwdg1  sbc hl,de
;         jr c,shwdg0
;         or 16
;         inc a
;         jr shwdg1
; shwdg0  add hl,de
;         push af
;         rst 16
;         pop af
;         ret 

;; score method 2
score defb '2000'
uscor   ld a,(hl)
        add a,b
        ld (hl),a
        cp 58
        ret c
        sub 10
        ld (hl),a
uscor0  dec hl
        inc (hl)
        ld a,(hl)
        cp 58
        ret c
        sub 10
        ld (hl),a
        jp uscor0

        ;add 250 to score
        ; ld hl,score+3
        ; ld b,2
        ; call uscor
        ; ld hl,score+4
        ; ld b,5
        ; call uscor

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