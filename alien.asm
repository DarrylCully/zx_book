ENTRY_POINT equ 32768


    org ENTRY_POINT

     ld a,71 ;white ink(7) black paper (0) bright (64)
    ld (23693),a ;set screen colours
   ; xor a ; load acc to zero
    call 0xdaf      ;cls // 3503
    call 8859 ;set border colours

        
mloop   call gpatts ; get player attributes.
        defb 17,239,41 ; remove green paper, add blue paper + ink.
        halt            ;wait for tv beam
        ld ix,entab     ;point to odd spaceships
        call mship      ;move spaceships
        
        halt      
        ld ix,entab+4   ;point to even spaceships
        call mship      ;move even spaceships
        call gwave      ;generate fresh wave

        call blkcar 
        jp mloop        ;back to start of loop

blkcar call gpatts ; get player attributes.
        defb 17,232,40 ; remove red paper/blue ink, add blue paper.
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
        ld de,4
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

gpatts ld bc,(xcoord) ; player coordinates.


dship   ;ld a,67         ; green ink (4) black paper (0) bright (64)
        ;ld (23695),a    ; set temporary colours
        ld hl,shipg
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

sprite  ld a,(xcoord)    ;draws sprite(hl)
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

xcoord  defb 0
ycoord  defb 0
tmp0    defw 0
tmp1    defb 0


entab   defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0
        defb 0,0,0,0



    end ENTRY_POINT