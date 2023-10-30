;==============================================================
; Palette value definitions, for easy reference, plus some
; nice names for some (incomplete)
;==============================================================
.ENUM $00
clBlack:	   .db
	clRGB000:	db
clDarkestRed:  .db
	clRGB100:	db
clDarkRed:	   .db
	clRGB200:	db
clRed:		   .db
	clRGB300:	db
clDarkestGreen: .db
	clRGB010:	db
	clRGB110:	db
	clRGB210:	db
	clRGB310:	db
clDarkGreen:   .db
	clRGB020:	db
	clRGB120:	db
clDarkYellow:  .db
	clRGB220:	db
	clRGB320:	db
clGreen:	   .db
	clRGB030:	db
	clRGB130:	db
	clRGB230:	db
clYellow:	   .db
	clRGB330:	db
clDarkestBlue: .db
	clRGB001:	db
clDarkestPurple: .db
	clRGB101:	db
	clRGB201:	db
	clRGB301:	db
	clRGB011:	db
clDkGrey:	   .db
	clRGB111:	db
	clRGB211:	db
clPaleRed:	   .db
	clRGB311:	db
	clRGB021:	db
	clRGB121:	db
	clRGB221:	db
	clRGB321:	db
	clRGB031:	db
	clRGB131:	db
	clRGB231:	db
	clRGB331:	db
clDarkBlue:	   .db
	clRGB002:	db
	clRGB102:	db
	clRGB202:	db
	clRGB302:	db
	clRGB012:	db
	clRGB112:	db
	clRGB212:	db
	clRGB312:	db
	clRGB022:	db
	clRGB122:	db
clLtGrey:	   .db
	clRGB222:	db
clPink:		   .db
	clRGB322:	db
	clRGB032:	db
	clRGB132:	db
	clRGB232:	db
	clRGB332:	db
clBlue:		   .db
	clRGB003:	db
	clRGB103:	db
	clRGB203:	db
clMagenta:	   .db
	clRGB303:	db
	clRGB013:	db
	clRGB113:	db
	clRGB213:	db
	clRGB313:	db
	clRGB023:	db
	clRGB123:	db
	clRGB223:	db
	clRGB323:	db
clCyan:		   .db
	clRGB033:	db
	clRGB133:	db
	clRGB233:	db
clWhite:	   .db
	clRGB333:	db
.ENDE


;==============================================================
; Palette loader
; Parameters:
; hl = location
; b  = number of values to write
; c  = palette index to start at (<32)
;==============================================================
.section "Palette loader" FREE
LoadPalette:
	push af
	push bc
	push hl
	    ld a,c
	    out ($bf),a     ; Palette index
	    ld a,$c0
	    out ($bf),a     ; Palette write identifier
	    ld c,$be
	    otir            ; Output b bytes starting at hl to port c
	pop hl
	pop bc
	pop af
    ret
.ends

;==============================================================
; Set VRAM address to hl
;==============================================================
.section "VRAM address to hl" FREE
VRAMToHL:
    push af
        ld a,l
        out ($BF),a
        ld a,h
        or $40
        out ($BF),a
    pop af
    ret
.ends

;==============================================================
; Write ASCII text pointed to by hl to VRAM
; Stops when it finds a null byte, skips control characters,
; understands \n
; Pass name table address in iy, it will be modified
;==============================================================
.section "Write ASCII" FREE
VRAMToIY:
    push hl
    push iy
    pop hl
    call VRAMToHL
    pop hl
    ret

WriteASCII:
    push af
    push bc
    push hl
        call VRAMToIY
    	_WriteTilesLoop:
		    ld a,(hl)	; Value to write
		    cp $00		; compare a with $00, set z flag if they match
		    jp z,_WriteTilesLoopEnd	; if so, it's the string end so stop writing it
		    cp 10		; Check for LF
		    jp z,_NewLine
		    sub $20
            jp c,_SkipControlChar
		    out ($BE),a	; Output to VRAM address, which is auto-incremented after each write
		    ld a,%00000000
		    push hl
		    pop hl  ; delay
		    out ($BE),a
            _SkipControlChar:
		    inc hl
		    jp _WriteTilesLoop
	    _NewLine:
		    ; Go to the next line, ie. next multiple of 32=$20
            push hl
                push iy
                pop hl
                ld bc,64
                add hl,bc
                push hl
                pop iy
                call VRAMToIY
            pop hl
            _NoNewLine:
		    inc hl
		    jp _WriteTilesLoop
	    _WriteTilesLoopEnd:
    pop hl
    pop bc
    pop af
    ret
.ends


;==============================================================
; Number writer
; Writes hex byte in a to the screen
;==============================================================
.section "Write a to screen" FREE
WriteNumber:    ; writes hex byte in a to VRAM
    push af
    push bc
        ld b,a      ; back up a
        ; Strip to digits:
        ; Digit 1
        srl a
        srl a
        srl a
        srl a
        call WriteDigit
        ; Digit 2
        ld a,b
        and $f
        call WriteDigit
    pop bc
    pop af
    ret
WriteNumberEx:    ; writes the hex byte in a in a position unique to its value
    push bc
    push hl
        ld b,$00
        ld c,a
        ld hl,NameTableAddress
        add hl,bc
        add hl,bc
        add hl,bc
        add hl,bc
        call WriteNumber
    pop hl
    pop bc
    ret

WriteDigit:     ; writes the digit in a
    cp $0a      ; compare it to A - if it's less then it's 0-9
    jp c,IsNum
        add a,$07   ; if it's >9 then make it point at A-F
    IsNum:
    add a,$10

    out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
    ld a,%00000000
    out ($BE),a
    ret

WriteSpace:
    push af
        ld a,0
        out ($be),a
        out ($be),a
    pop af
    ret
.ends

;==============================================================
; Tile loader
; Parameters:
; hl = tile number to start at
; ix = location of tile data
; bc = No. of tiles to load
; d  = bits per pixel
;==============================================================
.section "Tile loader" FREE
LoadTiles:
    push af
    push bc
    push de
    push hl
    push ix
        ; Tell VDP where I want to write (hl<<5)
        sla l
        rl h
        sla l
        rl h
        sla l
        rl h
        sla l
        rl h
        sla l
        rl h
        ld a,l
        out ($bf),a
        ld a,h
        or $40
        out ($bf),a

        ; I need to output bc*8 bytes so I need to modify bc (<<3)
        sla c
        rl b
        sla c
        rl b
        sla c
        rl b

        ; Write data
        _Loop:
            ; Restore counter
            ld e,d

            _DataLoop:
                ; Write byte
                ld a,(ix+0)
                out ($be),a
                dec e
                inc ix
                jp nz,_DataLoop

            ; Restore counter
            ld e,d
            _BlankLoop:
                ; Write blank data to fill up the rest of the tile definition
                inc e
                ld a,e
                cp 5
                jp z,_NoMoreBlanks

                ld a,0
                out ($be),a
                jp _BlankLoop

            _NoMoreBlanks:

            dec bc
            ld a,b
            or c
            jp nz,_Loop

    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret
.ends

;==============================================================
; Image loader (bytes)
; Parameters:
; b  = width  (tiles)
; c  = height (tiles)
; ix = location of tile number data (bytes)
; iy = name table address of top-left tile
; Sets all tile flags to zero.
;==============================================================
.section "Draw image" FREE
DrawImageBytes:
    push af
    push bc     ; Width, height
    push de     ; Width, height counters
    push hl     ; h = high byte
    push ix     ; ROM location
    push iy     ; VRAM location
        _DrawRow:
            call VRAMToIY     ; Move to the right place
            ld d,b                  ; no. of tiles to loop through per row
            _DrawTile:
                ld a,(ix+0)
                out ($be),a
                ld a,h
                out ($be),a
                inc ix
                dec d
                jp nz,_DrawTile

            ld de,64                ; Move name table address
            add iy,de

            dec c
            jp nz,_DrawRow
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret
.ends

.section "Turn off screen" FREE
TurnOffScreen:
    push af
        ld a,%10000100  ; 28 line mode
        out ($bf),a
        ld a,$81
        out ($bf),a
    pop af
    ret
.ends

.section "Get VCount" FREE
;==============================================================
; V Counter reader
; Waits for 2 consecuitive identical values (to avoid garbage)
; Returns in a *and* b
;==============================================================
GetVCount:  ; returns scanline counter in a and b
    in a,($7e)  ; get value
    _Loop:
    ld b,a      ; store it
    in a,($7e)  ; and again
    cp b        ; Is it the same?
    jr nz,_Loop ; If not, repeat
    ret         ; If so, return it in a (and b)
.ends

.section "PAL/NTSC detection" FREE
;==============================================================
; Speed detector
; Returns a=1 for PAL, 0 for NTSC
; MUST have standard screen size (not stretched)
;==============================================================
IsPAL:
    push bc
        call _GetNumber
        ld bc,$02d2
        sbc hl,bc       ; halfway between lowest and highest values found :P
    pop bc
    jp c,_IsNTSC
    ld a,1
    ret
    _IsNTSC:
    ld a,0
    ret

_GetNumber: ; returns counter in hl
    _WaitForLine1:
        call GetVCount
        cp $01
        jp nz,_WaitForLine1

    ; Line 1, let's start counting while checking for line 0
    ld hl,$0000
    _WaitForLine0:
        inc hl
        call GetVCount
        cp $00
        jp nz,_WaitForLine0
    ret
.ends

.section "Japanese/Export detection" FREE
;==============================================================
; Region detector
; Returns a=1 for Japanese, 0 for export
; I got conflicting/not working information from various
;  sources so I disassembled a game and ripped out its routine.
;==============================================================
IsJapanese:
    ; Relevant ports:
    ; 3f: 76543210  Joypad port output
    ;     |||||||`- P1 B2 input enable
    ;     ||||||`-- P1 TH input enable
    ;     |||||`--- P2 B2 input enable
    ;     ||||`---- P2 TH input enable
    ;     |||`----- P1 B2 output
    ;     ||`------ P1 TH output
    ;     |`------- P2 B2 output
    ;      `------- P2 TH output
    ;   Explanation:
    ;   If the input bit is 1, the connection is an input as normal.
    ;   If it's 0 then the output bit is output by the connection.
    ;   Output is inverted when read back on Japanese systems.
    ; dd: 76543210  Joypad port input 2
    ;     || ||||`- P1 L
    ;     || |||`-- P1 R
    ;     || ||`--- P2 B1
    ;     || |`---- P2 B2
    ;     || `----- Reset
    ;     |`------- P1 TH
    ;      `------- P2 TH
    ;   Important: active low, so 1 = off

    ; Test 1:
    ld a,%11110101  ; Set both TH to output and output 1s
    out ($3f),a
    in a,($dd)
    and  %11000000  ; See what the TH inputs are
    cp   %11000000  ; If the bits have been inverted then it's definitely Japanese
    jp nz,_IsJap

    ld a,%01010101  ; Set both TH to output and output 0s
    out ($3f),a
    in a,($dd)
    and  %11000000  ; See what the TH inputs are
    jp nz,_IsJap    ; If the bits have been inverted then it's definitely Japanese

    ld a,%11111111  ; Set everything back to being inputs
    out ($3f),a

    ld a,0
    ret

    _IsJap:
    ld a,1
    ret
.ends

.section "Wait for VBlank without interrupts" FREE
WaitForVBlankNoInt:
    push bc
    push af
    _Loop:
        call GetVCount
        cp $c1
        jp nz,_Loop
    pop af
    pop bc
    ret
.ends

.section "Wait for c frames" FREE
WaitForCFrames: ; Waits for c frames
    push af
    push bc
        _DecLoop:
            _Loop1:
            call GetVCount
            cp $00
            jp nz,_Loop1

            _Loop2:
            call GetVCount
            cp $c1
            jp nz,_Loop2
    
            dec c
            jp nz,_DecLoop
    pop bc
    pop af
    ret
.ends

;==============================================================
; Sprite disabler
; Sets all sprites' Y-position to 208, thereby stopping display
; of any more sprites than I've used
;==============================================================
.section "No sprites" FREE
NoSprites:
    push af
    push bc
    push hl
        ld bc,64    ; how many sprites
        ld hl,SpriteTableAddress
        call VRAMToHL
      -:ld a,208+16 ; for 28-line mode
        out ($be),a
        dec bc
        ld a,b
        or c
        jp nz,-
    pop hl
    pop bc
    pop af
    ret
.ends

;==============================================================
; Clear VRAM
;==============================================================
.section "Clear VRAM" SEMIFREE
ClearVRAM:
    push hl
    push bc
    push af
        ld hl,0
        call VRAMToHL
        ld bc, $4000    ; Counter for 16KB of VRAM
        ld a,$00        ; Value to write
        _Loop:
            out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
            dec c
            jp nz,_Loop
            dec b
            jp nz,_Loop
    pop af
    pop bc
    pop hl
    ret
.ends

.section "Clear name table" FREE
ClearNameTable:
    push hl
    push bc
    push af
        ld hl,NameTableAddress
        call VRAMToHL
        ld bc,$700      ; for unstretched screens only!
        ld a,$00        ; Value to write
        -:
            out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
            dec c
            jp nz,-
            dec b
            jp nz,-
    pop af
    pop bc
    pop hl
    ret
.ends

.section "Hex to BCD" FREE
;==============================================================
; Hex to BCD convertor
; Inputs:  hex byte in a, should be <0x63
; Outputs: BCD byte in a
; Found in the middle of a Usenet flame war about whether daa
; can be used to do such a conversion :)
;==============================================================
Hex2BCD:
    push bc
        ld b,a  ; Original (hex) number
        ld c,8  ; How many bits
        ld a,0  ; Output (BCD) number, starts at 0
        _Hex2BCDLoop:
            sla b   ; shift b into carry
            adc a,a
            daa     ; Decimal adjust a, so shift = BCD x2 plus carry
            dec c   ; Repeat for 8 bits
            jp nz,_Hex2BCDLoop
    pop bc
    ret
.ends
