; SMS VGM player
; by Maxim

; VRAM mapping stuff
.define SpriteSet           1       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes

.define DoSplashScreen 1
.define DoUnnecessaryDetection 0

;==============================================================
; WLA-DX banking setup
; Note that this is a frame 2-only setup, allowing large data
; chunks in the first 32KB.
;==============================================================
.memorymap
DEFAULTSLOT 0
SLOTSIZE $8000
SLOT 0 $0000
SLOTSIZE $4000
SLOT 1 $8000
.ENDME
.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE $8000
BANKS 1
;BANKSIZE $4000
;BANKS 1
.ENDRO

.bank 0 slot 0
.org $0000

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.45,"SMS VGM player",SDSCNotes,"Maxim"
.section "SDSC notes" FREE
SDSCNotes:
;    123456789012345678901234567890123456789012345678901234567890123
.db "Beta version - by whatever means necessary, this software must"
.db " NOT appear on any rom tool's 'needed' list. Please note that"
.db " only the first 32KB is the actual program, and any data after"
.db " that is appended VGM music data.",0
.ends

;==============================================================
; Memory usage
;==============================================================
.enum $c000
VGMMemoryStart  ds 256
ButtonState     db
LastButtonState db
VisBuffer       ds 64
VisNumber       db
IsPalConsole    db
;.if DoUnnecessaryDetection == 1
IsJapConsole    db
FMChipDetected  db
;.endif
VBlankRoutine   dw
VisRoutine          dw  ; Routine to calculate vis
VisDisplayRoutine   dw  ; Routine to call in VBlank to update vis
VisChanged      db      ; Flag to signal that the vis needs to be initialised
SecondsChanged  db
LoopsChanged    db
PaletteChanged  db
PaletteNumber   db
RandomSR        dw  ; Random number generator shift register, needs seeding
.ENDE

;Useful defines and macros:
.include "graphics.inc"

.org $0000
;==============================================================
; Boot section
;==============================================================
.section "!Boot section" FORCE   ; Standard stuff (for the SMS anyway)
    di              ; disable interrupts (re-enable later)
    im 1            ; Interrupt mode 1
    ld sp, $dff0    ; load stack pointer to not-quite-the-end of user RAM (avoiding paging regs)
    jp main         ; jump to main program

    .db "Vis: "
    InitialVisNumber:
    .db 0
    .db " Colour: "
    InitialColourNumber:
    .db 0
.ends

.org $0038
;==============================================================
; Interrupt handler
;==============================================================
.section "!Interrupt handler" FORCE
    in a,($bf)      ; satisfy interrupt
    ; No checking of the value because I know I only have VBlank interrupts
    ld hl,(VBlankRoutine)
    call callHL
    reti

callHL:
    jp (hl)

NoVBlank:
    ret

VGMPlayerVBlank:
    call CheckInput ; Read input into memory
    call ShowTime   ; Update time display
    call ShowLoopNumber

    ld a,(PaletteChanged)
    cp 1
    call z,CyclePalette ; must do CRAM writes in VBlank

    ld a,(VisChanged)
    cp 1
    call z,InitialiseVisRoutine ; If the vis has changed then I need to do the initialisation in the vblank

    ld hl,(VisDisplayRoutine)   ; Draw vis
    call callHL

    ret
.ends

.org $0066
;==============================================================
; Pause button handler
;==============================================================
.section "!NMI handler" FORCE
    ; Debug resetter
;    jp $0000

    ; Dodgy PAL/NTSC speed switch
    push hl
    push de
        call VGMGetSpeed
        ld de,735
        and $ff
        sbc hl,de
        jp z,_ChangeToPAL
        ld hl,735
        jp _SetSpeed
        _ChangeToPAL:
        ld hl,882
        _SetSpeed:
        call VGMSetSpeed
    pop de
    pop hl
    retn
.ends


;==============================================================
; Main program
;==============================================================
main:
    ld a,$02
    ld ($ffff),a

    ; Load VDP with default values, thanks to Mike G :P
    ; hl = address of data
    ; b = size of data
    ; c = port
    ; otir = while (b>0) do {out (hl),c; b--}
    ld hl,VdpData
    ld b,VdpDataEnd-VdpData
    ld c,$bf
    otir

    call IsPAL
    ld (IsPalConsole),a
.if DoUnnecessaryDetection == 1
    call IsJapanese
    ld (IsJapConsole),a
    call HasFMChip
    ld (FMChipDetected),a
.endif

    ; Startup screen
.if DoSplashScreen == 1
    call ClearVRAM
    call DisplayVGMLogo
    call TurnOffScreen
.endif
    call ClearVRAM
    call NoSprites

    ; Load palette
    ld hl,PaletteData
    ld b,(PaletteDataEnd-PaletteData)
    ld c,0
    call LoadPalette

    ; Load tiles
    ld hl,0         ; Load font
    ld ix,TileData
    ld bc,$5f+1
    ld d,3
    call LoadTiles

    ld hl,$60       ; Load vis tiles
    ld ix,ScaleData
    ld bc,9
    ld d,2
    call LoadTiles

    .define BigNumbersOffset 105
    ld hl,BigNumbersOffset  ; Load big numbers
    ld ix,BigNumbers
    ld bc,41
    ld d,3
    call LoadTiles

    ld hl,146
    ld ix,Pad       ; Load pad image
    ld bc,$6d+1
    ld d,4
    call LoadTiles

    ld hl,256
    ld ix,PianoTiles
    ld bc,10
    ld d,3
    call LoadTiles

    .define NumSprites 4+7
    ld hl,$1c0-NumSprites
    ld ix,Sprites
    ld bc,NumSprites
    ld d,3
    call LoadTiles

    ; Initial button state values (all off)
    ld a,$ff
    ld (LastButtonState),a
    ld (ButtonState),a

    ; Draw text
    ld iy,NameTableAddress
    ld hl,TextData
    call WriteASCII

    ; Draw pad image
    ld bc,$0c0b     ; 12x11
    ld ix,PadData
    ld iy,NameTableAddress+2*20
    ld h,0
    call DrawImageBytes

    ; Put something in the shift register
    ld hl,$0210 ; the current time (am)
                ; a better seed would be... better. eg. a counter
    ld (RandomSR),hl

    ; Load settings, initialise stuff
    call LoadSettings
    call CyclePalette
    call InitialiseVis

    ; Reset VGM player
    call VGMInitialise

.if DoUnnecessaryDetection == 1
    ; Debug: show detected information
    ld hl,NameTableAddress+2*(32*27+4)
    call VRAMToHL
    ld a,(IsJapConsole)
    call WriteNumber
    ld hl,NameTableAddress+2*(32*27+11)
    call VRAMToHL
    ld a,(IsPalConsole)
    call WriteNumber
    ld hl,NameTableAddress+2*(32*27+17)
    call VRAMToHL
    ld a,(FMChipDetected)
    call WriteNumber
.endif

    ; Set VBlank routine
    ld hl,VGMPlayerVBlank
    ld (VBlankRoutine),hl

    ; Fake a VBlank to initialise various stuff
    call VGMPlayerVBlank

    ; Turn screen on
    ld a,%11110010
;         ||||| |`- Zoomed sprites -> 16x16 pixels
;         ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;         ||||`---- 30 row/240 line mode
;         |||`----- 28 row/224 line mode
;         ||`------ VBlank interrupts
;         |`------- Enable display
;         `-------- Must be set (VRAM size bit)
    out ($bf),a
    ld a,$81
    out ($bf),a

    InfiniteLoop:   ; to stop the program
        call VGMUpdate      ; Write sound data
        call ProcessInput   ; Process input from the last VBlank
        ld hl,(VisRoutine)  ; Update vis data
        call callHL

        ei
        halt
        jp InfiniteLoop

;==============================================================
; VGM offset to SMS offset convertor
; Inputs:
; a = offset of dword in VGM file
; Outputs:
; a = page number ($02+) *
; hl = offset ($8000-$bfff)
; * If dword is zero then a will be zero to show that
;==============================================================
.section "VGM to SMS offset" SEMIFREE
VGMOffsetToPageAndOffset:
    push ix
    push bc
    push de
        ; Remember the offset value in c becasue I need to use a
        ld c,a

        ; Page in the first page, remembering the current page on the stack
        ld a,($ffff)
        push af
        ld a,$02
        ld ($ffff),a

        ; offset of dword in .sms -> ix
        ld b,$80
        push bc
        pop ix

        ; Value stored there -> dehl
        ld l,(ix+0)
        ld h,(ix+1)
        ld e,(ix+2)
        ld d,(ix+3)

        ; See if it's zero
        ; If it is, h|l|d|e=0
        ld a,h
        or l
        or d
        or e
        jp nz,_NonZero
        ld c,$00
        jp _end

        _NonZero:
        ; Add offset, ie. dehl + c -> dehl
        ld b,$00
        and $ff     ; reset carry
        adc hl,bc
        ld bc,$0000 ; I've finished with c now
        push hl
            ld h,d
            ld l,e
            adc hl,bc
            ld d,h
            ld e,l
        pop hl

        ; Figure out which page it's on and store that in c now
        ; ddddddddeeeeeeeehhhhhhhhllllllll
        ; Page -----^^^^^^^^
        ld b,e
        sla b
        sla b
        ld a,h
        rlc a
        rlc a
        and %00000011
        or b
        add a,2
        ld c,a

        ; And then get the offset by modding by $4000 and adding $8000
        ; Do this by hl = (hl & 0x3fff) | 0x8000
        ; Quite neat in asm :P
        res 6,h
        set 7,h

        _end:
        ; Restore original page
        pop af
        ld ($ffff),a

        ; Output a=page number
        ld a,c
    pop de
    pop bc
    pop ix
    ret
.ends

.section "Draw GD3" SEMIFREE
MoveHLForwardByA:
    ; Adds A to HL
    ; If it goes over $c000, it handles the paging
    push bc
        ld b,$00
        ld c,a
        add hl,bc
        ld a,h
        and $f0 ; just 1st digit
        cp $c0  ; Is it c?
        jr nz,_OK
        ; Need to decrement by $4000
        res 6,h
        ; and page in the next page
        ld a,($ffff)
        inc a
        ld ($ffff),a
        _OK:
        ld a,c
    pop bc
    ret

MoveHLForward:
    ; Adds 1 to HL
    ; If it goes to $c000, it handles the paging
    inc hl
    push af
        ld a,h
        and $f0 ; just 1st digit
        cp $c0  ; Is it c?
        jr nz,_OK2
        ; Need to decrement by $4000
        res 6,h
        ; and page in the next page
        ld a,($ffff)
        inc a
        ld ($ffff),a
        _OK2:
    pop af
    ret

;==============================================================
; GD3 displayer
; Uses 34 bytes of RAM to buffer the text
;==============================================================
.define GD3DisplayerBuffer $c100
DrawGD3Tag:
    push hl
    push de
    push bc
    push af
        ; Page in first page
        ld a,$02
        ld ($ffff),a
        ld a,$14
        call VGMOffsetToPageAndOffset   ; a = page or 0, hl = offset

        ; See if it's come out as zero = no tag
        cp $00
        jr z,_NoGD3

        ; Page it in
        ld ($ffff),a

        ; Move to first string
        ld a,12
        call MoveHLForwardByA

        ld a,2
        call _DrawGD3String ; Title
        call _SkipGD3String
        call _DrawGD3String ; Game
        call _SkipGD3String
        call _DrawGD3String ; System
        call _SkipGD3String
        call _DrawGD3String ; Author

        jp _end

        _NoGD3:
        ; Say so
        ld hl,NoTagString
        call WriteASCII

        _end:
    pop af
    pop bc
    pop de
    pop hl
    ret
NoTagString:
.db 32,10,"         - No GD3 tag -",10,32,10,32,10,0

_SkipGD3String:
; Move hl forward until after the next zero word
    push bc
    push af
        _SkipLoop:
        ld c,(hl)
        call MoveHLForward
        ld b,(hl)
        call MoveHLForward

        ld a,b
        or c
        jp nz,_SkipLoop
    pop af
    pop bc
    ret

_DrawGD3String:
; Copy string from hl to RAM, (badly) converting from Unicode as I go
; Copy a maximum of MaxLength chars, terminate with \n\0
; Then draw to the screen
.define MaxLength 32                ; GD3 line length
    push af
    push de
    push bc
        ld de,GD3DisplayerBuffer    ; Where to store ASCII
        ld a,MaxLength
        _Loop:
            ; Get value into bc
            ld c,(hl)
            call MoveHLForward
            ld b,(hl)
            call MoveHLForward
            push hl
                ld h,b  ; Put it in hl
                ld l,c
                ; See if value is acceptable
                ld bc,$0000
                and $ff
                sbc hl,bc   ; If it's zero, it's the end
                jp z,_LoopEnd
                ld bc,128
                sbc hl,bc   ; If it's outside 7-bit ASCII, write a question mark
                jp c,_Acceptable
                    ; Not acceptable:
                    ld l,$bf    ; = '?'
                _Acceptable:
                push af
                    ld bc,128    ; Put it back to the right value
                    add hl,bc
                    ld a,l
                    ; Finally copy it to RAM
                    ld h,d
                    ld l,e
                    ld (hl),a
                    inc de
                pop af
                ; Fall through to next char
            _NextChar:
                dec a   ; decrement counter
                jp nz,_LoopAgain
                ; If it gets to zero:
                pop hl
                    call z,_SkipGD3String   ; go to the end of the string
                push hl
                jp z,_LoopEnd           ; finish
            _LoopAgain:
            pop hl
            jp _Loop
            _LoopEnd:
                ld h,d
                ld l,e
                cp 32           ; If first char is zero
                jr nz,_NotZero
                    ld (hl),' ' ; put a space so it'll work properly
                    inc hl
                _NotZero:
                ld (hl),$0a
                inc hl
                ld (hl),$00
                ld hl,GD3DisplayerBuffer
                call WriteASCII
            pop hl
    pop bc
    pop de
    pop af
    ret
.ends

;==============================================================
; Time displayer
;==============================================================
ShowTime:
.define TimeDisplayPosition NameTableAddress+2*(32*6+2)
    push af
        ld a,(SecondsChanged)
        cp 0
        jp z,+
    push bc
    push de
    push hl
        ; get digits
        ld de,(VGMTimeMins)     ; d = sec  e = min
        ; Set start name table address
        ld hl,TimeDisplayPosition
        ; Output digit(s)
        ld b,e
        call DrawByte
        ld bc,6     ; \ 5 cycles compared to
        add hl,bc   ; / 6 for 6x inc hl
        ld b,d
        call DrawByte
        ld a,0
        ld (SecondsChanged),a
    pop hl
    pop de
    pop bc
    +:
    pop af
    ret

;==============================================================
; Loop count displayer
;==============================================================
ShowLoopNumber:
.define LoopNumberPosition NameTableAddress+2*(32*6+13)
    push af
        ld a,(LoopsChanged)
        cp 0
        jp z,+
    push bc
    push hl
        ld a,(VGMLoopsPlayed)
        ld b,a
        ld hl,LoopNumberPosition
        call DrawByte
        ld a,0
        ld (LoopsChanged),a
    pop hl
    pop bc
    +:
    pop af
    ret

;==============================================================
; Write 2-digit BCD number in b using large numbers,
; at screen position hl (which is modified)
;==============================================================
DrawByte:
    push af
        ld a,b
        srl a
        srl a
        srl a
        srl a
        call DrawLargeDigit
        ; move to next space
        inc hl
        inc hl
        inc hl
        inc hl
        ld a,b
        and $0f
        call DrawLargeDigit
    pop af
    ret

;==============================================================
; Write lower BCD digit in a using large numbers,
; at screen position hl
;==============================================================
DrawLargeDigit:
    call VRAMToHL
    push bc
        ld c,a
        sla c   ; multiply by 2
    push af
        ld a,c
        add a,BigNumbersOffset
        out ($BE),a
        ld a,$00
        out ($BE),a
        ld a,c
        add a,BigNumbersOffset+1
        out ($BE),a
        ld a,$00
        out ($BE),a
        push de
            ld de,64
            add hl,de
        pop de
        call VRAMToHL
        ld a,c
        add a,BigNumbersOffset+20
        out ($BE),a
        ld a,$00
        out ($BE),a
        ld a,c
        add a,BigNumbersOffset+21
        out ($BE),a
        ld a,$00
        out ($BE),a
        push de
            ld de,64
            sbc hl,de
        pop de
    pop af
    pop bc
    ret

CheckInput: ; VBlank routine, just read it into memory
    in a,($dc)
    ld (ButtonState),a
    ret

ProcessInput:   ; Outside of VBlank, process what was read above
    push hl
    push af
    push bc
        ld a,(LastButtonState)  ; get last value
        ld b,a
        ld a,(ButtonState)      ; get current value
        cp b                    ; compare the two
        jp z,_Finish            ; and don't do anything if it hasn't changed
        ld (LastButtonState),a

        ; See which bits are unset = pressed
        bit 0,a         ; Up
        call z,NextVis
        bit 1,a         ; Down
        call z,VGMStop
/*
        bit 2,a         ; Left
        call z,_Left
*/
        bit 3,a         ; Right
        jp nz,+
        ld hl,PaletteChanged
        ld (hl),1
        +:

        bit 4,a         ; Button 1
        call z,VGMPlayPause

        ld c,a      ; backup value
        xor b       ; XOR a with the last value to get a 1 for each button that has changed, for hold-down buttons
        bit 5,a
        call nz,VGMFastForward  ; Button 2

        _Finish:
    pop bc
    pop af
    pop hl
    ret

CyclePalette:
    ; increment palette number
    ld a,(PaletteNumber)
    inc a
    ; Loop
    cp NumPalettes
    jp nz,+
    ld a,0
  +:ld (PaletteNumber),a
    ; Load palette
    ld hl,Palettes
    ; multiply a by 3 and add it to hl
    ld b,0
    ld c,a
    add a,a
    add a,c
    ld c,a
    add hl,bc

    ld b,3      ; main palette
    ld c,4
    call LoadPalette
    ld b,1      ; border colour
    ld c,31
    call LoadPalette

    ; reset flag
    ld a,0
    ld (PaletteChanged),a

    call SaveSettings

    ret

;==============================================================
; VGM routines
;==============================================================

.section "VGM routines" SEMIFREE
; VGM routines memory mapping:
.ENUM VGMMemoryStart
VGMCounterLocation      dw      ; 00-01 VGM data pointer
VGMWaitTotal            dw      ; 02-03 Wait length total
VGMFrameLength          dw      ; 04-05 Amount to wait per frame (allows fast-forwarding :P)
VGMPagingBuffer         ds 4    ; 06-09 Paging border buffer
VGMLoopPage             db      ; 0a    Loop point page
VGMLoopOffset           dw      ; 0b-0c Loop point offset when paged in
VGMPlayerState          db      ; 0d    Player state - playing, paused, stopped, etc
VGMPSGVolumes           ds 4    ; 0e-11 PSG volumes (channels 0-3)
VGMTimeMins             db      ; 12    Time (mins), BCD
VGMTimeSecs             db      ; 13    Time (secs), BCD
VGMTimeSamples          dw      ; 14-15 Time (samples)
VGMLoopsPlayed          db      ; 16    Number of loops played
VGMPSGTones             ds 6    ; 17-1c PSG tone values
VGMPSGTone1stByte       db      ; 1d    PSG tone first byte
VGMWaitTotalOverflow    db      ; 1e    Kludgy :/
VGMYM2413FNums          ds 12   ; 1f-1a 6 x word
VGMYM2413Blocks         ds 6    ; 1b-20 6 x byte
VGMYM2413Keys           ds 6    ; 21-26 6 x byte - byte is whole byte for reg 0x2n to make muting possible
VGMPSGNoiseIsPeriodic   db      ; 27    Byte flag, if set then noise is periodic/tone2
.ENDE

; Numbers for VGMPlayerState
.define VGMPlaying      1
.define VGMPaused       2
.define VGMStopped      4
.define VGMFast         8+1

; Integer divides HLBC by DE, returns result in BC and remainder in HL
; Found on Usenet
Divide16:
    exx
    ld b,16
    exx
    xor a
    scf
    jr Di2
Di1:
    or a
    sbc hl,de
    sbc a,0
    jr nc,Di2
    ccf
    adc hl,de
    adc a,0
    scf
Di2:
    ccf
    rl c
    rl b
    adc hl,hl
    adc a,0
    exx
    dec b
    exx
    jr nz,Di1
    sbc hl,de
    sbc a,0
    jr nc,Di3
    add hl,de
    scf
Di3:
    ccf
    rl c
    rl b
    ret

IsVGMFileAtOffset:  ; pass offset in ix, uses a, sets z if file found
    push ix
        ld a,(ix+0) ; V
        xor (ix+1)  ; g
        xor (ix+2)  ; m
        xor (ix+3)  ; <space>
        cp $7c
    pop ix
    ret

;==============================================================
; Initialise player/read one-time information
;==============================================================
VGMInitialise:
    push hl
    push af
        ; Check for VGM marker - I'll xor the bytes together and see what I get. Problem: "Vgm " gives the same result as "VGM "
        ld ix,$8000
        call IsVGMFileAtOffset
        jp nz,NoVGMFile

        ; Get lengths
        ld h,(ix+27)    ; Total
        ld l,(ix+26)
        ld b,(ix+25)
        ld c,(ix+24)
        ld de,44100
        call Divide16   ; hl = remainder, bc = number of seconds
        scf
        ld de,22050-1
        sbc hl,de
        jp c,+
        inc bc
        +:
        ld hl,0
        ld de,60
        call Divide16   ; hl = seconds, bc = minutes
        push hl
            ld hl,NameTableAddress+2*(32*9+10)
            call VRAMToHL
        pop hl
        ld a,c
        call Hex2BCD
        call WriteNumber
        ld a,$1a   ; Draw colon (faster than redefining name table address)
        out ($be),a
        ld a,0
        out ($be),a
        ld a,l
        call Hex2BCD
        call WriteNumber

        ; Lazy cut and paste
        ld h,(ix+35)    ; Loop
        ld l,(ix+34)
        ld b,(ix+33)
        ld c,(ix+32)
        ld de,44100
        call Divide16   ; hl = remainder, bc = number of seconds
        scf
        ld de,22050-1
        sbc hl,de
        jp c,+
        inc bc
        +:
        ld hl,0
        ld de,60
        call Divide16   ; hl = seconds, bc = minutes
        push hl
            ld hl,NameTableAddress+2*(32*10+10)
            call VRAMToHL
        pop hl
        ld a,c
        call Hex2BCD
        call WriteNumber
        ld a,$1a   ; Draw colon (faster than redefining name table address)
        out ($be),a
        ld a,0
        out ($be),a
        ld a,l
        call Hex2BCD
        call WriteNumber

        ; Get loop information
        ld a,$1c
        call VGMOffsetToPageAndOffset   ; now a=page/0, b=offset
        ld (VGMLoopPage),a
        ld (VGMLoopOffset),hl

        ; Draw GD3 tag
        push iy
            ld iy,NameTableAddress+64*19  ; GD3 location
            call DrawGD3Tag
        pop iy

        ; Initialise playback speed
        ld a,(IsPalConsole)
        cp $01
        jp z,_IsPal
        ld hl,735
        jp _SetSpeed
        _IsPal:
        ld hl,882
        _SetSpeed:
        ld (VGMFrameLength),hl

        ; Do multiple-time initialisations
        call _Stop
    pop af
    pop hl
    ret

; hl contains the new frame length
VGMSetSpeed:
    ld (VGMFrameLength),hl
    ret    

; returns speed in hl
VGMGetSpeed:
    ld hl,(VGMFrameLength)
    ret

; Internal playback control routines
_Play:
    push af
    ld a,VGMPlaying
    ld (VGMPlayerState),a
    pop af
    ret

_Pause:
    push af
        ld a,VGMPaused
        ld (VGMPlayerState),a
    pop af
    call MuteAllSound
    ret

_Stop:
    push hl
    push af
        ld a,VGMStopped
        ld (VGMPlayerState),a

        ; Switch to first page
        ld a,2
        ld ($ffff),a

        ; Move pointer to the start of the VGM data
        ld hl,$8040
        ld (VGMCounterLocation),hl

        ; Set various stuff to initial values:
        ld hl,$0000
        ld a,$00
        ld (VGMWaitTotal),hl        ; Number of samples to wait for next read
        ld (VGMWaitTotalOverflow),a
        ld (VGMTimeMins),a          ; Time
        ld (VGMTimeSecs),a
        ld (VGMTimeSamples),hl
        ld (VGMLoopsPlayed),a       ; Number of loops
        ld a,1
        ld (SecondsChanged),a       ; Trigger a time display update
        ld (LoopsChanged),a
        
        ld hl,$ffff                 ; Volumes -> off
        ld (VGMPSGVolumes),hl
        ld (VGMPSGVolumes+2),hl

        ld hl,VGMYM2413Keys         ; Turn off keys -> stop vis showing it
        ld a,0
        ld c,6
      -:ld (hl),a
        inc hl
        dec c
        jp nz,-
    pop af
    pop hl
    call MuteAllSound
    ret

_Fast:
    push af
    ld a,VGMFast
    ld (VGMPlayerState),a
    pop af
    ret

MuteAllSound:
    push af
        ; PSG:
        ld a,%10011111
        out ($7f),a
        ld a,%10111111
        out ($7f),a
        ld a,%11011111
        out ($7f),a
        ld a,%11111111
        out ($7f),a
        ; YM2413: turn off keys - reg to f0, val to f1
        push bc
        push hl
            ld b,$20
            ld c,6
            ld hl,VGMYM2413Keys
            -:
            ld a,b
            out ($f0),a
            ld a,(hl)
            res 4,a
            out ($f1),a
            inc b
            inc hl
            dec c
            jp nz,-
        pop hl
        pop bc
    pop af
    ret

RestoreVolumes:
    ; Write out volumes stored in memory
    push hl
    push bc
        ld hl,VGMPSGVolumes ; where to copy from
        ld c,$7f            ; where to output to
        ld b,4              ; how many to output
        otir

        ld hl,VGMYM2413Keys
        ld b,$20
        ld c,6
        -:
        ld a,b
        out ($f0),a
        ld a,(hl)
        res 4,a
        res 5,a
        out ($f1),a
        inc hl
        inc b
        dec c
        jp nz,-
    pop bc
    pop hl
    ret

VGMPlayPause:
    push af
        ld a,(VGMPlayerState)
        cp VGMPlaying   ; if it's playing then pause
        call z,_Pause
        cp VGMPaused    ; if it's paused then play
        call z,RestoreVolumes
        call z,_Play
        cp VGMStopped   ; if it's stopped then play from the start
        call z,_Play
    pop af
    ret

; Stop playback
VGMStop:
    call _Stop
    ret

; Toggle 4x speed
VGMFastForward:
    push af
        ld a,(VGMPlayerState)
        cp VGMPlaying
        call z,_Fast
        cp VGMFast
        call z,_Play
    pop af
    ret

; Move to loop location
; regs as in VGMUpdate
VGMDoLoop:
    push af
        ld a,(VGMLoopPage)  ; page loop is on
        cp $00              ; Is it zero? If so, that means there's no looping
        jp z,_NoLooping
        jp _IsLooping

        _NoLooping:
        ; so I'd better stop
        call _Stop
        jp _EndVGMDoLoop

        _IsLooping:
        ld ($ffff),a            ; Page
        ld bc,(VGMLoopOffset)   ; offset
        ld a,(VGMLoopsPlayed)
        inc a
        daa
        ld (VGMLoopsPlayed),a
        ld (LoopsChanged),a

        _EndVGMDoLoop:
    pop af
    ret

VGMUpdate:
    push af     ; general use
    push bc     ; data location - change to ix?
    push de     ; general use
    push hl     ; wait total

    ld a,(VGMPlayerState)       ; see what state we're in
    and VGMPlaying
    jp z,EndGetDataLoop         ; if we're not playing then exit

    ld bc,(VGMCounterLocation)  ; find where I'm looking
    ld hl,(VGMWaitTotal)        ; get the current amount to wait
    jp DoINeedToWait            ; in case a wait has been left over
    GetData:
        ld a,(VGMPlayerState)       ; see what state we're in
        and VGMPlaying
        jp z,EndGetDataLoop        ; if we're not playing then exit

        ; Debug display of information (location/page)
/*        push af
        push hl
            ld a,b
            ld hl,NameTableAddress+2*(32*27+24)
            call VRAMToHL
            call WriteNumber
            ld a,($ffff);
            ld hl,NameTableAddress+2*(32*27+29)
            call VRAMToHL
            call WriteNumber
        pop hl
        pop af
*/
        ; Paging handler:
        ; First check to see if I've already moved to reading from RAM:
        ld de,VGMPagingBuffer
        push hl
            ld h,b      ; ld hl,bc...
            ld l,c
            and $ff     ; reset carry flag
            sbc hl,de   ; subtract value, if result is negative then it's not in RAM
            ld a,l      ; for comparison if I am in RAM (1)
        pop hl
        jp c,NotInRAM
        ; If I am in RAM then see if I'm more than 2 bytes in
        cp 2            ; (1)
        jp c,ReadData   ; if negative then I'm not there yet
        ; otherwise, move back to ROM
        push hl
            ld de,VGMPagingBuffer-$8000+2
            ld h,b      ; ld hl,bc...
            ld l,c
            and $ff     ; reset carry flag (hmmph! doing this a lot)
            sbc hl,de
            ld b,h      ; ld bc,hl...
            ld c,l
        pop hl
        jp ReadData


        NotInRAM:
        ; If I'm too close to the end of a page (<3 bytes) the next instruction might
        ; go over to the next page:
        push hl
            ld hl,$bffd ; Highest safe value
            ld d,b      ; ld de,bc...
            ld e,c
            and $ff     ; reset carry flag
            sbc hl,de   ; subtract value, if result is negative then it's unsafe
        pop hl
        jp c,DoPagingThing
        jp ReadData

        DoPagingThing:
        ; So, I'll copy the last 2 bytes of this page, and the first 2 bytes of the
        ; next, into RAM and continue from there:
        ld de,($bffe)
        ld (VGMPagingBuffer),de
        ; Now change page:
        ld a,($ffff)
        inc a
        ld ($ffff),a
        ; Now copy the first 2 bytes to my buffer:
        ld de,($8000)
        ld (VGMPagingBuffer+2),de
        ; And move the data pointer to the buffer
        ; It's moving from $c000-n to VGMPagingBuffer+2-n
        push hl
            ld de,VGMPagingBuffer+2-$c000
            ld h,b      ; ld hl,bc...
            ld l,c
            add hl,de
            ld b,h      ; ld bc,hl...
            ld c,l
        pop hl

        ReadData:
        ld a,(bc)   ; read the VGM data
        ; Debug display of information
;        call WriteNumberEx
        cp $4f      ; GG st
        jp z,GameGearStereo
        cp $50      ; PSG
        jp z,PSG
        cp $61      ; wait n
        jp z,WaitNSamples
        cp $62      ; wait 1/60
        jp z,Wait160th
        cp $63      ; wait 1/50
        jp z,Wait150th
        cp $66      ; End of file
        jp z,EndOfFile
        cp $51      ; YM2413
        jp z,YM2413
        and $50
        cp $50      ; YMxxxx
        jp z,YMxxxx
        inc bc
        jp ReadData  ; for invalid data

    GameGearStereo: ; discard
        inc bc
;        ld a,(bc)
;        out ($06),a ; output stereo data :( crashes a real SMS(2)
        inc bc
        jp GetData

    PSG:
        inc bc
        ld a,(bc)   ; get data
        out ($7f),a ; output it

        ; Data analysis:
        push bc
            ; Is it a volume write?
            ld b,a  ; b = backup of data written
            and %10010000   ; volume write bitmask
            cp  %10010000
            ld a,b  ; restore
            jp nz,_NotVolume
            ; It is a volume write
            ; Channel = bits 5 and 6
            srl a
            srl a
            srl a
            srl a
            srl a
            and %00000011
            ld c,a  ; c = channel
          +:push hl
                ld hl,VGMPSGVolumes
                ld e,c
                ld d,$00
                add hl,de
                ld (hl),b
            pop hl

            _NotVolume:

            ; Restore data
            ld a,b
            bit 7,a         ; Is it a tone2?
            jp z,_Tone2
            and %10010000   ; Is it a tone1?
            cp  %10000000
            jp z,_Tone1
            ld a,$ff        ; reset VGMPSGTone1stByte if it's not a tone write
            ld (VGMPSGTone1stByte),a

            jp _ToneEnd

            _Tone1:         ; Tone1
                ld a,b
                ld (VGMPSGTone1stByte),a
                
                and %01100000   ; Is it noise?
                cp  %01100000
                jp nz,+++
                ld a,b
                and %11110111
                cp  %11100011   ; Is it periodic tone2?
                jp z,+
                ld a,0
                jp ++
                +:
                ld a,1
                ++:
                ld (VGMPSGNoiseIsPeriodic),a
                +++:
                jp _ToneEnd

            _Tone2:         ; Tone2
                ld a,(VGMPSGTone1stByte)    ; Look at 1st byte
                srl a           ; Extract channel number
                srl a
                srl a
                srl a
                srl a
                and %00000011
                cp 3            ; If it's channel 3 then exit
                jp z,_ToneEnd
                ld c,a          ; c = channel
                push de
                    ; Extract frequency
                    ld a,(VGMPSGTone1stByte)
                    and $0f
                    ld e,a      ; de = ????????0000ffff
                    ld a,b
                    and %00111111   ; also resets carry
                    ld d,a      ; d = 00ffffff
                    ld a,$00
                    rr d        ; Shift right, input from carry, output into carry
                    rr a        ; Do the same
                    rr d        ; Repeat 4 times
                    rr a
                    rr d
                    rr a
                    rr d
                    rr a        ; Now d = 000000ff, a = ffff0000
                    or e
                    ld e,a      ; Now de = 000000ffffffffff which is correct (at last)

                  +:push hl
                    push de
                        ld hl,VGMPSGTones
                        ld d,$00
                        ld e,c
                        add hl,de
                        add hl,de   ; Add channel*2 to offset to get the right slot
                    pop de
                        ld (hl),e   ; Finally store it
                        inc hl
                        ld (hl),d
                    pop hl                       
                pop de
            _ToneEnd:
        pop bc

        inc bc
        jp GetData

    WaitNSamples:
        ; get number
        push af
/*            ; Debug dispaly of wait value (and below)
            push hl
                ld hl,NameTableAddress
                call VRAMToHL
            pop hl*/
            inc bc
            ld a,(bc)
;            call WriteNumber
            ld e,a  ; read into e then d (big-endian)
            inc bc
            ld a,(bc)
;            call WriteNumber
            ld d,a
            inc bc
        pop af
        ; Add it to the total
        add hl,de
        ; If it's gone past ffff then I need to handle that :/
        ; The carry flag will be set if it is
        jp nc,DoINeedToWait

        ld a,(VGMWaitTotalOverflow)
        inc a   ; Add 2 for every overflow = 2 x 8000 to add
        inc a
        ld (VGMWaitTotalOverflow),a

        jp DoINeedToWait

    Wait160th:  ; add 735 to the total number of samples
        inc bc
        ld de,735
        add hl,de
        jp DoINeedToWait

    Wait150th:  ; add 882 to the total number of samples
        inc bc
        ld de,882
        add hl,de
        jp DoINeedToWait

    EndOfFile:
        ; don't inc bc
        call VGMDoLoop
        jp GetData

    YM2413:
        inc bc
        ld a,(bc)   ; Register
        out ($f0),a
        ld e,a      ; e = register
        inc bc
        ld a,(bc)   ; Get data
        out ($f1),a
        inc bc
        ld d,a      ; d = data
        ; Analyse the register
        ld a,e
        and $f0
        cp $10
        jp z,_1x
        cp $20
        jp z,_2x
        jp GetData

        _1x:    ; FNum low bits
        push bc
        push hl
            ld a,e
            and $0f
            cp $6
            jp nc,+  ; don't look at it if it's past $x5

            sla a
            ld b,0
            ld c,a
            ld hl,VGMYM2413FNums
            add hl,bc
            ld (hl),d   ; store new value (ignore high byte)
            +:
        pop hl
        pop bc
        jp GetData

        _2x:    ; FNum high bit/block/key
        push bc
        push hl
            ld a,e
            and $0f
            ld e,a  ; e = channel number
            cp $6
            jp nc,+  ; don't look at it if it's past $5 (rhythm mode has 0-5 tone channels)

            sla a   ; I want 2*ch+1
            inc a
            ld b,0
            ld c,a
            ld hl,VGMYM2413FNums
            add hl,bc
            ld a,d
            and %00000001
            ld (hl),a   ; store new high bit (ignore low byte)

            ld hl,VGMYM2413Blocks   ; Block
            ld b,0
            ld c,e
            add hl,bc
            ld a,d
            rra
            and %00000111
            ld (hl),a

            ld hl,VGMYM2413Keys     ; Key
            ld b,0
            ld c,e
            add hl,bc
            ld (hl),d

            +:
        pop hl
        pop bc
        jp GetData

    YMxxxx: ; discard
        inc bc
        inc bc
        inc bc
        jp GetData

    DoINeedToWait:
        ; If hl>=FrameLength then subtract FrameLength and exit loop
        ; otherwise loop

        ; Debug display of information (wait total)
/*        push hl
            ld hl,NameTableAddress
            call VRAMToHL
        pop hl
        ld a,h
        call WriteNumber
        ld a,l
        call WriteNumber
*/
        ld (VGMWaitTotal),hl    ; store hl in memory
        ld de,(VGMFrameLength)
        ld a,(VGMPlayerState)   ; fast forward?
        cp VGMFast
        jp nz,_NotFast
        push hl
            push de
            pop hl
            add hl,hl
            add hl,hl
            push hl
            pop de
        pop hl
        _NotFast:
        scf
        ccf                     ; make sure carry bit is 0 because I have to use sbc (no 16-bit sub)
        sbc hl,de               ; subtract samples per frame
        jp c,NoWait
        ; If we're here then the result is >=0
        ; so we want to put the new value in memory
        ld (VGMWaitTotal),hl
        ; add frame length to the total number of samples played
        push hl
            ld hl,(VGMTimeSamples)
            add hl,de
            ld (VGMTimeSamples),hl
            ld de,44100         ; see if it's over 44100
            scf
            ccf
            sbc hl,de
            jp c,_DoneTime
            ; If we're here then hl was >44100
            ld a,1
            ld (SecondsChanged),a
            _IncSeconds:
                ld (VGMTimeSamples),hl  ; save the subtracted hl
                ld a,(VGMTimeSecs)
                inc a                   ; and add 1 second
                daa
                ld (VGMTimeSecs),a
                cp $60                  ; if it's at 60 seconds...
                jp z,_IncMinutes
                jp _DoneTime
            _IncMinutes:
                ld a,$00                ; zero the seconds
                ld (VGMTimeSecs),a
                ld a,(VGMTimeMins)
                inc a                   ; and add 1 minute
                daa
                ld (VGMTimeMins),a
            _DoneTime:
        pop hl
        ; and exit to wait for the next vblank
        ld (VGMCounterLocation),bc
        jp EndGetDataLoop
    NoWait:
        ; If we're here then the result is <0, ie. total<frame size so I can safely add $8000
        ld hl,(VGMWaitTotal)    ; restore hl
        ld a,(VGMWaitTotalOverflow)
        cp 0
        jp z,GetData            ; continue
        ; Pay back an overflow
        dec a
        ld (VGMWaitTotalOverflow),a
        ld de,$8000
        add hl,de
        jp DoINeedToWait    ; Yes I do!
    EndGetDataLoop:
    pop hl
    pop de
    pop bc
    pop af
    ret
.ends

;==============================================================
; No VGM file - does not return
; Maybe add a picture here?
;==============================================================
.section "No VGM file" SEMIFREE
NoVGMFile:
    ; Clear the screen
    ld hl,NameTableAddress
    call VRAMToHL
    ld bc,$700
    ld a,$00
    _Loop:
        out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
        dec c
        jp nz,_Loop
        dec b
        jp nz,_Loop

    ; Display a message
    ld iy,NameTableAddress
    ld hl,NoVGMText
    call WriteASCII
/*
    ; Debug: display what we've found
    ld hl,NameTableAddress
    call VRAMToHL
    ld c,160
    ld hl,$8000
    _Loop1:
        ld a,(hl)
        call WriteNumber
        inc hl
        dec c
        jp nz,_Loop1
*/
    ; Main screen turn on
    ld a,%11010000
    out ($bf),a
    ld a,$81
    out ($bf),a
    
    halt
.ends

;==============================================================
; Visualisation routines
;==============================================================
.section "Visualisations" SEMIFREE
.define VisLocation NameTableAddress+2*(32*12)

.define NumVisRoutines 5

VisRoutines:        ; calculation for vis
.dw PianoVis,       FrequencyVis,   VolumeVis,      CalcSnow,       NoRoutine

VisDisplayRoutines: ; VBlank display update
.dw DrawPianoVis,   DrawVisBuffer,  DrawVisBuffer,  UpdateSnow,     DrawVisBuffer

VisInitRoutines:    ; Initialisation
.dw PianoVisInit,   NoRoutine,      NoRoutine,      InitSnow,       ClearBuffer

NoRoutine:
    ret

VisRoutinesStrings:
.dw PianoVisString,FrequencyVisString,VolumeVisString,SnowVisString,NoVisString

NoVisString:
.db "     Visualisation disabled     ",0
VolumeVisString:
.db " Tone 1  Tone 2  Tone 3  Noise  ",0
FrequencyVisString:
.db "100   Freq   200  /Hz  500 1k 7k",0
PianoVisString:
.db "       Dave's Piano-matic       ",0
SnowVisString:
.db 0

InitialiseVis:  ; General vis initialisation
    call InitialiseVisRoutine
    ret

NextVis:
    push af
        ld a,(VisNumber)
        inc a
        cp NumVisRoutines
        jr nz,_NoWrap
        ld a,$00
        _NoWrap:
        ld (VisNumber),a
        call InitialiseVisRoutine
        ld a,1
        ld (VisChanged),a
    pop af
    call SaveSettings
    ret

InitialiseVisRoutine:    ; Per-routine initialisation (runs in VBlank)
    push hl
    push de
    push iy
    push bc
        call NoSprites

        ld a,(VisNumber)
        sla a   ; a*2
        ld d,$00
        ld e,a

        ld iy,VisRoutinesStrings    ; Get location of string
        add iy,de
        ld h,(iy+1)
        ld l,(iy+0)
        ld iy,VisLocation+32*2*4    ; Where to draw it
        call WriteASCII             ; Draw it

        ld iy,VisInitRoutines       ; Do initialisation
        add iy,de
        ld h,(iy+1)
        ld l,(iy+0)
        call callHL

        ld iy,VisDisplayRoutines    ; Update VBlank routine
        add iy,de
        ld b,(iy+1)
        ld c,(iy+0)
        ld (VisDisplayRoutine),bc

        ld iy,VisRoutines           ; and non-VBlank routine
        add iy,de
        ld b,(iy+1)
        ld c,(iy+0)
        ld (VisRoutine),bc

        ld a,0                      ; Reset flag
        ld (VisChanged),a
    pop bc
    pop iy
    pop de
    pop hl
    ret

UpdateVis:
    push hl
    push af
    push de
        ld a,(VisNumber)
        sla a       ; multiply by 2
        ld hl,VisRoutines
        ld d,$00
        ld e,a
        add hl,de   ; hl = address of address of routine
        ld e,(hl)
        inc hl
        ld d,(hl)
        push de
        pop hl
        call callHL ; jump to address
    pop de
    pop af
    pop hl
    ret

ClearBuffer:   ; uses a,c,hl
    ; Clear buffer every frame
    ld c,64
    ld hl,VisBuffer
  -:ld a,0
    ld (hl),a
    inc hl
    dec c
    jp nz,-
    ret

DrawVisBuffer:
    push hl ; for various stuff
    push de ; second variable for maths
    push bc ; b = row count c = column count
    push af ; for maths
        ; Draw buffer to screen
        ld hl,VisLocation
        call VRAMToHL
        ld b,4      ; Number of rows
        _DrawVisBufferLoop:
            ; Draw line
            ld c,32
            ld hl,VisBuffer
            _DrawRow:
                ld a,(hl)           ; Get the value for this slot
                ld d,b              ; d = 8*(b-1)
                dec d
                sla d
                sla d
                sla d
                sub d               ; Subtract d = minimum value
                jp nc,_AboveMinimum
                ld a,0              ; If the result is negative then I want 0
                _AboveMinimum:
                cp 9                ; Is it 9 or more?
                jp c,_LessThan8
                ld a,8              ; If not, I want 8
                _LessThan8:
                add a,96
                out ($BE),a
                push hl ; delay
                pop hl
                ld a,$00
                out ($BE),a
                inc hl
                dec c
                jp nz,_DrawRow
            dec b
            jp nz,_DrawVisBufferLoop
    pop af
    pop bc
    pop de
    pop hl
    ret

FrequencyVis:
    push af
    push hl
    push de
    push bc
    push ix
    push iy
        call ClearBuffer

        ld ix,VGMPSGTones
        ld iy,VGMPSGVolumes

        ld b,3  ; how many channels to do
        _Loop:
            ld e,(ix+0)
            ld d,(ix+1)     ; de contains the frequency value 0000-03ff
            ld a,(iy+0)
            cpl
            and $0f
            ld c,a          ; c  contains the volume 0-f (inverted to be normal sense)

            ; Divide frequency by 32 = >>5
            ld a,e
            and %11100000
            ld e,a
            rr d    ; Shift d and e together for 2 bits (10 bit number)
            rr e
            rr d
            rr e
            srl e   ; Then just shift e, inputting 0, for the remaining 3 shifts
            srl e
            srl e
            ; I want 31-(this value)
            ld a,31
            sub e
            ld e,a

            ; Add volume to value for channel
            ld hl,VisBuffer
            ld d,$00
            add hl,de
            ld a,c
            add a,(hl)
            ld (hl),a

            inc ix          ; Move to next values
            inc ix
            inc iy

            dec b           ; Loop counter
            jp nz,_Loop

        ; Noise: bump every value by up to 3
        ld c,32
        ld hl,VisBuffer
        ld a,(VGMPSGVolumes+3)
        cpl
        and $f
        ld b,a
        srl b
        srl b
        _AddNoise:
            ld a,(hl)
            add a,b
            ld (hl),a
            inc hl
            dec c
            jp nz,_AddNoise
    pop iy
    pop ix
    pop bc
    pop de
    pop hl
    pop af
    ret

VolumeVis:
    push af
    push hl
    push bc
    push ix
        call ClearBuffer

        ld hl,VGMPSGVolumes
        ld ix,VisBuffer
        ld b,4          ; How many channels to do
        _VolVisLoop:
            ld a,(hl)   ; Get volume
            cpl
            and $0f
            sla a       ; Multiply by 2 to fill the vis
            ld c,8      ; Fill 8 slots with that
            _VolVisFillLoop:
                ld (ix+0),a
                inc ix
                dec c
                jp nz,_VolVisFillLoop
            inc hl
            dec b
            jp nz,_VolVisLoop
    pop ix
    pop bc
    pop hl
    pop af
    ret

PianoVisInit:
    push af
    push bc
    push hl
        ; Draw tiles
        ld hl,VisLocation
        call VRAMToHL
        ld hl,PianoTileNumbers
        ld c,$be
        ld b,64*2
        otir
        ; draw 2 blank lines
        ld a,0
        ld b,32*2*2
      -:out ($be),a
        dec b
        jp nz,-

        ; Define hand sprites
        ld hl,SpriteTableAddress
        call VRAMToHL
        ld a,208+16         ; Hand Y-position
        ld c,12*2
      -:out ($be),a
        dec c
        jp nz,-
        ld bc,128
        add hl,bc
        call VRAMToHL
        ld c,12
      -:ld a,0              ; Hand x-position
        out ($be),a
        ld a,$bc            ; Tile number (-$100 for spriteset 1)
        out ($be),a
        ld a,8
        out ($be),a
        ld a,$be
        out ($be),a
        dec c
        jp nz,-
    pop hl
    pop bc
    pop af
    ret

PianoVisMinValsPSG:
; Values pre-calculated
; = minimum values for each note
; Run through list from left, if PSG value is >= value then it's this note
;      C   C#    D   D#    E    F   F#    G   G#    A   A#    B
.dw 7041,
.dw 6646,6273,5921,5589,5275,4979,4700,4436,4187,3952,3730,3521
.dw 3323,3137,2961,2794,2638,2490,2350,2218,2093,1976,1865,1760
.dw 1662,1569,1480,1397,1319,1245,1175,1109,1047, 988, 933, 880
.dw  831, 784, 740, 699, 659, 622, 587, 554, 523, 494, 466, 440
.dw  415, 392, 370, 349, 330, 311, 294, 277, 262, 247, 233, 220
.dw  208, 196, 185, 175, 165, 156, 147, 139, 131, 123, 117, 110
.dw  104,  98,  93,  87,  82,  78,  73,  69,  65,  62,  58,  55
.dw   52,  49,  46,  44,  41,  39,  37,  35,  33,  31,  29,  28
.dw   26,  25,  23,  22,  21,  19,  18,  17,  16,  15,  14,   0

PianoVisMinValsFM:
; Same again, for FM
; Number = FNum << Block
; Minimum value for MIDI note n = 2^(n/12) * 83.76486

;       C    C#     D    D#     E     F    F#     G    G#     A    A#     B
.dw     0
.dw   168,  177,  188,  199,  211,  224,  237,  251,  266,  282,  299,  316
.dw   335,  355,  376,  398,  422,  447,  474,  502,  532,  564,  597,  633
.dw   670,  710,  752,  797,  844,  895,  948, 1004, 1063, 1127, 1194, 1265
.dw  1340, 1420, 1504, 1594, 1689, 1789, 1895, 2008, 2127, 2254, 2388, 2530
.dw  2680, 2840, 3009, 3188, 3377, 3578, 3791, 4016, 4255, 4508, 4776, 5060
.dw  5361, 5680, 6017, 6375, 6754, 7156, 7582, 8032, 8510, 9016, 9552,10120
.dw 10722,11359,12035,12751,13509,14312,15163,16065,17020,18032,19104,20240
.dw 21444,22719,24070,25501,27017,28624,30326,32129,34040,36064,38209,40481
.dw 42888,45438,48140,51002,54035,57248,60652,64259,64259,64259,64259,64259 ; repeated value on purpose! next is past 65535

PianoXPositions:
; Lookup is much easier and faster (at runtime) than calculating
;     C  C#   D  D#   E   F  F#   G  G#   A  A#   B
.db   0
.db   0,  2,  4,  6,  8, 12, 14, 16, 18, 20, 22, 24
.db  28, 30, 32, 34, 36, 40, 42, 44, 46, 48, 50, 52
.db  56, 58, 60, 62, 64, 68, 70, 72, 74, 76, 78, 80
.db  84, 86, 88, 90, 92, 96, 98,100,102,104,106,108
.db 112,114,116,118,120,124,126,128,130,132,134,136
.db 140,142,144,146,148,152,154,156,158,160,162,164
.db 168,170,172,174,176,180,182,184,186,188,190,192
.db 196,198,190,202,204,208,210,212,214,216,218,220
.db 224,226,228,230,232,236,238,240,242,244,246,0

.define n 108   ; y-pos (note)
.define s n-7   ;       (sharp)
.define x 208+16;       (not shown)
PianoYPositions:
.db x
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,n
.db n,s,n,s,n,n,s,n,s,n,s,x
.undef n,s,x

PianoVis:
    ; Calculate hand x,y positions
    ; Store as xyxyxy in vis buffer

    push ix
    push iy
    push af
    push bc
    push de
    push hl

    ld ix,VGMPSGTones       ; 3 x 2 bytes
    ld iy,VisBuffer         ; Where to store x,y
    ld c,3  ; Number of hands (PSG)
    --:
    push bc
    ld d,(ix+1)             ; Tone value
    ld e,(ix+0)

    ; Handle periodic noise when c=1
    ld a,c
    cp 1
    jp nz,+
    ld a,(VGMPSGNoiseIsPeriodic)
    cp 1
    jp nz,+
    ; I need to divide the frequency by 16, ie, <<4
    sla e
    rl  d
    sla e
    rl  d
    sla e
    rl  d
    sla e
    rl  d
    ; I also want it to use the ch3 volume for ch2
    dec c
    +:
    ; If volume=0 then don't show the hand
    ld hl,VGMPSGVolumes+3
    ld b,0
    scf
    ccf
    sbc hl,bc
    ld a,(hl)
    and $f
    cp $f
    jp z,_NoHand
    ld b,0          ; counter
    push iy
    ld iy,PianoVisMinValsPSG; values to look at
    scf
    ccf
  -:ccf             ; set carry flag -> sbc = <=
    ld h,(iy+1)     ; get value stored there
    ld l,(iy+0)
    sbc hl,de       ; Subtract actual value
    jp c,+          ; Loop until I find a value less than or equal to the note
    inc iy
    inc iy
    inc b
    jp -
  +:pop iy          ; b is now the key number, counting from the left
    push bc         ; need to preserve c
        ld c,b
        ld b,0
        ld hl,PianoXPositions
        add hl,bc
        ld a,(hl)
        ld (iy+0),a
        ld hl,PianoYPositions
        add hl,bc
        ld a,(hl)
        ld (iy+1),a
    pop bc
    jp +
    _NoHand:    ; Don't show the hand if applicable
    ld (iy+1),208+16
  +:pop bc
    dec c
    inc ix
    inc ix
    inc iy
    inc iy
    jp nz,--

    ; Now do FM :)
    ld ix,0 ; channel number
    ld c,6  ; Number of hands (FM) - assume rhythm mode (probably 100% of games)
    --:
    ; If key isn't pressed then don't bother
    push ix
    pop hl
    ld de,VGMYM2413Keys
    add hl,de
    bit 4,(hl)  ; Is the key pressed?
    jp z,_NoHand2
    ; Get FNum
    push ix
        ld de,VGMYM2413FNums
        add ix,ix       ; 2 bytes per channel
        add ix,de
        ld h,(ix+1)     ; FNum
        ld l,(ix+0)
    pop ix
    ; Get Block
    push ix
        ld de,VGMYM2413Blocks
        add ix,de
        ld a,(ix+0)     ; Block
    pop ix
    or a
    jp z,+              ; ship shifting if a=0
  -:sla l               ; shift hl by Block bits
    rl  h
    jp c,_Overflow
    dec a
    jp nz,-
  +:push hl
    pop de              ; Now de = FNum << Block
    jp _DEtoNoteNum
    _Overflow:          ; If, when shifting FNum by Block, it overflows 16 bits then act as if the value was 65535
    ld de,65534
;    jp _DEtoNoteNum
    _DEtoNoteNum:
    ld b,0          ; counter
    push iy
    ld iy,PianoVisMinValsFM ; values to look at
    scf
    ccf
  -:ccf             ; set carry flag -> sbc = >
    ld h,(iy+1)     ; get value stored there
    ld l,(iy+0)
    sbc hl,de       ; Subtract actual value
    jp nc,+         ; Loop until I find a value greater than the note
    inc iy
    inc iy
    inc b
    jp -
  +:dec b           ; b is now the key number, counting from the left
    pop iy
    push bc         ; need to preserve c
        ld c,b
        ld b,0
        ld hl,PianoXPositions
        add hl,bc
        ld a,(hl)
        ld (iy+0),a
        ld hl,PianoYPositions
        add hl,bc
        ld a,(hl)
        ld (iy+1),a
    pop bc
    jp +
    _NoHand2:    ; Don't show the hand if applicable
    ld (iy+1),208+16
  +:dec c
    inc ix
    inc iy
    inc iy
    jp nz,--

    pop hl
    pop de
    pop bc
    pop af
    pop iy
    pop ix

    ret

DrawPianoVis:
    ; Load hand x,y positions from VisBuffer
    ; Set sprite positions accordingly

    .define NumHands 9
    push af
    push bc
    push ix
    push hl

        ld c,$be    ; VDP port

        ld hl,SpriteTableAddress
        call VRAMToHL

        ld ix,VisBuffer

        ld b,NumHands
      -:ld a,(ix+1)     ; y-pos
        out ($be),a
        out ($be),a
        inc ix
        inc ix
        dec b
        jp nz,-

        ld bc,128
        add hl,bc
        call VRAMToHL

        ld ix,VisBuffer
        ld b,NumHands   ; How many hands to draw
        -:
            ld a,(ix+0) ; 19
            out ($be),a ; x-pos
            push af ; 11
            nop     ;  4
            nop     ;  4
            nop     ;  4
            nop     ;  4
            nop     ;  4 = 31
                in a,($be)  ; skip #
            pop af  ; 10
            add a,8 ;  7
            nop     ;  4
            nop     ;  4
            nop     ;  4 = 29
            out ($be),a ; x-pos
            push ix ; 15
            pop ix  ; 14 = 29
            in a,($be)  ; skip #
            dec b   ;  4
            inc ix  ; 10
            inc ix  ; 10
            jp nz,- ; 10 = 53 :P
    pop hl
    pop ix
    pop bc
    pop af
    ret


.define NumSnowFlakes 32

SnowText:
.db 10,"  Dancing snow...",10,10
.db "               ...with flashing",0

InitSnow:
    push af
    push bc
    push hl
        ; Blank vis area
        ld hl,VisLocation
        call VRAMToHL
        ld bc,32*2*5
      -:ld a,0
        out ($be),a
        dec bc
        ld a,b
        or c
        jp nz,-

        ; Fill vis area with my text
        ld hl,SnowText
        ld iy,VisLocation
        call WriteASCII

        ; Define sprites
        ld hl,SpriteTableAddress+128
        call VRAMToHL
        ld c,NumSnowFlakes
      -:ld a,0
        out ($be),a
        ld a,$b0                ; Tile number
        push ix
        pop ix      ; delay
        out ($be),a
        dec c
        jp nz,-

        ; Fill buffer with initial random poitions
        ld hl,VisBuffer
        ld c,NumSnowFlakes*2
        -:
        call GetRandomNumber
        ld (hl),a
        inc hl
        dec c
        jp nz,-

    pop hl
    pop bc
    pop af
    ret

CalcSnow:
    push ix
    push af
    push bc

    ; XXXX,YYYY stored in VisBuffer
    ld ix,VisBuffer
    ld c,NumSnowFlakes
    -:          ; X values:
    ld a,c      ; get index
    and $3      ; reduce to 0, 1, 2 or 3

    ; Make it change if the corresponding PSG channel is loud enough
    push hl
    push bc
    push af
        ld hl,VGMPSGVolumes
        ld b,0
        ld c,a  ; 0,1,2,3 or a
        add hl,bc
        ld a,(hl)
        cpl
        and $0f
        cp $a
        jp c,+
    pop af  ; vol high
    add a,(ix+1) ; change value semi-randomly
    jp ++

    +: ; vol low
    pop af
    ; no change
    ++:
    pop bc
    pop hl

    and $3      ; remove high bits
    dec a       ; now it's -1, 0, +1 or +2
    cp 2
    jp nz,+
    ld a,0      ; make +2 -> 0
    +:

    add a,(ix+0); Add it to the current value
    ld (ix+0),a ; that's what I want
    inc ix
    dec c
    jp nz,-

    ld c,NumSnowFlakes
    -:          ; Y values:
    ld a,(ix+0) ; get current value
    inc a       ; increment
    ld (ix+0),a ; that's what I want
    cp $ff-$20  ; If it's time to change...
    jp nz,+
    ; I want a new random x-pos
    call GetRandomNumber
    and 248
    ld (ix+(-NumSnowFlakes)),a
    +:
    inc ix
    dec c
    jp nz,-

    pop bc
    pop af
    pop ix
    ret

UpdateSnow:
    push af
    push bc
    push hl
        ; Noise flashing
        ld a,(VGMPSGVolumes+3)
        cpl
        and $0f
        cp $0
        jp z,+
        ; noise on
        ld c,clRGB233
        jp ++
      +:; noise off
        ld c,clWhite

     ++:; Write colour to CRAM
        ld a,22
        out ($bf),a
        ld a,%11000000
        out ($bf),a
        ld a,c
        out ($be),a

        ; Display sprites
        ld hl,SpriteTableAddress+128
        call VRAMToHL
        ; Write X positions
        ld c,NumSnowFlakes
        ld ix,VisBuffer
      -:ld a,(ix+0)
        out ($be),a
        ; Sprite number $bb or ???
        ld a,c      ; snowflake number
        add a,(ix+0); x-pos
        rra
        add a,(ix+NumSnowFlakes)
        rra
        rra         ; look at a higher bit for slower changing
        and %110
        add a,$b5   ; start of snowflake sprites
        out ($be),a ; skip sprite number
        inc ix
        dec c
        jp nz,-

        ld hl,SpriteTableAddress
        call VRAMToHL
        ; Write Y positions
        ld c,NumSnowFlakes
        ld hl,VisBuffer+NumSnowFlakes
      -:ld a,(hl)
        out ($be),a ; output it
        inc hl
        dec c
        nop
        jp nz,-

    pop hl
    pop bc
    pop af
    ret

GetRandomNumber:
    ; Returns a random 8-bit number in a
    push bc
    push de
    ld de,(RandomSR)  ; Load shift register value
    ld b,0              ; reset output
    ld c,8              ; no. of bits to shift by
  -:ld a,e
    srl d               ; Shift de into b
    rr e
    rr b
    and %00001001       ; Tap bits 0 and 3 (before shifting) (same as PSG noise :)
    jp pe,+             ; if odd parity, input 1 into shift register (ie. xor of tapped bits)
    set 7,d
  +:dec c
    jp nz,-

    ld (RandomSR),de    ; Put shift register back in RAM
    ld a,b              ; Return in a
    pop de
    pop bc
    ret



.ends

;==============================================================
; VDP initialisation data
;==============================================================
VdpData:
.db %00000110,$80
;    |||||||`- Disable synch
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during scrolling
;    `-------- Fix right 8 columns during scrolling
.db %10000000,$81
;    ||||| |`- Zoomed sprites -> 16x16 pixels
;    ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;    ||||`---- 30 row/240 line mode
;    |||`----- 28 row/224 line mode
;    ||`------ VBlank interrupts
;    |`------- Enable display
;    `-------- Must be set (VRAM size bit)
.db NameTableAddress>>10,$82
.db SpriteTableAddress>>7,$85
.db SpriteSet<<2,$86
.db $f,$87
;    `-------- Border palette colour (sprite palette)
.db $00,$88
;    ``------- Horizontal scroll
.db $20,$89
;    ``------- Vertical scroll
.db $ff,$8a
;    ``------- Line interrupt spacing
VdpDataEnd:

;==============================================================
; My chosen palette
;==============================================================
PaletteData:
.db clRGB000,clRGB111,clRGB222,clRGB333,clRGB010,clRGB121,clRGB232,clRGB100,clRGB211,clRGB322,clRGB000,clRGB000,clRGB000,clRGB000,clRGB000,clRGB000
.db clRGB303,clRGB000,clRGB110,clRGB310,clRGB321,clRGB330,clRGB333
PaletteDataEnd:

.define NumPalettes 6

Palettes:
.db clRGB010,clRGB121,clRGB232  ; original (green)
.db clRGB001,clRGB112,clRGB223  ; blue
.db clRGB100,clRGB211,clRGB322  ; red
.db clRGB023,clRGB223,clRGB233  ; light blue
.db clRGB000,clRGB111,clRGB222  ; greyscale
.db clRGB300,clRGB322,clRGB323  ; bright red

TileData:
;.include "fonts\Arial.inc"
;.include "fonts\Bookman.inc"
;.include "fonts\Courier.inc"
;.include "fonts\Dungeon.inc"
;.include "fonts\Lucida Console.inc"
;.include "fonts\South Park.inc"
;.include "fonts\Times.inc"
;.include "fonts\Trebuchet.inc"
.include "fonts\Verdana.inc"
;.include "fonts\Westminster.inc"

BigNumbers:
.include "Big numbers.inc"

Pad:
.include "3D pad.inc"

PadData:
.include "3D pad (tile numbers).inc"
PadDataEnd:

ScaleData:
.include "scale.inc"

TextData:
.incbin "Text.txt"
.db $00

NoVGMText:
.incbin "No VGM file message.txt"
.db $00

PianoTiles:
.include "piano.inc"

PianoTileNumbers:
.include "piano (tile numbers).inc"

Sprites:
.include "snow.inc"
.include "hand.inc"


.section "VGM logo displayer" SEMIFREE
;==============================================================
; Code
;==============================================================
SplashScreenVBlank:
    ; check input, if anything then skip the splash
    in a,($dc)
    and %00111111
    cp  %00111111
    ret z
    ; I want to jump elsewhere
    inc sp
    inc sp
    inc sp
    inc sp
    ret

DisplayVGMLogo:
    call NoSprites

    ; Set all palette entries to white
    ld a,0
    out ($bf),a
    ld a,$c0
    out ($bf),a     ; Set to CRAM
    ld c,32
    ld a,clWhite
    _WhiteLoop:
        out ($be),a
        dec c
        jp nz,_WhiteLoop

    ; Load tiles
    ld hl,1
    ld ix,LogoTiles
    ld bc,$132
    ld d,4
    call LoadTiles

    ld hl,NameTableAddress+2*(32*6+1)
    call VRAMToHL

    ; Draw tiles
    ld hl,LogoTileNumbers
    ld c,$be
    ld b,0
    otir
    otir
    otir
    ld b,$c0
    otir

    ; Set VBlank routine
    ld hl,SplashScreenVBlank
    ld (VBlankRoutine),hl

    ; Turn screen on
    ld a,%11110000
    out ($bf),a
    ld a,$81
    out ($bf),a

    ld b,3
    ld de,LogoPalette
    ld c,15     ; how many steps to fade in

    _FadeIn:
    ; Pause 2 frames
    ei
    halt
    ei
    halt

    ; Move palette
    push de
    pop hl
    call LoadPalette
    dec c
    jp nz,_FadeIn

    ld c,20 ; how many frames to wait
    _WaitLoop:
        ei
        halt
        dec c
        jp nz,_WaitLoop

    ld b,7
    ld de,FadeOutPalette
    ld c,30

    _FadeOut:
    ; Pause 1 frame
    ei
    halt
    ; Move palette
    push de
    pop hl
    call LoadPalette
    inc c
    ld a,c
    cp 32+16
    jp nz,_FadeOut

    ret

;==============================================================
; Data
;==============================================================

LogoPalette:
.db clLtGrey,clDkGrey,clBlack

FadeOutPalette:
.db clRGB010,clRGB121,clRGB232,clRGB333,clRGB222,clRGB111,clRGB000

LogoTiles:
.include "VGM logo.inc"

LogoTileNumbers:
.include "VGM logo (tile numbers).inc"

.ends

.section "Save/load settings in BBRAM" FREE
_Marker:
.db "VGM"
_MarkerEnd:

SaveSettings:
    push af
    push bc
    push hl
    push de
        ld a,%00001000  ; select BBRAM
        ld ($fffc),a
        ; write marker
        ld bc,_MarkerEnd-_Marker
        ld hl,_Marker
        ld de,$8001
        ldir
        ; write data
        ld a,(VisNumber)
        ld ($8004),a
        ; write data
        ld a,(PaletteNumber)
        ld ($8005),a

        ld a,0          ; deselect BBRAM
        ld ($fffc),a
    pop de
    pop hl
    pop bc
    pop af
    ret

LoadSettings:
    push af
    push bc
    push hl
    push ix
        ld a,%00001000  ; select BBRAM
        ld ($fffc),a
        ; Check marker
        ld c,_MarkerEnd-_Marker
        ld hl,_Marker
        ld ix,$8001
        ; I have to do this one myself...
        -:
            ld a,(hl)   ; read ROM
            cp (ix+0)   ; compare to RAM
            ; if they aren't equal then exit
            jp nz,_Error
            inc hl
            inc ix
            dec c
            jp nz,-

        ; If we're here then ther marker is OK, so read stuff in
        ld a,($8004)
        ld (VisNumber),a
        ld a,($8005)
        dec a
        ld (PaletteNumber),a
        jp ++
        
        _Error:
        ld a,0
        ld (VisNumber),a
        ld a,-1
        ld (PaletteNumber),a

        ++:
        ld a,0          ; deselect BBRAM
        ld ($fffc),a
    pop ix
    pop hl
    pop bc
    pop af
    ret

.ends
