; SMS VGM player
; by Maxim

; VRAM mapping stuff
.define SpriteSet           1       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define NameTableAddress    $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes

.define Debug

.function TilemapAddress(x, y) NameTableAddress+2*(x+y*32) 
.function colour(r,g,b) (r+(g<<2)+(b<<4))

.macro SetDebugColour(r,g,b)
  .ifdef Debug
    push af
      ld a,$10
      out ($bf),a
      ld a,$c0
      out ($bf),a
      ld a,colour(r,g,b)
      out ($be),a
    pop af
  .endif
.endm

; WLA-DX banking setup
.memorymap
defaultslot 0
slotsize $4000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $4000
banks 1
.endro

.bank 0 slot 0
.org $0000

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 0.94,"SMS VGM player",SDSCNotes,"Maxim"
.section "SDSC notes" FREE
SDSCNotes:
;    123456789012345678901234567890123456789012345678901234567890123
.db "By whatever means necessary, this software must"
.db " NOT appear on any rom tool's 'needed' list. Please note that"
.db " only the first 32KB is the actual program, and any data after"
.db " that is appended VGM music data.",0
.ends

;==============================================================
; Memory usage
;==============================================================
.enum $c000 export
Port3EValue                     db
VGMMemoryStart                  dsb 256
ButtonState                     db
LastButtonState                 db
VisBuffer                       dsb 256 ; Visualisers can do as they wish with this
VisNumber                       db
IsPalConsole                    db
IsJapConsole                    db
FMChipDetected                  db
FMChipEnabled                   db
VBlankRoutine                   dw
VisRoutine                      dw ; Routine to calculate vis
VisDisplayRoutine               dw ; Routine to call in VBlank to update vis
VisChanged                      db ; Flag to signal that the vis needs to be initialised
SecondsChanged                  db
LoopsChanged                    db
PaletteChanged                  db
PaletteNumber                   db
RandomSR                        dw ; Random number generator shift register, needs seeding
GD3DisplayerBuffer              dsb 33
.ende

.include "PhantasyStardecompressors.asm"

.define PAGING_SLOT_0 $fffd
.define PAGING_SLOT_1 $fffe
.define PAGING_SLOT_2 $ffff
.define PORT_MEMORY_CONTROL $3e
.define PORT_AUDIO_CONTROL $f2

.org $0000
;==============================================================
; Boot section
;==============================================================
.section "!Boot section" FORCE   ; Standard stuff (for the SMS anyway)
  di              ; disable interrupts (re-enable later)
  im 1            ; Interrupt mode 1
  ld sp, $dff0    ; load stack pointer to not-quite-the-end of user RAM (avoiding paging regs)
  jr main         ; jump to main program
.ends

.org $10
.section "GetByte" force
GetByte:
  ; get byte (easy)
  ld a,(bc)
  ; increment address (hard)
  inc bc
  bit 6,b
  ret z
  ; bc is >$cx (presuming this is the only place it is changed)
  ; so flip to the next page
  push af
    ld a,(PAGING_SLOT_2)
    inc a
    call z,VGMStop ; give up at page $100(!)
    ld (PAGING_SLOT_2),a
    ld b,$80 ; wrap to $8000
  pop af
  ret
.ends

.org $0038
;==============================================================
; Interrupt handler
;==============================================================
.section "!Interrupt handler" FORCE
  ex af,af'
  exx
    in a,($bf)      ; satisfy interrupt
    ; No checking of the value because I know I only have VBlank interrupts
    ld hl,(VBlankRoutine)
    call callHL
  exx
  ex af,af'
  ei
  reti

callHL:
  jp (hl)

NoVBlank:
  ret
.ends

.section "VGM Player vblank" free
VGMPlayerVBlank:
  SetDebugColour(1,0,0)
  call CheckInput ; Read input into memory
  call ShowTime   ; Update time display
  call ShowLoopNumber
  SetDebugColour(2,0,0)

  ld a,(PaletteChanged)
  or a
  call nz,NextPalette ; must do CRAM writes in VBlank

  ld a,(VisChanged)
  or a
  call nz,InitialiseVis ; If the vis has changed then I need to do the initialisation in the vblank

  ld hl,(VisDisplayRoutine)   ; Draw vis
  SetDebugColour(3,0,0)
  call callHL
  SetDebugColour(0,0,0)

  ret
.ends

.org $0066
;==============================================================
; Pause button handler
;==============================================================
.section "!NMI handler" FORCE
  ; Debug resetter
;  jr $0000

  ; Dodgy PAL/NTSC speed switch
  push hl
  push de
      call VGMGetSpeed
      ld de,735
      or a
      sbc hl,de
      jr z,_ChangeToPAL
      ld hl,735
      jr _SetSpeed
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
  xor a
  ld (PAGING_SLOT_0),a
  inc a
  ld (PAGING_SLOT_1),a
  ld (PAGING_SLOT_2),a

  call TurnOffScreen
  
  ; Black palette
  xor a
  out ($bf),a
  ld a,$c0
  out ($bf),a
  ld b,32
  xor a
-:out ($be),a
  djnz -

  ; Load VDP with default values, thanks to Mike G :P
  ; hl = address of data
  ; b = size of data
  ; c = port
  ; otir = while (b>0) do {out (hl),c; b--}
  ld hl,VdpData
  ld b,_sizeof_VdpData
  ld c,$bf
  otir
  
  ; Clear RAM
  ld hl,$c000+1
  ld de,$c000+2
  ld bc,$1ff0-1
  ld (hl),0
  ldir

  call CheckPort3EValue

  call IsPAL
  ld (IsPalConsole),a
  call IsJapanese
  ld (IsJapConsole),a
  call HasFMChip
  ld (FMChipDetected),a
  xor a
  ld (FMChipEnabled),a

  ; Startup screen
  call ClearVRAM
  call NoSprites

  ; Load palette
  ld hl,PaletteData
  ld b,_sizeof_PaletteData
  ld c,0
  call LoadPalette

  ; Load tiles
  ld de,$4000     ; Load font
  ld hl,TileData
  call LoadTiles4BitRLENoDI

  ld de,$60*32+$4000     ; Load vis tiles
  ld hl,ScaleData
  call LoadTiles4BitRLENoDI

  .define BigNumbersOffset 105

  ld de,32*BigNumbersOffset+$4000
  ld hl,BigNumbers
  call LoadTiles4BitRLENoDI

  ld de,32*146+$4000
  ld hl,Pad
  call LoadTiles4BitRLENoDI

  ld de,32*256+$4000
  ld hl,PianoTiles
  call LoadTiles4BitRLENoDI

  ld de,32*$1b0+$4000
  ld hl,Sprites
  call LoadTiles4BitRLENoDI

  ; Initial button state values (all off)
  ld a,$ff
  ld (LastButtonState),a
  ld (ButtonState),a

  ; Draw text
  ld iy,TilemapAddress(0, 0)
  ld hl,TextData
  call WriteASCII

  ; Draw pad image
  ld bc,$0c0b     ; 12x11
  ld ix,PadData
  ld iy,TilemapAddress(19, 1)
  ld h,0
  call DrawImageBytes

  ; Put something in the shift register
  ld hl,$0210 ; the current time (am)
              ; a better seed would be.. better. eg. a counter
  ld (RandomSR),hl

  ; Load settings, initialise stuff
  call LoadSettings
  call UpdatePalette
  call InitialiseVis

  ; Reset VGM player
  call VGMInitialise

.ifdef Debug
  ; Debug: show detected information
  ld hl, TilemapAddress(3, 24)
  call VRAMToHL
  ld a,(IsJapConsole)
  call WriteNumber
  ld hl, TilemapAddress(3, 25)
  call VRAMToHL
  ld a,(IsPalConsole)
  call WriteNumber
  ld hl, TilemapAddress(3, 26)
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
  ld a,%11100010
;       ||||| |`- Zoomed sprites -> 16x16 pixels
;       ||||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;       ||||`---- 30 row/240 line mode
;       |||`----- 28 row/224 line mode
;       ||`------ VBlank interrupts
;       |`------- Enable display
;       `-------- Must be set (VRAM size bit)
  out ($bf),a
  ld a,$81
  out ($bf),a

  /* Auto-play is weird.. it plays super-fast
  call VGMStop
  call VGMUpdate
  call VGMPlayPause
  call VGMUpdate
  */

  ei
InfiniteLoop:   ; to stop the program
  halt
  SetDebugColour(0,1,0)
  call VGMUpdate      ; Write sound data
  SetDebugColour(0,2,0)
  call ProcessInput   ; Process input from the last VBlank
  SetDebugColour(0,3,0)
  ld hl,(VisRoutine)  ; Update vis data
  call callHL
  SetDebugColour(0,0,0)

  jr InfiniteLoop

;==============================================================
; VGM offset to SMS offset convertor
; Inputs:
; a = offset of dword in VGM file
; Outputs:
; a = page number ($02+) *
; hl = offset ($8000-$bfff)
; * If dword is zero then a will be zero to show that
;==============================================================
.section "VGM to SMS offset" free
VGMOffsetToPageAndOffset:
  push ix
  push bc
  push de
    ; Remember the offset value in c because I need to use a
    ld c,a

    ; Page in the first page, remembering the current page on the stack
    ld a,(PAGING_SLOT_2)
    push af
      ld a,1
      ld (PAGING_SLOT_2),a

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
      jr nz,_NonZero
      ld c,$00
      jr _end

_NonZero:
      ; Add offset, ie. dehl + c -> dehl
      ld b,$00
      add hl,bc
      jr nc,+
      inc de
+:
      ; Figure out which page it's on and store that in c now
      ; ddddddddeeeeeeeehhhhhhhhllllllll
      ; Page -----^^^^^^^^
      ld a,e
      ld b,h
      sla b
      rla
      sla b
      rla
      inc a ; VGM stub is one page
      ld c,a

      ; And then get the offset by modding by $4000 and adding $8000
      ; Do this by hl = (hl & 0x3fff) | 0x8000
      ; Quite neat in asm :P
      res 6,h
      set 7,h

_end:
    ; Restore original page
    pop af
    ld (PAGING_SLOT_2),a

    ; Output a=page number
    ld a,c
  pop de
  pop bc
  pop ix
  ret
.ends

.section "Draw GD3" free
MoveHLForwardByA:
  ; Adds A to HL
  ; If it goes over $c000, it handles the paging
  add a,l
  jr nc,+
  inc h
+:ld l,a
  ; check for >$c000
  ld a,h
  cp $c0
  ret nz

  ; Need to decrement by $4000
  res 6,h
  ; and page in the next page
  ld a,(PAGING_SLOT_2)
  inc a
  ld (PAGING_SLOT_2),a
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
    ld a,(PAGING_SLOT_2)
    inc a
    ld (PAGING_SLOT_2),a
    _OK2:
  pop af
  ret

;==============================================================
; GD3 displayer
; Uses 34 bytes of RAM to buffer the text
;==============================================================
DrawGD3Tag:
  push hl
  push de
  push bc
  push af
    ; Page in first page
    ld a,$14 ; offset of GD3 tag location
    call VGMOffsetToPageAndOffset   ; a = page or 0, hl = offset

    ; See if it's come out as zero = no tag
    or a
    jr z,_NoGD3

    ; Page it in
    ld b,a
    ld a,(PAGING_SLOT_2)
    push af
      ld a,b
      ld (PAGING_SLOT_2),a

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
    pop af
    ld (PAGING_SLOT_2),a

    jr _end

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
-:  ld c,(hl)
    call MoveHLForward
    ld b,(hl)
    call MoveHLForward

    ld a,b
    or c
    jr nz,-
  pop af
  pop bc
  ret

_DrawGD3String:
; Copy string from hl to RAM, (badly) converting from Unicode as I go
; Copy a maximum of MaxLength chars, terminate with \n\0
; Then draw to the screen
.define MaxLength 31                ; GD3 line length
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
      ld h,a ; backup char counter
      ; See if value is acceptable
      ld a,b
      or a ; high values are bad
      jr nz,_Unacceptable
      ld a,c
      or a
      jr z,_LoopEnd ; zero means end of string
      bit 7,a ; we only do 7-bit ASCII
      jr z,_Acceptable
_Unacceptable:
      ld a,'?';$bf    ; = '?'
_Acceptable:
      ld (de),a
      ld a,h ; restore counter
    pop hl
    inc de
     ; Fall through to next char
_NextChar:
    dec a   ; decrement counter
    jr nz,_Loop
    ; If it gets to zero:
    call _SkipGD3String   ; go to the end of the string
    push hl
    ; fall through to finish

_LoopEnd:
      ld h,d
      ld l,e
      cp 32           ; If no chars were written
      jr nz,+
      ld (hl),' ' ; put a space so it'll work properly
      inc hl
+:    ld (hl),$0a
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
  ld a,(SecondsChanged)
  or a
  ret z
  ; get digits
  ld de,(VGMTimeMins)     ; d = sec  e = min
  ; Set start name table address
  ld hl, TilemapAddress(2, 6)
  ; Output digit(s)
  ld b,e
  call DrawByte
  ld bc,6
  add hl,bc
  ld b,d
  call DrawByte
  xor a
  ld (SecondsChanged),a
  ret

;==============================================================
; Loop count displayer
;==============================================================
ShowLoopNumber:
  push af
    ld a,(LoopsChanged)
    or a
    jr z,+
    push bc
    push hl
      ld a,(VGMLoopsPlayed)
      ld b,a
      ld hl, TilemapAddress(13, 6)
      call DrawByte
      xor a
      ld (LoopsChanged),a
    pop hl
    pop bc
+:pop af
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
  push hl
    sla a ; multiply by 2
    ld c,a
    add a,BigNumbersOffset
    out ($be),a
    xor a
    out ($be),a
    ld a,c
    add a,BigNumbersOffset+1
    out ($be),a
    xor a
    out ($be),a
    ld a,c

    push bc
      ld bc,64
      add hl,bc
      call VRAMToHL
    pop bc

    add a,BigNumbersOffset+20
    out ($be),a
    xor a
    out ($be),a
    ld a,c
    add a,BigNumbersOffset+21
    out ($be),a
    xor a
    out ($be),a
  pop hl
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
    jr z,_Finish            ; and don't do anything if it hasn't changed
    ld (LastButtonState),a

    ; See which bits are unset = pressed
    bit 0,a         ; Up
    call z,NextVis
    bit 1,a         ; Down
    call z,VGMStop
;    bit 2,a         ; Left
;    call z,_Left
    bit 3,a         ; Right
    jr nz,+
    ld hl,PaletteChanged
    ld (hl),1
+:
    bit 4,a         ; Button 1
    call z,VGMPlayPause

    xor b       ; XOR a with the last value to get a 1 for each button that has changed, for hold-down buttons
    bit 5,a
    call nz,VGMFastForward  ; Button 2

    _Finish:
  pop bc
  pop af
  pop hl
  ret

NextPalette:
  ; increment palette number
  ld a,(PaletteNumber)
  inc a
  ; Loop
  cp _sizeof_Palettes/3
  jr nz,+
  xor a
+:ld (PaletteNumber),a
  call SaveSettings

UpdatePalette:
  ; Load palette
  ld hl,Palettes
  ld a,(PaletteNumber)
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
  ld c,16
  call LoadPalette

  ; reset flag
  xor a
  ld (PaletteChanged),a
  ret

;==============================================================
; VGM routines
;==============================================================

.section "VGM routines" free
; VGM routines memory mapping:
.enum VGMMemoryStart
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
VGMStartPage            db      ; 28    Start point page
VGMStartOffset          dw      ; 29-2a Start point offset when paged in
.ende

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
  jr +
-:or a
  sbc hl,de
  sbc a,0
  jr nc,+
  ccf
  adc hl,de
  adc a,0
  scf
+:ccf
  rl c
  rl b
  adc hl,hl
  adc a,0
  exx
  dec b
  exx
  jr nz,-
  sbc hl,de
  sbc a,0
  jr nc,+
  add hl,de
  scf
+:ccf
  rl c
  rl b
  ret
  
IsVGMFileAtOffset:  ; pass offset in ix, uses a, sets z if file found
  push bc
    rst GetByte
    cp 'V'
    jr nz,_fail
    rst GetByte
    cp 'g'
    jr nz,_fail
    rst GetByte
    cp 'm'
    jr nz,_fail
    rst GetByte
    cp ' '
_fail:
  pop bc
  ret

;==============================================================
; Initialise player/read one-time information
;==============================================================
VGMInitialise:
  push hl
  push af
    ; Check for VGM marker
    ld bc,$8000
    call IsVGMFileAtOffset
    jp nz,NoVGMFile

    ; Get lengths
    push bc
    pop ix
    ld h,(ix+27)    ; Total
    ld l,(ix+26)
    ld b,(ix+25)
    ld c,(ix+24)
    ld de,44100
    call Divide16   ; hl = remainder, bc = number of seconds
    ; round up by seeing if the remainder was >= 22050 samples
    scf
    ld de,22050
    sbc hl,de
    jr c,+
    inc bc
+:  ld hl,0
    ld de,60
    call Divide16   ; hl = seconds, bc = minutes
    push hl
      ld hl, TilemapAddress(10, 9)
      call VRAMToHL
    pop hl
    ld a,c
    call Hex2BCD
    call WriteNumber
    ld a,$1a   ; Draw colon (faster than redefining name table address)
    out ($be),a
    xor a
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
    jr c,+
    inc bc
    +:
    ld hl,0
    ld de,60
    call Divide16   ; hl = seconds, bc = minutes
    push hl
      ld hl, TilemapAddress(10, 10)
      call VRAMToHL
    pop hl
    ld a,c
    call Hex2BCD
    call WriteNumber
    ld a,$1a   ; Draw colon (faster than redefining name table address)
    out ($be),a
    xor a
    out ($be),a
    ld a,l
    call Hex2BCD
    call WriteNumber

    ; Get loop information
    ld a,$1c
    call VGMOffsetToPageAndOffset   ; now a=page/0, hl=offset
    ld (VGMLoopPage),a
    ld (VGMLoopOffset),hl

    ; get VGM data location
    ld a,$34
    call VGMOffsetToPageAndOffset   ; now a=page/0, hl=offset
    or a
    jr nz,+
    ; defaults
    ld a,1
    ld hl,$8040
+:  ld (VGMStartPage),a
    ld (VGMStartOffset),hl

    ; Draw GD3 tag
    push iy
      ld iy,TilemapAddress(1,18) ; GD3 location
      call DrawGD3Tag
    pop iy

    ; Initialise playback speed
    ld a,(IsPalConsole)
    cp $01
    jr z,_IsPal
    ld hl,735
    jr _SetSpeed
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

    ; Move pointer to the start of the VGM data
    ld hl,(VGMStartOffset)
    ld (VGMCounterLocation),hl

    ; Set various stuff to initial values:
    ld hl,$0000
    xor a
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
    xor a
    ld c,6
  -:ld (hl),a
    inc hl
    dec c
    jr nz,-
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

_MutePSG:
.db %10011111
.db %10111111
.db %11011111
.db %11111111

MuteAllSound:
  push af
  push bc
  push hl
    ; PSG:
    ld hl,_MutePSG ; where to copy from
    ld c,$7f       ; where to output to
    ld b,4         ; how many to output
    otir

    ; YM2413: turn off keys - reg to f0, val to f1
    ld c,$20
    ld b,6
    ld hl,VGMYM2413Keys
-:  ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
      ld a,c
      out ($f0),a
      ld a,(hl)
      res 4,a
      push hl
      pop hl
      out ($f1),a
    ld a,(Port3EValue)
    out (PORT_MEMORY_CONTROL),a
    inc c
    inc hl
    djnz -
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
    ld c,$20
    ld b,6
-:  ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
      ld a,c
      out ($f0),a
      ld a,(hl)
      res 4,a
      res 5,a
      push hl
      pop hl ; delay
      out ($f1),a
    ld a,(Port3EValue)
    out (PORT_MEMORY_CONTROL),a
    inc hl
    inc c
    djnz -
  pop bc
  pop hl
  ret

VGMPlayPause:
  push af
    ld a,(VGMPlayerState)
    cp VGMPlaying   ; if it's playing then pause
    call z,_Pause
    cp VGMPaused    ; if it's paused then play
    jr nz,+
    call RestoreVolumes
    call _Play
  +:cp VGMStopped   ; if it's stopped then play from the start
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
    or a                ; Is it zero? If so, that means there's no looping
    jr nz,_IsLooping

_NoLooping:
    ; so I'd better stop
    call _Stop
    jr _EndVGMDoLoop

_IsLooping:
    ld (PAGING_SLOT_2),a            ; Page
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

.ifdef Debugx
    ; Debug display of information (location/page)
    push af
    push hl
      ld a,b
      ld hl, TilemapAddress(18, 24)
      call VRAMToHL
      call WriteNumber
      ld a,c
      call WriteNumber
      ld a,(PAGING_SLOT_2)
      ld hl, TilemapAddress(15, 24)
      call VRAMToHL
      call WriteNumber
    pop hl
    pop af
.endif

ReadData:
    rst GetByte
.ifdef Debug
    ; Debug display of information
;    call WriteNumberEx
.endif
    cp $4f      ; GG st
    jp z,GameGearStereo
    cp $50      ; PSG
    jp z,PSG
    cp $51      ; YM2413
    jp z,YM2413
    cp $61      ; wait n
    jp z,WaitNSamples
    cp $62      ; wait 1/60
    jp z,Wait160th
    cp $63      ; wait 1/50
    jp z,Wait150th
    cp $66      ; end of file
    jp z,EndOfFile
    cp $67      ; data block
    jr z,DataBlock
    ; block ranges
    ld d,a ; backup
    and $f0
    cp $70
    jr z,WaitSmallN
    cp $80
    jr z,YM2612SampleWithWait
    ; unhandled ranges
    ld a,d
    cp $30          ; <$30 = undefined
    jr c,ReadData
    cp $51          ; $30-$50 = 1 operand
    jr c,Unhandled1Byte
    cp $60          ; $51-$5f = 2 operands
    jr c,Unhandled2Bytes
    cp $a1          ; up to $a0 = undefined
    jr c,ReadData
    cp $c0          ; $a0-$bf = 2 operands
    jr c,Unhandled2Bytes
    cp $d0          ; $c0-$df = 3 operands
    jr c,Unhandled3Bytes
;    jr Unhandled4Bytes ; remaining is $e0-$ff = 4 operands
; fall through

Unhandled4Bytes:
    rst GetByte
Unhandled3Bytes:
    rst GetByte
Unhandled2Bytes:
    rst GetByte
Unhandled1Byte:
    rst GetByte
    jr GetData

WaitSmallN:
    ld a,d ;restore
    and $f
    inc a
    ; fall through

WaitASamples:
    ld e,a
    ld d,0
    jp _WaitDESamples

YM2612SampleWithWait:
    ld a,d ;restore
    and $f
    jr WaitASamples

DataBlock:
    ; skip next byte (compatibility byte)
    rst GetByte
    ; skip next byte (block type)
    rst GetByte
    ; read in block size (32 bits)
    push de
    push hl
      rst GetByte
      ld e,a
      rst GetByte
      ld d,a
      rst GetByte
      ld l,a
      rst GetByte
      ld h,a
      ; hlde = block size
      ; we need to skip it
      ; save bc for now
      ld (VGMCounterLocation),bc
      ; get de into bc so I can divide
      ld b,d
      ld c,e
      ld de,$4000
      call Divide16
      ; now bc = number of pages to skip, hl = bytes to skip
      ; figure out the new location
      ld d,b
      ld e,c
      ld bc,(VGMCounterLocation)
      add hl,bc ; can't overflow, bc<$c000 and hl<$4000
      ; if the answer is >$c000 then we need to subtract $4000 and add one page
      ld a,h
      cp $c0
      jr c,+
      sub $40
      ld h,a
      inc de
+:    ; save the offset back to bc
      ld b,h
      ld c,l
      ; page in the new page
      ld a,(PAGING_SLOT_2)
      add a,e
      call c,_Stop ; bail if past the addressable range
      ld (PAGING_SLOT_2),a
      ; check on d - if non-zero then we still need to bail
      ld a,d
      or a
      call nz,_Stop ; bail if past the addressable range
    pop hl
    pop de
    jp GetData

GameGearStereo: ; discard
    rst GetByte
;    out ($06),a ; output stereo data :( crashes a real SMS(2)
    jp GetData

PSG:
    rst GetByte
    out ($7f),a ; output it

    ; Data analysis:
    push bc
      ; Is it a volume write?
      ld b,a  ; b = backup of data written
      and %10010000   ; volume write bitmask
      cp  %10010000
      ld a,b  ; restore
      jr nz,_NotVolume
      ; It is a volume write
      ; Channel = bits 5 and 6
      srl a
      srl a
      srl a
      srl a
      srl a
      and %00000011
      ld c,a  ; c = channel
      push hl
        ld hl,VGMPSGVolumes
        ld e,c
        ld d,$00
        add hl,de
        ld (hl),b
      pop hl
      jr _PSGAnalysisEnd

_NotVolume:
      bit 7,a         ; Is it a tone2?
      jr z,_Tone2
      ; must be a tone1
      and %10010000   ; Is it a tone1?
      cp  %10000000
      jr z,_Tone1
XX:
      ld a,$ff        ; reset VGMPSGTone1stByte if it's not a tone write
      ld (VGMPSGTone1stByte),a

      jr _PSGAnalysisEnd

      _Tone1:         ; Tone1
          ld a,b
          ld (VGMPSGTone1stByte),a

          and %01100000   ; Is it noise?
          cp  %01100000
          jr nz,+++
          ld a,b
          and %11110111
          cp  %11100011   ; Is it periodic tone2?
          jr z,+
          xor a
          jr ++
          +:
          ld a,1
          ++:
          ld (VGMPSGNoiseIsPeriodic),a
          +++:
          jr _PSGAnalysisEnd

      _Tone2:         ; Tone2
          ld a,(VGMPSGTone1stByte)    ; Look at 1st byte
          srl a           ; Extract channel number
          srl a
          srl a
          srl a
          srl a
          and %00000011
          cp 3            ; If it's channel 3 then exit
          jr z,_PSGAnalysisEnd
          ld c,a          ; c = channel
          push de
              ; Extract frequency
              ld a,(VGMPSGTone1stByte)
              and $0f
              ld e,a      ; de = ????????0000ffff
              ld a,b
              and %00111111   ; also resets carry
              ld d,a      ; d = 00ffffff
              xor a
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
_PSGAnalysisEnd:
    pop bc
    jp GetData

WaitNSamples:
    ; get number
    rst GetByte
    ld e,a  ; read into e then d (big-endian)
    rst GetByte
    ld d,a
    ; fall through

_WaitDESamples:
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
    ld de,735
    add hl,de
    jp DoINeedToWait

Wait150th:  ; add 882 to the total number of samples
    ld de,882
    add hl,de
    jr DoINeedToWait

EndOfFile:
    call VGMDoLoop
    jp GetData

YM2413:
    ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
      rst GetByte
      out ($f0),a
      ld e,a      ; e = register

      rst GetByte ; Delay needed?
      out ($f1),a
      ld d,a      ; d = data
    ld a,(Port3EValue)
    out (PORT_MEMORY_CONTROL),a
    
    ; Analyse the register
    ld a,e
    and $f0
    cp $10
    jr z,_1x
    cp $20
    jr z,_2x
    jp GetData

_1x:    ; FNum low bits
    push bc
    push hl
      ld a,e
      and $0f
      cp $6
      jr nc,+  ; don't look at it if it's past $x5

      sla a
      ld b,0
      ld c,a
      ld hl,VGMYM2413FNums
      add hl,bc
      ld (hl),d   ; store new value (ignore high byte)
+:  pop hl
    pop bc
    jp GetData

_2x:    ; FNum high bit/block/key
    push bc
    push hl
      ld a,e
      and $0f
      ld e,a  ; e = channel number
      cp $6
      jr nc,+  ; don't look at it if it's past $5 (rhythm mode has 0-5 tone channels)

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
      
      ; If a YM2413 key down is seen, we want to enable FM.
      ; We only do it here because some VGMs may have FM initialisation, but enabling FM would mute PSG on a Mark III.
      ld a,d
      or a
      jr z,+
      ld a,(FMChipEnabled)
      or a
      jr nz,+
      call EnableFM

+:  pop hl
    pop bc
    jp GetData
/*
YMxxxx: ; discard 2 bytes
    rst GetByte
    rst GetByte
    jp GetData
*/


DoINeedToWait:
    ; If hl>=FrameLength then subtract FrameLength and exit loop
    ; otherwise loop

.ifdef Debugx
    ; Debug display of information (wait total)
    push hl
      ld hl, TilemapAddress(11, 25)
      call VRAMToHL
    pop hl
    ld a,h
    call WriteNumber
    ld a,l
    call WriteNumber
.endif

    ld (VGMWaitTotal),hl    ; store hl in memory
    ld de,(VGMFrameLength)
    ld a,(VGMPlayerState)   ; fast forward?
    cp VGMFast
    jr nz,+
    ; fast: multiply frame length by 4
    ; so we eat up 4x wait length
    push hl
      ld h,d
      ld l,e
      add hl,hl
      add hl,hl
      ld d,h
      ld e,l
    pop hl

+:  or a
    sbc hl,de               ; subtract samples per frame
    jr c,NoWait
    ; If we're here then the result is >=0
    ; so we want to put the new value in memory
    ld (VGMWaitTotal),hl

    ; add frame length to the time played
    push hl
      ld hl,(VGMTimeSamples)
      add hl,de
      ld (VGMTimeSamples),hl

      ld de,44100         ; see if it's over 44100
      or a
      sbc hl,de
      jr c,+

      ; If we're here then hl was >44100
      ; so increment the number of seconds
      ld a,1
      ld (SecondsChanged),a

      ; save the remainder samples
      ld (VGMTimeSamples),hl
      ; increment the seconds
      ld a,(VGMTimeSecs)
      inc a
      daa
      ld (VGMTimeSecs),a
      cp $60                  ; if it's at 60 seconds..
      jr nz,+
      xor a                ; zero the seconds
      ld (VGMTimeSecs),a
      ld a,(VGMTimeMins)
      inc a                   ; and add 1 minute
      daa
      ld (VGMTimeMins),a
+:  pop hl
    ; and exit to wait for the next vblank
    ld (VGMCounterLocation),bc
    jr EndGetDataLoop

NoWait:
    ; If we're here then the result is <0, ie. total<frame size 
    ; that also so I can safely add $8000 to pay back any overflows
    ld hl,(VGMWaitTotal)    ; restore hl

    ld a,(VGMWaitTotalOverflow)
    or a
    jp z,GetData            ; nothing to pay back

    ; Pay back an overflow
    dec a
    ld (VGMWaitTotalOverflow),a
    ld de,$8000
    add hl,de
    jr DoINeedToWait    ; Yes I do!

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
.section "No VGM file" free
_ClearScreen:
    ; Clear the screen
    ld hl,TilemapAddress(0,0)
    call VRAMToHL
    ld bc,$700
    xor a
  -:out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
    dec c
    jr nz,-
    dec b
    jr nz,-
    ret

NoVGMFile:
    call _ClearScreen

    ; Display a message
    ld iy,TilemapAddress(0,0)
    ld hl,NoVGMText
    call WriteASCII
/*
    ; Debug: display what we've found
    ld hl,TilemapAddress(0,0)
    call VRAMToHL
    ld c,160
    ld hl,$8000
    _Loop1:
        ld a,(hl)
        call WriteNumber
        inc hl
        dec c
        jr nz,_Loop1
*/
    ; Main screen turn on
    ld a,%11000000
    out ($bf),a
    ld a,$81
    out ($bf),a

  -:halt
    jr -


.ends

;==============================================================
; Visualisation routines
;==============================================================
.section "Visualisations" free
.define VisLocation TilemapAddress(0,12)

.define NumVisRoutines 5

VisRoutines:        ; calculation for vis, done after data is processed
.dw PianoVis,       FrequencyVis,         VolumeVis,            CalcSnow,       NoRoutine

VisDisplayRoutines: ; VBlank display update
.dw DrawPianoVis,   DrawVisBufferAsBars,  DrawVisBufferAsBars,  UpdateSnow,     NoRoutine

VisInitRoutines:    ; Initialisation
.dw PianoVisInit,   NoRoutine,            NoRoutine,            InitSnow,       NoVisInit

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

NextVis:
  push af
    ld a,(VisNumber)
    inc a
    cp NumVisRoutines
    jr nz,_NoWrap
    xor a
    _NoWrap:
    ld (VisNumber),a
    ld a,1
    ld (VisChanged),a
    call SaveSettings
  pop af
  ret

InitialiseVis:    ; Per-routine initialisation (runs in VBlank)
    call NoSprites

    ld a,(VisNumber)
    ; Check range
    cp NumVisRoutines
    jr c,+
    xor a ; zero if out of range
+:  add a,a ; a *= 2
    ld d,0
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
    push de
      call callHL
    pop de

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

    xor a                      ; Reset flag
    ld (VisChanged),a
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

ClearBuffer:   ; uses a,bc,de,hl
    ; Clear VisBuffer to all 0
    ld bc,_sizeof_VisBuffer-1
    ld hl,VisBuffer
    ld de,VisBuffer+1
    xor a
    ld (hl),a
    ldir
    ret

DrawVisBufferAsBars:
; Draw first 32 entries of VisBuffer as vertical bars of height == value
    ld hl,VisLocation
    call VRAMToHL
    ld c,4      ; Number of rows
--: ; Draw line
    ld b,32
    ld hl,VisBuffer
    ld d,c              ; d = 8*(c-1)
    dec d
    sla d
    sla d
    sla d
-:  ld a,(hl)           ; Get the height for this entry
    ; Convert to a tile height
    sub d               ; Subtract d = minimum value for this tile
    jr nc,_AboveMinimum
    xor a              ; If the result is negative then I want 0
    _AboveMinimum:
    cp 9                ; Is it 9 or more?
    jr c,_LessThan8
    ld a,8              ; If not, I want 8
    _LessThan8:
    add a,96 ; tile offset
    out ($BE),a
    push hl ; delay
    pop hl
    xor a
    out ($BE),a
    inc hl
    djnz -
    dec c
    jr nz,--
    ret

FrequencyVis:
    call ClearBuffer

    ld ix,VGMPSGTones
    ld iy,VGMPSGVolumes

    ld b,3  ; how many channels to do
-:  ld e,(ix+0)
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

    djnz -

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
        jr nz,_AddNoise
    ret

VolumeVis:
    call ClearBuffer

    ld hl,VGMPSGVolumes
    ld de,VisBuffer
    ld c,4          ; How many channels to do
--: ld a,(hl)   ; Get volume
    cpl
    and $0f
    sla a       ; Multiply by 2 to fill the vis
    ld b,8      ; Fill 8 slots with that
-:  ld (de),a
    inc de
    djnz -
    inc hl
    dec c
    jr nz,--
    ret

PianoVisInit:
    push af
    push bc
    push hl
        call ClearBuffer
        ; Draw tiles
        ld hl,VisLocation
        call VRAMToHL
        ld hl,PianoTileNumbers
        ; We may run into the active display so output it slower than otir
        ld c,$be
        ld b,64*2
-:      outi
        jr nz, -
        ; draw 2 blank lines
        xor a
        ld b,32*2*2
      -:out ($be),a
        dec b
        jr nz,-

        ; Define hand sprites
        ld hl,SpriteTableAddress
        call VRAMToHL
        ld a,208+16         ; Hand Y-position
        ld c,12*2
      -:out ($be),a
        dec c
        jr nz,-
        ld bc,128
        add hl,bc
        call VRAMToHL
        ld c,12
      -:xor a               ; Hand x-position
        out ($be),a
        nop                 ; Slow down
        nop
        ld a,$b0            ; Tile number (-$100 for spriteset 1)
        out ($be),a
        nop
        nop
        ld a,8
        out ($be),a
        nop
        nop
        ld a,$b3
        out ($be),a
        dec c
        jr nz,-
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
.db 196,198,200,202,204,208,210,212,214,216,218,220
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
      jr nz,+
      ld a,(VGMPSGNoiseIsPeriodic)
      cp 1
      jr nz,+
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
      jr z,_NoHand
      ld b,0          ; counter
      push iy
      ld iy,PianoVisMinValsPSG; values to look at
      scf
      ccf
    -:ccf             ; set carry flag -> sbc = <=
      ld h,(iy+1)     ; get value stored there
      ld l,(iy+0)
      sbc hl,de       ; Subtract actual value
      jr c,+          ; Loop until I find a value less than or equal to the note
      inc iy
      inc iy
      inc b
      jr -
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
      jr +
      _NoHand:    ; Don't show the hand if applicable
      ld (iy+1),208+16
  +:pop bc
    dec c
    inc ix
    inc ix
    inc iy
    inc iy
    jr nz,--

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
    jr z,_NoHand2
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
    jr z,+              ; ship shifting if a=0
  -:sla l               ; shift hl by Block bits
    rl  h
    jr c,_Overflow
    dec a
    jr nz,-
  +:push hl
    pop de              ; Now de = FNum << Block
    jr _DEtoNoteNum
    _Overflow:          ; If, when shifting FNum by Block, it overflows 16 bits then act as if the value was 65535
    ld de,65534
;    jr _DEtoNoteNum
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
    jr nc,+         ; Loop until I find a value greater than the note
    inc iy
    inc iy
    inc b
    jr -
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
    jr +
    _NoHand2:    ; Don't show the hand if applicable
    ld (iy+1),208+16
  +:dec c
    inc ix
    inc iy
    inc iy
    jr nz,--

    ret

DrawPianoVis:
    ; Load hand x,y positions from VisBuffer
    ; Set sprite positions accordingly

    .define NumHands 9
;    push af
;    push bc
;    push ix
;    push hl

        ld c,$be    ; VDP port

        ld hl,SpriteTableAddress
        call VRAMToHL
        
        ld a,(VisBuffer+20)
        xor 1
        ld (VisBuffer+20),a
        jr nz,_reverse

        ld ix,VisBuffer

        ld b,NumHands
      -:ld a,(ix+1)     ; y-pos
        out ($be),a
        nop
        nop
        nop
        nop
        out ($be),a
        inc ix
        inc ix
        dec b
        jr nz,-

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
            jr nz,- ; 10 = 53 :P
;    pop hl
;    pop ix
;    pop bc
;    pop af
    ret

_reverse:
        ld ix,VisBuffer+NumHands*2-2

        ld b,NumHands
      -:ld a,(ix+1)     ; y-pos
        out ($be),a
        nop
        nop
        nop
        nop
        out ($be),a
        dec ix
        dec ix
        dec b
        jr nz,-

        ld bc,128
        add hl,bc
        call VRAMToHL

        ld ix,VisBuffer+NumHands*2-2
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
            dec ix  ; 10
            dec ix  ; 10
            jr nz,- ; 10 = 53 :P
;    pop hl
;    pop ix
;    pop bc
;    pop af
    ret

NoVisInit:
  call ClearBuffer
  call DrawVisBufferAsBars
  ret

.define NumSnowFlakes 32

SnowText:
.db 10,"  Dancing snow..",10,10
.db "               ..with flashing",0

InitSnow:
    push af
    push bc
    push hl
        ; Blank vis area
        ld hl,VisLocation
        call VRAMToHL
        ld bc,32*2*5
      -:xor a
        out ($be),a
        dec bc
        ld a,b
        or c
        jr nz,-

        ; Fill vis area with my text
        ld hl,SnowText
        ld iy,VisLocation
        call WriteASCII

        ; Define sprites
        ld hl,SpriteTableAddress+128
        call VRAMToHL
        ld c,NumSnowFlakes
      -:xor a
        out ($be),a
        ld a,$b5                ; Tile number - blank
        push ix
        pop ix      ; delay
        out ($be),a
        dec c
        jr nz,-

        ; Fill buffer with initial random poitions
        ld hl,VisBuffer
        ld c,NumSnowFlakes*2
        -:
        call GetRandomNumber
        ld (hl),a
        inc hl
        dec c
        jr nz,-

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
        jr c,+
    pop af  ; vol high
    add a,(ix+1) ; change value semi-randomly
    jr ++

    +: ; vol low
    pop af
    ; no change
    ++:
    pop bc
    pop hl

    and $3      ; remove high bits
    dec a       ; now it's -1, 0, +1 or +2
    cp 2
    jr nz,+
    xor a      ; make +2 -> 0
    +:

    add a,(ix+0); Add it to the current value
    ld (ix+0),a ; that's what I want
    inc ix
    dec c
    jr nz,-

    ld c,NumSnowFlakes
    -:          ; Y values:
    ld a,(ix+0) ; get current value
    inc a       ; increment
    ld (ix+0),a ; that's what I want
    cp $ff-$20  ; If it's time to change..
    jr nz,+
    ; I want a new random x-pos
    call GetRandomNumber
    and 248
    ld (ix+(-NumSnowFlakes)),a
    +:
    inc ix
    dec c
    jr nz,-

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
;        cp $0
        jr z,+
        ; noise on -> light cyan
        ld c,colour(2,3,3)
        jr ++
      +:; noise off -> white
        ld c,colour(3,3,3)

     ++:; Write colour to CRAM
        ld a,22                ; palette index 22
        out ($bf),a
        ld a,%11000000         ; CRAM write
        out ($bf),a
        ld a,c                 ; data
        out ($be),a

        ; Display sprites
        ld hl,SpriteTableAddress+128
        call VRAMToHL
        ; Write X positions
        ld c,NumSnowFlakes
        ld ix,VisBuffer
      -:ld a,(ix+0)
        out ($be),a
        ; Sprite number $b4+
        ld a,c      ; snowflake number
        add a,(ix+0); x-pos
        rra
        add a,(ix+NumSnowFlakes)
        rra
        rra         ; look at a higher bit for slower changing
        and %110
        add a,$b4   ; start of snowflake sprites
        out ($be),a ; skip sprite number
        inc ix
        dec c
        jr nz,-

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
        jr nz,-

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
    jr nz,-

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
.db %00000100,$80
;    |||||||`- Disable synch
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during horizontal scrolling
;    `-------- Fix right 8 columns during vertical scrolling
.db %10000000,$81
;     ||||||`- Zoomed sprites -> 16x16 pixels
;     |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;     ||||`--- Mode5 bit on PBC - must be 0
;     |||`---- 30 row/240 line mode
;     ||`----- 28 row/224 line mode
;     |`------ Enable VBlank interrupts
;     `------- Enable display
.db (NameTableAddress>>10) |%11110001,$82
.db (SpriteTableAddress>>7)|%10000001,$85
.db (SpriteSet<<2)         |%11111011,$86
.db $0|$f0,$87
;    `-------- Border palette colour (sprite palette)
.db $00,$88
;    ``------- Horizontal scroll
.db $00,$89
;    ``------- Vertical scroll
.db $ff,$8a
;    ``------- Line interrupt spacing ($ff to disable)


;==============================================================
; My chosen palette
;==============================================================
PaletteData:
.incbin "art\big-numbers.palette"
.db 0 ; black border
.incbin "art\sprites.palette" skip 1

Palettes:
.db colour(0,1,2),colour(1,2,3),colour(2,3,3)
.db colour(0,1,0),colour(1,2,2),colour(2,3,2)  ; original (green)
.db colour(0,0,1),colour(1,1,2),colour(2,2,3)  ; blue
.db colour(1,0,0),colour(2,1,1),colour(3,2,2)  ; red
.db colour(0,2,3),colour(2,2,3),colour(2,3,3)  ; light blue
.db colour(0,0,0),colour(1,1,1),colour(2,2,2)  ; greyscale
.db colour(3,0,0),colour(3,2,2),colour(3,2,3)  ; bright red

TileData:
.incbin "fonts\ZXChicagoPod.tiles.withdupes.pscompr"

BigNumbers:
.incbin "art\big-numbers.tiles.pscompr"

Pad:
.incbin "art\3d-pad.tiles.pscompr"

PadData:
.incbin "art\3d-pad.lsbtilemap"

ScaleData:
.incbin "art\scale.tiles.pscompr"

TextData:
.incbin "Text.txt"
.db $00

NoVGMText:
.incbin "no-vgm-file-message.txt"
.db $00

PianoTiles:
.incbin "art\piano.tiles.pscompr"

PianoTileNumbers:
.include "art\piano.tilemap.inc"

Sprites:
.incbin "art\sprites.tiles.pscompr"

.section "VGM logo displayer" free
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
    ; I think this is some almighty kludge I invented?
    ; TODO fix this horror
    inc sp
    inc sp
    inc sp
    inc sp
    ret

DisplayVGMLogo:
    call NoSprites

    ; Set all palette entries to white
    xor a          ; palette index 0
    out ($bf),a
    ld a,$c0
    out ($bf),a     ; CRAM write
    ld c,32
    ld a,colour(3,3,3)
    _WhiteLoop:
        out ($be),a
        dec c
        jr nz,_WhiteLoop

    ; Load tiles
    ld de,$4000
    ld hl,LogoTiles
    call LoadTiles4BitRLENoDI

    ld hl,TilemapAddress(0,1)
    call VRAMToHL

    ; Draw tiles
    ld de,$7800
    ld hl,LogoTileNumbers
    call LoadTilemapToVRAM

    ; Set VBlank routine
    ld hl,SplashScreenVBlank
    ld (VBlankRoutine),hl

    ; Turn screen on
    ld a,%11100000
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
    jr nz,_FadeIn

    ld c,20 ; how many frames to wait
    _WaitLoop:
        ei
        halt
        dec c
        jr nz,_WaitLoop

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
    jr nz,_FadeOut

    ret

;==============================================================
; Data
;==============================================================

LogoPalette:
.db colour(2,2,2),colour(1,1,1),colour(0,0,0)

FadeOutPalette:
.db colour(0,1,0),colour(1,2,1),colour(2,3,2),colour(3,3,3),colour(2,2,2),colour(1,1,1),colour(0,0,0)

LogoTiles:
.incbin "art\vgm-logo.tiles.pscompr"

LogoTileNumbers:
.incbin "art\vgm-logo.tilemap.pscompr"

.ends

.section "Save/load settings in BBRAM" FREE
_Marker:
.db "VGM"

SaveSettings:
    push af
    push bc
    push hl
    push de
        ld a,%00001000  ; select BBRAM
        ld ($fffc),a
        ; write marker
        ld bc,_sizeof__Marker
        ld hl,_Marker
        ld de,$8001
        ldir
        ; write data
        ld a,(VisNumber)
        ld ($8004),a
        ; write data
        ld a,(PaletteNumber)
        ld ($8005),a

        xor a          ; deselect BBRAM
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
        ld c,_sizeof__Marker
        ld hl,_Marker
        ld ix,$8001
        ; I have to do this one myself..
        -:
            ld a,(hl)   ; read ROM
            cp (ix+0)   ; compare to RAM
            ; if they aren't equal then exit
            jr nz,_Error
            inc hl
            inc ix
            dec c
            jr nz,-

        ; If we're here then ther marker is OK, so read stuff in
        ld a,($8004)
        ld (VisNumber),a
        ld a,($8005)
        ld (PaletteNumber),a
        jr ++
        
        _Error:
        xor a
        ld (VisNumber),a
        ld (PaletteNumber),a

        ++:
        xor a          ; deselect BBRAM
        ld ($fffc),a
    pop ix
    pop hl
    pop bc
    pop af
    ret

.ends

.section "Check port 3e value" free
CheckPort3EValue:
  ld a,(Port3EValue)
  cpl               ; make 1 = true
  and %11101000     ; mask to expansion, cart, card, BIOS slots
  cp  %10000000
  ret z
  cp  %01000000
  ret z
  cp  %00100000
  ret z
  cp  %00001000
  ret z
  ; value is obviously wrong - assume cartridge slot
  ld a,%10101011 ; enable normal stuff
  ld (Port3EValue),a
  ret
.ends

.section "FM enable" free
EnableFM:
; FM hardware enabled by setting bit 0 of port $f2
; Port $3E:
; 1 = disable, 0 = enable
; %10101100
;  ||||||``-- unknown
;  |||||`---- I/O
;  ||||`----- BIOS *
;  |||`------ RAM
;  ||`------- card *
;  |`-------- cartridge *
;  `--------- expansion slot *
; * mutually exclusive

  ; disable I/O chip
  ld a,(Port3EValue)
  set 2,a ; set bit 2 to disable the I/O chip
  out (PORT_MEMORY_CONTROL),a

  ; https://www.smspower.org/Development/AudioControlPort
  ld a,$03
  out (PORT_AUDIO_CONTROL),a

  ; enable I/O chip
  ld a,(Port3EValue)
  out (PORT_MEMORY_CONTROL),a
  
  ld a,1
  ld (FMChipEnabled),a
  ret
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

;==============================================================
; Clear VRAM
;==============================================================
.section "Clear VRAM" free
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
            ld a,(hl)    ; Value to write
            cp $00        ; compare a with $00, set z flag if they match
            jp z,_WriteTilesLoopEnd    ; if so, it's the string end so stop writing it
            cp 10        ; Check for LF
            jp z,_NewLine
            sub $20
            jp c,_SkipControlChar
            out ($BE),a    ; Output to VRAM address, which is auto-incremented after each write
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
    push hl
    pop hl
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
    ;   On Export systems, you read back the current output; on Japanese, it is the user input.
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
    cp   %11000000  ; If the bits are not 1 then it's definitely Japanese
    jp nz,_IsJap

    ld a,%01010101  ; Set both TH to output and output 0s
    out ($3f),a
    in a,($dd)
    and %11000000  ; See what the TH inputs are
    jp nz,_IsJap    ; If the bits are not 0 then it's definitely Japanese

    ld a,%11111111  ; Set everything back to being inputs
    out ($3f),a

    xor a
    ret

    _IsJap:
    ld a,1
    ret
.ends

.section "FM detection" FREE
;==============================================================
; FM detector
; Returns a=1 for FM chip found, 0 otherwise
;==============================================================
HasFMChip:
    ; If there's an FM chip I should be able to modify $f2
    ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
      in a,(PORT_AUDIO_CONTROL)      ; Get what's there
      ld b,a
      xor %00000001   ; Toggle bit 0
      out (PORT_AUDIO_CONTROL),a     ; Write it out
      in a,(PORT_AUDIO_CONTROL)      ; Read it back
      cp b
      ; Write a 0 at the end to be in PSG mode
      ld a,0
      out (PORT_AUDIO_CONTROL),a
    ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
    jp z,_NoFM
    ld a,1
    ret
    _NoFM:
    xor a
    ret
.ends

