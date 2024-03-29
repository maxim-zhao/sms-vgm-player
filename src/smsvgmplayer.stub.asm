; SMS VGM player
; by Maxim

; VRAM mapping stuff
.define SpriteSet               1       ; 0 for sprites to use tiles 0-255, 1 for 256+
.define TilemapBaseAddress      $3800   ; must be a multiple of $800; usually $3800; fills $700 bytes (unstretched)
.define SpriteTableBaseAddress  $3f00   ; must be a multiple of $100; usually $3f00; fills $100 bytes

;.define Debug

.define VGMSTARTPAGE 1 ; 1 for 16KB, 2 for 32KB

; This is sensitive to our font image
.asciitable
map 0 = $ff
map "\n" = $fe
map " " to "\"" = 0
map "&" to ")" = $3
map "+" to ";" = $7
map "?" = $18
map "A" to "Z" = $19
map "a" to "z" = $33
map "~" = $4d
.enda

.function TilemapAddress(x, y) TilemapBaseAddress+2*(x+y*32) 
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

.macro SetDebugColourB()
  .ifdef Debug
    push af
      ld a,$10
      out ($bf),a
      ld a,$c0
      out ($bf),a
      ld a,b
      out ($be),a
    pop af
  .endif
.endm

; WLA-DX banking setup
.if VGMSTARTPAGE == 1

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

.else

.memorymap
defaultslot 0
slotsize $8000
slot 0 $0000
.endme

.rombankmap
bankstotal 1
banksize $8000
banks 1
.endro

.endif

.bank 0 slot 0
.org $0000

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.sdsctag 2.0,"SMS VGM player",SDSCNotes,"Maxim"
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
ButtonState                     db
LastButtonState                 db
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
VisBuffer                       dsb 512 ; Visualisers can do as they wish with this
LoopsChanged                    db
PaletteChanged                  db
PaletteNumber                   db
RandomSR                        dw ; Random number generator shift register, needs seeding
GD3DisplayerBuffer              dsb 33
VGMMemoryStart                  dsb 256
VDPRegister81Value              db
InitVisDI                       db
PSGDecoderBuffer                dsb 34
.ende

.bank 0 slot 0
.org 0

.include "PhantasyStardecompressors.asm"
.include "PhantasyStarGaidenDecompressor.asm"

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
  SetDebugColour(0,1,0) ; Dark green
  call VGMUpdate      ; Read/handle sound data
  SetDebugColour(1,0,0) ; Dark red
  call CheckInput ; Read input into memory
  call ShowTime   ; Update time display
  call ShowLoopNumber
  SetDebugColour(2,0,0) ; Medium red

  ld a,(PaletteChanged)
  or a
  call nz,NextPalette ; Try to get CRAM dots in VBlank

  ld a,(VisChanged)
  or a
  call nz,InitialiseVis ; If the vis has changed then I need to do the initialisation in the vblank

  ld hl,(VisDisplayRoutine)   ; Draw vis
  SetDebugColour(3,0,0) ; Bright red
  call callHL
  SetDebugColour(0,0,0) ; All done

  ret
.ends

.org $0066
;==============================================================
; Pause button handler
;==============================================================
.section "!NMI handler" FORCE
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
  ld a,VGMSTARTPAGE
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
  ld hl,VDPRegisterInitData
  ld b,_sizeof_VDPRegisterInitData
  ld c,$bf
  otir
    
  ; Clear RAM
  ld hl,$c000+1
  ld de,$c000+2
  ld bc,$1ff0-1
  ld (hl),0
  ldir

  ld a,(VDPRegisterInitData+2)
  ld (VDPRegister81Value),a

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

.function TileVRAMAddressFromIndex(n) $4000+n*32

  ; Load tiles
  ld hl,TileVRAMAddressFromIndex(TileIndex_Font) ; Load font
  ld ix,TileData
  call PSG_decompress

  ld hl,TileVRAMAddressFromIndex(TileIndex_BigNumbers)
  ld ix,BigNumbers
  call PSG_decompress

  ld hl,TileVRAMAddressFromIndex(TileIndex_3DPad)
  ld ix,Pad
  call PSG_decompress

  ; Initial button state values (all off)
  ld a,$ff
  ld (LastButtonState),a
  ld (ButtonState),a

  ; Draw text
  ld iy,TilemapAddress(0, 0)
  ld hl,TextData
  call WriteASCII
  
  ; Fix up colons
  ld hl,TilemapAddress(6,6)
  call VRAMToHL
  ld a,<(TileIndex_BigNumbers+17)
  out ($be),a
  ld a,>(TileIndex_BigNumbers+17)
  out ($be),a
  ld hl,TilemapAddress(6,7)
  call VRAMToHL
  ld a,<(TileIndex_BigNumbers+17)
  out ($be),a
  ld a,>(TileIndex_BigNumbers+17)
  out ($be),a

  ; Draw pad image
  ld bc,$0c0b     ; 12x11
  ld ix,PadData
  ld iy,TilemapAddress(19, 1)
  call DrawImageArea

  ; Put something in the shift register
  ld a,r
  ld l,a
  ld h,$55
  ld (RandomSR),hl

  ; Load settings, initialise stuff
  call LoadSettings
  call UpdatePalette
  xor a
  ld (InitVisDI),a
  call InitialiseVis
  ld a,1
  ld (InitVisDI),a

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

  ; Turn screen on
  ld a,(VDPRegister81Value)
  or %01000000
  ld (VDPRegister81Value),a
  out ($bf),a
  ld a,$81
  out ($bf),a

  ; Fake a VBlank to initialise various stuff
  call VGMPlayerVBlank

  ei
InfiniteLoop:   ; to stop the program
  halt
  SetDebugColour(0,2,0) ; Medium green
  call ProcessInput   ; Process input from the last VBlank
  SetDebugColour(0,3,0) ; Bright green
  ld hl,(VisRoutine)  ; Vis processing
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
      ld a,VGMSTARTPAGE
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
      .repeat VGMSTARTPAGE
      inc a ; VGM stub is one page
      .endr
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
    cp $c0  ; Is it c?
    jr nz,+
    ; Need to decrement by $4000
    res 6,h
    ; and page in the next page
    ld a,(PAGING_SLOT_2)
    inc a
    ld (PAGING_SLOT_2),a
+:pop af
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
      
      ; We prefer the English or Japanese according to the system region
      call _DrawGd3StringForRegion ; Title
      call _DrawGd3StringForRegion ; Game
      call _DrawGd3StringForRegion ; System
      call _DrawGd3StringForRegion ; Author
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

_DrawGd3StringForRegion:
  ; hl points to the strings, null-separated
  push hl ; preserve current pointer (to English string)
    ld a,(hl)
    inc hl
    or (hl)
    inc hl
    jr nz, +
    ; No English, use Japanese regardless of region
    call _DrawGD3String
-:pop de ; discard preserved value
  ret
  
+:  ; We have English, do we prefer it?
    ld a,(IsJapConsole)
    ;inc a ; test: force to Japanese
    or a
    jr nz,+
    ; English please
    dec hl
    dec hl
    call _DrawGD3String
    call _SkipGD3String
    jr -
  
+:  ; Japanese preferred, but do we have it?
    call _SkipGD3String
    ld a,(hl)
    inc hl
    or (hl)
    jr z, + ; No Japanese, try English
    ; Else use it
    inc hl
    call _DrawGD3String
    jr -
    
+:pop hl
  call _DrawGD3String
  jp _SkipGD3String ; and ret
  
  
  
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
-:  ; Get value into bc. GD3 uses UTF-16.
    ld c,(hl)
    call MoveHLForward
    ld b,(hl)
    call MoveHLForward
    push af ; backup char counter
      ld a,b
      or c
      jr z,_done
      ; Map character from UTF-16 to our font
      call _MapToFont
      ld (de),a
    pop af
    inc de
    dec a   ; decrement counter
    jr nz,-
    ; If it gets to zero:
    call _SkipGD3String   ; go to the end of the string
    jr +
    ; fall through to finish

_done:
    pop af
+:  push hl ; hl is returned
      ex de,hl
      cp 32           ; If no chars were written
      jr nz,+
      ld (hl),' ' ; put a space so it'll work properly
      inc hl
+:    ld (hl),ASC('\n')
      inc hl
      ld (hl),ASC(0)
      ld hl,GD3DisplayerBuffer
      call WriteASCII
    pop hl
  pop bc
  pop de
  pop af
  ret

_MapToFont:
  ; bc = UTF-16 word
  ; Return a = character
  ; Our font is in Unicode order but is not a full set. We therefore apply some offsetting.
  push hl
  push de
    ; fullwidth is in the range $ff00+
    ; We want to convert it to normal width in the range $0020+
    ; mapping to bc-$ff00+$20 = bc+$120
    ld a,b
    cp $ff
    jr nz,+
; Halfwidth is there too, do we need to support it? Maybe, if it is easy?
    ld hl,$0120
    add hl,bc
    ld b,h
    ld c,l
  
+:  ld hl,_lookupData+3 ; No need to start at 0, we backtrack to it if needed
    ld d,_sizeof__lookupData / 3
-:  ; Read word from table
    ld a,(hl)
    inc hl
    push hl
      ld h,(hl)
      ld l,a
      ; Add to bc, we will get carry if -hl>bc
      add hl,bc
      jr nc,+
      ; Carry means -hl wasn't big enough yet
    pop hl
    inc hl
    inc hl
    dec d
    ; We ran out of table
    jr nz,-
--: ld a,ASC('?') ; no match found
-:pop de
  pop hl
  ret

+:  pop hl
    ; If we get a carry then we want the previous entry's data. We are pointing at byte 2 of the following entry...
    dec hl
    dec hl
    ld a,(hl) ; byte offset
    cp -1
    jr z,-- ; -1 means unhandled range
    dec hl
    ld e,(hl)
    dec hl
    ld l,(hl)
    ld h,e
    add hl,bc
    add l
    ; now a = what we wanted
    jr -

; UTF-16 and index to use
_lookupData:
.table dw, db
; For each range of 16-bit Unicode, subtract the word and add the byte to get a tile index.
; If the byte is -1, it's an invalid char.
;  !"&'()+,-./0123456789:;?ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz~、いうきくさしすたでとのぷやょよるんァアィイウェエォオカガキギクグケゲコゴサザシジスズセソゾタダチッテデトドナニハバパヒビピフブプヘベペホボポマミムメモャュョラリルレロワンヴ・三上中之二井仁代伝佳保優元克内出剣千博口古史司和圭坂基塚士外大子宮小尾山岩島崎川幸康彦徳忍志忠慎戦早昌朗木本村林橋武河沼法洋王生田知祐福秘穂竹織考草藤行説谷通進郎野鎌長陽雀雄魔
.row $0000, -1
.row $ffe0, $00 ; " !""
.row $ffdd, -1
.row $ffda, $03 ; "&'()"
.row $ffd6, -1
.row $ffd5, $07 ; "+,-./0123456789:;"
.row $ffc4, -1
.row $ffc1, $18 ; "?"
.row $ffc0, -1
.row $ffbf, $19 ; "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
.row $ffa5, -1
.row $ff9f, $33 ; "abcdefghijklmnopqrstuvwxyz"
.row $ff85, -1
.row $ff82, $4d ; "~"
.row $ff81, -1
.row $ff4e, $0e ; "²³"
.row $ff4c, -1
.row $ff49, $a5 ; "·"
.row $ff48, -1
.row $ff1f, $33 ; "á"
.row $ff1e, -1
.row $ff17, $37 ; "é"
.row $ff16, -1
.row $cfff, $4e ; "、"
.row $cffe, -1
.row $cfbc, $4f ; "い"
.row $cfbb, -1
.row $cfba, $50 ; "う"
.row $cfb9, -1
.row $cfb3, $51 ; "き"
.row $cfb2, -1
.row $cfb1, $52 ; "く"
.row $cfb0, -1
.row $cfab, $53 ; "さ"
.row $cfaa, -1
.row $cfa9, $54 ; "し"
.row $cfa8, -1
.row $cfa7, $55 ; "す"
.row $cfa6, -1
.row $cfa1, $56 ; "た"
.row $cfa0, -1
.row $cf99, $57 ; "でと"
.row $cf97, -1
.row $cf92, $59 ; "の"
.row $cf91, -1
.row $cf89, $5a ; "ぷ"
.row $cf88, -1
.row $cf7c, $5b ; "や"
.row $cf7b, -1
.row $cf79, $5c ; "ょよ"
.row $cf77, -1
.row $cf75, $5e ; "る"
.row $cf74, -1
.row $cf6d, $5f ; "ん"
.row $cf6c, -1
.row $cf5f, $60 ; "ァアィイ"
.row $cf5b, -1
.row $cf5a, $64 ; "ウェエォオカガキギクグケゲコゴサザシジスズセ"
.row $cf44, -1
.row $cf43, $7a ; "ソゾタダチ"
.row $cf3e, -1
.row $cf3d, $7f ; "ッ"
.row $cf3c, -1
.row $cf3a, $80 ; "テデトドナニ"
.row $cf34, -1
.row $cf31, $86 ; "ハバパヒビピフブプヘベペホボポマミムメモャ"
.row $cf1c, -1
.row $cf1b, $9b ; "ュ"
.row $cf1a, -1
.row $cf19, $9c ; "ョ"
.row $cf18, -1
.row $cf17, $9d ; "ラリルレロ"
.row $cf12, -1
.row $cf11, $a2 ; "ワ"
.row $cf10, -1
.row $cf0d, $a3 ; "ンヴ"
.row $cf0b, -1
.row $cf05, $a5 ; "・ー"
.row $cf03, -1
.row $b200, $09 ; "一"
.row $b1ff, -1
.row $b1f7, $a6 ; "三上"
.row $b1f5, -1
.row $b1d3, $a8 ; "中"
.row $b1d2, -1
.row $b1b5, $a9 ; "之"
.row $b1b4, -1
.row $b174, $aa ; "二"
.row $b173, -1
.row $b16b, $ab ; "井"
.row $b16a, -1
.row $b13f, $ac ; "仁"
.row $b13e, -1
.row $b11d, $ad ; "代"
.row $b11c, -1
.row $b0e3, $ae ; "伝"
.row $b0e2, -1
.row $b08d, $af ; "佳"
.row $b08c, -1
.row $b023, $b0 ; "保"
.row $b022, -1
.row $aed6, $b1 ; "優"
.row $aed5, -1
.row $aebd, $b2 ; "元"
.row $aebc, -1
.row $aeb5, $b3 ; "克"
.row $aeb4, -1
.row $ae7b, $b4 ; "内"
.row $ae7a, -1
.row $ae06, $b5 ; "出"
.row $ae05, -1
.row $ad9d, $b6 ; "剣"
.row $ad9c, -1
.row $acbd, $b7 ; "千"
.row $acbc, -1
.row $aca6, $b8 ; "博"
.row $aca5, -1
.row $ac1d, $b9 ; "口古"
.row $ac1b, -1
.row $ac0e, $bb ; "史"
.row $ac0d, -1
.row $ac08, $bc ; "司"
.row $ac07, -1
.row $ab74, $bd ; "和"
.row $ab73, -1
.row $a8d3, $be ; "圭"
.row $a8d2, -1
.row $a8be, $bf ; "坂"
.row $a8bd, -1
.row $a806, $c0 ; "基"
.row $a805, -1
.row $a7a6, $c1 ; "塚"
.row $a7a5, -1
.row $a715, $c2 ; "士"
.row $a714, -1
.row $a6ea, $c3 ; "外"
.row $a6e9, -1
.row $a6d9, $c4 ; "大"
.row $a6d8, -1
.row $a4b0, $c5 ; "子"
.row $a4af, -1
.row $a452, $c6 ; "宮"
.row $a451, -1
.row $a3f1, $c7 ; "小"
.row $a3f0, -1
.row $a3c2, $c8 ; "尾"
.row $a3c1, -1
.row $a38f, $c9 ; "山"
.row $a38e, -1
.row $a357, $ca ; "岩"
.row $a356, -1
.row $a30a, $cb ; "島"
.row $a309, -1
.row $a2f2, $cc ; "崎"
.row $a2f1, -1
.row $a223, $cd ; "川"
.row $a222, -1
.row $a188, $ce ; "幸"
.row $a187, -1
.row $a149, $cf ; "康"
.row $a148, -1
.row $a09a, $d0 ; "彦"
.row $a099, -1
.row $a04d, $d1 ; "徳"
.row $a04c, -1
.row $a033, $d2 ; "忍"
.row $a032, -1
.row $a029, $d3 ; "志"
.row $a028, -1
.row $a020, $d4 ; "忠"
.row $a01f, -1
.row $9eb2, $d5 ; "慎"
.row $9eb1, -1
.row $9dda, $d6 ; "戦"
.row $9dd9, -1
.row $9a17, $d7 ; "早"
.row $9a16, -1
.row $99f4, $d8 ; "昌"
.row $99f3, -1
.row $98e9, $d9 ; "朗"
.row $98e8, -1
.row $98d8, $da ; "木"
.row $98d7, -1
.row $98d4, $db ; "本"
.row $98d3, -1
.row $98af, $dc ; "村"
.row $98ae, -1
.row $9869, $dd ; "林"
.row $9868, -1
.row $95b5, $de ; "橋"
.row $95b4, -1
.row $949a, $df ; "武"
.row $9499, -1
.row $934d, $e0 ; "河"
.row $934c, -1
.row $9344, $e1 ; "沼"
.row $9343, -1
.row $932b, $e2 ; "法"
.row $932a, -1
.row $92f5, $e3 ; "洋"
.row $92f4, -1
.row $8c75, $e4 ; "王"
.row $8c74, -1
.row $8ae1, $e5 ; "生"
.row $8ae0, -1
.row $8ad0, $e6 ; "田"
.row $8acf, -1
.row $881b, $e7 ; "知"
.row $881a, -1
.row $86b0, $e8 ; "祐"
.row $86af, -1
.row $8671, $e9 ; "福"
.row $8670, -1
.row $8628, $ea ; "秘"
.row $8627, -1
.row $85be, $eb ; "穂"
.row $85bd, -1
.row $8507, $ec ; "竹"
.row $8506, -1
.row $81ac, $ed ; "織"
.row $81ab, -1
.row $7ffd, $ee ; "考"
.row $7ffc, -1
.row $7cb7, $ef ; "草"
.row $7cb6, -1
.row $7a1c, $f0 ; "藤"
.row $7a1b, -1
.row $77b4, $f1 ; "行"
.row $77b3, -1
.row $7554, $f2 ; "説"
.row $7553, -1
.row $73c9, $f3 ; "谷"
.row $73c8, -1
.row $6fe6, $f4 ; "通"
.row $6fe5, -1
.row $6fce, $f5 ; "進"
.row $6fcd, -1
.row $6f32, $f6 ; "郎"
.row $6f31, -1
.row $6e32, $f7 ; "野"
.row $6e31, -1
.row $6c74, $f8 ; "鎌"
.row $6c73, -1
.row $6a89, $f9 ; "長"
.row $6a88, -1
.row $6983, $fa ; "陽"
.row $6982, -1
.row $6940, $fb ; "雀"
.row $693f, -1
.row $693c, $fc ; "雄"
.row $693b, -1
.row $64ac, $fd ; "魔"
.endt
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
; at screen position hl. Preserve bc, de, hl; trash a
;==============================================================
DrawLargeDigit:
  call VRAMToHL
  push bc
  push de
  push hl
    add a,a
    add a,a ; multiply by 4
    push hl
      ld hl,BigNumbersTilemap
      add l
      ld l,a
      adc h
      sub l
      ld h,a
      call _drawTwoTiles ; leaves hl pointing at the last byte used
      
      ; One row below is 39 bytes after that
      ld bc,39
      add hl,bc
      ; Save that
      ex de,hl
    pop hl
    
    ld bc,64
    add hl,bc
    call VRAMToHL
    
    ex de,hl
    call _drawTwoTiles
  pop hl
  pop de
  pop bc
  ret
_drawTwoTiles:
  ld a,(hl)
  out ($be),a               ; 11 -> start
  inc hl                    ; 6
  ld a,(hl)                 ; 7
  nop                       ; 4
  out ($be),a               ; 11 -> 28 cycles
  inc hl                    ; 6
  ld a,(hl)                 ; 7
  nop                       ; 4
  out ($be),a               ; 11 -> 28 cycles
  inc hl                    ; 6
  ld a,(hl)                 ; 7
  nop                       ; 4
  out ($be),a               ; 11 -> 28 cycles
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
.enum VGMMemoryStart export
VGMCounterLocation      dw      ; VGM data pointer
VGMWaitTotal            dw      ; Wait length total
VGMFrameLength          dw      ; Amount to wait per frame (allows fast-forwarding :P)
VGMLoopPage             db      ; Loop point page
VGMLoopOffset           dw      ; Loop point offset when paged in
VGMPlayerState          db      ; Player state - playing, paused, stopped, etc
VGMTimeMins             db      ; Time (mins), BCD
VGMTimeSecs             db      ; Time (secs), BCD
VGMTimeSamples          dw      ; Time (samples)
VGMLoopsPlayed          db      ; Number of loops played
VGMPSGVolumes           dsb 4   ; PSG volumes (channels 0-3)
VGMPSGTones             dsb 6   ; PSG tone values
VGMPSGTone1stByte       db      ; PSG tone first byte
VGMPSGNoiseMode         db      ; PSG noise mode in low 3 bits
VGMWaitTotalOverflow    db      ; Kludgy :/
VGMYM2413Registers      dsb $38 ; Just the registers, analyse yourself :)
VGMStartPage            db      ; Start point page
VGMStartOffset          dw      ; Start point offset when paged in
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
+:  ld hl, TilemapAddress(10, 9)
    call VRAMToHL
    ld hl,0
    ld de,60
    call Divide16   ; hl = seconds, bc = minutes
    ld a,c
    call Hex2BCD
    call WriteNumber
    ld a,ASC(':')   ; Draw colon (faster than redefining name table address)
    out ($be),a
    push hl
    pop hl
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
    ld a,ASC(':')  ; Draw colon (faster than redefining name table address)
    out ($be),a
    push hl
    pop hl
    ld a,0
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
    ld a,VGMSTARTPAGE
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

    ld hl,VGMYM2413Registers    ; Zero registers
    xor a
    ld c,$38
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

    ; YM2413: volumes to $f
    ld c,$30
    ld b,9
    ld hl,VGMYM2413Registers+$30
-:  ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
      ld a,c
      out ($f0),a
      ld a,(hl)
      and %11110000
      or  %00001111
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

    ld hl,VGMYM2413Registers+$30
    ld c,$30
    ld b,9
-:  ld a,(Port3EValue)
    set 2,a
    out (PORT_MEMORY_CONTROL),a
      ld a,c
      out ($f0),a
      ld a,(hl)
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
      ld a,$ff        ; reset VGMPSGTone1stByte if it's not a tone write
      ld (VGMPSGTone1stByte),a

      jr _PSGAnalysisEnd

      _Tone1:         ; Tone1
          ld a,b
          ld (VGMPSGTone1stByte),a

          and %01100000   ; Is it ch3?
          cp  %01100000
          jp nz,+++
          ld a,b
          and %00000111
          ld (VGMPSGNoiseMode),a
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
    
    ; Store the value
    ld a,e
    cp $39
    jp nc,GetData
    push hl
    push bc
      ld b,d
      ld d,0
      ld hl,VGMYM2413Registers
      add hl,de
      ld (hl),b

      ; If a YM2413 key down is seen, we want to enable FM.
      ; We only do it here because some VGMs may have FM initialisation, but enabling FM would mute PSG on a Mark III.
      and $f0
      cp $20
      jp nz,+
      ld a,b
      or %00010000
      jr z,+
      ld a,(FMChipEnabled)
      or a
      jr nz,+
      call EnableFM
+:  pop bc
    pop hl
    jp GetData


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
    ; fast: multiply frame length by 8
    ; so we eat up 8x wait length
    ex de,hl
    add hl,hl
    add hl,hl
    add hl,hl
    ex de,hl

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

    ; Main screen turn on
    ld a,(VDPRegister81Value)
    or %01000000
    ld (VDPRegister81Value),a
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
.define VisTextLocation TilemapAddress(0,16)

.struct VisData
  InitFunction    dw
  ProcessFunction dw
  DrawFunction    dw
  Unused          dw
.endst

VisRoutines:
.dstruct instanceof VisData nolabels data InitPianoVis,     ProcessPianoVis,      DrawPianoVis
.dstruct instanceof VisData nolabels data InitFrequencyVis, ProcessFrequencyVis,  DrawVisBufferAsBars
.dstruct instanceof VisData nolabels data InitVolumeVis,    ProcessVolumeVis,     DrawVisBufferAsBars
.dstruct instanceof VisData nolabels data InitSnowVis,      ProcessSnowVis,       DrawSnowVis
.dstruct instanceof VisData nolabels data InitLogoVis,      ProcessLogoVis,       DrawLogoVis
.dstruct instanceof VisData nolabels data InitNoVis,        NoRoutine,            NoRoutine

.define NumVisRoutines _sizeof_VisRoutines/_sizeof_VisData

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
NoRoutine:
  ret

InitialiseVis:    ; Per-routine initialisation (runs in VBlank)
    call NoSprites

    ld a,(VisNumber)
    ; Check range
    cp NumVisRoutines
    jr c,+
    xor a ; zero if out of range
+:  add a,a ; a *= 8
    add a,a
    add a,a
    ld d,0
    ld e,a
    ld ix,VisRoutines
    add ix,de
    
    ld h,(ix+VisData.InitFunction+1)
    ld l,(ix+VisData.InitFunction+0)
    push ix
      call callHL
    pop ix

    ld h,(ix+VisData.DrawFunction+1)
    ld l,(ix+VisData.DrawFunction+0)
    ld (VisDisplayRoutine),hl

    ld h,(ix+VisData.ProcessFunction+1)
    ld l,(ix+VisData.ProcessFunction+0)
    ld (VisRoutine),hl

    xor a
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
.ends

.section "VIs buffer helpers" free

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
    add a,TileIndex_Scale ; tile offset
    out ($BE),a
    push hl ; delay
    pop hl
    ld a,>TileIndex_Scale
    out ($BE),a
    inc hl
    djnz -
    dec c
    jr nz,--
    ret
    
BlankVisArea:
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
  ret
  
NormalTilemap:
  ld hl,$8200 | %11110001 | (TilemapBaseAddress >> 10)
  call SetVDPRegister
  ; No scrolling
  ld hl,$8800
  call SetVDPRegister
  ld hl,$8900
  call SetVDPRegister
  ret
.ends

.section "Frequency bars vis" free
FrequencyVisString:
; Mid values for | to left of each number:
;    |110|127|149|179|226|304|466|999
;     |115|132|155|189|241|333|537|1398
;      |118|137|163|200|259|368|636|2330
;       |123|143|171|212|280|411|777|6991
.asc " 100  Freq   200  /Hz  500 1k 7k",$ff

InitFrequencyVis:
  call UpdatePalette
  call NormalTilemap
  
  ld hl,TileVRAMAddressFromIndex(TileIndex_Scale) ; Load vis tiles
  ld ix,ScaleData
  call PSG_decompress
  
  call NoSprites
  call ClearBuffer
  call DrawVisBufferAsBars
  ld hl,FrequencyVisString
  ld iy,VisTextLocation
  call WriteASCII
  ret

ProcessFrequencyVis:
    call ClearBuffer

    ld ix,VGMPSGTones
    ld iy,VGMPSGVolumes

    ld b,3  ; how many channels to do
-:  ld a,(iy+0)
    cpl
    and $0f
    jr z,+
    ld c,a          ; c  contains the volume 0-f (inverted to be normal sense)

    ; TODO: periodic noise

    ld a,(ix+0)
    ld d,(ix+1)     ; da contains the frequency value 0000-03ff
    ; 000000ddeeeeeeee
    ;       ^^^^^ We want these bits; we shift them into d
    .repeat 3
    rla
    rlc d
    .endr
    ; I want 31-(this value)
    ld a,31
    sub d
    ld e,a

    ; Add volume to value for channel
    ld hl,VisBuffer
    ld d,0
    add hl,de
    ld a,c
    add a,(hl)
    ld (hl),a
+:
    inc ix          ; Move to next values
    inc ix
    inc iy

    djnz -

    ; Noise: bump every value by up to 3
    ld a,(VGMPSGVolumes+3)
    cpl
    and $f
    jr z,+
    ld c,a
    srl c
    srl c
    ld b,32
    ld hl,VisBuffer
_AddNoise:
    ld a,(hl)
    add a,c
    ld (hl),a
    inc hl
    djnz _AddNoise
+:  
    ; Now try FM
    SetDebugColour(3,0,3)
    ld ix,VGMYM2413Registers
    ; 6 or 9 channels?
    ld b,9
    ld a,(ix+$e)
    and %00100000
    jr z,+
    ld b,9
+:
--: ;SetDebugColourB()
    ; Check key bit
    ld a,(ix+$20)
    and %00010000
    jr z,_skipChannel
    ; Get volume
    ld a,(ix+$30)
    cpl
    and $f
    jr z,_skipChannel
    ; Non-zero volume, but what's the frequency?
    ; Our frequency buckets are for the PSG, so we have 32 buckets based on the high 5 bits of the half-wavelength value.
    ; PSG freq = clock / 32 / x
    ; FM freq = f-num * clock / 72 / 2^(19 - block)
    ; So some algebra:
    ; f-num * clock / 72 / 2^(19 - block) = clock / 32 / x
    ; f-num / 72 / 2^(19 - block) = 1 / 32 / x
    ; 32 / 72 / 2 * x = 2^(19 - block) / f-num
    ; x = 2^(19 - block) / f-num * 4.5
    ; We can't compute that... so we do a lookup.
    ; Now get the frequency
    ld l,(ix+$10) ; F-num low 8 bits
    ld a,(ix+$20)
    ld c,a
    and 1
    ld h,a ; high bit -> hl is F-num, 0..511
    ; 0 frequency is not a note
    ld a,h
    or l
    jr z,_skipChannel
    ld a,c
    srl a
    and %111
    jr z,+
    push bc
      ld b,a ; Block, 0..7
-:    add hl,hl
      djnz -
    pop bc
+:
    ; now hl = frequency number in 16 bits
    ; Now we want to find which note hl corresponds to.
    ; We add the (negative) values from _FMNoteThresholds sequentially. When it underflows, the slot is n-1.
    ld a,31
    ld iy, _FMNoteThresholds
    ex de,hl ; test value in de now
-:  ld l,(iy+0)
    ld h,(iy+1)
    add hl,de
    jr c, +
    dec a
    inc iy
    inc iy
    jr -
+:  or a
    jp m,_skipChannel ; too low
    ; We want to put it in slot a. Check for >31
    cp 32
    jr nc,_skipChannel ; too high?
    ; Look up the slot
    ld hl,VisBuffer
    ld d,$00
    ld e,a
    add hl,de
    ; Get volume again
    ld a,(ix+$30)
    cpl
    and $f
    ; Add it on
    add a,(hl)
    ld (hl),a
    
_skipChannel:
    inc ix
    djnz --
    ret
    
_FMNoteThresholds:
;.dw -1152, -37, -39, -43, -45, -49, -52, -57, -62, -66, -73, -80, -88, -97, -108, -120, -136, -153, -176, -202, -237, -279, -335, -410, -512, -658, -878, -1228, -1844, -3072, -6144, -32767
.dw 65536-36864, -18432, -12288, -9216, -7372, -6144, -5266, -4608, -4096, -3686, -3351, -3072, -2835, -2633, -2457, -2304, -2168, -2048, -1940, -1843, -1755, -1675, -1602, -1536, -1474, -1417, -1365, -1316, -1271, -1228, -1189, -1152, -1
.ends

.section "Volume bars vis" free
VolumeVisString:
.asc " Tone 1  Tone 2  Tone 3  Noise  ",$ff
InitVolumeVis:
  call UpdatePalette
  call NormalTilemap

  ld hl,TileVRAMAddressFromIndex(TileIndex_Scale) ; Load vis tiles
  ld ix,ScaleData
  call PSG_decompress

  call NoSprites
  call ClearBuffer
  call DrawVisBufferAsBars
  ld hl,VolumeVisString
  ld iy,VisTextLocation
  call WriteASCII
  ret

ProcessVolumeVis:
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
.ends

.section "Piano vis" free
PianoVisString:
.asc "       Dave's Piano-matic       ",$ff
InitPianoVis:
    call UpdatePalette
    call NormalTilemap

    ld hl,TileVRAMAddressFromIndex(TileIndex_Piano)
    ld ix,PianoTiles
    call PSG_decompress

    ld hl,TileVRAMAddressFromIndex(TileIndex_Sprite_BigHand)
    ld ix,Hands
    call PSG_decompress

    ld hl,PianoVisString
    ld iy,VisTextLocation
    call WriteASCII
    
    ; Set sprites to 8x16 mode    
    ld a,(VDPRegister81Value)
    or %00000010
    ld (VDPRegister81Value),a
    out ($bf),a
    ld a,$81
    out ($bf),a

    call ClearBuffer
    ; Draw tiles
    ld hl,VisLocation
    call VRAMToHL
    ld hl,PianoTileNumbers
    ; We may run into the active display so output it slower than otir
    ld c,$be
    ld b,64*2
-:  outi
    jr nz, -
    ; draw 2 blank lines
    xor a
    ld b,32*2*2
-:  out ($be),a
    djnz -
    
    ret

PianoVisMinValsPSG:
; These correspond to the octave of values from 932 to 1760, which themselves are the thresholds between "standard" (12TET, A440) notes as PSG half-wavelengths
; (at 3579545Hz). These values are then the deltas to subtract in turn to determine if a note is "this one", working from high to low. It's complicated!
; Note  MIDI number  Frequency /Hz  Threshold from previous  PSG value of threshold
;   C           36           65.40                    63.57                    1760
;   C#          37           69.29                    67.35                    1661
;   D           38           73.41                    71.35                    1568
;   D#          39           77.78                    75.59                    1480
;   E           40           82.40                    80.09                    1397
;   F           41           87.30                    84.85                    1318
;   F#          42           92.49                    89.90                    1244
;   G           43           97.99                    95.24                    1174
;   G#          44          103.82                    100.9                    1108
;   A           45          110.00                    106.9                    1046
;   A#          46          116.54                    113.2                     988
;   B           47          123.47                    120.0                     932 
.dw -932, -56, -58, -62, -66, -70, -74, -79, -83, -88, -93

PianoVisMinValsFM:
; Same again, for FM
; We assume the FNum/Block have been scaled to be >255/whatever
; We have these minimum values for notes:
; Note  MIDI  Minimum F-num
; G       19            251 
; G#      20            266 
; A       21            282
; A#      22            299 
; B       23            316 
; C       24            335 
; C#      25            355 
; D       26            376 
; D#      27            399 
; E       28            422 
; F       29            447 
; F#      30            474 
; G       31            502
; G#      32            532
; So we make a similar table of "add each in turn and when you hit 0 the count is your note index". Then you have to sort the rest out in code.
.dw -266, -16, -17, -17, -19, -20, -21, -23, -23, -25, -27, -28, -30

.define MaxOctave 8 ; We have 9 octaves to show
.define MaxNote 7 ; But the highest one only has 8 notes in it

PianoXPositions:
;     C  C#   D  D#   E   F  F#   G  G#   A  A#   B
.db   8, 10, 12, 14, 16, 20, 22, 24, 26, 28, 30, 32
PianoYPositions:
.db 108,101,108,101,108,108,101,108,101,108,101,108

_Times28:
.db 0,28,28*2,28*3,28*4,28*5,28*6,28*7,28*8,28*9

ProcessPianoVis:
    ; Calculate hand x,y positions
    ; Store as xyxyxy in vis buffer
    ; Skip if no note
    ; Terminate with a 0
    ld ix,VGMPSGVolumes     ; 4 bytes, f = silent
    ld hl,VGMPSGTones       ; 4x2 bytes
    ld iy,VisBuffer         ; Where to store x,y. We have count at the start, xs from +16 and ys from +32
    ld b,4  ; Number of hands (PSG)
--: push bc
      ; Check volume
      ld a,(ix+0)
      ; Convert to 0..f
      cpl
      and $f
      ; Skip if silent
      jp z,_NoHand
      
      ; Get the tone
      ld e,(hl)
      inc hl
      ld d,(hl)
      dec hl
      ; 0 ->  silent
      ld a,e
      or d
      jp z,_NoHand
      
      push hl
        ; If noise (b=1) then it might be a 1/16 tone
        ld a,b
        dec a
        jp nz,+ ; most common
        ld a,(VGMPSGNoiseMode)
        and %100
        jp nz,_NoHandPop ; No hand for regular noise
        jp _GetFreqForPeriodic
+:
        ; We want to scale the PSG value into our target octave
        ; This means doubling it until it is >880. The max value is 1023.
        ;    C   C#    D   D#    E    F   F#    G   G#    A  A#   B   C (again)
        ; 1760 1661 1568 1480 1397 1318 1244 1174 1108 1046 988 932 880
        ld c,2 ; octave counter
_ProcessPSGFreq:     
-:      ld hl,-880-1
        add hl,de
        jr c,+
        sla e
        rl d
        inc c
        jr -
+:
        ld a,c
        or a
        jp m,_NoHandPop
        ; The highest note is octave 8
        cp MaxOctave+1
        jp nc,_NoHandPop
        
        ; now we have de in the range 881..1760 and c = octave number
        ex de,hl
        ld b,11
        push iy
          ld iy,PianoVisMinValsPSG
        -:ld e,(iy+0)     ; get (negative) value stored there
          ld d,(iy+1)
          add hl,de       ; Add value
          jr nc,+         ; Loop until I find a value where it no-carries which means the negative value was enough to go past 0
          dec b           ; else it's a lower note
          inc iy
          inc iy
          jr -
+:      pop iy
        ; Now we are octave c, note b
        ld a,c
        cp MaxOctave
        jr nz,+
        ld a,b
        cp MaxNote+1
        jr nc,_NoHandPop
+:      
        ; Look up the y just from the note
        ld d,0
        ld e,b
        ld hl,PianoYPositions
        add hl,de
        ld a,(hl)
        ld (iy+32),a
        ; And the x
        ld hl,PianoXPositions
        add hl,de
        ld a,(hl)
        ; The octave count adds 28px per octave
        ld hl,_Times28
        ld b,0
        add hl,bc
        ld b,(hl)
        add a,b
        ld (iy+16),a
        ; Next output slot
        inc iy
_NoHandPop:
      pop hl
_NoHand:    ; Don't show the hand if applicable
      inc hl
      inc hl
      inc ix
    pop bc
    dec b
    jp nz,-- ; too far for djnz (only just)

    SetDebugColour(3,0,3)
    ; Now do FM :)
    ; First check for rhythm mode
    ld b,6
    ld a,(VGMYM2413Registers+$e)
    and %00100000
    jr nz,+
    ld b,9
+:  ld ix,VGMYM2413Registers
--: push bc
      ; Check for key down
      ld a,(ix+$20)
      and %00010000
      jr z,_NoHandFM
      ; Check for volume >0
      ld a,(ix+$30)
      cpl
      and $f
      jr z,_NoHandFM
      ; Now get the frequency
      ld l,(ix+$10) ; F-num low 8 bits
      ld a,(ix+$20)
      ld c,a
      and 1
      ld h,a ; high bit -> hl is F-num, 0..511
      ; 0 frequency is not a note
      ld a,h
      or l
      jr z,_NoHandFM
      ld a,c
      srl a
      and %111
      ld b,a ; Block, 0..7
      ; F-num is a frequency base, block is an octave shift.
      ; We normalise it here
-:    ld a,h
      or a
      jr nz,+
      add hl,hl
      dec b
      jr -
+:    ; Now we want to find which note hl corresponds to.
      push iy
        ld c,0
        ld iy,PianoVisMinValsFM
-:      ld e,(iy+0)
        ld d,(iy+1)
        add hl,de       ; Add value
        jr nc,+         ; Loop until I find a value where it no-carries which means the negative value was enough to go past 0
        inc c           ; else it's a higher note
        inc iy
        inc iy
        jr -
+:    pop iy
      ; Now we have b = octave, c = note but for the wrong definition of each of those.
      ; For c, it can range from 0 -> G up to 13 -> G#
      ;    GGAABCCDDEFFGGAABCCDDEFFGGAABCCDDEFFGGAABCCDDEFFGGAABCCDDEFFGGAAB
      ;     # #  # #  # # #  # #  # # #  # #  # # #  # #  # # #  # #  # # #
      ;              0:|------------|        2:|------X-----|             ; What I have: block 2, index 8
      ; -1:|------------|        1:|------------|        3:|------------| ;
      ;       0:|----------|          2:|----------|          4:|----------|
      ;                   1:|----------|          3:|-X--------|          ; What I want: block 3, index 3
      ; So, we need to:
      ; 1. Subtract 5 from the note index 
      ; 2. Increment the block index
      ; 3. If note index is negative, add 12 and decrement block index
      ld a,c
      sub 5
      jr nc,+
      add 12
      dec b
+:    inc b

      ; put the note in de
      ld e,a
      ld d,0

      ; Now we sanity-check the note is in range
      ld a,b
      or a
      jp m,_NoHandFM
      ; The highest note is octave 8
      sub MaxOctave
      jr c,+
      jr nz,_NoHandFM
      ; In the highest octave, check for the max note
      cp MaxNote+1
      jr nc,_NoHandFM      
+:
      ; Now we are good!
      ; Look up the y just from the note
      ld hl,PianoYPositions
      add hl,de
      ld a,(hl)
      ld (iy+32),a
      ; And the x
      ld hl,PianoXPositions
      add hl,de
      ld a,(hl)
      ; The octave count adds 28px per octave
      ld hl,_Times28
      ld e,b
      add hl,de
      ld b,(hl)
      add a,b
      ld (iy+16),a
      ; Next output slot
      inc iy
    
_NoHandFM:
      inc ix ; Next channel
    pop bc
    djnz --

    ; Store the total count
    push iy
    pop hl
    ld de,$10000-VisBuffer
    add hl,de
    ld a,l
    ld (VisBuffer+0),a

    ret
    
_GetFreqForPeriodic:
        ; Set octave counter to -2 rather than 2
        ld c,-2
        ; Check the mode
        ld a,(VGMPSGNoiseMode)
        and %11
        cp %11
        jr nz,+
        ; Get tone value from from ch 3
        ld de,(VGMPSGTones+4)
        ; But is it 0?
        ld a,d
        or e
        jp z,_NoHandPop
        jp _ProcessPSGFreq

+:      ; It is periodic but not tone 3 linked!
        ld de,$0008 ; equivalent for %00
        ; Else double e a+1 times
        inc a
-:      sla e
        dec a
        jr nz,-
        jp _ProcessPSGFreq


DrawPianoVis:
    ; Load hand x,y positions from VisBuffer
    ; Set sprite positions accordingly

    .define NumHands 9

    ld hl,SpriteTableBaseAddress
    call VRAMToHL
    
    ; Get count
    ld a,(VisBuffer+0)
    or a
    jr z,_noHands
    ld c,a ; save for later

    ; Decide if we want big or small hands
    cp 5
    jr c,_bigHands

_smallHands:
    ld hl,VisBuffer+32
    ld b,c
-:  ld a,(hl)     ;  7 ; y-pos
    out ($be),a   ; 11 -> 38 total
    inc hl        ;  7
    djnz -        ; 13

    ; Terminate sprite table
    ld a,$d0
    out ($be),a

    ld hl,SpriteTableBaseAddress+128
    call VRAMToHL
    ld hl,VisBuffer+16

    ld b,c
-:  ld a,(hl)     ;  7
    add 2         ;  7 ; Small hand finger is 2px closer to the left
    out ($be),a   ; 11 -> 36 ; x-pos
    inc hl        ;  6
    nop           ;  4
    ld a,TileIndex_Sprite_SmallHand ;  7 ; Small hand tile index
    out ($be),a   ; 11 -> 28
    djnz -        ; 13
    ret
    
_bigHands:
    ld hl,VisBuffer+32
    ld b,c
-:  ld a,(hl)     ;  7 ; y-pos
    out ($be),a   ; 11 -> 31 total
    inc hl        ;  7
    nop           ;  4
    nop           ;  4
    out ($be),a   ; 11 -> 26 total
    djnz -        ; 13

    ; Terminate sprite table
    ld a,$d0
    out ($be),a

    ld hl,SpriteTableBaseAddress+128
    call VRAMToHL
    ld hl,VisBuffer+16

    ld b,c
-:  ld a,(hl)     ;  7
    out ($be),a   ; 11 -> 31 ; x-pos
    ld c,a        ;  4
    inc hl        ;  6
    ld a,TileIndex_Sprite_BigHand ;  7 ; Big hand tile index (LHS)
    out ($be),a   ; 11 -> 28
    ld a,c        ;  4
    add 8         ;  7
    nop           ;  4
    out ($be),a   ; 11 -> 26 ; x-pos
    nop           ;  4
    nop           ;  4
    ld a,TileIndex_Sprite_BigHand + 2 ;  7 ; Big hand tile index (RHS)
    out ($be),a   ; 11 -> 26
    djnz -        ; 13
    ret

_noHands:
    ; Just terminate the sprite table
    ld a,$d0
    out ($be),a
    ret
.ends

.section "No vis" free
InitNoVis:
  call TurnOffScreen
  ld hl,TileVRAMAddressFromIndex(TileIndex_3DPad)
  ld ix,Pad
  call PSG_decompress

  call UpdatePalette
  call NoSprites
  call BlankVisArea
  call NormalTilemap
  call TurnOnScreen
  
  ret
.ends

.section "Snow vis" free
.define NumSnowFlakes 64
.enum VisBuffer export
  ; I want to have various data for each snowflake, but I also want to make the sprite table upload easy.
  ; So I have a table of XNs and a table of Ys...
  ; And then the extra data, aligned relative to the preceding for constant offsets
  SnowflakeYs dsb 64
  SnowflakeXSpeeds dsb 64 ; <- iy points here
  SnowflakeXLos dsb 64
  SnowflakeXNs dsw 64
  SnowflakeAngleSpeeds dsw 64 ; <- ix points here
.ende
; So now I can traverse the byte-sized tables with XSpeed at Y+64,
; and XN at AngleSpeed-128
SnowText:
.asc "\n"
.asc "  Dancing snow...\n"
.asc "\n"
.asc "               ..with flashing",$ff

InitSnowVis:
    call UpdatePalette
    call NormalTilemap

    ld hl,TileVRAMAddressFromIndex(TileIndex_Sprite_Snow)
    ld ix,Snow
    call PSG_decompress

    ; Set sprites to 8x8 mode    
    ld a,(VDPRegister81Value)
    and %11111101
    ld (VDPRegister81Value),a
    out ($bf),a
    ld a,$81
    out ($bf),a
  
  call BlankVisArea

  ; Fill vis area with my text
  ld hl,SnowText
  ld iy,VisLocation
  call WriteASCII

  ; Fill buffer with initial spaced Ys
  xor a
  ld hl,SnowflakeYs
  ld b,NumSnowFlakes
-:ld (hl),a
  inc hl
  add a,3
  djnz -
  
  ; Then X speeds
  ld b,NumSnowFlakes
-:call GetRandomNumber
  ld (hl),a
  inc hl
  djnz -
  
  ; Then X low bytes, which we don't care about
  ld hl,SnowflakeXNs
  
  ; Then XNs
  ld b,NumSnowFlakes
-:call GetRandomNumber
  ld (hl),a
  inc hl
  ; Skip n for now
  inc hl
  djnz -
  
  ; Then AngleSpeeds are all random
  ld b,NumSnowFlakes
-:call GetRandomNumber
  ld (hl),a
  inc hl
  call GetRandomNumber
  and %00011111 ; Slow it down
  sub %00001111
  ld (hl),a
  inc hl
  djnz -
  ret

ProcessSnowVis:
  ld iy,SnowflakeXSpeeds
  ld ix,SnowflakeAngleSpeeds
  ld b,NumSnowFlakes
-:; For each snowflake...
  ; Get the current X
  ld l,(iy+64)
  ld h,(ix-128)
  ; Get the speed
  ld e,(iy+0)

  ; TODO make it FM sensitive?
  ; Make it invert X speed if the corresponding PSG channel is loud enough
  push bc
  push hl
    ld hl,VGMPSGVolumes
    ld a,b
    and 3 ; so it is in the range 0..3
    ld c,a
    ld b,0
    add hl,bc
    ld a,(hl)
    cpl
    and $0f
    cp $a
    jr c,+
    ; Invert the speed
    ld a,e
    neg
    ld (iy+0),a
+:  ; vol low
  pop hl
  pop bc
  
  ; Extend as signed
  ld d,0
  bit 7,e
  jr z,+
  dec d
+:; now de = speed to add
  add hl,de
  ; Save back
  ld (iy+64),l
  ld (ix-128),h
  
  ; And apply the spin
  ld a,(ix+0) ; Angle
  ld c,(ix+1) ; Speed
  add a,c
  ld (ix+0),a
  
  ; And make that the N
  ; Get high 2 bits
  rlca
  rlca
  and %11
  add a,TileIndex_Sprite_Snow   ; start of snowflake sprites
  ld (ix-127),a
  
  ; Just increment Y
  ld a,(iy-64)
  inc a
  cp 192
  jr nz,+
  ; We want to randomise the X, speed and spin again
  call GetRandomNumber
  ld (ix-128),a ; X
  call GetRandomNumber
  ld (iy+0),a ; speed
  call GetRandomNumber
  and %00011111 ; Slow it down
  sub %00001111
  ld (ix+1),a ; Rotation speed
  ld a,-8
+:ld (iy-64),a ; Y
  
  ; Move on
  inc iy
  inc ix
  inc ix
  djnz -

  ret
        
DrawSnowVis:
  ; Noise flashing
  ld a,(VGMPSGVolumes+3)
  cpl
  and $0f
  jr z,+
  ; noise on -> light cyan
  ld c,colour(2,3,3)
  jr ++
+:; noise off -> white
  ld c,colour(3,3,3)

++:; Write colour to CRAM
  ld a,22       ; palette index 22
  out ($bf),a
  ld a,$c0      ; CRAM write
  out ($bf),a
  ld a,c        ; data
  out ($be),a

  ; Emit to sprite table
  ld hl,SpriteTableBaseAddress
  call VRAMToHL
  ld hl,SnowflakeYs
  ld b,NumSnowFlakes*2 ; because outi + djnz will double-decrement b
  ld c,$be
-:outi ; 16
  djnz - ; 13 -> 29 cycles. otir is too fast!

  ld hl,SpriteTableBaseAddress+128
  call VRAMToHL
  ld b,0 ; for 128 bytes*2
  ld hl,SnowflakeXNs
-:outi
  djnz -
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

.section "Logo vis" free
.enum VisBuffer export
LogoX .dw
LogoXLo db
LogoXHi db
LogoY .dw
LogoYLo db
LogoYHi db
LogoSpeed dw
LogoXSign db
LogoYSign db
CurrentPalette db
NewColours db
PaletteOffset dw
HighestVolumeSeen db
LogoBrightness db ; 0-5
LogoBrightnessSmoothed db ; 0-5
SmoothingCounter db
.ende

; Increasing the X scroll moves the logo right.
; Increasing the Y scroll moves it up.
; Therefore we want to keep X in the range 8..198 and Y in the range -166..0.

.define MinX 8
.define MaxX 256-63
.define MinY 0
.define MaxY 192-32
.define BaseLogoSpeed $80

LogoPalettes:
.db colour(1,0,1), colour(2,0,2), colour(3,0,3)
.db colour(1,0,0), colour(2,0,0), colour(3,0,0)
.db colour(1,1,0), colour(2,2,0), colour(3,3,0)
.db colour(0,1,0), colour(0,2,0), colour(0,3,0)
.db colour(0,1,1), colour(0,2,2), colour(0,3,3)
.db colour(0,0,1), colour(0,0,2), colour(0,0,3)
.define NumPalettes _sizeof_LogoPalettes / 3

InitLogoVis:
  call TurnOffScreen
  call NoSprites
  
  ld hl,TileVRAMAddressFromIndex(TileIndex_Logo)
  ld ix,LogoTiles
  call PSG_decompress
  
  ; Draw into secondary tilemap at $2800
  ld hl,LogoTileNumbers
  ld de,$4000|$2800
  call LoadTilemapToVRAM

  ; Switch to it secondary tilemap
  ld hl,$8200 | %11110001 | ($2800 >> 10)
  call SetVDPRegister
  
  call TurnOnScreen
  
  ; Set border to black
  ld hl,$c010
  call SetVDPAddress
  xor a
  out ($be),a

  call ClearBuffer
  
  ; Random X, Y (in high byte)
-:call GetRandomNumber
  cp MaxX-MinX+1
  jr nc,-
  add MinX
  ld (LogoXHi),a
  
-:call GetRandomNumber
  cp MaxY+1
  jr nc,-
  ld (LogoYHi),a
  
  ld a,-1
  ld (CurrentPalette),a
  call RandomPalette
  
  ld hl,BaseLogoSpeed
  ld (LogoSpeed),hl
  xor a
  ld (LogoXSign),a
  ld (LogoYSign),a
  
  ld a,6 ; Bugs out of this is any lower
  ld (HighestVolumeSeen),a
  
  xor a
  ld (LogoBrightness),a
  ld (LogoBrightnessSmoothed),a
  
  ret
  
ProcessLogoVis:
  ; Add all active volumes together
  ld hl,VGMPSGVolumes
  ld e,0
  ld b,4
-:ld a,(hl)
  ; Invert scale
  cpl
  and $f
  ; Add
  add e
  ld e,a
  inc hl
  djnz -

  ; Now for FM...
  ld ix,VGMYM2413Registers
  ld a,(ix+$e)
  and %00100000
  jr z,_toneMode
_rhythmMode:
  ld a,(ix+$e)
  and %00011111
  ld b,a
  ; Each 1 bit shall contribute the corresponding volume from ix+$36..$38
  bit 4,b
  jr nc,+
  ld a,(ix+$36)
  cpl
  and $f
  add e
  ld e,a
+:bit 3,b
  jr nc,+
  ld a,(ix+$37)
  cpl
  and $f0
  srl a
  srl a
  srl a
  srl a
  add e
  ld e,a
+:bit 2,b
  jr nc,+
  ld a,(ix+$37)
  cpl
  and $f
  add e
  ld e,a
+:bit 1,b
  jr nc,+
  ld a,(ix+$38)
  cpl
  and $f0
  srl a
  srl a
  srl a
  srl a
  add e
  ld e,a
+:bit 0,b
  jr nc,+
  ld a,(ix+$38)
  cpl
  and $f
  add e
  ld e,a
+:; Now the instruments...
  ld b,6
  jr +
  
_toneMode:
  ld b,9
+:; Check key bit
-:ld a,(ix+$20)
  and %00010000
  jr z,+
  ; Key is pressed, get vol
  ld a,(ix+$30)
  cpl
  and $f
  ; Add to total
  add e
  ld e,a
+:inc ix
  djnz -

  ; Now we have (in theory) up to 15 per channel that is on, which is a max of
  ; 15*4 (PSG) + 15*11 (FM rhythm) = 225 (for mixed mode)
  ; 15*4 = 60 (for PSG only)
  ; 15*11 = 165 (for FM only)

.ifdef Debug
  ; Debug: show value
  ld hl, TilemapAddress(8,0)-$800
  call VRAMToHL
  ld a,e
  call WriteNumber
.endif
  
  ; Update max 
  ld a,(HighestVolumeSeen)
  cp e
  jr nc,+
  ld a,e
  ld (HighestVolumeSeen),a
+:; Now we have e compared to HighestVolumeSeen. We want to calculate the logo brightness on a scale of 0..5,
  ; so we want to compute (e * 5 + HighestVolumeSeen-1) / HighestVolumeSeen
  ; First compute e*5
  ld d,0
  ld h,d
  ld l,e
  add hl,hl
  add hl,hl
  add hl,de ; now hl = e*5
  ; Add HighestVolumeSeen-1
  ld a,(HighestVolumeSeen)
  ld e,a
  ld d,0
  add hl,de
  dec hl
  ; Then divide it by HighestVolumeSeen
  ld b,h
  ld c,l
  ld hl,0
  call Divide16
  ; Result should be in c
  ld a,c
  ld (LogoBrightness),a
  ; Now update LogoBrightnessSmoothed
  ; If LogoBrightness >= LogoBrightnessSmoothed, set it to that value 
  ; and set SmoothingCounter to the start value.
  ; Otherwise, decrement SmoothingCounter and if it reaches 0, decrement LogoBrightnessSmoothed.
  ld hl,LogoBrightnessSmoothed
  cp (hl) ; carry -> smoothed value is >a so we should (maybe) decrement it
  jr nc,_noDecay
_decay:
  ld a,(SmoothingCounter)
  dec a
  ld (SmoothingCounter),a
  jr nz,_done
  dec (hl)
  jr _resetCounter
_noDecay:
  ; Else smoothed value is <= a, so we set it to a
  ld (hl),a
  ; And reset the counter
_resetCounter:
  ld a,10
  ld (SmoothingCounter),a
_done:
  
.ifdef Debug
  ; Debug: show value
  ld hl, TilemapAddress(8,1)-$800
  call VRAMToHL
  ld a,(LogoBrightness)
  call WriteNumber
  ld hl, TilemapAddress(10,0)-$800
  call VRAMToHL
  ld a,(HighestVolumeSeen)
  call WriteNumber
  ld hl, TilemapAddress(8,2)-$800
  call VRAMToHL
  ld a,(LogoBrightnessSmoothed)
  call WriteNumber
.endif

  ; Now do the bouncing at edges
  ld a,(LogoXHi)
  cp MaxX+1
  jr c,+
  ; Go left
  xor a
  ld (LogoXSign),a
  inc a
  ld (NewColours),a
  jr ++
+:cp MinX
  jr nc,++
  ; Go right
  ld a,1
  ld (LogoXSign),a
  ld (NewColours),a
++:
  ld hl,(LogoX)
  ld de,(LogoSpeed)
  ld a,(LogoXSign)
  or a
  jr z,+
  add hl,de
  jr ++
+:sbc hl,de
++:
  ld (LogoX),hl

  ; Y side
  ; We have our variable going between 0 and an upper limit LogoYHi.
  ; This means we want to go down when >LogoYHi but also go up when <0 - which is an overflow, so we instead check for halfway between LogoYHi and 256
  ld a,(LogoYHi)
  cp (256+MaxY)/2
  jr c,+
  ; <0, go up
  ld a,1
  ld (LogoYSign),a
  ld (NewColours),a
  jr ++
+:cp MaxY+1
  jr c,++
  ; Go down
  xor a
  ld (LogoYSign),a
  inc a
  ld (NewColours),a
++:
  ld hl,(LogoY)
  ld de,(LogoSpeed)
  ld a,(LogoYSign)
  or a
  jr z,+
  add hl,de
  jr ++
+:sbc hl,de
++:
  ld (LogoY),hl

  ; Pick a palette if needed
  ld a,(NewColours)
  or a
  call nz,RandomPalette
  ret  
  
RandomPalette:
  ld hl,CurrentPalette
-:call GetRandomNumber
  and 7
  cp NumPalettes
  jr nc,-
  cp (hl)
  jr z,-
  ld (hl),a
  ; Look up offset
  ld e,a
  add a,a
  add a,e
  ld e,a
  ld d,0
  ld hl,LogoPalettes
  add hl,de
  ld (PaletteOffset),hl
  xor a
  ld (NewColours),a
  ret

DrawLogoVis:
  ld hl,$c00a ; Logo colours
  call VRAMToHL
  ; We emit 5-LogoBrightnessSmoothed blacks...
  ld hl,LogoBrightnessSmoothed
  ld a,5
  sub (hl)
  jr z,+
  ld b,a
  xor a
-:out ($be),a
  djnz -
+:; Then LogoBrightnessSmoothed+1 of our palette, doubled up
  ld b,(hl)
  inc b
  ld hl,(PaletteOffset)
-:ld a,(hl)
  out ($be),a
  dec b
  jr z,+
  out ($be),a
  inc hl
  djnz -
  
+:; Done

  ; Update scroll registers
  ld a,(LogoXHi)
  ld l,a
  ld h,$88
  call SetVDPRegister
  ; We invert the scroll compared to the Y value. This is to try to make the maths easier above.
  ld a,(LogoYHi)
  neg
  sub 256-224
  ld l,a
  ld h,$89
  call SetVDPRegister
  ret
.ends

;==============================================================
; VDP initialisation data
;==============================================================
VDPRegisterInitData:
.db %00100100,$80
;    |||||||`- Disable synch
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during horizontal scrolling
;    `-------- Fix right 8 columns during vertical scrolling
.db %10100000,$81
;     ||||||`- Zoomed sprites -> 16x16 pixels
;     |||||`-- Doubled sprites -> 2 tiles per sprite, 8x16
;     ||||`--- Mode5 bit on PBC - must be 0
;     |||`---- 30 row/240 line mode
;     ||`----- 28 row/224 line mode
;     |`------ Enable VBlank interrupts
;     `------- Enable display
.db (TilemapBaseAddress>>10) |%11110001,$82
.db (SpriteTableBaseAddress>>7)|%10000001,$85
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
.incbin "art\hands.palette" skip 1

Palettes:
.db colour(0,1,2),colour(1,2,3),colour(2,3,3)
.db colour(0,1,0),colour(1,2,2),colour(2,3,2)  ; original (green)
.db colour(0,0,1),colour(1,1,2),colour(2,2,3)  ; blue
.db colour(1,0,0),colour(2,1,1),colour(3,2,2)  ; red
.db colour(0,2,3),colour(2,2,3),colour(2,3,3)  ; light blue
.db colour(0,0,0),colour(1,1,1),colour(2,2,2)  ; greyscale
.db colour(3,0,0),colour(3,2,2),colour(3,2,3)  ; bright red

.enum 0 export ; Tile indices
TileIndex_Font          dsb 256
TileIndex_BigNumbers    dsb 33
TileIndex_3DPad         dsb 110
.union
  TileIndex_Scale         dsb 9   ; 0-8
.nextu
  TileIndex_Piano         dsb 11
  TileIndex_Sprite_BigHand  dsb 4
  TileIndex_Sprite_SmallHand  dsb 2
.nextu
  TileIndex_Logo          dsb 33
.nextu
  TileIndex_Sprite_Snow  dsb 4
.endu
.ende

.macro TileMapFilter
.redefine _out \1+TileMapFilterOffset
.endm
.macro TileMapFilteredIncBin(filename, offset)
.redefine TileMapFilterOffset offset
.incbin filename filter TileMapFilter filtersize 2
.endm

TileData:
.incbin "fonts\ZXChicagoPod.tiles.withdupes.psgcompr"

BigNumbers:
.incbin "art\big-numbers.tiles.psgcompr"
BigNumbersTilemap:
  TileMapFilteredIncBin("art\big-numbers.tilemap.bin", TileIndex_BigNumbers)

Pad:
.incbin "art\3d-pad.tiles.psgcompr"

PadData:
  TileMapFilteredIncBin("art\3d-pad.tilemap.bin", TileIndex_3DPad)

ScaleData:
.incbin "art\scale.tiles.psgcompr"

TextData:
.asc "   SMS VGM player\n"
.asc "   v2.00 by Maxim\n"
.asc "\n"
.asc "  Time       Loop\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "    Total\n"
.asc "    Loop\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc "\n"
.asc " https://www.smspower.org/Music\n"
.asc "JP:__ Location:__/____\n"
.asc "50:__ Wait:____\n"
.asc "FM:__\n"
.db $ff

NoVGMText:
.asc "        ################\n"
.asc "        # No VGM file! #\n"
.asc "        ################\n"
.asc "\n"
.asc " No VGM file has been found by\n"
.asc " the player. You have to add a\n"
.asc " file to the player for it to\n"
.asc " work. You can do this by using\n"
.asc " this commandline:\n"
.asc "\n"
.asc " copy /b vgmplayer.stub+\n"
.asc "         vgmfile.vgm output.sms\n"
.asc "\n"
.asc "      (all on one line)\n"
.asc " where:\n"
.asc " - vgmplayer.stub is the 16KB\n"
.asc "   player file\n"
.asc " - vgmfile.vgm is an\n"
.asc "   UNCOMPRESSED VGM file\n"
.asc " - output.sms is the combined\n"
.asc "   file which will be created\n"
.asc "\n"
.asc "  http://www.smspower.org/Music\n"
.db $ff

PianoTiles:
.incbin "art\piano.tiles.psgcompr"

PianoTileNumbers:
  TileMapFilteredIncBin("art\piano.tilemap.bin", TileIndex_Piano)

Hands:
.incbin "art\hands.tiles.8x16.psgcompr"

Snow:
.incbin "art\snow.tiles.8x16.psgcompr"

LogoTiles:
.incbin "art\screensaver.tiles.psgcompr"

LogoTileNumbers:
.incbin "art\screensaver.tilemap.pscompr"


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
        ld a,(VDPRegister81Value)
        and %10111111
        ld (VDPRegister81Value),a
        out ($bf),a
        ld a,$81
        out ($bf),a
    pop af
    ret

TurnOnScreen:
    push af
        ld a,(VDPRegister81Value)
        or %01000000
        ld (VDPRegister81Value),a
        out ($bf),a
        ld a,$81
        out ($bf),a
    pop af
    ret
.ends

;==============================================================
; Sprite disabler
; Sets all sprites' Y-position to $d0, thereby stopping display
; of any more sprites than I've used
;==============================================================
.section "No sprites" FREE
NoSprites:
    push af
    push hl
        ld hl,SpriteTableBaseAddress
        call VRAMToHL
        ld a,$d0
        out ($be),a
    pop hl
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
; Pass name table address in iy, it will not be modified
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
-:    ld a,(hl)    ; Value to write
      inc hl
      cp $ff ; EOT
      jr z,_done
      cp $fe ; Newline
      jp z,_NewLine
      out ($BE),a    ; Output to VRAM address, which is auto-incremented after each write
      ld a,0
      push hl
      pop hl  ; delay
      out ($BE),a
      jp -
_NewLine:
      ; Go to the next line, ie. next multiple of 32=$20
      push hl
        push iy
        pop hl
        ld bc,64
        add hl,bc
        call VRAMToHL
        push hl
        pop iy
      pop hl
      jp -
_done:
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
;==============================================================
.section "Draw image" FREE
DrawImageArea:
--: call VRAMToIY     ; Move to the right place
    push bc
-:    ld a,(ix+0)
      inc ix
      out ($be),a
      ld a,(ix+0)
      inc ix
      out ($be),a
      djnz -
    pop bc
    
    ld de,64 ; Move name table address
    add iy,de

    dec c
    jp nz,--
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
SetVDPAddress:
SetVDPRegister:
  ld a,l
  out ($BF),a
  ld a,h
  out ($BF),a
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
        ld hl,TilemapBaseAddress
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
    jp c,+
    add a,ASC('A')-ASC('9')+1   ; if it's >9 then make it point at A-F
+:  add a,ASC('0')

    out ($BE),a ; Output to VRAM address, which is auto-incremented after each write
    push hl
    pop hl
    ld a,0
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

