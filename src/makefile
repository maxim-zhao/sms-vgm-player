WLAZ80 = wla-z80.exe
WLALINK = wlalink.exe
BMP2TILE = bmp2tile.exe

# These are targets that aren't files
.PHONY: all clean default

# Disable built-in rules
.SUFFIXES:

default: smsvgmplayer.stub smsvgmplayer-unicode.stub

# WLA DX will tell us what files we need - but we have to generate the font asm first
auto.makefile: smsvgmplayer.stub.asm fonts/font-ascii.asm
	$(WLAZ80) -M -t -o smsvgmplayer.stub.o smsvgmplayer.stub.asm > $@

-include auto.makefile

auto-unicode.makefile: smsvgmplayer.stub.asm fonts/font-unicode.asm
	$(WLAZ80) -D UNICODE -M -t -o smsvgmplayer-unicode.stub.o smsvgmplayer.stub.asm > $@

-include auto-unicode.makefile

art/%.tilemap.bin: art/%.png
	$(BMP2TILE) $< -savetilemap $@

art/screensaver.tilemap.zx0: art/screensaver.png
	$(BMP2TILE) $< -tileoffset 410 -savetilemap $@

art/3d-pad.tilemap.zx0: art/3d-pad.png
	$(BMP2TILE) $< -tileoffset 256 -savetilemap $@

%.tiles.psgcompr: %.png
	$(BMP2TILE) $< -savetiles $@

%.tiles.zx0: %.png
	$(BMP2TILE) $< -savetiles $@

%.tiles.withdupes.psgcompr: %.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

%.tiles.withdupes.zx0: %.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

%.tiles.8x16.psgcompr: %.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

%.tiles.8x16.zx0: %.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

%.palette: %.png
	$(BMP2TILE) $< -fullpalette -savepalette $@

fonts/%.bin.zx0: fonts/%.bin
	fonts/zx0.exe -f $< $@

fonts/%.bin: fonts/font-unicode.asm fonts/font-ascii.asm

fonts/font-unicode.asm: fonts/fontdump.py fonts/MxPlusAmstradPC_VWF.ttf fonts/misaki_gothic.ttf
	python fonts/fontdump.py \
	-size=8 -offset=0 fonts/MxPlusAmstradPC_VWF.ttf \
	-size=8 -offset=0 fonts/misaki_gothic.ttf \
	-outdir=fonts -filename=$@ -skip=ff

fonts/font-ascii.asm: fonts/fontdump.py fonts/MxPlusAmstradPC_VWF.ttf
	python fonts/fontdump.py \
	-size=8 -offset=0 fonts/MxPlusAmstradPC_VWF.ttf \
	-size=8 -offset=0 fonts/misaki_gothic.ttf \
	-outdir=fonts -filename=$@ -skip=22,23,33,4e,4f,50,51,52,53,54,55,56,57,58,59,5a,5b,5c,5d,5e,5f,60,61,62,63,64,65,66,67,68,69,6a,6b,6c,6d,6e,6f,70,71,72,73,74,75,76,77,78,79,7a,7b,7c,7d,7e,7f,80,81,82,83,84,85,86,87,88,89,8a,8b,8c,8d,8e,8f,90,91,92,93,94,95,96,97,98,99,9a,9b,9c,9d,9e,9f,ff
# We squeeze as much as we can in, including kana...

# Range     Size  Action  Content
# 0        1317   ✅     ASCIIish
# 2 (upper) <126  🗑     Combining chars - not filterable
# 3         562   🗑     Greek
# 4         690   🗑     Cyrillic
# 5         231   🗑     Hebrew
# 1d         35   🗑     Random wrong stuff?
# 1e         90   ✅     Extended roman, need this
# 20        424   ✅     Extra punctuation, super/subscript numbers
# 21        333   🗑     Arrows, fractions, currencies
# 22        387   🗑     Maths
# 23         66   🗑     "Technical"
# 24        131   🗑     Circled numbers and letters
# 25        671   🗑     Box drawing
# 26        193   🗑     "Miscellaneous symbols"
# 27         19   🗑     "Dingbats"
# 30       1387   ✅     Kana
# 32         92   ✅     Enclosed CJK
# 33        270   ✅     CJK compatibility
# 4e..9f  55578   ✅     CJK
# fb         27   🗑     Ligatures
# ff          ?   🗑     Fullwidth

# And then we build it
smsvgmplayer.stub.o: 
	$(WLAZ80) -o $@ $<

smsvgmplayer-unicode.stub.o: 
	$(WLAZ80) -D UNICODE -o $@ $<

smsvgmplayer.stub: smsvgmplayer.stub.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -S linkfile $@

smsvgmplayer-unicode.stub: smsvgmplayer-unicode.stub.o
	echo [objects] > linkfile-unicode
	echo $< >> linkfile-unicode
	$(WLALINK) -d -r -v -S linkfile-unicode $@

%.sms: % smsvgmplayer-unicode.stub
	copy /y /b smsvgmplayer-unicode.stub+$< $@
	copy /y smsvgmplayer-unicode.sym $@.sym

clean:
	del art\*.zx0
	del art\*.bin
	del art\*.palette
	del smsvgmplayer*.stub
	del smsvgmplayer*.stub.o
	del auto.makefile
	del fonts\chunk*
	del fonts\font*.asm
