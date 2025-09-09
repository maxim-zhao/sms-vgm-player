WLAZ80 = wla-z80.exe
WLALINK = wlalink.exe
BMP2TILE = bmp2tile.exe

# These are targets that aren't files
.PHONY: all clean default

# Disable built-in rules
.SUFFIXES:

default: smsvgmplayer.stub

# WLA DX will tell us what files we need
# Problem: it can't tell us about files needed by generated asm...
auto.makefile: smsvgmplayer.stub.asm fonts/font.asm
	$(WLAZ80) -M -t -o smsvgmplayer.stub.o smsvgmplayer.stub.asm > $@

-include auto.makefile

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

fonts/%.bin: fonts/font.asm

fonts/font.asm: fonts/fontdump.py fonts/MxPlusAmstradPC_VWF.ttf fonts/misaki_gothic.ttf
	python fonts/fontdump.py \
	-size=8 -offset=0 fonts/MxPlusAmstradPC_VWF.ttf \
	-size=8 -offset=0 fonts/misaki_gothic.ttf \
	-outdir=fonts -skip=ff,26,25,24,22,21,20,4,3

# And then we build it
%.o: 
	$(WLAZ80) -o $@ $<

smsvgmplayer.stub: smsvgmplayer.stub.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -S linkfile $@

%.sms: % smsvgmplayer.stub
	copy /y /b smsvgmplayer.stub+$< $@
	copy /y smsvgmplayer.sym $@.sym

clean:
	del art\*.zx0
	del art\*.bin
	del art\*.palette
	del smsvgmplayer.stub
	del smsvgmplayer.stub.o
	del auto.makefile
	del fonts\chunk*
	del fonts\font.asm
