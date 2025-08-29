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
auto.makefile: smsvgmplayer.stub.asm
	$(WLAZ80) -M -t -o smsvgmplayer.stub.o smsvgmplayer.stub.asm > $@

-include auto.makefile

# We want to build the resources automatically
art/%.tilemap.bin: art/%.png
	$(BMP2TILE) $< -savetilemap $@

art/screensaver.tilemap.zx0: art/screensaver.png
	$(BMP2TILE) $< -tileoffset 399 -savetilemap $@

art/3d-pad.tilemap.zx0: art/3d-pad.png
	$(BMP2TILE) $< -tileoffset 289 -savetilemap $@

#%.tilemap.pscompr: %.png
#	$(BMP2TILE) $< -savetilemap $@
#
#%.tiles.pscompr: %.png
#	$(BMP2TILE) $< -savetiles $@

%.tiles.psgcompr: %.png
	$(BMP2TILE) $< -savetiles $@

%.tiles.withdupes.pscompr: %.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

%.tiles.withdupes.psgcompr: %.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

%.tiles.8x16.pscompr: %.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

%.tiles.8x16.psgcompr: %.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

%.palette: %.png
	$(BMP2TILE) $< -fullpalette -savepalette $@

fonts/%.bin.lzsa2: fonts/%.bin
	fonts/lzsa.exe -r -f 2 $< $@

%.bin.zx0: %.bin
	fonts/zx0.exe -f $< $@

fonts/%.bin: fonts/fontdump.py fonts/ZX_Chicago.ttf fonts/misaki_gothic.ttf
	python fonts/fontdump.py -size=8 -offset=-1 fonts/ZX_Chicago.ttf -offset=0 fonts/misaki_gothic.ttf -outdir=fonts

fonts/font.asm: fonts/fontdump.py fonts/ZX_Chicago.ttf fonts/misaki_gothic.ttf
	python fonts/fontdump.py -size=8 -offset=-1 fonts/ZX_Chicago.ttf -offset=0 fonts/misaki_gothic.ttf -outdir=fonts

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
	del art\*.pscompr
	del art\*.psgcompr
	del art\*.bin
	del art\*.palette
	del smsvgmplayer.stub
	del smsvgmplayer.stub.o
	del auto.makefile
	del fonts\*.bin*
	del fonts\font.asm
