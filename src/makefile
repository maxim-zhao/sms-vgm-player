WLAZ80 = wla-z80.exe
WLALINK = wlalink.exe
BMP2TILE = bmp2tile.exe

# These are targets that aren't files
.PHONY: all clean default

# Disable built-in rules
.SUFFIXES:

default: smsvgmplayer.stub

# WLA DX will tell us what files we need
auto.makefile: smsvgmplayer.stub.asm
	$(WLAZ80) -M -t -o smsvgmplayer.stub.o smsvgmplayer.stub.asm > $@

-include auto.makefile

# We want to build the resources automatically
art/%.tilemap.bin: art/%.png
	$(BMP2TILE) $< -savetilemap $@

art/screensaver.tilemap.pscompr: art/screensaver.png
	$(BMP2TILE) $< -tileoffset 399 -savetilemap $@

%.tilemap.pscompr: %.png
	$(BMP2TILE) $< -savetilemap $@

%.tiles.pscompr: %.png
	$(BMP2TILE) $< -savetiles $@

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

# And then we build it
%.o: 
	$(WLAZ80) -o $@ $<

smsvgmplayer.stub: smsvgmplayer.stub.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -S linkfile $@

# To guarantee at least 64KB, we copy three stubs on the end(!)
# Else Emulicious won't emulate a mapper
%.sms: % smsvgmplayer.stub
	copy /y /b smsvgmplayer.stub+$<+smsvgmplayer.stub+smsvgmplayer.stub+smsvgmplayer.stub $@
	copy /y smsvgmplayer.sym $@.sym

clean:
	del art\*.pscompr
	del art\*.psgcompr
	del art\*.bin
	del art\*.palette
	del smsvgmplayer.stub
	del smsvgmplayer.stub.o
	del auto.makefile