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
%.tilemap.inc: %.bmp
	$(BMP2TILE) $< -savetilemap $@
  
art/3d-pad.lsbtilemap: art/3d-pad.bmp
	$(BMP2TILE) $< -tileoffset 146 -savetilemap $@

art/piano.tilemap.inc: art/piano.bmp
	$(BMP2TILE) $< -tileoffset 256 -savetilemap $@

%.tilemap.pscompr: %.bmp
	$(BMP2TILE) $< -savetilemap $@

%.tiles.pscompr: %.bmp
	$(BMP2TILE) $< -savetiles $@

%.palette: %.bmp
	$(BMP2TILE) $< -fullpalette -savepalette $@

# And then we build it
%.o: 
	$(WLAZ80) -o $@ $<

smsvgmplayer.stub: smsvgmplayer.stub.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -S linkfile $@
  
test.sms: smsvgmplayer.stub vgm/tune2.vgm
	copy /y /b smsvgmplayer.stub+vgm\tune2.vgm test.sms
	copy /y smsvgmplayer.sym test.sym

clean:
	del art\*.pscompr
	del art\*.inc
	del art\*.palette
	del smsvgmplayer.stub
	del smsvgmplayer.stub.o
	del auto.makefile