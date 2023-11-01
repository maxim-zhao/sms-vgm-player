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

%.tilemap.pscompr: %.bmp
	$(BMP2TILE) $< -savetilemap $@

%.tiles.pscompr: %.bmp
	$(BMP2TILE) $< -noremovedupes -savetiles $@

%.pcmenc: %
	$(PCMENC) $< -p 4 -dt1 474 -dt2 11 -dt3 475 -rto 2 -cpuf 3546893 -smooth 10

# 8x16 mode uncompressed, no tile dedupe
%.8x16.bin: png/%.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

# Uncompressed, no tile dedupe
%.bin: png/%.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

# Uncompressed with tile dedupe
%.bin: png/%.unoptimised.png
	$(BMP2TILE) $< -removedupes -savetiles $@

# Compressed, no tile dedupe, 8x16
%.8x16.pscompr: png/%.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

# Compressed, no tile dedupe
%.pscompr: png/%.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

# Compressed with tile dedupe
%.pscompr: png/%.unoptimised.png
	$(BMP2TILE) $< -removedupes -savetiles $@

# Tilemap with tile dedupe
%.tilemap.bin: png/%.unoptimised.png
	$(BMP2TILE) $< -removedupes -savetilemap $@

# LSB tilemap with tile dedupe
%.lsbtilemap: png/%.unoptimised.png
	$(BMP2TILE) $< -removedupes -savetilemap $@

# Tilemap without tile dedupe
%.tilemap.bin: png/%.png
	$(BMP2TILE) $< -noremovedupes -savetilemap $@

# Palettes
%.palette: png/%.png
	$(BMP2TILE) $< -smspalette -fullpalette -savepalette $@

%.palette: png/%.unoptimised.png
	$(BMP2TILE) $< -smspalette -fullpalette -savepalette $@

# 1bpp art, no tile dedupe
%.1bpp: png/%.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

# And then we build it
%.o: 
	$(WLAZ80) -o $@ $<

smsvgmplayer.stub: smsvgmplayer.stub.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -S linkfile $@

clean:
	del art\*.pscompr
	del art\*.inc
	del smsvgmplayer.stub
	del smsvgmplayer.stub.o
	del auto.makefile