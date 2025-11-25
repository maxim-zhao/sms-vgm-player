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
	$(BMP2TILE) $< -tileoffset 223 -savetilemap $@

art/3d-pad.tilemap.zx0: art/3d-pad.png
	$(BMP2TILE) $< -savetilemap $@

%.tiles.zx0: %.png
	$(BMP2TILE) $< -savetiles $@

%.tiles.withdupes.zx0: %.png
	$(BMP2TILE) $< -noremovedupes -savetiles $@

%.tiles.8x16.zx0: %.png
	$(BMP2TILE) $< -noremovedupes -8x16 -savetiles $@

%.palette: %.png
	$(BMP2TILE) $< -fullpalette -savepalette $@

fonts/%.bin.zx0: fonts/%.bin
	fonts/zx0.exe -f $< $@

fonts/%.bin: fonts/font-unicode.asm

fonts/font-unicode.asm: fonts/fontdump.py fonts/MxPlusAmstradPC_VWF.ttf fonts/misaki_gothic.ttf fonts/quan.ttf
	python fonts/fontdump.py \
	-size=8 -offset=0 fonts/MxPlusAmstradPC_VWF.ttf \
	-size=8 -offset=0 fonts/quan.ttf \
	-size=8 -offset=0 fonts/misaki_gothic.ttf \
	-outdir=fonts -filename=$@ -skip=ff

fonts/font-ascii.asm: fonts/fontdump.py fonts/MxPlusAmstradPC_VWF.ttf fonts/misaki_gothic.ttf fonts/quan.ttf
	python fonts/fontdump.py \
	-size=8 -offset=0 fonts/MxPlusAmstradPC_VWF.ttf \
	-size=8 -offset=0 fonts/quan.ttf \
	-size=8 -offset=0 fonts/misaki_gothic.ttf \
	-outdir=fonts -filename=$@ -skip=4..1c,22..24,28..fa,fc..fd,ff
# We squeeze as much as we can in, including kana...
# Range   Size  "ASCII"Characters
# 00       995  ✅     Basic Latin
# 01      1064  ✅     Latin extended
# 02       336  ✅     IPA extensions, Spacing Modifier Letters
# 03       486  ✅     Combining Diacritical Marks, Greek and Coptic
# 04       518  ❌     Cyrillic
# 05       184  ❌     Cyrillic, Armenian, Hebrew
# 06..1c     -  ❌     Arabic, Syraic, Thaana, Devanagari, Bengali, Gurmukhi, Gujarati, Oriya, Tamil, Telugu, Kannada, Malayalam, Sinhala, Thai, Tibetan, Myanmar, Georgian, Hangul Jamo, Ethiopic, Cherokee, Unified Canadian Aboriginal Syllabics, Ogham, Tagalog, Hanunoo, Buhid, Tagbanwa, Khmer, Mongolian, Limbu, Tai Le, Khmer Symbols (a few characters present)
# 1d       102  ✅     Phonetic Extensions
# 1e       405  ✅     Latin Extended Additional
# 1f        17  ✅     Greek Extended
# 20       430  ✅     Punctuation, symbols
# 21       371  ✅     Letterlike symbols, number forms, arrows
# 22       388  ❌     Maths
# 23        78  ❌     Misc technical
# 24       406  ❌     Enclosed alphanumerics
# 25       716  ✅     Box Drawing
# 26       427  ✅     Misc symbols - inc. ♪
# 27       248  ✅     Dingbats, maths, arrows
# 28         -  ❌     Braille, arrows
# 29        15  ❌     Maths
# 2A..2B     -  ❌     Maths, misc symbols
# 2c        36  ❌     ?
# 2d         -  ❌     ?
# 2E       122  ❌     CJK Radicals
# 2F        17  ❌     Kangxi Radicals
# 30      1187  ✅     CJK Symbols and Punctuation, Hiragana, Katakana
# 31       580  ❌     Bopomofo
# 32         -  ❌     Enclosed CJK Letters and Months
# 33         -  ❌     CJK Compatibility
# 34..4D   622  ❌     Unified Ideographs Extension A
# 4E..9F 72205  ❌     CJK Unified Ideographs
# a0..ab    37  ❌     Yi (?)
# AC..D7 22308  ❌     Hangul Syllables
# D8..fa     -  ❌     Surrogates, private use, CJK compat
# fb        21  ✅     Alphabetic Presentation Forms
# fc..fd     -  ❌     ?
# fe       267  ✅     CJK weird punctuation
# ff         -  ❌     Full-width


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
	del auto*.makefile
	del fonts\chunk*
	del fonts\font*.asm
