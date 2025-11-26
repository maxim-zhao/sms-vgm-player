SMS VGM Player
==============

This is a "stub" which is able to play VGM files on the Sega Master System. These are logged music from video games.

It is only able to play back music composed for the sound chips it has: the Sega PSG and the YM2413 (if present). It also requires you to decompress the VGM file (if it is compressed; most are) and append it to the ROM.

![Screenshot](screenshots/screenshot1.png)

If you have trouble with playback, try:

1. For compatibility with some flash cartridges, the ROM must be padded to a multiple of 64KB.
2. For compatibility with some emulators, the ROM must be be padded to a minimum of 64KB.

The included `makerom.py` Python script will do all this for you. You can invoke it by:

1. Make sure you have Python on your computer
2. Put `makerom.py`, `smsvgmplayer.stub` and some VGM files in the same directory
3. Double-click `makerom.py`

This will 
1. Make a ROM with all VGM files it finds, called `vgms.sms`
2. If there are any M3U playlists, it will make one file for each playlist

Alternatively, if you are comfortable with a commandline, you can run it as so:

```
python make_rom.py <path to M3U file>
```

```
python make_rom.py <path to VGM file> <path to VGM file> <...>
```

If you need extended Unicode support for GD3 tag characters, move `smsvgmplayer.stub` away and rename `smsvgmplayer-unicode.stub` to `smsvgmplayer.stub`.


Character support
-----------------

`smsvgmplayer.stub` includes support for the Latin alphabet (with many, but not all, accented characters), as well as Hiragana and Katakana.

`smsvgmplayer-unicode.stub` includes support for over 10,000 characters including the most common Kanji, Korean, and many other scripts and symbols. Rename it to `smsvgmplayer.stub` to use the Python script.

The font data comes from three sources:
1. A [variable-width modified version](https://www.pentacom.jp/pentacom/bitfontmaker2/gallery/?id=19005) of [Amstrad PC](https://int10h.org/oldschool-pc-fonts/fontlist/font?amstrad_pc) for Latin
2. [QuanPixel](https://diaowinner.itch.io/galmuri-extended) for CJK
3. [Misaki Gothic](https://littlelimit.net/misaki.htm) adds a few more

There is no emoji support.


History
-------

- Version 2.2
  - Added support for multiple VGM files - assuming they have a valid EOF offset
    - Non-looping files will automatically advance, looping ones will still loop forever
  - Improvements for piano-matic visualizer:
    - Implemented flickering when there are more than 8 hands (rather than have the hands partially disappear)
    - Do not draw two hands in the same place, to reduce the chance of switching to small hands/flickering
  - The time now flashes when playback is paused
  - Playback starts automatically on boot 

- Version 2.1
  - Extended font support for accented characters, Kanji, Korean, and more... 18088 characters supported
  - GD3 tag is now drawn in a variable-width font

- Version 2.00
  - Functioning FM support, including for mixed PSG/FM music and on real systems
  - Works on a real system without graphical corruption
  - Enhanced frequency visualisation to support FM data
  - Enhanced snow visualisation to have better animation and more snowflakes
  - Added screensaver visualisation
  - Optimised code to remove lag sometimes seen on complicated VGMs
  - Reduced to 16KB
  - Added (incomplete) support for Japanese GD3 tags, about 175 characters supported

- Version 0.45
  - History has forgotten so long ago :)