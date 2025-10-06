# !/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (c) 2019 Tamflex
# Released under the MIT license
# https://opensource.org/licenses/mit-license.php

# reference:
# http://stackoverflow.com/questions/4458696/finding-out-what-characters-a-font-supports

import os
import re
import sys
import PIL
from PIL import Image
from PIL import ImageFont
from PIL import ImageDraw
from itertools import chain
import fontTools
from fontTools.ttLib import TTFont
from fontTools.unicode import Unicode
import struct

def get_font_codepoints(path):
    with TTFont(path, 0, allowVID=0, ignoreDecompileErrors=True, fontNumber=-1) as ttf:
        for x in ttf['cmap'].tables:
            if isinstance(x, fontTools.ttLib.tables._c_m_a_p.cmap_format_6):
                continue # Misaki has boxes in a table like this, but we want to exclude them
            for y in x.cmap.items():
                yield (y[0], Unicode[y[0]])


class CharacterInfo:
    def __init__(self, codepoint, font, drawing_offset, img_height):
        # Remember the details...
        self.codepoint = codepoint
        # We add 4px padding because the font may draw outside its reported box
        width = font.getbbox(chr(codepoint))[2] + 8
        # Now draw the font to an image
        self.img = Image.new('1', (width, img_height), 0)
        imgdraw = ImageDraw.Draw(self.img)
        imgdraw.text((4, drawing_offset), chr(codepoint), fill=1, font=font)
        bbox = self.img.getbbox()
        if bbox:
            self.img = self.img.crop((bbox[0], 0, bbox[2], 8))
        else:
            # Image is blank, so make it a space
            self.img = Image.new('1', (2, 8), 0)


def convert(args):
    codepoints = {}
    # 1. Parse the args in order. Filename after args affecting it.
    size = 8
    offset = 0
    filename = ''
    outdir = '.'
    skip = set()
    for arg in args[1:]:
        match = re.fullmatch("-([a-z]+)=(.+)", arg)
        if match:
            key = match.group(1)
            value = match.group(2)
            if key == "size":
                size = int(value)
            elif key == "offset":
                offset = int(value)
            elif key == 'outdir':
                outdir = value
            elif key == 'skip':
                for x in value.split(','):
                    if ".." in x:
                        parts = [int(y, 16) for y in x.split("..")]
                        for n in range(parts[0], parts[1]+1):
                            skip.add(n)
                    else:
                        skip.add(int(x, 16))
            elif key == 'filename':
                filename = value
            else:
                print(f"Unknown key {key}")
        else:
            # Must be a filename
            # 2. Load the font for drawing
            print(f"Loading {arg} at size {size}")
            font = ImageFont.truetype(arg, size)
            # Add a dummy space glyph
            if 32 not in codepoints:
                cp = CharacterInfo(32, font, offset, 8)
                # Rewrite it
                cp.img = Image.new('1', (2, 8), 0)
                codepoints[32] = cp
                
            for (codepoint, name) in get_font_codepoints(arg):
                # 3. Add to dict if not already present
                if codepoint not in codepoints and codepoint > 32:
                    codepoints[codepoint] = CharacterInfo(codepoint, font, offset, 8)

    # Transfer to a sorted list
    chars = sorted(codepoints.values(), key=lambda x: x.codepoint)
    
    # Remove the skipped ones
    chars = [x for x in chars if x.codepoint // 256 not in skip]
    
    width = sum([x.img.width for x in chars])
    maxwidth = max([x.img.width for x in chars])
    print(f"Total character count: {len(chars)}. Pixel width: {width}. Max width: {maxwidth}")
    
    # 7-8. Save lookup table
    # Plan:
    # 1. Split to 256 code point chunks
    # 2. For each chunk, encode a table of 256 offsets (with 0 = no character) followed by the pixel data
    # 3. Compress these using ZX7 or similar
    # 4. At runtime, decode the needed chunk to RAM (for ASCII, this will only be needed once) and follow the trail.
    #    256 words + 256 8px wide chars = 2560 bytes, will be smaller sometimes
    bucket_factor = 256
    #         ZX0   upkr    largest chunk
    #  256 -> 60230 56354   2000            -> easiest to handle in asm
    #  512 -> 59245 55031   3545
    # 1024 -> 58410 53980   5930            -> only about 2KB saved, I need more :(
    # 2048 -> 57480         11413           -> buffer too large

    # 1. Split to chunks
    buckets = {}
    for char in chars:
        bucket_key = (char.codepoint // bucket_factor)
        dict = buckets.setdefault(bucket_key, {})
        dict[char.codepoint % bucket_factor] = char
    
    # 2. Emit a lookup table for these. As they are quite sparse, we will emit (key, pointer) pairs
    with open(filename, 'w') as asm:
        asm.write('.section "Font chunks lookup" force\n')
        asm.write('Chunks:\n')
        asm.write('.table byte, byte, word ; high byte of char, bank, offset\n')
        for i in buckets:
            asm.write(f'.row ${i:02x}, :Chunk{i:04x}, Chunk{i:04x}\n')

            # Then encode an uncompressed file for each chunk
            with open(f'{outdir}/chunk{i}_{i:02x}.bin', 'wb') as bin:
                local_chars = buckets[i]
                # lookup of widthx256, followed by bits
                for n in range(bucket_factor):
                    if n in local_chars:
                        bin.write(struct.pack('B', local_chars[n].img.width))
                    else:
                        bin.write(struct.pack('B', 0))
                # Then the data
                for char in local_chars.values():
                    for x in range(char.img.width):
                        byte = 0
                        for y in range(8):
                            pixel = char.img.getpixel((x, y))
                            bit = 0 if pixel == 0 else 1  # 0 = black, 255 = white
                            byte |= (bit << (7 - y))  # Top pixel is MSB
                        bin.write(struct.pack('B', byte))

                # Also an image so I know what's in there
                sheet = Image.new('RGB', (128, bucket_factor//2), (0, 0, 0))
                for y in range(0, bucket_factor//2, 8):
                    for x in range(0, 128, 8):
                        index = y // 8 * 16 + x // 8
                        if index in local_chars:
                            sheet.paste(local_chars[index].img, (x, y))
                sheet.save(f'{outdir}/chunk{i}_{i:02x}.png')
            
        asm.write('.db $ff ; Terminator\n')
        asm.write('.ends\n')
        asm.write('.slot 2\n')

        # Then emit to the asm, expecting compression to happen in the meantime
        for i in buckets:
            asm.write(f'.section "Chunk{i:04x}" superfree\n')
            asm.write(f'Chunk{i:04x}: .incbin "{outdir}/chunk{i}_{i:02x}.bin.zx0"\n')
            asm.write('.ends\n')
    

if __name__ == '__main__':
    convert(sys.argv)
