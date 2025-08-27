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
                #if y[0] < 128:
                #    # We don't want ASCII (?)
                #    continue
                yield (y[0], Unicode[y[0]])


def convert_ttf(path, fontsize, w, h, voffset, imgwidth):
    base = os.path.basename(path)
    fontname = os.path.splitext(base)[0]
    if not os.path.exists(fontname):
        os.mkdir(fontname)
    chars = sorted(set(get_font_codepoints(path)))
    font = ImageFont.truetype(path, fontsize)
    font2 = ImageFont.truetype(path, fontsize*10)
    charsperrow = imgwidth // w
    imgheight = h * (len(chars) // charsperrow) + (h if len(chars) % charsperrow > 0 else 0)
    sheet = Image.new('RGB', (imgwidth, imgheight), (0, 0, 0))
    sheetdraw = ImageDraw.Draw(sheet)
    x = 0
    y = voffset
    for c in chars:
        if c[1][0] == '?':
            continue
        sheetdraw.text((x, y), chr(c[0]), (255, 255, 255), font=font)
        x += w
        if x == imgwidth:
            x = 0
            y += h

        (cw, ch) = font2.getbbox(chr(c[0]))[2:4]
        img = Image.new('RGB', (w*10, h*10), (255, 255, 255))
        draw = ImageDraw.Draw(img)
        draw.text(((w*10 - cw) * 0.5, (h*10 - ch) * 0.5), chr(c[0]), (0, 0, 0), font=font2)
        img.save('{}/{:d}.{}.png'.format(fontname, c[0], c[1]))
    sheet.save('{}.png'.format(fontname))


class CharacterInfo:
    def __init__(self, codepoint, font, drawing_offset, img_height):
        # Remember the details...
        self.codepoint = codepoint
        width = font.getbbox(chr(codepoint))[2]
        # Now draw the font to an image
        self.img = Image.new('1', (width, img_height), 0)
        imgdraw = ImageDraw.Draw(self.img)
        imgdraw.text((0, drawing_offset), chr(codepoint), fill=1, font=font)
        bbox = self.img.getbbox()
        if bbox:
            self.img = self.img.crop((bbox[0], 0, bbox[2], 8))


def convert(args):
    codepoints = {}
    # 1. Parse the args in order. Filename after args affecting it.
    size = 8
    offset = 0
    filename = ''
    outdir = '.'
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
            else:
                print(f"Unknown key {key}")
        else:
            # Must be a filename
            # 2. Load the font for drawing
            print(f"Loading {arg} at size {size}")
            font = ImageFont.truetype(arg, size)
            for (codepoint, name) in get_font_codepoints(arg):
                # 3. Add to dict if not already present
                if codepoint not in codepoints and codepoint > 32:
                    codepoints[codepoint] = CharacterInfo(codepoint, font, offset, 8)

    # Transfer to a sorted list
    chars = sorted(codepoints.values(), key=lambda x: x.codepoint)
    
    # 7-8. Save lookup table
    # Plan:
    # 1. Split to 256 code point chunks
    # 2. For each chunk, encode a table of 256 offsets (with 0 = no character) followed by the pixel data
    # 3. Compress these using ZX7 or similar
    # 4. At runtime, decode the needed chunk to RAM (for ASCII, this will only be needed once) and follow the trail.
    #    256 words + 256 8px wide chars = 2560 bytes, will be smaller sometimes
    
    # 1. Split to chunks
    buckets = {}
    for char in chars:
        bucket_key = (char.codepoint // 256)
        dict = buckets.setdefault(bucket_key, {})
        dict[char.codepoint % 256] = char
    
    # 2. Emit a lookup table for these. As they are quite sparse, we will emit (key, pointer) pairs
    with open(f'{outdir}/font.asm', 'w') as asm:
        asm.write('.section "Font chunks lookup" force\n')
        asm.write('Chunks:\n')
        asm.write('.table byte, byte, word ; high byte of char, bank, offset\n')
        for i in buckets:
            asm.write(f'.row ${i:02x}, :Chunk{i:04x}, Chunk{i:04x}\n')

            # Then encode an uncompressed file for each chunk
            with open(f'{outdir}/chunk{i:02x}.bin', 'wb') as bin:
                local_chars = buckets[i]
                # First the lookup
                offset = 512
                for n in range(256):
                    if n in local_chars:
                        bin.write(struct.pack('<H', offset))
                        offset += local_chars[n].img.width
                    else:
                        bin.write(struct.pack('<H', 0))
                # Then the data
                for char in local_chars.values():
                    for x in range(char.img.width):
                        byte = 0
                        for y in range(8):
                            pixel = char.img.getpixel((x, y))
                            bit = 0 if pixel == 0 else 1  # 0 = black, 255 = white
                            byte |= (bit << (7 - y))  # Top pixel is MSB
                        bin.write(struct.pack('B', byte))
        asm.write('.ends\n')

        # Then emit to the asm, expecting compression to happen in the meantime
        for i in buckets:
            asm.write(f'.section "Chunk{i:04x}" superfree\n')
            asm.write(f'Chunk{i:04x}: .incbin "{outdir}/chunk{i:02x}.bin.zx0"\n')
            asm.write('.ends\n')
    

if __name__ == '__main__':
    convert(sys.argv)
