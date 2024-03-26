import sys
import os
import re
import yaml
from glob import glob
from pathlib import Path
import gzip

def main():
  # First find all the .vgm files
  files = [y for x in os.walk(".") for y in glob(os.path.join(x[0], '*.vg?'))]
  
  charUsage = {}
  
  for file in files:
    try:
      with gzip.open(file) as f:
        data = f.read()
        
      # Parsing time
      gd3offset = int.from_bytes(data[0x14:0x18], byteorder="little") + 0x14
      gd3tag = data[gd3offset:gd3offset+4].decode("utf-8")
      if gd3tag != "Gd3 ":
        raise Exception(f"Invalid GD3 tag")
        
      # Read tags as UTF-16
      offset = gd3offset + 12
      length = int.from_bytes(data[gd3offset+8:gd3offset+12], byteorder="little")
      s = "".join(data[offset:offset+length].decode("utf-16").split('\0')[0:8])
      # title x2
      # game x2
      # system x2
      # author x2
      #print(f"{file}: {s}")
      for c in s:
        if c in charUsage:
          charUsage[c] += 1
        else:
          charUsage[c] = 1

    except Exception as e:
      print(f"Error processing {file}: {e}")
      
  # Boost the chars we require
  for c in "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz+:":
    charUsage[c] = 1e9
    
  # And suppress those we want to remap
  remapping = { 'á': 'a', 'é': 'e', 'ー': '-', '一': '-', '·': '・', '²':'2', '³': '3' }
  for c in remapping.keys():
    charUsage[c] = 0
  
  # Also suppress fullwidth, we handle those in code
  for i in range(0xff01, 0xffff):
    charUsage[chr(i)] = 0
    
  charUsage = dict(sorted(charUsage.items(), key=lambda item: item[1], reverse=True))
  #print(charUsage)
  #for k,v in charUsage.items():
  #  print(f"{k} = {ord(k):04x}: {v}")
    
  # Now try to generate some source. We want this sort of thing:
  # .row -$0000, -1
  # .row -$0020, $00 ; " !""
  # .row -$0023, -1
  # .row -$0026, $03 ; "&'()"
  
  previousValue = -1
  tileIndex = 0x00
  chars = ""
    
  # Take the top 256, and then sort them
  #print(charUsage.keys())
  topChars = sorted([x for x in charUsage.keys()][0:254]) # Two values are for control codes
  print("".join(topChars))

  # We want to apply the remapping, e.g. of '·' to '・', 'é' to 'e', ...
  # We now make a dict of char to tile index, so we can look up in it.
  tileIndex = 0
  charToTile = {}
  for char in topChars:
    charToTile[char] = tileIndex
    tileIndex += 1
  # Then we add in the remappings, using the existing dict to find the tile index
  for key, value in remapping.items():
    charToTile[key] = charToTile[value]
  
  # Re-order to Unicode order
  charToTile = dict(sorted(charToTile.items(), key=lambda item: item[0]))
  
  previousValue = -1
  print(".row $0000, -1")
  for char, tileIndex in charToTile.items():
    if ord(char) == previousValue + 1:
      # Add to previous line
      print(char, end="")
    else:
      # We saw a jump
      # End previous row
      if previousValue > -1:
        print("\"")
        # Add a "null" row
        print(f".row ${0x10000 - (previousValue + 1):04x}, -1")
      # Start a data row
      print(f".row ${0x10000 - ord(char):04x}, ${tileIndex:02x} ; \"{char}", end="")
    previousValue = ord(char)
  # End previous row
  print("\"")
    

sys.stdout.reconfigure(encoding='utf-8')

if __name__ == "__main__":
    main()
