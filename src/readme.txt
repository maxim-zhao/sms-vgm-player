VGM player 0.94
by Maxim

:: To use ::

You need to append a VGM file to the stub. You can do this by using this
commandline (DOS/Windows):

copy /b vgmplayer.stub+filename.vgm output.sms

If you're a Linuxist or whatever then you should know how to concatenate
files.

*** THE VGM FILE MUST BE DECOMPRESSED! ***
I'm not about to port ZLib to the SMS. So, do one of these:
1. Load the file in VGMTool and press the "Decompress VGZ" button
2. Rename the file to filename.vgm.gz. Open it in something like 7-Zip 
   and extract it.

Then open your .sms file in an emulator. Some low-quality or hacky
emulators might have problems if you don't also pad the file to a power
of 2 size (64KB, 128KB, etc). 

Or, write it to a devcart and play it on a real SMS. You will have 
trouble with Everdrive flash carts and clones if you don't pad to a 
multiple of 64KB.

:: Controls ::

There's a picture showing what to do when you use it. But still...

- Press Button 1 to play the file, or to pause it.
- Hold Button 2 when it's playing to play at 4x speed.
- Press Down when playing or paused to stop.
- Press Up to cycle through the visualisations.
- Press Right to change the colour scheme

The player will automatically detect the speed of the system it's
playing on*. If you think it's wrong, press the Pause button to switch
between 50Hz and 60Hz modes.

* If you use it on an emulator with independent TV type and speed
  settings, it will be wrong if you use it set to PAL/60Hz or NTSC/50Hz.

:: Cool features ::

- Play back 60Hz VGMs on 50Hz hardware without slowdown. And play 50Hz
  files on 60Hz hardware without speedup.
- Visualisation of the sound
- A big picture of a joypad!
- An annoying bumper screen, which you can press a button to skip
- Piano visualisation teaches you to play! Well, if you have 9
  super-fast hands maybe... (depending on the music)

:: Limitations ::

- Only does SMS PSG and YM2413 - GG stereo writes crash a real SMS, and
  I didn't write code to detect a GG yet
- VGM size is limited to 3.95MB...
- Only frame-accurate - sampled audio will not work. Ever.
- Only plays one file, concatenating more won't do anything.
- The visualisations are not too intelligent, they just look at
  frequency and volume data and try to display it in a relatively simple
  way. So games doing strange stuff might visualise strangely. In
  particular, periodic noise isn't handled properly.
- Most of the visualisations don't do FM. It's impossible to get volume
  envelope data for FM, which makes it tricky.
- If you're playing a file at the opposite speed to which it was
  recorded, you're susceptible to aliasing between 50 and 60Hz. You
  probably won't notice anything.
- If you do stuff like hold 2 while pressing 1 then you can get strange
  results.

Emulator results:

- Any emulators I mention are ones I've tried and noticed something
  about, it's not an exhaustive test by any means.
- Speed checker tested on 6 emulators and 1 SMS, and works on all of
  them.
- Some emulators (Massage, BrSMS, Dega) will not work properly with
  files smaller than 48KB, because they do not handle paging for them.
  You get either the "No VGM file" screen or nothing at all. Pad your
  file to make it work.
  Update: I've changed some stuff to make them work a bit better, but
  you still get junk in the GD3 section.
- Some emulators (Massage, BrSMS) do not handle files which are not
  multiples of 16KB properly.
- Some emulators do not handle the bumper screen palette fades properly
  (Dega).
- Some emulators somehow manage to play some files so they don't match
  the actual length/loop times (Dega).
- Some emulators miss the first notes of FM VGMs (Meka).

Real system results:

- Works great! Seriously, though, it brought up some issues, like the
  need to space VRAM writes when they might extend into the display
  period (for example, Dave's Piano-matic after adding FM). But I fixed
  'em all.
- Well, the FM stuff might be a bit screwy. I didn't test that on a
  real system yet.

Miscellaneous:

- I used to detect PAL/NTSC, Japanese/Export and whether there's an FM
  chip. Now I only detect and use the first (to decide how fast to play
  back the file). If you don't have an FM chip, the console eats FM
  writes happily (many games write to it if it's there or not).
- Dave's Piano-matic is the terrible name I came up with for the piano
  vis because Dave suggested it. The idea is that you can learn to play
  the tunes by copying the keyboard. I suggest you use VGMTool to strip
  down to just one or two channels to avoid confusion, although it does
  look cool when the Japanese BIOS tune plays with 7 hands at once...
  Note that on a real system the sprite limit means that if there are
  more than 4 hands, you get flickering/disappearing/partial hands.
  Also note that if the VGM file you're using doesn't happen to use
  near-perfect pitch (or whatever the word is) then sometimes it'll play
  2 notes on the same key, or vibrato between 2 keys, etc...
- The FM piano vis ignores volume data, so if a game plays music
  silently, it shows. By doing this, I found a hidden layer on a WB3
  track :) Sadly it's painful to listen to...
- The snow vis works better with some files than others. Quiet files
  don't do anything!
- Pay attention to the SDSC tag notes!
- If you like, you can make new colour schemes. Save a screenshot, and
  change the three colours that change (background, and two lighter
  versions of the same colour) in an image editor, being sure to only
  use valid SMS colours (get one of the many palette test programs and
  take a screenshot to see). Then tell me what colours you used (if
  you're clever, in the form 031, 232, etc) and it'll be in the next
  version.
- I recommend you always use the same filename for the combined .sms
  file to avoid filling your saves folder with one save for each
  filename.

Development:

Assembler:        WLA DX
Debugger:         Meka
Tile generator:   BMP2Tile by me
Image editor:     Paint Shop Pro
Text file editor: ConTEXT
Disassembler:     z80dasm
Hex editor:       frhed
Z80 reference:    Official Z80 user manual
SMS reference:    Richard Talbot-Watkins' document, Charles' documents

Result:

  13231 bytes (40.38%) free of total 32768.


Maxim 2023/11/02
