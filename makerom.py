import sys
import os
import gzip
import glob
import pathlib
import fnmatch

def main():
    if not os.path.isfile("smsvgmplayer.stub"):
        print("Please put smsvgmplayer.stub in the same directory as this script")
        return 1

    if len(sys.argv) < 2:
        # Do all VGMs and playlists in the dir
        vgms = glob.glob("*.vg?")
        if len(vgms) > 0:
            make_rom_from_vgms("vgms.sms", sorted(vgms))
        for f in glob.glob("*.m3u"):
            make_rom_from_playlist(f)
    else:
        args = sys.argv[1:]
        vgms = [a for a in args if fnmatch.fnmatch(a, "*.vg?")]
        if len(vgms) > 0:
            make_rom_from_vgms("vgms.sms", vgms)
        playlists = [a for a in args if fnmatch.fnmatch(a, "*.m3u")]
        if len(playlists) > 0:
            for f in playlists:
                make_rom_from_playlist(f)

def make_rom_from_playlist(filename):
    # Read all contents
    with open(filename, "r", encoding="utf-8") as f:
        lines = [line.rstrip("\n") for line in f]
    # Convert lines to full paths
    m3udir = pathlib.Path(filename).parent
    paths = [str((m3udir / x).resolve()) for x in lines]
    make_rom_from_vgms(filename + ".sms", paths)

def make_rom_from_vgms(output_filename, filenames):
    with open(output_filename, "wb") as output:
        # Copy the stub
        with open("smsvgmplayer.stub", "rb") as stub:
            output.write(stub.read())
        # Copy the VGM
        isGzip = False
        for filename in filenames:
            with open(filename, "rb") as vgm:
                # Check for gzip
                if vgm.read(2) == b'\x1f\x8b':
                    isGzip = True
                else:
                    vgm.seek(0)
                    output.write(vgm.read())
            if isGzip:
                with gzip.open(filename, "rb") as vgm:
                    output.write(vgm.read())
        # Finally, pad to a multiple of 64KB
        padding = 64 * 1024 - output.tell() % (64 * 1024)
        output.write(bytearray(padding))
        
        print("Created " + output_filename)

if __name__ == "__main__":
    main()
