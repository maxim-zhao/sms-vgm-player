import sys
import os
import gzip
import glob

def main():
    if not os.path.isfile("smsvgmplayer.stub"):
        print("Please put smsvgmplayer.stub in the same directory as this script")
        return 1

    if len(sys.argv) < 2:
        # Do all VGMs in the dir
        for f in glob.glob("*.vg?"):
            make_rom(f)
    else:
        for f in sys.argv[1:]:
            make_rom(f)

def make_rom(filename):
    with open(filename+".sms", "wb") as output:
        # Copy the stub
        with open("smsvgmplayer.stub", "rb") as stub:
            output.write(stub.read())
        # Copy the VGM
        isGzip = False
        with open(filename, "rb") as vgm:
            # Check for gzip
            if vgm.read(2) == b'\x1f\x8b':
                isGzip = true
            else:
                vgm.seek(0)
                output.write(vgm.read())
        if isGzip:
            with gzip.open(filename, "rb") as vgm:
                output.write(vgm.read())
        # Finally, pad to a multiple of 64KB
        padding = 64 * 1024 - output.tell() % (64 * 1024)
        output.write(bytearray(padding))
        
        print("Created " + filename + ".sms")

if __name__ == "__main__":
    main()
    