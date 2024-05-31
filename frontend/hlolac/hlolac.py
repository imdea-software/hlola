#!/usr/bin/python3
from pathlib import Path
import shutil
import sys
import subprocess
import os
import stat

outpath = "/tmp/toolc/"
shutil.rmtree(outpath,ignore_errors=True)
outpathlibs = outpath+"Lib/"
outpathinnerspecs = outpath+"INNERSPECSDIR/"
os.makedirs(outpathlibs)
os.makedirs(outpathinnerspecs)
outbin = "./a.out"
forcebuild = False

try:
    oindex = sys.argv.index("-o")
    outbin = sys.argv.pop(oindex+1)
    del sys.argv[oindex]
except ValueError:{} # Element not in the list

try:
    fbix = sys.argv.index("--force-build")
    forcebuild = True
    del sys.argv[fbix]
except ValueError:{} # Element not in the list

if len(sys.argv) > 1:
    hlolafiles = map(Path,sys.argv[1:])
else:
    hlolafiles = list(Path(".").rglob("*.[hH][lL][oO][lL][aA]"))

hlolabasedir = str(Path(os.path.dirname(os.path.realpath(__file__))).parent.parent) + "/src/"
preprocbin = str(Path(os.path.dirname(os.path.realpath(__file__))).parent.parent) + "/frontend/hlolac/preproc/preproc"

for file in hlolafiles:
    if not os.path.isfile(file):
        print("File does not exist: "+str(file))
        sys.exit()
    args = [preprocbin, outpath, file]
    cp = subprocess.run(args)
    if cp.returncode != 0:
        print("Error in file: "+str(file))
        sys.exit()

libfiles = list(Path(outpathlibs).rglob("*.hs"))
for file in libfiles:
    shutil.copy(str(file), hlolabasedir+'/Lib/')

innerspecfiles = list(Path(outpathinnerspecs).rglob("*.hs"))
for file in innerspecfiles:
    shutil.copy(str(file), hlolabasedir+'/INNERSPECSDIR/')

mainfile = outpath+"Main.hs"
if os.path.isfile(mainfile):
    shutil.copy(mainfile, hlolabasedir)

print("Running Stack")
sys.stdout.flush()
if forcebuild:
    args = ["stack", "clean"]
    subprocess.run(args, cwd=hlolabasedir)
args = ["stack", "install", "--local-bin-path", outpath]
cp = subprocess.run(args, cwd=hlolabasedir)
if cp.returncode != 0:
    sys.exit("Error in Stack")

if os.path.isfile(mainfile):
    shutil.copy(outpath+"/HLola",outbin)
