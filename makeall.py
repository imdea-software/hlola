#!/usr/bin/python3
from pathlib import Path
import os
import sys
import subprocess

def tolatex(fname):
    outfile = "/Users/felipe.gorostiaga/Documents/ares/papers/2021-Slices-and-SWS/specs/" + os.path.splitext(fname)[0] + ".tex"
    with open(outfile, 'w') as outf:
        with open(fname, 'r') as inf:
            subprocess.Popen(["/Users/felipe.gorostiaga/Documents/HLola/frontend/latex_highlighter/latex_highlighter"], stdin=inf, stdout=outf)

hlolafiles = map(Path,sys.argv[1:])

for f in hlolafiles:
    tolatex(f)
