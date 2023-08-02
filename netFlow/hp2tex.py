#!/usr/bin/env python3
import argparse

accum = None
outnumbers = []

def pushtoaccum(n):
  global accum
  accum = n + (0 if accum == None else accum)

def pushtoout():
  global accum
  global outnumbers
  if accum == None:
    return
  outnumbers.append(accum)

def process(line):
  global accum
  mnum = line[line.find('\t'):-1]
  if mnum != "":
    pushtoaccum(int(mnum[1:]))
  if line.startswith("END_SAMPLE "):
    pushtoout()
    accum = None

def totex(numbers, yscale, skip):
  preamble = """\\documentclass{standalone}
\\usepackage{pgfplots}
\\pgfplotsset{every x tick label/.append style={font=\\small}}
\\pgfplotsset{every y tick label/.append style={font=\\small}}
\\pgfplotsset{ylabel style={yshift=-0.6cm}}
\\pgfplotsset{xlabel style={yshift=0.15cm}}
\\begin{document}
\\begin{tikzpicture}
\\begin{axis}[
    xmin=0,
    ymin=0,
    xlabel={Time},
    ylabel={Memory},
]
\\addplot[no markers,red] table {"""

  postamble = """};
\end{axis}
\end{tikzpicture}
\end{document}"""
  print(preamble)
  x = 0
  for ix, number in enumerate(numbers):
    if (ix % (skip+1) == 0):
      print(x, round(number*yscale,4))
      x = x + 1
  print(postamble)

def main():
  global outnumbers
  '''
  Use:
  
  ./hp2ps.py --file file.hp [--yscale scale]
  '''
  parser = argparse.ArgumentParser('Convert hp to latex')
  parser.add_argument('--file', help='File to process', type=str)
  parser.add_argument('--yscale', help='Scale the y', type=float, default=1)
  parser.add_argument('--skip', help='Skip samples', type=int, default=0)
  args = parser.parse_args()

  with open(args.file) as file:
    for line in file:
      process(line)
  totex(outnumbers,args.yscale, args.skip)

if __name__ == '__main__':
    main()
