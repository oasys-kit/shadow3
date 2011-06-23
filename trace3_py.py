import Shadow as sp
import numpy as np
import sys

src = sp.Source()
oe1 = sp.OE()
ray = sp.Beam()
i=0


if sys.argv[1]=='-s':
  src.load('start.00')
  ray.genSource(src)
  
if sys.argv[1]=='-t':
  ray.load('begin.dat')  
  for file in open("systemfile.dat","r").readlines():
    if file!="":      
      oe1.load(file.strip())
      ray.traceOE(oe1,i)
      i=i+1

if sys.argv[1]=='-a':
  src.load('start.00')
  ray.genSource(src)
  for file in open("systemfile.dat","r").readlines():
    if file!="":
      oe1.load(file.strip())
      ray.traceOE(oe1,i)
      i=i+1
  
