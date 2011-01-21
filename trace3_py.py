import Shadow as sp
import numpy as np
import sys

src = sp.Source()
oe1 = sp.OE()
ray = sp.Ray()
i=0


if sys.argv[1]=='-s':
  src.read('start.00')
  ray.genSource(src)
  
if sys.argv[1]=='-t':
  ray.read('begin.dat')  
  for file in open("systemfile.dat","r").readlines():
    if file!="":      
      oe1.read(file.strip())
      ray.trace(oe1,i)
      i=i+1

if sys.argv[1]=='-a':
  src.read('start.00')
  ray.genSource(src)
  for file in open("systemfile.dat","r").readlines():
    if file!="":
      oe1.read(file.strip())
      ray.trace(oe1,i)
      i=i+1
  