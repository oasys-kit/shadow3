#
#
# Shadow3 python script to run a source and an optical system from files
#
#
# It uses the currently defined Shadow system defined in the (existing) 
# files start.xx and systemfile.dat
#
# Author: Niccolo Canestrari, Manuel Sanchew del Rio
#         ESRF (c) 2011
#

#
# import block
#
import Shadow 
import sys

src = Shadow.Source()
oe1 = Shadow.OE()
beam = Shadow.Beam()
i=0


if len(sys.argv)==1:
  print ("Usage: \n  trace3_py.py -a ->creates source \n  trace3_py.py -t ->runs trace (optical system) \n  trace3_py.py -a ->runs both source and trace \n" )
  exit()

if sys.argv[1]=='-s':
  src.load('start.00')
  beam.genSource(src)
  
if sys.argv[1]=='-t':
  beam.load('begin.dat')  
  for file in open("systemfile.dat","r").readlines():
    if file!="":      
      oe1.load(file.strip())
      beam.traceOE(oe1,i)
      i=i+1

if sys.argv[1]=='-a':
  src.load('start.00')
  beam.genSource(src)
  for file in open("systemfile.dat","r").readlines():
    if file!="":
      oe1.load(file.strip())
      beam.traceOE(oe1,i)
      i=i+1
  
