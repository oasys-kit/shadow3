from __future__ import print_function
import Shadow.ShadowLibExtensions as sd
import numpy 
import matplotlib.pyplot as plt
import matplotlib
import os
import socket
import getpass
import datetime


class ArgsError(Exception):
  def __init__(self,value):
    self.value = value
  def __str__(self):
    return repr(self.value)


class NoValueSelectedError(Exception):
  def __init__(self,value):
    self.value = value
  def __str__(self):
    return repr(self.value)


class Histo1_Ticket:
  def __init__(self):
    self.histogram = None
    self.bin_center = None
    self.bin_left = None
    self.figure = None
    self.xrange = None
    self.yrange = None
    self.xtitle = None
    self.ytitle = None
    self.title = None
    self.fwhm = None

class plotxy_Ticket:
  def __init__(self):
    self.figure = None
    self.xrange = None
    self.yrange = None
    self.xtitle = None
    self.ytitle = None
    self.title = None
    self.fwhmx = None
    self.fwhmy = None


def ErrorMsg(fromFunc,value):
  print (fromFunc+" called with an error in the arguments.\nCheck help function")
  return ArgsError(value)

def getshonecol_CheckArg(beam,col):
  if not isinstance(beam,(sd.Beam,str)): raise ErrorMsg('getshonecol','beam')
  if not isinstance(col,int): raise ErrorMsg('getshonecol','col')
  if col<1 or col>33: raise ErrorMsg('getshonecol','col')

def getshcol_CheckArg(beam,col): #the rest of checks are included in the function getshonecol_CheckArg 
  if not isinstance(beam,(sd.Beam,str)): raise ErrorMsg('getshcol','beam')
  if not isinstance(col,(int,tuple,list)): raise ErrorMsg('getshcol','col')
  if isinstance(col, int): 
    if col<1 or col>33: raise ErrorMsg('getshcol','col')
  else:
    for c in col:
      if not isinstance(c,int): raise ErrorMsg('getshcol','col')
      if c<1 or c>33: raise ErrorMsg('getshcol','col')

def Histo1_CheckArg(beam,col,xrange,yrange,nbins,nolost,ref,write,title,xtitle,ytitle,calfwhm,noplot):
  if not isinstance(beam,(sd.Beam,str)): raise ErrorMsg('Histo1','beam')
  if not isinstance(col,int): raise ErrorMsg('Histo1','col')  
  if col<1 or col>33: raise ErrorMsg('Histo1','col')
  # the next 3 lines don't matter, it is a trick to pass the test when None
  if xrange==None: xrange=(1.0,2.0) 
  if yrange==None: yrange=(1.0,2.0) 
  if xtitle==None: xtitle='pippo'  
  if ytitle==None: ytitle='pippo'
  if not isinstance(xrange,(tuple,list)): raise ErrorMsg('Histo1','xrange') 
  if len(xrange)!=2: raise ErrorMsg('Histo1','xrange')
  if not isinstance(xrange[0],(int,float)) or not isinstance(xrange[1],(int,float)): raise ErrorMsg('Histo1','xrange')
  if not isinstance(yrange,(tuple,list)): raise ErrorMsg('Histo1','yrange')
  if len(yrange)!=2: raise ErrorMsg('Histo1','yrange')
  if not isinstance(yrange[0],(int,float)) or not isinstance(yrange[1],(int,float)): raise ErrorMsg('Histo1','yrange')
  if not isinstance(nbins,int): raise ErrorMsg('Histo1','nbins')
  if nbins<=0: raise ErrorMsg('Histo1','nbins')  
  if nolost!=0 and nolost!=1 and nolost!=2: raise ErrorMsg('Histo1','nolost')
  if ref>=22 and ref<=33: ref = 1
  if ref!=0 and ref!=1: raise ErrorMsg('Histo1','ref')
  if write!=0 and write!=1: raise ErrorMsg('Histo1','write')
  if not isinstance(title,str): raise ErrorMsg('Histo1','title')
  if not isinstance(xtitle,str): raise ErrorMsg('Histo1','xtitle')
  if not isinstance(ytitle,str): raise ErrorMsg('Histo1','ytitle')
  if calfwhm!=0 and calfwhm!=1: raise ErrorMsg('Histo1','calfwhm')
  if noplot!=0 and noplot!=1: raise ErrorMsg('Histo1','noplot')
  
def plotxy_CheckArg(beam,cols1,cols2,nbins,nbins_h,level,xrange,yrange,nolost,title,xtitle,ytitle,noplot,calfwhm,contour):
  if not isinstance(beam,(sd.Beam,str)): raise ErrorMsg('plotxy','beam')
  if cols1<1 or cols1>33: raise ErrorMsg('plotxy','cols1')
  if cols2<1 or cols2>33: raise ErrorMsg('plotxy','cols2')
  if not isinstance(nbins,int): raise ErrorMsg('plotxy','nbins')
  if nbins<=0: raise ErrorMsg('plotxy','nbins')
  if not isinstance(nbins_h,int): raise ErrorMsg('plotxy','nbins_h')
  if nbins_h<=0: raise ErrorMsg('plotxy','nbins_h')
  if not isinstance(level,int): raise ErrorMsg('plotxy','level')
  if level<=0: raise ErrorMsg('plotxy','level')
  # the next 4 lines don't matter, it is a trick to pass the test when None
  if xrange==None: xrange=(1.0,2.0) 
  if yrange==None: yrange=(1.0,2.0) 
  if xtitle==None: xtitle='pippo'
  if ytitle==None: ytitle='pippo' 
  if not isinstance(xrange,(tuple,list)): raise ErrorMsg('plotxy','xrange') 
  if len(xrange)!=2: raise ErrorMsg('plotxy','xrange')
  if not isinstance(xrange[0],(int,float)) or not isinstance(xrange[1],(int,float)): raise ErrorMsg('plotxy','xrange')
  if not isinstance(yrange,(tuple,list)): raise ErrorMsg('plotxy','yrange')
  if len(yrange)!=2: raise ErrorMsg('plotxy','yrange')
  if not isinstance(yrange[0],(int,float)) or not isinstance(yrange[1],(int,float)): raise ErrorMsg('plotxy','yrange')
  if nolost!=0 and nolost!=1 and nolost!=2: raise ErrorMsg('plotxy','nolost')
  if not isinstance(title,str): raise ErrorMsg('plotxy','title')
  if not isinstance(xtitle,str): raise ErrorMsg('plotxy','xtitle')
  if not isinstance(ytitle,str): raise ErrorMsg('plotxy','ytitle')
  if noplot!=0 and noplot!=1: raise ErrorMsg('plotxy','noplot')
  #if ref!=0 and ref!=1: raise ErrorMsg('plotxy','ref')
  if calfwhm!=0 and calfwhm!=1 and calfwhm!=2: raise ErrorMsg('plotxy','calfwhm')
  if not isinstance(contour,int): raise ErrorMsg('plotxy','contour')
  if contour<0 or contour>6: raise ErrorMsg('plotxy','contour')


def setGoodRange(col):
  if col.size == 0:
      return [-1,1]
  rmin = min(col)
  rmax = max(col)
  if rmin>0.0:
    rmin = rmin*0.95
  else:
    rmin = rmin*1.05
  if rmax<0.0:
    rmax = rmax*0.95
  else:
    rmax = rmax*1.05
  if rmin==rmax:
    rmin = rmin*0.95
    rmax = rmax*1.05
    if rmin==0.0:
      rmin = -1.0
      rmax =  1.0
  return [rmin,rmax]
  
def findIndex(xx,n,la,lb):  
  return int( numpy.floor((xx-(lb-la)*0.5/n-la)*n/(lb-la)) )
  

def calcFWHM(h,binSize):
  t = numpy.where(h>max(h)*0.5)
  return binSize*(t[0][-1]-t[0][0]+1), t[0][-1], t[0][0]


def Histo1_write(title,bins,h,w,col,beam,ref):
  if isinstance(beam,sd.Beam): usubtitle = "Shadow running in dir "+os.getcwd()
  if isinstance(beam,str): usubtitle = os.getcwd()+beam
  now = str(datetime.datetime.now())  
  usubtitle += " "+now+" "+getpass.getuser()+"@"+socket.gethostname()
  file = open(title,'w')  
  print ("#F HISTO1", file = file)
  print ("#C This file has been created using histo1 (python ShadowTools)", file = file)
  print ("#D "+now, file = file)
  print ("#UTITLE", file = file)
  print ("#USUBTITLE "+usubtitle, file = file)
  print ("#UTTEXT", file = file)
  print ("#C COLUMN 1 CORRESPONDS TO ABSCISSAS IN THE CENTER OF EACH BIN", file = file)
  print ("#C COLUMN 2 CORRESPONDS TO ABSCISSAS IN THE THE LEFT CORNER OF THE BIN", file = file)
  print ("#C COLUMN 3 CORRESPONDS TO INTENSITY (COUNTING RAYS)", file = file)
  print ("#C COLUMN 4 CORRESPONDS TO INTENSITY (WEIGHTED IF SELECTED)", file = file)
  print (" ", file = file)
  print ("#S 1 histogram", file = file)
  print ("#N 4" , file = file)
  print ("#L "+getLabel(col)[1]+"  "+(getLabel(col))[1]+"  "+"intensity (rays)"+"  "+(getLabel(ref))[1], file = file)
  for i in range(len(h)):
    print ("%f\t%f\t%f\t%f" % ( (bins[i]+bins[i+1])*0.5, bins[i], h[i], w[i] ), file = file)
  file.close()
  

def getLabel(col):
  Label= [
          [  r"$x$ [user unit]", "x [user unit]"  ],
          [  r"$y$ [user unit]", "y [user unit]"  ],
          [  r"$z$ [user unit]", "z [user unit]"  ],
          [  r"$\dot{x}$ [rads]", "x' [rads]"  ],
          [  r"$\dot{y}$ [rads]", "y' [rads]"  ],
          [  r"$\dot{z}$ [rads]", "z' [rads]"  ],
          [  r"$\mathbf{E}_{\sigma x}$", "Es_x"  ],
          [  r"$\mathbf{E}_{\sigma y}$", "Es_y"  ],
          [  r"$\mathbf{E}_{\sigma z}$", "Es_z"  ],
          [  r"ray flag", "Ray flag"  ],
          [  r"E [eV]", "Energy"  ],
          [  r"Ray index", "Ray index"  ],
          [  r"s", "Opt. Path"  ],
          [  r"$\phi_{\sigma}$", "phase_s"  ],
          [  r"$\phi_{\pi}$", "phase_p"  ],
          [  r"$\mathbf{E}_{\pi x}$", "Ep_x"  ],
          [  r"$\mathbf{E}_{\pi y}$", "Ep_y"  ],
          [  r"$\mathbf{E}_{\pi z}$", "Ep_z"  ],
          [  r"$\lambda$ [$\AA$]", "wavelength"  ],
          [  r"$R= \sqrt{x^2+y^2+z^2}$", "R [user unit]"  ],
          [  r"$\theta$" , "theta"  ],
          [  r"$\Vert\mathbf{E_{\sigma}}+\mathbf{E_{\pi}}\Vert$", "Electromagnetic vector magnitude"  ],
          [  r"$\Vert\mathbf{E_{\sigma}}+\mathbf{E_{\pi}}\Vert^2$", "intensity (weight column = 23: |E|^2 (total intensity))"  ],
          [  r"$\Vert\mathbf{E_{\sigma}}\Vert^2$", "intensity (weight column = 24: |E_s|^2 (sigma intensity))"  ],
          [  r"$\Vert\mathbf{E_{\pi}}\Vert^2$", "intensity (weight column = 25: |E_p|^2 (pi intensity))"  ],
          [  r"$K = \frac{2 \pi}{\lambda} [A^{-1}]$", "K magnitude"  ],
          [  r"$K_x = \frac{2 \pi}{\lambda} \dot{x}$ [$\AA^{-1}$]", "K_x"  ],
          [  r"$K_y = \frac{2 \pi}{\lambda} \dot{y}$ [$\AA^{-1}$]", "K_y"  ],
          [  r"$K_z = \frac{2 \pi}{\lambda} \dot{z}$ [$\AA^{-1}$]", "K_z"  ],
          [  r"$S_0 = \Vert\mathbf{E}_{\sigma}\Vert^2 + \Vert\mathbf{E}_{\pi}\Vert^2 $", "S0"  ],
          [  r"$S_1 = \Vert\mathbf{E}_{\sigma}\Vert^2 - \Vert\mathbf{E}_{\pi}\Vert^2 $", "S1"  ],
          [  r"$S_2 = 2 \Vert\mathbf{E}_{\sigma}\Vert \cdot \Vert\mathbf{E}_{\pi}\Vert \cos{(\phi_{\sigma}-\phi_{\pi})}$", "S2"  ],
          [  r"$S_3 = 2 \Vert\mathbf{E}_{\sigma}\Vert \cdot \Vert\mathbf{E}_{\pi}\Vert \sin{(\phi_{\sigma}-\phi_{\pi})}$", "S3"  ]
         ]
  return Label[col]
