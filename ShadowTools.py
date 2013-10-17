import numpy 
import ShadowLibExtensions as sd
try: 
    import matplotlib.pyplot as plt
    from matplotlib import cm
    from matplotlib import figure as matfig
    import pylab
    import matplotlib
except ImportError: 
    print(sys.exc_info()[1]) 
    pass

import ShadowToolsPrivate as stp
from ShadowToolsPrivate import Histo1_Ticket as Histo1_Ticket
from ShadowToolsPrivate import plotxy_Ticket as plotxy_Ticket
import os

A2EV = 50676.89919462

def getshonecol(beam,col):
  '''
  Extract a column from a shadow file (eg. begin.dat) or a Shadow.Beam instance. 
  The column are numbered in the fortran convention, i.e. starting from 1.
  It returns a numpy.array filled with the values of the chosen column.
  
  Inumpy.ts:
     beam     : str instance with the name of the shadow file to be loaded. OR
                Shadow.Beam initialized instance.
     col      : int for the chosen columns.
     
  Outputs:
     numpy.array 1-D with length numpy.INT.
     
  Error:
     if an error occurs an ArgsError is raised.
     
  Possible choice for col are:
           1   X spatial coordinate [user's unit]
           2   Y spatial coordinate [user's unit]
           3   Z spatial coordinate [user's unit]
           4   Xp direction or divergence [rads]
           5   Yp direction or divergence [rads]
           6   Zp direction or divergence [rads]
           7   X component of the electromagnetic vector (s-polariz)
           8   Y component of the electromagnetic vector (s-polariz)
           9   Z component of the electromagnetic vector (s-polariz)
          10   Lost ray flag
          11   Energy [eV]
          12   Ray index
          13   Optical path length
          14   Phase (s-polarization)
          15   Phase (p-polarization)
          16   X component of the electromagnetic vector (p-polariz)
          17   Y component of the electromagnetic vector (p-polariz)
          18   Z component of the electromagnetic vector (p-polariz)
          19   Wavelength [A]
          20   R= SQRT(X^2+Y^2+Z^2)
          21   angle from Y axis
          22   the magnituse of the Electromagnetic vector
          23   |E|^2 (total intensity)
          24   total intensity for s-polarization
          25   total intensity for p-polarization
          26   K = 2 pi / lambda [A^-1]
          27   K = 2 pi / lambda * col4 [A^-1]
          28   K = 2 pi / lambda * col5 [A^-1]
          29   K = 2 pi / lambda * col6 [A^-1]
          30   S0-stokes = |Es|^2 + |Ep|^2
          31   S1-stokes = |Es|^2 - |Ep|^2
          32   S2-stokes = 2 |Es| |Ep| cos(phase_s-phase_p)
          33   S3-stokes = 2 |Es| |Ep| sin(phase_s-phase_p)
  '''
  try: stp.getshonecol_CheckArg(beam,col)
  except stp.ArgsError as e: raise e
  col=col-1
  if isinstance(beam,sd.Beam):
    ray = beam.rays
  else:
    bm = sd.Beam()
    bm.load(beam)
    ray = bm.rays
  if col>=0 and col<18 and col!=10:  column =  ray[:,col]
  if col==10: column =  ray[:,col]/A2EV
  if col==18: column =  2*numpy.pi*1.0e8/ray[:,10]
  if col==19: column =  numpy.sqrt(ray[:,0]*ray[:,0]+ray[:,1]*ray[:,1]+ray[:,2]*ray[:,2])
  if col==20: column =  numpy.arccos(ray[:,4])
  if col==21: column =  numpy.sqrt(numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8,15,16,17] ]),axis=0))
  if col==22: column =  numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8,15,16,17] ]),axis=0)
  if col==23: column =  numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
  if col==24: column =  numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
  if col==25: column =  ray[:,10]*1.0e8
  if col==26: column =  ray[:,3]*ray[:,10]*1.0e8
  if col==27: column =  ray[:,4]*ray[:,10]*1.0e8
  if col==28: column =  ray[:,5]*ray[:,10]*1.0e8
  if col==29:
    E2s = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
    E2p = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
    column =  E2p+E2s    
  if col==30:
    E2s = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
    E2p = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
    column =  E2p-E2s
  if col==31:
    E2s = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
    E2p = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
    Cos = numpy.cos(ray[:,13]-ray[:,14])
    column =  2*E2s*E2p*Cos
  if col==32:
    E2s = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
    E2p = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
    Sin = numpy.sin(ray[:,13]-ray[:,14])
    column =  2*E2s*E2p*Sin
  return column



def getshcol(beam,col):
  '''
  Extract multiple columns from a shadow file (eg.'begin.dat') or a Shadow.Beam instance. 
  The column are numbered in the fortran convention, i.e. starting from 1.
  It returns a numpy.array filled with the values of the chosen column.
  
  Inumpy.ts:
     beam     : str instance with the name of the shadow file to be loaded. OR
                Shadow.Beam initialized instance.
     col      : tuple or list instance of int with the number of columns chosen.
     
  Outputs:
     numpy.array 2-D with dimension R x numpy.INT. Where R is the total number of column chosen
     
  Error:
     if an error occurs an ArgsError is raised.      

  Possible choice for col are:
           1   X spatial coordinate [user's unit]
           2   Y spatial coordinate [user's unit]
           3   Z spatial coordinate [user's unit]
           4   X' direction or divergence [rads]
           5   Y' direction or divergence [rads]
           6   Z' direction or divergence [rads]
           7   X component of the electromagnetic vector (s-polariz)
           8   Y component of the electromagnetic vector (s-polariz)
           9   Z component of the electromagnetic vector (s-polariz)
          10   Lost ray flag
          11   Energy [eV]
          12   Ray index
          13   Optical path length
          14   Phase (s-polarization)
          15   Phase (p-polarization)
          16   X component of the electromagnetic vector (p-polariz)
          17   Y component of the electromagnetic vector (p-polariz)
          18   Z component of the electromagnetic vector (p-polariz)
          19   Wavelength [A]
          20   R= SQRT(X^2+Y^2+Z^2)
          21   angle from Y axis
          22   the magnituse of the Electromagnetic vector
          23   |E|^2 (total intensity)
          24   total intensity for s-polarization
          25   total intensity for p-polarization
          26   K = 2 pi / lambda [A^-1]
          27   K = 2 pi / lambda * col4 [A^-1]
          28   K = 2 pi / lambda * col5 [A^-1]
          29   K = 2 pi / lambda * col6 [A^-1]
          30   S0-stokes = |Es|^2 + |Ep|^2
          31   S1-stokes = |Es|^2 - |Ep|^2
          32   S2-stokes = 2 |Es| |Ep| cos(phase_s-phase_p)
          33   S3-stokes = 2 |Es| |Ep| sin(phase_s-phase_p)
  '''
  try: stp.getshcol_CheckArg(beam,col)
  except stp.ArgsError as e: raise e
  if isinstance(beam,sd.Beam):
    bm = beam
  else:
    bm = sd.Beam()
    bm.load(beam)  
  ret = []
  if isinstance(col, int): return getshonecol(bm,col)
  for c in col:
    ret.append(getshonecol(bm,c))
  return tuple(ret)

def histo1(beam,col,xrange=None,yrange=None,nbins=50,nolost=0,ref=0,write=0,title='HISTO1',xtitle=None,ytitle=None,calfwhm=0,noplot=0):
  '''
  Plot the histogram of a column, simply counting the rays, or weighting with the intensity.
  It returns a ShadowTools.Histo1_Ticket which contains the histogram data, and the figure.
  
  Inumpy.ts:
     beam     : str instance with the name of the shadow file to be loaded, or a Shadow.Beam initialized instance.
     col      : int for the chosen column.
  
  Optional Inumpy.ts:
     xrange   : tuple or list of length 2 describing the interval of interest for x, the data read from the chosen column.
     yrange   : tuple or list of length 2 describing the interval of interest for y, counts or intensity depending on ref.
     nbins    : number of bins of the histogram.
     nolost   : 
           0   All rays
           1   Only good rays
           2   Only lost rays
     ref      : 
           0   only count the rays
           1   weight with intensity (look at 23 |E|^2 total intensity)
     write    : 
           0   don't write any file
           1   write the histogram into the file 'HISTO1'.
     title    : title of the figure, it will appear on top of the window.
     xtitle   : label for the x axis.
     ytitle   : label for the y axis.
     calfwhm : 
           0   don't compute the fwhm
           1   compute the fwhm
     noplot   : 
           0   plot the histogram
           1   don't plot the histogram
  orientation :
  'vertical'   x axis for data, y for intensity
'horizontal'   y axis for data, x for intensity
     plotxy   : 
           0   standalone version
           1   to use within plotxy
  Outputs:
     ShadowTools.Histo1_Ticket instance.
     
  Error:
     if an error occurs an ArgsError is raised.

  Possible choice for col are:
           1   X spatial coordinate [user's unit]
           2   Y spatial coordinate [user's unit]
           3   Z spatial coordinate [user's unit]
           4   X' direction or divergence [rads]
           5   Y' direction or divergence [rads]
           6   Z' direction or divergence [rads]
           7   X component of the electromagnetic vector (s-polariz)
           8   Y component of the electromagnetic vector (s-polariz)
           9   Z component of the electromagnetic vector (s-polariz)
          10   Lost ray flag
          11   Energy [eV]
          12   Ray index
          13   Optical path length
          14   Phase (s-polarization)
          15   Phase (p-polarization)
          16   X component of the electromagnetic vector (p-polariz)
          17   Y component of the electromagnetic vector (p-polariz)
          18   Z component of the electromagnetic vector (p-polariz)
          19   Wavelength [A]
          20   R= SQRT(X^2+Y^2+Z^2)
          21   angle from Y axis
          22   the magnituse of the Electromagnetic vector
          23   |E|^2 (total intensity)
          24   total intensity for s-polarization
          25   total intensity for p-polarization
          26   K = 2 pi / lambda [A^-1]
          27   K = 2 pi / lambda * col4 [A^-1]
          28   K = 2 pi / lambda * col5 [A^-1]
          29   K = 2 pi / lambda * col6 [A^-1]
          30   S0-stokes = |Es|^2 + |Ep|^2
          31   S1-stokes = |Es|^2 - |Ep|^2
          32   S2-stokes = 2 |Es| |Ep| cos(phase_s-phase_p)
          33   S3-stokes = 2 |Es| |Ep| sin(phase_s-phase_p)
  '''
  try: stp.Histo1_CheckArg(beam,col,xrange,yrange,nbins,nolost,ref,write,title,xtitle,ytitle,calfwhm,noplot)
  except stp.ArgsError as e: raise e  
  col=col-1
  if ref==1: ref = 23
  plt.ioff()
  
  figure = pylab.plt.figure()
  axHist = figure.add_axes([0.1,0.1,0.8,0.8])

  if ytitle!=None: 
    ytitlesave=ytitle
  else:
    ytitlesave=None
  if ref==0: 
    x, a = getshcol(beam,(col+1,10))
    w = numpy.ones(len(x))
  else:
    x, a, w = getshcol(beam,(col+1,10,ref))
  if nolost==0: 
    t = numpy.where(a!=-3299)
    ytitle = 'All rays'
  if nolost==1: 
    t = numpy.where(a==1.0)
    ytitle = 'Good rays'
  if nolost==2: 
    t = numpy.where(a!=1.0)
    ytitle = 'Lost rays'
  if len(t[0])==0:
    print "no rays match the selection, the histogram will not be plotted"
    return 
  if ref==0:
    ytitle = 'counts ' + ytitle
    h,bins,patches = axHist.hist(x[t],bins=nbins,range=xrange,histtype='step',alpha=0.5)
    if yrange==None: yrange = [0.0, numpy.max(h)*1.1]
    hw=h
  if ref>=22: 
    ytitle = (stp.getLabel(ref-1))[0] + ' ' + ytitle
    h,bins = numpy.histogram(x[t],range=xrange,bins=nbins)
    hw,bins,patches = axHist.hist(x[t],range=xrange, bins=nbins,histtype='step',alpha=0.5,weights=w[t])
    if yrange==None: yrange = [0.0, numpy.max(hw)*1.1]
  fwhm = None
  if calfwhm==1:
    fwhm, tf, ti = stp.calcFWHM(hw,bins[1]-bins[0])
    axHist.plot([bins[ti],bins[tf+1]],[max(h)*0.5,max(h)*0.5],'x-')
    print "fwhm = ", fwhm
  if write==1: stp.Histo1_write(title,bins,h,hw,col,beam,ref-1)  

  if xtitle==None: xtitle=(stp.getLabel(col))[0]
  axHist.set_xlabel(xtitle)
  
  if ytitlesave!=None:
    axHist.set_ylabel(ytitlesave)
  else:
    axHist.set_ylabel(ytitle)
  if title!=None: axHist.set_title(title)
  if xrange!=None: axHist.set_xlim(xrange)
  if yrange!=None: axHist.set_ylim(yrange)    

  if noplot==0: 
    figure.show()  
  
  ticket = Histo1_Ticket()    
  ticket.histogram = hw
  ticket.bin_center = bins[:-1]+(bins[1]-bins[0])*0.5
  ticket.bin_left = bins[:-1] 
  ticket.figure = figure
  ticket.xrange = xrange
  ticket.yrange = yrange
  ticket.xtitle = xtitle
  ticket.ytitle = ytitle
  ticket.title = title
  ticket.fwhm = fwhm
  ticket.intensity = w[t].sum()
  return ticket
 



def plotxy(beam,cols1,cols2,nbins=25,nbins_h=None,level=5,xrange=None,yrange=None,nolost=0,title='PLOTXY',xtitle=None,ytitle=None,noplot=0,calfwhm=0,contour=0):
  '''
  Draw the scatter or contour or pixel-like plot of two columns of a Shadow.Beam instance or of a given shadow file, along with histograms for the intensity on the top and right side.
  Inumpy.ts:
     beam     : str instance with the name of the shadow file to be loaded, or a Shadow.Beam initialized instance.
     cols1    : first column.
     cols2    : second column.
  
  Optional Inumpy.ts:
     nbins    : int for the size of the grid (nbins x nbins). It will affect the plot only if non scatter.
     nbins_h  : int for the number of bins for the histograms
     level    : int number of level to be drawn. It will affect the plot only if contour.
     xrange   : tuple or list of length 2 describing the interval of interest for x, the data read from the chosen column.
     yrange   : tuple or list of length 2 describing the interval of interest for y, counts or intensity depending on ref.
     nolost   : 
           0   All rays
           1   Only good rays
           2   Only lost rays
     title    : title of the figure, it will appear on top of the window.
     xtitle   : label for the x axis.
     ytitle   : label for the y axis.
     noplot   : 
           0   plot the histogram
           1   don't plot the histogram
     calfwhm :
           0   don't compute the fwhm
           1   compute the fwhm and draw it
           2   in addition to calfwhm=1, it computes now the intensity in a
               slit of FWHM_h x FWHM_v
     contour  :
           0   scatter plot
           1   contour, black & white, only counts (without intensity)
           2   contour, black & white, with intensity.
           3   contour, colored, only counts (without intensity)
           4   contour, colored, with intensity.
           5   pixelized, colored, only counts (without intensity)
           6   pixelized, colored, with intensity.
  Outputs:
     ShadowTools.Histo1_Ticket instance.
     
  Error:
     if an error occurs an ArgsError is raised.

  Possible choice for col are:
           1   X spatial coordinate [user's unit]
           2   Y spatial coordinate [user's unit]
           3   Z spatial coordinate [user's unit]
           4   X' direction or divergence [rads]
           5   Y' direction or divergence [rads]
           6   Z' direction or divergence [rads]
           7   X component of the electromagnetic vector (s-polariz)
           8   Y component of the electromagnetic vector (s-polariz)
           9   Z component of the electromagnetic vector (s-polariz)
          10   Lost ray flag
          11   Energy [eV]
          12   Ray index
          13   Optical path length
          14   Phase (s-polarization)
          15   Phase (p-polarization)
          16   X component of the electromagnetic vector (p-polariz)
          17   Y component of the electromagnetic vector (p-polariz)
          18   Z component of the electromagnetic vector (p-polariz)
          19   Wavelength [A]
          20   R= SQRT(X^2+Y^2+Z^2)
          21   angle from Y axis
          22   the magnituse of the Electromagnetic vector
          23   |E|^2 (total intensity)
          24   total intensity for s-polarization
          25   total intensity for p-polarization
          26   K = 2 pi / lambda [A^-1]
          27   K = 2 pi / lambda * col4 [A^-1]
          28   K = 2 pi / lambda * col5 [A^-1]
          29   K = 2 pi / lambda * col6 [A^-1]
          30   S0-stokes = |Es|^2 + |Ep|^2
          31   S1-stokes = |Es|^2 - |Ep|^2
          32   S2-stokes = 2 |Es| |Ep| cos(phase_s-phase_p)
          33   S3-stokes = 2 |Es| |Ep| sin(phase_s-phase_p)
  '''
  if nbins_h==None: nbins_h=nbins+1
  try: 
    stp.plotxy_CheckArg(beam,cols1,cols2,nbins,nbins_h,level,xrange,yrange,nolost,title,xtitle,ytitle,noplot,calfwhm,contour)
  except stp.ArgsError as e: 
    raise e
  plt.ioff()
  col1,col2,col3,col4 = getshcol(beam,(cols1,cols2,10,23,))

  nbins=nbins+1
  if xtitle==None: xtitle=(stp.getLabel(cols1-1))[0]
  if ytitle==None: ytitle=(stp.getLabel(cols2-1))[0]
  
  if nolost==0: t = numpy.where(col3!=-3299)
  if nolost==1: t = numpy.where(col3==1.0)
  if nolost==2: t = numpy.where(col3!=1.0)  

  if xrange==None: xrange = stp.setGoodRange(col1[t])
  if yrange==None: yrange = stp.setGoodRange(col2[t])
  #print xrange
  #print yrange
  tx = numpy.where((col1>xrange[0])&(col1<xrange[1]))
  ty = numpy.where((col2>yrange[0])&(col2<yrange[1]))
  
  tf = set(list(t[0])) & set(list(tx[0])) & set(list(ty[0]))
  t = (numpy.array(sorted(list(tf))),)
  if len(t[0])==0: 
    print "no point selected"
    return None
  
  figure = pylab.plt.figure(figsize=(12,8),dpi=96)
  
  ratio = 8.0/12.0
  left, width = 0.1*ratio, 0.65*ratio
  bottom, height = 0.1, 0.65
  bottom_h = bottom+height+0.02
  left_h = left+width+0.02*ratio

  rect_scatter = [0.10*ratio, 0.10, 0.65*ratio, 0.65]
  rect_histx =   [0.10*ratio, 0.77, 0.65*ratio, 0.20]
  rect_histy =   [0.77*ratio, 0.10, 0.20*ratio, 0.65]
  rect_text =    [1.00*ratio, 0.10, 1.20*ratio, 0.65]


  axScatter = figure.add_axes(rect_scatter)
  axScatter.set_xlabel(xtitle)
  axScatter.set_ylabel(ytitle)

  if contour==0:
    axScatter.scatter(col1[t],col2[t],s=0.5)
  if contour>0 and contour<7:
    if contour==1 or contour==3 or contour==5: w = numpy.ones( len(col1) )
    if contour==2 or contour==4 or contour==6: w = col4
    grid = numpy.zeros(nbins*nbins).reshape(nbins,nbins)
    for i in t[0]:
      indX = stp.findIndex(col1[i],nbins,xrange[0],xrange[1])
      indY = stp.findIndex(col2[i],nbins,yrange[0],yrange[1])
      try:
        grid[indX][indY] = grid[indX][indY] + w[i]
      except IndexError:
        pass
    X, Y = numpy.mgrid[xrange[0]:xrange[1]:nbins*1.0j,yrange[0]:yrange[1]:nbins*1.0j]
    L = numpy.linspace(numpy.amin(grid),numpy.amax(grid),level)
    if contour==1 or contour==2: axScatter.contour(X, Y, grid, colors='k', levels=L)
    if contour==3 or contour==4: axScatter.contour(X, Y, grid, levels=L)
    if contour==5 or contour==6: axScatter.pcolor(X, Y, grid)  
  #axScatter.set_xlim(xrange)
  #axScatter.set_ylim(yrange)
  
  #axScatter.axis(xmin=xrange[0],xmax=xrange[1])
  #axScatter.axis(ymin=yrange[0],ymax=yrange[1])
  
  for tt in axScatter.get_xticklabels():
    tt.set_size('x-small')
  for tt in axScatter.get_yticklabels():
    tt.set_size('x-small')
  
  #if ref==0: col4 = numpy.ones(len(col4),dtype=float)
  
  axHistx = figure.add_axes(rect_histx, sharex=axScatter)
  axHisty = figure.add_axes(rect_histy, sharey=axScatter)
  
  binx = numpy.linspace(xrange[0],xrange[1],nbins_h)
  biny = numpy.linspace(yrange[0],yrange[1],nbins_h)
  if contour==0 or contour==1 or contour==3 or contour==5:
    hx, binx, patchx = axHistx.hist(col1[t],bins=binx,range=xrange,histtype='step',color='k')
    hy, biny, patchy = axHisty.hist(col2[t],bins=biny,range=yrange,orientation='horizontal',histtype='step',color='k')
  if contour==2 or contour==4 or contour==6:
    hx, binx, patchx = axHistx.hist(col1[t],bins=binx,range=xrange,weights=col4[t],histtype='step',color='b')
    hy, biny, patchy = axHisty.hist(col2[t],bins=biny,range=yrange,weights=col4[t],orientation='horizontal',histtype='step',color='b')
  for tl in axHistx.get_xticklabels(): tl.set_visible(False)
  for tl in axHisty.get_yticklabels(): tl.set_visible(False)
  for tt in axHisty.get_xticklabels():
    tt.set_rotation(270)
    tt.set_size('x-small')
  for tt in axHistx.get_yticklabels():
    tt.set_size('x-small')

  intensityinslit = 0.0
  if calfwhm>=1:
    fwhmx,txf, txi = stp.calcFWHM(hx,binx[1]-binx[0])
    fwhmy,tyf, tyi = stp.calcFWHM(hy,biny[1]-biny[0])
    axHistx.plot([binx[txi],binx[txf+1]],[max(hx)*0.5,max(hx)*0.5],'x-')
    axHisty.plot([max(hy)*0.5,max(hy)*0.5],[biny[tyi],biny[tyf+1]],'x-')
    print "fwhm horizontal:  ", fwhmx
    print "fwhm vertical:    ", fwhmy
  if calfwhm>=2:
    xx1 = binx[txi]
    xx2 = binx[txf+1]
    yy1 = biny[tyi]
    yy2 = biny[tyf+1]
    print "limits horizontal:  ", binx[txi],binx[txf+1]
    print "limits vertical:  ", biny[tyi],biny[tyf+1]
    axScatter.plot([xx1,xx2,xx2,xx1,xx1],[yy1,yy1,yy2,yy2,yy1])
    #fwhmx,txf, txi = stp.calcFWHM(hx,binx[1]-binx[0])
    #fwhmy,tyf, tyi = stp.calcFWHM(hy,biny[1]-biny[0])
    #calculate intensity in slit
    if nolost==0: tt = numpy.where(col3!=-3299)
    if nolost==1: tt = numpy.where(col3==1.0)
    if nolost==2: tt = numpy.where(col3!=1.0)  

    ttx = numpy.where((col1>=xx1)&(col1<=xx2))
    tty = numpy.where((col2>=yy1)&(col2<=yy2))
  
    ttf = set(list(tt[0])) & set(list(ttx[0])) & set(list(tty[0]))
    tt = (numpy.array(sorted(list(ttf))),)
    if len(tt[0])>0: 
      intensityinslit = col4[tt].sum()
      print "Intensity in slit: ",intensityinslit
    
  if title!=None:
    axHistx.set_title(title)
  axText = figure.add_axes(rect_text)
  ntot = len(numpy.where(col3!=3299)[0])
  ngood = len(numpy.where(col3==1)[0])
  nbad = ntot - ngood
  if nolost==0: axText.text(0.0,0.8,"ALL RAYS")
  if nolost==1: axText.text(0.0,0.8,"GOOD RAYS")
  if nolost==2: axText.text(0.0,0.8,"LOST RAYS")
  tmps = "intensity: "+str(col4[t].sum())
  if calfwhm == 2:
      tmps=tmps+" (in slit:"+str(intensityinslit)+") "
  axText.text(0.0,0.7,tmps)
  axText.text(0.0,0.6,"total number of rays: "+str(ntot))
  axText.text(0.0,0.5,"total good rays: "+str(ngood))
  axText.text(0.0,0.4,"total lost rays: "+str(ntot-ngood))
  if calfwhm>=1:
    axText.text(0.0,0.3,"fwhm H: "+str(fwhmx))
    axText.text(0.0,0.2,"fwhm V: "+str(fwhmy))
  if isinstance(beam,str): axText.text(0.0,0.1,"FILE: "+beam)
  if isinstance(beam,sd.Beam): axText.text(0.0,0.1,"from Shadow3 Beam instance")
  axText.text(0.0,0.0,"DIR: "+os.getcwd())
  axText.set_axis_off()
  pylab.plt.draw()
  if noplot==0: figure.show()
  ticket = plotxy_Ticket()
  ticket.figure = figure
  ticket.xrange = xrange
  ticket.yrange = yrange
  ticket.xtitle = xtitle
  ticket.ytitle = ytitle
  ticket.title = title
  if calfwhm>=1:
    ticket.fwhmx = fwhmx
    ticket.fwhmy = fwhmy
  ticket.intensity = col4[t].sum()
  ticket.averagex = numpy.average( col1[t] )
  ticket.averagey = numpy.average( col2[t] )
  ticket.intensityinslit = intensityinslit
  return ticket  
  
if __name__=="__main__":
  import Shadow as sd
  from matplotlib import pyplot as plt
  a = sd.Beam()
  a.load("begin.dat")
  ticket1 = histo1(a,1,nbins=25,noplot=1,calfwhm=1)
  ticket1.figure.show()
  raw_inumpy.t()
  ticket1 = plotxy(a,(1,3),noplot=1)
  ticket1.figure.show()
  raw_inumpy.t()

