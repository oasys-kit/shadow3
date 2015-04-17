import numpy
import Shadow.ShadowLibExtensions as sd
import sys
import os
try:
    import matplotlib.pylab as plt
    from matplotlib import collections

except ImportError: 
    print(sys.exc_info()[1]) 
    pass

import Shadow.ShadowToolsPrivate as stp
from Shadow.ShadowToolsPrivate import Histo1_Ticket as Histo1_Ticket
from Shadow.ShadowToolsPrivate import plotxy_Ticket as plotxy_Ticket
import os

#A2EV = 50676.89919462
codata_h = numpy.array(6.62606957e-34)
codata_ec = numpy.array(1.602176565e-19)
codata_c = numpy.array(299792458.0)
A2EV = 2.0*numpy.pi/(codata_h*codata_c/codata_ec*1e2)

#TODO: delete. Implemented for beam object
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



#TODO: delete. Implemented for beam object
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

def histo1(beam, col, notitle=0, nofwhm=0,  bar=0,  **kwargs):
    """
    Plot the histogram of a column, as calculated by Shadow.Beam.histo1 using matplotlib

    NOTE: This will replaces the old histo1 still available as histo1_old

    :param beam: a Shadow.Beam() instance, or a file name with Shadow binary file
    :param col: the Shadow column number (start from 1)
    :param notitle: set to 1 to avoid displaying title
    :param nofwhm: set to 1 to avoid labeling FWHM value
    :param bar: 1=bar plot, 0=line plot
    :param kwargs: keywords accepted by Shadow.Beam.histo1()
    :return: the dictionary returned by Shadow.beam.histo1() with some keys added.
    """

    title = "histo1"

    if isinstance(beam,str):
        beam1 = sd.Beam()
        beam1.load(beam)
        title += " - file: "+beam
        beam = beam1

    tk2 = beam.histo1(col, **kwargs)



    h = tk2["histogram"]
    bins = tk2["bin_left"]
    xrange = tk2["xrange"]
    yrange = [0,1.1*numpy.max(h)]
    fwhm = tk2["fwhm"]

    xtitle = "column %d"%tk2["col"]
    ytitle = "counts ("

    if tk2["nolost"] == 0:
        ytitle += " all rays"
    if tk2["nolost"] == 1:
        ytitle += " good rays"
    if tk2["nolost"] == 2:
        ytitle += " lost rays"

    if tk2["ref"] == 0:
        ytitle += " = weight: number of rays"
    else:
        if tk2["ref"] == 23:
            ytitle += " - weight: intensity"
        else:
            ytitle += " - weight column: %d"%(tk2["ref"])

    ytitle += ")"


    if fwhm != None: print ("fwhm = %g" % fwhm)

    fig0 = plt.figure()
    ax = fig0.add_subplot(111)

    ax.set_xlabel(xtitle)
    ax.set_ylabel(ytitle)
    if notitle != 1: ax.set_title(title)
    ax.set_xlim(xrange[0],xrange[1])
    ax.set_ylim(yrange[0],yrange[1])
    ax.grid(True)

    if bar:
        l = ax.bar(bins, h, 1.0*(bins[1]-bins[0]),color='blue') #,error_kw=dict(elinewidth=2,ecolor='red'))
    else:
        l = plt.plot(tk2["bin_path"], tk2["histogram_path"], color='blue') #,error_kw=dict(elinewidth=2,ecolor='red'))

    if tk2["fwhm"] != None:
        hh = 0.5*numpy.max(tk2["histogram"])
        lines = [ [ (tk2["fwhm_coordinates_h"][0],hh), \
                    (tk2["fwhm_coordinates_h"][1],hh) ]]
        print(lines)
        lc = collections.LineCollection(lines,color='red',linewidths=2)
        ax.add_collection(lc)
        if nofwhm != 1:
            if tk2["fwhm_coordinates_h"][0] < 0:
                shift1 = 0.9
            else:
                shift1 = 1.0
            ax.annotate('FWHM=%f'%tk2["fwhm"], xy=(shift1*tk2["fwhm_coordinates_h"][0],1.01*tk2["fwhm_coordinates_v"][0]))

    plt.show()
    return tk2

#TODO: delete. Reimplemented using Shadow.beam.histo1()
def histo1_old(beam,col,xrange=None,yrange=None,nbins=50,nolost=0,ref=0,write=0,title='HISTO1',xtitle=None,ytitle=None,calfwhm=0,noplot=0):
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
  #plot_nicc.ioff()
  plt.ioff()
  
  figure = plt.figure()
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
    print ("no rays match the selection, the histogram will not be plotted")
    return 
  if ref==0:
    ytitle = 'counts ' + ytitle
    h,bins,patches = axHist.hist(x[t],bins=nbins,range=xrange,histtype='step',alpha=0.5)
    if yrange==None: yrange = [0.0, numpy.max(h)]
    hw=h
  if ref>=22: 
    ytitle = (stp.getLabel(ref-1))[0] + ' ' + ytitle
    h,bins = numpy.histogram(x[t],range=xrange,bins=nbins)
    hw,bins,patches = axHist.hist(x[t],range=xrange, bins=nbins,histtype='step',alpha=0.5,weights=w[t])
    if yrange==None: yrange = [0.0, numpy.max(hw)]
  fwhm = None
  if calfwhm==1:
    fwhm, tf, ti = stp.calcFWHM(hw,bins[1]-bins[0])
    axHist.plot([bins[ti],bins[tf+1]],[max(h)*0.5,max(h)*0.5],'x-')
    print ("fwhm = %g" % fwhm)
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
    plt.show()
  
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


def plotxy_gnuplot(beam,col_h,col_v,execute=1,ps=0,pdf=0,title="",viewer='okular',**kwargs):
  """
  A plotxy implemented for gnuplot.
  It uses Shadow.beam.plotxy() for calculations.
  It creates files for gnuplot (plotxy.gpl and plotxy_*.dat)
  It can run gnuplot (system call) and display ps or pdf outputs

  :param beam: it can be a SHADOW binary file, an instance of Shadow.Beam() or a dictionary from Shadow.Beam.plotxy
  :param col_h: the H column for the plot. Irrelevant if beam is a dictionary
  :param col_v: the V column for the plot. Irrelevant if beam is a dictionary
  :param execute: set to 1 to make a system call to execute gnuplot (default=1)
  :param ps: set to 1 to get postscript output (irrelevant if pdf=1
  :param pdf: set to 1 for pdf output (prioritaire over ps)
  :param viewer: set to the ps or pdf viewer (default='okular')
  :param kwargs: keywords to be passed to Shadow.beam.plotxy()
  :return: the dictionary produced by Shadow.beam.plotxy with some keys added
  """
  if title == "":
      title = "plotxy"

  if isinstance(beam,dict):
    tkt = beam
    col_h = tkt["col_h"]
    col_v = tkt["col_v"]
  else:
    if isinstance(beam,str):
      beam1 = sd.Beam()
      beam1.load(beam)
      title += " - file: "+beam
      beam = beam1
    tkt = beam.plotxy(col_h,col_v,**kwargs)

  f = open("plotxy_histtop.dat",'w')
  for i in range(tkt["nbins_h"]):
      f.write("%12.5f  %12.5f \n"%( tkt["bin_h_left"][i], tkt["histogram_h"][i] ))
      f.write("%12.5f  %12.5f \n"%( tkt["bin_h_right"][i], tkt["histogram_h"][i] ))
  f.close()
  print("File written to disk: plotxy_histside.dat")

  f = open("plotxy_histside.dat",'w')
  for i in range(tkt["nbins_v"]):
      f.write("%12.5f  %12.5f \n"%( tkt["histogram_v"][i], tkt["bin_v_left"][i]  ))
      f.write("%12.5f  %12.5f \n"%( tkt["histogram_v"][i], tkt["bin_v_right"][i]  ))
  f.close()

  print("File written to disk: plotxy_histtop.dat")

  f = open("plotxy_grid.dat",'w')
  f.write(" # plotxy grid data for plotxy.gpl\n")
  f.write(" # Xbin Ybin Weight\n")
  for i in range(tkt["nbins_h"]):
    for j in range(tkt["nbins_v"]):
        f.write("%25.20f  %25.20f  %25.20f\n"%(tkt["bin_h_center"][i],tkt["bin_v_center"][j], tkt["histogram"][i,j] ))
    f.write("\n")
  f.close()
  print("File written to disk: plotxy_grid.dat")

  txt = """
    #GnuPlot command file for PLOTXY
    #Minimum version: gnuplot 4.2 patchlevel 6
    #
    {set_terminal}

    set multiplot

    #
    # top histogram
    #
    set lmargin screen 0.2125
    set rmargin screen 0.70
    set bmargin screen 0.75
    set tmargin screen 0.90
    unset xtics
    unset x2tics
    unset ytics
    unset y2tics
    unset key
    unset xlabel
    unset ylabel
    unset x2label
    unset y2label

    set x2tics mirror
    set x2label "  {title} "
     set xrange[  {xrange[0]} :  {xrange[1]} ]
     set yrange[*:*]
    plot "plotxy_histtop.dat" u 1:2 w lines lt -1 notitle


    #
    # side histogram
    #
    set lmargin screen 0.10
    set rmargin screen 0.2125
    set bmargin screen 0.10
    set tmargin screen 0.75
    unset xtics
    unset x2tics
    unset ytics
    unset y2tics
    unset key
    unset xlabel
    unset ylabel
    unset x2label
    unset y2label

    set ytics
    set ylabel "Column  {col_v}"

    set xrange[*:*]
    set yrange[  {yrange[0]} :  {yrange[1]} ]
    plot "plotxy_histside.dat" u (-$1):2 w lines lt -1 notitle

    #
    # scattered/contour plot
    #
    set lmargin screen 0.2125
    set rmargin screen 0.70
    set bmargin screen 0.10
    set tmargin screen 0.75
    unset xtics
    unset x2tics
    unset ytics
    unset y2tics
    unset key
    unset xlabel
    unset ylabel
    unset x2label
    unset y2label

    set xlabel "Column  {col_h}"

    set xrange[  {xrange[0]} :   {xrange[1]} ]
    set yrange[  {yrange[0]} :   {yrange[1]} ]
    #
    # IF PIXEL UNCOMMENT THIS
    #
    set pm3d map
    set palette gray
    splot "./plotxy_grid.dat" u 1:2:3 notitle

    #
    # info column
    #
    set obj 10 rect from graph 1.20, graph 1 to graph 1.61, graph 0
    set label "{label_id}"       at graph 1.21, graph 0.9
    set label "{label_good}"       at graph 1.21, graph 0.5
    set label "TOT  =    {nrays}"    at graph 1.21, graph 0.30
    set label "LOST =    {lost_rays}"    at graph 1.21, graph 0.25
    set label "GOOD =    {good_rays}"    at graph 1.21, graph 0.20
    set label "INTENS =  {intensity}" at graph 1.21, graph 0.15
    set label "{label_weight}"      at graph 1.21, graph 0.10
    replot

    unset multiplot

    {set_pause}

    """
  #add kws to dictionnary to be used in the template

  tkt["set_terminal"] = "set terminal x11 size 900,600"
  tkt["set_pause"] = "pause -1 'Press <Enter> to end graphic '"
  if ps:
      tkt["set_terminal"] = "set terminal postscript \n    set output 'plotxy.ps' "
      tkt["set_pause"] = ""
  if pdf:
      tkt["set_terminal"] = "set terminal pdf \n    set output 'plotxy.pdf' "
      tkt["set_pause"] = ""

  tkt["title"] = title
  tkt["lost_rays"] = tkt["nrays"] - tkt["good_rays"]
  tkt["label_id"] = os.getenv("USER")+"@"+os.getenv("HOST")
  if tkt["ref"] == 0:
      tkt["label_weight"] = "WEIGHT: RAYS"
  else:
      if tkt["ref"] == 1 or tkt["ref"] == 23:
        tkt["label_weight"] = "WEIGHT: INTENSITY"
      else:
        tkt["label_weight"] = "WEIGHT: COLUMN %d"%(tkt["ref"])

  if tkt["nolost"] == 0:
      tkt["label_good"] = "--ALL RAYS"
  elif tkt["nolost"] == 1:
      tkt["label_good"] = "--GOOD ONLY"
  else:
      tkt["label_good"] = "--ONLY LOSSES"

  txt2 = txt.format_map(tkt)

  f = open("plotxy.gpl",'w')
  f.write(txt2)
  f.close()

  print("File written to disk: plotxy.gpl")

  if execute:
      os.system("gnuplot plotxy.gpl")
      if ps:
          os.system(viewer+" plotxy.ps")
      if pdf:
        os.system(viewer+" plotxy.pdf")

  return tkt


def plotxy(beam,col_h,col_v, nofwhm=1, title="", **kwargs):
  """

  plotxy implementation using matplotlib.
  Calculations are done using Shadow.beam.plotxy()

  :param beam: it can be a SHADOW binary file, an instance of Shadow.Beam() or a dictionary from Shadow.Beam.plotxy
  :param col_h: The column for the H coordinate in the plot (irrelevant of beam is a dictionary)
  :param col_v: The column for the H coordinate in the plot (irrelevant of beam is a dictionary)
  :param nofwhm: set to 0 to label the FWHM value in the plot (default do not label)
  :param kwargs: keywrods passed to Shadow.Beam.plotxy
  :return: the dictionary returned by Shadow.beam.plotxy() with some added keys.
  """
  if title == "":
      title = "plotxy"

  if isinstance(beam,dict):
    tkt = beam
    col_h = tkt["col_h"]
    col_v = tkt["col_v"]
  else:
    if isinstance(beam,str):
      beam1 = sd.Beam()
      beam1.load(beam)
      title += " - file: "+beam
      beam = beam1
    tkt = beam.plotxy(col_h,col_v,**kwargs)


  xtitle = "Column %d"%tkt["col_h"]
  ytitle = "Column %d"%tkt["col_v"]

  figure = plt.figure(figsize=(12,8),dpi=96)

  ratio = 8.0/12.0

  rect_scatter = [0.10*ratio, 0.10, 0.65*ratio, 0.65]
  rect_histx =   [0.10*ratio, 0.77, 0.65*ratio, 0.20]
  rect_histy =   [0.77*ratio, 0.10, 0.20*ratio, 0.65]
  rect_text =    [1.00*ratio, 0.10, 1.20*ratio, 0.65]

  #
  #main plot
  #
  axScatter = figure.add_axes(rect_scatter)
  axScatter.set_xlabel(xtitle)
  axScatter.set_ylabel(ytitle)

  # axScatter.set_xlim(tkt["xrange"])
  # axScatter.set_ylim(tkt["yrange"])

  axScatter.axis(xmin=tkt["xrange"][0],xmax=tkt["xrange"][1])
  axScatter.axis(ymin=tkt["yrange"][0],ymax=tkt["yrange"][1])
  #axScatter.pcolor(tkt["bin_h_edges"], tkt["bin_v_edges"], tkt["histogram"].T)
  axScatter.pcolormesh(tkt["bin_h_edges"], tkt["bin_v_edges"], tkt["histogram"].T)

  for tt in axScatter.get_xticklabels():
    tt.set_size('x-small')
  for tt in axScatter.get_yticklabels():
    tt.set_size('x-small')

  #
  #histograms
  #
  axHistx = figure.add_axes(rect_histx, sharex=axScatter)
  axHisty = figure.add_axes(rect_histy, sharey=axScatter)

  #for practical purposes, writes the full histogram path
  tmp_h_b = []
  tmp_h_h = []
  for s,t,v in zip(tkt["bin_h_left"],tkt["bin_h_right"],tkt["histogram_h"]):
    tmp_h_b.append(s)
    tmp_h_h.append(v)
    tmp_h_b.append(t)
    tmp_h_h.append(v)
  tmp_v_b = []
  tmp_v_h = []
  for s,t,v in zip(tkt["bin_v_left"],tkt["bin_v_right"],tkt["histogram_v"]):
    tmp_v_b.append(s)
    tmp_v_h.append(v)
    tmp_v_b.append(t)
    tmp_v_h.append(v)

  axHistx.plot(tmp_h_b,tmp_h_h)
  axHisty.plot(tmp_v_h,tmp_v_b)

  for tl in axHistx.get_xticklabels(): tl.set_visible(False)
  for tl in axHisty.get_yticklabels(): tl.set_visible(False)
  for tt in axHisty.get_xticklabels():
    tt.set_rotation(270)
    tt.set_size('x-small')
  for tt in axHistx.get_yticklabels():
    tt.set_size('x-small')

  if tkt["fwhm_h"] != None:
      hh = 0.5*numpy.max(tkt["histogram_h"])
      lines = [ [ (tkt["fwhm_coordinates_h"][0],hh), \
                  (tkt["fwhm_coordinates_h"][1],hh) ]]
      lc = collections.LineCollection(lines,color='red',linewidths=2)
      axHistx.add_collection(lc)
      if nofwhm != 1:
          if tkt["fwhm_coordinates_h"][0] < 0:
              shift1 = 0.9
          else:
              shift1 = 1.0
          axHistx.annotate('FWHM=%f'%tkt["fwhm_h"], xy=(shift1*tkt["fwhm_coordinates_h"][0],1.01*hh))

  if tkt["fwhm_v"] != None:
      hh = 0.5*numpy.max(tkt["histogram_v"])
      lines = [ [ (hh,tkt["fwhm_coordinates_v"][0]), \
                  (hh,tkt["fwhm_coordinates_v"][1]) ]]
      lc = collections.LineCollection(lines,color='green',linewidths=2)
      axHisty.add_collection(lc)
      if nofwhm != 1:
          if tkt["fwhm_coordinates_v"][0] < 0:
              shift1 = 0.9
          else:
              shift1 = 1.0
          axHisty.annotate('FWHM=%f'%tkt["fwhm_v"], xy=(shift1*tkt["fwhm_coordinates_v"][0],1.01*hh))


  if title!=None:
    axHistx.set_title(title)
  axText = figure.add_axes(rect_text)
  if tkt["nolost"] == 0: axText.text(0.0,0.8,"ALL RAYS")
  if tkt["nolost"] == 1: axText.text(0.0,0.8,"GOOD RAYS")
  if tkt["nolost"] == 2: axText.text(0.0,0.8,"LOST RAYS")

  #tmps = "intensity: %f"%(tkt["intensity"])

  axText.text(0.0,0.7,"intensity: %8.2f"%(tkt["intensity"]))
  axText.text(0.0,0.6,"total number of rays: "+str(tkt["nrays"]))
  axText.text(0.0,0.5,"total good rays: "+str(tkt["good_rays"]))
  axText.text(0.0,0.4,"total lost rays: "+str(tkt["nrays"]-tkt["nrays"]))
  calfwhm = 1
  if tkt["fwhm_h"] != None:
    axText.text(0.0,0.3,"fwhm H: "+str(tkt["fwhm_h"]))
  if tkt["fwhm_v"] != None:
    axText.text(0.0,0.2,"fwhm V: "+str(tkt["fwhm_v"]))

  if isinstance(beam,str): axText.text(0.0,0.1,"FILE: "+beam)
  if isinstance(beam,sd.Beam): axText.text(0.0,0.1,"from Shadow.Beam instance")
  if tkt["ref"] == 0:
    axText.text(0.0,0.0,"WEIGHT: RAYS")
  else:
      axText.text(0.0,0.0,"WEIGHT: INTENSITY")
  axText.set_axis_off()
  plt.show()
  return tkt

#TODO: delete. Reimplemented using Shadow.Beam.plotxy()
def plotxy_old(beam,cols1,cols2,nbins=25,nbins_h=None,level=5,xrange=None,yrange=None,nolost=0,title='PLOTXY',xtitle=None,ytitle=None,noplot=0,calfwhm=0,contour=0):
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
  #plot_nicc.ioff()
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
    print ("no point selected")
    return None
  
  #figure = pylab.plt.figure(figsize=(12,8),dpi=96)
  figure = plt.figure(figsize=(12,8),dpi=96)

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
    print ("fwhm horizontal:  %g" % fwhmx)
    print ("fwhm vertical:    %g" % fwhmy)
  if calfwhm>=2:
    xx1 = binx[txi]
    xx2 = binx[txf+1]
    yy1 = biny[tyi]
    yy2 = biny[tyf+1]
    print ("limits horizontal: %g %g " % (binx[txi],binx[txf+1]))
    print ("limits vertical:   %g %g " % (biny[tyi],biny[tyf+1]))
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
      print ("Intensity in slit: %g ",intensityinslit)
    
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
  #pylab.plt.draw()
  plt.draw()
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

#
# waviness
#
def waviness_write(dic1,file="waviness.inp"):
    """
    dumps an input waviness file from python dictionary
    :param dic1: the input dictionary
    :param file: the file name
    :return: None
    """
    f = open(file,'w')
    f.write("%s"%(dic1["file"]))
    f.write("%d %d \n"%(dic1["npointx"],dic1["npointy"]) )
    f.write("%f %f \n"%(dic1["width"],dic1["xlength"]) )
    f.write("%d %f \n"%(dic1["nharmonics"],dic1["slp"]) )
    f.write("%d \n"%(dic1["iseed"]) )
    c = dic1["c"]
    y = dic1["y"]
    g = dic1["g"]
    for i in range(dic1["nharmonics"]+1): 
        f.write("%f  %f  %f \n"%(c[i],y[i],g[i]) )
    f.close()

def waviness_read(file="waviness.inp"):
    """
    reads a waviness input file into a python dictionary
    :param file: the file name
    :return: a dictionary
    """

    with open(file) as f:
        filedat = f.readline()
        #print("filedat= %s "%(filedat))

        npointx, npointy = [int(x) for x in f.readline().split()]
        #print("npointx= %d, npointy= %d"%(npointx,npointy))

        width, xlength = [float(x) for x in f.readline().split()]
        #print("width= %f, xlength= %f"%(width,xlength))


        nharmonics, slp = [float(x) for x in f.readline().split()]
        nharmonics = int(nharmonics)
        #print("nharmonics= %d, slp= %f"%(nharmonics,slp))

        iseed = int(f.readline())

        #print("iseed= %d"%(iseed))

        array1 = [[float(x) for x in line.split()] for line in f]
        array1 = numpy.array(array1)
        #print("array1.shape",array1.shape)

        return { "file":filedat, "npointx":npointx, "npointy":npointy, \
                 "width":width, "xlength":xlength, "nharmonics":nharmonics, \
                 "slp":slp, "iseed":iseed, \
                 "c":array1[:,0].copy(), "y":array1[:,1].copy(), "g":array1[:,2].copy() }


def slopes(z,x,y):
    """
    ;+
    ; NAME:
    ;	slopes
    ; PURPOSE:
    ;       This function calculates the slope errors of a surface along the mirror 
    ;       length y and mirror width x. 
    ; CATEGORY:
    ;	SHADOW tools
    ; CALLING SEQUENCE:
    ;	(slope,slopesrms) = slopes(z,x,y)
    ; INPUTS:
    ;	x: the width array of dimensions (Nx)
    ;	y: the length array of dimensions (Ny)
    ;	z: the surface array of dimensions (Nx,Ny)
    ; OUTPUTS:
    ;   slope: an array of dimension (2,Nx,Ny) with the slopes errors in rad
    ;            along y in out[0,:,:] and along Y in out[1,:,:]
    ;	slopesrms: a 4-dim array with 
    ;            [slopeErrorRMS_X_arcsec,slopeErrorRMS_Y_arcsec, 
    ;             slopeErrorRMS_X_urad,slopeErrorRMS_Y_urad]
    ;
    ; MODIFICATION HISTORY:
    ;       MSR 1994 written
    ;       08-04-15 srio@esrf.eu makes calculations in double precision.
    ;       2014-09-11 documented
    ;       2012-02-10 srio@esrf.eu python version
    ;-
    ;
    """

    nx = z.shape[0]
    ny = z.shape[1]

    slope = numpy.zeros((2,nx,ny))

    #; 
    #; slopes in x direction
    #;
    for i in range(nx-1):
        step = x[i+1] - x[i]
        slope[0,i,:] = numpy.arctan( (z[i+1,:] - z[i,:] ) / step )
    slope[0,nx-1,:] = slope[0,nx-2,:]

    #;
    #; slopes in y direction
    #; 
    for i in range(ny-1):
        step = y[i+1] - y[i]
        slope[1,:,i] = numpy.arctan( (z[:,i+1] - z[:,i] ) / step )
    slope[1,:,ny-1] = slope[1,:,ny-2]

    slopermsX = slope[0,:,:].std()
    slopermsY = slope[1,:,:].std()
    slopermsXsec = slopermsX*180.0/numpy.pi*3600.0
    slopermsYsec = slopermsY*180.0/numpy.pi*3600.0
    slopesrms = numpy.array([slopermsXsec,slopermsYsec, slopermsX*1e6,slopermsY*1e6])

    print('\n **** slopes: ****')
    print(' Slope error rms in X direction: %f arcsec'%(slopermsXsec))
    print('                               : %f urad'%(slopermsX*1e6))
    print(' Slope error rms in Y direction: %f arcsec'%(slopermsYsec))
    print('                               : %f urad'%(slopermsY*1e6))
    print(' *****************')

    return (slope,slopesrms)


def write_shadow_surface(s,xx,yy,outFile='presurface.dat'):
    """
      write_shadowSurface: writes a mesh in the SHADOW/presurface format
      SYNTAX: 
           out = write_shadowSurface(z,x,y,outFile=outFile)
      INPUTS:
           z - 2D array of heights
           x - 1D array of spatial coordinates along mirror width.
           y - 1D array of spatial coordinates along mirror length.
     
      OUTPUTS:
           out - 1=Success, 0=Failure
           outFile - output file in SHADOW format. If undefined, the
                     file is names "presurface.dat"
     
    """
    out = 1

    try:
       fs = open(outFile, 'w')
    except IOError:
       out = 0
       print ("Error: can\'t open file: "+outFile)
       return 
    else:
        # dimensions
        fs.write( repr(xx.size)+" "+repr(yy.size)+" \n" ) 
        # y array
        for i in range(yy.size): 
            fs.write(' ' + repr(yy[i]) )
        fs.write("\n")
        # for each x element, the x value and the corresponding z(y)
        # profile
        for i in range(xx.size): 
            tmps = ""
            for j in range(yy.size): 
                tmps = tmps + "  " + repr(s[j,i])
            fs.write(' ' + repr(xx[i]) + " " + tmps )
            fs.write("\n")
        fs.close()
        print ("write_shadow_surface: File for SHADOW "+outFile+" written to disk.")


def waviness_calc(file="waviness.dat",npointx=10,npointy=100,width=20.1,xlength=113.1,\
                  nharmonics=3,slp=0.9, iseed=2387427,\
                  c=[1.0,1.0,1.0,1.0],y=[0.0,0.0,0.0,0.0],g=[0.0,0.0,0.0,0.0]):

    """
    ;+
    ; NAME: 	WAVINESS_CALC
    ;
    ; PURPOSE:      This program generates a random error surface
    ;
    ;               The main method for slope errors (called hereafter waviness) is 
    ;               described in: 
    ;                  M. Sanchez del Rio and A. Marcelli,
    ;                  NIM-A319 (1992) 170-177
    ;                  http://dx.doi.org/10.1016/0168-9002(92)90550-N
    ; 
    ;               An alternative method (called hereafter harmonic_sum) consists
    ;               in a simple sum of harmonics:
    ;                 z = \sum_{n=1}^{N} {b_n cos( 2 \pi l/L + \psi_n) }
    ;
    ;                 (see e.g., Eq 11 in Shi et al, J Synchrotron Rad. 21 (2014)
    ;                  http://dx.doi.org/10.1107/S160057751400650X )
    ;
    ;               The switching of one or another method is done via the
    ;               sign of "Estimated Slope Error RMS [arcsec]" in the 
    ;               input interface, or slp in the input structure,
    ;               Set >0 for waviness, <0 for harmonic_sum
    ;
    ; CATEGORY:     SHADOW tools
    ;	
    ; CALLING SEQUENCE:
    ;	out = waviness_calc(input)
    ; INPUT KEYWORDS:
    ;                file: name of the output file
    ;                npointx: number of points along mirror width
    ;                npointy: number of points along mirror width
    ;                xlength:  the mirror length (apologies for the X)
    ;                width: is the mirror width
    ;                nharmonics: is n_max in Eq.8 (see reference)
    ;                slp: estimation of the target slope error RMS in arcsec
    ;                       for the default method (waviness). Must be >0
    ;                       For the other method (see later) it must be negative,
    ;                iseed: seed to initializa random generator
    ;                c: an array of coeff C_n in Eq. 9. 
    ;                     Only the first N+1 indices are read.
    ;                y: an array of coeff y_n^0 in Eq. 8 normalized.
    ;                     This is the initial constant shift. 
    ;                     One means the profile can be shifted to any position 
    ;                     along the mirror length. Zero means no initial shift.
    ;                g: an array of coeff g_n (random shift) in Eq. 8 normalized.
    ;                       
    ;
    ;                For the alternative method (harmonic_sum): 
    ;
    ;                   To use this method, slp must be set negative. 
    ;                   In this method the profile is:
    ;                   z(y) = \sum_{n=1}^{N}
    ;                    { b_0 n^{g_n} cos( 2 \pi (0.5+(y/L) + r_n y_g ) }
    ;                   or in other words, a sum of N harmonics with a power
    ;                   law of exponent g_n.
    ;
    ;                   y is the coordinate along the mirror, with zero in the 
    ;                     mirror center.
    ;                   b_0 is a constant equal to abs(SLP) 
    ;                   g_n is the exponent of n stored in g for each harmonic
    ;                   L is the mirror length xlength
    ;                   r_n is a random value in [0,1) for each harmonic
    ;                   y_g is a normalized interval to apply the random shift
    ;                     stored in Y. 0 means no random shift, 1 means shift in 
    ;                     the full mirror length.
    ;                   Note that in this method c[0],y[0] and g[0] are not used
    ;                     as summation index starts with one.
    ; 
    ;	OUTPUT: 
    ;                   the surface x, y, z
    ;
    ; MODIFICATION HISTORY:
    ;			June 1991	first version
    ;			September 1991	update 
    ;			October 1991	moved to Sun
    ;			September 1992	distribution version
    ;			February 2006	translated to IDL (srio@esrf.fr)
    ;			2014-09-09 srio@esrf.eu dimensionated to 5000pts.
    ;                                added new method (SLP<0). Added doc.
    ;                   2015-02-10 srio@esrf.eu python version
    ;
    ;-
    """
    method = 0
    if (slp < 0):
        method = 1
        print('waviness_calc: Using method: harmonic sum')
    else:
        print('waviness_calc: Using standard method')

    zz=numpy.zeros((npointx,npointy))
    # ;c
    # ;c  input
    # ;c
    yfact = 1.0
    slp = slp/3600.0 * numpy.pi / 180.0   # pass to rads
    xfact = xlength / numpy.pi

    if (method == 0):
        xntot = 0.0
        coe = numpy.zeros(nharmonics+1)
        shi = numpy.zeros(nharmonics+1)
        shin = numpy.zeros(nharmonics+1)
        ranc = numpy.zeros(nharmonics+1)

        xx = numpy.zeros(npointx)

        for i in range(nharmonics+1):
            a1 = c[i]
            a2 = y[i]
            a3 = g[i]
            xntot = xntot + a1
            #; eq 9 in ref.
            amp = slp * xlength / (0.5 + float(i) ) / numpy.sqrt(2.) / numpy.pi
            coe[i] = a1 * amp
            # 
            shin[i] = a2
            ranc[i] = a3 

        for i in range(nharmonics+1):
            coe[i] = coe[i] / numpy.sqrt(xntot)   # for a single harmonic

        #         ;c
        #         ;c	begin calculations
        #         ;c
        stp = numpy.pi/(npointy-1)
        stpx = width/(npointx-1)
        stpy = xlength/(npointy-1)


        for k in range(npointx):
            xx[k] = -(width / 2.0) + stpx * k
            for j in range(nharmonics+1):
                tmp = numpy.random.rand()
                shi[j] = shin[j] * numpy.pi + tmp * numpy.pi * ranc[j]

            y = numpy.zeros(npointy)
            for j in range(nharmonics+1):
                x = -numpy.pi / 2.0 + numpy.arange(npointy) * stp
                nn = float(j)
                ynew = coe[j]*( (-1.0)**nn * numpy.cos((2.0 * nn + 1.0) * (x + shi[j])))
                y = ynew + y

            x = x * xfact
            yy = x
            y = y * yfact
            zz[k,:] = y
    else:
        xx = numpy.linspace(-width/2.,width/2.,npointx)
        yy = numpy.linspace(-xlength/2.,xlength/2.,npointy)
        for k in range(npointx):
            zz1 = yy*0.0
            for j in range(1,nharmonics+1):
                arg = 2.0 * numpy.pi * j * (0.5 + yy/xlength)
                ran1 = 2.0 * numpy.pi * numpy.random.rand() * y[j]
                zz1 = zz1 + c[j]*float(j)**(g[j])*numpy.cos(arg+ran1)
            zz[k,:] = zz1 * numpy.abs(slp) # global multiplicative factor slp

    return (xx,yy,zz)


if __name__=="__main__":


  #
  #test waviness
  #
  do_waviness = 0
  if do_waviness:
      myfile = "waviness.inp"
      try:
          f = open(myfile)
          f.close()
          tmp = waviness_read(file=myfile)
    
          (x,y,z) = waviness_calc(\
                        file=tmp["file"], npointx=tmp["npointx"], npointy=tmp["npointy"],\
                        width=tmp["width"],xlength=tmp["xlength"],\
                        nharmonics=tmp["nharmonics"],slp=tmp["slp"], iseed=tmp["iseed"],\
                        c=tmp["c"],y=tmp["y"], g=tmp["g"])
          
          waviness_write(tmp,file="tmp.inp")
      except:
          (x,y,z) = waviness_calc()
    
      write_shadow_surface(z.T,x,y,outFile='waviness.dat')
      slopes(z,x,y)
 
  #
  #test new plots
  #
  do_histo1 = 0
  if do_histo1:
      #t = histo1_old("begin.dat",3,nbins=103, xrange=[-0.0015,0.0015])
      t = histo1("begin.dat",3,bar=1, nbins=103,nofwhm=1, ref=1, xrange=[-0.0015,0.0015])

  do_plotxy = 0
  if do_plotxy:
      nbins = 110

      # t = histo1_old("begin.dat",3,nbins=103, xrange=[-0.0015,0.0015])

      # import Shadow
      # tmp = Shadow.Beam()
      # tmp.load("begin.dat")
      # tkt2 = tmp.plotxy(1,3,nbins=nbins, nolost=0, ref=1, \
      #                      xrange=[-0.015,0.015], yrange=[-0.002,0.002])
      # plotxy_gnuplot(tkt2,0,0,execute=1)
      # plotxy(tkt2,0,0)


      tkt = plotxy_gnuplot("begin.dat",1,3,nbins=nbins, nolost=1, ref=0, \
                     xrange=[-0.015,0.015], yrange=[-0.002,0.002],execute=0,pdf=1)


      # tkt = plotxy("begin.dat",1,3,nbins=nbins, nolost=1, \
      #                xrange=[-0.015,0.015], yrange=[-0.002,0.002])
