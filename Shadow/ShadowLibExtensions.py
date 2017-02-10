#
#
# This file contains the new classes programmed in python  added to the
# main objects (Beam, OE and Source) defined in C (in ShadowLib)
#
# It also define GeometricSource and Beamline
#

#TODO: add user units to all distance values

import Shadow.ShadowLib as ShadowLib
import numpy
import inspect
import copy


class Beam(ShadowLib.Beam):
  def __init__(self, N=None):
    ShadowLib.Beam.__init__(self)
    if N is not None:
      self.SetRayZeros(N)

  def duplicate(self):
      beam_copy = Beam()
      beam_copy.rays = copy.deepcopy(self.rays)
      return beam_copy

  def retrace(self,dist):
    try:
      tof = (-self.rays[:,1].flatten() + dist)/self.rays[:,4].flatten()
      self.rays[:,0] += tof*self.rays[:,3].flatten()
      self.rays[:,1] += tof*self.rays[:,4].flatten()
      self.rays[:,2] += tof*self.rays[:,5].flatten()
    except AttributeError:
      print ('retrace: No rays')

  def traceCompoundOE(self,compoundOE,from_oe=1,write_start_files=0,write_end_files=0,\
                      write_star_files=0, write_mirr_files=0):
      """
      traces a compound optical element

      Note that when using write_*_files keyword, the files are written by python, not
                  by SHADOW (so FWRITE is not changed), with the exception of write_mirr_files. In this case
                  the code changes in the oe copy FWRITE=1 (mirror files only). This affects the returned
                  list of oe's after tracing.

      :param compoundOE: input object
      :param from_oe: index of the first oe (for tracing compoundOE after an existing system) (default=1)
      :param write_start_files: 0=No (default), 1=Yes (all), 2: only first and last ones
      :param write_end_files:  0=No (default), 1=Yes (all), 2: only first and last ones
      :param write_star_files:  0=No (default), 1=Yes (all), 2: only first and last ones
      :param write_mirr_files:  0=No (default), 1=Yes (all), 2: only first and last ones
      :return: a new compoundOE with the list of the OE objects after tracing (the info of end.xx files)
      """
      # oe_index = from_oe
      # oe_n = compoundOE.number_oe()
      # list = CompoundOE()
      #
      # for i,oe in enumerate(compoundOE.list):
      #   print("\nTracing compound oe %d from %d. Absolute oe number is: %d"%(i+1,oe_n,oe_index))
      #
      #   print(">>>>>>>>> FILE_SOURCE before 0",len(oe.FILE_SOURCE))
      #   oe1 = oe.duplicate()
      #   print(">>>>>>>>> FILE_SOURCE before 1",len(oe1.FILE_SOURCE))
      #   iwrite = 0
      #   if write_mirr_files == 1: iwrite = 1
      #   if write_mirr_files == 2 and i == 0: iwrite = 1
      #   if write_mirr_files == 2 and i == oe_n-1: iwrite = 1
      #   if iwrite:
      #       oe1.FWRITE = 1
      #   iwrite = 0
      #   if write_start_files == 1: iwrite = 1
      #   if write_start_files == 2 and i == 0: iwrite = 1
      #   if write_start_files == 2 and i == oe_n-1: iwrite = 1
      #
      #   if iwrite:
      #       #TODO: check possible bug: the length of FILE_SOURCE is changed when writing start file
      #       print(">>>>>>>> TRACE BEFORE WRITE  <<<<<< type(oe1.FILE_SOURCE)",i,(oe1.FILE_SOURCE),len(oe1.FILE_SOURCE))
      #       #tmp = oe1.duplicate()
      #       oe1.write("start.%02d"%(oe_index))
      #       print(">>>>>>>> TRACE AFTER WRITE  <<<<<< type(oe1.FILE_SOURCE)",i,(oe1.FILE_SOURCE),len(oe1.FILE_SOURCE))
      #
      #       print("File written to disk: start.%02d"%(oe_index))
      #
      #   print(">>>>>>>>> FILE_SOURCE before",len(oe1.FILE_SOURCE),len(oe1.FILE_REFL))
      #   #tmp = oe1.duplicate()
      #   self.traceOE(oe1,oe_index)
      #   print(">>>>>>>>> FILE_SOURCE after",len(oe1.FILE_SOURCE),len(oe1.FILE_REFL))
      #
      #   list.append(oe1)
      #
      #   iwrite = 0
      #   if write_star_files == 1: iwrite = 1
      #   if write_star_files == 2 and i == 0: iwrite = 1
      #   if write_star_files == 2 and i == oe_n-1: iwrite = 1
      #   if iwrite == 1:
      #       self.write("star.%02d"%(oe_index))
      #       print("File written to disk: star.%02d"%(oe_index))
      #
      #   iwrite = 0
      #   if write_end_files == 1: iwrite = 1
      #   if write_end_files == 2 and i == 0: iwrite = 1
      #   if write_end_files == 2 and i == oe_n-1: iwrite = 1
      #   if write_end_files == 1:
      #       oe1.write("end.%02d"%(oe_index))
      #       print("File written to disk: end.%02d"%(oe_index))
      #
      #   oe_index += 1
      #
      # return list

      oe_n = len(compoundOE.list)
      for i in range(oe_n):
          print("\nTracing compound oe %d from %d. Absolute oe number is: %d"%(i+1,oe_n,from_oe+i))

          #if wanted to write mirr.xx, tell SHADOW to do it
          if write_mirr_files == 1:
            compoundOE.list[i].FWRITE = 1
          if write_mirr_files == 2:
            if i == 0 or i == oe_n-1:
                compoundOE.list[i].FWRITE = 1
          #dump start.xx files, if selected
          if write_start_files == 1:
            compoundOE.list[i].write("start.%02d"%(from_oe+i))
          if write_start_files == 2:
            if i == 0 or i == oe_n-1:
                compoundOE.list[i].write("start.%02d"%(from_oe+i))



          self.traceOE( compoundOE.list[i], from_oe+1)

          #dump star.xx files, if selected
          if write_star_files == 1:
            self.write("star.%02d"%(from_oe+i))
          if write_star_files == 2:
            if i == 0 or i == oe_n-1:
                self.write("star.%02d"%(from_oe+i))
          #dump end.xx files, of selected
          if write_end_files == 1:
            compoundOE.list[i].write("end.%02d"%(from_oe+i))
          if write_end_files == 2:
            if i == 0 or i == oe_n-1:
                compoundOE.list[i].write("end.%02d"%(from_oe+i))

      return

  def get_standard_deviation(self,col, nolost=1, ref=0):
      '''
      returns the standard deviation of one viariable in the beam
      :param col: variable (shadow column number)
      :param nolost: 0 = use all rays, 1=good only, 2= lost only
      :param ref: 0 = no weight, 1=weight with intensity (col23)
      :return:
      '''
      x = self.getshonecol(col=col,nolost=nolost)
      if ref == 0:
          return x.std()
      else:
          w = self.getshonecol(23,nolost=nolost)
          average = numpy.average(x, weights=w)
          variance = numpy.average( (x-average)**2, weights=w)
          return(numpy.sqrt(variance))


  #added srio 2015

  def getshonecol(self,col, nolost=0):
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
            14   Phase (s-polarization) in rad
            15   Phase (p-polarization) in rad
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

    #A2EV = 50676.89919462
    codata_h = numpy.array(6.62606957e-34)
    codata_ec = numpy.array(1.602176565e-19)
    codata_c = numpy.array(299792458.0)
    A2EV = 2.0*numpy.pi/(codata_h*codata_c/codata_ec*1e2)

    col=col-1
    ray = self.rays

    column = None

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
        column =  E2s-E2p
    if col==31:
        E2s = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
        E2p = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
        Cos = numpy.cos(ray[:,13]-ray[:,14])
        column =  2*numpy.sqrt(E2s*E2p)*Cos
    if col==32:
        E2s = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [6,7,8] ]),axis=0)
        E2p = numpy.sum(numpy.array([ ray[:,i]*ray[:,i] for i in [15,16,17] ]),axis=0)
        Sin = numpy.sin(ray[:,13]-ray[:,14])
        column =  2*numpy.sqrt(E2s*E2p)*Sin

    if nolost == 0:
        return column.copy()

    if nolost == 1:
        f  = numpy.where(ray[:,9] > 0.0)
        if len(f[0])==0:
            print ('getshonecol: no GOOD rays, returning empty array')
            return numpy.empty(0)
        return column[f].copy()

    if nolost == 2:
        f  = numpy.where(ray[:,9] < 0.0)
        if len(f[0])==0:
            print ('getshonecol: no BAD rays, returning empty array')
            return numpy.empty(0)
        return column[f].copy()

    return None

  def getshcol(self,col,nolost=0):
      '''
      Extract multiple columns from a shadow file (eg.'begin.dat') or a Shadow.Beam instance.
      The column are numbered in the fortran convention, i.e. starting from 1.
      It returns a numpy.array filled with the values of the chosen column.

      Inputs:
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
              14   Phase (s-polarization) in rad
              15   Phase (p-polarization) in rad
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
      ret = []
      if isinstance(col, int): return self.getshonecol(col,nolost=nolost)
      for c in col:
        ret.append(self.getshonecol(c,nolost=nolost))
      return tuple(ret)

  def intensity(self,nolost=0):
      w = self.getshonecol(23,nolost=nolost)
      return w.sum()

  def nrays(self,nolost=0):
      try:
        w = self.getshonecol(10)
      except Exception:
          print("Error: Empty beam...")
          return 0

      if nolost == 0:
          return w.size
      if nolost == 1:
          return numpy.array(numpy.where(w >= 0)).size
      if nolost == 2:
          return numpy.array(numpy.where(w < 0)).size


  def histo1(self,col,xrange=None,nbins=50,nolost=0,ref=0,write=None,factor=1.0,calculate_widths=1):
      """
      Calculate the histogram of a column, simply counting the rays, or weighting with another column.
      It returns a dictionary which contains the histogram data.

      :param col: int for the chosen column.
      :param xrange:  tuple or list of length 2 describing the interval of interest for x, the data read from the chosen column.
                    (default: None, thus using min and max of the array)
      :param nbins:  number of bins of the histogram.
      :param nolost:
               0   All rays
               1   Only good rays
               2   Only lost rays
      :param ref:
               0, None, "no", "NO" or "No":   only count the rays
               23, "Yes", "YES" or "yes":     weight with intensity (look at col=23 |E|^2 total intensity)
               other value: use that column as weight
      :param write:
               None (default)   don't write any file
               file_name   write the histogram into the file 'file_name'.
      :param factor:  a scalar factor to multiply the selected column before histogramming
                    (e.g., for changing scale from cm to um then factor=1e4).
      :param calculate_widths:
      :return:         a python dictionary with the calculated histogram. The following keys are set:
               error, col, write, nolost, nbins, xrange, factor
               histogram, bins, histogram_sigma, bin_center, bin_left, bin_right,
               intensity, fwhm, nrays, good_rays,



      Memorandum: Possible choice for col are:
               1   X spatial coordinate [user's unit]
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
              14   Phase (s-polarization) in rad
              15   Phase (p-polarization) in rad
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
      """



      '''


      Inputs:
         beam     : str instance with the name of the shadow file to be loaded, or a Shadow.Beam initialized instance.
         col      :

      Optional keywords:
         xrange   :
         nbins    :
         nolost   :
         ref      :
         write    :
         factor   :

      Outputs:



      Error:
         if an error occurs an ArgsError is raised.


      '''
      #initialize return value
      ticket = {'error':1}

      #coli = col - 1

      if ref == None: ref = 0
      if ref == "No": ref = 0
      if ref == "NO": ref = 0
      if ref == "no": ref = 0

      if ref == "Yes": ref = 23
      if ref == "YES": ref = 23
      if ref == "yes": ref = 23

      if ref == 1:
          print("Shadow.Beam.histo1: Warning: weighting with column 1 (X) [not with intensity as may happen in old versions]")

      # copy the inputs
      ticket['col'] = col
      ticket['write'] = write
      ticket['nolost'] = nolost
      ticket['nbins'] = nbins
      ticket['xrange'] = xrange

      ticket['factor'] = factor
      ticket['ref'] = ref
      
      if ref==0:
        x = self.getshonecol(col, nolost=nolost)
        w = numpy.ones(len(x))
      else:
        x, w = self.getshcol((col,ref),nolost=nolost)

      if factor != 1.0: x *= factor


      if xrange == None:
          xrange = [x.min(), x.max() ]

      h,bins = numpy.histogram(x,bins=nbins,range=xrange,weights=w)
      #evaluate the histogram with squares of the weight for error calculations
      h2,bins2 = numpy.histogram(x,bins=nbins,range=xrange,weights=(w*w))
  
      #Evaluation of histogram error.
      # See Pag 17 in Salvat, Fernandez-Varea and Sempau
      # Penelope, A Code System for Monte Carlo Simulation of
      # Electron and Photon Transport, AEN NEA  (2003)
      #
      # See James, Rep. Prog. Phys., Vol 43 (1980) pp 1145-1189 (special attention to pag. 1184)
      h_sigma = numpy.sqrt( h2 - h*h/float(len(w)) )

      if write != None and write != "":
          f = open(write,'w')
          f.write('#F %s \n'%(write))
          f.write('#C This file has been created using Shadow.Beam.histo1() \n')
          f.write('#C COLUMN 1 CORRESPONDS TO ABSCISSAS IN THE CENTER OF EACH BIN\n')
          f.write('#C COLUMN 2 CORRESPONDS TO ABSCISSAS IN THE THE LEFT CORNER OF THE BIN\n')
          f.write('#C COLUMN 3 CORRESPONDS TO INTENSITY\n')
          f.write('#C COLUMN 4 CORRESPONDS TO ERROR: SIGMA_INTENSITY\n')
          f.write('#C col = %d\n'%(col))
          f.write('#C nolost = %d\n'%(nolost))
          f.write('#C nbins = %d\n'%(nbins))
          f.write('#C ref = %d\n'%(ref),)
          f.write(' \n')
          f.write('#S 1 histogram\n')
          f.write('#N 4\n')
          f.write('#L X1  X2  Y  YERR\n')
          for i in range(len(h)):
            f.write('%f\t%f\t%f\t%f\n' % ( (bins[i]+bins[i+1])*0.5, bins[i], h[i], h_sigma[i] ))
          f.close()
          print('histo1: file written to disk: %s'%(write))



      #
      # output
      ticket['error'] = 0
      ticket['histogram'] = h
      ticket['bins'] = bins
      ticket['histogram_sigma'] = h_sigma
      bin_center = bins[:-1]+(bins[1]-bins[0])*0.5
      ticket['bin_center'] = bin_center
      ticket['bin_left'] = bins[:-1]
      ticket['bin_right'] = bins[:-1]+(bins[1]-bins[0])
      ticket['xrange'] = xrange
      ticket['intensity'] = self.intensity(nolost=nolost)
      ticket['fwhm'] = None
      ticket['nrays'] = self.nrays(nolost=0)
      ticket['good_rays'] = self.nrays(nolost=1)

      #for practical purposes, writes the points the will define the histogram area
      tmp_b = []
      tmp_h = []
      for s,t,v in zip(ticket["bin_left"],ticket["bin_right"],ticket["histogram"]):
        tmp_b.append(s)
        tmp_h.append(v)
        tmp_b.append(t)
        tmp_h.append(v)
      ticket['histogram_path'] = numpy.array(tmp_h)
      ticket['bin_path'] = numpy.array(tmp_b)

      if calculate_widths > 0:
          #CALCULATE fwhm
          tt = numpy.where(h>=max(h)*0.5)
          if h[tt].size > 1:
              binSize = bins[1]-bins[0]
              ticket['fwhm'] = binSize*(tt[0][-1]-tt[0][0])
              ticket['fwhm_coordinates'] = (bin_center[tt[0][0]],bin_center[tt[0][-1]])


      if calculate_widths == 2:
            # CALCULATE FW at 25% HEIGHT
            tt = numpy.where(h>=max(h)*0.25)
            if h[tt].size > 1:
                binSize = bins[1]-bins[0]
                ticket['fw25%m'] = binSize*(tt[0][-1]-tt[0][0])
            else:
                ticket["fw25%m"] = None

            # CALCULATE FW at 75% HEIGHT
            tt = numpy.where(h>=max(h)*0.75)
            if h[tt].size > 1:
                binSize = bins[1]-bins[0]
                ticket['fw75%m'] = binSize*(tt[0][-1]-tt[0][0])
            else:
                ticket["fw75%m"] = None

      return ticket

  def get_good_range(self,icol, nolost=0):
    """

    :param icol: the column number (SHADOW convention, starting from 1)
    :param nolost: lost rays flag (0=all, 1=good, 2=losses)
    :return: [rmin,rmax] the selected range
    """
    col = self.getshonecol(icol,nolost=nolost)
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



  def histo2(self,col_h,col_v,nbins=25,ref=23, nbins_h=None, nbins_v=None, nolost=0,xrange=None,yrange=None,
             calculate_widths=1):
    """

    performs 2d histogram to prepare data for a plotxy plot

    It uses histogram2d for calculations

    Note that this Shadow.Beam.histo2 was previously called Shadow.Beam.plotxy

    :param col_h: the horizontal column
    :param col_v: the vertical column
    :param nbins: number of bins
    :param ref      :
               0, None, "no", "NO" or "No":   only count the rays
               23, "Yes", "YES" or "yes":     weight with intensity (look at col=23 |E|^2 total intensity)
               other value: use that column as weight
    :param nbins_h: number of bins in H
    :param nbins_v: number of bins in V
    :param nolost: 0 or None: all rays, 1=good rays, 2=only losses
    :param xrange: range for H
    :param yrange: range for V
    :param calculate_widths: 0=No, 1=calculate FWHM (default), 2=Calculate FWHM and FW at 25% and 75% if Maximum
    :return: a dictionary with all data needed for plot
    """


    ticket = {'error':1}


    if ref == None: ref = 0
    if ref == "No": ref = 0
    if ref == "NO": ref = 0
    if ref == "no": ref = 0

    if ref == "Yes": ref = 23
    if ref == "YES": ref = 23
    if ref == "yes": ref = 23

    if ref == 1:
          print("Shadow.Beam.histo2: Warning: weighting with column 1 (X) [not with intensity as may happen in old versions]")

    if nbins_h == None: nbins_h = nbins
    if nbins_v == None: nbins_v = nbins

    # copy the inputs
    ticket['col_h'] = col_h
    ticket['col_v'] = col_v
    ticket['nolost'] = nolost
    ticket['nbins_h'] = nbins_h
    ticket['nbins_v'] = nbins_v
    ticket['ref'] = ref

    (col1,col2) = self.getshcol((col_h,col_v),nolost=nolost)

    if xrange==None: xrange = self.get_good_range(col_h,nolost=nolost)
    if yrange==None: yrange = self.get_good_range(col_v,nolost=nolost)

    if ref == 0:
        weights = col1*0+1
    else:
        weights = self.getshonecol(ref,nolost=nolost)

    (hh,xx,yy) = numpy.histogram2d(col1, col2, bins=[nbins_h,nbins_v], range=[xrange,yrange], normed=False, weights=weights)

    ticket['xrange'] = xrange
    ticket['yrange'] = yrange
    ticket['bin_h_edges'] = xx
    ticket['bin_v_edges'] = yy
    ticket['bin_h_left'] = numpy.delete(xx,-1)
    ticket['bin_v_left'] = numpy.delete(yy,-1)
    ticket['bin_h_right'] = numpy.delete(xx,0)
    ticket['bin_v_right'] = numpy.delete(yy,0)
    ticket['bin_h_center'] = 0.5*(ticket['bin_h_left']+ticket['bin_h_right'])
    ticket['bin_v_center'] = 0.5*(ticket['bin_v_left']+ticket['bin_v_right'])
    ticket['histogram'] = hh
    ticket['histogram_h'] = hh.sum(axis=1)
    ticket['histogram_v'] = hh.sum(axis=0)
    ticket['intensity'] = self.intensity(nolost=nolost)
    ticket['nrays'] = self.nrays(nolost=0)
    ticket['good_rays'] = self.nrays(nolost=1)


    # CALCULATE fwhm

    if calculate_widths > 0:
        h = ticket['histogram_h']
        tt = numpy.where(h>=max(h)*0.5)
        if h[tt].size > 1:
            binSize = ticket['bin_h_center'][1]-ticket['bin_h_center'][0]
            ticket['fwhm_h'] = binSize*(tt[0][-1]-tt[0][0])
            ticket['fwhm_coordinates_h'] = (ticket['bin_h_center'][tt[0][0]],ticket['bin_h_center'][tt[0][-1]])
        else:
            ticket["fwhm_h"] = None

        h = ticket['histogram_v']
        tt = numpy.where(h>=max(h)*0.5)
        if h[tt].size > 1:
            binSize = ticket['bin_v_center'][1]-ticket['bin_v_center'][0]
            ticket['fwhm_v'] = binSize*(tt[0][-1]-tt[0][0])
            ticket['fwhm_coordinates_v'] = (ticket['bin_v_center'][tt[0][0]],ticket['bin_v_center'][tt[0][-1]])
        else:
            ticket["fwhm_v"] = None


    if calculate_widths == 2:
        # CALCULATE FW at 25% HEIGHT
        h = ticket['histogram_h']
        tt = numpy.where(h>=max(h)*0.25)
        if h[tt].size > 1:
            binSize = ticket['bin_h_center'][1]-ticket['bin_h_center'][0]
            ticket['fw25%m_h'] = binSize*(tt[0][-1]-tt[0][0])
        else:
            ticket["fw25%m_h"] = None

        h = ticket['histogram_v']
        tt = numpy.where(h>=max(h)*0.25)
        if h[tt].size > 1:
            binSize = ticket['bin_v_center'][1]-ticket['bin_v_center'][0]
            ticket['fw25%m_v'] = binSize*(tt[0][-1]-tt[0][0])
        else:
            ticket["fw25%m_v"] = None

        # CALCULATE FW at 75% HEIGHT
        h = ticket['histogram_h']
        tt = numpy.where(h>=max(h)*0.75)
        if h[tt].size > 1:
            binSize = ticket['bin_h_center'][1]-ticket['bin_h_center'][0]
            ticket['fw75%m_h'] = binSize*(tt[0][-1]-tt[0][0])
        else:
            ticket["fw75%m_h"] = None

        h = ticket['histogram_v']
        tt = numpy.where(h>=max(h)*0.75)
        if h[tt].size > 1:
            binSize = ticket['bin_v_center'][1]-ticket['bin_v_center'][0]
            ticket['fw75%m_v'] = binSize*(tt[0][-1]-tt[0][0])
        else:
            ticket["fw75%m_v"] = None

    return ticket

  def plotxy(self,*args, **kwargs):
    print("Deprecated use of Shadow.plotxy(): Use Shadow.histo2()")
    ticket = self.histo2(*args,**kwargs)
    return(ticket)

class OE(ShadowLib.OE):
  def __init__(self):
    ShadowLib.OE.__init__(self)

  # here methods to initialize the OE

  # renamed setScreens -> set_screens srio@esrf.eu
  def set_screens(self,
                 n_screen=1, 
                 i_screen=numpy.zeros(10),
                 i_abs=numpy.zeros(10),
                 sl_dis=numpy.zeros(10),
                 i_slit=numpy.zeros(10),
                 i_stop=numpy.zeros(10),
                 k_slit=numpy.zeros(10),
                 thick=numpy.zeros(10),
                 file_abs=numpy.array(['', '', '', '', '', '', '', '', '', '']),
                 rx_slit=numpy.zeros(10),
                 rz_slit=numpy.zeros(10),
                 cx_slit=numpy.zeros(10),
                 cz_slit=numpy.zeros(10),
                 file_scr_ext=numpy.array(['', '', '', '', '', '', '', '', '', ''])
                 ):
    self.F_SCREEN = 1
    if n_screen<=10 and n_screen>0: 
      self.N_SCREEN = n_screen 
    else:
      print ('Shadow cannot handle more then 10 screens')
      return
    self.I_SCREEN     = i_screen
    self.I_ABS        = i_abs
    self.SL_DIS       = sl_dis
    self.I_SLIT       = i_slit
    self.I_STOP       = i_stop
    self.K_SLIT       = k_slit
    self.THICK        = thick
    self.FILE_ABS     = file_abs
    self.RX_SLIT      = rx_slit
    self.RZ_SLIT      = rz_slit
    self.CX_SLIT      = cx_slit
    self.CZ_SLIT      = cz_slit
    self.FILE_SCR_EXT = file_scr_ext

    return self

  def set_empty(self,T_INCIDENCE=0,T_REFLECTION=180.0,T_SOURCE=0.0,T_IMAGE=0.0,ALPHA=0.0):
    """
    Defines an empty optical element (useful as an arm, e.g. to rotate reference frames)
    By default, there is no change in the optical axis direction.

    :param T_INCIDENCE: incidence angle (default=0)
    :param T_REFLECTION: reflection angle (default=180)
    :param T_SOURCE: distance from previous o.e. (default=0)
    :param T_IMAGE: ddistance to next or (default=0)
    :param ALPHA: mirror oriantation angle (default=0)
    :return:
    """
    self.F_REFRAC = 2
    self.T_INCIDENCE = T_INCIDENCE
    self.T_REFLECTION = T_REFLECTION
    self.T_SOURCE = T_SOURCE
    self.T_IMAGE = T_IMAGE
    self.ALPHA = ALPHA
    return self

  # #
  # # TODO: REMOVE START HERE
  # #
  # def setOutput(self,fwrite=0):
  #   self.FWRITE = fwrite
  #   return self
  #
  # def setFrameOfReference(self,source_distance=10.0,image_distance=20.0,source_angle=10.0,image_angle=10.0,alpha=0.0):
  #   self.T_SOURCE     = source_distance
  #   self.T_IMAGE      = image_distance
  #   self.T_INCIDENCE  = source_angle
  #   self.T_REFLECTION = image_angle
  #   self.ALPHA        = alpha
  #   return self
  #
  # def setSeed(self,istar=12345701):
  #   self.ISTAR1       = istar
  #   return self
  #
  # def setConvex(self):
  #   self.F_CONVEX = 1
  #   return self
  #
  # def setConcave(self):
  #   self.F_CONVEX = 0
  #   return self
  #
  # def setCylindric(self,cyl_ang=0.0):
  #   self.FCYL = 1
  #   self.CIL_ANG = cyl_ang
  #   return self
  #
  # def unsetCylinder(self):
  #   self.FCYL = 0
  #   return self
  #
  # def setAutoFocus(self,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
  #   self.F_EXT = 0
  #   self.F_DEFAULT = f_default
  #   if f_default==0:
  #     self.SSOUR = ssour
  #     self.SIMAG = simag
  #     self.THETA = theta
  #   return self
  #
  # def unsetReflectivity(self):
  #   self.F_REFLEC = 0
  #   return self
  #
  # def setReflectivityFull(self,f_refl=0,file_refl='GAAS.SHA',rparams=numpy.zeros(2,dtype=numpy.float64),f_thick=0):
  #   self.F_REFLEC = 1
  #   self.F_REFL = f_refl
  #   self.FILE_REFL = file_refl
  #   self.ALFA = rparams[0]
  #   self.GAMMA = rparams[1]
  #   self.F_THICK = f_thick
  #   return self
  #
  # def setReflectivityScalar(self,f_refl=0,file_refl='GAAS.SHA',rparams=numpy.zeros(2,dtype=numpy.float64),f_thick=0):
  #   self.F_REFLEC = 2
  #   self.F_REFL = f_refl
  #   self.FILE_REFL = file_refl
  #   self.F_THICK = f_thick
  #   return self
  #
  # def setMultilayer(self,f_reflec=1,file_refl='GAAS.SHA',f_thick=0):
  #   self.F_REFLEC = f_reflec
  #   self.F_REFL = 2
  #   self.FILE_REFL = file_refl
  #   self.F_THICK = f_thick
  #   return self
  #
  # def setSpheric(self,rmirr=20.0):
  #   self.FMIRR = 1
  #   self.F_EXT = 1
  #   self.RMIRR = rmirr
  #   return self
  #
  # def setSphericAuto(self,fparams=None):#):f_default=0,ssour=0.0,simag=0.0,theta=0.0):
  #   self.FMIRR = 1
  #   if fparams == None:
  #     self.setAutoFocus(1)
  #   else:
  #     self.setAutoFocus(0,ssour=fparams[0],simag=fparams[1],theta=fparams[2])
  #   return self
  #
  # def setEllipsoid(self,ell_the=0.0,axmaj=0.0,axmin=0.0):
  #   self.FMIRR = 2
  #   self.F_EXT = 1
  #   self.ELL_THE = ell_the
  #   self.AXMAJ = axmaj
  #   self.AXMIN = axmin
  #   return self
  #
  # def setEllipsoidAuto(self,fparams=None):#ell_the=0.0,axmaj=0.0,axmin=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
  #   self.FMIRR = 2
  #   if fparams==None:
  #     self.setAutoFocus(1)
  #   else:
  #     self.setAutoFocus(0,ssour=fparams[0],simag=fparams[1],theta=fparams[2])
  #   return self
  #
  # def setToroid(self,f_torus=0,r_maj=0.0,r_min=0.0):
  #   self.FMIRR = 3
  #   self.F_EXT = 1
  #   self.F_TORUS = f_torus
  #   self.R_MAJ = r_maj
  #   self.R_MIN = r_min
  #   return self
  #
  # def setToroidAuto(self,f_torus=0,r_maj=0.0,r_min=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
  #   self.FMIRR = 3
  #   self.F_TORUS = f_torus
  #   self.R_MAJ = r_maj
  #   self.R_MIN = r_min
  #   self.setAutoFocus(f_default,ssour,simag,theta)
  #   return self
  #
  # def setParaboloid(self,f_side=1,param=0.0):
  #   self.FMIRR = 4
  #   self.F_EXT = 1
  #   self.F_SIDE = f_side
  #   self.PARAM = param
  #   return self
  #
  # def setParaboloidAuto(self,f_side=1,fparams=None):#f_side=1,param=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0,f_side=0):
  #   self.FMIRR = 4
  #   self.F_SIDE = f_side
  #   if fparams==None:
  #     self.setAutoFocus(1)
  #   else:
  #     self.setAutoFocus(0,ssour=fparam[0],simag=fparam[1],theta=fparams[2])
  #   return self
  #
  # def setPlane(self):
  #   self.FMIRR = 5
  #   self.F_EXT = 1
  #   return self
  #
  # def setCodlingSlit(self,cod_len=0.0,cod_wid=0.0): # HERE ASK MANOLO, always 1 or 0
  #   self.FMIRR = 6
  #   self.F_EXT = 1
  #   self.COD_LEN = 0.0
  #   self.COD_WID = 0.0
  #   return self
  #
  # def setHyperboloid(self,ell_the=0.0,axmaj=0.0,axmin=0.0):
  #   self.FMIRR = 7
  #   self.F_EXT = 1
  #   self.ELL_THE = ell_the
  #   self.AXMAJ = axmaj
  #   self.AXMIN = axmin
  #   return self
  #
  # def setHyperboloidAuto(self,fparams=None):#ell_the=0.0,axmaj=0.0,axmin=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
  #   self.FMIRR = 2
  #   if fparams==None:
  #     self.setAutoFocus(1)
  #   else:
  #     self.setAutoFocus(0,ssour=fparams[0],simag=fparams[1],theta=fparams[2])
  #   return self
  #
  # def setCone(self,cone_a=0.0):
  #   self.FMIRR = 8
  #   self.F_EXT = 1
  #   self.CONE_A = cone_a
  #   return self
  #
  # def setPoly(self,file_mir=''):
  #   self.FMIRR = 9
  #   self.F_EXT = 1
  #   self.FILE_MIR = file_mir
  #   return self
  #
  # def setCCC(self,ccc=numpy.array([1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],dtype=numpy.float64)):
  #   self.FMIRR = 10
  #   self.F_EXT = 1
  #   self.CCC[:] = ccc[:]
  #   return self
  #
  # def setRipple(self,f_g_s=0,xyAmpWavPha=numpy.array([0.0,0.0,0.0,0.0,0.0,0.0],dtype=numpy.float64),file_rip=''):
  #   self.F_RIPPLE = 1
  #   self.F_G_S = f_g_s
  #   self.X_RIP_AMP = xyAmpWavPha[0]
  #   self.X_RIP_WAV = xyAmpWavPha[1]
  #   self.X_PHASE   = xyAmpWavPha[2]
  #   self.Y_RIP_AMP = xyAmpWavPha[3]
  #   self.Y_RIP_WAV = xyAmpWavPha[4]
  #   self.Y_PHASE   = xyAmpWavPha[5]
  #   self.FILE_RIP  = file_rip
  #   return self
  #
  # def setDimensions(self,fshape=1,params=numpy.zeros(4,dtype=numpy.float64)):
  #   self.FHIT_C = 1
  #   self.FSHAPE = fshape
  #   self.RLEN1  = params[0]
  #   self.RLEN2  = params[1]
  #   self.RWIDX1 = params[2]
  #   self.RWIDX2 = params[3]
  #   #TODO set self.FHIT_C = 0 elsewhere
  #   return self
  #
  # def setReflector(self):
  #   self.F_REFRACT = 0
  #   return self
  #
  # def setRefractor(self,r_ind_obj = 1.0,r_ind_ima = 1.0,r_attenuation_obj = 0.0,r_attenuation_ima = 0.0):
  #   self.F_REFRACT = 1
  #   self.R_IND_OBJ = r_ind_obj
  #   self.R_IND_IMA = r_ind_ima
  #   self.R_ATTENUATION_OBJ = r_attenuation_obj
  #   self.R_ATTENUATION_IMA = r_attenuation_ima
  #   return self
  #
  # def unsetCrystal(self):
  #   self.F_CRYSTAL = 0
  #   return self
  #
  # def setCrystal(self,file_refl=b'',a_bragg=0.0):
  #   self.F_CRYSTAL = 1
  #   self.FILE_REFL = file_refl
  #   self.F_REFLECT = 0
  #
  #   if a_bragg!=0.0:
  #     self.F_BRAGG_A = 1
  #     self.A_BRAGG = a_bragg
  #   return self
  #
  # def setJohansson(self,r_johansson=None):
  #   self.F_JOHANSSON = 1
  #   #TODO set self.F_JOHANSSON = 0 elsewhere
  #   if r_johansson!=None:
  #     self.F_EXT = 1
  #     self.R_JOHANSSON=r_johansson
  #   else:
  #     self.F_EXT = 0
  #   return self
  #
  # def setGratingRulingConstant(self,f_ruling=1,ruling=0.0):
  #   self.F_GRATING = 1
  #   self.F_RULING = f_ruling
  #   self.RULING = ruling
  #   return self
  #
  # def setGratingHolographic(self,holo_params=numpy.zeros(7,dtype=numpy.float64),f_pw=2,f_pw_c=0,f_virtual=0):
  #   self.F_GRATING = 1
  #   self.F_RULING = 2
  #   self.HOLO_R1 = holo_params[0]
  #   self.HOLO_R2 = holo_params[1]
  #   self.HOLO_DEL = holo_params[2]
  #   self.HOLO_GAM = holo_params[3]
  #   self.HOLO_W = holo_params[4]
  #   self.HOLO_RT1 = holo_params[5]
  #   self.HOLO_RT2 = holo_params[6]
  #   self.F_PW = f_pw
  #   self.F_PW_C = f_pw_c
  #   self.F_VIRTUAL = f_virtual
  #   return self
  #
  # def setGratingFan(self,azim_fan=0.0,dist_fan=0.0,coma_fac=0.0):
  #   self.F_GRATING = 1
  #   self.F_RULING = 3
  #   self.AZIM_FAN = azim_fan
  #   self.DIST_FAN = dist_fan
  #   self.COMA_FAC = coma_fac
  #   return self
  #
  # def setGratingReserved(self):
  #   self.F_GRATING = 1
  #   self.F_RULING = 4
  #   return self
  #
  # def setGratingPolynomial(self,poly_params=numpy.zeros(5,dtype=numpy.float64),f_rul_abs=0):
  #   self.F_GRATING = 1
  #   self.F_RULING = 5
  #   self.F_RUL_ABS = f_rul_abs
  #   self.RULING = poly_params[0]
  #   self.RUL_A1 = poly_params[1]
  #   self.RUL_A2 = poly_params[2]
  #   self.RUL_A3 = poly_params[3]
  #   self.RUL_A4 = poly_params[4]
  #   return self
  #
  # def setMosaic(self,mosaic_seed=4732093,spread_mos=0.0,thickness=0.0):
  #   self.F_MOSAIC = 1
  #   self.MOSAIC_SEED = mosaic_seed
  #   self.SPREAD_MOS = spread_mos
  #   self.THICKNESS = thickness
  #   return self
  #
  # def setAutoTuning(self,f_phot_cent=0,phot_cent=5000.0,r_lambda=100.0):
  #   self.F_CENTRAL = 1
  #   self.F_PHOT_CENT = f_phot_cent
  #   self.PHOT_CENT = phot_cent
  #   self.R_LAMBDA = r_lambda
  #   return self
  #
  # def setAutoMonochromator(self,f_phot_cent=0,phot_cent=5000.0,r_lambda=100.0,f_mono=0,f_hunt=1,hparam=numpy.zeros(3,dtype=numpy.float64)):
  #   self.setAutoTuning(f_phot_cent=f_phot_cent,phot_cent=phot_cent,r_lambda=r_lambda)
  #   self.F_MONO = f_mono
  #   self.F_HUNT = f_hunt
  #   self.HUNT_H = hparam[0]
  #   self.HUNT_L = hparam[1]
  #   self.BLAZE = hparam[2]
  #   return self
  #
  # #
  # # TODO: REMOVE END HERE
  # #

  def unit(self):
      if self.DUMMY == 1.0:
          return 'cm'
      if self.DUMMY == 0.1:
          return 'mm'
      if self.DUMMY == 100.0:
          return 'm'

      return (str(self.DUMMY)+" * m")

  def to_dictionary(self):
    mem = inspect.getmembers(self)
    mydict = {}
    for i,var in enumerate(mem):
      if var[0].isupper():
        mydict[var[0]]= var[1]
    return(mydict)

  # def duplicate(self):
  #   oe_new = OE()
  #   mem = inspect.getmembers(self)
  #   for i,var in enumerate(mem):
  #     if var[0].isupper():
  #       tmp = getattr(self,var[0])
  #       setattr(oe_new,var[0],var[1])
  #   return(oe_new)

  def duplicate(self):
        new_oe = OE()

        new_oe.FMIRR             = self.FMIRR
        new_oe.F_TORUS           = self.F_TORUS
        new_oe.FCYL              = self.FCYL
        new_oe.F_EXT             = self.F_EXT
        new_oe.FSTAT             = self.FSTAT
        new_oe.F_SCREEN          = self.F_SCREEN
        new_oe.F_PLATE           = self.F_PLATE
        new_oe.FSLIT             = self.FSLIT
        new_oe.FWRITE            = self.FWRITE
        new_oe.F_RIPPLE          = self.F_RIPPLE
        new_oe.F_MOVE            = self.F_MOVE
        new_oe.F_THICK           = self.F_THICK
        new_oe.F_BRAGG_A         = self.F_BRAGG_A
        new_oe.F_G_S             = self.F_G_S
        new_oe.F_R_RAN           = self.F_R_RAN
        new_oe.F_GRATING         = self.F_GRATING
        new_oe.F_MOSAIC          = self.F_MOSAIC
        new_oe.F_JOHANSSON       = self.F_JOHANSSON
        new_oe.F_SIDE            = self.F_SIDE
        new_oe.F_CENTRAL         = self.F_CENTRAL
        new_oe.F_CONVEX          = self.F_CONVEX
        new_oe.F_REFLEC          = self.F_REFLEC
        new_oe.F_RUL_ABS         = self.F_RUL_ABS
        new_oe.F_RULING          = self.F_RULING
        new_oe.F_PW              = self.F_PW
        new_oe.F_PW_C            = self.F_PW_C
        new_oe.F_VIRTUAL         = self.F_VIRTUAL
        new_oe.FSHAPE            = self.FSHAPE
        new_oe.FHIT_C            = self.FHIT_C
        new_oe.F_MONO            = self.F_MONO
        new_oe.F_REFRAC          = self.F_REFRAC
        new_oe.F_DEFAULT         = self.F_DEFAULT
        new_oe.F_REFL            = self.F_REFL
        new_oe.F_HUNT            = self.F_HUNT
        new_oe.F_CRYSTAL         = self.F_CRYSTAL
        new_oe.F_PHOT_CENT       = self.F_PHOT_CENT
        new_oe.F_ROUGHNESS       = self.F_ROUGHNESS
        new_oe.F_ANGLE           = self.F_ANGLE
        new_oe.NPOINT            = self.NPOINT
        new_oe.NCOL              = self.NCOL
        new_oe.N_SCREEN          = self.N_SCREEN
        new_oe.ISTAR1            = self.ISTAR1
        new_oe.CIL_ANG           = self.CIL_ANG
        new_oe.ELL_THE           = self.ELL_THE
        new_oe.N_PLATES          = self.N_PLATES
        new_oe.IG_SEED           = self.IG_SEED
        new_oe.MOSAIC_SEED       = self.MOSAIC_SEED
        new_oe.ALPHA             = self.ALPHA
        new_oe.SSOUR             = self.SSOUR
        new_oe.THETA             = self.THETA
        new_oe.SIMAG             = self.SIMAG
        new_oe.RDSOUR            = self.RDSOUR
        new_oe.RTHETA            = self.RTHETA
        new_oe.OFF_SOUX          = self.OFF_SOUX
        new_oe.OFF_SOUY          = self.OFF_SOUY
        new_oe.OFF_SOUZ          = self.OFF_SOUZ
        new_oe.ALPHA_S           = self.ALPHA_S
        new_oe.RLEN1             = self.RLEN1
        new_oe.RLEN2             = self.RLEN2
        new_oe.RMIRR             = self.RMIRR
        new_oe.AXMAJ             = self.AXMAJ
        new_oe.AXMIN             = self.AXMIN
        new_oe.CONE_A            = self.CONE_A
        new_oe.R_MAJ             = self.R_MAJ
        new_oe.R_MIN             = self.R_MIN
        new_oe.RWIDX1            = self.RWIDX1
        new_oe.RWIDX2            = self.RWIDX2
        new_oe.PARAM             = self.PARAM
        new_oe.HUNT_H            = self.HUNT_H
        new_oe.HUNT_L            = self.HUNT_L
        new_oe.BLAZE             = self.BLAZE
        new_oe.RULING            = self.RULING
        new_oe.ORDER             = self.ORDER
        new_oe.PHOT_CENT         = self.PHOT_CENT
        new_oe.X_ROT             = self.X_ROT
        new_oe.D_SPACING         = self.D_SPACING
        new_oe.A_BRAGG           = self.A_BRAGG
        new_oe.SPREAD_MOS        = self.SPREAD_MOS
        new_oe.THICKNESS         = self.THICKNESS
        new_oe.R_JOHANSSON       = self.R_JOHANSSON
        new_oe.Y_ROT             = self.Y_ROT
        new_oe.Z_ROT             = self.Z_ROT
        new_oe.OFFX              = self.OFFX
        new_oe.OFFY              = self.OFFY
        new_oe.OFFZ              = self.OFFZ
        new_oe.SLLEN             = self.SLLEN
        new_oe.SLWID             = self.SLWID
        new_oe.SLTILT            = self.SLTILT
        new_oe.COD_LEN           = self.COD_LEN
        new_oe.COD_WID           = self.COD_WID
        new_oe.X_SOUR            = self.X_SOUR
        new_oe.Y_SOUR            = self.Y_SOUR
        new_oe.Z_SOUR            = self.Z_SOUR
        new_oe.X_SOUR_ROT        = self.X_SOUR_ROT
        new_oe.Y_SOUR_ROT        = self.Y_SOUR_ROT
        new_oe.Z_SOUR_ROT        = self.Z_SOUR_ROT
        new_oe.R_LAMBDA          = self.R_LAMBDA
        new_oe.THETA_I           = self.THETA_I
        new_oe.ALPHA_I           = self.ALPHA_I
        new_oe.T_INCIDENCE       = self.T_INCIDENCE
        new_oe.T_SOURCE          = self.T_SOURCE
        new_oe.T_IMAGE           = self.T_IMAGE
        new_oe.T_REFLECTION      = self.T_REFLECTION

        #TODO due to an incomprehensible bug, the FILE_SOURCE variable in oe
        #changes from len=1024 to len=2048 when shadow runs. Therefore gives
        #a crash when copying to a new object. With this we force the dimension
        new_oe.FILE_SOURCE       = self.FILE_SOURCE[:1023]

        new_oe.FILE_RIP          = self.FILE_RIP
        new_oe.FILE_REFL         = self.FILE_REFL
        new_oe.FILE_MIR          = self.FILE_MIR
        new_oe.FILE_ROUGH        = self.FILE_ROUGH
        new_oe.FZP               = self.FZP
        new_oe.HOLO_R1           = self.HOLO_R1
        new_oe.HOLO_R2           = self.HOLO_R2
        new_oe.HOLO_DEL          = self.HOLO_DEL
        new_oe.HOLO_GAM          = self.HOLO_GAM
        new_oe.HOLO_W            = self.HOLO_W
        new_oe.HOLO_RT1          = self.HOLO_RT1
        new_oe.HOLO_RT2          = self.HOLO_RT2
        new_oe.AZIM_FAN          = self.AZIM_FAN
        new_oe.DIST_FAN          = self.DIST_FAN
        new_oe.COMA_FAC          = self.COMA_FAC
        new_oe.ALFA              = self.ALFA
        new_oe.GAMMA             = self.GAMMA
        new_oe.R_IND_OBJ         = self.R_IND_OBJ
        new_oe.R_IND_IMA         = self.R_IND_IMA
        new_oe.R_ATTENUATION_OBJ = self.R_ATTENUATION_OBJ
        new_oe.R_ATTENUATION_IMA = self.R_ATTENUATION_IMA
        new_oe.F_R_IND           = self.F_R_IND
        new_oe.FILE_R_IND_OBJ    = self.FILE_R_IND_OBJ
        new_oe.FILE_R_IND_IMA    = self.FILE_R_IND_IMA
        new_oe.RUL_A1            = self.RUL_A1
        new_oe.RUL_A2            = self.RUL_A2
        new_oe.RUL_A3            = self.RUL_A3
        new_oe.RUL_A4            = self.RUL_A4
        new_oe.F_POLSEL          = self.F_POLSEL
        new_oe.F_FACET           = self.F_FACET
        new_oe.F_FAC_ORIENT      = self.F_FAC_ORIENT
        new_oe.F_FAC_LATT        = self.F_FAC_LATT
        new_oe.RFAC_LENX         = self.RFAC_LENX
        new_oe.RFAC_LENY         = self.RFAC_LENY
        new_oe.RFAC_PHAX         = self.RFAC_PHAX
        new_oe.RFAC_PHAY         = self.RFAC_PHAY
        new_oe.RFAC_DELX1        = self.RFAC_DELX1
        new_oe.RFAC_DELX2        = self.RFAC_DELX2
        new_oe.RFAC_DELY1        = self.RFAC_DELY1
        new_oe.RFAC_DELY2        = self.RFAC_DELY2
        new_oe.FILE_FAC          = self.FILE_FAC
        new_oe.F_SEGMENT         = self.F_SEGMENT
        new_oe.ISEG_XNUM         = self.ISEG_XNUM
        new_oe.ISEG_YNUM         = self.ISEG_YNUM
        new_oe.FILE_SEGMENT      = self.FILE_SEGMENT
        new_oe.FILE_SEGP         = self.FILE_SEGP
        new_oe.SEG_LENX          = self.SEG_LENX
        new_oe.SEG_LENY          = self.SEG_LENY
        new_oe.F_KOMA            = self.F_KOMA
        new_oe.FILE_KOMA         = self.FILE_KOMA
        new_oe.F_EXIT_SHAPE      = self.F_EXIT_SHAPE
        new_oe.F_INC_MNOR_ANG    = self.F_INC_MNOR_ANG
        new_oe.ZKO_LENGTH        = self.ZKO_LENGTH
        new_oe.RKOMA_CX          = self.RKOMA_CX
        new_oe.RKOMA_CY          = self.RKOMA_CY
        new_oe.F_KOMA_CA         = self.F_KOMA_CA
        new_oe.FILE_KOMA_CA      = self.FILE_KOMA_CA
        new_oe.F_KOMA_BOUNCE     = self.F_KOMA_BOUNCE
        new_oe.X_RIP_AMP         = self.X_RIP_AMP
        new_oe.X_RIP_WAV         = self.X_RIP_WAV
        new_oe.X_PHASE           = self.X_PHASE
        new_oe.Y_RIP_AMP         = self.Y_RIP_AMP
        new_oe.Y_RIP_WAV         = self.Y_RIP_WAV
        new_oe.Y_PHASE           = self.Y_PHASE
        new_oe.N_RIP             = self.N_RIP
        new_oe.ROUGH_X           = self.ROUGH_X
        new_oe.ROUGH_Y           = self.ROUGH_Y
        new_oe.OE_NUMBER         = self.OE_NUMBER
        new_oe.IDUMMY            = self.IDUMMY
        new_oe.DUMMY             = self.DUMMY

        new_oe.CX_SLIT      = copy.deepcopy(self.CX_SLIT)
        new_oe.CZ_SLIT      = copy.deepcopy(self.CZ_SLIT)
        new_oe.D_PLATE      = copy.deepcopy(self.D_PLATE)
        new_oe.FILE_ABS     = copy.deepcopy(self.FILE_ABS)
        new_oe.FILE_SCR_EXT = copy.deepcopy(self.FILE_SCR_EXT)
        new_oe.I_ABS        = copy.deepcopy(self.I_ABS)
        new_oe.I_SCREEN     = copy.deepcopy(self.I_SCREEN)
        new_oe.I_SLIT       = copy.deepcopy(self.I_SLIT)
        new_oe.I_STOP       = copy.deepcopy(self.I_STOP)
        new_oe.K_SLIT       = copy.deepcopy(self.K_SLIT)
        new_oe.RX_SLIT      = copy.deepcopy(self.RX_SLIT)
        new_oe.RZ_SLIT      = copy.deepcopy(self.RZ_SLIT)
        new_oe.SCR_NUMBER   = copy.deepcopy(self.SCR_NUMBER)
        new_oe.SL_DIS       = copy.deepcopy(self.SL_DIS)
        new_oe.THICK        = copy.deepcopy(self.THICK)
        new_oe.CCC          = copy.deepcopy(self.CCC)

        return new_oe

  def screeninfo(self):
        txt = ""
        TSLIT  = ('SLIT       ','STOP       ')
        TSLITSHAPE = ('RECTANGULAR','ELLIPTICAL ','EXTERNAL   ')
        if self.F_SCREEN == 1:
            txt += '  SCREENS:     %d defined \n'%self.N_SCREEN
            for i in range(self.N_SCREEN):
                 txt += '    SCREEN %d : \n'%(i+1)
                 if self.I_SCREEN[i] == 0:
                     txt += '      AFTER o.e.       at %f %s\n'%(self.SL_DIS[i],self.unit())
                 else:
                     txt += '      BEFORE o.e.      at %f %s\n'%(self.SL_DIS[i],self.unit())
                 if self.I_SLIT[i] != 0:
                     txt += "      Type:  %s        %s  \n"%(TSLIT[self.I_STOP[i]],TSLITSHAPE[self.K_SLIT[i]])
                     txt += "      Dimensions: X: %f, Z: %f  \n"%(self.RX_SLIT[i],self.RZ_SLIT[i])
                     txt += "      Center at:  X: %f, Z: %f  \n"%(self.CX_SLIT[i],self.CZ_SLIT[i])
                 if self.I_ABS[i] != 0:
                     txt += "      Attenuator thickness:  %f  %s\n"%(self.THICK[i],self.unit())
                     txt += "      File with optical constants: %s\n"%(self.FILE_ABS[i].strip().decode())

        return txt

  def mirinfo(self, title=None):
    '''
    mimics SHADOW mirinfo postprocessor. Returns a text array.
    :return: a text array with the result
    '''
    #
    txt = ''

    type1 = {}
    type1['1']  = 'SPHERICAL   '
    type1['2']  = 'ELLIPTICAL  '
    type1['3']  = 'TOROIDAL    '
    type1['4']  = 'PARABOLICAL '
    type1['5']  = 'PLANE       '
    type1['6']  = 'CODLING SLIT'
    type1['7']  = 'HYPERBOLICAL'
    type1['8']  = 'CONICAL     '
    type1['9']  = 'POLYNOMIAL  '
    type1['10'] = 'CONIC EXTNAL'
    type1['11'] = '            '
    type1['12'] = '            '

    TOPLIN = '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n'
    T20 = '                    '
    T60 = T20 + T20 + T20
    txt += TOPLIN
    txt += '********************   OPTICAL ELEMENT  DESCRIPTION   ********************\n'

    if title == None:
        txt += '\n\n'
    else:
        txt += title+'\n'
    txt += TOPLIN

    if self.F_REFRAC == 2:
        txt += 'Element type                            EMPTY\n'
    else:

        txt += 'Surface figure was defined as: %s \n'%(type1[str(self.FMIRR)])
        if ( (self.FMIRR == 1) or (self.FMIRR == 2) or (self.FMIRR == 4) or \
             (self.FMIRR == 7) or (self.FMIRR == 8) or (self.FMIRR == 9) or \
             (self.FMIRR == 10) ):
            if self.FCYL == 0:
                txt += 'Cylindrical figure                      NO\n'
            else:
                txt += 'Cylindrical figure                      YES\n'
                txt += 'Cylinder axis angle from X-axis         %f \n'%(self.CIL_ANG*180.0/numpy.pi)


        if self.F_ROUGHNESS == 1:
            txt += 'Roughness on from '+self.FILE_ROUGH.strip()
            txt += 'RMS in Y (angstroms)                   %f \n'%(self.ROUGH_Y)
            txt += 'RMS in X (angstroms)                   %f \n'%(self.ROUGH_X)
        if self.F_REFRAC == 0:
            txt += 'Element type                            REFLECTOR\n'
        if self.F_REFRAC == 1:
            txt += 'Element type                            REFRACTOR\n'
        if self.F_REFRAC == 2:
            txt += 'Element type                            EMPTY\n'

        if ((self.F_GRATING == 0) and (self.F_CRYSTAL ==0)):
            if self.F_FACET == 1:
                txt += 'Element type                       Faceted Mirror\n'
                txt += 'Facet size (X)                          %f\n'%(self.RFAC_LENX)
                txt += 'Facet size (Y)                          %f\n'%(self.RFAC_LENY)
                txt += 'Facet polynomial from                   %s\n'%(self.FILE_FAC.strip().decode())
                if self.F_POLSEL == 3:
                    txt += 'Intercept used                         CLOSEST\n'
                if self.F_POLSEL == 2:
                    txt += 'Intercept used                         2nd CLOSEST\n'
                if self.F_POLSEL == 1:
                    txt += 'Intercept used                         2nd FARTHEST\n'
                if self.F_POLSEL == 4:
                    txt += 'Intercept used                         FARTHEST\n'
            if self.F_KOMA == 1:
                txt += 'Element type                Multi-bounce Tube Array\n'
                if self.F_KOMA_CA == 1:
                    txt += 'Paramters from  %s\n'%(self.FILE_KOMA_CA.strip().decode())
                    txt += 'Tube radii specified as (r(Z))**2\n'
                else:
                    txt += 'Paramters from  %s\n'(self.FILE_KOMA.strip().decode())
                    txt += 'Tube radii specified as r(Z)\n'

        if ((self.F_GRATING == 0) and (self.F_CRYSTAL == 1)):
            txt += 'Element type                            CRYSTAL\n'
            txt += 'Lattice Spacing                         %g Angstroms\n'%(1e8*self.D_SPACING)
            # txt += 'Bragg Reflection from file:             %s\n'%(self.FILE_REFL.strip().decode())
            if self.F_MOSAIC == 1:
                txt += 'MOSAIC Crystal selected                \n'
                txt += 'Mosaic crystal spread (st. dev) %f %s\n'%(self.SPREAD_MOS*180.0/numpy.pi,'deg')
                txt += 'Mosaic crystal spread (FWHM)    %f %s\n'%(2*numpy.sqrt(2*numpy.log(2))*self.SPREAD_MOS*180.0/numpy.pi,'deg')
                txt += 'Mosaic crystal thickness        %f %s\n'%(self.THICKNESS,self.unit())
            else:
                if self.F_BRAGG_A == 1:
                    txt += 'Asymmetric Cut angle                    %f deg\n'%(self.A_BRAGG*180.0/numpy.pi)
                if self.F_JOHANSSON == 1:
                    txt += 'JOHANSSON Geometry selected            \n'
                    txt += 'Johansson radius                         %f %s\n'(self.R_JOHANSSON,self.unit())


        if self.F_GRATING == 1:
            if self.FZP == 1:
                txt += 'Element type                 Fresnel Zone Plate\n'
            txt += 'Element type                            GRATING\n'
            txt += 'Order choosen ( inside are < 0 )        %d\n'%(self.ORDER)
            if self.F_CENTRAL == 1:
                txt += 'Automatic Tuning                        YES\n'
                if ((self.F_MONO == 0) and (self.F_CRYSTAL == 0)):
                    txt += 'Mount                                   SEYA / TGM\n'
                if ((self.F_MONO == 0) and (self.F_CRYSTAL == 1)):
                    txt += 'Mount                                   BRAGG\n'
                if ((self.F_MONO == 1) and (self.F_CRYSTAL == 0)):
                    txt += 'Mount                                   ERG\n'
                if ((self.F_MONO == 2) and (self.F_CRYSTAL == 0)):
                    txt += 'Mount                                   Const. INCIDENCE\n'
                if ((self.F_MONO == 3) and (self.F_CRYSTAL == 0)):
                    txt += 'Mount                                   Const. DIFFRACTION\n'
                if ((self.F_MONO == 4) and (self.F_CRYSTAL == 0)):
                    txt += 'Mount                                   Const. BLAZE\n'

            if (self.F_RULING == 0) and (self.F_CRYSTAL == 0):
                txt += 'Constant ruling [ lines/cm ]            %f\n'%(self.RULING)
            if (self.F_RULING == 1) and (self.F_CRYSTAL == 0):
                txt += 'Uniform ruling. At pole [ lines/cm ]    %f\n'%(self.RULING)

            if (self.F_RULING == 2) and (self.F_CRYSTAL == 0):
                txt += '    Holographic grating: \n'
                txt += '    Recording Wavelength:       %f Angstroms\n'%(self.HOLO_W)
                txt += '    Input Slit Distance:        %f \n'%(self.HOLO_R1)
                txt += '    Exit Slit Distance:         %f \n'%(self.HOLO_R2)
                txt += '    Input Slit Angle:           %f deg\n'%(self.HOLO_DEL)
                txt += '    Exit Slit Angle:            %f deg\n'%(self.HOLO_GAM)
                txt += '    Input  Slit rotation angle  %f deg\n'%(self.HOLO_RT1)
                txt += '    Output Slit rotation angle  %f deg\n'%(self.HOLO_RT2)

                if (self.F_PW == 0):         txt += 'Spherical / Spherical\n'
                if (self.F_PW == 1):         txt += 'Plane     / Spherical\n'
                if (self.F_PW == 2):         txt += 'Spherical / Plane\n'
                if (self.F_PW == 3):         txt += 'Plane     / Plane\n'
                if (self.F_PW_C == 0):       txt += 'Spherical   / Spherical\n'
                if (self.F_PW_C == 1):       txt += 'Cylindrical / Spherical\n'
                if (self.F_PW_C == 2):       txt += 'Spherical   / Cylindrical\n'
                if (self.F_PW_C == 3):       txt += 'Cylindrical / Cylindrical\n'
                if (self.F_VIRTUAL == 0):    txt += 'Real      / Real\n'
                if (self.F_VIRTUAL == 1):    txt += 'Real      / Virtual\n'
                if (self.F_VIRTUAL == 2):    txt += 'Virtual   / Real\n'
                if (self.F_VIRTUAL == 3):    txt + 'Virtual   / Virtual\n'
            if (self.F_RULING == 3) and (self.F_CRYSTAL == 0):
                txt += 'Oriental fan type grating.\n'
                txt += 'Fan pole angle from Y axis          %f deg\n'%(self.AZIM_FAN)
                txt += '        distance from grating pole  %f %s\n'%(self.DIST_FAN,self.unit())
                txt += 'Coma correction factor              %f\n'%(self.COMA_FAC)
                txt += 'Line density at grating pole        %f lines/cm\n'%(self.RULING)
            if (self.F_RULING == 5) and (self.F_CRYSTAL == 0):
                txt += 'Mechanically ruled grating. Polinomial Coefficients: \n'
                txt += 'Zero order term Coefficient:  %f lines/(%s  )\n'%(self.RULING,"cm")
                txt += 'First                         %f lines/(%s^2)\n'%(self.RUL_A1,"cm")
                txt += 'Second                        %f lines/(%s^3)\n'%(self.RUL_A2,"cm")
                txt += 'Third                         %f lines/(%s^4)\n'%(self.RUL_A3,"cm")
                txt += 'Fourth                        %f lines/(%s^5)\n'%(self.RUL_A4,"cm")


        if self.F_REFRAC == 1 and self.F_CRYSTAL == 0:
            if self.F_R_IND == 0:
                txt += "Index of refraction in object space: %14.10f. Attenuation coeff: %f\n"%(self.R_IND_OBJ,self.R_ATTENUATION_OBJ)
                txt += "Index of refraction in image space: %14.10f. Attenuation coeff: %f\n"%(self.R_IND_IMA,self.R_ATTENUATION_IMA)
            elif self.F_R_IND == 1:
                txt += "Index of refraction in object space from file %s\n"%(self.FILE_R_IND_OBJ.strip().decode())
                txt += "Index of refraction in image space: %14.10f. Attenuation coeff: %f\n"%(self.R_IND_IMA,self.R_ATTENUATION_IMA)
            elif self.F_R_IND == 2:
                txt += "Index of refraction in object space: %14.10f. Attenuation coeff: %f\n"%(self.R_IND_OBJ,self.R_ATTENUATION_OBJ)
                txt += "Index of refraction in image space from file %s\n"%(self.FILE_R_IND_IMA.strip().decode())
            elif self.F_R_IND == 3:
                txt += "Index of refraction in object space from file %s\n"%(self.FILE_R_IND_OBJ.strip().decode())
                txt += "Index of refraction in image space from file %s\n"%(self.FILE_R_IND_IMA.strip().decode())

            #txt += 'Relative Index of Refraction            %f\n'%(self.ALFA)

        if self.F_CRYSTAL == 0:
            if self.F_REFLEC == 0:
                txt += 'Reflectivity                            OFF\n'
            else:
                if self.F_REFL == 0:
                    txt += 'Reflectivity      ON     coefficients from: %s\n'%(self.FILE_REFL.strip().decode())
                if self.F_REFL == 1:
                    txt += 'Reflectivity      ON     coefficients from input parameter\n'
                if self.F_REFL == 2:
                    txt += 'Multilayer        ON     coefficients and geometry from :  %s\n'%(self.FILE_REFL.strip().decode())


            if self.F_REFLEC == 1: txt += 'Polarization dependence                 YES\n'
            if self.F_REFLEC == 2: txt += 'Polarization dependence                 NO\n'
        else:
            txt += 'Crystal structure parameters from file: %s \n'%(self.FILE_REFL.strip().decode())

        if self.FHIT_C == 0:
            txt += 'Optical element dimensions                       UNLIMITED\n'
        else:
            if self.FSHAPE == 0:
                txt += 'Invalid o.e. dimensions ( FSHAPE=0 )\n'
            if self.FSHAPE == 1:
                txt += 'Mirror dimensions ( rectangular ):\n'
                txt += '          X plus: %f, X minus: %f, Y plus: %f, Y minus: %f\n'%\
                       (self.RWIDX1,self.RWIDX2,self.RLEN1,self.RLEN2)
            if self.FSHAPE == 2:
                txt += 'Mirror dimensions ( elliptical ) :\n'
                txt += '          Major Axis: %f, Minor axis: %f \n'%\
                        (self.RWIDX2,self.RLEN2)
            if self.FSHAPE == 3:
                txt += 'Mirror dimensions ( elliptical + hole )\n'
                txt += 'A. Outside border: %f, %f\n'%\
                        (self.RWIDX2,self.RLEN2)
                txt += 'A. Inner border: %f, %f\n'%\
                        (self.RWIDX1,self.RLEN1)

    txt += TOPLIN
    txt += 'Central Axis parameters :\n'
    txt += 'Source Plane Distance                    %f %s\n'%(self.T_SOURCE,self.unit())
    txt += 'Image  Plane                             %f %s\n'%(self.T_IMAGE,self.unit())
    txt += 'Incidence Angle                          %f %s\n'%(self.T_INCIDENCE*180.0/numpy.pi,'deg')
    txt += 'Reflection/Diffraction Angle             %f %s\n'%(self.T_REFLECTION*180.0/numpy.pi,'deg')

    txt += self.screeninfo()

    if self.F_REFRAC != 2:
        if self.FMIRR != 5:
            if self.F_EXT == 1:
                txt += 'Mirror parameters                       EXTERNAL\n'
            else:
                if self.FMIRR != 10:
                    txt += 'Mirror parameters                       COMPUTED\n'
                    if self.F_DEFAULT == 1:
                        txt += 'Same configuration as Central Axis      YES\n'
                    else:
                        txt += 'Same configuration as Central Axis      NO\n'
                    txt += 'Objective focus at                       %f %s\n'%(self.SSOUR, self.unit())
                    txt += 'Image focus at                           %f %s\n'%(self.SIMAG, self.unit())
                    txt += 'Incidence angle                          %f %s\n'%(self.THETA*180.0/numpy.pi, 'deg')


            txt += 'Parameters used follow:\n'
            if self.FMIRR == 1:
                txt += 'Spherical Radius  %f\n'%(self.RMIRR)
            if self.FMIRR == 2:
                ECCENT = numpy.sqrt(self.AXMAJ**2-self.AXMIN**2)/self.AXMAJ
                txt += '   Semi-major axis   %f %s\n'%(self.AXMAJ,self.unit())
                txt += '   Semi-minor axis   %f %s\n'%(self.AXMIN,self.unit())
                txt += '   Semi-focal-length %f %s\n'%(numpy.sqrt(self.AXMAJ**2-self.AXMIN**2),self.unit())
                txt += '   Eccentricity      %f\n'%(ECCENT)
            if self.FMIRR == 3:
                txt  += '   Major Radius (optical)     %f %s\n'%(self.R_MAJ+self.R_MIN, self.unit())
                txt  += '   Minor Radius               %f %s\n'%(self.R_MIN, self.unit())
            if self.FMIRR == 4:
                txt += '   Parabola Param.  %f\n'%(self.PARAM)
            if self.FMIRR == 5:
                txt += '   Plane mirror \n'
            if self.FMIRR == 6:
                txt += '   Codling Slit\n'
            if self.FMIRR == 7:
                AFOCI = numpy.sqrt(self.AXMIN**2+self.AXMAJ**2)
                ECCENT = AFOCI/numpy.abs(self.AXMAJ)
                txt += '   Semi-major axis   %f %s\n'%(self.AXMAJ,self.unit())
                txt += '   Semi-minor axis   %f %s\n'%(self.AXMIN,self.unit())
                txt += '   Semi-focal-length %f %s\n'%(AFOCI,self.unit())
                txt += '   Eccentricity      %f\n'%(ECCENT)
            if self.FMIRR == 8:
                txt += '   Cone half-angle   %f %s\n'%(self.CONE_A*180.0/numpy.pi,'deg')
            if self.FMIRR == 9:
                txt += '   Polynomial Coeff file    %s\n'%(self.FILE_MIR.strip().decode())




        if self.FSTAT == 0:
            txt += 'Source of this O.E. moved               NO\n'
        else:
            txt += 'Source of this O.E. moved               YES\n'
            txt += 'In SOURCE reference frame: \n'
            txt += 'Source Movement X:  %f %s\n'%(self.X_SOUR,self.unit())
            txt += '                Y:  %f %s\n'%(self.Y_SOUR,self.unit())
            txt += '                Z:  %f %s\n'%(self.Z_SOUR,self.unit())
            txt += 'Source rot at X:    %f %s\n'%(self.X_SOUR_ROT*180.0/numpy.pi,'deg')
            txt += '              Y:    %f %s\n'%(self.Y_SOUR_ROT*180.0/numpy.pi,'deg')
            txt += '              Z:    %f %s\n'%(self.Z_SOUR_ROT*180.0/numpy.pi,'deg')
            txt += 'In MIRROR reference frame: \n'
            txt += 'Source distance     %f %s\n'%(self.RDSOUR,self.unit())
            txt += '       rotation     %f %s\n'%(self.ALPHA_S*180.0/numpy.pi,'deg')
            txt += 'Incidence angle     %f %s\n'%(self.RTHETA*180.0/numpy.pi,'deg')
            txt += 'Source offset X:    %f %s\n'%(self.OFF_SOUX,self.unit())
            txt += '              Y:    %f %s\n'%(self.OFF_SOUY,self.unit())
            txt += '              Z:    %f %s\n'%(self.OFF_SOUZ,self.unit())

        if self.F_MOVE == 0:
            txt += 'Mirror at pole position ( no mov. )     YES\n'
        else:
            txt += 'Mirror moved from pole. Parameters :\n'
            txt += 'Displacement along X:    %f %s\n'%(self.OFFX,self.unit())
            txt += '                   Y:    %f %s\n'%(self.OFFY,self.unit())
            txt += '                   Z:    %f %s\n'%(self.OFFZ,self.unit())
            txt += 'Rotation around X:    %f %s\n'%(self.X_ROT*180.0/numpy.pi,'deg')
            txt += '                Y:    %f %s\n'%(self.Y_ROT*180.0/numpy.pi,'deg')
            txt += '                Z:    %f %s\n'%(self.Z_ROT*180.0/numpy.pi,'deg')
        if ( (self.FMIRR == 1) or (self.FMIRR == 2) or (self.FMIRR == 4) or\
            (self.FMIRR == 5) or (self.FMIRR == 7) or (self.FMIRR == 8) or\
            (self.FMIRR == 9) or (self.FMIRR == 10) ):
            txt += '\n'+TOPLIN
            txt += 'OE surface in form of conic equation: \n'
            txt += '    c[1]*X^2 + c[2]*Y^2 + c[3]*Z^2 + \n'
            txt += '    c[4]*X*Y + c[5]*Y*Z + c[6]*X*Z  + \n'
            txt += '    c[7]*X + c[8]*Y + c[9]*Z + c[10] = 0  \n'
            txt += ' with \n'
            for i in range(10):
                txt += '  c[%d]= %g\n'%(i+1,self.CCC[i])

    txt += TOPLIN
    txt += '***************                 E N D                  ***************\n'
    txt += TOPLIN

    return txt



class CompoundOE():

  def __init__(self,list=None, name='', user_units_to_cm=1.0):
    if list == None:
        self.list = []
    else:
        self.list = list
    self.name = name
    self.user_units_to_cm = user_units_to_cm
    self = list #.__init__()

  def set_name(self,name):
      self.name = name

  def number_oe(self):
      return len(self.list)

  def unit(self):
      if self.user_units_to_cm == 1.0:
          return 'cm'
      if self.user_units_to_cm == 0.1:
          return 'mm'
      if self.user_units_to_cm == 100.0:
          return 'm'

      return (str(self.user_units_to_cm)+" * m")

  def info(self,file=''):
    """
    write a summary of the real distances, focal distances and orientation angles.
    :param file: set to a file name to dump tesult into ir
    :return: a text array
    """
    # print("CompoundOE name: %s, found %d elements"%(self.name,self.number_oe()))
    # for i,j in enumerate(self.list):
    #   print('oe %d, p=%f, q=%f'%(1+i,j.T_SOURCE,j.T_IMAGE))

    txt = '  ********  SUMMARY OF DISTANCES ********\n'
    txt += '   ** DISTANCES FOR ALL O.E. [%s] **           \n'%self.unit()
    txt += "%12s %12s %14s %14s %14s %14s \n"%('OE','TYPE','p['+self.unit()+']','q['+self.unit()+']','src-oe','src-screen')


    tot = 0.0
    alphatot=0.0
    deflection_H = 0.0
    deflection_V = 0.0
    pihalf = numpy.pi/2
    txt1 = ''
    txt2 = ''
    oeshape = '?'

    for i,oe in enumerate(self.list):
        #1) Distances summary
        oetype = 'UNKNOWN'
        if oe.F_REFRAC == 1:
            oetype = 'REFRACTOR'
        else:
            oetype = 'MIRROR'
        if oe.F_CRYSTAL == 1: oetype = 'CRYSTAL'
        if oe.F_GRATING == 1: oetype = 'GRATING'
        if oe.F_REFRAC == 2: oetype = 'EMPTY'

        tot = tot + oe.T_SOURCE + oe.T_IMAGE
        totoe = tot - oe.T_IMAGE
        line="%12d %12s %14.4f %14.4f %14.4f %14.4f \n"%(i+1,oetype,oe.T_SOURCE,oe.T_IMAGE,totoe,tot)
        txt1 += line

        # 2) focusing summary

        if oe.FMIRR != 5 and oe.FMIRR != 9:
           if oe.FMIRR == 1:
             if oe.FCYL == 0:
                 oeshape='SPHERE'
             else:
                 oeshape='CYLINDER'
           if oe.FMIRR == 2:
             if oe.FCYL == 0:
                 oeshape='ELLIPSOID'
             else:
                 oeshape='ELLIPSE'
           if oe.FMIRR == 3:
             oeshape='TOROID'
           if oe.FMIRR == 4:
             if oe.FCYL == 0:
                 oeshape='PARABOLID'
             else:
                 oeshape='PARABOLA'
           if oe.FMIRR == 6:
             oeshape='CODLING SLIT'
           if oe.FMIRR == 7:
             if oe.FCYL == 0:
                 oeshape='HYPERBOLOID'
             else:
                 oeshape='HYPERBOLA'
           if oe.FMIRR == 8:
             oeshape='CONE'
           if oe.FMIRR == 9:
             oeshape='POLYNOMIAL'
           if oe.FMIRR == 10:
             oeshape='CONIC COEFF'
           if oe.F_DEFAULT == 1:
             pp = oe.T_SOURCE
             qq = oe.T_IMAGE
           else:
             pp = oe.SSOUR
             qq = oe.SIMAG

           if oe.F_EXT == 1:
              line = '%10d %10s %10s %10s %10s \n'%( i+1,oeshape,'?','?','?')
           else:
              line = '%10d %10s %10.2f %10.2f %10.2f \n'%(i+1,oeshape,pp,qq,pp/qq)
           txt2 += line


        if oe.IDUMMY == 0: # oe not changed by shadow, angles in deg changed to rad
            T_INCIDENCE  = oe.T_INCIDENCE  * numpi.pi / 180.0
            T_REFLECTION = oe.T_REFLECTION * numpi.pi / 180.0
            ALPHA        = oe.ALPHA        * numpi.pi / 180.0
        else:
            T_INCIDENCE  = oe.T_INCIDENCE
            T_REFLECTION = oe.T_REFLECTION
            ALPHA        = oe.ALPHA


        # 3) total deflection

        alphatot = alphatot + ALPHA

        deflection_H = deflection_H +  numpy.sin(alphatot) *  ( (pihalf-T_INCIDENCE) + (pihalf-T_REFLECTION) )
        deflection_V = deflection_V +  numpy.cos(alphatot) *  ( (pihalf-T_INCIDENCE) + (pihalf-T_REFLECTION) )

    txt += txt1
    txt += '\n'
    txt += '   ** FOCUSING ELEMENTS **           \n'
    # focusing elements
    line = '%10s %10s %10s %10s %10s \n'%('OE','SHAPE','p_foc','q_foc','1/M')
    txt += line
    txt += txt2


    txt += '\n'
    line = 'Sum of Alphas [deg]:         %f \n'%(alphatot*180/numpy.pi)
    txt += line
    line = 'Sum of Alphas Mod 180 [deg]: %f \n'%( numpy.mod(alphatot*180/numpy.pi,180))
    txt += line
    line = 'Sum of Alphas Mod 360 [deg]: %f \n'%( numpy.mod(alphatot*180/numpy.pi,360))
    txt += line

    txt += '\n'

    if oe.IDUMMY != 1:
        txt += "**Warning: oe.IDUMMY = %d**\n"%(oe.IDUMMY)
        txt += "**         SHADOW did not run, therefore autosetting angles are not considered**\n\n"
    line = 'Total deflection angle H = %12.6f rad = %9.3f deg\n'%(deflection_H,deflection_H*180/numpy.pi)
    txt += line
    line = 'Total deflection angle V = %12.6f rad = %9.3f deg \n'%(deflection_V,deflection_V*180/numpy.pi)
    txt += line

    if file != '':
        f = open(file,mode='w')
        for line in txt:
            f.write(line)
        f.close()
        print("File written to disk (compoundOE summary): ",file)

    return(txt)


  def sysinfo(self,title="",comment=""):

    txt = "\n"

    TOPLIN = '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n'
    TSCR = {}
    TYPE1 = {}

    TSCR["1"] = 'AFTER Mirror'
    TSCR["2"] = 'BEFORE  Mirror'

    TYPE1["1"]  = 'SPHERICAL   '
    TYPE1["2"]  = 'ELLIPTICAL  '
    TYPE1["3"]  = 'TOROIDAL    '
    TYPE1["4"]  = 'PARABOLICAL '
    TYPE1["5"]  = 'PLANE       '
    TYPE1["6"]  = 'CODLING SLIT'
    TYPE1["7"]  = 'HYPERBOLICAL'
    TYPE1["8"]  = 'CONICAL     '
    TYPE1["9"]  = '            '
    TYPE1["10"] = '            '
    TYPE1["11"] = '            '
    TYPE1["12"] = '            '
    BREAK	='    ----------------\n'

    txt += TOPLIN
    txt += '**************  S Y S T E M      D E S C R I P T I O N  **************\n'
    txt += TOPLIN
    txt += title + "\n"
    txt += comment + "\n"
    txt += TOPLIN

    txt += 'Units (length) in use:  %s \n\n'%(self.unit())
    for i,oe in enumerate(self.list):
        J = i + 1
        txt += ' \n'
        txt += 'Optical Element # %d      System Number: \n'%(J)

        if oe.F_REFRAC == 2:
            TEXT = "EMPTY ELEMENT"
        else:
            if oe.F_REFRAC == 0:
                TEXT = "REFLECTOR"
            else:
                TEXT = "REFRACTOR"
            if oe.F_CRYSTAL == 1:
                TEXT += "-CRYSTAL"
            elif oe.F_GRATING == 1:
                TEXT += "-GRATING"
            else:
                TEXT += "-MIRROR"

            TEXT += "    " + TYPE1[str(oe.FMIRR)]

            if oe.FHIT_C == 1:
                TEXT += "    DIMENSIONS CHECKED"
            else:
                TEXT += "    UNLIMITED"

            if oe.F_EXT == 1:
                TEXT += "    CURVATURE: EXTERNAL"
            else:
                TEXT += "    CURVATURE: COMPUTED"

            if oe.F_REFLEC == 0:
                TEXT += "    REFLECTIVITY OFF"
            else:
                TEXT += "    REFLECTIVITY ON"

        TODEG = 180.0/numpy.pi

        if oe.IDUMMY == 0 and oe.F_CENTRAL == 1:
            TEXT += "\n\n ** Error: Shadow.CompoundOE.sysinfo(): F_CENTRAL == %d => T_INCIDENCE and T_REFLECTION not calculated. **\n\n"%oe.F_CENTRAL

        if oe.IDUMMY == 0: # oe not changed by shadow, angles in deg changed to rad
            T_INCIDENCE  = oe.T_INCIDENCE / TODEG
            T_REFLECTION = oe.T_REFLECTION / TODEG
            ALPHA        = oe.ALPHA / TODEG
        else:
            T_INCIDENCE  = oe.T_INCIDENCE
            T_REFLECTION = oe.T_REFLECTION
            ALPHA        = oe.ALPHA

        TODEG = 180.0/numpy.pi

        txt +=  '\n'
        txt +=  TEXT+"\n"
        txt +=  '\n'
        txt +=  '  Orientation        %f deg\n'%(ALPHA*TODEG)
        txt +=  '  Source Plane       %f %s \n'   %(oe.T_SOURCE,oe.unit())
        txt +=  '  Incidence Ang.     %f deg\n'%(T_INCIDENCE*TODEG)
        txt +=  '  Reflection Ang.    %f deg\n'%(T_REFLECTION*TODEG)
        txt +=  '  Image Plane        %f %s\n'   %(oe.T_IMAGE,oe.unit())
        txt += 	BREAK

        txt += oe.screeninfo()

    txt += "\n\n                          OPTICAL SYSTEM CONFIGURATION\n"
    txt += "                           Laboratory Reference Frame.\n"

    #txt += "OPT. Elem #       X =                 Y =                 Z =\n\n"
    # optaxis_file = "optax.02"
    #
    # f = open(optaxis_file,'r')
    # for i,oe in enumerate(self.list):
    #     n = f.readline()
    #     source = f.readline()
    #     if i == 0:
    #         txt += "       0        "+source
    #     mirror = f.readline()
    #     image = f.readline()
    #     txt += "       %d        "%(i+1) + mirror
    #     txt += "          %d'    "%(i+1) + image + "\n"
    #     for j in range(5):
    #         tmp = f.readline()
    # f.close()

    txt += "OPT. Elem #       X =                 Y =                 Z =\n\n"

    #
    # this "nasty" part is because we may not have the optax.xx files available so we recalculate
    # everything. This is a translation of the OPTAXIS routine in shadow_kernel.F90
    #
    CENTRAL = numpy.zeros( (25) )
    U_VEC =   numpy.zeros( (4) )
    V_VEC =   numpy.zeros( (4) )
    W_VEC =   numpy.zeros( (4) )
    V_REF =   numpy.zeros( (4) )
    V_PERP =  numpy.zeros( (4) )
    for i,oe in enumerate(self.list):

        if oe.IDUMMY == 0: # oe not changed by shadow, angles in deg changed to rad
            T_INCIDENCE  = oe.T_INCIDENCE / TODEG
            T_REFLECTION = oe.T_REFLECTION / TODEG
            ALPHA        = oe.ALPHA / TODEG
        else:
            T_INCIDENCE  = oe.T_INCIDENCE
            T_REFLECTION = oe.T_REFLECTION
            ALPHA        = oe.ALPHA

        COSAL = numpy.cos(ALPHA)
        SINAL = numpy.sin(ALPHA)
        PIHALF = numpy.pi/2
        PI = numpy.pi
        DEFLECTION	=   T_INCIDENCE + T_REFLECTION
        if i == 0:
            U_VEC [1]	=   COSAL
            U_VEC [2]	=   0.0
            U_VEC [3]   =   SINAL
            V_VEC [1]   = - numpy.sin(PIHALF - T_INCIDENCE)*SINAL
            V_VEC [2]   =   numpy.cos(PIHALF - T_INCIDENCE)
            V_VEC [3]   =   numpy.sin(PIHALF - T_INCIDENCE)*COSAL
            W_VEC [1]   = - numpy.sin(PI - T_INCIDENCE)*SINAL
            W_VEC [2]   =   numpy.cos(PI - T_INCIDENCE)
            W_VEC [3]   =   numpy.sin(PI - T_INCIDENCE)*COSAL
            V_REF [1]   = - numpy.sin(PI - DEFLECTION)*SINAL
            V_REF [2]   =   numpy.cos(PI - DEFLECTION)
            V_REF [3]   =   numpy.sin(PI - DEFLECTION)*COSAL
            V_PERP [1]  = - numpy.sin(3*PIHALF - DEFLECTION)*SINAL
            V_PERP [2]  =   numpy.cos(3*PIHALF - DEFLECTION)
            V_PERP [3]  =   numpy.sin(3*PIHALF - DEFLECTION)*COSAL

            CENTRAL[1]  =   .0
            CENTRAL[2]  =   .0
            CENTRAL[3]  =   .0
            CENTRAL[4]  =   .0
            CENTRAL[5]  =   oe.T_SOURCE
            CENTRAL[6]  =   .0
            CENTRAL[7] 	=   oe.T_IMAGE*V_REF[1]
            CENTRAL[8] 	=   oe.T_IMAGE*V_REF[2] + oe.T_SOURCE
            CENTRAL[9] 	=   oe.T_IMAGE*V_REF[3]
            CENTRAL[10]	=   U_VEC[1]
            CENTRAL[11]	=   U_VEC[2]
            CENTRAL[12]	=   U_VEC[3]
            CENTRAL[13]	=   V_VEC[1]
            CENTRAL[14]	=   V_VEC[2]
            CENTRAL[15]	=   V_VEC[3]
            CENTRAL[16]	=   W_VEC[1]
            CENTRAL[17]	=   W_VEC[2]
            CENTRAL[18]	=   W_VEC[3]
            CENTRAL[19]	=   V_REF[1]
            CENTRAL[20]	=   V_REF[2]
            CENTRAL[21]	=   V_REF[3]
            CENTRAL[22] =   V_PERP[1]
            CENTRAL[23] =   V_PERP[2]
            CENTRAL[24] =   V_PERP[3]
            txt += "       0    %18.11f     %18.11f     %18.11f \n"%(CENTRAL[1],CENTRAL[2],CENTRAL[3])
        else:
            #    ! ** Computes now the OLD mirror reference frame in the lab. coordinates
            #    ! ** system. The rotation angle ALPHA of the current mirror is defined in
            #    ! ** this reference frame, as ALPHA measure the angle between the two
            #    ! ** incidence planes (not necessarily the same).
            CENTRAL_OLD = CENTRAL.copy()
            U_OLD = numpy.array(  [0.0,CENTRAL[10],CENTRAL[11],CENTRAL[12]])
            R_OLD = numpy.array(  [0.0,CENTRAL[19],CENTRAL[20],CENTRAL[21]])
            RP_OLD = numpy.array( [0.0,CENTRAL[22],CENTRAL[23],CENTRAL[24]])


            #    ! ** This vector is the NORMAL of the new mirror in the OMRF (U,R_OLD,RP_OLD) **
            V_TEMP = numpy.zeros(4)
            V_TEMP [1]	= - numpy.sin(PI - T_INCIDENCE)*SINAL
            V_TEMP [2]	=   numpy.cos(PI - T_INCIDENCE)
            V_TEMP [3]	=   numpy.sin(PI - T_INCIDENCE)*COSAL

            #    ! ** Rotate it finally to (x,y,z) SRF **
            W_VEC [1]	=    V_TEMP[1]*U_OLD[1] + V_TEMP[2]*R_OLD[1] + V_TEMP[3]*RP_OLD[1]
            W_VEC [2]	=    V_TEMP[1]*U_OLD[2] + V_TEMP[2]*R_OLD[2] + V_TEMP[3]*RP_OLD[2]
            W_VEC [3]	=    V_TEMP[1]*U_OLD[3] + V_TEMP[2]*R_OLD[3] + V_TEMP[3]*RP_OLD[3]

            #    ! ** This vector is the reflected beam from the new mirror in the OMRF **
            V_TEMP[1] = -  numpy.sin(PI - DEFLECTION)*SINAL
            V_TEMP[2] =    numpy.cos(PI - DEFLECTION)
            V_TEMP[3] =    numpy.sin(PI - DEFLECTION)*COSAL

            #    ! ** Express it now in the (x,y,z) SRF
            V_REF[1] = V_TEMP[1] * U_OLD[1] + V_TEMP[2] * R_OLD[1] + V_TEMP[3]*RP_OLD[1]
            V_REF[2] = V_TEMP[1] * U_OLD[2] + V_TEMP[2] * R_OLD[2] + V_TEMP[3]*RP_OLD[2]
            V_REF[3] = V_TEMP[1] * U_OLD[3] + V_TEMP[2] * R_OLD[3] + V_TEMP[3]*RP_OLD[3]

            #    ! ** This is now the perp. vector in the OMRF **
            V_TEMP[1] = - numpy.sin(3*PIHALF - DEFLECTION)*SINAL
            V_TEMP[2] =   numpy.cos(3*PIHALF - DEFLECTION)
            V_TEMP[3] =   numpy.sin(3*PIHALF - DEFLECTION)*COSAL

            #    ! ** Rotate it to the SRF\
            V_PERP[1] = V_TEMP[1]*U_OLD[1] + V_TEMP[2]*R_OLD[1] + V_TEMP[3]*RP_OLD[1]
            V_PERP[2] = V_TEMP[1]*U_OLD[2] + V_TEMP[2]*R_OLD[2] + V_TEMP[3]*RP_OLD[2]
            V_PERP[3] = V_TEMP[1]*U_OLD[3] + V_TEMP[2]*R_OLD[3] + V_TEMP[3]*RP_OLD[3]

            #    ! ** This is the tangent vector in the OMRF **
            V_TEMP[1] = - numpy.sin(PIHALF - T_INCIDENCE)*SINAL
            V_TEMP[2] =   numpy.cos(PIHALF - T_INCIDENCE)
            V_TEMP[3] =   numpy.sin(PIHALF - T_INCIDENCE)*COSAL

            #    ! ** Rotate it to the SRF.
            V_VEC[1] = V_TEMP[1] * U_OLD[1] + V_TEMP[2] * R_OLD[1] + V_TEMP[3] * RP_OLD[1]
            V_VEC[2] = V_TEMP[1] * U_OLD[2] + V_TEMP[2] * R_OLD[2] + V_TEMP[3] * RP_OLD[2]
            V_VEC[3] = V_TEMP[1] * U_OLD[3] + V_TEMP[2] * R_OLD[3] + V_TEMP[3] * RP_OLD[3]

            #    ! ** Last, we generate U_VEC in the OMRF **
            V_TEMP[1] =   COSAL
            V_TEMP[2] =   .0
            V_TEMP[3] =   SINAL

            #    ! ** rotate to SRF
            U_VEC[1] = V_TEMP[1] * U_OLD[1] + V_TEMP[2] * R_OLD[1] + V_TEMP[3] * RP_OLD[1]
            U_VEC[2] = V_TEMP[1] * U_OLD[2] + V_TEMP[2] * R_OLD[2] + V_TEMP[3] * RP_OLD[2]
            U_VEC[3] = V_TEMP[1] * U_OLD[3] + V_TEMP[2] * R_OLD[3] + V_TEMP[3] * RP_OLD[3]

            #    ! ** All done. Write to the array and leave.
            CENTRAL[1]  =   CENTRAL_OLD[7]
            CENTRAL[2]  =   CENTRAL_OLD[8]
            CENTRAL[3]  =   CENTRAL_OLD[9]
            CENTRAL[4]  =   oe.T_SOURCE * R_OLD[1] + CENTRAL[1]
            CENTRAL[5]  =   oe.T_SOURCE * R_OLD[2] + CENTRAL[2]
            CENTRAL[6]  =   oe.T_SOURCE * R_OLD[3] + CENTRAL[3]
            CENTRAL[7]  =   oe.T_IMAGE *  V_REF[1] + CENTRAL[4]
            CENTRAL[8]  =   oe.T_IMAGE *  V_REF[2] + CENTRAL[5]
            CENTRAL[9]  =   oe.T_IMAGE *  V_REF[3] + CENTRAL[6]
            CENTRAL[10] =   U_VEC[1]
            CENTRAL[11] =   U_VEC[2]
            CENTRAL[12] =   U_VEC[3]
            CENTRAL[13] =   V_VEC[1]
            CENTRAL[14] =   V_VEC[2]
            CENTRAL[15] =   V_VEC[3]
            CENTRAL[16] =   W_VEC[1]
            CENTRAL[17] =   W_VEC[2]
            CENTRAL[18] =   W_VEC[3]
            CENTRAL[19] =   V_REF[1]
            CENTRAL[20] =   V_REF[2]
            CENTRAL[21] =   V_REF[3]
            CENTRAL[22] =   V_PERP[1]
            CENTRAL[23] =   V_PERP[2]
            CENTRAL[24] =   V_PERP[3]

        txt += "       %d    %18.11f     %18.11f     %18.11f \n"%(i+1,CENTRAL[4],CENTRAL[5],CENTRAL[6])
        txt += "          %d'   %18.11f     %18.11f     %18.11f \n"%(i+1,CENTRAL[7],CENTRAL[8],CENTRAL[9])

    txt += TOPLIN
    txt += '********                 E N D                  ***************\n'
    txt += TOPLIN

    if oe.IDUMMY == 0:
        txt += "Warning: Shadow.CompoundOE.sysinfo(): It seems that infor is obtained from start.xx and not from end.xx info\n"

    return(txt)



  def mirinfo(self,title=None):
      """
      Mimics the SHADOW mirinfo
      :return: a text array
      """
      txt = ""
      for i,oe in enumerate(self.list):
          if title == None:
              title1 = "oe %d in compoundOE"%(i+1)
              if self.name != "":
                  title1 += " name: %s "%(self.name)
          else:
              title1 = title
          txt += oe.mirinfo(title=title1)+"\n"
      return txt

  def get_oe_index(self,oe_index):
      """
      returns the pointer to the oe with index oe_index
      :param oe_index:
      :return:
      """
      if oe_index >= self.number_oe():
          print("Error returning element index %d : Not enough optical elements (max index=%d)"%(oe_index,self.number_oe()-1))
          return None
      else:
          tmp = self.list[oe_index]
          return tmp

  def duplicate(self):
      """
      Makes a copy of the compound optical element
      :return:
      """
      new_coe = CompoundOE()
      new_coe.set_name(self.name)
      for i,oe in enumerate(self.list):
          tmp = oe.duplicate()
          new_coe.append(tmp)
      return new_coe


  def add_drift_space_downstream(self,dd):
      """
      Adds empty space to the last element of the compound oe
      :param dd: The distance
      :return: None
      """
      self.list[-1].T_IMAGE += dd

  def add_drift_space_upstream(self,dd):
      """
      Adds empty space before the first element of the compound oe
      :param dd: The distance
      :return: None
      """
      self.list[0].T_SOURCE += dd

  def append(self,item):
    """
    append an instance of Shadow.OW or Shadow.CompoundOE
    :param item: an OE or CompoundOE to append
    :return: the CompoundOE updated with a copy of item appended
    """
    if isinstance(item, OE):
        self.list.append(item.duplicate())
        return self

    if isinstance(item, CompoundOE):
        for i in range(item.number_oe()):
            self.list.append(item.list[i].duplicate())
        return self

    print("Failed to append: object not understood: %s. "%type(item))
    return self

  def append_lens(self,p,q,surface_shape=1,convex_to_the_beam=1,diameter=None,cylinder_angle=None,\
                  prerefl_file=None, refraction_index=1.0, attenuation_coefficient=0.0,\
                  radius=500e-2,interthickness=0.001,use_ccc=0):
      """
      Adds and sets a lens (two interfaces) to the compound optical element

      :param p: distance source-first lens interface
      :param q: distance last lens interface to image plane
      :param surface_shape: 1=sphere 4=paraboloid, 5=plane (other surfaces not yet implamented)
      :param convex_to_the_beam: convexity of the first interface exposed to the beam 0=No, 1=Yes
                                 the second interface has opposite convexity
      :param diameter: lens diameter. Set to None for infinite dimension
      :param cylinder_angle: None=not cylindrical, 0=meridional 90=sagittal
      :param prerefl_file:file name (from prerefl) to get the refraction index. If set
                then the keywords refraction_index and attenuation_coefficient are not used.
      :param refraction_index: n (real) #ignored if prerefl_file points to file.
      :param attenuation_coefficient:mu (real); ignored if prerefl file points to file.      :param radius: lens radius (for pherical, or radius at the tip for paraboloid)
      :param interthickness: lens thickness (distance between the two interfaces at the center of the lenses)
      :param use_ccc 0=set shadow using surface shape (FMIRR=1,4,5), 1=set shadow using CCC coeffs (FMIRR=10)
      :return: self
      """
      oe1 = OE()
      oe2 = OE()

      #units
      oe1.DUMMY = self.user_units_to_cm
      oe2.DUMMY = self.user_units_to_cm

      #set constant values for both interfaces
      oe1.T_INCIDENCE = 0.0
      oe1.T_REFLECTION = 180.0
      oe2.T_INCIDENCE = 0.0
      oe2.T_REFLECTION = 180.0
      oe1.F_REFRAC = 1
      oe2.F_REFRAC = 1

      oe1.F_EXT = 1
      oe2.F_EXT = 1

      # write no output files. If wanted they are written by python in traceCompoundOE
      oe1.FWRITE = 3
      oe2.FWRITE = 3



      if use_ccc:
          oe1.FMIRR = 10
          oe2.FMIRR = 10
      else:
          oe1.FMIRR = surface_shape
          oe2.FMIRR = surface_shape
      #set values that depend on the interface number

      oe1.T_SOURCE = p
      oe1.T_IMAGE = interthickness*0.5
      oe2.T_SOURCE = interthickness*0.5
      oe2.T_IMAGE = q

      #refraction index
      if prerefl_file != None and prerefl_file!= "":
          oe1.F_R_IND = 2 #keyboard in object space, file in image space
          oe1.R_IND_OBJ = 1.0
          oe1.R_ATTENUATION_OBJ = 0.0
          # oe1.FILE_R_IND_IMA = prerefl_file.encode('utf-8')


          if isinstance(prerefl_file,bytes):
            oe1.FILE_R_IND_IMA = prerefl_file
          else:
            oe1.FILE_R_IND_IMA = bytes(prerefl_file, 'utf-8')

          oe2.F_R_IND = 1 #file in object space, keyboard in image space
          # oe2.FILE_R_IND_OBJ = prerefl_file.encode('utf-8')

          if isinstance(prerefl_file,bytes):
            oe2.FILE_R_IND_OBJ = prerefl_file
          else:
            oe2.FILE_R_IND_OBJ = bytes(prerefl_file, 'utf-8')

          oe2.R_IND_IMA = 1.0
          oe2.R_ATTENUATION_IMA = 0.0
      else:
          oe1.F_R_IND = 0
          oe1.R_IND_OBJ = 1.0
          oe1.R_ATTENUATION_OBJ = 0.0
          oe1.R_IND_IMA = refraction_index
          oe1.R_ATTENUATION_IMA = attenuation_coefficient

          oe2.F_R_IND = 0
          oe2.R_IND_OBJ = refraction_index
          oe2.R_ATTENUATION_OBJ = attenuation_coefficient
          oe2.R_IND_IMA = 1.0
          oe2.R_ATTENUATION_IMA = 0.0

      #diameter
      if diameter == None:
          oe1.FHIT_C = 0
          oe2.FHIT_C = 0
      else:
          #if diameter is scalar, set a round aperture
          if isinstance(diameter,(int,float)):
              oe1.FHIT_C = 1
              oe2.FHIT_C = 1
              oe1.FSHAPE = 2 #ellipse
              oe2.FSHAPE = 2
              oe1.RWIDX1 = 0.0
              oe2.RWIDX1 = 0.0
              oe1.RWIDX2 = diameter*0.5
              oe2.RWIDX2 = diameter*0.5
              oe1.RLEN1  = 0.0
              oe2.RLEN1  = 0.0
              oe1.RLEN2  = diameter*0.5
              oe2.RLEN2  = diameter*0.5
          #if diameter is a list or tuple, set a rectanglular aperture
          else:
              oe1.FHIT_C = 1
              oe2.FHIT_C = 1
              oe1.FSHAPE = 1 #rectangle
              oe2.FSHAPE = 1
              oe1.RWIDX1 = 0.5*diameter[0]
              oe2.RWIDX1 = 0.5*diameter[0]
              oe1.RWIDX2 = 0.5*diameter[0]
              oe2.RWIDX2 = 0.5*diameter[0]
              oe1.RLEN1  = 0.5*diameter[1]
              oe2.RLEN1  = 0.5*diameter[1]
              oe1.RLEN2  = 0.5*diameter[1]
              oe2.RLEN2  = 0.5*diameter[1]



      #radii
      if surface_shape == 1: #spherical
        oe1.RMIRR = radius
        oe2.RMIRR = radius
        oe1.CCC =  numpy.array([1.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,-2.0*radius,0.0])
        oe2.CCC =  numpy.array([1.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,-2.0*radius,0.0])
      if surface_shape == 4: #parabolical
        oe1.PARAM = radius
        oe2.PARAM = radius
        oe1.CCC = numpy.array([1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,-2.0*radius,0.0])
        oe2.CCC = numpy.array([1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,-2.0*radius,0.0])

      if surface_shape != 1 and surface_shape != 4 and surface_shape != 5:
          print("Error setting lens: surface shape not implemented")

      if convex_to_the_beam == 1:
          if use_ccc == 0:
              oe1.F_CONVEX = 1
              oe2.F_CONVEX = 0
          else:
              oe1.F_CONVEX = 0 # note that the needed changes are done here, nothing to do in shadow3
              oe2.F_CONVEX = 0
          oe1.CCC[4] = -oe1.CCC[4]
          oe1.CCC[5] = -oe1.CCC[5]
          oe1.CCC[8] = -oe1.CCC[8]
      else:
          if use_ccc == 0:
              oe1.F_CONVEX = 0
              oe2.F_CONVEX = 1
          else:
              oe1.F_CONVEX = 0
              oe2.F_CONVEX = 0 # note that the needed changes are done here, nothing to do in shadow3
          oe2.CCC[4] = -oe2.CCC[4]
          oe2.CCC[5] = -oe2.CCC[5]
          oe2.CCC[8] = -oe2.CCC[8]

      if cylinder_angle == None:
          oe1.FCYL = 0
          oe2.FCYL = 0
      else:
          oe1.FCYL = 1
          oe2.FCYL = 1
          oe1.CIL_ANG = cylinder_angle
          oe2.CIL_ANG = cylinder_angle


      print("appending 2 elements")
      self.append(oe1)
      self.append(oe2)
      return self

  def append_crl(self,p0,q0, nlenses=30, slots_empty=0, radius=500e-2, thickness=625e-4, interthickness=0.001, \
                  surface_shape=1, convex_to_the_beam=1, diameter=None, cylinder_angle=None,\
                  prerefl_file=None, refraction_index=1.0, attenuation_coefficient=0.0,\
                  use_ccc=0):
        """
        Builds the stack of oe for a CRL.

        Notes: if nlenses=0 sets a single "lens" with flat interfaces and no change of refraction index (like empty)

                The refraction index should be input either by i) prerefl_index or ii) refraction_index and
                attenuation_coefficient keywords. The first one is prioritary.

                slots_empty: if different from zero, adds a distance equal to thickness*slots_empty to q0. The
                intention is to simulate a lens that is off but the path should be considered.



        :param p0:distance source-first lens interface
        :param q0:distance last lens interface to image plane
        :param nlenses: number of lenses
        :param slot_empty: number of empty slots (default=0)
        :param radius:lens radius (for pherical, or radius at the tip for paraboloid)
        :param thickness: lens thickness (piling thickness)
        :param interthickness:lens thickness (distance between the two interfaces at the center of the lenses)
        :param surface_shape:1=sphere 4=paraboloid, 5=plane (other surfaces not yet implamented)
        :param convex_to_the_beam:convexity of the first interface exposed to the beam 0=No, 1=Yes
                                 the second interface has opposite convexity
        :param diameter:lens diameter. Set to None for infinite dimension
        :param cylinder_angle:None=not cylindrical, 0=meridional 90=sagittal
        :param prerefl_file:file name (from prerefl) to get the refraction index. If set
                then the keywords refraction_index and attenuation_coefficient are not used.
        :param refraction_index: n (real) #ignored if prerefl_file points to file.
        :param attenuation_coefficient:mu (real); ignored if prerefl file points to file.
        :param use_ccc:0=set shadow using surface shape (FMIRR=1,4,5), 1=set shadow using CCC coeffs (FMIRR=10)
        :return: self
        """

        p_or_q = 0.5*(thickness - interthickness)

        if nlenses == 0: # add an empty lens + a distance (slots_empty-1) for keeping the total distance
            pi = p0 + p_or_q
            qi = q0 + p_or_q + max(slots_empty-1,0)*thickness
            self.append_lens(pi, qi, surface_shape=5, \
                          interthickness=interthickness, \
                          refraction_index=1.0, attenuation_coefficient=0.0, \
                          use_ccc=use_ccc)
        else:
            for i in range(nlenses):
                pi = p_or_q
                qi = p_or_q
                if i == 0:
                    pi += p0
                if i == nlenses-1:
                    qi += q0 + slots_empty*thickness

                self.append_lens(pi, qi, surface_shape=surface_shape, convex_to_the_beam=convex_to_the_beam,\
                              diameter=diameter, cylinder_angle=cylinder_angle, radius=radius,\
                              interthickness=interthickness, prerefl_file=prerefl_file, \
                              refraction_index=refraction_index, attenuation_coefficient=attenuation_coefficient, \
                              use_ccc=use_ccc)
        return self

  def append_transfocator(self,p0,q0, nlenses=[4,8], slots_empty=0, radius=500e-2, thickness=625e-4, \
                  interthickness=0.001, \
                  surface_shape=1, convex_to_the_beam=1, diameter=None, cylinder_angle=None,\
                  prerefl_file=None, refraction_index=1.0, attenuation_coefficient=0.0,\
                  use_ccc=0):
        """
        Builds the stack of oe for a TRANSFOCATOR. A transfocator is a stack of CRLs. append_transfocator
        is therefore very similar to append_crl, but now arguments are lists instead of scalar. However,
        if the value of a particular keyword is an scalar and a list is expected, then it is automatically
        replicated "nslots" times, where nslots=len(nlenses)

        Notes: if nlenses=0 sets a single "lens" with flat interfaces and no change of refraction index (like empty)

                The refraction index should be input either by i) prerefl_index or ii) refraction_index and
                attenuation_coefficient keywords. The first one is prioritary.

                slots_empty: if different from zero, adds a distance equal to thickness*slots_empty to q0. The
                intention is to simulate a lens that is off but the path should be considered.

                Note that all arrays must be "list". If you are using numpy arrays, convert them:  array.tolist()



        :param p0 (list):distance previous continuation plane to first lens for each CRL
                (usually [p,0,0,...]
        :param q0 (scalar):distance last lens in each CRLto continuation plane
        :param nlenses (list): number of lenses
        :param slots_empty (list): number of empty slots
        :param radius (list):lens radius (for pherical, or radius at the tip for paraboloid)
        :param thickness (list): lens thickness (piling thickness)
        :param interthickness (list):lens thickness (distance between the two interfaces at the center of the lenses)
        :param surface_shape (list):1=sphere 4=paraboloid, 5=plane (other surfaces not yet implamented)
        :param convex_to_the_beam (list):convexity of the first interface exposed to the beam 0=No, 1=Yes
                                 the second interface has opposite convexity
        :param diameter (list):lens diameter. Set to None for infinite dimension
        :param cylinder_angle (list):None=not cylindrical, 0=meridional 90=sagittal
        :param prerefl_file (list):file name (from prerefl) to get the refraction index. If set
                then the keywords refraction_index and attenuation_coefficient are not used.
        :param refraction_index (list): n (real) #ignored if prerefl_file points to file.
        :param attenuation_coefficient (list):mu (real); ignored if prerefl file points to file.
        :param use_ccc (scalar):0=set shadow using surface shape (FMIRR=1,4,5), 1=set shadow using CCC coeffs (FMIRR=10)
        :return: self
        """

        # replicate inputs when they are scalar
        nslots = len(nlenses)

        if isinstance(p0, list) == False: p0 = [ p0 for i in range(nslots)]
        if isinstance(q0, list) == False: q0 = [ q0 for i in range(nslots)]
        if isinstance(slots_empty, list) == False: slots_empty = [ slots_empty for i in range(nslots)]
        if isinstance(radius, list) == False: radius = [ radius for i in range(nslots)]
        if isinstance(thickness, list) == False: thickness = [ thickness for i in range(nslots)]
        if isinstance(interthickness, list) == False: interthickness = [ interthickness for i in range(nslots)]
        if isinstance(surface_shape, list) == False: surface_shape = [ surface_shape for i in range(nslots)]
        if isinstance(convex_to_the_beam, list) == False: convex_to_the_beam = [ convex_to_the_beam for i in range(nslots)]
        if isinstance(diameter, list) == False: diameter = [ diameter for i in range(nslots)]
        if isinstance(cylinder_angle, list) == False: cylinder_angle = [ cylinder_angle for i in range(nslots)]
        if isinstance(prerefl_file, list) == False: prerefl_file = [ prerefl_file for i in range(nslots)]
        if isinstance(refraction_index, list) == False: refraction_index = [ refraction_index for i in range(nslots)]
        if isinstance(attenuation_coefficient, list) == False:
            attenuation_coefficient = [ attenuation_coefficient for i in range(nslots)]



        for i in range(len(nlenses)):
            self.append_crl(p0[i], q0[i], nlenses=nlenses[i], slots_empty=slots_empty[i], \
                          radius=radius[i], thickness=thickness[i], interthickness=interthickness[i], \
                          surface_shape=surface_shape[i],convex_to_the_beam=convex_to_the_beam[i],\
                          diameter=diameter[i], cylinder_angle=cylinder_angle[i],\
                          prerefl_file=prerefl_file[i],refraction_index=refraction_index[i], \
                          attenuation_coefficient=attenuation_coefficient[i],\
                          use_ccc=0)
        return self

  def append_kb(self, p0, q0, grazing_angles_mrad=[3.0,3.0],separation=100.0, mirror_orientation_angle=0,\
          focal_positions=[0,0],shape=[2,2],\
          dimensions1=[0,0],dimensions2=[0,0],\
          reflectivity_kind=[0,0],reflectivity_files=["",""],surface_error_files=["",""]):
      """
      Appends a KB (Kirkpatrick-Baez) system
      First mirror is focusing in vertical plane, second mirror focusses in horizontal plane.

      Note that SHADOW rotates the image plane because the second mirror has mirror orientation angle 90 ded


      :param p0: distance from previous source plane (continuation plane) to center of KB (!!!)
      :param q0: distance from center of KB (!!!) to image plane (continuation plane)
      :param grazing_angles_mrad: grazing angle in mrad for both mirrors. Default:  grazing_angles_mrad=[3.0,3.0]
      :param separation: separation between the mirrors from center of first mirror (VFM) to center of second one (HFM).
                                ()Default=100). The continuation plane is set in the middle.
      :param mirror_orientation_angle: set the mirror orientation angle with respect to the previous o.e. for the
                                first mirror of the  KB
      :param focal_positions: the focal positions [p_focal,q_focal] measured from the center of KB.
                                If set to [0,0] then uses p0 and q0
      :param shape: the shape code for the surfaces 1=spherica, 2=elliptical. Default: shape=[2,2]
      :param dimensions1: the dimensions [width,length] for the first mirror. Default: [0,0] meaning infinite dimensions.
      :param dimensions2: the dimensions [width,length] for the second  mirror. Default: [0,0] meaning infinite dimensions.
      :param reflectivity_kind: flag for reflectivity: 0=ideal reflector, 1=mirror, 2=multilayer. Default=[0,0]
                                If reflectivity is set to mirror or multilayer, the corresponding file must be entered in
                                the reflectivity_files keyword.
      :param reflectivity_files: the reflectivity files, if needed. If mirror, the file must have been created
                                by prerefl. If multilayer, the file must come from pre_mlayer.
      :param surface_error_files: Set to file names containing the surface error mesh.
                                Default: surface_error_files=["",""] which means that no surface error is considered.
      :return: self
      """
      oe1 = OE()
      oe2 = OE()

      #units
      oe1.DUMMY = self.user_units_to_cm
      oe2.DUMMY = self.user_units_to_cm

      #incidence angles
      oe1.T_INCIDENCE = 90.0 - grazing_angles_mrad[0]*1e-3*180.0/numpy.pi
      oe1.T_REFLECTION = oe1.T_INCIDENCE
      oe2.T_INCIDENCE = 90.0 - grazing_angles_mrad[1]*1e-3*180.0/numpy.pi
      oe2.T_REFLECTION = oe2.T_INCIDENCE
      # orientation
      oe1.ALPHA = mirror_orientation_angle # first VFM
      oe2.ALPHA = 90.0 # second HFM

      # distances
      oe1.T_SOURCE = p0 - 0.5*separation
      oe1.T_IMAGE = 0.5*separation
      oe2.T_SOURCE = 0.5*separation
      oe2.T_IMAGE = q0 - 0.5*separation

      # mirror shape
      oe1.FMIRR = shape[0] #1=spherical, 2=elliptical
      oe2.FMIRR = shape[1]
      oe1.FCYL = 1
      oe2.FCYL = 1
      oe1.CIL_ANG = 0
      oe2.CIL_ANG = 0

      # auto focusing

      #focal positions from center of kb
      if focal_positions[0] != 0:
          pfocal = focal_positions[0]
      else:
          pfocal = p0

      if focal_positions[1] != 0:
          qfocal = focal_positions[1]
      else:
          qfocal = q0

      oe1.F_EXT = 0 # internal
      oe1.F_DEFAULT = 0
      oe1.SSOUR = pfocal - 0.5*separation
      oe1.SIMAG = 0.5*separation + qfocal
      oe1.THETA = oe1.T_INCIDENCE

      oe2.F_EXT = 0 # focii coincident=no
      oe2.F_DEFAULT = 0
      oe2.SSOUR = pfocal + 0.5*separation
      oe2.SIMAG = qfocal - 0.5*separation



      oe2.THETA = oe2.T_INCIDENCE

      oe1.F_REFLEC = 0  # 0=skip reflectivity, 1=Full dependence
      oe2.F_REFLEC = 0
      oe1.F_REFL = 0 #prerefl
      oe2.F_REFL = 0
      # oe1.FILE_REFL = ''.encode('utf-8')
      # oe2.FILE_REFL = ''.encode('utf-8')
      oe1.FILE_REFL = bytes('', 'utf-8')
      oe2.FILE_REFL = bytes('', 'utf-8')

      if dimensions1 != [0,0]:
          oe1.FHIT_C = 1  #  mirror dimensions finite: yes (1), no(0).
          oe1.FSHAPE = 1  # rectangle
          oe1.RWIDX1 = 0.5 * dimensions1[0]
          oe1.RWIDX2 = 0.5 * dimensions1[0]
          oe1.RLEN1  = 0.5 * dimensions1[1]
          oe1.RLEN2  = 0.5 * dimensions1[1]
      else:
          oe1.FHIT_C = 0  #  mirror dimensions finite: yes (1), no(0).

      if dimensions2 != [0,0]:
          oe2.FHIT_C = 1  #  mirror dimensions finite: yes (1), no(0).
          oe2.FSHAPE = 1  # rectangle
          oe2.RWIDX1 = 0.5 * dimensions2[0]
          oe2.RWIDX2 = 0.5 * dimensions2[0]
          oe2.RLEN1  = 0.5 * dimensions2[1]
          oe2.RLEN2  = 0.5 * dimensions2[1]

      else:
          oe2.FHIT_C = 0  #  mirror dimensions finite: yes (1), no(0).

      #
      # reflectivity
      #
      if reflectivity_kind[0] == 0:  # ideal
          oe1.F_REFLEC = 0

      if reflectivity_kind[0] == 1:  # mirror
          oe1.F_REFLEC = 1
          oe1.F_REFL = 0   # prerefl mirror
          # oe1.FILE_REFL = reflectivity_files[0].encode('utf-8')
          oe1.FILE_REFL = reflectivity_files[0]

      if reflectivity_kind[0] == 2:  # multilayer
          oe1.F_REFLEC = 1
          oe1.F_REFL = 2   # multilayer
          if isinstance(reflectivity_files[0],bytes):
            oe1.FILE_REFL = reflectivity_files[0]
          else:
            oe1.FILE_REFL = bytes(reflectivity_files[0], 'utf-8')

      if reflectivity_kind[1] == 0:  # ideal
          oe2.F_REFLEC = 0

      if reflectivity_kind[1] == 1:  # mirror
          oe2.F_REFLEC = 1
          oe2.F_REFL = 0   # prerefl mirror
          if isinstance(reflectivity_files[1], bytes):
            oe2.FILE_REFL = reflectivity_files[1]
          else:
            oe2.FILE_REFL = bytes(reflectivity_files[1], 'utf-8')

      if reflectivity_kind[1] == 2:  # multilayer
          oe2.F_REFLEC = 1
          oe2.F_REFL = 2   # multilayer
          # oe2.FILE_REFL = reflectivity_files[1].encode('utf-8')
          if isinstance(reflectivity_files[1],bytes):
            oe2.FILE_REFL = reflectivity_files[1]
          else:
            oe2.FILE_REFL = bytes(reflectivity_files[1], 'utf-8')

      #
      #surface errors
      #
      if surface_error_files[0] != "":
        oe1.F_RIPPLE = 1
        # oe1.FILE_RIP = surface_error_files[0].encode('utf-8')
        if isinstance(surface_error_files[0],bytes):
            oe1.FILE_RIP = surface_error_files[0]
        else:
            oe1.FILE_RIP = bytes(surface_error_files[0], 'utf-8')
        oe1.F_G_S = 2
      else:
        oe1.F_RIPPLE = 0

      if surface_error_files[1] != "":
        oe2.F_RIPPLE = 1
        # oe2.FILE_RIP = surface_error_files[1].encode('utf-8')
        if isinstance(surface_error_files[1],bytes):
            oe2.FILE_RIP = surface_error_files[1]
        else:
            oe2.FILE_RIP = bytes(surface_error_files[1], 'utf-8')
        oe2.F_G_S = 2
      else:
        oe2.F_RIPPLE = 0

      # write no output files. If wanted they are written by python in traceCompoundOE
      oe1.FWRITE = 3
      oe2.FWRITE = 3

      self.append(oe1)
      self.append(oe2)

      return self


  def append_monochromator_double_crystal(self, p0, q0, photon_energy_ev=14000,separation=0.0,\
          dimensions1=[0,0],dimensions2=[0,0],\
          reflectivity_file=""):
      """
      Appends a double crystal monochromator (with plane crystals)

      :param p0: distance from previous source plane (continuation plane) to center of doble crystal monochrtomator
      :param q0: distance from center of double crystal monochromator to image plane (continuation plane)
      :param set_photon_energy: photon energy in eV to set the monochromator
      :param separation: separation between the crystals (Default=0). The continuation plane is set in the middle.
      :param dimensions1: the dimensions [width,length] for the first mirror. Default: [0,0] meaning infinite dimensions.
      :param dimensions2: the dimensions [width,length] for the second  mirror. Default: [0,0] meaning infinite dimensions.
      :param reflectivity_files: the reflectivity files as created by bragg
      :return: self
      """
      oe1 = OE()
      oe2 = OE()

      #units
      oe1.DUMMY = self.user_units_to_cm
      oe2.DUMMY = self.user_units_to_cm

      # #incidence angles
      # oe1.T_INCIDENCE = 90.0 - grazing_angles_mrad[0]*1e-3*180.0/numpy.pi
      # oe1.T_REFLECTION = oe1.T_INCIDENCE
      # oe2.T_INCIDENCE = 90.0 - grazing_angles_mrad[1]*1e-3*180.0/numpy.pi
      # oe2.T_REFLECTION = oe2.T_INCIDENCE

      oe1.F_CRYSTAL = 1
      oe2.F_CRYSTAL = 1

      # orientation
      oe1.ALPHA = 0.0 # first VFM
      oe2.ALPHA = 180.0 # second HFM
      #
      # distances
      oe1.T_SOURCE = p0 - 0.5*separation
      oe1.T_IMAGE = 0.5*separation
      oe2.T_SOURCE = 0.5*separation
      oe2.T_IMAGE = q0 - 0.5*separation
      #
      # crystal shape 5 (plane)
      oe1.FMIRR = 5
      oe2.FMIRR = 5

      oe1.F_CENTRAL = 1
      oe2.F_CENTRAL = 1
      oe1.F_PHOT_CENT = 0 # eV
      oe2.F_PHOT_CENT = 0 # eV
      oe1.PHOT_CENT = photon_energy_ev
      oe2.PHOT_CENT = photon_energy_ev

      oe1.T_INCIDENCE = 0.0    # not used (autosetting)
      oe1.T_REFLECTION = 0.0   # not used (autosetting)
      oe2.T_INCIDENCE = 0.0    # not used (autosetting)
      oe2.T_REFLECTION = 0.0   # not used (autosetting)


      # oe1.FILE_REFL = reflectivity_file.encode('utf-8')
      # oe2.FILE_REFL = reflectivity_file.encode('utf-8')
      if isinstance(reflectivity_file,bytes):
          oe1.FILE_REFL = reflectivity_file
          oe2.FILE_REFL = reflectivity_file
      else:
          oe1.FILE_REFL = bytes(reflectivity_file, 'utf-8')
          oe2.FILE_REFL = bytes(reflectivity_file, 'utf-8')

      #
      #
      if dimensions1 != [0,0]:
          oe1.FHIT_C = 1  #  mirror dimensions finite: yes (1), no(0).
          oe1.FSHAPE = 1  # rectangle
          oe1.RWIDX1 = 0.5 * dimensions1[0]
          oe1.RWIDX2 = 0.5 * dimensions1[0]
          oe1.RLEN1  = 0.5 * dimensions1[1]
          oe1.RLEN2  = 0.5 * dimensions1[1]
      else:
          oe1.FHIT_C = 0  #  mirror dimensions finite: yes (1), no(0).

      if dimensions2 != [0,0]:
          oe2.FHIT_C = 1  #  mirror dimensions finite: yes (1), no(0).
          oe2.FSHAPE = 1  # rectangle
          oe2.RWIDX1 = 0.5 * dimensions2[0]
          oe2.RWIDX2 = 0.5 * dimensions2[0]
          oe2.RLEN1  = 0.5 * dimensions2[1]
          oe2.RLEN2  = 0.5 * dimensions2[1]

      else:
          oe2.FHIT_C = 0  #  mirror dimensions finite: yes (1), no(0).


      # write no output files. If wanted they are written by python in traceCompoundOE
      oe1.FWRITE = 3
      oe2.FWRITE = 3

      self.append(oe1)
      self.append(oe2)

      return self

  def dump_start_files(self,offset=0):
    root='start'
    for i,oe in enumerate(self.list):
      oe.write('%s.%02d'%(root,i+1+offset))
      print('File written to disk: %s.%02d\n'%(root,i+1+offset))

  def dump_end_files(self,offset=0):
    root='end'
    for i,oe in enumerate(self.list):
      oe.write('%s.%02d'%(root,i+1+offset))
      print('File written to disk: %s.%02d\n'%(root,i+1+offset))

  def dump_systemfile(self,offset=0):
    f = open('systemfile.dat','w')
    for i,oe in enumerate(self.list):
      f.write('start.%02d\n' %(i+1+offset))
    f.close()
    print('File written to disk: systemfile.dat')

  def length(self):
    length = 0.0
    for i,oe in enumerate(self.list):
        length += oe.T_SOURCE + oe.T_IMAGE
    return length



class Source(ShadowLib.Source):
    def __init__(self):
        ShadowLib.Source.__init__(self)

    def to_dictionary(self):
        """
        returns a python dictionary of the Shadow.Source instance
        :return: a dictionary
        """
        mem = inspect.getmembers(self)
        mydict = {}
        for i,var in enumerate(mem):
            if var[0].isupper():
                mydict[var[0]]= var[1]
        return(mydict)

    def duplicate(self):
        """
        makes a copy of the source
        :return: new instance of Shadow.Source()
        """
        src_new = Source()
        mem = inspect.getmembers(self)
        for i,var in enumerate(mem):
          if var[0].isupper():
            setattr(src_new,var[0],var[1])
        return(src_new)

    #Gaussian source
    def set_divergence_gauss(self, sigmaxp, sigmazp):
        """
        sets Gaussian source in divergence space
        :param sigmaxp: SIGDIX for SHADOW
        :param sigmazp: SIGDIZ for SHADOW
        :return: self
        """
        self.FDISTR = 3
        self.HDIV1 = 1.0
        self.HDIV2 = 1.0
        self.VDIV1 = 1.0
        self.VDIV2 = 1.0
        self.SIGDIX = sigmaxp
        self.SIGDIZ = sigmazp
        return self

    def set_spatial_gauss(self,sigmax, sigmaz):
        """
        sets Gaussian source in real space
        :param sigmax: SIGMAX for SHADOW.
        :param sigmaz: SIGMAZ for SHADOW.
        :return: self
        """
        self.FSOUR = 3
        self.SIGMAX = sigmax
        self.SIGMAZ = sigmaz
        return self

    def set_gauss(self,sigmax,sigmaz,sigmaxp,sigmazp):
        """
        Sets a Gaussian source in both real and divergence spaces
        :param sigmax: SIGMAX for SHADOW.
        :param sigmaz: SIGMAZ for SHADOW.
        :param sigmaxp: SIGDIX for SHADOW.
        :param sigmazp: SIGDIZ for SHADOW.
        :return: self
        """
        self.set_divergence_gauss(sigmaxp,sigmazp)
        self.set_spatial_gauss(sigmax,sigmaz)
        return self

    def set_energy_monochromatic(self,emin):
        """
        Sets a single energy line for the source (monochromatic)
        :param emin: the energy in eV
        :return: self
        """
        self.F_COLOR =  1
        self.F_PHOT =  0 #eV
        self.PH1 = emin
        return self

    def set_energy_box(self,emin,emax):
        """
        Sets a box energy distribution for the source (monochromatic)
        :param emin: minimum energy in eV
        :param emax: maximum energy in eV
        :return: self
        """
        self.F_COLOR =  3
        self.F_PHOT =  0 #eV
        self.PH1 = emin
        self.PH2 = emax
        return self

    def set_pencil(self):
        """
        Sets a pencil beam (zero size, zero divergence)
        :return:
        """
        self.FSOUR = 0
        self.FDISTR = 1
        self.HDIV1 = 0.0
        self.HDIV2 = 0.0
        self.VDIV1 = 0.0
        self.VDIV2 = 0.0
        return self

    def apply_gaussian_undulator(self, undulator_length_in_m=1.0,user_unit_to_m=1e2, verbose=1, und_e0=None):
        """
        Convolves the already defined Gaussian source (for the electrons) with the photon emission
        for an undulator.

        :param undulator_length_in_m:
        :param user_unit_to_m:
        :param verbose: set to 0 for silent output
        :param und_e0: the setting photon energy in eV, if undefined (None) reads from SHADOW PH1 variable
        :return: self
        """

        #user_unit_to_m = 1e-2
        codata_c = numpy.array(299792458.0)
        codata_h = numpy.array(6.62606957e-34)
        codata_ec = numpy.array(1.602176565e-19)
        m2ev = codata_c*codata_h/codata_ec 

        if und_e0 == None:
            if self.F_COLOR ==  3: # box
                und_e0 = 0.5*(self.PH1+self.PH2)
            else:
                und_e0 = self.PH1


        lambda1 = m2ev/und_e0
        if verbose:
            print("---adding undulator radiation in Gaussian approximation:")
            print('')
            print("   photon energy [eV]: %f "%(und_e0))
            print("   photon wavelength [A]: %f "%(lambda1*1e10))
            
        # calculate sizes of the photon undulator beam
        # see formulas 25 & 30 in Elleaume (Onaki & Elleaume)
        s_phot = 2.740/(4e0*numpy.pi)*numpy.sqrt(undulator_length_in_m*lambda1)
        sp_phot = 0.69*numpy.sqrt(lambda1/undulator_length_in_m)
    
        if verbose:
            print('')
            print('   RMS electon size H/V [um]: '+
                         repr(self.SIGMAX*1e6*user_unit_to_m)+ ' /  '+
                         repr(self.SIGMAZ*1e6*user_unit_to_m) )
            print('   RMS electon divergence H/V[urad]: '+
                         repr(self.SIGDIX*1e6)+ ' /  '+
                         repr(self.SIGDIZ*1e6)  )
            print('')
            print('   RMS radiation size [um]: '+repr(s_phot*1e6))
            print('   RMS radiation divergence [urad]: '+repr(sp_phot*1e6))
            print('')
            print('   Photon beam (convolution): ')
    
        photon_h = numpy.sqrt( self.SIGMAX**2 + (s_phot/user_unit_to_m)**2)
        photon_v = numpy.sqrt( self.SIGMAZ**2 + (s_phot/user_unit_to_m)**2)
        photon_hp = numpy.sqrt(self.SIGDIX**2 + sp_phot**2 )
        photon_vp = numpy.sqrt(self.SIGDIZ**2 + sp_phot**2 )

        if verbose:
            print('   RMS size H/V [um]: '+ repr(photon_h*1e6*user_unit_to_m) + '  /  '+repr(photon_v*1e6*user_unit_to_m))
            print('   RMS divergence H/V [um]: '+ repr(photon_hp*1e6) + '  /  '+repr(photon_vp*1e6))
 
        self.SIGMAX = photon_h
        self.SIGMAZ = photon_v
        self.SIGDIX = photon_hp
        self.SIGDIZ = photon_vp

        return self

 
    def sourcinfo(self,title=None):
        '''
        mimics SHADOW sourcinfo postprocessor. Returns a text array.
        :return: a text string
        '''

        txt = ''
        TOPLIN = '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n'

        TSPATIAL = {}
        TDEPTH = {}
        TANG = {}
        TPHOT = {}
        TPOL = {}

        TSPATIAL['1'] = 'POINT       '
        TSPATIAL['4'] = 'GAUSSIAN    '
        TSPATIAL['2'] = 'RECTANGULAR '
        TSPATIAL['3'] = 'ELLIPTICAL  '
        TSPATIAL['5'] = 'PHASE SPACE ELLIPSE'
        TDEPTH['1']   = 'DEPTH OFF   '
        TDEPTH['2']   = 'DEPTH ON    '
        TDEPTH['3']   = 'RECTANGULAR '
        TDEPTH['4']   = 'GAUSSIAN    '
        TDEPTH['5']   = 'SYNCHROTRON '
        TANG['1']     = 'UNIFORM     '
        TANG['2']     = 'LAMBERTIAN  '
        TANG['3']     = 'GAUSSIAN    '
        TANG['4']     = 'SYNCHROTRON '
        TANG['5']     = 'CONICAL     '
        TANG['6']     = 'SYNCHROTRON (exact)'
        TANG['7']     = 'PHASE SPACE ELLIPSE'
        TPHOT['1']    = 'PHOTON OFF  '
        TPHOT['2']    = 'PHOTON ON   '
        TPHOT['3']    = 'SINGLE LINE '
        TPHOT['4']    = 'MULTI LINE  '
        TPHOT['5']    = 'BOX DISTR.  '
        TPOL['1']     = 'SR PARALLEL '
        TPOL['2']     = 'SR PERPEND  '
        TPOL['3']     = 'SR TOTAL    '



        txt += TOPLIN
        txt += '**************  S O U R C E       D E S C R I P T I O N  **************\n'
        if title == None:
            txt += '\n\n'
        else:
            txt += title+'\n'
        txt += TOPLIN


        # !C
        # !C Type of computation
        # !C
        if self.FGRID == 0:
            txt += 'Random Source\n'
        if self.FGRID == 1:
            txt += 'Grid Source\n'
        if self.FGRID == 2:
            txt += 'Mixed Type Source. Spatial: GRID  , directions: RANDOM\n'
        if self.FGRID == 3:
            txt += 'Mixed Type Source. Spatial: RANDOM, directions: GRID\n'
        if self.FGRID == 4:
            txt += 'Phase space ellipses. RANDOM around each ellipse.\n'
        if self.FGRID == 5:
            txt += 'Phase space ellipses. GRID around each ellipse.\n'

        txt += 'Generated total %d rays.\n'%(self.NPOINT)

        # !C
        # !C Spatial type and values
        # !C

        if self.FSOURCE_DEPTH == 1:
            txt += 'Source assumed BIDIMENSIONAL (flat).\n'
        else:
            txt += 'Source assumed TRIDIMENSIONAL.\n'


        if self.F_WIGGLER == 1:
            txt += 'Source Type: Wiggler\n'
            txt += 'File with Cumulative Distribution Function: %s\n'%self.FILE_TRAJ.strip().decode()
        elif self.F_WIGGLER == 2:
            txt += 'Source Type: Undulator\n'
            txt += 'File with Cumulative Distribution Function: %s\n'%self.FILE_TRAJ.strip().decode()
        else:
            txt += 'Source Spatial Characteristics: '+TSPATIAL[str(1+self.FSOUR)]+'\n'

        if ((self.FSOUR == 1) or (self.FSOUR)) == 2:
            txt += 'Source width: %17.9g and Height: %17.9g'%(self.WXSOU,self.WZSOU)

        if ((self.FSOUR == 3) or (self.FSOUR == 4)):
            txt += 'Sigma X: %17.9g and Sigma Z: %17.9g'%(self.SIGMAX,self.SIGMAZ)


        txt += '\n'


        if self.FSOURCE_DEPTH == 2:
            txt += 'Depth:  UNIFORM      Value   : %17.9g\n'%(self.WYSOU)
        if self.FSOURCE_DEPTH == 3:
            txt += 'Depth:  GAUSSIAN.    Sigma-y : %17.9g\n'%(self.SIGMAY)
        if self.FSOURCE_DEPTH == 4:
            txt += 'Depth:  SYNCHROTRON SOURCE.\n'

        # !C
        # !C Source Emission
        # !C
        if self.F_WIGGLER == 0:
            txt += TOPLIN
            txt += 'Source Emission Characteristics\n'
            txt += 'Distribution Type: %s \n'%(TANG[str(self.FDISTR)])
            if self.FDISTR != 5:
                txt += 'Distribution Limits. +X : %17.9g   -X: %17.9g   rad\n'%(self.HDIV1,self.HDIV2)
                txt += '                     +Z : %17.9g   -Z: %17.9g   rad\n'%(self.VDIV1,self.VDIV2)

            if ((self.FDISTR == 3) or (self.FDISTR == 7)):
                txt += 'Horiz. StDev : %17.9g\n'%(self.SIGDIX)
                txt += 'Verti. StDev : %17.9g\n'%(self.SIGDIZ)

            if self.FDISTR == 5:
                txt += 'Cone Outer Aperture : %17.9g Inner Aperture : %17.9g \n'%(self.CONE_MAX,self.CONE_MIN)

        # !C
        # !C Synchrotron Case
        # !C
        if ((self.FDISTR == 4) or (self.FDISTR == 6)):
            if self.F_WIGGLER == 0:
                txt += 'Magnetic Radius = %17.9g  m.  Beam Energy = %17.9g  GeV.\n'%(self.R_MAGNET,self.BENER)
            txt += 'Beam Emittancies. EPSI_X: %17.9g EPSI_Z: %17.9g \n'%(self.EPSI_X,self.EPSI_Z)
            txt += 'Distance from Waist.   X: %17.9g      Z: %17.9g \n'%(self.EPSI_DX,self.EPSI_DZ)
            txt += 'Polarization Used: %s \n'%(TPOL[str(self.F_POL)])
        # !C
        # !C Photon Energy
        # !C
        txt += TOPLIN
        if self.F_COLOR != 0:
            photonArray = {}
            WAVE = {}
            photonArray['1'] = self.PH1
            photonArray['2'] = self.PH2
            photonArray['3'] = self.PH3
            photonArray['4'] = self.PH4
            photonArray['5'] = self.PH5
            photonArray['6'] = self.PH6
            photonArray['7'] = self.PH7
            photonArray['8'] = self.PH8
            photonArray['9'] = self.PH9
            photonArray['10'] = self.PH10
            WAVE['1'] = self.PH1
            WAVE['2'] = self.PH2
            WAVE['3'] = self.PH3
            WAVE['4'] = self.PH4
            WAVE['5'] = self.PH5
            WAVE['6'] = self.PH6
            WAVE['7'] = self.PH7
            WAVE['8'] = self.PH8
            WAVE['9'] = self.PH9
            WAVE['10'] = self.PH10

            if self.F_WIGGLER == 0:
                txt += 'Source Photon Energy Distribution: %s \n'%(TPHOT[str(2+self.F_COLOR)])
            codata_h = numpy.array(6.62606957e-34)
            codata_ec = numpy.array(1.602176565e-19)
            codata_c = numpy.array(299792458.0)
            TOANGS = codata_h*codata_c/codata_ec*1e10
            if self.F_COLOR <=2:
                if self.F_COLOR == 1:
                    if (self.F_PHOT == 0): WAVE['1'] = TOANGS/photonArray['1']
                    if (self.F_PHOT == 1): photonArray['1'] = TOANGS/WAVE['1']
                    txt += 'Photon Energy: %12.5g  eV, or %12.5g  Angs. \n'%(photonArray['1'],WAVE['1'])
                else:
                    for J in range(1,1+self.N_COLOR):
                        if (self.F_PHOT == 0): WAVE[str(J)] = TOANGS/photonArray[str(J)]
                        if (self.F_PHOT == 1): photonArray[str(J)] = TOANGS/WAVE[str(J)]
                        txt += 'Photon Energy: %12.5g  eV, or %12.5g  Angs.'%(photonArray[str(J)],WAVE[str(J)])
            else:
                if (self.F_PHOT == 0): WAVE['1'] = TOANGS/photonArray['1']
                if (self.F_PHOT == 1): photonArray['1'] = TOANGS/WAVE['1']
                if (self.F_PHOT == 0): WAVE['2'] = TOANGS/photonArray['2']
                if (self.F_PHOT == 1): photonArray['2'] = TOANGS/WAVE['2']
                txt += 'From Photon Energy: %17.9g eV or %17.9g Angs.\n'%(photonArray['1'],WAVE['1'])
                txt += ' to  Photon Energy: %17.9g eV or %17.9g Angs.\n'%(photonArray['2'],WAVE['2'])

        if self.F_WIGGLER == 0:
            if self.F_POLAR:
                txt += 'Angular difference in phase is %12.5g \n'%(self.POL_ANGLE*180.0/numpy.pi)
                txt += 'Degree of polarization is %12.5g \n'%(self.POL_DEG)
                if self.F_COHER == 0:
                    txt += 'Source points have INCOHERENT phase.\n'
                else:
                    txt += 'Source points have COHERENT phase.\n'

        #
        # optimization
        #
        if self.F_BOUND_SOUR > 0:
            txt += 'Source optimization (rejection, variance reduction) used: \n'
            txt += '    total number of rays been created: %d \n'%(self.NTOTALPOINT)
            txt += '    accepted rays (stored): %d \n'%self.NPOINT
            txt += '    rejected:               %d \n'%(self.NTOTALPOINT-self.NPOINT)
            txt += '    created/accepted ratio: %f \n'%(float(self.NTOTALPOINT)/float(self.NPOINT))

            if self.F_BOUND_SOUR == 1:
                txt += '    file with phase-space volume: '+self.FILE_BOUND.strip().decode()+'\n'
            else:
                txt += '    file with slit/acceptance: '+self.FILE_BOUND.strip().decode()+'\n'


        txt += TOPLIN
        txt += '***************                 E N D                  ***************\n'
        txt += TOPLIN
        return (txt)


#-----------------------------------------------
# TESTS
#-----------------------------------------------

def test_only_source():
    print("------------------ test_only_source() --------------------------------------------")

    src = Source()

    #set Gaussian source
    sh, sv, shp, svp = 100e-4, 10e-4, 10e-6, 1e-6
    src.set_gauss(sh, sv, shp, svp)
    src.write('start.00')

    #run shadow source
    beam = Beam()
    beam.genSource(src)

    beam.write('begin.dat')
    src.write('end.00')

    #analyze source results
    print('Intensity source, all, good and lost rays: %f, %f, %f , '%\
          (beam.intensity(nolost=0),beam.intensity(nolost=1),beam.intensity(nolost=2) ))

    #print( src.to_dictionary() )
    print(src.sourcinfo(title='sourcinfo in python'))

    #4 histograms
    ticket_h = beam.histo1(col=1, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref="Yes",calculate_widths=2)
    print('Histogram FW25M col1: %f: '%(ticket_h['fw25%m']))
    print('Histogram FWHM col1: %f, stdev: %f, initial: %f: '%(ticket_h['fwhm'],ticket_h['fwhm']/2*numpy.sqrt(2*numpy.log(2)),sh))
    print('Histogram FW75M col1: %f: '%(ticket_h['fw75%m']))


    ticket_h = beam.histo1(col=3, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref="Yes")
    print('Histogram FWHM col3: %f, stdev: %f, initial: %f: '%(ticket_h['fwhm'],ticket_h['fwhm']/2*numpy.sqrt(2*numpy.log(2)),sv))
    ticket_h = beam.histo1(col=4, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref="Yes")
    print('Histogram FWHM col4: %f, stdev: %f, initial: %f: '%(ticket_h['fwhm'],ticket_h['fwhm']/2*numpy.sqrt(2*numpy.log(2)),shp))
    ticket_h = beam.histo1(col=6, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref="Yes")
    print('Histogram FWHM col6: %f, stdev: %f, initial: %f: '%(ticket_h['fwhm'],ticket_h['fwhm']/2*numpy.sqrt(2*numpy.log(2)),svp))

    #side histograms using histo2
    ticket_p = beam.histo2(1,3,nbins=500,ref="Yes",calculate_widths=2)
    print('Histogram FW25M: col1: %f, col3: %f: '%(ticket_p['fw25%m_h'],ticket_p['fw25%m_v']))
    print('Histogram FWHM: col1: %f, col3: %f: '%(ticket_p['fwhm_h'],ticket_p['fwhm_v']))
    print('Histogram FW75M: col1: %f, col3: %f: '%(ticket_p['fw75%m_h'],ticket_p['fw75%m_v']))

    return beam

def test_source_and_trace():
    print("------------------ test_source_and_trace() --------------------------------------------")

    beam = Beam()

    src = Source()
    beam.genSource(src)

    oe1 = OE()

    beam.traceOE(oe1,1)

    oe1.write('end.01')
    beam.write('star.01')

    #analysis
    #print(beam.getshonecol(11,nolost=1))
    print('Intensity after oe 1 all, good and lost rays: %f, %f, %f , '%\
          (beam.intensity(nolost=0),beam.intensity(nolost=1),beam.intensity(nolost=2) ))

    #print( oe1.to_dictionary() )
    print(src.sourcinfo(title='sourcinfo in python'))
    print(oe1.mirinfo(title='mirinfo in python'))

    ticket = beam.histo1(col=3, nbins =11, nolost=1, write='HISTO1', xrange=[-0.055, 0.055], ref="Yes")

    if ticket['error'] == 0:
        beam.write('star.01')
        bins = ticket['bin_left']
        bins_c = ticket['bin_center']
        h = ticket['histogram']
        print('shape, bins: ',bins.shape)
        print('shape, histogram: ',h.shape)
        # for i,hi in enumerate(h):
        #     print(i,bins_c[i],bins[i], hi)
    else:
        print('Error in histogram calculations')



def test_undulator_gaussian():
    print("------------------ test_undulator_gaussian() --------------------------------------------")
    # example ESRF ID30B, data in m,rad
    emittH = 3.9e-9
    emittV = 10e-12
    betaH = 35.6
    betaV = 3.0
    sigmaH = numpy.sqrt(emittH/betaH)
    sigmaV = numpy.sqrt(emittV/betaV)
    sigmaHp = emittH/sigmaH
    sigmaVp = emittV/sigmaV

    src = Source()
    src.set_gauss(sigmaH*1e2, sigmaV*1e2, sigmaHp, sigmaVp) #cm,rad
    src.set_energy_monochromatic(14000.0)

    print("BEFORE sH: %f um,sV: %f um, sHp: %f urad, sVp: %f  urad"%\
          (src.SIGMAX*1e4,src.SIGMAZ*1e4,src.SIGDIX*1e6,src.SIGDIZ*1e6))
    src.apply_gaussian_undulator(undulator_length_in_m=2.8,\
        user_unit_to_m=1e-2,verbose=1)
    print("AFTER  sH: %f um,sV: %f um, sHp: %f urad, sVp: %f  urad"%\
          (src.SIGMAX*1e4,src.SIGMAZ*1e4,src.SIGDIX*1e6,src.SIGDIZ*1e6))

    src.write('start.00')
    print("File written to disk: start.00")
    # create source
    beam = Beam()
    beam.genSource(src)
    beam.write("beginG.dat")
    print("File written to disk: beginG.dat")
    src.write('end.00')
    print("File written to disk: end.00")

def test_lens():
    print("------------------ test_lens() --------------------------------------------")
    print("setting lens system like Example: lens_single_sysplot.ws")

    src = Source()
    src.set_energy_monochromatic(4600)
    src.set_gauss(0.2,0.2,1e-6,1e-6)
    src.NPOINT = 5000
    src.ISTAR1 = 677543155
    src.write("start.00")


    # create source
    beam = Beam()
    beam.genSource(src)
    src.write("end.00")
    beam.write("begin.dat")

    lens = CompoundOE()
    lens.append_lens(1000.0,1000.0,surface_shape=1,convex_to_the_beam=1,diameter=None,cylinder_angle=None,\
                     radius=1000.0,interthickness=5.0,\
                     refraction_index=1.5,attenuation_coefficient=0.0, \
                     use_ccc=0)

    #lens.dump_start_files()--
    listEnd = beam.traceCompoundOE(lens,write_start_files=1,write_end_files=1,write_star_files=1)
    lens.info()
    print(lens.mirinfo())



def test_crl_snigirev():
    print("------------------ test_crl_snigirev) --------------------------------------------")
    print("setting CRL system like Example: crl_snigirev1996.ws")

    src = Source()
    src.set_energy_monochromatic(14000)
    src.set_spatial_gauss(0.00638,0.00638)
    #conical
    src.FDISTR = 5
    src.CONE_MIN = 0.0
    src.CONE_MAX = 10e-6

    src.NPOINT = 5000
    src.ISTAR1 = 677543155
    src.write("start.00")

    # create source
    beam = Beam()
    beam.genSource(src)
    beam.write("begin.dat")
    src.write("end.00")

    # crl parameters
    crl_nlenses = 30            # number of lenses
    crl_shape = 1               #1=Sphere 4=Paraboloid 5=Plan
    crl_cylinder = 0.0          #None: no cylindrical, 0=meridional, 1=sagittal         :
    crl_r = 300e-4              #radius (at tip for parabolas) or major axis for ell/hyp      :
    crl_diameter = None         # 600e-4       #lens physical aperture
    crl_interthickness = 25e-4  #thickness between two interfaces (in material)
    crl_thickness = 625e-4      #total thickness of a single lens
    crl_fs_before = 3000.0      #free space before the first lens
    crl_fs_after = 189.87       #free space after the last lens


    # shadow3> prerefl_test
    #     prerefl_test: calculates refraction index for a given energy
    #                   using a file created by the prerefl preprocessor.
    #
    #  File Name (from prerefl): Al5_55.dat
    #  Photon energy [eV]:   14000
    #  ------------------------------------------------------------------------
    #  Inputs:
    #     prerefl file: Al5_55.dat gives for E=   14000.000000000000      eV:
    #     energy [eV]:                          14000.000000000000
    #     wavelength [A]:                      0.88560137800029992
    #     wavenumber (2 pi/lambda) [cm^-1]:     709482351.55332136
    #  Outputs:
    #     refraction index = (1-delta) + i*beta :
    #     delta:                             2.7710264971503307E-006
    #     beta:                              1.7175194768200010E-008
    #     real(n):                          0.99999722897350285
    #     attenuation coef [cm^-1]:          24.370995145057691
    #  ------------------------------------------------------------------------
    crl_file = "" # "Al5_55.dat"     #material file (from prerefl preprocessor)
    refraction_index = 0.99999722897350285
    attenuation_coefficient = 24.370995145057691


    # initialize compound oe
    crl = CompoundOE(name = 'crl_snigirev1996')

    # method 0: manuel loop, 1: use append_crl
    method = 1

    if method == 0:
        p0 = crl_fs_before
        q0 = crl_fs_after
        p_or_q = 0.5*(crl_thickness - crl_interthickness)
        for i in range(crl_nlenses):
            pi = p_or_q
            qi = p_or_q
            if i == 0:
                pi = crl_fs_before
            if i == crl_nlenses-1:
                qi = crl_fs_after

            crl.append_lens(pi,qi,surface_shape=crl_shape,\
                            convex_to_the_beam=0,diameter=crl_diameter,\
                            prerefl_file=crl_file, \
                            refraction_index=refraction_index, attenuation_coefficient=attenuation_coefficient,\
                            cylinder_angle=crl_cylinder,radius=crl_r,interthickness=crl_interthickness,\
                            use_ccc=1)
    else:
        crl.append_crl(crl_fs_before, crl_fs_after, nlenses=crl_nlenses, surface_shape=crl_shape, \
                       convex_to_the_beam=0,diameter=crl_diameter,\
                       prerefl_file=crl_file,\
                       refraction_index=refraction_index, attenuation_coefficient=attenuation_coefficient, \
                       cylinder_angle=crl_cylinder,radius=crl_r,interthickness=crl_interthickness,\
                       use_ccc=1)


    # trace system
    beam.traceCompoundOE(crl,\
              write_start_files=0,write_end_files=0,write_star_files=0)

    #write only last result file
    beam.write("star.600")
    print("\nFile written to disk: star.600")
    print("\nNumber of interfaces: %d"%(crl.number_oe()))

    txt = crl.info()
    print(txt)
    print("\n\n\n\n\n\n\n\n\n\n\n")
    txt = crl.mirinfo()
    print(txt)


def test_id30b():
    print("------------------ test_id30b() --------------------------------------------")
    print("setting Transfocator for ID30B")

    #
    # Gaussian undulator source
    #

    #ID30 TDR data, pag 10, in m
    emittH = 3.9e-9
    emittV = 10e-12
    betaH = 35.6
    betaV = 3.0

    sigmaXp = numpy.sqrt(emittH/betaH)
    sigmaZp = numpy.sqrt(emittV/betaV)
    sigmaX = emittH/sigmaXp
    sigmaZ = emittV/sigmaZp
    print("\n\nElectron sizes H:%f um, V:%fu m;\nelectron divergences: H:%f urad, V:%f urad"%\
          (sigmaX*1e6, sigmaZ*1e6, sigmaXp*1e6, sigmaZp*1e6))

    # set Gaussian undulator source at 14 keV
    src = Source()
    photon_energy_ev = 14000
    src.set_energy_monochromatic(photon_energy_ev)

    src.set_gauss(sigmaX*1e2,sigmaZ*1e2,sigmaXp,sigmaZp)

    print("\n\nElectron sizes stored H:%f um, V:%f um;\nelectron divergences: H:%f urad, V:%f urad"%\
          (src.SIGMAX*1e4,src.SIGMAZ*1e4,src.SIGDIX*1e6,src.SIGDIZ*1e6))

    src.apply_gaussian_undulator(undulator_length_in_m=2.8, user_unit_to_m=1e-2, verbose=1)

    print("\n\nElectron sizes stored (undulator) H:%f um, V:%f um;\nelectron divergences: H:%f urad, V:%f urad"%\
          (src.SIGMAX*1e4,src.SIGMAZ*1e4,src.SIGDIX*1e6,src.SIGDIZ*1e6))

    src.NPOINT = 5000
    src.ISTAR1 = 677543155


    src.write("start.00")

    # create source
    beam = Beam()
    beam.genSource(src)
    beam.write("begin.dat")
    src.write("end.00")

    #
    # transfocator id30B
    #

    #set transfocator units in cm ================================================================================


    # geometry of the TF

    tf_slots   = [  1,  2,  4,  8,   1,   2,   1]  # slots
    tf_on_off  = [  1,  1,  1,  1,   1,   1,   1]  # set (1) or unset (0)

    nslots = len(tf_slots)

    tf_lens_thickness = [0.3 for i in range(nslots)]   #total thickness of a single lens in cm
    # for each slot, positional gap  of the first lens in cm
    tf_step    = [  4,   4, 1.9, 6.1,   4,   4, tf_lens_thickness[-1]]
    tf_radii   = [.05, .05, .05, .05, 0.1, 0.1, 0.15]  # radii of the lenses in cm


    # File Name (from prerefl): Be5_55.dat
    # Photon energy [eV]:   14000
    # ------------------------------------------------------------------------
    # Inputs:
    #    prerefl file: Be5_55.dat gives for E=   14000.000000000000      eV:
    #    energy [eV]:                          14000.000000000000
    #    wavelength [A]:                      0.88560137800029992
    #    wavenumber (2 pi/lambda) [cm^-1]:     709482351.55332136
    # Outputs:
    #    refraction index = (1-delta) + i*beta :
    #    delta:                             1.7354949043424384E-006
    #    beta:                              4.4123016940187606E-010
    #    real(n):                          0.99999826450509566
    #    attenuation coef [cm^-1]:         0.62609003632702676
    # ------------------------------------------------------------------------
    refraction_index = 0.99999826450509566
    attenuation_coefficient = 0.626090036 #TODO: check units

    # position of the TF measured from the center of the transfocator
    tf_p = 5960
    tf_q = 1000 # 9760 - tf_p

    #calculated values

    # these are distances p and q with TF length removed
    tf_length = numpy.array(tf_step).sum()  #tf length in cm
    tf_fs_before = tf_p - 0.5*tf_length     #distance from source to center of transfocator
    tf_fs_after  = tf_q - 0.5*tf_length     # distance from center of transfocator to image

    # for each slot, these are the empty distances before and after the lenses
    tf_p0 = numpy.zeros(nslots)
    tf_q0 = numpy.array(tf_step) - (numpy.array(tf_slots) * tf_lens_thickness)
    # add now the p q distances
    tf_p0[0]  += tf_fs_before
    tf_q0[-1] += tf_fs_after

    nlenses = numpy.array(tf_slots)*numpy.array(tf_on_off)
    slots_empty = (numpy.array(tf_slots)-nlenses)


    # # this is for calculations with xraylib (focal distances)
    # xrl_symbol = "Be"
    # xrl_density = 1.845


    # build transfocator
    tf = CompoundOE(name='TF ID30B')

    tf.append_transfocator(tf_p0.tolist(), tf_q0.tolist(), nlenses=nlenses, radius=tf_radii,\
                    slots_empty=0, surface_shape=4, convex_to_the_beam=0, diameter=None,\
           #prerefl_file="Be5_55.dat",\
           refraction_index=refraction_index,attenuation_coefficient=attenuation_coefficient, \
           cylinder_angle=0.0,interthickness=50e-4,thickness=0.3,\
           use_ccc=0)

    #trace system
    tf.dump_systemfile()
    beam.traceCompoundOE(tf,\
             write_start_files=2,write_end_files=2,write_star_files=2,write_mirr_files=2)

    #write only last result file
    beam.write("star_tf.dat")
    print("\nFile written to disk: star_tf.dat")

    print("\nLens stack: ",nlenses," empty slots: ",slots_empty)
    print("\nNumber of interfaces: %d"%(tf.number_oe()))
    print("\nTotal beamline length (from compound element) %f m"%(1e-2*tf.length()))
    print("\nTotal Transfocator length %f m"%(1e-2*tf_length))
    print("\nTotal Transfocator length (from compound element): %f %s "%(tf.length()-tf_fs_after-tf_fs_before,tf.unit()))
    print("\ntf_fs_before: %f m, tf_fs_after: %f m"%(tf_fs_before*1e-2*tf.user_units_to_cm,tf_fs_after*1e-2*tf.user_units_to_cm))


def test_id23_2():
    print("------------------ test_id23_2() --------------------------------------------")
    print("setting KB for ID23-2")

    # create source
    src = Source()
    src.set_energy_monochromatic(14200.0)
    SIGMAX = 0.00374784
    SIGMAZ = 0.000425671
    SIGDIX = 0.000107037
    SIGDIZ = 5.55325e-06
    src.set_gauss(SIGMAX,SIGMAZ,SIGDIX,SIGDIZ)
    src.write("start.00")

    beam = Beam()
    beam.genSource(src)
    beam.write("begin.dat")
    src.write("end.00")

    kb = CompoundOE(name='KB')
    kb.append_kb(4275,180,separation=4315-4275,grazing_angles_mrad=[3.9,17.8],shape=[2,2], \
                 dimensions1=[6,20],dimensions2=[6,30],reflectivity_kind=[0,0],reflectivity_files=["",""],\
                 ) # surface_error_files=["waviness.dat","waviness.dat"])

    # trace
    kb.dump_systemfile()
    beam.traceCompoundOE(kb,write_start_files=1,write_end_files=1,write_star_files=1)

    #postprocessors
    print(kb.info())
    print(kb.sysinfo())

def test_dcm():
    print("------------------ test_dcm() --------------------------------------------")
    print("setting double crystal monochromator")

    # create source
    src = Source()
    src.set_energy_box(13990,14010)
    SIGMAX = 0.00374784
    SIGMAZ = 0.000425671
    SIGDIX = 0.000107037
    SIGDIZ = 5.55325e-06
    src.set_gauss(SIGMAX,SIGMAZ,SIGDIX,SIGDIZ)
    src.write("start.00")

    beam = Beam()
    beam.genSource(src)
    beam.write("begin.dat")
    src.write("end.00")

    dcm = CompoundOE(name='DCM')

    reflectivity_file = "Si13_15.111"
    f = open(reflectivity_file, 'w')
    f.write( """
        0 1.759399e+09 3.135416e-08
        14 14 1.000000e+00
        (   4.00000000000e+00,  -7.34788079488e-16 )
        (   4.00000000000e+00,   7.34788079488e-16 )
        (  -1.46957615898e-15,  -4.00000000000e+00 )
        (  -1.46957615898e-15,   4.00000000000e+00 )
        1.524328e+01 -3.651642e+01 4.386357e+01
        1.524328e+01 -3.651642e+01 4.386357e+01
        9
           1.30000000000e+04    1.35740000000e-01    1.30917000000e-01
            1.35740000000e-01    1.30917000000e-01
           1.32500000000e+04    1.31900690252e-01    1.25990413271e-01
            1.31900690252e-01    1.25990413271e-01
           1.35000000000e+04    1.28058000000e-01    1.21338000000e-01
            1.28058000000e-01    1.21338000000e-01
           1.37500000000e+04    1.24498036127e-01    1.16936139866e-01
            1.24498036127e-01    1.16936139866e-01
           1.40000000000e+04    1.20953000000e-01    1.12770000000e-01
            1.20953000000e-01    1.12770000000e-01
           1.42500000000e+04    1.17656268813e-01    1.08799775725e-01
            1.17656268813e-01    1.08799775725e-01
           1.45000000000e+04    1.14370000000e-01    1.05034000000e-01
            1.14370000000e-01    1.05034000000e-01
           1.47500000000e+04    1.11311975417e-01    1.01458412925e-01
            1.11311975417e-01    1.01458412925e-01
           1.50000000000e+04    1.08262000000e-01    9.80639000000e-02
            1.08262000000e-01    9.80639000000e-02
      """)
    f.close()
    dcm.append_monochromator_double_crystal(4275,180,separation=10, photon_energy_ev=14000.0, \
                 dimensions1=[6,20],dimensions2=[0,0],reflectivity_file=reflectivity_file )


    #trace
    dcm.dump_systemfile()
    dcm.dump_start_files()


    beam.traceCompoundOE(dcm,write_start_files=1,write_end_files=1,write_star_files=1)
    dcm.dump_end_files()
    print(dcm.sysinfo())

    if 0: # test duplicate elements
        src1 = src.duplicate()
        src1.NPOINT=15000
        print("\n\n>>> orig NPOINT=%d, copy NPOINT=%d"%(src.NPOINT,src1.NPOINT))

        print("\n\n")
        oen = OE()
        oen.T_IMAGE = 1.00
        oen_bis = oen.duplicate()
        oen_bis.T_IMAGE = 2.0
        print("\n\n>>> orig T_IMAGE=%f, copy T_IMAGE=%f"%(oen.T_IMAGE,oen_bis.T_IMAGE))

    if 0: # test plotxy
        tkt = beam.histo2(1,3,nbins_h=3,nbins_v=3)
        print(tkt)
        print("H left",tkt["bin_h_left"])
        print("H righ",tkt["bin_h_right"])
        print("H cent",tkt["bin_h_center"])
        print("H edges",tkt["bin_h_edges"])
        print("H shape: ",tkt["histogram"].shape)

def test_sysinfo_withscreen():

    beam = Beam()
    oe0  = Source()
    oe1  = OE()
    oe2  = OE()

    #
    #define variables (see source.nml and oe.nml for doc)
    #

    oe0.FSOURCE_DEPTH = 0
    oe0.F_PHOT = 0
    oe0.F_POLAR = 0
    oe0.HDIV1 = 0.001
    oe0.HDIV2 = 0.001
    oe0.PH1 = 1000.0
    oe0.VDIV1 = 0.002
    oe0.VDIV2 = 0.002

    oe1.DUMMY = 1.0
    oe1.FMIRR = 3
    oe1.T_INCIDENCE = 80.0
    oe1.T_REFLECTION = 80.0
    oe1.T_SOURCE = 20.0

    oe2.DUMMY = 1.0
    oe2.F_REFRAC = 2
    oe2.F_SCREEN = 1
    oe2.N_SCREEN = 3
    oe2.I_SCREEN[0] = 1 # before
    oe2.I_SCREEN[1] = 0 # after
    oe2.I_SCREEN[2] = 0 # after
    oe2.SL_DIS[0] = 0.0 # distance
    oe2.SL_DIS[1] = 1.0 # distance
    oe2.SL_DIS[2] = 2.0 # distance
    oe2.I_SLIT[0] = 1 # aperture
    oe2.I_SLIT[1] = 1 # aperture
    oe2.I_SLIT[2] = 0 # aperture

    oe2.I_STOP[0] = 1 # before
    oe2.I_STOP[1] = 0 # after
    oe2.I_STOP[2] = 0 # after

    # oe2.I_ABS[2] = 1
    # tmp = oe2.FILE_ABS.copy()
    # tmp[2] = b'lllllllllll'
    # oe2.FILE_ABS =  tmp

    oe2.T_IMAGE = 0.0
    oe2.T_INCIDENCE = 0.0
    oe2.T_REFLECTION = 180.0
    oe2.T_SOURCE = 0.0

    beam.genSource(oe0)

    coe = CompoundOE(name="test sysinfo")
    coe.append(oe1)
    coe.append(oe2)

    beam.traceCompoundOE(coe)

    txt = coe.sysinfo()
    print(txt)




if __name__ == '__main__':
    do_tests = 0
    if do_tests:
        test_only_source()
        test_source_and_trace()
        test_undulator_gaussian()
        test_lens()
        test_crl_snigirev()
        test_id30b()
        test_id23_2()
        test_dcm()
        test_sysinfo_withscreen()


