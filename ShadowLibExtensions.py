#
# This file contains the new classes programmed in python  added to the 
# main objects (Beam, OE and Source) defined in C (in ShadowLib)
#
# It also define GeometricSource and Beamline
#
import sys
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

      IMPORTANT: Note that shadow3 changes the values of the OE when tracing (i.e., oe1 changes after
                 beam.traceOE(oe1) ). The same happens with compoundOE: Each oe inside compoundOE is
                 changed after tracing.

      :param compoundOE: input object
      :param from_oe: index of the first oe (for tracing compoundOE after an existing system) (default=1)
      :param write_start_files: 0=No (default), 1=Yes (all), 2: only first and last ones
      :param write_end_files:  0=No (default), 1=Yes (all), 2: only first and last ones
      :param write_star_files:  0=No (default), 1=Yes (all), 2: only first and last ones
      :param write_mirr_files:  0=No (default), 1=Yes (all), 2: only first and last ones
      :return: a list of the OE objects after tracing (the info of end.xx files)
      """
      oe_index = from_oe
      oe_n = compoundOE.number_oe()
      list = []


      for i,oe in enumerate(compoundOE.list):
        print("\nTracing compound oe %d from %d. Absolute oe number is: %d"%(i+1,oe_n,oe_index))

        iwrite = 0
        if write_mirr_files == 1: iwrite = 1
        if write_mirr_files == 2 and i == 0: iwrite = 1
        if write_mirr_files == 2 and i == oe_n-1: iwrite = 1
        if iwrite:
            oe.FWRITE = 1
        iwrite = 0
        if write_start_files == 1: iwrite = 1
        if write_start_files == 2 and i == 0: iwrite = 1
        if write_start_files == 2 and i == oe_n-1: iwrite = 1

        if iwrite:
            oe.write("start.%02d"%(oe_index))
            print("File written to disk: start.%02d"%(oe_index))





        self.traceOE(oe,oe_index)

        list.append(oe)

        iwrite = 0
        if write_star_files == 1: iwrite = 1
        if write_star_files == 2 and i == 0: iwrite = 1
        if write_star_files == 2 and i == oe_n-1: iwrite = 1
        if iwrite == 1:
            self.write("star.%02d"%(oe_index))
            print("File written to disk: star.%02d"%(oe_index))


        iwrite = 0
        if write_end_files == 1: iwrite = 1
        if write_end_files == 2 and i == 0: iwrite = 1
        if write_end_files == 2 and i == oe_n-1: iwrite = 1
        if write_end_files == 1:
            oe.write("end.%02d"%(oe_index))
            print("File written to disk: end.%02d"%(oe_index))

        oe_index += 1

      return list

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

      ret = []
      if isinstance(col, int): return self.getshonecol(col,nolost=nolost)
      for c in col:
        ret.append(self.getshonecol(c,nolost=nolost))
      return tuple(ret)

  def intensity(self,nolost=0):
      w = self.getshonecol(23,nolost=nolost)
      return w.sum()

  def histo1(self,col,xrange=None,nbins=50,nolost=0,ref=0,write=None,factor=1.0):
      '''
      Calculate the histogram of a column, simply counting the rays, or weighting with the intensity.
      It returns a dictionary which contains the histogram data.

      Inumpy.ts:
         beam     : str instance with the name of the shadow file to be loaded, or a Shadow.Beam initialized instance.
         col      : int for the chosen column.

      Optional keywords:
         xrange   : tuple or list of length 2 describing the interval of interest for x, the data read from the chosen column.
                    (default: None, thus using min and max of the array)
         nbins    : number of bins of the histogram.
         nolost   :
               0   All rays
               1   Only good rays
               2   Only lost rays
         ref      :
               0   only count the rays
               1   weight with intensity (look at 23 |E|^2 total intensity)
         write    :
               None (default)   don't write any file
               file_name   write the histogram into the file 'file_name'.
         factor   : a scalar factor to multiply the selected column before histogramming
                    (e.g., for changing scale from cm to um then factor=1e4).

      Outputs:
         a python dictionary with the calculated histogram. The following keys are set:
               histogram, histogram_sigma, bin_center, bin_left, xramge, intensity
               xrange, nbins, ref, nolost


      Error:
         if an error occurs an ArgsError is raised.

      Possible choice for col are:
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

      #initialize return value
      ticket = {'error':1}
      # copy the inputs
      ticket['col'] = col
      ticket['write'] = write
      ticket['nolost'] = nolost
      ticket['nbins'] = nbins
      ticket['xrange'] = xrange


      coli = col - 1
      if ref == 1: ref = 23


      if ref==0:
        x, a = self.getshcol((col,10))
        w = numpy.ones(len(x))
      else:
        x, a, w = self.getshcol((col,10,ref))

      if factor != 1.0: x *= factor


      if nolost==0:
        t = numpy.where(a!=-3299)

      if nolost==1:
        t = numpy.where(a > 0.0)

      if nolost==2:
        t = numpy.where(a < 0.0)

      if nolost > 2:
        print ('invalid value for nolost flag: %d'%(nolost))
        #raise KeyError ('invalid value for nolost flag: %d'%(nolost))
        return ticket


      t = numpy.array(t)
      t.shape = -1

      if t.size == 0:
        print ('no rays match the selection, the histogram will not be calculated')
        return ticket

      if xrange == None:
          xrange = [x[t].min(), x[t].max() ]


      h,bins = numpy.histogram(x[t],bins=nbins,range=xrange,weights=w[t])
      #evaluate the histogram with squares of the weight for error calculations
      h2,bins2 = numpy.histogram(x[t],bins=nbins,range=xrange,weights=(w*w)[t])

      #Evaluation of histogram error.
      # See Pag 17 in Salvat, Fernandez-Varea and Sempau
      # Penelope, A Code System for Monte Carlo Simulation of
      # Electron and Photon Transport, AEN NEA  (2003)
      #
      # See James, Rep. Prog. Phys., Vol 43 (1980) pp 1145-1189 (special attention to pag. 1184)
      h_sigma = numpy.sqrt( h2 - h*h/float(len(t)) )

      if write != None and write != "":
          file = open(write,'w')
          print ('#F %s'%(write), file = file)
          print ('#C This file has been created using Shadow.Beam.histo1() ', file = file)
          #print ('#D '+now, file = file)
          #print ('#UTITLE', file = file)
          #print ('#USUBTITLE '+usubtitle, file = file)
          #print ('#UTTEXT', file = file)
          print ('#C COLUMN 1 CORRESPONDS TO ABSCISSAS IN THE CENTER OF EACH BIN', file = file)
          print ('#C COLUMN 2 CORRESPONDS TO ABSCISSAS IN THE THE LEFT CORNER OF THE BIN', file = file)
          print ('#C COLUMN 3 CORRESPONDS TO INTENSITY', file = file)
          print ('#C COLUMN 4 CORRESPONDS TO ERROR: SIGMA_INTENSITY', file = file)
          print ('#C col = %d'%(col), file = file)
          print ('#C nolost = %d'%(nolost), file = file)
          print ('#C nbins = %d'%(nbins), file = file)
          print ('#C ref = %d'%(ref), file = file)
          print (' ', file = file)
          print ('#S 1 histogram', file = file)
          print ('#N 4' , file = file)
          #print ('#L '+getLabel(col)[1]+'  '+(getLabel(col))[1]+'  '+'intensity (rays)'+'  '+(getLabel(ref))[1], file = file)
          print ('#L X1  X2  Y  YERR', file = file)
          for i in range(len(h)):
            print ('%f\t%f\t%f\t%f' % ( (bins[i]+bins[i+1])*0.5, bins[i], h[i], h_sigma[i] ), file = file)
          file.close()
          print('histo1: file written to disk: %s'%(write))



      #
      # output
      ticket['error'] = 0
      ticket['histogram'] = h
      ticket['histogram_sigma'] = h_sigma
      ticket['bin_center'] = bins[:-1]+(bins[1]-bins[0])*0.5
      ticket['bin_left'] = bins[:-1]
      ticket['xrange'] = xrange
      ticket['intensity'] = w[t].sum()
      ticket['fwhm'] = None

      #CALCILATE fwhm
      fwhmx = None
      tt = numpy.where(h>max(h)*0.5)
      if h[tt].size > 0:
          binSize = bins[1]-bins[0]
          #LIMITS
          #txf = tt[0][-1]
          #txi = tt[0][0]
          ticket['fwhm'] = binSize*(tt[0][-1]-tt[0][0]+1)

      return ticket


class GeometricSource(ShadowLib.Source):
  def __init__(self):
    ShadowLib.Source.__init__(self)

  def setNumberRaysInit(self,nRays=5000,seed=6775431):
    self.N = nRays
    if(seed%2==0): seed += 1
    self.ISTAR1 = seed
    self.NCOL = 18 # shadow3 it will be rewritten anyway

  def setSpaceDistribution(self,transverse='gauss',longitude='no',param=numpy.zeros(3)):
    distTran = {'point':0 ,'rectangle':1,'ellipse':2,'gauss':3}
    distLong = {'no':1 ,'flat':2,'gauss':3}#,'synchrotron':4} synchrotron implemented in a different class
    try:
      self.FSOUR = distType[transverse]
      self.FSOURCE_DEPTH = distType[longitude]

      if self.FSOUR in [1,2]:
        self.WXSOU = param[0]
        self.WZSOU = param[1]
      if self.FSOUR==3:
        self.SIGMAX = param[0]
        self.SIGMAZ = param[1]

      if self.FSOURCE_DEPTH==2:
        self.WYSOU = param[2]
      if self.FSOURCE_DEPTH==3:
        self.SIGMAY = param[2]

    except KeyError:
      print ('setSpaceDistribution: wrong distribution name')
#TODO    
#  def setAngleDistribution(self,angle='gauss',)

#
# TODO: Put all classes starting with capital letter
# in the meantime, bind as this one:  
#
class geometricSource(GeometricSource):
  def __init__(self): 
    print(' DEPRECATION WARNING: Use GeometricSource ')
    GeometricSource.__init__(self)

class OE(ShadowLib.OE):
  def __init__(self):
    ShadowLib.OE.__init__(self)

# here methods to initialize the OE
  def setScreens(self, 
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
                 file_src_ext=numpy.array(['', '', '', '', '', '', '', '', '', ''])
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
    self.FILE_SRC_EXT = file_src_ext

    return self

    #SCR_NUMBER automastically set


  def setOutput(self,fwrite=0):
    self.FWRITE = fwrite
    return self



  def setFrameOfReference(self,source_distance=10.0,image_distance=20.0,source_angle=10.0,image_angle=10.0,alpha=0.0):
    self.T_SOURCE     = source_distance
    self.T_IMAGE      = image_distance
    self.T_INCIDENCE  = source_angle
    self.T_REFLECTION = image_angle
    self.ALPHA        = alpha
    return self


  def setSeed(self,istar=12345701):
    self.ISTAR1       = istar
    return self


  def setConvex(self):
    self.F_CONVEX = 1
    return self


  def setConcave(self):
    self.F_CONVEX = 0
    return self


  def setCylindric(self,cyl_ang=0.0):
    self.FCYL = 1
    self.CIL_ANG = cyl_ang
    return self


  def unsetCylinder(self):
    self.FCYL = 0
    return self


  def setAutoFocus(self,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
    self.F_EXT = 0
    self.F_DEFAULT = f_default
    if f_default==0:
      self.SSOUR = ssour
      self.SIMAG = simag
      self.THETA = theta
    return self


  def unsetReflectivity(self):
    self.F_REFLEC = 0
    return self

  def setReflectivityFull(self,f_refl=0,file_refl='GAAS.SHA',rparams=numpy.zeros(2,dtype=numpy.float64),f_thick=0):
    self.F_REFLEC = 1
    self.F_REFL = f_refl
    self.FILE_REFL = file_refl
    self.ALFA = rparams[0]
    self.GAMMA = rparams[1]
    self.F_THICK = f_thick
    return self

  def setReflectivityScalar(self,f_refl=0,file_refl='GAAS.SHA',rparams=numpy.zeros(2,dtype=numpy.float64),f_thick=0):
    self.F_REFLEC = 2
    self.F_REFL = f_refl
    self.FILE_REFL = file_refl
    self.F_THICK = f_thick
    return self

  def setMultilayer(self,f_reflec=1,file_refl='GAAS.SHA',f_thick=0):
    self.F_REFLEC = f_reflec
    self.F_REFL = 2
    self.FILE_REFL = file_refl
    self.F_THICK = f_thick
    return self


  def setSpheric(self,rmirr=20.0):
    self.FMIRR = 1
    self.F_EXT = 1
    self.RMIRR = rmirr
    return self

  def setSphericAuto(self,fparams=None):#):f_default=0,ssour=0.0,simag=0.0,theta=0.0):
    self.FMIRR = 1
    if fparams == None:
      self.setAutoFocus(1)
    else:
      self.setAutoFocus(0,ssour=fparams[0],simag=fparams[1],theta=fparams[2])
    return self


  def setEllipsoid(self,ell_the=0.0,axmaj=0.0,axmin=0.0):
    self.FMIRR = 2
    self.F_EXT = 1
    self.ELL_THE = ell_the
    self.AXMAJ = axmaj
    self.AXMIN = axmin
    return self

  def setEllipsoidAuto(self,fparams=None):#ell_the=0.0,axmaj=0.0,axmin=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
    self.FMIRR = 2
    if fparams==None:
      self.setAutoFocus(1)
    else:
      self.setAutoFocus(0,ssour=fparams[0],simag=fparams[1],theta=fparams[2])
    return self


  def setToroid(self,f_torus=0,r_maj=0.0,r_min=0.0):
    self.FMIRR = 3
    self.F_EXT = 1
    self.F_TORUS = f_torus
    self.R_MAJ = r_maj
    self.R_MIN = r_min
    return self

  def setToroidAuto(self,f_torus=0,r_maj=0.0,r_min=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
    self.FMIRR = 3
    self.F_TORUS = f_torus
    self.R_MAJ = r_maj
    self.R_MIN = r_min
    self.setAutoFocus(f_default,ssour,simag,theta)
    return self


  def setParaboloid(self,f_side=1,param=0.0):
    self.FMIRR = 4
    self.F_EXT = 1
    self.F_SIDE = f_side
    self.PARAM = param
    return self

  def setParaboloidAuto(self,f_side=1,fparams=None):#f_side=1,param=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0,f_side=0):
    self.FMIRR = 4
    self.F_SIDE = f_side
    if fparams==None:
      self.setAutoFocus(1)
    else:
      self.setAutoFocus(0,ssour=fparam[0],simag=fparam[1],theta=fparams[2])
    return self


  def setPlane(self):
    self.FMIRR = 5
    self.F_EXT = 1
    return self


  def setCodlingSlit(self,cod_len=0.0,cod_wid=0.0): # HERE ASK MANOLO, always 1 or 0
    self.FMIRR = 6
    self.F_EXT = 1
    self.COD_LEN = 0.0
    self.COD_WID = 0.0
    return self


  def setHyperboloid(self,ell_the=0.0,axmaj=0.0,axmin=0.0):
    self.FMIRR = 7
    self.F_EXT = 1
    self.ELL_THE = ell_the
    self.AXMAJ = axmaj
    self.AXMIN = axmin
    return self

  def setHyperboloidAuto(self,fparams=None):#ell_the=0.0,axmaj=0.0,axmin=0.0,f_default=0,ssour=0.0,simag=0.0,theta=0.0):
    self.FMIRR = 2
    if fparams==None:
      self.setAutoFocus(1)
    else:
      self.setAutoFocus(0,ssour=fparams[0],simag=fparams[1],theta=fparams[2])
    return self


  def setCone(self,cone_a=0.0):
    self.FMIRR = 8
    self.F_EXT = 1
    self.CONE_A = cone_a
    return self


  def setPoly(self,file_mir=''):
    self.FMIRR = 9
    self.F_EXT = 1
    self.FILE_MIR = file_mir
    return self


  def setCCC(self,ccc=numpy.array([1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],dtype=numpy.float64)):
    self.FMIRR = 10
    self.F_EXT = 1
    self.CCC[:] = ccc[:]
    return self


  def setRipple(self,f_g_s=0,xyAmpWavPha=numpy.array([0.0,0.0,0.0,0.0,0.0,0.0],dtype=numpy.float64),file_rip=''):
    self.F_RIPPLE = 1
    self.F_G_S = f_g_s
    self.X_RIP_AMP = xyAmpWavPha[0]
    self.X_RIP_WAV = xyAmpWavPha[1]
    self.X_PHASE   = xyAmpWavPha[2]
    self.Y_RIP_AMP = xyAmpWavPha[3]
    self.Y_RIP_WAV = xyAmpWavPha[4]
    self.Y_PHASE   = xyAmpWavPha[5]
    self.FILE_RIP  = file_rip 
    return self


  def setDimensions(self,fshape=1,params=numpy.zeros(4,dtype=numpy.float64)):
    self.FHIT_C = 1
    self.FSHAPE = fshape
    self.RLEN1  = params[0]
    self.RLEN2  = params[1]
    self.RWIDX1 = params[2]
    self.RWIDX2 = params[3]
    #TODO set self.FHIT_C = 0 elsewhere
    return self




  def setReflector(self):
    self.F_REFRACT = 0
    return self


  def setRefractor(self,r_ind_obj = 1.0,r_ind_ima = 1.0,r_attenuation_obj = 0.0,r_attenuation_ima = 0.0):
    self.F_REFRACT = 1
    self.R_IND_OBJ = r_ind_obj
    self.R_IND_IMA = r_ind_ima
    self.R_ATTENUATION_OBJ = r_attenuation_obj
    self.R_ATTENUATION_IMA = r_attenuation_ima
    return self


  def unsetCrystal(self):
    self.F_CRYSTAL = 0
    return self

  def setCrystal(self,file_refl=b'',a_bragg=0.0):
    self.F_CRYSTAL = 1
    self.FILE_REFL = file_refl
    self.F_REFLECT = 0

    if a_bragg!=0.0:
      self.F_BRAGG_A = 1
      self.A_BRAGG = a_bragg
    return self


  def setJohansson(self,r_johansson=None):
    self.F_JOHANSSON = 1 
    #TODO set self.F_JOHANSSON = 0 elsewhere
    if r_johansson!=None:
      self.F_EXT = 1
      self.R_JOHANSSON=r_johansson
    else:
      self.F_EXT = 0
    return self
 

  def setGratingRulingConstant(self,f_ruling=1,ruling=0.0):
    self.F_GRATING = 1
    self.F_RULING = f_ruling
    self.RULING = ruling
    return self


  def setGratingHolographic(self,holo_params=numpy.zeros(7,dtype=numpy.float64),f_pw=2,f_pw_c=0,f_virtual=0):
    self.F_GRATING = 1
    self.F_RULING = 2
    self.HOLO_R1 = holo_params[0]
    self.HOLO_R2 = holo_params[1]
    self.HOLO_DEL = holo_params[2]
    self.HOLO_GAM = holo_params[3]
    self.HOLO_W = holo_params[4]
    self.HOLO_RT1 = holo_params[5]
    self.HOLO_RT2 = holo_params[6]
    self.F_PW = f_pw
    self.F_PW_C = f_pw_c
    self.F_VIRTUAL = f_virtual
    return self


  def setGratingFan(self,azim_fan=0.0,dist_fan=0.0,coma_fac=0.0):
    self.F_GRATING = 1
    self.F_RULING = 3
    self.AZIM_FAN = azim_fan
    self.DIST_FAN = dist_fan
    self.COMA_FAC = coma_fac
    return self


  def setGratingReserved(self):
    self.F_GRATING = 1
    self.F_RULING = 4
    return self


  def setGratingPolynomial(self,poly_params=numpy.zeros(5,dtype=numpy.float64),f_rul_abs=0):
    self.F_GRATING = 1
    self.F_RULING = 5
    self.F_RUL_ABS = f_rul_abs
    self.RULING = poly_params[0]
    self.RUL_A1 = poly_params[1]
    self.RUL_A2 = poly_params[2]
    self.RUL_A3 = poly_params[3]
    self.RUL_A4 = poly_params[4]
    return self

  def setMosaic(self,mosaic_seed=4732093,spread_mos=0.0,thickness=0.0):
    self.F_MOSAIC = 1
    self.MOSAIC_SEED = mosaic_seed
    self.SPREAD_MOS = spread_mos
    self.THICKNESS = thickness
    return self

  def setAutoTuning(self,f_phot_cent=0,phot_cent=5000.0,r_lambda=100.0):
    self.F_CENTRAL = 1
    self.F_PHOT_CENT = f_phot_cent
    self.PHOT_CENT = phot_cent
    self.R_LAMBDA = r_lambda
    return self

  def setAutoMonochromator(self,f_phot_cent=0,phot_cent=5000.0,r_lambda=100.0,f_mono=0,f_hunt=1,hparam=numpy.zeros(3,dtype=numpy.float64)):
    self.setAutoTuning(f_phot_cent=f_phot_cent,phot_cent=phot_cent,r_lambda=r_lambda)
    self.F_MONO = f_mono
    self.F_HUNT = f_hunt
    self.HUNT_H = hparam[0]
    self.HUNT_L = hparam[1]
    self.BLAZE = hparam[2]
    return self

  def to_dictionary(self):
    mem = inspect.getmembers(self)
    mydict = {}
    for i,var in enumerate(mem):
      if var[0].isupper():
        mydict[var[0]]= var[1]
    return(mydict)


  def mirinfo(self, title=None):
    '''
    mimics SHADOW mirinfo postprocessor. Returns a text array.
    :return:
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
    txt += '********************   MIRROR  DESCRIPTION   ********************\n'

    if title == None:
        txt += '\n\n'
    else:
        txt += title+'\n'
    txt += TOPLIN


    txt += 'Surface figure was defined as: %s \n'%(type1[str(self.FMIRR)])
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
    else:
        txt += 'Element type                            REFRACTOR\n'

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
        txt += 'Lattice Spacing                         %f\n'%(self.D_SPACING)
        txt += 'Bragg Reflection from  %s\n'%(self.FILE_REFL.strip().decode())
        if self.F_MOSAIC == 1:
            txt += 'MOSAIC Crystal selected                \n'
            txt += 'Mosaic crystal spread (st. dev)  [DEG]  %f\n'%(self.SPREAD_MOS*180.0/numpy.pi)
            txt += 'Mosaic crystal thickness [cm]           %f\n'%(self.THICKNESS)
        else:
            if self.F_BRAGG_A == 1:
                txt += 'Asymmetric Cut angle  [DEG]             %f\n'%(self.A_BRAGG*180.0/numpy/pi)
            if self.F_JOHANSSON == 1:
                txt += 'JOHANSSON Geometry selected            \n'
                txt += 'Johansson radius                         %f\n'(self.R_JOHANSSON)


    if self.F_GRATING == 1:
        if self.FZP == 1:
            txt += 'Element type                 Fresnel Zone Plate\n'
        txt += 'Element type                            GRATING\n'
        txt += 'Order choosen ( inside are < 0 )        *d\n'%(self.ORDER)
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
            txt += 'Holographic grating. Recording Wavelength:  %f\n'%(self.RULING)

            txt += 'Input Slit Dist.'+T20+'Exit Slit Dist.'+T20+'Input Slit Angle',T60,'Exit Slit Angle\n'
            txt += '%16.9g'%(self.HOLO_R1)+'%16.9g'%(self.HOLO_R2)+'%16.9g'%(self.HOLO_DEL)+'%16.9g'%(self.HOLO_GAM)+'\n'
            txt += 'Input  Slit rotation angle  %f \n'%(self.HOLO_RT1*180.0/numpy.pi)
            txt += 'Output Slit rotation angle  %f \n'%(self.HOLO_RT2*180.0/numpy.pi)
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
        if (self.F_RULING == 5) and (self.F_CRYSTAL == 0):
            txt += 'Mechanically ruled grating. Polinomial Coefficients: \n'
            txt += 'Zero order term Coefficient:  %f\n'%(self.RULING)
            txt += 'First                         %f\n'%(self.RUL_A1)
            txt += 'Second                        %f\n'%(self.RUL_A2)
            txt += 'Third                         %f\n'%(self.RUL_A3)
            txt += 'Fourth                        %f\n'%(self.RUL_A4)
        if (self.F_RULING == 3) and (self.F_CRYSTAL == 0):
            txt += 'Oriental fan type grating.\n'
            txt += 'Fan pole angle from Y axis          %f\n'%(self.AZIM_FAN)
            txt += '        distance from grating pole  %f\n'%(self.DIST_FAN)
            txt += 'Coma correction factor              %f\n'%(self.COMA_FAC)
            txt += 'Line density at grating pole        %f\n'%(self.RULING)

    if self.F_REFRAC == 1:
        txt += 'Relative Index of Refraction            %f\n'%(self.ALFA)

    if self.F_REFLEC == 0:
        txt += 'Reflectivity                            OFF\n'
    else:
        if self.F_REFL == 0:
            txt += 'Reflectivity      ON     coefficients from: %s'%(self.FILE_REFL.strip().decode())
        if self.F_REFL == 1:
            txt += 'Reflectivity      ON     coefficients from TT:'
        if self.F_REFL == 2:
            txt += 'Multilayer        ON     coefficients and geometry from :  %s'%(self.FILE_REFL.strip().decode())


    if self.F_REFLEC == 1: txt += 'Polarization dependence                 YES\n'
    if self.F_REFLEC == 2: txt += 'Polarization dependence                 NO\n'

    if self.FHIT_C == 0:
        txt += 'Mirror dimensions                       UNLIMITED\n'
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
    txt += 'Source Plane Distance                    %f\n'%(self.T_SOURCE)
    txt += 'Image  Plane                             %f\n'%(self.T_IMAGE)
    txt += 'Incidence Angle                          %f\n'%(self.T_INCIDENCE*180.0/numpy.pi)
    txt += 'Reflection/Diffraction Angle             %f\n'%(self.T_REFLECTION*180.0/numpy.pi)

    if self.F_EXT == 1:
        txt += 'Mirror parameters                       EXTERNAL\n'
    else:
        if self.FMIRR != 10:
            txt += 'Mirror parameters                       COMPUTED\n'
            if self.F_DEFAULT == 1:
                txt += 'Same configuration as Central Axis      YES\n'
            else:
                txt += 'Same configuration as Central Axis      NO\n'
            txt += 'Objective focus at                       %f\n'%(self.SSOUR)
            txt += 'Image focus at                           %f\n'%(self.SIMAG)
            txt += 'Incidence angle                          %f\n'%(self.THETA*180.0/numpy.pi)


    txt += 'Parameters used follow:\n'
    if self.FMIRR == 1:
        txt += 'Spherical Radius  %f\n'%(self.RMIRR)
    if self.FMIRR == 2:
        ECCENT = numpy.sqrt(self.AXMAJ**2-self.AXMIN**2)/self.AXMAJ
        txt += '   Semi-major axis   %f\n'%(self.AXMAJ)
        txt += '   Semi-minor axis   %f\n'%(self.AXMIN)
        txt += '   Semi-focal-length %f\n'%(numpy.sqrt(self.AXMAJ**2-self.AXMIN**2))
        txt += '   Eccentricity      %f\n'%(ECCENT)
    if self.FMIRR == 3:
        txt  += '   Major Radius (optical)     %f\n'%(self.R_MAJ+self.R_MIN)
        txt  += '   Minor Radius               %f\n'%(self.R_MIN)
    if self.FMIRR == 4:
        txt += '   Parabola Param.  %f\n'%(self.PARAM)
    if self.FMIRR == 5:
        txt += '   Plane mirror \n'
    if self.FMIRR == 6:
        txt += '   Codling Slit\n'
    if self.FMIRR == 7:
        AFOCI = numpy.sqrt(self.AXMIN**2+self.AXMAJ**2)
        ECCENT = AFOCI/numpy.abs(self.AXMAJ)
        txt += '   Semi-major axis   %f\n'%(self.AXMAJ)
        txt += '   Semi-minor axis   %f\n'%(self.AXMIN)
        txt += '   Semi-focal-length %f\n'%(AFOCI)
        txt += '   Eccentricity      %f\n'%(ECCENT)
    if self.FMIRR == 8:
        txt += '   Cone half-angle   %f\n'%(self.CONE_A*180.0/numpy.pi)
    if self.FMIRR == 9:
        txt += '   Polynomial Coeff file    %s\n'%(self.FILE_MIR.strip().decode())

    if self.FSTAT == 0:
        txt += 'Source of this O.E. moved               NO\n'
    else:
        txt += 'Source of this O.E. moved               YES\n'
        txt += 'In SOURCE reference frame: \n'
        txt += 'Source Movement X:  %f\n'%(self.X_SOUR)
        txt += '                Y:  %f\n'%(self.Y_SOUR)
        txt += '                Z:  %f\n'%(self.Z_SOUR)
        txt += 'Source rot at X:    %f\n'%(self.X_SOUR_ROT*180.0/numpy.pi)
        txt += '              Y:    %f\n'%(self.Y_SOUR_ROT*180.0/numpy.pi)
        txt += '              Z:    %f\n'%(self.Z_SOUR_ROT*180.0/numpy.pi)
        txt += 'In MIRROR reference frame: \n'
        txt += 'Source distance     %f\n'%(self.RDSOUR)
        txt += '       rotation     %f\n'%(self.ALPHA_S*180.0/numpy.pi)
        txt += 'Incidence angle     %f\n'%(self.RTHETA*180.0/numpy.pi)
        txt += 'Source offset X:    %f\n'%(self.OFF_SOUX)
        txt += '              Y:    %f\n'%(self.OFF_SOUY)
        txt += '              Z:    %f\n'%(self.OFF_SOUZ)

    if self.F_MOVE == 0:
        txt += 'Mirror at pole position ( no mov. )     YES\n'
    else:
        txt += 'Mirror moved from pole. Parameters :\n'
        txt += 'Displacement along X:    %f\n'%(self.OFFX)
        txt += '                   Y:    %f\n'%(self.OFFY)
        txt += '                   Z:    %f\n'%(self.OFFZ)
        txt += 'Rotation around X:    %f\n'%(self.X_ROT*180.0/numpy.pi)
        txt += '                Y:    %f\n'%(self.Y_ROT*180.0/numpy.pi)
        txt += '                Z:    %f\n'%(self.Z_ROT*180.0/numpy.pi)
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
            txt += '  c[%d]= %f\n'%(i,self.CCC[i])

    txt += TOPLIN
    txt += '***************                 E N D                  ***************\n'
    txt += TOPLIN

    return txt


# not yet ready
class CompoundOE():
  def __init__(self,list=None, name=''):
    if list == None:
        self.list = []
    else:
        self.list = list
    self.name = name
    self = list #.__init__()
    #self.type = type(OE)

  def set_name(self,name):
      self.name = name

  def number_oe(self):
      return len(self.list)


  def print(self):
      print("CompoundOE name: %s, found %d elements"%(self.name,self.number_oe()))
      for i,j in enumerate(self.list):
          print('oe %d, p=%f, q=%f'%(1+i,j.T_SOURCE,j.T_IMAGE))

  def mirinfo(self):
      txt = ""
      for i,oe in enumerate(self.list):
          txt += oe.mirinfo(title="oe %d in compoundOE name: %s "%(i+1,self.name))
      return txt

  def append_oe(self,item):
    if isinstance(item, OE):
        self.list.append(item)
    else:
        print("Failed to append: only OE can be appended. ")
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
      :return:
      """
      oe1 = OE()
      oe2 = OE()

      #set constant values for both interfaces
      oe1.T_INCIDENCE = 0.0
      oe1.T_REFLECTION = 180.0
      oe2.T_INCIDENCE = 0.0
      oe2.T_REFLECTION = 180.0
      oe1.F_REFRAC = 1
      oe2.F_REFRAC = 1

      oe1.F_EXT = 1
      oe2.F_EXT = 2

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
          oe1.FILE_R_IND_IMA = prerefl_file.encode('utf-8')

          oe2.F_R_IND = 1 #file in object space, keyboard in image space
          oe2.FILE_R_IND_OBJ = prerefl_file.encode('utf-8')
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
          oe1.FHIT_C = 1
          oe2.FHIT_C = 1
          oe1.FSHAPE = 2 #ellipse
          oe2.FSHAPE = 2
          oe1.RWIDX1 = 0.0
          oe2.RWIDX1 = 0.0
          oe1.RWIDX2 = diameter*0.5
          oe2.RWIDX2 = diameter*0.5
          oe1.RLEN1 = 0.0
          oe2.RLEN1 = 0.0
          oe1.RLEN2 = diameter*0.5
          oe2.RLEN2 = diameter*0.5

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



      self.append_oe(oe1)
      self.append_oe(oe2)

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
        :param surface_shape:1=sphere 4=paraboloid, 5=plane (other surfaces not yet implamented)
        :param convex_to_the_beam:convexity of the first interface exposed to the beam 0=No, 1=Yes
                                 the second interface has opposite convexity
        :param diameter:lens diameter. Set to None for infinite dimension
        :param cylinder_angle:None=not cylindrical, 0=meridional 90=sagittal
        :param prerefl_file:file name (from prerefl) to get the refraction index. If set
                then the keywords refraction_index and attenuation_coefficient are not used.
        :param refraction_index: n (real) #ignored if prerefl_file points to file.
        :param attenuation_coefficient:mu (real); ignored if prerefl file points to file.
        :param radius:lens radius (for pherical, or radius at the tip for paraboloid)
        :param thickness: lens thickness (piling thickness)
        :param interthickness:lens thickness (distance between the two interfaces at the center of the lenses)
        :param use_ccc:0=set shadow using surface shape (FMIRR=1,4,5), 1=set shadow using CCC coeffs (FMIRR=10)
        :return:
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
        :param surface_shape (list):1=sphere 4=paraboloid, 5=plane (other surfaces not yet implamented)
        :param convex_to_the_beam (list):convexity of the first interface exposed to the beam 0=No, 1=Yes
                                 the second interface has opposite convexity
        :param diameter (list):lens diameter. Set to None for infinite dimension
        :param cylinder_angle (list):None=not cylindrical, 0=meridional 90=sagittal
        :param prerefl_file (list):file name (from prerefl) to get the refraction index. If set
                then the keywords refraction_index and attenuation_coefficient are not used.
        :param refraction_index (list): n (real) #ignored if prerefl_file points to file.
        :param attenuation_coefficient (list):mu (real); ignored if prerefl file points to file.
        :param radius (list):lens radius (for pherical, or radius at the tip for paraboloid)
        :param thickness (list): lens thickness (piling thickness)
        :param interthickness (list):lens thickness (distance between the two interfaces at the center of the lenses)
        :param use_ccc (scalar):0=set shadow using surface shape (FMIRR=1,4,5), 1=set shadow using CCC coeffs (FMIRR=10)
        :return:
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
            # print("Appending file **%s**"%(prerefl_file[i]))
            # print("Calling append_crl with p0:%f, q0:%f, nlenses=%f, slots_empty=%f, \
            #               radius=%f, thickness=%f, interthickness=%f, \
            #               surface_shape=%f,convex_to_the_beam=%f,\
            #               diameter=%f, cylinder_angle=%f,\
            #               prerefl_file=%s, \
            #               use_ccc=0"%(p0[i], q0[i],nlenses[i],slots_empty[i], \
            #               radius[i], thickness[i], interthickness[i], \
            #               surface_shape[i],convex_to_the_beam[i],\
            #               diameter[i], cylinder_angle[i],\
            #               prerefl_file[i]))

            self.append_crl(p0[i], q0[i], nlenses=nlenses[i], slots_empty=slots_empty[i], \
                          radius=radius[i], thickness=thickness[i], interthickness=interthickness[i], \
                          surface_shape=surface_shape[i],convex_to_the_beam=convex_to_the_beam[i],\
                          diameter=diameter[i], cylinder_angle=cylinder_angle[i],\
                          prerefl_file=prerefl_file[i],refraction_index=refraction_index[i], \
                          attenuation_coefficient=attenuation_coefficient[i],\
                          use_ccc=0)


  def dump_start_files(self,offset=0):
    for i,oe in enumerate(self.list):
      oe.write('start.%02d'%(i+1+offset))
      print('File written to disk: start.%02d\n'%(i+1+offset))

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
        mem = inspect.getmembers(self)
        mydict = {}
        for i,var in enumerate(mem):
            if var[0].isupper():
                mydict[var[0]]= var[1]
        return(mydict)

    #Gaussian source
    def set_divergence_gauss(self, sigmaxp, sigmazp):
        self.FDISTR = 3
        self.HDIV1 = 1.0
        self.HDIV2 = 1.0
        self.VDIV1 = 1.0
        self.VDIV2 = 1.0
        self.SIGDIX = sigmaxp
        self.SIGDIZ = sigmazp

    def set_spatial_gauss(self,sigmax, sigmaz):
        self.FSOUR = 3
        self.SIGMAX = sigmax
        self.SIGMAZ = sigmaz

    def set_gauss(self,sigmax,sigmaz,sigmaxp,sigmazp):
        self.set_divergence_gauss(sigmaxp,sigmazp)
        self.set_spatial_gauss(sigmax,sigmaz)

    def set_energy_monochromatic(self,emin):
        self.F_COLOR =  1
        self.F_PHOT =  0 #eV
        self.PH1 = emin

    def set_energy_box(self,emin,emax):
        self.F_COLOR =  3
        self.F_PHOT =  0 #eV
        self.PH1 = emin
        self.PH2 = emax

    def set_pencil(self):
        self.FSOUR = 0
        self.FDISTR = 1
        self.HDIV1 = 0.0
        self.HDIV2 = 0.0
        self.VDIV1 = 0.0
        self.VDIV2 = 0.0

    def apply_gaussian_undulator(self, undulator_length_in_m=1.0,user_unit_to_m=1e2, verbose=1, und_e0=None):

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
    

 
    def sourcinfo(self,title=None):
        '''
        mimics SHADOW sourcinfo postprocessor. Returns a text array.
        :return:
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



        txt += 'Source Spatial Characteristics: '+TSPATIAL[str(1+self.FSOUR)]
        txt += '\n'


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
            txt += '    created/accepted ratio: %d \n'%(float(self.NTOTALPOINT)/float(self.NPOINT))

            if self.F_BOUND_SOUR == 1:
                txt += '    file with phase-space volume: '+self.FILE_BOUND.strip().decode()
            else:
                txt += '    file with slit/acceptance: '+self.FILE_BOUND.strip().decode()


        txt += TOPLIN
        txt += '***************                 E N D                  ***************\n'
        txt += TOPLIN
        return (txt)

if __name__ == '__main__':
    #
    # test
    #
    do_test = 6 # 1=only source ; 2= source and trace ; 3=undulator_gaussian ; 4 lens, like in lens_single_plot.ws
                # 6=ID30B

    if ((do_test == 1) or (do_test == 2)):
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
        ticket_h = beam.histo1(col=1, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref=1)
        print('Histogram FWHM: %f, stdev: %f, initial: %f\n: '%(ticket_h['fwhm'],ticket_h['fwhm']/2.35,sh))
        ticket_h = beam.histo1(col=3, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref=1)
        print('Histogram FWHM: %f, stdev: %f, initial: %f\n: '%(ticket_h['fwhm'],ticket_h['fwhm']/2.35,sv))
        ticket_h = beam.histo1(col=4, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref=1)
        print('Histogram FWHM: %f, stdev: %f, initial: %f\n: '%(ticket_h['fwhm'],ticket_h['fwhm']/2.35,shp))
        ticket_h = beam.histo1(col=6, nbins = 500, nolost=1, write='HISTO1', xrange=None , ref=1)
        print('Histogram FWHM: %f, stdev: %f, initial: %f\n: '%(ticket_h['fwhm'],ticket_h['fwhm']/2.35,svp))

    if do_test == 2:

        oe1 = OE()
        oe1.write('start.01')
        #oe1.load('tmp/start.01')
        beam.traceOE(oe1,1)

        oe1.write('end.01')
        beam.write('star.01')

        #analysis
        #print(beam.getshonecol(11,nolost=1))
        print('Intensity after oe 1 all, good and lost rays: %f, %f, %f , '%\
              (beam.intensity(nolost=0),beam.intensity(nolost=1),beam.intensity(nolost=2) ))
        
        #print( oe1.to_dictionary() )
        print(oe1.mirinfo(title='mirinfo in python'))

        ticket = beam.histo1(col=3, nbins =11, nolost=1, write='HISTO1', xrange=[-0.055, 0.055], ref=1)

        if ticket['error'] == 0:
            beam.write('star.01')
            bins = ticket['bin_left']
            bins_c = ticket['bin_center']
            h = ticket['histogram']
            print('shape, bins: ',bins.shape)
            print('shape, histogram: ',h.shape)
            for i,hi in enumerate(h):
                print(i,bins_c[i],bins[i], hi)
        else:
            print('Error in histogram calculations')

    if do_test == 3:
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

    if do_test == 4:
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
        lens.print()
        print(lens.mirinfo())



    if do_test == 5:
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
        beam.write("star.60")
        print("\nFile written to disk: star.60")
        print("\nNumber of interfaces: %d"%(crl.number_oe()))
        #crl.dump_systemfile()        # lens.print()
        #print(crl.mirinfo())



    if do_test == 6:
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
        attenuation_coefficient = 0.626090036

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
        print("\nTotal Transfocator length (from compound element): %f cm "%(tf.length()-tf_fs_after-tf_fs_before))
        print("\ntf_fs_before: %f m, tf_fs_after: %f m"%(tf_fs_before*1e-2,tf_fs_after*1e-2))



