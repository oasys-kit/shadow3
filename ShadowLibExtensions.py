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


class Beam(ShadowLib.Beam):
  def __init__(self, N=None):
    ShadowLib.Beam.__init__(self)
    if N is not None:
      self.SetRayZeros(N)

  def retrace(self,dist):
    try:
      tof = (-self.rays[:,1].flatten() + dist)/self.rays[:,4].flatten()
      self.rays[:,0] += tof*self.rays[:,3].flatten()
      self.rays[:,1] += tof*self.rays[:,4].flatten()
      self.rays[:,2] += tof*self.rays[:,5].flatten()
    except AttributeError:
      print ('retrace: No rays')

  def getStandardDeviations(self,weighted=True):
    try:
      if len(self.rays)==0:
        raise AttributeError
      if weighted:
        w = beam.rays[:,6]*beam.rays[:,6] + beam.rays[:,7]*beam.rays[:,7] + beam.rays[:,8]*beam.rays[:,8] 
        w+= beam.rays[:,15]*beam.rays[:,15] + beam.rays[:,16]*beam.rays[:,16] + beam.rays[:,17]*beam.rays[:,17]
        xStd  = wstd(self.rays[:,0],w)
        yStd  = wstd(self.rays[:,1],w)
        zStd  = wstd(self.rays[:,2],w)
        xpStd = wstd(self.rays[:,3],w)
        ypStd = wstd(self.rays[:,4],w)
        zpStd = wstd(self.rays[:,5],w)
        eStd  = wstd(self.rays[:,10],w)/AE2V
      else:
        xStd  = self.rays[:,0].std()
        yStd  = self.rays[:,1].std()
        zStd  = self.rays[:,2].std()
        xpStd = self.rays[:,3].std()
        ypStd = self.rays[:,4].std()
        zpStd = self.rays[:,5].std()
        eStd  = self.rays[:,10].std()/AE2V
      return xStd,yStd,zStd,xpStd,ypStd,zpStd,eStd
    except AttributeError:
      print ('getStandardDeviations: No rays')


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
        return column

    if nolost == 1:
        f  = numpy.where(ray[:,9] > 0.0)
        if len(f[0])==0:
            print ('getshonecol: no GOOD rays, returning empty array')
            return numpy.empty(0)
        return column[f]

    if nolost == 2:
        f  = numpy.where(ray[:,9] < 0.0)
        if len(f[0])==0:
            print ('getshonecol: no BAD rays, returning empty array')
            return numpy.empty(0)
        return column[f]

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


      col = col - 1
      if ref == 1: ref = 23


      if ref==0:
        x, a = self.getshcol((col+1,10))
        w = numpy.ones(len(x))
      else:
        x, a, w = self.getshcol((col+1,10,ref))

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

      if write != None:
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
    if fparams==None:
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


  def setDimensions(self,fshape=0,params=numpy.zeros(4,dtype=numpy.float64)):
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
    pass
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
        if self.FSHAPE == 1:
            txt += 'Mirror dimensions ( rectangular ):\n'
            txt += '          X plus: %f, X minus: %f, Y plus: %f, Y minus: %f\n'%\
                   (self.RWIDX1,self.RWIDX2,self.RLEN1,self.RLEN2)
        if self.FSHAPE == 2:
            txt += 'Mirror dimensions ( elliptical ) :\n'
            txt += '          Major Axis: %f, Minor axis: %f \n'%\
                    (self.RWIDX1,self.RWIDX2)
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
class Beamline(list):
  def __init__(self):
    self = list.__init__()
    self.type = type(OE)

  def append(self,item):
    if isinstance(item,str):
      if item.find('crl.')!=-1: 
        self.appendCRL(item)
      else:
        txt = item
        item = OE()
        item.load(txt)
    if not isinstance(item,self.type): raise TypeError( 'item is not of type %s' % self.type )
    super(Beamline,self).append(item)

  def trace(self,beam):
    if beam.rays==None:
      raise ValueError( 'beam not initialized yet' )

    for i in range(len(self)):
      beam.traceOE(self[i],i+1)
      if self[i].F_WRITE==1 or self[i].F_WRITE==2:  beam.write('star.%02d' % i+1)

  def prepareInputShadow3Executables(self):
    f = file('system.dat','w')
    for i in range(len(self)):
      f.write('start.%02d\n' % i+1)
      self[i].write('start.%02d' % i+1)
    f.close()

  def distances(self):
    distance = [ self[0].T_SOURCE ]
    distance.extend( [ self[i-1].T_IMAGE+self[i].T_SOURCE for i in range(1,len(self))] )
    distance.append(self[-1].T_IMAGE)
    return distance

  def writeDistances(self,f=sys.stdout):
    dist = self.distances()
    #print >>f, 'distance from source to oe1: %f' % dist[0]
    print ('distance from source to oe1: %f' % dist[0], file = f)
    for i in range(1,len(self)):
      #print >>f, 'distance from oe%d to oe%d: %f' % (i,i+1,dist[i])
      print ('distance from oe%d to oe%d: %f' % (i,i+1,dist[i]), file = f)
    #print >>f, 'distance from oe%d to image' % (len(self),dist[-1])
    #print >>f, 'distance from source to image' % (sum(dist))
    print ('distance from oe%d to image' % (len(self),dist[-1]), file = f)
    print ('distance from source to image' % (sum(dist)), file = f)

  def appendCRL(self,crlfilename):
    f = file(crlfilename,'r')
    txt = f.read()
    f.close()




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
    write_shadowfiles = 1
    do_test = 2 # 1=only source ; 2= source and trace

    if do_test >= 1:
        print('running >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> source')
        src = Source()

        #set Gaussian source
        sh, sv, shp, svp = 100e-4, 10e-4, 10e-6, 1e-6
        src.set_gauss(sh, sv, shp, svp)
        if write_shadowfiles: src.write('start.00')

        #run shadow source
        beam = Beam()
        beam.genSource(src)
        if write_shadowfiles:
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

    if do_test >= 2:
        print('running >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> trace')

        oe1 = OE()
        if write_shadowfiles: oe1.write('start.01')
        #oe1.load('tmp/start.01')
        beam.traceOE(oe1,1)
        if write_shadowfiles:
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







