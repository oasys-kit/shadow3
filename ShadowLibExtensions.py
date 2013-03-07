#
# This file contains the new classes programmed in python  added to the 
# main objects (Beam, OE and Source) defined in C (in ShadowLib)
#
# It also define GeometricSource and Beamline
#
import ShadowLib
from numpy import float64, zeros, array 
import sys


class Beam(ShadowLib.Beam):
  def __init__(self, N=None):
    ShadowLib.Beam.__init__(self)
    if N is not None:
      self.SetRayZeros(N)

  def retrace(self,dist):
    try:
      tof = (-self.rays[:,1].flatten() + dist)/self.rays[:,4].flatten()
      self.rays[:,0] += tof*self.rays[:,3].flatten()
      self.rays[:,2] += tof*self.rays[:,5].flatten()
    except AttributeError:
      print 'retrace: No rays'

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

class GeometricSource(ShadowLib.Source):
  def __init__(self):
    ShadowLib.Source.__init__(self)

  def setNumberRaysInit(self,nRays=5000,seed=6775431):
    self.N = nRays
    if(seed%2==0): seed += 1
    self.ISTAR1 = seed
    self.NCOL = 18 # shadow3 it will be rewritten anyway

  def setSpaceDistribution(self,transverse='gauss',longitude='no',param=zeros(3)):
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
      print 'setSpaceDistribution: wrong distribution name'
#TODO    
#  def setAngleDistribution(self,angle='gauss',)

#
# TODO: Put all classes starting with capital letter
# in the meantime, bind as this one:  
#
class geometricSource(GeometricSource):
  def __init__(self): 
    print(" DEPRECATION WARNING: Use GeometricSource ")
    GeometricSource.__init__(self)

class OE(ShadowLib.OE):
  def __init__(self):
    ShadowLib.OE.__init__(self)

# here methods to initialize the OE
  def setScreens(self, 
                 n_screen=1, 
                 i_screen=zeros(10), 
                 i_abs=zeros(10), 
                 i_slit=zeros(10), 
                 i_stop=zeros(10), 
                 k_slit=zeros(10),
                 thick=zeros(10),
                 file_abs=array(['', '', '', '', '', '', '', '', '', '']),
                 rx_slit=zeros(10),
                 rz_slit=zeros(10),
                 sl_dis=zeros(10),
                 file_src_ext=array(['', '', '', '', '', '', '', '', '', ''])
                 ):
    self.F_SCREEN = 1
    if n_screen<=10 and n_screen>0: 
      self.N_SCREEN = n_screen 
    else:
      print "Shadow cannot handle more then 10 screens"
      return
    self.I_SCREEN     = i_screen
    self.I_ABS        = i_abs
    self.I_SLIT       = i_slit
    self.I_STOP       = i_stop
    self.K_SLIT       = k_slit
    self.THICK        = thick
    self.FILE_ABS     = file_abs
    self.RX_SLIT      = rx_slit
    self.RZ_SLIT      = rz_slit
    self.SL_DIS       = sl_dis
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
    self.CYL_ANG = cyl_ang
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

  def setReflectivityFull(self,f_refl=0,file_refl='GAAS.SHA',rparams=zeros(2,dtype=float64),f_thick=0):
    self.F_REFLEC = 1
    self.F_REFL = f_refl
    self.FILE_REFL = file_refl
    self.ALFA = rparams[0]
    self.GAMMA = rparams[1]
    self.F_THICK = f_thick
    return self

  def setReflectivityScalar(self,f_refl=0,file_refl='GAAS.SHA',rparams=zeros(2,dtype=float64),f_thick=0):
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


  def setCCC(self,ccc=array([1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],dtype=float64)):
    self.FMIRR = 10
    self.F_EXT = 1
    self.CCC[:] = ccc[:]
    return self


  def setRipple(self,f_g_s=0,xyAmpWavPha=array([0.0,0.0,0.0,0.0,0.0,0.0],dtype=float64),file_rip=''):
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


  def setDimensions(self,fshape=0,params=zeros(4,dtype=float64)):
    self.FHIT_C = 1
    self.FSHAPE = fshape
    self.RLEN1  = param[0]
    self.RLEN2  = param[1]
    self.RWIDX1 = param[2]
    self.RWIDX2 = param[3]
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

  def setCrystal(self,file_refl='',a_bragg=0.0):
    self.F_CRYSTAL = 1
    self.FILE_REFL = file_refl #TODO check if it does not change place in memory
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


  def setGratingHolographic(self,holo_params=zeros(7,dtype=float64),f_pw=2,f_pw_c=0,f_virtual=0):
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


  def setGratingPolynomial(self,poly_params=zeros(5,dtype=float64),f_rul_abs=0):
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

  def setAutoTuning(self,f_phot_cent=0,photo_cent=5000.0,r_lambda=100.0):
    self.F_CENTRAL = 1
    self.F_PHOT_CENT = f_phot_cent
    self.PHOT_CENT = phot_cent
    self.R_LAMBDA = r_lambda
    return self

  def setAutoMonochromator(self,f_phot_cent=0,phot_cent=5000.0,r_lambda=100.0,f_mono=0,f_hunt=1,hparam=zeros(3,dtype=float64)):
    self.setAutoTuning(f_photo_cent=f_photo_cent,phot_cent=phot_cent,r_lambda=r_lambda)
    self.F_MONO = f_mono
    self.F_HUNT = f_hunt
    self.HUNT_H = hparam[0]
    self.HUNT_L = hparam[1]
    self.BLAZE = hparam[2]
    return self


# not yet ready
class Beamline(list):
  def __init__(self):
    self = list.__init__()
    self.type = type(OE)

  def append(self,item):
    if isinstance(item,str):
      if item.find("crl.")!=-1: 
        self.appendCRL(item)
      else:
        txt = item
        item = OE()
        item.load(txt)
    if not isinstance(item,self.type): raise TypeError, 'item is not of type %s' % self.type
    super(Beamline,self).append(item)

  def trace(self,beam):
    if beam.rays==None:
      raise ValueError, 'beam not initialized yet'

    for i in range(len(self)):
      beam.traceOE(self[i],i+1)
      if self[i].F_WRITE==1 or self[i].F_WRITE==2:  beam.write("star.%02d" % i+1)

  def prepareInputShadow3Executables(self):
    f = file('system.dat','w')
    for i in range(len(self)):
      f.write('start.%02d\n' % i+1)
      self[i].write("start.%02d" % i+1)
    f.close()

  def distances(self):
    distance = [ self[0].T_SOURCE ]
    distance.extend( [ self[i-1].T_IMAGE+self[i].T_SOURCE for i in range(1,len(self))] )
    distance.append(self[-1].T_IMAGE)
    return distance

  def writeDistances(self,f=sys.stdout):
    dist = self.distances()
    print >>f, "distance from source to oe1: %f" % dist[0]
    for i in range(1,len(self)):
      print >>f, "distance from oe%d to oe%d: %f" % (i,i+1,dist[i])
    print >>f, "distance from oe%d to image" % (len(self),dist[-1])
    print >>f, "distance from source to image" % (sum(dist))

  def appendCRL(self,crlfilename):
    f = file(crlfilename,"r")
    txt = f.read()
    f.close()


class Source(ShadowLib.Source):
  def __init__(self):
    ShadowLib.Source.__init__(self)

