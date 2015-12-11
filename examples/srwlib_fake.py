#############################################################################
# SRWLib for Python v 0.061
#############################################################################

#****************************************************************************
#****************************************************************************
# SRWLib Python Classes
#****************************************************************************
#****************************************************************************
import sys
class SRWLParticle(object):
  """Charged Particle"""

  def __init__(self, _x=0, _y=0, _z=0, _xp=0, _yp=0, _gamma=1, _relE0=1, _nq=-1):
    """
    :param _x: horizontal coordinate [m]
    :param _y: vertical coordinate [m]
    :param _z: longitudinal coordinate [m]
    :param _xp: horizontal transverse velocity component btx = vx/c (angles for relativistic particle)
    :param _yp: vertical transverse velocity component bty = vy/c (angles for relativistic particle)
    :param _gamma: relative energy
    :param _relE0: rest mass (energy) in units of electron rest mass, e.g. 1 for electron, 1836.1526988 (=938.272013/0.510998902) for proton
    :param _nq: charge of the particle related to absolute value of electron charge, -1 for electron, 1 for positron and for proton
    """
    self.x = _x
    self.y = _y
    self.z = _z
    self.xp = _xp
    self.yp = _yp
    self.gamma = _gamma
    self.relE0 = _relE0
    self.nq = _nq

  @property
  def energy(self):
    return self.gamma*5.10998902e-4*self.relE0
  @property
  def relRestMass(self):
    return self.relE0*5.10998902e-4

  @energy.setter
  def energy(self,value):
    self.gamma = value/5.10998902e-4/self.relE0
  @relRestMass.setter
  def relRestMass(self,value):
    self.relE0=value/5.10998902e-4

#****************************************************************************
class SRWLPartBeam(object):
  """Particle Beam"""

  def __init__(self, _Iavg=0, _nPart=0, _partStatMom1=None, _arStatMom2=None):
    """
    :param _Iavg: average current [A]
    :param _nPart: number of particles (in a bunch)
    :param _partStatMom1: particle type used for 1st order statistical moments
    :param _arStatMom2: 2nd order statistical moments
      [ 0]: <(x-x0)^2>
      [ 1]: <(x-x0)*(xp-xp0)>
      [ 2]: <(xp-xp0)^2>
      [ 3]: <(y-y0)^2>
      [ 4]: <(y-y0)*(yp-yp0)>
      [ 5]: <(yp-yp0)^2>
      [ 6]: <(x-x0)*(y-y0)>
      [ 7]: <(xp-xp0)*(y-y0)>
      [ 8]: <(x-x0)*(yp-yp0)>
      [ 9]: <(xp-xp0)*(yp-yp0)>
      [10]: <(E-E0)^2>/E0^2
      [11]: <(s-s0)^2>
      [12]: <(s-s0)*(E-E0)>/E0
      [13]: <(x-x0)*(E-E0)>/E0
      [14]: <(xp-xp0)*(E-E0)>/E0
      [15]: <(y-y0)*(E-E0)>/E0
      [16]: <(yp-yp0)*(E-E0)>/E0
      [17]: <(x-x0)*(s-s0)>
      [18]: <(xp-xp0)*(s-s0)>
      [19]: <(y-y0)*(s-s0)>
      [20]: <(yp-yp0)*(s-s0)>
    """
    self.Iavg = _Iavg
    self.nPart = _nPart
    self.partStatMom1 = SRWLParticle() if _partStatMom1 is None else _partStatMom1
    self.arStatMom2 = array('d', [0] * 21) if _arStatMom2 is None else _arStatMom2

  @property
  def arrStatMom2(self): 
    return self.arStatMom2 

  @arrStatMom2.setter
  def arrStatMom2(self,value):
    self.arStatMom2 = value

  def save(self,fname,mode='hdf5'):
    """
    save Charged Particle instance to file
    :param fname: filename
    :param mode: 'hdf5' (default) or 'bin'
    """
    try:
      h5py  = __import__('h5py',  globals(), locals(), [], -1)
      numpy = __import__('numpy', globals(), locals(), [], -1)
    except ImportError:
      mode = 'bin'
    if mode=='hdf5':
      f = h5py.File(fname,'w')
      fkeys=set([])
      f.visit(fkeys.add)
      h5grp = "Source/Beam/Particle"
      if h5grp not in fkeys: f.create_group(h5grp)
      varnames = ["energy","nq","relRestMass","x","y","z","xp","yp"]
      varDesc = aux_hdf5_obj_dict(self,varnames)
      aux_hdf5_dict_save(f,h5grp,varDesc)
      f.close()
    else:
      f = open(fname+'_part.dat','wb')
      pickle.dump(self,f,pickle.HIGHEST_PROTOCOL)
      f.close()
  


#****************************************************************************
class SRWLMagFld(object):
  """Magnetic Field (base class)"""
  
class SRWLMagFld3D(SRWLMagFld):
  """Magnetic Field: Arbitrary 3D"""
  
  def __init__(self, _arBx=None, _arBy=None, _arBz=None, _nx=0, _ny=0, _nz=0, _rx=0, _ry=0, _rz=0, _nRep=1, _interp=1, _arX=None, _arY=None, _arZ=None):
    """
    :param _arBx: horizontal magnetic field component array [T]
    :param _arBy: vertical magnetic field component array [T]
    :param _arBz: longitudinal magnetic field component array [T]
    :param _nx: number of magnetic field data points in the horizontal direction
    :param _ny: number of magnetic field data points in the vertical direction
    :param _nz: number of magnetic field data points in the longitudinal direction
    :param _rx: range of horizontal coordinate for which the field is defined [m]
    :param _ry: range of vertical coordinate for which the field is defined [m]
    :param _rz: range of longitudinal coordinate for which the field is defined [m]
    :param _nRep: "number of periods", i.e. number of times the field is "repeated" in the longitudinal direction
    :param _interp: interpolation method to use (e.g. for trajectory calculation), 1- bi-linear (3D), 2- (bi-)quadratic (3D), 3- (bi-)cubic (3D)
    :param _arX: optional array of horizontal transverse coordinate of an irregular 3D mesh (if this array is defined, rx will be ignored)
    :param _arY: optional array of vertical transverse coordinate of an irregular 3D mesh (if this array is defined, ry will be ignored)
    :param _arZ: optional array of longitudinal coordinate of an irregular 3D mesh (if this array is defined, rz will be ignored)
    """
    self.arBx = array('d') if _arBx is None else _arBx
    self.arBy = array('d') if _arBy is None else _arBy
    self.arBz = array('d') if _arBz is None else _arBz
    self.nx = _nx
    self.ny = _ny
    self.nz = _nz
    self.rx = _rx
    self.ry = _ry
    self.rz = _rz
    self.arX = array('d') if _arX is None else _arX
    self.arY = array('d') if _arY is None else _arY
    self.arZ = array('d') if _arZ is None else _arZ
    self.nRep = _nRep
    self.interp = _interp

class SRWLMagFldM(SRWLMagFld):
  """Magnetic Field: Multipole Magnet"""
  
  def __init__(self, _G=0, _m=2, _n_or_s='n', _Leff=0, _Ledge=0):
    """
    :param _G: field parameter [T] for dipole, [T/m] for quadrupole (negative means defocusing for x), [T/m^2] for sextupole, [T/m^3] for octupole
    :param _m: multipole order 1 for dipole, 2 for quadrupoole, 3 for sextupole, 4 for octupole
    :param _n_or_s: normal ('n') or skew ('s')
    :param _Leff: effective length [m]
    :param _Ledge: "soft" edge length for field variation from 10% to 90% [m]; G/(1 + ((z-zc)/d)^2)^2 fringe field dependence is assumed
    """
    self.G = _G
    self.m = _m
    self.n_or_s = _n_or_s
    self.Leff = _Leff
    self.Ledge = _Ledge

class SRWLMagFldS(SRWLMagFld):
  """Magnetic Field: Solenoid"""
  
  def __init__(self, _B=0, _Leff=0):
    """
    :param _B: magnetic field [T]
    :param _Leff: effective length [m]
    """
    self.B = _B
    self.Leff = _Leff

class SRWLMagFldH(SRWLMagFld):
  """Magnetic Field: Undulator Harmonic"""
  
  def __init__(self, _n=1, _h_or_v='v', _B=0, _ph=0, _s=1, _a=1):
    """
    :param _n: harmonic number
    :param _h_or_v: magnetic field plane horzontal ('h') or vertical ('v')
    :param _B: magnetic field amplitude [T]
    :param _ph: initial phase [rad]
    :param _s: symmetry vs longitudinal position 1 - symmetric (B ~ cos(2*Pi*n*z/per + ph)) , -1 - anti-symmetric (B ~ sin(2*Pi*n*z/per + ph))
    :param _a: coefficient for transverse depenednce B*cosh(2*Pi*n*a*y/per)*cos(2*Pi*n*z/per + ph)
    """
    self.n = _n
    self.h_or_v = _h_or_v
    self.B = _B
    self.ph = _ph
    self.s = _s
    self.a = _a

class SRWLMagFldU(SRWLMagFld):
  """Magnetic Field: Undulator"""
  
  def __init__(self, _arHarm=None, _per=0, _nPer=0):
    """
    :param _arHarm: array of field harmonics
    :param _per: period length [m]
    :param _nPer: number of periods (will be rounded to integer)
    """
    self.arHarm = [] if _arHarm is None else _arHarm
    self.per = _per
    self.nPer = _nPer

  def allocate(self, _nHarm):
    self.arHarm = [SRWLMagFldH()]*_nHarm

class SRWLMagFldC(SRWLMagFld):
  """Magnetic Field: Container"""
  
  def __init__(self, _arMagFld=None, _arXc=None, _arYc=None, _arZc=None):
    """
    :param _arMagFld: magnetic field structures array
    :param _arXc: horizontal center positions of magnetic field elements in arMagFld array [m]
    :param _arYc: vertical center positions of magnetic field elements in arMagFld array [m]
    :param _arZc: longitudinal center positions of magnetic field elements in arMagFld array [m]
    """
    self.arMagFld = [] if _arMagFld is None else _arMagFld
    self.arXc = array('d') if _arXc is None else _arXc
    self.arYc = array('d') if _arYc is None else _arYc
    self.arZc = array('d') if _arZc is None else _arZc

  def allocate(self, _nElem):
    self.arMagFld = [SRWLMagFld()]*_nElem
    self.arXc = array('d', [0]*_nElem)
    self.arYc = array('d', [0]*_nElem)
    self.arZc = array('d', [0]*_nElem)



#****************************************************************************
class SRWLPrtTrj(object):
  """Charged Particle Trajectory"""

  def __init__(self, _arX=None, _arXp=None, _arY=None, _arYp=None, _arZ=None, _arZp=None, _np=0, _ctStart=0, _ctEnd=0, _partInitCond=None):
    """
    :param _arX: array of horizontal position [m]
    :param _arXp: array of horizontal relative velocity (trajectory angle) [rad]
    :param _arY: array of vertical position [m]
    :param _arYp: array of vertical relative velocity (trajectory angle) [rad]
    :param _arZ: array of longitudinal positions [m]
    :param _arZp: array of longitudinal relative velocity [rad]
    :param _np: number of trajectory points
    :param _ctStart: start value of independent variable (c*t) for which the trajectory should be (/is) calculated (is constant step enough?)
    :param _ctEnd: end value of independent variable (c*t) for which the trajectory should be (/is) calculated (is constant step enough?)
    :param _partInitCond: particle type and initial conditions for which the trajectory should be (/is) calculated
    """
    self.arX = array('d') if _arX is None else _arX
    self.arY = array('d') if _arY is None else _arY
    self.arY = array('d') if _arZ is None else _arZ
    self.arXp = array('d') if _arXp is None else _arXp
    self.arYp = array('d') if _arYp is None else _arYp
    self.arZp = array('d') if _arZp is None else _arZp
    self.np = _np
    self.ctStart = _ctStart
    self.ctEnd = _ctEnd
    self.partInitCond = SRWLParticle() if _partInitCond is None else _partInitCond

  def allocate(self, _np):
    _np = int(_np)
    self.arX = array('d', [0] * _np)
    self.arXp = array('d', [0] * _np)
    self.arY = array('d', [0] * _np)
    self.arYp = array('d', [0] * _np)
    self.arZ = array('d', [0] * _np)
    self.arZp = array('d', [0] * _np)
    self.np = _np



#****************************************************************************
class SRWLKickM(object):
  """Kick Matrix (for fast trajectory calculation)"""
  
  def __init__(self, _arKickMx=None, _arKickMy=None, _order=2, _nx=0, _ny=0, _nz=0, _rx=0, _ry=0, _rz=0, _x=0, _y=0, _z=0):
    """
    :param _arKickMx: horizontal kick-matrix (tabulated on the same transverse grid vs x and y as vertical kick-matrix)
    :param _arKickMy: vertical kick-matrix (tabulated on the same transverse grid vs x and y as horizontal kick-matrix)
    :param _order: kick order: 1- first order (in this case kick matrix data is assumed to be in [T*m]), 2- second order (kick matrix data is assumed to be in [T^2*m^2])
    :param _nx: numbers of points in kick matrices in horizontal direction
    :param _ny: numbers of points in kick matrices in vertical direction
    :param _nz: number of steps in longitudinal direction
    :param _rx: range covered by kick matrices in horizontal direction [m]
    :param _ry: range covered by kick matrices in vertical direction [m]
    :param _rz: extension in longitudinal direction [m]
    :param _x: horizontal coordinate of center point [m]
    :param _y: vertical coordinate of center point [m]
    :param _z: longitudinal coordinate of center point [m]
    """
    self.arKickMx = array('d') if _arKickMx is None else _arKickMx
    self.arKickMy = array('d') if _arKickMy is None else _arKickMy
    self.order = _order
    self.nx = _nx
    self.ny = _ny
    self.nz = _nz
    self.rx = _rx
    self.ry = _ry
    self.rz = _rz
    self.x = _x
    self.y = _y
    self.z = _z



#****************************************************************************
class SRWLGsnBm(object):
  """Gaussian Beam"""
  
  def __init__(self, _x=0, _y=0, _z=0, _xp=0, _yp=0, _avgPhotEn=1, _pulseEn=1, _repRate=1, _polar=1, _sigX=10e-06,
         _sigY=10e-06, _sigT=1e-15, _mx=0, _my=0):
    """
    :param _x: average horizontal coordinates of waist [m]
    :param _y: average vertical coordinates of waist [m]
    :param _z: average longitudinal coordinate of waist [m]
    :param _xp: average horizontal angle at waist [rad]
    :param _yp: average verical angle at waist [rad]
    :param _avgPhotEn: average photon energy [eV]
    :param _pulseEn: energy per pulse [J]
    :param _repRate: rep. rate [Hz]
    :param _polar: polarization 1- lin. hor., 2- lin. vert., 3- lin. 45 deg., 4- lin.135 deg., 5- circ. right, 6- circ. left
    :param _sigX: rms beam size vs horizontal position [m] at waist (for intensity)
    :param _sigY: rms beam size vs vertical position [m] at waist (for intensity)
    :param _sigT: rms pulse duration [s] (for intensity)
    :param _mx: transverse Gauss-Hermite mode order in horizontal direction
    :param _my: transverse Gauss-Hermite mode order in vertical direction
    """
    self.x = _x
    self.y = _y
    self.z = _z
    self.xp = _xp
    self.yp = _yp
    self.avgPhotEn = _avgPhotEn
    self.pulseEn = _pulseEn
    self.repRate = _repRate
    self.polar = _polar
    self.sigX = _sigX
    self.sigY = _sigY
    self.sigT = _sigT
    self.mx = _mx
    self.my = _my



#****************************************************************************
class SRWLRadMesh(object):
  """Radiation Mesh (Sampling)"""
  
  def __init__(self, _eStart=0, _eFin=0, _ne=1, _xStart=0, _xFin=0, _nx=1, _yStart=0, _yFin=0, _ny=1, _zStart=0):
    """
    :param _eStart: initial value of photon energy (/time)
    :param _eFin: final value of photon energy (/time)
    :param _ne: number of points vs photon energy
    :param _xStart: initial value of horizontal position
    :param _xFin: final value ofhorizontal position
    :param _nx: number of points vs horizontal positions
    :param _yStart: initial value of vertical position
    :param _yFin: final value of vertical position
    :param _ny: number of points vs vertical positions
    :param _zStart: longitudinal position
    """
    self.eStart = _eStart
    self.eFin = _eFin
    self.ne = _ne
    self.xStart = _xStart
    self.xFin = _xFin
    self.nx = _nx
    self.yStart = _yStart
    self.yFin = _yFin
    self.ny = _ny
    self.zStart = _zStart

  @property
  def zFin(self):
    return self.zStart

  @zFin.setter
  def zFin(self,value):
    self.zStart=value # or maybe not this is the less important alias

#****************************************************************************
class SRWLStokes(object):
  """Radiation Stokes Parameters"""
  
  #def __init__(self, _arS0=None, _arS1=None, _arS2=None, _arS3=None, _typeStokes='f', _eStart=0, _eFin=0, _ne=0, _xStart=0, _xFin=0, _nx=0, _yStart=0, _yFin=0, _ny=0):
  def __init__(self, _arS=None, _typeStokes='f', _eStart=0, _eFin=0, _ne=0, _xStart=0, _xFin=0, _nx=0, _yStart=0, _yFin=0, _ny=0):
    """
    :param _arS: flat C-aligned array of all Stokes components (outmost loop over Stokes parameter number); NOTE: only 'f' (float) is supported for the moment (Jan. 2012)
    :param _typeStokes: electric field numerical type: 'f' (float) or 'd' (double, not supported yet)
    :param _eStart: initial value of photon energy (/time)
    :param _eFin: final value of photon energy (/time)
    :param _ne: numbers of points vs photon energy
    :param _xStart: initial value of horizontal position
    :param _xFin: final value of photon horizontal position
    :param _nx: numbers of points vs horizontal position
    :param _yStart: initial value of vertical position
    :param _yFin: final value of vertical position
    :param _ny: numbers of points vs vertical position
    """
    self.arS = _arS #flat C-aligned array of all Stokes components (outmost loop over Stokes parameter number); NOTE: only 'f' (float) is supported for the moment (Jan. 2012)
    self.numTypeStokes = _typeStokes #electric field numerical type: 'f' (float) or 'd' (double)
    self.mesh = SRWLRadMesh(_eStart, _eFin, _ne, _xStart, _xFin, _nx, _yStart, _yFin, _ny) #to make mesh an instance variable
    self.avgPhotEn = 0 #average photon energy for time-domain simulations  
    self.presCA = 0 #presentation/domain: 0- coordinates, 1- angles
    self.presFT = 0 #presentation/domain: 0- frequency (photon energy), 1- time
    self.unitStokes = 1 #electric field units: 0- arbitrary, 1- Phot/s/0.1%bw/mm^2 ?

    nProd = _ne*_nx*_ny #array length to store one component of complex electric field

  @property
  def arrStokes(self): 
    return self.arS
  @property
  def photonEnergy(self):
    return self.avgPhotEn
  @property
  def wSpace(self):
    return self.presCA
  @property
  def wDomain(self):
    return self.presFT
  @arrStokes.setter
  def arrStokes(self,value):
    self.arS=value
  @photonEnergy.setter
  def photonEnergy(self,value):
    self.avgPhotEn=value
  @wSpace.setter
  def wSpace(self,value):
    self.presCA=value
  @wDomain.setter
  def wDomain(self,value):
    self.presFT=value

  def save(self,fname,mode='hdf5'):
    """
    save Stokes instance to hdf5 file
    :param fname: filename
    :param mode: 'hdf5' (default) or 'bin'
    """
    try:
      h5py  = __import__('h5py',  globals(), locals(), [], -1)
      numpy = __import__('numpy', globals(), locals(), [], -1)
    except ImportError:
      mode='bin'
    if mode=='hdf5':
      f = h5py.File(fname,'w')
      fkeys=set([])
      f.visit(fkeys.add)
      h5grp = "Data"
      if h5grp not in fkeys: f.create_group(h5grp)
      varnames = ["arrStokes"]
      varDesc = aux_hdf5_obj_dict(self,varnames,isarray=True)
      aux_hdf5_dict_save(f,h5grp,varDesc,shape=(4,self.mesh.ny,self.mesh.nx,self.mesh.ne))
      f.close()
      self.mesh.save(fname)
    else:
      f = open(fname+'_stk.dat','wb')
      pickle.dump(self,f,pickle.HIGHEST_PROTOCOL)
      f.close()


#****************************************************************************
class SRWLWfr(object):
  """Radiation Wavefront (Electric Field)"""

  def __init__(self, _arEx=None, _arEy=None, _typeE='f', _eStart=0, _eFin=0, _ne=0, _xStart=0, _xFin=0, _nx=0, _yStart=0, _yFin=0, _ny=0, _zStart=0, _partBeam=None):
    """
    :param _arEx: horizontal complex electric field component array; NOTE: only 'f' (float) is supported for the moment (Jan. 2011)
    :param _arEy: vertical complex electric field component array
    :param _typeE: electric field numerical type: 'f' (float) or 'd' (double)
    :param _eStart: initial value of photon energy (/time)
    :param _eFin: final value of photon energy (/time)
    :param _ne: numbers of points vs photon energy
    :param _xStart: initial value of horizontal positions
    :param _xFin: final value of horizontal positions
    :param _nx: numbers of points vs horizontal positions
    :param _yStart: initial vertical positions
    :param _yFin: final value of vertical positions
    :param _ny: numbers of points vs vertical positions
    :param _zStart: longitudinal position
    :param _partBeam: particle beam source; strictly speaking, it should be just SRWLParticle; however, "multi-electron" information can appear useful for those cases when "multi-electron intensity" can be deduced from the "single-electron" one by convolution

    Some additional parameters, that are not included in constructor arguments:
    Rx, Ry: instant wavefront radii
    dRx, dRy: error of wavefront radii
    xc, yc: transverse coordinates of wavefront instant "source center"
    avgPhotEn: average photon energy for time-domain simulations
    presCA: presentation/domain: 0- coordinates, 1- angles
    presFT: presentation/domain: 0- frequency (photon energy), 1- time
    unitElFld: electric field units: 0- arbitrary, 1- sqrt(Phot/s/0.1%bw/mm^2)
    arElecPropMatr: effective 1st order "propagation matrix" for electron beam parameters
    arMomX, arMomY: statistical moments (of Wigner distribution); to check the exact number of moments required
    arWfrAuxData: array of auxiliary wavefront data
    """
    self.arEx = _arEx
    self.arEy = _arEy
    self.mesh = SRWLRadMesh(_eStart, _eFin, _ne, _xStart, _xFin, _nx, _yStart, _yFin, _ny)
    self.numTypeElFld = _typeE
    self.partBeam = SRWLPartBeam() if _partBeam is None else _partBeam

    self.Rx = 0 #instant wavefront radii
    self.Ry = 0
    self.dRx = 0 #error of wavefront radii
    self.dRy = 0
    self.xc = 0 #instant transverse coordinates of wavefront instant "source center"
    self.yc = 0
    self.avgPhotEn = 0 #average photon energy for time-domain simulations
    self.presCA = 0 #presentation/domain: 0- coordinates, 1- angles
    self.presFT = 0 #presentation/domain: 0- frequency (photon energy), 1- time
    self.unitElFld = 1 #electric field units: 0- arbitrary, 1- sqrt(Phot/s/0.1%bw/mm^2) ?
    self.arElecPropMatr = array('d', [0] * 20) #effective 1st order "propagation matrix" for electron beam parameters
    self.arMomX = array('d', [0] * 11 * _ne) #statistical moments (of Wigner distribution); to check the exact number of moments required
    self.arMomY = array('d', [0] * 11 * _ne)
    self.arWfrAuxData = array('d', [0] * 30) #array of auxiliary wavefront data

    nProd = _ne * _nx * _ny #array length to store one component of complex electric field
    EXNeeded = 0
    EYNeeded = 0

  @property
  def arrEhor(self):
    return self.arEx
  @property
  def arrEver(self):
    return self.arEy
  @property
  def arrElecPropMatr(self):
    return self.arElecPropMatr
  @property
  def arrMomX(self): 
    return self.arMomX
  @property
  def arrMomY(self): 
    return self.arMomY
  @property
  def photonEnergy(self):
    return self.avgPhotEn
  @property
  def wSpace(self):
    return self.presCA
  @property
  def wDomain(self):
    return self.presFT
  @property
  def wEFieldUnit(self):
    return self.unitElFld

  @arrEhor.setter
  def arrEhor(self,value):
    self.arEx=value
  @arrEver.setter
  def arrEver(self,value):
    self.arEy=value
  @arrElecPropMatr.setter
  def arrElecPropMatr(self,value):
    self.arElecPropMatr = value
  @arrMomX.setter
  def arrMomX(self,value):
    self.arMomX = value
  @arrMomY.setter
  def arrMomY(self,value):
    self.arMomY = value
  @photonEnergy.setter
  def photonEnergy(self,value):
    self.avgPhotEn=value
  @wSpace.setter
  def wSpace(self,value):
    self.presCA=value
  @wDomain.setter
  def wDomain(self,value):
    self.presFT=value
  @wEFieldUnit.setter
  def wEFieldUnit(self,value):
    self.unitElFld=value

#****************************************************************************
class SRWLOpt(object):
  """Optical Element (base class)"""

class SRWLOptD(SRWLOpt):
  """Optical Element: Drift Space"""
  
  def __init__(self, _L=0):
    """
    :param _L: Length [m]
    """
    self.L = _L

class SRWLOptA(SRWLOpt):
  """Optical Element: Aperture / Obstacle"""
  
  def __init__(self, _shape='r', _ap_or_ob='a', _Dx=0, _Dy=0, _x=0, _y=0):
    """
    :param _shape: 'r' for rectangular, 'c' for circular
    :param _ap_or_ob: 'a' for aperture, 'o' for obstacle
    :param _Dx: horizontal transverse dimension [m]; in case of circular aperture, only Dx is used for diameter
    :param _Dy: vertical transverse dimension [m]; in case of circular aperture, Dy is ignored
    :param _x: horizontal transverse coordinate of center [m]
    :param _y: vertical transverse coordinate of center [m]
    """
    self.shape = _shape #'r' for rectangular, 'c' for circular
    self.ap_or_ob = _ap_or_ob #'a' for aperture, 'o' for obstacle
    self.Dx = _Dx #transverse dimensions [m]; in case of circular aperture, only Dx is used for diameter
    self.Dy = _Dy
    self.x = _x #transverse coordinates of center [m]
    self.y = _y

class SRWLOptL(SRWLOpt):
  """Optical Element: Thin Lens"""
  
  def __init__(self, _Fx=1e+23, _Fy=1e+23, _x=0, _y=0):
    """
    :param _Fx: focal length in horizontal plane [m]
    :param _Fy: focal length in vertical plane [m]
    :param _x: horizontal coordinate of center [m]
    :param _y: vertical coordinate of center [m]
    """
    self.Fx = _Fx #focal lengths [m]
    self.Fy = _Fy
    self.x = _x #transverse coordinates of center [m]
    self.y = _y

class SRWLOptZP(SRWLOpt):
  """Optical Element: Thin Lens"""
  
  def __init__(self, _nZones=100, _rn=0.1e-03, _thick=10e-06, _delta1=1e-06, _atLen1=0.1, _delta2=0, _atLen2=1e-06, _x=0, _y=0):
    """
    :param _nZones: total number of zones
    :param _rn: auter zone radius [m]
    :param _thick: thickness [m]
    :param _delta1: refractuve index decrement of the "main" material
    :param _atLen1: attenuation length [m] of the "main" material
    :param _delta2: refractuve index decrement of the "complementary" material
    :param _atLen2: attenuation length [m] of the "complementary" material
    :param _x: horizontal transverse coordinate of center [m]
    :param _y: vertical transverse coordinates of center [m]
    """
    self.nZones = _nZones #total number of zones
    self.rn = _rn #auter zone radius [m]
    self.thick = _thick #thickness [m]
    self.delta1 = _delta1 #refractuve index decrement of the "main" material
    self.delta2 = _delta2 #refractuve index decrement of the "complementary" material
    self.atLen1 = _atLen1 #attenuation length [m] of the "main" material
    self.atLen2 = _atLen2 #attenuation length [m] of the "complementary" material
    self.x = _x #transverse coordinates of center [m]
    self.y = _y

class SRWLOptWG(SRWLOpt):
  """Optical Element: Waveguide"""
  
  def __init__(self, _L=1, _Dx=10e-03, _Dy=10e-03, _x=0, _y=0):
    """
    :param _L: length [m]
    :param _Dx: horizontal transverse dimension [m]
    :param _Dy: vertical transverse dimension [m]
    :param _x: horizontal transverse coordinate of center [m]
    :param _y: vertical transverse coordinate of center [m]
    """
    self.L = _L #length [m]
    self.Dx = _Dx #transverse dimensions [m]
    self.Dy = _Dy
    self.x = _x #transverse coordinates of center [m]
    self.y = _y

class SRWLOptG(SRWLOpt):
  """Optical Element: Grating (planar)"""
  
  def __init__(self, _grDen=100, _disPl='v', _ang=0.7854, _m=1, _refl=1):
    """
    :param _grDen: groove density [lines/mm]
    :param _disPl: dispersion plane: 'x' ('h') or 'y' ('v')
    :param _ang: angle between optical axis and grating plane [rad]
    :param _m: output order
    :param _refl: average reflectivity (with respect to intensity)
    """
    self.grDen = _grDen #groove density [lines/mm]
    self.disPl = _disPl #dispersion plane: 'x' ('h') or 'y' ('v')
    self.ang = _ang #angle between optical axis and grating plane [rad]
    self.m = _m #output order
    self.refl = _refl #average reflectivity (with resp. to intensity)

class SRWLOptT(SRWLOpt):
  """Optical Element: Transmission (generic)"""
  
  def __init__(self, _nx=1, _ny=1, _rx=1e-03, _ry=1e-03, _arTr=None, _extTr=0, _Fx=1e+23, _Fy=1e+23, _x=0, _y=0, _ne=1, _eStart=0, _eFin=0):
    """
    :param _nx: number of transmission data points in the horizontaldirection
    :param _ny: number of transmission data points in the vertical direction
    :param _rx: range of the horizontal coordinate [m] for which the transmission is defined
    :param _ry: range of the vertical coordinate [m] for which the transmission is defined
    :param _arTr: complex C-aligned data array (of 2*ne*nx*ny length) storing amplitude transmission and optical path difference as function of transverse coordinates
    :param _extTr: transmission outside the grid/mesh is zero (0), or it is same as on boundary (1)
    :param _Fx: estimated focal length in the horizontal plane [m]
    :param _Fy: estimated focal length in the vertical plane [m]
    :param _x: horizontal transverse coordinate of center [m]
    :param _y: vertical transverse coordinate of center [m]
    :param _ne: number of transmission data points vs photon energy
    :param _eStart: initial value of photon energy
    :param _eFin: final value of photon energy
    """
    self.arTr = _arTr #complex C-aligned data array (of 2*ne*nx*ny length) storing amplitude transmission and optical path difference as function of transverse position
    self.ne = _ne #number of transmission data points vs photon energy
    self.nx = _nx #numbers of transmission data points in the horizontal and vertical directions
    self.ny = _ny
    self.eStart = _eStart #initial and final values of photon energy
    self.eFin = _eFin
    self.rx = _rx #ranges of horizontal and vertical coordinates [m] for which the transmission is defined
    self.ry = _ry
    self.extTr = _extTr #0- transmission outside the grid/mesh is zero; 1- it is same as on boundary
    self.Fx = _Fx #estimated focal lengths [m]
    self.Fy = _Fy
    self.x = _x #transverse coordinates of center [m]
    self.y = _y
    #if _ne > 1: _Fx, _Fy should be arrays vs photon energy?


class SRWLOptC(SRWLOpt):
  """Optical Element: Container"""
  
  def __init__(self, _arOpt=None, _arProp=None):
    """
    :param _arOpt: optical element structures list (or array)
    :param _arProp: list of lists of propagation parameters to be used for each individual optical element
      Each element _arProp[i] is a list in which elements mean:
      [0]: Auto-Resize (1) or not (0) Before propagation
      [1]: Auto-Resize (1) or not (0) After propagation
      [2]: Relative Precision for propagation with Auto-Resizing (1. is nominal)
      [3]: Allow (1) or not (0) for semi-analytical treatment of the quadratic (leading) phase terms at the propagation
      [4]: Do any Resizing on Fourier side, using FFT, (1) or not (0)
      [5]: Horizontal Range modification factor at Resizing (1. means no modification)
      [6]: Horizontal Resolution modification factor at Resizing
      [7]: Vertical Range modification factor at Resizing
      [8]: Vertical Resolution modification factor at Resizing
      [9]: Type of wavefront Shift before Resizing (vs which coordinates; not yet implemented)
      [10]: New Horizontal wavefront Center position after Shift (not yet implemented)
      [11]: New Vertical wavefront Center position after Shift (not yet implemented)
    """
    self.arOpt = _arOpt #optical element structures array
    self.arProp = _arProp #list of lists of propagation parameters to be used for individual optical elements


def loadParticle(fname, mode='hdf5'):
    """
    load Charged Particle instance to hdf5 file
    :param fname: filename
    :param mode: 'hdf5' (default) or 'bin'
    """
    res = SRWLParticle()
    try:
        h5py  = __import__('h5py',   globals(),  locals(),  [],  -1)
        numpy = __import__('numpy',  globals(),  locals(),  [],  -1)
        f = h5py.File(fname, 'r')
    except ImportError:
        mode = 'bin'
    except IOError:
        mode = 'bin'
    if mode=='hdf5':
        try:
            h5grp = "Source/Beam/Particle"
            varnames = ["energy", "nq", "relRestMass", "x", "y", "z", "xp", "yp"]
            res = aux_hdf5_obj_load(f, res, h5grp, varnames)
            f.close()
        except KeyError:
            err = sys.exc_info()[1]
            print("some objects have not been found,  please control hdf5 file: "+fname)
            print("SRWLParicle: ",  err.message)
    else:
        f = open(fname+'_part.dat', 'rb')
        res = pickle.load(f)
        f.close()
    return res



def loadPartBeam(fname, mode='hdf5'):
  """
  load Particle instance to hdf5 file
  :param fname: filename
  :param mode: 'hdf5' (default) or 'bin'
  """
  res = SRWLPartBeam()
  h5grp = "Source/Beam"
  try:
    h5py  = __import__('h5py',   globals(),  locals(),  [],  -1)
    numpy = __import__('numpy',  globals(),  locals(),  [],  -1)
    f = h5py.File(fname, 'r')
  except ImportError:
    mode = 'bin'
  except IOError:
    mode = 'bin'
  if mode=='hdf5':
    try:
      varnames = ["Iavg", "nPart"]
      res = aux_hdf5_obj_load(f, res, h5grp, varnames)
      varnames=["arrStatMom2"]
      res = aux_hdf5_obj_load(f, res, h5grp, varnames, isarray=True)
      f.close()
      res.partStatMom1 = loadParticle(fname)
    except KeyError:
        err = sys.exc_info()[1]
        print("some objects have not been found,  please control hdf5 file: "+fname)
        print("SRWLPartBeam: ",  err.message)
  else:
    f = open(fname+'_ebeam.dat', 'rb')
    res = pickle.load(f)
    f.close()
  return res


def loadStokes(fname, mode='hdf5'):
  """
  load Stokes instance to hdf5 file
  :param fname: filename
  :param mode: 'hdf5' (default) or 'bin'
  """
  res = SRWLStokes()
  h5grp = "Data"
  try:
    h5py  = __import__('h5py',   globals(),  locals(),  [],  -1)
    numpy = __import__('numpy',  globals(),  locals(),  [],  -1)
    f = h5py.File(fname, 'r')
  except ImportError:
    mode = 'bin'
  except IOError:
    mode = 'bin'
  if mode=='hdf5':
    try:
      varnames = ["arrStokes"]
      res = aux_hdf5_obj_load(f, res, h5grp, varnames, isarray=True)
      f.close()
      res.mesh = loadRadMesh(fname)
    except KeyError:
      err = sys.exc_info()[1]
      print("some objects have not been found,  please control hdf5 file: "+fname)
      print("SRWLStokes: ",  err.message)
  else:
    f = open(fname+'_stk.dat', 'rb')
    res = pickle.load(f)
    f.close()
  return res

def getStokesFromASCII(fname):
  import numpy as np
  nLinesHead = 11
  with open(fname, 'r') as f: hlp = f.readlines(nLinesHead)
  ne, nx, ny = [int(hlp[i].replace('#', '').split()[0]) for i in [3, 6, 9]]
  ns = 1
  testStr = hlp[nLinesHead-1]
  if testStr[0]=='#': ns = int(testStr.replace('#', '').split()[0])
  e0, e1, x0, x1, y0, y1 = [float(hlp[i].replace('#', '').split()[0]) for i in [1, 2, 4, 5, 7, 8]]
  stk = SRWLStokes()
  stk.mesh.ne, stk.mesh.eStart, stk.mesh.eFin = ne, e0, e1
  stk.mesh.nx, stk.mesh.xStart, stk.mesh.xFin = nx, x0, x1 
  stk.mesh.ny, stk.mesh.yStart, stk.mesh.yFin = ny, y0, y1
  stk.arS = array('f', [0]*ne*nx*ny) 
  flnp = np.ndarray(buffer=stk.arS, shape=(len(stk.arS)), dtype=stk.arS.typecode)
  flnp[:] = np.loadtxt(fname)
  return stk

