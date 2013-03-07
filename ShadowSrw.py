import ShadowLib 
import numpy 
import sys
import h5py 
import pickle

try:
  import srwlib
except ImportError:
  import srwlib_fake as srwlib

A2EV     =  50676.89919462

#=========================================================================#
#                             functions                                   #
#=========================================================================#

def LoadStokesHDF5(fname):
  f = h5py.File(fname,'r')
  StokesData = f['StokesData'].value
  if StokesData.ndim==4: StokesData = StokesData[0]
  StokesMesh = pickle.loads( ''.join(f['StokesMesh'].value) )
  StokesHeader = ''.join(f['StokesHeader'].value )
  eBeam = pickle.loads( ''.join(f['Electrons'].value) )
  try:
    order = list(f['order'].value)
  except KeyError:
    order = [0,1,2]
  StokesData = SetDataInOrder(StokesData,order)
  f.close()
  return StokesData, StokesMesh, StokesHeader, eBeam

def SetDataInOrder(data,o):
  g = [0,1,2]
  s0 = (g[0],o[0])
  o[s0[0]], o[s0[1]] = o[s0[1]], o[s0[0]]
  s1 = (g[0],o[0])
  o[s1[0]], o[s1[1]] = o[s1[1]], o[s1[0]]
  s2 = (g[0],o[0])
  o[s2[0]], o[s2[1]] = o[s2[1]], o[s2[0]]
  tmp = data.swapaxes(s0[0],s0[1])
  tmp = data.swapaxes(s1[0],s1[1])
  tmp = data.swapaxes(s2[0],s2[1])
  return tmp.copy()


def getParam(data,mesh,eBeam,N):
  d = dict()
  d['N']      = N
  d['Eph0']   = mesh.eStart
  d['EphD']   = (mesh.eFin-mesh.eStart)/(mesh.ne-1) if mesh.ne>1 else None
  d['NEph']   = mesh.ne

  d['Xp0']    = mesh.xStart
  d['XpD']    = (mesh.xFin-mesh.xStart)/(mesh.nx-1) if mesh.nx>1 else None
  d['Xpph0']  = d['Xp0']/mesh.zStart
  d['XpphD']  = d['XpD']/mesh.zStart  
  d['NXpph']  = mesh.nx

  d['Zp0']    = mesh.yStart
  d['ZpD']    = (mesh.yFin-mesh.yStart)/(mesh.ny-1) if mesh.ny>1 else None
  d['Zpph0']  = d['Zp0']/mesh.zStart
  d['ZpphD']  = d['ZpD']/mesh.zStart
  d['NZpph']  = mesh.ny

  d['volume'] = d['XpD']*d['ZpD']*1.0e6 # m^2 to mm^2
  if mesh.ne>1 : d['volume'] *= d['EphD']
  d['ratio']  = data.sum()*d['volume']/N
  d['dist']   = mesh.zStart
  d['A2EV']   = A2EV
  return d
  
def GetImageFromEnergy(E_sel,mesh,data):
  if E_sel<mesh.eStart or E_sel>mesh.eFin :
    print "Energy selected out of bound!"
    raise ValueError
  i_sel = (E_sel*1.0-mesh.eStart)/(mesh.eFin-mesh.eStart)*(mesh.ne-1)
  il = numpy.int( numpy.floor(i_sel) )
  ir = il + 1
  if il*1.0==i_sel:
    image = data[:,:,il]
  else:
    wl = (i_sel-il)*(i_sel-il)
    wr = (ir-i_sel)*(ir-i_sel)
    image = (data[:,:,il]*wl + data[:,:,ir]*wr) / (wl+wr)
  return image


def SetCDF2arrays(data):
  if data.dtype==numpy.float32: data=numpy.asarray(data,dtype=numpy.float64)
  N0,N1 = data.shape
  tmp = data
  CDF1 = numpy.array( [ tmp[:,0:i].sum(axis=1) for i in range(N1) ] ).T
  CDF1 -= numpy.repeat( CDF1[:,0].reshape(N0,1),N1,axis=1 )
  CDF1 /= numpy.repeat( CDF1[:,-1].reshape(N0,1),N1,axis=1 )
  tmp = data.sum(axis=1)
  CDF0 = numpy.array( [ tmp[0:i].sum() for i in range(N0) ] )
  CDF0 -= CDF0[0]
  CDF0 /= CDF0[-1]
  CDF0 = CDF0.flatten()
  CDF1 = CDF1.flatten().reshape(N0,N1)  
  return CDF0, CDF1


def SetCDF3arrays(data):
  if data.dtype==numpy.float32: data=numpy.asarray(data,dtype=numpy.float64)
  N0,N1,N2 = data.shape
  tmp = data
  tmp.shape = N0*N1,N2
  CDF2 = numpy.array( [ tmp[:,0:i+1].sum(axis=1) for i in range(N2) ] ).T
  tmp.shape = N0,N1,N2
  CDF2 -= numpy.repeat( CDF2[:,0].reshape(N0*N1,1),N2,axis=1)
  CDF2 /= numpy.repeat( CDF2[:,-1].reshape(N0*N1,1),N2,axis=1)
  CDF2.shape = N0,N1,N2
  tmp = data.sum(axis=2)
  CDF1 = numpy.array( [ tmp[:,0:i+1].sum(axis=1) for i in range(N1) ] ).T
  CDF1 -= numpy.repeat( CDF1[:,0].reshape(N0,1),N1,axis=1 )
  CDF1 /= numpy.repeat( CDF1[:,-1].reshape(N0,1),N1,axis=1 )
  tmp = data.sum(axis=2).sum(axis=1)
  CDF0 = numpy.array( [ tmp[0:i+1].sum() for i in range(N0) ] )
  CDF0 -= CDF0[0]
  CDF0 /= CDF0[-1]
  CDF0 = CDF0.flatten()
  CDF1 = CDF1.flatten().reshape(N0,N1)
  CDF2 = CDF2.flatten().reshape(N0,N1,N2)
  return CDF0, CDF1, CDF2


def GenRays2D(PHZP,PHXP,mesh,N):
  Xpph0 = mesh.xStart/mesh.zStart
  XpphD = -(mesh.xStart-mesh.xFin)/(mesh.nx-1)/mesh.zStart
  Zpph0 = mesh.yStart/mesh.zStart
  ZpphD = -(mesh.yStart-mesh.yFin)/(mesh.ny-1)/mesh.zStart
  rnd   = numpy.random.random((N,))
  zpph  = ShadowLib.FastCDFfromZeroIndex(PHZP,rnd)
  rnd   = numpy.random.random((N,))
  xpph  = ShadowLib.FastCDFfromOneIndex(PHXP,zpph,rnd)
  xpph  = numpy.sin( xpph*XpphD + Xpph0 )
  zpph  = numpy.sin( zpph*ZpphD + Zpph0 )
  ypph  = numpy.sqrt(1.0 - xpph*xpph + zpph*zpph)
  Eph   = numpy.ones(N,dtype=numpy.float32) * mesh.eStart * A2EV
  return Eph, xpph, ypph, zpph

def GenRays3D(PHZP,PHXP,PHE,mesh,N):
  Eph0  = mesh.eStart
  EphD  = -(mesh.eStart-mesh.eFin)/(mesh.ne-1)
  Xpph0 = mesh.xStart/mesh.zStart
  XpphD = -(mesh.xStart-mesh.xFin)/(mesh.nx-1)/mesh.zStart
  Zpph0 = mesh.yStart/mesh.zStart
  ZpphD = -(mesh.yStart-mesh.yFin)/(mesh.ny-1)/mesh.zStart
  rnd   = numpy.random.random((N,))
  zpph  = ShadowLib.FastCDFfromZeroIndex(PHZP,rnd)
  rnd   = numpy.random.random((N,))
  xpph  = ShadowLib.FastCDFfromOneIndex(PHXP,zpph,rnd)
  rnd   = numpy.random.random((N,))
  Eph   = ShadowLib.FastCDFfromTwoIndex(PHE,zpph,xpph,rnd)
  Eph   = Eph*EphD + Eph0
  xpph  = numpy.sin( xpph*XpphD + Xpph0 )
  zpph  = numpy.sin( zpph*ZpphD + Zpph0 )
  ypph  = numpy.sqrt(1.0 - xpph*xpph + zpph*zpph)
  return Eph, xpph, ypph, zpph



def GenMacroElectronSimple(eBeam,Nel):
  multX = 0.5/(eBeam.arStatMom2[0]*eBeam.arStatMom2[0] - eBeam.arStatMom2[1]*eBeam.arStatMom2[1])
  BX = eBeam.arStatMom2[0]*multX
  GX = eBeam.arStatMom2[2]*multX
  AX = eBeam.arStatMom2[1]*multX

  multZ = 0.5/(eBeam.arStatMom2[3]*eBeam.arStatMom2[3] - eBeam.arStatMom2[4]*eBeam.arStatMom2[4])
  BZ = eBeam.arStatMom2[3]*multZ
  GZ = eBeam.arStatMom2[5]*multZ
  AZ = eBeam.arStatMom2[4]*multZ

  SigPX = 1.0/numpy.sqrt(2.0*GX)
  SigQX = numpy.sqrt(GX/(2.0*(BX*GX - AX*AX)))
  SigPZ = 1.0/numpy.sqrt(2.0*GZ)
  SigQZ = numpy.sqrt(GZ/(2.0*(BZ*GZ - AZ*AZ)))

  xpe = SigQX * numpy.random.normal(loc=0.0,scale=1.0,size=Nel)
  xe  = SigPX * numpy.random.normal(loc=0.0,scale=1.0,size=Nel) + AX*xpe/GX
  zpe = SigQZ * numpy.random.normal(loc=0.0,scale=1.0,size=Nel)
  ze  = SigPZ * numpy.random.normal(loc=0.0,scale=1.0,size=Nel) + AZ*zpe/GZ

  xe *= 1.0e2 #from m to cm
  ze *= 1.0e2 #from m to cm
  return xe, ze


def GenMacroElectron(eBeam,Nel,limx=None,limz=None):
  multX = 0.5/(eBeam.arStatMom2[0]*eBeam.arStatMom2[0] - eBeam.arStatMom2[1]*eBeam.arStatMom2[1])
  BX = eBeam.arStatMom2[0]*multX
  GX = eBeam.arStatMom2[2]*multX
  AX = eBeam.arStatMom2[1]*multX

  multZ = 0.5/(eBeam.arStatMom2[3]*eBeam.arStatMom2[3] - eBeam.arStatMom2[4]*eBeam.arStatMom2[4])
  BZ = eBeam.arStatMom2[3]*multZ
  GZ = eBeam.arStatMom2[5]*multZ
  AZ = eBeam.arStatMom2[4]*multZ

  SigPX = 1.0/numpy.sqrt(2.0*GX)
  SigQX = numpy.sqrt(GX/(2.0*(BX*GX - AX*AX)))
  SigPZ = 1.0/numpy.sqrt(2.0*GZ)
  SigQZ = numpy.sqrt(GZ/(2.0*(BZ*GZ - AZ*AZ)))

  NEWN = 0

  xpe = SigQX * numpy.random.normal(loc=0.0,scale=1.0,size=Nel)
  while True:
    if limx==None: break
    index = numpy.where(abs(xpe)>limx)
    if len(index[0])==0: break
    NEWN += len(index[0])
    xpe[index] = SigQX * numpy.random.normal(loc=0.0,scale=1.0,size=len(index[0]))
  xe  = SigPX * numpy.random.normal(loc=0.0,scale=1.0,size=Nel) + AX*xpe/GX

  zpe = SigQZ * numpy.random.normal(loc=0.0,scale=1.0,size=Nel)
  while True:
    if limz==None:
      break
    index = numpy.where(abs(zpe)>limz)
    if len(index[0])==0:
      break
    NEWN += len(index[0])
    zpe[index] = SigQZ * numpy.random.normal(loc=0.0,scale=1.0,size=len(index[0]))
  ze  = SigPZ * numpy.random.normal(loc=0.0,scale=1.0,size=Nel) + AZ*zpe/GZ

  xpe = numpy.sin(xpe)
  zpe = numpy.sin(zpe)

  ye  = numpy.zeros(NE,dtype=numpy.float64)
  ype = numpy.sqrt(1.0 - xpe*xpe+zpe*zpe)
  Ee  = sigmaERel * numpy.random.normal(loc=0.0,scale=1.0,size=Nel)
  
  xe *= 1.0e2 #from m to cm
  ye *= 1.0e2 #from m to cm
  ze *= 1.0e2 #from m to cm

  return Ee, xe, ye, ze, xpe, ype, zpe, NEWN

def SetBeam(xe,ze,xpph,ypph,zpph,Eph,N,ye=None):
  beam = ShadowLib.Beam()
  beam.SetRayZeros(N)

  beam.rays[:,0] = xe
  if ye!=None:
    beam.rays[:,1] = ye
    
  beam.rays[:,2] = ze
  beam.rays[:,3] = xpph
  beam.rays[:,4] = ypph
  beam.rays[:,5] = zpph
  ratIO = zpph / xpph
  beam.rays[:,8] = 1.0 / numpy.sqrt( 1.0 + ratIO*ratIO )
  beam.rays[:,6] = -ratIO * beam.rays[:,8] 
  beam.rays[:,9] += 1.0
  beam.rays[:,10] = Eph * A2EV
  beam.rays[:,11] = numpy.arange(N) + 1.0
  
  return beam

def WriteParameters(fname,d):
  f = open(fname,'w')
  paramName = {'N','ratio','A2EV',
               'Eph0','EphD','NEph',
               'Xp0','XpD','NXpph',
               'Zp0','ZpD','NZpph'}
  for p in paramName:
    print >> f, p, '=', d[p] 


def genShadowBeam(fnameHdf5,N=100000):
  
  data, mesh, hlp, ebeam = LoadStokesHDF5(fnameHdf5)
  param = getParam(data,mesh,ebeam,N)
    
  if mesh.ne==1 and mesh.nx>1 and mesh.ny>1: #2 dim 
    data.shape = (mesh.ny, mesh.nx)
    sys.stdout.write("setting up CDFs from spectral-angular distribution ")
    sys.stdout.flush()
    PHZP, PHXP = SetCDF2arrays(data)
    sys.stdout.write("done\n")
    sys.stdout.write("sampling CDFs for rays ")
    sys.stdout.flush()
    Eph, xpph, ypph, zpph = GenRays2D(PHZP,PHXP,mesh,N)
    sys.stdout.write("done\n")
    
  if mesh.ne>1 and mesh.nx>1 and mesh.ny>1:
    sys.stdout.write("setting up CDFs from spectral-angular distribution ")
    sys.stdout.flush()
    PHZP, PHXP, PHE = SetCDF3arrays(data)
    sys.stdout.write("done\n")
    sys.stdout.write("sampling CDFs for rays ")
    sys.stdout.flush()
    Eph, xpph, ypph, zpph = GenRays3D(PHZP,PHXP,PHE,mesh,N)
    sys.stdout.write("done\n")
  
  del data
                                                 
  sys.stdout.write("generating source size ")
  sys.stdout.flush()
  xe, ze = GenMacroElectronSimple(ebeam,N)
  sys.stdout.write("done\n")
                                                               
  sys.stdout.write("copying to Beam ")
  sys.stdout.flush()
  beam = SetBeam(xe,ze,xpph,ypph,zpph,Eph,N)
  sys.stdout.write("done\n")
                                                                                 
  return beam, param

