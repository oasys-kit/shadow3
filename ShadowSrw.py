import ShadowLib as sdl
import numpy as np
import sys
import h5py
import copy
import cPickle as pickle

try:
  import srwlib
except ImportError:
  import srwlib_fake as srwlib

A2EV     =  50676.89919462

#=========================================================================#
#                             functions                                   #
#=========================================================================#

def LoadStokesFromSRW(fname, energy=None, distance=30.):
  if isinstance(fname,tuple):
    stk, eBeam = fname
  else:
    try:
      if open(fname+"_stk.dat").readline()[0]=='#':
        stk = srwlib.loadStokesFromASCII(fname+"_stk.dat")
        stk.mesh.zStart = distance
      else:
        stk = srwlib.loadStokes(fname)
    except AttributeError:
      stk = pickle.load(open(fname+"_stk.dat","rb"))
    try:
      eBeam = srwlib.loadPartBeam(fname)
    except AttributeError:
      eBeam = pickle.load(open(fname+"_ebeam.dat","rb"))
  StokesDataBuffer = np.ndarray(shape=(4,stk.mesh.ny,stk.mesh.nx,stk.mesh.ne),buffer=stk.arS,dtype=stk.arS.typecode)
  if StokesDataBuffer.ndim==4: StokesData = StokesDataBuffer[0]
  if StokesData.dtype != np.float64: StokesData = np.asarray(StokesData,dtype=np.float64)
  StokesHeader = "imported over hdf5 file"
  try: order = list(f['order'].value)
  except: order = [0,1,2]
  StokesData = SetDataInOrder(StokesData,order)
  
  if energy!=None:
    if energy<stk.mesh.eStart or energy>stk.mesh.eFin: raise ValueError
    e = np.linspace(stk.mesh.eStart,stk.mesh.eFin,stk.mesh.ne)
    i = np.where( abs(e-energy)==abs(e-energy).min() )
    StokesData = StokesData[:,:,i[0][0]]
    stk.mesh.ne = 1; stk.mesh.eStart = energy; stk.mesh.eFin = energy

  return StokesData, stk.mesh, StokesHeader, eBeam

def SetDataInOrder(data,o):
  if o==[0,1,2]: tmp = data
  if o==[0,2,1]: tmp = np.swapaxes(data,1,2)
  if o==[1,0,2]: tmp = np.swapaxes(data,0,1)
  if o==[1,2,0]: tmp = np.swapaxes(data,0,1); tmp = np.swapaxes(tmp ,0,2)
  if o==[2,0,1]: tmp = np.swapaxes(data,0,1); tmp = np.swapaxes(tmp ,1,2)
  if o==[2,1,0]: tmp = np.swapaxes(data,0,2)
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
  il = np.int( np.floor(i_sel) )
  ir = il + 1
  if il*1.0==i_sel:
    image = data[:,:,il]
  else:
    wl = (i_sel-il)*(i_sel-il)
    wr = (ir-i_sel)*(ir-i_sel)
    image = (data[:,:,il]*wl + data[:,:,ir]*wr) / (wl+wr)
  return image


def SetCDF2arrays(data):
  if data.dtype==np.float32: data=np.asarray(data,dtype=np.float64)
  N0,N1 = data.shape
  tmp = data
  CDF1 = np.array( [ tmp[:,0:i].sum(axis=1) for i in range(N1) ] ).T
  CDF1 -= np.repeat( CDF1[:,0].reshape(N0,1),N1,axis=1 )
  CDF1 /= np.repeat( CDF1[:,-1].reshape(N0,1),N1,axis=1 )
  tmp = data.sum(axis=1)
  CDF0 = np.array( [ tmp[0:i].sum() for i in range(N0) ] )
  CDF0 -= CDF0[0]
  CDF0 /= CDF0[-1]
  CDF0 = CDF0.flatten()
  CDF1 = CDF1.flatten().reshape(N0,N1)
  return CDF0, CDF1


def SetCDF3arrays(data):
  if data.dtype==np.float32: data=np.asarray(data,dtype=np.float64)
  N0,N1,N2 = data.shape
  tmp = data
  tmp.shape = N0*N1,N2
  CDF2 = np.array( [ tmp[:,0:i+1].sum(axis=1) for i in range(N2) ] ).T
  tmp.shape = N0,N1,N2
  CDF2 -= np.repeat( CDF2[:,0].reshape(N0*N1,1),N2,axis=1)
  CDF2 /= np.repeat( CDF2[:,-1].reshape(N0*N1,1),N2,axis=1)
  CDF2.shape = N0,N1,N2
  tmp = data.sum(axis=2)
  CDF1 = np.array( [ tmp[:,0:i+1].sum(axis=1) for i in range(N1) ] ).T
  CDF1 -= np.repeat( CDF1[:,0].reshape(N0,1),N1,axis=1 )
  CDF1 /= np.repeat( CDF1[:,-1].reshape(N0,1),N1,axis=1 )
  tmp = data.sum(axis=2).sum(axis=1)
  CDF0 = np.array( [ tmp[0:i+1].sum() for i in range(N0) ] )
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
  rnd   = np.random.random((N,))
  zpph  = sdl.FastCDFfromZeroIndex(PHZP,rnd)
  rnd   = np.random.random((N,))
  xpph  = sdl.FastCDFfromOneIndex(PHXP,zpph,rnd)
  xpph  = np.sin( xpph*XpphD + Xpph0 )
  zpph  = np.sin( zpph*ZpphD + Zpph0 )
  ypph  = np.sqrt(1.0 - xpph*xpph + zpph*zpph)
  Eph   = np.ones(N,dtype=np.float32) * mesh.eStart
  return Eph, xpph, ypph, zpph

def GenRays3D(PHZP,PHXP,PHE,mesh,N):
  Eph0  = mesh.eStart
  EphD  = -(mesh.eStart-mesh.eFin)/(mesh.ne-1)
  Xpph0 = mesh.xStart/mesh.zStart
  XpphD = -(mesh.xStart-mesh.xFin)/(mesh.nx-1)/mesh.zStart
  Zpph0 = mesh.yStart/mesh.zStart
  ZpphD = -(mesh.yStart-mesh.yFin)/(mesh.ny-1)/mesh.zStart
  rnd   = np.random.random((N,))
  zpph  = sdl.FastCDFfromZeroIndex(PHZP,rnd)
  rnd   = np.random.random((N,))
  xpph  = sdl.FastCDFfromOneIndex(PHXP,zpph,rnd)
  rnd   = np.random.random((N,))
  Eph   = sdl.FastCDFfromTwoIndex(PHE,zpph,xpph,rnd)
  Eph   = Eph*EphD + Eph0
  xpph  = np.sin( xpph*XpphD + Xpph0 )
  zpph  = np.sin( zpph*ZpphD + Zpph0 )
  ypph  = np.sqrt(1.0 - xpph*xpph + zpph*zpph)
  return Eph, xpph, ypph, zpph



def GenMacroElectronSimple(eBeam,Nel):
  meanx = [eBeam.partStatMom1.x,eBeam.partStatMom1.xp]
  covx  = [[eBeam.arStatMom2[0],eBeam.arStatMom2[1]],[eBeam.arStatMom2[1],eBeam.arStatMom2[2]]]
  xe, xpe = np.random.multivariate_normal(mean=meanx,cov=covx,size=Nel).T

  meanz = [eBeam.partStatMom1.y,eBeam.partStatMom1.yp]
  covz  = [[eBeam.arStatMom2[3],eBeam.arStatMom2[4]],[eBeam.arStatMom2[4],eBeam.arStatMom2[5]]]
  ze, zpe = np.random.multivariate_normal(mean=meanz,cov=covz,size=Nel).T

  xe *= 1.0e2 #from m to cm
  ze *= 1.0e2 #from m to cm
  return xe, ze


def GenMacroElectron(eBeam,Nel,lim=None):
  meanx = [eBeam.partStatMom1.x,eBeam.partStatMom1.xp]
  covx  = [[eBeam.arStatMom2[0],eBeam.arStatMom2[1]],[eBeam.arStatMom2[1],eBeam.arStatMom2[2]]]
  xe, xpe = np.random.multivariate_normal(mean=meanx,cov=covx,size=Nel).T

  meanz = [eBeam.partStatMom1.y,eBeam.partStatMom1.yp]
  covz  = [[eBeam.arStatMom2[3],eBeam.arStatMom2[4]],[eBeam.arStatMom2[4],eBeam.arStatMom2[5]]]
  ze, zpe = np.random.multivariate_normal(mean=meanz,cov=covz,size=Nel).T

  NEWN = 0

  while True:
    if lim==None: break
    limx, dist = lim[0], lim[2]
    index = np.where(abs(xe+dist*xpe)>limx)
    if len(index[0])==0: break
    NEWN += len(index[0])
    xe[index], xpe[index] = np.random.multivariate_normal(mean=meanx,cov=covx,size=len(index[0])).T

  while True:
    if lim==None: break
    limz, dist = lim[1], lim[2]
    index = np.where(abs(ze+dist*zpe)>limz)
    if len(index[0])==0: break
    NEWN += len(index[0])
    ze[index], zpe[index] = np.random.multivariate_normal(mean=meanz,cov=covz,size=len(index[0])).T

  xpe = np.sin(xpe)
  zpe = np.sin(zpe)

  ye  = np.zeros(Nel,dtype=np.float64)
  ype = np.sqrt(1.0 - xpe*xpe+zpe*zpe)
  Ee  = np.random.normal(loc=0.0,scale=np.sqrt(eBeam.arStatMom2[10]),size=Nel)#GeV to eV

  xe *= 1.0e2 #from m to cm
  ye *= 1.0e2 #from m to cm
  ze *= 1.0e2 #from m to cm

  return Ee, xe, ye, ze, xpe, ype, zpe, NEWN

def SetBeam(xe,ze,xpph,ypph,zpph,Eph,N,ye=None):
  beam = sdl.Beam()
  beam.SetRayZeros(N)

  beam.rays[:,0] = xe
  if ye!=None: beam.rays[:,1] = ye

  beam.rays[:,2] = ze
  beam.rays[:,3] = xpph
  beam.rays[:,4] = ypph
  beam.rays[:,5] = zpph
  ratIO = zpph / xpph
  beam.rays[:,8] = 1.0 / np.sqrt( 1.0 + ratIO*ratIO )
  beam.rays[:,6] = -ratIO * beam.rays[:,8]
  beam.rays[:,9] += 1.0
  beam.rays[:,10] = Eph * A2EV
  beam.rays[:,11] = np.arange(N) + 1.0

  return beam

def WriteParameters(fname,d):
  f = open(fname,'w')
  paramName = {'N','ratio','A2EV',
               'Eph0','EphD','NEph',
               'Xp0','XpD','NXpph',
               'Zp0','ZpD','NZpph'}
  for p in paramName:
    print >> f, p, '=', d[p]

def genShadowBeam(fname,N=100000,method='ME',energy=None,lim=None,canted=None, distance=30.):
  if method=='ME':
    return genShadowBeamME(fname,N,energy, distance)
  elif method=='SE':
    return genShadowBeamSE(fname,N,energy,lim,canted,distance)
  else:
    raise AttributeError


def genShadowBeamME(fname,N=100000, energy=None, distance=30.):
  data, mesh, hlp, ebeam = LoadStokesFromSRW(fname, energy=energy, distance=distance)
  param = getParam(data,mesh,ebeam,N)

  if mesh.ne==1 and mesh.nx>1 and mesh.ny>1: #2 dim
    data.shape = (mesh.ny, mesh.nx)
    sys.stdout.write("setting up CDFs from angular distribution ")
    sys.stdout.flush()
    PHZP, PHXP = SetCDF2arrays(data)
    sys.stdout.write("done\n")
    sys.stdout.write("sampling CDFs for rays ")
    sys.stdout.flush()
    Eph, xpph, ypph, zpph = GenRays2D(PHZP,PHXP,mesh,N)
    sys.stdout.write("done\n")
    del data, PHZP, PHXP

  if mesh.ne>1 and mesh.nx>1 and mesh.ny>1:
    sys.stdout.write("setting up CDFs from spectral-angular distribution ")
    sys.stdout.flush()
    PHZP, PHXP, PHE = SetCDF3arrays(data)
    sys.stdout.write("done\n")
    sys.stdout.write("sampling CDFs for rays ")
    sys.stdout.flush()
    Eph, xpph, ypph, zpph = GenRays3D(PHZP,PHXP,PHE,mesh,N)
    sys.stdout.write("done\n")
    del data, PHZP, PHXP, PHE

  sys.stdout.write("generating source size ")
  sys.stdout.flush()
  xe, ze = GenMacroElectronSimple(ebeam,N)
  sys.stdout.write("done\n")

  sys.stdout.write("copying to Beam ")
  sys.stdout.flush()
  beam = SetBeam(xe,ze,xpph,ypph,zpph,Eph,N)
  sys.stdout.write("done\n")

  return beam, param

def genShadowBeamSE(fname,N=100000,energy=None,lim=None,canted=None,distance=30.):
  data, mesh, hlp, ebeam = LoadStokesFromSRW(fname, energy=energy, distance=distance)
  param = getParam(data,mesh,ebeam,N)

  if mesh.ne==1 or mesh.ny==1 or mesh.nx==1: raise ValueError

  sys.stdout.write("setting up CDFs from Single Electron spectral-angular distribution ")
  sys.stdout.flush()
  PHZP, PHXP, PHE = SetCDF3arrays(data)
  sys.stdout.write("done\n")

  sys.stdout.write("sampling CDFs for rays ")
  sys.stdout.flush()
  Eph, zpph, ypph, xpph = GenRays3D(PHZP,PHXP,PHE,mesh,N)
  sys.stdout.write("done\n")

  sys.stdout.write("generating electrons ")
  sys.stdout.flush()
  if lim==None:
    lim = [-2.0*mesh.xStart, -2.0*mesh.yStart, mesh.zStart]
  elif len(lim)==2:
    lim = [ lim[0]*2.0, lim[1]*2.0, mesh.zStart ] #elif -> exit from if/else, it does not enter to else.
  else:
    lim = [ lim[0]*2.0, lim[1]*2.0, lim[2] ]
  if canted!=None:
    d=canted
    m2xx, m2xxp, m2xpxp = ebeam.arStatMom2[0], ebeam.arStatMom2[1], ebeam.arStatMom2[2]
    m2yy, m2yyp, m2ypyp = ebeam.arStatMom2[3], ebeam.arStatMom2[4], ebeam.arStatMom2[5]
    ebeam.arStatMom2[0], ebeam.arStatMom2[1] = m2xx + 2.0*d*m2xxp + d**2*m2xpxp, m2xxp + d*m2xpxp
    ebeam.arStatMom2[3], ebeam.arStatMom2[4] = m2yy + 2.0*d*m2yyp + d**2*m2ypyp, m2yyp + d*m2ypyp
  
  Ee, xe, ye, ze, xpe, ype, zpe, NEWN = GenMacroElectron(ebeam,N,lim)
  sys.stdout.write("done\n")

  sys.stdout.write("stretching and rotating rays produce with Single Electron spectral-angular distribution to match the Multi Electron one ")
  sys.stdout.flush()
  Eph = Eph * (1.0 + 2.0 * Ee)
  #sdl.vecRotate(xpph,ypph,zpph,xpe,ype,zpe)
  xpph += xpe
  zpph += zpe
  ypph = np.sqrt(1.-xpph**2-zpph**2)
  sys.stdout.write("done\n")

  sys.stdout.write('')
  while True:
    if energy==None: break
    index = np.where( (Eph<energy[0])|(Eph>energy[1]) )
    if len(index[0])==0:
      sys.stdout.write("\rrepeating simulation to fit energy range: %d Pass the Check Over %d"%(N,N))
      break
    NEWNE = len(index[0])

    sys.stdout.write("\rrepeating simulation to fit energy range: %d Pass the Check Over %d"%(N-NEWNE,N))
    sys.stdout.flush()
    Eph[index], zpph[index], ypph[index], xpph[index] = GenRays3D(PHZP,PHXP,PHE,mesh,NEWNE)
    Ee[index], xe[index], ye[index], ze[index], xpe[index], ype[index], zpe[index], NEWNP = GenMacroElectron(ebeam,NEWNE,lim)
    NEWN += NEWNP+NEWNE
    Eph[index] = Eph[index] * (1.0 + 2.0 * Ee[index])
    xpph[index] += xpe[index]
    zpph[index] += zpe[index]
    ypph[index] = np.sqrt(1.-xpph[index]**2-zpph[index]**2)


  del data, PHZP, PHXP, PHE
  sys.stdout.write('\n')
  sys.stdout.write("remove rays falling outside the acceptance slit ")
  tof = mesh.zStart*1.e2/ypph
  xph = xe + np.multiply(tof,xpph)
  zph = ze + np.multiply(tof,zpph)
  index = np.where( (abs(xph)<lim[0]*0.5*1.e2) & (abs(zph)<lim[1]*0.5*1.e2) )
  param["ratio"] = param["ratio"] * N / (N+NEWN)
  sys.stdout.write("done\n")

  del xpe, ype, zpe, Ee, xph, zph

  xe = xe[index] * 1.0
  ze = ze[index] * 1.0
  xpph = xpph[index] * 1.0
  ypph = ypph[index] * 1.0
  zpph = zpph[index] * 1.0
  Eph = Eph[index] * 1.0

  sys.stdout.write("copying to Beam ")
  sys.stdout.flush()
  beam = SetBeam(xe,ze,xpph,ypph,zpph,Eph,len(index[0]))
  sys.stdout.write("done\n")

  return beam, param
