# -*- coding: utf-8 -*-
#############################################################################
# SRWLIB Example: Simulating experiments with CRL
# v 0.02
#############################################################################

from __future__ import print_function #Python 2.7 compatibility
import os
import sys
from copy import deepcopy

from srwlib import *
import Shadow as sd

import h5py as h5
import numpy as np
import pickle

print('SRWLIB Python Wavefront Calculation:')
print('Simulating FMX Beamline layout with CRL')

#Auxiliary function to write the Stokes Distributions data to HDF5 file:
def AuxSaveHDF5Data(stk, electrons, filePath):
  s ='#C-aligned Intensity (inner loop is vs photon energy, outer loop vs vertical position)\n'
  s+='#' + repr(stk.mesh.eStart) + ' #Initial Photon Energy [eV]\n'
  s+='#' + repr(stk.mesh.eFin) +   ' #Final Photon Energy [eV]\n'
  s+='#' + repr(stk.mesh.ne) +     ' #Number of points vs Photon Energy\n'
  s+='#' + repr(stk.mesh.xStart) + ' #Initial Horizontal Position [m]\n'
  s+='#' + repr(stk.mesh.xFin) +   ' #Final Horizontal Position [m]\n'
  s+='#' + repr(stk.mesh.nx) +     ' #Number of points vs Horizontal Position\n'
  s+='#' + repr(stk.mesh.yStart) + ' #Initial Vertical Position [m]\n'
  s+='#' + repr(stk.mesh.yFin) +   ' #Final Vertical Position [m]\n'
  s+='#' + repr(stk.mesh.ny) +     ' #Number of points vs Vertical Position\n'
  Astk = np.array(stk.arS).reshape(4,stk.mesh.ny,stk.mesh.nx,stk.mesh.ne)
  fi = h5.File(filePath, 'w')
  fi.create_dataset('StokesData',data=Astk)
  fi.create_dataset('StokesMesh', data=np.array( list( pickle.dumps(stk.mesh) ) ) )
  fi.create_dataset('StokesHeader', data=np.array( list(s) ) )
  fi.create_dataset('Electrons', data=np.array( list( pickle.dumps(electrons) ) ) )
  fi.close()

#**********************Input Parameters:
WorkingFolder = 'srw2shadow' #example data sub-folder name
IntensPartCoh = 'stk_part_coh.txt' #file name for output SR intensity data
IntensInitial = 'stk_initial.hdf5'

def setUndulator():
  #***********Undulator
  numPer = 71.5 #Number of ID Periods (without counting for terminations
  undPer = 0.021 #Period Length [m]
  By = 0.80371 #Peak Vertical field [T]
  phBy = 0 #Initial Phase of the Vertical field component
  sBy = -1 #Symmetry of the Vertical field component vs Longitudinal position
  return SRWLMagFldU([SRWLMagFldH(1, 'v', By, phBy, sBy, 1)], undPer, numPer) #Planar Undulator

def setMagFldC():
  xcID = 0 #Transverse Coordinates of Undulator Center [m]
  ycID = 0
  zcID = 1.25 #0 #Longitudinal Coordinate of Undulator Center [m]
  return SRWLMagFldC([setUndulator()], array('d', [xcID]), array('d', [ycID]), array('d', [zcID])) #Container of all Field Elements

def setElectronBeam():
  #***********Electron Beam
  elecBeam = SRWLPartBeam()
  elecBeam.Iavg = 0.5 #Average Current [A]
  elecBeam.partStatMom1.x = 0. #60.e-06 #Initial Transverse Coordinates (initial Longitudinal Coordinate will be defined later on) [m]
  elecBeam.partStatMom1.y = 0. #-0.00025
  elecBeam.partStatMom1.z = 0. #-0.5*undPer*(numPer + 4) #Initial Longitudinal Coordinate (set before the ID)
  elecBeam.partStatMom1.xp = 0. #Initial Relative Transverse Velocities
  elecBeam.partStatMom1.yp = 0.
  elecBeam.partStatMom1.gamma = 3./0.51099890221e-03 #Relative Energy
  #2nd order statistical moments
  elecBeam.arStatMom2[0] = (33.3317e-06)**2 #<(x-x0)^2>
  elecBeam.arStatMom2[1] = 0
  elecBeam.arStatMom2[2] = (16.5008e-06)**2 #<(x'-x'0)^2>
  elecBeam.arStatMom2[3] = (2.91204e-06)**2 #(15.4091e-06)**2 #<(y-y0)^2>
  elecBeam.arStatMom2[4] = 0
  elecBeam.arStatMom2[5] = (2.74721e-06)**2 #<(y'-y'0)^2>
  elecBeam.arStatMom2[10] = (0.89e-03)**2 #<(E-E0)^2>/E0^2
  return elecBeam

def setPrecisionParameter():
  #***********Precision Parameters for SR calculation
  meth = 1 #SR calculation method: 0- "manual", 1- "auto-undulator", 2- "auto-wiggler"
  relPrec = 0.01 #relative precision
  zStartInteg = 0 #longitudinal position to start integration (effective if < zEndInteg)
  zEndInteg = 0 #longitudinal position to finish integration (effective if > zStartInteg)
  npTraj = 20000
  sampFactNxNyForProp = 0.2 #0.1 #sampling factor for adjusting nx, ny (effective if > 0)
  return [meth, relPrec, zStartInteg, zEndInteg, npTraj, 0, 0], sampFactNxNyForProp

def setInitMesh(mesh):
  mesh.zStart = 20 #42 #+ 1.25 #36.25 #Longitudinal Position [m] from Center of Straight Section at which SR has to be calculated
  mesh.eStart = 12700. #Initial Photon Energy [eV]
  mesh.eFin = mesh.eStart #Final Photon Energy [eV]
  mesh.xStart = -0.0004 #-0.001375 #Initial Horizontal Position [m]
  mesh.xFin = 0.0004 #0.001375 #Final Horizontal Position [m]
  mesh.yStart = -0.00025 #-0.0008 #Initial Vertical Position [m]
  mesh.yFin = 0.00025 #0.0008 #Final Vertical Position [m]
  mesh.ne = 1
  mesh.ny = 81
  mesh.nx = 81
  return mesh
  
def setWfr():
  #****************** Initial Wavefront
  wfr = SRWLWfr() #For intensity distribution at fixed photon energy
  wfr.allocate(1, 81, 81) #Numbers of points vs Photon Energy, Horizontal and Vertical Positions
  wfr.mesh = setInitMesh(wfr.mesh)
  wfr.partBeam = elecBeam
  return wfr


#sys.exit(0)

def setSrwBeamline():
  #***************** Optical Elements and Propagation Parameters
  hfm_f = 9.92727 # [m] p=42.0 q=13.0 

  Drift_Slits_HFM = SRWLOptD(22.) #Drift from first Slits to Horizontally-Focusing Mirror (HFM)
  HFM = SRWLOptL(9.92727) #HFM as Thin Lens
  Drift_HFM_SSA = SRWLOptD(13.) #Drift from HFM to Secondary Source Aperture (SSA)
  SSA = SRWLOptA('r', 'a', 30e-06, 10e-03)
  Drift_SSA_VKB = SRWLOptD(11.) #Drift from SSA to Center of Vertically Focusing K-B Mirror (VKB)
  ApKB = SRWLOptA('r', 'a', 1.25e-03, 1.25e-03) #Aperture Before K-B
  angVKB = 2.5e-03 #grazing angle at VKB center [rad] 2.499e-03 ok!
  VKB = SRWLOptMirEl(_p=64.75, _q=1., _ang_graz=angVKB, _r_sag=1.e+40, _size_tang=0.5, _nvx=0, _nvy=cos(angVKB), _nvz=-sin(angVKB), _tvx=0, _tvy=-sin(angVKB), _x=0, _y=0, _treat_in_out=1) 
  Drift_VKB_HKB = SRWLOptD(0.5) #Distance between centers of Vertically and Horizontally focusing K-B mirrors 
  angHKB = 2.5e-03 #[rad]
  HKB = SRWLOptMirEl(_p=11.5, _q=0.5, _ang_graz=angHKB, _r_sag=1.e+40, _size_tang=0.5, _nvx=cos(angHKB), _nvy=0, _nvz=-sin(angHKB), _tvx=-sin(angHKB), _tvy=0, _x=0, _y=0, _treat_in_out=1) 
  Drift_HKB_Sample = SRWLOptD(0.5) #Drift from HKB Center to Sample
#Wavefront Propagation Parameters:
  #                    [ 0] [1] [2]  [3] [4] [5]  [6]  [7]  [8]  [9] [10] [11] 
  ppDrift_Slits_HFM =  [ 0,  0, 1.0,  1,  0, 4.0, 4.0, 3.0, 4.0,  0,  0,   0]
  ppHFM =              [ 0,  0, 1.0,  0,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppDrift_HFM_SSA =    [ 0,  0, 1.0,  1,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppSSA =              [ 0,  0, 1.0,  0,  0, 1.0, 3.0, 1.0, 4.0,  0,  0,   0]
  ppDrift_SSA_VKB =    [ 0,  0, 1.0,  1,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppApKB =             [ 0,  0, 1.0,  0,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppVKB =              [ 0,  0, 1.0,  1,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppDrift_VKB_HKB =    [ 0,  0, 1.0,  1,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppHKB =              [ 0,  0, 1.0,  1,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppDrift_HKB_Sample = [ 0,  0, 1.0,  1,  0, 1.0, 1.0, 1.0, 1.0,  0,  0,   0]
  ppFinal =            [ 0,  0, 1.0,  0,  1, 0.2, 2.0, 0.2, 4.0,  0,  0,   0]

#[ 0]: Auto-Resize (1) or not (0) Before propagation
#[ 1]: Auto-Resize (1) or not (0) After propagation
#[ 2]: Relative Precision for propagation with Auto-Resizing (1. is nominal)
#[ 3]: Allow (1) or not (0) for semi-analytical treatment of the quadratic (leading) phase terms at the propagation
#[ 4]: Do any Resizing on Fourier side, using FFT, (1) or not (0)
#[ 5]: Horizontal Range modification factor at Resizing (1. means no modification)
#[ 6]: Horizontal Resolution modification factor at Resizing
#[ 7]: Vertical Range modification factor at Resizing
#[ 8]: Vertical Resolution modification factor at Resizing
#[ 9]: Type of wavefront Shift before Resizing (not yet implemented)
#[10]: New Horizontal wavefront Center position after Shift (not yet implemented)
#[11]: New Vertical wavefront Center position after Shift (not yet implemented)

#"Beamline" - Container of Optical Elements (together with the corresponding wavefront propagation instructions)
  return SRWLOptC([Drift_Slits_HFM,   HFM,   Drift_HFM_SSA,   SSA,   Drift_SSA_VKB,   ApKB,   VKB,   Drift_VKB_HKB,   HKB,   Drift_HKB_Sample], 
                  [ppDrift_Slits_HFM, ppHFM, ppDrift_HFM_SSA, ppSSA, ppDrift_SSA_VKB, ppApKB, ppVKB, ppDrift_VKB_HKB, ppHKB, ppDrift_HKB_Sample, ppFinal]) 

#the first slit is already in beam (begin.dat)

#**************** Optical elements in shadow


#HFM at 42 for now we do not use mono

def setShadowBeamline():
  hfm = sd.OE().setFrameOfReference(4200.,1300.,89.8567963,89.8567963,90.)
  hfm = hfm.setEllipsoidAuto().setCylindric(0.)
  hfm = hfm.setReflector().setScreens()#.setCrystal(file_refl='Si111_thick')
  #.setAutoTuning(0,12663.6,5000.)
  #small deviation from default
  hfm.I_SLIT[0] = 1
  hfm.SL_DIS[0] = 1300.0
  hfm.RX_SLIT[0] = 1.0
  hfm.RZ_SLIT[0] = 3.0e-1
  #no ripple now  
  #hfm = hfm.setRipple(2,np.array([0.,0.,0.,3.8e-8,1.,0.]),"mistral_80cm_4mm-sample.dat")

  kbv = sd.OE().setFrameOfReference(1100.,25.,89.8567963,89.8567963,90.)
  kbv = kbv.setEllipsoidAuto([6475.,100.,89.8567963]).setCylindric(0.)
  #kbv = kbv.setEllipsoidAuto([6700.,100.,89.8567963]).setCylindric(0.)
  kbv = kbv.setReflector()#.setDimensions(params=25.0*np.ones(4))#.setCrystal(file_refl='Si111_thick')
  #.setAutoTuning(0,12663.6,5000.)
  #no ripple now
  #kbv = kbv.setRipple(2,np.array([0.,0.,0.,3.8e-8,1.,0.]),"mistral_80cm_4mm-sample.dat")
  
  kbh = sd.OE().setFrameOfReference(25.,50.,89.8567963,89.8567963,-90.)
  kbh = kbh.setEllipsoidAuto([1150.,50.,89.8567963]).setCylindric(0.)
  kbh = kbh.setReflector()#.setDimensions(params=25.0*np.ones(4))#.setCrystal(file_refl='Si111_thick')
  #.setAutoTuning(0,12663.6,5000.)
  #no ripple now
  #kbv = kbv.setRipple(2,np.array([0.,0.,0.,3.8e-8,1.,0.]),"mistral_80cm_4mm-sample.dat")

  return [hfm, kbv, kbh]

def srwInitial():
  #***********Electron Beam
  elecBeam = setElectronBeam()

  #***********Undulator
  und = setUndulator()

  #***********Precision Parameters
  arPrecF = [0]*5 #for spectral flux vs photon energy
  arPrecF[0] = 1 #initial UR harmonic to take into account
  arPrecF[1] = 9 #final UR harmonic to take into account
  arPrecF[2] = 1.2 #longitudinal integration precision parameter
  arPrecF[3] = 1.2 #azimuthal integration precision parameter
  arPrecF[4] = 2 #calculate flux (1) or flux per unit surface (2)

  #***********UR Stokes Parameters (mesh) for Spectral Flux
  print('   Setup Stokes mesh ... ', end='')
  stkF = SRWLStokes() #for spectral flux vs photon energy
  stkF.allocate(1, 81, 81) #numbers of points vs photon energy, horizontal and vertical positions
  stkF.mesh = setInitMesh(stkF.mesh)
  print('done')

  #**********************Calculation (SRWLIB function calls)
  print('   Performing Spectral Flux (Stokes parameters) calculation ... ', end='')
  srwl.CalcStokesUR(stkF, elecBeam, und, arPrecF)
  print('done')

  #print('   Saving intensity data to file ... ', end='')
  #AuxSaveS0Data(stkF, os.path.join(os.getcwd(), strExDataFolderName, strFluxOutFileName))
  #print('done')

  print('   Saving intensity data to file ... ', end='')
  AuxSaveHDF5Data(stkF, elecBeam, os.path.join(os.getcwd(), WorkingFolder, IntensInitial))
  print('done')



#**********************Calculation (SRWLIB function calls)

def SrwSimulation():
  elecBeam = setElectronBeam()
  magFldCnt = setMagFldC()
  meshInitPartCoh = setInitMesh(SRWLRadMesh())
  sampFactNxNyForProp = 0.2
  optBL = setSrwBeamline()
  print('   Simulating Partially-Coherent Wavefront Propagation ... ', end='')
  radStokesProp = srwl_wfr_emit_prop_multi_e(elecBeam, magFldCnt, meshInitPartCoh, 1, 0.01, 50000, 10, 20, 
                                             os.path.join(os.getcwd(), WorkingFolder, IntensPartCoh), sampFactNxNyForProp, optBL)
  print('done')

def ShadowSimulation():
  if not os.path.exists(os.path.join(os.getcwd(), WorkingFolder, IntensInitial)):
    srwInitial()
  beam, dic = sd.ShadowSrw.genShadowBeam(os.path.join(WorkingFolder, IntensInitial),N=200000)
  os.chdir(os.path.join(os.getcwd(),WorkingFolder))
  beam.write('begin.dat')
  beamline = setShadowBeamline()
  i=0
  for oe in beamline:
    i+=1
    oe.write("start.%02d"%i)
    beam.traceOE(oe,i)
    #beam.write("star.%02d"%i)
  os.chdir('..')


def main(argv):
  if argv=='shadow':
    ShadowSimulation()
  elif argv=='srw':
    SrwSimulation()
  elif argv=='help':
    print("python <script.py> [shadow,srw,help]")
    sys.exit()
  else:
    print("python <script.py> [shadow,srw,help]")
    sys.exit(-2)

if __name__=="__main__":
  main(sys.argv[1])
