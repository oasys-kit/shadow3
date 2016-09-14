#
# Python script to run shadow3 and test a system with crystal in both python and command mode
#
import Shadow
import numpy
import os

os.system("rm -f star.01 begin.dat start.01 start.00 end.01 end.00")

#
# preprocessor
#
Shadow.ShadowPreprocessorsXraylib.bragg( interactive=False, DESCRIPTOR="Si",H_MILLER_INDEX=1,K_MILLER_INDEX=1,L_MILLER_INDEX=1,TEMPERATURE_FACTOR=1.0,E_MIN=5000.0,E_MAX=15000.0,E_STEP=1.0,SHADOW_FILE="bragg.dat")


#
# initialize shadow3 source (oe0) and beam
#
beam = Shadow.Beam()
oe0 = Shadow.Source()
oe1 = Shadow.OE()

#
# Define variables. See meaning of variables in: 
#  https://raw.githubusercontent.com/srio/shadow3/master/docs/source.nml 
#  https://raw.githubusercontent.com/srio/shadow3/master/docs/oe.nml
#
oe0.FDISTR = 1
oe0.FSOUR = 0
oe0.F_COLOR = 3
oe0.F_PHOT = 0
oe0.PH1 = 9980.0
oe0.PH2 = 10020.0
oe0.VDIV1 = 5e-05
oe0.VDIV2 = 5e-05

oe1.DUMMY = 1.0
oe1.FILE_REFL = b'bragg.dat'
oe1.FWRITE = 0
oe1.F_CENTRAL = 1
oe1.F_CRYSTAL = 1
oe1.PHOT_CENT = 10000.0
oe1.R_LAMBDA = 5000.0
oe1.T_IMAGE = 200.0
oe1.T_INCIDENCE = 78.595
oe1.T_REFLECTION = 78.595
oe1.T_SOURCE = 100.0



#Run SHADOW to create the source

oe0.write("start.00")

beam.genSource(oe0)

oe0.write("end.00")


#
#run optical element 1
#
print("    Running optical element: %d"%(1))

oe1.write("start.01")

beam.traceOE(oe1,1)

oe1.write("end.01")

Shadow.ShadowTools.histo1(beam,11,nbins=101,nolost=1,ref=23)

#
# now run shadow3 in the traditional command mode
#
f = open("shadow3.inp",'w')
f.write("source\n")    
f.write("systemfile\n")    

f.write("trace\n")    
f.write("batch\n")    
f.write("0\n")    
f.write("start.01\n")    
f.write("0\n")    
f.write("EXIT\n")    
f.write("exit\n")    
f.close()

os.system("../shadow3 < shadow3.inp")

Shadow.ShadowTools.histo1("star.01",11,nbins=101,nolost=1,ref=23)
