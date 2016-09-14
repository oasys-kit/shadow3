#
# Python script to run shadow3. Created automatically with ShadowTools.make_python_script_from_list().
#
import Shadow
import numpy
import os

os.system("rm -f star.01 begin.dat start.01 start.00 end.01 end.00")

#
# preprocessor
#
Shadow.ShadowPreprocessorsXraylib.prerefl(interactive=False,SYMBOL="SiC",FILE="reflec.dat",DENSITY=3.217,E_MIN=100.0,E_MAX=20000.0,E_STEP=1.0)


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
oe0.F_PHOT = 0
oe0.PH1 = 1000.0

oe1.DUMMY = 1.0
oe1.FCYL = 1
oe1.FILE_REFL = b'reflec.dat'
oe1.FMIRR = 1
oe1.FWRITE = 0
oe1.F_REFLEC = 1



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


Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,nolost=1,title="Real space")

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
Shadow.ShadowTools.plotxy("star.01",1,3,nbins=101,nolost=1,title="Real space")

