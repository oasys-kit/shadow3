#
# Python script to run shadow3. Created automatically with ShadowTools.make_python_script_from_list().
#
import Shadow
import numpy

# write (1) or not (0) SHADOW files start.xx end.xx star.xx
iwrite = 1

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

oe0.FDISTR = 3
oe0.F_COLOR = 3
oe0.F_PHOT = 0
oe0.HDIV1 = 0.0
oe0.HDIV2 = 0.0
oe0.NPOINT = 100
oe0.PH1 = 8799.999
oe0.PH2 = 8799.999
oe0.SIGDIX = 4.728541797631135e-06
oe0.SIGDIZ = 4.095010077148124e-06
oe0.SIGMAX = 0.0015810951361940363
oe0.SIGMAZ = 0.0006681031579752021
oe0.VDIV1 = 0.0
oe0.VDIV2 = 0.0

oe1.DUMMY = 1.0
oe1.FHIT_C = 1
oe1.FILE_SOURCE = b'begin.dat'
oe1.FILE_RIP = b'mirror.dat'
oe1.FMIRR = 3
oe1.FWRITE = 0
oe1.F_G_S = 2
oe1.F_RIPPLE = 1
oe1.RLEN1 = 6.075
oe1.RLEN2 = 6.075
oe1.RWIDX1 = 5.0
oe1.RWIDX2 = 5.0
oe1.T_IMAGE = 1000.0
oe1.T_INCIDENCE = 89.828
oe1.T_REFLECTION = 89.828
oe1.T_SOURCE = 4000.0



#Run SHADOW to create the source

if iwrite:
    oe0.write("start.00")

beam.genSource(oe0)

if iwrite:
    oe0.write("end.00")
    beam.write("begin.dat")


#
#run optical element 1
#
print("    Running optical element: %d"%(1))
if iwrite:
    oe1.write("start.01")

beam.traceOE(oe1,1)

if iwrite:
    oe1.write("end.01")
    beam.write("star.01")


Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,nolost=1,title="Real space")
# Shadow.ShadowTools.plotxy(beam,1,4,nbins=101,nolost=1,title="Phase space X")
# Shadow.ShadowTools.plotxy(beam,3,6,nbins=101,nolost=1,title="Phase space Z")
    
