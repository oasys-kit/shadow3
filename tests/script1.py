#
# Python script to run shadow3. Created automatically with mk_script.py.
#
import Shadow

# write (1) or not (0) SHADOW files start.xx end.xx star.xx
iwrite = 0

#
# initialize shadow3 source (oe0) and beam
#
beam = Shadow.Beam()
oe0 = Shadow.Source()

#
#define variables (see source.nml and oe.nml for doc)
#

oe0.FDISTR = 3
oe0.FSOURCE_DEPTH = 0
oe0.F_PHOT = 0
oe0.HDIV1 = 0.0
oe0.HDIV2 = 0.0
oe0.NPOINT = 20000
oe0.NTOTALPOINT = 50000000
oe0.PH1 = 35700.0
oe0.SIGDIX = 0.0001
oe0.SIGDIZ = 4.3e-06
oe0.SIGMAX = 0.00482
oe0.SIGMAZ = 0.00095
oe0.VDIV1 = 0.0
oe0.VDIV2 = 0.0

oe0.F_BOUND_SOUR = 0 #2 
oe0.FILE_BOUND = b'myaperture.dat'


#Run SHADOW to create the source

if iwrite:
    oe0.write("start.00")


beam.genSource(oe0)

if iwrite:
    oe0.write("end.00")
    beam.write("begin.dat")


# Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,title="Real space")
# Shadow.ShadowTools.plotxy(beam,1,4,nbins=101,title="Phase space X")
# Shadow.ShadowTools.plotxy(beam,3,6,nbins=101,title="Phase space Z")

print(oe0.sourcinfo())

