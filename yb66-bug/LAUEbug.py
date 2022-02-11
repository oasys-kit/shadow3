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
oe0.IDO_VX = 0
oe0.IDO_VZ = 0
oe0.IDO_X_S = 0
oe0.IDO_Y_S = 0
oe0.IDO_Z_S = 0
oe0.PH1 = 9098.0
oe0.PH2 = 9102.0
oe0.SIGDIX = 1.70200001e-05
oe0.SIGDIZ = 1.10600004e-05
oe0.SIGMAX = 0.0407229997
oe0.SIGMAZ = 0.00902100001
oe0.VDIV1 = 0.0
oe0.VDIV2 = 0.0
oe0.NPOINT = 200

oe1.A_BRAGG = 55.0
oe1.DUMMY = 1.0
oe1.FHIT_C = 1
oe1.FILE_REFL = b'c5_15.111'
oe1.F_BRAGG_A = 1
oe1.F_CENTRAL = 1
oe1.F_CRYSTAL = 1
oe1.F_REFRAC = 1
oe1.PHOT_CENT = 9100.0
oe1.RLEN1 = 0.300000012
oe1.RLEN2 = 0.300000012
oe1.RWIDX1 = 0.300000012
oe1.RWIDX2 = 0.300000012
oe1.R_LAMBDA = 5000.0
oe1.THICKNESS = 0.01
oe1.T_IMAGE = 0.0
oe1.T_INCIDENCE = 15.682
oe1.T_REFLECTION = 125.682
oe1.T_SOURCE = 4000.0


#THIS IS BRAGG!!
# oe1.A_BRAGG = 55.0
# oe1.DUMMY = 1.0
# oe1.FHIT_C = 1
# oe1.FILE_REFL = b'c5_15.111'
# oe1.F_BRAGG_A = 1
# oe1.F_CENTRAL = 1
# oe1.F_CRYSTAL = 1
# oe1.PHOT_CENT = 9100.0
# oe1.RLEN1 = 0.300000012
# oe1.RLEN2 = 0.300000012
# oe1.RWIDX1 = 0.300000012
# oe1.RWIDX2 = 0.300000012
# oe1.R_LAMBDA = 5000.0
# oe1.THICKNESS = 0.01
# oe1.T_IMAGE = 0.0
# oe1.T_INCIDENCE = 15.6822173514
# oe1.T_REFLECTION = 125.6820926074
# oe1.T_SOURCE = 4000.0

oe1.FILE_SOURCE = b'begin.dat'



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


x = beam.getshcol(6)
col23 = beam.getshcol(23)
col24 = beam.getshcol(24)
col25 = beam.getshcol(25)

print(x.shape,col24.shape)


from srxraylib.plot.gol import plot
plot(x,col23,
    x,col24,
    x, col25,
    linestyle=['','',''],marker=['.','.','.'], legend=['total %g' % col23.sum(),'sigma %g' % col24.sum(),'pi %g' % col25.sum()],
     title="intensities")

# Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,nolost=1,title="Real space")
# Shadow.ShadowTools.plotxy(beam,1,4,nbins=101,nolost=1,title="Phase space X")
# Shadow.ShadowTools.plotxy(beam,3,6,nbins=101,nolost=1,title="Phase space Z")
    
