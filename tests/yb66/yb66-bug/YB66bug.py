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

oe0.FDISTR = 1
oe0.FSOUR = 0
oe0.F_PHOT = 0
oe0.IDO_VX = 0
oe0.IDO_VZ = 0
oe0.IDO_X_S = 0
oe0.IDO_Y_S = 0
oe0.IDO_Z_S = 0
oe0.ISTAR1 = 5676561
oe0.NPOINT = 300
oe0.PH1 = 2600.0
oe0.POL_DEG = 0.5
oe0.VDIV1 = 0.0005
oe0.VDIV2 = 0.0005

oe1.DUMMY = 1.0
oe1.FILE_REFL = b'Si2_5.111'
# oe1.FILE_REFL = b'xcrystal.bra'
oe1.F_ANGLE = 1
oe1.F_CENTRAL = 1
oe1.F_CRYSTAL = 1
oe1.PHOT_CENT = 2600.0
oe1.R_LAMBDA = 5000.0
oe1.T_IMAGE = 5.0
oe1.T_INCIDENCE = 40.4884250916
oe1.T_REFLECTION = 40.4884250916
oe1.T_SOURCE = 1500.0
oe1.FILE_SOURCE = b'begin.dat'

# this is laue
# oe1.A_BRAGG = 90.001
# oe1.F_REFRAC = 1
# oe1.THICKNESS = 0.0001


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
    
