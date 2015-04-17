#
# example of running shadow under python
#
# corresponds to the example in Chapter 2.4 of the SHADOW Primer:
#       http://ftp.esrf.eu/pub/scisoft/shadow3/Shadow3Primer.pdf
#
# Notes:
#
# -This example sets directly the parameters of the sorce and optical 
#  element for the corresponding case. This is the "direct" way, there are
#  some methods in Shadow.Source and Shadow.OE for helping to do that, but
#  they are under development, incomplete and subject to be modified
#
# -The flag "iwrite" permits to write SHADOW files (start.xx, end.xx begin.dat,
#  star.xx) directly from python. This is not necessary, because 
#  SHADOW-python can be run without writing files. Moreover, SHADOW may write 
#  itself this files of the OE (deactivated by setting oe.FWRITE=3)
#
# -More information in these files available in the source distribution:
#      README_PYTHON.txt General information on SHADOW-python
#      source.nml the description of the variables in Shadow.Source
#      oe.nml the description of the variables in Shadow.OE
#


import Shadow

# write (1) or not (0) SHADOW files start.xx end.xx star.xx
iwrite = 0


#
# initialize elements
#
source = Shadow.Source()
oe = Shadow.OE()
beam = Shadow.Beam()

#
# Source definition: point source with flat divergence
#
source.NPOINT = 30000 #number of rays
source.ISTAR1 = 10001 #seed
source.FSOUR  = 0     #point source
source.FDISTR = 1     #flat distribution in divergences, and values
source.HDIV1  = 0.05
source.HDIV2  = 0.05
source.VDIV1  = 0.01
source.VDIV2  = 0.01
#monochromatic: 0 eV
source.F_COLOR = 1
source.F_PHOT  = 0
source.PH1     = 0.0
source.POL_DEG = 1.0


if iwrite: 
    source.write("start_py.00")

#Run SHADOW to create the source
beam.genSource(source)
if iwrite:
    beam.write("begin.dat")


#
# Define the first (and only) optical element, a spherical mirror 
# in "autofocusing" mode
#angles
oe.T_INCIDENCE  = 20.0
oe.T_REFLECTION = 20.0
#distances
oe.T_SOURCE = 20.0
oe.T_IMAGE  = 40.0
#spherical mirror
oe.FMIRR  =  1
#autofocusing
oe.F_EXT     = 0
oe.F_DEFAULT = 1
# tell SHADOW not to write files
oe.FWRITE = 3

if iwrite:
    oe.write("start.01")

#Run Shadow/Trace for this first (and only) o.e.
beam.traceOE(oe,1)

if iwrite:
    oe.write("end.01")
    beam.write("star.01")

#
# display results (using ShadowTools, matplotlib needed)
#
Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,title="Real space")
Shadow.ShadowTools.plotxy(beam,1,4,nbins=101,title="Phase space X")
Shadow.ShadowTools.plotxy(beam,3,6,nbins=101,title="Phase space Z")

