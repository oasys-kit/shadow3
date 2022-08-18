#
# Python script to run shadow3. Created automatically with make_python_script_from_list().
#
import Shadow
import numpy


def define_source():
    #
    # initialize shadow3 source (oe0) and beam
    #
    oe0 = Shadow.Source()

    # Define variables. See https://raw.githubusercontent.com/oasys-kit/shadow3/master/docs/source.nml

    oe0.FSOUR = 0
    oe0.F_PHOT = 0
    oe0.HDIV1 = 0.0
    oe0.HDIV2 = 0.0
    oe0.IDO_VX = 0
    oe0.IDO_VZ = 0
    oe0.IDO_X_S = 0
    oe0.IDO_Y_S = 0
    oe0.IDO_Z_S = 0
    oe0.NPOINT = 15000
    oe0.PH1 = 8000.0
    oe0.VDIV1 = 7.5e-05
    oe0.VDIV2 = 7.5e-05

    return oe0
    
def run_source(oe0, iwrite=False):
    # iwrite (1) or not (0) SHADOW files start.xx end.xx star.xx

    #Run SHADOW to create the source

    if iwrite:
        oe0.write("start.00")

    beam = Shadow.Beam()
    beam.genSource(oe0)

    if iwrite:
        oe0.write("end.00")
        beam.write("begin.dat")

    return beam


def define_beamline():
    # initialize elements
    oe_list = []

    
    oe1 = Shadow.OE()
    oe_list.append(oe1)

    # Define variables. See https://raw.githubusercontent.com/oasys-kit/shadow3/master/docs/oe.nml

    oe1.DUMMY = 100.0
    oe1.FILE_REFL = b'xcrystal.bra'
    oe1.F_CENTRAL = 1
    oe1.F_CRYSTAL = 1
    oe1.PHOT_CENT = 8000.0
    oe1.R_LAMBDA = 5000.0
    oe1.T_IMAGE = 0.01
    oe1.T_INCIDENCE = 75.6898061088
    oe1.T_REFLECTION = 75.6898061088
    oe1.T_SOURCE = 3.0

    oe1.FILE_SOURCE = b'begin.dat'  #   very important, otherwise does not run shadow3-trace-systemfile


    return oe_list

    
def run_beamline(beam_in, oe_list, iwrite=0):
    beam = beam_in.duplicate()
        
    #
    #run optical element 1
    #
    print("    Running optical element: %d"%(1))
    oe1 = oe_list[1-1]
    if iwrite:
        oe1.write("start.01")
    
    beam.traceOE(oe1,1)
    oe1 = oe_list[1-1]
    if iwrite:
        oe1.write("end.01")
        beam.write("star.01")

    return beam

#
# main
#

oe0 = define_source()

beam = run_source(oe0, iwrite=1)
    
oe_list = define_beamline()

beam = run_beamline(beam, oe_list, iwrite=1)


from srxraylib.plot.gol import set_qt
set_qt()
Shadow.ShadowTools.plotxy(beam,6,23,nbins=101,nolost=1,title="Reflectivity")
# Shadow.ShadowTools.plotxy(beam,1,4,nbins=101,nolost=1,title="Phase space X")
# Shadow.ShadowTools.plotxy(beam,3,6,nbins=101,nolost=1,title="Phase space Z")
