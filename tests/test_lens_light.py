import Shadow
import numpy

# using mac oasys, for plots
# from srxraylib.plot.gol import set_qt
# set_qt()

#
# runs an absorber of 10 um thickness for a source at 10 keV
#
#

def run_example_lens(user_units_to_cm=1.0,npoint=5000,use_prerefl=0):
    #
    # Python script to run shadow3. Created automatically with ShadowTools.make_python_script_from_list().
    #
    #
    # initialize shadow3 source (oe0) and beam
    #
    beam = Shadow.Beam()
    oe0 = Shadow.Source()
    oe1 = Shadow.OE()
    oe2 = Shadow.OE()

    #
    # Define variables. See meaning of variables in:
    #  https://raw.githubusercontent.com/srio/shadow3/master/docs/source.nml
    #  https://raw.githubusercontent.com/srio/shadow3/master/docs/oe.nml
    #

    oe0.FDISTR = 3
    oe0.FSOURCE_DEPTH = 0
    oe0.F_PHOT = 0
    oe0.HDIV1 = 1.0
    oe0.HDIV2 = 1.0
    oe0.ISTAR1 = 123456
    oe0.NPOINT = 500000
    oe0.PH1 = 8000.0
    oe0.SIGDIX = 2.49999994e-05
    oe0.SIGDIZ = 8.00000089e-06
    oe0.SIGMAX = 0.0122999996 / user_units_to_cm
    oe0.SIGMAZ = 0.000699999975 / user_units_to_cm
    oe0.VDIV1 = 1.0
    oe0.VDIV2 = 1.0



    oe1.CCC = numpy.array([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.0, 0.0])
    oe1.FCYL = 1
    if use_prerefl:
        oe1.F_R_IND = 2
        oe1.R_ATTENUATION_OBJ = 0.0
        oe1.R_IND_OBJ = 1.0
        oe1.FILE_R_IND_IMA = b'prerefl.dat'
    else:
        oe1.F_R_IND = 0
        oe1.R_IND_OBJ =          1.0
        oe1.R_IND_IMA =          0.9999923264754235
        oe1.R_ATTENUATION_OBJ =  0.0
        oe1.R_ATTENUATION_IMA =  150.727

    oe1.FMIRR = 10
    oe1.FWRITE = 3
    oe1.F_EXT = 1
    oe1.F_REFRAC = 1
    oe1.T_INCIDENCE = 0.0
    oe1.T_REFLECTION = 180.0
    oe1.T_SOURCE = 4700.9 / user_units_to_cm
    oe1.T_IMAGE = 0.01 / user_units_to_cm
    oe1.DUMMY = user_units_to_cm

    oe2.CCC = numpy.array([0.0, 292.67523*user_units_to_cm**2, 0.0045013279*user_units_to_cm**2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.13418387*user_units_to_cm, 0.0])
    oe2.FCYL = 1
    if use_prerefl:
        oe2.F_R_IND = 1
        oe2.FILE_R_IND_OBJ = b'prerefl.dat'
        oe2.R_ATTENUATION_IMA = 0.0
        oe2.R_IND_IMA = 1.0
    else:
        oe2.F_R_IND = 0
        oe2.R_IND_OBJ =          0.9999923264754235
        oe2.R_IND_IMA =          1.0
        oe2.R_ATTENUATION_OBJ =  150.727
        oe2.R_ATTENUATION_IMA =  0.0

    oe2.FMIRR = 10
    oe2.FWRITE = 3
    oe2.F_EXT = 1
    oe2.F_REFRAC = 1
    oe2.T_INCIDENCE = 0.0
    oe2.T_REFLECTION = 180.0
    oe2.T_SOURCE = 0.0 / user_units_to_cm
    oe2.T_IMAGE = 30.065 / user_units_to_cm
    oe2.DUMMY = user_units_to_cm



    beam.genSource(oe0)


    #
    #run optical element 1
    #
    print("    Running optical element: %d"%(1))

    beam.traceOE(oe1,1)


    #
    #run optical element 2
    #
    print("    Running optical element: %d"%(2))

    beam.traceOE(oe2,2)


    # print(oe0.sourcinfo())
    # print(oe1.mirinfo())
    # print(oe2.mirinfo())

    return beam


def test_lens():


    #
    # inputs
    #
    cm_or_mm = 1            # 0=using cm, 1=using mm
    use_prerefl = 0          # 0=No, 1=Yes


    if cm_or_mm == 0:
        user_units_to_cm = 1.0
        title = "Units are cm"
    elif cm_or_mm == 1:
        user_units_to_cm = 0.1
        title = "Units are mm"
    else:
        print("No way...")


    #
    # run prerefl
    #
    if use_prerefl:
        import xraylib
        symbol = "Si"
        density = xraylib.ElementDensity(xraylib.SymbolToAtomicNumber(symbol))
        Shadow.ShadowPreprocessorsXraylib.prerefl(interactive=0,SYMBOL=symbol,DENSITY=density,FILE="prerefl.dat",E_MIN=5000.0,E_MAX=15000.0,E_STEP=100.0)

    #
    # run SHADOW
    #
    beam = run_example_lens(user_units_to_cm=user_units_to_cm)


    # tkt = Shadow.ShadowTools.plotxy(beam,3,6,ref=0,nolost=1,nbins=301,title="Z,Z' "+title)
    # print("Intensity: %f "%tkt["intensity"])
    # print("Number of rays: %d, number of GOOD rays: %d "%(beam.nrays(nolost=0),beam.nrays(nolost=1)))

    intensity = beam.intensity(nolost=1)
    nrays = beam.nrays(nolost=1)

    print("intensity: %f, good rays=: %d" % (intensity, nrays))

    assert (intensity > 3600)
    assert (intensity < 3700)
    assert (nrays > 439000)
    assert (nrays < 440000)


    #numpy.testing.assert_almost_equal(sh100,xrl100,2)


if __name__ == "__main__":
    test_lens()
