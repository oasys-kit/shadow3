import Shadow
import numpy
import xraylib

#
# runs an absorber of 10 um thickness for a source at 10 keV
#
# The element symbol and number of rays can be set in test_attenuator()
#

def run_example_attenuator(user_units_to_cm=1.0,npoint=5000,iwrite=0):
    #
    # Python script to run shadow3. Created automatically with ShadowTools.make_python_script_from_list().
    #
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
    oe0.FSOURCE_DEPTH = 0
    oe0.F_PHOT = 0
    oe0.PH1 = 10000.0
    oe0.ISTAR1 = 1234567
    oe0.NPOINT = npoint

    oe1.F_REFRAC = 2
    oe1.DUMMY = user_units_to_cm
    oe1.T_SOURCE = 100.0 / user_units_to_cm
    oe1.T_IMAGE = 100.0 / user_units_to_cm

    oe1.T_INCIDENCE = 0.0
    oe1.T_REFLECTION = 180.0

    oe1.F_SCREEN = 1
    oe1.N_SCREEN = 1
    oe1.I_ABS = numpy.array([1, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    oe1.FILE_ABS = numpy.array([b'prerefl.dat', b'', b'', b'', b'', b'', b'', b'', b'', b''])
    oe1.THICK = numpy.array([10e-4/user_units_to_cm, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0])


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

    print(oe1.mirinfo())

    return beam


def test_attenuator():


    #
    # create preprocessor file
    #
    symbol = "Cu"
    number_of_rays = 10000
    cm_or_mm = 0  # 0=using cm, 1=using mm


    #
    # run prerefl
    #
    density = xraylib.ElementDensity(xraylib.SymbolToAtomicNumber(symbol))

    Shadow.ShadowPreprocessorsXraylib.prerefl(interactive=0,SYMBOL=symbol,DENSITY=density,FILE="prerefl.dat",E_MIN=5000.0,E_MAX=15000.0,E_STEP=100.0)

    #
    # run SHADOW for a sample of thickness 10 um
    #

    if cm_or_mm == 0:
        beam = run_example_attenuator(user_units_to_cm=1.0,npoint=number_of_rays)
    else:
        beam = run_example_attenuator(user_units_to_cm=0.1,npoint=number_of_rays)


    print("Intensity: %f"%(beam.intensity(nolost=1)))


    #
    # check SHADOW results against xraylib attenuation
    #
    cs1 = xraylib.CS_Total(xraylib.SymbolToAtomicNumber(symbol), 10.0)

    # print("xralib Fe cross section [cm^2/g]: %f"%cs1)
    # print("xralib mu  [cm^-1]: %f"%(cs1*density))

    sh100 = 100*beam.intensity(nolost=1)/number_of_rays
    xrl100 = 100*numpy.exp(-cs1*density*10e-4)
    print("xralib attenuation [per cent] for 10 um %s at 10 keV: %f"%(symbol,xrl100))
    print("Transmission [per cent]: %f"%(sh100))

    numpy.testing.assert_almost_equal(sh100,xrl100,2)


if __name__ == "__main__":
    test_attenuator()
