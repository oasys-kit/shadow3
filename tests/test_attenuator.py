import Shadow



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

    oe1.DUMMY = 1.0
    oe1.F_REFRAC = 2
    oe1.DUMMY = user_units_to_cm
    oe1.T_SOURCE = 100.0 / user_units_to_cm
    oe1.T_IMAGE = 100.0 / user_units_to_cm

    oe1.T_INCIDENCE = 0.0
    oe1.T_REFLECTION = 180.0

    oe1.F_SCREEN = 1
    oe1.N_SCREEN = 1
    oe1.I_ABS = numpy.array([1, 0, 0, 0, 0, 0, 0, 0, 0, 0])
    oe1.FILE_ABS = numpy.array([b'Fe5_15.refl', b'', b'', b'', b'', b'', b'', b'', b'', b''])
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

if __name__ == "__main__":

    import numpy
    import xraylib


    #
    # create preprocessor file
    #
    symbol = "Fe"
    density = xraylib.ElementDensity(xraylib.SymbolToAtomicNumber(symbol))

    Shadow.ShadowPreprocessorsXraylib.prerefl(interactive=0,SYMBOL=symbol,DENSITY=density,FILE="Fe5_15.refl",E_MIN=5000.0,E_MAX=15000.0,E_STEP=100.0)

    #
    # run SHADOW for a sample of thickness 10 um

    npoint = 10000
    beam_cm = run_example_attenuator(user_units_to_cm=1.0,npoint=npoint)

    #
    # beam_mm = run_example_crystal_mosaic(user_units_to_cm=0.1)
    #
    # Shadow.ShadowTools.plotxy(beam_cm,1,3,nbins=101,nolost=1,title="Using units: cm")
    print("Intensity: %f"%(beam_cm.intensity(nolost=1)))


    #
    # check SHADOW results against xraylib attenuation
    #
    cs1 = xraylib.CS_Total(xraylib.SymbolToAtomicNumber("Fe"), 10.0)

    # print("xralib Fe cross section [cm^2/g]: %f"%cs1)
    # print("xralib mu  [cm^-1]: %f"%(cs1*density))

    sh100 = 100*beam_cm.intensity(nolost=1)/npoint
    xrl100 = 100*numpy.exp(-cs1*density*10e-4)
    print("xralib attenuation [per cent] for 10 um Fe at 10 keV: %f"%(xrl100))
    print("Transmission [per cent]: %f"%(sh100))

    numpy.testing.assert_almost_equal(sh100,xrl100,2)


