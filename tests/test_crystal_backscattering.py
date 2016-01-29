import Shadow

#
# runs an asymmetric crystal in backscattering
#
# See workspace on the Tutorials
#


def run_example_crystal_backscattering(user_units_to_cm=1.0,npoint=5000,iwrite=0):
    #
    # Python script to run shadow3. Created automatically with ShadowTools.make_python_script_from_list().
    #
    import Shadow
    import numpy

    # write (1) or not (0) SHADOW files start.xx end.xx star.xx
    iwrite = 0

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
    oe0.F_COLOR = 3
    oe0.F_PHOT = 0
    oe0.HDIV1 = 5e-09
    oe0.HDIV2 = 5e-09
    oe0.ISTAR1 = 0
    oe0.NPOINT = npoint
    oe0.PH1 = 9131.5
    oe0.PH2 = 9132.5
    oe0.VDIV1 = 5e-07
    oe0.VDIV2 = 5e-07





    oe1.A_BRAGG = -88.5
    oe1.FILE_REFL = b'si5_15.008'
    oe1.F_BRAGG_A = 1
    oe1.F_CRYSTAL = 1
    oe1.F_MOVE = 1
    oe1.T_INCIDENCE = 88.5
    oe1.T_REFLECTION = -88.5
    oe1.THICKNESS = 5.0 / user_units_to_cm
    oe1.T_IMAGE = 500.0 / user_units_to_cm
    oe1.T_SOURCE = 500.0 / user_units_to_cm
    oe1.DUMMY = user_units_to_cm


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


def test_crystal_backscattering():

    Shadow.ShadowPreprocessorsXraylib.bragg(interactive=0,DESCRIPTOR="Si",H_MILLER_INDEX=0,
                                            K_MILLER_INDEX=0,L_MILLER_INDEX=8,TEMPERATURE_FACTOR=0.764,
                                            E_MIN=5000.0,E_MAX=15000.0,E_STEP=100,SHADOW_FILE="si5_15.008")

    number_of_rays = 10000
    cm_or_mm = 0  # 0=using cm, 1=using mm




    if cm_or_mm == 0:
        beam = run_example_crystal_backscattering(user_units_to_cm=1.0,npoint=number_of_rays)
        title = "Units are cm"
    elif cm_or_mm == 1:
        beam = run_example_crystal_backscattering(user_units_to_cm=0.1,npoint=number_of_rays)
        title = "Units are mm"
    else:
        print("No way...")


    print("Intensity: %f"%(beam.intensity(nolost=1)))
    Shadow.ShadowTools.histo1(beam,11,nolost=1,nbins=501,ref="Yes")


    Shadow.ShadowTools.plotxy(beam,3,6,nbins=101,nolost=1,ref="Yes",title="Z,Z' divergence space "+title)
    #numpy.testing.assert_almost_equal(sh100,xrl100,2)


if __name__ == "__main__":
    test_crystal_backscattering()