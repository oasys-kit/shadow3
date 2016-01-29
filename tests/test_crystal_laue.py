import Shadow

#
# runs a Laue crystal. Taken from a workspace in the Tutorials
#
# See: http://dx.doi.org/10.1088/0031-8949/73/5/014
#


def run_example_crystal_laue(user_units_to_cm=1.0,npoint=5000,iwrite=0):
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


    oe0.FDISTR = 3
    oe0.FSOURCE_DEPTH = 0
    oe0.F_COLOR = 3
    oe0.F_PHOT = 0
    oe0.HDIV1 = 0.0
    oe0.HDIV2 = 0.0
    oe0.PH1 = 9098.0
    oe0.PH2 = 9102.0
    oe0.SIGDIX = 1.70200001e-05
    oe0.SIGDIZ = 1.10600004e-05
    oe0.SIGMAX = 0.0407229997  / user_units_to_cm
    oe0.SIGMAZ = 0.00902100001 / user_units_to_cm
    oe0.VDIV1 = 0.0
    oe0.VDIV2 = 0.0
    oe0.NPOINT = npoint

    oe1.A_BRAGG = 55.0
    oe1.FHIT_C = 1
    oe1.FILE_REFL = b'bragg.dat'
    oe1.F_BRAGG_A = 1
    oe1.F_CENTRAL = 1
    oe1.F_CRYSTAL = 1
    oe1.F_REFRAC = 1
    oe1.PHOT_CENT = 9100.0
    oe1.T_INCIDENCE = 15.682
    oe1.T_REFLECTION = 125.682
    oe1.RLEN1 = 0.300000012      / user_units_to_cm
    oe1.RLEN2 = 0.300000012      / user_units_to_cm
    oe1.RWIDX1 = 0.300000012     / user_units_to_cm
    oe1.RWIDX2 = 0.300000012     / user_units_to_cm
    oe1.THICKNESS = 0.0099999998 / user_units_to_cm
    oe1.T_IMAGE = 0.0            / user_units_to_cm
    oe1.T_SOURCE = 4000.0        / user_units_to_cm
    oe1.DUMMY =                    user_units_to_cm

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


def test_crystal_laue():

    Shadow.ShadowPreprocessorsXraylib.bragg(interactive=0,DESCRIPTOR="Diamond",H_MILLER_INDEX=1,
                                            K_MILLER_INDEX=1,L_MILLER_INDEX=1,TEMPERATURE_FACTOR=1.0,
                                            E_MIN=9000.0,E_MAX=11000.0,E_STEP=50,SHADOW_FILE="bragg.dat")

    number_of_rays = 10000
    cm_or_mm = 0  # 0=using cm, 1=using mm




    if cm_or_mm == 0:
        beam = run_example_crystal_laue(user_units_to_cm=1.0,npoint=number_of_rays)
        title = "Units are cm"
    elif cm_or_mm == 1:
        beam = run_example_crystal_laue(user_units_to_cm=0.1,npoint=number_of_rays)
        title = "Units are mm"
    else:
        print("No way...")


    print("Intensity: %f"%(beam.intensity(nolost=1)))
    Shadow.ShadowTools.histo1(beam,11,nolost=1,ref="Yes")


    Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,nolost=1,title="Real space "+title)
    #numpy.testing.assert_almost_equal(sh100,xrl100,2)


if __name__ == "__main__":
    test_crystal_laue()