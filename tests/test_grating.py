import Shadow
import numpy

#
# runs a plane grating
#
# See: http://dx.doi.org/10.1088/0031-8949/73/5/014
#

def run_example_grating(user_units_to_cm=1.0,npoint=5000,iwrite=0):
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
    oe3 = Shadow.OE()

    #
    # Define variables. See meaning of variables in:
    #  https://raw.githubusercontent.com/srio/shadow3/master/docs/source.nml
    #  https://raw.githubusercontent.com/srio/shadow3/master/docs/oe.nml
    #


    oe0.CONE_MAX = 0.0549999997
    oe0.FDISTR = 5
    oe0.FSOUR = 0
    oe0.FSOURCE_DEPTH = 0
    oe0.F_COLOR = 2
    oe0.F_POLAR = 0
    oe0.NPOINT = npoint
    oe0.PH1 = 5145.0
    oe0.PH2 = 5145.24023


    oe1.FMIRR = 1
    oe1.F_DEFAULT = 0
    oe1.THETA = 5.0
    oe1.T_INCIDENCE = 5.0
    oe1.T_REFLECTION = 5.0


    oe1.DUMMY = user_units_to_cm
    oe1.SIMAG = 1.00000003e+16 / user_units_to_cm
    oe1.SSOUR = 85.0 / user_units_to_cm
    oe1.T_SOURCE = 85.0 / user_units_to_cm
    oe1.T_IMAGE = 75.0 / user_units_to_cm



    oe2.ALPHA = 180.0
    oe2.F_CENTRAL = 1
    oe2.F_GRATING = 1
    oe2.F_PHOT_CENT = 1
    oe2.R_LAMBDA = 5145.0
    oe2.T_INCIDENCE = 5.0
    oe2.T_REFLECTION = 5.0

    oe2.F_RULING = 1  # works for 0, 1 and 4 (cte in XY, cte in grating, VLS with zero coeffs)
    oe2.DUMMY = user_units_to_cm
    oe2.RULING = 18000.0 * user_units_to_cm
    oe2.T_IMAGE = 0.0 / user_units_to_cm
    oe2.T_SOURCE = 0.0 / user_units_to_cm



    oe3.ALPHA = 180.0
    oe3.FMIRR = 1
    oe3.F_DEFAULT = 0
    oe3.THETA = 5.0
    oe3.T_INCIDENCE = 5.0
    oe3.T_REFLECTION = 5.0


    oe3.DUMMY = user_units_to_cm
    oe3.SIMAG = 85.0 / user_units_to_cm
    oe3.SSOUR = 100000000000000.0 / user_units_to_cm
    oe3.T_IMAGE = 85.0 / user_units_to_cm
    oe3.T_SOURCE = 105.0 / user_units_to_cm


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


    #
    #run optical element 2
    #
    print("    Running optical element: %d"%(2))
    if iwrite:
        oe2.write("start.02")
    beam.traceOE(oe2,2)
    if iwrite:
        oe2.write("end.02")
        beam.write("star.02")


    #
    #run optical element 3
    #
    print("    Running optical element: %d"%(3))
    if iwrite:
        oe3.write("start.03")
    beam.traceOE(oe3,3)
    if iwrite:
        oe3.write("end.03")
        beam.write("star.03")

    #print(oe1.mirinfo())
    print(oe2.mirinfo())
    #print(oe3.mirinfo())

    return beam


def test_grating():


    number_of_rays = 10000
    cm_or_mm = 1  # 0=using cm, 1=using mm


    if cm_or_mm == 0:
        beam = run_example_grating(user_units_to_cm=1.0,npoint=number_of_rays)
    elif cm_or_mm == 1:
        beam = run_example_grating(user_units_to_cm=0.1,npoint=number_of_rays)
    else:
        print("No way...")


    print("Intensity: %f"%(beam.intensity(nolost=1)))


    Shadow.ShadowTools.histo1(beam,3,nolost=1)

    #numpy.testing.assert_almost_equal(sh100,xrl100,2)


if __name__ == "__main__":
    test_grating()