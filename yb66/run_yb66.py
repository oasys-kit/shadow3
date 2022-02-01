import numpy
import os
from srxraylib.plot.gol import plot


from xoppylib.decorators.xraylib_decorated import XraylibDecorated
from xoppylib.decorators.dabax_decorated import DabaxDecorated

from xoppylib.crystals.tools import bragg_calc, bragg_calc2

from xoppylib.crystals.tools import run_diff_pat as run_diff_pat_new
from xoppylib.crystals.tools import bragg_calc2
import xraylib
from dabax.dabax_xraylib import DabaxXraylib



if __name__ == "__main__":

    xrl = XraylibDecorated()
    dx = DabaxDecorated()

    do_plot = 1



    #
    # script to make the calculations (created by XOPPY:crystal)
    #



    descriptor = 'YB66'
    SCANFROM = 0  # in microradiants
    SCANTO = 100  # in microradiants
    TEMPER = 1.0
    ENERGY = 8040.0
    SCANPOINTS = 200

    dx = DabaxXraylib()

    #
    # compute
    #

    os.system("rm xcrystal.bra")

    print("Using crystal descriptor: ", descriptor)

    version = 2.2    # 1=Si, v1;  2.1=Si v2; 2.2 = YB66 v2

    if version == 1:
        from xoppylib.crystals.create_bragg_preprocessor_file_v1 import create_bragg_preprocessor_file_v1

        create_bragg_preprocessor_file_v1(interactive=False,
            DESCRIPTOR="Si", H_MILLER_INDEX=1, K_MILLER_INDEX=1, L_MILLER_INDEX=1, TEMPERATURE_FACTOR=1.0,
            E_MIN=5000.0, E_MAX=15000.0, E_STEP=100.0,
            SHADOW_FILE="xcrystal.bra",
            material_constants_library=xraylib)

    elif version == 2.1:
        from xoppylib.crystals.create_bragg_preprocessor_file_v2 import create_bragg_preprocessor_file_v2

        create_bragg_preprocessor_file_v2(interactive=False,
            DESCRIPTOR="Si", H_MILLER_INDEX=1, K_MILLER_INDEX=1, L_MILLER_INDEX=1, TEMPERATURE_FACTOR=1.0,
            E_MIN=5000.0, E_MAX=15000.0, E_STEP=100.0,
            SHADOW_FILE="xcrystal.bra",
            material_constants_library=xraylib)

    elif version == 2.2:
        bragg_dictionary = bragg_calc2(descriptor=descriptor,
                                       hh=4, kk=0, ll=0,
                                       temper=1.0,
                                       emin=ENERGY - 100.0, emax=ENERGY + 100.0,
                                       estep=(SCANTO - SCANFROM) / SCANPOINTS, fileout="xcrystal.bra",
                                       material_constants_library=dx)


    if False:
        run_diff_pat_new(
            bragg_dictionary,
            preprocessor_file="xcrystal.bra",
            MOSAIC=0,
            GEOMETRY=0,
            SCAN=2,
            UNIT=1,
            SCANFROM=SCANFROM,
            SCANTO=SCANTO,
            SCANPOINTS=SCANPOINTS,
            ENERGY=ENERGY,
            ASYMMETRY_ANGLE=0.0,
            THICKNESS=0.7,
            MOSAIC_FWHM=0.1,
            RSAG=125.0,
            RMER=1290.0,
            ANISOTROPY=0,
            POISSON=0.22,
            CUT="2 -1 -1 ; 1 1 1 ; 0 0 0",
            FILECOMPLIANCE="mycompliance.dat",
        )

        # import os
        # command = "..\diff_pat.exe < xoppy.inp"
        # # print("Running command '%s' in directory: %s " % (command, locations.home_bin_run()))
        # print("\n--------------------------------------------------------\n")
        # os.system(command)
        # print("\n--------------------------------------------------------\n")


        #
        # example plot
        #
        from srxraylib.plot.gol import plot

        data = numpy.loadtxt("diff_pat.dat", skiprows=5)
        plot(data[:, 0], data[:, -1])


    #
    # shadow3
    #
    import Shadow
    from crystal_diff_profile_vs_angle import define_source, run_source, define_beamline, run_beamline

    oe0 = define_source()
    beam3_source = run_source(oe0)


    oe_list = define_beamline()
    oe_list[0].FWRITE = 1
    beam3 = run_beamline(beam3_source, oe_list)

    Shadow.ShadowTools.plotxy(beam3, 6, 23, nbins=201, nolost=1, title="shadow3 diff profile")

    os.system("rm star.01")
    os.system("../shadow3 < shadow3.inp")
    b = Shadow.Beam()
    b.load("star.01")
    Shadow.ShadowTools.plotxy(b, 6, 23, nbins=201, nolost=1, title="shadow3 diff profile FROM < shadow3.inp")

