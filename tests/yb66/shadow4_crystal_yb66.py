
import numpy

from shadow4.sources.source_geometrical.source_geometrical import SourceGeometrical
from shadow4.beamline.optical_elements.crystals.s4_plane_crystal import S4PlaneCrystal, S4PlaneCrystalElement


from shadow4.tools.graphics import plotxy

from shadow4.syned.element_coordinates import ElementCoordinates

from syned.beamline.optical_elements.crystals.crystal import Crystal, DiffractionGeometry


def get_sigmas_radiation(photon_energy,undulator_length):
    import scipy.constants as codata
    lambdan = 1e-10 * codata.h*codata.c/codata.e*1e10 / photon_energy # in m
    print("wavelength in m",lambdan)
    return 1e6*2.740/4/numpy.pi*numpy.sqrt(lambdan*undulator_length),1e6*0.69*numpy.sqrt(lambdan/undulator_length)


def crystal_diffraction_with_collimated_beam(do_plot=True):

    #
    # collimated source
    #

    if True:
        src = SourceGeometrical()
        src.set_energy_distribution_singleline(value=8000, unit='eV')
        src.set_spatial_type_rectangle(width=1e-3, height=1e-3)
        src.set_angular_distribution_uniform(0,0,-100e-6,100e-6)

        beam = src.get_beam(POL_DEG=0.5, POL_ANGLE=numpy.pi/4, F_COHER=False)

        print(beam.info())
        SX, SZ = (1e6*beam.get_standard_deviation(1),1e6*beam.get_standard_deviation(3))


    #
    # crystal definition
    #

    if True: # create preprocessor file
        descriptor = 'YB66'
        SCANFROM = 0  # in microradiants
        SCANTO = 100  # in microradiants
        TEMPER = 1.0
        ENERGY = 8040.0
        SCANPOINTS = 200

        print("Using crystal descriptor: ", descriptor)
        from xoppylib.crystals.tools import bragg_calc2
        from dabax.dabax_xraylib import DabaxXraylib
        bragg_dictionary = bragg_calc2(descriptor=descriptor,
                                       hh=4, kk=0, ll=0,
                                       temper=1.0,
                                       emin=ENERGY - 100.0, emax=ENERGY + 100.0,
                                       estep=(SCANTO - SCANFROM) / SCANPOINTS, fileout="xcrystal.bra",
                                       material_constants_library=DabaxXraylib())

    crystal1 = S4PlaneCrystalElement(
                                optical_element=S4PlaneCrystal(
                                    name="Plane crystal",
                                    boundary_shape=None,
                                    material="YB66",
                                    diffraction_geometry=DiffractionGeometry.BRAGG,  # ?? not supposed to be in syned...
                                    miller_index_h=1,
                                    miller_index_k=1,
                                    miller_index_l=1,
                                    asymmetry_angle=0.0,
                                    thickness=0.010,  ###########################
                                    f_central=True,
                                    f_phot_cent=0,
                                    phot_cent=8000.0,
                                    file_refl="xcrystal.bra",
                                    f_bragg_a=False,
                                    # a_bragg=0.0,
                                    f_johansson=False,
                                    r_johansson=1.0,
                                    f_mosaic=False,
                                    spread_mos=0.4 * numpy.pi / 180,
                                    f_ext=0,
                                    material_constants_library_flag=3,
                                ),
                                coordinates=ElementCoordinates(p=0.0, q=5000.0e-3,
                                            angle_radial=0.0, angle_azimuthal=0.0, angle_radial_out=0.0))

    # print(crystal1.info())
    # print(crystal1.get_optical_element().get_surface_shape().get_conic_coefficients())
    #
    # trace
    #

    beam2, mirr2 = crystal1.trace_beam(beam)

    if do_plot:
        plotxy(beam2, 6, 23, nbins=100, title="INTENSITY VS Z'")
    #
    # #
    # if do_plot:
    #     plotxy(beam2, 1, 3, nbins=100, title="FOCAL PLANE")
    #     plotxy(mirr2, 1, 3, nbins=100, title="LENS HEIGHT")
    #     # plotxy(mirr2, 4, 5, nbins=100, title="FOOT DIV")
    #
    # FX, FZ = (1e6*beam2.get_standard_deviation(1),1e6*beam2.get_standard_deviation(3))
    # print("Source dimensions: %f %f um"%(SX,SZ))
    # print("Focal dimensions: %f %f um"%(FX,FZ))
    # print("Demagnification: %g %g"%(SX/FX,SX/FZ))




if __name__ == "__main__":
    from srxraylib.plot.gol import set_qt
    set_qt()

    crystal_diffraction_with_collimated_beam(do_plot=True)


