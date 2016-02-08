import Shadow
from Shadow.ShadowPreprocessorsXraylib import prerefl, bragg
from srxraylib.metrology import dabam

def test_kb_with_external_errors():

    #
    # initialize shadow3 source (oe0) and beam
    #
    beam = Shadow.Beam()
    oe0  = Shadow.Source()

    #

    # preprocessors
    #



    prerefl(interactive=0,SYMBOL="Pt",DENSITY=21.45,FILE="Pt5_55.dat",E_MIN=5000.0,E_MAX=55000.0,E_STEP=100.0)

    bragg(interactive=0,DESCRIPTOR="Si",H_MILLER_INDEX=1,K_MILLER_INDEX=1,L_MILLER_INDEX=1,TEMPERATURE_FACTOR=1.0,
                                            E_MIN=5000.0,E_MAX=53000.0,E_STEP=50,SHADOW_FILE="si5_55.111")
    dm = dabam.dabam()
    dm.set_input_shadowCalc(1)
    dm.set_input_shadowNx(10)
    dm.set_input_shadowWidth(10.0)
    dm.load(21)


    oe0.BENER = 6.03999996
    oe0.EPSI_X = 5.5200001e-08
    oe0.EPSI_Z = 1.07999998e-09
    oe0.FDISTR = 4
    oe0.FSOURCE_DEPTH = 4
    oe0.F_COLOR = 3
    oe0.F_PHOT = 0
    oe0.HDIV1 = 0.00079999998
    oe0.HDIV2 = 0.00079999998
    oe0.ISTAR1 = 567656
    oe0.NCOL = 0
    oe0.NPOINT = 500000
    oe0.N_COLOR = 0
    oe0.PH1 = 10000.0
    oe0.PH2 = 10010.0
    oe0.POL_DEG = 0.0
    oe0.R_ALADDIN = 2685.56747
    oe0.R_MAGNET = 26.8556747
    oe0.SIGDIX = 0.0
    oe0.SIGDIZ = 0.0
    oe0.SIGMAX = 0.00230000005
    oe0.SIGMAY = 0.0
    oe0.SIGMAZ = 0.000360000005
    oe0.VDIV1 = 2.20000002e-05
    oe0.VDIV2 = 2.20000002e-05
    oe0.WXSOU = 0.0
    oe0.WYSOU = 0.0
    oe0.WZSOU = 0.0


    bl = Shadow.CompoundOE(name="BL")
    bl.append_monochromator_double_crystal(p0=3153,q0=503,photon_energy_ev=10005,separation=40.0,
                    reflectivity_file="si5_55.111")

    bl.append_kb(p0=498.0,q0=95.0,separation=30.0,focal_positions=[4154.0,95.0],
                 dimensions1=[6.0,30.0],dimensions2=[6.0,30.0],reflectivity_kind=[1,1],
                 reflectivity_files=["Pt5_55.dat",
                                     "Pt5_55.dat"],
                 surface_error_files=["Shadow.dat",
                                      "Shadow.dat"])
                 # surface_error_files=["/mnt/scisoft/users/srio/Working/rt/CRG-LISA/MICROFUOCO_v01/mirror.dat",
                 #                      "/mnt/scisoft/users/srio/Working/rt/CRG-LISA/MICROFUOCO_v01/mirror.dat"])


    beam.genSource(oe0)

    beam.traceCompoundOE(bl)

    return beam




if __name__ == "__main__":
    beam = test_kb_with_external_errors()
    Shadow.ShadowTools.plotxy(beam,1,3,nbins=101,nolost=1,title="Real space")