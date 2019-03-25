












































!----
!---- MODULE:  shadow_variables
!----
!---- Main module for shadow
!---- Contains: 
!----       1)  the common variables to be passed trought routines
!----           (now called "variables pool")
!----       2) the routines that need to access these common variables: 
!----             rwname    
!----             ...
!----       3) the main routines for source and trace calculations:
!----             source1 (geometrical sources)
!----             ...
!----       4) internal (private)  routines needed by code in 2) and 3):
!----             put_variables, get_variables, source_bound, 
!----
!----
!----
!---- Example of usage: see test_shadow.f95 for some tests.
!----                   see gen_source (main program for creating a shadow source)
!----
!----



Module shadow_variables
    !---- Use Modules ----!
    use shadow_globaldefinitions
    use gfile
    use stringio, only: myConcat
    !---- Variables ----!
    implicit none

    !
    ! HERE THE DECLARATION OF THE VARIABLES POOL
    !
    ! WHAT YOU PUT HERE, IT IS SEEN BY ALL THE ROUTINES HERE AND 
    ! THOSE PROGRAMS STATING "use shadow"
    !
    ! THE OLD COMMON BLOCK IS WRITTEN HERE (COMMENTED) FOR MEMORANDUM

    ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    ! C
    ! C This is the COMMON blocks set which insure communication between
    ! C the program subroutines; it also contain initial parameters
    ! C
    ! C *********************************************************************
    
    !!--	REAL*8 PI, TWOPI, PIHALF, TODEG, TORAD, ORIGIN
    !!--	REAL*8 X_VRS, Y_VRS, Z_VRS
    !!--	REAL*8 TOCM, TOANGS
    !!--
    ! this is obselete now...
    ! real(kind=kind(1.0d0)), parameter ::  N_DIM = 25001
    !to shadow_globaldefinitions real(kind=skr), parameter :: pi     = 3.141592653589793238462643
    !to shadow_globaldefinitions real(kind=skr), parameter :: twopi  = 6.283185307179586467925287
    !to shadow_globaldefinitions real(kind=skr), parameter :: pihalf = 1.570796326794896619231322
    !to shadow_globaldefinitions real(kind=skr), parameter :: todeg  =57.295779513082320876798155
    !to shadow_globaldefinitions real(kind=skr), parameter :: torad  = 0.017453292519943295769237

    ! TODO: Change these values with new codata values (see NIST)
    !to shadow_globaldefinitions real(kind=skr), parameter :: tocm =   1.239852D-4
    !to shadow_globaldefinitions real(kind=skr), parameter :: toangs = 1.239852D+4
    integer(kind=ski), parameter  :: aDim=10

    ! again the same variables encapsulated in a structure 
    type, public, bind(C) :: poolSource






integer(kind=ski) :: FDISTR
integer(kind=ski) :: FGRID
integer(kind=ski) :: FSOUR
integer(kind=ski) :: FSOURCE_DEPTH
integer(kind=ski) :: F_COHER
integer(kind=ski) :: F_COLOR
integer(kind=ski) :: F_PHOT
integer(kind=ski) :: F_POL
integer(kind=ski) :: F_POLAR
integer(kind=ski) :: F_OPD
integer(kind=ski) :: F_WIGGLER
integer(kind=ski) :: F_BOUND_SOUR
integer(kind=ski) :: F_SR_TYPE
integer(kind=ski) :: ISTAR1
integer(kind=ski) :: NPOINT
integer(kind=ski) :: NCOL
integer(kind=ski) :: N_CIRCLE
integer(kind=ski) :: N_COLOR
integer(kind=ski) :: N_CONE
integer(kind=ski) :: IDO_VX
integer(kind=ski) :: IDO_VZ
integer(kind=ski) :: IDO_X_S
integer(kind=ski) :: IDO_Y_S
integer(kind=ski) :: IDO_Z_S
integer(kind=ski) :: IDO_XL
integer(kind=ski) :: IDO_XN
integer(kind=ski) :: IDO_ZL
integer(kind=ski) :: IDO_ZN
real(kind=skr) :: SIGXL1
real(kind=skr) :: SIGXL2
real(kind=skr) :: SIGXL3
real(kind=skr) :: SIGXL4
real(kind=skr) :: SIGXL5
real(kind=skr) :: SIGXL6
real(kind=skr) :: SIGXL7
real(kind=skr) :: SIGXL8
real(kind=skr) :: SIGXL9
real(kind=skr) :: SIGXL10
real(kind=skr) :: SIGZL1
real(kind=skr) :: SIGZL2
real(kind=skr) :: SIGZL3
real(kind=skr) :: SIGZL4
real(kind=skr) :: SIGZL5
real(kind=skr) :: SIGZL6
real(kind=skr) :: SIGZL7
real(kind=skr) :: SIGZL8
real(kind=skr) :: SIGZL9
real(kind=skr) :: SIGZL10
real(kind=skr) :: CONV_FACT
real(kind=skr) :: CONE_MAX
real(kind=skr) :: CONE_MIN
real(kind=skr) :: EPSI_DX
real(kind=skr) :: EPSI_DZ
real(kind=skr) :: EPSI_X
real(kind=skr) :: EPSI_Z
real(kind=skr) :: HDIV1
real(kind=skr) :: HDIV2
real(kind=skr) :: PH1
real(kind=skr) :: PH2
real(kind=skr) :: PH3
real(kind=skr) :: PH4
real(kind=skr) :: PH5
real(kind=skr) :: PH6
real(kind=skr) :: PH7
real(kind=skr) :: PH8
real(kind=skr) :: PH9
real(kind=skr) :: PH10
real(kind=skr) :: RL1
real(kind=skr) :: RL2
real(kind=skr) :: RL3
real(kind=skr) :: RL4
real(kind=skr) :: RL5
real(kind=skr) :: RL6
real(kind=skr) :: RL7
real(kind=skr) :: RL8
real(kind=skr) :: RL9
real(kind=skr) :: RL10
real(kind=skr) :: BENER
real(kind=skr) :: POL_ANGLE
real(kind=skr) :: POL_DEG
real(kind=skr) :: R_ALADDIN
real(kind=skr) :: R_MAGNET
real(kind=skr) :: SIGDIX
real(kind=skr) :: SIGDIZ
real(kind=skr) :: SIGMAX
real(kind=skr) :: SIGMAY
real(kind=skr) :: SIGMAZ
real(kind=skr) :: VDIV1
real(kind=skr) :: VDIV2
real(kind=skr) :: WXSOU
real(kind=skr) :: WYSOU
real(kind=skr) :: WZSOU
real(kind=skr) :: PLASMA_ANGLE
character(kind=skc,len=1024) :: FILE_TRAJ
character(kind=skc,len=1024) :: FILE_SOURCE
character(kind=skc,len=1024) :: FILE_BOUND
integer(kind=ski) :: OE_NUMBER
integer(kind=ski) :: NTOTALPOINT
integer(kind=ski) :: IDUMMY
real(kind=skr) :: DUMMY
integer(kind=ski) :: F_NEW






    end type poolSource
    
    
    type, public, bind(C) :: poolOE








integer(kind=ski) :: FMIRR
integer(kind=ski) :: F_TORUS
integer(kind=ski) :: FCYL
integer(kind=ski) :: F_EXT
integer(kind=ski) :: FSTAT
integer(kind=ski) :: F_SCREEN
integer(kind=ski) :: F_PLATE
integer(kind=ski) :: FSLIT
integer(kind=ski) :: FWRITE
integer(kind=ski) :: F_RIPPLE
integer(kind=ski) :: F_MOVE
integer(kind=ski) :: F_THICK
integer(kind=ski) :: F_BRAGG_A
integer(kind=ski) :: F_G_S
integer(kind=ski) :: F_R_RAN
integer(kind=ski) :: F_GRATING
integer(kind=ski) :: F_MOSAIC
integer(kind=ski) :: F_JOHANSSON
integer(kind=ski) :: F_SIDE
integer(kind=ski) :: F_CENTRAL
integer(kind=ski) :: F_CONVEX
integer(kind=ski) :: F_REFLEC
integer(kind=ski) :: F_RUL_ABS
integer(kind=ski) :: F_RULING
integer(kind=ski) :: F_PW
integer(kind=ski) :: F_PW_C
integer(kind=ski) :: F_VIRTUAL
integer(kind=ski) :: FSHAPE
integer(kind=ski) :: FHIT_C
integer(kind=ski) :: F_MONO
integer(kind=ski) :: F_REFRAC
integer(kind=ski) :: F_DEFAULT
integer(kind=ski) :: F_REFL
integer(kind=ski) :: F_HUNT
integer(kind=ski) :: F_CRYSTAL
integer(kind=ski) :: F_PHOT_CENT
integer(kind=ski) :: F_ROUGHNESS
integer(kind=ski) :: F_ANGLE
integer(kind=ski) :: NPOINT
integer(kind=ski) :: NCOL
integer(kind=ski) :: N_SCREEN
integer(kind=ski) :: ISTAR1
real(kind=skr) :: CIL_ANG
real(kind=skr) :: ELL_THE
integer(kind=ski) :: N_PLATES
integer(kind=ski) :: IG_SEED
integer(kind=ski) :: MOSAIC_SEED
real(kind=skr) :: ALPHA
real(kind=skr) :: SSOUR
real(kind=skr) :: THETA
real(kind=skr) :: SIMAG
real(kind=skr) :: RDSOUR
real(kind=skr) :: RTHETA
real(kind=skr) :: OFF_SOUX
real(kind=skr) :: OFF_SOUY
real(kind=skr) :: OFF_SOUZ
real(kind=skr) :: ALPHA_S
real(kind=skr) :: RLEN1
real(kind=skr) :: RLEN2
real(kind=skr) :: RMIRR
real(kind=skr) :: AXMAJ
real(kind=skr) :: AXMIN
real(kind=skr) :: CONE_A
real(kind=skr) :: R_MAJ
real(kind=skr) :: R_MIN
real(kind=skr) :: RWIDX1
real(kind=skr) :: RWIDX2
real(kind=skr) :: PARAM
real(kind=skr) :: HUNT_H
real(kind=skr) :: HUNT_L
real(kind=skr) :: BLAZE
real(kind=skr) :: RULING
real(kind=skr) :: ORDER
real(kind=skr) :: PHOT_CENT
real(kind=skr) :: X_ROT
real(kind=skr) :: D_SPACING
real(kind=skr) :: A_BRAGG
real(kind=skr) :: SPREAD_MOS
real(kind=skr) :: THICKNESS
real(kind=skr) :: R_JOHANSSON
real(kind=skr) :: Y_ROT
real(kind=skr) :: Z_ROT
real(kind=skr) :: OFFX
real(kind=skr) :: OFFY
real(kind=skr) :: OFFZ
real(kind=skr) :: SLLEN
real(kind=skr) :: SLWID
real(kind=skr) :: SLTILT
real(kind=skr) :: COD_LEN
real(kind=skr) :: COD_WID
real(kind=skr) :: X_SOUR
real(kind=skr) :: Y_SOUR
real(kind=skr) :: Z_SOUR
real(kind=skr) :: X_SOUR_ROT
real(kind=skr) :: Y_SOUR_ROT
real(kind=skr) :: Z_SOUR_ROT
real(kind=skr) :: R_LAMBDA
real(kind=skr) :: THETA_I
real(kind=skr) :: ALPHA_I
real(kind=skr) :: T_INCIDENCE
real(kind=skr) :: T_SOURCE
real(kind=skr) :: T_IMAGE
real(kind=skr) :: T_REFLECTION
character(kind=skc,len=1024) :: FILE_SOURCE
character(kind=skc,len=1024) :: FILE_RIP
character(kind=skc,len=1024) :: FILE_REFL
character(kind=skc,len=1024) :: FILE_MIR
character(kind=skc,len=1024) :: FILE_ROUGH
integer(kind=ski) :: FZP
real(kind=skr) :: HOLO_R1
real(kind=skr) :: HOLO_R2
real(kind=skr) :: HOLO_DEL
real(kind=skr) :: HOLO_GAM
real(kind=skr) :: HOLO_W
real(kind=skr) :: HOLO_RT1
real(kind=skr) :: HOLO_RT2
real(kind=skr) :: AZIM_FAN
real(kind=skr) :: DIST_FAN
real(kind=skr) :: COMA_FAC
real(kind=skr) :: ALFA
real(kind=skr) :: GAMMA
real(kind=skr) :: R_IND_OBJ
real(kind=skr) :: R_IND_IMA
real(kind=skr) :: R_ATTENUATION_OBJ
real(kind=skr) :: R_ATTENUATION_IMA
integer(kind=ski) :: F_R_IND
character(kind=skc,len=1024) :: FILE_R_IND_OBJ
character(kind=skc,len=1024) :: FILE_R_IND_IMA
real(kind=skr) :: RUL_A1
real(kind=skr) :: RUL_A2
real(kind=skr) :: RUL_A3
real(kind=skr) :: RUL_A4
integer(kind=ski) :: F_POLSEL
integer(kind=ski) :: F_FACET
integer(kind=ski) :: F_FAC_ORIENT
integer(kind=ski) :: F_FAC_LATT
real(kind=skr) :: RFAC_LENX
real(kind=skr) :: RFAC_LENY
real(kind=skr) :: RFAC_PHAX
real(kind=skr) :: RFAC_PHAY
real(kind=skr) :: RFAC_DELX1
real(kind=skr) :: RFAC_DELX2
real(kind=skr) :: RFAC_DELY1
real(kind=skr) :: RFAC_DELY2
character(kind=skc,len=1024) :: FILE_FAC
integer(kind=ski) :: F_SEGMENT
integer(kind=ski) :: ISEG_XNUM
integer(kind=ski) :: ISEG_YNUM
character(kind=skc,len=1024) :: FILE_SEGMENT
character(kind=skc,len=1024) :: FILE_SEGP
real(kind=skr) :: SEG_LENX
real(kind=skr) :: SEG_LENY
integer(kind=ski) :: F_KOMA
character(kind=skc,len=1024) :: FILE_KOMA
integer(kind=ski) :: F_EXIT_SHAPE
integer(kind=ski) :: F_INC_MNOR_ANG
real(kind=skr) :: ZKO_LENGTH
real(kind=skr) :: RKOMA_CX
real(kind=skr) :: RKOMA_CY
integer(kind=ski) :: F_KOMA_CA
character(kind=skc,len=1024) :: FILE_KOMA_CA
integer(kind=ski) :: F_KOMA_BOUNCE
real(kind=skr) :: X_RIP_AMP
real(kind=skr) :: X_RIP_WAV
real(kind=skr) :: X_PHASE
real(kind=skr) :: Y_RIP_AMP
real(kind=skr) :: Y_RIP_WAV
real(kind=skr) :: Y_PHASE
integer(kind=ski) :: N_RIP
real(kind=skr) :: ROUGH_X
real(kind=skr) :: ROUGH_Y
integer(kind=ski) :: OE_NUMBER
integer(kind=ski) :: IDUMMY
real(kind=skr) :: DUMMY
real(kind=skr), dimension(10) :: CX_SLIT
real(kind=skr), dimension(10) :: CZ_SLIT
real(kind=skr), dimension(10) :: D_PLATE
character(kind=skc, len=1024), dimension(10) :: FILE_ABS
character(kind=skc, len=1024), dimension(10) :: FILE_SCR_EXT
integer(kind=ski), dimension(10) :: I_ABS
integer(kind=ski), dimension(10) :: I_SCREEN
integer(kind=ski), dimension(10) :: I_SLIT
integer(kind=ski), dimension(10) :: I_STOP
integer(kind=ski), dimension(10) :: K_SLIT
real(kind=skr), dimension(10) :: RX_SLIT
real(kind=skr), dimension(10) :: RZ_SLIT
integer(kind=ski), dimension(10) :: SCR_NUMBER
real(kind=skr), dimension(10) :: SL_DIS
real(kind=skr), dimension(10) :: THICK
real(kind=skr), dimension(10) :: CCC

    end type poolOE



    public  :: PoolOELoad,PoolOEWrite,PoolSourceLoad,PoolSourceWrite
    public  :: PoolOEDefault, PoolSourceDefault
    private :: PoolSourceToGf,PoolOEToGf,GfToPoolSource,GfToPoolOE

Contains


  !
  !
  !
  
  Subroutine PoolSourceToGf(src,gf)
    
    type(gfType),intent(inout)  :: gf 
    type(poolSource),intent(in) :: src 
    logical                     :: iOut
    integer(kind=ski)           :: zero=0
    
    iOut = GfTypeAllocate(gf,zero,zero)
    !! START CODE CREATED AUTOMATICALLY (cpp)






iOut=GfForceSetValue(gf, "FDISTR", src%FDISTR) .and. iOut
iOut=GfForceSetValue(gf, "FGRID", src%FGRID) .and. iOut
iOut=GfForceSetValue(gf, "FSOUR", src%FSOUR) .and. iOut
iOut=GfForceSetValue(gf, "FSOURCE_DEPTH", src%FSOURCE_DEPTH) .and. iOut
iOut=GfForceSetValue(gf, "F_COHER", src%F_COHER) .and. iOut
iOut=GfForceSetValue(gf, "F_COLOR", src%F_COLOR) .and. iOut
iOut=GfForceSetValue(gf, "F_PHOT", src%F_PHOT) .and. iOut
iOut=GfForceSetValue(gf, "F_POL", src%F_POL) .and. iOut
iOut=GfForceSetValue(gf, "F_POLAR", src%F_POLAR) .and. iOut
iOut=GfForceSetValue(gf, "F_OPD", src%F_OPD) .and. iOut
iOut=GfForceSetValue(gf, "F_WIGGLER", src%F_WIGGLER) .and. iOut
iOut=GfForceSetValue(gf, "F_BOUND_SOUR", src%F_BOUND_SOUR) .and. iOut
iOut=GfForceSetValue(gf, "F_SR_TYPE", src%F_SR_TYPE) .and. iOut
iOut=GfForceSetValue(gf, "ISTAR1", src%ISTAR1) .and. iOut
iOut=GfForceSetValue(gf, "NPOINT", src%NPOINT) .and. iOut
iOut=GfForceSetValue(gf, "NCOL", src%NCOL) .and. iOut
iOut=GfForceSetValue(gf, "N_CIRCLE", src%N_CIRCLE) .and. iOut
iOut=GfForceSetValue(gf, "N_COLOR", src%N_COLOR) .and. iOut
iOut=GfForceSetValue(gf, "N_CONE", src%N_CONE) .and. iOut
iOut=GfForceSetValue(gf, "IDO_VX", src%IDO_VX) .and. iOut
iOut=GfForceSetValue(gf, "IDO_VZ", src%IDO_VZ) .and. iOut
iOut=GfForceSetValue(gf, "IDO_X_S", src%IDO_X_S) .and. iOut
iOut=GfForceSetValue(gf, "IDO_Y_S", src%IDO_Y_S) .and. iOut
iOut=GfForceSetValue(gf, "IDO_Z_S", src%IDO_Z_S) .and. iOut
iOut=GfForceSetValue(gf, "IDO_XL", src%IDO_XL) .and. iOut
iOut=GfForceSetValue(gf, "IDO_XN", src%IDO_XN) .and. iOut
iOut=GfForceSetValue(gf, "IDO_ZL", src%IDO_ZL) .and. iOut
iOut=GfForceSetValue(gf, "IDO_ZN", src%IDO_ZN) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL1", src%SIGXL1) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL2", src%SIGXL2) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL3", src%SIGXL3) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL4", src%SIGXL4) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL5", src%SIGXL5) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL6", src%SIGXL6) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL7", src%SIGXL7) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL8", src%SIGXL8) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL9", src%SIGXL9) .and. iOut
iOut=GfForceSetValue(gf, "SIGXL10", src%SIGXL10) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL1", src%SIGZL1) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL2", src%SIGZL2) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL3", src%SIGZL3) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL4", src%SIGZL4) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL5", src%SIGZL5) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL6", src%SIGZL6) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL7", src%SIGZL7) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL8", src%SIGZL8) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL9", src%SIGZL9) .and. iOut
iOut=GfForceSetValue(gf, "SIGZL10", src%SIGZL10) .and. iOut
iOut=GfForceSetValue(gf, "CONV_FACT", src%CONV_FACT) .and. iOut
iOut=GfForceSetValue(gf, "CONE_MAX", src%CONE_MAX) .and. iOut
iOut=GfForceSetValue(gf, "CONE_MIN", src%CONE_MIN) .and. iOut
iOut=GfForceSetValue(gf, "EPSI_DX", src%EPSI_DX) .and. iOut
iOut=GfForceSetValue(gf, "EPSI_DZ", src%EPSI_DZ) .and. iOut
iOut=GfForceSetValue(gf, "EPSI_X", src%EPSI_X) .and. iOut
iOut=GfForceSetValue(gf, "EPSI_Z", src%EPSI_Z) .and. iOut
iOut=GfForceSetValue(gf, "HDIV1", src%HDIV1) .and. iOut
iOut=GfForceSetValue(gf, "HDIV2", src%HDIV2) .and. iOut
iOut=GfForceSetValue(gf, "PH1", src%PH1) .and. iOut
iOut=GfForceSetValue(gf, "PH2", src%PH2) .and. iOut
iOut=GfForceSetValue(gf, "PH3", src%PH3) .and. iOut
iOut=GfForceSetValue(gf, "PH4", src%PH4) .and. iOut
iOut=GfForceSetValue(gf, "PH5", src%PH5) .and. iOut
iOut=GfForceSetValue(gf, "PH6", src%PH6) .and. iOut
iOut=GfForceSetValue(gf, "PH7", src%PH7) .and. iOut
iOut=GfForceSetValue(gf, "PH8", src%PH8) .and. iOut
iOut=GfForceSetValue(gf, "PH9", src%PH9) .and. iOut
iOut=GfForceSetValue(gf, "PH10", src%PH10) .and. iOut
iOut=GfForceSetValue(gf, "RL1", src%RL1) .and. iOut
iOut=GfForceSetValue(gf, "RL2", src%RL2) .and. iOut
iOut=GfForceSetValue(gf, "RL3", src%RL3) .and. iOut
iOut=GfForceSetValue(gf, "RL4", src%RL4) .and. iOut
iOut=GfForceSetValue(gf, "RL5", src%RL5) .and. iOut
iOut=GfForceSetValue(gf, "RL6", src%RL6) .and. iOut
iOut=GfForceSetValue(gf, "RL7", src%RL7) .and. iOut
iOut=GfForceSetValue(gf, "RL8", src%RL8) .and. iOut
iOut=GfForceSetValue(gf, "RL9", src%RL9) .and. iOut
iOut=GfForceSetValue(gf, "RL10", src%RL10) .and. iOut
iOut=GfForceSetValue(gf, "BENER", src%BENER) .and. iOut
iOut=GfForceSetValue(gf, "POL_ANGLE", src%POL_ANGLE) .and. iOut
iOut=GfForceSetValue(gf, "POL_DEG", src%POL_DEG) .and. iOut
iOut=GfForceSetValue(gf, "R_ALADDIN", src%R_ALADDIN) .and. iOut
iOut=GfForceSetValue(gf, "R_MAGNET", src%R_MAGNET) .and. iOut
iOut=GfForceSetValue(gf, "SIGDIX", src%SIGDIX) .and. iOut
iOut=GfForceSetValue(gf, "SIGDIZ", src%SIGDIZ) .and. iOut
iOut=GfForceSetValue(gf, "SIGMAX", src%SIGMAX) .and. iOut
iOut=GfForceSetValue(gf, "SIGMAY", src%SIGMAY) .and. iOut
iOut=GfForceSetValue(gf, "SIGMAZ", src%SIGMAZ) .and. iOut
iOut=GfForceSetValue(gf, "VDIV1", src%VDIV1) .and. iOut
iOut=GfForceSetValue(gf, "VDIV2", src%VDIV2) .and. iOut
iOut=GfForceSetValue(gf, "WXSOU", src%WXSOU) .and. iOut
iOut=GfForceSetValue(gf, "WYSOU", src%WYSOU) .and. iOut
iOut=GfForceSetValue(gf, "WZSOU", src%WZSOU) .and. iOut
iOut=GfForceSetValue(gf, "PLASMA_ANGLE", src%PLASMA_ANGLE) .and. iOut
iOut=GfForceSetValue(gf, "FILE_TRAJ", src%FILE_TRAJ) .and. iOut
iOut=GfForceSetValue(gf, "FILE_SOURCE", src%FILE_SOURCE) .and. iOut
iOut=GfForceSetValue(gf, "FILE_BOUND", src%FILE_BOUND) .and. iOut
iOut=GfForceSetValue(gf, "OE_NUMBER", src%OE_NUMBER) .and. iOut
iOut=GfForceSetValue(gf, "NTOTALPOINT", src%NTOTALPOINT) .and. iOut
iOut=GfForceSetValue(gf, "IDUMMY", src%IDUMMY) .and. iOut
iOut=GfForceSetValue(gf, "DUMMY", src%DUMMY) .and. iOut
iOut=GfForceSetValue(gf, "F_NEW", src%F_NEW) .and. iOut






    !! END CODE CREATED AUTOMATICALLY (cpp)
  End Subroutine PoolSourceToGf
  
  !
  !
  !
  
  
  Subroutine PoolOEToGf(oe,gf)
    
    type(gfType),intent(inout)  :: gf 
    type(poolOE),intent(in)     :: oe 
    logical                     :: iOut
    integer(kind=ski)           :: zero=0,i
    !
    
    iOut = GfTypeAllocate(gf,zero,zero)

    !! START CODE CREATED AUTOMATICALLY (cpp)
    
    !srio danger
    !iOut= iOut .and. GfForceSetValue(gf,"NPOINT",pool01%NPOINTOE)




!    do i=1, arrdim !       iOut=GfForceSetValue(gf, "name"//trim(str(i)), oe%name(i)) .and. iOut !    end do



!    do i=1, arrdim !       iOut=GfForceSetValue(gf, "name"//trim(str(i)), oe%name(i)) .and. iOut !    end do






iOut=GfForceSetValue(gf, "FMIRR", oe%FMIRR) .and. iOut
iOut=GfForceSetValue(gf, "F_TORUS", oe%F_TORUS) .and. iOut
iOut=GfForceSetValue(gf, "FCYL", oe%FCYL) .and. iOut
iOut=GfForceSetValue(gf, "F_EXT", oe%F_EXT) .and. iOut
iOut=GfForceSetValue(gf, "FSTAT", oe%FSTAT) .and. iOut
iOut=GfForceSetValue(gf, "F_SCREEN", oe%F_SCREEN) .and. iOut
iOut=GfForceSetValue(gf, "F_PLATE", oe%F_PLATE) .and. iOut
iOut=GfForceSetValue(gf, "FSLIT", oe%FSLIT) .and. iOut
iOut=GfForceSetValue(gf, "FWRITE", oe%FWRITE) .and. iOut
iOut=GfForceSetValue(gf, "F_RIPPLE", oe%F_RIPPLE) .and. iOut
iOut=GfForceSetValue(gf, "F_MOVE", oe%F_MOVE) .and. iOut
iOut=GfForceSetValue(gf, "F_THICK", oe%F_THICK) .and. iOut
iOut=GfForceSetValue(gf, "F_BRAGG_A", oe%F_BRAGG_A) .and. iOut
iOut=GfForceSetValue(gf, "F_G_S", oe%F_G_S) .and. iOut
iOut=GfForceSetValue(gf, "F_R_RAN", oe%F_R_RAN) .and. iOut
iOut=GfForceSetValue(gf, "F_GRATING", oe%F_GRATING) .and. iOut
iOut=GfForceSetValue(gf, "F_MOSAIC", oe%F_MOSAIC) .and. iOut
iOut=GfForceSetValue(gf, "F_JOHANSSON", oe%F_JOHANSSON) .and. iOut
iOut=GfForceSetValue(gf, "F_SIDE", oe%F_SIDE) .and. iOut
iOut=GfForceSetValue(gf, "F_CENTRAL", oe%F_CENTRAL) .and. iOut
iOut=GfForceSetValue(gf, "F_CONVEX", oe%F_CONVEX) .and. iOut
iOut=GfForceSetValue(gf, "F_REFLEC", oe%F_REFLEC) .and. iOut
iOut=GfForceSetValue(gf, "F_RUL_ABS", oe%F_RUL_ABS) .and. iOut
iOut=GfForceSetValue(gf, "F_RULING", oe%F_RULING) .and. iOut
iOut=GfForceSetValue(gf, "F_PW", oe%F_PW) .and. iOut
iOut=GfForceSetValue(gf, "F_PW_C", oe%F_PW_C) .and. iOut
iOut=GfForceSetValue(gf, "F_VIRTUAL", oe%F_VIRTUAL) .and. iOut
iOut=GfForceSetValue(gf, "FSHAPE", oe%FSHAPE) .and. iOut
iOut=GfForceSetValue(gf, "FHIT_C", oe%FHIT_C) .and. iOut
iOut=GfForceSetValue(gf, "F_MONO", oe%F_MONO) .and. iOut
iOut=GfForceSetValue(gf, "F_REFRAC", oe%F_REFRAC) .and. iOut
iOut=GfForceSetValue(gf, "F_DEFAULT", oe%F_DEFAULT) .and. iOut
iOut=GfForceSetValue(gf, "F_REFL", oe%F_REFL) .and. iOut
iOut=GfForceSetValue(gf, "F_HUNT", oe%F_HUNT) .and. iOut
iOut=GfForceSetValue(gf, "F_CRYSTAL", oe%F_CRYSTAL) .and. iOut
iOut=GfForceSetValue(gf, "F_PHOT_CENT", oe%F_PHOT_CENT) .and. iOut
iOut=GfForceSetValue(gf, "F_ROUGHNESS", oe%F_ROUGHNESS) .and. iOut
iOut=GfForceSetValue(gf, "F_ANGLE", oe%F_ANGLE) .and. iOut
iOut=GfForceSetValue(gf, "NPOINT", oe%NPOINT) .and. iOut
iOut=GfForceSetValue(gf, "NCOL", oe%NCOL) .and. iOut
iOut=GfForceSetValue(gf, "N_SCREEN", oe%N_SCREEN) .and. iOut
iOut=GfForceSetValue(gf, "ISTAR1", oe%ISTAR1) .and. iOut
iOut=GfForceSetValue(gf, "CIL_ANG", oe%CIL_ANG) .and. iOut
iOut=GfForceSetValue(gf, "ELL_THE", oe%ELL_THE) .and. iOut
iOut=GfForceSetValue(gf, "N_PLATES", oe%N_PLATES) .and. iOut
iOut=GfForceSetValue(gf, "IG_SEED", oe%IG_SEED) .and. iOut
iOut=GfForceSetValue(gf, "MOSAIC_SEED", oe%MOSAIC_SEED) .and. iOut
iOut=GfForceSetValue(gf, "ALPHA", oe%ALPHA) .and. iOut
iOut=GfForceSetValue(gf, "SSOUR", oe%SSOUR) .and. iOut
iOut=GfForceSetValue(gf, "THETA", oe%THETA) .and. iOut
iOut=GfForceSetValue(gf, "SIMAG", oe%SIMAG) .and. iOut
iOut=GfForceSetValue(gf, "RDSOUR", oe%RDSOUR) .and. iOut
iOut=GfForceSetValue(gf, "RTHETA", oe%RTHETA) .and. iOut
iOut=GfForceSetValue(gf, "OFF_SOUX", oe%OFF_SOUX) .and. iOut
iOut=GfForceSetValue(gf, "OFF_SOUY", oe%OFF_SOUY) .and. iOut
iOut=GfForceSetValue(gf, "OFF_SOUZ", oe%OFF_SOUZ) .and. iOut
iOut=GfForceSetValue(gf, "ALPHA_S", oe%ALPHA_S) .and. iOut
iOut=GfForceSetValue(gf, "RLEN1", oe%RLEN1) .and. iOut
iOut=GfForceSetValue(gf, "RLEN2", oe%RLEN2) .and. iOut
iOut=GfForceSetValue(gf, "RMIRR", oe%RMIRR) .and. iOut
iOut=GfForceSetValue(gf, "AXMAJ", oe%AXMAJ) .and. iOut
iOut=GfForceSetValue(gf, "AXMIN", oe%AXMIN) .and. iOut
iOut=GfForceSetValue(gf, "CONE_A", oe%CONE_A) .and. iOut
iOut=GfForceSetValue(gf, "R_MAJ", oe%R_MAJ) .and. iOut
iOut=GfForceSetValue(gf, "R_MIN", oe%R_MIN) .and. iOut
iOut=GfForceSetValue(gf, "RWIDX1", oe%RWIDX1) .and. iOut
iOut=GfForceSetValue(gf, "RWIDX2", oe%RWIDX2) .and. iOut
iOut=GfForceSetValue(gf, "PARAM", oe%PARAM) .and. iOut
iOut=GfForceSetValue(gf, "HUNT_H", oe%HUNT_H) .and. iOut
iOut=GfForceSetValue(gf, "HUNT_L", oe%HUNT_L) .and. iOut
iOut=GfForceSetValue(gf, "BLAZE", oe%BLAZE) .and. iOut
iOut=GfForceSetValue(gf, "RULING", oe%RULING) .and. iOut
iOut=GfForceSetValue(gf, "ORDER", oe%ORDER) .and. iOut
iOut=GfForceSetValue(gf, "PHOT_CENT", oe%PHOT_CENT) .and. iOut
iOut=GfForceSetValue(gf, "X_ROT", oe%X_ROT) .and. iOut
iOut=GfForceSetValue(gf, "D_SPACING", oe%D_SPACING) .and. iOut
iOut=GfForceSetValue(gf, "A_BRAGG", oe%A_BRAGG) .and. iOut
iOut=GfForceSetValue(gf, "SPREAD_MOS", oe%SPREAD_MOS) .and. iOut
iOut=GfForceSetValue(gf, "THICKNESS", oe%THICKNESS) .and. iOut
iOut=GfForceSetValue(gf, "R_JOHANSSON", oe%R_JOHANSSON) .and. iOut
iOut=GfForceSetValue(gf, "Y_ROT", oe%Y_ROT) .and. iOut
iOut=GfForceSetValue(gf, "Z_ROT", oe%Z_ROT) .and. iOut
iOut=GfForceSetValue(gf, "OFFX", oe%OFFX) .and. iOut
iOut=GfForceSetValue(gf, "OFFY", oe%OFFY) .and. iOut
iOut=GfForceSetValue(gf, "OFFZ", oe%OFFZ) .and. iOut
iOut=GfForceSetValue(gf, "SLLEN", oe%SLLEN) .and. iOut
iOut=GfForceSetValue(gf, "SLWID", oe%SLWID) .and. iOut
iOut=GfForceSetValue(gf, "SLTILT", oe%SLTILT) .and. iOut
iOut=GfForceSetValue(gf, "COD_LEN", oe%COD_LEN) .and. iOut
iOut=GfForceSetValue(gf, "COD_WID", oe%COD_WID) .and. iOut
iOut=GfForceSetValue(gf, "X_SOUR", oe%X_SOUR) .and. iOut
iOut=GfForceSetValue(gf, "Y_SOUR", oe%Y_SOUR) .and. iOut
iOut=GfForceSetValue(gf, "Z_SOUR", oe%Z_SOUR) .and. iOut
iOut=GfForceSetValue(gf, "X_SOUR_ROT", oe%X_SOUR_ROT) .and. iOut
iOut=GfForceSetValue(gf, "Y_SOUR_ROT", oe%Y_SOUR_ROT) .and. iOut
iOut=GfForceSetValue(gf, "Z_SOUR_ROT", oe%Z_SOUR_ROT) .and. iOut
iOut=GfForceSetValue(gf, "R_LAMBDA", oe%R_LAMBDA) .and. iOut
iOut=GfForceSetValue(gf, "THETA_I", oe%THETA_I) .and. iOut
iOut=GfForceSetValue(gf, "ALPHA_I", oe%ALPHA_I) .and. iOut
iOut=GfForceSetValue(gf, "T_INCIDENCE", oe%T_INCIDENCE) .and. iOut
iOut=GfForceSetValue(gf, "T_SOURCE", oe%T_SOURCE) .and. iOut
iOut=GfForceSetValue(gf, "T_IMAGE", oe%T_IMAGE) .and. iOut
iOut=GfForceSetValue(gf, "T_REFLECTION", oe%T_REFLECTION) .and. iOut
iOut=GfForceSetValue(gf, "FILE_SOURCE", oe%FILE_SOURCE) .and. iOut
iOut=GfForceSetValue(gf, "FILE_RIP", oe%FILE_RIP) .and. iOut
iOut=GfForceSetValue(gf, "FILE_REFL", oe%FILE_REFL) .and. iOut
iOut=GfForceSetValue(gf, "FILE_MIR", oe%FILE_MIR) .and. iOut
iOut=GfForceSetValue(gf, "FILE_ROUGH", oe%FILE_ROUGH) .and. iOut
iOut=GfForceSetValue(gf, "FZP", oe%FZP) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_R1", oe%HOLO_R1) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_R2", oe%HOLO_R2) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_DEL", oe%HOLO_DEL) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_GAM", oe%HOLO_GAM) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_W", oe%HOLO_W) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_RT1", oe%HOLO_RT1) .and. iOut
iOut=GfForceSetValue(gf, "HOLO_RT2", oe%HOLO_RT2) .and. iOut
iOut=GfForceSetValue(gf, "AZIM_FAN", oe%AZIM_FAN) .and. iOut
iOut=GfForceSetValue(gf, "DIST_FAN", oe%DIST_FAN) .and. iOut
iOut=GfForceSetValue(gf, "COMA_FAC", oe%COMA_FAC) .and. iOut
iOut=GfForceSetValue(gf, "ALFA", oe%ALFA) .and. iOut
iOut=GfForceSetValue(gf, "GAMMA", oe%GAMMA) .and. iOut
iOut=GfForceSetValue(gf, "R_IND_OBJ", oe%R_IND_OBJ) .and. iOut
iOut=GfForceSetValue(gf, "R_IND_IMA", oe%R_IND_IMA) .and. iOut
iOut=GfForceSetValue(gf, "R_ATTENUATION_OBJ", oe%R_ATTENUATION_OBJ) .and. iOut
iOut=GfForceSetValue(gf, "R_ATTENUATION_IMA", oe%R_ATTENUATION_IMA) .and. iOut
iOut=GfForceSetValue(gf, "F_R_IND", oe%F_R_IND) .and. iOut
iOut=GfForceSetValue(gf, "FILE_R_IND_OBJ", oe%FILE_R_IND_OBJ) .and. iOut
iOut=GfForceSetValue(gf, "FILE_R_IND_IMA", oe%FILE_R_IND_IMA) .and. iOut
iOut=GfForceSetValue(gf, "RUL_A1", oe%RUL_A1) .and. iOut
iOut=GfForceSetValue(gf, "RUL_A2", oe%RUL_A2) .and. iOut
iOut=GfForceSetValue(gf, "RUL_A3", oe%RUL_A3) .and. iOut
iOut=GfForceSetValue(gf, "RUL_A4", oe%RUL_A4) .and. iOut
iOut=GfForceSetValue(gf, "F_POLSEL", oe%F_POLSEL) .and. iOut
iOut=GfForceSetValue(gf, "F_FACET", oe%F_FACET) .and. iOut
iOut=GfForceSetValue(gf, "F_FAC_ORIENT", oe%F_FAC_ORIENT) .and. iOut
iOut=GfForceSetValue(gf, "F_FAC_LATT", oe%F_FAC_LATT) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_LENX", oe%RFAC_LENX) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_LENY", oe%RFAC_LENY) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_PHAX", oe%RFAC_PHAX) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_PHAY", oe%RFAC_PHAY) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_DELX1", oe%RFAC_DELX1) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_DELX2", oe%RFAC_DELX2) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_DELY1", oe%RFAC_DELY1) .and. iOut
iOut=GfForceSetValue(gf, "RFAC_DELY2", oe%RFAC_DELY2) .and. iOut
iOut=GfForceSetValue(gf, "FILE_FAC", oe%FILE_FAC) .and. iOut
iOut=GfForceSetValue(gf, "F_SEGMENT", oe%F_SEGMENT) .and. iOut
iOut=GfForceSetValue(gf, "ISEG_XNUM", oe%ISEG_XNUM) .and. iOut
iOut=GfForceSetValue(gf, "ISEG_YNUM", oe%ISEG_YNUM) .and. iOut
iOut=GfForceSetValue(gf, "FILE_SEGMENT", oe%FILE_SEGMENT) .and. iOut
iOut=GfForceSetValue(gf, "FILE_SEGP", oe%FILE_SEGP) .and. iOut
iOut=GfForceSetValue(gf, "SEG_LENX", oe%SEG_LENX) .and. iOut
iOut=GfForceSetValue(gf, "SEG_LENY", oe%SEG_LENY) .and. iOut
iOut=GfForceSetValue(gf, "F_KOMA", oe%F_KOMA) .and. iOut
iOut=GfForceSetValue(gf, "FILE_KOMA", oe%FILE_KOMA) .and. iOut
iOut=GfForceSetValue(gf, "F_EXIT_SHAPE", oe%F_EXIT_SHAPE) .and. iOut
iOut=GfForceSetValue(gf, "F_INC_MNOR_ANG", oe%F_INC_MNOR_ANG) .and. iOut
iOut=GfForceSetValue(gf, "ZKO_LENGTH", oe%ZKO_LENGTH) .and. iOut
iOut=GfForceSetValue(gf, "RKOMA_CX", oe%RKOMA_CX) .and. iOut
iOut=GfForceSetValue(gf, "RKOMA_CY", oe%RKOMA_CY) .and. iOut
iOut=GfForceSetValue(gf, "F_KOMA_CA", oe%F_KOMA_CA) .and. iOut
iOut=GfForceSetValue(gf, "FILE_KOMA_CA", oe%FILE_KOMA_CA) .and. iOut
iOut=GfForceSetValue(gf, "F_KOMA_BOUNCE", oe%F_KOMA_BOUNCE) .and. iOut
iOut=GfForceSetValue(gf, "X_RIP_AMP", oe%X_RIP_AMP) .and. iOut
iOut=GfForceSetValue(gf, "X_RIP_WAV", oe%X_RIP_WAV) .and. iOut
iOut=GfForceSetValue(gf, "X_PHASE", oe%X_PHASE) .and. iOut
iOut=GfForceSetValue(gf, "Y_RIP_AMP", oe%Y_RIP_AMP) .and. iOut
iOut=GfForceSetValue(gf, "Y_RIP_WAV", oe%Y_RIP_WAV) .and. iOut
iOut=GfForceSetValue(gf, "Y_PHASE", oe%Y_PHASE) .and. iOut
iOut=GfForceSetValue(gf, "N_RIP", oe%N_RIP) .and. iOut
iOut=GfForceSetValue(gf, "ROUGH_X", oe%ROUGH_X) .and. iOut
iOut=GfForceSetValue(gf, "ROUGH_Y", oe%ROUGH_Y) .and. iOut
iOut=GfForceSetValue(gf, "OE_NUMBER", oe%OE_NUMBER) .and. iOut
iOut=GfForceSetValue(gf, "IDUMMY", oe%IDUMMY) .and. iOut
iOut=GfForceSetValue(gf, "DUMMY", oe%DUMMY) .and. iOut
iOut=GfSetArrValue(gf, "CX_SLIT", oe%CX_SLIT) .and. iOut
iOut=GfSetArrValue(gf, "CZ_SLIT", oe%CZ_SLIT) .and. iOut
iOut=GfSetArrValue(gf, "D_PLATE", oe%D_PLATE) .and. iOut
iOut=GfSetArrValue(gf, "FILE_ABS",oe%FILE_ABS) .and. iOut
iOut=GfSetArrValue(gf, "FILE_SCR_EXT",oe%FILE_SCR_EXT) .and. iOut
iOut=GfSetArrValue(gf, "I_ABS", oe%I_ABS) .and. iOut
iOut=GfSetArrValue(gf, "I_SCREEN", oe%I_SCREEN) .and. iOut
iOut=GfSetArrValue(gf, "I_SLIT", oe%I_SLIT) .and. iOut
iOut=GfSetArrValue(gf, "I_STOP", oe%I_STOP) .and. iOut
iOut=GfSetArrValue(gf, "K_SLIT", oe%K_SLIT) .and. iOut
iOut=GfSetArrValue(gf, "RX_SLIT", oe%RX_SLIT) .and. iOut
iOut=GfSetArrValue(gf, "RZ_SLIT", oe%RZ_SLIT) .and. iOut
iOut=GfSetArrValue(gf, "SCR_NUMBER", oe%SCR_NUMBER) .and. iOut
iOut=GfSetArrValue(gf, "SL_DIS", oe%SL_DIS) .and. iOut
iOut=GfSetArrValue(gf, "THICK", oe%THICK) .and. iOut
iOut=GfSetArrValue(gf, "CCC", oe%CCC) .and. iOut

    !! END CODE CREATED AUTOMATICALLY (cpp)
  End Subroutine PoolOEToGf
  
  !
  !
  !
  
  
  Subroutine GfToPoolSource(gf,src)
    
    type(gfType), intent(in)        :: gf 
    type(poolSource), intent(inout) :: src 
    logical                         :: iOut


    !  WARNING WARNING WARNING 
    !  due to mysterious reasons in g95:
    ! G95 (GCC 4.1.2 (g95 0.92!) Jun 18 2009)
    !  the line following this comment is essential to overcome a 
    !  bug of the compiler 
    !
    !  for more info : 
    !  http://groups.google.ca/group/comp.lang.fortran/browse_thread/thread/cc520f089fc27460
    !  http://gcc.gnu.org/bugzilla/show_bug.cgi?id=41479
    !  http://gcc.gnu.org/wiki/GFortranBinaries
    !  use other more mature compiler to avoid it!
    !srio danger and smeagolas too

    SELECT CASE (OS)
      CASE (1) 
           iout = gffilewrite(gf,"tmp.dat")
      CASE DEFAULT
           iout = gffilewrite(gf,"/dev/null")
    END SELECT

    !! START CODE CREATED AUTOMATICALLY (cpp)






iOut=GfGetValue(gf,"FDISTR",src%FDISTR)  .and. iOut
iOut=GfGetValue(gf,"FGRID",src%FGRID)  .and. iOut
iOut=GfGetValue(gf,"FSOUR",src%FSOUR)  .and. iOut
iOut=GfGetValue(gf,"FSOURCE_DEPTH",src%FSOURCE_DEPTH)  .and. iOut
iOut=GfGetValue(gf,"F_COHER",src%F_COHER)  .and. iOut
iOut=GfGetValue(gf,"F_COLOR",src%F_COLOR)  .and. iOut
iOut=GfGetValue(gf,"F_PHOT",src%F_PHOT)  .and. iOut
iOut=GfGetValue(gf,"F_POL",src%F_POL)  .and. iOut
iOut=GfGetValue(gf,"F_POLAR",src%F_POLAR)  .and. iOut
iOut=GfGetValue(gf,"F_OPD",src%F_OPD)  .and. iOut
iOut=GfGetValue(gf,"F_WIGGLER",src%F_WIGGLER)  .and. iOut
iOut=GfGetValue(gf,"F_BOUND_SOUR",src%F_BOUND_SOUR)  .and. iOut
iOut=GfGetValue(gf,"F_SR_TYPE",src%F_SR_TYPE)  .and. iOut
iOut=GfGetValue(gf,"ISTAR1",src%ISTAR1)  .and. iOut
iOut=GfGetValue(gf,"NPOINT",src%NPOINT)  .and. iOut
iOut=GfGetValue(gf,"NCOL",src%NCOL)  .and. iOut
iOut=GfGetValue(gf,"N_CIRCLE",src%N_CIRCLE)  .and. iOut
iOut=GfGetValue(gf,"N_COLOR",src%N_COLOR)  .and. iOut
iOut=GfGetValue(gf,"N_CONE",src%N_CONE)  .and. iOut
iOut=GfGetValue(gf,"IDO_VX",src%IDO_VX)  .and. iOut
iOut=GfGetValue(gf,"IDO_VZ",src%IDO_VZ)  .and. iOut
iOut=GfGetValue(gf,"IDO_X_S",src%IDO_X_S)  .and. iOut
iOut=GfGetValue(gf,"IDO_Y_S",src%IDO_Y_S)  .and. iOut
iOut=GfGetValue(gf,"IDO_Z_S",src%IDO_Z_S)  .and. iOut
iOut=GfGetValue(gf,"IDO_XL",src%IDO_XL)  .and. iOut
iOut=GfGetValue(gf,"IDO_XN",src%IDO_XN)  .and. iOut
iOut=GfGetValue(gf,"IDO_ZL",src%IDO_ZL)  .and. iOut
iOut=GfGetValue(gf,"IDO_ZN",src%IDO_ZN)  .and. iOut
iOut=GfGetValue(gf,"SIGXL1",src%SIGXL1)  .and. iOut
iOut=GfGetValue(gf,"SIGXL2",src%SIGXL2)  .and. iOut
iOut=GfGetValue(gf,"SIGXL3",src%SIGXL3)  .and. iOut
iOut=GfGetValue(gf,"SIGXL4",src%SIGXL4)  .and. iOut
iOut=GfGetValue(gf,"SIGXL5",src%SIGXL5)  .and. iOut
iOut=GfGetValue(gf,"SIGXL6",src%SIGXL6)  .and. iOut
iOut=GfGetValue(gf,"SIGXL7",src%SIGXL7)  .and. iOut
iOut=GfGetValue(gf,"SIGXL8",src%SIGXL8)  .and. iOut
iOut=GfGetValue(gf,"SIGXL9",src%SIGXL9)  .and. iOut
iOut=GfGetValue(gf,"SIGXL10",src%SIGXL10)  .and. iOut
iOut=GfGetValue(gf,"SIGZL1",src%SIGZL1)  .and. iOut
iOut=GfGetValue(gf,"SIGZL2",src%SIGZL2)  .and. iOut
iOut=GfGetValue(gf,"SIGZL3",src%SIGZL3)  .and. iOut
iOut=GfGetValue(gf,"SIGZL4",src%SIGZL4)  .and. iOut
iOut=GfGetValue(gf,"SIGZL5",src%SIGZL5)  .and. iOut
iOut=GfGetValue(gf,"SIGZL6",src%SIGZL6)  .and. iOut
iOut=GfGetValue(gf,"SIGZL7",src%SIGZL7)  .and. iOut
iOut=GfGetValue(gf,"SIGZL8",src%SIGZL8)  .and. iOut
iOut=GfGetValue(gf,"SIGZL9",src%SIGZL9)  .and. iOut
iOut=GfGetValue(gf,"SIGZL10",src%SIGZL10)  .and. iOut
iOut=GfGetValue(gf,"CONV_FACT",src%CONV_FACT)  .and. iOut
iOut=GfGetValue(gf,"CONE_MAX",src%CONE_MAX)  .and. iOut
iOut=GfGetValue(gf,"CONE_MIN",src%CONE_MIN)  .and. iOut
iOut=GfGetValue(gf,"EPSI_DX",src%EPSI_DX)  .and. iOut
iOut=GfGetValue(gf,"EPSI_DZ",src%EPSI_DZ)  .and. iOut
iOut=GfGetValue(gf,"EPSI_X",src%EPSI_X)  .and. iOut
iOut=GfGetValue(gf,"EPSI_Z",src%EPSI_Z)  .and. iOut
iOut=GfGetValue(gf,"HDIV1",src%HDIV1)  .and. iOut
iOut=GfGetValue(gf,"HDIV2",src%HDIV2)  .and. iOut
iOut=GfGetValue(gf,"PH1",src%PH1)  .and. iOut
iOut=GfGetValue(gf,"PH2",src%PH2)  .and. iOut
iOut=GfGetValue(gf,"PH3",src%PH3)  .and. iOut
iOut=GfGetValue(gf,"PH4",src%PH4)  .and. iOut
iOut=GfGetValue(gf,"PH5",src%PH5)  .and. iOut
iOut=GfGetValue(gf,"PH6",src%PH6)  .and. iOut
iOut=GfGetValue(gf,"PH7",src%PH7)  .and. iOut
iOut=GfGetValue(gf,"PH8",src%PH8)  .and. iOut
iOut=GfGetValue(gf,"PH9",src%PH9)  .and. iOut
iOut=GfGetValue(gf,"PH10",src%PH10)  .and. iOut
iOut=GfGetValue(gf,"RL1",src%RL1)  .and. iOut
iOut=GfGetValue(gf,"RL2",src%RL2)  .and. iOut
iOut=GfGetValue(gf,"RL3",src%RL3)  .and. iOut
iOut=GfGetValue(gf,"RL4",src%RL4)  .and. iOut
iOut=GfGetValue(gf,"RL5",src%RL5)  .and. iOut
iOut=GfGetValue(gf,"RL6",src%RL6)  .and. iOut
iOut=GfGetValue(gf,"RL7",src%RL7)  .and. iOut
iOut=GfGetValue(gf,"RL8",src%RL8)  .and. iOut
iOut=GfGetValue(gf,"RL9",src%RL9)  .and. iOut
iOut=GfGetValue(gf,"RL10",src%RL10)  .and. iOut
iOut=GfGetValue(gf,"BENER",src%BENER)  .and. iOut
iOut=GfGetValue(gf,"POL_ANGLE",src%POL_ANGLE)  .and. iOut
iOut=GfGetValue(gf,"POL_DEG",src%POL_DEG)  .and. iOut
iOut=GfGetValue(gf,"R_ALADDIN",src%R_ALADDIN)  .and. iOut
iOut=GfGetValue(gf,"R_MAGNET",src%R_MAGNET)  .and. iOut
iOut=GfGetValue(gf,"SIGDIX",src%SIGDIX)  .and. iOut
iOut=GfGetValue(gf,"SIGDIZ",src%SIGDIZ)  .and. iOut
iOut=GfGetValue(gf,"SIGMAX",src%SIGMAX)  .and. iOut
iOut=GfGetValue(gf,"SIGMAY",src%SIGMAY)  .and. iOut
iOut=GfGetValue(gf,"SIGMAZ",src%SIGMAZ)  .and. iOut
iOut=GfGetValue(gf,"VDIV1",src%VDIV1)  .and. iOut
iOut=GfGetValue(gf,"VDIV2",src%VDIV2)  .and. iOut
iOut=GfGetValue(gf,"WXSOU",src%WXSOU)  .and. iOut
iOut=GfGetValue(gf,"WYSOU",src%WYSOU)  .and. iOut
iOut=GfGetValue(gf,"WZSOU",src%WZSOU)  .and. iOut
iOut=GfGetValue(gf,"PLASMA_ANGLE",src%PLASMA_ANGLE)  .and. iOut
iOut=GfGetValue(gf,"FILE_TRAJ",src%FILE_TRAJ)  .and. iOut
iOut=GfGetValue(gf,"FILE_SOURCE",src%FILE_SOURCE)  .and. iOut
iOut=GfGetValue(gf,"FILE_BOUND",src%FILE_BOUND)  .and. iOut
iOut=GfGetValue(gf,"OE_NUMBER",src%OE_NUMBER)  .and. iOut
iOut=GfGetValue(gf,"NTOTALPOINT",src%NTOTALPOINT)  .and. iOut
iOut=GfGetValue(gf,"IDUMMY",src%IDUMMY)  .and. iOut
iOut=GfGetValue(gf,"DUMMY",src%DUMMY)  .and. iOut
iOut=GfGetValue(gf,"F_NEW",src%F_NEW)  .and. iOut






    !! END CODE CREATED AUTOMATICALLY (cpp)

    IF (.not. iOut) THEN
      print *,'GfToPoolSource: Warning: GfGetValue failed to get some variables'
    ENDIF
  End Subroutine GfToPoolSource
  
  !
  !
  !
  
  
  Subroutine GfToPoolOE(gf,oe)
    
    type(gfType),intent(in)     :: gf 
    type(poolOE),intent(inout)  :: oe 
    logical                     :: iOut
    integer(kind=ski)           :: zero=0,i
    
    !  WARNING WARNING WARNING
    !  due to mysterious reasons in g95:
    ! G95 (GCC 4.1.2 (g95 0.92!) Jun 18 2009)
    !  the line following this comment is essential to overcome a
    !  bug of the compiler
    !
    !  for more info :
    !  http://groups.google.ca/group/comp.lang.fortran/browse_thread/thread/cc520f089fc27460
    !  http://gcc.gnu.org/bugzilla/show_bug.cgi?id=41479
    !  http://gcc.gnu.org/wiki/GFortranBinaries
    !  use other more mature compiler to avoid it!
    !srio danger and smeagolas too

    SELECT CASE (OS)
      CASE (1) 
           iout = gffilewrite(gf,"tmp.dat")
      CASE DEFAULT ! so 2,3,4
           iout = gffilewrite(gf,"/dev/null")
    END SELECT

!     iOut= GfGetValue(gf,"FMIRR",oe%FMIRR)                       .and. iOut 
!     ...
!     iOut= GfGetValue(gf,"THICK(10)",oe%THICK(10))               .and. iOut 

    !! START CODE CREATED AUTOMATICALLY (cpp)








iOut=GfGetValue(gf, "FMIRR", oe%FMIRR) .and. iOut
iOut=GfGetValue(gf, "F_TORUS", oe%F_TORUS) .and. iOut
iOut=GfGetValue(gf, "FCYL", oe%FCYL) .and. iOut
iOut=GfGetValue(gf, "F_EXT", oe%F_EXT) .and. iOut
iOut=GfGetValue(gf, "FSTAT", oe%FSTAT) .and. iOut
iOut=GfGetValue(gf, "F_SCREEN", oe%F_SCREEN) .and. iOut
iOut=GfGetValue(gf, "F_PLATE", oe%F_PLATE) .and. iOut
iOut=GfGetValue(gf, "FSLIT", oe%FSLIT) .and. iOut
iOut=GfGetValue(gf, "FWRITE", oe%FWRITE) .and. iOut
iOut=GfGetValue(gf, "F_RIPPLE", oe%F_RIPPLE) .and. iOut
iOut=GfGetValue(gf, "F_MOVE", oe%F_MOVE) .and. iOut
iOut=GfGetValue(gf, "F_THICK", oe%F_THICK) .and. iOut
iOut=GfGetValue(gf, "F_BRAGG_A", oe%F_BRAGG_A) .and. iOut
iOut=GfGetValue(gf, "F_G_S", oe%F_G_S) .and. iOut
iOut=GfGetValue(gf, "F_R_RAN", oe%F_R_RAN) .and. iOut
iOut=GfGetValue(gf, "F_GRATING", oe%F_GRATING) .and. iOut
iOut=GfGetValue(gf, "F_MOSAIC", oe%F_MOSAIC) .and. iOut
iOut=GfGetValue(gf, "F_JOHANSSON", oe%F_JOHANSSON) .and. iOut
iOut=GfGetValue(gf, "F_SIDE", oe%F_SIDE) .and. iOut
iOut=GfGetValue(gf, "F_CENTRAL", oe%F_CENTRAL) .and. iOut
iOut=GfGetValue(gf, "F_CONVEX", oe%F_CONVEX) .and. iOut
iOut=GfGetValue(gf, "F_REFLEC", oe%F_REFLEC) .and. iOut
iOut=GfGetValue(gf, "F_RUL_ABS", oe%F_RUL_ABS) .and. iOut
iOut=GfGetValue(gf, "F_RULING", oe%F_RULING) .and. iOut
iOut=GfGetValue(gf, "F_PW", oe%F_PW) .and. iOut
iOut=GfGetValue(gf, "F_PW_C", oe%F_PW_C) .and. iOut
iOut=GfGetValue(gf, "F_VIRTUAL", oe%F_VIRTUAL) .and. iOut
iOut=GfGetValue(gf, "FSHAPE", oe%FSHAPE) .and. iOut
iOut=GfGetValue(gf, "FHIT_C", oe%FHIT_C) .and. iOut
iOut=GfGetValue(gf, "F_MONO", oe%F_MONO) .and. iOut
iOut=GfGetValue(gf, "F_REFRAC", oe%F_REFRAC) .and. iOut
iOut=GfGetValue(gf, "F_DEFAULT", oe%F_DEFAULT) .and. iOut
iOut=GfGetValue(gf, "F_REFL", oe%F_REFL) .and. iOut
iOut=GfGetValue(gf, "F_HUNT", oe%F_HUNT) .and. iOut
iOut=GfGetValue(gf, "F_CRYSTAL", oe%F_CRYSTAL) .and. iOut
iOut=GfGetValue(gf, "F_PHOT_CENT", oe%F_PHOT_CENT) .and. iOut
iOut=GfGetValue(gf, "F_ROUGHNESS", oe%F_ROUGHNESS) .and. iOut
iOut=GfGetValue(gf, "F_ANGLE", oe%F_ANGLE) .and. iOut
iOut=GfGetValue(gf, "NPOINT", oe%NPOINT) .and. iOut
iOut=GfGetValue(gf, "NCOL", oe%NCOL) .and. iOut
iOut=GfGetValue(gf, "N_SCREEN", oe%N_SCREEN) .and. iOut
iOut=GfGetValue(gf, "ISTAR1", oe%ISTAR1) .and. iOut
iOut=GfGetValue(gf, "CIL_ANG", oe%CIL_ANG) .and. iOut
iOut=GfGetValue(gf, "ELL_THE", oe%ELL_THE) .and. iOut
iOut=GfGetValue(gf, "N_PLATES", oe%N_PLATES) .and. iOut
iOut=GfGetValue(gf, "IG_SEED", oe%IG_SEED) .and. iOut
iOut=GfGetValue(gf, "MOSAIC_SEED", oe%MOSAIC_SEED) .and. iOut
iOut=GfGetValue(gf, "ALPHA", oe%ALPHA) .and. iOut
iOut=GfGetValue(gf, "SSOUR", oe%SSOUR) .and. iOut
iOut=GfGetValue(gf, "THETA", oe%THETA) .and. iOut
iOut=GfGetValue(gf, "SIMAG", oe%SIMAG) .and. iOut
iOut=GfGetValue(gf, "RDSOUR", oe%RDSOUR) .and. iOut
iOut=GfGetValue(gf, "RTHETA", oe%RTHETA) .and. iOut
iOut=GfGetValue(gf, "OFF_SOUX", oe%OFF_SOUX) .and. iOut
iOut=GfGetValue(gf, "OFF_SOUY", oe%OFF_SOUY) .and. iOut
iOut=GfGetValue(gf, "OFF_SOUZ", oe%OFF_SOUZ) .and. iOut
iOut=GfGetValue(gf, "ALPHA_S", oe%ALPHA_S) .and. iOut
iOut=GfGetValue(gf, "RLEN1", oe%RLEN1) .and. iOut
iOut=GfGetValue(gf, "RLEN2", oe%RLEN2) .and. iOut
iOut=GfGetValue(gf, "RMIRR", oe%RMIRR) .and. iOut
iOut=GfGetValue(gf, "AXMAJ", oe%AXMAJ) .and. iOut
iOut=GfGetValue(gf, "AXMIN", oe%AXMIN) .and. iOut
iOut=GfGetValue(gf, "CONE_A", oe%CONE_A) .and. iOut
iOut=GfGetValue(gf, "R_MAJ", oe%R_MAJ) .and. iOut
iOut=GfGetValue(gf, "R_MIN", oe%R_MIN) .and. iOut
iOut=GfGetValue(gf, "RWIDX1", oe%RWIDX1) .and. iOut
iOut=GfGetValue(gf, "RWIDX2", oe%RWIDX2) .and. iOut
iOut=GfGetValue(gf, "PARAM", oe%PARAM) .and. iOut
iOut=GfGetValue(gf, "HUNT_H", oe%HUNT_H) .and. iOut
iOut=GfGetValue(gf, "HUNT_L", oe%HUNT_L) .and. iOut
iOut=GfGetValue(gf, "BLAZE", oe%BLAZE) .and. iOut
iOut=GfGetValue(gf, "RULING", oe%RULING) .and. iOut
iOut=GfGetValue(gf, "ORDER", oe%ORDER) .and. iOut
iOut=GfGetValue(gf, "PHOT_CENT", oe%PHOT_CENT) .and. iOut
iOut=GfGetValue(gf, "X_ROT", oe%X_ROT) .and. iOut
iOut=GfGetValue(gf, "D_SPACING", oe%D_SPACING) .and. iOut
iOut=GfGetValue(gf, "A_BRAGG", oe%A_BRAGG) .and. iOut
iOut=GfGetValue(gf, "SPREAD_MOS", oe%SPREAD_MOS) .and. iOut
iOut=GfGetValue(gf, "THICKNESS", oe%THICKNESS) .and. iOut
iOut=GfGetValue(gf, "R_JOHANSSON", oe%R_JOHANSSON) .and. iOut
iOut=GfGetValue(gf, "Y_ROT", oe%Y_ROT) .and. iOut
iOut=GfGetValue(gf, "Z_ROT", oe%Z_ROT) .and. iOut
iOut=GfGetValue(gf, "OFFX", oe%OFFX) .and. iOut
iOut=GfGetValue(gf, "OFFY", oe%OFFY) .and. iOut
iOut=GfGetValue(gf, "OFFZ", oe%OFFZ) .and. iOut
iOut=GfGetValue(gf, "SLLEN", oe%SLLEN) .and. iOut
iOut=GfGetValue(gf, "SLWID", oe%SLWID) .and. iOut
iOut=GfGetValue(gf, "SLTILT", oe%SLTILT) .and. iOut
iOut=GfGetValue(gf, "COD_LEN", oe%COD_LEN) .and. iOut
iOut=GfGetValue(gf, "COD_WID", oe%COD_WID) .and. iOut
iOut=GfGetValue(gf, "X_SOUR", oe%X_SOUR) .and. iOut
iOut=GfGetValue(gf, "Y_SOUR", oe%Y_SOUR) .and. iOut
iOut=GfGetValue(gf, "Z_SOUR", oe%Z_SOUR) .and. iOut
iOut=GfGetValue(gf, "X_SOUR_ROT", oe%X_SOUR_ROT) .and. iOut
iOut=GfGetValue(gf, "Y_SOUR_ROT", oe%Y_SOUR_ROT) .and. iOut
iOut=GfGetValue(gf, "Z_SOUR_ROT", oe%Z_SOUR_ROT) .and. iOut
iOut=GfGetValue(gf, "R_LAMBDA", oe%R_LAMBDA) .and. iOut
iOut=GfGetValue(gf, "THETA_I", oe%THETA_I) .and. iOut
iOut=GfGetValue(gf, "ALPHA_I", oe%ALPHA_I) .and. iOut
iOut=GfGetValue(gf, "T_INCIDENCE", oe%T_INCIDENCE) .and. iOut
iOut=GfGetValue(gf, "T_SOURCE", oe%T_SOURCE) .and. iOut
iOut=GfGetValue(gf, "T_IMAGE", oe%T_IMAGE) .and. iOut
iOut=GfGetValue(gf, "T_REFLECTION", oe%T_REFLECTION) .and. iOut
iOut=GfGetValue(gf, "FILE_SOURCE", oe%FILE_SOURCE) .and. iOut
iOut=GfGetValue(gf, "FILE_RIP", oe%FILE_RIP) .and. iOut
iOut=GfGetValue(gf, "FILE_REFL", oe%FILE_REFL) .and. iOut
iOut=GfGetValue(gf, "FILE_MIR", oe%FILE_MIR) .and. iOut
iOut=GfGetValue(gf, "FILE_ROUGH", oe%FILE_ROUGH) .and. iOut
iOut=GfGetValue(gf, "FZP", oe%FZP) .and. iOut
iOut=GfGetValue(gf, "HOLO_R1", oe%HOLO_R1) .and. iOut
iOut=GfGetValue(gf, "HOLO_R2", oe%HOLO_R2) .and. iOut
iOut=GfGetValue(gf, "HOLO_DEL", oe%HOLO_DEL) .and. iOut
iOut=GfGetValue(gf, "HOLO_GAM", oe%HOLO_GAM) .and. iOut
iOut=GfGetValue(gf, "HOLO_W", oe%HOLO_W) .and. iOut
iOut=GfGetValue(gf, "HOLO_RT1", oe%HOLO_RT1) .and. iOut
iOut=GfGetValue(gf, "HOLO_RT2", oe%HOLO_RT2) .and. iOut
iOut=GfGetValue(gf, "AZIM_FAN", oe%AZIM_FAN) .and. iOut
iOut=GfGetValue(gf, "DIST_FAN", oe%DIST_FAN) .and. iOut
iOut=GfGetValue(gf, "COMA_FAC", oe%COMA_FAC) .and. iOut
iOut=GfGetValue(gf, "ALFA", oe%ALFA) .and. iOut
iOut=GfGetValue(gf, "GAMMA", oe%GAMMA) .and. iOut
iOut=GfGetValue(gf, "R_IND_OBJ", oe%R_IND_OBJ) .and. iOut
iOut=GfGetValue(gf, "R_IND_IMA", oe%R_IND_IMA) .and. iOut
iOut=GfGetValue(gf, "R_ATTENUATION_OBJ", oe%R_ATTENUATION_OBJ) .and. iOut
iOut=GfGetValue(gf, "R_ATTENUATION_IMA", oe%R_ATTENUATION_IMA) .and. iOut
iOut=GfGetValue(gf, "F_R_IND", oe%F_R_IND) .and. iOut
iOut=GfGetValue(gf, "FILE_R_IND_OBJ", oe%FILE_R_IND_OBJ) .and. iOut
iOut=GfGetValue(gf, "FILE_R_IND_IMA", oe%FILE_R_IND_IMA) .and. iOut
iOut=GfGetValue(gf, "RUL_A1", oe%RUL_A1) .and. iOut
iOut=GfGetValue(gf, "RUL_A2", oe%RUL_A2) .and. iOut
iOut=GfGetValue(gf, "RUL_A3", oe%RUL_A3) .and. iOut
iOut=GfGetValue(gf, "RUL_A4", oe%RUL_A4) .and. iOut
iOut=GfGetValue(gf, "F_POLSEL", oe%F_POLSEL) .and. iOut
iOut=GfGetValue(gf, "F_FACET", oe%F_FACET) .and. iOut
iOut=GfGetValue(gf, "F_FAC_ORIENT", oe%F_FAC_ORIENT) .and. iOut
iOut=GfGetValue(gf, "F_FAC_LATT", oe%F_FAC_LATT) .and. iOut
iOut=GfGetValue(gf, "RFAC_LENX", oe%RFAC_LENX) .and. iOut
iOut=GfGetValue(gf, "RFAC_LENY", oe%RFAC_LENY) .and. iOut
iOut=GfGetValue(gf, "RFAC_PHAX", oe%RFAC_PHAX) .and. iOut
iOut=GfGetValue(gf, "RFAC_PHAY", oe%RFAC_PHAY) .and. iOut
iOut=GfGetValue(gf, "RFAC_DELX1", oe%RFAC_DELX1) .and. iOut
iOut=GfGetValue(gf, "RFAC_DELX2", oe%RFAC_DELX2) .and. iOut
iOut=GfGetValue(gf, "RFAC_DELY1", oe%RFAC_DELY1) .and. iOut
iOut=GfGetValue(gf, "RFAC_DELY2", oe%RFAC_DELY2) .and. iOut
iOut=GfGetValue(gf, "FILE_FAC", oe%FILE_FAC) .and. iOut
iOut=GfGetValue(gf, "F_SEGMENT", oe%F_SEGMENT) .and. iOut
iOut=GfGetValue(gf, "ISEG_XNUM", oe%ISEG_XNUM) .and. iOut
iOut=GfGetValue(gf, "ISEG_YNUM", oe%ISEG_YNUM) .and. iOut
iOut=GfGetValue(gf, "FILE_SEGMENT", oe%FILE_SEGMENT) .and. iOut
iOut=GfGetValue(gf, "FILE_SEGP", oe%FILE_SEGP) .and. iOut
iOut=GfGetValue(gf, "SEG_LENX", oe%SEG_LENX) .and. iOut
iOut=GfGetValue(gf, "SEG_LENY", oe%SEG_LENY) .and. iOut
iOut=GfGetValue(gf, "F_KOMA", oe%F_KOMA) .and. iOut
iOut=GfGetValue(gf, "FILE_KOMA", oe%FILE_KOMA) .and. iOut
iOut=GfGetValue(gf, "F_EXIT_SHAPE", oe%F_EXIT_SHAPE) .and. iOut
iOut=GfGetValue(gf, "F_INC_MNOR_ANG", oe%F_INC_MNOR_ANG) .and. iOut
iOut=GfGetValue(gf, "ZKO_LENGTH", oe%ZKO_LENGTH) .and. iOut
iOut=GfGetValue(gf, "RKOMA_CX", oe%RKOMA_CX) .and. iOut
iOut=GfGetValue(gf, "RKOMA_CY", oe%RKOMA_CY) .and. iOut
iOut=GfGetValue(gf, "F_KOMA_CA", oe%F_KOMA_CA) .and. iOut
iOut=GfGetValue(gf, "FILE_KOMA_CA", oe%FILE_KOMA_CA) .and. iOut
iOut=GfGetValue(gf, "F_KOMA_BOUNCE", oe%F_KOMA_BOUNCE) .and. iOut
iOut=GfGetValue(gf, "X_RIP_AMP", oe%X_RIP_AMP) .and. iOut
iOut=GfGetValue(gf, "X_RIP_WAV", oe%X_RIP_WAV) .and. iOut
iOut=GfGetValue(gf, "X_PHASE", oe%X_PHASE) .and. iOut
iOut=GfGetValue(gf, "Y_RIP_AMP", oe%Y_RIP_AMP) .and. iOut
iOut=GfGetValue(gf, "Y_RIP_WAV", oe%Y_RIP_WAV) .and. iOut
iOut=GfGetValue(gf, "Y_PHASE", oe%Y_PHASE) .and. iOut
iOut=GfGetValue(gf, "N_RIP", oe%N_RIP) .and. iOut
iOut=GfGetValue(gf, "ROUGH_X", oe%ROUGH_X) .and. iOut
iOut=GfGetValue(gf, "ROUGH_Y", oe%ROUGH_Y) .and. iOut
iOut=GfGetValue(gf, "OE_NUMBER", oe%OE_NUMBER) .and. iOut
iOut=GfGetValue(gf, "IDUMMY", oe%IDUMMY) .and. iOut
iOut=GfGetValue(gf, "DUMMY", oe%DUMMY) .and. iOut
iOut=GfGetArrValue(gf, "CX_SLIT", oe%CX_SLIT) .and. iOut
iOut=GfGetArrValue(gf, "CZ_SLIT", oe%CZ_SLIT) .and. iOut
iOut=GfGetArrValue(gf, "D_PLATE", oe%D_PLATE) .and. iOut
iOut=GfGetArrValue(gf, "FILE_ABS", oe%FILE_ABS) .and. iOut
iOut=GfGetArrValue(gf, "FILE_SCR_EXT", oe%FILE_SCR_EXT) .and. iOut
iOut=GfGetArrValue(gf, "I_ABS", oe%I_ABS) .and. iOut
iOut=GfGetArrValue(gf, "I_SCREEN", oe%I_SCREEN) .and. iOut
iOut=GfGetArrValue(gf, "I_SLIT", oe%I_SLIT) .and. iOut
iOut=GfGetArrValue(gf, "I_STOP", oe%I_STOP) .and. iOut
iOut=GfGetArrValue(gf, "K_SLIT", oe%K_SLIT) .and. iOut
iOut=GfGetArrValue(gf, "RX_SLIT", oe%RX_SLIT) .and. iOut
iOut=GfGetArrValue(gf, "RZ_SLIT", oe%RZ_SLIT) .and. iOut
iOut=GfGetArrValue(gf, "SCR_NUMBER", oe%SCR_NUMBER) .and. iOut
iOut=GfGetArrValue(gf, "SL_DIS", oe%SL_DIS) .and. iOut
iOut=GfGetArrValue(gf, "THICK", oe%THICK) .and. iOut
iOut=GfGetArrValue(gf, "CCC", oe%CCC) .and. iOut

    !! END CODE CREATED AUTOMATICALLY (cpp)

    IF (.not. iOut) THEN
      print *,'GfToPoolOE: GfGetValue failed to get some variables'
    ENDIF
  End Subroutine GfToPoolOE
  
  
  subroutine PoolSourceLoad(pool00,filename) !bind(C,NAME="PoolSourceLoad")
    type (poolSource), intent(inout) :: pool00
    character(len=*), intent(in)     :: filename
    
    type (gfType) :: gf
    
    if(.not.GfFileLoad(gf,filename))  print *, "unable to load all file"
    call PoolSourceDefault(pool00)
    call GfToPoolSource(gf,pool00)
    
    return
  end subroutine PoolSourceLoad
  
  
  subroutine PoolSourceWrite(pool00,filename) !bind(C,NAME="PoolSourceWrite")
    type (poolSource), intent(inout) :: pool00
    character(len=*), intent(in)     :: filename
    type (gfType) :: gf

    call PoolSourceToGf(pool00,gf)
    if(.not.GfFileWrite(gf,filename)) print *, "unable to write all gftype"
    
    return
  end subroutine PoolSourceWrite
  
  subroutine PoolOELoad(pool01,filename) !bind(C,NAME="PoolOELoad")
    type (poolOE), intent(inout) :: pool01
    character(len=*), intent(in) :: filename
    
    type (gfType) :: gf
    
    if(.not.GfFileLoad(gf,filename))  print *, "unable to load all file"
    call PoolOEDefault(pool01)
    call GfToPoolOE(gf,pool01)
    
    return
  end subroutine PoolOELoad
  
  
  subroutine PoolOEWrite(pool01,filename) !bind(C,NAME="PoolOEWrite")
    type (poolOE), intent(inout) :: pool01
    character(len=*), intent(in) :: filename
    
    type (gfType) :: gf
    
    call PoolOEToGf(pool01,gf)
    if(.not.GfFileWrite(gf,filename)) print *, "unable to write all gftype"
    
    return
  end subroutine PoolOEWrite
  
!
!
!
    
  subroutine PoolSourceDefault(src)
    type (poolSource), intent(inout) :: src
    integer(kind=ski) :: i






src%FDISTR=2
src%FGRID=0
src%FSOUR=3
src%FSOURCE_DEPTH=1
src%F_COHER=0
src%F_COLOR=1
src%F_PHOT=1
src%F_POL=3
src%F_POLAR=1
src%F_OPD=1
src%F_WIGGLER=0
src%F_BOUND_SOUR=0
src%F_SR_TYPE=0
src%ISTAR1=6775431
src%NPOINT=5000
src%NCOL=18
src%N_CIRCLE=0
src%N_COLOR=2
src%N_CONE=0
src%IDO_VX=1
src%IDO_VZ=1
src%IDO_X_S=1
src%IDO_Y_S=1
src%IDO_Z_S=1
src%IDO_XL=0
src%IDO_XN=0
src%IDO_ZL=0
src%IDO_ZN=0
src%SIGXL1=0.0
src%SIGXL2=0.0
src%SIGXL3=0.0
src%SIGXL4=0.0
src%SIGXL5=0.0
src%SIGXL6=0.0
src%SIGXL7=0.0
src%SIGXL8=0.0
src%SIGXL9=0.0
src%SIGXL10=0.0
src%SIGZL1=0.0
src%SIGZL2=0.0
src%SIGZL3=0.0
src%SIGZL4=0.0
src%SIGZL5=0.0
src%SIGZL6=0.0
src%SIGZL7=0.0
src%SIGZL8=0.0
src%SIGZL9=0.0
src%SIGZL10=0.0
src%CONV_FACT=0.0
src%CONE_MAX=0.0
src%CONE_MIN=0.0
src%EPSI_DX=0.0
src%EPSI_DZ=0.0
src%EPSI_X=0.0
src%EPSI_Z=0.0
src%HDIV1=5.0e-7
src%HDIV2=5.0e-7
src%PH1=1.0e1
src%PH2=1.01e3
src%PH3=0.0
src%PH4=0.0
src%PH5=0.0
src%PH6=0.0
src%PH7=0.0
src%PH8=0.0
src%PH9=0.0
src%PH10=0.0
src%RL1=0.0
src%RL2=0.0
src%RL3=0.0
src%RL4=0.0
src%RL5=0.0
src%RL6=0.0
src%RL7=0.0
src%RL8=0.0
src%RL9=0.0
src%RL10=0.0
src%BENER=0.0
src%POL_ANGLE=0.0
src%POL_DEG=1.0
src%R_ALADDIN=0.0
src%R_MAGNET=0.0
src%SIGDIX=1.0e-3
src%SIGDIZ=1.0e-4
src%SIGMAX=1.0e-3
src%SIGMAY=1.0e-3
src%SIGMAZ=1.0e-3
src%VDIV1=5.0e-6
src%VDIV2=5.0e-6
src%WXSOU=1.0e-1
src%WYSOU=2.0e-1
src%WZSOU=2.0e-1
src%PLASMA_ANGLE=0.0
src%FILE_TRAJ=" "
src%FILE_SOURCE=" "
src%FILE_BOUND=" "
src%OE_NUMBER=0
src%NTOTALPOINT=10000000
src%IDUMMY=0
src%DUMMY=0.0
src%F_NEW=0






  end subroutine PoolSourceDefault

  subroutine PoolOEDefault(oe)
    type (poolOE), intent(inout) :: oe
    integer(kind=ski) :: i








oe%FMIRR=5
oe%F_TORUS=0
oe%FCYL=0
oe%F_EXT=0
oe%FSTAT=0
oe%F_SCREEN=0
oe%F_PLATE=0
oe%FSLIT=0
oe%FWRITE=0
oe%F_RIPPLE=0
oe%F_MOVE=0
oe%F_THICK=0
oe%F_BRAGG_A=0
oe%F_G_S=0
oe%F_R_RAN=0
oe%F_GRATING=0
oe%F_MOSAIC=0
oe%F_JOHANSSON=0
oe%F_SIDE=0
oe%F_CENTRAL=0
oe%F_CONVEX=0
oe%F_REFLEC=0
oe%F_RUL_ABS=0
oe%F_RULING=0
oe%F_PW=0
oe%F_PW_C=0
oe%F_VIRTUAL=0
oe%FSHAPE=1
oe%FHIT_C=0
oe%F_MONO=0
oe%F_REFRAC=0
oe%F_DEFAULT=1
oe%F_REFL=0
oe%F_HUNT=1
oe%F_CRYSTAL=0
oe%F_PHOT_CENT=0
oe%F_ROUGHNESS=0
oe%F_ANGLE=0
oe%NPOINT=5000
oe%NCOL=18
oe%N_SCREEN=0
oe%ISTAR1=0
oe%CIL_ANG=0.0
oe%ELL_THE=0.0
oe%N_PLATES=0
oe%IG_SEED=0
oe%MOSAIC_SEED=1626261131
oe%ALPHA=0.0
oe%SSOUR=0.0
oe%THETA=0.0
oe%SIMAG=0.0
oe%RDSOUR=0.0
oe%RTHETA=0.0
oe%OFF_SOUX=0.0
oe%OFF_SOUY=0.0
oe%OFF_SOUZ=0.0
oe%ALPHA_S=0.0
oe%RLEN1=0.0
oe%RLEN2=0.0
oe%RMIRR=0.0
oe%AXMAJ=0.0
oe%AXMIN=0.0
oe%CONE_A=0.0
oe%R_MAJ=0.0
oe%R_MIN=0.0
oe%RWIDX1=0.0
oe%RWIDX2=0.0
oe%PARAM=0.0
oe%HUNT_H=0.0
oe%HUNT_L=0.0
oe%BLAZE=0.0
oe%RULING=1.2e4
oe%ORDER=-1.0
oe%PHOT_CENT=5.0
oe%X_ROT=0.0
oe%D_SPACING=0.0
oe%A_BRAGG=0.0
oe%SPREAD_MOS=0.0
oe%THICKNESS=0.0
oe%R_JOHANSSON=0.0
oe%Y_ROT=0.0
oe%Z_ROT=0.0
oe%OFFX=0.0
oe%OFFY=0.0
oe%OFFZ=0.0
oe%SLLEN=0.0
oe%SLWID=0.0
oe%SLTILT=0.0
oe%COD_LEN=0.0
oe%COD_WID=0.0
oe%X_SOUR=0.0
oe%Y_SOUR=0.0
oe%Z_SOUR=0.0
oe%X_SOUR_ROT=0.0
oe%Y_SOUR_ROT=0.0
oe%Z_SOUR_ROT=0.0
oe%R_LAMBDA=5.0
oe%THETA_I=0.0
oe%ALPHA_I=0.0
oe%T_INCIDENCE=8.8e1
oe%T_SOURCE=1.0e1
oe%T_IMAGE=2.0e1
oe%T_REFLECTION=8.8e1
oe%FILE_SOURCE="begin.dat"
oe%FILE_RIP=""
oe%FILE_REFL=""
oe%FILE_MIR=""
oe%FILE_ROUGH=""
oe%FZP=0
oe%HOLO_R1=3.0e2
oe%HOLO_R2=3.0e2
oe%HOLO_DEL=-2.0e1
oe%HOLO_GAM=-2.0e1
oe%HOLO_W=4.87986e3
oe%HOLO_RT1=0.0
oe%HOLO_RT2=0.0
oe%AZIM_FAN=0.0
oe%DIST_FAN=0.0
oe%COMA_FAC=0.0
oe%ALFA=0.0
oe%GAMMA=0.0
oe%R_IND_OBJ=1.0
oe%R_IND_IMA=1.0
oe%R_ATTENUATION_OBJ=0.0
oe%R_ATTENUATION_IMA=0.0
oe%F_R_IND=0
oe%FILE_R_IND_OBJ=""
oe%FILE_R_IND_IMA=""
oe%RUL_A1=0.0
oe%RUL_A2=0.0
oe%RUL_A3=0.0
oe%RUL_A4=0.0
oe%F_POLSEL=4
oe%F_FACET=0
oe%F_FAC_ORIENT=0
oe%F_FAC_LATT=0
oe%RFAC_LENX=1.0e1
oe%RFAC_LENY=1.0e1
oe%RFAC_PHAX=0.0
oe%RFAC_PHAY=0.0
oe%RFAC_DELX1=0.0
oe%RFAC_DELX2=0.0
oe%RFAC_DELY1=0.0
oe%RFAC_DELY2=0.0
oe%FILE_FAC=""
oe%F_SEGMENT=0
oe%ISEG_XNUM=1
oe%ISEG_YNUM=1
oe%FILE_SEGMENT=""
oe%FILE_SEGP=""
oe%SEG_LENX=0.0
oe%SEG_LENY=0.0
oe%F_KOMA=0
oe%FILE_KOMA=""
oe%F_EXIT_SHAPE=0
oe%F_INC_MNOR_ANG=0
oe%ZKO_LENGTH=0.0
oe%RKOMA_CX=0.0
oe%RKOMA_CY=0.0
oe%F_KOMA_CA=0
oe%FILE_KOMA_CA=""
oe%F_KOMA_BOUNCE=0
oe%X_RIP_AMP=0.0
oe%X_RIP_WAV=0.0
oe%X_PHASE=0.0
oe%Y_RIP_AMP=0.0
oe%Y_RIP_WAV=0.0
oe%Y_PHASE=0.0
oe%N_RIP=0
oe%ROUGH_X=0.0
oe%ROUGH_Y=0.0
oe%OE_NUMBER=0
oe%IDUMMY=0
oe%DUMMY=0.0
FORALL(i=1:10) oe%CX_SLIT(i)=0.0
FORALL(i=1:10) oe%CZ_SLIT(i)=0.0
FORALL(i=1:10) oe%D_PLATE(i)=0.0
FORALL(i=1:10) oe%FILE_ABS(i)=""
FORALL(i=1:10) oe%FILE_SCR_EXT(i)=""
FORALL(i=1:10) oe%I_ABS(i)=0
FORALL(i=1:10) oe%I_SCREEN(i)=0
FORALL(i=1:10) oe%I_SLIT(i)=0
FORALL(i=1:10) oe%I_STOP(i)=0
FORALL(i=1:10) oe%K_SLIT(i)=0
FORALL(i=1:10) oe%RX_SLIT(i)=0.0
FORALL(i=1:10) oe%RZ_SLIT(i)=0.0
FORALL(i=1:10) oe%SCR_NUMBER(i)=0
FORALL(i=1:10) oe%SL_DIS(i)=0.0
FORALL(i=1:10) oe%THICK(i)=0.0
FORALL(i=1:10) oe%CCC(i)=0.0

  end subroutine PoolOEDefault


!
!
!
    
End Module shadow_variables

