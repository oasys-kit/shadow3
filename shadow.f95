!----
!---- MODULE:  shadow
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

Module shadow
    !---- Use Modules ----!

    use stringio
    use gfile
    use beamio
    use math
    use math_imsl

    !---- Variables ----!
    implicit none

    !
    ! HERE THE DECLARATION OF THE VARIABLES POOL
    !
    ! WHAT YOU PUT HERE, IT IS SEEN BY ALL THE ROUTINES HERE AND 
    ! THOSE PROGRAMS STATING "use shadow"
    !
    ! THE OLD COMMON BLOCK IS WRITTEN HERE (COMMENTED) FOR MEMORANDUM

    ! this is to store (permanently) the input variables in start.00 
    type(GfType)     :: gStart00

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
    real(kind=kind(1.0d0)), parameter :: pi     = 3.141592653589793238462643
    real(kind=kind(1.0d0)), parameter :: twopi  = 6.283185307179586467925287
    real(kind=kind(1.0d0)), parameter :: pihalf = 1.570796326794896619231322
    real(kind=kind(1.0d0)), parameter :: todeg  =57.295779513082320876798155
    real(kind=kind(1.0d0)), parameter :: torad =  0.017453292519943295769237

    real(kind=kind(1.0d0)),dimension(3), parameter :: origin = (/0.0,0.0,0.0/)
    real(kind=kind(1.0d0)),dimension(3), parameter :: x_vrs = (/1.0,0.0,0.0/)
    real(kind=kind(1.0d0)),dimension(3), parameter :: y_vrs = (/0.0,1.0,0.0/)
    real(kind=kind(1.0d0)),dimension(3), parameter :: z_vrs = (/0.0,0.0,1.0/)
    
    ! TODO: Change these values with new codata values (see NIST)
    real(kind=kind(1.0d0)), parameter :: tocm =   1.239852D-4
    real(kind=kind(1.0d0)), parameter :: toangs = 1.239852D+4 

    ! now the declaration of the variables in start.00
    !
    ! NOTE: FOR ADDING A NEW VARIABLE, IT SHOULD BE ADDED IN 3 PLACES
    !     1) here (declaration)
    !     2) in get_variables ( get variables into the variables pool)
    !     3) in put_variables ( copy variables from pool to gFile type, 
    !        needed for writing end.xx)

    integer(kind=4)                                         :: FDISTR
    integer(kind=4)                                         :: FGRID
    integer(kind=4)                                         :: FSOUR
    integer(kind=4)                                         :: FSOURCE_DEPTH
    integer(kind=4)                                         :: F_COHER
    integer(kind=4)                                         :: F_COLOR
    integer(kind=4)                                         :: F_PHOT
    integer(kind=4)                                         :: F_POL
    integer(kind=4)                                         :: F_POLAR
    integer(kind=4)                                         :: F_OPD
    integer(kind=4)                                         :: F_WIGGLER
    integer(kind=4)                                         :: F_BOUND_SOUR
    integer(kind=4)                                         :: F_SR_TYPE
    integer(kind=4)                                         :: ISTAR1
    integer(kind=4)                                         :: NPOINT
    integer(kind=4)                                         :: NCOL
    integer(kind=4)                                         :: N_CIRCLE
    integer(kind=4)                                         :: N_COLOR
    integer(kind=4)                                         :: N_CONE
    integer(kind=4)                                         :: IDO_VX
    integer(kind=4)                                         :: IDO_VZ
    integer(kind=4)                                         :: IDO_X_S
    integer(kind=4)                                         :: IDO_Y_S
    integer(kind=4)                                         :: IDO_Z_S
    integer(kind=4)                                         :: IDO_XL
    integer(kind=4)                                         :: IDO_XN
    integer(kind=4)                                         :: IDO_ZL
    integer(kind=4)                                         :: IDO_ZN
    real(kind=kind(1.0d0))                          :: SIGXL1
    real(kind=kind(1.0d0))                          :: SIGXL2
    real(kind=kind(1.0d0))                          :: SIGXL3
    real(kind=kind(1.0d0))                          :: SIGXL4
    real(kind=kind(1.0d0))                          :: SIGXL5
    real(kind=kind(1.0d0))                          :: SIGXL6
    real(kind=kind(1.0d0))                          :: SIGXL7
    real(kind=kind(1.0d0))                          :: SIGXL8
    real(kind=kind(1.0d0))                          :: SIGXL9
    real(kind=kind(1.0d0))                          :: SIGXL10
    real(kind=kind(1.0d0))                          :: SIGZL1
    real(kind=kind(1.0d0))                          :: SIGZL2
    real(kind=kind(1.0d0))                          :: SIGZL3
    real(kind=kind(1.0d0))                          :: SIGZL4
    real(kind=kind(1.0d0))                          :: SIGZL5
    real(kind=kind(1.0d0))                          :: SIGZL6
    real(kind=kind(1.0d0))                          :: SIGZL7
    real(kind=kind(1.0d0))                          :: SIGZL8
    real(kind=kind(1.0d0))                          :: SIGZL9
    real(kind=kind(1.0d0))                          :: SIGZL10
    real(kind=kind(1.0d0))                          :: CONV_FACT
    real(kind=kind(1.0d0))                          :: CONE_MAX
    real(kind=kind(1.0d0))                          :: CONE_MIN
    real(kind=kind(1.0d0))                          :: EPSI_DX
    real(kind=kind(1.0d0))                          :: EPSI_DZ
    real(kind=kind(1.0d0))                          :: EPSI_X
    real(kind=kind(1.0d0))                          :: EPSI_Z
    real(kind=kind(1.0d0))                          :: HDIV1
    real(kind=kind(1.0d0))                          :: HDIV2
    real(kind=kind(1.0d0))                          :: PH1
    real(kind=kind(1.0d0))                          :: PH2
    real(kind=kind(1.0d0))                          :: PH3
    real(kind=kind(1.0d0))                          :: PH4
    real(kind=kind(1.0d0))                          :: PH5
    real(kind=kind(1.0d0))                          :: PH6
    real(kind=kind(1.0d0))                          :: PH7
    real(kind=kind(1.0d0))                          :: PH8
    real(kind=kind(1.0d0))                          :: PH9
    real(kind=kind(1.0d0))                          :: PH10
    real(kind=kind(1.0d0))                          :: RL1
    real(kind=kind(1.0d0))                          :: RL2
    real(kind=kind(1.0d0))                          :: RL3
    real(kind=kind(1.0d0))                          :: RL4
    real(kind=kind(1.0d0))                          :: RL5
    real(kind=kind(1.0d0))                          :: RL6
    real(kind=kind(1.0d0))                          :: RL7
    real(kind=kind(1.0d0))                          :: RL8
    real(kind=kind(1.0d0))                          :: RL9
    real(kind=kind(1.0d0))                          :: RL10
    real(kind=kind(1.0d0))                          :: BENER
    real(kind=kind(1.0d0))                          :: POL_ANGLE
    real(kind=kind(1.0d0))                          :: POL_DEG
    real(kind=kind(1.0d0))                          :: R_ALADDIN
    real(kind=kind(1.0d0))                          :: R_MAGNET
    real(kind=kind(1.0d0))                          :: SIGDIX
    real(kind=kind(1.0d0))                          :: SIGDIZ
    real(kind=kind(1.0d0))                          :: SIGMAX
    real(kind=kind(1.0d0))                          :: SIGMAY
    real(kind=kind(1.0d0))                          :: SIGMAZ
    real(kind=kind(1.0d0))                          :: VDIV1
    real(kind=kind(1.0d0))                          :: VDIV2
    real(kind=kind(1.0d0))                          :: WXSOU
    real(kind=kind(1.0d0))                          :: WYSOU
    real(kind=kind(1.0d0))                          :: WZSOU
    real(kind=kind(1.0d0))                          :: PLASMA_ANGLE
    character(len=512)                              :: FILE_TRAJ
    character(len=512)                              :: FILE_SOURCE
    character(len=512)                              :: FILE_BOUND
    integer(kind=4)                                 :: OE_NUMBER
    integer(kind=4)                                 :: IDUMMY
    real(kind=kind(1.0d0))                          :: DUMMY
    integer(kind=4)                                         :: F_NEW

    ! now the variables in start.0x
    ! (commented by now...)

    ! integer, parameter  :: aDim=10

    !     integer(kind=4)                                         :: FMIRR
    !     integer(kind=4)                                         :: F_TORUS
    !     integer(kind=4)                                         :: FCYL
    !     integer(kind=4)                                         :: F_EXT
    !     integer(kind=4)                                         :: FSTAT
    !     integer(kind=4)                                         :: F_SCREEN
    !     integer(kind=4)                                         :: F_PLATE
    !     integer(kind=4)                                         :: FSLIT
    !     integer(kind=4)                                         :: FWRITE
    !     integer(kind=4)                                         :: F_RIPPLE
    !     integer(kind=4)                                         :: F_MOVE
    !     integer(kind=4)                                         :: F_THICK
    !     integer(kind=4)                                         :: F_BRAGG_A
    !     integer(kind=4)                                         :: F_G_S
    !     integer(kind=4)                                         :: F_R_RAN
    !     integer(kind=4)                                         :: F_GRATING
    !     integer(kind=4)                                         :: F_MOSAIC
    !     integer(kind=4)                                         :: F_JOHANSSON
    !     integer(kind=4)                                         :: F_SIDE
    !     integer(kind=4)                                         :: F_CENTRAL
    !     integer(kind=4)                                         :: F_CONVEX
    !     integer(kind=4)                                         :: F_REFLEC
    !     integer(kind=4)                                         :: F_RUL_ABS
    !     integer(kind=4)                                         :: F_RULING
    !     integer(kind=4)                                         :: F_PW
    !     integer(kind=4)                                         :: F_PW_C
    !     integer(kind=4)                                         :: F_VIRTUAL
    !     integer(kind=4)                                         :: FSHAPE
    !     integer(kind=4)                                         :: FHIT_C
    !     integer(kind=4)                                         :: F_MONO
    !     integer(kind=4)                                         :: F_REFRAC
    !     integer(kind=4)                                         :: F_DEFAULT
    !     integer(kind=4)                                         :: F_REFL
    !     integer(kind=4)                                         :: F_HUNT
    !     integer(kind=4)                                         :: F_CRYSTAL
    !     integer(kind=4)                                         :: F_PHOT_CENT
    !     integer(kind=4)                                         :: F_ROUGHNESS
    !     integer(kind=4)                                         :: F_ANGLE
    !     integer(kind=4)                                         :: NPOINTOE
    !     !!    integer(kind=4)                                         :: NCOL
    !     integer(kind=4)                                         :: N_SCREEN
    !     !!    integer(kind=4)                                         :: ISTAR1
    !     real(kind=kind(1.0d0))                          :: CIL_ANG
    !     real(kind=kind(1.0d0))                          :: ELL_THE
    !     integer(kind=4)                                         :: N_PLATES
    !     integer(kind=4)                                         :: IG_SEED
    !     integer(kind=4)                                         :: MOSAIC_SEED
    !     real(kind=kind(1.0d0))                          :: ALPHA
    !     real(kind=kind(1.0d0))                          :: SSOUR
    !     real(kind=kind(1.0d0))                          :: THETA
    !     real(kind=kind(1.0d0))                          :: SIMAG
    !     real(kind=kind(1.0d0))                          :: RDSOUR
    !     real(kind=kind(1.0d0))                          :: RTHETA
    !     real(kind=kind(1.0d0))                          :: OFF_SOUX
    !     real(kind=kind(1.0d0))                          :: OFF_SOUY
    !     real(kind=kind(1.0d0))                          :: OFF_SOUZ
    !     real(kind=kind(1.0d0))                          :: ALPHA_S
    !     real(kind=kind(1.0d0))                          :: RLEN1
    !     real(kind=kind(1.0d0))                          :: RLEN2
    !     real(kind=kind(1.0d0))                          :: RMIRR
    !     real(kind=kind(1.0d0))                          :: AXMAJ
    !     real(kind=kind(1.0d0))                          :: AXMIN
    !     real(kind=kind(1.0d0))                          :: CONE_A
    !     real(kind=kind(1.0d0))                          :: R_MAJ
    !     real(kind=kind(1.0d0))                          :: R_MIN
    !     real(kind=kind(1.0d0))                          :: RWIDX1
    !     real(kind=kind(1.0d0))                          :: RWIDX2
    !     real(kind=kind(1.0d0))                          :: PARAM
    !     real(kind=kind(1.0d0))                          :: HUNT_H
    !     real(kind=kind(1.0d0))                          :: HUNT_L
    !     real(kind=kind(1.0d0))                          :: BLAZE
    !     real(kind=kind(1.0d0))                          :: RULING
    !     real(kind=kind(1.0d0))                          :: ORDER
    !     real(kind=kind(1.0d0))                          :: PHOT_CENT
    !     real(kind=kind(1.0d0))                          :: X_ROT
    !     real(kind=kind(1.0d0))                          :: D_SPACING
    !     real(kind=kind(1.0d0))                          :: A_BRAGG
    !     real(kind=kind(1.0d0))                          :: SPREAD_MOS
    !     real(kind=kind(1.0d0))                          :: THICKNESS
    !     real(kind=kind(1.0d0))                          :: R_JOHANSSON
    !     real(kind=kind(1.0d0))                          :: Y_ROT
    !     real(kind=kind(1.0d0))                          :: Z_ROT
    !     real(kind=kind(1.0d0))                          :: OFFX
    !     real(kind=kind(1.0d0))                          :: OFFY
    !     real(kind=kind(1.0d0))                          :: OFFZ
    !     real(kind=kind(1.0d0))                          :: SLLEN
    !     real(kind=kind(1.0d0))                          :: SLWID
    !     real(kind=kind(1.0d0))                          :: SLTILT
    !     real(kind=kind(1.0d0))                          :: COD_LEN
    !     real(kind=kind(1.0d0))                          :: COD_WID
    !     real(kind=kind(1.0d0))                          :: X_SOUR
    !     real(kind=kind(1.0d0))                          :: Y_SOUR
    !     real(kind=kind(1.0d0))                          :: Z_SOUR
    !     real(kind=kind(1.0d0))                          :: X_SOUR_ROT
    !     real(kind=kind(1.0d0))                          :: Y_SOUR_ROT
    !     real(kind=kind(1.0d0))                          :: Z_SOUR_ROT
    !     real(kind=kind(1.0d0))                          :: R_LAMBDA
    !     real(kind=kind(1.0d0))                          :: THETA_I
    !     real(kind=kind(1.0d0))                          :: ALPHA_I
    !     real(kind=kind(1.0d0))                          :: T_INCIDENCE
    !     real(kind=kind(1.0d0))                          :: T_SOURCE
    !     real(kind=kind(1.0d0))                          :: T_IMAGE
    !     real(kind=kind(1.0d0))                          :: T_REFLECTION
    !     !    character(len=512)                              :: FILE_SOURCE
    !     character(len=512)                              :: FILE_RIP
    !     character(len=512)                              :: FILE_REFL
    !     character(len=512)                              :: FILE_MIR
    !     character(len=512)                              :: FILE_ROUGH
    !     integer(kind=4)                                         :: FZP
    !     real(kind=kind(1.0d0))                          :: HOLO_R1
    !     real(kind=kind(1.0d0))                          :: HOLO_R2
    !     real(kind=kind(1.0d0))                          :: HOLO_DEL
    !     real(kind=kind(1.0d0))                          :: HOLO_GAM
    !     real(kind=kind(1.0d0))                          :: HOLO_W
    !     real(kind=kind(1.0d0))                          :: HOLO_RT1
    !     real(kind=kind(1.0d0))                          :: HOLO_RT2
    !     real(kind=kind(1.0d0))                          :: AZIM_FAN
    !     real(kind=kind(1.0d0))                          :: DIST_FAN
    !     real(kind=kind(1.0d0))                          :: COMA_FAC
    !     real(kind=kind(1.0d0))                          :: ALFA
    !     real(kind=kind(1.0d0))                          :: GAMMA
    !     real(kind=kind(1.0d0))                          :: R_IND_OBJ
    !     real(kind=kind(1.0d0))                          :: R_IND_IMA
    !     real(kind=kind(1.0d0))                          :: RUL_A1
    !     real(kind=kind(1.0d0))                          :: RUL_A2
    !     real(kind=kind(1.0d0))                          :: RUL_A3
    !     real(kind=kind(1.0d0))                          :: RUL_A4
    !     integer(kind=4)                                 :: F_POLSEL
    !     integer(kind=4)                                 :: F_FACET
    !     integer(kind=4)                                 :: F_FAC_ORIENT
    !     integer(kind=4)                                 :: F_FAC_LATT
    !     real(kind=kind(1.0d0))                          :: RFAC_LENX
    !     real(kind=kind(1.0d0))                          :: RFAC_LENY
    !     real(kind=kind(1.0d0))                          :: RFAC_PHAX
    !     real(kind=kind(1.0d0))                          :: RFAC_PHAY
    !     real(kind=kind(1.0d0))                          :: RFAC_DELX1
    !     real(kind=kind(1.0d0))                          :: RFAC_DELX2
    !     real(kind=kind(1.0d0))                          :: RFAC_DELY1
    !     real(kind=kind(1.0d0))                          :: RFAC_DELY2
    !     character(len=512)                              :: FILE_FAC
    !     integer(kind=4)                                 :: F_SEGMENT
    !     integer(kind=4)                                 :: ISEG_XNUM
    !     integer(kind=4)                                 :: ISEG_YNUM
    !     character(len=512)                              :: FILE_SEGMENT
    !     character(len=512)                              :: FILE_SEGP
    !     real(kind=kind(1.0d0))                          :: SEG_LENX
    !     real(kind=kind(1.0d0))                          :: SEG_LENY
    !     integer(kind=4)                                 :: F_KOMA
    !     character(len=512)                              :: FILE_KOMA
    !     integer(kind=4)                                 :: F_EXIT_SHAPE
    !     integer(kind=4)                                 :: F_INC_MNOR_ANG
    !     real(kind=kind(1.0d0))                          :: ZKO_LENGTH
    !     real(kind=kind(1.0d0))                          :: RKOMA_CX
    !     real(kind=kind(1.0d0))                          :: RKOMA_CY
    !     integer(kind=4)                                 :: F_KOMA_CA
    !     character(len=512)                              :: FILE_KOMA_CA
    !     integer(kind=4)                                 :: F_KOMA_BOUNCE
    !     real(kind=kind(1.0d0))                          :: X_RIP_AMP
    !     real(kind=kind(1.0d0))                          :: X_RIP_WAV
    !     real(kind=kind(1.0d0))                          :: X_PHASE
    !     real(kind=kind(1.0d0))                          :: Y_RIP_AMP
    !     real(kind=kind(1.0d0))                          :: Y_RIP_WAV
    !     real(kind=kind(1.0d0))                          :: Y_PHASE
    !     integer(kind=4)                                 :: N_RIP
    !     real(kind=kind(1.0d0))                          :: ROUGH_X
    !     real(kind=kind(1.0d0))                          :: ROUGH_Y
    !     !    integer(kind=4)                            :: OE_NUMBER
    !     !    integer(kind=4)                            :: IDUMMY
    !     !    real(kind=kind(1.0d0))                     :: DUMMY
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: CX_SLIT 
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: CZ_SLIT 
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: D_PLATE 
    !     character(len=512), dimension(aDim)             :: FILE_ABS 
    !     character(len=512), dimension(aDim)             :: FILE_SCR_EXT 
    !     integer(kind=4), dimension(aDim)                :: I_ABS 
    !     integer(kind=4), dimension(aDim)                :: I_SCREEN 
    !     integer(kind=4), dimension(aDim)                :: I_SLIT 
    !     integer(kind=4), dimension(aDim)                :: I_STOP 
    !     integer(kind=4), dimension(aDim)                :: K_SLIT 
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: RX_SLIT 
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: RZ_SLIT 
    !     integer(kind=4), dimension(aDim)                :: SCR_NUMBER 
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: SL_DIS 
    !     real(kind=kind(1.0d0)), dimension(aDim)         :: THICK 

    !  now the variables in the ex-common blocks not defined in the gfiles
    !  They will be added as far as needed...

    !!--	INTEGER*4 FMIRR,F_TORUS,FCYL,FANG,FSTAT,F_COHER, &
    !!--     		FANG1,FSLIT,FGRID,F_NEW,FSOURCE_DEPTH, &
    !!--     		FSOUR,FDISTR,FWRITE,F_BRAGG_A, &
    !!--     		F_POL,F_POLAR,F_RIPPLE, &
    !!--     		F_MOVE,F_HOW,F_G_S,F_READ_RIP,F_R_RAN, &
    !!--     		F_GRATING,F_SIDE,F_CENTRAL,F_VIRTUAL, &
    !!--     		F_COLOR,F_CONVEX,F_REFLEC,F_RULING, &
    !!--     		F_RUL_ABS,F_BOUND_SOUR,F_THICK, &
    !!--     		F_EXT,F_PHOT,F_SCREEN,F_PLATE,FSHAPE, &
    !!--     		FHIT_C,F_PW,F_MONO,F_DEFAULT,F_REFL, &
    !!--     		F_HUNT,F_PHOT_CENT,F_CRYSTAL,F_REFRAC, &
    !!--     		F_PW_C,F_OPD,F_WIGGLER,FZP,F_SR_TYPE, &
    !!--     		F_ROUGHNESS,FDUMMY,F_ANGLE
    !!--
    !!--
    !!--	INTEGER*4 NPOINT,NCOL,ISTAR1,IDO_X_S,IDO_Z_S, &
    !!--     		IDO_Y_S,IDO_VZ,IDO_VX,IDO_XL,IDO_XN, &
    !!--     		IDO_ZL,IDO_ZN,N_PLATES,IG_SEED,N_COLOR, &
    !!--     		N_CIRCLE,N_CONE,N_SCREEN,I_SCREEN, &
    !!--     		I_SLIT,K_SLIT,I_STOP, &
    !!--     		I_ABS,MPURGE,NDEG 
    !!--
    !!--	INTEGER*4 F_POLSEL,F_FACET,IFAC_X,IFAC_Y, &
    !!--     		F_FAC_ORIENT,F_FAC_LATT, &
    !!--     		F_KOMA,KOXX,I_KOMA,F_DOT,F_KOMA_BOUNCE, &
    !!--     		F_EXIT_SHAPE,F_KOMA_CA,F_INC_MNOR_ANG, &
    !!--     		ISEG_XNUM,ISEG_YNUM,F_SEGMENT
    !!--!!
    !!--!! added srio@esrf.eu 2008-10-02 to include some variables 
    !!--!! found in gfiles but not found (!!) in common blocks...
    !!--!!
    !!--!! Warning: They are declared here, BUT NOT STORED IN COMMONS 
    !!--!!    (I found problems when trying to do so...) !!!
    !!--!!
    !!--	INTEGER(kind=4) :: OE_NUMBER,IDUMMY
    !!--	REAL(kind=8)    :: DUMMY
    !!--	INTEGER(kind=4),dimension(10) ::  SCR_NUMBER
    !!--
    !!--
    !!--       	REAL*8	ALPHA,SSOUR,THETA,PSOUR,SIMAG, &
    !!--     		RDSOUR,RTHETA,PSREAL,TIMDIS, &
    !!--     		DELTA,RDELTA,OFF_SOUX,OFF_SOUY,OFF_SOUZ, &
    !!--     		ALPHA_S 
    !!--	
    !!--	REAL*8	RLEN,RLEN1,RLEN2,CCC,RMIRR,CONE_A,  &
    !!--     		AXMAJ,AXMIN,AFOCI,ECCENT,R_MAJ,R_MIN,  &
    !!--     		RWIDX,RWIDX1,RWIDX2,PARAM,  &
    !!--     		PCOEFF,CIL_ANG,ELL_THE 
    !!--	REAL*8	RULING,ORDER,BETA,PHOT_CENT,R_LAMBDA,  &
    !!--     		HUNT_L,HUNT_H,BLAZE,D_SPACING,AZIM_FAN,  &
    !!--     		DIST_FAN,COMA_FAC,RUL_A1,RUL_A2,RUL_A3, &
    !!--     		RUL_A4,A_BRAGG
    !!--
    !!--	REAL*8	X_ROT,Y_ROT,Z_ROT,OFFX,OFFY,OFFZ, &
    !!--     		U_MIR,V_MIR,W_MIR
    !!--
    !!--	REAL*8	SLLEN,SLWID,SLTILT,COD_LEN,COD_WID
    !!--
    !!--	REAL*8	WXSOU,WYSOU,WZSOU,SIGMAX,SIGMAY,SIGMAZ, &
    !!--     		HDIV1,HDIV2,VDIV1,VDIV2,SIGDIX,SIGDIZ, &
    !!--     		CONV_FACT,CONE_MAX,CONE_MIN,X_SOUR,Y_SOUR, &
    !!--     		Z_SOUR,X_SOUR_ROT,Y_SOUR_ROT,Z_SOUR_ROT, &
    !!--     		U_SOUR,V_SOUR,W_SOUR,PLASMA_ANGLE
    !!-- 
    !!--	REAL*8	PHOTON,BENER,R_ALADDIN, &
    !!--     		EPSI_X,EPSI_Z,EPSI_DX,EPSI_DZ,R_MAGNET
    !!--
    !!--	REAL*8 	COSDEL,SINDEL,COSTHE,SINTHE,COSTHR, &
    !!--     		SINTHR,COSDER,SINDER,COSAL,SINAL, &
    !!--     		COSAL_S,SINAL_S,COSTHE_I,SINTHE_I, &
    !!--     		COSAL_I,SINAL_I
    !!--
    !!--	REAL*8	RIMCEN,VNIMAG,UXIM,VZIM, &
    !!--     		D_PLATE,C_STAR,C_PLATE, &
    !!--     		UX_PL,VZ_PL,WY_PL, &
    !!--     		THETA_I,ALPHA_I
    !!--
    !!--	REAL*8	CENTRAL,T_INCIDENCE,T_SOURCE, &
    !!--     		T_IMAGE,T_REFLECTION
    !!--
    !!--	REAL*8 	X_RIP_AMP,X_RIP_WAV,X_PHASE, &
    !!--     		Y_RIP_AMP,Y_RIP_WAV,Y_PHASE, &
    !!--     		ROUGH_X, ROUGH_Y
    !!--
    !!--	REAL*8	RFAC_LENX,RFAC_LENY,RFAC_PHAX,RFAC_PHAY, &
    !!--     		RFAC_DELX1,RFAC_DELX2,RFAC_DELY1, &
    !!--     		RFAC_DELY2
    !!--
    !!--	REAL*8	ZKO_LENGTH,RKOMA_A,RKOMA_B, &
    !!--     		RKOMA_CX,RKOMA_CY, &
    !!--     		SEG_LENX,SEG_LENY
    !!--
    !!--	REAL*8 	AMPLI,X_GR,Y_GR,SIGNUM, &
    !!--     		SIG_XMIN,SIG_XMAX,SIG_X, &
    !!--     		SIG_YMIN,SIG_YMAX,SIG_Y, &
    !!--     		AMPL_IN
    !!--
    !!--	INTEGER*4 N_RIP
    !!--
    !!--	REAL*8 	ALFA,GAMMA,POL_ANGLE,POL_DEG, &
    !!--     		R_IND_OBJ,R_IND_IMA, &
    !!--     		PH1,PH2,PH3,PH4,PH5,PH6,PH7,PH8,PH9,PH10, &
    !!--     		RL1,RL2,RL3,RL4,RL5,RL6,RL7,RL8,RL9,RL10, &
    !!--     		SIGXL1,SIGXL2,SIGXL3,SIGXL4,SIGXL5, &
    !!--     		SIGXL6,SIGXL7,SIGXL8,SIGXL9,SIGXL10, &
    !!--     		SIGZL1,SIGZL2,SIGZL3,SIGZL4,SIGZL5, &
    !!--     		SIGZL6,SIGZL7,SIGZL8,SIGZL9,SIGZL10
    !!--
    !!--	REAL*8	RX_SLIT,RZ_SLIT,CX_SLIT,CZ_SLIT,SL_DIS, &
    !!--     		UX_SCR,WY_SCR,VZ_SCR, &
    !!--     		THICK
    !!--
    !!--	REAL*8 	HOLO_R1,HOLO_R2,HOLO_DEL,HOLO_GAM, &
    !!--     		HOLO_W,HOLO1,HOLO2,HOLO_RT1, &
    !!--     		HOLO_RT2
    !!--
    !!--! C+++
    !!--! C
    !!--! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    !!--! C
    !!--! C This is the COMMON blocks set which insure communication between
    !!--! C the program subroutines; it also contain initial parameters
    !!--! C
    !!--! C
    !!--! C---
    !!--     	CHARACTER *80	FILE_RIP, FILE_REFL, FILE_ABS  , FILE_MIR
    !!--	CHARACTER *80	FILE_SOURCE, FILE_TRAJ, FILE_BOUND
    !!--	CHARACTER *80	FFILE, FILE_ROUGH, FILE_KOMA, FILE_FAC
    !!--	CHARACTER *80	FILE_SEGMENT,FILE_SEGP,FILE_KOMA_CA
    !!--	CHARACTER *80	FILE_SCR_EXT  
    !!--!!srio     	PARAMETER 	(N_DIM 	= 1000000 + 1)
    !!--     	EXTERNAL	IRINT,RNUMBER,IYES
    !!--
    !!--
    !!--     	COMMON	/MATHBLK/	PI,TWOPI,PIHALF,TODEG,TORAD,ORIGIN(3), &
    !!--     				X_VRS(3),Y_VRS(3),Z_VRS(3),TOCM,TOANGS
    !!--     	COMMON	/FLAGS	/	FMIRR,F_TORUS,FCYL,FANG,FSTAT,F_COHER, &
    !!--     				FANG1,FSLIT,FGRID,F_NEW,FSOURCE_DEPTH, &
    !!--     				FSOUR,FDISTR,FWRITE,F_BRAGG_A, &
    !!--     				F_JOHANSSON, &
    !!--     				F_POL,F_POLAR,F_RIPPLE,F_MOSAIC, &
    !!--     				F_MOVE,F_HOW,F_G_S,F_READ_RIP,F_R_RAN, &
    !!--     				F_GRATING,F_SIDE,F_CENTRAL,F_VIRTUAL, &
    !!--     				F_COLOR,F_CONVEX,F_REFLEC,F_RULING, &
    !!--     				F_RUL_ABS,F_BOUND_SOUR,F_THICK, &
    !!--     				F_EXT,F_PHOT,F_SCREEN,F_PLATE,FSHAPE, &
    !!--     				FHIT_C,F_PW,F_MONO,F_DEFAULT,F_REFL, &
    !!--     				F_HUNT,F_PHOT_CENT,F_CRYSTAL,F_REFRAC, &
    !!--     				F_PW_C,F_OPD,F_WIGGLER,FZP,F_SR_TYPE, &
    !!--     				F_ROUGHNESS,FDUMMY,F_ANGLE,F_SEGMENT
    !!--     	COMMON	/CALC	/	NPOINT,NCOL,ISTAR1,IDO_X_S,IDO_Z_S, &
    !!--     				IDO_Y_S,IDO_VZ,IDO_VX,IDO_XL,IDO_XN, &
    !!--     				IDO_ZL,IDO_ZN,N_PLATES,IG_SEED,N_COLOR, &
    !!--     				N_CIRCLE,N_CONE,N_SCREEN,I_SCREEN(10), &
    !!--     				I_SLIT(10),K_SLIT(10),I_STOP(10), &
    !!--     				I_ABS(10),MPURGE(2),NDEG,MOSAIC_SEED
    !!--     	COMMON	/SYSTEM1/	ALPHA,SSOUR,THETA,PSOUR(3),SIMAG, &
    !!--     				RDSOUR,RTHETA,PSREAL(3),TIMDIS, &
    !!--     				DELTA,RDELTA,OFF_SOUX,OFF_SOUY,OFF_SOUZ, &
    !!--     				ALPHA_S
    !!--     	COMMON	/MIRROR	/	RLEN,RLEN1,RLEN2,CCC(10),RMIRR,CONE_A, &
    !!--     				AXMAJ,AXMIN,AFOCI,ECCENT,R_MAJ,R_MIN, &
    !!--     				RWIDX,RWIDX1,RWIDX2,PARAM, &
    !!--     				PCOEFF(0:4,0:4,0:4),CIL_ANG,ELL_THE
    !!--     	COMMON	/GRATING/	RULING,ORDER,BETA,PHOT_CENT,R_LAMBDA, &
    !!--     				HUNT_L,HUNT_H,BLAZE,D_SPACING,AZIM_FAN, &
    !!--     				DIST_FAN,COMA_FAC,RUL_A1,RUL_A2,RUL_A3, &
    !!--     				RUL_A4,A_BRAGG
    !!--        COMMON  /XTAL   /       SPREAD_MOS, R_JOHANSSON, THICKNESS
    !!--     	COMMON	/ROT_MIR/	X_ROT,Y_ROT,Z_ROT,OFFX,OFFY,OFFZ, &
    !!--     				U_MIR(3),V_MIR(3),W_MIR(3)
    !!--     	COMMON	/SLIT	/	SLLEN,SLWID,SLTILT,COD_LEN,COD_WID
    !!--! C
    !!--! C The SOURCE and ALADDIN are also in ../source/bm/bm.blk. KEEP
    !!--! C THESE IN SYNC.
    !!--! C
    !!--     	COMMON	/SOURCE	/	WXSOU,WYSOU,WZSOU,SIGMAX,SIGMAY,SIGMAZ, &
    !!--     				HDIV1,HDIV2,VDIV1,VDIV2,SIGDIX,SIGDIZ, &
    !!--     				CONV_FACT,CONE_MAX,CONE_MIN,X_SOUR, &
    !!--     				Y_SOUR, &
    !!--     				Z_SOUR,X_SOUR_ROT,Y_SOUR_ROT,Z_SOUR_ROT, &
    !!--     				U_SOUR(3),V_SOUR(3),W_SOUR(3), &
    !!--     				PLASMA_ANGLE
    !!--     	COMMON	/ALADDIN/	PHOTON(10),BENER,R_ALADDIN, &
    real(kind=kind(1.0d0)),dimension(10)   :: photon
    !!--     				EPSI_X,EPSI_Z,EPSI_DX,EPSI_DZ,R_MAGNET
    !!--     	COMMON	/TRIG	/	COSDEL,SINDEL,COSTHE,SINTHE,COSTHR, &
    !!--     				SINTHR,COSDER,SINDER,COSAL,SINAL, &
    !!--     				COSAL_S,SINAL_S,COSTHE_I,SINTHE_I, &
    !!--     				COSAL_I,SINAL_I
    !!--     	COMMON	/IMAGE	/	RIMCEN(3),VNIMAG(3),UXIM(3),VZIM(3), &
    !!--     				D_PLATE(5),C_STAR(3),C_PLATE(3), &
    !!--     				UX_PL(3),VZ_PL(3),WY_PL(3), &
    !!--     				THETA_I,ALPHA_I
    !!--     	COMMON	/AXIS	/	CENTRAL(20,24),T_INCIDENCE,T_SOURCE, &
    !!--     				T_IMAGE,T_REFLECTION
    !!--     	COMMON	/RIPPLE	/	X_RIP_AMP,X_RIP_WAV,X_PHASE, &
    !!--     				Y_RIP_AMP,Y_RIP_WAV,Y_PHASE, &
    !!--     				ROUGH_X, ROUGH_Y
    !!--	COMMON  /SEGMENT/	ISEG_XNUM,ISEG_YNUM, &
    !!--     				SEG_LENX,SEG_LENY
    !!--	COMMON	/FACET	/	F_POLSEL,F_FACET,IFAC_X,IFAC_Y, &
    !!--     				F_FAC_ORIENT,F_FAC_LATT,RFAC_LENX, &
    !!--     				RFAC_LENY,RFAC_PHAX,RFAC_PHAY, &
    !!--     				RFAC_DELX1,RFAC_DELX2,RFAC_DELY1, &
    !!--     				RFAC_DELY2
    !!--	COMMON	/KOMA	/	F_KOMA,KOXX,I_KOMA,F_DOT,ZKO_LENGTH, &
    !!--     				RKOMA_CX,RKOMA_CY,F_KOMA_CA, &
    !!--     				F_KOMA_BOUNCE,F_EXIT_SHAPE, &
    !!--     				F_INC_MNOR_ANG
    !!--     	COMMON	/NAMES	/	FILE_SOURCE,FILE_TRAJ, &
    !!--     				FILE_RIP,FILE_REFL, &
    !!--     				FILE_SEGMENT,FILE_SEGP, &
    !!--     				FILE_ABS  (10),FILE_SCR_EXT  (10), &
    !!--     				FILE_MIR,FILE_BOUND, &
    !!--     				FILE_ROUGH,FILE_FAC,FILE_KOMA, &
    !!--     				FILE_KOMA_CA
    !!--     	COMMON	/RIPP_2 /	AMPLI(10),X_GR(10),Y_GR(10),SIGNUM(10), &
    !!--     				SIG_XMIN(10),SIG_XMAX(10),SIG_X(10), &
    !!--     				SIG_YMIN(10),SIG_YMAX(10),SIG_Y(10), &
    !!--     				AMPL_IN(10),N_RIP
    !!--     	COMMON	/LIGHT	/	ALFA,GAMMA,POL_ANGLE,POL_DEG, &
    !!--     				R_IND_OBJ,R_IND_IMA, &
    !!--     				PH1,PH2,PH3,PH4,PH5,PH6,PH7,PH8,PH9, &
    !!--     				PH10, &
    !!--     				RL1,RL2,RL3,RL4,RL5,RL6,RL7,RL8,RL9, &
    !!--     				RL10, &
    !!--     				SIGXL1,SIGXL2,SIGXL3,SIGXL4,SIGXL5, &
    !!--     				SIGXL6,SIGXL7,SIGXL8,SIGXL9,SIGXL10, &
    !!--     				SIGZL1,SIGZL2,SIGZL3,SIGZL4,SIGZL5, &
    !!--     				SIGZL6,SIGZL7,SIGZL8,SIGZL9,SIGZL10
    !!--	COMMON	/SCREENS/	RX_SLIT(10),RZ_SLIT(10),CX_SLIT(10), &
    !!--     				CZ_SLIT(10),SL_DIS(10), &
    !!--     				UX_SCR(3,2),WY_SCR(3,2),VZ_SCR(3,2), &
    !!--     				THICK(10)
    !!--     	COMMON	/HOLO	/	HOLO_R1,HOLO_R2,HOLO_DEL,HOLO_GAM, &
    !!--     				HOLO_W,HOLO1(3),HOLO2(3),HOLO_RT1, &
    !!--     				HOLO_RT2
    !!--
    !!--!!       common /srio/ OE_NUMBER,IDUMMY, DUMMY, SCR_NUMBER
    !!--


    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    ! public :: rwname
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public :: rwname, source1


    !---- List of private functions ----!
    !---- List of private subroutines ----!
    private :: get_variables, put_variables, source_bound



    !---- Definitions ----!
    ! this is an example of a type 
    !Type, public :: GfType
    !   character(len=512) :: fileName
    !   ! logical for allocation
    !   logical            :: alloc1
    !   integer(kind=4)            :: nLines
    !   integer(kind=4)            :: nVariables
    !   character(len=512), dimension(:), allocatable :: fileLines
    !   character(len=512), dimension(:), allocatable :: variableNames
    !   character(len=512), dimension(:), allocatable :: variableValues
    !End Type GfType


    !---- Interfaces ----!
    ! this is an example as used in gfile
    !Interface  GfGetValue
    !   Module Procedure GfGetValueString
    !   Module Procedure GfGetValueInteger
    !   Module Procedure GfGetValueReal
    !End Interface


  Contains
    !
    !---- Public Routines ----!
    !

    !C+++
    !C	SUBROUTINE	RWNAME ( File, Flag, iErr )
    !C
    !C	PURPOSE		To read or write the optical element parameters
    !C			to the disk.
    !C
    !C	ALGORITHM	None.
    !C
    !C	ARGUMENTS	NAME   file-name
    !C
    !C			FLAG   on input, 'R_SOUR' read source
    !C			                 'W_SOUR' write source
    !C					 'R_OE'   read oe
    !C					 'W_OE'   write oe
    !C
    !C			IERR  on output, 0 = successful completion
    !C				         -1 = namelist error
    !C				      	 -2 = file-not-found
    !C					 -3 = error in flag
    !C---
     	SUBROUTINE RWNAME (NAME,WHAT,IFLAG)

        implicit none

     	character(len=*),       intent(in)    ::  NAME, WHAT
        integer(kind=4),                intent(out)   ::  iflag

     	logical               ::  esta, iOut
        Type(GfType)          ::  gEnd00

        iflag = 0



        ! c
        ! c Performs I/O
        ! c

        select case (trim(what))

          case('W_SOUR') 
	    !CALL gfsave_source (NAME, IFLAG)
            ! makes a local copy of gStart00 to avoid changing it 
            gEnd00 = gStart00
            ! this copies the values of the variables pool in gEnd00
            call put_variables(gEnd00)

            ! dump gStart00 to file
            iOut = GfFileWrite(gStart00,trim(name))
            if (.not. iOut) then 
              iFlag=-1
              call leave("SHADOW-RWNAME","Error writing file: "//trim(name),iFlag)
            end if

          case('R_SOUR') 
            !
            ! check if file exists
            !
            inquire(file=trim(name),exist=esta)
    
            if ( .not. esta) then
              iflag = -2
              CALL LEAVE ("SHADOW-RWNAME","Error, file does not exist: "//trim(name), IFLAG)
            end if

            ! this loads the variables of the gfile into gStart00 type
            iOut = GfFileLoad(gStart00,trim(name))
            if (.not. iOut) then 
              iFlag=-1
              call leave("SHADOW-RWNAME","Error reading file: "//trim(name),iFlag)
            end if
             
            ! prints to the terminal the list of variables
            ! iOut = GfTypePrint(gStart00)

            ! this copies the values of the individual variables in gStart00
            ! to the variable pool
            call get_variables(gStart00)

          case('W_OE') 
	    !CALL gfsave_oe (NAME, IFLAG)
            print *,"SHADOW-RWNAME: Not yet implemented."
            stop

          case('R_OE') 
	    !CALL gfload_oe (NAME, IFLAG)
            print *,"SHADOW-RWNAME: Not yet implemented."
            stop

          case default 
            print *,"SHADOW-RWNAME: Undefined label: "//trim(what)
            stop
       end select

	RETURN
        !CALL LEAVE ( 'RWNAME', 'Undefined action. Check code.',IFLAG)
    END SUBROUTINE RWNAME

    !
    !
    !

    SUBROUTINE GET_VARIABLES (G1)
    !
    !  SUBROUTINE GET_VARIABLES (G1)
    !       get variables from g1 (gfile type) into common variables
    !       
    !       In other words, the variables from start.xx files are 
    !       stored into a g1 variable-type. For making these variables
    !       known by shadow, they have to be in the ex-common blocks, 
    !       (now the varibles in this shadow module). And this is done here
    !

        implicit none

        Type(GfType),  intent(in)  :: g1

        logical                    :: iOut
        integer(kind=4)            :: iFlag
        ! this may be turned for debugging purposes
        logical                    :: verbose=.false.

        ! FDISTR 

        iOut = GfGetValue(g1,"FDISTR",FDISTR)
        if (verbose .and. iOut) print *,"Loaded FDISTR: ",FDISTR
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FDISTR",iFlag)

        ! FGRID 
        iOut = GfGetValue(g1,"FGRID",FGRID)
        if (verbose .and. iOut) print *,"Loaded FGRID: ",FGRID
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FGRID",iFlag)

        ! FSOUR 
        iOut = GfGetValue(g1,"FSOUR",FSOUR)
        if (verbose .and. iOut) print *,"Loaded FSOUR: ",FSOUR
        if ( .not. iOut) print *,"shadow get_variables Failed go load variable"
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FSOUR",iFlag)

        ! FSOURCE_DEPTH 
        iOut = GfGetValue(g1,"FSOURCE_DEPTH",FSOURCE_DEPTH)
        if (verbose .and. iOut) print *,"Loaded FSOURCE_DEPTH: ",FSOURCE_DEPTH
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FSOURCE_DEPTH",iFlag)

        ! F_COHER 
        iOut = GfGetValue(g1,"F_COHER",F_COHER)
        if (verbose .and. iOut) print *,"Loaded F_COHER: ",F_COHER
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_COHER",iFlag)

        ! F_COLOR 
        iOut = GfGetValue(g1,"F_COLOR",F_COLOR)
        if (verbose .and. iOut) print *,"Loaded F_COLOR: ",F_COLOR
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_COLOR",iFlag)

        ! F_PHOT 
        iOut = GfGetValue(g1,"F_PHOT",F_PHOT)
        if (verbose .and. iOut) print *,"Loaded F_PHOT: ",F_PHOT
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_PHOT",iFlag)

        ! F_POL 
        iOut = GfGetValue(g1,"F_POL",F_POL)
        if (verbose .and. iOut) print *,"Loaded F_POL: ",F_POL
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_POL",iFlag)

        ! F_POLAR 
        iOut = GfGetValue(g1,"F_POLAR",F_POLAR)
        if (verbose .and. iOut) print *,"Loaded F_POLAR: ",F_POLAR
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_POLAR",iFlag)

        ! F_OPD 
        iOut = GfGetValue(g1,"F_OPD",F_OPD)
        if (verbose .and. iOut) print *,"Loaded F_OPD: ",F_OPD
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_OPD",iFlag)

        ! F_WIGGLER 
        iOut = GfGetValue(g1,"F_WIGGLER",F_WIGGLER)
        if (verbose .and. iOut) print *,"Loaded F_WIGGLER: ",F_WIGGLER
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_WIGGLER",iFlag)

        ! F_BOUND_SOUR 
        iOut = GfGetValue(g1,"F_BOUND_SOUR",F_BOUND_SOUR)
        if (verbose .and. iOut) print *,"Loaded F_BOUND_SOUR: ",F_BOUND_SOUR
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_BOUND_SOUR",iFlag)

        ! F_SR_TYPE 
        iOut = GfGetValue(g1,"F_SR_TYPE",F_SR_TYPE)
        if (verbose .and. iOut) print *,"Loaded F_SR_TYPE: ",F_SR_TYPE
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_SR_TYPE",iFlag)

        ! ISTAR1 
        iOut = GfGetValue(g1,"ISTAR1",ISTAR1)
        if (verbose .and. iOut) print *,"Loaded ISTAR1: ",ISTAR1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable ISTAR1",iFlag)

        ! NPOINT 
        iOut = GfGetValue(g1,"NPOINT",NPOINT)
        if (verbose .and. iOut) print *,"Loaded NPOINT: ",NPOINT
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable NPOINT",iFlag)

        ! NCOL 
        iOut = GfGetValue(g1,"NCOL",NCOL)
        if (verbose .and. iOut) print *,"Loaded NCOL: ",NCOL
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable NCOL",iFlag)

        ! N_CIRCLE 
        iOut = GfGetValue(g1,"N_CIRCLE",N_CIRCLE)
        if (verbose .and. iOut) print *,"Loaded N_CIRCLE: ",N_CIRCLE
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable N_CIRCLE",iFlag)

        ! N_COLOR 
        iOut = GfGetValue(g1,"N_COLOR",N_COLOR)
        if (verbose .and. iOut) print *,"Loaded N_COLOR: ",N_COLOR
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable N_COLOR",iFlag)

        ! N_CONE 
        iOut = GfGetValue(g1,"N_CONE",N_CONE)
        if (verbose .and. iOut) print *,"Loaded N_CONE: ",N_CONE
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable N_CONE",iFlag)

        ! IDO_VX 
        iOut = GfGetValue(g1,"IDO_VX",IDO_VX)
        if (verbose .and. iOut) print *,"Loaded IDO_VX: ",IDO_VX
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_VX",iFlag)

        ! IDO_VZ 
        iOut = GfGetValue(g1,"IDO_VZ",IDO_VZ)
        if (verbose .and. iOut) print *,"Loaded IDO_VZ: ",IDO_VZ
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_VZ",iFlag)

        ! IDO_X_S 
        iOut = GfGetValue(g1,"IDO_X_S",IDO_X_S)
        if (verbose .and. iOut) print *,"Loaded IDO_X_S: ",IDO_X_S
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_X_S",iFlag)

        ! IDO_Y_S 
        iOut = GfGetValue(g1,"IDO_Y_S",IDO_Y_S)
        if (verbose .and. iOut) print *,"Loaded IDO_Y_S: ",IDO_Y_S
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_Y_S",iFlag)

        ! IDO_Z_S 
        iOut = GfGetValue(g1,"IDO_Z_S",IDO_Z_S)
        if (verbose .and. iOut) print *,"Loaded IDO_Z_S: ",IDO_Z_S
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_Z_S",iFlag)

        ! IDO_XL 
        iOut = GfGetValue(g1,"IDO_XL",IDO_XL)
        if (verbose .and. iOut) print *,"Loaded IDO_XL: ",IDO_XL
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_XL",iFlag)

        ! IDO_XN 
        iOut = GfGetValue(g1,"IDO_XN",IDO_XN)
        if (verbose .and. iOut) print *,"Loaded IDO_XN: ",IDO_XN
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_XN",iFlag)

        ! IDO_ZL 
        iOut = GfGetValue(g1,"IDO_ZL",IDO_ZL)
        if (verbose .and. iOut) print *,"Loaded IDO_ZL: ",IDO_ZL
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_ZL",iFlag)

        ! IDO_ZN 
        iOut = GfGetValue(g1,"IDO_ZN",IDO_ZN)
        if (verbose .and. iOut) print *,"Loaded IDO_ZN: ",IDO_ZN
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDO_ZN",iFlag)

        ! SIGXL1 
        iOut = GfGetValue(g1,"SIGXL1",SIGXL1)
        if (verbose .and. iOut) print *,"Loaded SIGXL1: ",SIGXL1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL1",iFlag)

        ! SIGXL2 
        iOut = GfGetValue(g1,"SIGXL2",SIGXL2)
        if (verbose .and. iOut) print *,"Loaded SIGXL2: ",SIGXL2
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL2",iFlag)

        ! SIGXL3 
        iOut = GfGetValue(g1,"SIGXL3",SIGXL3)
        if (verbose .and. iOut) print *,"Loaded SIGXL3: ",SIGXL3
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL3",iFlag)

        ! SIGXL4 
        iOut = GfGetValue(g1,"SIGXL4",SIGXL4)
        if (verbose .and. iOut) print *,"Loaded SIGXL4: ",SIGXL4
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL4",iFlag)

        ! SIGXL5 
        iOut = GfGetValue(g1,"SIGXL5",SIGXL5)
        if (verbose .and. iOut) print *,"Loaded SIGXL5: ",SIGXL5
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL5",iFlag)

        ! SIGXL6 
        iOut = GfGetValue(g1,"SIGXL6",SIGXL6)
        if (verbose .and. iOut) print *,"Loaded SIGXL6: ",SIGXL6
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL6",iFlag)

        ! SIGXL7 
        iOut = GfGetValue(g1,"SIGXL7",SIGXL7)
        if (verbose .and. iOut) print *,"Loaded SIGXL7: ",SIGXL7
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL7",iFlag)

        ! SIGXL8 
        iOut = GfGetValue(g1,"SIGXL8",SIGXL8)
        if (verbose .and. iOut) print *,"Loaded SIGXL8: ",SIGXL8
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL8",iFlag)

        ! SIGXL9 
        iOut = GfGetValue(g1,"SIGXL9",SIGXL9)
        if (verbose .and. iOut) print *,"Loaded SIGXL9: ",SIGXL9
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL9",iFlag)

        ! SIGXL10 
        iOut = GfGetValue(g1,"SIGXL10",SIGXL10)
        if (verbose .and. iOut) print *,"Loaded SIGXL10: ",SIGXL10
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGXL10",iFlag)

        ! SIGZL1 
        iOut = GfGetValue(g1,"SIGZL1",SIGZL1)
        if (verbose .and. iOut) print *,"Loaded SIGZL1: ",SIGZL1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL1",iFlag)

        ! SIGZL2 
        iOut = GfGetValue(g1,"SIGZL2",SIGZL2)
        if (verbose .and. iOut) print *,"Loaded SIGZL2: ",SIGZL2
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL2",iFlag)

        ! SIGZL3 
        iOut = GfGetValue(g1,"SIGZL3",SIGZL3)
        if (verbose .and. iOut) print *,"Loaded SIGZL3: ",SIGZL3
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL3",iFlag)

        ! SIGZL4 
        iOut = GfGetValue(g1,"SIGZL4",SIGZL4)
        if (verbose .and. iOut) print *,"Loaded SIGZL4: ",SIGZL4
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL4",iFlag)

        ! SIGZL5 
        iOut = GfGetValue(g1,"SIGZL5",SIGZL5)
        if (verbose .and. iOut) print *,"Loaded SIGZL5: ",SIGZL5
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL5",iFlag)

        ! SIGZL6 
        iOut = GfGetValue(g1,"SIGZL6",SIGZL6)
        if (verbose .and. iOut) print *,"Loaded SIGZL6: ",SIGZL6
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL6",iFlag)

        ! SIGZL7 
        iOut = GfGetValue(g1,"SIGZL7",SIGZL7)
        if (verbose .and. iOut) print *,"Loaded SIGZL7: ",SIGZL7
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL7",iFlag)

        ! SIGZL8 
        iOut = GfGetValue(g1,"SIGZL8",SIGZL8)
        if (verbose .and. iOut) print *,"Loaded SIGZL8: ",SIGZL8
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL8",iFlag)

        ! SIGZL9 
        iOut = GfGetValue(g1,"SIGZL9",SIGZL9)
        if (verbose .and. iOut) print *,"Loaded SIGZL9: ",SIGZL9
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL9",iFlag)

        ! SIGZL10 
        iOut = GfGetValue(g1,"SIGZL10",SIGZL10)
        if (verbose .and. iOut) print *,"Loaded SIGZL10: ",SIGZL10
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGZL10",iFlag)

        ! CONV_FACT 
        iOut = GfGetValue(g1,"CONV_FACT",CONV_FACT)
        if (verbose .and. iOut) print *,"Loaded CONV_FACT: ",CONV_FACT
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable CONV_FACT",iFlag)

        ! CONE_MAX 
        iOut = GfGetValue(g1,"CONE_MAX",CONE_MAX)
        if (verbose .and. iOut) print *,"Loaded CONE_MAX: ",CONE_MAX
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable CONE_MAX",iFlag)

        ! CONE_MIN 
        iOut = GfGetValue(g1,"CONE_MIN",CONE_MIN)
        if (verbose .and. iOut) print *,"Loaded CONE_MIN: ",CONE_MIN
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable CONE_MIN",iFlag)

        ! EPSI_DX 
        iOut = GfGetValue(g1,"EPSI_DX",EPSI_DX)
        if (verbose .and. iOut) print *,"Loaded EPSI_DX: ",EPSI_DX
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable EPSI_DX",iFlag)

        ! EPSI_DZ 
        iOut = GfGetValue(g1,"EPSI_DZ",EPSI_DZ)
        if (verbose .and. iOut) print *,"Loaded EPSI_DZ: ",EPSI_DZ
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable EPSI_DZ",iFlag)

        ! EPSI_X 
        iOut = GfGetValue(g1,"EPSI_X",EPSI_X)
        if (verbose .and. iOut) print *,"Loaded EPSI_X: ",EPSI_X
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable EPSI_X",iFlag)

        ! EPSI_Z 
        iOut = GfGetValue(g1,"EPSI_Z",EPSI_Z)
        if (verbose .and. iOut) print *,"Loaded EPSI_Z: ",EPSI_Z
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable EPSI_Z",iFlag)

        ! HDIV1 
        iOut = GfGetValue(g1,"HDIV1",HDIV1)
        if (verbose .and. iOut) print *,"Loaded HDIV1: ",HDIV1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable HDIV1",iFlag)

        ! HDIV2 
        iOut = GfGetValue(g1,"HDIV2",HDIV2)
        if (verbose .and. iOut) print *,"Loaded HDIV2: ",HDIV2
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable HDIV2",iFlag)

        ! PH1 
        iOut = GfGetValue(g1,"PH1",PH1)
        if (verbose .and. iOut) print *,"Loaded PH1: ",PH1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH1",iFlag)

        ! PH2 
        iOut = GfGetValue(g1,"PH2",PH2)
        if (verbose .and. iOut) print *,"Loaded PH2: ",PH2
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH2",iFlag)

        ! PH3 
        iOut = GfGetValue(g1,"PH3",PH3)
        if (verbose .and. iOut) print *,"Loaded PH3: ",PH3
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH3",iFlag)

        ! PH4 
        iOut = GfGetValue(g1,"PH4",PH4)
        if (verbose .and. iOut) print *,"Loaded PH4: ",PH4
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH4",iFlag)

        ! PH5 
        iOut = GfGetValue(g1,"PH5",PH5)
        if (verbose .and. iOut) print *,"Loaded PH5: ",PH5
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH5",iFlag)

        ! PH6 
        iOut = GfGetValue(g1,"PH6",PH6)
        if (verbose .and. iOut) print *,"Loaded PH6: ",PH6
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH6",iFlag)

        ! PH7 
        iOut = GfGetValue(g1,"PH7",PH7)
        if (verbose .and. iOut) print *,"Loaded PH7: ",PH7
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH7",iFlag)

        ! PH8 
        iOut = GfGetValue(g1,"PH8",PH8)
        if (verbose .and. iOut) print *,"Loaded PH8: ",PH8
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH8",iFlag)

        ! PH9 
        iOut = GfGetValue(g1,"PH9",PH9)
        if (verbose .and. iOut) print *,"Loaded PH9: ",PH9
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH9",iFlag)

        ! PH10 
        iOut = GfGetValue(g1,"PH10",PH10)
        if (verbose .and. iOut) print *,"Loaded PH10: ",PH10
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PH10",iFlag)

        ! RL1 
        iOut = GfGetValue(g1,"RL1",RL1)
        if (verbose .and. iOut) print *,"Loaded RL1: ",RL1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL1",iFlag)

        ! RL2 
        iOut = GfGetValue(g1,"RL2",RL2)
        if (verbose .and. iOut) print *,"Loaded RL2: ",RL2
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL2",iFlag)

        ! RL3 
        iOut = GfGetValue(g1,"RL3",RL3)
        if (verbose .and. iOut) print *,"Loaded RL3: ",RL3
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL3",iFlag)

        ! RL4 
        iOut = GfGetValue(g1,"RL4",RL4)
        if (verbose .and. iOut) print *,"Loaded RL4: ",RL4
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL4",iFlag)

        ! RL5 
        iOut = GfGetValue(g1,"RL5",RL5)
        if (verbose .and. iOut) print *,"Loaded RL5: ",RL5
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL5",iFlag)

        ! RL6 
        iOut = GfGetValue(g1,"RL6",RL6)
        if (verbose .and. iOut) print *,"Loaded RL6: ",RL6
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL6",iFlag)

        ! RL7 
        iOut = GfGetValue(g1,"RL7",RL7)
        if (verbose .and. iOut) print *,"Loaded RL7: ",RL7
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL7",iFlag)

        ! RL8 
        iOut = GfGetValue(g1,"RL8",RL8)
        if (verbose .and. iOut) print *,"Loaded RL8: ",RL8
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL8",iFlag)

        ! RL9 
        iOut = GfGetValue(g1,"RL9",RL9)
        if (verbose .and. iOut) print *,"Loaded RL9: ",RL9
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL9",iFlag)

        ! RL10 
        iOut = GfGetValue(g1,"RL10",RL10)
        if (verbose .and. iOut) print *,"Loaded RL10: ",RL10
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable RL10",iFlag)

        ! BENER 
        iOut = GfGetValue(g1,"BENER",BENER)
        if (verbose .and. iOut) print *,"Loaded BENER: ",BENER
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable BENER",iFlag)

        ! POL_ANGLE 
        iOut = GfGetValue(g1,"POL_ANGLE",POL_ANGLE)
        if (verbose .and. iOut) print *,"Loaded POL_ANGLE: ",POL_ANGLE
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable POL_ANGLE",iFlag)

        ! POL_DEG 
        iOut = GfGetValue(g1,"POL_DEG",POL_DEG)
        if (verbose .and. iOut) print *,"Loaded POL_DEG: ",POL_DEG
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable POL_DEG",iFlag)

        ! R_ALADDIN 
        iOut = GfGetValue(g1,"R_ALADDIN",R_ALADDIN)
        if (verbose .and. iOut) print *,"Loaded R_ALADDIN: ",R_ALADDIN
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable R_ALADDIN",iFlag)

        ! R_MAGNET 
        iOut = GfGetValue(g1,"R_MAGNET",R_MAGNET)
        if (verbose .and. iOut) print *,"Loaded R_MAGNET: ",R_MAGNET
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable R_MAGNET",iFlag)

        ! SIGDIX 
        iOut = GfGetValue(g1,"SIGDIX",SIGDIX)
        if (verbose .and. iOut) print *,"Loaded SIGDIX: ",SIGDIX
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGDIX",iFlag)

        ! SIGDIZ 
        iOut = GfGetValue(g1,"SIGDIZ",SIGDIZ)
        if (verbose .and. iOut) print *,"Loaded SIGDIZ: ",SIGDIZ
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGDIZ",iFlag)

        ! SIGMAX 
        iOut = GfGetValue(g1,"SIGMAX",SIGMAX)
        if (verbose .and. iOut) print *,"Loaded SIGMAX: ",SIGMAX
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGMAX",iFlag)

        ! SIGMAY 
        iOut = GfGetValue(g1,"SIGMAY",SIGMAY)
        if (verbose .and. iOut) print *,"Loaded SIGMAY: ",SIGMAY
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGMAY",iFlag)

        ! SIGMAZ 
        iOut = GfGetValue(g1,"SIGMAZ",SIGMAZ)
        if (verbose .and. iOut) print *,"Loaded SIGMAZ: ",SIGMAZ
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable SIGMAZ",iFlag)

        ! VDIV1 
        iOut = GfGetValue(g1,"VDIV1",VDIV1)
        if (verbose .and. iOut) print *,"Loaded VDIV1: ",VDIV1
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable VDIV1",iFlag)

        ! VDIV2 
        iOut = GfGetValue(g1,"VDIV2",VDIV2)
        if (verbose .and. iOut) print *,"Loaded VDIV2: ",VDIV2
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable VDIV2",iFlag)

        ! WXSOU 
        iOut = GfGetValue(g1,"WXSOU",WXSOU)
        if (verbose .and. iOut) print *,"Loaded WXSOU: ",WXSOU
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable WXSOU",iFlag)

        ! WYSOU 
        iOut = GfGetValue(g1,"WYSOU",WYSOU)
        if (verbose .and. iOut) print *,"Loaded WYSOU: ",WYSOU
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable WYSOU",iFlag)

        ! WZSOU 
        iOut = GfGetValue(g1,"WZSOU",WZSOU)
        if (verbose .and. iOut) print *,"Loaded WZSOU: ",WZSOU
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable WZSOU",iFlag)

        ! PLASMA_ANGLE 
        iOut = GfGetValue(g1,"PLASMA_ANGLE",PLASMA_ANGLE)
        if (verbose .and. iOut) print *,"Loaded PLASMA_ANGLE: ",PLASMA_ANGLE
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable PLASMA_ANGLE",iFlag)

        ! FILE_TRAJ 
        iOut = GfGetValue(g1,"FILE_TRAJ",FILE_TRAJ)
        if (verbose .and. iOut) print *,"Loaded FILE_TRAJ: ",trim(FILE_TRAJ)
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FILE_TRAJ",iFlag)

        ! FILE_SOURCE 
        iOut = GfGetValue(g1,"FILE_SOURCE",FILE_SOURCE)
        if (verbose .and. iOut) print *,"Loaded FILE_SOURCE: ",trim(FILE_SOURCE)
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FILE_SOURCE",iFlag)

        ! FILE_BOUND 
        iOut = GfGetValue(g1,"FILE_BOUND",FILE_BOUND)
        if (verbose .and. iOut) print *,"Loaded FILE_BOUND: ",trim(FILE_BOUND)
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable FILE_BOUND",iFlag)

        ! OE_NUMBER 
        iOut = GfGetValue(g1,"OE_NUMBER",OE_NUMBER)
        if (verbose .and. iOut) print *,"Loaded OE_NUMBER: ",OE_NUMBER
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable OE_NUMBER",iFlag)

        ! IDUMMY 
        iOut = GfGetValue(g1,"IDUMMY",IDUMMY)
        if (verbose .and. iOut) print *,"Loaded IDUMMY: ",IDUMMY
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable IDUMMY",iFlag)

        ! DUMMY 
        iOut = GfGetValue(g1,"DUMMY",DUMMY)
        if (verbose .and. iOut) print *,"Loaded DUMMY: ",DUMMY
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable DUMMY",iFlag)

        ! F_NEW 
        iOut = GfGetValue(g1,"F_NEW",F_NEW)
        if (verbose .and. iOut) print *,"Loaded F_NEW: ",F_NEW
        if ( .not. iOut) call mssg("SHADOW-GET_VARIABLES","Failed to load variable F_NEW",iFlag)

    END SUBROUTINE GET_VARIABLES

    !
    !
    !



    SUBROUTINE PUT_VARIABLES (G1)
    !
    !  SUBROUTINE PUT_VARIABLES (G1)
    !
    !       put variables from common variables into g1 (gfile type) 
    !       
    !       In other words, the variables from the ex-common blocks
    !       must be stored into a g1 variable-type, before being 
    !       written into start.xx files.  And this is done here.
    !

        implicit none

        Type(GfType),  intent(in out)  :: g1
        logical                        :: verbose=.false.
        logical                        :: iOut

        ! FDISTR 
        iOut = GfSetValue(g1,"FDISTR",FDISTR)
        if (verbose .and. iOut) print *,"Stored FDISTR: ",FDISTR

        ! FGRID 
        iOut = GfSetValue(g1,"FGRID",FGRID)
        if (verbose .and. iOut) print *,"Stored FGRID: ",FGRID

        ! FSOUR 
        iOut = GfSetValue(g1,"FSOUR",FSOUR)
        if (verbose .and. iOut) print *,"Stored FSOUR: ",FSOUR

        ! FSOURCE_DEPTH 
        iOut = GfSetValue(g1,"FSOURCE_DEPTH",FSOURCE_DEPTH)
        if (verbose .and. iOut) print *,"Stored FSOURCE_DEPTH: ",FSOURCE_DEPTH

        ! F_COHER 
        iOut = GfSetValue(g1,"F_COHER",F_COHER)
        if (verbose .and. iOut) print *,"Stored F_COHER: ",F_COHER

        ! F_COLOR 
        iOut = GfSetValue(g1,"F_COLOR",F_COLOR)
        if (verbose .and. iOut) print *,"Stored F_COLOR: ",F_COLOR

        ! F_PHOT 
        iOut = GfSetValue(g1,"F_PHOT",F_PHOT)
        if (verbose .and. iOut) print *,"Stored F_PHOT: ",F_PHOT

        ! F_POL 
        iOut = GfSetValue(g1,"F_POL",F_POL)
        if (verbose .and. iOut) print *,"Stored F_POL: ",F_POL

        ! F_POLAR 
        iOut = GfSetValue(g1,"F_POLAR",F_POLAR)
        if (verbose .and. iOut) print *,"Stored F_POLAR: ",F_POLAR

        ! F_OPD 
        iOut = GfSetValue(g1,"F_OPD",F_OPD)
        if (verbose .and. iOut) print *,"Stored F_OPD: ",F_OPD

        ! F_WIGGLER 
        iOut = GfSetValue(g1,"F_WIGGLER",F_WIGGLER)
        if (verbose .and. iOut) print *,"Stored F_WIGGLER: ",F_WIGGLER

        ! F_BOUND_SOUR 
        iOut = GfSetValue(g1,"F_BOUND_SOUR",F_BOUND_SOUR)
        if (verbose .and. iOut) print *,"Stored F_BOUND_SOUR: ",F_BOUND_SOUR

        ! F_SR_TYPE 
        iOut = GfSetValue(g1,"F_SR_TYPE",F_SR_TYPE)
        if (verbose .and. iOut) print *,"Stored F_SR_TYPE: ",F_SR_TYPE

        ! ISTAR1 
        iOut = GfSetValue(g1,"ISTAR1",ISTAR1)
        if (verbose .and. iOut) print *,"Stored ISTAR1: ",ISTAR1

        ! NPOINT 
        iOut = GfSetValue(g1,"NPOINT",NPOINT)
        if (verbose .and. iOut) print *,"Stored NPOINT: ",NPOINT

        ! NCOL 
        iOut = GfSetValue(g1,"NCOL",NCOL)
        if (verbose .and. iOut) print *,"Stored NCOL: ",NCOL

        ! N_CIRCLE 
        iOut = GfSetValue(g1,"N_CIRCLE",N_CIRCLE)
        if (verbose .and. iOut) print *,"Stored N_CIRCLE: ",N_CIRCLE

        ! N_COLOR 
        iOut = GfSetValue(g1,"N_COLOR",N_COLOR)
        if (verbose .and. iOut) print *,"Stored N_COLOR: ",N_COLOR

        ! N_CONE 
        iOut = GfSetValue(g1,"N_CONE",N_CONE)
        if (verbose .and. iOut) print *,"Stored N_CONE: ",N_CONE

        ! IDO_VX 
        iOut = GfSetValue(g1,"IDO_VX",IDO_VX)
        if (verbose .and. iOut) print *,"Stored IDO_VX: ",IDO_VX

        ! IDO_VZ 
        iOut = GfSetValue(g1,"IDO_VZ",IDO_VZ)
        if (verbose .and. iOut) print *,"Stored IDO_VZ: ",IDO_VZ

        ! IDO_X_S 
        iOut = GfSetValue(g1,"IDO_X_S",IDO_X_S)
        if (verbose .and. iOut) print *,"Stored IDO_X_S: ",IDO_X_S

        ! IDO_Y_S 
        iOut = GfSetValue(g1,"IDO_Y_S",IDO_Y_S)
        if (verbose .and. iOut) print *,"Stored IDO_Y_S: ",IDO_Y_S

        ! IDO_Z_S 
        iOut = GfSetValue(g1,"IDO_Z_S",IDO_Z_S)
        if (verbose .and. iOut) print *,"Stored IDO_Z_S: ",IDO_Z_S

        ! IDO_XL 
        iOut = GfSetValue(g1,"IDO_XL",IDO_XL)
        if (verbose .and. iOut) print *,"Stored IDO_XL: ",IDO_XL

        ! IDO_XN 
        iOut = GfSetValue(g1,"IDO_XN",IDO_XN)
        if (verbose .and. iOut) print *,"Stored IDO_XN: ",IDO_XN

        ! IDO_ZL 
        iOut = GfSetValue(g1,"IDO_ZL",IDO_ZL)
        if (verbose .and. iOut) print *,"Stored IDO_ZL: ",IDO_ZL

        ! IDO_ZN 
        iOut = GfSetValue(g1,"IDO_ZN",IDO_ZN)
        if (verbose .and. iOut) print *,"Stored IDO_ZN: ",IDO_ZN

        ! SIGXL1 
        iOut = GfSetValue(g1,"SIGXL1",SIGXL1)
        if (verbose .and. iOut) print *,"Stored SIGXL1: ",SIGXL1

        ! SIGXL2 
        iOut = GfSetValue(g1,"SIGXL2",SIGXL2)
        if (verbose .and. iOut) print *,"Stored SIGXL2: ",SIGXL2

        ! SIGXL3 
        iOut = GfSetValue(g1,"SIGXL3",SIGXL3)
        if (verbose .and. iOut) print *,"Stored SIGXL3: ",SIGXL3

        ! SIGXL4 
        iOut = GfSetValue(g1,"SIGXL4",SIGXL4)
        if (verbose .and. iOut) print *,"Stored SIGXL4: ",SIGXL4

        ! SIGXL5 
        iOut = GfSetValue(g1,"SIGXL5",SIGXL5)
        if (verbose .and. iOut) print *,"Stored SIGXL5: ",SIGXL5

        ! SIGXL6 
        iOut = GfSetValue(g1,"SIGXL6",SIGXL6)
        if (verbose .and. iOut) print *,"Stored SIGXL6: ",SIGXL6

        ! SIGXL7 
        iOut = GfSetValue(g1,"SIGXL7",SIGXL7)
        if (verbose .and. iOut) print *,"Stored SIGXL7: ",SIGXL7

        ! SIGXL8 
        iOut = GfSetValue(g1,"SIGXL8",SIGXL8)
        if (verbose .and. iOut) print *,"Stored SIGXL8: ",SIGXL8

        ! SIGXL9 
        iOut = GfSetValue(g1,"SIGXL9",SIGXL9)
        if (verbose .and. iOut) print *,"Stored SIGXL9: ",SIGXL9

        ! SIGXL10 
        iOut = GfSetValue(g1,"SIGXL10",SIGXL10)
        if (verbose .and. iOut) print *,"Stored SIGXL10: ",SIGXL10

        ! SIGZL1 
        iOut = GfSetValue(g1,"SIGZL1",SIGZL1)
        if (verbose .and. iOut) print *,"Stored SIGZL1: ",SIGZL1

        ! SIGZL2 
        iOut = GfSetValue(g1,"SIGZL2",SIGZL2)
        if (verbose .and. iOut) print *,"Stored SIGZL2: ",SIGZL2

        ! SIGZL3 
        iOut = GfSetValue(g1,"SIGZL3",SIGZL3)
        if (verbose .and. iOut) print *,"Stored SIGZL3: ",SIGZL3

        ! SIGZL4 
        iOut = GfSetValue(g1,"SIGZL4",SIGZL4)
        if (verbose .and. iOut) print *,"Stored SIGZL4: ",SIGZL4

        ! SIGZL5 
        iOut = GfSetValue(g1,"SIGZL5",SIGZL5)
        if (verbose .and. iOut) print *,"Stored SIGZL5: ",SIGZL5

        ! SIGZL6 
        iOut = GfSetValue(g1,"SIGZL6",SIGZL6)
        if (verbose .and. iOut) print *,"Stored SIGZL6: ",SIGZL6

        ! SIGZL7 
        iOut = GfSetValue(g1,"SIGZL7",SIGZL7)
        if (verbose .and. iOut) print *,"Stored SIGZL7: ",SIGZL7

        ! SIGZL8 
        iOut = GfSetValue(g1,"SIGZL8",SIGZL8)
        if (verbose .and. iOut) print *,"Stored SIGZL8: ",SIGZL8

        ! SIGZL9 
        iOut = GfSetValue(g1,"SIGZL9",SIGZL9)
        if (verbose .and. iOut) print *,"Stored SIGZL9: ",SIGZL9

        ! SIGZL10 
        iOut = GfSetValue(g1,"SIGZL10",SIGZL10)
        if (verbose .and. iOut) print *,"Stored SIGZL10: ",SIGZL10

        ! CONV_FACT 
        iOut = GfSetValue(g1,"CONV_FACT",CONV_FACT)
        if (verbose .and. iOut) print *,"Stored CONV_FACT: ",CONV_FACT

        ! CONE_MAX 
        iOut = GfSetValue(g1,"CONE_MAX",CONE_MAX)
        if (verbose .and. iOut) print *,"Stored CONE_MAX: ",CONE_MAX

        ! CONE_MIN 
        iOut = GfSetValue(g1,"CONE_MIN",CONE_MIN)
        if (verbose .and. iOut) print *,"Stored CONE_MIN: ",CONE_MIN

        ! EPSI_DX 
        iOut = GfSetValue(g1,"EPSI_DX",EPSI_DX)
        if (verbose .and. iOut) print *,"Stored EPSI_DX: ",EPSI_DX

        ! EPSI_DZ 
        iOut = GfSetValue(g1,"EPSI_DZ",EPSI_DZ)
        if (verbose .and. iOut) print *,"Stored EPSI_DZ: ",EPSI_DZ

        ! EPSI_X 
        iOut = GfSetValue(g1,"EPSI_X",EPSI_X)
        if (verbose .and. iOut) print *,"Stored EPSI_X: ",EPSI_X

        ! EPSI_Z 
        iOut = GfSetValue(g1,"EPSI_Z",EPSI_Z)
        if (verbose .and. iOut) print *,"Stored EPSI_Z: ",EPSI_Z

        ! HDIV1 
        iOut = GfSetValue(g1,"HDIV1",HDIV1)
        if (verbose .and. iOut) print *,"Stored HDIV1: ",HDIV1

        ! HDIV2 
        iOut = GfSetValue(g1,"HDIV2",HDIV2)
        if (verbose .and. iOut) print *,"Stored HDIV2: ",HDIV2

        ! PH1 
        iOut = GfSetValue(g1,"PH1",PH1)
        if (verbose .and. iOut) print *,"Stored PH1: ",PH1

        ! PH2 
        iOut = GfSetValue(g1,"PH2",PH2)
        if (verbose .and. iOut) print *,"Stored PH2: ",PH2

        ! PH3 
        iOut = GfSetValue(g1,"PH3",PH3)
        if (verbose .and. iOut) print *,"Stored PH3: ",PH3

        ! PH4 
        iOut = GfSetValue(g1,"PH4",PH4)
        if (verbose .and. iOut) print *,"Stored PH4: ",PH4

        ! PH5 
        iOut = GfSetValue(g1,"PH5",PH5)
        if (verbose .and. iOut) print *,"Stored PH5: ",PH5

        ! PH6 
        iOut = GfSetValue(g1,"PH6",PH6)
        if (verbose .and. iOut) print *,"Stored PH6: ",PH6

        ! PH7 
        iOut = GfSetValue(g1,"PH7",PH7)
        if (verbose .and. iOut) print *,"Stored PH7: ",PH7

        ! PH8 
        iOut = GfSetValue(g1,"PH8",PH8)
        if (verbose .and. iOut) print *,"Stored PH8: ",PH8

        ! PH9 
        iOut = GfSetValue(g1,"PH9",PH9)
        if (verbose .and. iOut) print *,"Stored PH9: ",PH9

        ! PH10 
        iOut = GfSetValue(g1,"PH10",PH10)
        if (verbose .and. iOut) print *,"Stored PH10: ",PH10

        ! RL1 
        iOut = GfSetValue(g1,"RL1",RL1)
        if (verbose .and. iOut) print *,"Stored RL1: ",RL1

        ! RL2 
        iOut = GfSetValue(g1,"RL2",RL2)
        if (verbose .and. iOut) print *,"Stored RL2: ",RL2

        ! RL3 
        iOut = GfSetValue(g1,"RL3",RL3)
        if (verbose .and. iOut) print *,"Stored RL3: ",RL3

        ! RL4 
        iOut = GfSetValue(g1,"RL4",RL4)
        if (verbose .and. iOut) print *,"Stored RL4: ",RL4

        ! RL5 
        iOut = GfSetValue(g1,"RL5",RL5)
        if (verbose .and. iOut) print *,"Stored RL5: ",RL5

        ! RL6 
        iOut = GfSetValue(g1,"RL6",RL6)
        if (verbose .and. iOut) print *,"Stored RL6: ",RL6

        ! RL7 
        iOut = GfSetValue(g1,"RL7",RL7)
        if (verbose .and. iOut) print *,"Stored RL7: ",RL7

        ! RL8 
        iOut = GfSetValue(g1,"RL8",RL8)
        if (verbose .and. iOut) print *,"Stored RL8: ",RL8

        ! RL9 
        iOut = GfSetValue(g1,"RL9",RL9)
        if (verbose .and. iOut) print *,"Stored RL9: ",RL9

        ! RL10 
        iOut = GfSetValue(g1,"RL10",RL10)
        if (verbose .and. iOut) print *,"Stored RL10: ",RL10

        ! BENER 
        iOut = GfSetValue(g1,"BENER",BENER)
        if (verbose .and. iOut) print *,"Stored BENER: ",BENER

        ! POL_ANGLE 
        iOut = GfSetValue(g1,"POL_ANGLE",POL_ANGLE)
        if (verbose .and. iOut) print *,"Stored POL_ANGLE: ",POL_ANGLE

        ! POL_DEG 
        iOut = GfSetValue(g1,"POL_DEG",POL_DEG)
        if (verbose .and. iOut) print *,"Stored POL_DEG: ",POL_DEG

        ! R_ALADDIN 
        iOut = GfSetValue(g1,"R_ALADDIN",R_ALADDIN)
        if (verbose .and. iOut) print *,"Stored R_ALADDIN: ",R_ALADDIN

        ! R_MAGNET 
        iOut = GfSetValue(g1,"R_MAGNET",R_MAGNET)
        if (verbose .and. iOut) print *,"Stored R_MAGNET: ",R_MAGNET

        ! SIGDIX 
        iOut = GfSetValue(g1,"SIGDIX",SIGDIX)
        if (verbose .and. iOut) print *,"Stored SIGDIX: ",SIGDIX

        ! SIGDIZ 
        iOut = GfSetValue(g1,"SIGDIZ",SIGDIZ)
        if (verbose .and. iOut) print *,"Stored SIGDIZ: ",SIGDIZ

        ! SIGMAX 
        iOut = GfSetValue(g1,"SIGMAX",SIGMAX)
        if (verbose .and. iOut) print *,"Stored SIGMAX: ",SIGMAX

        ! SIGMAY 
        iOut = GfSetValue(g1,"SIGMAY",SIGMAY)
        if (verbose .and. iOut) print *,"Stored SIGMAY: ",SIGMAY

        ! SIGMAZ 
        iOut = GfSetValue(g1,"SIGMAZ",SIGMAZ)
        if (verbose .and. iOut) print *,"Stored SIGMAZ: ",SIGMAZ

        ! VDIV1 
        iOut = GfSetValue(g1,"VDIV1",VDIV1)
        if (verbose .and. iOut) print *,"Stored VDIV1: ",VDIV1

        ! VDIV2 
        iOut = GfSetValue(g1,"VDIV2",VDIV2)
        if (verbose .and. iOut) print *,"Stored VDIV2: ",VDIV2

        ! WXSOU 
        iOut = GfSetValue(g1,"WXSOU",WXSOU)
        if (verbose .and. iOut) print *,"Stored WXSOU: ",WXSOU

        ! WYSOU 
        iOut = GfSetValue(g1,"WYSOU",WYSOU)
        if (verbose .and. iOut) print *,"Stored WYSOU: ",WYSOU

        ! WZSOU 
        iOut = GfSetValue(g1,"WZSOU",WZSOU)
        if (verbose .and. iOut) print *,"Stored WZSOU: ",WZSOU

        ! PLASMA_ANGLE 
        iOut = GfSetValue(g1,"PLASMA_ANGLE",PLASMA_ANGLE)
        if (verbose .and. iOut) print *,"Stored PLASMA_ANGLE: ",PLASMA_ANGLE

        ! FILE_TRAJ 
        iOut = GfSetValue(g1,"FILE_TRAJ",FILE_TRAJ)
        if (verbose .and. iOut) print *,"Stored FILE_TRAJ: ",trim(FILE_TRAJ)

        ! FILE_SOURCE 
        iOut = GfSetValue(g1,"FILE_SOURCE",FILE_SOURCE)
        if (verbose .and. iOut) print *,"Stored FILE_SOURCE: ",trim(FILE_SOURCE)

        ! FILE_BOUND 
        iOut = GfSetValue(g1,"FILE_BOUND",FILE_BOUND)
        if (verbose .and. iOut) print *,"Stored FILE_BOUND: ",trim(FILE_BOUND)

        ! OE_NUMBER 
        iOut = GfSetValue(g1,"OE_NUMBER",OE_NUMBER)
        if (verbose .and. iOut) print *,"Stored OE_NUMBER: ",OE_NUMBER

        ! IDUMMY 
        iOut = GfSetValue(g1,"IDUMMY",IDUMMY)
        if (verbose .and. iOut) print *,"Stored IDUMMY: ",IDUMMY

        ! DUMMY 
        iOut = GfSetValue(g1,"DUMMY",DUMMY)
        if (verbose .and. iOut) print *,"Stored DUMMY: ",DUMMY

        ! F_NEW 
        iOut = GfSetValue(g1,"F_NEW",F_NEW)
        if (verbose .and. iOut) print *,"Stored F_NEW: ",F_NEW

    END SUBROUTINE PUT_VARIABLES

    !
    !
    !

    !C +++
    !C 	SUBROUTINE		SOURCE_BOUND
    !C 
    !C 	Purpose			To optimize the source generation by
    !C 				rejecting the rays generated outside a
    !C 				given region. The three directions are
    !C 				considered to be ``uncoupled''.
    !C 
    !C 	Algorithm		Acceptance/rejection method.
    !C 
    !C ---

    SUBROUTINE	SOURCE_BOUND	(POS, DIR, IFLAG)


	! IMPLICIT	REAL*8		(A-E,G-H,O-Z)
	! IMPLICIT	INTEGER*4	(F,I-N)
	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (F,I-N)
    
    !C 
    !C  Save the values that the caller expects to be there next time this
    !C  routine is called. The following chunk is basically an unnamed COMMON
    !C  block (w/out the corruption of the namespace, of course).
    !C 
    	SAVE		IX, IY, IZ, XMIN, YMIN, ZMIN, &
          			NX, NX1, NY, NY1, NZ, NZ1, &
         			XS, X1S, YS, Y1S, ZS, Z1S, &
                           X1MIN, Y1MIN, Z1MIN
         	DIMENSION	POS(3), DIR(3)
         	DIMENSION	IX(101,101), IY(101,101), IZ(101,101)
    !C 	
    !C  checks for initialization
    !C 
         	IF (IFLAG.LT.0) THEN
    !! #ifdef vms
    !!      	  OPEN	(30, FILE=FILE_BOUND, STATUS='OLD', READONLY, 
    !!      $		     FORM='UNFORMATTED', IOSTAT=IERR)
    !! #else
         	  OPEN	(30, FILE=FILE_BOUND, STATUS='OLD', FORM='UNFORMATTED', IOSTAT=IERR)
    !! #endif
         	 IF (IERR.NE.0) THEN
         	   WRITE(6,*)'Error opening file: ',S_BOUND
         	   STOP
         	 END IF
         	  READ (30,ERR=101)	NX, XMIN, XS
         	  READ (30,ERR=101)	NX1, X1MIN, X1S
         	  READ (30,ERR=101)	NY, YMIN, YS
         	  READ (30,ERR=101)	NY1, Y1MIN, Y1S
         	  READ (30,ERR=101)	NZ, ZMIN, ZS
         	  READ (30,ERR=101)	NZ1, Z1MIN, Z1S
         	  DO 11 I=1,NX
         	   READ (30,ERR=101)	(IX(I,J),J=1,NX1)
    11     	  CONTINUE
         	  DO 21 I=1,NY
         	   READ (30,ERR=101)	(IY(I,J),J=1,NY1)
    21     	  CONTINUE
         	  DO 31 I=1,NZ
         	   READ (30,ERR=101)	(IZ(I,J),J=1,NZ1)
    31     	  CONTINUE
         	  CLOSE (30)
         	 WRITE(6,*)'Phase space boundaries file read succesfully.'
         	  RETURN
    101	 WRITE(6,*)'Error reading from file ',S_BOUND
         	 STOP
         	 ELSE IF (IFLAG.EQ.1) THEN
    !C 
    !C  Normal entry
    !C  Tests for ''good'' hits; if a bad one fund, return
    !C 
         	  IFLAG = -1
    	  IF (XS.NE.0) THEN
         	    JX  = (POS(1) - XMIN)/XS + 1
    	  ELSE
    	    JX	= 1
    	  END IF
    	  IF (X1S.NE.0) THEN
         	    JX1 = (DIR(1) - X1MIN)/X1S + 1
    	  ELSE
    	    JX1	= 1
    	  END IF
    !C 
    !C  tests first for bounds limits; if outside any, no sense trying more.
    !C 
         	 IF (JX.LT.1.OR.JX.GT.NX) RETURN
    !C      	  IFLAG = -11
         	 IF (JX1.LT.1.OR.JX1.GT.NX1) RETURN
    !C 
    !C  within bounds; test for acceptance
    !C 
    ! D     	  IFLAG = -101
         	 IF ( IX(JX,JX1).EQ.0) RETURN
    !C 
    !C  ''x'' is OK; continue with Y,Z
    !C  
    ! D     	 IFLAG = -2
    	  IF (YS.NE.0) THEN
         	    JY  = (POS(2) - YMIN)/YS + 1
    	  ELSE
    	    JY	= 1
    	  END IF
    	  IF (Y1S.NE.0) THEN
         	    JY1 = (DIR(2) - Y1MIN)/Y1S + 1
    	  ELSE
    	    JY1	= 0
    	  END IF
         	 IF (JY.LT.1.OR.JY.GT.NY) RETURN
    ! D     	 IFLAG = -21
         	 IF (JY1.LT.1.OR.JY1.GT.NY1) RETURN
    ! D     	 IFLAG = -201
         	 IF ( IY(JY,JY1).EQ.0) RETURN
    ! D     	 IFLAG = -3
    	  IF (ZS.NE.0) THEN
         	    JZ  = (POS(3) - ZMIN)/ZS + 1
    	  ELSE
    	    JZ	= 1
    	  END IF
    	  IF (Z1S.NE.0) THEN
         	    JZ1 = (DIR(3) - Z1MIN)/Z1S + 1
    	  ELSE
    	    JZ1 =1
    	  END IF
         	 IF (JZ.LT.1.OR.JZ.GT.NZ) RETURN
    ! D     	 IFLAG = -31
         	 IF (JZ1.LT.1.OR.JZ1.GT.NZ1) RETURN
    ! D     	 IFLAG = -301
         	 IF ( IZ(JZ,JZ1).EQ.0) RETURN
    !C 
    !C  the ray is acceptable;
    !C 
         	 IFLAG = 1
         	 RETURN
         	END IF
    END SUBROUTINE SOURCE_BOUND
    !
    !
    !

    ! C+++
    ! C	SUBROUTINE	SOURCE
    ! C
    ! C	PURPOSE		To generate a (12,ndim) array describing the system
    ! C			source.
    ! C
    ! C	INPUTS		From modules SETSOUR, MSETUP, through COMMON.BLK
    ! C			The following two input parameters are meant to
    ! C			be passed to the WRITE_OFF subroutine to control
    ! C			the output format.
    ! C	    INFILE	Name of the input start file. 
    ! C	    FNAME	Name of the output file. This enables us to use
    ! C			use ASCII or BINARY output files (BEGIN.DAT's).
    ! C	    IOFORM = 0, Binary output file.
    ! C		   = 1, ASCII output file. (only unix version)
    ! C
    ! C	OUTPUTS		File BEGIN.DAT
    ! C
    ! C	MODIFICATIONS	Included wiggler and undulator source (2/26/88)
    ! C
    ! C---
    	SUBROUTINE 	SOURCE1 (infile, FNAME, IOFORM)


	! IMPLICIT	REAL*8		(A-E,G-H,O-Z)
	! IMPLICIT	INTEGER*4	(F,I-N)
	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (F,I-N)

    ! C
    	INTEGER(KIND=4)		IOFORM
    !!srio
    	INTEGER(KIND=4)		C_X,C_Y,C_Z,C_VX,C_VZ,C_XN,C_ZN
    	CHARACTER*(*)	infile, FNAME
    ! C
    	CHARACTER*80	ERRMSG
    
    
    !!	DIMENSION	BEGIN(12,N_DIM), PHASE(3,N_DIM), AP(3,N_DIM)
            real(kind=kind(1.0d0)), dimension(:,:), allocatable :: begin,phase,ap

    !! needed for calling source_bound
            real(kind=kind(1.0d0)), dimension(3)                :: XDUM, YDUM
    
    
    
    !!	DIMENSION 	DIREC(3),AP_VEC(3),E_TEMP(3),SB_POS(3), &
    !!     			VTEMP(3),GRID(6,N_DIM),A_VEC(3),A_TEMP(3), &
    !!     			E_BEAM(3)
    	DIMENSION 	DIREC(3),AP_VEC(3),E_TEMP(3),SB_POS(3), &
         			VTEMP(3),A_VEC(3),A_TEMP(3), E_BEAM(3)
            real(kind=kind(1.0d0)), dimension(:,:), allocatable :: grid
    
    
    	DIMENSION	SIGXL(10),SIGZL(10)
    
    !!	REAL*8		SEED_Y(5,N_DIM),Y_X(5,N_DIM),Y_XPRI(5,N_DIM), &
    !!                       Y_Z(5,N_DIM),Y_ZPRI(5,N_DIM), &
    !!     			Y_CURV(5,N_DIM),Y_PATH(5,N_DIM)
    !!	REAL*8		Y_TEMP(N_DIM),C_TEMP(N_DIM),X_TEMP(N_DIM), &
    !!     			Z_TEMP(N_DIM),ANG_TEMP(N_DIM),P_TEMP(N_DIM), &
    !!                       ANG2_TEMP(N_DIM)
    
    !!        real(kind=8), dimension(:,:), allocatable :: seed_y,y_x,y_xpri, &
    !!                       y_z,y_zpri,y_curv,y_path
    
    !!        real(kind=8), dimension(:), allocatable :: y_temp,c_temp,x_temp, &
    !!                       z_temp, ang_temp, p_temp, ang2_temp, abd2_temp 
    
    
    	DOUBLE PRECISION YRAN,DPS_RAN1,DPS_RAN2
    	DOUBLE PRECISION TMP_A,TMP_B,DPS_RAN3
    
    !!srio	DIMENSION	CDFX(31,31,51),CDFZ(31,51),CDFW(51)
    !!srio	DIMENSION	D_POL(31,31,51)
    !!srio	DIMENSION	UPHI(31,31,51),UTHETA(31,51),UENER(51)
    	DIMENSION	JI(2),DZ(2),THE_INT(2)
    	DIMENSION	II(4),DX(4),PHI_INT(4)
    
    	DIMENSION	RELINT(10),PRELINT(10)
    
    
    ! C
    ! C Save the *big* arrays so it will:
    ! C  -- zero out the elements.
    ! C  -- put in the global heap.
    ! C
    !!	SAVE		BEGIN, PHASE, AP, &
    !!      			DIREC,AP_VEC,E_TEMP,SB_POS,&
    !!     			GRID,A_VEC,A_TEMP,E_BEAM,&
    !!     			SIGXL,SIGZL,&
    !!     			SEED_Y,Y_X,Y_XPRI,Y_Z,Y_ZPRI,&
    !!     			Y_CURV,Y_PATH,&
    !!     			Y_TEMP,C_TEMP,X_TEMP,Z_TEMP,&
    !!     			ANG_TEMP,P_TEMP,ANG2_TEMP
    
    !srio      	DATA  SQRT_2  /1.4142135623730950488016887D0/
    
    
    !! load gfile (moved from gen_source)
    
    
    
         	ISTAT = 0
         	IDUMM = 0
	    call rwname(infile,'R_SOUR',iFlag)
            !call gfload_source (infile, idumm)
            IF (IFLAG.NE.0) CALL LEAVE ('SHADOW-SOURCE1','Failed to read input file: '//infile,idumm)
            !! PROVISIONAL STUFF...
            IF ((FDISTR.EQ.4).OR.(FSOURCE_DEPTH.EQ.4).OR.(F_WIGGLER.GT.0)) THEN
               ITMP=1
               CALL LEAVE ('SOURCE1','Only geometrical source. No synchrotron source allowed here',ITMP)
            ENDIF
    !!        CALL  SOURCE1 (BGNFILE, IOFORM)
    
    !!
    !! Allocate arrays
    !!
    
               if (ALLOCATED(begin)) then
                   ITMP=1
                   CALL LEAVE ('SOURCE1','Arrays already allocated.',ITMP)
               else
                  ALLOCATE(begin(12,NPOINT),stat=ierr)
                  ALLOCATE(phase(3,NPOINT),stat=ierr)
                  ALLOCATE(ap(3,NPOINT),stat=ierr)
    
    !!     			VTEMP(3),GRID(6,N_DIM),A_VEC(3),A_TEMP(3), &
                  ALLOCATE(grid(6,NPOINT),stat=ierr)
    
    
    !!	REAL*8		SEED_Y(5,N_DIM),Y_X(5,N_DIM),Y_XPRI(5,N_DIM), &
    !!                       Y_Z(5,N_DIM),Y_ZPRI(5,N_DIM), &
    !!     			Y_CURV(5,N_DIM),Y_PATH(5,N_DIM)
    !!              ALLOCATE(seed_y(5,NPOINT),stat=ierr)
    !!              ALLOCATE(y_x(5,NPOINT),stat=ierr)
    !!              ALLOCATE(y_xpri(5,NPOINT),stat=ierr)
    !!              ALLOCATE(y_z(5,NPOINT),stat=ierr)
    !!              ALLOCATE(y_zpri(5,NPOINT),stat=ierr)
    !!              ALLOCATE(y_curv(5,NPOINT),stat=ierr)
    !!              ALLOCATE(y_path(5,NPOINT),stat=ierr)
    
    !!	REAL*8		Y_TEMP(N_DIM),C_TEMP(N_DIM),X_TEMP(N_DIM), &
    !!     			Z_TEMP(N_DIM),ANG_TEMP(N_DIM),P_TEMP(N_DIM), &
    !!                       ANG2_TEMP(N_DIM)
    !!              ALLOCATE(y_temp(NPOINT),stat=ierr)
    !!              ALLOCATE(c_temp(NPOINT),stat=ierr)
    !!              ALLOCATE(x_temp(NPOINT),stat=ierr)
    !!              ALLOCATE(z_temp(NPOINT),stat=ierr)
    !!              ALLOCATE(ang_temp(NPOINT),stat=ierr)
    !!              ALLOCATE(p_temp(NPOINT),stat=ierr)
    !!              ALLOCATE(ang2_temp(NPOINT),stat=ierr)
               end if
    
    
    
    !!srio test empty arrays to file:
    !!srio  CALL WRITE_OFF('tmp.dat',BEGIN,PHASE,AP,18,333,IFLAG,IOFORM,IERR)
    !!srio
    
         	KREJ = 0
         	NREJ = 0
    ! C
    ! C Sets up some variables needed for the rest of the routine
    ! C
    ! C First figure out the number of columns written out for each ray.
    ! C
    	IF (F_POLAR.EQ.1) THEN
    	  NCOL	= 18
    	ELSE IF (F_OPD.EQ.1) THEN
    	  NCOL	= 13
    	ELSE
    	  NCOL	= 12
    	END IF
    
    
    !!srio	IF (F_WIGGLER.EQ.1) THEN
    !!srio ! C
    !!srio ! C Normal wigger case:
    !!srio ! C read in the wiggler trajectory, tangent and radius, and other parameters
    !!srio ! C
    !!srio #ifdef vms
    !!srio 	  OPEN	(29, FILE=FILE_TRAJ, STATUS='OLD', 
    !!srio      $			FORM='UNFORMATTED', READONLY)
    !!srio #else
    !!srio 	  OPEN	(29, FILE=FILE_TRAJ, STATUS='OLD', 
    !!srio      $			FORM='UNFORMATTED')
    !!srio #endif
    !!srio 	  READ	(29)	NP_TRAJ,PATH_STEP,BENER,RAD_MIN,RAD_MAX,PH1,PH2
    !!srio 	  DO 13 I = 1, NP_TRAJ
    !!srio 	    READ (29)	
    !!srio      $		XIN,YIN,SEEDIN,ANGIN,CIN
    !!srio ! C+++
    !!srio ! C The program will build the splines for generating the stocastic source.
    !!srio ! C the splines are defined by:
    !!srio ! C
    !!srio ! C      Y(X) = G(2,I)+X(I)*(G(3,I)+X(I)*(G(4,I)+X(I)*G(5,I)))
    !!srio ! C
    !!srio ! C which is valid between the interval X(I) and X(I+1)
    !!srio ! C
    !!srio ! C We define the 5 arrays:
    !!srio ! C    Y_X(5,N)    ---> X(Y)
    !!srio ! C    Y_XPRI(5,N) ---> X'(Y)
    !!srio ! C    Y_CURV(5,N) ---> CURV(Y)
    !!srio ! C    Y_PATH(5,N) ---> PATH(Y)
    !!srio ! C    F(1,N) contains the array of Y values where the nodes are located.
    !!srio ! C+++
    !!srio 	    Y_TEMP(I)	= YIN*CONV_FACT			! Convert to user units
    !!srio 	    X_TEMP(I)	= XIN*CONV_FACT			! Convert to user units
    !!srio 	    SEED_Y(1,I)	= SEEDIN
    !!srio 	    ANG_TEMP(I)	= ANGIN
    !!srio 	    C_TEMP(I)	= CIN
    !!srio 	    P_TEMP(I)	= (I-1)*PATH_STEP*CONV_FACT	! Convert to user units
    !!srio ! C
    !!srio ! C Array initialization:
    !!srio ! C
    !!srio 	    Y_X(1,I)	= Y_TEMP(I)
    !!srio 	    Y_XPRI(1,I)	= Y_TEMP(I)
    !!srio 	    Y_CURV(1,I)	= Y_TEMP(I)
    !!srio 	    Y_PATH(1,I)	= Y_TEMP(I)
    !!srio 13	  CONTINUE
    !!srio 	  CLOSE	(29)
    !!srio ! C
    !!srio ! C Generate the (5) splines. Notice that the nodes are always in the first
    !!srio ! C element already.
    !!srio ! C      Y_X     : on input, first row contains nodes.
    !!srio ! C      X_TEMP  : input array to which to fit the splines
    !!srio ! C      NP_TRAJ : # of spline points
    !!srio ! C      IER     : status flag
    !!srio ! C On output:
    !!srio ! C      Y_X(1,*)    : spline nodes
    !!srio ! C      Y_X(2:5,*)  : spline coefficients (relative to X_TEMP)
    !!srio ! C
    !!srio 	  NP_SY	= NP_TRAJ
    !!srio 	  IER	= 1
    !!srio ! C*************************************
    !!srio 	  CALL	PIECESPL(SEED_Y, Y_TEMP,   NP_SY,   IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_X,    X_TEMP,   NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_XPRI, ANG_TEMP, NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_CURV, C_TEMP,   NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_PATH, P_TEMP,   NP_TRAJ, IER)
    !!srio ! C+++
    !!srio ! C Compute the path length to the middle (origin) of the wiggler.
    !!srio ! C We need to know the "center" of the wiggler coordinate.
    !!srio ! C input:     Y_PATH  ---> spline array
    !!srio ! C            NP_TRAJ ---> # of points
    !!srio ! C            Y_TRAJ  ---> calculation point (ind. variable)
    !!srio ! C output:    PATH0   ---> value of Y_PATH at X = Y_TRAJ. If
    !!srio ! C                         Y_TRAJ = 0, then PATH0 = 1/2 length 
    !!srio ! C                         of trajectory.
    !!srio ! C+++
    !!srio 	  Y_TRAJ	= 0.0D0
    !!srio 	  CALL	SPL_INT	(Y_PATH, NP_TRAJ, Y_TRAJ, PATH0, IER)
    !!srio ! C
    !!srio ! C These flags are set because of the original program structure.
    !!srio ! C
    !!srio  	F_PHOT		= 0
    !!srio   	F_COLOR		= 3
    !!srio ! C	FGRID		= 0
    !!srio   	FSOUR		= 3
    !!srio   	FDISTR		= 4
    !!srio 	ELSE IF (F_WIGGLER.EQ.3) THEN
    !!srio ! C
    !!srio ! C Elliptical wiggler case:
    !!srio ! C
    !!srio #ifdef vms
    !!srio 	  OPEN	(29, FILE=FILE_TRAJ, STATUS='OLD', 
    !!srio      $			FORM='UNFORMATTED', READONLY)
    !!srio #else
    !!srio 	  OPEN	(29, FILE=FILE_TRAJ, STATUS='OLD', 
    !!srio      $			FORM='UNFORMATTED')
    !!srio #endif
    !!srio 	  READ	(29)	NP_TRAJ,PATH_STEP,BENER,RAD_MIN,RAD_MAX,PH1,PH2
    !!srio 	  DO 14 I = 1, NP_TRAJ
    !!srio 	    READ (29) XIN,YIN,ZIN,SEEDIN,ANGIN1,ANGIN2,CIN	
    !!srio ! C+++
    !!srio ! C The program will build the splines for generating the stocastic source.
    !!srio ! C the splines are defined by:
    !!srio ! C
    !!srio ! C      Y(X) = G(2,I)+X(I)*(G(3,I)+X(I)*(G(4,I)+X(I)*G(5,I)))
    !!srio ! C
    !!srio ! C which is valid between the interval X(I) and X(I+1)
    !!srio ! C
    !!srio ! C We define the 7 arrays:
    !!srio ! C    Y_X(5,N)    ---> X(Y)
    !!srio ! C    Y_XPRI(5,N) ---> X'(Y)
    !!srio ! C    Y_Z(5,N)    ---> Z(Y)
    !!srio ! C    Y_ZPRI(5,N) ---> Z'(Y)
    !!srio ! C    Y_CURV(5,N) ---> CURV(Y)
    !!srio ! C    Y_PATH(5,N) ---> PATH(Y)
    !!srio ! C    F(1,N) contains the array of Y values where the nodes are located.
    !!srio ! C+++
    !!srio 	    Y_TEMP(I)	= YIN*CONV_FACT			! Convert to user units
    !!srio 	    X_TEMP(I)	= XIN*CONV_FACT			! Convert to user units
    !!srio 	    Z_TEMP(I)	= ZIN*CONV_FACT			! Convert to user units
    !!srio 	    SEED_Y(1,I)	= SEEDIN
    !!srio 	    ANG_TEMP(I)= ANGIN1
    !!srio 	    ANG2_TEMP(I)= ANGIN2
    !!srio 	    C_TEMP(I)	= CIN
    !!srio 	    P_TEMP(I)	= (I-1)*PATH_STEP*CONV_FACT	! Convert to user units
    !!srio ! C
    !!srio ! C Array initialization:
    !!srio ! C
    !!srio 	    Y_X(1,I)	= Y_TEMP(I)
    !!srio 	    Y_XPRI(1,I)	= Y_TEMP(I)
    !!srio 	    Y_Z(1,I)	= Y_TEMP(I)
    !!srio 	    Y_ZPRI(1,I)	= Y_TEMP(I)
    !!srio 	    Y_CURV(1,I)	= Y_TEMP(I)
    !!srio 	    Y_PATH(1,I)	= Y_TEMP(I)
    !!srio 14	  CONTINUE
    !!srio 	  CLOSE	(29)
    !!srio ! C
    !!srio ! C Generate the (7) splines. Notice that the nodes are always in the first
    !!srio ! C element already.
    !!srio ! C      Y_X (or Y_Z)       : on input, first row contains nodes.
    !!srio ! C      X_TEMP (or Z_TEMP) : input array to which fit the splines
    !!srio ! C      NP_TRAJ : # of spline points
    !!srio ! C      IER     : status flag
    !!srio ! C On output:
    !!srio ! C      Y_X(1,*) or (Y_Z(1,*))     : spline nodes
    !!srio ! C      Y_X(2:5,*) (or Y_Z(2:5,*)) : spline coefficients (relative to
    !!srio ! C                                   X_TEMP (or Z_TEMP))
    !!srio ! C
    !!srio 	  NP_SY	= NP_TRAJ
    !!srio 	  IER	= 1
    !!srio ! C*************************************
    !!srio 	  CALL	PIECESPL(SEED_Y, Y_TEMP,   NP_SY,   IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_X,    X_TEMP,   NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_Z,    Z_TEMP,   NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_XPRI, ANG_TEMP, NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_ZPRI, ANG2_TEMP, NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_CURV, C_TEMP,   NP_TRAJ, IER)
    !!srio 	  IER	= 0
    !!srio 	  CALL	CUBSPL	(Y_PATH, P_TEMP,   NP_TRAJ, IER)
    !!srio ! C+++
    !!srio ! C Compute the path length to the middle (origin) of the wiggler.
    !!srio ! C We need to know the "center" of the wiggler coordinate.
    !!srio ! C input:     Y_PATH  ---> spline array
    !!srio ! C            NP_TRAJ ---> # of points
    !!srio ! C            Y_TRAJ  ---> calculation point (ind. variable)
    !!srio ! C output:    PATH0   ---> value of Y_PATH at X = Y_TRAJ. If
    !!srio ! C                         Y_TRAJ = 0, then PATH0 = 1/2 length 
    !!srio ! C                         of trajectory.
    !!srio ! C+++
    !!srio 	  Y_TRAJ	= 0.0D0
    !!srio 	  CALL	SPL_INT	(Y_PATH, NP_TRAJ, Y_TRAJ, PATH0, IER)
    !!srio ! C
    !!srio ! C These flags are set because of the original program structure.
    !!srio ! C
    !!srio  	F_PHOT		= 0
    !!srio   	F_COLOR		= 3
    !!srio ! C	FGRID		= 0
    !!srio   	FSOUR		= 3
    !!srio   	FDISTR		= 4
    !!srio 	ELSE IF (F_WIGGLER.EQ.2) THEN
    !!srio ! C
    !!srio ! C Uudulator case : first read in the CDF's and degree of polarization.
    !!srio ! C
    !!srio #ifdef vms
    !!srio 	  OPEN	(30, FILE=FILE_TRAJ, STATUS='OLD',
    !!srio      $	  	FORM='UNFORMATTED', READONLY)
    !!srio #else
    !!srio 	  OPEN	(30, FILE=FILE_TRAJ, STATUS='OLD',
    !!srio      $	  	FORM='UNFORMATTED')
    !!srio #endif
    !!srio 	  READ	(30)	NE,NT,NP,IANGLE
    !!srio 
    !!srio 	  DO 17 K = 1,NE
    !!srio 17	     READ  	(30)	UENER(K)
    !!srio 	  DO 27 K = 1,NE
    !!srio 	     DO 27 J = 1,NT
    !!srio 27	        READ   	(30)   	UTHETA(J,K)
    !!srio 	  DO 37 K = 1,NE
    !!srio 	     DO 37 J = 1,NT
    !!srio 		DO 37 I = 1,NP
    !!srio 37	           READ	(30)	UPHI(I,J,K)
    !!srio 
    !!srio 	  DO 47 K = 1,NE
    !!srio 47	     READ   	(30) 	CDFW(K)
    !!srio 	  DO 57 K = 1,NE
    !!srio 	     DO 57 J = 1,NT
    !!srio 57	        READ	(30)	CDFZ(J,K)
    !!srio 	  DO 67 K = 1,NE
    !!srio 	     DO 67 J = 1,NT
    !!srio 		DO 67 I = 1,NP
    !!srio 67	           READ	(30)	CDFX(I,J,K)
    !!srio 
    !!srio 	  DO 87 K = 1,NE
    !!srio 	     DO 87 J = 1,NT
    !!srio 		DO 87 I = 1,NP
    !!srio 87	  	   READ	(30)	D_POL(I,J,K)
    !!srio 
    !!srio D	  READ	(30)	(UENER(K), K = 1,NE)
    !!srio D	  READ	(30)	((UTHETA(J,K), J = 1,NT), K = 1,NE)
    !!srio D	  READ	(30)	(((UPHI(I,J,K), I = 1,NP), J = 1,NT), K = 1,NE)
    !!srio D
    !!srio D	  READ	(30)	(CDFW(K), K = 1,NE)
    !!srio D	  READ	(30)	((CDFZ(J,K), J = 1,NT), K = 1,NE)
    !!srio D	  READ	(30)	(((CDFX(I,J,K), I = 1,NP), J = 1,NT), K = 1,NE)
    !!srio D
    !!srio D	  READ	(30)	(((D_POL(I,J,K), I = 1,NP), J = 1,NT), K = 1,NE)
    !!srio 
    !!srio 	  CLOSE	(30)
    !!srio ! C
    !!srio ! C These flags are set because of the original program structure.
    !!srio ! C
    !!srio 	  F_PHOT	= 0
    !!srio 	  F_COLOR	= 3
    !!srio ! C	  FGRID		= 0
    !!srio 	  FSOUR		= 3
    !!srio 	  F_COHER	= 1
    !!srio 	ELSE
     	  RAD_MIN	= ABS(R_MAGNET)
     	  RAD_MAX	= ABS(R_MAGNET)
    !!srio 	END IF
    !!srio ! C
    !!srio ! C Prepares some SR variables; the vertical divergence replaces the emittance
    !!srio ! C
    !!srio      	IF (FDISTR.EQ.4.OR.FDISTR.EQ.6) THEN
    !!srio 	  F_COHER = 0
    !!srio 	  IF (R_ALADDIN.LT.0.0D0) THEN
    !!srio 	    POL_ANGLE = -90.0D0
    !!srio 	  ELSE
    !!srio 	    POL_ANGLE = 90.0D0
    !!srio 	  END IF
    !!srio 	END IF
    !!srio 	POL_ANGLE     =   TORAD*POL_ANGLE
    !!srio ! C
    !!srio ! C Saved values of emittance that user inputs.  Write these out to 
    !!srio ! C namelist instead of EPSI/SIGMA.  6/25/93 clw.
    !!srio ! C
    
         	IF (FSOUR.EQ.3) THEN
    	  EPSI_XOLD = EPSI_X
    	  EPSI_ZOLD = EPSI_Z
    	  IF (SIGMAX.NE.0.0D0) THEN
    	    EPSI_X	=   EPSI_X/SIGMAX
    	  ELSE
    	    EPSI_X	=   0.0D0
    	  END IF
    	  IF (SIGMAZ.NE.0.0D0) THEN
    	    EPSI_Z	=   EPSI_Z/SIGMAZ
    	  ELSE
    	    EPSI_Z	=   0.0D0
    	  END IF
         	END IF
    
         	PHOTON(1) = PH1
         	PHOTON(2) = PH2
         	PHOTON(3) = PH3
         	PHOTON(4) = PH4
         	PHOTON(5) = PH5
         	PHOTON(6) = PH6
         	PHOTON(7) = PH7
         	PHOTON(8) = PH8
         	PHOTON(9) = PH9
         	PHOTON(10) = PH10
    ! C
    ! C sets up the acceptance/rejection method for optimizing source
    ! C notice that this is acceptable ONLY for random sources
    ! C
         	IF ( F_BOUND_SOUR.EQ.1 .AND. FGRID.EQ.0 ) THEN
              ITMP=-1
         	  CALL 	SOURCE_BOUND (XDUM,YDUM,ITMP)
         	END IF
    ! C
    ! C tests for grids
    ! C
    	IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    	  SIGXL(1) = SIGXL1
    	  SIGXL(2) = SIGXL2
    	  SIGXL(3) = SIGXL3
    	  SIGXL(4) = SIGXL4
    	  SIGXL(5) = SIGXL5
    	  SIGXL(6) = SIGXL6
    	  SIGXL(7) = SIGXL7
    	  SIGXL(8) = SIGXL8
    	  SIGXL(9) = SIGXL9
    	  SIGXL(10) = SIGXL10
    ! C
    	  SIGZL(1) = SIGZL1
    	  SIGZL(2) = SIGZL2
    	  SIGZL(3) = SIGZL3
    	  SIGZL(4) = SIGZL4
    	  SIGZL(5) = SIGZL5
    	  SIGZL(6) = SIGZL6
    	  SIGZL(7) = SIGZL7
    	  SIGZL(8) = SIGZL8
    	  SIGZL(9) = SIGZL9
    	  SIGZL(10) = SIGZL10
    ! C
    ! C The next two assignments are just for convenience of the original program 
    ! C structure.
    ! C
    	  FSOUR	= 4
    	  FDISTR = 7
    	END IF
    
    	IF (F_PHOT.EQ.1) THEN
    	  IF (F_COLOR.EQ.1) THEN
    	    PHOTON(1)	=   TOANGS/PHOTON(1)
    	  ELSE IF (F_COLOR.EQ.2.OR.F_COLOR.EQ.4) THEN
    	    DO  21 I=1,N_COLOR
    	      PHOTON(I)	=   TOANGS/PHOTON(I)
    21	    CONTINUE
    	  ELSE IF (F_COLOR.EQ.3) THEN
    	    DO 31 I=1,2
    	      PHOTON(I)	=   TOANGS/PHOTON(I)
    31	    CONTINUE
         	  END IF
         	END IF
    ! C
    ! C If the S.R. case has been chosen, set up the subroutine for the
    ! C  vertical distribution.
    ! C
    ! Csrio	IF (FDISTR.EQ.6) CALL ALADDIN1 (DUMMY,DUMMY,-1,IER)
    ! Csrio     	IF (FDISTR.EQ.4) 
    ! Csrio     $	CALL WHITE (RAD_MIN,RAD_MAX,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,0)
    ! C
    ! C Calculate the total number of rays.
    ! C
    102	CONTINUE
    
    	IF (FDISTR.NE.5) THEN
    	  NMOM	= IDO_VX * IDO_VZ
    	ELSE
    	  NMOM	= (N_CONE * N_CIRCLE) 
    	  IDO_VX = N_CIRCLE
    	  IDO_VZ = N_CONE
    	END IF
    	NSPACE	= IDO_X_S * IDO_Y_S * IDO_Z_S
    
    	IF (FGRID.EQ.0) THEN
    	  NTOTAL	= NPOINT
    	ELSE IF (FGRID.EQ.1) THEN
    	  NTOTAL	= NSPACE * NMOM
    	ELSE IF (FGRID.EQ.2) THEN
    	  NTOTAL	= NSPACE * NPOINT
    	ELSE IF (FGRID.EQ.3) THEN
    	  NTOTAL	= NPOINT * NMOM
    	ELSE IF (FGRID.EQ.4) THEN
    	  NTOTAL	= IDO_XL * NPOINT * IDO_ZL * NPOINT
    	ELSE IF (FGRID.EQ.5) THEN
    	  NTOTAL	= IDO_XL * IDO_XN * IDO_ZL * IDO_ZN
    	END IF
    
            ITMP=0
         	IF (NTOTAL.LE.0)	CALL LEAVE ('SOURCE','NPOINT = 0',ITMP)
    !!     	IF (NTOTAL.GT.N_DIM)	CALL LEAVE ('SOURCE','Too many rays.',ITMP)
    ! C
    ! C Compute the steps and iteration count limits for the grid generation.
    ! C
    	IF (IDO_X_S.GT.1)	STEP_X	= 1.0D0/(IDO_X_S - 1)
    	IF (IDO_Y_S.GT.1)	STEP_Y	= 1.0D0/(IDO_Y_S - 1)
    	IF (IDO_Z_S.GT.1)	STEP_Z	= 1.0D0/(IDO_Z_S - 1)
    	IF (IDO_VX.GT.1)	STEP_VX	= 1.0D0/(IDO_VX - 1)
    	IF (IDO_VZ.GT.1)	STEP_VZ	= 1.0D0/(IDO_VZ - 1)
    	IF (IDO_XN.GT.1)	STEP_XN = 1.0D0/(IDO_XN - 1)
    	IF (IDO_ZN.GT.1)	STEP_ZN = 1.0D0/(IDO_ZN - 1)
    	CL_X	= (IDO_X_S - 1) / 2.0D0
    	CL_Y	= (IDO_Y_S - 1) / 2.0D0
    	CL_Z	= (IDO_Z_S - 1) / 2.0D0
    	CL_VX	= (IDO_VX - 1) / 2.0D0
    	CL_VZ	= (IDO_VZ - 1) / 2.0D0
    	CL_XN	= (IDO_XN - 1) / 2.0D0
    	CL_ZN	= (IDO_ZN - 1) / 2.0D0
    ! C
    ! C First fill out a "typical" part of the GRID direction.
    ! C
    	INDEXMOM	= 0	
    	IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
    	  DO 41 I = 1, NPOINT
    	    GRID (4,I)	= WRAN (ISTAR1)
    	    GRID (6,I)	= WRAN (ISTAR1)
    41	  CONTINUE
    	  INDEXMOM	= NPOINT
    	ELSE IF (FGRID.EQ.1.OR.FGRID.EQ.3) THEN
    !!srio 	  DO 51 C_VX = -CL_VX, CL_VX
    	  DO 51 C_VX = -INT(CL_VX), INT(CL_VX)
    !!srio 	    DO 61 C_VZ = -CL_VZ, CL_VZ
    	    DO 61 C_VZ = -INT(CL_VZ), INT(CL_VZ)
    	      INDEXMOM	= INDEXMOM + 1
    	      GRID (4,INDEXMOM)	= C_VX * STEP_VX + 0.5D0
    	      GRID (6,INDEXMOM)	= C_VZ * STEP_VZ + 0.5D0
    61	    CONTINUE
    51	  CONTINUE
    ! C	  IF (FDISTR.EQ.5) THEN
    ! C	    INDEXMOM = INDEXMOM + 1
    ! C	    GRID (4,INDEXMOM)	= 0.0D0
    ! C	    GRID (6,INDEXMOM)	= -1.0D0
    ! C	  END IF
    	ELSE IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    	  DO 71 I = 1, IDO_XL
    	    IF (FGRID.EQ.4) THEN
    	      DO 81 J = 1, NPOINT
    	        INDEXMOM		= INDEXMOM + 1
    	        GRID(1,INDEXMOM)	= SIGXL(I)
    	        GRID(2,INDEXMOM)	= WRAN (ISTAR1)
    	        GRID(4,INDEXMOM)	= WRAN (ISTAR1)
    81	      CONTINUE
    	    ELSE
    !!srio	      DO 91 C_XN = -CL_XN, CL_XN
    	      DO 91 C_XN = -INT(CL_XN), INT(CL_XN)
    	        INDEXMOM		= INDEXMOM + 1
    	        GRID(1,INDEXMOM)	= SIGXL(I)
    	        GRID(2,INDEXMOM)	= WRAN (ISTAR1)
    	        GRID(4,INDEXMOM)	= C_XN * STEP_XN + 0.5D0
    91 	      CONTINUE
    	    END IF
    71 	  CONTINUE
    	END IF
    ! C
    ! C Now fill out the entire GRID.
    ! C
    	INDEXSPA = 0
    	IF (FGRID.EQ.0) THEN
    	  DO 103 I = 1, NPOINT
    	    GRID (1,I)	= WRAN (ISTAR1)
    	    GRID (2,I)	= WRAN (ISTAR1)
    	    GRID (3,I)	= WRAN (ISTAR1)
    103	  CONTINUE
    	  INDEXSPA = NPOINT
    	ELSE IF (FGRID.EQ.3) THEN
    	  DO 113 I = 1, NPOINT
    	    TEMPX = WRAN (ISTAR1)
    	    TEMPY = WRAN (ISTAR1)
    	    TEMPZ = WRAN (ISTAR1)
    	    DO 121 J = 1, INDEXMOM
    	      INDEXSPA	= INDEXSPA + 1
    	      GRID(1,INDEXSPA)	= TEMPX
    	      GRID(2,INDEXSPA)	= TEMPY
    	      GRID(3,INDEXSPA)	= TEMPZ
    	      GRID(4,INDEXSPA)	= GRID (4,J)
    	      GRID(6,INDEXSPA)	= GRID (6,J)
    121	    CONTINUE
    113	  CONTINUE
    	ELSE IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
    !!srio	  DO 131 C_X = -CL_X, CL_X
    !!srio	    DO 141 C_Y = -CL_Y, CL_Y
    !!srio	      DO 151 C_Z = -CL_Z, CL_Z
    	  DO 131 C_X = -INT(CL_X), INT(CL_X)
    	    DO 141 C_Y = -INT(CL_Y), INT(CL_Y)
    	      DO 151 C_Z = -INT(CL_Z), INT(CL_Z)
    		DO 161 J = 1, INDEXMOM
    	      	  INDEXSPA	= INDEXSPA + 1
    	      	  GRID (1,INDEXSPA)	= C_X * STEP_X + 0.5D0
    	      	  GRID (2,INDEXSPA)	= C_Y * STEP_Y + 0.5D0
    		  GRID (3,INDEXSPA)	= C_Z * STEP_Z + 0.5D0
    		  GRID (4,INDEXSPA)	= GRID (4,J)
    		  GRID (6,INDEXSPA)	= GRID (6,J)
    161		CONTINUE
    151	      CONTINUE
    141	    CONTINUE
    131	  CONTINUE
    	ELSE IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    	  DO 171 I = 1, IDO_ZL
    	    IF (FGRID.EQ.4) THEN
    	      DO 181 J = 1, NPOINT
    	        TEMP = WRAN (ISTAR1)
    	        DO 191 K = 1, IDO_XL*NPOINT
    	          INDEXSPA		= INDEXSPA + 1
    	          GRID(1,INDEXSPA)	= GRID(1,K)
    		      GRID(2,INDEXSPA)	= GRID(2,K)
    	          GRID(4,INDEXSPA)	= GRID(4,K)
    	          GRID(3,INDEXSPA)	= SIGZL(I)
    	          GRID(6,INDEXSPA)	= TEMP
    191	        CONTINUE
    181	      CONTINUE
    	    ELSE
    !!srio	      DO 201 C_ZN = -CL_ZN, CL_ZN
    	      DO 201 C_ZN = -INT(CL_ZN), INT(CL_ZN)
    	        TEMP	= C_ZN * STEP_ZN + 0.5D0
    	        DO 211 K = 1, IDO_XL*IDO_XN
    	          INDEXSPA		= INDEXSPA + 1
    	          GRID(1,INDEXSPA)	= GRID(1,K)
    		  	  GRID(2,INDEXSPA)	= GRID(2,K)
    	          GRID(4,INDEXSPA)	= GRID(4,K)
    	          GRID(3,INDEXSPA)	= SIGZL(I)
    	          GRID(6,INDEXSPA)	= TEMP
    211	        CONTINUE
    201	      CONTINUE
    	    END IF
    171	  CONTINUE
    	END IF	
    ! C
    ! C---------------------------------------------------------------------
    ! C           POSITIONS
    ! C
    ! C
         	KK	=   0
         	MM	=   0
    	DO 10000 ITIK=1,NTOTAL
         	KK	=  KK + 1
         	IF (KK.EQ.250) THEN
         	  ITOTRAY = KK + MM*250
         	 IF (MM.EQ.0) THEN
         	  WRITE(6,*)'Generated ',ITOTRAY,' rays out of ',NTOTAL
         	 ELSE
         	  WRITE(6,*)'          ',ITOTRAY
         	 END IF
         	  KK = 0
         	  MM = MM + 1
         	END IF
    ! C
    ! C The following entry point is for the "optimized" source
    ! C
    ! C
    10001	CONTINUE
    !!srio ! C
    !!srio ! C
    !!srio ! C The following interpolation is done for wiggler: GRID -> Y, Y -> X, Y -> Z, 
    !!srio ! C Y -> X', Y -> RAD, Y -> path length from middle of wiggler.
    !!srio ! C 
    !!srio ! C Either wiggler case
    !!srio ! C
    !!srio     	IF ((F_WIGGLER.EQ.1).OR.(F_WIGGLER.EQ.3)) THEN
    !!srio ! C
    !!srio ! C Normal wiggler case
    !!srio ! C
    !!srio 	  IF (F_WIGGLER.EQ.1) THEN
    !!srio 	    ARG_Y = GRID(2,ITIK)
    !!srio 	    CALL SPL_INT (SEED_Y, NP_SY,   ARG_Y,  Y_TRAJ,    IER)
    !!srio 	    CALL SPL_INT (Y_X,    NP_TRAJ, Y_TRAJ, X_TRAJ,    IER)
    !!srio 	    CALL SPL_INT (Y_XPRI, NP_TRAJ, Y_TRAJ, ANGLE,     IER)
    !!srio 	    CALL SPL_INT (Y_CURV, NP_TRAJ, Y_TRAJ, CURV,      IER)
    !!srio 	    CALL SPL_INT (Y_PATH, NP_TRAJ, Y_TRAJ, EPSI_PATH, IER)
    !!srio           END IF
    !!srio ! C
    !!srio ! C Elliptical wiggler case
    !!srio ! C
    !!srio           IF (F_WIGGLER.EQ.3) THEN
    !!srio 	    ARG_Y = GRID(2,ITIK)
    !!srio 	    CALL SPL_INT (SEED_Y, NP_SY,   ARG_Y,  Y_TRAJ,    IER)
    !!srio 	    CALL SPL_INT (Y_X,    NP_TRAJ, Y_TRAJ, X_TRAJ,    IER)
    !!srio 	    CALL SPL_INT (Y_Z,    NP_TRAJ, Y_TRAJ, Z_TRAJ,    IER)
    !!srio 	    CALL SPL_INT (Y_XPRI, NP_TRAJ, Y_TRAJ, ANGLE1,    IER)
    !!srio 	    CALL SPL_INT (Y_ZPRI, NP_TRAJ, Y_TRAJ, ANGLE2,    IER)
    !!srio 	    CALL SPL_INT (Y_CURV, NP_TRAJ, Y_TRAJ, CURV,      IER)
    !!srio 	    CALL SPL_INT (Y_PATH, NP_TRAJ, Y_TRAJ, EPSI_PATH, IER)
    !!srio           END IF
    !!srio ! C
    !!srio 	  EPSI_PATH	= EPSI_PATH - PATH0	! now refer to wiggler's origin
    !!srio 	  IF (CURV.LT.0) THEN
    !!srio 	    POL_ANGLE	= 90.0D0		! instant orbit is CW
    !!srio 	  ELSE
    !!srio 	    POL_ANGLE	= -90.0D0		!		   CCW
    !!srio 	  END IF
    !!srio 	  IF (CURV.EQ.0) THEN
    !!srio 	    R_MAGNET	= 1.0D+20
    !!srio 	  ELSE
    !!srio 	    R_MAGNET	= ABS(1.0D0/CURV)
    !!srio 	  END IF
    !!srio 	  POL_ANGLE 	= TORAD*POL_ANGLE
    !!srio ! C above statement added 24 march 1992 to change POL_ANGLE to radians. clw.
    !!srio ! C
    !!srio     	ELSE IF (FSOURCE_DEPTH.EQ.4) THEN		! Synchrontron depth
    !!srio      	  ANGLE		=   GRID(2,ITIK) * (HDIV1 + HDIV2) - HDIV2
    !!srio 	  EPSI_PATH	=   ABS(R_ALADDIN)*ANGLE
    !!srio ! C
    !!srio ! C Undulator case : first interpolate for the photon energy.
    !!srio ! C
    !!srio     	ELSE IF (F_WIGGLER.EQ.2) THEN
    !!srio 	  ESEED		= GRID(2,ITIK)* CDFW(NE)
    !!srio 	  DO 221 K = 1, NE-1
    !!srio 	    IF (ESEED.LE.CDFW(K+1)) GO TO 510
    !!srio 221	  CONTINUE
    !!srio 510    	  DK		= (ESEED - CDFW(K))/(CDFW(K+1) - CDFW(K))
    !!srio 	  KI		= K
    !!srio 	  PENERGY	= UENER(K) + DK*(UENER(K+1) - UENER(K))
    !!srio 	  Q_WAVE	= TWOPI*PENERGY/TOCM
    !!srio ! C
    !!srio ! C then interpolate for theta (Z').
    !!srio ! C
    !!srio 	  ZSEED		= GRID(6,ITIK) 
    !!srio 
    !!srio 	  INDEX	= 1
    !!srio 	  DO 231 K = KI, KI+1
    !!srio 	      CZMAX	= ZSEED*CDFZ(NT,K)
    !!srio 	      DO 241 J = 1, NT-1
    !!srio 	        IF (CZMAX.LE.CDFZ(J+1,K)) THEN
    !!srio 		  JI(INDEX) = J
    !!srio     	  	  DZ(INDEX) = (CZMAX - CDFZ(J,K))/(CDFZ(J+1,K) - 
    !!srio      $							CDFZ(J,K))
    !!srio 		  THE_INT(INDEX) = UTHETA(J,K) + DZ(INDEX)*
    !!srio      $				(UTHETA(J+1,K) - UTHETA(J,K))
    !!srio 		  GO TO 520
    !!srio 	        END IF	
    !!srio 241	      CONTINUE
    !!srio 520	      INDEX = INDEX + 1
    !!srio 231	  CONTINUE
    !!srio 
    !!srio 	  THETA	= THE_INT(1) + DK*(THE_INT(2)-THE_INT(1))
    !!srio 
    !!srio ! C
    !!srio ! C Finally interpolate for phi (X').
    !!srio ! C
    !!srio 	  XSEED		= GRID(4,ITIK) 
    !!srio 
    !!srio 	  INDEX	= 1
    !!srio 	  DO 251 K = KI, KI+1
    !!srio 	    JNOW = JI(K-KI+1)
    !!srio 	    DO 261 J = JNOW, JNOW + 1
    !!srio 	        CXMAX	= XSEED * CDFX(NP,J,K)
    !!srio 	        DO 271 I = 1, NP-1
    !!srio 	          IF (CXMAX.LE.CDFX(I+1,J,K)) THEN
    !!srio 		    II(INDEX) = I
    !!srio     	  	    DX(INDEX) = (CXMAX - CDFX(I,J,K))
    !!srio      $				/(CDFX(I+1,J,K) - CDFX(I,J,K))
    !!srio 	  	    PHI_INT(INDEX) = UPHI(I,J,K) + DX(INDEX)*
    !!srio      $					(UPHI(I+1,J,K) - UPHI(I,J,K))
    !!srio 		    GO TO 530
    !!srio 	          END IF	
    !!srio 271	        CONTINUE
    !!srio 530	      INDEX = INDEX + 1
    !!srio 261	    CONTINUE
    !!srio 251	  CONTINUE
    !!srio 
    !!srio 	  PHI1 = PHI_INT(1) + DZ(1)*(PHI_INT(2) - PHI_INT(1))
    !!srio 	  PHI2 = PHI_INT(3) + DZ(2)*(PHI_INT(4) - PHI_INT(3))
    !!srio 	  PHI  = PHI1 + DK*(PHI2 - PHI1)
    !!srio 
    !!srio ! C
    !!srio ! C Also the degree of polarization.
    !!srio ! C
    !!srio 
    !!srio ! C ++++
    !!srio ! C
    !!srio ! C BEGIN BUG BUG BUG BUG (Tue Apr  8 21:25:51 CDT 1997)
    !!srio ! C
    !!srio ! C DELTAI, DELTAJ and DELTAK are used uninitialized here, and I have no 
    !!srio ! C idea what these are supposed represent. I'm setting it to zero for 
    !!srio ! C now, which is what most compilers do (not F77 standard however, and
    !!srio ! C on some systems, you understandably get garbage). Also, fixed THETA3
    !!srio ! C calculation (was THETA4 -- typo). -- MK
    !!srio ! C
    !!srio 	  DELTAI = 0.0D0
    !!srio 	  DELTAJ = 0.0D0
    !!srio 	  DELTAK = 0.0D0
    !!srio ! C
    !!srio ! C END BUG  BUG
    !!srio ! C
    !!srio ! C ---
    !!srio 	  THETA1	= D_POL(I,J,K) + (D_POL(I,J,K+1)     - D_POL(I,J,K))*		DELTAK
    !!srio 	  THETA2	= D_POL(I,J+1,K) + (D_POL(I,J+1,K+1)   - D_POL(I,J+1,K))*		DELTAK
    !!srio 	  THETA3	= D_POL(I+1,J,K) + (D_POL(I+1,J,K+1)   - D_POL(I+1,J,K))*		DELTAK
    !!srio 	  THETA4	= D_POL(I+1,J+1,K) + (D_POL(I+1,J+1,K+1) - D_POL(I+1,J+1,K))* 	DELTAK
    !!srio 	  PHI1		= THETA1 + (THETA2-THETA1)*DELTAJ	
    !!srio 	  PHI2		= THETA3 + (THETA4-THETA3)*DELTAJ	
    !!srio 	  POL_DEG	= PHI1 + (PHI2-PHI1)*DELTAI
    !!srio ! C
    !!srio 	  POL_ANGLE	= 90.0D0
    !!srio 	  EPSI_PATH	= 0.0D0
    !!srio 	  I_CHANGE	= 1
    !!srio 	  POL_ANGLE 	= TORAD*POL_ANGLE
    !!srio ! C above statement added 24 march 1992 to change POL_ANGLE to radians. clw.
    !!srio ! C
    !!srio ! C If the cdf's are in polar coordinates switch them to cartesian angles.
    !!srio ! C
    !!srio 	  IF (IANGLE.EQ.1) THEN
    !!srio 	    A_Z = ASIN(SIN(THETA)*SIN(PHI))
    !!srio 	    A_X = ACOS(COS(THETA)/COS(A_Z))
    !!srio 	    THETA	= A_Z
    !!srio 	    PHI		= A_X
    !!srio 	  END IF
    !!srio ! C
    !!srio ! C Decide in which quadrant THETA and PHI are.
    !!srio ! C
    !!srio 	  IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
    !!srio 	    IF (WRAN(ISTAR1).LT.0.5)	PHI = -PHI
    !!srio 	    IF (WRAN(ISTAR1).LT.0.5)	THETA = -THETA
    !!srio 	  END IF
    !!srio        END IF                          !Undulator ends.
    
    	GO TO (1,2,3,4,5,5,7), FSOUR+1
    
    1	CONTINUE
    ! C
    ! C Point source **
    ! C
    	GO TO 111
    
    2	CONTINUE
    ! C
    ! C Rectangular source 
    ! C
    	XXX 		= (-1.0D0 + 2.0D0*GRID(1,ITIK))*WXSOU/2
    	ZZZ 		= (-1.0D0 + 2.0D0*GRID(3,ITIK))*WZSOU/2
    	GO TO 111
    
    3	CONTINUE
    ! C
    ! C Elliptical source **
    ! C Uses a transformation algorithm to generate a uniform variate distribution
    ! C
    	IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
    	  PHI		= TWOPI*GRID(1,ITIK)*(IDO_X_S-1)/IDO_X_S
    	ELSE
    	  PHI 		= TWOPI*GRID(1,ITIK)
    	END IF
    	RADIUS 		= SQRT(GRID(3,ITIK))
    	XXX 		= WXSOU*RADIUS*COS(PHI)
    	ZZZ 		= WZSOU*RADIUS*SIN(PHI)
    	GO TO 111
    
    4	CONTINUE
    ! C
    ! C Gaussian -- In order to accomodate the generation nof finite emittance
    ! C beams, we had to remove the 'grid' case.
    ! C 
    	ARG_X 		= GRID(1,ITIK)
    	ARG_Z 		= GRID(3,ITIK)
    ! C
    ! C Compute the actual distance (EPSI_W*) from the orbital focus
    ! C
    	EPSI_WX		= EPSI_DX + EPSI_PATH
    	EPSI_WZ		= EPSI_DZ + EPSI_PATH
         	CALL GAUSS (SIGMAX, EPSI_X, EPSI_WX, XXX, E_BEAM(1), istar1)
         	CALL GAUSS (SIGMAZ, EPSI_Z, EPSI_WZ, ZZZ, E_BEAM(3), istar1)
    !!srio ! C
    !!srio ! C For normal wiggler, XXX is perpendicular to the electron trajectory at 
    !!srio ! C the point defined by (X_TRAJ,Y_TRAJ,0).
    !!srio ! C
    !!srio 	IF (F_WIGGLER.EQ.1) THEN
    !!srio 	  YYY	= Y_TRAJ - XXX*SIN(ANGLE)
    !!srio 	  XXX	= X_TRAJ + XXX*COS(ANGLE)
    !!srio 	  GO TO 550
    !!srio 	ELSE IF (F_WIGGLER.EQ.2) THEN
    !!srio 	  ANGLEX	= E_BEAM(1) + PHI
    !!srio 	  ANGLEV	= E_BEAM(3) + THETA
    !!srio 	  DIREC(1)	= TAN(ANGLEX)
    !!srio 	  DIREC(2)	= 1.0D0
    !!srio 	  DIREC(3)	= TAN(ANGLEV)/COS(ANGLEX)
    !!srio 	  CALL	NORM	(DIREC,DIREC)
    !!srio 	  GO TO 1111	  
    !!srio         ELSE IF (F_WIGGLER.EQ.3) THEN
    !!srio           VTEMP(1) = XXX
    !!srio           VTEMP(2) = 0.0D0
    !!srio           VTEMP(3) = ZZZ
    !!srio           ANGLE1= -ANGLE1
    !!srio           ANGLE3= 0.0D0
    !!srio           CALL ROTATE(VTEMP,ANGLE3,ANGLE2,ANGLE1,VTEMP)
    !!srio           XXX=X_TRAJ + VTEMP(1)
    !!srio           YYY=Y_TRAJ + VTEMP(2)
    !!srio           ZZZ=Z_TRAJ + VTEMP(3)
    !!srio 	END IF
    	GO TO 111
    
    5 	CONTINUE
    ! C
    ! C Ellipses in phase space (spatial components).
    ! C
    	IF (FGRID.EQ.4) THEN
    	  PHI_X		= TWOPI * GRID(4,ITIK)
    	  PHI_Z		= TWOPI * GRID(6,ITIK)
    	ELSE
    	  PHI_X		= TWOPI * GRID(4,ITIK) * (IDO_XN-1) / IDO_XN
    	  PHI_Z		= TWOPI * GRID(6,ITIK) * (IDO_ZN-1) / IDO_ZN
    	END IF
    	XXX		= GRID(1,ITIK)*SIGMAX*COS(PHI_X)
    	ZZZ		= GRID(3,ITIK)*SIGMAZ*COS(PHI_Z)
    	GO TO 111
    
    7	CONTINUE
    
    !!srio ! C
    !!srio ! C Dense Plasma Source
    !!srio ! C	PLASMA_ANGLE is the full angle source cone opening,
    !!srio ! C	WYSOU is the total length of the source, and
    !!srio ! C	the angular distribution for a ray originating at
    !!srio ! C	any point in the dense plasma source is conical.
    !!srio ! C
    !!srio 	PLASMA_APERTURE = TAN(PLASMA_ANGLE/2.0D0)
    !!srio ! C
    !!srio ! C	The commented out statement for YYY below let to a uniform
    !!srio ! C	depth distribution, with the same number of points being
    !!srio ! C 	generated in any X-Z plane disk irrespective of the y-position.
    !!srio ! C	Since we want the POINT DENSITY in each disk to remain the 
    !!srio ! C 	same, we use the cubic root of the uniformly distributed
    !!srio ! C	random number GRID(2,ITIK) instead of the random number itself.
    !!srio ! C	This is because if (dN/dY)*(1/area) must be constant, Y must
    !!srio ! C	be proportional to the cubic root of N.  Since the probability
    !!srio ! C	of the non-uniformly distributed variable (k*N^(1/3)) must
    !!srio ! C 	be 0 over the interval (0,1) over which N is defined, k is 1
    !!srio ! C	and Y is simply equal to the cube root of N.
    !!srio ! C
    !!srio 	DPS_RAN1	= 	GRID(2,ITIK)
    !!srio 
    !!srio 	DPS_RAN2	= 	WRAN(ISTAR1)
    !!srio 	
    !!srio 	IF (DPS_RAN2.LE.0.5000) THEN
    !!srio 		YYY	=	 (DPS_RAN1**(1/3.D0) - 1)*WYSOU/2.D0
    !!srio 	ELSE
    !!srio 		YYY	=	-(DPS_RAN1**(1/3.D0) - 1)*WYSOU/2.D0
    !!srio 	ENDIF
    !!srio 
    !!srio 	IF (YYY.GT.0.0) THEN
    !!srio 		RMAX	=  PLASMA_ANGLE*(WYSOU/2.D0-YYY)
    !!srio 	ELSE
    !!srio 		RMAX	=  ABS(PLASMA_ANGLE*(YYY+WYSOU/2.D0))
    !!srio 	END IF
    !!srio 
    !!srio 	IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
    !!srio 	  PHI		= TWOPI*GRID(1,ITIK)*(IDO_X_S-1)/IDO_X_S
    !!srio 	ELSE
    !!srio 	  PHI 		= TWOPI*GRID(1,ITIK)
    !!srio 	END IF
    !!srio 	RADIUS 		= SQRT(GRID(3,ITIK))
    !!srio 	XXX 		= RMAX*RADIUS*COS(PHI)
    !!srio 	ZZZ 		= RMAX*RADIUS*SIN(PHI)
    ! C	GOTO 111
    	GOTO 550
    
     111	CONTINUE
    ! C
    ! C---------------------------------------------------------------------
    ! C                      DEPTH
    ! C
    ! C
    	 GO TO (110,220,330,440)  FSOURCE_DEPTH
    ! C
    ! C No depth case.
    ! C
    110	 GO TO 550
    ! C
    ! C Uniform depth distribution
    ! C
     220	YYY 		= (-1.0D0 + 2.0D0*GRID(2,ITIK))*WYSOU/2
    	 GO TO 550
    ! C
    ! C Gaussian depth distribution 
    ! C
    330	ARG_Y 		= GRID(2,ITIK)
    
    	CALL MDNRIS (ARG_Y,YYY,IER)
    	  IF (IER.NE.0) WRITE(6,*)'Warning ! Error in YYY,MNDRIS (SOURCE)'
    
    	YYY 		= YYY*SIGMAY
    
    	 GO TO 550
    ! C
    ! C Synchrotron depth distribution
    ! C
    440	CONTINUE
    !!srio ! CC	R_ALADDIN NEGATIVE FOR COUNTER-CLOCKWISE SOURCE	
    !!srio 	IF (R_ALADDIN.LT.0) THEN
    !!srio 	  YYY		=   (ABS(R_ALADDIN) + XXX) * SIN(ANGLE)
    !!srio 	ELSE
    !!srio 	  YYY 		=   ( R_ALADDIN - XXX) * SIN(ANGLE)
    !!srio 	END IF
    !!srio 	XXX 		=   COS(ANGLE) * XXX +
    !!srio      $				R_ALADDIN * (1.0D0 - COS(ANGLE))
    !!srio 
    550	CONTINUE
    ! C
    ! C---------------------------------------------------------------------
    ! C             DIRECTIONS
    ! C
    ! C   Generates now the direction of the rays.
    ! C
    ! C
    101	CONTINUE
    	I_CHANGE	= 1
    	GO TO (11,11,33,44,55,44,77), FDISTR
    
    11	CONTINUE
    ! C
    ! C   Uniform distribution ( Isotrope emitter ) and cosine  source
    ! C
    ! C   Distinction not ready yet. Not important for small apertures 
    ! C
         	XMAX1 		=   TAN(HDIV1)
         	XMAX2		= - TAN(HDIV2)
         	ZMAX1		=   TAN(VDIV1)
    	ZMAX2		= - TAN(VDIV2)
    	XRAND 		= (GRID(4,ITIK)*(XMAX1 - XMAX2) + XMAX2)
         	ZRAND 		= (GRID(6,ITIK)*(ZMAX1 - ZMAX2) + ZMAX2)
    	THETAR 		= ATAN(SQRT(XRAND**2+ZRAND**2))
        	CALL 	ATAN_2 (ZRAND,XRAND,PHIR)
    	DIREC(1) 	= COS(PHIR)*SIN(THETAR)
         	DIREC(2) 	= COS(THETAR)
    	DIREC(3) 	= SIN(PHIR)*SIN(THETAR)
    ! C     	ARG	=   GRID(6,ITIK)*(SIN(VDIV1) + SIN(VDIV2)) - SIN(VDIV2)
    ! C     	PHIR	=   GRID(4,ITIK)*(HDIV1 + HDIV2) - HDIV1
    ! C     	THETAR  =   ASIN(ARG)
    ! C     	DIREC(1)	=   SIN(PHIR)*COS(THETAR)
    ! C     	DIREC(2)	=   COS(PHIR)*COS(THETAR)
    ! C     	DIREC(3)	=   SIN(THETAR)
    	GO TO 1111
    
    33 	CONTINUE
    ! C
    ! C Gaussian emitter 
    ! C Note : an emitter cannot have an angular gaussian distribution, as a
    ! C gaussian is defined from -infin. to + infin. It might be an useful
    ! C approximation in several cases. This program uses a gaussian 
    ! C distribution onto an ideal image plane, independent in x and z. This 
    ! C approximation will not break down for large sigma.
    ! C
    	ARG_VX 		= GRID(4,ITIK)
    	ARG_VZ 		= GRID(6,ITIK)
    
    	CALL MDNRIS (ARG_VX,DIR_X,IER)
    	  IF (IER.NE.0) WRITE(6,*) 'Warning !Error in DIR_X:MNDRIS(SOURCE)'
    
    	DIREC(1) 	= DIR_X*SIGDIX
    
    	CALL MDNRIS (ARG_VZ,DIR_Z,IER)
    	  IF (IER.NE.0) WRITE(6,*)'Warning !Error in DIR_Z:MNDRIS(SOURCE)'
    
    	DIREC(3) 	= DIR_Z*SIGDIZ
    	DIREC(2) 	= 1.0D0
    
    	CALL NORM (DIREC,DIREC)
    
    	GO TO 1111
    
    44	CONTINUE
    !!srio ! C
    !!srio ! C Synchrotron source 
    !!srio ! C Note. The angle of emission IN PLANE is the same as the one used
    !!srio ! C before. This will give rise to a source curved along the orbit.
    !!srio ! C The elevation angle is instead characteristic of the SR distribution.
    !!srio ! C The electron beam emittance is included at this stage. Note that if
    !!srio ! C EPSI = 0, we'll have E_BEAM = 0.0, with no changes.
    !!srio ! C
    !!srio     	IF (F_WIGGLER.EQ.3) ANGLE=0        ! Elliptical Wiggler.
    !!srio      	ANGLEX		=   ANGLE + E_BEAM(1)
    !!srio 	DIREC(1) 	=   TAN(ANGLEX)
    !!srio      	IF (R_ALADDIN.LT.0.0D0) DIREC(1) = - DIREC(1)
    !!srio 	DIREC(2) 	=   1.0D0
    !!srio 	ARG_ANG 	=   GRID(6,ITIK)
    !!srio ! C
    !!srio ! C In the case of SR, we take into account the fact that the electron
    !!srio ! C trajectory is not orthogonal to the field. This will give a correction
    !!srio ! C to the photon energy.  We can write it as a correction to the 
    !!srio ! C magnetic field strength; this will linearly shift the critical energy
    !!srio ! C and, with it, the energy of the emitted photon.
    !!srio ! C
    !!srio      	 E_TEMP(3)	=   TAN(E_BEAM(3))/COS(E_BEAM(1))
    !!srio      	 E_TEMP(2)	=   1.0D0
    !!srio      	 E_TEMP(1)	=   TAN(E_BEAM(1))
    !!srio      	 CALL	NORM	(E_TEMP,E_TEMP)
    !!srio      	 CORREC	=   SQRT(1.0D0-E_TEMP(3)**2)
    !!srio 4400    IF (FDISTR.EQ.6) THEN
    !!srio ! Csrio	  CALL ALADDIN1 (ARG_ANG,ANGLEV,F_POL,IER)
    !!srio      	  Q_WAVE	=   TWOPI*PHOTON(1)/TOCM*CORREC
    !!srio      	  POL_DEG	=   ARG_ANG
    !!srio      	ELSE IF (FDISTR.EQ.4) THEN
    !!srio      	  ARG_ENER	=   WRAN (ISTAR1)
    !!srio 	  RAD_MIN	=   ABS(R_MAGNET)
    !!srio ! Csrio     	  CALL WHITE 
    !!srio ! Csrio     $	      (RAD_MIN,CORREC,ARG_ENER,ARG_ANG,Q_WAVE,ANGLEV,POL_DEG,1)
    !!srio      	END IF
    !!srio       	IF (ANGLEV.LT.0.0) I_CHANGE = -1
    !!srio      	ANGLEV		=   ANGLEV + E_BEAM(3)
    !!srio ! C
    !!srio ! C Test if the ray is within the specified limits
    !!srio ! C
    !!srio      	IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
    !!srio      	 IF (ANGLEV.GT.VDIV1.OR.ANGLEV.LT.-VDIV2) THEN
    !!srio      	  ARG_ANG = WRAN(ISTAR1)
    !!srio ! C
    !!srio ! C If it is outside the range, then generate another ray.
    !!srio ! C
    !!srio      	  GO TO 4400
    !!srio      	 END IF
    !!srio      	END IF
    !!srio 	DIREC(3) 	=   TAN(ANGLEV)/COS(ANGLEX)
    !!srio     	IF (F_WIGGLER.EQ.3) THEN
    !!srio            CALL ROTATE (DIREC, ANGLE3,ANGLE2,ANGLE1,DIREC)
    !!srio         END IF
    !!srio      	CALL	NORM	(DIREC,DIREC)
         	GO TO 1111
    55	CONTINUE
    ! C   Now generates a set of rays along a cone centered about the normal,
    ! C   plus a ray along the normal itself.
    ! C     	
    	IF (FGRID.EQ.1.OR.FGRID.EQ.3) THEN
    	  ANGLE	=   TWOPI*GRID(4,ITIK)*(IDO_VX-1)/IDO_VX
    	ELSE
    	  ANGLE	=   TWOPI*GRID(4,ITIK)
    	END IF
    ! C temp fix -- 16 Jan 1987
    ! C     	  ANG_CONE	=   CONE_MIN + 
    ! C     $			(CONE_MAX - CONE_MIN)*GRID(6,ITIK)
         	ANG_CONE	=   COS(CONE_MIN) - GRID(6,ITIK)*(COS(CONE_MIN)-COS(CONE_MAX))
         	ANG_CONE	=  ACOS(ANG_CONE)
         	DIREC(1)	=   SIN(ANG_CONE)*COS(ANGLE)
         	DIREC(2)	=   COS(ANG_CONE)
         	DIREC(3)	=   SIN(ANG_CONE)*SIN(ANGLE)
    ! C
    	GO TO 1111
    
    77	CONTINUE
    ! C
    ! C Ellipses in phase space (momentum components).
    ! C
    	ANGLEX		= GRID(1,ITIK)*SIGDIX*SIN(PHI_X)
    	ANGLEV		= GRID(3,ITIK)*SIGDIZ*SIN(PHI_Z)
    	DIREC(1)	= SIN(ANGLEX)
    	DIREC(3)	= SIN(ANGLEV)
    	DIREC(2)	= SQRT(1.0D0 - DIREC(1)**2 - DIREC(3)**2)
    	GO TO 1111
    
    1111 	CONTINUE
    ! C
    ! C  ---------------------------------------------------------------------
    ! C                 POLARIZATION
    ! C
    ! C   Generates the polarization of the ray. This is defined on the
    ! C   source plane, so that A_VEC is along the X-axis and AP_VEC is along Z-axis.
    ! C   Then care must be taken so that A will be perpendicular to the ray 
    ! C   direction.
    ! C
    ! C   In the case of SR, the value of POL_DEG is set by the call to
    ! C   the module ALADDIN, so that it is possible to take into account the
    ! C   angular dependence of the source polarization.
    ! C
         	 A_VEC(1)		=   1.0D0
         	 A_VEC(2)		=   0.0D0
         	 A_VEC(3)		=   0.0D0
    ! C
    ! C   Rotate A_VEC so that it will be perpendicular to DIREC and with the
    ! C   right components on the plane.
    ! C 
        	CALL	CROSS	(A_VEC,DIREC,A_TEMP)
         	CALL	CROSS	(DIREC,A_TEMP,A_VEC)
         	CALL	NORM	(A_VEC,A_VEC)
    	CALL	CROSS	(A_VEC,DIREC,AP_VEC)
    	CALL	NORM	(AP_VEC,AP_VEC)
    
    	IF (F_POLAR.EQ.1) THEN
    ! C
    ! C   WaNT A**2 = AX**2 + AZ**2 = 1 , instead of A_VEC**2 = 1 .
    ! C
    	 DENOM	= SQRT(1.0D0 - 2.0D0*POL_DEG + 2.0D0*POL_DEG**2)
    	 AX	= POL_DEG/DENOM
    	 CALL	SCALAR	(A_VEC,AX,A_VEC)
    ! C
    ! C   Same procedure for AP_VEC
    ! C
    	 AZ	= (1-POL_DEG)/DENOM
    	 CALL	SCALAR 	(AP_VEC,AZ,AP_VEC)
    	ELSE
    !!srio !!srio ! C
    !!srio !!srio ! C If don't want the full polarization, then POL_DEG is only defined in the 
    !!srio !!srio ! C case of synchrotron radiation.
    !!srio !!srio ! C
    !!srio !!srio 	 IF (FDISTR.EQ.4.OR.FDISTR.EQ.6.OR.F_WIGGLER.NE.0) THEN
    !!srio !!srio 	   IF (WRAN(ISTAR1).GT.POL_DEG) THEN
    !!srio !!srio ! C
    !!srio !!srio ! C A_VEC is along x or z -axis according to POL_DEG.
    !!srio !!srio ! C
    !!srio !!srio 	     A_VEC(1)	= AP_VEC(1)
    !!srio !!srio 	     A_VEC(2)	= AP_VEC(2)
    !!srio !!srio 	     A_VEC(3)	= AP_VEC(3)
    !!srio !!srio 	   END IF
    !!srio !!srio 	 END IF
     	END IF
    ! C
    ! C Now the phases of A_VEC and AP_VEC.
    ! C
    	 IF (F_COHER.EQ.1) THEN
    	   PHASEX	= 0.0D0
    	 ELSE
    	   PHASEX	= WRAN(ISTAR1) * TWOPI
    	 END IF
    	 PHASEZ		= PHASEX + POL_ANGLE*I_CHANGE
    !!srio ! C
    !!srio ! C---------------------------------------------------------------------
    !!srio ! C            PHOTON   ENERGY
    !!srio ! C
    !!srio ! C Generates the wavevector of the ray. Depending on the choice, a
    !!srio ! C single or a set of Q is created.NOTE: units are cm -1
    !!srio ! C
    !!srio ! C
    !!srio ! C
    !!srio ! C In the case of SR, Q_WAVE is already known
    !!srio ! C
    !!srio 	IF (FDISTR.EQ.4.OR.FDISTR.EQ.6.OR.F_WIGGLER.NE.0) GO TO 2050
         	GO TO (2020,2030,2040,2045)	F_COLOR
    
    2010	CONTINUE
    ! C
    ! C Not interested in the photon energy. Set at 0.0
    ! C
         	GO TO 2050
    
    2020	CONTINUE
    ! C
    ! CSingle line. 
    ! C
         	Q_WAVE	=   TWOPI*PHOTON(1)/TOCM
         	GO TO 2050
    
    2030	CONTINUE
    ! C
    ! C Several photon energies (up to 10) with same relative intensities.
    ! C
         	N_TEST	=   WRAN (ISTAR1)*N_COLOR + 1
         	Q_WAVE	=   TWOPI*PHOTON(N_TEST)/TOCM
         	GO TO 2050
    
    2040	CONTINUE
    ! C
    ! C Box photon distribution
    ! C
         	PHOT_CH	=   PHOTON(1) + (PHOTON(2) - PHOTON(1))*WRAN(ISTAR1)
         	Q_WAVE	=   TWOPI*PHOT_CH/TOCM
         	GO TO 2050
    
    2045	CONTINUE
    ! C
    ! C Several photon energies (up to 10) with different relative intensities.
    ! C
    	RELINT(1)	=	RL1
    	RELINT(2)	=	RL2
    	RELINT(3)	=	RL3
    	RELINT(4)	=	RL4
    	RELINT(5)	=	RL5
    	RELINT(6)	=	RL6
    	RELINT(7)	=	RL7
    	RELINT(8)	=	RL8
    	RELINT(9)	=	RL9
    	RELINT(10)	=	RL10
    
    ! C
    ! C	Normalize so that each energy has a probability and so that the sum 
    ! C	of the probabilities of all the energies is 1.
    ! C
    
    	TMP_A = 0
    	DO 2046 J=1,N_COLOR
    		TMP_A = TMP_A + RELINT(J) 	
    2046	CONTINUE	
    	DO 2047 J=1,N_COLOR
    		RELINT(J)=RELINT(J)/TMP_A
    2047	CONTINUE
    
    ! C
    ! C	Arrange the probabilities so that they comprise the (0,1) interval,
    ! C	e.g. (energy1,0.3), (energy2, 0.1), (energy3, 0.6) is translated to
    ! C	0.0, 0.3, 0.4, 1.0. Then a random number falling in an interval
    ! C	assigned to a certain energy results in the ray being assigned that
    ! C	photon energy.
    ! C
    	TMP_B = 0
    	DO 2048 J=1,N_COLOR
    		TMP_B = TMP_B + RELINT(J)	
    		PRELINT(J) = TMP_B
    2048	CONTINUE
    
    	DPS_RAN3 = WRAN(ISTAR1)
    	IF (DPS_RAN3.GE.0.AND.DPS_RAN3.LE.PRELINT(1)) THEN
         		Q_WAVE = TWOPI*PHOTON(1)/TOCM
    	END IF
    		
    	DO 2049 J=2,N_COLOR
    	  IF (DPS_RAN3.GT.PRELINT(J-1).AND.DPS_RAN3.LE.PRELINT(J)) THEN
         	    Q_WAVE = TWOPI*PHOTON(J)/TOCM
    	  END IF
    
    2049	CONTINUE
    
         	GO TO 2050
    
    ! C
    ! C Create the final array 
    ! C
    2050	CONTINUE
                    BEGIN (1,ITIK) 	=   XXX
                    BEGIN (2,ITIK) 	=   YYY
                    BEGIN (3,ITIK) 	=   ZZZ
                    BEGIN (4,ITIK) 	=    DIREC(1)
                    BEGIN (5,ITIK) 	=    DIREC(2)
    !!srioTest BEGIN (5,ITIK) 	=    1.0D0
                    BEGIN (6,ITIK) 	=    DIREC(3)
                    BEGIN (7,ITIK)	=   A_VEC(1)
    !!srioTest BEGIN (7,ITIK)	=   1.0D0
                    BEGIN (8,ITIK)	=   A_VEC(2)
                    BEGIN (9,ITIK)	=   A_VEC(3)
                    BEGIN (10,ITIK)	=   1.0D0
                    BEGIN (11,ITIK)	=   Q_WAVE
    !!srio                BEGIN (12,ITIK)	=   FLOAT (ITIK)
                    BEGIN (12,ITIK)	=   ITIK
    	IF (F_POLAR.EQ.1) THEN
                    PHASE (1,ITIK)	=   0.0D0
                    PHASE (2,ITIK)  	=   PHASEX
                    PHASE (3,ITIK)  	=   PHASEZ
                    AP    (1,ITIK)	=   AP_VEC(1)
                    AP    (2,ITIK)	=   AP_VEC(2)
                    AP    (3,ITIK)	=   AP_VEC(3)
    	END IF
    ! C
    ! C All rays are generated. Test for acceptance if optimized source is
    ! C specified.
    ! C
         	IF (F_BOUND_SOUR.EQ.1 .AND. FGRID.EQ.0 ) THEN
         	  SB_POS(1) = XXX
         	  SB_POS(2) = YYY 
         	  SB_POS(3) = ZZZ
         	  ITEST = 1
         	  CALL SOURCE_BOUND (SB_POS, DIREC, ITEST)
         	 IF (ITEST.LT.0) THEN
         	   K_REJ = K_REJ + 1
         	   N_REJ = N_REJ + 1
    ! C     	   WRITE(6,*) 'itest ===',ITEST
         	  IF (K_REJ.EQ.500) THEN
         	    WRITE(6,*)N_REJ,'   rays have been rejected so far.'
         	    WRITE(6,*)ITIK, '                  accepted.'
         	    K_REJ = 0
         	  END IF 
    	  DO 301 J=1,6
         	    GRID(J,ITIK) = WRAN(ISTAR1)
    301    	  CONTINUE
         	  GOTO 10001
         	 END IF
         	END IF
    10000	CONTINUE
    
    	IFLAG	= 0
          	CALL WRITE_OFF(FNAME,BEGIN,PHASE,AP,NCOL,NTOTAL,IFLAG,IOFORM,IERR)
         	IF (IERR.NE.0) THEN
    	    ERRMSG = 'Error Writing File '// FNAME
    	    CALL LEAVE ('SOURCE', ERRMSG, IERR)
    	ENDIF
         	NPOINT = NTOTAL
    	IF (FSOUR.EQ.3) THEN
    ! C
    ! C Reset EPSI_X and EPSI_Z to the values input by the user.
    ! C
    	   EPSI_X = EPSI_XOLD
    	   EPSI_Z = EPSI_ZOLD
    	ENDIF
    
    !! deallocate everything...
    
    IF (allocated(begin)) deallocate(begin)
    IF (allocated(phase)) deallocate(phase)
    IF (allocated(ap)) deallocate(ap)
    !!IF (allocated(seed_y)) deallocate(seed_y)
    !!IF (allocated(y_x)) deallocate(y_x)
    !!IF (allocated(y_xpri)) deallocate(y_xpri)
    !!IF (allocated(y_z)) deallocate(y_z)
    !!IF (allocated(y_zpri)) deallocate(y_zpri)
    !!IF (allocated(y_curv)) deallocate(y_curv)
    !!IF (allocated(y_path)) deallocate(y_path)
    !!IF (allocated(y_temp)) deallocate(y_temp)
    !!IF (allocated(c_temp)) deallocate(c_temp)
    !!IF (allocated(x_temp)) deallocate(x_temp)
    !!IF (allocated(z_temp)) deallocate(z_temp)
    !!IF (allocated(ang_temp)) deallocate(ang_temp)
    !!IF (allocated(p_temp)) deallocate(p_temp)
    !!IF (allocated(ang2_temp)) deallocate(ang2_temp)
    
         	WRITE(6,*)'Exit from SOURCE'
         	RETURN
    END SUBROUTINE SOURCE1
    !
    !
    !

End Module shadow

