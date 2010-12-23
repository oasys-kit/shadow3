# 1 "src/shadow_variables.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "src/shadow_variables.f90"
!----
!---- MODULE: shadow_variables
!----
!---- Main module for shadow
!---- Contains:
!---- 1) the common variables to be passed trought routines
!---- (now called "variables pool")
!---- 2) the routines that need to access these common variables:
!---- rwname
!---- ...
!---- 3) the main routines for source and trace calculations:
!---- source1 (geometrical sources)
!---- ...
!---- 4) internal (private) routines needed by code in 2) and 3):
!---- put_variables, get_variables, source_bound,
!----
!----
!----
!---- Example of usage: see test_shadow.f95 for some tests.
!---- see gen_source (main program for creating a shadow source)
!----
!----

Module shadow_variables
    !---- Use Modules ----!
 use shadow_kind, only : ski, skr, skc
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

    !!-- REAL*8 PI, TWOPI, PIHALF, TODEG, TORAD, ORIGIN
    !!-- REAL*8 X_VRS, Y_VRS, Z_VRS
    !!-- REAL*8 TOCM, TOANGS
    !!--
    ! this is obselete now...
    ! real(kind=kind(1.0d0)), parameter :: N_DIM = 25001
    real(kind=skr), parameter :: pi = 3.141592653589793238462643
    real(kind=skr), parameter :: twopi = 6.283185307179586467925287
    real(kind=skr), parameter :: pihalf = 1.570796326794896619231322
    real(kind=skr), parameter :: todeg =57.295779513082320876798155
    real(kind=skr), parameter :: torad = 0.017453292519943295769237

    real(kind=skr),dimension(3), parameter :: origin = (/0.0,0.0,0.0/)
    real(kind=skr),dimension(3), parameter :: x_vrs = (/1.0,0.0,0.0/)
    real(kind=skr),dimension(3), parameter :: y_vrs = (/0.0,1.0,0.0/)
    ! srio removed parameter as it may change in mirror1
    real(kind=skr),dimension(3) :: z_vrs = (/0.0,0.0,1.0/)

    ! TODO: Change these values with new codata values (see NIST)
    real(kind=skr), parameter :: tocm = 1.239852D-4
    real(kind=skr), parameter :: toangs = 1.239852D+4
    integer(kind=ski), parameter :: aDim=10

    ! now the declaration of the variables in start.00
    !
    ! NOTE: FOR ADDING A NEW VARIABLE, IT SHOULD BE ADDED IN 3 PLACES
    ! 1) here (declaration)
    ! 2) in get_variables ( get variables into the variables pool)
    ! 3) in put_variables ( copy variables from pool to gFile type,
    ! needed for writing end.xx)

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
    character(kind=skc, len=1024) :: FILE_TRAJ
    character(kind=skc, len=1024) :: FILE_SOURCE
    character(kind=skc, len=1024) :: FILE_BOUND
    integer(kind=ski) :: OE_NUMBER
    integer(kind=ski) :: IDUMMY
    real(kind=skr) :: DUMMY
    integer(kind=ski) :: F_NEW

    ! again the same variables encapsulated in a structure
    !, bind(C)
    type, public :: poolSource

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
       character(kind=skc, len=1024) :: FILE_TRAJ
       character(kind=skc, len=1024) :: FILE_SOURCE
       character(kind=skc, len=1024) :: FILE_BOUND
       integer(kind=ski) :: OE_NUMBER
       integer(kind=ski) :: IDUMMY
       real(kind=skr) :: DUMMY
       integer(kind=ski) :: F_NEW

    end type poolSource


    ! now the declaration of the variables in start.xx
    !
    ! NOTE: FOR ADDING A NEW VARIABLE, IT SHOULD BE ADDED IN 3 PLACES
    ! 1) here (declaration)
    ! 2) in get_variablesOE ( get variables into the variables pool)
    ! 3) in put_variablesOE ( copy variables from pool to gFile type,
    ! needed for writing end.xx)


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
    integer(kind=ski) :: NPOINTOE
    !! integer(kind=ski) :: NCOL
    integer(kind=ski) :: N_SCREEN
    !! integer(kind=ski) :: ISTAR1
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
    ! ! character(kind=skc, len=1024) :: FILE_SOURCE
    character(kind=skc, len=1024) :: FILE_RIP
    character(kind=skc, len=1024) :: FILE_REFL
    character(kind=skc, len=1024) :: FILE_MIR
    character(kind=skc, len=1024) :: FILE_ROUGH
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
    character(kind=skc, len=1024) :: FILE_FAC
    integer(kind=ski) :: F_SEGMENT
    integer(kind=ski) :: ISEG_XNUM
    integer(kind=ski) :: ISEG_YNUM
    character(kind=skc, len=1024) :: FILE_SEGMENT
    character(kind=skc, len=1024) :: FILE_SEGP
    real(kind=skr) :: SEG_LENX
    real(kind=skr) :: SEG_LENY
    integer(kind=ski) :: F_KOMA
    character(kind=skc, len=1024) :: FILE_KOMA
    integer(kind=ski) :: F_EXIT_SHAPE
    integer(kind=ski) :: F_INC_MNOR_ANG
    real(kind=skr) :: ZKO_LENGTH
    real(kind=skr) :: RKOMA_CX
    real(kind=skr) :: RKOMA_CY
    integer(kind=ski) :: F_KOMA_CA
    character(kind=skc, len=1024) :: FILE_KOMA_CA
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
    ! integer(kind=ski) :: OE_NUMBER
    ! integer(kind=ski) :: IDUMMY
    ! real(kind=skr) :: DUMMY
    real(kind=skr), dimension(aDim) :: CX_SLIT
    real(kind=skr), dimension(aDim) :: CZ_SLIT
    real(kind=skr), dimension(aDim) :: D_PLATE
    character(kind=skc, len=1024), dimension(aDim) :: FILE_ABS
    character(kind=skc, len=1024), dimension(aDim) :: FILE_SCR_EXT
    integer(kind=ski), dimension(aDim) :: I_ABS
    integer(kind=ski), dimension(aDim) :: I_SCREEN
    integer(kind=ski), dimension(aDim) :: I_SLIT
    integer(kind=ski), dimension(aDim) :: I_STOP
    integer(kind=ski), dimension(aDim) :: K_SLIT
    real(kind=skr), dimension(aDim) :: RX_SLIT
    real(kind=skr), dimension(aDim) :: RZ_SLIT
    integer(kind=ski), dimension(aDim) :: SCR_NUMBER
    real(kind=skr), dimension(aDim) :: SL_DIS
    real(kind=skr), dimension(aDim) :: THICK

    !, bind(C)
    type, public :: poolOE

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
       integer(kind=ski) :: NPOINTOE

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

       character(kind=skc, len=1024) :: FILE_SOURCE
       character(kind=skc, len=1024) :: FILE_RIP
       character(kind=skc, len=1024) :: FILE_REFL
       character(kind=skc, len=1024) :: FILE_MIR
       character(kind=skc, len=1024) :: FILE_ROUGH
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
       character(kind=skc, len=1024) :: FILE_FAC
       integer(kind=ski) :: F_SEGMENT
       integer(kind=ski) :: ISEG_XNUM
       integer(kind=ski) :: ISEG_YNUM
       character(kind=skc, len=1024) :: FILE_SEGMENT
       character(kind=skc, len=1024) :: FILE_SEGP
       real(kind=skr) :: SEG_LENX
       real(kind=skr) :: SEG_LENY
       integer(kind=ski) :: F_KOMA
       character(kind=skc, len=1024) :: FILE_KOMA
       integer(kind=ski) :: F_EXIT_SHAPE
       integer(kind=ski) :: F_INC_MNOR_ANG
       real(kind=skr) :: ZKO_LENGTH
       real(kind=skr) :: RKOMA_CX
       real(kind=skr) :: RKOMA_CY
       integer(kind=ski) :: F_KOMA_CA
       character(kind=skc, len=1024) :: FILE_KOMA_CA
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

       real(kind=skr), dimension(aDim) :: CX_SLIT
       real(kind=skr), dimension(aDim) :: CZ_SLIT
       real(kind=skr), dimension(aDim) :: D_PLATE
       character(kind=skc, len=1024), dimension(aDim) :: FILE_ABS
       character(kind=skc, len=1024), dimension(aDim) :: FILE_SCR_EXT
       integer(kind=ski), dimension(aDim) :: I_ABS
       integer(kind=ski), dimension(aDim) :: I_SCREEN
       integer(kind=ski), dimension(aDim) :: I_SLIT
       integer(kind=ski), dimension(aDim) :: I_STOP
       integer(kind=ski), dimension(aDim) :: K_SLIT
       real(kind=skr), dimension(aDim) :: RX_SLIT
       real(kind=skr), dimension(aDim) :: RZ_SLIT
       integer(kind=ski), dimension(aDim) :: SCR_NUMBER
       real(kind=skr), dimension(aDim) :: SL_DIS
       real(kind=skr), dimension(aDim) :: THICK


    end type poolOE

    ! now the variables in the ex-common blocks not defined in the gfiles
    ! They will be added as far as needed...

    !!-- INTEGER*4 FMIRR,F_TORUS,FCYL,FANG,FSTAT,F_COHER, &
    !!-- FANG1,FSLIT,FGRID,F_NEW,FSOURCE_DEPTH, &
    !!-- FSOUR,FDISTR,FWRITE,F_BRAGG_A, &
    !!-- F_POL,F_POLAR,F_RIPPLE, &
    !!-- F_MOVE,F_HOW,F_G_S,F_READ_RIP,F_R_RAN, &
    !!-- F_GRATING,F_SIDE,F_CENTRAL,F_VIRTUAL, &
    !!-- F_COLOR,F_CONVEX,F_REFLEC,F_RULING, &
    !!-- F_RUL_ABS,F_BOUND_SOUR,F_THICK, &
    !!-- F_EXT,F_PHOT,F_SCREEN,F_PLATE,FSHAPE, &
    !!-- FHIT_C,F_PW,F_MONO,F_DEFAULT,F_REFL, &
    !!-- F_HUNT,F_PHOT_CENT,F_CRYSTAL,F_REFRAC, &
    !!-- F_PW_C,F_OPD,F_WIGGLER,FZP,F_SR_TYPE, &
    !!-- F_ROUGHNESS,FDUMMY,F_ANGLE
    !!--
    !!--
    !!-- INTEGER*4 NPOINT,NCOL,ISTAR1,IDO_X_S,IDO_Z_S, &
    !!-- IDO_Y_S,IDO_VZ,IDO_VX,IDO_XL,IDO_XN, &
    !!-- IDO_ZL,IDO_ZN,N_PLATES,IG_SEED,N_COLOR, &
    !!-- N_CIRCLE,N_CONE,N_SCREEN,I_SCREEN, &
    !!-- I_SLIT,K_SLIT,I_STOP, &
    !!-- I_ABS,MPURGE,NDEG
    !!--
    !!-- INTEGER*4 F_POLSEL,F_FACET,IFAC_X,IFAC_Y, &
    !!-- F_FAC_ORIENT,F_FAC_LATT, &
    !!-- F_KOMA,KOXX,I_KOMA,F_DOT,F_KOMA_BOUNCE, &
    !!-- F_EXIT_SHAPE,F_KOMA_CA,F_INC_MNOR_ANG, &
    !!-- ISEG_XNUM,ISEG_YNUM,F_SEGMENT
    !!--!!
    !!--!! added srio@esrf.eu 2008-10-02 to include some variables
    !!--!! found in gfiles but not found (!!) in common blocks...
    !!--!!
    !!--!! Warning: They are declared here, BUT NOT STORED IN COMMONS
    !!--!! (I found problems when trying to do so...) !!!
    !!--!!
    !!-- INTEGER(kind=ski) :: OE_NUMBER,IDUMMY
    !!-- REAL(kind=skr) :: DUMMY
    !!-- INTEGER(kind=ski),dimension(10) :: SCR_NUMBER
    !!--
    !!--
    !!-- REAL*8 ALPHA,SSOUR,THETA,PSOUR,SIMAG, &
    !!-- RDSOUR,RTHETA,PSREAL,TIMDIS, &
    !!-- DELTA,RDELTA,OFF_SOUX,OFF_SOUY,OFF_SOUZ, &
    !!-- ALPHA_S
    !!--
    !!-- REAL*8 RLEN,RLEN1,RLEN2,CCC,RMIRR,CONE_A, &
    !!-- AXMAJ,AXMIN,AFOCI,ECCENT,R_MAJ,R_MIN, &
    !!-- RWIDX,RWIDX1,RWIDX2,PARAM, &
    !!-- PCOEFF,CIL_ANG,ELL_THE
    !!-- REAL*8 RULING,ORDER,BETA,PHOT_CENT,R_LAMBDA, &
    !!-- HUNT_L,HUNT_H,BLAZE,D_SPACING,AZIM_FAN, &
    !!-- DIST_FAN,COMA_FAC,RUL_A1,RUL_A2,RUL_A3, &
    !!-- RUL_A4,A_BRAGG
    ! used in setsour
    real(kind=skr) :: beta,delta,rdelta
    !!--
    !!-- REAL*8 X_ROT,Y_ROT,Z_ROT,OFFX,OFFY,OFFZ, &
    !!-- U_MIR,V_MIR,W_MIR
    !!--
    !!-- REAL*8 SLLEN,SLWID,SLTILT,COD_LEN,COD_WID
    !!--
    !!-- REAL*8 WXSOU,WYSOU,WZSOU,SIGMAX,SIGMAY,SIGMAZ, &
    !!-- HDIV1,HDIV2,VDIV1,VDIV2,SIGDIX,SIGDIZ, &
    !!-- CONV_FACT,CONE_MAX,CONE_MIN,X_SOUR,Y_SOUR, &
    !!-- Z_SOUR,X_SOUR_ROT,Y_SOUR_ROT,Z_SOUR_ROT, &
    !!-- U_SOUR,V_SOUR,W_SOUR,PLASMA_ANGLE
    !!--
    !!-- REAL*8 PHOTON,BENER,R_ALADDIN, &
    !!-- EPSI_X,EPSI_Z,EPSI_DX,EPSI_DZ,R_MAGNET
    !!--
    !!-- REAL*8 COSDEL,SINDEL,COSTHE,SINTHE,COSTHR, &
    !!-- SINTHR,COSDER,SINDER,COSAL,SINAL, &
    !!-- COSAL_S,SINAL_S,COSTHE_I,SINTHE_I, &
    !!-- COSAL_I,SINAL_I
    !!--
    !!-- REAL*8 RIMCEN,VNIMAG,UXIM,VZIM, &
    !!-- D_PLATE,C_STAR,C_PLATE, &
    !!-- UX_PL,VZ_PL,WY_PL, &
    !!-- THETA_I,ALPHA_I
    !!--
    !!-- REAL*8 CENTRAL,T_INCIDENCE,T_SOURCE, &
    !!-- T_IMAGE,T_REFLECTION
    !!--
    !!-- REAL*8 X_RIP_AMP,X_RIP_WAV,X_PHASE, &
    !!-- Y_RIP_AMP,Y_RIP_WAV,Y_PHASE, &
    !!-- ROUGH_X, ROUGH_Y
    !!--
    !!-- REAL*8 RFAC_LENX,RFAC_LENY,RFAC_PHAX,RFAC_PHAY, &
    !!-- RFAC_DELX1,RFAC_DELX2,RFAC_DELY1, &
    !!-- RFAC_DELY2
    !!--
    !!-- REAL*8 ZKO_LENGTH,RKOMA_A,RKOMA_B, &
    !!-- RKOMA_CX,RKOMA_CY, &
    !!-- SEG_LENX,SEG_LENY
    !!--
    !!-- REAL*8 AMPLI,X_GR,Y_GR,SIGNUM, &
    !!-- SIG_XMIN,SIG_XMAX,SIG_X, &
    !!-- SIG_YMIN,SIG_YMAX,SIG_Y, &
    !!-- AMPL_IN
    !!--
    !!-- INTEGER*4 N_RIP
    !!--
    !!-- REAL*8 ALFA,GAMMA,POL_ANGLE,POL_DEG, &
    !!-- R_IND_OBJ,R_IND_IMA, &
    !!-- PH1,PH2,PH3,PH4,PH5,PH6,PH7,PH8,PH9,PH10, &
    !!-- RL1,RL2,RL3,RL4,RL5,RL6,RL7,RL8,RL9,RL10, &
    !!-- SIGXL1,SIGXL2,SIGXL3,SIGXL4,SIGXL5, &
    !!-- SIGXL6,SIGXL7,SIGXL8,SIGXL9,SIGXL10, &
    !!-- SIGZL1,SIGZL2,SIGZL3,SIGZL4,SIGZL5, &
    !!-- SIGZL6,SIGZL7,SIGZL8,SIGZL9,SIGZL10
    !!--
    !!-- REAL*8 RX_SLIT,RZ_SLIT,CX_SLIT,CZ_SLIT,SL_DIS, &
    !!-- UX_SCR,WY_SCR,VZ_SCR, &
    !!-- THICK
    !!--
    !!-- REAL*8 HOLO_R1,HOLO_R2,HOLO_DEL,HOLO_GAM, &
    !!-- HOLO_W,HOLO1,HOLO2,HOLO_RT1, &
    !!-- HOLO_RT2
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
    !!-- CHARACTER *80 FILE_RIP, FILE_REFL, FILE_ABS , FILE_MIR
    !!-- CHARACTER *80 FILE_SOURCE, FILE_TRAJ, FILE_BOUND
    !!-- CHARACTER *80 FFILE, FILE_ROUGH, FILE_KOMA, FILE_FAC
    character(kind=skc, len=1024) :: FFILE
    !!-- CHARACTER *80 FILE_SEGMENT,FILE_SEGP,FILE_KOMA_CA
    !!-- CHARACTER *80 FILE_SCR_EXT
    !!--!!srio PARAMETER (N_DIM = 1000000 + 1)
    !!-- EXTERNAL IRINT,RNUMBER,IYES
    !!--
    !!--
    !!-- COMMON /MATHBLK/ PI,TWOPI,PIHALF,TODEG,TORAD,ORIGIN(3), &
    !!-- X_VRS(3),Y_VRS(3),Z_VRS(3),TOCM,TOANGS
    !!-- COMMON /FLAGS / FMIRR,F_TORUS,FCYL,FANG,FSTAT,F_COHER, &
    !!-- FANG1,FSLIT,FGRID,F_NEW,FSOURCE_DEPTH, &
    !!-- FSOUR,FDISTR,FWRITE,F_BRAGG_A, &
    !!-- F_JOHANSSON, &
    !!-- F_POL,F_POLAR,F_RIPPLE,F_MOSAIC, &
    !!-- F_MOVE,F_HOW,F_G_S,F_READ_RIP,F_R_RAN, &
    !!-- F_GRATING,F_SIDE,F_CENTRAL,F_VIRTUAL, &
    !!-- F_COLOR,F_CONVEX,F_REFLEC,F_RULING, &
    !!-- F_RUL_ABS,F_BOUND_SOUR,F_THICK, &
    !!-- F_EXT,F_PHOT,F_SCREEN,F_PLATE,FSHAPE, &
    !!-- FHIT_C,F_PW,F_MONO,F_DEFAULT,F_REFL, &
    !!-- F_HUNT,F_PHOT_CENT,F_CRYSTAL,F_REFRAC, &
    !!-- F_PW_C,F_OPD,F_WIGGLER,FZP,F_SR_TYPE, &
    !!-- F_ROUGHNESS,FDUMMY,F_ANGLE,F_SEGMENT
    !!-- COMMON /CALC / NPOINT,NCOL,ISTAR1,IDO_X_S,IDO_Z_S, &
    !!-- IDO_Y_S,IDO_VZ,IDO_VX,IDO_XL,IDO_XN, &
    !!-- IDO_ZL,IDO_ZN,N_PLATES,IG_SEED,N_COLOR, &
    !!-- N_CIRCLE,N_CONE,N_SCREEN,I_SCREEN(10), &
    !!-- I_SLIT(10),K_SLIT(10),I_STOP(10), &
    !!-- I_ABS(10),MPURGE(2),NDEG,MOSAIC_SEED
    !!-- COMMON /SYSTEM1/ ALPHA,SSOUR,THETA,PSOUR(3),SIMAG, &
    !!-- RDSOUR,RTHETA,PSREAL(3),TIMDIS, &
    real(kind=skr),dimension(3) :: psour,psreal
    !!-- DELTA,RDELTA,OFF_SOUX,OFF_SOUY,OFF_SOUZ, &
    !!-- ALPHA_S
    !!-- COMMON /MIRROR / RLEN,RLEN1,RLEN2,CCC(10),RMIRR,CONE_A, &
    !!-- AXMAJ,AXMIN,AFOCI,ECCENT,R_MAJ,R_MIN, &
    !!-- RWIDX,RWIDX1,RWIDX2,PARAM, &
    real(kind=skr),dimension(10) :: ccc
    !!-- PCOEFF(0:4,0:4,0:4),CIL_ANG,ELL_THE
    real(kind=skr),dimension(0:4,0:4,0:4) :: pcoeff
    !!-- COMMON /GRATING/ RULING,ORDER,BETA,PHOT_CENT,R_LAMBDA, &
    !!-- HUNT_L,HUNT_H,BLAZE,D_SPACING,AZIM_FAN, &
    !!-- DIST_FAN,COMA_FAC,RUL_A1,RUL_A2,RUL_A3, &
    !!-- RUL_A4,A_BRAGG
    !!-- COMMON /XTAL / SPREAD_MOS, R_JOHANSSON, THICKNESS
    !!-- COMMON /ROT_MIR/ X_ROT,Y_ROT,Z_ROT,OFFX,OFFY,OFFZ, &
    !!-- U_MIR(3),V_MIR(3),W_MIR(3)
    real(kind=skr),dimension(3) :: u_mir,v_mir,w_mir
    !!-- COMMON /SLIT / SLLEN,SLWID,SLTILT,COD_LEN,COD_WID
    !!--! C
    !!--! C The SOURCE and ALADDIN are also in ../source/bm/bm.blk. KEEP
    !!--! C THESE IN SYNC.
    !!--! C
    !!-- COMMON /SOURCE / WXSOU,WYSOU,WZSOU,SIGMAX,SIGMAY,SIGMAZ, &
    !!-- HDIV1,HDIV2,VDIV1,VDIV2,SIGDIX,SIGDIZ, &
    !!-- CONV_FACT,CONE_MAX,CONE_MIN,X_SOUR, &
    !!-- Y_SOUR, &
    !!-- Z_SOUR,X_SOUR_ROT,Y_SOUR_ROT,Z_SOUR_ROT, &
    !!-- U_SOUR(3),V_SOUR(3),W_SOUR(3), &
    real(kind=skr),dimension(3) :: u_sour,v_sour,w_sour
    !!-- PLASMA_ANGLE
    !!-- COMMON /ALADDIN/ PHOTON(10),BENER,R_ALADDIN, &
    real(kind=skr),dimension(10) :: photon
    !!-- EPSI_X,EPSI_Z,EPSI_DX,EPSI_DZ,R_MAGNET
    !!-- COMMON /TRIG / COSDEL,SINDEL,COSTHE,SINTHE,COSTHR, &
    !!-- SINTHR,COSDER,SINDER,COSAL,SINAL, &
    !!-- COSAL_S,SINAL_S,COSTHE_I,SINTHE_I, &
    !!-- COSAL_I,SINAL_I

    ! used in setsour
    real(kind=skr) :: COSDEL,SINDEL,COSTHE,SINTHE
    real(kind=skr) :: COSTHR,SINTHR,COSDER,SINDER,COSAL,SINAL
    real(kind=skr) :: COSTHE_I,SINTHE_I
    ! used ?
    real(kind=skr) :: COSAL_S,SINAL_S
    real(kind=skr) :: COSAL_I,SINAL_I

    !!-- COMMON /IMAGE / RIMCEN(3),VNIMAG(3),UXIM(3),VZIM(3), &
    !!-- D_PLATE(5),C_STAR(3),C_PLATE(3), &
    !!-- UX_PL(3),VZ_PL(3),WY_PL(3), &
    !!-- THETA_I,ALPHA_I
    real(kind=skr),dimension(3) :: rimcen,vnimag, uxim, vzim, c_star, &
                                           c_plate, ux_pl, vz_pl, wy_pl
    !!-- COMMON /AXIS / CENTRAL(20,24),T_INCIDENCE,T_SOURCE, &
    real(kind=skr),dimension(20,24) :: central
    !!-- T_IMAGE,T_REFLECTION
    !!-- COMMON /RIPPLE / X_RIP_AMP,X_RIP_WAV,X_PHASE, &
    !!-- Y_RIP_AMP,Y_RIP_WAV,Y_PHASE, &
    !!-- ROUGH_X, ROUGH_Y
    !!-- COMMON /SEGMENT/ ISEG_XNUM,ISEG_YNUM, &
    !!-- SEG_LENX,SEG_LENY
    !!-- COMMON /FACET / F_POLSEL,F_FACET,IFAC_X,IFAC_Y, &
    !!-- F_FAC_ORIENT,F_FAC_LATT,RFAC_LENX, &
    !!-- RFAC_LENY,RFAC_PHAX,RFAC_PHAY, &
    !!-- RFAC_DELX1,RFAC_DELX2,RFAC_DELY1, &
    !!-- RFAC_DELY2
    !!-- COMMON /KOMA / F_KOMA,KOXX,I_KOMA,F_DOT,ZKO_LENGTH, &
    !!-- RKOMA_CX,RKOMA_CY,F_KOMA_CA, &
    !!-- F_KOMA_BOUNCE,F_EXIT_SHAPE, &
    !!-- F_INC_MNOR_ANG
    !!-- COMMON /NAMES / FILE_SOURCE,FILE_TRAJ, &
    !!-- FILE_RIP,FILE_REFL, &
    !!-- FILE_SEGMENT,FILE_SEGP, &
    !!-- FILE_ABS (10),FILE_SCR_EXT (10), &
    !!-- FILE_MIR,FILE_BOUND, &
    !!-- FILE_ROUGH,FILE_FAC,FILE_KOMA, &
    !!-- FILE_KOMA_CA
    ! srio: used in surface
    real(kind=skr),dimension(10) :: AMPLI,X_GR,Y_GR,SIGNUM
    !!-- COMMON /RIPP_2 / AMPLI(10),X_GR(10),Y_GR(10),SIGNUM(10), &
    real(kind=skr),dimension(10) :: SIG_X,SIG_XMIN,SIG_XMAX, &
                                            SIG_Y,SIG_YMIN,SIG_YMAX, AMPL_IN
    !!-- SIG_XMIN(10),SIG_XMAX(10),SIG_X(10), &
    !!-- SIG_YMIN(10),SIG_YMAX(10),SIG_Y(10), &
    !!-- AMPL_IN(10),N_RIP
    !!-- COMMON /LIGHT / ALFA,GAMMA,POL_ANGLE,POL_DEG, &
    !!-- R_IND_OBJ,R_IND_IMA, &
    !!-- PH1,PH2,PH3,PH4,PH5,PH6,PH7,PH8,PH9, &
    !!-- PH10, &
    !!-- RL1,RL2,RL3,RL4,RL5,RL6,RL7,RL8,RL9, &
    !!-- RL10, &
    !!-- SIGXL1,SIGXL2,SIGXL3,SIGXL4,SIGXL5, &
    !!-- SIGXL6,SIGXL7,SIGXL8,SIGXL9,SIGXL10, &
    !!-- SIGZL1,SIGZL2,SIGZL3,SIGZL4,SIGZL5, &
    !!-- SIGZL6,SIGZL7,SIGZL8,SIGZL9,SIGZL10
    !!-- COMMON /SCREENS/ RX_SLIT(10),RZ_SLIT(10),CX_SLIT(10), &
    !!-- CZ_SLIT(10),SL_DIS(10), &
    !!-- UX_SCR(3,2),WY_SCR(3,2),VZ_SCR(3,2), &
    !!-- THICK(10)
    real(kind=skr),dimension(3,2) :: UX_SCR,WY_SCR,VZ_SCR
    !!-- COMMON /HOLO / HOLO_R1,HOLO_R2,HOLO_DEL,HOLO_GAM, &
    !!-- HOLO_W,HOLO1(3),HOLO2(3),HOLO_RT1, &
    !!-- HOLO_RT2

    real(kind=skr),dimension(3) :: HOLO1,HOLO2

    !!--
    !!--!! common /srio/ OE_NUMBER,IDUMMY, DUMMY, SCR_NUMBER
    !!--


    !! variables passed between reflec and fresnel
    real(kind=skr),dimension(200) :: t_oe, gratio
    real(kind=skr) :: delo, beto, dele, bete, dels, bets


End Module shadow_variables
