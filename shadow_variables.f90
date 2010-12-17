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
    real(kind=skr), parameter :: pi     = 3.141592653589793238462643
    real(kind=skr), parameter :: twopi  = 6.283185307179586467925287
    real(kind=skr), parameter :: pihalf = 1.570796326794896619231322
    real(kind=skr), parameter :: todeg  =57.295779513082320876798155
    real(kind=skr), parameter :: torad  = 0.017453292519943295769237

    ! TODO: Change these values with new codata values (see NIST)
    real(kind=skr), parameter :: tocm =   1.239852D-4
    real(kind=skr), parameter :: toangs = 1.239852D+4
    integer(kind=ski), parameter  :: aDim=10

    ! again the same variables encapsulated in a structure 
    type, public, bind(C) :: poolSource
       
       integer(kind=ski)          :: FDISTR
       integer(kind=ski)          :: FGRID
       integer(kind=ski)          :: FSOUR
       integer(kind=ski)          :: FSOURCE_DEPTH
       integer(kind=ski)          :: F_COHER
       integer(kind=ski)          :: F_COLOR
       integer(kind=ski)          :: F_PHOT
       integer(kind=ski)          :: F_POL
       integer(kind=ski)          :: F_POLAR
       integer(kind=ski)          :: F_OPD
       integer(kind=ski)          :: F_WIGGLER
       integer(kind=ski)          :: F_BOUND_SOUR
       integer(kind=ski)          :: F_SR_TYPE
       integer(kind=ski)          :: ISTAR1
       integer(kind=ski)          :: NPOINT
       integer(kind=ski)          :: NCOL
       integer(kind=ski)          :: N_CIRCLE
       integer(kind=ski)          :: N_COLOR
       integer(kind=ski)          :: N_CONE
       integer(kind=ski)          :: IDO_VX
       integer(kind=ski)          :: IDO_VZ
       integer(kind=ski)          :: IDO_X_S
       integer(kind=ski)          :: IDO_Y_S
       integer(kind=ski)          :: IDO_Z_S
       integer(kind=ski)          :: IDO_XL
       integer(kind=ski)          :: IDO_XN
       integer(kind=ski)          :: IDO_ZL
       integer(kind=ski)          :: IDO_ZN
       real(kind=skr)             :: SIGXL1
       real(kind=skr)             :: SIGXL2
       real(kind=skr)             :: SIGXL3
       real(kind=skr)             :: SIGXL4
       real(kind=skr)             :: SIGXL5
       real(kind=skr)             :: SIGXL6
       real(kind=skr)             :: SIGXL7
       real(kind=skr)             :: SIGXL8
       real(kind=skr)             :: SIGXL9
       real(kind=skr)             :: SIGXL10
       real(kind=skr)             :: SIGZL1
       real(kind=skr)             :: SIGZL2
       real(kind=skr)             :: SIGZL3
       real(kind=skr)             :: SIGZL4
       real(kind=skr)             :: SIGZL5
       real(kind=skr)             :: SIGZL6
       real(kind=skr)             :: SIGZL7
       real(kind=skr)             :: SIGZL8
       real(kind=skr)             :: SIGZL9
       real(kind=skr)             :: SIGZL10
       real(kind=skr)             :: CONV_FACT
       real(kind=skr)             :: CONE_MAX
       real(kind=skr)             :: CONE_MIN
       real(kind=skr)             :: EPSI_DX
       real(kind=skr)             :: EPSI_DZ
       real(kind=skr)             :: EPSI_X
       real(kind=skr)             :: EPSI_Z
       real(kind=skr)             :: HDIV1
       real(kind=skr)             :: HDIV2
       real(kind=skr)             :: PH1
       real(kind=skr)             :: PH2
       real(kind=skr)             :: PH3
       real(kind=skr)             :: PH4
       real(kind=skr)             :: PH5
       real(kind=skr)             :: PH6
       real(kind=skr)             :: PH7
       real(kind=skr)             :: PH8
       real(kind=skr)             :: PH9
       real(kind=skr)             :: PH10
       real(kind=skr)             :: RL1
       real(kind=skr)             :: RL2
       real(kind=skr)             :: RL3
       real(kind=skr)             :: RL4
       real(kind=skr)             :: RL5
       real(kind=skr)             :: RL6
       real(kind=skr)             :: RL7
       real(kind=skr)             :: RL8
       real(kind=skr)             :: RL9
       real(kind=skr)             :: RL10
       real(kind=skr)             :: BENER
       real(kind=skr)             :: POL_ANGLE
       real(kind=skr)             :: POL_DEG
       real(kind=skr)             :: R_ALADDIN
       real(kind=skr)             :: R_MAGNET
       real(kind=skr)             :: SIGDIX
       real(kind=skr)             :: SIGDIZ
       real(kind=skr)             :: SIGMAX
       real(kind=skr)             :: SIGMAY
       real(kind=skr)             :: SIGMAZ
       real(kind=skr)             :: VDIV1
       real(kind=skr)             :: VDIV2
       real(kind=skr)             :: WXSOU
       real(kind=skr)             :: WYSOU
       real(kind=skr)             :: WZSOU
       real(kind=skr)             :: PLASMA_ANGLE
       character(len=1024)       :: FILE_TRAJ
       character(len=1024)       :: FILE_SOURCE
       character(len=1024)       :: FILE_BOUND
       integer(kind=ski)          :: OE_NUMBER
       integer(kind=ski)          :: IDUMMY
       real(kind=skr)             :: DUMMY
       integer(kind=ski)          :: F_NEW
       
    end type poolSource
    
    
    type, public, bind(C) :: poolOE
       
       integer(kind=ski)          :: FMIRR
       integer(kind=ski)          :: F_TORUS
       integer(kind=ski)          :: FCYL
       integer(kind=ski)          :: F_EXT
       integer(kind=ski)          :: FSTAT
       integer(kind=ski)          :: F_SCREEN
       integer(kind=ski)          :: F_PLATE
       integer(kind=ski)          :: FSLIT
       integer(kind=ski)          :: FWRITE
       integer(kind=ski)          :: F_RIPPLE
       integer(kind=ski)          :: F_MOVE
       integer(kind=ski)          :: F_THICK
       integer(kind=ski)          :: F_BRAGG_A
       integer(kind=ski)          :: F_G_S
       integer(kind=ski)          :: F_R_RAN
       integer(kind=ski)          :: F_GRATING
       integer(kind=ski)          :: F_MOSAIC
       integer(kind=ski)          :: F_JOHANSSON
       integer(kind=ski)          :: F_SIDE
       integer(kind=ski)          :: F_CENTRAL
       integer(kind=ski)          :: F_CONVEX
       integer(kind=ski)          :: F_REFLEC
       integer(kind=ski)          :: F_RUL_ABS
       integer(kind=ski)          :: F_RULING
       integer(kind=ski)          :: F_PW
       integer(kind=ski)          :: F_PW_C
       integer(kind=ski)          :: F_VIRTUAL
       integer(kind=ski)          :: FSHAPE
       integer(kind=ski)          :: FHIT_C
       integer(kind=ski)          :: F_MONO
       integer(kind=ski)          :: F_REFRAC
       integer(kind=ski)          :: F_DEFAULT
       integer(kind=ski)          :: F_REFL
       integer(kind=ski)          :: F_HUNT
       integer(kind=ski)          :: F_CRYSTAL
       integer(kind=ski)          :: F_PHOT_CENT
       integer(kind=ski)          :: F_ROUGHNESS
       integer(kind=ski)          :: F_ANGLE
       integer(kind=ski)          :: NPOINTOE
       
       integer(kind=ski)          :: NCOL
       integer(kind=ski)          :: N_SCREEN
       
       integer(kind=ski)          :: ISTAR1
       real(kind=skr)             :: CIL_ANG
       real(kind=skr)             :: ELL_THE
       integer(kind=ski)          :: N_PLATES
       integer(kind=ski)          :: IG_SEED
       integer(kind=ski)          :: MOSAIC_SEED
       real(kind=skr)             :: ALPHA
       real(kind=skr)             :: SSOUR
       real(kind=skr)             :: THETA
       real(kind=skr)             :: SIMAG
       real(kind=skr)             :: RDSOUR
       real(kind=skr)             :: RTHETA
       real(kind=skr)             :: OFF_SOUX
       real(kind=skr)             :: OFF_SOUY
       real(kind=skr)             :: OFF_SOUZ
       real(kind=skr)             :: ALPHA_S
       real(kind=skr)             :: RLEN1
       real(kind=skr)             :: RLEN2
       real(kind=skr)             :: RMIRR
       real(kind=skr)             :: AXMAJ
       real(kind=skr)             :: AXMIN
       real(kind=skr)             :: CONE_A
       real(kind=skr)             :: R_MAJ
       real(kind=skr)             :: R_MIN
       real(kind=skr)             :: RWIDX1
       real(kind=skr)             :: RWIDX2
       real(kind=skr)             :: PARAM
       real(kind=skr)             :: HUNT_H
       real(kind=skr)             :: HUNT_L
       real(kind=skr)             :: BLAZE
       real(kind=skr)             :: RULING
       real(kind=skr)             :: ORDER
       real(kind=skr)             :: PHOT_CENT
       real(kind=skr)             :: X_ROT
       real(kind=skr)             :: D_SPACING
       real(kind=skr)             :: A_BRAGG
       real(kind=skr)             :: SPREAD_MOS
       real(kind=skr)             :: THICKNESS
       real(kind=skr)             :: R_JOHANSSON
       real(kind=skr)             :: Y_ROT
       real(kind=skr)             :: Z_ROT
       real(kind=skr)             :: OFFX
       real(kind=skr)             :: OFFY
       real(kind=skr)             :: OFFZ
       real(kind=skr)             :: SLLEN
       real(kind=skr)             :: SLWID
       real(kind=skr)             :: SLTILT
       real(kind=skr)             :: COD_LEN
       real(kind=skr)             :: COD_WID
       real(kind=skr)             :: X_SOUR
       real(kind=skr)             :: Y_SOUR
       real(kind=skr)             :: Z_SOUR
       real(kind=skr)             :: X_SOUR_ROT
       real(kind=skr)             :: Y_SOUR_ROT
       real(kind=skr)             :: Z_SOUR_ROT
       real(kind=skr)             :: R_LAMBDA
       real(kind=skr)             :: THETA_I
       real(kind=skr)             :: ALPHA_I
       real(kind=skr)             :: T_INCIDENCE
       real(kind=skr)             :: T_SOURCE
       real(kind=skr)             :: T_IMAGE
       real(kind=skr)             :: T_REFLECTION
       
       character(len=1024)       :: FILE_SOURCE
       character(len=1024)       :: FILE_RIP
       character(len=1024)       :: FILE_REFL
       character(len=1024)       :: FILE_MIR
       character(len=1024)       :: FILE_ROUGH
       integer(kind=ski)          :: FZP
       real(kind=skr)             :: HOLO_R1
       real(kind=skr)             :: HOLO_R2
       real(kind=skr)             :: HOLO_DEL
       real(kind=skr)             :: HOLO_GAM
       real(kind=skr)             :: HOLO_W
       real(kind=skr)             :: HOLO_RT1
       real(kind=skr)             :: HOLO_RT2
       real(kind=skr)             :: AZIM_FAN
       real(kind=skr)             :: DIST_FAN
       real(kind=skr)             :: COMA_FAC
       real(kind=skr)             :: ALFA
       real(kind=skr)             :: GAMMA
       real(kind=skr)             :: R_IND_OBJ
       real(kind=skr)             :: R_IND_IMA
       real(kind=skr)             :: RUL_A1
       real(kind=skr)             :: RUL_A2
       real(kind=skr)             :: RUL_A3
       real(kind=skr)             :: RUL_A4
       integer(kind=ski)          :: F_POLSEL
       integer(kind=ski)          :: F_FACET
       integer(kind=ski)          :: F_FAC_ORIENT
       integer(kind=ski)          :: F_FAC_LATT
       real(kind=skr)             :: RFAC_LENX
       real(kind=skr)             :: RFAC_LENY
       real(kind=skr)             :: RFAC_PHAX
       real(kind=skr)             :: RFAC_PHAY
       real(kind=skr)             :: RFAC_DELX1
       real(kind=skr)             :: RFAC_DELX2
       real(kind=skr)             :: RFAC_DELY1
       real(kind=skr)             :: RFAC_DELY2
       character(len=1024)       :: FILE_FAC
       integer(kind=ski)          :: F_SEGMENT
       integer(kind=ski)          :: ISEG_XNUM
       integer(kind=ski)          :: ISEG_YNUM
       character(len=1024)       :: FILE_SEGMENT
       character(len=1024)       :: FILE_SEGP
       real(kind=skr)             :: SEG_LENX
       real(kind=skr)             :: SEG_LENY
       integer(kind=ski)          :: F_KOMA
       character(len=1024)       :: FILE_KOMA
       integer(kind=ski)          :: F_EXIT_SHAPE
       integer(kind=ski)          :: F_INC_MNOR_ANG
       real(kind=skr)             :: ZKO_LENGTH
       real(kind=skr)             :: RKOMA_CX
       real(kind=skr)             :: RKOMA_CY
       integer(kind=ski)          :: F_KOMA_CA
       character(len=1024)       :: FILE_KOMA_CA
       integer(kind=ski)          :: F_KOMA_BOUNCE
       real(kind=skr)             :: X_RIP_AMP
       real(kind=skr)             :: X_RIP_WAV
       real(kind=skr)             :: X_PHASE
       real(kind=skr)             :: Y_RIP_AMP
       real(kind=skr)             :: Y_RIP_WAV
       real(kind=skr)             :: Y_PHASE
       integer(kind=ski)          :: N_RIP
       real(kind=skr)             :: ROUGH_X
       real(kind=skr)             :: ROUGH_Y
       
       integer(kind=ski)          :: OE_NUMBER
       integer(kind=ski)          :: IDUMMY
       real(kind=skr)             :: DUMMY
       
       real(kind=skr), dimension(aDim)            :: CX_SLIT
       real(kind=skr), dimension(aDim)            :: CZ_SLIT
       real(kind=skr), dimension(aDim)            :: D_PLATE
       character(len=1024), dimension(aDim)      :: FILE_ABS
       character(len=1024), dimension(aDim)      :: FILE_SCR_EXT
       integer(kind=ski), dimension(aDim)         :: I_ABS
       integer(kind=ski), dimension(aDim)         :: I_SCREEN
       integer(kind=ski), dimension(aDim)         :: I_SLIT
       integer(kind=ski), dimension(aDim)         :: I_STOP
       integer(kind=ski), dimension(aDim)         :: K_SLIT
       real(kind=skr), dimension(aDim)            :: RX_SLIT
       real(kind=skr), dimension(aDim)            :: RZ_SLIT
       integer(kind=ski), dimension(aDim)         :: SCR_NUMBER
       real(kind=skr), dimension(aDim)            :: SL_DIS
       real(kind=skr), dimension(aDim)            :: THICK
       
       
    end type poolOE



    public  :: PoolOELoad,PoolOEWrite,PoolSourceLoad,PoolSourceWrite
    private :: PoolSourceToGf,PoolOEToGf,GfToPoolSource,GfToPoolOE

Contains


  !
  !
  !
  
  Subroutine PoolSourceToGf(pool00,gf)
    
    type(gfType),intent(inout)  :: gf 
    type(poolSource),intent(in) :: pool00 
    logical                     :: iOut
    integer(kind=ski)             :: zero=0
    
    iOut = GfTypeAllocate(gf,zero,zero)
    !! START CODE CREATED AUTOMATICALLY (makecode1.pro)
    
    iOut= iOut .and. GfForceSetValue(gf,"FDISTR",pool00%FDISTR)
    iOut= iOut .and. GfForceSetValue(gf,"FGRID",pool00%FGRID)
    iOut= iOut .and. GfForceSetValue(gf,"FSOUR",pool00%FSOUR)
    iOut= iOut .and. GfForceSetValue(gf,"FSOURCE_DEPTH",pool00%FSOURCE_DEPTH)
    iOut= iOut .and. GfForceSetValue(gf,"F_COHER",pool00%F_COHER)
    iOut= iOut .and. GfForceSetValue(gf,"F_COLOR",pool00%F_COLOR)
    iOut= iOut .and. GfForceSetValue(gf,"F_PHOT",pool00%F_PHOT)
    iOut= iOut .and. GfForceSetValue(gf,"F_POL",pool00%F_POL)
    iOut= iOut .and. GfForceSetValue(gf,"F_POLAR",pool00%F_POLAR)
    iOut= iOut .and. GfForceSetValue(gf,"F_OPD",pool00%F_OPD)
    iOut= iOut .and. GfForceSetValue(gf,"F_WIGGLER",pool00%F_WIGGLER)
    iOut= iOut .and. GfForceSetValue(gf,"F_BOUND_SOUR",pool00%F_BOUND_SOUR)
    iOut= iOut .and. GfForceSetValue(gf,"F_SR_TYPE",pool00%F_SR_TYPE)
    iOut= iOut .and. GfForceSetValue(gf,"ISTAR1",pool00%ISTAR1)
    iOut= iOut .and. GfForceSetValue(gf,"NPOINT",pool00%NPOINT)
    iOut= iOut .and. GfForceSetValue(gf,"NCOL",pool00%NCOL)
    iOut= iOut .and. GfForceSetValue(gf,"N_CIRCLE",pool00%N_CIRCLE)
    iOut= iOut .and. GfForceSetValue(gf,"N_COLOR",pool00%N_COLOR)
    iOut= iOut .and. GfForceSetValue(gf,"N_CONE",pool00%N_CONE)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_VX",pool00%IDO_VX)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_VZ",pool00%IDO_VZ)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_X_S",pool00%IDO_X_S)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_Y_S",pool00%IDO_Y_S)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_Z_S",pool00%IDO_Z_S)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_XL",pool00%IDO_XL)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_XN",pool00%IDO_XN)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_ZL",pool00%IDO_ZL)
    iOut= iOut .and. GfForceSetValue(gf,"IDO_ZN",pool00%IDO_ZN)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL1",pool00%SIGXL1)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL2",pool00%SIGXL2)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL3",pool00%SIGXL3)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL4",pool00%SIGXL4)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL5",pool00%SIGXL5)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL6",pool00%SIGXL6)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL7",pool00%SIGXL7)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL8",pool00%SIGXL8)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL9",pool00%SIGXL9)
    iOut= iOut .and. GfForceSetValue(gf,"SIGXL10",pool00%SIGXL10)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL1",pool00%SIGZL1)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL2",pool00%SIGZL2)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL3",pool00%SIGZL3)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL4",pool00%SIGZL4)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL5",pool00%SIGZL5)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL6",pool00%SIGZL6)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL7",pool00%SIGZL7)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL8",pool00%SIGZL8)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL9",pool00%SIGZL9)
    iOut= iOut .and. GfForceSetValue(gf,"SIGZL10",pool00%SIGZL10)
    iOut= iOut .and. GfForceSetValue(gf,"CONV_FACT",pool00%CONV_FACT)
    iOut= iOut .and. GfForceSetValue(gf,"CONE_MAX",pool00%CONE_MAX)
    iOut= iOut .and. GfForceSetValue(gf,"CONE_MIN",pool00%CONE_MIN)
    iOut= iOut .and. GfForceSetValue(gf,"EPSI_DX",pool00%EPSI_DX)
    iOut= iOut .and. GfForceSetValue(gf,"EPSI_DZ",pool00%EPSI_DZ)
    iOut= iOut .and. GfForceSetValue(gf,"EPSI_X",pool00%EPSI_X)
    iOut= iOut .and. GfForceSetValue(gf,"EPSI_Z",pool00%EPSI_Z)
    iOut= iOut .and. GfForceSetValue(gf,"HDIV1",pool00%HDIV1)
    iOut= iOut .and. GfForceSetValue(gf,"HDIV2",pool00%HDIV2)
    iOut= iOut .and. GfForceSetValue(gf,"PH1",pool00%PH1)
    iOut= iOut .and. GfForceSetValue(gf,"PH2",pool00%PH2)
    iOut= iOut .and. GfForceSetValue(gf,"PH3",pool00%PH3)
    iOut= iOut .and. GfForceSetValue(gf,"PH4",pool00%PH4)
    iOut= iOut .and. GfForceSetValue(gf,"PH5",pool00%PH5)
    iOut= iOut .and. GfForceSetValue(gf,"PH6",pool00%PH6)
    iOut= iOut .and. GfForceSetValue(gf,"PH7",pool00%PH7)
    iOut= iOut .and. GfForceSetValue(gf,"PH8",pool00%PH8)
    iOut= iOut .and. GfForceSetValue(gf,"PH9",pool00%PH9)
    iOut= iOut .and. GfForceSetValue(gf,"PH10",pool00%PH10)
    iOut= iOut .and. GfForceSetValue(gf,"RL1",pool00%RL1)
    iOut= iOut .and. GfForceSetValue(gf,"RL2",pool00%RL2)
    iOut= iOut .and. GfForceSetValue(gf,"RL3",pool00%RL3)
    iOut= iOut .and. GfForceSetValue(gf,"RL4",pool00%RL4)
    iOut= iOut .and. GfForceSetValue(gf,"RL5",pool00%RL5)
    iOut= iOut .and. GfForceSetValue(gf,"RL6",pool00%RL6)
    iOut= iOut .and. GfForceSetValue(gf,"RL7",pool00%RL7)
    iOut= iOut .and. GfForceSetValue(gf,"RL8",pool00%RL8)
    iOut= iOut .and. GfForceSetValue(gf,"RL9",pool00%RL9)
    iOut= iOut .and. GfForceSetValue(gf,"RL10",pool00%RL10)
    iOut= iOut .and. GfForceSetValue(gf,"BENER",pool00%BENER)
    iOut= iOut .and. GfForceSetValue(gf,"POL_ANGLE",pool00%POL_ANGLE)
    iOut= iOut .and. GfForceSetValue(gf,"POL_DEG",pool00%POL_DEG)
    iOut= iOut .and. GfForceSetValue(gf,"R_ALADDIN",pool00%R_ALADDIN)
    iOut= iOut .and. GfForceSetValue(gf,"R_MAGNET",pool00%R_MAGNET)
    iOut= iOut .and. GfForceSetValue(gf,"SIGDIX",pool00%SIGDIX)
    iOut= iOut .and. GfForceSetValue(gf,"SIGDIZ",pool00%SIGDIZ)
    iOut= iOut .and. GfForceSetValue(gf,"SIGMAX",pool00%SIGMAX)
    iOut= iOut .and. GfForceSetValue(gf,"SIGMAY",pool00%SIGMAY)
    iOut= iOut .and. GfForceSetValue(gf,"SIGMAZ",pool00%SIGMAZ)
    iOut= iOut .and. GfForceSetValue(gf,"VDIV1",pool00%VDIV1)
    iOut= iOut .and. GfForceSetValue(gf,"VDIV2",pool00%VDIV2)
    iOut= iOut .and. GfForceSetValue(gf,"WXSOU",pool00%WXSOU)
    iOut= iOut .and. GfForceSetValue(gf,"WYSOU",pool00%WYSOU)
    iOut= iOut .and. GfForceSetValue(gf,"WZSOU",pool00%WZSOU)
    iOut= iOut .and. GfForceSetValue(gf,"PLASMA_ANGLE",pool00%PLASMA_ANGLE)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_TRAJ",pool00%FILE_TRAJ)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SOURCE",pool00%FILE_SOURCE)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_BOUND",pool00%FILE_BOUND)
    iOut= iOut .and. GfForceSetValue(gf,"OE_NUMBER",pool00%OE_NUMBER)
    iOut= iOut .and. GfForceSetValue(gf,"IDUMMY",pool00%IDUMMY)
    iOut= iOut .and. GfForceSetValue(gf,"DUMMY",pool00%DUMMY)
    iOut= iOut .and. GfForceSetValue(gf,"F_NEW",pool00%F_NEW)
    !! END CODE CREATED AUTOMATICALLY (makecode1.pro)
  End Subroutine PoolSourceToGf
  
  !
  !
  !
  
  
  Subroutine PoolOEToGf(pool01,gf)
    
    type(gfType),intent(inout)    :: gf 
    type(poolOE),intent(in)     :: pool01 
    logical                     :: iOut
    integer(kind=ski)             :: zero=0
    
    iOut = GfTypeAllocate(gf,zero,zero)
    !! START CODE CREATED AUTOMATICALLY (makecode1.pro)
    
    iOut= iOut .and. GfForceSetValue(gf,"FMIRR",pool01%FMIRR)
    iOut= iOut .and. GfForceSetValue(gf,"F_TORUS",pool01%F_TORUS)
    iOut= iOut .and. GfForceSetValue(gf,"FCYL",pool01%FCYL)
    iOut= iOut .and. GfForceSetValue(gf,"F_EXT",pool01%F_EXT)
    iOut= iOut .and. GfForceSetValue(gf,"FSTAT",pool01%FSTAT)
    iOut= iOut .and. GfForceSetValue(gf,"F_SCREEN",pool01%F_SCREEN)
    iOut= iOut .and. GfForceSetValue(gf,"F_PLATE",pool01%F_PLATE)
    iOut= iOut .and. GfForceSetValue(gf,"FSLIT",pool01%FSLIT)
    iOut= iOut .and. GfForceSetValue(gf,"FWRITE",pool01%FWRITE)
    iOut= iOut .and. GfForceSetValue(gf,"F_RIPPLE",pool01%F_RIPPLE)
    iOut= iOut .and. GfForceSetValue(gf,"F_MOVE",pool01%F_MOVE)
    iOut= iOut .and. GfForceSetValue(gf,"F_THICK",pool01%F_THICK)
    iOut= iOut .and. GfForceSetValue(gf,"F_BRAGG_A",pool01%F_BRAGG_A)
    iOut= iOut .and. GfForceSetValue(gf,"F_G_S",pool01%F_G_S)
    iOut= iOut .and. GfForceSetValue(gf,"F_R_RAN",pool01%F_R_RAN)
    iOut= iOut .and. GfForceSetValue(gf,"F_GRATING",pool01%F_GRATING)
    iOut= iOut .and. GfForceSetValue(gf,"F_MOSAIC",pool01%F_MOSAIC)
    iOut= iOut .and. GfForceSetValue(gf,"F_JOHANSSON",pool01%F_JOHANSSON)
    iOut= iOut .and. GfForceSetValue(gf,"F_SIDE",pool01%F_SIDE)
    iOut= iOut .and. GfForceSetValue(gf,"F_CENTRAL",pool01%F_CENTRAL)
    iOut= iOut .and. GfForceSetValue(gf,"F_CONVEX",pool01%F_CONVEX)
    iOut= iOut .and. GfForceSetValue(gf,"F_REFLEC",pool01%F_REFLEC)
    iOut= iOut .and. GfForceSetValue(gf,"F_RUL_ABS",pool01%F_RUL_ABS)
    iOut= iOut .and. GfForceSetValue(gf,"F_RULING",pool01%F_RULING)
    iOut= iOut .and. GfForceSetValue(gf,"F_PW",pool01%F_PW)
    iOut= iOut .and. GfForceSetValue(gf,"F_PW_C",pool01%F_PW_C)
    iOut= iOut .and. GfForceSetValue(gf,"F_VIRTUAL",pool01%F_VIRTUAL)
    iOut= iOut .and. GfForceSetValue(gf,"FSHAPE",pool01%FSHAPE)
    iOut= iOut .and. GfForceSetValue(gf,"FHIT_C",pool01%FHIT_C)
    iOut= iOut .and. GfForceSetValue(gf,"F_MONO",pool01%F_MONO)
    iOut= iOut .and. GfForceSetValue(gf,"F_REFRAC",pool01%F_REFRAC)
    iOut= iOut .and. GfForceSetValue(gf,"F_DEFAULT",pool01%F_DEFAULT)
    iOut= iOut .and. GfForceSetValue(gf,"F_REFL",pool01%F_REFL)
    iOut= iOut .and. GfForceSetValue(gf,"F_HUNT",pool01%F_HUNT)
    iOut= iOut .and. GfForceSetValue(gf,"F_CRYSTAL",pool01%F_CRYSTAL)
    iOut= iOut .and. GfForceSetValue(gf,"F_PHOT_CENT",pool01%F_PHOT_CENT)
    iOut= iOut .and. GfForceSetValue(gf,"F_ROUGHNESS",pool01%F_ROUGHNESS)
    iOut= iOut .and. GfForceSetValue(gf,"F_ANGLE",pool01%F_ANGLE)
    !srio danger
    !iOut= iOut .and. GfForceSetValue(gf,"NPOINT",pool01%NPOINTOE)
    iOut= iOut .and. GfForceSetValue(gf,"NCOL",pool01%NCOL)
    iOut= iOut .and. GfForceSetValue(gf,"N_SCREEN",pool01%N_SCREEN)
    iOut= iOut .and. GfForceSetValue(gf,"ISTAR1",pool01%ISTAR1)
    iOut= iOut .and. GfForceSetValue(gf,"CIL_ANG",pool01%CIL_ANG)
    iOut= iOut .and. GfForceSetValue(gf,"ELL_THE",pool01%ELL_THE)
    iOut= iOut .and. GfForceSetValue(gf,"N_PLATES",pool01%N_PLATES)
    iOut= iOut .and. GfForceSetValue(gf,"IG_SEED",pool01%IG_SEED)
    iOut= iOut .and. GfForceSetValue(gf,"MOSAIC_SEED",pool01%MOSAIC_SEED)
    iOut= iOut .and. GfForceSetValue(gf,"ALPHA",pool01%ALPHA)
    iOut= iOut .and. GfForceSetValue(gf,"SSOUR",pool01%SSOUR)
    iOut= iOut .and. GfForceSetValue(gf,"THETA",pool01%THETA)
    iOut= iOut .and. GfForceSetValue(gf,"SIMAG",pool01%SIMAG)
    iOut= iOut .and. GfForceSetValue(gf,"RDSOUR",pool01%RDSOUR)
    iOut= iOut .and. GfForceSetValue(gf,"RTHETA",pool01%RTHETA)
    iOut= iOut .and. GfForceSetValue(gf,"OFF_SOUX",pool01%OFF_SOUX)
    iOut= iOut .and. GfForceSetValue(gf,"OFF_SOUY",pool01%OFF_SOUY)
    iOut= iOut .and. GfForceSetValue(gf,"OFF_SOUZ",pool01%OFF_SOUZ)
    iOut= iOut .and. GfForceSetValue(gf,"ALPHA_S",pool01%ALPHA_S)
    iOut= iOut .and. GfForceSetValue(gf,"RLEN1",pool01%RLEN1)
    iOut= iOut .and. GfForceSetValue(gf,"RLEN2",pool01%RLEN2)
    iOut= iOut .and. GfForceSetValue(gf,"RMIRR",pool01%RMIRR)
    iOut= iOut .and. GfForceSetValue(gf,"AXMAJ",pool01%AXMAJ)
    iOut= iOut .and. GfForceSetValue(gf,"AXMIN",pool01%AXMIN)
    iOut= iOut .and. GfForceSetValue(gf,"CONE_A",pool01%CONE_A)
    iOut= iOut .and. GfForceSetValue(gf,"R_MAJ",pool01%R_MAJ)
    iOut= iOut .and. GfForceSetValue(gf,"R_MIN",pool01%R_MIN)
    iOut= iOut .and. GfForceSetValue(gf,"RWIDX1",pool01%RWIDX1)
    iOut= iOut .and. GfForceSetValue(gf,"RWIDX2",pool01%RWIDX2)
    iOut= iOut .and. GfForceSetValue(gf,"PARAM",pool01%PARAM)
    iOut= iOut .and. GfForceSetValue(gf,"HUNT_H",pool01%HUNT_H)
    iOut= iOut .and. GfForceSetValue(gf,"HUNT_L",pool01%HUNT_L)
    iOut= iOut .and. GfForceSetValue(gf,"BLAZE",pool01%BLAZE)
    iOut= iOut .and. GfForceSetValue(gf,"RULING",pool01%RULING)
    iOut= iOut .and. GfForceSetValue(gf,"ORDER",pool01%ORDER)
    iOut= iOut .and. GfForceSetValue(gf,"PHOT_CENT",pool01%PHOT_CENT)
    iOut= iOut .and. GfForceSetValue(gf,"X_ROT",pool01%X_ROT)
    iOut= iOut .and. GfForceSetValue(gf,"D_SPACING",pool01%D_SPACING)
    iOut= iOut .and. GfForceSetValue(gf,"A_BRAGG",pool01%A_BRAGG)
    iOut= iOut .and. GfForceSetValue(gf,"SPREAD_MOS",pool01%SPREAD_MOS)
    iOut= iOut .and. GfForceSetValue(gf,"THICKNESS",pool01%THICKNESS)
    iOut= iOut .and. GfForceSetValue(gf,"R_JOHANSSON",pool01%R_JOHANSSON)
    iOut= iOut .and. GfForceSetValue(gf,"Y_ROT",pool01%Y_ROT)
    iOut= iOut .and. GfForceSetValue(gf,"Z_ROT",pool01%Z_ROT)
    iOut= iOut .and. GfForceSetValue(gf,"OFFX",pool01%OFFX)
    iOut= iOut .and. GfForceSetValue(gf,"OFFY",pool01%OFFY)
    iOut= iOut .and. GfForceSetValue(gf,"OFFZ",pool01%OFFZ)
    iOut= iOut .and. GfForceSetValue(gf,"SLLEN",pool01%SLLEN)
    iOut= iOut .and. GfForceSetValue(gf,"SLWID",pool01%SLWID)
    iOut= iOut .and. GfForceSetValue(gf,"SLTILT",pool01%SLTILT)
    iOut= iOut .and. GfForceSetValue(gf,"COD_LEN",pool01%COD_LEN)
    iOut= iOut .and. GfForceSetValue(gf,"COD_WID",pool01%COD_WID)
    iOut= iOut .and. GfForceSetValue(gf,"X_SOUR",pool01%X_SOUR)
    iOut= iOut .and. GfForceSetValue(gf,"Y_SOUR",pool01%Y_SOUR)
    iOut= iOut .and. GfForceSetValue(gf,"Z_SOUR",pool01%Z_SOUR)
    iOut= iOut .and. GfForceSetValue(gf,"X_SOUR_ROT",pool01%X_SOUR_ROT)
    iOut= iOut .and. GfForceSetValue(gf,"Y_SOUR_ROT",pool01%Y_SOUR_ROT)
    iOut= iOut .and. GfForceSetValue(gf,"Z_SOUR_ROT",pool01%Z_SOUR_ROT)
    iOut= iOut .and. GfForceSetValue(gf,"R_LAMBDA",pool01%R_LAMBDA)
    iOut= iOut .and. GfForceSetValue(gf,"THETA_I",pool01%THETA_I)
    iOut= iOut .and. GfForceSetValue(gf,"ALPHA_I",pool01%ALPHA_I)
    iOut= iOut .and. GfForceSetValue(gf,"T_INCIDENCE",pool01%T_INCIDENCE)
    iOut= iOut .and. GfForceSetValue(gf,"T_SOURCE",pool01%T_SOURCE)
    iOut= iOut .and. GfForceSetValue(gf,"T_IMAGE",pool01%T_IMAGE)
    iOut= iOut .and. GfForceSetValue(gf,"T_REFLECTION",pool01%T_REFLECTION)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SOURCE",pool01%FILE_SOURCE)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_RIP",pool01%FILE_RIP)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_REFL",pool01%FILE_REFL)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_MIR",pool01%FILE_MIR)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ROUGH",pool01%FILE_ROUGH)
    iOut= iOut .and. GfForceSetValue(gf,"FZP",pool01%FZP)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_R1",pool01%HOLO_R1)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_R2",pool01%HOLO_R2)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_DEL",pool01%HOLO_DEL)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_GAM",pool01%HOLO_GAM)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_W",pool01%HOLO_W)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_RT1",pool01%HOLO_RT1)
    iOut= iOut .and. GfForceSetValue(gf,"HOLO_RT2",pool01%HOLO_RT2)
    iOut= iOut .and. GfForceSetValue(gf,"AZIM_FAN",pool01%AZIM_FAN)
    iOut= iOut .and. GfForceSetValue(gf,"DIST_FAN",pool01%DIST_FAN)
    iOut= iOut .and. GfForceSetValue(gf,"COMA_FAC",pool01%COMA_FAC)
    iOut= iOut .and. GfForceSetValue(gf,"ALFA",pool01%ALFA)
    iOut= iOut .and. GfForceSetValue(gf,"GAMMA",pool01%GAMMA)
    iOut= iOut .and. GfForceSetValue(gf,"R_IND_OBJ",pool01%R_IND_OBJ)
    iOut= iOut .and. GfForceSetValue(gf,"R_IND_IMA",pool01%R_IND_IMA)
    iOut= iOut .and. GfForceSetValue(gf,"RUL_A1",pool01%RUL_A1)
    iOut= iOut .and. GfForceSetValue(gf,"RUL_A2",pool01%RUL_A2)
    iOut= iOut .and. GfForceSetValue(gf,"RUL_A3",pool01%RUL_A3)
    iOut= iOut .and. GfForceSetValue(gf,"RUL_A4",pool01%RUL_A4)
    iOut= iOut .and. GfForceSetValue(gf,"F_POLSEL",pool01%F_POLSEL)
    iOut= iOut .and. GfForceSetValue(gf,"F_FACET",pool01%F_FACET)
    iOut= iOut .and. GfForceSetValue(gf,"F_FAC_ORIENT",pool01%F_FAC_ORIENT)
    iOut= iOut .and. GfForceSetValue(gf,"F_FAC_LATT",pool01%F_FAC_LATT)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_LENX",pool01%RFAC_LENX)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_LENY",pool01%RFAC_LENY)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_PHAX",pool01%RFAC_PHAX)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_PHAY",pool01%RFAC_PHAY)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_DELX1",pool01%RFAC_DELX1)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_DELX2",pool01%RFAC_DELX2)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_DELY1",pool01%RFAC_DELY1)
    iOut= iOut .and. GfForceSetValue(gf,"RFAC_DELY2",pool01%RFAC_DELY2)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_FAC",pool01%FILE_FAC)
    iOut= iOut .and. GfForceSetValue(gf,"F_SEGMENT",pool01%F_SEGMENT)
    iOut= iOut .and. GfForceSetValue(gf,"ISEG_XNUM",pool01%ISEG_XNUM)
    iOut= iOut .and. GfForceSetValue(gf,"ISEG_YNUM",pool01%ISEG_YNUM)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SEGMENT",pool01%FILE_SEGMENT)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SEGP",pool01%FILE_SEGP)
    iOut= iOut .and. GfForceSetValue(gf,"SEG_LENX",pool01%SEG_LENX)
    iOut= iOut .and. GfForceSetValue(gf,"SEG_LENY",pool01%SEG_LENY)
    iOut= iOut .and. GfForceSetValue(gf,"F_KOMA",pool01%F_KOMA)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_KOMA",pool01%FILE_KOMA)
    iOut= iOut .and. GfForceSetValue(gf,"F_EXIT_SHAPE",pool01%F_EXIT_SHAPE)
    iOut= iOut .and. GfForceSetValue(gf,"F_INC_MNOR_ANG",pool01%F_INC_MNOR_ANG)
    iOut= iOut .and. GfForceSetValue(gf,"ZKO_LENGTH",pool01%ZKO_LENGTH)
    iOut= iOut .and. GfForceSetValue(gf,"RKOMA_CX",pool01%RKOMA_CX)
    iOut= iOut .and. GfForceSetValue(gf,"RKOMA_CY",pool01%RKOMA_CY)
    iOut= iOut .and. GfForceSetValue(gf,"F_KOMA_CA",pool01%F_KOMA_CA)
    iOut= iOut .and. GfForceSetValue(gf,"FILE_KOMA_CA",pool01%FILE_KOMA_CA)
    iOut= iOut .and. GfForceSetValue(gf,"F_KOMA_BOUNCE",pool01%F_KOMA_BOUNCE)
    iOut= iOut .and. GfForceSetValue(gf,"X_RIP_AMP",pool01%X_RIP_AMP)
    iOut= iOut .and. GfForceSetValue(gf,"X_RIP_WAV",pool01%X_RIP_WAV)
    iOut= iOut .and. GfForceSetValue(gf,"X_PHASE",pool01%X_PHASE)
    iOut= iOut .and. GfForceSetValue(gf,"Y_RIP_AMP",pool01%Y_RIP_AMP)
    iOut= iOut .and. GfForceSetValue(gf,"Y_RIP_WAV",pool01%Y_RIP_WAV)
    iOut= iOut .and. GfForceSetValue(gf,"Y_PHASE",pool01%Y_PHASE)
    iOut= iOut .and. GfForceSetValue(gf,"N_RIP",pool01%N_RIP)
    iOut= iOut .and. GfForceSetValue(gf,"ROUGH_X",pool01%ROUGH_X)
    iOut= iOut .and. GfForceSetValue(gf,"ROUGH_Y",pool01%ROUGH_Y)
    iOut= iOut .and. GfForceSetValue(gf,"OE_NUMBER",pool01%OE_NUMBER)
    iOut= iOut .and. GfForceSetValue(gf,"IDUMMY",pool01%IDUMMY)
    iOut= iOut .and. GfForceSetValue(gf,"DUMMY",pool01%DUMMY)
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(1)",pool01%CX_SLIT(1))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(2)",pool01%CX_SLIT(2))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(3)",pool01%CX_SLIT(3))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(4)",pool01%CX_SLIT(4))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(5)",pool01%CX_SLIT(5))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(6)",pool01%CX_SLIT(6))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(7)",pool01%CX_SLIT(7))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(8)",pool01%CX_SLIT(8))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(9)",pool01%CX_SLIT(9))
    iOut= iOut .and. GfForceSetValue(gf,"CX_SLIT(10)",pool01%CX_SLIT(10))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(1)",pool01%CZ_SLIT(1))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(2)",pool01%CZ_SLIT(2))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(3)",pool01%CZ_SLIT(3))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(4)",pool01%CZ_SLIT(4))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(5)",pool01%CZ_SLIT(5))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(6)",pool01%CZ_SLIT(6))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(7)",pool01%CZ_SLIT(7))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(8)",pool01%CZ_SLIT(8))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(9)",pool01%CZ_SLIT(9))
    iOut= iOut .and. GfForceSetValue(gf,"CZ_SLIT(10)",pool01%CZ_SLIT(10))
    iOut= iOut .and. GfForceSetValue(gf,"D_PLATE(1)",pool01%D_PLATE(1))
    iOut= iOut .and. GfForceSetValue(gf,"D_PLATE(2)",pool01%D_PLATE(2))
    iOut= iOut .and. GfForceSetValue(gf,"D_PLATE(3)",pool01%D_PLATE(3))
    iOut= iOut .and. GfForceSetValue(gf,"D_PLATE(4)",pool01%D_PLATE(4))
    iOut= iOut .and. GfForceSetValue(gf,"D_PLATE(5)",pool01%D_PLATE(5))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(1)",pool01%FILE_ABS(1))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(2)",pool01%FILE_ABS(2))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(3)",pool01%FILE_ABS(3))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(4)",pool01%FILE_ABS(4))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(5)",pool01%FILE_ABS(5))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(6)",pool01%FILE_ABS(6))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(7)",pool01%FILE_ABS(7))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(8)",pool01%FILE_ABS(8))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(9)",pool01%FILE_ABS(9))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_ABS(10)",pool01%FILE_ABS(10))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(1)",pool01%FILE_SCR_EXT(1))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(2)",pool01%FILE_SCR_EXT(2))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(3)",pool01%FILE_SCR_EXT(3))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(4)",pool01%FILE_SCR_EXT(4))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(5)",pool01%FILE_SCR_EXT(5))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(6)",pool01%FILE_SCR_EXT(6))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(7)",pool01%FILE_SCR_EXT(7))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(8)",pool01%FILE_SCR_EXT(8))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(9)",pool01%FILE_SCR_EXT(9))
    iOut= iOut .and. GfForceSetValue(gf,"FILE_SCR_EXT(10)",pool01%FILE_SCR_EXT(10))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(1)",pool01%I_ABS(1))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(2)",pool01%I_ABS(2))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(3)",pool01%I_ABS(3))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(4)",pool01%I_ABS(4))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(5)",pool01%I_ABS(5))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(6)",pool01%I_ABS(6))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(7)",pool01%I_ABS(7))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(8)",pool01%I_ABS(8))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(9)",pool01%I_ABS(9))
    iOut= iOut .and. GfForceSetValue(gf,"I_ABS(10)",pool01%I_ABS(10))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(1)",pool01%I_SCREEN(1))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(2)",pool01%I_SCREEN(2))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(3)",pool01%I_SCREEN(3))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(4)",pool01%I_SCREEN(4))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(5)",pool01%I_SCREEN(5))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(6)",pool01%I_SCREEN(6))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(7)",pool01%I_SCREEN(7))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(8)",pool01%I_SCREEN(8))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(9)",pool01%I_SCREEN(9))
    iOut= iOut .and. GfForceSetValue(gf,"I_SCREEN(10)",pool01%I_SCREEN(10))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(1)",pool01%I_SLIT(1))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(2)",pool01%I_SLIT(2))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(3)",pool01%I_SLIT(3))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(4)",pool01%I_SLIT(4))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(5)",pool01%I_SLIT(5))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(6)",pool01%I_SLIT(6))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(7)",pool01%I_SLIT(7))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(8)",pool01%I_SLIT(8))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(9)",pool01%I_SLIT(9))
    iOut= iOut .and. GfForceSetValue(gf,"I_SLIT(10)",pool01%I_SLIT(10))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(1)",pool01%I_STOP(1))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(2)",pool01%I_STOP(2))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(3)",pool01%I_STOP(3))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(4)",pool01%I_STOP(4))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(5)",pool01%I_STOP(5))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(6)",pool01%I_STOP(6))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(7)",pool01%I_STOP(7))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(8)",pool01%I_STOP(8))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(9)",pool01%I_STOP(9))
    iOut= iOut .and. GfForceSetValue(gf,"I_STOP(10)",pool01%I_STOP(10))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(1)",pool01%K_SLIT(1))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(2)",pool01%K_SLIT(2))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(3)",pool01%K_SLIT(3))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(4)",pool01%K_SLIT(4))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(5)",pool01%K_SLIT(5))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(6)",pool01%K_SLIT(6))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(7)",pool01%K_SLIT(7))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(8)",pool01%K_SLIT(8))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(9)",pool01%K_SLIT(9))
    iOut= iOut .and. GfForceSetValue(gf,"K_SLIT(10)",pool01%K_SLIT(10))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(1)",pool01%RX_SLIT(1))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(2)",pool01%RX_SLIT(2))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(3)",pool01%RX_SLIT(3))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(4)",pool01%RX_SLIT(4))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(5)",pool01%RX_SLIT(5))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(6)",pool01%RX_SLIT(6))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(7)",pool01%RX_SLIT(7))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(8)",pool01%RX_SLIT(8))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(9)",pool01%RX_SLIT(9))
    iOut= iOut .and. GfForceSetValue(gf,"RX_SLIT(10)",pool01%RX_SLIT(10))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(1)",pool01%RZ_SLIT(1))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(2)",pool01%RZ_SLIT(2))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(3)",pool01%RZ_SLIT(3))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(4)",pool01%RZ_SLIT(4))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(5)",pool01%RZ_SLIT(5))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(6)",pool01%RZ_SLIT(6))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(7)",pool01%RZ_SLIT(7))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(8)",pool01%RZ_SLIT(8))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(9)",pool01%RZ_SLIT(9))
    iOut= iOut .and. GfForceSetValue(gf,"RZ_SLIT(10)",pool01%RZ_SLIT(10))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(1)",pool01%SCR_NUMBER(1))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(2)",pool01%SCR_NUMBER(2))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(3)",pool01%SCR_NUMBER(3))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(4)",pool01%SCR_NUMBER(4))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(5)",pool01%SCR_NUMBER(5))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(6)",pool01%SCR_NUMBER(6))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(7)",pool01%SCR_NUMBER(7))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(8)",pool01%SCR_NUMBER(8))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(9)",pool01%SCR_NUMBER(9))
    iOut= iOut .and. GfForceSetValue(gf,"SCR_NUMBER(10)",pool01%SCR_NUMBER(10))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(1)",pool01%SL_DIS(1))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(2)",pool01%SL_DIS(2))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(3)",pool01%SL_DIS(3))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(4)",pool01%SL_DIS(4))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(5)",pool01%SL_DIS(5))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(6)",pool01%SL_DIS(6))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(7)",pool01%SL_DIS(7))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(8)",pool01%SL_DIS(8))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(9)",pool01%SL_DIS(9))
    iOut= iOut .and. GfForceSetValue(gf,"SL_DIS(10)",pool01%SL_DIS(10))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(1)",pool01%THICK(1))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(2)",pool01%THICK(2))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(3)",pool01%THICK(3))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(4)",pool01%THICK(4))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(5)",pool01%THICK(5))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(6)",pool01%THICK(6))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(7)",pool01%THICK(7))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(8)",pool01%THICK(8))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(9)",pool01%THICK(9))
    iOut= iOut .and. GfForceSetValue(gf,"THICK(10)",pool01%THICK(10))
    !! END CODE CREATED AUTOMATICALLY (makecode1.pro)
  End Subroutine PoolOEToGf
  
  !
  !
  !
  
  
  Subroutine GfToPoolSource(gf,pool00)
    
    type(gfType), intent(in)        :: gf 
    type(poolSource), intent(inout) :: pool00 
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

!print *,'<><><> GfToPoolSource: OS_NAME is: '//OS_NAME
    SELECT CASE (OS_NAME)
      CASE ("Linux") 
           iout = gffilewrite(gf,"/dev/null")
      CASE ("Windows") 
           iout = gffilewrite(gf,"tmp.dat")
      CASE DEFAULT
           iout = gffilewrite(gf,"/dev/null")
    END SELECT

    !! START CODE CREATED AUTOMATICALLY (makecode1.pro)
    iOut= iOut .and. GfGetValue(gf,"FDISTR",pool00%FDISTR) 
    iOut= iOut .and. GfGetValue(gf,"FGRID",pool00%FGRID) 
    iOut= iOut .and. GfGetValue(gf,"FSOUR",pool00%FSOUR)
    iOut= iOut .and. GfGetValue(gf,"FSOURCE_DEPTH",pool00%FSOURCE_DEPTH) 
    iOut= iOut .and. GfGetValue(gf,"F_COHER",pool00%F_COHER) 
    iOut= iOut .and. GfGetValue(gf,"F_COLOR",pool00%F_COLOR) 
    iOut= iOut .and. GfGetValue(gf,"F_PHOT",pool00%F_PHOT) 
    iOut= iOut .and. GfGetValue(gf,"F_POL",pool00%F_POL) 
    iOut= iOut .and. GfGetValue(gf,"F_POLAR",pool00%F_POLAR)
    iOut= iOut .and. GfGetValue(gf,"F_OPD",pool00%F_OPD) 
    iOut= iOut .and. GfGetValue(gf,"F_WIGGLER",pool00%F_WIGGLER) 
    iOut= iOut .and. GfGetValue(gf,"F_BOUND_SOUR",pool00%F_BOUND_SOUR) 
    iOut= iOut .and. GfGetValue(gf,"F_SR_TYPE",pool00%F_SR_TYPE) 
    iOut= iOut .and. GfGetValue(gf,"ISTAR1",pool00%ISTAR1) 
    iOut= iOut .and. GfGetValue(gf,"NPOINT",pool00%NPOINT) 
    iOut= iOut .and. GfGetValue(gf,"NCOL",pool00%NCOL) 
    iOut= iOut .and. GfGetValue(gf,"N_CIRCLE",pool00%N_CIRCLE) 
    iOut= iOut .and. GfGetValue(gf,"N_COLOR",pool00%N_COLOR) 
    iOut= iOut .and. GfGetValue(gf,"N_CONE",pool00%N_CONE) 
    iOut= iOut .and. GfGetValue(gf,"IDO_VX",pool00%IDO_VX) 
    iOut= iOut .and. GfGetValue(gf,"IDO_VZ",pool00%IDO_VZ) 
    iOut= iOut .and. GfGetValue(gf,"IDO_X_S",pool00%IDO_X_S) 
    iOut= iOut .and. GfGetValue(gf,"IDO_Y_S",pool00%IDO_Y_S) 
    iOut= iOut .and. GfGetValue(gf,"IDO_Z_S",pool00%IDO_Z_S) 
    iOut= iOut .and. GfGetValue(gf,"IDO_XL",pool00%IDO_XL) 
    iOut= iOut .and. GfGetValue(gf,"IDO_XN",pool00%IDO_XN) 
    iOut= iOut .and. GfGetValue(gf,"IDO_ZL",pool00%IDO_ZL) 
    iOut= iOut .and. GfGetValue(gf,"IDO_ZN",pool00%IDO_ZN) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL1",pool00%SIGXL1) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL2",pool00%SIGXL2) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL3",pool00%SIGXL3) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL4",pool00%SIGXL4) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL5",pool00%SIGXL5) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL6",pool00%SIGXL6) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL7",pool00%SIGXL7) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL8",pool00%SIGXL8) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL9",pool00%SIGXL9) 
    iOut= iOut .and. GfGetValue(gf,"SIGXL10",pool00%SIGXL10) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL1",pool00%SIGZL1) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL2",pool00%SIGZL2) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL3",pool00%SIGZL3) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL4",pool00%SIGZL4) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL5",pool00%SIGZL5) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL6",pool00%SIGZL6) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL7",pool00%SIGZL7) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL8",pool00%SIGZL8) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL9",pool00%SIGZL9) 
    iOut= iOut .and. GfGetValue(gf,"SIGZL10",pool00%SIGZL10) 
    iOut= iOut .and. GfGetValue(gf,"CONV_FACT",pool00%CONV_FACT) 
    iOut= iOut .and. GfGetValue(gf,"CONE_MAX",pool00%CONE_MAX) 
    iOut= iOut .and. GfGetValue(gf,"CONE_MIN",pool00%CONE_MIN) 
    iOut= iOut .and. GfGetValue(gf,"EPSI_DX",pool00%EPSI_DX) 
    iOut= iOut .and. GfGetValue(gf,"EPSI_DZ",pool00%EPSI_DZ) 
    iOut= iOut .and. GfGetValue(gf,"EPSI_X",pool00%EPSI_X) 
    iOut= iOut .and. GfGetValue(gf,"EPSI_Z",pool00%EPSI_Z) 
    iOut= iOut .and. GfGetValue(gf,"HDIV1",pool00%HDIV1) 
    iOut= iOut .and. GfGetValue(gf,"HDIV2",pool00%HDIV2) 
    iOut= iOut .and. GfGetValue(gf,"PH1",pool00%PH1) 
    iOut= iOut .and. GfGetValue(gf,"PH2",pool00%PH2) 
    iOut= iOut .and. GfGetValue(gf,"PH3",pool00%PH3) 
    iOut= iOut .and. GfGetValue(gf,"PH4",pool00%PH4) 
    iOut= iOut .and. GfGetValue(gf,"PH5",pool00%PH5) 
    iOut= iOut .and. GfGetValue(gf,"PH6",pool00%PH6) 
    iOut= iOut .and. GfGetValue(gf,"PH7",pool00%PH7) 
    iOut= iOut .and. GfGetValue(gf,"PH8",pool00%PH8) 
    iOut= iOut .and. GfGetValue(gf,"PH9",pool00%PH9) 
    iOut= iOut .and. GfGetValue(gf,"PH10",pool00%PH10) 
    iOut= iOut .and. GfGetValue(gf,"RL1",pool00%RL1) 
    iOut= iOut .and. GfGetValue(gf,"RL2",pool00%RL2) 
    iOut= iOut .and. GfGetValue(gf,"RL3",pool00%RL3) 
    iOut= iOut .and. GfGetValue(gf,"RL4",pool00%RL4) 
    iOut= iOut .and. GfGetValue(gf,"RL5",pool00%RL5) 
    iOut= iOut .and. GfGetValue(gf,"RL6",pool00%RL6) 
    iOut= iOut .and. GfGetValue(gf,"RL7",pool00%RL7) 
    iOut= iOut .and. GfGetValue(gf,"RL8",pool00%RL8) 
    iOut= iOut .and. GfGetValue(gf,"RL9",pool00%RL9) 
    iOut= iOut .and. GfGetValue(gf,"RL10",pool00%RL10) 
    iOut= iOut .and. GfGetValue(gf,"BENER",pool00%BENER) 
    iOut= iOut .and. GfGetValue(gf,"POL_ANGLE",pool00%POL_ANGLE) 
    iOut= iOut .and. GfGetValue(gf,"POL_DEG",pool00%POL_DEG) 
    iOut= iOut .and. GfGetValue(gf,"R_ALADDIN",pool00%R_ALADDIN) 
    iOut= iOut .and. GfGetValue(gf,"R_MAGNET",pool00%R_MAGNET) 
    iOut= iOut .and. GfGetValue(gf,"SIGDIX",pool00%SIGDIX) 
    iOut= iOut .and. GfGetValue(gf,"SIGDIZ",pool00%SIGDIZ) 
    iOut= iOut .and. GfGetValue(gf,"SIGMAX",pool00%SIGMAX) 
    iOut= iOut .and. GfGetValue(gf,"SIGMAY",pool00%SIGMAY) 
    iOut= iOut .and. GfGetValue(gf,"SIGMAZ",pool00%SIGMAZ) 
    iOut= iOut .and. GfGetValue(gf,"VDIV1",pool00%VDIV1) 
    iOut= iOut .and. GfGetValue(gf,"VDIV2",pool00%VDIV2) 
    iOut= iOut .and. GfGetValue(gf,"WXSOU",pool00%WXSOU) 
    iOut= iOut .and. GfGetValue(gf,"WYSOU",pool00%WYSOU) 
    iOut= iOut .and. GfGetValue(gf,"WZSOU",pool00%WZSOU) 
    iOut= iOut .and. GfGetValue(gf,"PLASMA_ANGLE",pool00%PLASMA_ANGLE) 
    iOut= iOut .and. GfGetValue(gf,"FILE_TRAJ",pool00%FILE_TRAJ) 
    iOut= iOut .and. GfGetValue(gf,"FILE_SOURCE",pool00%FILE_SOURCE) 
    iOut= iOut .and. GfGetValue(gf,"FILE_BOUND",pool00%FILE_BOUND) 
    iOut= iOut .and. GfGetValue(gf,"OE_NUMBER",pool00%OE_NUMBER) 
    iOut= iOut .and. GfGetValue(gf,"IDUMMY",pool00%IDUMMY) 
    iOut= iOut .and. GfGetValue(gf,"DUMMY",pool00%DUMMY) 
    iOut= iOut .and. GfGetValue(gf,"F_NEW",pool00%F_NEW) 
    !! END CODE CREATED AUTOMATICALLY (makecode1.pro)
  End Subroutine GfToPoolSource
  
  !
  !
  !
  
  
  Subroutine GfToPoolOE(gf,pool01)
    
    type(gfType),intent(in)     :: gf 
    type(poolOE),intent(inout)  :: pool01 
    logical                     :: iOut
    integer(kind=ski)             :: zero=0
    
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

!print *,'<><><> GfToPoolOE: OS_NAME is: '//OS_NAME
    SELECT CASE (OS_NAME)
      CASE ("Linux") 
           iout = gffilewrite(gf,"/dev/null")
      CASE ("Windows") 
           iout = gffilewrite(gf,"tmp.dat")
      CASE DEFAULT
           iout = gffilewrite(gf,"/dev/null")
    END SELECT

    !! START CODE CREATED AUTOMATICALLY (makecode1.pro)
    
    iOut= iOut .and. GfGetValue(gf,"FMIRR",pool01%FMIRR)
    iOut= iOut .and. GfGetValue(gf,"F_TORUS",pool01%F_TORUS)
    iOut= iOut .and. GfGetValue(gf,"FCYL",pool01%FCYL)
    iOut= iOut .and. GfGetValue(gf,"F_EXT",pool01%F_EXT)
    iOut= iOut .and. GfGetValue(gf,"FSTAT",pool01%FSTAT)
    iOut= iOut .and. GfGetValue(gf,"F_SCREEN",pool01%F_SCREEN)
    iOut= iOut .and. GfGetValue(gf,"F_PLATE",pool01%F_PLATE)
    iOut= iOut .and. GfGetValue(gf,"FSLIT",pool01%FSLIT)
    iOut= iOut .and. GfGetValue(gf,"FWRITE",pool01%FWRITE)
    iOut= iOut .and. GfGetValue(gf,"F_RIPPLE",pool01%F_RIPPLE)
    iOut= iOut .and. GfGetValue(gf,"F_MOVE",pool01%F_MOVE)
    iOut= iOut .and. GfGetValue(gf,"F_THICK",pool01%F_THICK)
    iOut= iOut .and. GfGetValue(gf,"F_BRAGG_A",pool01%F_BRAGG_A)
    iOut= iOut .and. GfGetValue(gf,"F_G_S",pool01%F_G_S)
    iOut= iOut .and. GfGetValue(gf,"F_R_RAN",pool01%F_R_RAN)
    iOut= iOut .and. GfGetValue(gf,"F_GRATING",pool01%F_GRATING)
    iOut= iOut .and. GfGetValue(gf,"F_MOSAIC",pool01%F_MOSAIC)
    iOut= iOut .and. GfGetValue(gf,"F_JOHANSSON",pool01%F_JOHANSSON)
    iOut= iOut .and. GfGetValue(gf,"F_SIDE",pool01%F_SIDE)
    iOut= iOut .and. GfGetValue(gf,"F_CENTRAL",pool01%F_CENTRAL)
    iOut= iOut .and. GfGetValue(gf,"F_CONVEX",pool01%F_CONVEX)
    iOut= iOut .and. GfGetValue(gf,"F_REFLEC",pool01%F_REFLEC)
    iOut= iOut .and. GfGetValue(gf,"F_RUL_ABS",pool01%F_RUL_ABS)
    iOut= iOut .and. GfGetValue(gf,"F_RULING",pool01%F_RULING)
    iOut= iOut .and. GfGetValue(gf,"F_PW",pool01%F_PW)
    iOut= iOut .and. GfGetValue(gf,"F_PW_C",pool01%F_PW_C)
    iOut= iOut .and. GfGetValue(gf,"F_VIRTUAL",pool01%F_VIRTUAL)
    iOut= iOut .and. GfGetValue(gf,"FSHAPE",pool01%FSHAPE)
    iOut= iOut .and. GfGetValue(gf,"FHIT_C",pool01%FHIT_C)
    iOut= iOut .and. GfGetValue(gf,"F_MONO",pool01%F_MONO)
    iOut= iOut .and. GfGetValue(gf,"F_REFRAC",pool01%F_REFRAC)
    iOut= iOut .and. GfGetValue(gf,"F_DEFAULT",pool01%F_DEFAULT)
    iOut= iOut .and. GfGetValue(gf,"F_REFL",pool01%F_REFL)
    iOut= iOut .and. GfGetValue(gf,"F_HUNT",pool01%F_HUNT)
    iOut= iOut .and. GfGetValue(gf,"F_CRYSTAL",pool01%F_CRYSTAL)
    iOut= iOut .and. GfGetValue(gf,"F_PHOT_CENT",pool01%F_PHOT_CENT)
    iOut= iOut .and. GfGetValue(gf,"F_ROUGHNESS",pool01%F_ROUGHNESS)
    iOut= iOut .and. GfGetValue(gf,"F_ANGLE",pool01%F_ANGLE)
    iOut= iOut .and. GfGetValue(gf,"NPOINT",pool01%NPOINTOE)
    iOut= iOut .and. GfGetValue(gf,"NCOL",pool01%NCOL)
    iOut= iOut .and. GfGetValue(gf,"N_SCREEN",pool01%N_SCREEN)
    iOut= iOut .and. GfGetValue(gf,"ISTAR1",pool01%ISTAR1)
    iOut= iOut .and. GfGetValue(gf,"CIL_ANG",pool01%CIL_ANG)
    iOut= iOut .and. GfGetValue(gf,"ELL_THE",pool01%ELL_THE)
    iOut= iOut .and. GfGetValue(gf,"N_PLATES",pool01%N_PLATES)
    iOut= iOut .and. GfGetValue(gf,"IG_SEED",pool01%IG_SEED)
    iOut= iOut .and. GfGetValue(gf,"MOSAIC_SEED",pool01%MOSAIC_SEED)
    iOut= iOut .and. GfGetValue(gf,"ALPHA",pool01%ALPHA)
    iOut= iOut .and. GfGetValue(gf,"SSOUR",pool01%SSOUR)
    iOut= iOut .and. GfGetValue(gf,"THETA",pool01%THETA)
    iOut= iOut .and. GfGetValue(gf,"SIMAG",pool01%SIMAG)
    iOut= iOut .and. GfGetValue(gf,"RDSOUR",pool01%RDSOUR)
    iOut= iOut .and. GfGetValue(gf,"RTHETA",pool01%RTHETA)
    iOut= iOut .and. GfGetValue(gf,"OFF_SOUX",pool01%OFF_SOUX)
    iOut= iOut .and. GfGetValue(gf,"OFF_SOUY",pool01%OFF_SOUY)
    iOut= iOut .and. GfGetValue(gf,"OFF_SOUZ",pool01%OFF_SOUZ)
    iOut= iOut .and. GfGetValue(gf,"ALPHA_S",pool01%ALPHA_S)
    iOut= iOut .and. GfGetValue(gf,"RLEN1",pool01%RLEN1)
    iOut= iOut .and. GfGetValue(gf,"RLEN2",pool01%RLEN2)
    iOut= iOut .and. GfGetValue(gf,"RMIRR",pool01%RMIRR)
    iOut= iOut .and. GfGetValue(gf,"AXMAJ",pool01%AXMAJ)
    iOut= iOut .and. GfGetValue(gf,"AXMIN",pool01%AXMIN)
    iOut= iOut .and. GfGetValue(gf,"CONE_A",pool01%CONE_A)
    iOut= iOut .and. GfGetValue(gf,"R_MAJ",pool01%R_MAJ)
    iOut= iOut .and. GfGetValue(gf,"R_MIN",pool01%R_MIN)
    iOut= iOut .and. GfGetValue(gf,"RWIDX1",pool01%RWIDX1)
    iOut= iOut .and. GfGetValue(gf,"RWIDX2",pool01%RWIDX2)
    iOut= iOut .and. GfGetValue(gf,"PARAM",pool01%PARAM)
    iOut= iOut .and. GfGetValue(gf,"HUNT_H",pool01%HUNT_H)
    iOut= iOut .and. GfGetValue(gf,"HUNT_L",pool01%HUNT_L)
    iOut= iOut .and. GfGetValue(gf,"BLAZE",pool01%BLAZE)
    iOut= iOut .and. GfGetValue(gf,"RULING",pool01%RULING)
    iOut= iOut .and. GfGetValue(gf,"ORDER",pool01%ORDER)
    iOut= iOut .and. GfGetValue(gf,"PHOT_CENT",pool01%PHOT_CENT)
    iOut= iOut .and. GfGetValue(gf,"X_ROT",pool01%X_ROT)
    iOut= iOut .and. GfGetValue(gf,"D_SPACING",pool01%D_SPACING)
    iOut= iOut .and. GfGetValue(gf,"A_BRAGG",pool01%A_BRAGG)
    iOut= iOut .and. GfGetValue(gf,"SPREAD_MOS",pool01%SPREAD_MOS)
    iOut= iOut .and. GfGetValue(gf,"THICKNESS",pool01%THICKNESS)
    iOut= iOut .and. GfGetValue(gf,"R_JOHANSSON",pool01%R_JOHANSSON)
    iOut= iOut .and. GfGetValue(gf,"Y_ROT",pool01%Y_ROT)
    iOut= iOut .and. GfGetValue(gf,"Z_ROT",pool01%Z_ROT)
    iOut= iOut .and. GfGetValue(gf,"OFFX",pool01%OFFX)
    iOut= iOut .and. GfGetValue(gf,"OFFY",pool01%OFFY)
    iOut= iOut .and. GfGetValue(gf,"OFFZ",pool01%OFFZ)
    iOut= iOut .and. GfGetValue(gf,"SLLEN",pool01%SLLEN)
    iOut= iOut .and. GfGetValue(gf,"SLWID",pool01%SLWID)
    iOut= iOut .and. GfGetValue(gf,"SLTILT",pool01%SLTILT)
    iOut= iOut .and. GfGetValue(gf,"COD_LEN",pool01%COD_LEN)
    iOut= iOut .and. GfGetValue(gf,"COD_WID",pool01%COD_WID)
    iOut= iOut .and. GfGetValue(gf,"X_SOUR",pool01%X_SOUR)
    iOut= iOut .and. GfGetValue(gf,"Y_SOUR",pool01%Y_SOUR)
    iOut= iOut .and. GfGetValue(gf,"Z_SOUR",pool01%Z_SOUR)
    iOut= iOut .and. GfGetValue(gf,"X_SOUR_ROT",pool01%X_SOUR_ROT)
    iOut= iOut .and. GfGetValue(gf,"Y_SOUR_ROT",pool01%Y_SOUR_ROT)
    iOut= iOut .and. GfGetValue(gf,"Z_SOUR_ROT",pool01%Z_SOUR_ROT)
    iOut= iOut .and. GfGetValue(gf,"R_LAMBDA",pool01%R_LAMBDA)
    iOut= iOut .and. GfGetValue(gf,"THETA_I",pool01%THETA_I)
    iOut= iOut .and. GfGetValue(gf,"ALPHA_I",pool01%ALPHA_I)
    iOut= iOut .and. GfGetValue(gf,"T_INCIDENCE",pool01%T_INCIDENCE)
    iOut= iOut .and. GfGetValue(gf,"T_SOURCE",pool01%T_SOURCE)
    iOut= iOut .and. GfGetValue(gf,"T_IMAGE",pool01%T_IMAGE)
    iOut= iOut .and. GfGetValue(gf,"T_REFLECTION",pool01%T_REFLECTION)
    iOut= iOut .and. GfGetValue(gf,"FILE_SOURCE",pool01%FILE_SOURCE)
    iOut= iOut .and. GfGetValue(gf,"FILE_RIP",pool01%FILE_RIP)
    iOut= iOut .and. GfGetValue(gf,"FILE_REFL",pool01%FILE_REFL)
    iOut= iOut .and. GfGetValue(gf,"FILE_MIR",pool01%FILE_MIR)
    iOut= iOut .and. GfGetValue(gf,"FILE_ROUGH",pool01%FILE_ROUGH)
    iOut= iOut .and. GfGetValue(gf,"FZP",pool01%FZP)
    iOut= iOut .and. GfGetValue(gf,"HOLO_R1",pool01%HOLO_R1)
    iOut= iOut .and. GfGetValue(gf,"HOLO_R2",pool01%HOLO_R2)
    iOut= iOut .and. GfGetValue(gf,"HOLO_DEL",pool01%HOLO_DEL)
    iOut= iOut .and. GfGetValue(gf,"HOLO_GAM",pool01%HOLO_GAM)
    iOut= iOut .and. GfGetValue(gf,"HOLO_W",pool01%HOLO_W)
    iOut= iOut .and. GfGetValue(gf,"HOLO_RT1",pool01%HOLO_RT1)
    iOut= iOut .and. GfGetValue(gf,"HOLO_RT2",pool01%HOLO_RT2)
    iOut= iOut .and. GfGetValue(gf,"AZIM_FAN",pool01%AZIM_FAN)
    iOut= iOut .and. GfGetValue(gf,"DIST_FAN",pool01%DIST_FAN)
    iOut= iOut .and. GfGetValue(gf,"COMA_FAC",pool01%COMA_FAC)
    iOut= iOut .and. GfGetValue(gf,"ALFA",pool01%ALFA)
    iOut= iOut .and. GfGetValue(gf,"GAMMA",pool01%GAMMA)
    iOut= iOut .and. GfGetValue(gf,"R_IND_OBJ",pool01%R_IND_OBJ)
    iOut= iOut .and. GfGetValue(gf,"R_IND_IMA",pool01%R_IND_IMA)
    iOut= iOut .and. GfGetValue(gf,"RUL_A1",pool01%RUL_A1)
    iOut= iOut .and. GfGetValue(gf,"RUL_A2",pool01%RUL_A2)
    iOut= iOut .and. GfGetValue(gf,"RUL_A3",pool01%RUL_A3)
    iOut= iOut .and. GfGetValue(gf,"RUL_A4",pool01%RUL_A4)
    iOut= iOut .and. GfGetValue(gf,"F_POLSEL",pool01%F_POLSEL)
    iOut= iOut .and. GfGetValue(gf,"F_FACET",pool01%F_FACET)
    iOut= iOut .and. GfGetValue(gf,"F_FAC_ORIENT",pool01%F_FAC_ORIENT)
    iOut= iOut .and. GfGetValue(gf,"F_FAC_LATT",pool01%F_FAC_LATT)
    iOut= iOut .and. GfGetValue(gf,"RFAC_LENX",pool01%RFAC_LENX)
    iOut= iOut .and. GfGetValue(gf,"RFAC_LENY",pool01%RFAC_LENY)
    iOut= iOut .and. GfGetValue(gf,"RFAC_PHAX",pool01%RFAC_PHAX)
    iOut= iOut .and. GfGetValue(gf,"RFAC_PHAY",pool01%RFAC_PHAY)
    iOut= iOut .and. GfGetValue(gf,"RFAC_DELX1",pool01%RFAC_DELX1)
    iOut= iOut .and. GfGetValue(gf,"RFAC_DELX2",pool01%RFAC_DELX2)
    iOut= iOut .and. GfGetValue(gf,"RFAC_DELY1",pool01%RFAC_DELY1)
    iOut= iOut .and. GfGetValue(gf,"RFAC_DELY2",pool01%RFAC_DELY2)
    iOut= iOut .and. GfGetValue(gf,"FILE_FAC",pool01%FILE_FAC)
    iOut= iOut .and. GfGetValue(gf,"F_SEGMENT",pool01%F_SEGMENT)
    iOut= iOut .and. GfGetValue(gf,"ISEG_XNUM",pool01%ISEG_XNUM)
    iOut= iOut .and. GfGetValue(gf,"ISEG_YNUM",pool01%ISEG_YNUM)
    iOut= iOut .and. GfGetValue(gf,"FILE_SEGMENT",pool01%FILE_SEGMENT)
    iOut= iOut .and. GfGetValue(gf,"FILE_SEGP",pool01%FILE_SEGP)
    iOut= iOut .and. GfGetValue(gf,"SEG_LENX",pool01%SEG_LENX)
    iOut= iOut .and. GfGetValue(gf,"SEG_LENY",pool01%SEG_LENY)
    iOut= iOut .and. GfGetValue(gf,"F_KOMA",pool01%F_KOMA)
    iOut= iOut .and. GfGetValue(gf,"FILE_KOMA",pool01%FILE_KOMA)
    iOut= iOut .and. GfGetValue(gf,"F_EXIT_SHAPE",pool01%F_EXIT_SHAPE)
    iOut= iOut .and. GfGetValue(gf,"F_INC_MNOR_ANG",pool01%F_INC_MNOR_ANG)
    iOut= iOut .and. GfGetValue(gf,"ZKO_LENGTH",pool01%ZKO_LENGTH)
    iOut= iOut .and. GfGetValue(gf,"RKOMA_CX",pool01%RKOMA_CX)
    iOut= iOut .and. GfGetValue(gf,"RKOMA_CY",pool01%RKOMA_CY)
    iOut= iOut .and. GfGetValue(gf,"F_KOMA_CA",pool01%F_KOMA_CA)
    iOut= iOut .and. GfGetValue(gf,"FILE_KOMA_CA",pool01%FILE_KOMA_CA)
    iOut= iOut .and. GfGetValue(gf,"F_KOMA_BOUNCE",pool01%F_KOMA_BOUNCE)
    iOut= iOut .and. GfGetValue(gf,"X_RIP_AMP",pool01%X_RIP_AMP)
    iOut= iOut .and. GfGetValue(gf,"X_RIP_WAV",pool01%X_RIP_WAV)
    iOut= iOut .and. GfGetValue(gf,"X_PHASE",pool01%X_PHASE)
    iOut= iOut .and. GfGetValue(gf,"Y_RIP_AMP",pool01%Y_RIP_AMP)
    iOut= iOut .and. GfGetValue(gf,"Y_RIP_WAV",pool01%Y_RIP_WAV)
    iOut= iOut .and. GfGetValue(gf,"Y_PHASE",pool01%Y_PHASE)
    iOut= iOut .and. GfGetValue(gf,"N_RIP",pool01%N_RIP)
    iOut= iOut .and. GfGetValue(gf,"ROUGH_X",pool01%ROUGH_X)
    iOut= iOut .and. GfGetValue(gf,"ROUGH_Y",pool01%ROUGH_Y)
    iOut= iOut .and. GfGetValue(gf,"OE_NUMBER",pool01%OE_NUMBER)
    iOut= iOut .and. GfGetValue(gf,"IDUMMY",pool01%IDUMMY)
    iOut= iOut .and. GfGetValue(gf,"DUMMY",pool01%DUMMY)
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(1)",pool01%CX_SLIT(1))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(2)",pool01%CX_SLIT(2))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(3)",pool01%CX_SLIT(3))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(4)",pool01%CX_SLIT(4))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(5)",pool01%CX_SLIT(5))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(6)",pool01%CX_SLIT(6))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(7)",pool01%CX_SLIT(7))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(8)",pool01%CX_SLIT(8))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(9)",pool01%CX_SLIT(9))
    iOut= iOut .and. GfGetValue(gf,"CX_SLIT(10)",pool01%CX_SLIT(10))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(1)",pool01%CZ_SLIT(1))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(2)",pool01%CZ_SLIT(2))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(3)",pool01%CZ_SLIT(3))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(4)",pool01%CZ_SLIT(4))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(5)",pool01%CZ_SLIT(5))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(6)",pool01%CZ_SLIT(6))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(7)",pool01%CZ_SLIT(7))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(8)",pool01%CZ_SLIT(8))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(9)",pool01%CZ_SLIT(9))
    iOut= iOut .and. GfGetValue(gf,"CZ_SLIT(10)",pool01%CZ_SLIT(10))
    iOut= iOut .and. GfGetValue(gf,"D_PLATE(1)",pool01%D_PLATE(1))
    iOut= iOut .and. GfGetValue(gf,"D_PLATE(2)",pool01%D_PLATE(2))
    iOut= iOut .and. GfGetValue(gf,"D_PLATE(3)",pool01%D_PLATE(3))
    iOut= iOut .and. GfGetValue(gf,"D_PLATE(4)",pool01%D_PLATE(4))
    iOut= iOut .and. GfGetValue(gf,"D_PLATE(5)",pool01%D_PLATE(5))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(1)",pool01%FILE_ABS(1))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(2)",pool01%FILE_ABS(2))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(3)",pool01%FILE_ABS(3))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(4)",pool01%FILE_ABS(4))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(5)",pool01%FILE_ABS(5))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(6)",pool01%FILE_ABS(6))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(7)",pool01%FILE_ABS(7))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(8)",pool01%FILE_ABS(8))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(9)",pool01%FILE_ABS(9))
    iOut= iOut .and. GfGetValue(gf,"FILE_ABS(10)",pool01%FILE_ABS(10))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(1)",pool01%FILE_SCR_EXT(1))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(2)",pool01%FILE_SCR_EXT(2))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(3)",pool01%FILE_SCR_EXT(3))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(4)",pool01%FILE_SCR_EXT(4))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(5)",pool01%FILE_SCR_EXT(5))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(6)",pool01%FILE_SCR_EXT(6))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(7)",pool01%FILE_SCR_EXT(7))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(8)",pool01%FILE_SCR_EXT(8))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(9)",pool01%FILE_SCR_EXT(9))
    iOut= iOut .and. GfGetValue(gf,"FILE_SCR_EXT(10)",pool01%FILE_SCR_EXT(10))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(1)",pool01%I_ABS(1))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(2)",pool01%I_ABS(2))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(3)",pool01%I_ABS(3))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(4)",pool01%I_ABS(4))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(5)",pool01%I_ABS(5))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(6)",pool01%I_ABS(6))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(7)",pool01%I_ABS(7))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(8)",pool01%I_ABS(8))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(9)",pool01%I_ABS(9))
    iOut= iOut .and. GfGetValue(gf,"I_ABS(10)",pool01%I_ABS(10))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(1)",pool01%I_SCREEN(1))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(2)",pool01%I_SCREEN(2))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(3)",pool01%I_SCREEN(3))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(4)",pool01%I_SCREEN(4))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(5)",pool01%I_SCREEN(5))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(6)",pool01%I_SCREEN(6))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(7)",pool01%I_SCREEN(7))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(8)",pool01%I_SCREEN(8))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(9)",pool01%I_SCREEN(9))
    iOut= iOut .and. GfGetValue(gf,"I_SCREEN(10)",pool01%I_SCREEN(10))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(1)",pool01%I_SLIT(1))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(2)",pool01%I_SLIT(2))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(3)",pool01%I_SLIT(3))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(4)",pool01%I_SLIT(4))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(5)",pool01%I_SLIT(5))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(6)",pool01%I_SLIT(6))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(7)",pool01%I_SLIT(7))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(8)",pool01%I_SLIT(8))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(9)",pool01%I_SLIT(9))
    iOut= iOut .and. GfGetValue(gf,"I_SLIT(10)",pool01%I_SLIT(10))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(1)",pool01%I_STOP(1))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(2)",pool01%I_STOP(2))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(3)",pool01%I_STOP(3))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(4)",pool01%I_STOP(4))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(5)",pool01%I_STOP(5))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(6)",pool01%I_STOP(6))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(7)",pool01%I_STOP(7))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(8)",pool01%I_STOP(8))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(9)",pool01%I_STOP(9))
    iOut= iOut .and. GfGetValue(gf,"I_STOP(10)",pool01%I_STOP(10))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(1)",pool01%K_SLIT(1))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(2)",pool01%K_SLIT(2))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(3)",pool01%K_SLIT(3))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(4)",pool01%K_SLIT(4))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(5)",pool01%K_SLIT(5))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(6)",pool01%K_SLIT(6))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(7)",pool01%K_SLIT(7))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(8)",pool01%K_SLIT(8))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(9)",pool01%K_SLIT(9))
    iOut= iOut .and. GfGetValue(gf,"K_SLIT(10)",pool01%K_SLIT(10))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(1)",pool01%RX_SLIT(1))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(2)",pool01%RX_SLIT(2))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(3)",pool01%RX_SLIT(3))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(4)",pool01%RX_SLIT(4))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(5)",pool01%RX_SLIT(5))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(6)",pool01%RX_SLIT(6))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(7)",pool01%RX_SLIT(7))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(8)",pool01%RX_SLIT(8))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(9)",pool01%RX_SLIT(9))
    iOut= iOut .and. GfGetValue(gf,"RX_SLIT(10)",pool01%RX_SLIT(10))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(1)",pool01%RZ_SLIT(1))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(2)",pool01%RZ_SLIT(2))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(3)",pool01%RZ_SLIT(3))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(4)",pool01%RZ_SLIT(4))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(5)",pool01%RZ_SLIT(5))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(6)",pool01%RZ_SLIT(6))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(7)",pool01%RZ_SLIT(7))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(8)",pool01%RZ_SLIT(8))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(9)",pool01%RZ_SLIT(9))
    iOut= iOut .and. GfGetValue(gf,"RZ_SLIT(10)",pool01%RZ_SLIT(10))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(1)",pool01%SCR_NUMBER(1))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(2)",pool01%SCR_NUMBER(2))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(3)",pool01%SCR_NUMBER(3))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(4)",pool01%SCR_NUMBER(4))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(5)",pool01%SCR_NUMBER(5))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(6)",pool01%SCR_NUMBER(6))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(7)",pool01%SCR_NUMBER(7))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(8)",pool01%SCR_NUMBER(8))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(9)",pool01%SCR_NUMBER(9))
    iOut= iOut .and. GfGetValue(gf,"SCR_NUMBER(10)",pool01%SCR_NUMBER(10))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(1)",pool01%SL_DIS(1))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(2)",pool01%SL_DIS(2))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(3)",pool01%SL_DIS(3))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(4)",pool01%SL_DIS(4))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(5)",pool01%SL_DIS(5))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(6)",pool01%SL_DIS(6))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(7)",pool01%SL_DIS(7))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(8)",pool01%SL_DIS(8))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(9)",pool01%SL_DIS(9))
    iOut= iOut .and. GfGetValue(gf,"SL_DIS(10)",pool01%SL_DIS(10))
    iOut= iOut .and. GfGetValue(gf,"THICK(1)",pool01%THICK(1))
    iOut= iOut .and. GfGetValue(gf,"THICK(2)",pool01%THICK(2))
    iOut= iOut .and. GfGetValue(gf,"THICK(3)",pool01%THICK(3))
    iOut= iOut .and. GfGetValue(gf,"THICK(4)",pool01%THICK(4))
    iOut= iOut .and. GfGetValue(gf,"THICK(5)",pool01%THICK(5))
    iOut= iOut .and. GfGetValue(gf,"THICK(6)",pool01%THICK(6))
    iOut= iOut .and. GfGetValue(gf,"THICK(7)",pool01%THICK(7))
    iOut= iOut .and. GfGetValue(gf,"THICK(8)",pool01%THICK(8))
    iOut= iOut .and. GfGetValue(gf,"THICK(9)",pool01%THICK(9))
    iOut= iOut .and. GfGetValue(gf,"THICK(10)",pool01%THICK(10))
    !! END CODE CREATED AUTOMATICALLY (makecode1.pro)
  End Subroutine GfToPoolOE
  
  
  subroutine PoolSourceLoad(pool00,filename) !bind(C,NAME="PoolSourceLoad")
    type (poolSource), intent(inout) :: pool00
    character(len=*), intent(in)     :: filename
    
    type (gfType) :: gf
    
    if(.not.GfFileLoad(gf,filename))  print *, "unable to load all file"
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
  
    
End Module shadow_variables

