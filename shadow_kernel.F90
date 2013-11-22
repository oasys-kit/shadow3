!!!!512512
!----
!---- MODULE:  shadow_kernel
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

Module shadow_kernel
    !---- Use Modules ----!

	use, intrinsic :: ISO_C_BINDING
  	use stringio
  	use gfile
  	use shadow_beamio
  	use shadow_math
  	use shadow_variables
  	use shadow_roughness
	use shadow_globaldefinitions !, only : ski, skr, skc

  	implicit none
  

  	integer (kind=ski) :: ione, izero, itwo, ithree, ifour, i_one, i_two, i101, i201, i501
  	parameter (izero=0)
  	parameter (ione=1)
  	parameter (itwo=2)
  	parameter (ithree=3)
  	parameter (ifour=4)
  	parameter (i_one=-1)
  	parameter (i_two=-2)
  	parameter (i101=101)
  	parameter (i201=201)
  	parameter (i501=501)
  
  	!---- Variables ----!

    real(kind=skr),dimension(3), parameter :: origin = (/0.0,0.0,0.0/)
    real(kind=skr),dimension(3), parameter :: x_vrs = (/1.0,0.0,0.0/)
    real(kind=skr),dimension(3), parameter :: y_vrs = (/0.0,1.0,0.0/)
    ! srio removed parameter as it may change in mirror1
    real(kind=skr),dimension(3)            :: z_vrs = (/0.0,0.0,1.0/)
    
    ! now the declaration of the variables in start.00
    !
    ! NOTE: FOR ADDING A NEW VARIABLE, IT SHOULD BE ADDED IN *.def

  	!---- Variables SOURCE ----!
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ftype(kind=fkind) :: name
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ftype(kind=fkind,len=length) :: name
#include "shadow_source.def"


!     integer(kind=ski)     :: FDISTR
!...
!     integer(kind=ski)     :: F_NEW




  !---- Variables OE ----!
  
    
    ! now the declaration of the variables in start.xx
    !
    ! NOTE: FOR ADDING A NEW VARIABLE, IT SHOULD BE ADDED IN *.def
    
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ftype(kind=fkind) :: name
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ftype(kind=fkind,len=length) :: name
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) ftype(kind=fkind), dimension(arrdim) :: name
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) ftype(kind=fkind, len=length), dimension(arrdim) :: name
#include "shadow_oe_without_repetitions.def"



!     integer(kind=ski)     :: FMIRR
!...
!     real(kind=skr), dimension(aDim)       :: THICK
    
  !---- Variables EX-COMMONS ----!

    !  now the variables in the ex-common blocks not defined in the gfiles
    
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
    !!--	INTEGER(kind=ski) :: OE_NUMBER,IDUMMY
    !!--	REAL(kind=skr)    :: DUMMY
    !!--	INTEGER(kind=ski),dimension(10) ::  SCR_NUMBER
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
    ! used in setsour
    real(kind=skr)    :: beta,delta,rdelta
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
    character(len=1024)          :: FFILE
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
    real(kind=skr),dimension(3) :: psour,psreal
    !!--     				DELTA,RDELTA,OFF_SOUX,OFF_SOUY,OFF_SOUZ, &
    !!--     				ALPHA_S
    !!--     	COMMON	/MIRROR	/	RLEN,RLEN1,RLEN2,CCC(10),RMIRR,CONE_A, &
    !!--     				AXMAJ,AXMIN,AFOCI,ECCENT,R_MAJ,R_MIN, &
    !!--     				RWIDX,RWIDX1,RWIDX2,PARAM, &
!!ccc    real(kind=skr),dimension(10) :: ccc
    !!--     				PCOEFF(0:4,0:4,0:4),CIL_ANG,ELL_THE
    ! used for polynomial mirrors (FMIRR=9)
    integer(kind=ski) :: ndeg
    real(kind=skr),dimension(0:4,0:4,0:4) :: pcoeff
    !!--     	COMMON	/GRATING/	RULING,ORDER,BETA,PHOT_CENT,R_LAMBDA, &
    !!--     				HUNT_L,HUNT_H,BLAZE,D_SPACING,AZIM_FAN, &
    !!--     				DIST_FAN,COMA_FAC,RUL_A1,RUL_A2,RUL_A3, &
    !!--     				RUL_A4,A_BRAGG
    !!--        COMMON  /XTAL   /       SPREAD_MOS, R_JOHANSSON, THICKNESS
    !!--     	COMMON	/ROT_MIR/	X_ROT,Y_ROT,Z_ROT,OFFX,OFFY,OFFZ, &
    !!--     				U_MIR(3),V_MIR(3),W_MIR(3)
    real(kind=skr),dimension(3) :: u_mir,v_mir,w_mir
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
    real(kind=skr),dimension(3)   :: u_sour,v_sour,w_sour
    !!--     				PLASMA_ANGLE
    !!--     	COMMON	/ALADDIN/	PHOTON(10),BENER,R_ALADDIN, &
    real(kind=skr),dimension(10)   :: photon
    !!--     				EPSI_X,EPSI_Z,EPSI_DX,EPSI_DZ,R_MAGNET
    !!--     	COMMON	/TRIG	/	COSDEL,SINDEL,COSTHE,SINTHE,COSTHR, &
    !!--     				SINTHR,COSDER,SINDER,COSAL,SINAL, &
    !!--     				COSAL_S,SINAL_S,COSTHE_I,SINTHE_I, &
    !!--     				COSAL_I,SINAL_I

    ! used in setsour
    real(kind=skr)           :: COSDEL,SINDEL,COSTHE,SINTHE
    real(kind=skr)           :: COSTHR,SINTHR,COSDER,SINDER,COSAL,SINAL
    real(kind=skr)           :: COSTHE_I,SINTHE_I
    ! used ?
    real(kind=skr)           :: COSAL_S,SINAL_S
    real(kind=skr)           :: COSAL_I,SINAL_I

    !!--     	COMMON	/IMAGE	/	RIMCEN(3),VNIMAG(3),UXIM(3),VZIM(3), &
    !!--     				D_PLATE(5),C_STAR(3),C_PLATE(3), &
    !!--     				UX_PL(3),VZ_PL(3),WY_PL(3), &
    !!--     				THETA_I,ALPHA_I
    real(kind=skr),dimension(3) :: rimcen,vnimag, uxim, vzim, c_star, &
                                           c_plate, ux_pl, vz_pl, wy_pl
    !!--     	COMMON	/AXIS	/	CENTRAL(20,24),T_INCIDENCE,T_SOURCE, &
    !real(kind=skr),dimension(20,24) :: central
    ! redimensionate !
    !real(kind=skr),dimension(200,24) :: central
    !srio@esrf.eu removes the history, central contains only the current oe
    real(kind=skr),dimension(24) :: central
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
    ! srio: used in surface
    real(kind=skr),dimension(10) :: AMPLI,X_GR,Y_GR,SIGNUM
    !!--     	COMMON	/RIPP_2 /	AMPLI(10),X_GR(10),Y_GR(10),SIGNUM(10), &
    real(kind=skr),dimension(10) :: SIG_X,SIG_XMIN,SIG_XMAX, &
                                            SIG_Y,SIG_YMIN,SIG_YMAX, AMPL_IN
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
    real(kind=skr),dimension(3,2) :: UX_SCR,WY_SCR,VZ_SCR
    !!--     	COMMON	/HOLO	/	HOLO_R1,HOLO_R2,HOLO_DEL,HOLO_GAM, &
    !!--     				HOLO_W,HOLO1(3),HOLO2(3),HOLO_RT1, &
    !!--     				HOLO_RT2

    real(kind=skr),dimension(3) :: HOLO1,HOLO2

    !!--
    !!--!!       common /srio/ OE_NUMBER,IDUMMY, DUMMY, SCR_NUMBER
    !!--


    !! variables passed between reflec and fresnel
    real(kind=skr),dimension(500)  :: t_oe, gratio, mlroughness1, mlroughness2
    real(kind=skr)                 :: delo, beto, dele, bete, dels, bets


  
  !---- Everything FROM HERE is private unless explicitly made public ----!
  !     private
  
  
  !---- List of public functions ----!
  ! public :: rwname
  !---- List of public overloaded functions ----!
  !---- List of public subroutines ----!
        public :: rwname, input_source1, sourceGeom, source_bound
        public :: reset, switch_inp, trace_step

        public :: PoolOEToGlobal,PoolSourceToGlobal
        public :: GlobalToPoolOE,GlobalToPoolSource
        public :: traceoe,Shadow3Trace
        ! these routines should be moved to shadow_postprocessors
        public :: presurface_translate, prerefl_test, pre_mlayer_scan
  
  
  !---- List of private functions ----!
  !---- List of private subroutines ----!
  !private :: get_variables00, put_variables00, source_bound
  !private :: get_variablesOE, put_variablesOE
  
  
  
  
  !---- Definitions ----!
  ! this is an example of a type 
  !Type, public :: GfType
  !   character(len=sklen) :: fileName
  !   ! logical for allocation
  !   logical            :: alloc1
  !   integer(kind=ski)            :: nLines
  !   integer(kind=ski)            :: nVariables
  !   character(len=sklen), dimension(:), allocatable :: fileLines
  !   character(len=sklen), dimension(:), allocatable :: variableNames
  !   character(len=sklen), dimension(:), allocatable :: variableValues
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
    integer(kind=ski),        intent(out)   ::  iflag
    
    logical               ::  esta, iOut
    ! smeagolas
    type(poolSource)      ::  pool00
    type(poolOE)          ::  pool01        
    
    !        Type(GfType)          ::  gEnd00, gEndOE
    
    iflag = 0
    
    
    
    ! c
    ! c Performs I/O
    ! c
    
    select case (trim(what))
       
    case('W_SOUR') 
       call GlobalToPoolSource(pool00)
       call PoolSourceWrite(pool00,trim(name))
       iOut = .true.
       if (.not. iOut) then 
          iFlag=-1
          call leave("SHADOW-RWNAME","Error writing file: "//trim(name),iFlag)
       else 
          !print *,">>>> File written to disk: "//trim(name)
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
       !            iOut = GfFileLoad(gStart00,trim(name))
       call PoolSourceLoad(pool00,trim(name))
       call PoolSourceToGlobal(pool00)
       iout = .true.
       if (.not. iOut) then 
          iFlag=-1
          call leave("SHADOW-RWNAME","Error reading file: "//trim(name),iFlag)
       end if
       
       ! prints to the terminal the list of variables
       ! iOut = GfTypePrint(gStart00)
       
    case('W_OE') 
       
       call GlobalToPoolOE(pool01)
       call PoolOEWrite(pool01,trim(name))
       iout = .true.
       if (.not. iOut) then 
          iFlag=-1
          call leave("SHADOW-RWNAME","Error writing file: "//trim(name),iFlag)
       else 
          !print *,">>>> File written to disk: "//trim(name)
       end if
       
       
    case('R_OE') 
       
       !
       ! check if file exists
       !
       inquire(file=trim(name),exist=esta)
       
       if ( .not. esta) then
          iflag = -2
          CALL LEAVE ("SHADOW-RWNAME","Error, file does not exist: "//trim(name), IFLAG)
       end if
       
       ! this loads the variables of the gfile into gStart00 type
       !            iOut = GfFileLoad(gStartOE,trim(name))
       call PoolOELoad(pool01,trim(name))
       iout = .true.
       call PoolOEToGlobal(pool01)
       if (.not. iOut) then 
          iFlag=-1
          call leave("SHADOW-RWNAME","Error reading file: "//trim(name),iFlag)
       end if
       
       ! prints to the terminal the list of variables
       ! iOut = GfTypePrint(gStartOE)
       
    case default 
       print *,"SHADOW-RWNAME: Undefined label: "//trim(what)
       stop
    end select
    
    RETURN
  END SUBROUTINE RWNAME
  
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
  !     Modified:               srio@esrf.eu 2012-05-24
  !                             Two methods are available:
  !
  !                             F_BOUND_SOUR = 1 : the "traditional" method
  !                             as described in the SHADOW SOURCE USER GUIDE
  !                             This method is based over sampling on a 3D
  !                             histogram, it is cumbersome and the results 
  !                             are usually not accurate.
  !
  !                             F_BOUND_SOUR = 2 : the new method that send
  !                             each ray to a rectangular slit at a given 
  !                             distance DIST, and the ray is rejected if not 
  !                             passing trough the slit. 
  !                             The file format with the slit info is:
  !                             DIST  H_MIN H_MAX V_MIN V_MAX
  !                             Where 
  !                             DIST is the distance from the source to the slit
  !                             H_MIN H_MAX are the slit horizontal limits 
  !                                         (in user units)
  !                             V_MIN V_MAX are the slit horizontal limits 
  !                                         (in user units)
  !                             If DIST is set to zero, then an "angular"
  !                             aperture is used, where H_MIN H_MAX 
  !                             V_MIN V_MAX are the angular limits in rads.
  !
  !
  !
  !C ---
  
  SUBROUTINE SOURCE_BOUND (POS, DIR, IFLAG)
    
    
    implicit none
    !IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
    !IMPLICIT INTEGER(kind=ski)        (F,I-N)
    
    !C 
    !C  Save the values that the caller expects to be there next time this
    !C  routine is called. The following chunk is basically an unnamed COMMON
    !C  block (w/out the corruption of the namespace, of course).
    !C 
    real(kind=skr),dimension(3),intent(in) :: POS,DIR
    integer(kind=ski)                      :: IFLAG

    !DIMENSION	POS(3), DIR(3)
    !DIMENSION	IX(101,101), IY(101,101), IZ(101,101)
    real(kind=skr),dimension(101,101)  :: IX,IY,IZ
    real(kind=skr)  :: XMIN, YMIN, ZMIN, XS, X1S, YS, Y1S, ZS, Z1S
    real(kind=skr)  :: X1MIN, Y1MIN, Z1MIN
    integer(kind=ski) :: NX, NX1, NY, NY1, NZ, NZ1
    integer(kind=ski) :: ierr,i,j,jx,j1x,jy,j1y,jx1,jy1,jz,jz1
    real(kind=skr)  :: distSlit, h_min, h_max, v_min, v_max
    real(kind=skr)  :: rdist, posSlitH, posSlitV

    SAVE IX, IY, IZ, XMIN, YMIN, ZMIN, &
         NX, NX1, NY, NY1, NZ, NZ1, &
         XS, X1S, YS, Y1S, ZS, Z1S, &
         X1MIN, Y1MIN, Z1MIN, &
         distSlit, h_min, h_max, v_min, v_max
    !C 	
    !C  checks for initialization
    !C 
    IF (IFLAG.LT.0) THEN
       IF (F_BOUND_SOUR .eq. 1) THEN !  histo3 method
         !OPEN (30, FILE=FILE_BOUND, STATUS='OLD', FORM='UNFORMATTED', IOSTAT=IERR)
         ! changed to formatted, srio@esrf.eu 20120525
         OPEN (30, FILE=FILE_BOUND, STATUS='OLD', FORM='FORMATTED', IOSTAT=IERR)
         IF (IERR.NE.0) THEN
            WRITE(6,*)'Error opening file: '//trim(FILE_BOUND)
            STOP 'Fatal error. Aborted'
         END IF
         READ (30,*,ERR=101)    NX, XMIN, XS
         READ (30,*,ERR=101)    NX1, X1MIN, X1S
         READ (30,*,ERR=101)    NY, YMIN, YS
         READ (30,*,ERR=101)    NY1, Y1MIN, Y1S
         READ (30,*,ERR=101)    NZ, ZMIN, ZS
         READ (30,*,ERR=101)    NZ1, Z1MIN, Z1S
         !DO 11 I=1,NX
         DO I=1,NX
            READ (30,*,ERR=101)    (IX(I,J),J=1,NX1)
!11       CONTINUE
         END DO
         !DO 21 I=1,NY
         DO I=1,NY
            READ (30,*,ERR=101)    (IY(I,J),J=1,NY1)
!21       CONTINUE
         END DO
         !DO 31 I=1,NZ
         DO I=1,NZ
            READ (30,*,ERR=101)    (IZ(I,J),J=1,NZ1)
!31       CONTINUE
         END DO
         WRITE(6,*)'Phase space boundaries file read succesfully.'
       ELSE ! method 2, slit
         OPEN (30, FILE=FILE_BOUND, STATUS='OLD', FORM='FORMATTED', IOSTAT=IERR)
         IF (IERR.NE.0) THEN
            WRITE(6,*)'Error opening file: '//trim(FILE_BOUND)
            STOP 'Fatal error. Aborted'
         END IF
!!print *,'Reding file....'
         READ (30,*,ERR=101)    distSlit,h_min,h_max,v_min,v_max
         !READ (30,*,ERR=101)    distSlit
         !READ (30,ERR=101)    distSlit,h_min,h_max,v_min,v_max
         !READ (30,ERR=101)    distSlit,h_min,h_max,v_min,v_max
         !READ (30,ERR=101)    distSlit,h_min,h_max,v_min,v_max
         !READ (30,ERR=101)    distSlit,h_min,h_max,v_min,v_max
!!print *,'distSlit,h_min,h_max,v_min,v_max: ',distSlit,h_min,h_max,v_min,v_max
         WRITE(6,*)'File with slit boundaries read succesfully.'
       END IF

       CLOSE (30)
       RETURN
101    WRITE(6,*)'Error reading from file '//trim(FILE_BOUND)
       STOP
    ELSE IF (IFLAG.EQ.1) THEN
       !C 
       !C  Normal entry
       !C  Tests for ''good'' hits; if a bad one fund, return
       !C 
       IFLAG = -1
       IF (F_BOUND_SOUR .EQ. 1) THEN ! method 1: histo3
  
         IF (XS.NE.0) THEN
            JX  = (POS(1) - XMIN)/XS + 1
         ELSE
            JX = 1
         END IF
         IF (X1S.NE.0) THEN
            JX1 = (DIR(1) - X1MIN)/X1S + 1
         ELSE
            JX1 = 1
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
            JY = 1
         END IF
         IF (Y1S.NE.0) THEN
            JY1 = (DIR(2) - Y1MIN)/Y1S + 1
         ELSE
            JY1 = 0
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
            JZ = 1
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
       ELSE ! method 2: slit
         IF (ABS(distSlit) .le. 1d-10) THEN ! angle
            IF ( (dir(1).ge.h_min) .and. (dir(1).le.h_max) .and. &
                 (dir(3).ge.v_min) .and. (dir(3).le.v_max) ) IFLAG = 1
         ELSE ! slit
            rdist = (-pos(2)+distSlit)/dir(2)
            ! check for perpendicular rays
            ! if (dir(2).lt.1d-16) rdist = 0.0
            posSlitH = pos(1)+rdist*dir(1)
            !posSlit(2) = pos(2)+rdist*dir(2,:)
            posSlitV = pos(3)+rdist*dir(3)
            IF ( (posSlitH.ge.h_min) .and. (posSlitH.le.h_max) .and. &
                 (posSlitV.ge.v_min) .and. (posSlitV.le.v_max) ) IFLAG = 1
         END IF
       END IF

       RETURN
     
    END IF
  END SUBROUTINE SOURCE_BOUND
  !
  !
  !
  
  !
  ! USED BY TRACE....
  !

! C
! C
! C+++
! C	SUBROUTINE	CRYSTAL
! C
! C	PURPOSE		Computes the reflectivity of a symmetric Bragg crystal 
! C			according to the dynamic theory of x-ray diffraction.
! C
! C	ALGORITHM	Reference B.E.Warren, X-Ray Diffraction, 
! C			Addison-Wesley 1969.  See also M.J.Bedzyk, G.Materlik 
! C			and M.V.Kovalchuk, Phy. Rev. B30, 2453(1984).
! C			For mosaic crystal reflectivity see Zachariasen, 
! C			Theory of x-ray diffraction in crystals, Dover (1966)
! C			formula 4.24, and Bacon and Lowde, Acta Crystall.1
! C			pag 303 (1948) formula 17. 
! C
! C	MODIFIED	July 1989, M. Sanchez del Rio for asymmetry part,
! C			July 1990, mosaic part.
! C			August 1992, laue part.
! C
! C---

  Subroutine CRYSTAL (Q_PHOT, SIN_Q_ANG, SIN_Q_REF, SIN_BRG, &
       R_S, R_P,PHASE_S, PHASE_P, DEPHT_MFP_S, DEPHT_MFP_P,  &
       DELTA_REF, THETA_B, KWHAT)
    
    IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
    IMPLICIT INTEGER(kind=ski)        (F,I-N)
    
    REAL(KIND=skr)		ENERGY(1000)
    REAL(KIND=skr)		FP_A(1000),FPP_A(1000),FP_B(1000),FPP_B(1000)
    COMPLEX*16	CI,FA,FB,STRUCT,F_0,REFRAC
    COMPLEX*16	RCS1,RCP1,RCS2,RCP2,RCS,RCP
    COMPLEX*16	ETA_S,ETA_P
    COMPLEX*16	GA,GA_BAR,GB,GB_BAR,FH,FH_BAR
    COMPLEX*16	psi_h,psi_hbar,psi_0,psi_conj       !laue&perfect
    COMPLEX*16	ctemp,cry_q,cry_z                   !laue&perfect
    COMPLEX*16	br_x1,br_x2,br_delta1,br_delta2     !laue&perfect
    COMPLEX*16	br_c1,br_c2                         !laue&perfect
    REAL(KIND=skr)		CA(3),CB(3),FOA,F1A,F2A,FOB,F1B,F2B
    INTEGER(KIND=ski)	ATNUM_A,ATNUM_B
    ! C
    ! C SAVE the variables that need to be saved across subsequent invocations
    ! C of this subroutine. Note: D_SPACING is not included in the SAVE block
    ! C because it's included in the COMMON.BLK file.
    ! C
    SAVE		I_LATT,RN, &
         ATNUM_A,ATNUM_B,TEMPER, &
         GA,GA_BAR,GB,GB_BAR, &
         CA,CB, &
         NREFL, ENERGY, FP_A, FPP_A, FP_B, FPP_B
    ! C
    CI	= (0.0D0,1.0D0)
    ! C
    ! C If flag is < 0, reads in the reflectivity data
    ! C
    IF (KWHAT.LT.0) THEN
       OPEN (25,FILE=FILE_REFL,STATUS='OLD', FORM='FORMATTED', IOSTAT=iErr)
        ! srio added test
        if (iErr /= 0 ) then
          print *,"MIRROR: File not found: "//trim(file_refl)
          stop 'File not found. Aborted.'
        end if
       READ (25,*) I_LATT,RN,D_SPACING
       READ (25,*) ATNUM_A,ATNUM_B,TEMPER
       READ (25,*) GA
       READ (25,*) GA_BAR
       READ (25,*) GB
       READ (25,*) GB_BAR
       READ (25,*) CA(1),CA(2),CA(3)
       READ (25,*) CB(1),CB(2),CB(3)
       READ (25,*) NREFL
       DO 199 I = 1, NREFL
          READ (25,*) ENERGY(I), FP_A(I), FPP_A(I)
199    READ (25,*)    FP_B(I), FPP_B(I)
       CLOSE (25)
       RETURN
    ELSE
       ! C
       ! C Computes reflectivities at given wavelength and angle.
       ! C
       PHOT	= Q_PHOT/TWOPI*TOCM
       IF (PHOT.LT.ENERGY(1).OR.PHOT.GT.ENERGY(NREFL)) THEN
 	  CALL	MSSG ('CRYSTAL ','Incoming photon energy is out of range.',IERR)
          R_S = 0.0D0
	  R_P	= 0.0D0
          PHASE_S = 0.0D0
          PHASE_P = 0.0D0
	  THETA_B = 0.0D0
          RETURN
       END IF
       ! C
       ! C Interpolate for the atomic scattering factor.
       ! C
       DO 299 I = 1, NREFL
299    IF (ENERGY(I).GT.PHOT) GO TO 101
       ! C
       I = NREFL
101    NENER = I - 1	
       F1A	= FP_A(NENER) + (FP_A(NENER+1) - FP_A(NENER)) *  &
            (PHOT - ENERGY(NENER)) / & 
            (ENERGY(NENER+1) - ENERGY(NENER))
       F2A	= FPP_A(NENER) + (FPP_A(NENER+1) - FPP_A(NENER)) *  &
            (PHOT - ENERGY(NENER)) /  &
            (ENERGY(NENER+1) - ENERGY(NENER))
       F1B	= FP_B(NENER) + (FP_B(NENER+1) - FP_B(NENER)) *  &
            (PHOT - ENERGY(NENER)) /  &
            (ENERGY(NENER+1) - ENERGY(NENER))
       F2B	= FPP_B(NENER) + (FPP_B(NENER+1) - FPP_B(NENER)) *  &
            (PHOT - ENERGY(NENER)) /  &
            (ENERGY(NENER+1) - ENERGY(NENER))
       R_LAM0 	= TWOPI/Q_PHOT
       ! C
       ! C Calculates the reflection algles and other useful parameters
       ! C
       SIN_ALFA  = SIN(A_BRAGG)
       COS_ALFA  = SQRT(1.0D0-SIN_ALFA**2)
       COS_Q_ANG = SQRT(1.0D0-SIN_Q_ANG**2)
       ! C	Debugging: COSS_Q_REF is passed in the arguments to take into 
       ! C	account the possible crystal movements in the asymmetrical case.
       ! C	COS_Q_REF = COS_Q_ANG + R_LAM0*SIN_ALFA/D_SPACING
       COS_Q_REF = SQRT(1.0D0-SIN_Q_REF**2)
       IF (COS_Q_REF.GT.1.0) THEN
          CALL MSSG ('Error in Crystal','cos>1',IERR)
          R_S	= 0.0D0
          R_P	= 0.0D0
          PHASE_S = 0.0D0
          PHASE_P = 0.0D0
          GO TO 1122
       END IF
       ! C	SIN_Q_REF = SQRT(1.0D0-COS_Q_REF**2)
       SIN_GRA	  = R_LAM0/D_SPACING/2.0D0
       GRAZE	  = ASIN(SIN_GRA)
       ASS_FAC	  = SIN_Q_ANG/SIN_Q_REF
       ! C
       ! C Interpolation
       ! C
       if (f_refrac.eq.1.and.f_mosaic.eq.1) then
          sin_q   = sin_brg
       else 
          SIN_Q   = SIN_Q_ANG*COS_ALFA - COS_Q_ANG*SIN_ALFA
       end if
       ! C
       ! C MSR 92/10/24 for the inclined monochromator the above definition of
       ! C sin_q does not work. Set as in Mosaic Laue. To be confirmed
       ! C and check with the Laue case
       ! C
       if (f_refrac.ne.1.and.f_bragg_a.eq.1) sin_q=sin_brg
       RATIO	= abs(SIN_Q/R_LAM0*1.0D-8)
       ! C
       ! C
       ! C
       FOA	= CA(3)*RATIO**2 + CA(2)*RATIO + CA(1)
       FOB	= CB(3)*RATIO**2 + CB(2)*RATIO + CB(1)
       FA	= FOA + F1A + CI*F2A
       FB	= FOB + F1B + CI*F2B
       ! C
       ! C Compute the ABSORPtion coefficient and Fo.
       ! C
       IF (I_LATT.EQ.0) THEN
          ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*(DIMAG(FA)+DIMAG(FB)))
          F_0 = 4*((F1A + ATNUM_A + F1B + ATNUM_B) + CI*(F2A + F2B))
       ELSE IF (I_LATT.EQ.1) THEN
          ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*(DIMAG(FA)+DIMAG(FB)))
          F_0 = 4*((F1A + ATNUM_A + F1B + ATNUM_B) + CI*(F2A + F2B))
       ELSE IF (I_LATT.EQ.2) THEN
          FB	 = (0.0D0,0.0D0)
          ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*DIMAG(FA))
          F_0 = 4*(F1A + ATNUM_A + CI*F2A)
       ELSE IF (I_LATT.EQ.3) THEN
          ABSORP = 2.0D0*RN*R_LAM0*(DIMAG(FA)+DIMAG(FB))
          F_0 = (F1A + ATNUM_A + F1B + ATNUM_B) + CI*(F2A + F2B)
       ELSE IF (I_LATT.EQ.4) THEN
          FB     = (0.0D0,0.0D0)
          ABSORP = 2.0D0*RN*R_LAM0*(2.0D0*(DIMAG(FA)))
          F_0 = 2*(F1A+ CI*F2A )
       ELSE IF (I_LATT.EQ.5) THEN
          FB     = (0.0D0,0.0D0)
          ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*(DIMAG(FA)))
          F_0 = 4*(F1A + CI*F2A )
       END IF
       ! C	
       ! C FH and FH_BAR are the structure factors for (h,k,l) and (-h,-k,-l).
       ! C
       ! C srio, Added TEMPER here (95/01/19)
       FH 	= ( (GA * FA) + (GB * FB) )*TEMPER
       FH_BAR	= ( (GA_BAR * FA) + (GB_BAR * FB) )*TEMPER
       ! using mysqrt to avoid problems in windows. See mysqrt in
       ! shadow_math module
       ! STRUCT 	= SQRT(FH * FH_BAR) 
       STRUCT = mySQRT(FH * FH_BAR) 
       ! C
       ! C computes refractive index.
       ! C
       REFRAC = (1.0D0,0.0D0) - R_LAM0**2*RN*F_0/TWOPI
       DELTA_REF  = 1.0D0 - DREAL(REFRAC)
       ! C
       ! C THETA_B is the Bragg angle corrected for refraction for sym case,
       ! C following Warren
       ! C
       if (f_refrac.ne.1) then
          THETA_B = R_LAM0/(1-(DELTA_REF/SIN_GRA**2))/2.0D0/D_SPACING
          THETA_B = ASIN(THETA_B)
          ! C
          ! C Now THETA_B is the Bragg angle corrected for refraction for asym case
          ! C following Handbook of SR
          ! C
          if (f_refrac.eq.1) ass_fac = -1.0d0*ass_fac
          THETA_INC_O = 0.5D0*(1.0D0+1.0D0/ASS_FAC)*(THETA_B-GRAZE)
          THETA_B = GRAZE + A_BRAGG + THETA_INC_O   
       else
          theta_b = graze
       end if
       ! C
       IF (F_MOSAIC.EQ.1) THEN
          ! C
          ! C >>>>>>>>>>>>>>>>>>>> Mosaic crystal calculation <<<<<<<<<<<<<<<<<<
          ! C
          R_STRUCT = DREAL(STRUCT)
          ! C srio, remove the TEMPER factor from here and placed in FH and FH_BAR
          ! C srio        QS_MOSAIC=(RN*R_STRUCT*TEMPER)**2*R_LAM0**3
          QS_MOSAIC=(RN*R_STRUCT)**2*R_LAM0**3 &
               /SIN(2*(ASIN(SIN_brg)))
          QP_MOSAIC = QS_MOSAIC*(COS(2.0D0*GRAZE))**2
          A_MOSAIC  = THICKNESS*ABSORP/SIN_Q_ANG
          EP        = abs(ASIN(SIN_brg) - THETA_B)
          OMEGA =(DEXP(-EP**2/2.0D0/SPREAD_MOS**2)) &
               /SQRT(TWOPI)/SPREAD_MOS
          AAS_MOSAIC = OMEGA*QS_MOSAIC/ABSORP
          AAP_MOSAIC = OMEGA*QP_MOSAIC/ABSORP
          ! *
          ! * Transmission case
          ! *
          if (f_refrac.eq.1) then
             rs_mosaic = sinh(aas_mosaic*a_mosaic) *  &
                  exp(-a_mosaic*(1+aas_mosaic))
             rp_mosaic = sinh(aap_mosaic*a_mosaic) *  &
                  exp(-a_mosaic*(1+aap_mosaic))
             ! *
             ! * Reflection case
             ! *
          else 
             RS_MOSAIC = 1+AAS_MOSAIC+(SQRT(1+2*AAS_MOSAIC))/ &
                  DTANH(A_MOSAIC*SQRT(1+2*AAS_MOSAIC))
             RP_MOSAIC = 1+AAP_MOSAIC+(SQRT(1+2*AAP_MOSAIC))/ &
                     DTANH(A_MOSAIC*SQRT(1+2*AAP_MOSAIC))
             RS_MOSAIC = AAS_MOSAIC / RS_MOSAIC
             RP_MOSAIC = AAP_MOSAIC / RP_MOSAIC
          end if
          R_S     = SQRT(RS_MOSAIC)
          R_P     = SQRT(RP_MOSAIC)
          ! *
          ! * Mean value of depht into the crystal. To be used in MIRROR
          ! *
          DEPHT_MFP_S = 1.0D0 /OMEGA /QS_MOSAIC
          DEPHT_MFP_P = 1.0D0 /OMEGA /QP_MOSAIC
          ! *
          ! *No phase change are introduced by now. (The formulae of reflectivity 
          ! *are already intensity, and no complex coefficient are considered).
          ! *This is not important because a mosaic crystal breaks always coherence
          ! *
          PHASE_S = 0.0D0
          PHASE_P = 0.0D0
       ELSE
          ! C
          ! C >>>>>>>>>>>>>>>>>>>> Perfect crystal calculation <<<<<<<<<<<<<<<<<
          ! C
          ! C Main calculation (symmetrical case and asym incident case)
          ! C I change to reflectivity formulae of Zachariasen,
          ! C for a definition of ETA taking into account the angle with
          ! C Bragg planes. MSR 6/28/90
          ! C
          ! C	EP	= -abs(ASIN(SIN_BRG)) + graze ! scalar def      
          ! C	ALPHA_ZAC = -2.0D0*EP*SIN(2*GRAZE)    ! of alpha_zac
          ! C
          ALPHA_ZAC =-((R_LAM0/D_SPACING)**2-2*R_LAM0* &
               SIN_BRG/D_SPACING)
          ! D
          ! D MK's debugging. Don't touch!!
          ! D
          ! D	XXX1 = R_LAM0/D_SPACING
          ! D	XXX2 = XXX1**2
          ! D	XXX3 = 2.0D0*R_LAM0*SIN_BRG
          ! D	XXX4 = XXX3/D_SPACING
          ! D	XXX5 = XXX2 - XXX4
          ! D	ALPHA_ZAC = -XXX5
          ! C
          IF (F_REFRAC.EQ.1) THEN
             ! *
             ! * Transmission (Laue) case and general Reflection (Bragg) case
             ! * NB: The general Bragg case is commented (cc). It is not yet being
             ! * used. We still use the Thick Crystal Approximation case.
             ! *
             ! C
             ! C   PSI_CONJ = F*( note: PSI_HBAR is PSI at -H position and is
             ! C   proportional to fh_bar but PSI_CONJ is complex conjugate os PSI_H) 
             ! C
             ! C   This part has been written by G.J. Chen and M. Sanchez del Rio. 
             ! C   We use the formula [3.130] of Zachariasen's book.
             ! C
             gamma_0 = -1.0D0*sin_q_ang
             gamma_h = -1.0D0*sin_q_ref
             sin_brg   = -1.0D0*sin_brg
             
             if (f_refrac.eq.0) gamma_h = - gamma_h
             cry_b = gamma_0/gamma_h
             
             cry_t = 0.5D0*(-1.D0/abs(gamma_0) +1.D0/abs(gamma_h))*thickness 
             cry_a = pi/r_lam0*(thickness/gamma_0)
             cry_alpha = -((r_lam0/d_spacing)**2+2D0*r_lam0* &
                  sin_brg/d_spacing)
             
             psi_h = rn*r_lam0**2/pi*fh
             psi_hbar = rn*r_lam0**2/pi*fh_bar
             psi_0 = rn*r_lam0**2/pi*f_0
             psi_conj = rn*r_lam0**2/pi*dconjg(fh)
             
             cry_q = cry_b*psi_h*psi_hbar
             cry_z = (1.0D0-cry_b)*0.50D0*psi_0 + cry_b*0.50D0*cry_alpha
             
             ! C
             ! C s-polarization
             ! C
             !ctemp = cdsqrt(cry_q  + cry_z**2)
             ctemp = mysqrt(cry_q  + cry_z**2)
             br_x1 = (-1.0d0*cry_z+ctemp)/psi_hbar
             br_x2 = (-1.0d0*cry_z-ctemp)/psi_hbar
             br_delta1 = 0.5d0*(psi_0-cry_z+ctemp)
             br_delta2 = 0.5d0*(psi_0-cry_z-ctemp)
             br_c1 = -1.d0*ci*thickness*twopi/(-1.0D0*abs(gamma_0))/r_lam0* &
                  br_delta1
             br_c2 = -1.d0*ci*thickness*twopi/(-1.0D0*abs(gamma_0))/r_lam0* &
                  br_delta2
             ! C
             ! C a very big exponential produces numerical overflow. If so, the value
             ! C is changed artificially to avoid the overflow. This is equivalent to 
             ! C use the thick crystal approximation
             ! C changed 700 -> 100 as per MSR.  3/24/95
             ! C
             if (dreal(br_c1).gt.100.or.dreal(br_c2).gt.100) then 
                if (dreal(br_c1).gt.100) br_c1 = 100.0d0+ci*dimag(br_c1)
                if (dreal(br_c2).gt.100) br_c2 = 100.0d0+ci*dimag(br_c2)
             end if
             
             br_c1 = cdexp(br_c1)
             br_c2 = cdexp(br_c2)
             
             ! Cc	if (f_refrac.eq.1) then 
             rcs = br_x1*br_x2*(br_c1-br_c2)/(br_x2-br_x1)             ! laue
             ! Cc	else if (f_refrac.eq.0) then
             ! Cc	  rcs = br_x1*br_x2*(br_c1-br_c2)/(br_c2*br_x2-br_c1*br_x1) ! bragg
             ! Cc	end if
             ! C
             ! C	r_s = (1.0d0/abs(cry_b))*rcs*dconjg(rcs)
             r_s1 = sqrt((1.0d0/abs(cry_b))*rcs*dconjg(rcs))
             rcs = rcs/sqrt(abs(cry_b))
             ! C
             ! C p-polarization
             ! C
             c_ppol = abs(cos(torad*2.0d0*graze))
             
             !ctemp = cdsqrt(cry_q*c_ppol**2  + cry_z**2)
             ctemp = mysqrt(cry_q*c_ppol**2  + cry_z**2)
             br_x1 = (-1.0d0*cry_z+ctemp)/(psi_hbar*c_ppol)
             br_x2 = (-1.0d0*cry_z-ctemp)/(psi_hbar*c_ppol)
             br_delta1 = 0.5d0*(psi_0-cry_z+ctemp)
             br_delta2 = 0.5d0*(psi_0-cry_z-ctemp)
             br_c1 = -1.0d0*ci*thickness*twopi/(-1.0d0*abs(gamma_0))/r_lam0* &
                  br_delta1
             br_c2 = -1.0d0*ci*thickness*twopi/(-1.0d0*abs(gamma_0))/r_lam0* &
                  br_delta2
             ! C
             ! C a very big exponential produces numerical overflow. If so the value
             ! C is changed to avoid the overflow. This is equivalent to the thick
             ! C crystal approximation
             ! C changed 700 -> 100 as per MSR.  3/24/95
             ! C
             if (dreal(br_c1).gt.100.or.dreal(br_c2).gt.100) then 
                if (dreal(br_c1).gt.100) br_c1 = 100.0d0+ci*dimag(br_c1)
                if (dreal(br_c2).gt.100) br_c2 = 100.0d0+ci*dimag(br_c2)
             end if
             
             br_c1 = cdexp(br_c1)
             br_c2 = cdexp(br_c2)
             
             ! Cc	if (f_refrac.eq.1) then
             rcp = br_x1*br_x2*(br_c1-br_c2)/(br_x2-br_x1)             ! laue
             ! Cc	else if (f_refrac.eq.0) then
             ! Cc	  rcp = br_x1*br_x2*(br_c1-br_c2)/(br_c2*br_x2-br_c1*br_x1) ! bragg
             ! Cc	end if
             ! C
             ! C	r_p = (1.0d0/abs(cry_b))*rcp*dconjg(rcp)
             r_p1 = sqrt( (1.0d0/abs(cry_b))*rcp*dconjg(rcp) )
             !rcp = rcp/dsqrt(abs(cry_b))
             rcp = rcp/sqrt(abs(cry_b))
             
          ELSE IF (F_REFRAC.EQ.0) THEN
             ! *
             ! * Reflection case
             ! *
             GAMMA = RN*R_LAM0**2/PI
             ETA_S = -0.5D0*ASS_FAC*ALPHA_ZAC+0.5D0*GAMMA*F_0*(1.D0+ASS_FAC)
             ETA_S = ETA_S/(GAMMA*SQRT(ASS_FAC)*STRUCT)
             ETA_P = ETA_S/(ABS(COS_Q_ANG*COS_Q_REF-SIN_Q_ANG*SIN_Q_REF))
             RCS1= ETA_S+SQRT(ETA_S**2-1)
             RCP1= ETA_P+SQRT(ETA_P**2-1)
             RCS2= ETA_S-SQRT(ETA_S**2-1)
             RCP2= ETA_P-SQRT(ETA_P**2-1)
             IF (((CDABS(RCS1))**2).LE.1) THEN
                RCS	= RCS1
             ELSE 
                RCS	= RCS2
             END IF
             RCS	= RCS*SQRT(FH/FH_BAR)
             IF (((CDABS(RCP1))**2).LE.1) THEN
                RCP	= RCP1
             ELSE 
                RCP	= RCP2
             END IF
             RCP	= RCP*SQRT(FH/FH_BAR)
          END IF
          
          IF (GRAZE.GT.45*TORAD) 	RCP = -RCP
          
          R_S	= CDABS(RCS)
          PP	= DREAL(RCS)
          QQ	= DIMAG(RCS)
          CALL	ATAN_2	(QQ,PP,PHASE_S)
          R_P	= CDABS(RCP)
          PP	= DREAL(RCP)
          QQ	= DIMAG(RCP)
          CALL	ATAN_2	(QQ,PP,PHASE_P)
          
       END IF
1122   RETURN
    END IF
  End Subroutine crystal
  
  
  ! C+++
  ! C
  ! C	SUBROUTINE	DIFFRAC
  ! C
  ! C	PURPOSE		to compute the rotation angle to be given to a
  ! C			grating at a given wavelength, for the case of 
  ! C			a constant included angle (TGM, SEYA)
  ! C
  ! C	OUTPUTS 	X_ROT rotation angle in degrees
  ! C--
  
  Subroutine DIFFRAC
    
    IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
    IMPLICIT INTEGER(kind=ski)        (F,I-N)
    
    
    DEFLEC	=   ( T_INCIDENCE + T_REFLECTION )
    THETA0	=   DEFLEC/2*TORAD
    
    SIN_X_ROT =  - ORDER*R_LAMBDA*1.0D-7*RULING/2/COS(THETA0)/10
    IF (ABS(SIN_X_ROT).GE.1.0D0)	THEN
       WRITE(6,*) &
            '**************************************************************' &
            ,'*********************     WARNING   **************************' &
            ,'**************************************************************' &
            ,'            DIFFRACTION    ANGLE  IS  COMPLEX	              ' &
            ,'**************************************************************'
       CALL LEAVE ('DIFFRAC','Check program inputs.',izero)
    ELSE
    END IF
    X_ROT	=   ASIN(SIN_X_ROT)
    X_ROT	=   X_ROT*TODEG
    RETURN
  End Subroutine Diffrac
  
  ! C
  ! C+++
  ! C	subroutine	HOLO_SET
  ! C
  ! C	this subroutine will compute the ruling density at the origin for
  ! C	the case of an holographic grating. This will ensure accuracy in
  ! C	the optical axis position. It will also compute some vectors
  ! C	used later.
  ! C
  ! C--
  Subroutine HOLO_SET
    
    IMPLICIT REAL(kind=skr)    (A-E,G-H,O-Z)
    IMPLICIT INTEGER(kind=ski) (F,I-N)
    
    REAL*8	HYPER(3),HYPER1(3),PPOUT(3),VNOR(3),VTAN(3)
    REAL*8	DIS1(3),DIS2(3)
    REAL*8	VTEMP(3)
    DATA	PPOUT	/3*0.0D0/
    DATA	VNOR	/2*0.0D0,1.0D0/
    ! C
    ! C Establishes the reference vectors used to compute the grooves.
    ! C The azimuthal rotation angles are defined as CCW
    ! C
    ALPHA_1	=   HOLO_RT1*TORAD
    ALPHA_2 =   HOLO_RT2*TORAD
    IF (F_RULING.EQ.2) THEN
       IF (F_PW.EQ.0) THEN
          HOLO1(1) =   HOLO_R1*SIN(HOLO_DEL*TORAD)*SIN(ALPHA_1)
          HOLO1(2) =   HOLO_R1*SIN(HOLO_DEL*TORAD)*COS(ALPHA_1)
          HOLO1(3) =   HOLO_R1*COS(HOLO_DEL*TORAD)
          HOLO2(1) =   HOLO_R2*SIN(HOLO_GAM*TORAD)*SIN(ALPHA_2)
          HOLO2(2) =   HOLO_R2*SIN(HOLO_GAM*TORAD)*COS(ALPHA_2)
          HOLO2(3) =   HOLO_R2*COS(HOLO_GAM*TORAD)
       ELSE IF (F_PW.EQ.1) THEN
          HOLO1(1) =   SIN(HOLO_DEL*TORAD)*SIN(ALPHA_1)
          HOLO1(2) = - SIN(HOLO_DEL*TORAD)*COS(ALPHA_1)
          HOLO1(3) = - COS(HOLO_DEL*TORAD)
          HOLO2(1) =   HOLO_R2*SIN(HOLO_GAM*TORAD)*SIN(ALPHA_2)
          HOLO2(2) =   HOLO_R2*SIN(HOLO_GAM*TORAD)*COS(ALPHA_2)
          HOLO2(3) =   HOLO_R2*COS(HOLO_GAM*TORAD)
       ELSE IF (F_PW.EQ.2) THEN
          HOLO1(1) =   HOLO_R1*SIN(HOLO_DEL*TORAD)*SIN(ALPHA_1)
          HOLO1(2) =   HOLO_R1*SIN(HOLO_DEL*TORAD)*COS(ALPHA_1)
          HOLO1(3) =   HOLO_R1*COS(HOLO_DEL*TORAD)
          HOLO2(1) =   SIN(HOLO_GAM*TORAD)*SIN(ALPHA_2)
          HOLO2(2) = - SIN(HOLO_GAM*TORAD)*COS(ALPHA_2)
          HOLO2(3) = - COS(HOLO_GAM*TORAD)
       ELSE IF (F_PW.EQ.3) THEN
          HOLO1(1) =   SIN(HOLO_DEL*TORAD)*SIN(ALPHA_1)
          HOLO1(2) = - SIN(HOLO_DEL*TORAD)*COS(ALPHA_1)
          HOLO1(3) = - COS(HOLO_DEL*TORAD)
          HOLO2(1) =   SIN(HOLO_GAM*TORAD)*SIN(ALPHA_2)
          HOLO2(2) = - SIN(HOLO_GAM*TORAD)*COS(ALPHA_2)
          HOLO2(3) = - COS(HOLO_GAM*TORAD)
       ELSE
       END IF
    ELSE
    END IF
    ! C
    ! C HOLO1,HOLO2 are the laser sources positions in the case of spherical
    ! C waves, direction cosines for plane waves.
    ! C
    DO 11 I=1,3
       DIS1(I) = HOLO1(I)
       DIS2(I) = HOLO2(I)
11  CONTINUE
       ! C
       ! C DIS1,DIS2 are the direction cosines from laser sources to intercepts
       ! C
    IF (F_PW.EQ.0) THEN		! Both spherical sources
       CALL	VECTOR	(HOLO1,PPOUT,DIS1)
       CALL	VECTOR	(HOLO2,PPOUT,DIS2)
       CALL	NORM	(DIS1,DIS1)
       CALL	NORM	(DIS2,DIS2)
    ELSE IF (F_PW.EQ.1) THEN	! plane/spherical
       CALL	VECTOR	(HOLO2,PPOUT,DIS2)
       CALL	NORM	(DIS2,DIS2)
    ELSE IF (F_PW.EQ.2) THEN	! spherical/plane
       CALL	VECTOR	(HOLO1,PPOUT,DIS1)
       CALL	NORM	(DIS1,DIS1)
    ELSE IF (F_PW.EQ.3) THEN	! plane/plane
       ! C  Nothing to do. DIS1,DIS2 are already normalized.
    ELSE
    END IF
    ! C
    ! C If one of the source is a virtual one, we have to change the direction
    ! C of one of the two versors.
    ! C
    IF (F_VIRTUAL.EQ.1) THEN		! real /virtual
       CALL SCALAR (DIS2,-1.0D0,DIS2)
    ELSE IF (F_VIRTUAL.EQ.2) THEN		! virtual /real
       CALL SCALAR (DIS1,-1.0D0,DIS1)
    ELSE IF (F_VIRTUAL.EQ.3) THEN		! virtual /virtual
       CALL SCALAR (DIS1,-1.0D0,DIS1)
       CALL SCALAR (DIS2,-1.0D0,DIS2)
    END IF
    ! C
    CALL	VECTOR	(DIS2,DIS1,HYPER)
    CALL	NORM	(HYPER,HYPER)
    ! C
    ! C  HYPER is the normal to the hyperboloid in PPOUT. Its direction will
    ! C  be in the y-z plane, if ALPHA_1,ALPHA_2 are zero.
    ! C
    CALL	CROSS	(HYPER,VNOR,VTEMP)
    CALL	NORM	(VTEMP,VTEMP)
    CALL	CROSS	(VNOR,VTEMP,VTAN)
    CALL	NORM	(VTAN,VTAN)
    ! C
    ! C  VTAN is now a vector tangent to the grating surface and orthogonal
    ! C  to the groove. It MUST be (0,1,0) if the ALPHA_1, ALPHA_2 are zero.
    ! C  We compute now the ruling density at that point.
    ! C
    CALL	VECTOR	(DIS2,DIS1,HYPER1)
    CALL	DOT	(VTAN,HYPER1,ADJUST)
    RULING	=   ADJUST*1.0D8/HOLO_W
    
    OPEN  (24,FILE='RULING',STATUS='UNKNOWN')
    REWIND (24)
    
    IF (F_VIRTUAL.EQ.0) THEN
       WRITE (24,*) 'Source:	REAL,		Exit:	REAL'
    ELSE IF (F_VIRTUAL.EQ.1) THEN
       WRITE (24,*) 'Source:	REAL,		Exit:	VIRTUAL'
    ELSE IF (F_VIRTUAL.EQ.2) THEN
       WRITE (24,*) 'Source:	VIRTUAL,	Exit:	REAL'
    ELSE IF (F_VIRTUAL.EQ.3) THEN
       WRITE (24,*) 'Source:	VIRTUAL,	Exit:	VIRTUAL'
    END IF
    WRITE (24,1010) HOLO_DEL
    WRITE (24,1020) HOLO_R1
    WRITE (24,1030) HOLO_GAM
    WRITE (24,1040) HOLO_R2
    WRITE (24,1200) HOLO1(1),HOLO1(2),HOLO1(3)
    WRITE (24,1210) HOLO2(1),HOLO2(2),HOLO2(3)
    WRITE (24,1220) VTAN(1),VTAN(2),VTAN(3)
    WRITE (24,1000)
    WRITE (24,*)	RULING
    WRITE (24,1100)
    CLOSE (24)
    RETURN
1000 FORMAT (1X,'The ruling density at the origin is : ')
1010 FORMAT (1X,'Entrance slit side incidence angle : ',G19.12)
1015 FORMAT (1X,'Source is: ',A20)
1020 FORMAT (1X,'Entrance slit side distance        : ',G19.12)
1025 FORMAT (1X,'Exits is : ',A20)
1030 FORMAT (1X,'Exit slit side incidence angle : ',G19.12)
1040 FORMAT (1X,'Exit slit side distance        : ',G19.12)
1100 FORMAT (1X,'Lines/cm.')
1200 FORMAT (1X,'Position of Entrance Slit Source : ',/, &
          1x,3(5X,G19.12))
1210 FORMAT (1X,'Position of Exit     Slit Source : ',/, &
          1x,3(5X,G19.12))
1220 FORMAT (1X,'Vector orthogonal to grooves at (0,0,0) : ',/, &
          1x,3(5X,G19.12))
  End Subroutine holo_set
  
  
  
  ! C+++
  ! C	SUBROUTINE	IMAGE
  ! C
  ! C	PURPOSE		Computes the intercept of the beam with the 
  ! C			continuation plane. May also compute intercepts
  ! C			on plates.
  ! C
  ! C	ALGORITHM	Direct calculation.
  ! C
  ! C	INPUTS		a) The precomputed parameters through common 
  ! C			   blocks, as obtained from IMREF.
  ! C			b) i_what, OE counter.
  ! C			c) ray, the array describing the beam.
  ! C
  ! C	OUTPUTS		a) ray, the array describing the beam.
  ! C			b) STARxx.DAT, where xx = i_what
  ! C
  ! C---
  
  Subroutine IMAGE1  (RAY,AP,PHASE,I_WHAT)
    
    ! ** This s. computes the intersection of the ray with the image plane.
    ! ** The format is the standard (12,N) matrix. The result is in the form
    ! ** of the (u,v) coordinates onto the image plane. These will be used
    ! ** by the module RESTART to generate the source for the next optical
    ! ** element, if any.
    
    
    !IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
    !IMPLICIT INTEGER(kind=ski)        (F,I-N)
    implicit none

    
    integer(kind=ski),            intent(in)    :: i_what
    real(kind=skr),dimension(:,:),intent(inout) ::  RAY,AP,PHASE
    
    real(kind=skr),dimension(6 ,npoint) ::  RAY_STORE
    real(kind=skr),dimension(12,npoint) ::  PLATE
    
    ! 
    real(kind=skr)  :: xnew,znew
    real(kind=skr)  :: above,below,dist,q_in_mod,rr_attenuation_ima,tmp
    real(kind=skr)  :: UX_1, WY_1, VZ_1, VV_1, VV_2, VV_3, A_1, u_1, u_2, v_1, v_2
    real(kind=skr)  :: rr_reflectivity, test1, test2, a_2, a_3
    integer(kind=ski)               :: j, icheck, iflag, jj, ierr, kounts

    
    real(kind=skr),dimension(3)  :: V_OUT,P_MIR,P_IMAG,A_VEC,AP_VEC,PLATE_CEN

    ! todo danger: srio to check this change - save removed!! 
    ! C
    ! C Save some local large arrays to avoid overflowing stack.
    ! C
    !SAVE		PLATE, RAY_STORE
    
    WRITE(6,*)'Call to IMAGE'
    
    
    ! ** Computes first the intercept onto the true image plane, to be used
    ! ** by RESTART.
    
    DO 100 J=1,NPOINT
       
       ! ** Checks if the ray has been reflected by the mirror.
       
       IF (RAY(10,J).LT.-1.0D6)  THEN
          GO TO 100
       END IF
       IF (N_PLATES.GT.0) THEN
          RAY_STORE(1,J) = RAY(1,J)
          RAY_STORE(2,J) = RAY(2,J)
          RAY_STORE(3,J) = RAY(3,J)
          RAY_STORE(4,J) = RAY(4,J)
          RAY_STORE(5,J) = RAY(5,J)
          RAY_STORE(6,J) = RAY(6,J)
       END IF
       P_MIR(1)	=   RAY(1,J)
       P_MIR(2)	=   RAY(2,J)
       P_MIR(3)	=   RAY(3,J)
       
       V_OUT(1)  =   RAY(4,J)
       V_OUT(2)  =   RAY(5,J)
       V_OUT(3)  =   RAY(6,J)
       
       A_VEC(1)	=   RAY(7,J)
       A_VEC(2)	=   RAY(8,J)
       A_VEC(3)	=   RAY(9,J)
       
       ABOVE	=   T_IMAGE - P_MIR(1)*C_STAR(1) &
            - P_MIR(2)*C_STAR(2) &
            - P_MIR(3)*C_STAR(3)
       
       BELOW   =   C_STAR(1)*V_OUT(1) + C_STAR(2)*V_OUT(2) + &
            C_STAR(3)*V_OUT(3)
       
       IF (BELOW.NE.0.0D0) THEN
          DIST	=   ABOVE/BELOW
       ELSE
          RAY(10,J)  = - 3.0D6
          GO TO 100
       END IF
       
       ! ** Computes now the intersections onto TRUE image plane.
       
       P_IMAG(1)  =   P_MIR(1) + DIST*V_OUT(1)
       P_IMAG(2)  =   P_MIR(2) + DIST*V_OUT(2)
       P_IMAG(3)  =   P_MIR(3) + DIST*V_OUT(3)
       
       
       ! ** Rotate now the results in the STAR (or TRUE image) reference plane.
       ! ** Computes the projection of P_MIR onto the image plane versors.
       
       CALL VECTOR (RIMCEN,P_IMAG,P_IMAG)
       
       CALL DOT (P_IMAG,UXIM,UX_1)
       CALL DOT (P_IMAG,VZIM,VZ_1)
       CALL DOT (P_IMAG,VNIMAG,WY_1)
       
       ! ** Computes now the new directions for the beam in the U,V,N ref.
       
       CALL DOT (V_OUT,UXIM,VV_1)
       CALL DOT (V_OUT,VNIMAG,VV_2)
       CALL DOT (V_OUT,VZIM,VV_3)
       
       ! ** Computes the new directions of A in the U,V,N ref.frame
       
       CALL DOT (A_VEC,UXIM,A_1)
       CALL DOT (A_VEC,VNIMAG,A_2)
       CALL DOT (A_VEC,VZIM,A_3)
       
      rr_reflectivity = 1.0D0
      IF (F_REFRAC.EQ.1) THEN 
         !r_indImag_ima = 0d0 !ABS(7.35e-8)
         ! we need now to call get_refraction_index (again, first called in mirror1)
         ! because rr_attenuation_ima is not global
         Q_IN_MOD    =   RAY(11,J)
         call get_refraction_index(ione,Q_IN_MOD,tmp,tmp,tmp,rr_attenuation_ima)
         IF (ABS(rr_attenuation_ima).GT.1e-15) THEN
           ! for amplitudes use sqrt(reflectivity)
           !rr_reflectivity = sqrt(exp(-ABS(r_attenuation_ima*DIST)))
           rr_reflectivity = sqrt(exp(-ABS(rr_attenuation_ima*DIST)))
         END IF
         !if (j.le.10) print *,'>>>>0 ima: ',rr_attenuation_ima,DIST,rr_reflectivity
      END IF
       ! ** Saves the results
       
       RAY(1,J)  =   UX_1
       RAY(2,J)  =   WY_1
       RAY(3,J)  =   VZ_1
       RAY(4,J)  =   VV_1
       RAY(5,J)  =   VV_2
       RAY(6,J)  =   VV_3
       RAY(7,J)  =   A_1 * rr_reflectivity
       RAY(8,J)  =   A_2 * rr_reflectivity
       RAY(9,J)  =   A_3 * rr_reflectivity

       IF (NCOL.GT.12) THEN
          ! changed srio@esrf.eu 2012-01-26
          ! the optical path is always positive (use ABS(DIST)) and 
          ! must not be affected by the refraction index if refractor
          ! is not set
          IF (F_REFRAC.EQ.0) THEN 
            PHASE (1,J) = PHASE(1,J) + ABS(DIST)
          ELSE 
            PHASE (1,J) = PHASE(1,J) + ABS(DIST)*R_IND_IMA
          END IF
          !PHASE	(1,J) = PHASE (1,J) + DIST*R_IND_IMA

        IF (NCOL.EQ.18) THEN
           AP_VEC(1) = AP(1,J)
           AP_VEC(2) = AP(2,J)
           AP_VEC(3) = AP(3,J)
           CALL DOT (AP_VEC,UXIM,A_1)
           CALL DOT (AP_VEC,VNIMAG,A_2)
           CALL DOT (AP_VEC,VZIM,A_3)
           AP(1,J) = A_1 * rr_reflectivity
           AP(2,J) = A_2 * rr_reflectivity
           AP(3,J) = A_3 * rr_reflectivity
        END IF
       END IF
     
100 CONTINUE
     
     ! C
     ! C 4/26/93 making use of the slit rotation option.
     ! C
     
    IF (FSLIT.EQ.1) THEN
     
       U_1   = - SLLEN/2
       U_2   =   SLLEN/2
       V_1   = - SLWID/2
       V_2   =   SLWID/2
       SLTILT= TORAD*SLTILT
       
       DO 200 ICHECK=1,NPOINT
          XNEW = RAY(1,ICHECK)*COS(SLTILT) + &
               RAY(3,ICHECK)*SIN(SLTILT)
          ZNEW = RAY(3,ICHECK)*COS(SLTILT) - &
               RAY(1,ICHECK)*SIN(SLTILT)
          
          TEST1 = (XNEW - U_1)*(U_2 - XNEW)
          TEST2 = (ZNEW - V_1)*(V_2 - ZNEW)
          ! C TEST1 = (RAY(1,ICHECK) - U_1)*(U_2 - RAY(1,ICHECK))
          ! C TEST2 = (RAY(3,ICHECK) - V_1)*(V_2 - RAY(3,ICHECK))
          IF (TEST1.GE.0.0D0.AND.TEST2.GE.0.0D0) THEN
             PLATE(10,ICHECK) = 1.0D0
          ELSE
             RAY(10,ICHECK)   = - 2.0D0
             PLATE(10,ICHECK) = - 2.0D0
          END IF
          
200    CONTINUE
          
    ELSE
    END IF

       ! C
       ! C Write out file if flag is enabled
       ! C
    IF ((FWRITE.EQ.0).OR.(FWRITE.EQ.2)) THEN
       CALL FNAME (FFILE, 'star', I_WHAT, izero)
       IFLAG = 0
       CALL WRITE_OFF (FFILE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,izero,IERR)
       IF (IERR.NE.0) CALL LEAVE ('IMAGE','Error writing STAR',IERR)
    END IF
    ! C
    ! C Computes now the (optional) images.
    ! C
    DO 300 JJ=1,N_PLATES
       
       PLATE_CEN(1) = C_STAR(1)*D_PLATE(JJ)
       PLATE_CEN(2) = C_STAR(2)*D_PLATE(JJ)
       PLATE_CEN(3) = C_STAR(3)*D_PLATE(JJ)
       
       DO 400 J=1,NPOINT
          ! C
          ! C Checks if the ray has been reflected by the mirror.
          ! C
          IF (RAY(10,J).LT.-1.0D6) THEN
             PLATE(10,J)  = - 1.0D6
             GO TO 400
          ELSE
          END IF
          ! C
          P_MIR(1) = RAY_STORE(1,J) 
          P_MIR(2) = RAY_STORE(2,J)
          P_MIR(3) = RAY_STORE(3,J)
          
          V_OUT(1) = RAY_STORE(4,J)
          V_OUT(2) = RAY_STORE(5,J)
          V_OUT(3) = RAY_STORE(6,J)
          
          ABOVE = (PLATE_CEN(1) - P_MIR(1))*C_PLATE(1) + &
                  (PLATE_CEN(2) - P_MIR(2))*C_PLATE(2) + &
                  (PLATE_CEN(3) - P_MIR(3))*C_PLATE(3)
          
          BELOW  = C_PLATE(1)*V_OUT(1) + C_PLATE(2)*V_OUT(2) + &
                   C_PLATE(3)*V_OUT(3)
          
          IF (BELOW.NE.0.0D0) THEN
             DIST = ABOVE/BELOW
          ELSE
             PLATE(10,J) = -3.0D0
             GO TO 400
          END IF
          ! C
          ! C Computes now the intersections onto the PLATE plane.
          ! C
          P_IMAG(1) = P_MIR(1) + DIST*V_OUT(1)
          P_IMAG(2) = P_MIR(2) + DIST*V_OUT(2)
          P_IMAG(3) = P_MIR(3) + DIST*V_OUT(3)
          ! C
          ! C Rotate now the results in the PLATE plane.
          ! C Computes the projection of P_IMAG onto the image plane versors.
          ! C
          CALL VECTOR (PLATE_CEN,P_IMAG,P_IMAG)
          
          CALL DOT (P_IMAG,UX_PL,UX_1)
          CALL DOT (P_IMAG,VZ_PL,VZ_1)
          CALL DOT (P_IMAG,WY_PL,WY_1)
          ! C
          ! C Computes now the new directions for the beam in the U,V,N ref.
          ! C
          CALL DOT (V_OUT,UX_PL,VV_1)
          CALL DOT (V_OUT,WY_PL,VV_2)
          CALL DOT (V_OUT,VZ_PL,VV_3)
          
          PLATE(1,J)  =   UX_1
          PLATE(2,J)  =   WY_1
          PLATE(3,J)  =   VZ_1
          PLATE(4,J)  =   VV_1
          PLATE(5,J)  =   VV_2
          PLATE(6,J)  =   VV_3
          PLATE(7,J)   =   RAY(7,J)
          PLATE(8,J)   =   RAY(8,J)
          PLATE(9,J)   =   RAY(9,J)
          PLATE(10,J)  =   RAY(10,J)
          PLATE(11,J)  =   RAY(11,J)
          PLATE(12,J)  =   RAY(12,J)
400    CONTINUE
          
       ! C
       KOUNTS = 100*I_WHAT + JJ
       CALL FNAME(FFILE, 'plate', KOUNTS, ifour)
       IFLAG = 0
       CALL WRITE_OFF (FFILE,PLATE,PHASE,AP,NCOL,NPOINT,IFLAG, izero,IERR)
       IF (IERR.NE.0) CALL LEAVE ('IMAGE','Error writing PLATE',IERR)
       ! C The following cannot be implemented until the phase and AP is recompute at
       ! C the plate.
       ! C	IF (F_POLAR.EQ.1) THEN
       ! C	  CALL	FNAME	(FFILE, 'PLATEP', I_WHAT, 2)
       ! C	  CALL  WRITE_AP	(FFILE,NPOINT,PLATE,AP,IERR)
       ! C	  IF (IERR.EQ.1) STOP 'Error writing PLATEP'
       ! C	  CALL	FNAME	(FFILE, 'PHPLAT', I_WHAT, 2)
       ! C	  CALL  WRITE_PHA	(FFILE,NPOINT,PHASE,IERR)
       ! C	  IF (IERR.EQ.1) STOP 'Error writing PLATEP'
       ! C	END IF
       
300 CONTINUE
9990 FORMAT (I4.4)
    WRITE(6,*)'Exit from IMAGE'
         
  End Subroutine image1
  
       
  ! C+++
  ! C	SUBROUTINE 	IMREF
  ! C
  ! C	PURPOSE		Defines the reference frame for IMAGE and SCREEN
  ! C			computes some variables
  ! C
  ! C---
  Subroutine IMREF
    
    
    !	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
    !	IMPLICIT INTEGER(kind=ski)        (F,I-N)
    !     	DIMENSION	UX_SC(3),WY_SC(3),VZ_SC(3)
    
    real(kind=skr),dimension(3)  :: UX_SC,WY_SC,VZ_SC
    integer(kind=ski)                      :: I
    
    ! C
    WRITE(6,*)'Call to IMREF'
    ! C
    ! C   Defines some useful variables
    ! C
    IF (F_PLATE.EQ.1) THEN
       THETA_I  =   PIHALF - T_REFLECTION
       ALPHA_I  =   0.0D0
    ELSE
       THETA_I  =   THETA_I*TORAD
       ALPHA_I  =   ALPHA_I*TORAD
    END IF
    
    SINTHE_I  =   SIN( THETA_I )
    COSTHE_I  =   COS( THETA_I )
    SINAL_I   =   SIN( ALPHA_I )
    COSAL_I   =   COS( ALPHA_I )
    ! C
    ! C   Generates the versor normal to the 'continuation' plane.
    ! C   The angles for the grating case have been established in setsour.
    ! C
    VNIMAG(1)   =   .0D0
    VNIMAG(2)   =   SIN( T_REFLECTION )
    VNIMAG(3)   =   COS( T_REFLECTION )
    ! C
    ! C   Computes the image center for STAR
    ! C
    RIMCEN(1) =  VNIMAG(1)*T_IMAGE
    RIMCEN(2) =  VNIMAG(2)*T_IMAGE
    RIMCEN(3) =  VNIMAG(3)*T_IMAGE
    ! C
    ! C   Computes the image plane coefficients fo STAR
    ! C
    C_STAR(1)  =   VNIMAG(1)
    C_STAR(2)  =   VNIMAG(2)
    C_STAR(3)  =   VNIMAG(3)
    ! C
    ! C   Computes now the other versors
    ! C
    UXIM(1)	=   1.0D0
    UXIM(2) =    .0D0
    UXIM(3) =    .0D0
    
    VZIM(1) =    .0D0
    VZIM(2) = - COS( T_REFLECTION )
    VZIM(3) =   SIN( T_REFLECTION )
    ! C
    ! C   Same thing for PLATE
    ! C
    C_PLATE(1)  = - SINAL_I*COSTHE_I
    C_PLATE(2)  =   COSAL_I*COSTHE_I
    C_PLATE(3)  =   SINTHE_I
    
    WY_PL(1) =   C_PLATE(1)
    WY_PL(2) =   C_PLATE(2)
    WY_PL(3) =   C_PLATE(3)
    
    CALL CROSS (C_PLATE,Z_VRS,UX_PL)
    CALL NORM (UX_PL,UX_PL)
    CALL CROSS (UX_PL,C_PLATE,VZ_PL)
    CALL NORM (VZ_PL,VZ_PL)
    
    ! C
    ! C In the case of the screen, the screen x-versor is always parallel
    ! C to X_VRS. This will also take care ot the normal incidence case
    ! C F.C.-19mar84
    ! C
    UX_SC(1)	=  UXIM(1)
    UX_SC(2)	=  UXIM(2)
    UX_SC(3)	=  UXIM(3)
    IF (F_SCREEN.EQ.1) THEN
       DO 11 I=1,2
     	  IF (I.EQ.1) THEN
             WY_SC(1) = 0.0D0
             WY_SC(2)  =   SIN(T_REFLECTION)
             WY_SC(3)  =   COS(T_REFLECTION)
          ELSE
             WY_SC(1)  =   0.0D0
             WY_SC(2)  =   SIN(T_INCIDENCE)
             WY_SC(3)  = - COS(T_INCIDENCE)
          END IF
        
          CALL CROSS 	(UX_SC,WY_SC,VZ_SC)
          CALL NORM 	(VZ_SC,VZ_SC)
          
          UX_SCR(1,I)  =   UX_SC(1)
          UX_SCR(2,I)  =   UX_SC(2)
          UX_SCR(3,I)  =   UX_SC(3)
          VZ_SCR(1,I)   =   VZ_SC(1)
          VZ_SCR(2,I)   =   VZ_SC(2)
          VZ_SCR(3,I)   =   VZ_SC(3)
          WY_SCR(1,I)   =   WY_SC(1)
          WY_SCR(2,I)   =   WY_SC(2)
          WY_SCR(3,I)   =   WY_SC(3)
11     CONTINUE

    ELSE
    END IF

    WRITE(6,*)'Exit from IMREF'

  End Subroutine imref


! C+++
! C	SUBROUTINE	OPTAXIS
! C
! C	PURPOSE		Defines the optical axis in the lab reference
! C			frame.
! C---
  SUBROUTINE OPTAXIS (I_MIRROR)
    
    ! ** This Subroutine keeps the accounting of the optycal system in the
    ! ** source reference frame. This is done by means of the array OPTAXIS
    ! **	CENTRAL (1:3)   Source coordinates for the 'I' mirror
    ! **	CENTRAL (4:6)   Mirror optical center coordinates
    ! **	CENTRAL (7:9)   Image position.
    ! **	CENTRAL (10:12) Binormal vector 	>> U_VEC
    ! **	CENTRAL (13:15) Tangent vector  	>> V_VEC
    ! **	CENTRAL (16:18) Mirror normal   	>> W_VEC
    ! **	CENTRAL (19:21) Reflected versor 	>> V_REF
    ! **	CENTRAL (22:24) Normal to reflec.	>> V_PERP
    ! ** The outputs are in the source reference frame.
    ! ** We use T_REFLECTION to take in account the case of the grating.
    
    ! todo: remove implicits
    !	implicit real(kind=kind(1.0d0)) (a-e,g-h,o-z)
    !	implicit integer(kind=ski)        (f,i-n)
    
    integer(kind=ski),intent(in)         :: i_mirror
    real(kind=skr),dimension(3)  :: V_REF,V_TEMP,U_VEC,V_VEC,W_VEC
    real(kind=skr),dimension(3)  :: V_PERP,U_OLD,V_OLD,W_OLD,R_OLD
    real(kind=skr),dimension(3)  :: RP_OLD
    real(kind=skr)               :: deflection
    integer(kind=ski)            :: kount,iwhich,i_write,j

    character(len=sklen)         :: stmp
    integer(kind=ski)            :: eof,itmp
    real(kind=skr),dimension(24) :: central_old

    WRITE(6,*)'Call to OPTAXIS'
    CALL	FNAME (FFILE,'optax',I_MIRROR,izero)
    DEFLECTION	=   T_INCIDENCE + T_REFLECTION
    IF (I_MIRROR.EQ.1) THEN
       U_VEC (1)	=   COSAL
       U_VEC (2)	=   0.0D0
       U_VEC (3)     =   SINAL
       V_VEC (1)     	= - SIN(PIHALF - T_INCIDENCE)*SINAL
       V_VEC (2)     	=   COS(PIHALF - T_INCIDENCE)
       V_VEC (3)     =   SIN(PIHALF - T_INCIDENCE)*COSAL
       W_VEC (1)    	= - SIN(PI - T_INCIDENCE)*SINAL
       W_VEC (2)    	=   COS(PI - T_INCIDENCE)
       W_VEC (3)    	=   SIN(PI - T_INCIDENCE)*COSAL
       V_REF (1)     	= - SIN(PI - DEFLECTION)*SINAL
       V_REF (2)     	=   COS(PI - DEFLECTION)
       V_REF (3)     =   SIN(PI - DEFLECTION)*COSAL
       V_PERP (1)     	= - SIN(3*PIHALF - DEFLECTION)*SINAL
       V_PERP (2)     =   COS(3*PIHALF - DEFLECTION)
       V_PERP (3)    =   SIN(3*PIHALF - DEFLECTION)*COSAL
       
       CENTRAL(1)  	=   .0D0
       CENTRAL(2)  	=   .0D0
       CENTRAL(3)  	=   .0D0
       CENTRAL(4)  	=   .0D0
       CENTRAL(5)  	=   T_SOURCE
       CENTRAL(6)  	=   .0D0
       CENTRAL(7) 	=   T_IMAGE*V_REF(1)
       CENTRAL(8) 	=   T_IMAGE*V_REF(2) + T_SOURCE
       CENTRAL(9) 	=   T_IMAGE*V_REF(3)
       CENTRAL(10)	=   U_VEC(1)
       CENTRAL(11)	=    U_VEC(2)
       CENTRAL(12)	=     U_VEC(3)
       CENTRAL(13)	=   V_VEC(1)
       CENTRAL(14)	=    V_VEC(2)
       CENTRAL(15)	=     V_VEC(3)
       CENTRAL(16)	=   W_VEC(1)
       CENTRAL(17)	=    W_VEC(2)
       CENTRAL(18)	=     W_VEC(3)
       CENTRAL(19)	=   V_REF(1)
       CENTRAL(20)	=    V_REF(2)
       CENTRAL(21)	=     V_REF(3)
       CENTRAL(22) 	=   V_PERP(1)
       CENTRAL(23) 	=    V_PERP(2)
       CENTRAL(24) 	=     V_PERP(3)
    ELSE
       !KOUNT	=   I_MIRROR
       !IWHICH  =   I_MIRROR - 1
       iwhich = 1
       kount = 1
       central_old(:) = central(:)
       ! ** Computes now the OLD mirror reference frame in the lab. coordinates
       ! ** system. The rotation angle ALPHA of the current mirror is defined in
       ! ** this reference frame, as ALPHA measure the angle between the two
       ! ** incidence planes (not necessarily the same).
       
       U_OLD (1)	=   CENTRAL (10)
       U_OLD (2)	=   CENTRAL (11)
       U_OLD (3)	=   CENTRAL (12)
       ! *     	V_OLD (1)	=   CENTRAL (IWHICH,13)
       ! *     	 V_OLD (2)	=   CENTRAL (IWHICH,14)
       ! *     	  V_OLD (3)	=   CENTRAL (IWHICH,15)
       ! *     	W_OLD (1)	=   CENTRAL (IWHICH,16)
       ! *     	 W_OLD (2)	=   CENTRAL (IWHICH,17)
       ! *     	  W_OLD (3)	=   CENTRAL (IWHICH,18)
       R_OLD (1)     	=   CENTRAL (19)
       R_OLD (2)     	=   CENTRAL (20)
       R_OLD (3)    	=   CENTRAL (21)
       RP_OLD (1)     	=   CENTRAL (22)
       RP_OLD (2)    	=   CENTRAL (23)
       RP_OLD (3)   	=   CENTRAL (24)
       ! ** This vector is the NORMAL of the new mirror in the OMRF (U,R_OLD,RP_OLD) **
       V_TEMP (1)	= - SIN(PI - T_INCIDENCE)*SINAL
       V_TEMP (2)	=   COS(PI - T_INCIDENCE)
       V_TEMP (3)	=   SIN(PI - T_INCIDENCE)*COSAL
       ! ** Rotate it finally to (x,y,z) SRF **
       W_VEC (1)	=    V_TEMP(1)*U_OLD(1) + &
            V_TEMP(2)*R_OLD(1) + &
            V_TEMP(3)*RP_OLD(1)
       W_VEC (2)	=    V_TEMP(1)*U_OLD(2) + &
            V_TEMP(2)*R_OLD(2) + &
            V_TEMP(3)*RP_OLD(2)
       W_VEC (3)	=    V_TEMP(1)*U_OLD(3) + &
            V_TEMP(2)*R_OLD(3) + &
            V_TEMP(3)*RP_OLD(3)
       ! ** This vector is the reflected beam from the new mirror in the OMRF **
       V_TEMP (1)    	= -  SIN(PI - DEFLECTION)*SINAL
       V_TEMP (2)    	=    COS(PI - DEFLECTION)
       V_TEMP (3)    	=    SIN(PI - DEFLECTION)*COSAL
       ! ** Express it now in the (x,y,z) SRF
       V_REF (1)	=    V_TEMP(1)*U_OLD(1) + &
            V_TEMP(2)*R_OLD(1) + &
            V_TEMP(3)*RP_OLD(1)
       V_REF (2)	=    V_TEMP(1)*U_OLD(2) + &
            V_TEMP(2)*R_OLD(2) + &
            V_TEMP(3)*RP_OLD(2)
       V_REF (3)	=    V_TEMP(1)*U_OLD(3) + &
            V_TEMP(2)*R_OLD(3) + &
            V_TEMP(3)*RP_OLD(3)
       ! ** This is now the perp. vector in the OMRF **
       V_TEMP (1)     = - SIN(3*PIHALF - DEFLECTION)*SINAL 
       V_TEMP (2)     =   COS(3*PIHALF - DEFLECTION)
       V_TEMP (3)     =   SIN(3*PIHALF - DEFLECTION)*COSAL
       ! ** Rotate it to the SRF
       V_PERP (1)	=    V_TEMP(1)*U_OLD(1) + &
            V_TEMP(2)*R_OLD(1) + &
            V_TEMP(3)*RP_OLD(1)
       V_PERP (2)	=    V_TEMP(1)*U_OLD(2) + &
            V_TEMP(2)*R_OLD(2) + &
            V_TEMP(3)*RP_OLD(2)
       V_PERP (3)	=    V_TEMP(1)*U_OLD(3) + &
            V_TEMP(2)*R_OLD(3) + &
            V_TEMP(3)*RP_OLD(3)
       ! ** This is the tangent vector in the OMRF **
       V_TEMP (1)	= - SIN(PIHALF - T_INCIDENCE)*SINAL
       V_TEMP (2)	=   COS(PIHALF - T_INCIDENCE)
       V_TEMP (3)	=   SIN(PIHALF - T_INCIDENCE)*COSAL
       ! ** Rotate it to the SRF.
       V_VEC (1)	=    V_TEMP(1)*U_OLD(1) + &
            V_TEMP(2)*R_OLD(1) + &
            V_TEMP(3)*RP_OLD(1)
       V_VEC (2)	=    V_TEMP(1)*U_OLD(2) + &
            V_TEMP(2)*R_OLD(2) + &
            V_TEMP(3)*RP_OLD(2)
       V_VEC (3)	=    V_TEMP(1)*U_OLD(3) + &
            V_TEMP(2)*R_OLD(3) + &
            V_TEMP(3)*RP_OLD(3)
       ! ** Last, we generate U_VEC in the OMRF **
       V_TEMP (1)	=   COSAL
       V_TEMP (2)	=   .0D0
       V_TEMP (3)	=   SINAL
       ! ** rotate to SRF
       U_VEC (1)	=    V_TEMP(1)*U_OLD(1) + &
            V_TEMP(2)*R_OLD(1) + &
            V_TEMP(3)*RP_OLD(1)
       U_VEC (2)	=    V_TEMP(1)*U_OLD(2) + &
            V_TEMP(2)*R_OLD(2) + &
            V_TEMP(3)*RP_OLD(2)
       U_VEC (3)	=    V_TEMP(1)*U_OLD(3) + &
            V_TEMP(2)*R_OLD(3) + &
            V_TEMP(3)*RP_OLD(3)
       ! ** All done. Write to the array and leave.
       
       CENTRAL (1)   =   CENTRAL_old (7)
       CENTRAL (2)   =   CENTRAL_old (8)
       CENTRAL (3)   =   CENTRAL_old (9)
       CENTRAL (4)   =  T_SOURCE*R_OLD(1) + CENTRAL(1)
       CENTRAL (5)   =  T_SOURCE*R_OLD(2) + CENTRAL(2)
       CENTRAL (6)   =  T_SOURCE*R_OLD(3) + CENTRAL(3)
       CENTRAL (7)   =   T_IMAGE*V_REF(1) + CENTRAL(4)
       CENTRAL (8)   =   T_IMAGE*V_REF(2) + CENTRAL(5)
       CENTRAL (9)   =   T_IMAGE*V_REF(3) + CENTRAL(6)
       CENTRAL (10)  =   U_VEC(1)
       CENTRAL (11)  =    U_VEC(2)
       CENTRAL (12)  =     U_VEC(3)
       CENTRAL (13)  =   V_VEC(1)
       CENTRAL (14)  =    V_VEC(2)
       CENTRAL (15)  =     V_VEC(3)
       CENTRAL (16)  =   W_VEC(1)
       CENTRAL (17)  =    W_VEC(2)
       CENTRAL (18)  =     W_VEC(3)
       CENTRAL (19)  =   V_REF(1)
       CENTRAL (20)  =    V_REF(2)
       CENTRAL (21)  =     V_REF(3)
       CENTRAL (22)  =   V_PERP(1)
       CENTRAL (23)  =    V_PERP(2)
       CENTRAL (24)  =     V_PERP(3)
       
    END IF
    

    ! srio@esrf.eu 20110412 avoid writing optax.xx if FWRITE=3
    IF (FWRITE.NE.3) THEN 

    OPEN (UNIT=23,FILE= FFILE,STATUS='UNKNOWN')
    REWIND (23)

!    ! method 1
!    DO I_WRITE=1,I_MIRROR
!       WRITE (23,*) I_WRITE
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =  1,3)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =  4,6)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =  7,9)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =10,12)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =13,15)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =16,18)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =19,21)
!       WRITE (23,*) ( CENTRAL(I_WRITE,J), J =22,24)
!    END DO

    ! method 2
       ! if I_MIRROR > 1 copy last file
       IF (I_MIRROR.GT.1) THEN 
         itmp = I_MIRROR-1
         CALL  FNAME (FFILE,'optax',itmp,izero)
         OPEN (UNIT=24,FILE= FFILE,STATUS='UNKNOWN',IOSTAT=eof)
         REWIND (24)
         DO WHILE (eof == 0)
            read(24,'(a)', IOSTAT=eof) stmp
            if (eof == 0) write(23,'(a)') trim(stmp)
         END DO
         CLOSE(24)
       END IF
       !I_WRITE=I_MIRROR
       WRITE (23,*) I_MIRROR
       WRITE (23,*) ( CENTRAL(J), J =  1,3)
       WRITE (23,*) ( CENTRAL(J), J =  4,6)
       WRITE (23,*) ( CENTRAL(J), J =  7,9)
       WRITE (23,*) ( CENTRAL(J), J =10,12)
       WRITE (23,*) ( CENTRAL(J), J =13,15)
       WRITE (23,*) ( CENTRAL(J), J =16,18)
       WRITE (23,*) ( CENTRAL(J), J =19,21)
       WRITE (23,*) ( CENTRAL(J), J =22,24)
    CLOSE (23)
    END IF
    WRITE(6,*)'Exit from OPTAXIS'
  End Subroutine optaxis
  
  
  ! C+++
  ! C
  ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  ! C
  ! C	subroutine	intercept	( xin, vin, tpar, iflag)
  ! C
  ! C	purpose		computes the intercepts onto the mirror surface
  ! C
  ! C	arguments	xin	ray starting position     mirror RF
  ! C			vin	ray direction		  mirror RF
  ! C			tpar	distance from start of
  ! C				intercept
  ! C			iflag   input		1	ordinary case
  ! C					       -1	ripple case
  ! C			iflag	output		0	success
  ! C					       -1       complex sol.
  ! C
  Subroutine INTERCEPT (XIN, VIN, TPAR, IFLAG)
    implicit none
    
    real(kind=skr),dimension(3),intent(in) :: xin,vin
    real(kind=skr),intent(out)             :: tpar
    integer(kind=ski),intent(out)          :: iflag
    real(kind=skr)                         :: AA,BB,CC,DETER,DENOM,TPAR1,TPAR2

    ! C
    ! C tests for non-conic mirrors
    ! C
    IF (FMIRR.EQ.3) THEN  ! Torus
       CALL QUARTIC( XIN, VIN, TPAR, IFLAG)
       IF (IFLAG.LT.0) RETURN
    ELSE IF (FMIRR.EQ.9) THEN ! Gen poly
       IF (F_KOMA.NE.1) THEN
          CALL POLY ( XIN, VIN, TPAR, IFLAG)
       ELSE
          CALL SPOLY ( XIN, VIN, TPAR, IFLAG)
       END IF
       IF (IFLAG.LT.0) RETURN
    ELSE 
       ! C
       ! C conic mirrors
       ! C
       AA       =   CCC(1)*VIN(1)**2  &
            + CCC(2)*VIN(2)**2  &
            + CCC(3)*VIN(3)**2 &
            + CCC(4)*VIN(1)*VIN(2)  &
            + CCC(5)*VIN(2)*VIN(3) &
            + CCC(6)*VIN(1)*VIN(3)
       BB       =   CCC(1)*XIN(1)*VIN(1)*2 &
            + CCC(2)*XIN(2)*VIN(2)*2 &
            + CCC(3)*XIN(3)*VIN(3)*2 &
            + CCC(4)*(XIN(2)*VIN(1)  &
            + XIN(1)*VIN(2)) &
            + CCC(5)*(XIN(3)*VIN(2)  &
            + XIN(2)*VIN(3)) &
            + CCC(6)*(XIN(1)*VIN(3)  &
            + XIN(3)*VIN(1))  &
            + CCC(7)*VIN(1)  &
            + CCC(8)*VIN(2)  &
            + CCC(9)*VIN(3) 
! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/hyperbola_fixes_2008-10-22.txt
       ! Csrio     $              + CCC(10)
       CC       =   CCC(1)*XIN(1)**2  &
            + CCC(2)*XIN(2)**2  &
            + CCC(3)*XIN(3)**2   &
            + CCC(4)*XIN(2)*XIN(1)  &
            + CCC(5)*XIN(2)*XIN(3)  &
            + CCC(6)*XIN(1)*XIN(3) &
            + CCC(7)*XIN(1) &
            + CCC(8)*XIN(2) &
            + CCC(9)*XIN(3) &
            + CCC(10)
       ! C
       ! C Solve now the second deg. equation **
       ! C
       IF (ABS(AA).GT.1.0D-15) THEN
          DENOM       = 0.5D0/AA
       ELSE IF (BB.NE.0.0D0) THEN
          TPAR         = - CC/BB
          GO TO 100
       ELSE
          WRITE(6,*)'Intercept error. All coefficients were zero'
          !STOP
          GO TO 200
       END IF
       ! C
       !DETER       = BB**2 - CC*AA*4
       DETER = BB*BB - CC*AA*4
       ! C
       IF (DETER.LT.0.0) THEN
          GO TO 200
       ELSE
          ! niccolo.canestrari@gmail.com
          ! avoid loss of significance B ~ DETER, that happens if AA<<
          !TPAR1       = -(BB + SQRT(DETER))*DENOM
          !TPAR2       = -(BB - SQRT(DETER))*DENOM
          if(ABS(4*CC*AA/BB/BB).ge.1.0D-6) then  ! normal case
            TPAR1 = -(BB + SQRT(DETER))*DENOM
            TPAR2 = -(BB - SQRT(DETER))*DENOM
          else  ! patological case, e.g., in parabolic lenses close to the pole
            TPAR1 = - CC/BB - AA*CC**2/BB**3
            TPAR2 = -(BB/AA + TPAR1)
          endif
       END IF

       IF (IFLAG.LT.0) THEN
          ! C
          ! C Ripple case : always take the closest intercept onto the ideal surface.
          ! C
          IF (ABS(TPAR1).LT.ABS(TPAR2)) THEN
             TPAR = TPAR1
          ELSE
             TPAR = TPAR2
          END IF
       ELSE 
          ! C
          ! C tests for convexity
          ! C
          ! Csrio              IF (FMIRR.NE.7.AND.F_CONVEX.NE.1) THEN
          ! Csrio                TPAR      =   MAX (TPAR1,TPAR2)
          ! Csrio              ELSE
          ! Csrio                TPAR      =   MIN (TPAR1,TPAR2)
          ! Csrio              END IF
          ! C
          ! C Changed to take the solution closer to the mirror pole. 
          ! C This was needed to correctly calculate hyperbolic Laue crystals
          ! C srio@esrf.eu 2008-10-15
          ! C see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/hyperbola_fixes_2008-10-22.txt
          ! C
          IF ( ABS(TPAR1-T_SOURCE).LE.ABS(TPAR2-T_SOURCE)  ) THEN
             TPAR=TPAR1
          ELSE
             TPAR=TPAR2
          END IF
       END IF
    END IF
    ! C
    ! C Success
    ! C
100 IFLAG =  0
    RETURN
    ! C
    ! C failure
    ! C
200 IFLAG = -1
    RETURN
  End Subroutine intercept

  
  ! C+++
  ! C	SUBROUTINE	QUARTIC
  ! C
  ! C	PURPOSE		To compute the intercepts on a given torus for
  ! C			a ray.
  ! C
  ! C	INPUT		XIN	ray starting point
  ! C			V       ray direction
  ! C			i_res	-1, ripple case
  ! C				 1, ordinary torus
  ! C
  ! C	OUTPUT		a) "answer", distance from point to intercept
  ! C			    the value returned is the LARGEST of the 4
  ! C			    possible values.
  ! C			b) "i_res", status flag; +1 good
  ! C			  			 -1 all intercepts complex
  ! C---
  Subroutine QUARTIC (XIN, V, ANSWER, I_RES)
    ! C
    
    IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
    IMPLICIT INTEGER(kind=ski)        (F,I-N)
    
    COMPLEX*16	H_OUTPUT(4)
    
    !srio danger  (to test)
    real(kind=skr),dimension(8)  :: h_output88
    equivalence (h_output88(1), H_OUTPUT(1))
    
    DIMENSION	XIN(3),P(3),V(3),COEFF(5),TEST1(4),TEST2(4)
    ANSWER	=   0.0D0
    P(1)	= XIN(1)
    P(2)	= XIN(2)
    ! C
    ! C move the ref. frame to the torus one.
    ! C
    IF (F_TORUS.EQ.0) THEN
       P(3)	= XIN(3) - R_MAJ - R_MIN   
    ELSE IF (F_TORUS.EQ.1) THEN
       P(3)	= XIN(3) - R_MAJ + R_MIN   
    ELSE IF (F_TORUS.EQ.2) THEN
       P(3)	= XIN(3) + R_MAJ - R_MIN   
    ELSE IF (F_TORUS.EQ.3) THEN
       P(3)	= XIN(3) + R_MAJ + R_MIN   
    END IF
    ! ** Evaluates the quartic coefficients **
    A	= R_MAJ**2 - R_MIN**2
    B	= - (R_MAJ**2 + R_MIN**2)
    AA	= P(1)*V(1)**3 + P(2)*V(2)**3 + P(3)*V(3)**3 + &
         V(1)*V(2)**2*P(1) + V(1)**2*V(2)*P(2) + &
         V(1)*V(3)**2*P(1) + V(1)**2*V(3)*P(3) + &
         V(2)*V(3)**2*P(2) + V(2)**2*V(3)*P(3)
    AA	= 4*AA
    BB	= 3*P(1)**2*V(1)**2 + 3*P(2)**2*V(2)**2 +  &
         3*P(3)**2*V(3)**2 + &
         V(2)**2*P(1)**2 + V(1)**2*P(2)**2 +  &
         V(3)**2*P(1)**2 + V(1)**2*P(3)**2 + &
         V(3)**2*P(2)**2 + V(2)**2*P(3)**2 + &
         A*V(1)**2 + B*V(2)**2 + B*V(3)**2 + &
         4*V(1)*V(2)*P(1)*P(2) +  &
         4*V(1)*V(3)*P(1)*P(3) +  &
         4*V(2)*V(3)*P(2)*P(3)
    BB	= 2*BB
    CC	= P(1)**3*V(1) + P(2)**3*V(2) + P(3)**3*V(3) + &
         P(2)*P(1)**2*V(2) + P(1)*P(2)**2*V(1) + &
         P(3)*P(1)**2*V(3) + P(1)*P(3)**2*V(1) + &
         P(3)*P(2)**2*V(3) + P(2)*P(3)**2*V(2) + &
         A*V(1)*P(1) + B*V(2)*P(2) + B*V(3)*P(3)
    CC	= 4*CC
    DD	= P(1)**4 + P(2)**4 + P(3)**4 + &
         2*P(1)**2*P(2)**2 + 2*P(1)**2*P(3)**2 + &
         2*P(2)**2*P(3)**2 + &
         2*A*P(1)**2 + 2*B*P(2)**2 + 2*B*P(3)**2 + &
         A**2
    ! D	WRITE(6,*)' AA ',AA
    ! D	WRITE(6,*)' BB ',BB
    ! D	WRITE(6,*)' CC ',CC
    ! D	WRITE(6,*)' DD ',DD
    COEFF(1)	=  1.0D0
    COEFF(2)	=  AA
    COEFF(3)	=  BB
    COEFF(4)	=  CC
    COEFF(5)	=  DD
    
    ! srio danger
    !     	CALL 	ZRPOLY (COEFF,4,H_OUTPUT,IER)
    CALL ZRPOLY (COEFF,ifour,H_OUTPUT88,IER)
    IF (IER.NE.0) WRITE(6,*)'Watch out: error in ZRPOLY',IER
    DO 91 I = 1,4
       TEST1(I)	= DIMAG( H_OUTPUT(I) )
91  CONTINUE
    CHECK 	= TEST1(1)*TEST1(2)*TEST1(3)*TEST1(4)
    IF (CHECK.NE.0.0D0) THEN
       ! C all the solutions are complex; the beam is completely out of
       ! C of the mirror.
       I_RES	= -1
       RETURN
    ELSE
    END IF
    IF (I_RES.LT.0) THEN
       ! C
       ! C Ripple case : take the closest intercept.
       ! C
       ANSWER	= 1.0D+20
       DO 11 I = 1,4 
          IF (TEST1(I).EQ.0.0D0) THEN
             IF (ABS(DREAL(H_OUTPUT(I))).LT.ABS(ANSWER)) &
                  ANSWER = DREAL( H_OUTPUT(I) )
          END IF
11     CONTINUE
    ELSE
       ! C
       ! C Usual case. 
       ! C
       N_TEST	= 0
       DO 21 I = 1, 4
          TEMP=DREAL(H_OUTPUT(I))
          IF (TEST1(I).EQ.0.0D0) THEN
             ! C
             ! C In the facet calculation, we only consider the positive
             ! C intercepted length while in the Shadow we consider both the
             ! C positive and negative solutions.
             ! C the following arrangement can seperate the facet and
             ! C original calculations. 5/12/92 G.J.
             ! C
             IF (F_FACET.GT.0) THEN
                IF (TEMP.GE.0.0D0) THEN
                   N_TEST = N_TEST + 1
                   TEST2(N_TEST)        = TEMP
                END IF
             ELSE
                N_TEST = N_TEST + 1
                TEST2(N_TEST)	= TEMP
             END IF
          END IF
21     CONTINUE
          ! C
          ! C Sort the real intercept in ascending order.
          ! C
       DO 31 I = 1, N_TEST
          IMIN	= I
          AMIN	= TEST2(I)
          DO 41 J = I, N_TEST
             IF (TEST2(J).LT.AMIN) THEN
                AMIN	= TEST2(J)
                IMIN	= J
             END IF
41        CONTINUE
          XTEMP	= TEST2(I)
          TEST2(I)	= TEST2(IMIN)
          TEST2(IMIN)	= XTEMP
31     CONTINUE
! C
! C Pick the output according to F_TORUS.
! C
       IF (F_TORUS.EQ.0) THEN
          ANSWER	= TEST2(N_TEST)
       ELSE IF (F_TORUS.EQ.1) THEN
          IF (N_TEST.GT.1) THEN
             ANSWER	= TEST2(N_TEST-1)
          ELSE
             I_RES	= -1
             RETURN
          END IF
       ELSE IF (F_TORUS.EQ.2) THEN
          IF (N_TEST.GT.1) THEN
             ANSWER	= TEST2(2)
          ELSE
             I_RES	= -1
             RETURN
          END IF
       ELSE IF (F_TORUS.EQ.3) THEN
          ANSWER	= TEST2(1)
       END IF
    END IF
    
    !srio@esrf.eu 2011-10-12 fixed a bug with toroidal mirrors 
    !set at a distance zero from previous star.xx file
    ! WHY ANSWER SHOULD BE POSITIVE?
    ! if the distance from source to torus is zero, it can be negative
    i_res=1
    return
    !IF (ANSWER.GT.0.0D0.AND.ANSWER.LT.1.0D+20) THEN
    !   I_RES = 1
    !   RETURN
    !ELSE
    !   I_RES	= - 1
    !   RETURN
    !END IF
  End Subroutine quartic
  
  
  ! C+++
  ! C
  ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  ! C
  ! C	SUBROUTINE	SPOLY
  ! C
  ! C	PURPOSE		To compute the intercepts with a general
  ! C			polinomial surface of order up to 4. This
  ! C			include as a special case the torus and all
  ! C          		the conics. However, the calculations are more
  ! C			complex here and the simpler conic and torus
  ! C			case should be used whenever possible
  ! C
  ! C	ALGORITHM	Uses ZRPOLY. The coefficient of the distance
  ! C			of the source point to the surface are computed
  ! C			and passed to ZRPOLY.
  ! C
  ! C	INPUT		XIN:	ray origin 
  ! C			VIN:	ray direction
  ! C			I_RES:  -1, ripple case
  ! C				 1, ordinary case
  ! C
  ! C			NDEG is in common blk
  ! C
  ! C	OUTPUT		ANSWER  distance from point to intercept
  ! C			I_RES	flag: 0 successfull
  ! C				     -1 complex
  ! C
  ! C---
  
  Subroutine SPOLY (XIN, VIN, ANSWER, I_RES)
    
    IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
    IMPLICIT INTEGER(kind=ski)        (F,I-N)
    
    COMPLEX*16	H_OUTPUT(4)
    
    !srio danger  (to test)
    real(kind=skr),dimension(8)  :: h_output88
    equivalence (h_output88(1), H_OUTPUT(1))
    
    INTEGER(KIND=ski) N_TEST
    DIMENSION	TEST1(4),TEST2(4)
    DIMENSION	XIN(3), VIN(3)
    DIMENSION	COEFF(5),TCOEFF(5)
    ANSWER	=   0.0D0
    X0	= XIN(1)
    Y0	= XIN(2)
    Z0	= XIN(3)
    X1	= VIN(1)
    Y1	= VIN(2)
    Z1	= VIN(3) 
    ! C
    GO TO (1,2,3,4,5) NDEG+1
5   CONTINUE
    ! C
    ! C This is the  4th degree case
    ! C
    CTMP	=   0.0D0 
    CTMP	=   CTMP + PCOEFF(4,0,0) *X1**4
    CTMP	=   CTMP + PCOEFF(0,4,0) *Y1**4
    CTMP	=   CTMP + PCOEFF(0,0,4) *Z1**4
    CTMP	=   CTMP + PCOEFF(0,3,1) *Z1 *Y1**3 
    CTMP	=   CTMP + PCOEFF(1,0,3) *X1 *Z1**3 
    CTMP	=   CTMP + PCOEFF(1,3,0) *X1 *Y1**3 
    CTMP	=   CTMP + PCOEFF(3,0,1) *X1**3 *Z1 
    CTMP	=   CTMP + PCOEFF(3,1,0) *X1**3 *Y1 
    CTMP	=   CTMP + PCOEFF(0,1,3) *Y1 *Z1**3 
    CTMP	=   CTMP + PCOEFF(2,0,2) *X1**2 *Z1**2 
    CTMP	=   CTMP + PCOEFF(2,2,0) *X1**2 *Y1**2 
    CTMP	=   CTMP + PCOEFF(0,2,2) *Y1**2 *Z1**2 
    CTMP	=   CTMP + PCOEFF(1,1,2) *X1 *Y1 *Z1**2 
    CTMP	=   CTMP + PCOEFF(1,2,1) *X1 *Y1**2 *Z1 
    CTMP	=   CTMP + PCOEFF(2,1,1) *X1**2 *Y1 *Z1 
    ! C
    TCOEFF (5) = CTMP
4   CONTINUE
    ! C
    ! C 3th degree
    ! C
    CTMP	=   0.0D0
    CTMP	=   CTMP + 4* PCOEFF(4,0,0) *X0 *X1**3 
    CTMP	=   CTMP + 4* PCOEFF(0,4,0) *Y0 *Y1**3 
    CTMP	=   CTMP + 4* PCOEFF(0,0,4) *Z0 *Z1**3
    CTMP	=   CTMP +    PCOEFF(0,0,3) *Z1**3
    CTMP	=   CTMP +    PCOEFF(0,3,0) *Y1**3
    CTMP	=   CTMP +    PCOEFF(3,0,0) *X1**3
    CTMP	=   CTMP +    PCOEFF(0,1,2) *Y1 *Z1**2
    CTMP	=   CTMP +    PCOEFF(0,1,3) *Y0 *Z1**3
    CTMP	=   CTMP +    PCOEFF(0,2,1) *Y1**2 *Z1
    CTMP	=   CTMP +    PCOEFF(0,3,1) *Y1**3 *Z0
    CTMP	=   CTMP +    PCOEFF(1,0,2) *X1 *Z1**2
    CTMP	=   CTMP +    PCOEFF(1,0,3) *X0 *Z1**3
    CTMP	=   CTMP +    PCOEFF(1,2,0) *X1 *Y1**2
    CTMP	=   CTMP +    PCOEFF(1,3,0) *X0 *Y1**3
    CTMP	=   CTMP +    PCOEFF(2,0,1) *X1**2 *Z1
    CTMP	=   CTMP +    PCOEFF(2,1,0) *X1**2 *Y1 
    CTMP	=   CTMP +    PCOEFF(3,0,1) *X1**3 *Z0
    CTMP	=   CTMP +    PCOEFF(3,1,0) *X1**3 *Y0
    CTMP	=   CTMP + 3* PCOEFF(0,1,3) *Y1 *Z0 *Z1**2
    CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y1 *Y0 *Z1**2
    CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y1**2 *Z0 *Z1
    CTMP	=   CTMP + 3* PCOEFF(0,3,1) *Y0 *Y1**2 *Z1
    CTMP	=   CTMP + 3* PCOEFF(1,0,3) *X1 *Z0 *Z1**2
    CTMP	=   CTMP +    PCOEFF(1,1,1) *X1 *Y1 *Z1
    CTMP	=   CTMP +    PCOEFF(1,1,2) *X0 *Y1 *Z1**2
    CTMP	=   CTMP +    PCOEFF(1,1,2) *X1 *Y0 *Z1**2
    CTMP	=   CTMP +    PCOEFF(1,2,1) *X0 *Y1**2 *Z1
    CTMP	=   CTMP +    PCOEFF(1,2,1) *X1 *Y1**2 *Z0
    CTMP	=   CTMP + 3* PCOEFF(1,3,0) *X1 *Y0 *Y1**2
    CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X0 *X1 *Z1**2
    CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X1**2 *Z0 *Z1
    CTMP	=   CTMP +    PCOEFF(2,1,1) *X1**2 *Y0 *Z1
    CTMP	=   CTMP +    PCOEFF(2,1,1) *X1**2 *Y1 *Z0
    CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X0 *X1 *Y1**2
    CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X1**2 *Y0 *Y1
    CTMP	=   CTMP + 3* PCOEFF(3,0,1) *X0 *X1**2 *Z1
    CTMP	=   CTMP + 3* PCOEFF(3,1,0) *X0 *X1**2 *Y1
    CTMP	=   CTMP + 2* PCOEFF(1,1,2) *X1 *Y1 *Z0 *Z1
    CTMP	=   CTMP + 2* PCOEFF(1,2,1) *X1 *Y0 *Y1 *Z1
    CTMP	=   CTMP + 2* PCOEFF(2,1,1) *X0 *X1 *Y1 *Z1
    ! C     	
    TCOEFF (4) = CTMP
3   CONTINUE
    ! C
    ! C 2nd degree
    ! C
    CTMP	=   0.0D0
    CTMP	=   CTMP + 6*PCOEFF(4,0,0) *X0**2 *X1**2 
    CTMP	=   CTMP + 6*PCOEFF(0,4,0) *Y0**2 *Y1**2 
    CTMP	=   CTMP + 6*PCOEFF(0,0,4) *Z0**2 *Z1**2
    CTMP	=   CTMP +   PCOEFF(0,0,2) *Z1**2
    CTMP	=   CTMP +   PCOEFF(0,2,0) *Y1**2
    CTMP	=   CTMP +   PCOEFF(2,0,0) *X1 *X1
    CTMP	=   CTMP +   PCOEFF(0,0,3) *3*Z1**2*Z0
    CTMP	=   CTMP +   PCOEFF(0,1,1) *Y1 *Z1
    CTMP	=   CTMP +   PCOEFF(0,1,2) *Y0 *Z1**2
    CTMP	=   CTMP +   PCOEFF(0,2,1) *Y1**2 *Z0
    CTMP	=   CTMP +   PCOEFF(0,2,2) *Y0**2 *Z1**2
    CTMP	=   CTMP +   PCOEFF(0,2,2) *Y1**2 *Z0**2
    CTMP	=   CTMP +   PCOEFF(0,3,0) *3*Y0 *Y1**2
    CTMP	=   CTMP +   PCOEFF(1,0,1) *X1 *Z1
    CTMP	=   CTMP +   PCOEFF(1,0,2) *X0 *Z1**2
    CTMP	=   CTMP +   PCOEFF(1,1,0) *X1 *Y1
    CTMP	=   CTMP +   PCOEFF(1,2,0) *X0 *Y1**2
    CTMP	=   CTMP +   PCOEFF(2,0,1) *X1**2 *Z0
    CTMP	=   CTMP +   PCOEFF(2,0,2) *X0**2 *Z1**2
    CTMP	=   CTMP +   PCOEFF(2,0,2) *X1**2 *Z0**2
    CTMP	=   CTMP +   PCOEFF(2,1,0) *X1**2 *Y0
    CTMP	=   CTMP +   PCOEFF(2,2,0) *X0**2 *Y1**2
    CTMP	=   CTMP +   PCOEFF(2,2,0) *X1**2 *Y0**2
    CTMP	=   CTMP +   PCOEFF(3,0,0) *3*X0 *X1**2
    CTMP	=   CTMP +   PCOEFF(0,1,2) *2*Y1 *Z0 *Z1
    CTMP	=   CTMP +   PCOEFF(0,1,3) *3*Y0 *Z0 *Z1**2
    CTMP	=   CTMP +   PCOEFF(0,1,3) *3*Y1 *Z0**2 *Z1
    CTMP	=   CTMP +   PCOEFF(0,2,1) *2*Y0 *Y1 *Z1
    CTMP	=   CTMP +   PCOEFF(0,3,1) *3*Y0 *Y1**2 *Z0
    CTMP	=   CTMP +   PCOEFF(0,3,1) *3*Y0**2 *Y1 *Z1
    CTMP	=   CTMP +   PCOEFF(1,0,2) *2*X1 *Z0 *Z1
    CTMP	=   CTMP +   PCOEFF(1,0,3) *3*X0 *Z0 *Z1**2
    CTMP	=   CTMP +   PCOEFF(1,0,3) *3*X1 *Z0**2 *Z1
    CTMP	=   CTMP +   PCOEFF(1,1,1) *X0 *Y1 *Z1
    CTMP	=   CTMP +   PCOEFF(1,1,1) *X1 *Y0 *Z1
    CTMP	=   CTMP +   PCOEFF(1,1,1) *X1 *Y1 *Z0
    CTMP	=   CTMP +   PCOEFF(1,1,2) *X0 *Y0 *Z1**2
    CTMP	=   CTMP +   PCOEFF(1,1,2) *X1 *Y1 *Z0**2
    CTMP	=   CTMP +   PCOEFF(1,2,0) *2*X1 *Y0 *Y1
    CTMP	=   CTMP +   PCOEFF(1,2,1) *X0 *Y1**2 *Z0
    CTMP	=   CTMP +   PCOEFF(1,2,1) *X1 *Y0**2 *Y1
    CTMP	=   CTMP +   PCOEFF(1,3,0) *3*X0 *Y0 *Y1**2
    CTMP	=   CTMP +   PCOEFF(1,3,0) *3*X1 *Y0**2 *Y1
    CTMP	=   CTMP +   PCOEFF(2,0,1) *2*X0 *X1 *Z1
    CTMP	=   CTMP +   PCOEFF(2,1,0) *2*X0 *X1 *Y1
    CTMP	=   CTMP +   PCOEFF(2,1,1) *X0**2 *Y1 *Z1
    CTMP	=   CTMP +   PCOEFF(2,1,1) *X1**2 *Y0 *Z0
    CTMP	=   CTMP +   PCOEFF(3,0,1) *3*X0 *X1**2 *Z0
    CTMP	=   CTMP +   PCOEFF(3,0,1) *3*X0**2 *X1 *Z1
    CTMP	=   CTMP +   PCOEFF(3,1,0) *3*X0 *X1**2 *Y0
    CTMP	=   CTMP +   PCOEFF(3,1,0) *3*X0**2 *X1 *Y1
    CTMP	=   CTMP +   PCOEFF(0,2,2) *4*Y0 *Y1 *Z0 *Z1
    CTMP	=   CTMP +   PCOEFF(1,1,2) *2*X0 *Y1 *Z0 *Z1
    CTMP	=   CTMP +   PCOEFF(1,1,2) *2*X1 *Y0 *Z0 *Z1
    CTMP	=   CTMP +   PCOEFF(1,2,1) *2*X0 *Y0 *Y1 *Z1
    CTMP	=   CTMP +   PCOEFF(1,2,1) *2*X1 *Y0 *Y1 *Z0
    CTMP	=   CTMP +   PCOEFF(2,0,2) *4*X0 *X1 *Z0 *Z1
    CTMP	=   CTMP +   PCOEFF(2,1,1) *2*X0 *X1 *Y0 *Z1
    CTMP	=   CTMP +   PCOEFF(2,1,1) *2*X0 *X1 *Y1 *Z0
    CTMP	=   CTMP +   PCOEFF(2,2,0) *4*X0 *X1 *Y0 *Y1
    ! C
    TCOEFF (3) = CTMP
2   CONTINUE
    ! C
    ! C 1st degree
    ! C
    CTMP	=   0.0D0
    CTMP	=   CTMP + 4* PCOEFF(4,0,0) *X0**3 *X1 
    CTMP	=   CTMP + 4* PCOEFF(0,4,0) *Y0**3 *Y1
    CTMP	=   CTMP + 4* PCOEFF(0,0,4) *Z0**3 *Z1 
    CTMP	=   CTMP +    PCOEFF(0,0,1) *Z1
    CTMP	=   CTMP +    PCOEFF(0,1,0) *Y1
    CTMP	=   CTMP +    PCOEFF(1,0,0) *X1
    CTMP	=   CTMP + 2* PCOEFF(0,0,2) *Z0 *Z1
    CTMP	=   CTMP + 3* PCOEFF(0,0,3) *Z0**2 *Z1
    CTMP	=   CTMP +    PCOEFF(0,1,1) *Y0 *Z1
    CTMP	=   CTMP +    PCOEFF(0,1,1) *Y1 *Z0
    CTMP	=   CTMP +    PCOEFF(0,1,2) *Y1 *Z0**2
    CTMP	=   CTMP +    PCOEFF(0,1,3) *Y0 *Z0**3
    CTMP	=   CTMP + 2* PCOEFF(0,2,0) *Y0 *Y1
    CTMP	=   CTMP +    PCOEFF(0,2,1) *Y0**2 *Z1
    CTMP	=   CTMP + 3* PCOEFF(0,3,0) *Y0**2 *Y1
    CTMP	=   CTMP +    PCOEFF(0,3,1) *Y0**3 *Z1
    CTMP	=   CTMP +    PCOEFF(1,0,1) *X0 *Z1
    CTMP	=   CTMP +    PCOEFF(1,0,1) *X1 *Z0
    CTMP	=   CTMP +    PCOEFF(1,0,2) *X1 *Z0**2
    CTMP	=   CTMP +    PCOEFF(1,0,3) *X1 *Z0**3
    CTMP	=   CTMP +    PCOEFF(1,1,0) *X0 *Y1
    CTMP	=   CTMP +    PCOEFF(1,1,0) *X1 *Y0
    CTMP	=   CTMP +    PCOEFF(1,2,0) *X1 *Y0**2
    CTMP	=   CTMP +    PCOEFF(1,3,0) *X1 *Y0**3
    CTMP	=   CTMP + 2* PCOEFF(2,0,0) *X0 *X1
    CTMP	=   CTMP +    PCOEFF(2,0,1) *X0**2 *Z1
    CTMP	=   CTMP +    PCOEFF(2,1,0) *X0**2 *Y1
    CTMP	=   CTMP + 3* PCOEFF(3,0,0) *X0**2 *X1
    CTMP	=   CTMP +    PCOEFF(3,0,1) *X0**3 *Z1
    CTMP	=   CTMP +    PCOEFF(3,1,0) *X0**3 *Y1
    CTMP	=   CTMP + 2* PCOEFF(0,1,2) *Y0 *Z0 *Z1
    CTMP	=   CTMP + 3* PCOEFF(0,1,3) *Y0 *Z0**2 *Z1
    CTMP	=   CTMP + 2* PCOEFF(0,2,1) *Y0 *Y1 *Z0
    CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y0 *Y1 *Z0**2
    CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y0**2 *Z0 *Z1
    CTMP	=   CTMP + 3* PCOEFF(0,3,1) *Y0**2 *Y1 *Z0
    CTMP	=   CTMP + 2* PCOEFF(1,0,2) *X0 *Z0 *Z1
    CTMP	=   CTMP + 3* PCOEFF(1,0,3) *X0 *Z0**2 *Z1
    CTMP	=   CTMP +    PCOEFF(1,1,1) *X0 *Y0 *Z1
    CTMP	=   CTMP +    PCOEFF(1,1,1) *X0 *Y1 *Z0
    CTMP	=   CTMP +    PCOEFF(1,1,1) *X1 *Y0 *Z0
    CTMP	=   CTMP +    PCOEFF(1,1,2) *X0 *Y1 *Z0**2
    CTMP	=   CTMP +    PCOEFF(1,1,2) *X1 *Y0 *Z0**2
    CTMP	=   CTMP + 2* PCOEFF(1,2,0) *X0 *Y0 *Y1
    CTMP	=   CTMP +    PCOEFF(1,2,1) *X0 *Y0**2 *Z1
    CTMP	=   CTMP +    PCOEFF(1,2,1) *X1 *Y0**2 *Z0
    CTMP	=   CTMP + 3* PCOEFF(1,3,0) *X0 *Y0**2 *Y1
    CTMP	=   CTMP + 2* PCOEFF(2,0,1) *X0 *X1 *Z0
    CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X0 *X1 *Z0**2
    CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X0**2 *Z0 *Z1
    CTMP	=   CTMP + 2* PCOEFF(2,1,0) *X0 *X1 *Y0
    CTMP	=   CTMP +    PCOEFF(2,1,1) *X0**2 *Y0 *Z1
    CTMP	=   CTMP +    PCOEFF(2,1,1) *X0**2 *Y1 *Z0
    CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X0 *X1 *Y0**2
    CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X0**2 *Y0 *Y1
    CTMP	=   CTMP + 3* PCOEFF(3,0,1) *X0**2 *X1 *Z0
    CTMP	=   CTMP + 3* PCOEFF(3,1,0) *X0**2 *X1 *Y0
    CTMP	=   CTMP + 2* PCOEFF(1,1,2) *X0 *Y0 *Z0 *Z1
    CTMP	=   CTMP + 2* PCOEFF(1,2,1) *X0 *Y0 *Y1 *Z0
    CTMP	=   CTMP + 2* PCOEFF(2,1,1) *X0 *X1 *Y0 *Z0
    ! C
    TCOEFF  (2) = CTMP
1   CONTINUE
    ! C
    ! C 0th degree
    ! C
    CTMP	=   0.0D0
    CTMP	=   CTMP +   PCOEFF(0,0,0) 
    CTMP	=   CTMP +   PCOEFF(4,0,0) *X0**4 
    CTMP	=   CTMP +   PCOEFF(0,4,0) *Y0**4 
    CTMP	=   CTMP +   PCOEFF(0,0,4) *Z0**4
    CTMP	=   CTMP +   PCOEFF(0,0,1) *Z0
    CTMP	=   CTMP +   PCOEFF(0,0,2) *Z0**2
    CTMP	=   CTMP +   PCOEFF(0,0,3) *Z0**3
    CTMP	=   CTMP +   PCOEFF(0,1,0) *Y0
    CTMP	=   CTMP +   PCOEFF(0,2,0) *Y0**2
    CTMP	=   CTMP +   PCOEFF(0,3,0) *Y0**3
    CTMP	=   CTMP +   PCOEFF(1,0,0) *X0
    CTMP	=   CTMP +   PCOEFF(2,0,0) *X0**2
    CTMP	=   CTMP +   PCOEFF(3,0,0) *X0**3
    CTMP	=   CTMP +   PCOEFF(0,1,1) *Y0 *Z0
    CTMP	=   CTMP +   PCOEFF(0,1,2) *Y0 *Z0**2
    CTMP	=   CTMP +   PCOEFF(0,1,3) *Y0 *Z0**3
    CTMP	=   CTMP +   PCOEFF(0,2,1) *Y0**2 *Z0
    CTMP	=   CTMP +   PCOEFF(0,2,2) *Y0**2 *Z0**2
    CTMP	=   CTMP +   PCOEFF(0,3,1) *Y0**3 *Z0
    CTMP	=   CTMP +   PCOEFF(1,0,1) *X0 *Z0
    CTMP	=   CTMP +   PCOEFF(1,0,2) *X0 *Z0**2
    CTMP	=   CTMP +   PCOEFF(1,0,3) *X0 *Z0**3
    CTMP	=   CTMP +   PCOEFF(1,1,0) *X0 *Y0
    CTMP	=   CTMP +   PCOEFF(1,2,0) *X0 *Y0**2
    CTMP	=   CTMP +   PCOEFF(1,3,0) *X0 *Y0**3
    CTMP	=   CTMP +   PCOEFF(2,0,1) *X0**2 *Z0
    CTMP	=   CTMP +   PCOEFF(2,0,2) *X0**2 *Z0**2
    CTMP	=   CTMP +   PCOEFF(2,1,0) *X0**2 *Y0
    CTMP	=   CTMP +   PCOEFF(2,2,0) *X0**2 *Y0**2
    CTMP	=   CTMP +   PCOEFF(3,0,1) *X0**3 *Z0
    CTMP	=   CTMP +   PCOEFF(3,1,0) *X0**3 *Y0
    CTMP	=   CTMP +   PCOEFF(1,1,1) *X0 *Y0 *Z0
    CTMP	=   CTMP +   PCOEFF(1,1,2) *X0 *Y0 *Z0**2
    CTMP	=   CTMP +   PCOEFF(1,2,1) *X0 *Y0**2 *Z0
    CTMP	=   CTMP +   PCOEFF(2,1,1) *X0**2 *Y0 *Z0
    ! C
    TCOEFF (1) = CTMP
    ! D	TYPE *,'TCOEFF = ',TCOEFF
    ! C
    ! C Inverts array for ZRPOLY
    ! C
    DO I=NDEG+1,1,-1
       COEFF(NDEG-I+2) = TCOEFF(I)
    END DO
    ! D	TYPE *,COEFF
    !srio danger
    !     	CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT,IER)
    CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT88,IER)
    ! C
    ! C Tests for success;
    ! C
    ! D     	TYPE *,IER
    IF (IER.EQ.130) THEN
       ! C
       ! C if IER=130 degree declared too large; try again;
       ! C
       MOVE = 0
       DO WHILE (IER.NE.0)
          MOVE = MOVE + 1
     	  IF (MOVE.EQ.4) CALL LEAVE ('POLY','ERROR IN POLY',izero)
        DO I=1,NDEG
           TCOEFF(I) = COEFF(I+1)
        END DO
        DO I=1,NDEG
           COEFF(I)=TCOEFF(I)
        END DO
        ! D	TYPE *,'MOVE = ',MOVE,COEFF
        !srio danger
        !     	   CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT,IER)
        CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT88,IER)
        ! D	TYPE *,IER
        END DO
     ELSE IF (IER.EQ.131) THEN
        ! C
        ! C fatal error
        ! C
        CALL LEAVE ('POLY','Fatal error.',izero)
     END IF
     ! C
     ! C Tests for reality of intercepts 
     ! C
     CHECK = 1
     ! D	TYPE *,H_OUTPUT
     DO I=1,NDEG
        TEST1(I)	=DIMAG(H_OUTPUT(I))
        CHECK	= CHECK*TEST1(I)	! Complex part
     END DO
     IF (CHECK.NE.0.0D0) THEN
        ! C
        ! C All the solutions are complex; the beam is completely out of
        ! C of the mirror.
        ! C
        I_RES	= -1
        RETURN
     END IF
     ! C
     ! C At least a good ray;
     ! C
     IF (I_RES.LT.0) THEN
        ! C
        ! C Ripple case : take the closest intercept
        ! C
        ANSWER	= 1.0D30
        DO I=1,NDEG
           TEST = ABS (DIMAG(H_OUTPUT(I)))
           IF (TEST.LT.1.0E-14) THEN
              IF (ABS(DREAL(H_OUTPUT(I))).LT.ABS(ANSWER)) &
                   ANSWER = DREAL ( H_OUTPUT(I) )
           END IF
        END DO
     ELSE
        ! C
        ! C Ordinary case : looks for the maximum of the real values; this will set the
        ! C intercept at the fartest sheet of the surface
        ! C
        ANSWER	= -1.0D30
        DO I=1,NDEG
           TEST = ABS (DIMAG(H_OUTPUT(I)))
           IF (TEST.LT.1.0E-14) THEN
              ANSWER = DMAX1( ANSWER,DREAL ( H_OUTPUT(I) ))
           END IF
        END DO
     END IF
     
     ! C
     ! C Seperate the F_POLSEL case with the usual Shadow treatment
     ! C
     
     IF (F_POLSEL.NE.0) THEN
        N_TEST	= 0
        DO 299 I = 1, NDEG
           TEMP=DREAL(H_OUTPUT(I))
           IF (TEST1(I).EQ.0.0D0.AND.TEMP.GE.0.0D0) THEN
              IF (F_KOMA.NE.1) THEN
                 N_TEST		= N_TEST + 1
                 TEST2(N_TEST)	= TEMP
              ELSE
                 IF (TEMP.GE.1.0D-10) THEN
                    N_TEST		= N_TEST + 1
                    TEST2(N_TEST)	= TEMP
                 END IF
              END IF
           END IF
299     CONTINUE
           ! C
           ! C Sort the real intercept in ascending order.
           ! C
        DO I = 1, N_TEST
           IMIN	= I
           AMIN	= TEST2(I)
           DO J = I, N_TEST
              IF (TEST2(J).LT.AMIN) THEN
                 AMIN	= TEST2(J)
                 IMIN	= J
              END IF
           END DO
           XTEMP	= TEST2(I)
           TEST2(I)	= TEST2(IMIN)
           TEST2(IMIN)	= XTEMP
        END DO
        ! C
        ! C Pick the output according to F_TORUS.
        ! C
        IF (F_POLSEL.EQ.4) THEN
           ANSWER	= TEST2(N_TEST)
        ELSE IF (F_POLSEL.EQ.1) THEN
           IF (N_TEST.GT.1) THEN
              ANSWER	= TEST2(N_TEST-1)
           ELSE
              I_RES	= -1
              RETURN
           END IF
        ELSE IF (F_POLSEL.EQ.2) THEN
           IF (N_TEST.GT.1) THEN
              ANSWER	= TEST2(2)
           ELSE
              I_RES	= -1
              RETURN
           END IF
        ELSE IF (F_POLSEL.EQ.3) THEN
           ANSWER	= TEST2(1)
        END IF
     END IF
     
     IF (ANSWER.GT.0.0D0.AND.ANSWER.LT.1.0D+20) THEN
        I_RES = 1
        RETURN
     ELSE
        I_RES	= - 1
        RETURN
     END IF
   End Subroutine spoly
   
   
! C+++
! C
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! C
! C	SUBROUTINE	POLY
! C
! C	PURPOSE		To compute the intercepts with a general
! C			polinomial surface of order up to 4. This
! C			include as a special case the torus and all
! C          		the conics. However, the calculations are more
! C			complex here and the simpler conic and torus
! C			case should be used whenever possible
! C
! C	ALGORITHM	Uses ZRPOLY. The coefficient of the distance
! C			of the source point to the surface are computed
! C			and passed to ZRPOLY.
! C
! C	INPUT		XIN:	ray origin 
! C			VIN:	ray direction
! C			I_RES:  -1, ripple case
! C				 1, ordinary case
! C
! C			NDEG is in common blk
! C
! C	OUTPUT		ANSWER  distance from point to intercept
! C			I_RES	flag: 0 successfull
! C				     -1 complex
! C
! C---


!todo srio: is this the same as spoly?

   SUBROUTINE POLY (XIN, VIN, ANSWER, I_RES)
     
     
     IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
     IMPLICIT INTEGER(kind=ski)        (F,I-N)
     
     COMPLEX*16	H_OUTPUT(4)
     
     !srio danger  (to test)
     real(kind=skr),dimension(8)  :: h_output88
     equivalence (h_output88(1), H_OUTPUT(1))
     
     INTEGER(KIND=ski)		N_TEST
     DIMENSION	TEST1(4),TEST2(4)
     DIMENSION	XIN(3), VIN(3)
     DIMENSION	COEFF(5),TCOEFF(5)
     ANSWER	=   0.0D0
     X0	= XIN(1)
     Y0	= XIN(2)
     Z0	= XIN(3)
     X1	= VIN(1)
     Y1	= VIN(2)
     Z1	= VIN(3) 
     ! C
     GO TO (1,2,3,4,5) NDEG+1
5    CONTINUE
     ! C
     ! C This is the 4th degree case
     ! C
     CTMP	=   0.0D0 
     CTMP	=   CTMP + PCOEFF(4,0,0) *X1**4
     CTMP	=   CTMP + PCOEFF(0,4,0) *Y1**4
     CTMP	=   CTMP + PCOEFF(0,0,4) *Z1**4
     CTMP	=   CTMP + PCOEFF(0,3,1) *Z1 *Y1**3 
     CTMP	=   CTMP + PCOEFF(1,0,3) *X1 *Z1**3 
     CTMP	=   CTMP + PCOEFF(1,3,0) *X1 *Y1**3 
     CTMP	=   CTMP + PCOEFF(3,0,1) *X1**3 *Z1 
     CTMP	=   CTMP + PCOEFF(3,1,0) *X1**3 *Y1 
     CTMP	=   CTMP + PCOEFF(0,1,3) *Y1 *Z1**3 
     CTMP	=   CTMP + PCOEFF(2,0,2) *X1**2 *Z1**2 
     CTMP	=   CTMP + PCOEFF(2,2,0) *X1**2 *Y1**2 
     CTMP	=   CTMP + PCOEFF(0,2,2) *Y1**2 *Z1**2 
     CTMP	=   CTMP + PCOEFF(1,1,2) *X1 *Y1 *Z1**2 
     CTMP	=   CTMP + PCOEFF(1,2,1) *X1 *Y1**2 *Z1 
     CTMP	=   CTMP + PCOEFF(2,1,1) *X1**2 *Y1 *Z1 
     ! C
     TCOEFF (5) = CTMP
4    CONTINUE
     ! C
     ! C 3th degree
     ! C
     CTMP	=   0.0D0
     CTMP	=   CTMP + 4* PCOEFF(4,0,0) *X0 *X1**3 
     CTMP	=   CTMP + 4* PCOEFF(0,4,0) *Y0 *Y1**3 
     CTMP	=   CTMP + 4* PCOEFF(0,0,4) *Z0 *Z1**3
     CTMP	=   CTMP +    PCOEFF(0,0,3) *Z1**3
     CTMP	=   CTMP +    PCOEFF(0,3,0) *Y1**3
     CTMP	=   CTMP +    PCOEFF(3,0,0) *X1**3
     CTMP	=   CTMP +    PCOEFF(0,1,2) *Y1 *Z1**2
     CTMP	=   CTMP +    PCOEFF(0,1,3) *Y0 *Z1**3
     CTMP	=   CTMP +    PCOEFF(0,2,1) *Y1**2 *Z1
     CTMP	=   CTMP +    PCOEFF(0,3,1) *Y1**3 *Z0
     CTMP	=   CTMP +    PCOEFF(1,0,2) *X1 *Z1**2
     CTMP	=   CTMP +    PCOEFF(1,0,3) *X0 *Z1**3
     CTMP	=   CTMP +    PCOEFF(1,2,0) *X1 *Y1**2
     CTMP	=   CTMP +    PCOEFF(1,3,0) *X0 *Y1**3
     CTMP	=   CTMP +    PCOEFF(2,0,1) *X1**2 *Z1
     CTMP	=   CTMP +    PCOEFF(2,1,0) *X1**2 *Y1 
     CTMP	=   CTMP +    PCOEFF(3,0,1) *X1**3 *Z0
     CTMP	=   CTMP +    PCOEFF(3,1,0) *X1**3 *Y0
     CTMP	=   CTMP + 3* PCOEFF(0,1,3) *Y1 *Z0 *Z1**2
     CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y1 *Y0 *Z1**2
     CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y1**2 *Z0 *Z1
     CTMP	=   CTMP + 3* PCOEFF(0,3,1) *Y0 *Y1**2 *Z1
     CTMP	=   CTMP + 3* PCOEFF(1,0,3) *X1 *Z0 *Z1**2
     CTMP	=   CTMP +    PCOEFF(1,1,1) *X1 *Y1 *Z1
     CTMP	=   CTMP +    PCOEFF(1,1,2) *X0 *Y1 *Z1**2
     CTMP	=   CTMP +    PCOEFF(1,1,2) *X1 *Y0 *Z1**2
     CTMP	=   CTMP +    PCOEFF(1,2,1) *X0 *Y1**2 *Z1
     CTMP	=   CTMP +    PCOEFF(1,2,1) *X1 *Y1**2 *Z0
     CTMP	=   CTMP + 3* PCOEFF(1,3,0) *X1 *Y0 *Y1**2
     CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X0 *X1 *Z1**2
     CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X1**2 *Z0 *Z1
     CTMP	=   CTMP +    PCOEFF(2,1,1) *X1**2 *Y0 *Z1
     CTMP	=   CTMP +    PCOEFF(2,1,1) *X1**2 *Y1 *Z0
     CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X0 *X1 *Y1**2
     CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X1**2 *Y0 *Y1
     CTMP	=   CTMP + 3* PCOEFF(3,0,1) *X0 *X1**2 *Z1
     CTMP	=   CTMP + 3* PCOEFF(3,1,0) *X0 *X1**2 *Y1
     CTMP	=   CTMP + 2* PCOEFF(1,1,2) *X1 *Y1 *Z0 *Z1
     CTMP	=   CTMP + 2* PCOEFF(1,2,1) *X1 *Y0 *Y1 *Z1
     CTMP	=   CTMP + 2* PCOEFF(2,1,1) *X0 *X1 *Y1 *Z1
     ! C     	
     TCOEFF (4) = CTMP
3    CONTINUE
     ! C
     ! C 2nd degree
     ! C
     CTMP	=   0.0D0
     CTMP	=   CTMP + 6*PCOEFF(4,0,0) *X0**2 *X1**2 
     CTMP	=   CTMP + 6*PCOEFF(0,4,0) *Y0**2 *Y1**2 
     CTMP	=   CTMP + 6*PCOEFF(0,0,4) *Z0**2 *Z1**2
     CTMP	=   CTMP +   PCOEFF(0,0,2) *Z1**2
     CTMP	=   CTMP +   PCOEFF(0,2,0) *Y1**2
     CTMP	=   CTMP +   PCOEFF(2,0,0) *X1 *X1
     CTMP	=   CTMP +   PCOEFF(0,0,3) *3*Z1**2*Z0
     CTMP	=   CTMP +   PCOEFF(0,1,1) *Y1 *Z1
     CTMP	=   CTMP +   PCOEFF(0,1,2) *Y0 *Z1**2
     CTMP	=   CTMP +   PCOEFF(0,2,1) *Y1**2 *Z0
     CTMP	=   CTMP +   PCOEFF(0,2,2) *Y0**2 *Z1**2
     CTMP	=   CTMP +   PCOEFF(0,2,2) *Y1**2 *Z0**2
     CTMP	=   CTMP +   PCOEFF(0,3,0) *3*Y0 *Y1**2
     CTMP	=   CTMP +   PCOEFF(1,0,1) *X1 *Z1
     CTMP	=   CTMP +   PCOEFF(1,0,2) *X0 *Z1**2
     CTMP	=   CTMP +   PCOEFF(1,1,0) *X1 *Y1
     CTMP	=   CTMP +   PCOEFF(1,2,0) *X0 *Y1**2
     CTMP	=   CTMP +   PCOEFF(2,0,1) *X1**2 *Z0
     CTMP	=   CTMP +   PCOEFF(2,0,2) *X0**2 *Z1**2
     CTMP	=   CTMP +   PCOEFF(2,0,2) *X1**2 *Z0**2
     CTMP	=   CTMP +   PCOEFF(2,1,0) *X1**2 *Y0
     CTMP	=   CTMP +   PCOEFF(2,2,0) *X0**2 *Y1**2
     CTMP	=   CTMP +   PCOEFF(2,2,0) *X1**2 *Y0**2
     CTMP	=   CTMP +   PCOEFF(3,0,0) *3*X0 *X1**2
     CTMP	=   CTMP +   PCOEFF(0,1,2) *2*Y1 *Z0 *Z1
     CTMP	=   CTMP +   PCOEFF(0,1,3) *3*Y0 *Z0 *Z1**2
     CTMP	=   CTMP +   PCOEFF(0,1,3) *3*Y1 *Z0**2 *Z1
     CTMP	=   CTMP +   PCOEFF(0,2,1) *2*Y0 *Y1 *Z1
     CTMP	=   CTMP +   PCOEFF(0,3,1) *3*Y0 *Y1**2 *Z0
     CTMP	=   CTMP +   PCOEFF(0,3,1) *3*Y0**2 *Y1 *Z1
     CTMP	=   CTMP +   PCOEFF(1,0,2) *2*X1 *Z0 *Z1
     CTMP	=   CTMP +   PCOEFF(1,0,3) *3*X0 *Z0 *Z1**2
     CTMP	=   CTMP +   PCOEFF(1,0,3) *3*X1 *Z0**2 *Z1
     CTMP	=   CTMP +   PCOEFF(1,1,1) *X0 *Y1 *Z1
     CTMP	=   CTMP +   PCOEFF(1,1,1) *X1 *Y0 *Z1
     CTMP	=   CTMP +   PCOEFF(1,1,1) *X1 *Y1 *Z0
     CTMP	=   CTMP +   PCOEFF(1,1,2) *X0 *Y0 *Z1**2
     CTMP	=   CTMP +   PCOEFF(1,1,2) *X1 *Y1 *Z0**2
     CTMP	=   CTMP +   PCOEFF(1,2,0) *2*X1 *Y0 *Y1
     CTMP	=   CTMP +   PCOEFF(1,2,1) *X0 *Y1**2 *Z0
     CTMP	=   CTMP +   PCOEFF(1,2,1) *X1 *Y0**2 *Y1
     CTMP	=   CTMP +   PCOEFF(1,3,0) *3*X0 *Y0 *Y1**2
     CTMP	=   CTMP +   PCOEFF(1,3,0) *3*X1 *Y0**2 *Y1
     CTMP	=   CTMP +   PCOEFF(2,0,1) *2*X0 *X1 *Z1
     CTMP	=   CTMP +   PCOEFF(2,1,0) *2*X0 *X1 *Y1
     CTMP	=   CTMP +   PCOEFF(2,1,1) *X0**2 *Y1 *Z1
     CTMP	=   CTMP +   PCOEFF(2,1,1) *X1**2 *Y0 *Z0
     CTMP	=   CTMP +   PCOEFF(3,0,1) *3*X0 *X1**2 *Z0
     CTMP	=   CTMP +   PCOEFF(3,0,1) *3*X0**2 *X1 *Z1
     CTMP	=   CTMP +   PCOEFF(3,1,0) *3*X0 *X1**2 *Y0
     CTMP	=   CTMP +   PCOEFF(3,1,0) *3*X0**2 *X1 *Y1
     CTMP	=   CTMP +   PCOEFF(0,2,2) *4*Y0 *Y1 *Z0 *Z1
     CTMP	=   CTMP +   PCOEFF(1,1,2) *2*X0 *Y1 *Z0 *Z1
     CTMP	=   CTMP +   PCOEFF(1,1,2) *2*X1 *Y0 *Z0 *Z1
     CTMP	=   CTMP +   PCOEFF(1,2,1) *2*X0 *Y0 *Y1 *Z1
     CTMP	=   CTMP +   PCOEFF(1,2,1) *2*X1 *Y0 *Y1 *Z0
     CTMP	=   CTMP +   PCOEFF(2,0,2) *4*X0 *X1 *Z0 *Z1
     CTMP	=   CTMP +   PCOEFF(2,1,1) *2*X0 *X1 *Y0 *Z1
     CTMP	=   CTMP +   PCOEFF(2,1,1) *2*X0 *X1 *Y1 *Z0
     CTMP	=   CTMP +   PCOEFF(2,2,0) *4*X0 *X1 *Y0 *Y1
     ! C
     TCOEFF (3) = CTMP
2    CONTINUE
     ! C
     ! C 1st degree
     ! C
     CTMP	=   0.0D0
     CTMP	=   CTMP + 4* PCOEFF(4,0,0) *X0**3 *X1 
     CTMP	=   CTMP + 4* PCOEFF(0,4,0) *Y0**3 *Y1
     CTMP	=   CTMP + 4* PCOEFF(0,0,4) *Z0**3 *Z1 
     CTMP	=   CTMP +    PCOEFF(0,0,1) *Z1
     CTMP	=   CTMP +    PCOEFF(0,1,0) *Y1
     CTMP	=   CTMP +    PCOEFF(1,0,0) *X1
     CTMP	=   CTMP + 2* PCOEFF(0,0,2) *Z0 *Z1
     CTMP	=   CTMP + 3* PCOEFF(0,0,3) *Z0**2 *Z1
     CTMP	=   CTMP +    PCOEFF(0,1,1) *Y0 *Z1
     CTMP	=   CTMP +    PCOEFF(0,1,1) *Y1 *Z0
     CTMP	=   CTMP +    PCOEFF(0,1,2) *Y1 *Z0**2
     CTMP	=   CTMP +    PCOEFF(0,1,3) *Y0 *Z0**3
     CTMP	=   CTMP + 2* PCOEFF(0,2,0) *Y0 *Y1
     CTMP	=   CTMP +    PCOEFF(0,2,1) *Y0**2 *Z1
     CTMP	=   CTMP + 3* PCOEFF(0,3,0) *Y0**2 *Y1
     CTMP	=   CTMP +    PCOEFF(0,3,1) *Y0**3 *Z1
     CTMP	=   CTMP +    PCOEFF(1,0,1) *X0 *Z1
     CTMP	=   CTMP +    PCOEFF(1,0,1) *X1 *Z0
     CTMP	=   CTMP +    PCOEFF(1,0,2) *X1 *Z0**2
     CTMP	=   CTMP +    PCOEFF(1,0,3) *X1 *Z0**3
     CTMP	=   CTMP +    PCOEFF(1,1,0) *X0 *Y1
     CTMP	=   CTMP +    PCOEFF(1,1,0) *X1 *Y0
     CTMP	=   CTMP +    PCOEFF(1,2,0) *X1 *Y0**2
     CTMP	=   CTMP +    PCOEFF(1,3,0) *X1 *Y0**3
     CTMP	=   CTMP + 2* PCOEFF(2,0,0) *X0 *X1
     CTMP	=   CTMP +    PCOEFF(2,0,1) *X0**2 *Z1
     CTMP	=   CTMP +    PCOEFF(2,1,0) *X0**2 *Y1
     CTMP	=   CTMP + 3* PCOEFF(3,0,0) *X0**2 *X1
     CTMP	=   CTMP +    PCOEFF(3,0,1) *X0**3 *Z1
     CTMP	=   CTMP +    PCOEFF(3,1,0) *X0**3 *Y1
     CTMP	=   CTMP + 2* PCOEFF(0,1,2) *Y0 *Z0 *Z1
     CTMP	=   CTMP + 3* PCOEFF(0,1,3) *Y0 *Z0**2 *Z1
     CTMP	=   CTMP + 2* PCOEFF(0,2,1) *Y0 *Y1 *Z0
     CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y0 *Y1 *Z0**2
     CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y0**2 *Z0 *Z1
     CTMP	=   CTMP + 3* PCOEFF(0,3,1) *Y0**2 *Y1 *Z0
     CTMP	=   CTMP + 2* PCOEFF(1,0,2) *X0 *Z0 *Z1
     CTMP	=   CTMP + 3* PCOEFF(1,0,3) *X0 *Z0**2 *Z1
     CTMP	=   CTMP +    PCOEFF(1,1,1) *X0 *Y0 *Z1
     CTMP	=   CTMP +    PCOEFF(1,1,1) *X0 *Y1 *Z0
     CTMP	=   CTMP +    PCOEFF(1,1,1) *X1 *Y0 *Z0
     CTMP	=   CTMP +    PCOEFF(1,1,2) *X0 *Y1 *Z0**2
     CTMP	=   CTMP +    PCOEFF(1,1,2) *X1 *Y0 *Z0**2
     CTMP	=   CTMP + 2* PCOEFF(1,2,0) *X0 *Y0 *Y1
     CTMP	=   CTMP +    PCOEFF(1,2,1) *X0 *Y0**2 *Z1
     CTMP	=   CTMP +    PCOEFF(1,2,1) *X1 *Y0**2 *Z0
     CTMP	=   CTMP + 3* PCOEFF(1,3,0) *X0 *Y0**2 *Y1
     CTMP	=   CTMP + 2* PCOEFF(2,0,1) *X0 *X1 *Z0
     CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X0 *X1 *Z0**2
     CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X0**2 *Z0 *Z1
     CTMP	=   CTMP + 2* PCOEFF(2,1,0) *X0 *X1 *Y0
     CTMP	=   CTMP +    PCOEFF(2,1,1) *X0**2 *Y0 *Z1
     CTMP	=   CTMP +    PCOEFF(2,1,1) *X0**2 *Y1 *Z0
     CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X0 *X1 *Y0**2
     CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X0**2 *Y0 *Y1
     CTMP	=   CTMP + 3* PCOEFF(3,0,1) *X0**2 *X1 *Z0
     CTMP	=   CTMP + 3* PCOEFF(3,1,0) *X0**2 *X1 *Y0
     CTMP	=   CTMP + 2* PCOEFF(1,1,2) *X0 *Y0 *Z0 *Z1
     CTMP	=   CTMP + 2* PCOEFF(1,2,1) *X0 *Y0 *Y1 *Z0
     CTMP	=   CTMP + 2* PCOEFF(2,1,1) *X0 *X1 *Y0 *Z0
     ! C
     TCOEFF  (2) = CTMP
1    CONTINUE
     ! C
     ! C 0th degree
     ! C
     CTMP	=   0.0D0
     CTMP	=   CTMP +   PCOEFF(0,0,0) 
     CTMP	=   CTMP +   PCOEFF(4,0,0) *X0**4 
     CTMP	=   CTMP +   PCOEFF(0,4,0) *Y0**4 
     CTMP	=   CTMP +   PCOEFF(0,0,4) *Z0**4
     CTMP	=   CTMP +   PCOEFF(0,0,1) *Z0
     CTMP	=   CTMP +   PCOEFF(0,0,2) *Z0**2
     CTMP	=   CTMP +   PCOEFF(0,0,3) *Z0**3
     CTMP	=   CTMP +   PCOEFF(0,1,0) *Y0
     CTMP	=   CTMP +   PCOEFF(0,2,0) *Y0**2
     CTMP	=   CTMP +   PCOEFF(0,3,0) *Y0**3
     CTMP	=   CTMP +   PCOEFF(1,0,0) *X0
     CTMP	=   CTMP +   PCOEFF(2,0,0) *X0**2
     CTMP	=   CTMP +   PCOEFF(3,0,0) *X0**3
     CTMP	=   CTMP +   PCOEFF(0,1,1) *Y0 *Z0
     CTMP	=   CTMP +   PCOEFF(0,1,2) *Y0 *Z0**2
     CTMP	=   CTMP +   PCOEFF(0,1,3) *Y0 *Z0**3
     CTMP	=   CTMP +   PCOEFF(0,2,1) *Y0**2 *Z0
     CTMP	=   CTMP +   PCOEFF(0,2,2) *Y0**2 *Z0**2
     CTMP	=   CTMP +   PCOEFF(0,3,1) *Y0**3 *Z0
     CTMP	=   CTMP +   PCOEFF(1,0,1) *X0 *Z0
     CTMP	=   CTMP +   PCOEFF(1,0,2) *X0 *Z0**2
     CTMP	=   CTMP +   PCOEFF(1,0,3) *X0 *Z0**3
     CTMP	=   CTMP +   PCOEFF(1,1,0) *X0 *Y0
     CTMP	=   CTMP +   PCOEFF(1,2,0) *X0 *Y0**2
     CTMP	=   CTMP +   PCOEFF(1,3,0) *X0 *Y0**3
     CTMP	=   CTMP +   PCOEFF(2,0,1) *X0**2 *Z0
     CTMP	=   CTMP +   PCOEFF(2,0,2) *X0**2 *Z0**2
     CTMP	=   CTMP +   PCOEFF(2,1,0) *X0**2 *Y0
     CTMP	=   CTMP +   PCOEFF(2,2,0) *X0**2 *Y0**2
     CTMP	=   CTMP +   PCOEFF(3,0,1) *X0**3 *Z0
     CTMP	=   CTMP +   PCOEFF(3,1,0) *X0**3 *Y0
     CTMP	=   CTMP +   PCOEFF(1,1,1) *X0 *Y0 *Z0
     CTMP	=   CTMP +   PCOEFF(1,1,2) *X0 *Y0 *Z0**2
     CTMP	=   CTMP +   PCOEFF(1,2,1) *X0 *Y0**2 *Z0
     CTMP	=   CTMP +   PCOEFF(2,1,1) *X0**2 *Y0 *Z0
     ! C
     TCOEFF (1) = CTMP
     ! D	WRITE(6,*)'TCOEFF = ',TCOEFF
     ! C
     ! C Inverts array for ZRPOLY
     ! C
     DO 11 I=NDEG+1,1,-1
        COEFF(NDEG-I+2) = TCOEFF(I)
11     	CONTINUE
        ! D	WRITE(6,*)COEFF
        ! srio danger
        !     	CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT,IER)
     	CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT88,IER)
      ! C
      ! C Tests for success;
      ! C
      ! D     	WRITE(6,*)IER
      IF (IER.EQ.130) THEN
         ! C
         ! C if IER=130 degree declared too large; try again;
         ! C
         MOVE = 0
21    	 IF (IER.NE.0) THEN
            MOVE = MOVE + 1
            IF (MOVE.EQ.4) CALL LEAVE ('POLY','ERROR IN POLY',izero)
            DO 31 I=1,NDEG
               TCOEFF(I) = COEFF(I+1)
31          CONTINUE
            DO 41 I=1,NDEG
               COEFF(I)=TCOEFF(I)
41          CONTINUE
               ! D	WRITE(6,*)'MOVE = ',MOVE,COEFF
               ! srio danger
               !     	   CALL 	ZRPOLY (COEFF,NDEG,H_OUTPUT,IER)
            CALL ZRPOLY (COEFF,NDEG,H_OUTPUT88,IER)
            ! D	WRITE(6,*)IER
            GOTO 21
         END IF
      ELSE IF (IER.EQ.131) THEN
         ! C
         ! C fatal error
         ! C
         CALL LEAVE ('POLY','Fatal error.',izero)
      END IF
      ! C
      ! C Tests for reality of intercepts 
      ! C
      CHECK = 1
      ! D	WRITE(6,*)H_OUTPUT
      DO 51 I=1,NDEG
         TEST1(I) = DIMAG(H_OUTPUT(I))
         CHECK	= CHECK*TEST1(I) 	! Complex part
51    CONTINUE
      IF (CHECK.NE.0.0D0) THEN
         ! C
         ! C All the solutions are complex; the beam is completely out of
         ! C of the mirror.
         ! C
         I_RES	= -1
         RETURN
      END IF
               ! C
      ! C At least a good ray;
      ! C
      IF (I_RES.LT.0) THEN
         ! C
         ! C Ripple case : take the closest intercept
         ! C
         ANSWER	= 1.0D30
         DO 61 I=1,NDEG
            TEST = ABS (DIMAG(H_OUTPUT(I)))
            IF (TEST.LT.1.0E-14) THEN
               IF (ABS(DREAL(H_OUTPUT(I))).LT.ABS(ANSWER)) &
                    ANSWER = DREAL ( H_OUTPUT(I) )
            END IF
61       CONTINUE
      ELSE
         ! C
         ! C Ordinary case : looks for the maximum of the real values; this will set the
         ! C intercept at the fartest sheet of the surface
         ! C
         ANSWER	= -1.0D30
         DO 71 I=1,NDEG
            TEST = ABS (DIMAG(H_OUTPUT(I)))
            IF (TEST.LT.1.0E-14) THEN
               ANSWER = DMAX1( ANSWER,DREAL ( H_OUTPUT(I) ))
            END IF
71       CONTINUE
      END IF
      IF (ANSWER.GT.-1.0D30.AND.ANSWER.LT.1.0D30) THEN
         I_RES	= 0
      ELSE
         I_RES = -1
      END IF
      RETURN
    End Subroutine poly
       
                   
! C+++
! C	SUBROUTINE	POLY_GRAD
! C
! C	PURPOSE		To compute the normal to a polinomial surface
! C
! C	INPUT		P (5,5,5) Polinomial coefficients ( COMMON )
! C			POS(3)	  Intercepts 		( Passed )
! C
! C	OUTPUT		VNOR(3)   Outward gradient; NOT normalized to 1
! C
! C---
 SUBROUTINE POLY_GRAD (POS, VNOR)

	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

       	DIMENSION	POS (3), VNOR(3)
       	X 	=   POS(1)
       	 Y	=   POS(2)
       	  Z	=   POS(3)
! C
! C X component
! C
       	CTMP	=   0.0D0
       	CTMP	=   CTMP +    PCOEFF(1,0,0) 
       	CTMP	=   CTMP +    PCOEFF(1,0,1) *Z
       	CTMP	=   CTMP +    PCOEFF(1,0,2) *Z**2
       	CTMP	=   CTMP +    PCOEFF(1,0,3) *Z**3
       	CTMP	=   CTMP +    PCOEFF(1,1,0) *Y
       	CTMP	=   CTMP +    PCOEFF(1,2,0) *Y**2
       	CTMP	=   CTMP +    PCOEFF(1,3,0) *Y**3
       	CTMP	=   CTMP + 2* PCOEFF(2,0,0) *X
       	CTMP	=   CTMP + 3* PCOEFF(3,0,0) *X*2
       	CTMP	=   CTMP + 4* PCOEFF(4,0,0) *X**3
       	CTMP	=   CTMP +    PCOEFF(1,1,1) *Y *Z
       	CTMP	=   CTMP +    PCOEFF(1,1,2) *Y *Z**2
       	CTMP	=   CTMP +    PCOEFF(1,2,1) *Y**2 *Z
       	CTMP	=   CTMP + 2* PCOEFF(2,0,1) *X *Z
       	CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X *Z**2
       	CTMP	=   CTMP + 2* PCOEFF(2,1,0) *X *Y
       	CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X *Y**2
       	CTMP	=   CTMP + 3* PCOEFF(3,0,1) *X**2 *Z
       	CTMP	=   CTMP + 3* PCOEFF(3,1,0) *X**2 *Y
       	CTMP	=   CTMP + 2* PCOEFF(2,1,1) *X *Y *Z
       	VNOR(1)	=   CTMP
! C
! C Y component
! C
       	CTMP	=   0.0D0
       	CTMP	=   CTMP +    PCOEFF(0,1,0) 
       	CTMP	=   CTMP +    PCOEFF(0,1,1) *Z
       	CTMP	=   CTMP +    PCOEFF(0,1,2) *Z**2
       	CTMP	=   CTMP +    PCOEFF(0,1,3) *Z**3
       	CTMP	=   CTMP + 2* PCOEFF(0,2,0) *Y
       	CTMP	=   CTMP + 3* PCOEFF(0,3,0) *Y**2
       	CTMP	=   CTMP + 4* PCOEFF(0,4,0) *Y**3
       	CTMP	=   CTMP +    PCOEFF(1,1,0) *X
       	CTMP	=   CTMP +    PCOEFF(2,1,0) *X*2
       	CTMP	=   CTMP +    PCOEFF(3,1,0) *X**3
       	CTMP	=   CTMP + 2* PCOEFF(0,2,1) *Y *Z
       	CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y *Z**2
       	CTMP	=   CTMP + 3* PCOEFF(0,3,1) *Y**2 *Z
       	CTMP	=   CTMP +    PCOEFF(1,1,1) *X *Z
       	CTMP	=   CTMP +    PCOEFF(1,1,2) *X *Z**2
       	CTMP	=   CTMP + 2* PCOEFF(1,2,0) *X *Y
       	CTMP	=   CTMP + 3* PCOEFF(1,3,0) *X *Y**2
       	CTMP	=   CTMP +    PCOEFF(2,1,1) *X**2 *Z
       	CTMP	=   CTMP + 2* PCOEFF(2,2,0) *X**2 *Y
       	CTMP	=   CTMP + 2* PCOEFF(1,2,1) *X *Y *Z
       	VNOR(2)	=   CTMP
! C
! C Z component
! C
       	CTMP	=   0.0D0
       	CTMP	=   CTMP +    PCOEFF(0,0,1) 
       	CTMP	=   CTMP + 2* PCOEFF(0,0,2) *Z
       	CTMP	=   CTMP + 3* PCOEFF(0,0,3) *Z**2
       	CTMP	=   CTMP + 4* PCOEFF(0,0,4) *Z**3
       	CTMP	=   CTMP +    PCOEFF(0,1,1) *Y
       	CTMP	=   CTMP +    PCOEFF(0,2,1) *Y**2
       	CTMP	=   CTMP +    PCOEFF(0,3,1) *Y**3
       	CTMP	=   CTMP +    PCOEFF(1,0,1) *X
       	CTMP	=   CTMP +    PCOEFF(2,0,1) *X*2
       	CTMP	=   CTMP +    PCOEFF(3,0,1) *X**3
       	CTMP	=   CTMP + 2* PCOEFF(0,1,2) *Y *Z
       	CTMP	=   CTMP + 3* PCOEFF(0,1,3) *Y *Z**2
       	CTMP	=   CTMP + 2* PCOEFF(0,2,2) *Y**2 *Z
       	CTMP	=   CTMP + 2* PCOEFF(1,0,2) *X *Z
       	CTMP	=   CTMP + 3* PCOEFF(1,0,3) *X *Z**2
       	CTMP	=   CTMP +    PCOEFF(1,1,1) *X *Y
       	CTMP	=   CTMP +    PCOEFF(1,2,1) *X *Y**2
       	CTMP	=   CTMP + 2* PCOEFF(2,0,2) *X**2 *Z
       	CTMP	=   CTMP +    PCOEFF(2,1,1) *X**2 *Y
       	CTMP	=   CTMP + 2* PCOEFF(1,1,2) *X *Y *Z
       	VNOR(3)	=   CTMP
! C
! C All done
! C
       	RETURN
End Subroutine poly_grad

! C+++
! C	SUBROUTINE	READPOLY
! C
! C	PURPOSE		To read in ther polinomial coefficents
! C
! C---

SUBROUTINE READPOLY (INFILE, IERR)

	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

     	!CHARACTER*80	INFILE
     	character(len=*) :: INFILE
	DO 11 I = 0,4
	  DO 21 J = 0,4
	    DO 31 K = 0,4
	      PCOEFF(I,J,K)	= 0.0D0
31	    CONTINUE
21	  CONTINUE
11	CONTINUE
     	OPEN (20, FILE=INFILE, STATUS='OLD')
     	  READ (20,*,ERR=10,END=10)	NDEG
          I = 0
41     	 IF (I.GE.0) THEN
     	   READ (20,*,IOSTAT=IERR) 	I,J,K, PCOEFF(I,J,K)
       	   IF (I.EQ.-1) GO TO 10
		 GOTO 41
     	 END IF
10	CLOSE (20)
       	IERR = 0
       	RETURN
      End Subroutine readpoly


! C+++
! C
! C	SUBROUTINE	MSETUP
! C
! C	PURPOSE		To compute the parameters specifying a given
! C			mirror.
! C
! C	OUTPUT		a) to common block
! C			b) MSETxx, xx being the OE number
! C
! C---
      SUBROUTINE MSETUP (IWHICH)

        ! todo: remove implicits
  implicit real(kind=skr) (a-e,g-h,o-z)
  implicit integer(kind=ski)        (f,i-n)
 
	DIMENSION 	VIN(3),VNORM(3),POS1(3),POS2(3), &
     			RNCEN(3),RTCEN(3), &
     			VI_DIV(3),VS_DIV(3),V_CENTER(3)
	INTEGER(KIND=ski)	SERR
! C
     	WRITE(6,*) 'Call to MSETUP'
     	CALL	FNAME	(FFILE,'mset',IWHICH,izero)
	VIN(1)	 = 0.0D0
	VIN(2)	 = COSDEL
	VIN(3)	 = -SINDEL
! C
! C Move from INPUT
! C
	RWIDX2	= -RWIDX2
	RLEN2	= -RLEN2
! C
! C Add the polynomial reading section for the facet operation
! C	5/12/92 G.J.
! C
	IF (F_FACET.EQ.1) THEN
	  CALL	READPOLY(FILE_FAC,IERR)
	  IF (IERR.NE.0) CALL LEAVE &
     		('MSETUP','Error from READPOLY in Facet',IERR)
	END IF

! C
! C Defines ratio of refraction indeces for refractor case
! C
! srio@esrf.eu 2012/10/05 ALFA will be recalculated in mirror as a function
! of the photon energy, if selected.
     	IF (F_REFRAC.EQ.1) ALFA = R_IND_IMA/R_IND_OBJ
! C
! C clear array
! C
!     	DO 100 I=1,10
! 100	CCC(I)	= 0.0D0
IF (FMIRR.NE.10) CCC=0.0D0

! C
! C Computes the mirror parameters for all cases
! C
! C              1 2 3 4 5 6 7 8 9 10
! C
	GO TO (1,2,3,4,5,5,7,8,9,10) FMIRR

 1	CONTINUE
! C
! C Spherical 
! C
     	IF (F_EXT.EQ.0) THEN
	RMIRR = SSOUR*SIMAG*2/COSTHE/(SSOUR + SIMAG)
	  IF (F_JOHANSSON.EQ.1) THEN
	    R_JOHANSSON = RMIRR
	    RMIRR = RMIRR/2.0D0
	  ELSE
	  END IF
     	ELSE
     	END IF

	CCC(1)	 =  1.0D0	! X**2  # = 0 in cylinder case
	CCC(2)	 =  1.0D0	! Y**2
	CCC(3)	 =  1.0D0	! Z**2
	CCC(4)	 =   .0D0	! X*Y   # = 0 in cylinder case
	CCC(5)	 =   .0D0	! Y*Z
	CCC(6)	 =   .0D0	! X*Z   # = 0 in cylinder case
	CCC(7)	 =   .0D0	! X     # = 0 in cylinder case
	CCC(8)	 =   .0D0	! Y
	CCC(9)	 = -2*RMIRR	! Z
	CCC(10)  =   .0D0	! G

	GO TO 3000
! C
! C Elliptical *
! C
 2	CONTINUE
     	IF (F_EXT.EQ.0) THEN
	  AXMAJ 	=  ( SSOUR + SIMAG )/2	
	  AXMIN 	=  SQRT(SIMAG*SSOUR)*COSTHE
     	ELSE
     	  ELL_THE = ELL_THE*TORAD
     	END IF

	AFOCI 	=  SQRT( AXMAJ**2-AXMIN**2 )
	
	ECCENT 	=  AFOCI/AXMAJ
! C
! C Computes the mirror center position
! C
     	IF (F_EXT.EQ.0) THEN
! C
! C The center is computed on the basis of the object and image positions
! C
	  YCEN  = (SSOUR-SIMAG)*0.5D0/ECCENT
	  ZCEN  = -SQRT(1-YCEN**2/AXMAJ**2)*AXMIN
     	ELSE
! C
! C The center is computed based on the pole position supplied by the
! C user; notice that ZCEN is always negative, by definition.
! C
     	  YCEN  = AXMAJ*AXMIN
     	  YCEN  = YCEN/SQRT(AXMIN**2+AXMAJ**2*TAN(ELL_THE)**2)
     	  ZCEN  = YCEN*TAN(ELL_THE)
     	  ZCEN  = - ABS(ZCEN)
     	 IF (COS(ELL_THE).LT.0) THEN
     	   YCEN = - ABS(YCEN)
     	 ELSE
     	   YCEN = ABS(YCEN)
     	 END IF
	!write(*,*) "YCEN,ZCEN: ",YCEN,ZCEN
     	END IF
! C
! C Computes now the normal in the mirror center.
! C
	RNCEN(1)  =   .0D0
	RNCEN(2)  = - 2*YCEN/AXMAJ**2
	RNCEN(3)  = - 2*ZCEN/AXMIN**2

	CALL NORM(RNCEN,RNCEN)
! C
! C Computes the tangent versor in the mirror center.
! C
	RTCEN(1)  =  .0D0
	RTCEN(2)  =   RNCEN(3)
	RTCEN(3)  = - RNCEN(2)
! C
! C Computes now the quadric coefficient with the mirror center
! C located at (0,0,0) and normal along (0,0,1)
! C
	A 	=  1/AXMIN**2
	B 	=  1/AXMAJ**2
     	C 	=  A

	CCC(1) 	=  A
	CCC(2) 	=  B*RTCEN(2)**2 + C*RTCEN(3)**2
	CCC(3) 	=  B*RNCEN(2)**2 + C*RNCEN(3)**2
	CCC(4) 	=  .0D0
	CCC(5) 	=  2*(B*RNCEN(2)*RTCEN(2)+C*RNCEN(3)*RTCEN(3))
	CCC(6) 	=  .0D0
	CCC(7) 	=  .0D0
	CCC(8) 	=  .0D0
	CCC(9) 	=  2*(B*YCEN*RNCEN(2)+C*ZCEN*RNCEN(3))
	CCC(10) =  .0D0

	GO TO 3000
 7	CONTINUE
! C
! C Hyperbolical *
! C
     	IF (F_EXT.EQ.0) THEN
	AXMAJ 	=  ( SSOUR - SIMAG )/2
! C
! C If AXMAJ > 0, then we are on the left branch of the hyp. Else we
! C are onto the right one. We have to discriminate between the two cases
! C In particular, if AXMAJ.LT.0 then the hiperb. will be convex.
! C
     	AFOCI	=  0.5D0*SQRT( SSOUR**2 + SIMAG**2 + 2*SSOUR*SIMAG* &
     			   COS(2*THETA) )
     	AXMIN  =  SQRT( AFOCI**2 - AXMAJ**2 )
     	ELSE
     	  ELL_THE = ELL_THE*TORAD
	  AFOCI = SQRT( AXMIN**2 + AXMAJ**2 )
     	END IF
	
	ECCENT 	=  AFOCI/ABS( AXMAJ )
! C
! C bug fix 02 april 1990, unable to specify own values for hyperbola. 
! C moved line beginning with AXMIN to within IF statement, added AFOCI.
! C
! C
! C Computes the center coordinates in the hiperbola RF. 
! C
     	IF (AXMAJ.GT.0.0D0) THEN
     	 YCEN	=   ( AXMAJ - SSOUR )/ECCENT			! < 0
     	ELSE
     	 YCEN	=   ( SSOUR - AXMAJ )/ECCENT			! > 0
     	END IF
     	ZCEN_ARG = ABS( YCEN**2/AXMAJ**2 - 1.0D0)
     	IF (ZCEN_ARG.GT.1.0D-14) THEN
     	  ZCEN	= - AXMIN * SQRT(ZCEN_ARG)			! < 0
     	ELSE
     	  ZCEN  = 0.0D0
     	END IF
! C
! C Computes now the normal in the same RF. The signs are forced to
! C suit our RF.
! C
     	RNCEN (1) =   0.0D0
     	RNCEN (2) = - ABS( YCEN )/AXMAJ**2			! < 0
     	RNCEN (3) = - ZCEN/AXMIN**2				! > 0

     	CALL 	NORM	(RNCEN,RNCEN)
! C
! C Computes the tangent in the same RF
! C
     	RTCEN (1) =   0.0D0
     	RTCEN (2) = - RNCEN(3)					! > 0
     	RTCEN (3) =   RNCEN(2)					! > 0
! C
! C Coefficients of the canonical form
! C
     	A	= - 1/AXMIN**2
     	B	=   1/AXMAJ**2
     	C	=   A
! C
! C Rotate now in the mirror RF. The equations are the same as for the
! C ellipse case.
! C
	CCC(1) 	=  A
	CCC(2) 	=  B*RTCEN(2)**2 + C*RTCEN(3)**2
	CCC(3) 	=  B*RNCEN(2)**2 + C*RNCEN(3)**2
	CCC(4) 	=  .0D0
	CCC(5) 	=  2*(B*RNCEN(2)*RTCEN(2)+C*RNCEN(3)*RTCEN(3))
	CCC(6) 	=  .0D0
	CCC(7) 	=  .0D0
	CCC(8) 	=  .0D0
	CCC(9) 	=  2*(B*YCEN*RNCEN(2)+C*ZCEN*RNCEN(3))
	CCC(10) =  .0D0

     	GO TO 3000
 3	CONTINUE
! C
! C Toroidal *
! C
     	IF (F_EXT.EQ.0) THEN
     	R_MAJ	=   SSOUR*SIMAG*2/COSTHE/(SSOUR + SIMAG)
     	R_MIN	=   SSOUR*SIMAG*2*COSTHE/(SSOUR + SIMAG)
     	ELSE
     	END IF
! C
! C NOTE : The major radius is the in reality the radius of the torus
! C max. circle. The true major radius is then
! C
     	R_MAJ	=   R_MAJ - R_MIN

     	GO TO 1		! This is used for possible later calculations
     			! involving MIRROR1.

 4	CONTINUE
! C
! C Parabolical *
! C
! C Computes the parabola 
! C
     	IF (F_SIDE.EQ.0) THEN  ! image is at infinity
     	  IF (F_EXT.EQ.0)	PARAM	=   2*SSOUR*COSTHE**2
     		YCEN	= - SSOUR*SINTHE**2
     		ZCEN	= - 2*SSOUR*SINTHE*COSTHE
     	ELSE  ! source is at infinity
     	  IF (F_EXT.EQ.0)	PARAM	=   2*SIMAG*COSTHE**2
     		YCEN	= - SIMAG*SINTHE**2
     		ZCEN	= - 2*SIMAG*SINTHE*COSTHE
     	END IF

     	CCC(1)	=   1.0D0
     	CCC(2)	=   COSTHE**2
     	CCC(3)	=   SINTHE**2
     	CCC(4)  =    .0D0
     	CCC(5)	=   2*COSTHE*SINTHE
     	CCC(6)	=    .0D0
     	CCC(7)	=    .0D0
     	CCC(8)	=    .0D0
     	CCC(9)	=   2*ZCEN*SINTHE - 2*PARAM*COSTHE
     	CCC(10) =    .0D0

     	IF (F_SIDE.EQ.0) THEN

     	CCC(5)	= - CCC(5)

     	ELSE
     	END IF

	GO TO 3000

5	CONTINUE
! C
! C Plane *
! C The sign of CCC(9) is < 0 to keep consistency with the other cases
! C normal.
! C
	DO 200 I = 1,10
200	CCC(I) 	=   0.0D0
	CCC(9) 	= - 1.0D0

	GO TO 3000
! C
! C Ice-cream cone
! C
 8	CONTINUE
       	CCC(1)	=  1.0D0
       	CCC(2)	=  1.0D0
! C      	CCC(3)	=  -(TAND (CONE_A))**2
       	CCC(3)	=  -(TAN (TORAD*CONE_A))**2
       	GO TO 3000
! C
! C Polynomial case
! C
 9	CONTINUE
       	CALL	READPOLY (FILE_MIR, IERR)
       	IF (IERR.NE.0) CALL LEAVE  &
     		('MSETUP','Return error from READPOLY',IERR)
! C
! C Conic coefficients defined externally (therefore do nothing here)
! C
 10	CONTINUE
!!         print *,">>Using FMIRR=10"
!!         OPEN (25,FILE="ccc.inp",STATUS='OLD',IOSTAT=IOSTAT)
!!         IF (IOSTAT.EQ.0) THEN
!! 	write(*,*) ">>Using conic coefficients from file ccc.inp"
!!         DO I=1,10  
!!           read(25,*) TMP
!!           CCC(I)=TMP
!!         END DO
!! 	CLOSE(25)
!!         ELSE
!!           print *,'>>File not found: ccc.inp'
!!         END IF
 
       	GO TO 3000
3000	CONTINUE


! C
! C Set to zero the coeff. involving X for the cylinder case, after 
! C projecting the surface on the desired plane.
! C
     	  CIL_ANG = TORAD*CIL_ANG
     	  COS_CIL = COS(CIL_ANG)
     	  SIN_CIL = SIN(CIL_ANG)
     	IF (FCYL.EQ.1) THEN
          if (fmirr.eq.10) &
          print *,'MSETUP: warning: using cylindrical shape (FCYL=1) with external coefficients (FMIRR=10)'
     	  A_1	=   CCC(1)
     	  A_2	=   CCC(2)
     	  A_3	=   CCC(3)
     	  A_4	=   CCC(4)
     	  A_5	=   CCC(5)
     	  A_6	=   CCC(6)
     	  A_7	=   CCC(7)
     	  A_8	=   CCC(8)
     	  A_9	=   CCC(9)
     	  A_10	=   CCC(10)
     	  CCC(1) =  A_1*SIN_CIL**4 + A_2*COS_CIL**2*SIN_CIL**2 -  & !X^2 
     		    A_4*COS_CIL*SIN_CIL**3
     	  CCC(2) =  A_2*COS_CIL**4 + A_1*COS_CIL**2*SIN_CIL**2 - & !Y^2
     		    A_4*COS_CIL**3*SIN_CIL
     	  CCC(3) =  A_3						 !Z^2
     	  CCC(4) =  - 2*A_1*COS_CIL*   SIN_CIL**3 -  &
     		      2*A_2*COS_CIL**3*SIN_CIL +     &
     		      2*A_4*COS_CIL**2*SIN_CIL**2		 !X Y
     	  CCC(5) =  A_5*COS_CIL**2 - A_6*COS_CIL*SIN_CIL	 !Y Z
     	  CCC(6) =  A_6*SIN_CIL**2 - A_5*COS_CIL*SIN_CIL	 !X Z
     	  CCC(7) =  A_7*SIN_CIL**2 - A_8*COS_CIL*SIN_CIL	 !X
     	  CCC(8) =  A_8*COS_CIL**2 - A_7*COS_CIL*SIN_CIL	 !Y
     	  CCC(9) =  A_9						 !Z
     	  CCC(10)=  A_10
     	END IF
! C
! C Set the correct mirror convexity. 
! C
     	IF (F_CONVEX.NE.0) THEN
          if (fmirr.eq.10) &
          print *,'MSETUP: warning: inverting convexity (F_CONVEX=1) with external coefficients (FMIRR=10)'
     	  CCC(5)  = - CCC(5)
     	  CCC(6)  = - CCC(6)
     	  CCC(9)  = - CCC(9)
     	ELSE
     	END IF
! C
! C Sets up the mirror reference frame; this include rotations only;
! C the translations are taken care of in ROTIT.
! C The flag F_MOVE controls the mirror motions; when reset (=0), the
! C variables are reset to zero here, leaving the START.xx file unchanged.
! C 
! C
!!	write(*,*) ">>>>>>>>>>> FINAL: <<<<<<<<<<<<<"
!!        DO I=1,10  
!!          write(*,*) "CCC[",i,"]=",CCC(I)
!!          write(27,*) CCC(I)
!!        END DO
!!        write(*,*) "CCC coeff copied to unit 27"
!!	write(*,*) ">>>>>>>>>>> END FINAL: <<<<<<<<<<<<<"
     	IF (F_MOVE.EQ.0) THEN
     	  X_ROT = 0.0D0
     	  Y_ROT = 0.0D0
          Z_ROT = 0.0D0
     	  OFFX  = 0.0D0
     	  OFFY  = 0.0D0
     	  OFFZ  = 0.0D0
     	END IF
     	 X_ROT	=   TORAD*X_ROT
     	 Y_ROT	=   TORAD*Y_ROT
     	 Z_ROT	=   TORAD*Z_ROT

     	 COSX	=   COS ( X_ROT )
     	 SINX	= - SIN ( X_ROT )

     	 COSY	=   COS ( Y_ROT )
     	 SINY	= - SIN ( Y_ROT )

     	 COSZ	=   COS ( Z_ROT )
     	 SINZ	= - SIN ( Z_ROT )
! C
! C Computes the rotation matrix coefficients
! C
     	 U_MIR(1)	=   COSZ*COSY
     	 V_MIR(1)	=   COSZ*SINX*SINY - SINZ*COSX
     	 W_MIR(1)	=   COSZ*SINY*COSX + SINZ*SINX

     	 U_MIR(2)	=   SINZ*COSY
     	 V_MIR(2)	=   SINZ*SINX*SINY + COSZ*COSX
     	 W_MIR(2)	=   SINZ*SINY*COSX - SINX*COSZ

     	 U_MIR(3)	= - SINY
     	 V_MIR(3)	=   COSY*SINX
     	 W_MIR(3)	=   COSY*COSX
! C
! C Set up the parameters for the ripple case, if needed
! C
     	IF (F_RIPPLE.EQ.1) THEN
	  IF (F_G_S.EQ.1) THEN
! C
! C Gaussian ripple selected.
! C
! C 

! C
! C CHECK/FIXME: Replace OPEN calls with library routine FOPENR()
! C	CALL FOPENR (20, FILE_RIP, 'FORMATTED', IFERR, IOSTAT)
! C
	OPEN (20,FILE=FILE_RIP,STATUS='OLD',IOSTAT=IOSTAT)
! C
	IF (IOSTAT.NE.0) THEN
	  CALL LEAVE ('MSETUP', &
     		      'Error opening file "' //  &
     		      FILE_RIP(1:IBLANK(FILE_RIP)) // '".', &
     		      IOSTAT)
	END IF
     	READ (20,*)	N_RIP
	READ (20,*)	F_R_RAN
	READ (20,*)	IG_SEED
     	  DO 300 I=1,N_RIP
     	 IF (F_R_RAN.NE.1) THEN
     	READ (20,*)	X_GR(I)
     	READ (20,*)	Y_GR(I)
     	READ (20,*)	AMPLI(I)
     	READ (20,*)	SIG_X(I)
     	READ (20,*)	SIG_Y(I)
     	READ (20,*)	SIGNUM(I)
     	 ELSE
     	READ (20,*)	AMPL_IN(I)
     	READ (20,*)	SIG_XMIN(I)
     	READ (20,*)	SIG_XMAX(I)
     	READ (20,*)	SIG_YMIN(I)
     	READ (20,*)	SIG_YMAX(I)
         END IF
300	  CONTINUE
     	CLOSE (20)

     	  IF (F_R_RAN.EQ.1) THEN
     		DO 1000 I=1,N_RIP
     		X_GR(I) =   RWIDX2 + WRAN(IG_SEED)*RWIDX1
     		Y_GR(I) =   RLEN2  + WRAN(IG_SEED)*RLEN1
     		AMPLI(I)	=   AMPL_IN(I)*WRAN(IG_SEED)
     		SIG_X(I)=   SIG_XMIN(I) + WRAN(IG_SEED)* &
     				(SIG_XMAX(I) - SIG_XMIN(I))
     		SIG_Y(I)=   SIG_YMIN(I) + WRAN(IG_SEED)* &
     				(SIG_YMAX(I) - SIG_YMIN(I))
     		TEST	=   WRAN(IG_SEED)
     			IF (TEST.LE.0.5) THEN
     		SIGNUM(I)	=   1.0D0
     			ELSE
     		SIGNUM(I)	= - 1.0D0
     			END IF
1000		CONTINUE
     	  END IF

! C
! C CHECK/FIXME: Replace OPEN calls with library routine FOPENR()
! C	CALL FOPENW (22, 'SURFACE_ERRORS', 'FORMATTED', IFERR, IOSTAT)
! C
	OPEN (22,STATUS='UNKNOWN',FILE='SURFACE_ERRORS',IOSTAT=IOSTAT)
	REWIND (22)
	IF (IOSTAT.NE.0) THEN
	  CALL LEAVE ('MSETUP', &
     		      'Error opening output file "SURFACE_ERRORS".', &
     		      IOSTAT)
	END IF
	WRITE (22,*) 	X_GR
	WRITE (22,*)    Y_GR
	WRITE (22,*)	AMPLI
	WRITE (22,*)	SIG_X
	WRITE (22,*)	SIG_Y
	WRITE (22,*)	SIGNUM
	WRITE (22,*)	'RANDOM'
	WRITE (22,*)	AMPL_IN
	WRITE (22,*)	SIG_XMIN
	WRITE (22,*)	SIG_XMAX
	WRITE (22,*)	SIG_YMIN
	WRITE (22,*)	SIG_YMAX
	CLOSE (22)

     	  ELSE IF (F_G_S.EQ.2) THEN
! C
! C External spline distortion selected: load input file.
! C
	IFLAG	= -1
	SERR    = 0
	CALL	SUR_SPLINE	(XIN,YIN,ZOUT,VIN,IFLAG,SERR)
	
	  ELSE
! C
! C sinusoidal ripples are selected
! C
	  END IF
     	END IF

	RLEN	=   RLEN1 - RLEN2
	RWIDX	=   RWIDX1 - RWIDX2
! Csrio
! Csrio Store the mirror paramerters (for test)
! Csrio
!	OPEN(UNIT=20, FILE="ccc.out", STATUS='UNKNOWN')
!	WRITE (20,1115) (CCC(I), I=1,10)
!        CLOSE(20)

! Csrio
! C
! C Store the mirror parametersi if debug mode is set
! C
	FDEBUG = 0
! C
! C SET FDEBUG = 1 if you want debugging -- MK.
! C
     	IF (FDEBUG.EQ.1) THEN
	OPEN(UNIT=20, FILE=FFILE, STATUS='UNKNOWN')
	REWIND (20)
     	WRITE (20,1110) IWHICH,FMIRR,FCYL
     	WRITE (20,1110) FHIT_C,FSHAPE,F_CONVEX,F_EXT
	WRITE (20,1112) 'Mirror radius ',RMIRR
	WRITE (20,1112) 'Major axis ',AXMAJ
	WRITE (20,1112) 'Minor axis ',AXMIN
	WRITE (20,1112) 'Eccentricity',ECCENT
	WRITE (20,1112) 'Major radius (optical)',R_MAJ+R_MIN
	WRITE (20,1112) 'Minor radius',R_MIN
	WRITE (20,1112) 'Parameter',PARAM
     	WRITE (20,1112) 'Cone angle',CONE_A
	WRITE (20,1112) 'Image edge ',RLEN1
	WRITE (20,1112) 'Source edge',RLEN2
	WRITE (20,1112) 'Right width',RWIDX1
	WRITE (20,1112) 'Left width',RWIDX2
	WRITE (20,1112) 'Upper V. div',VDIV1
	WRITE (20,1112) 'Lower V. div',VDIV2
	WRITE (20,1112) 'Right H.div',HDIV1
	WRITE (20,1112) 'Left H. div',HDIV2
	WRITE (20,1114) 'Quadric coefficients'
	WRITE (20,1115) (CCC(I), I=1,10)
 	CLOSE (20)
     	END IF
1110	FORMAT (1X,4(2X,I4))
1112	FORMAT (1X,A15,T17,G20.13)
1113	FORMAT (1X,G20.13,/)
1114	FORMAT (1X,A)
1115	FORMAT (1X,G20.13)
     	WRITE(6,*) 'Exit from MSETUP'

! D	WRITE (17,*)	'--------------------------------------------'
! D	WRITE (17,*)	'!MSETUP INPUT'
! D	WRITE (17,*)	IWHICH,FDIM,FMIRR,FCYL,F_MOVE
! D	WRITE (17,*)	COSDEL,SINDEL,COSTHE,SINTHE
! D	WRITE (17,*)	SSOUR,SIMAG
! D	WRITE (17,*)	'****'
! D	 WRITE (17,*)	DELTA,VDIV,HDIV
! D	 WRITE (17,*)	PSOUR
! D	WRITE (17,*)	'****'
! D	 WRITE (17,*)   RLEN1,RLEN2,RWIDX1,RWIDX2
! D	 WRITE (17,*)   PSOUR
! D	WRITE (17,*)	'****'
! D	 WRITE (17,*)	RLEN1,RLEN2,RWIDX1,RWIDX2
! D	 WRITE (17,*)	VDIV1,VDIV2,HDIV1,HDIV2
! D	WRITE (17,*)	'****'
! D	WRITE (17,*)	F_RIPPLE,F_R_RAN,IG_SEED
! D	WRITE (17,*)	FILE_RIP
! D	WRITE (17,*)	'--------------------------------------------'

! D	WRITE (17,*)	'--------------------------------------------'
! D	WRITE (17,*)	'MSETUP OUTPUT'
! D	WRITE (17,*)	RMIRR
! D	WRITE (17,*)	AXMAJ,AXMIN,AFOCI,ECCENT
! D	WRITE (17,*)	YCEN,ZCEN
! D	WRITE (17,*)	RNCEN
! D	WRITE (17,*)	RTCEN
! D	WRITE (17,*)	A,B,C
! D	WRITE (17,*)	R_MAJ,R_MIN
! D	WRITE (17,*)	CCC
! D	WRITE (17,*)
! D	WRITE (17,*)    FDIM
! D	 WRITE (17,*)	DELTA1
! D	 WRITE (17,*)	RLEN1,RLEN2,RLEN
! D	WRITE (17,*)	RWIDX,RWIDX1,RWIDX2
! D	WRITE (17,*)	'****'
! D	WRITE (17,*)    FDIM
! D	 WRITE (17,*)	VS_DIV
! D	 WRITE (17,*)	VI_DIV
! D	 WRITE (17,*)	Z_IMAGE,Z_SOURCE,X_MIRROR_1,X_MIRROR_2
! D	 WRITE (17,*)	VDIV1,VDIV2,HDIV1,HDIV2
! D	WRITE (17,*)	COS_VDIV1,COS_VDIV2
! D	WRITE (17,*)	TAN_HDIV1,TAN_HDIV2
! D	WRITE (17,*)	'--------------------------------------------'
End Subroutine msetup

!C +++
!C 	SUBROUTINE	MOSAIC
!C 
!C 	PURPOSE 	To compute the individual normal of the
!C 			mosaic piece inside the crystal
!C 			with which the ray is reflected.
!C 
!C 	ALGORITHM	We force the normal to verify both the Bragg law
!C 			and the gaussian distribution law around the 
!C 			crystal surface normal.
!C 
!C 	INPUT		a) incident ray direction
!C 			b) crystal surface normal
!C 			c) energy of the ray
!C 
!C 	OUTPUT		a) the new normal for calculate the refl ray
!C 			
!C 			
!C 			
!C ---
Subroutine MOSAIC_old (VVIN,VNOR,WAVEN,VNORG)

        !implicit real(kind=skr) (a-e,g-h,o-z)
        !implicit integer(kind=ski)        (f,i-n)

        !DIMENSION         VNORG(3),AA(3),AA1(3),VVIN(3),VNOR(3)
        implicit none
        real(kind=skr),dimension(3),  intent(in)  :: vvin,vnor
        real(kind=skr),               intent(in)  :: waven
        real(kind=skr),dimension(3),  intent(out) :: vnorg

        real(kind=skr)   :: sin_val, r_lambda, graze, sinn, dumm
        real(kind=skr)   :: theta_b, cos_b, sin_b, tan_b, cos_val, tan_val, epsilon1
        real(kind=skr)   :: argmax, argmin, xx, hh2, hhh, cos_xxx, xxx1
        !real(kind=skr)   :: xxx
        real(kind=skr),dimension(3)  :: aa, aa1
        integer(kind=ski):: ipp

        ! calculate incident angle
        CALL DOT (VVIN,VNOR,SIN_VAL)
        SIN_VAL        =   ABS(SIN_VAL)
        R_LAMBDA = TWOPI/WAVEN *1.0D8
! *
! * next, compute the Bragg angle without index of refraction corrections
! *
          GRAZE =  ASIN(R_LAMBDA*0.5D-8/D_SPACING)
          SINN  =  SIN(GRAZE)
! *
! * Compute now the correct Bragg angle (including N)
! *
          CALL CRYSTAL (WAVEN,SINN,SINN,DUMM,DUMM,DUMM, &
                             DUMM,DUMM,DUMM,DUMM,DUMM,THETA_B,ione)
! *
! * redefinition of bragg angle and incident angle. Now refered to 
! * the surfece normal
! *
        THETA_B  =  PI/2 - THETA_B
        COS_B   = COS(THETA_B)
        SIN_B   = SIN(THETA_B)
        TAN_B   = SIN_B/COS_B
        SIN_VAL = SQRT(1-SIN_VAL**2)
        COS_VAL = SQRT(1-SIN_VAL**2)
        TAN_VAL = SIN_VAL/COS_VAL
        EPSILON1 = -ASIN(SIN_VAL) + THETA_B   ! RADS.

! *
! * rotation of surface normal an angle epsilon around an axis
! * perpendicular to the surface normal and incident ray
! *
        CALL CROSS (VVIN,VNOR,AA)
        CALL ROTVECTOR (VNOR,AA,EPSILON1,AA1)
! *
! * generation of a random spread angle XX fitting with Bragg law
! *
        ARGMAX = (EPSILON1 + 2.0D0*THETA_B)/SPREAD_MOS
        IF (EPSILON1.GE.0) THEN
         ARGMIN = EPSILON1/SPREAD_MOS
        ELSE IF (EPSILON1.LT.0) THEN
         ARGMIN = -EPSILON1/SPREAD_MOS
        END IF
        CALL GNORMAL (ARGMIN,MOSAIC_SEED,i_two) 
        CALL GNORMAL (ARGMAX,MOSAIC_SEED,i_one) 
        CALL GNORMAL (XX,MOSAIC_SEED,izero)
! *
! *if a ray comes very far from Bragg position, GNORMAL fails to
! *give a value in the correct interval, because it goes to infinity
! *Then we consider the most probably value for avoiding the code stops.
! *
        IF ((XX.LT.ARGMIN).OR.(XX.GT.ARGMAX)) THEN 
        !XXX = 0
        XXX1 = 0
        print *,'MOSAIC: Error retrieving Gaussian random number in the interval: ',argmin,argmax
        GO TO 444
        END IF

        XX = SPREAD_MOS*XX
! *
! * rotation of AA1 an angle XXX around the incident ray axis VVIN
! * for having the normal we look for.
! *
        !HH2 = ABS(TAN_VAL - TAN_B)
        !HHH = SQRT(COS_B**(-2)+COS_VAL**(-2)-2*COS(XX)/COS_B/COS_VAL)
        !COS_XXX = (HHH**2-TAN_VAL**2-TAN_B**2)/(-2*TAN_VAL*TAN_B)
        !IF (ABS(COS_XXX).GT.1) CALL MSSG('Error in MOSAIC','cos>1',izero)
        !XXX = ACOS(COS_XXX)
        !XXX = ABS(XXX)
        xxx1 = (cos(xx) - cos_val*cos_b) / (sin_val*sin_b)
        xxx1 = acos(xxx1)
! *next lines give the random direction (ccw or acw) for the rotation
!
! note that random seed initialization is useless, as now WRAN only initializes 
! once at the beginning of the run. Thus, MOSAIC_SEED is not considered. 
        IPP = MOSAIC_SEED
        DUMM = WRAN (IPP)
!       IF (IPP.LT.0) XXX=-XXX
        IF (DUMM.LT.0.5) THEN 
           !XXX=-XXX
           xxx1 = -xxx1
        END IF

!test uniform distribution on beta: xxx1 = 2*pi*dumm 

!xxx = xxx*abs(sin(xxx))
!xxx = sin(xxx)

!write (33,*) theta_b*180/pi, asin(sin_val)*180/pi,xx*180/pi,xxx*180/pi,xxx1*180/pi

444     CALL ROTVECTOR (AA1,VVIN,xxx1,VNORG)
        CALL NORM (VNORG, VNORG)
        RETURN 
! *That's all. This part is done in MIRROR:
! *            CALL PROJ (VVIN,VNORG,VTEMP)
! *          RAY(4,ITIK)         = VVIN(1) - 2*VTEMP(1)
! *          RAY(5,ITIK)         = VVIN(2) - 2*VTEMP(2)
! *          RAY(6,ITIK)         = VVIN(3) - 2*VTEMP(3)
End Subroutine mosaic_old

!C +++
!C 	SUBROUTINE	MOSAIC
!C 
!C 	PURPOSE 	To compute the individual normal of the
!C 			mosaic piece inside the crystal
!C 			with which the ray is reflected.
!C 
!C 	ALGORITHM	We force the normal to verify both the Bragg law
!C 			and the gaussian distribution law around the 
!C 			crystal surface normal.
!C 
!C 	INPUT		a) incident ray direction
!C 			b) crystal surface normal
!C 			c) energy of the ray
!C 
!C 	OUTPUT		a) the new normal for calculate the refl ray
!C 			
!C 			
!C 			
!C ---
Subroutine MOSAIC (VVIN,VNOR,WAVEN,VNORG)

        !implicit real(kind=skr) (a-e,g-h,o-z)
        !implicit integer(kind=ski)        (f,i-n)

        !DIMENSION         VNORG(3),AA(3),AA1(3),VVIN(3),VNOR(3)
        implicit none
        real(kind=skr),dimension(3),  intent(in)  :: vvin,vnor
        real(kind=skr),               intent(in)  :: waven
        real(kind=skr),dimension(3),  intent(out) :: vnorg

        real(kind=skr)   :: sin_val, r_lambda, graze, sinn, dumm
        real(kind=skr)   :: theta_b, cos_b, sin_b, tan_b, cos_val, tan_val, epsilon1
        real(kind=skr)   :: argmax, argmin, xx , ss1, denom
        real(kind=skr),dimension(3)  :: aa, aa1
        integer(kind=ski):: ipp,m_flag

! calculate incident angle
        CALL DOT (VVIN,VNOR,SIN_VAL)
        SIN_VAL        =   ABS(SIN_VAL)
        R_LAMBDA = TWOPI/WAVEN *1.0D8
! *
! * next, compute the Bragg angle without index of refraction corrections
! *
          GRAZE =  ASIN(R_LAMBDA*0.5D-8/D_SPACING)
          SINN  =  SIN(GRAZE)
! *
! * Compute now the correct Bragg angle (including N)
! *
          CALL CRYSTAL (WAVEN,SINN,SINN,DUMM,DUMM,DUMM, &
                             DUMM,DUMM,DUMM,DUMM,DUMM,THETA_B,ione)
! *
! * Redefinition of bragg angle and incident angle. Now referred to 
! * the surfece normal (from here, theta_b is called theta_D in 
! * the mentioned paper, and the incident angle "VAL" is "alpha" in
! * the paper. 
! *
        THETA_B  =  PI/2 - THETA_B
        COS_B   = COS(THETA_B)
        SIN_B   = SIN(THETA_B)
        TAN_B   = SIN_B/COS_B
        SIN_VAL = SQRT(1-SIN_VAL**2)
        COS_VAL = SQRT(1-SIN_VAL**2)
        TAN_VAL = SIN_VAL/COS_VAL
        EPSILON1 = -ASIN(SIN_VAL) + THETA_B   ! RADS.

! *
! * rotation of surface normal an angle epsilon around an axis
! * perpendicular to the surface normal and incident ray
! *
        CALL CROSS_m_flag (VVIN,VNOR,AA,m_flag)
        call norm (aa, aa)
        CALL ROTVECTOR (VNOR,AA,EPSILON1,AA1)

! beta follows approximately a Gaussian distribution with 
! sigma=spread_mos/sqrt(ss1) where ss1 is the coefficient of the series 
! expansion of phi^2 as a function of beta^2 using the formula
! cos(phi) = cos_val*cos_b+cos(beta) sin_cal*sin_b

denom = Sqrt(1.0d0 - (Cos_b*Cos_val + Sin_b*Sin_val)**2)
if (denom .eq. 0) then 
  print *,'MOSAIC: Warning: failed to expansion coefficient.'
  vnorg = aa1
  return
end if
ss1 = (ACos(Cos_b*Cos_val + Sin_b*Sin_val)* Sin_b*Sin_val)/ denom

! *
! * generation of a random number XX (beta in the paper) following a Gaussian 
! * distribution
! *
        ARGMIN = 0.0d0
        ARGMAX = pi
        CALL GNORMAL (ARGMIN,MOSAIC_SEED,i_two) 
        CALL GNORMAL (ARGMAX,MOSAIC_SEED,i_one) 
        CALL GNORMAL (XX,MOSAIC_SEED,izero)

! next give the random direction (cw or ccw) for the rotation
!
! since shadow3, note that random seed initialization is useless, as now 
! WRAN only initializes once at the beginning of the run. 
! Thus, MOSAIC_SEED is not considered. 
        IPP = MOSAIC_SEED
        DUMM = WRAN (IPP)
        IF (DUMM.LT.0.5) THEN 
           XX=-XX
        END IF
        ! convert from sigma=1 to sigma=spread_mos/sqrt(ss1)
        XX = (SPREAD_MOS/sqrt(ss1))*XX

! *
! * rotation of AA1 an angle XX around the incident ray axis VVIN
! * for having the normal we look for.
! *
        CALL ROTVECTOR (AA1,VVIN,xx,VNORG)
        CALL NORM (VNORG, VNORG)
        RETURN 
End Subroutine mosaic



! C+++
! C	SUBROUTINE	READ_AXIS
! C
! C	PURPOSE		Reads in an OPTAXIS file
! C
! C---
SUBROUTINE READ_AXIS (I_MIRROR)

   integer(kind=ski), intent(in) :: i_mirror
   integer(kind=ski)             :: i, j, i_dumm
 
! C
! C Find out the name
! C
    CALL FNAME (FFILE,'optax',I_MIRROR,izero)
! C
! C Open and read the file
! C
    OPEN (UNIT=20, FILE= FFILE, STATUS='OLD')

    DO I=1,I_MIRROR
       READ (20,*) I_DUMM
       !READ (20,*) ( CENTRAL(I,J), J =  1,3)
       !READ (20,*) ( CENTRAL(I,J), J =  4,6)
       !READ (20,*) ( CENTRAL(I,J), J =  7,9)
       !READ (20,*) ( CENTRAL(I,J), J =10,12)
       !READ (20,*) ( CENTRAL(I,J), J =13,15)
       !READ (20,*) ( CENTRAL(I,J), J =16,18)
       !READ (20,*) ( CENTRAL(I,J), J =19,21)
       !READ (20,*) ( CENTRAL(I,J), J =22,24)
       READ (20,*) ( CENTRAL(J), J =  1,3)
       READ (20,*) ( CENTRAL(J), J =  4,6)
       READ (20,*) ( CENTRAL(J), J =  7,9)
       READ (20,*) ( CENTRAL(J), J =10,12)
       READ (20,*) ( CENTRAL(J), J =13,15)
       READ (20,*) ( CENTRAL(J), J =16,18)
       READ (20,*) ( CENTRAL(J), J =19,21)
       READ (20,*) ( CENTRAL(J), J =22,24)
    END DO
!100 CONTINUE
     CLOSE (20)
End Subroutine read_axis
    !
    !
    !

! C+++
! C	SUBROUTINE	REFLEC
! C
! C	PURPOSE		To compute the local reflectivity of a mirror or
! C                     multilayer. Also compute filter transmittivity.
! C
! C	FLAGS		k_what:  .eq. 0 --> initialization call. Reads
! C					in data file.
! C			         .gt. 0 --> performs computations.
! C			(see below)
! C
! C	ARGUMENTS	[ I ] PIN	: (x,y,z) of the intercept
! C			[ I ] wnum 	: wavenumber (cm-1) 
! C			[ I ] sin_ref	: sine of angle from surface
! C			[ I ] cos_pole  : cosine of angle of normal from pole
! C			[ O ] R_P 	: p-pol reflection coefficient
! C			[ O ] R_S 	: s-pol    "  "
! C			[ I ] ABSOR	: film thickness
! C			[ O ] ABSOR	: absorption coefficient
! C
! C---
SUBROUTINE REFLEC (PIN,WNUM,SIN_REF,COS_POLE,R_P,R_S,PHASEP,PHASES,ABSOR,K_WHAT)

implicit none
!IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
!IMPLICIT INTEGER(kind=ski)        (F,I-N)

real(kind=skr),dimension(3),intent(in)   :: pin
real(kind=skr),             intent(in)   :: wnum,sin_ref,cos_pole
integer(kind=ski),          intent(in)   :: k_what
real(kind=skr),             intent(inout):: absor
real(kind=skr),             intent(out)  :: r_s,r_p,phases,phasep

integer(kind=ski), parameter  :: dimMLenergy=300

real(kind=skr),dimension(1000)   :: zf1,zf2
real(kind=skr),dimension(dimMLenergy)    :: ener, &
            delta_s,beta_s,delta_e,beta_e,delta_o,beta_o

logical,dimension(5)    ::  ele,elo,els
character(len=sklen)    ::  file_grade

real(kind=skr),dimension(2,101,2,101) :: tspl,gspl
real(kind=skr),dimension(101)         :: tx,ty,gx,gy
real(kind=skr),dimension(6)           :: pds

real(kind=skr)   :: lateral_grade_constant=1D0 !initialization avoids save
real(kind=skr)   :: lateral_grade_slope=0D0 
real(kind=skr)   :: lateral_grade_quadratic=0D0 
integer(kind=ski):: i_grade=0

real(kind=skr)   :: ab_coeff, cos_ref, del_x, depth0, elfactor, gfact
real(kind=skr)   :: qmin, qmax, qstep, ratio, phot_ener, ratio1, ratio2
real(kind=skr)   :: rho, rs1, rs2, tfact, tfilm, wnum0, xin, xlam, yin, gamma1
integer(kind=ski):: i,j,nrefl,ierr,ier,index1,iunit
integer(kind=ski):: ngx, ngy, ntx, nty, nin, npair

!dimension	tspl (2,101,2,101),tx(101),ty(101),pds(6)
!dimension	gspl (2,101,2,101),gx(101),gy(101)
!external	dbcevl

! srio danger commented these commons, put in shadow_variables...
!        common /aaa/ 	t_oe,gratio
!        common /bbb/ 	delo,beto,dele,bete,dels,bets

! C
! C SAVE the variables that need to be saved across subsequent invocations
! C of this subroutine.
! C
SAVE        QMIN, QMAX, QSTEP, DEPTH0, NREFL, TFILM, &
            ZF1, ZF2, &
            NIN, ENER,  &
            DELTA_S, BETA_S,  &
            NPAIR, &
            DELTA_E,BETA_E, &
            DELTA_O,BETA_O, &
            TSPL,TX,TY,PDS, &
            GSPL,GX,GY, &
            NTX, NTY, NGX, NGY  ! added srio@esrf.eu 20130917
! C
! C Initialization call. The ZF1,ZF2 values do NOT correspond to the F1,F2
! C atomic scattering factors, as they contain a more complex form:
! C		ZFi = Fi*RADIUS*4*PI*ATOMS
! C WNUM is the WAVENUMBER (cm-1) of the ray.
! C ALFA and GAMMA are the complex dielectric function
! C		EPSILON	  = 1 - ALFA + i*GAMMA		[ i=sqrt(-1) ]
! C and are dependent ONLY on the material, while the reflectivities
! C depend of course on the geometry too.
! C ALFA and GAMMA may be generated by PREREF.EXE in [CERRINA.ABS],
! C which is based on B.Henke data (see program header for complete reference).
! C
! C Two flags control the execution.
! C 	F_REFL = 0	ZF1,ZF2 are read in as arrays
! C	       = 1	ALFA and GAMMA are defined in the I/O session
! C			and thus wavelength independent
! C	       = 2      d-spacings and optical constants for multilayer
! C			are read in 
! C 	K_WHAT = 0	Initialization
! C		 1	Reflection case
! C		 2	Absorption case
! C
IF (K_WHAT.EQ.0) THEN
    IF (F_REFL.EQ.0) THEN  !mirror
       !
       ! srio@esrf.eu 2012/09/28 change the prerefl file from bin to ascii
       ! (for compatibility with pre_mlayer and bragg, and for allowing 
       ! other codes to create it).
       ! Note: the old binary format is also accepted when reading 
       !  
        OPEN  (23,FILE=FILE_REFL,STATUS='OLD', &
                      FORM='UNFORMATTED', IOSTAT=iErr)
        IF (ierr /= 0 ) then
             PRINT *,"CRYSTAL: Error: File not found: "//TRIM(file_refl)
             STOP ' Fatal error: aborted'
        END IF

        READ (23,ERR=222) QMIN,QMAX,QSTEP,DEPTH0
        READ (23,IOSTAT=iErr) NREFL
        IF (iErr.NE.0) GOTO 222
        READ (23,IOSTAT=iErr) (ZF1(I),I=1,NREFL)
        IF (iErr.NE.0) GOTO 222
        READ (23,IOSTAT=iErr) (ZF2(I),I=1,NREFL)
        IF (iErr.NE.0) GOTO 222
        CLOSE (23)
        TFILM = ABSOR
        RETURN
! this part is for new ascii format
222     continue
        close(23)
        OPEN  (23,FILE=FILE_REFL,STATUS='OLD', &
                      FORM='FORMATTED', IOSTAT=iErr)
        ! srio added test
        IF (ierr /= 0 ) then
             PRINT *,"CRYSTAL: Error: File not found: "//TRIM(file_refl)
             STOP ' Fatal error: aborted'
        END IF
        READ (23,*) QMIN,QMAX,QSTEP,DEPTH0
        READ (23,*) NREFL
        READ (23,*) (ZF1(I),I=1,NREFL)
        READ (23,*) (ZF2(I),I=1,NREFL)
        CLOSE (23)
        TFILM = ABSOR
        RETURN
    ELSE IF (F_REFL.EQ.2) THEN  !multilayer
        ! C
        ! C  this version allows specification of the individual
        ! C  layer thicknesses.
        ! C
        ! C  input parameters:
        ! C  npair = no. of layer pairs (npair=0 means an ordinary mirror, 
        ! C          elo is the mirror
        ! C  xlam = wavelength (angstroms)
        ! C  elo = odd layer material
        ! C  ele = even layer material
        ! C  els = substrate material
        ! C  1.0 - delo - i*beto = complex refractive index (odd)
        ! C  1.0 - dele - i*bete = complex refractive index (even)
        ! C  t_oe   = thickness t(odd)+t(even) in Angstroms of each layer pair
        ! C  gratio = gamma ratio t(even)/(t(odd)+t(even))  of each layer pair
        ! C  phr = grazing angle in radians
        ! C
        ! C
        iunit = 23
        ! WARNING: I got sometimes segmentation fault around this point. 
        !          Problem not identified....  srio@esrf.eu 2010-08-26
        open(unit=iunit,FILE=FILE_REFL,status='OLD',IOSTAT=iErr)
        ! srio added test
        if (iErr /= 0 ) then
            print *,"MIRROR: File not found: "//trim(file_refl)
            stop 'File not found. Aborted.'
        end if
        READ(iunit,*) NIN
        IF (NIN > dimMLenergy) THEN 
            print *,'REFLEC: Error: In file: '//trim(file_refl)
            print *,'               Maximum number of energy points is',dimMLenergy
            print *,'               Using number of energy points',NIN
            stop 'Error reaing file. Aborted.'
        END IF 
        READ(iunit,*) (ENER(I), I = 1, NIN)
        DO 13 I=1,NIN
            READ(iunit,*) DELTA_S(I),BETA_S(I)
13      CONTINUE
        DO 23 I=1,NIN
            READ(iunit,*)  DELTA_E(I),BETA_E(I)
23      CONTINUE
        DO 33 I=1,NIN
            READ(iunit,*) DELTA_O(I),BETA_O(I)
33      CONTINUE
        READ(iunit,*) NPAIR
        if(npair .lt. 0) then ! if npair<0 roughness data is available
            do i = 1, abs(npair)
                read(iunit,*) t_oe(i),gratio(i),mlroughness1(i),mlroughness2(i)
            end do 
        else
            do i = 1, npair
                read(iunit,*) t_oe(i),gratio(i)
                mlroughness1(i)=0.0
                mlroughness2(i)=0.0
            end do
        endif
        npair = abs(npair)
        ! C
        ! C Is the multilayer thickness graded ?
        ! C
        read    (iunit,*)   i_grade
        ! 0=None
        ! 1=spline files 
        ! 2=quadic coefficients

        ! spline
        if (i_grade.eq.1) then
          read  (iunit,'(a)') file_grade   
          OPEN  (45, FILE=adjustl(FILE_GRADE), STATUS='OLD', & 
                FORM='UNFORMATTED', IOSTAT=iErr)
          ! srio added test
          if (iErr /= 0 ) then
            print *,"REFLEC: File not found: "//trim(adjustl(file_grade))
            stop 'File not found. Aborted.'
          end if

          READ  (45) NTX, NTY
          READ  (45) TX,TY
          !DO 205 I = 1, NTX
          !DO 205 J = 1, NTY
          DO I = 1, NTX
            DO J = 1, NTY
              READ  (45) TSPL(1,I,1,J),TSPL(1,I,2,J),    & ! spline for t
                         TSPL(2,I,1,J),TSPL(2,I,2,J)
            END DO
          END DO

          READ (45) NGX, NGY
          READ (45) GX,GY
          DO I = 1, NGX
            DO J = 1, NGY
              READ (45) GSPL(1,I,1,J),GSPL(1,I,2,J),    & ! spline for gamma
                        GSPL(2,I,1,J),GSPL(2,I,2,J)
            END DO
          END DO

          CLOSE (45)
        end if

        if (i_grade.eq.2) then  ! quadric coefficients
          !
          ! laterally gradded multilayer
          !
          read(iunit,*) lateral_grade_constant,lateral_grade_slope, &
                        lateral_grade_quadratic
        end if

        close(unit=iunit)
        tfilm = absor
        RETURN
    END IF
END IF
! C
! C This is the normal calculation part;
! C
! C If F_REFL is 1, ALFA and GAMMA are defined during the input session
! C and are not modified anymore (single line or closely spaced lines case)
! C
IF (F_REFL.EQ.0) THEN      !Both absorp and normal
    !reflectivity
    index1 =   (WNUM - QMIN)/QSTEP + 1
    ! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/compilation_fix2008-04-09.txt
    IF (index1.LT.1) THEN
       index1=1
       ! C     ('REFLEC','Photon energy below lower limit.',0)
       print *,"REFLEC: Warning: Photon energy below lower limit. Rerun prerefl."
    END IF
    IF (index1.GT.NREFL) THEN
       index1=NREFL-1
       ! C     ('REFLEC','Photon energy above upper limit.',0)
       print *,"REFLEC: Warning: Photon energy above upper limit. Rerun prerefl."
    END IF
    IF (index1.EQ.NREFL)  index1  = index1 - 1
    WNUM0  =   QSTEP*(index1-1) + QMIN
    DEL_X  =   WNUM - WNUM0
    DEL_X  =   DEL_X/QSTEP
    ALFA  =   ZF1(index1) + (ZF1(index1+1)-ZF1(index1))*DEL_X
    gamma1  =   ZF2(index1) + (ZF2(index1+1)-ZF2(index1))*DEL_X
    ! D  WRITE (37,1020) WNUM,WNUM0,INDEX,DEL_X,ALFA,GAMMA
    ! D1020  FORMAT (1X,2(E15.8,1X),I4,3(E15.8,1X))
END IF

IF (K_WHAT.EQ.1) THEN
    IF (F_REFL.NE.2) THEN
        ! C
        ! C Computes the optical coefficients.
        ! C
        COS_REF =  SQRT(1.0D0 - SIN_REF**2)
        RHO  =   SIN_REF**2 - ALFA
        RHO  =   RHO + SQRT ((SIN_REF**2 - ALFA)**2 + gamma1**2)
        RHO  =   SQRT(RHO/2)
        ! C
        ! C Computes now the reflectivities
        ! C
        RS1  =   4*(RHO**2)*(ABS(SIN_REF)-RHO)**2 + gamma1**2
        RS2  =   4*(RHO**2)*(ABS(SIN_REF)+RHO)**2 + gamma1**2
        R_S  =   RS1/RS2
        ! C
        ! C Computes now the polarization ratio
        ! C
        RATIO1  =   4*RHO**2*(RHO*ABS(SIN_REF)-COS_REF**2)**2 + &
        gamma1**2*SIN_REF**2
        RATIO2  =   4*RHO**2*(RHO*ABS(SIN_REF)+COS_REF**2)**2 + &
        gamma1**2*SIN_REF**2
        RATIO  =   RATIO1/RATIO2
        ! C
        ! C The reflectivity for p light will be
        ! C
        R_P  =   R_S*RATIO
        R_S  =   SQRT(R_S)
        R_P  =   SQRT(R_P)
    ELSE
        ! C
        ! C Multilayers reflectivity.
        ! C First interpolate for all the refractive indices.
        ! C
        XLAM  =   TWOPI/WNUM*1.0D8    ! Angstrom
        PHOT_ENER  = WNUM/TWOPI*TOCM  ! eV
        ELFACTOR  = LOG10(1.0D04/30.0D0)/300.0D0
        index1  = LOG10(PHOT_ENER/ENER(1))/ELFACTOR + 1
        ! C    INDEX  = 96.0*LOG10(PHOT_ENER/ENER(1)) + 1
        ! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/compilation_fix2008-04-09.txt
        IF (index1.LT.1) index1=1
        ! C    ('REFLEC','Photon energy too small.',2)
        IF (index1.GT.NIN) index1=NIN-1
        ! C    ('REFLEC','Photon energy too large.',2)
        DELS  = DELTA_S(index1) + (DELTA_S(index1+1) - DELTA_S(index1)) &
             *(PHOT_ENER - ENER(index1))/(ENER(index1+1) - ENER(index1))
        BETS  = BETA_S(index1) + (BETA_S(index1+1) - BETA_S(index1)) &
             *(PHOT_ENER - ENER(index1))/(ENER(index1+1) - ENER(index1))
        DELE  = DELTA_E(index1) + (DELTA_E(index1+1) - DELTA_E(index1)) &
             *(PHOT_ENER - ENER(index1))/(ENER(index1+1) - ENER(index1))
        BETE  = BETA_E(index1) + (BETA_E(index1+1) - BETA_E(index1)) &
             *(PHOT_ENER - ENER(index1))/(ENER(index1+1) - ENER(index1))
        DELO  = DELTA_O(index1) + (DELTA_O(index1+1) - DELTA_O(index1)) &
             *(PHOT_ENER - ENER(index1))/(ENER(index1+1) - ENER(index1))
        BETO  = BETA_O(index1) + (BETA_O(index1+1) - BETA_O(index1)) &
             *(PHOT_ENER - ENER(index1))/(ENER(index1+1) - ENER(index1))

        ! C
        ! C CALL FRESNEL (NPAIR,SIN_REF,COS_POLE,XLAM,R_S,R_P,PHASES,PHASEP)
        ! C
        ! C
        ! C If graded, compute the factor for t and gamma at the intercept PIN.
        ! C
        TFACT       = 1.0D0
        GFACT       = 1.0D0
        IF (I_GRADE.EQ.1) THEN
            XIN = PIN(1)
            YIN = PIN(2)
            CALL DBCEVL (TX,NTX,TY,NTY,TSPL,i101,XIN,YIN,PDS,IER)
            IF (IER.NE.0) THEN
              CALL      MSSG ('REFLEC','Spline error # ',IER)
              RETURN
            END IF
            TFACT = PDS(1)
            ! C
            CALL DBCEVL (GX,NGX,GY,NGY,GSPL,i101,XIN,YIN,PDS,IER)
            IF (IER.NE.0) THEN
              CALL MSSG ('REFLEC','Spline error # ',IER)
              RETURN
            END IF
            GFACT = PDS(1)
        ELSE IF (I_GRADE.EQ.2) THEN
            TFACT = lateral_grade_constant+ &
                    lateral_grade_slope*pin(2) + &
                    lateral_grade_quadratic*pin(2)*pin(2)
        ELSE
        END IF


        CALL FRESNEL  (TFACT,GFACT,NPAIR,SIN_REF,COS_POLE,XLAM, &
                         R_S,R_P,PHASES,PHASEP)
    END IF
ELSE IF(K_WHAT.EQ.2) THEN
    ! C
    ! C This is the transmission case. SIN_REF is now the incidence angle
    ! C onto the filter.
    ! C
    ! C Computes now the penetration depth. SIN_REF is now the cosine of
    ! C the incidence angle of the ray on the screen.
    ! C
    ! C         DEPTH  =   DEPTH0*GAMMA*SIN_REF/WNUM
    AB_COEFF  = WNUM*gamma1/ABS(SIN_REF)
    ! C
    ! C Computes the film absorption. The thickness is passed at the call with
    ! C K_WHAT = 0
    ! C
    ! C ABSOR is the attenuation of the A vector.
    ! C
    ABSOR  =   EXP(-TFILM*AB_COEFF/2.0D0)
END IF
RETURN
End Subroutine reflec


!C------------------------------------------------------------------------------
!C  subroutine FRESNEL 
!C------------------------------------------------------------------------------
!c  compute x-ray/u.v. reflection efficiency of multilayers
!c
!c  inputs: 
!c         tfact   : used for ML with graded thickness (thickness coeff)
!c         gfact   : used for ML with graded thickness (gamma coeff)
!c         n       : number of bilayers
!c         sin_ref :  sin of angle of incidence (grazing??)
!c         cos_pole:  cos of angle of between normal and pole??
!c outputs:
!c         ans = S polarization  reflectivity 
!c         anp = P polarization  reflectivity 
!c         phaseS = change of phase S
!c         phaseP = change di phase P
!c  other variables: 
!c        delo,dele,dels = parameter delta odd, even, substrate respectively
!c        belo,bele,bels = parametro beta odd, even, substrate respectively
!c        1.0 - delo - i*beto = complex refractive index (odd)
!c        1.0 - dele - i*bete = complex refractive index (even)
!c        t_o = thickness of odd layers (a)
!c        t_e = thickness of even layers (a)
!c
!c----------------------------------------------------------------------------
!C
!C
!C               vacuum
!C    |------------------------------|  \
!C    |          odd (n)             |  |
!C    |------------------------------|  | BILAYER # n
!C    |          even (n)            |  |
!C    |------------------------------|  /
!C    |          .                   |
!C    |          .                   |
!C    |          .                   |
!C    |------------------------------|  \
!C    |          odd (1)             |  |
!C    |------------------------------|  | BILAYER # 1
!C    |          even (1)            |  |
!C    |------------------------------|  /
!C    |                              |
!C    |///////// substrate //////////|
!C    |                              |
!C
!c----------------------------------------------------------------------------
!c----------------------------------------------------------------------------


subroutine FRESNEL (tfact,gfact,n,sin_ref,cos_pole,xlam,ans, anp,phaseS,phaseP)

implicit none

real(kind=skr),  intent(in)  :: tfact,gfact,sin_ref,cos_pole,xlam
real(kind=skr),  intent(out) :: ans, anp,phaseS,phaseP

real(kind=skr)     :: xmfv,sin_ref2, cos_ref2, pp, qq, refv
integer(kind=ski)  :: i,j,n

complex(kind=skx)  ::  ci,fo,fe,fv,ffe,ffv,ffvp,ffo,ffep,ffop,re2
complex(kind=skx)  ::  ro2,ao,ae,r,rp,fs,ffs,ffsp,rs2
real(kind=skr)     ::  gamma,thick,t_e,t_o

! nevot-croce roughness
real(kind=skr)     ::  sigma_o2,sigma_e2,sigma_s2,sigma_v2
complex(kind=skx)  ::  arg_o,arg_e,arg_s,arg_v
complex(kind=skx)  ::  fnevot_o,fnevot_e,fnevot_s,fnevot_v
real(kind=skr)     ::  prefact

!--------------------------------------
! another way...
!complex(kind=skx)  ::  ro,re,rs,sin_tra,cos_tra,qo2,qe2
!complex(kind=skx)  ::  sin_s,cos_s,sin_o,cos_o,sin_e,cos_e
!real(kind=skr)     ::  cos_ref
!--------------------------------------


! C

! "i" opmplex
ci=(0.0D0,1.0D0)

! (refraction index "odd,even,substrate")**2 
ro2=(1.0D0-delo-ci*beto)**2
re2=(1.0D0-dele-ci*bete)**2
rs2=(1.0D0-dels-ci*bets)**2

! angles
SIN_REF2 = SIN_REF**2
COS_REF2 = 1.0D0 - SIN_REF2

! f(o,e) = sin theta_inc - sin theta_ critical
fo = ro2 - COS_REF2
fe = re2 - COS_REF2
refv=SIN_REF2
xmfv=0.0D0

fv = Dcmplx(refv,xmfv)
fs = rs2 - COS_REF2

!fo=cDsqrt(fo)
!fe=cDsqrt(fe)
!fv=cDsqrt(fv)
!fs=cDsqrt(fs)
fo=mysqrt(fo)
fe=mysqrt(fe)
fv=mysqrt(fv)
fs=mysqrt(fs)

! Fresnel formula "S" (in function of incidence angle and critical angle)
ffe=(fe-fo)/(fe+fo)
ffo=-ffe
ffv=(fv-fo)/(fv+fo)
ffs=(fe-fs)/(fe+fs)
! Fresnel formula "P" (in function of incidence angle and critical angle)
ffep=(fe/re2-fo/ro2)/(fe/re2+fo/ro2)
ffop=-ffep
ffvp=(fv-fo/ro2)/(fv+fo/ro2)
ffsp=(fe/re2-fs/rs2)/(fe/re2+fs/rs2)

!-----------------------------------
! another way
! ro=(1.0D0-delo-ci*beto)
! re=(1.0D0-dele-ci*bete)
! rs=(1.0D0-dels-ci*bets)
! 
! ! 
! cos_ref = sqrt(1.0D0-sin_ref**2)   ! in vacuum 
! !!! snell (top to bottom propagation)
! cos_o = (1.0D0/ro)*cos_ref  ! in odd medium
! cos_e = (ro/re)*cos_o       ! in even medium
! cos_s = (re/rs)*cos_e       ! in substrate medium
! 
! sin_o = mysqrt(1.0d0 - cos_o**2)
! sin_e = mysqrt(1.0d0 - cos_e**2)
! sin_s = mysqrt(1.0d0 - cos_s**2)
! 
! ! even->odd interface
! ffe =  (re*sin_e - ro*sin_o)/ &  ! e->o
!        (re*sin_e + ro*sin_o)
! ffo=-ffe
! ffv = (sin_ref - ro*sin_o )/ &   ! v->o
!       (sin_ref + ro*sin_o )
! ! even->substrate interface
! ffs = (re*sin_e - rs*sin_s )/ &  ! e->s
!       (re*sin_e + rs*sin_s)
! 
! ! p-polarization
! ffep = (re*sin_o - ro*sin_e)/ &  ! e->o
!        (re*sin_o + ro*sin_e)
! 
! ffop=-ffep
! ffvp = (sin_o - ro*sin_ref )/&   !v->o
!        (sin_o + ro*sin_ref )
! 
! ffsp = (re*sin_s - rs*sin_e )/ &  ! e->s
!        (re*sin_s + rs*sin_e)
!-----------------------------------

! reflectivity initialization
r=(0.0D0,0.0D0)
rp=(0.0D0,0.0D0)
prefact=(8.*(PI**2.))/(xlam**2)


!c Nevot-Croce roughness
!c DO NOT include refraction index in the roughness formula
sigma_s2=0.0d0 ! sigma_s**2.0 !roughn. substrate
sigma_v2=0.0d0 ! sigma_v**2.0!roughn. vacuum

! loop over the bilayers
! remember thet "even" is the bottom sublayer
do 1 j=1,n   ! n is the number of bilayers
         ! C
         ! C compute the thickness for the odd and even material :
         ! C
         THICK = T_OE(J) * TFACT
         GAMMA = GRATIO(J) * GFACT
         t_e = GAMMA * THICK
         t_o = (1.0D0-GAMMA) * THICK
         ! C
         ao=-ci*(pi*fo*t_o*cos_pole/xlam)
         ae=-ci*(pi*fe*t_e*cos_pole/xlam)
         ao=cDexp(ao)
         ae=cDexp(ae)
         if(j.eq.1)go to 6
         ! even (botton) sublayer
         sigma_e2=mlroughness1(j)**2.0 !roughn. even layer
         !arg_e=FO*FE*sigma_e2/(CDSqrt(ro2)*CDSqrt(re2))
         arg_e=FO*FE*sigma_e2/(mysqrt(ro2)*mysqrt(re2))
         fnevot_e=cdexp(-prefact*arg_e)
         r=(ae**4)*(r+ffe*fnevot_e)/(r*ffe*fnevot_e+1.0)
         rp=(ae**4)*(rp+ffep*fnevot_e)/(rp*ffep*fnevot_e+1.0)
         !r=(ae**4)*(r+ffe)/(r*ffe+1.0D0)
         !rp=(ae**4)*(rp+ffep)/(rp*ffep+1.0D0)
         go to 7
6        continue  
         ! layer on top of substrate
         !arg_s=FE*FS*sigma_s2/(CDSqrt(re2)*CDSqrt(rs2))
         arg_s=FE*FS*sigma_s2/(mysqrt(re2)*mysqrt(rs2))
         fnevot_s=cdexp(-prefact*arg_s)
         r=(ae**4.0)*(r+ffs*fnevot_s)/(r*ffs*fnevot_s+1.0)
         rp=(ae**4.0)*(rp+ffsp*fnevot_s)/(rp*ffsp*fnevot_s+1.0)
         !r=(ae**4)*(r+ffs)/(r*ffs+1.0D0)
         !rp=(ae**4)*(rp+ffsp)/(rp*ffsp+1.0D0)
7        continue
         ! odd layer (top sublayer)
         sigma_o2=mlroughness2(j)**2.0 !roughn. odd layer
         !arg_o=FO*FE*sigma_o2/(CDSqrt(ro2)*CDSqrt(re2))
         arg_o=FO*FE*sigma_o2/(mysqrt(ro2)*mysqrt(re2))
         fnevot_o=cdexp(-prefact*arg_o)
         r=(ao**4.0)*(r+ffo*fnevot_o)/(r*ffo*fnevot_o+1.0)
         rp=(ao**4.0)*(rp+ffop*fnevot_o)/(rp*ffop*fnevot_o+1.0)
         !r=(ao**4)*(r+ffo)/(r*ffo+1.0D0)
         !rp=(ao**4)*(rp+ffop)/(rp*ffop+1.0D0)
1 continue

!
! vacuum interface
!
!arg_v=fo*fv*sigma_v2/CDSqrt(ro2)
arg_v=fo*fv*sigma_v2/mysqrt(ro2)
fnevot_v=cdexp(-prefact*arg_v)
r=(r+ffv*fnevot_v)/(r*ffv*fnevot_v+1.0)
!r=(r+ffv)/(r*ffv+1.0D0)
!
!added srio@esrf.eu 2012-06-07
!rp=(rp+ffvp)/(rp*ffvp+1.0)
rp=(rp+ffvp*fnevot_v)/(rp*ffvp*fnevot_v+1.0)

!
! calculate phases
!
pp = Dimag(r)
qq = Dreal(r)
CALL ATAN_2(PP,QQ,PHASES)       ! S phase change in units of radians
!! removed srio@esrf.eu 20131017 in conflict with change in 2012-06-07
!! rp=(rp+ffvp)/(rp*ffvp+1.0D0)
anp=cDabs(rp)
! C      anp=anp**2

PP = DIMAG(RP)
QQ = DREAL (RP)
CALL ATAN_2(PP,QQ,PHASEP)       ! P phase change in units of radians
ans=cDabs(r)
! C      ans=ans**2
!
! end 
!
return

End Subroutine fresnel


! C+++
! C	SUBROUTINE	NORMAL
! C
! C	PURPOSE		To compute the normal to the mirror surface in
! C			a given point.
! C
! C---
SUBROUTINE NORMAL (PIN,VOUT)

	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

	DIMENSION 	PIN(3),VOUT(3)

     	X_IN	= PIN(1)
     	Y_IN	= PIN(2)
     	Z_IN	= PIN(3)

! ** Computes the normal using the general quadric form;
! ** remember: the normal is 'outward' for a convex surface.

     	IF (FMIRR.NE.3.AND.FMIRR.NE.9) THEN
! ** general quadric case.

	  VOUT(1) = 2*CCC(1)*X_IN +  &
     			CCC(4)*Y_IN + CCC(6)*Z_IN + CCC(7)
	  VOUT(2) = 2*CCC(2)*Y_IN +  &
     			CCC(4)*X_IN + CCC(5)*Z_IN + CCC(8)
	  VOUT(3) = 2*CCC(3)*Z_IN +  &
     			CCC(5)*Y_IN + CCC(6)*X_IN + CCC(9)
     	ELSE IF (FMIRR.EQ.3) THEN
! ** Torus case. The z coordinate is offsetted due to the change in
! ** ref. frame for this case.
	  IF (F_TORUS.EQ.0) THEN
     	    Z_IN 	= Z_IN - R_MAJ - R_MIN
	  ELSE IF (F_TORUS.EQ.1) THEN
     	    Z_IN 	= Z_IN - R_MAJ + R_MIN
	  ELSE IF (F_TORUS.EQ.2) THEN
     	    Z_IN 	= Z_IN + R_MAJ - R_MIN
	  ELSE IF (F_TORUS.EQ.3) THEN
     	    Z_IN 	= Z_IN + R_MAJ + R_MIN
	  END IF

     	  PART	= X_IN**2 + Y_IN**2 + Z_IN**2

     	  VOUT(1)  = 4*X_IN*(PART + R_MAJ**2 - R_MIN**2)
     	  VOUT(2)  = 4*Y_IN*(PART - (R_MAJ**2 + R_MIN**2))
     	  VOUT(3)  = 4*Z_IN*(PART - (R_MAJ**2 + R_MIN**2))

        TEST=R_MAJ-R_MIN
	 IF (F_FACET.EQ.1.AND.TEST.EQ.0.0D0)  THEN
	  TEST=VOUT(1)**2+VOUT(2)**2+VOUT(3)**2
	    IF(TEST.EQ.0.0D0) THEN
	     VOUT(3)=1.0D0
	    END IF
	 END IF

       	ELSE IF (FMIRR.EQ.9) THEN
       	  CALL  POLY_GRAD (PIN,VOUT)
     	END IF
     	RETURN
    End Subroutine normal



! C+++
! C	SUBROUTINE	RESET
! C
! C	PURPOSE		To correctly initialize the variables and flags,
! C			avoiding cross-talk between different OE
! C
! C---
SUBROUTINE RESET

!todo: implicit none  In fact, the implicit here is useless, 
!                     only the common variables are affected!
	IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(kind=ski)        (F,I-N)
! C
     	WRITE(6,*)'Call to RESET'
! C
! C  FLAGS block
! C
     	FMIRR		= 	0
	F_TORUS		= 	0
     	FCYL		= 	0
     	FANG		=	0
     	FSTAT		=	0
     	FANG1		=	0
     	FSLIT		=	0
     	FGRID		=	0
     	F_NEW		=	0
     	FSOURCE_DEPTH	=	0
     	FSOUR		=	0
     	FDISTR		=	0
	F_OPD		= 	0
     	F_POL		=	0
     	F_RIPPLE	=	0
     	F_MOVE		=	0
     	F_DEFAULT	=	0
     	F_CENTRAL	=	0
     	FHIT_C		=	1
     	F_COLOR		=	0
     	F_CONVEX	=	0
     	F_EXT		=	0
     	F_GRATING	=	0
     	F_CRYSTAL	=	0
     	F_PHOT_CENT	=	0
     	F_MONO		=	0
     	F_HUNT		=       1
     	F_PLATE		=	0
     	F_COHER		=	0
     	F_PW		=	2
     	F_VIRTUAL	=       0
     	F_REFLEC	=	0
     	F_RULING	=	0
     	F_SCREEN	=	0
     	F_SIDE		=	0
     	F_REFL		=       0
     	F_G_S		=	0
     	F_READ_RIP	=	0
     	F_R_RAN		=	0
     	FSTAT		=	1
     	F_PHOT		=	0
     	F_REFRAC	=	0
     	FSHAPE		=	0
     	F_PW_C		=	0
     	FWRITE		=	0
     	FZP		=	0
        F_BRAGG_A       =       0
        F_JOHANSSON     =       0
        F_MOSAIC        =       0
        F_ROUGHNESS     =       0
        FDUMMY          =       0
        F_ANGLE         =       0
        F_R_IND         =       0
! C
! C  CALC block
! C
     	N_PLATES   	=	0
     	N_SCREEN	=	0
! C	NPOINT	carried on;
     	DO 11 I=1,10
     	I_SCREEN(I)	=	0
     	I_SLIT(I)	=	0
     	K_SLIT(I)	=	0
     	I_STOP(I)	=	0
     	I_ABS(I)	=	0
     	FILE_ABS(I)	=	'NONE SPECIFIED'
     	FILE_SCR_EXT(I)	=	'NONE SPECIFIED'
11     	CONTINUE
! C
! C  SYSTEM block
! C
     	ALPHA		=	0.0D0
     	SSOUR		=	0.0D0
     	SIMAG		=	0.0D0
     	THETA		=	0.0D0
     	RDSOUR		=	0.0D0
     	RTHETA		=	0.0D0
     	TIMDIS		=	0.0D0
     	DELTA		=	0.0D0
     	RDELTA		=	0.0D0
     	ALPHA_S		=	0.0D0
     	OFF_SOUX	=	0.0D0
     	OFF_SOUY	=	0.0D0
     	OFF_SOUZ	=	0.0D0
     	DO 100 I=1,3
     	PSOUR(I)	=	0.0D0
     	PSREAL(I)	=	0.0D0
100     continue
! C
! C  MIRROR block
! C
     	RLEN		=	0.0D0
     	RLEN1		=	0.0D0
     	RLEN2		=	0.0D0
     	RMIRR		=	0.0D0
     	AXMAJ		=	0.0D0
     	AXMIN		=	0.0D0
     	AFOCI		=	0.0D0
     	ECCENT		=	0.0D0
     	R_MAJ		=	0.0D0
     	R_MIN		=	0.0D0
     	RWIDX		=	0.0D0
     	RWIDX1		=	0.0D0
     	RWIDX2		=	0.0D0
     	CONE_A		=	0.0D0
     	PARAM		=	0.0D0

     	DO 200 I=1,10
200	CCC(I)		=	0.0D0
! C
! C GRATING block
! C
     	RULING		=	0.0D0
     	ORDER		=	0.0D0
     	BETA		=	0.0D0
     	PHOT_CENT 	=	0.0D0
     	R_LAMBDA  	=	0.0D0
     	HUNT_H		=	0.0D0
     	HUNT_L		=	0.0D0
     	BLAZE		=	0.0D0
     	D_SPACING	=	0.0D0
     	AZIM_FAN	=	0.0D0
     	DIST_FAN	=	0.0D0
     	COMA_FAC	=	0.0D0
	RUL_A1		= 	0.0D0
	RUL_A2		= 	0.0D0
	RUL_A3		= 	0.0D0
	RUL_A4		= 	0.0D0
        A_BRAGG         =       0.0D0
! C
! C CRYSTAL Block
! C
        SPREAD_MOS      =       0.0D0
        R_JOHANSSON     =       0.0D0
        THICKNESS       =       0.0D0
! C
! C  MIRROR block
! C
     	X_ROT		=	0.0D0
     	Y_ROT		=	0.0D0
     	Z_ROT		=	0.0D0
     	OFFX		=	0.0D0
     	OFFY		=	0.0D0
     	OFFZ		=	0.0D0
     	DO 300 I=1,3
     	U_MIR(I)   	=	0.0D0
     	V_MIR(I)   	=	0.0D0
300     W_MIR(I)   	=	0.0D0
     	U_MIR(1)   	=    1.0D0
     	V_MIR(2)   	=    1.0D0
     	W_MIR(3)   	=    1.0D0
! C
! C  SLIT block
! C
     	SLLEN		=	0.0D0
     	SLWID		=	0.0D0
     	SLTILT		=	0.0D0
     	COD_LEN		=	0.0D0
     	COD_WID		=	0.0D0
! C
! C  SOURCE block
! C
! C  --------- NO NEED ----------- not used again
! C
! C  ALADDIN block
! C
! C  --------- NO NEED ----------- not used again
! C
! C  TRIG block
! C
! C  --------- NO NEED ----------- recomputed all
! C
! C  IMAGE block
! C
     	THETA_I		=	0.0D0
     	ALPHA_I		=	0.0D0
     	DO 400 I=1,3
     	RIMCEN(I)	=	0.0D0
     	VNIMAG(I)	=	0.0D0
     	UXIM(I)		=	0.0D0
     	VZIM(I)		=	0.0D0
     	C_STAR(I)	=	0.0D0
     	C_PLATE(I)	=	0.0D0
     	UX_PL(I)	=	0.0D0
     	VZ_PL(I)	=	0.0D0
400     WY_PL(I)	=	0.0D0

     	DO 500 I=1,5
500     D_PLATE(I)	=	0.0D0
! C
! C  AXIS block
! C
! C	CENTRAL carried through;
     	T_INCIDENCE	=	0.0D0
     	T_REFLECTION	=	0.0D0
     	T_SOURCE	=	0.0D0
     	T_IMAGE		=	0.0D0
! C
! C  RIPPLE block
! C
     	X_RIP_AMP	=	0.0D0
     	Y_RIP_AMP	=	0.0D0
     	X_RIP_WAV	=	0.0D0
     	Y_RIP_WAV	=	0.0D0
     	X_PHASE		=	0.0D0
     	Y_PHASE		=	0.0D0
        ROUGH_X         =       0.0D0
        ROUGH_Y         =       0.0D0
! C
! C SEGMENT Block
! C
        F_SEGMENT       =       0
        ISEG_XNUM       =       1
        ISEG_YNUM       =       1
        SEG_LENX        =       0.0D0
        SEG_LENY        =       0.0D0
! C
! C FACET Block
! C
        F_FACET         =       0
        F_POLSEL        =       0
        IFAC_X          =       0
        IFAC_Y          =       0
        F_FAC_ORIENT    =       0
        F_FAC_LATT      =       0
        RFAC_LENX       =       0.0D0
        RFAC_LENY       =       0.0D0
        RFAC_PHAX       =       0.0D0
        RFAC_PHAY       =       0.0D0
        RFAC_DELX1      =       0.0D0
        RFAC_DELX2      =       0.0D0
        RFAC_DELY1      =       0.0D0
        RFAC_DELY2      =       0.0D0
! C
! C Kumakhov (KOMA) Block
! C
        F_KOMA          =       0
        F_KOMA_BOUNCE   =       0
        F_EXIT_SHAPE    =       0
        F_INC_MNOR_ANG  =       0
        F_KOMA_CA       =       0
        F_DOT           =       0
        I_KOMA          =       0
        KOXX            =       0
        RKOMA_CX        =       0.0D0
        RKOMA_CY        =       0.0D0
        ZKO_LENGTH      =       0.0D0
! C
! C  LIGHT block
! C
     	R_IND_OBJ	=       1.0D0
     	R_IND_IMA	=       1.0D0
        R_ATTENUATION_OBJ =     0.0D0
        R_ATTENUATION_IMA =     0.0D0
     	ALFA		=	0.0D0
     	GAMMA		=	0.0D0
     	POL_ANGLE	=	0.0D0
     	POL_DEG		=	0.0D0
! C	PHxx	not used again

! C
! C  SCREENS block
! C
     	DO 41 I = 1,3
     	DO 51 J = 1,2
     	UX_SCR(I,J)	=	0.0D0
     	WY_SCR(I,J)	=	0.0D0
     	VZ_SCR(I,J)	=	0.0D0
51     	CONTINUE
41     	CONTINUE
     	DO 61 I=1,10
     	RX_SLIT(I)	=	0.0D0
     	RZ_SLIT(I)	=	0.0D0
     	SL_DIS(I)	=	0.0D0
     	THICK(I)	=	0.0D0
61     	CONTINUE
! C
! C  HOLO block
! C
     	HOLO_R1		=	0.0D0
     	HOLO_R2		=	0.0D0
     	HOLO_DEL	=	0.0D0
     	HOLO_GAM	=	0.0D0
     	HOLO_W		=	0.0D0
     	HOLO_RT1	=	0.0D0
     	HOLO_RT2	=	0.0D0
     	DO 71 I=1,3
     	HOLO1(I)	=	0.0D0
     	HOLO2(I)	=	0.0D0
71     	CONTINUE
! C
! C Names Block
! C
     	FILE_MIR	=	'NONE SPECIFIED'
     	FILE_REFL	=	'NONE SPECIFIED'
     	FILE_RIP	=	'NONE SPECIFIED'
	FILE_SOURCE	=	'NONE SPECIFIED'
        FILE_ROUGH      =       'NONE SPECIFIED'
        FILE_KOMA       =       'NONE SPECIFIED'
        FILE_KOMA_CA    =       'NONE SPECIFIED'
        FILE_SEGMENT    =       'NONE SPECIFIED'
        FILE_SEGP       =       'NONE SPECIFIED'
        FILE_FAC        =       'NONE SPECIFIED'
        FILE_R_IND_OBJ  =       'NONE SPECIFIED'
        FILE_R_IND_IMA  =       'NONE SPECIFIED'

     	WRITE(6,*)'Exit from RESET'

     	RETURN

End Subroutine reset


! C+++
! C	SUBROUTINE	RESTART
! C
! C	PURPOSE		Rotates the beam from the IMAGE refernece
! C			frame to the MIRROR reference frame. The source
! C			is thus positioned at T_SOURCE from pole and at
! C			at angle T_INCIDENCE.
! C
! C	ARGUMENTS	[ I ]	RAY	: the beam as obtained from the
! C					  last IMAGE plane.
! C			[ O ]   RAY	: The same beam but in new RF
! C
! C	PARAMETERS	In Common blocks
! C
! C---
SUBROUTINE RESTART (RAY,PHASE,AP)

	IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(kind=ski)        (F,I-N)

        !DIMENSION RAY(12,NPOINT),AP(3,NPOINT), PHASE(3,NPOINT)
        real(kind=skr),dimension(:,:), intent(in out) :: RAY, PHASE, AP
        real(kind=skr),dimension(9)  :: temp_1,temp_2,temp_1p
        real(kind=skr),dimension(3)  :: temp_2p
        character(len=sklen)                   :: STMP
        integer(kind=ski)                      :: ITMP

	!DIMENSION 	RAY(12,N_DIM), AP(3,N_DIM), PHASE(3,N_DIM), &
     	!		TEMP_1(9),TEMP_2(9), TEMP_1P(3),TEMP_2P(3)

     	WRITE(6,*)'Call to RESTART'
! C
! C rotate of the absolute source movements
! C
     	IF (FSTAT.EQ.1) THEN
     	  CALL	ROT_SOUR (RAY,AP)
	  IFLAG	= 0

!!	SUBROUTINE WRITE_OFF (FNAME,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IFORM,IERR)

!     	  CALL	WRITE_OFF ('ROT_SOUR',RAY,PHASE,AP,NCOL,NPOINT,IFLAG, &
!                          0,IERR)
          STMP = 'ROT_SOUR'
          ITMP = 0
     	  CALL	WRITE_OFF (STMP,RAY,PHASE,AP,NCOL,NPOINT,IFLAG, ITMP,IERR)
     	 IF (IERR.NE.0) CALL LEAVE  &
     		('RESTART','Error writing ROT_SOUR',IERR)
     	END IF
! C	
! C Rotate now the general ray in the new mirror reference frame.
! C First we take care of the axial position of the mirror (ALPHA), then
! C we bring the old image (TEMP_1) in the new mirror reference frame (NEW).
! C This is equivalent to say that we do first a rotation around the image
! C normal and after we bring it on the mirror. This is done both for
! C the ray direction cosine and polarization vectors.
! C TEMP_1 and TEMP_2 are utility vectors.
! C
	DO 100 ITIME = 1,NPOINT
      	  IF (RAY(10,ITIME).LT.-1.0D6) THEN
     		GO TO 100
     	  ELSE
      	  END IF

		TEMP_1(1) =   RAY(1,ITIME)*COSAL + RAY(3,ITIME)*SINAL
		TEMP_1(2) =   RAY(2,ITIME)
		TEMP_1(3) = - RAY(1,ITIME)*SINAL + RAY(3,ITIME)*COSAL
! C
! C The former were the positions onto the image plane. Now we compute the
! C new directions and the polarization vector in the new RF.
! C
		TEMP_1(4) =   RAY(4,ITIME)*COSAL + RAY(6,ITIME)*SINAL
		TEMP_1(5) =   RAY(5,ITIME)
		TEMP_1(6) = - RAY(4,ITIME)*SINAL + RAY(6,ITIME)*COSAL

		TEMP_1(7) =   RAY(7,ITIME)*COSAL + RAY(9,ITIME)*SINAL
		TEMP_1(8) =   RAY(8,ITIME)
		TEMP_1(9) = - RAY(7,ITIME)*SINAL + RAY(9,ITIME)*COSAL
! C
! C We rotate now of the incidence angle, as in SOURCE, and we re-origin
! C at the "real" source position.
! C
		TEMP_2(1) =   TEMP_1(1) + PSREAL(1)
		TEMP_2(2) =   TEMP_1(2)*SINTHR + TEMP_1(3)*COSTHR  &
     					  + PSREAL(2)
		TEMP_2(3) = - TEMP_1(2)*COSTHR + TEMP_1(3)*SINTHR  &
     					  + PSREAL(3)

		TEMP_2(4) =   TEMP_1(4)
		TEMP_2(5) =   TEMP_1(5)*SINTHR + TEMP_1(6)*COSTHR
		TEMP_2(6) = - TEMP_1(5)*COSTHR + TEMP_1(6)*SINTHR

		TEMP_2(7) =   TEMP_1(7)
		TEMP_2(8) =   TEMP_1(8)*SINTHR + TEMP_1(9)*COSTHR
		TEMP_2(9) = - TEMP_1(8)*COSTHR + TEMP_1(9)*SINTHR
! C
! C Have to rotate now of the source rotation angle around Z. Use
! C again TEMP_1 for convenience.
! C
     		TEMP_1(1) =   TEMP_2(1)*COSAL_S - TEMP_2(2)*SINAL_S
     		TEMP_1(2) =   TEMP_2(1)*SINAL_S + TEMP_2(2)*COSAL_S
     		TEMP_1(3) =   TEMP_2(3)
     		TEMP_1(4) =   TEMP_2(4)*COSAL_S - TEMP_2(5)*SINAL_S
     		TEMP_1(5) =   TEMP_2(4)*SINAL_S + TEMP_2(5)*COSAL_S
     		TEMP_1(6) =   TEMP_2(6)
     		TEMP_1(7) =   TEMP_2(7)*COSAL_S - TEMP_2(8)*SINAL_S
     		TEMP_1(8) =   TEMP_2(7)*SINAL_S + TEMP_2(8)*COSAL_S
     		TEMP_1(9) =   TEMP_2(9)

! C Create now the final array.

     		RAY(1,ITIME)  =   TEMP_1(1)
		RAY(2,ITIME)  =   TEMP_1(2)
     		RAY(3,ITIME)  =   TEMP_1(3)
		RAY(4,ITIME)  =   TEMP_1(4)
		RAY(5,ITIME)  =   TEMP_1(5)
		RAY(6,ITIME)  =   TEMP_1(6)
     		RAY(7,ITIME)  =   TEMP_1(7)
		RAY(8,ITIME)  =   TEMP_1(8)
     		RAY(9,ITIME)  =   TEMP_1(9)
! C
! C The other elements, I=10,12 are not changed
! C
! C Same procedure for AP
! C
	IF (NCOL.EQ.18) THEN
		TEMP_1P(1) =   AP(1,ITIME)*COSAL + AP(3,ITIME)*SINAL
		TEMP_1P(2) =   AP(2,ITIME)
		TEMP_1P(3) = - AP(1,ITIME)*SINAL + AP(3,ITIME)*COSAL

		TEMP_2P(1) =   TEMP_1P(1)
		TEMP_2P(2) =   TEMP_1P(2)*SINTHR + TEMP_1P(3)*COSTHR
		TEMP_2P(3) = - TEMP_1P(2)*COSTHR + TEMP_1P(3)*SINTHR

     		TEMP_1P(1) =   TEMP_2P(1)*COSAL_S - TEMP_2P(2)*SINAL_S
     		TEMP_1P(2) =   TEMP_2P(1)*SINAL_S + TEMP_2P(2)*COSAL_S
     		TEMP_1P(3) =   TEMP_2P(3)

		AP(1,ITIME)	= TEMP_1P(1)
		AP(2,ITIME)	= TEMP_1P(2)
		AP(3,ITIME)	= TEMP_1P(3)
	END IF

100	CONTINUE

     	WRITE(6,*)'Exit from RESTART'

End Subroutine restart

!$$$$$$$

! C+++
! C	SUBROUTINE	ROT_FOR
! C
! C	PURPOSE		Applies the roto-translation of the mirror movements
! C			to the beam. This allows a complete decoupling
! C			of the system.
! C
! C	ARGUMENT	[ I ]	RAY	: the beam, as computed by 
! C					RESTART.
! C			[ O ] 	RAY	: the beam, as seen by a MOVED
! C					  mirror
! C
! C---
SUBROUTINE ROT_FOR (RAY,AP)

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)


	real(kind=skr),dimension(:,:),intent(in out) ::  ray,ap

     	!DIMENSION	RAY(12,N_DIM), AP(3,N_DIM)
     	DIMENSION	P_IN(3),V_IN(3),P_OUT(3),V_OUT(3), &
     			A_IN(3),A_OUT(3),AP_IN(3),AP_OUT(3)

! D	OPEN (33,FILE='ROTIN1',STATUS='NEW',FORM='UNFORMATTED')
! D	WRITE (33)	RAY_IN
! D	CLOSE (33)
     	
     	DO 11 I=1,NPOINT

     	P_IN(1)	=    RAY(1,I)
     	P_IN(2)	=    RAY(2,I)
     	P_IN(3)	=    RAY(3,I)
     	V_IN(1)	=    RAY(4,I)
     	V_IN(2)	=    RAY(5,I)
     	V_IN(3)	=    RAY(6,I)
     	A_IN(1)	=    RAY(7,I)
     	A_IN(2)	=    RAY(8,I)
     	A_IN(3)	=    RAY(9,I)

     	P_OUT(1)=    (P_IN(1) - OFFX)*U_MIR(1) + &
     		     (P_IN(2) - OFFY)*U_MIR(2) + &
     		     (P_IN(3) - OFFZ)*U_MIR(3)

     	P_OUT(2)=    (P_IN(1) - OFFX)*V_MIR(1) + &
     		     (P_IN(2) - OFFY)*V_MIR(2) + &
     		     (P_IN(3) - OFFZ)*V_MIR(3)

     	P_OUT(3)=    (P_IN(1) - OFFX)*W_MIR(1) + &
     		     (P_IN(2) - OFFY)*W_MIR(2) + &
     		     (P_IN(3) - OFFZ)*W_MIR(3)

     	V_OUT(1)=    V_IN(1)*U_MIR(1) + &
     		     V_IN(2)*U_MIR(2) + &
     		     V_IN(3)*U_MIR(3)

     	V_OUT(2)=    V_IN(1)*V_MIR(1) + &
     		     V_IN(2)*V_MIR(2) + &
     		     V_IN(3)*V_MIR(3)

     	V_OUT(3)=    V_IN(1)*W_MIR(1) + &
     		     V_IN(2)*W_MIR(2) + &
     		     V_IN(3)*W_MIR(3)

     	A_OUT(1)=    A_IN(1)*U_MIR(1) + &
     		     A_IN(2)*U_MIR(2) + &
     		     A_IN(3)*U_MIR(3)

     	A_OUT(2)=    A_IN(1)*V_MIR(1) + &
     		     A_IN(2)*V_MIR(2) + &
     		     A_IN(3)*V_MIR(3)

     	A_OUT(3)=    A_IN(1)*W_MIR(1) + &
     		     A_IN(2)*W_MIR(2) + &
     		     A_IN(3)*W_MIR(3)

     	RAY(1,I)	=    P_OUT(1)
     	RAY(2,I)	=    P_OUT(2)
     	RAY(3,I)	=    P_OUT(3)
     	RAY(4,I)	=    V_OUT(1)
     	RAY(5,I)	=    V_OUT(2)
     	RAY(6,I)	=    V_OUT(3)
     	RAY(7,I)	=    A_OUT(1)
     	RAY(8,I)	=    A_OUT(2)
     	RAY(9,I)	=    A_OUT(3)

	IF (NCOL.EQ.18) THEN
	AP_IN(1)	= AP(1,I)
	AP_IN(2)	= AP(2,I)
	AP_IN(3)	= AP(3,I)

     	AP_OUT(1)=    AP_IN(1)*U_MIR(1) + &
     		      AP_IN(2)*U_MIR(2) + &
     		      AP_IN(3)*U_MIR(3)

     	AP_OUT(2)=    AP_IN(1)*V_MIR(1) + &
     		      AP_IN(2)*V_MIR(2) + &
     		      AP_IN(3)*V_MIR(3)

     	AP_OUT(3)=    AP_IN(1)*W_MIR(1) + &
     		      AP_IN(2)*W_MIR(2) + &
     		      AP_IN(3)*W_MIR(3)

	AP(1,I)		= AP_OUT(1)
	AP(2,I)		= AP_OUT(2)
	AP(3,I)		= AP_OUT(3)
	END IF
11     	CONTINUE

! D	OPEN (33,FILE='ROTOUT1',STATUS='NEW',FORM='UNFORMATTED')
! D	WRITE (33)	RAY
! D	CLOSE (33)

End Subroutine rot_for


! C+++
! C	SUBROUTINE	ROT_BACK
! C
! C	PURPOSE		Applies the roto-translation of the mirror movements
! C			to the beam. This will bring bak the beam in the
! C			normal MIRROR frame.
! C
! C	ARGUMENT	[ I ]	RAY	: the beam, as computed by 
! C					  MIRROR.
! C			[ O ] 	RAY	: the beam, as seen back in the
! C					  mirror refernece frame.
! C
! C---
SUBROUTINE ROT_BACK(RAY,AP)

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)


	real(kind=skr),dimension(:,:),intent(in out) ::  ray,ap
     	!DIMENSION	RAY(12,N_DIM),AP(3,N_DIM)
     	DIMENSION	P_IN(3),V_IN(3),P_OUT(3),V_OUT(3), &
     			A_IN(3),A_OUT(3),AP_IN(3),AP_OUT(3)

! D	OPEN (33,FILE='ROTIN2',STATUS='NEW',FORM='UNFORMATTED')
! D	WRITE (33)	RAY
! D	CLOSE (33)
     	
     	DO 11 I=1,NPOINT

     	P_IN(1)	=    RAY(1,I)
     	P_IN(2)	=    RAY(2,I)
     	P_IN(3)	=    RAY(3,I)
     	V_IN(1)	=    RAY(4,I)
     	V_IN(2)	=    RAY(5,I)
     	V_IN(3)	=    RAY(6,I)
     	A_IN(1)	=    RAY(7,I)
     	A_IN(2)	=    RAY(8,I)
     	A_IN(3)	=    RAY(9,I)

     	P_OUT(1)=    P_IN(1)*U_MIR(1) + &
     		     P_IN(2)*V_MIR(1) + &
     		     P_IN(3)*W_MIR(1) + OFFX

     	P_OUT(2)=    P_IN(1)*U_MIR(2) + &
     		     P_IN(2)*V_MIR(2) + &
     		     P_IN(3)*W_MIR(2) + OFFY

     	P_OUT(3)=    P_IN(1)*U_MIR(3) + &
     		     P_IN(2)*V_MIR(3) + &
     		     P_IN(3)*W_MIR(3) + OFFZ

     	V_OUT(1)=    V_IN(1)*U_MIR(1) + &
     		     V_IN(2)*V_MIR(1) + &
     		     V_IN(3)*W_MIR(1)

     	V_OUT(2)=    V_IN(1)*U_MIR(2) + &
     		     V_IN(2)*V_MIR(2) + &
     		     V_IN(3)*W_MIR(2)

     	V_OUT(3)=    V_IN(1)*U_MIR(3) + &
     		     V_IN(2)*V_MIR(3) + &
     		     V_IN(3)*W_MIR(3)

     	A_OUT(1)=    A_IN(1)*U_MIR(1) + &
     		     A_IN(2)*V_MIR(1) + &
     		     A_IN(3)*W_MIR(1)

     	A_OUT(2)=    A_IN(1)*U_MIR(2) + &
     		     A_IN(2)*V_MIR(2) + &
     		     A_IN(3)*W_MIR(2)

     	A_OUT(3)=    A_IN(1)*U_MIR(3) + &
     		     A_IN(2)*V_MIR(3) + &
     		     A_IN(3)*W_MIR(3)

     	RAY(1,I)	=    P_OUT(1)
     	RAY(2,I)	=    P_OUT(2)
     	RAY(3,I)	=    P_OUT(3)
     	RAY(4,I)	=    V_OUT(1)
     	RAY(5,I)	=    V_OUT(2)
     	RAY(6,I)	=    V_OUT(3)
     	RAY(7,I)	=    A_OUT(1)
     	RAY(8,I)	=    A_OUT(2)
     	RAY(9,I)	=    A_OUT(3)

	IF (NCOL.EQ.18) THEN
	AP_IN(1)	= AP(1,I)
	AP_IN(2)	= AP(2,I)
	AP_IN(3)	= AP(3,I)

     	AP_OUT(1)=    AP_IN(1)*U_MIR(1) + &
     		      AP_IN(2)*V_MIR(1) + &
     		      AP_IN(3)*W_MIR(1)

     	AP_OUT(2)=    AP_IN(1)*U_MIR(2) + &
     		      AP_IN(2)*V_MIR(2) + &
     		      AP_IN(3)*W_MIR(2)

     	AP_OUT(3)=    AP_IN(1)*U_MIR(3) + &
     		      AP_IN(2)*V_MIR(3) + &
     		      AP_IN(3)*W_MIR(3)

	AP(1,I)		= AP_OUT(1)
	AP(2,I)		= AP_OUT(2)
	AP(3,I)		= AP_OUT(3)
	END IF
11     	CONTINUE

! D	OPEN (33,FILE='ROTOUT2',STATUS='NEW',FORM='UNFORMATTED')
! D	WRITE (33)	RAY
! D	CLOSE (33)

End Subroutine rot_back

! C+++
! C	SUBROUTINE	ROT_SOUR
! C
! C	PURPOSE		Applies the roto-translation of the SOURCE movements
! C			to the beam. This allows a complete decoupling
! C			of the system.
! C
! C	ARGUMENT	[ I ]	RAY	: the beam, as computed by 
! C					  SOURCE.
! C			[ O ] 	RAY	: the beam, after the source 
! C					  movement
! C
! C---
SUBROUTINE ROT_SOUR (RAY,AP)

	IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(kind=ski)        (F,I-N)
    
        real(kind=skr),dimension(:,:),intent(in out) :: ray,ap
     	!  DIMENSION	RAY(12,N_DIM), AP(3,N_DIM)
     	! DIMENSION	P_IN(3),V_IN(3),P_OUT(3),V_OUT(3), &
        ! 		A_IN(3),A_OUT(3),AP_IN(3),AP_OUT(3)
        real(kind=skr),dimension(3) :: p_in,v_in,p_out,v_out, &
                                               a_in,a_out,ap_in,ap_out

! D	OPEN (33,FILE='ROT_SOUR_IN',STATUS='NEW',FORM='UNFORMATTED')
! D	WRITE (33)	RAY_IN
! D	CLOSE (33)
     	
     	DO  10 I=1,NPOINT

     	P_IN(1)	=    RAY(1,I)
     	P_IN(2)	=    RAY(2,I)
     	P_IN(3)	=    RAY(3,I)
     	V_IN(1)	=    RAY(4,I)
     	V_IN(2)	=    RAY(5,I)
     	V_IN(3)	=    RAY(6,I)
     	A_IN(1)	=    RAY(7,I)
     	A_IN(2)	=    RAY(8,I)
     	A_IN(3)	=    RAY(9,I)

     	P_OUT(1)=    (P_IN(1) + X_SOUR)*U_SOUR(1) + &
     		     (P_IN(2) + Y_SOUR)*U_SOUR(2) + &
     		     (P_IN(3) + Z_SOUR)*U_SOUR(3)

     	P_OUT(2)=    (P_IN(1) + X_SOUR)*V_SOUR(1) + &
     		     (P_IN(2) + Y_SOUR)*V_SOUR(2) + &
     		     (P_IN(3) + Z_SOUR)*V_SOUR(3)

     	P_OUT(3)=    (P_IN(1) + X_SOUR)*W_SOUR(1) + &
     		     (P_IN(2) + Y_SOUR)*W_SOUR(2) + &
     		     (P_IN(3) + Z_SOUR)*W_SOUR(3)

     	V_OUT(1)=    V_IN(1)*U_SOUR(1) + &
     		     V_IN(2)*U_SOUR(2) + &
     		     V_IN(3)*U_SOUR(3)

     	V_OUT(2)=    V_IN(1)*V_SOUR(1) + &
     		     V_IN(2)*V_SOUR(2) + &
     		     V_IN(3)*V_SOUR(3)

     	V_OUT(3)=    V_IN(1)*W_SOUR(1) + &
     		     V_IN(2)*W_SOUR(2) + &
     		     V_IN(3)*W_SOUR(3)

     	A_OUT(1)=    A_IN(1)*U_SOUR(1) + &
     		     A_IN(2)*U_SOUR(2) + &
     		     A_IN(3)*U_SOUR(3)

     	A_OUT(2)=    A_IN(1)*V_SOUR(1) + &
     		     A_IN(2)*V_SOUR(2) + &
     		     A_IN(3)*V_SOUR(3)

     	A_OUT(3)=    A_IN(1)*W_SOUR(1) + &
     		     A_IN(2)*W_SOUR(2) + &
     		     A_IN(3)*W_SOUR(3)

     	RAY(1,I)	=    P_OUT(1)
     	RAY(2,I)	=    P_OUT(2)
     	RAY(3,I)	=    P_OUT(3)
     	RAY(4,I)	=    V_OUT(1)
     	RAY(5,I)	=    V_OUT(2)
     	RAY(6,I)	=    V_OUT(3)
     	RAY(7,I)	=    A_OUT(1)
     	RAY(8,I)	=    A_OUT(2)
     	RAY(9,I)	=    A_OUT(3)

! ** Same procedure for AP
	IF (NCOL.EQ.18) THEN
	  AP_IN(1)	= AP(1,I)
	  AP_IN(2)	= AP(2,I)
	  AP_IN(3)	= AP(3,I)

     	  AP_OUT(1)=    AP_IN(1)*U_SOUR(1) + &
     		        AP_IN(2)*U_SOUR(2) + &
     		        AP_IN(3)*U_SOUR(3)

     	  AP_OUT(2)=    AP_IN(1)*V_SOUR(1) + &
     		        AP_IN(2)*V_SOUR(2) + &
     		        AP_IN(3)*V_SOUR(3)

     	  AP_OUT(3)=    AP_IN(1)*W_SOUR(1) + &
     		        AP_IN(2)*W_SOUR(2) + &
     		        AP_IN(3)*W_SOUR(3)

	  AP(1,I)	= AP_OUT(1)
	  AP(2,I)	= AP_OUT(2)
	  AP(3,I)	= AP_OUT(3)
	END IF

10   	CONTINUE

! D	OPEN (33,FILE='ROT_SOUR_OUT',STATUS='NEW',FORM='UNFORMATTED')
! D	WRITE (33)	RAY
! D	CLOSE (33)

End Subroutine rot_sour

!C +++
!C 	SUBROUTINE	SCREEN
!C 
!C 	PURPOSE		This s. computes the intersection of the ray 
!C 			with the screen plane. The format is the 
!C 			standard (12,N) matrix. The result is in the 
!C 			form of the (u,v) coordinates onto the screen 
!C 			plane. These are filed away and the input ray 
!C 			is NOT changed, unless the screen is a stop or 
!C 			a slit. In that the case the appropriate flag 
!C 			is set if the ray has been lost.
!C 
!C 	ARGUMENTS	[ I ]	RAY	The beam description as computed
!C 					by RESTART or MIRROR
!C 			[ I ]   I_WHAT  Selects a screen vs.a stop
!C 			[ I ]   I_ELEMENT OE number
!C 
!C 	OUTPUT		To disk. Generate a SCRxxyy.DAT file, xx being 
!C 			the screen number and yy the OE number.
!C 
!C ---
SUBROUTINE SCREEN (RAY,AP_IN,PH_IN,I_WHAT,I_ELEMENT)

	IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(kind=ski)        (F,I-N)

        real(kind=skr),dimension(:,:),intent(in out):: ray,ap_in,ph_in

        character(len=sklen)        :: locfile,file_tmp
     	!CHARACTER*12	LOCFILE
	!CHARACTER*80	FILE_TMP

        real(kind=skr),dimension(12,NPOINT) :: out,ph_out,ap_out
	!DIMENSION 	RAY(12,N_DIM),OUT(12,N_DIM),PH_IN(3,N_DIM), &
     	!		PH_OUT(3,N_DIM),AP_IN(3,N_DIM),AP_OUT(3,N_DIM)
	!DIMENSION 	V_OUT(3),P_IN(3),P_SCREEN(3),A_VEC(3),AP_VEC(3)
     	!DIMENSION 	SCR_CEN(3),UX_SC(3),WY_SC(3),VZ_SC(3)
        real(kind=skr),dimension(3)::v_out,p_in,p_screen,a_vec,ap_vec
        real(kind=skr),dimension(3)::scr_cen,ux_sc,wy_sc,vz_sc
        ! added srio
        real(kind=skr),dimension(3):: ppout
!C 
!C  Save some local large arrays to avoid overflowing stack.
!C 
!       srio danger!!!
!	SAVE		OUT, PH_OUT, AP_OUT

     	WRITE(6,*)'Call to SCREEN'

     	IF (I_ABS(I_WHAT).EQ.1) THEN
     	  FTEMP	=  F_REFL
     	  F_REFL = 0
	  FILE_TMP = FILE_REFL
     	  FILE_REFL = FILE_ABS(I_WHAT)
     	  !srio CALL	REFLEC (DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM, &
     	  CALL	REFLEC (ppout,DUM,DUM,DUM,DUM,DUM,DUM,DUM, &
    			THICK(I_WHAT),izero)
     	END IF
! ** Set up the correct versors.
	KK = 0
	IF (I_SCREEN(I_WHAT).EQ.0)THEN
     		KK = 1
     		POLE =   SL_DIS(I_WHAT)
     	ELSE IF (I_SCREEN(I_WHAT).EQ.1) THEN
     		KK = 2
     		POLE = - SL_DIS(I_WHAT)
     	ELSE
     		CALL LEAVE ('SCREEN', &
     			'	Wrong values for screen position.',izero)
     	END IF

     	DO  10 I=1,3
     	 UX_SC(I) =   UX_SCR(I,KK)
     	 WY_SC(I) =   WY_SCR(I,KK)
     	 VZ_SC(I) =   VZ_SCR(I,KK)
10     	CONTINUE

! ** Computes first the intercept onto the screen plane and the 
! ** absorption, if set.
     	ATTEN	=   1.0D0

     	DO 100	J=1,NPOINT

! ** Checks if the ray has been reflected by the mirror.

     	IF (RAY(10,J).LT.-1.0D6) GO TO 100

     	P_IN(1)	=   RAY(1,J)
     	P_IN(2)	=   RAY(2,J)
	P_IN(3)	=   RAY(3,J)

     	V_OUT(1)  =   RAY(4,J)
     	V_OUT(2)  =   RAY(5,J)
     	V_OUT(3)  =   RAY(6,J)

	A_VEC(1) = RAY(7,J)
	A_VEC(2) = RAY(8,J)
	A_VEC(3) = RAY(9,J)

	AP_VEC(1) = AP_IN(1,J)
	AP_VEC(2) = AP_IN(2,J)
	AP_VEC(3) = AP_IN(3,J)

     	ABOVE	=   POLE	   - P_IN(1)*WY_SC(1) &
     			    	   - P_IN(2)*WY_SC(2) &
     			    	   - P_IN(3)*WY_SC(3)

     	BELOW   =   WY_SC(1)*V_OUT(1) + WY_SC(2)*V_OUT(2) + &
     		    WY_SC(3)*V_OUT(3)

     	IF (BELOW.NE.0.0D0) THEN
     		DIST	=   ABOVE/BELOW
     	ELSE
     		RAY(10,J)  = - 1.0D4*I_ELEMENT - 1.0D2*I_WHAT
     		GO TO 100
     	END IF

! ** Computes now the intersections onto screen plane.

     	P_SCREEN(1)  =   P_IN(1) + DIST*V_OUT(1)
     	P_SCREEN(2)  =   P_IN(2) + DIST*V_OUT(2)
     	P_SCREEN(3)  =   P_IN(3) + DIST*V_OUT(3)

! ** Rotate now the results in the SCREEN reference plane.
! ** Computes the projection of P_IN onto the image plane versors.

     	CALL	SCALAR	(WY_SC,POLE,SCR_CEN)
     	CALL 	VECTOR 	(SCR_CEN,P_SCREEN,P_SCREEN)

     	CALL 	DOT 	(P_SCREEN,UX_SC,UX_1)
     	CALL 	DOT 	(P_SCREEN,VZ_SC,VZ_1)
     	CALL 	DOT 	(P_SCREEN,WY_SC,WY_1)

! ** Computes now the new directions for the beam in the U,V,N ref.

     	CALL 	DOT 	(V_OUT,UX_SC,VV_1)
     	CALL 	DOT 	(V_OUT,WY_SC,VV_2)
     	CALL 	DOT 	(V_OUT,VZ_SC,VV_3)

! ** Computes the new directions of A in the U,V,N ref.frame

     	CALL DOT (A_VEC,UX_SC,A_1)
     	CALL DOT (A_VEC,WY_SC,A_2)
     	CALL DOT (A_VEC,VZ_SC,A_3)

! ** This will compute the transmission coefficient.
     	IF (I_ABS(I_WHAT).EQ.1) THEN
! bug found Peter Sondhauss peter.sondhauss@maxlab.lu.se 2013-07029
! there is a call to the subroutine REFLEC with a wrong setting of the flag 
! K_WHAT (last parameter). K_WHAT was set to be zero (izero), which is the 
! setting for initialization, i.e. reading of the file with dielectric 
! constants.  I have set K_WHAT to the value of 2 (transmission), or better 
! itwo. Now Shadow3 runs fast again, and the reflectivity values correspond 
! to those of the old Shadow version.
     	 !CALL	REFLEC (PPOUT,RAY(11,J),VV_2,DUM,DUM,DUM,DUM, &
     	 !		 DUM,ATTEN,izero)
     	 CALL	REFLEC (PPOUT,RAY(11,J),VV_2,DUM,DUM,DUM,DUM, &
     			 DUM,ATTEN,itwo)
     	END IF
! ** Saves the results
     	OUT(1,J)  =   UX_1
     	OUT(2,J)  =   WY_1
     	OUT(3,J)  =   VZ_1
     	OUT(4,J)  =   VV_1
     	OUT(5,J)  =   VV_2
     	OUT(6,J)  =   VV_3
     	RAY(7,J)  =   RAY(7,J)*ATTEN
     	OUT(7,J)  =   A_1*ATTEN
     	RAY(8,J)  =   RAY(8,J)*ATTEN
     	OUT(8,J)  =   A_2*ATTEN
     	RAY(9,J)  =   RAY(9,J)*ATTEN
     	OUT(9,J)  =   A_3*ATTEN
     	OUT(10,J) =   RAY(10,J)
     	OUT(11,J) =   RAY(11,J)
     	OUT(12,J) =   RAY(12,J)
	IF (NCOL.GT.12) THEN
	  PH_OUT (1,J) = PH_IN(1,J) + DIST
	  IF (NCOL.EQ.18) THEN
	    PH_OUT (2,J) = PH_IN(2,J)
	    PH_OUT (3,J) = PH_IN(3,J)
	    CALL	DOT	(AP_VEC,UX_SC,AP_1)
	    CALL	DOT	(AP_VEC,WY_SC,AP_2)
	    CALL	DOT	(AP_VEC,VZ_SC,AP_3)
	    AP_IN  (1,J) = AP_IN(1,J)*ATTEN
	    AP_OUT (1,J) = AP_1*ATTEN
	    AP_IN  (2,J) = AP_IN(2,J)*ATTEN
	    AP_OUT (2,J) = AP_2*ATTEN
	    AP_IN  (3,J) = AP_IN(3,J)*ATTEN
	    AP_OUT (3,J) = AP_3*ATTEN
	  END IF
	END IF
100	CONTINUE
     	IF (I_SLIT(I_WHAT).EQ.1)	THEN
    	 IF (K_SLIT(I_WHAT).EQ.0) THEN
!C 
!C 	  Rectangular first.
!C 
	  XCNTR = CX_SLIT(I_WHAT)
	  ZCNTR = CZ_SLIT(I_WHAT)
     	  U_1   = - RX_SLIT(I_WHAT)/2
     	  U_2   =   RX_SLIT(I_WHAT)/2
     	  V_1   = - RZ_SLIT(I_WHAT)/2
     	  V_2   =   RZ_SLIT(I_WHAT)/2
     	  DO 222 ICHECK=1,NPOINT
!C 
!C  Assume obstruction, and then reverse decision for aperture.
!C 
	    PX = OUT(1,ICHECK)-XCNTR
	    PZ = OUT(3,ICHECK)-ZCNTR
	    TEST = -1
	    IF ((PX.GT.U_2.OR.PX.LT.U_1).OR. &
     		(PZ.GT.V_2.OR.PZ.LT.V_1))THEN
	      TEST = 1
	    END IF
	    IF (I_STOP(I_WHAT).EQ.0) THEN
	      TEST = - TEST
	    END IF

     	    IF (TEST.LT.0.0D0) THEN
     		RAY (10,ICHECK)	  = - 1.0D2*I_ELEMENT - 1.0D0*I_WHAT
     		OUT (10,ICHECK)	  = - 1.0D2*I_ELEMENT - 1.0D0*I_WHAT
     	    END IF
222	  CONTINUE

!C 
!C  	Now elliptical
!C 
     	 ELSE IF (K_SLIT(I_WHAT).EQ.1) THEN
	  AXLAR	=   RX_SLIT(I_WHAT)**2/4
	  AXSMA	=   RZ_SLIT(I_WHAT)**2/4
	  XCNTR =   CX_SLIT(I_WHAT)
	  ZCNTR =   CZ_SLIT(I_WHAT)
     	  DO 300 I=1,NPOINT
	    PX = OUT(1,I)-XCNTR
	    PZ = OUT(3,I)-ZCNTR
     	    TEST = PX**2/AXLAR + PZ**2/AXSMA - 1.0D0
     	    IF (I_STOP(I_WHAT).EQ.1)   TEST = - TEST
     	    IF (TEST.GT.0.0D0) THEN
     	      RAY  (10,I) = - 1.0D2*I_ELEMENT - 1.0D0*I_WHAT
     	      OUT  (10,I) = - 1.0D2*I_ELEMENT - 1.0D0*I_WHAT
     	    END IF
300	  CONTINUE
     	ELSE
!C 
!C  Now external (K_SLIT = 2)
!C 
	  CALL	SCREEN_EXTERNAL(I_WHAT,I_ELEMENT,RAY,OUT)
     	END IF
     	ELSE
     	END IF
!C 
!C  EOF marker
!C 
     	KOUNTS	=   100*I_ELEMENT + I_WHAT
     	CALL	FNAME	(LOCFILE,'screen',KOUNTS,ifour)
	IFLAG	= 0
     	CALL	WRITE_OFF (LOCFILE,OUT,PH_OUT,AP_OUT,NCOL,NPOINT,IFLAG, &
                          izero,IERR)
     	IF	(IERR.NE.0)	CALL LEAVE  &
     		('SCREEN','Error writing SCREEN.',IERR)
     	IF (I_ABS(I_WHAT).EQ.1) THEN
	    F_REFL	=   FTEMP
	    FILE_REFL 	=   FILE_TMP
	END IF
	WRITE(6,*)'Exit from SCREEN'
	RETURN
End Subroutine screen

! C+++
! C	SUBROUTINE	SCREEN_EXTERNAL
! C
! C	PURPOSE		This routine handles the screen slit/stops when
! C			an external file with the coordinates is
! C			specified (K_SLIT = 2).
! C
! C	ARGUMENTS	
! C			[ I ]   I_SCR	The screen index
! C			[ I ]   I_ELEMENT OE number
! C                       [I/O]	RAY   	The beam description as computed
! C					by RESTART or MIRROR
! C                       [I/O]	RAY_OUT	The local copy of RAY in SCREEN.
! C
! C	Author:		Mumit Khan
! C
! C---
SUBROUTINE SCREEN_EXTERNAL(I_SCR,I_ELEMENT,RAY,RAY_OUT)

        !implicit real(kind=skr) (a-e,g-h,o-z)
        !implicit integer(kind=ski)        (f,i-n)
        implicit none 
       
        real(kind=skr),dimension(:,:),intent(in out) :: ray,ray_out
        integer(kind=ski),intent(in)                 :: i_scr,i_element
        ! C
        ! C Local variables
        ! C
        integer(kind=ski)    :: ierr,iflag,inp,ipoly,iray,istart
        integer(kind=ski)    :: n_polys,n_points
        real(kind=skr)       :: px, pz
        logical              :: ray_lost, hit_found
        character(len=sklen) :: filename

        !
        ! Allocatable variables
        !
        !real(kind=skr),dimension(MAX_NPOINTS)  :: xvec,zvec
        !integer(kind=ski),dimension(MAX_NPOLY) :: ivec1,ivec2
        real(kind=skr),dimension(:),allocatable    :: xvec,zvec
        integer(kind=ski),dimension(:),allocatable :: ivec1,ivec2

! C -------------  NOT LONGER APPLIES srio@esrf.eu 20120430 -------------
! C The following two parameters limit the polygons that can be read in.
! C There can be upto MAX_NPOLY polygons, and an accumulated total of 
! C MAX_NPOINTS points summed over all the polygons. The accumulated sum
! C includes polygon closure (eg., a rectangle has *5*, not 4 points after
! C closure). To up the limits, modify the following and recompile.
! C
!        integer(kind=ski),parameter :: max_npoly=50, max_npoints=1000

! C
! C Load the external polygon file that describe the patterns on the screen.
! C The file format is described in SCREEN_EXTERNAL_LOAD().
! C
! C Note that the two vectors, xvec and zvec, contain the points for all
! C the polygons. Two aux vectors, ivec1 and ivec2, contain the starting
! C indices (into xvec and zvec) and number of points per polygon.
! C 
        IFLAG = 0
        filename = FILE_SCR_EXT(I_SCR)
        CALL SCREEN_EXTERNAL_GETDIMENSIONS(filename, N_POLYS,N_POINTS,IFLAG)
        !print *,'>>> SCREEN_EXTERNAL_GETDIMENSIONS: N_POLYS: ',N_POLYS
        !print *,'>>> SCREEN_EXTERNAL_GETDIMENSIONS: N_POINTS: ',N_POINTS
        !print *,'>>> SCREEN_EXTERNAL_GETDIMENSIONS: IFLAG: ',IFLAG

        ! allocate vectors
        IF (.NOT. ALLOCATED(xvec)) THEN
          ALLOCATE(xvec(n_points),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"SCREEN_EXTERNAL: Error allocating xvec" ; STOP 4
          END IF
        END IF
        IF (.NOT. ALLOCATED(zvec)) THEN
          ALLOCATE(zvec(n_points),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"SCREEN_EXTERNAL: Error allocating zvec" ; STOP 4
          END IF
        END IF

        IF (.NOT. ALLOCATED(ivec1)) THEN
          ALLOCATE(ivec1(n_polys),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"SCREEN_EXTERNAL: Error allocating ivec1" ; STOP 4
          END IF
        END IF

        IF (.NOT. ALLOCATED(ivec2)) THEN
          ALLOCATE(ivec2(n_polys),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"SCREEN_EXTERNAL: Error allocating ivec2" ; STOP 4
          END IF
        END IF

        CALL SCREEN_EXTERNAL_LOAD(filename, &
!                MAX_NPOLY, MAX_NPOINTS, &
                XVEC,ZVEC,IVEC1,IVEC2,N_POLYS,N_POINTS,IFLAG)
        !print *,'>>> MAX_NPOLY: ',MAX_NPOLY
        !print *,'>>> MAX_NPOINTS: ',MAX_NPOINTS
        !print *,'>>> N_POLYS: ',N_POLYS
        !print *,'>>> N_POINTS: ',N_POINTS
        !print *,'>>> XVEC: ',XVEC(1:NPOINTS)

        IF (IFLAG .EQ. -1) THEN
          CALL MSSG ('SCREEN_EXTERNAL', &
               'Error reading EXTERNAL file', i_one)
          STOP 1
        ELSE IF (IFLAG .EQ. -2) THEN
          CALL MSSG ('SCREEN_EXTERNAL', &
               'Error in External polygon description', i_one)
          STOP 1
        END IF

! C
! C Algorithm: For each ray, see if it hits any of the polygons; if it does,
! C check if the polygon is a Aperture or Obstruction, and set the RAY_LOST
! C logical accordingly.
! C
!        DO 200 IRAY=1,NPOINT
        DO IRAY=1,NPOINT
          HIT_FOUND = .FALSE.
          PX = RAY_OUT(1,IRAY)
          PZ = RAY_OUT(3,IRAY)
          !DO 11 IPOLY = 1, N_POLYS
          DO IPOLY = 1, N_POLYS
            ISTART = IVEC1(IPOLY)
            INP = IVEC2(IPOLY)
            IFLAG = 0
            CALL PNPOLY (PX, PZ, XVEC(ISTART), ZVEC(ISTART), INP, IFLAG)
! C
! C IFLAG = -1 implies point *outside* polygon, 0 vertex, 1 inside.
! C I_STOP = 1 for obstruction, and 0 for aperture.
! C
! C We just need a single hit (inside+vertex) for either type of slit.
! C
            IF (IFLAG.EQ.1 .OR. IFLAG.EQ.0) HIT_FOUND = .TRUE.
! 11	  CONTINUE
          END DO 
! C
! C In case of aperture, a ray is lost if does NOT hit any of the polys;
! C in case of obstruction, a ray is lost if it hits *ANY* of the polys.
! C 
          RAY_LOST = .FALSE.
          IF (I_STOP(I_SCR) .EQ. 0 .AND. .NOT. HIT_FOUND) THEN
              RAY_LOST = .TRUE.
          ELSE IF (I_STOP(I_SCR) .EQ. 1 .AND. HIT_FOUND) THEN
              RAY_LOST = .TRUE.
          END IF

          IF (RAY_LOST) THEN
            RAY     (10,IRAY) = - 1.0D2*I_ELEMENT - 1.0D0*I_SCR
            RAY_OUT (10,IRAY) = - 1.0D2*I_ELEMENT - 1.0D0*I_SCR
          END IF
        END DO
! 200	CONTINUE
! C
        IF (ALLOCATED(xvec)) DEALLOCATE(xvec)
        IF (ALLOCATED(zvec)) DEALLOCATE(zvec)
        IF (ALLOCATED(ivec1)) DEALLOCATE(ivec1)
        IF (ALLOCATED(ivec2)) DEALLOCATE(ivec2)
        RETURN
End Subroutine screen_external


! C+++
! C	SUBROUTINE	SCREEN_EXTERNAL_LOAD
! C
! C	PURPOSE		This routine load the polygon(s) from a file.
! C
! C	ARGUMENTS	
! C			[ I ] FILE	Polygon file name
! C                     [ I ] NPOLY   	Number of polygons to be read
! C                     [ I ] NPOINT   	Number of *total* points to be read
! C                     [ O ] XVEC   	X points
! C                     [ O ] ZVEC   	Z points
! C                     [ O ] IVEC1   	Index of poly starting points
! C                     [ O ] IVEC2   	Number of points in each poly
! C                     [ O ] IFLAG	Returned value
! C					-1: Can't read file
! C					-2: Bad format, too many points
! C					 0: All ok.
! C
! C       FORMAT
! C					N_POLYGONS
! C					N_POINTS
! C					X1 Z1
! C					X2 Z2
! C					.....
! C					XN ZN
! C					N_POINTS
! C					X1 Z1
! C					....
! C					XN ZN
! C
! C	 EXAMPLE FILE
! C					2
! C					4
! C					-37.5	-12.5
! C					 37.5	-12.5
! C					 37.5	 12.5
! C					-37.5	 12.5
! C					5
! C					-20.0	-30.0
! C					 20.0	-30.0
! C					 20.0	-20.0
! C					-20.0	-20.0
! C					-20.0	-30.0
! C
! C Note that in the 2nd polygon, the closure is explicit, while implicit in
! C the first. The points can be in clockwise or counter-clockwise.
! C		
! C	AUTHOR 		Mumit Khan
! C	                M Sanchez del Rio: makes arrays allocatable
! C---

SUBROUTINE  SCREEN_EXTERNAL_LOAD (FILENAME,  &
!                 MAX_NPOLYS, MAX_NPOINTS, &
                 XVEC,ZVEC,IVEC1,IVEC2,NPOLY1,NPOINT1,IFLAG)
! C
        implicit none
        character(len=*),intent(in)  :: FILENAME
        !integer(kind=ski) :: max_npolys,max_npoints
        integer(kind=ski),intent(in)  :: npoly1,npoint1
        integer(kind=ski),intent(out) :: iflag

        !real(kind=skr),dimension(max_npoints) :: xvec,zvec
        !integer(kind=ski),dimension(max_npolys)         :: ivec1,ivec2
        integer(kind=ski)                                :: npoly,npoint
        real(kind=skr),dimension(npoint1),intent(in out)   :: xvec,zvec
        integer(kind=ski),dimension(npoly1),intent(in out) :: ivec1,ivec2
! C
        integer(kind=ski) :: np1, poly_index, pt_index, start_index
        integer(kind=ski),parameter :: io_unit=21
! C
        IFLAG = 0
        OPEN(IO_UNIT, FILE=FILENAME, status='OLD', ERR=199)
        READ(IO_UNIT, *, ERR=299, END=299) NPOLY
        IF (NPOLY .NE. NPOLY1) THEN
          CALL MSSG ('SCREEN_EXTERNAL','problem reading file',i_one)
          GOTO 299
        END IF
        !IF (NPOLY .GT. MAX_NPOLYS) THEN
        !  CALL MSSG ('SCREEN_EXTERNAL','Too many polygons',i_one)
        !  GOTO 299
        !END IF
! C
! C read all the polygons as a compound one (ie., in the same vectors).
! C
        NPOINT = 0
        !DO 10 POLY_INDEX = 1, NPOLY
        DO POLY_INDEX = 1, NPOLY
          READ(IO_UNIT, *, ERR=299, END=299) NP1
          !DO 20 PT_INDEX = 1, NP1
          DO PT_INDEX = 1, NP1
            NPOINT = NPOINT + 1
            !IF (NPOINT .GT. MAX_NPOINTS) THEN
            !  CALL MSSG ('SCREEN_EXTERNAL','Too many total points',i_one)
            !  GOTO 299
            !END IF
            READ(IO_UNIT,*,ERR=299,END=299) XVEC(NPOINT),ZVEC(NPOINT)
! 20          CONTINUE
          END DO 
 
! C
! C Make sure the polygon is "closed", so check the first and last points.
! C
          START_INDEX = NPOINT - NP1 + 1
          IF (XVEC(START_INDEX) .NE. XVEC(NPOINT) .OR. &
                   ZVEC(START_INDEX) .NE. ZVEC(NPOINT)) THEN
            NPOINT = NPOINT + 1
            NP1 = NP1 + 1
            XVEC(NPOINT) = XVEC(START_INDEX)
            ZVEC(NPOINT) = ZVEC(START_INDEX)
          END IF
          IVEC1(POLY_INDEX) = START_INDEX
          IVEC2(POLY_INDEX) = NP1
! C
! 10     CONTINUE
        END DO ! POLY_INDEX 
        CLOSE(IO_UNIT)
        GOTO 300

 199    CONTINUE
        IFLAG = -1
        GOTO 300

 299    CONTINUE
        CLOSE(IO_UNIT)
        IFLAG = -1
        GOTO 300

 300    CONTINUE
        IF (NPOINT .NE. NPOINT1) THEN
          CALL MSSG ('SCREEN_EXTERNAL','Prblem reading file',i_one)
        END IF
        RETURN
End Subroutine screen_external_load


! C+++
! C	SUBROUTINE	SCREEN_EXTERNAL_GETDIMENSIONS
! C
! C	PURPOSE		This routine gets the polygon(s) dimensions
! C
! C	ARGUMENTS	
! C			[ I ] FILE	Polygon file name
! C                     [ O ] NPOLY   	Number of polygons to be read
! C                     [ O ] NPOINT   	Number of *total* points to be read
! C                     [ O ] IFLAG	Returned value
! C					-1: Can't read file
! C					-2: Bad format, too many points
! C					 0: All ok.
! C		
! C	AUTHOR 		M Sanchez del Rio
! C---

SUBROUTINE  SCREEN_EXTERNAL_GETDIMENSIONS (FILENAME, NPOLY,NPOINT,IFLAG)
! C
    IMPLICIT NONE
    character(len=*),intent(in)   :: FILENAME
    integer(kind=ski),intent(out) :: npoly,npoint,iflag

    integer(kind=ski) :: np1, poly_index, pt_index, start_index
    integer(kind=ski),parameter :: io_unit=21
    real(kind=skr)    :: xtmp,ztmp,xtmp0,ztmp0

    IFLAG = 0
    OPEN(IO_UNIT, FILE=FILENAME, status='OLD', ERR=199)
    READ(IO_UNIT, *, ERR=299, END=299) NPOLY
! C
! C read all the polygons as a compound one (ie., in the same vectors).
! C
    NPOINT = 0
    DO POLY_INDEX = 1, NPOLY
      READ(IO_UNIT, *, ERR=299, END=299) NP1
      DO PT_INDEX = 1, NP1
        NPOINT = NPOINT + 1
        IF (PT_INDEX.EQ.1) THEN
          READ(IO_UNIT,*,ERR=299,END=299) XTMP0, ZTMP0
        ELSE 
          READ(IO_UNIT,*,ERR=299,END=299) XTMP, ZTMP
        END IF
      END DO
! C
! C Make sure the polygon is "closed", so check the first and last points.
! C
      !START_INDEX = NPOINT - NP1 + 1
      !IF (XVEC(START_INDEX) .NE. XVEC(NPOINT) .OR. &
      !         ZVEC(START_INDEX) .NE. ZVEC(NPOINT)) THEN
      IF (XTMP0 .NE. XTMP .OR. ZTMP0 .NE. ZTMP) THEN
        NPOINT = NPOINT + 1
        NP1 = NP1 + 1
      END IF
    END DO
    CLOSE(IO_UNIT)
    GOTO 300

 199    CONTINUE
    IFLAG = -1
    GOTO 300

 299    CONTINUE
    CLOSE(IO_UNIT)
    IFLAG = -1
    GOTO 300

 300    CONTINUE
    RETURN
End Subroutine screen_external_getdimensions

! C+++
! C	SUBROUTINE	SETSOUR
! C
! C	PURPOSE		This subroutine generates a full source 
! C			specification. A set of useful variables is 
! C			computed here, for use by SOURCE later.
! C
! C	ARGUMENTS	None
! C
! C	OUTPUT		To common blocks.
! C
! C---
SUBROUTINE SETSOUR

!todo implicit?
!    implicit real(kind=skr) (a-e,g-h,o-z)
!    implicit integer(kind=ski)        (f,i-n)

real(kind=skr)    :: q_phot,sinn,sinm,graze,theta_b,deflec
real(kind=skr)    :: sinbeta,theta_t
real(kind=skr)    :: cosx,sinx,cosy,siny,cosz,sinz
real(kind=skr)    :: dumm1=0,zero=0.0,tmp
integer(kind=ski) :: ipsflag,ierr


WRITE(6,*) 'Call to SETSOUR'

! srio@esrf.eu 2012-06-06
! debug (noticed by N. Canestrari) : 
! THETA IS DEFINED ONLY IF F_DEFAULT IS SET (i.e., F_EXT=1), 
! BUT IS ALSO USED in MSETUP THEN F_EXT=0
if (f_ext .eq. 1) theta = t_incidence

IF (F_DEFAULT.EQ.1) THEN
    SSOUR =  T_SOURCE
    SIMAG =  T_IMAGE
    THETA =  T_INCIDENCE
END IF


! C
IF (F_CRYSTAL.EQ.1) THEN
    CALL    CRYSTAL    (0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, &
        0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,i_one)  
    ! C
    ! C Change units and FWHM->S.D. for the mosaic case
    ! C
    !IF (F_MOSAIC.EQ.1) SPREAD_MOS = SPREAD_MOS*TORAD/2.35D0
    !more precisely:
    IF (F_MOSAIC.EQ.1) SPREAD_MOS = SPREAD_MOS*TORAD/(2*sqrt(2*log(2.0d0)))
    ! C
    ! C Define diffraction order for asymmetric case
    ! C
    IF (F_BRAGG_A.EQ.1) THEN
        A_BRAGG    =   TORAD*A_BRAGG
        !*
        !* In the crystal laue case the ORDER value selects the incidence of the
        !* beam: ORDER=-1 (defauls) the beam arrive on the bragg planes. 
        !*       ORDER=+1 the beam arrives under the bragg planes
        !*
        if (f_refrac.ne.1) then
            IF ((A_BRAGG.Le.0.0).or.(a_bragg.ge.pihalf)) ORDER = -1
            IF ((A_BRAGG.Gt.0.0).and.(a_bragg.lt.pihalf)) ORDER = +1
        end if
        F_RULING = 1
    ELSE IF (F_BRAGG_A.NE.1) THEN
        A_BRAGG    =   0.0D0
    END IF
    ! C
    ! C   define flags for crystals in transmission mode (laue geometry)
    ! C   (in the "symmetrical laue case" in fact we work with the
    ! C   asymmetrical with alpha=90 deg.)
    ! C
    if (f_crystal.eq.1.and.f_refrac.eq.1) then        !laue case
        if (f_bragg_a.ne.1) then
          a_bragg = pihalf  
          if(f_mosaic.ne.1) then
            f_bragg_a = 1
            a_bragg = pihalf  
            f_ruling = 1
          end if
        end if
    end if
    ! C
    ! C Define diffraction order for Johansson geometry 
    ! C
    IF (F_JOHANSSON.EQ.1) THEN
        A_BRAGG    =   TORAD*A_BRAGG
        IF (A_BRAGG.LT.0.0) ORDER = -1
        IF (A_BRAGG.GE.0.0) ORDER = +1
        F_BRAGG_A   = 1
        F_RULING    = 5
        F_RUL_ABS   = 1
    END IF
    !C
    !C Sets the mirror at the BRAGG angle.
    !C
    IF (F_CENTRAL.EQ.1.AND.F_CRYSTAL.EQ.1) THEN
        IF (F_PHOT_CENT.EQ.1)   PHOT_CENT = TOANGS/R_LAMBDA
        IF (F_PHOT_CENT.EQ.0)   R_LAMBDA  = TOANGS/PHOT_CENT
        Q_PHOT = TWOPI/R_LAMBDA*1.0D8
        !C
        !C next, compute the Bragg angle without index of refraction corrections
        !C
        GRAZE =  ASIN(R_LAMBDA*0.5D-8/D_SPACING)
        SINN  =  SIN(GRAZE+A_BRAGG)
        SINM  =  SIN(GRAZE-A_BRAGG)
        !C
        !C Compute now the correct Bragg angle (including N)
        !C (only valid for reflection geometry)
        !C
        if (f_refrac.eq.0) then                                  !bragg
           CALL CRYSTAL (Q_PHOT,SINN,SINM,zero,DUMM1,DUMM1,DUMM1, &
                                    DUMM1,DUMM1,DUMM1,DUMM1,THETA_B,ione)
        else if (f_refrac.eq.1) then                             !laue
           if (order.eq.-1) then             ! rays onto bragg planes
              if (a_bragg.ge.0) theta_b = a_bragg + graze
              if (a_bragg.lt.0) theta_b = pi - graze + a_bragg
           else ! rays below bragg planes
              if (a_bragg.ge.0) theta_b = a_bragg - graze
              if (a_bragg.lt.0) theta_b = pi + a_bragg + graze
           end if
        end if
        !C
        T_INCIDENCE = 90.0D0 - THETA_B*TODEG
        !C
        !C           T_REFLECTION = ACOSD(COS(THETA_B)*COSD(2.0*ALFA_ASS)+
        !C     $            SIND(2.0D0*ALFA_ASS)*SQRT((SIN(THETA_B))**2-
        !C     $            2.0D0*DELTA))
        !C      T_REFLECTION = 90.0D0 - T_REFLECTION
    END IF
END IF ! end of crystal
! C
! C Computes the line density at the pole for an holographic grating.
! C
IF (F_RULING.EQ.2) THEN
    CALL HOLO_SET
END IF
! C
! C Changes to SETSOUR for surface roughness calculations
! C
if (f_roughness.eq.1) then
    ipsflag = -1
    ierr = 0
    call pspect (tmp,tmp,ierr,ipsflag, file_rough)
    if (ierr.ne.0) call leave ('Error on return from roughness: PSPECT','SETSOUR',izero)
    if (f_grating.eq.0.and.f_bragg_a.eq.0) f_ruling = 10 
end if
! C
! C The following sequence is necessary now, as some of the angles
! C may be changed.
! C Debugged MSR 9/20/90: the variable RULING must be controlled by F_CRYSTAL
! C
IF (F_CRYSTAL.EQ.1) THEN
    RULING = SIN(ABS(A_BRAGG))/D_SPACING
    IF (F_CENTRAL.EQ.1) THEN
        F_MONO = 2
    ELSE
        F_MONO = -1
    END IF
END IF
! C
! C Auto-tuning of monochromators
! C F_CENTRAL and F_CRYSTAL statement added to get correct
! C reflection angle for crystal, i.e. F_MONO=2
! C 31 May 1990, clw.
! C
IF ((F_CENTRAL.EQ.1.AND.F_GRATING.EQ.1).OR. &
    (F_CENTRAL.EQ.1.AND.F_CRYSTAL.EQ.1)) THEN
    IF (F_PHOT_CENT.EQ.1)   PHOT_CENT = TOANGS/R_LAMBDA
    IF (F_PHOT_CENT.EQ.0)   R_LAMBDA  = TOANGS/PHOT_CENT
    IF (F_MONO.EQ.1) THEN
        ! C
        ! C ERG-GRASSHOPPER case
        ! C
        SINBETA =    ORDER*(TOCM/PHOT_CENT)*RULING  &
            + SIN (T_INCIDENCE*TORAD)
        BETA       =    ASIN( SINBETA )
        T_REFLECTION  =   BETA*TODEG
        IF (F_EXT.EQ.0) THEN
            ! C
            ! C The radius follows from the condition that the object lies on
            ! C the Rowland circle.
            ! C
            THETA_T    =   THETA*TORAD
            RMIRR = SSOUR/COS(THETA_T)
            ! C
            ! C This is to avoid that MSETUP will recompute the radius.
            ! C
            F_EXT =  1
        END IF
        ! C
        ! C Computes the distance to the rowland circle.
        ! C
        T_IMAGE   =   RMIRR*COS(BETA)
    ELSE IF (F_MONO.EQ.0) THEN
        ! C
        ! C TGM-SEYA MOUNT
        ! C DIFFRAC gives the rotation angle for the grating.
        ! C
        CALL      DIFFRAC
        F_MOVE = 1        ! This will rotate the mirror
    ELSE IF (F_MONO.EQ.2) THEN
        ! C          
        ! C constant incidence angle
        ! C
        SINBETA =    ORDER*(TOCM/PHOT_CENT)*RULING  &
            + SIN (T_INCIDENCE*TORAD)
        BETA       =    ASIN( SINBETA )
        T_REFLECTION  =   BETA*TODEG
        ! C
        ! C laue crystals
        ! C
        if (f_refrac.eq.1) then
            if (order.eq.-1)  then
                if (a_bragg.ge.0) &
                    t_reflection=(pihalf+a_bragg-graze)*todeg
                if (a_bragg.lt.0) &
                    t_reflection=(3.0d0*pihalf+a_bragg+graze)*todeg
            else
                if (a_bragg.ge.0) &
                    t_reflection=(pihalf+a_bragg+graze)*todeg
                if (a_bragg.lt.0) &
                    t_reflection=(3.0d0*pihalf+a_bragg-graze)*todeg
            end if
        end if
    ELSE IF (F_MONO.EQ.3) THEN
    ! C
    ! C constant diffraction angle
    ! C
        SINBETA =    ORDER*(TOCM/PHOT_CENT)*RULING  &
            + SIN (T_REFLECTION*TORAD)
        BETA       =    ASIN( SINBETA )
        T_INCIDENCE =   BETA*TODEG
    ELSE IF (F_MONO.EQ.4) THEN
        ! C
        ! C constant blaze mount (Hunter)
        ! C
        BLAZE   =   BLAZE*TORAD
        T_INCIDENCE = BLAZE + &
            ACOS(-ORDER*R_LAMBDA*1.0D-8/2*RULING/SIN(BLAZE))
        T_REFLECTION    =   T_INCIDENCE - 2*BLAZE
        DEFLEC = T_INCIDENCE + T_REFLECTION
        ! C
        ! C This will pivot the gratings around the monochromator center
        ! C
        IF (F_HUNT.EQ.1) THEN
            T_SOURCE =   HUNT_L/2 + HUNT_H/2/TAN(DEFLEC)
            T_IMAGE  =   HUNT_H/2/SIN(DEFLEC)
        ELSE IF (F_HUNT.EQ.2) THEN
            T_SOURCE =   HUNT_H/2/SIN(DEFLEC)
            T_IMAGE  =   HUNT_L/2 + HUNT_H/2/TAN(DEFLEC)
        END IF
        T_INCIDENCE =   T_INCIDENCE*TODEG
        T_REFLECTION=   T_REFLECTION*TODEG
        DEFLEC  =   DEFLEC*TODEG
        ! D           OPEN (25,FILE='BLAZE',STATUS='NEW')
        ! D           WRITE (25,*)    F_HUNT,RULING
        ! D           WRITE (25,*)    HUNT_H,HUNT_L
        ! D           WRITE (25,*)    T_INCIDENCE,T_REFLECTION
        ! D           WRITE (25,*)    T_SOURCE,T_IMAGE
        ! D           WRITE (25,*)    DEFLEC,BLAZE
        ! D           CLOSE (25)
    END IF
    ! C/
    ! C This is necessary as some of the T_XXXX will have been changed
    ! C
    IF (F_DEFAULT.EQ.1) THEN
        SSOUR    =  T_SOURCE
        SIMAG    =  T_IMAGE
        THETA    =  T_INCIDENCE
    END IF
ELSE
END IF ! end autotuning

! D        THETA_IN  =   THETA
THETA     =   TORAD*THETA       !Pass to radians
DELTA     =   PIHALF - THETA
! ** Some useful variables are now computed.
COSTHE    =   COS(THETA)
SINTHE    =   SIN(THETA)
COSDEL    =   COS(DELTA)
SINDEL    =   SIN(DELTA)
! ** THis is necessary now
! D        ALPHA_IN  =   ALPHA
ALPHA      =   ALPHA*TORAD
COSAL      =   COS(ALPHA)
SINAL      =   SIN(ALPHA)
! D        ALPHA_IS  =   ALPHA_S
ALPHA_S      =   ALPHA_S*TORAD
COSAL_S      =   COS(ALPHA_S)
SINAL_S   =   SIN(ALPHA_S)

X_PHASE      =   TORAD*X_PHASE
Y_PHASE      =   TORAD*Y_PHASE

T_INCIDENCE   =   T_INCIDENCE*TORAD
T_REFLECTION  =   T_REFLECTION*TORAD
       
PSOUR(1)  =   .0D0
PSOUR(2)  = - SSOUR*SINTHE
PSOUR(3)  =   SSOUR*COSTHE

! ** Now the 'real' case
IF (FSTAT.EQ.1) THEN ! move the source

    X_SOUR_ROT    =    TORAD*X_SOUR_ROT
    Y_SOUR_ROT    =    TORAD*Y_SOUR_ROT
    Z_SOUR_ROT    =    TORAD*Z_SOUR_ROT
    COSX    =   COS ( X_SOUR_ROT )
    SINX    = - SIN ( X_SOUR_ROT )
    COSY    =   COS ( Y_SOUR_ROT )
    SINY    = - SIN ( Y_SOUR_ROT )
    COSZ    =   COS ( Z_SOUR_ROT )
    SINZ    = - SIN ( Z_SOUR_ROT )

    U_SOUR(1)    =   COSZ*COSY
    V_SOUR(1)    =   COSZ*SINX*SINY - SINZ*COSX
    W_SOUR(1)    =   COSZ*SINY*COSX + SINZ*SINX

    U_SOUR(2)    =   SINZ*COSY
    V_SOUR(2)    =   SINZ*SINX*SINY + COSZ*COSX
    W_SOUR(2)    =   SINZ*SINY*COSX - SINX*COSZ

    U_SOUR(3)    = - SINY
    V_SOUR(3)    =   COSY*SINX
    W_SOUR(3)    =   COSY*COSX


    ! D        RTHETA_IN =   RTHETA

    RTHETA    =   TORAD*RTHETA       !Pass to radians
    RDELTA    =   PIHALF - RTHETA

    ! ** Some useful variables are now computed.

    COSTHR    =   COS(RTHETA)
    SINTHR    =   SIN(RTHETA)
    COSDER    =   COS(RDELTA)
    SINDER    =   SIN(RDELTA)

    PSREAL(1) =   OFF_SOUX
    PSREAL(2) =   OFF_SOUY - RDSOUR*SINTHR
    PSREAL(3) =   OFF_SOUZ + RDSOUR*COSTHR

ELSE ! source does not move
    PSREAL(1) =   0.0D0
    PSREAL(2) = - SIN(T_INCIDENCE)*T_SOURCE
    PSREAL(3) =   COS(T_INCIDENCE)*T_SOURCE
    RTHETA       =   T_INCIDENCE
    RDELTA    =   PIHALF - T_INCIDENCE
    SINTHR    =   SIN(T_INCIDENCE)
    COSTHR    =   COS(T_INCIDENCE)
    COSDER    =   COS(RDELTA)
    SINDER    =   SIN(RDELTA)
    RDSOUR      =   T_SOURCE
END IF

WRITE(6,*) 'Exit from SETSOUR'

End Subroutine setsour

! C+++
! C	SUBROUTINE	SUR_SPLINE
! C
! C	PURPOSE		To compute the interpolated surface from a 
! C			bi-cubic spline.
! C
! C	INPUT		An unformatted file prepared by PRESURFACE.
! C
! C	ARGUMENTS	Input:
! C			 {x,y} 	coordinates
! C			 IFlag: -1, readin file from FILESURF
! C			         0, compute z
! C				-2, clear arrays
! C			Output:
! C			 z	value of z at {x.y}
! C			 v[3]	normal to surface at {x,y,z}
! C			 Iflag:  0, normal completion
! C			        -1, out of bounds
! C			serr: surface spline error (-9 is bad)
! C
! C---
SUBROUTINE SUR_SPLINE (XIN, YIN, ZOUT, VVOUT, IERR, SERR)


! C This routine now takes an additional parameter SERR which indiates
! C whether errors occur when calculating the ray's intersection with the
! C mirror as specified by a PRESURFACE spline file.

! C Below, the CSPL, X, and Y have been changed to allow a maximum of
! C 201 points instead of 101.
! C

	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

     	!DIMENSION	CSPL (2,201,2,201),X(201),Y(201),PDS(6)
     	DIMENSION	X(501),PDS(6)
	real(kind=skr),dimension(:),allocatable :: Y
     	real(kind=skr),dimension(:,:,:,:),allocatable ::  CSPL

	INTEGER(KIND=ski)	SERR
! D	CHARACTER*80	FILE_RIP
!	EXTERNAL	DBCEVL
! C     	DIMENSION	XIN(3),YIN(3),VVIN(3),VVOUT(4)
     	DIMENSION	VVOUT(3)
! C     	DIMENSION	VTX(3),VTY(3)
! C
! C SAVE the variables that need to be saved across subsequent invocations
! C of this subroutine. Note: D_SPACING is not included in the SAVE block
! C because it's included in the COMMON.BLK file.
! C
	SAVE		NX, NY, X, Y, CSPL
! D
! D	FILE_RIP	= 'SURFACE'
! D
	SERR = 0
     	IF (IERR.EQ.-1) THEN
! C 
! C Replace OPEN calls with library routine FOPENR()
! C	  CALL FOPENR(20, FILE_RIP, 'UNFORMATTED', IFERR, IOSTAT)
! C
     	  OPEN  (20, FILE=FILE_RIP, STATUS='OLD', FORM='UNFORMATTED', &
     		IOSTAT=IOSTAT)
	  IF (IOSTAT.NE.0) THEN
	    CALL LEAVE ('SUR_SPLINE', &
     			'Error opening file "' //  &
     			FILE_RIP(1:IBLANK(FILE_RIP)) // '".', &
     			IOSTAT)
          END IF
     	  READ  (20) NX, NY

	  allocate( Y(NY) )
	  allocate( CSPL(2,501,2,NY) )
     	  READ  (20) X,Y
     	  READ  (20) CSPL
     	  CLOSE (20)
! C
! C Succesful completion
! C
! D	  WRITE(6,*)'Read ',NX,' by ',NY,' array.'
     	  IERR = 0
     	 RETURN
     	ELSE IF (IERR.EQ.-2) THEN !deallocate arrays
          IF(ALLOCATED( Y ))    DEALLOCATE(Y)
          IF(ALLOCATED( CSPL )) DEALLOCATE(CSPL)
     	  RETURN
     	ELSE 
! C
! C Compute the spline and derivatives at {x,y}. 
! C Use 501 points maximum instead of 101 as earlier.
! C If there is an error in this calculation, set the SERR variable
! C to -9 to indicate this fact.
! C
! C      	CALL	DBCEVL (X,NX,Y,NY,CSPL,101,XIN,YIN,PDS,IER)
     	CALL	DBCEVL (X,NX,Y,NY,CSPL,i501,XIN,YIN,PDS,IER)
! C
     	IF (IER.NE.0) THEN
    	  IER = -9
	  SERR = IER
! C   The 2 lines below are old stuff.
! C     	  CALL	MSSG ('SURF_SPLINE','Return error # ',IER)
! C     $	  CALL LEAVE ('SURF_SPLINE','Error in Spline Interpolation',IER)
! C
     	  RETURN
     	END IF
     	DSDX	=   PDS(2)
     	DSDY	=   PDS(3)
! C
! C Compute now direction cosines of normal; 
! C
     	VVOUT(1)=   -DSDX
     	VVOUT(2)=   -DSDY
     	VVOUT(3)=   1.0D0
! C
! C     	CALL	NORM	(VVOUT,VVOUT)
! C
! C Clean up
! C
     	ZOUT = PDS(1)
! C
! C All done; return to caller
! C
     	IERR = 0
     	RETURN
     	END IF
End Subroutine sur_spline


! C+++
! C	SUBROUTINE	SURFACE
! C
! C	PURPOSE		To compute the offset in z at (x,y) due to the 
! C			ripple.
! C
! C	Inputs:		
! C
! C---
! ** WARNING : To simplify problems, the sigma are really 1/sigma

SUBROUTINE SURFACE (P_IN,P_DISPL,V_NORMAL,SPLERR)

! C This subroutine has been modified to take the additional parameter 
! C SPLERR to keep track of errors in spline calulations.
        
        implicit none
        !implicit real(kind=skr) (a-e,g-h,o-z)
        !implicit integer(kind=ski)        (f,i-n)

        real(kind=skr),dimension(3),intent(in) ::  P_IN
        real(kind=skr),dimension(3),intent(out)::  P_DISPL,V_NORMAL
        integer(kind=ski),intent(out)          :: SPLERR
        real(kind=skr),dimension(3) ::  V_USE, V_CORR
        real(kind=skr)     :: xin, yin, zout, corr, corr_x, corr_y
        integer(kind=ski)  :: ierr, i

        !DIMENSION P_IN(3),P_DISPL(3),V_NORMAL(3),V_USE(3), V_CORR(3)

        splerr = 0
        IF (F_G_S.EQ.2) THEN
          XIN = P_IN(1)
          YIN = P_IN(2)
          IERR = 0

          ! C  Here SUR_SPLINE now returns a value for SPLERR which indicates whether
          ! C  errors occured when calculating the intersection of the ray with the
          ! C  mirror surface as represented as a spline surface generated by
          ! C  the utility PRESURFACE.

          CALL SUR_SPLINE (XIN, YIN, ZOUT, V_CORR, IERR, SPLERR)
          P_DISPL(1) = P_IN(1)
          P_DISPL(2) = P_IN(2)
          P_DISPL(3) = P_IN(3) + ZOUT
          GO TO 300
        END IF

        ! ** Computes the amplitude of the ripple in P_IN
        IF (F_G_S.EQ.0) THEN     ! Sinusoidal ripples

            IF (X_RIP_WAV.NE.0.D0) THEN
              CORR_X   =   X_RIP_AMP*COS( TWOPI*P_IN(1)/X_RIP_WAV + X_PHASE )
            ELSE
              CORR_X  =   .0D0
            END IF

            IF (Y_RIP_WAV.NE..0D0) THEN
              CORR_Y   =   Y_RIP_AMP*COS( TWOPI*P_IN(2)/Y_RIP_WAV + Y_PHASE )
            ELSE
              CORR_Y =   .0D0
            END IF
            ! C
            ! C Creates the first output vector.
            ! C
            P_DISPL(1) =   P_IN(1)
            P_DISPL(2) =   P_IN(2)
            P_DISPL(3) =   P_IN(3) + CORR_X + CORR_Y

        ELSE    ! Gaussian case

            CORR = 0.0D0

            DO I=1,N_RIP

            CORR =   CORR  &
              + SIGNUM(I)*AMPLI(I)*EXP( - (P_IN(1) - X_GR(I))**2*SIG_X(I)**2 &
              -  (P_IN(2) - Y_GR(I))**2*SIG_Y(I)**2 )
            END DO 

            P_DISPL(1)   =   P_IN(1)
            P_DISPL(2)   =   P_IN(2)
            P_DISPL(3)   =   P_IN(3) + CORR
        END IF

        ! ** Computes now the normal of the ripple. The two expressions cannot
        ! ** be separated in the calculation and must be normalized together, as
        ! **
        ! **  Normal =   GRADIENT ( F(x,y,z) + G(x,y,z) )
        ! **
        ! C
        ! C NOTICE THAT THE NORMALS ARE DEFINED AS IN MIRROR, I.E, UPWARD
        ! C
        IF (F_G_S.EQ.0) THEN
            IF (X_RIP_WAV.NE.0.0D0) THEN
                V_CORR(1)    =  TWOPI*X_RIP_AMP/X_RIP_WAV* &
                SIN( TWOPI*P_IN(1)/X_RIP_WAV + X_PHASE )
            ELSE
                V_CORR(1)    =    0.0D0
            END IF
            IF (Y_RIP_WAV.NE.0.0D0) THEN
                V_CORR(2)    =  TWOPI*Y_RIP_AMP/Y_RIP_WAV* &
                SIN( TWOPI*P_IN(2)/Y_RIP_WAV + Y_PHASE ) 
            ELSE
                V_CORR(2)    =    0.0D0
            END IF

        ELSE
 
            V_CORR(1) = 0.0D0
            V_CORR(2) = 0.0D0
            V_CORR(3) = 0.0D0

            DO I=1,N_RIP
              V_CORR(1) = V_CORR(1) + 2*SIG_X(I)**2*( P_IN(1) - X_GR(I))* &
                SIGNUM(I)*AMPLI(I)*EXP( - SIG_X(I)**2*(P_IN(1) - X_GR(I))**2 &
              - SIG_Y(I)**2*(P_IN(2) - Y_GR(I))**2 )
              V_CORR(2) = V_CORR(2) + 2*SIG_Y(I)**2*( P_IN(2) - Y_GR(I))* &
                SIGNUM(I)*AMPLI(I)*EXP( - SIG_X(I)**2*(P_IN(1) - X_GR(I))**2 &
              - SIG_Y(I)**2*(P_IN(2) - Y_GR(I))**2 )
            END DO 

        END IF
        ! C
        ! C ideal surface gradient
        ! C
300     continue 
        CALL NORMAL (P_IN,V_USE)

        IF (F_CONVEX.EQ.0) CALL SCALAR (V_USE,-1.0D0,V_USE)
        ! added srio@esrf.eu 20131119. Bug notified by Bernd Meyer. 
        if (v_use(3) .eq. 0) then 
            !print *,'SURFACE: Warning: Unable to get interpolated coordinates.'
            splerr = -90
            return
        endif

        V_USE(1) =  V_USE(1)/V_USE(3)
        V_USE(2) =  V_USE(2)/V_USE(3)
 
        V_NORMAL(1) =   V_USE(1) + V_CORR(1)
        V_NORMAL(2) =   V_USE(2) + V_CORR(2)
        V_NORMAL(3) =   1.0D0

        CALL NORM ( V_NORMAL, V_NORMAL)

        RETURN

End Subroutine surface


! C+++
! C	SUBROUTINE	FA_ROT
! C
! C
! C---
SUBROUTINE FA_ROT (P_IN,P_OUT,OFF,AU,AV,AW)

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

        DIMENSION    P_IN(3),P_OUT(3),OFF(3),AU(3),AV(3),AW(3)

     	P_OUT(1)=    (P_IN(1) - OFF(1))*AU(1) + &
     		     (P_IN(2) - OFF(2))*AU(2) + &
     		     (P_IN(3) - OFF(3))*AU(3)

     	P_OUT(2)=    (P_IN(1) - OFF(1))*AV(1) + &
     		     (P_IN(2) - OFF(2))*AV(2) + &
     		     (P_IN(3) - OFF(3))*AV(3)

     	P_OUT(3)=    (P_IN(1) - OFF(1))*AW(1) + &
     		     (P_IN(2) - OFF(2))*AW(2) + &
     		     (P_IN(3) - OFF(3))*AW(3)


        RETURN
End Subroutine fa_rot

! C+++
! C	SUBROUTINE	FA_ROTBACK
! C
! C
! C---
SUBROUTINE FA_ROTBACK (P_IN,P_OUT,OFF,AU,AV,AW)

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

        DIMENSION    P_IN(3),P_OUT(3),OFF(3),AU(3),AV(3),AW(3)



     	P_OUT(1)=    P_IN(1)*AU(1) + &
     		     P_IN(2)*AV(1) + &
     		     P_IN(3)*AW(1) + OFF(1)

     	P_OUT(2)=    P_IN(1)*AU(2) + &
     		     P_IN(2)*AV(2) + &
     		     P_IN(3)*AW(2) + OFF(2)

     	P_OUT(3)=    P_IN(1)*AU(3) + &
     		     P_IN(2)*AV(3) + &
     		     P_IN(3)*AW(3) + OFF(3)

        RETURN
End Subroutine fa_rotback



!
! and now, THE MONSTER!!!!
!

! C+++
! C	SUBROUTINE	MIRROR
! C
! C	PURPOSE 	To compute the intersection and the reflected 
! C			beam on the mirror surface.The results are 
! C			tranferred back to the main program.
! C
! C	ALGORITHM	Several, depending on the function.
! C
! C	INPUT		a) RAY, i.e., beam array
! C			b) data from MSETUP, through the common
! C
! C	OUTPUT		a) RAY
! C			b) MIRRxx, where xx is the OE number
! C			c) RMIRRxx, if the mirror has been moved.
! C			d) PHASExx, phase of the rays
! C---
Subroutine MIRROR1 (RAY,AP,PHASE,I_WHICH)

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

        real(kind=skr),dimension(:,:),intent(in out) :: ray,ap,phase

     	REAL (kind=skr)	K_PAR(3)

	!DIMENSION 	RAY(12,N_DIM),RAY_CALL(7),PHASE(3,N_DIM)
	dimension 	RAY_CALL(7)
     	!DIMENSION	CODLING(12,N_DIM),AP(3,N_DIM)
     	dimension	CODLING(12,npoint)
	DIMENSION 	VVIN(3),PPOUT(3),VTEMP(3),VNOR(3)
	DIMENSION	VNORG(3),VVOUT(3)
     	DIMENSION	P_TRUE(3),P_NORM(3),P_START(3),P_TEMP(3)
     	DIMENSION	GSCATTER(3),Q_IN(3),Q_OUT(3)
     	DIMENSION	AS_VEC(3),AS_TEMP(3),S_VEC(3)
	DIMENSION	AP_VEC(3),AP_TEMP(3)
      	DIMENSION	DIS1(3),DIS2(3),HYPER(3),HYPER1(3),VTAN(3)
     	DIMENSION	VHOLO1(3),VHOLO2(3),STEMP(3)
        !srio: already in implicit
	!REAL*8		RH_DENS
	!INTEGER*4	I_BASELINE,ISEGA,IPRO
	DIMENSION	PF_CENT(3),PF_NOR(3),PF_TAU(3)
	DIMENSION	PF_START(3),PF_VIN(3),PF_BNOR(3)
	DIMENSION	PF_OUT(3),PFNORMAL(3)
	!DIMENSION	ANGLE(4,N_DIM)
	dimension	ANGLE(4,npoint)
	!INTEGER		SURFERR
	integer(kind=ski)  :: SURFERR

        !srio: already in implicit
	!DOUBLE PRECISION	SCAT_FRAC,RGH_TMP1,RGH_TMP2
	!DOUBLE PRECISION	ROUGH_RMS, SIN_VAL 


! C
! C Save some local large arrays to avoid overflowing stack.
! C
! srio danger : commented this one as 
!               automatic variables cannot have the SAVE attribute
!	SAVE		CODLING, ANGLE


	IF (F_KOMA.NE.1) THEN
     	  WRITE(6,*) 'Call to MIRROR'
	END IF

! 
! skip calculations for an empty o.e.
! note that the element is empty, but the optical axes are changed accordingly
! the specified directions.
!
! added srio@esrf.eu 2013-07-29 for implementing MONTEL systems
!
    if (f_refrac.EQ.2) then 
       print *,'MIRROR1: Warning: Empty element (no mirr.xx file)'
       goto 99999
    endif

! C
	IF (F_KOMA.EQ.1.AND.F_DOT.EQ.1) THEN
	  GOTO 5009
	END IF
! C
	XFIRST 	=   RWIDX2
	XSECON 	=   RWIDX1
	YFIRST 	=   RLEN2	! source side
	YSECON 	=   RLEN1	! image  side

     	X_C_P   =   COD_LEN/2   ! codling slit
     	X_C_M   = - COD_LEN/2
     	Z_C_P	=   COD_WID/2
     	Z_C_M   = - COD_WID/2

! C
! C      The counting integers for the roughness calculation
! C
 
        KROUGH_COUNT  = 0
        KROUGH_COUNT2 = 0

     	K_1	=   0
     	K_2	=   0
     	K_3	=   0
     	K_4	=   0
     	K_5	=   0
	K_6	=   0

! C
! C  Initialize variables that are used later (assumed to be initialized).
! C
	G_MOD = 0.0D0
	G_FAC = 0.0D0
! C
! C  JOHANSSON SETUP 
! C
        IF (F_JOHANSSON.EQ.1) THEN
                 A_BRAGG_OLD = A_BRAGG
                 ORDER_OLD   = ORDER
                 G_FAC       =   1.0D0
        END IF
! C
! C
! C
     	IF (F_GRATING.EQ.1.OR.F_BRAGG_A.EQ.1) THEN
	  IF (F_RULING.EQ.0.OR.F_RULING.EQ.1) THEN
     		G_MOD	=   TWOPI*RULING*ORDER
		G_FAC	=   1.0D0
     	  ELSE IF (F_RULING.EQ.2) THEN
     		DO 199 I=1,3
     		 DIS1(I) = HOLO1(I)
     		 DIS2(I) = HOLO2(I)
 199		CONTINUE

! srio danger!!
! changed this, as Z_VERS does not exist. 
!     		CALL  CROSS (HOLO1,Z_VERS,VHOLO1)
!     		CALL  CROSS (HOLO2,Z_VERS,VHOLO2)
     		CALL  CROSS (HOLO1,Z_VRS,VHOLO1)
     		CALL  CROSS (HOLO2,Z_VRS,VHOLO2)

     		CALL  NORM (VHOLO1,VHOLO1)
     		CALL  NORM (VHOLO2,VHOLO2)
	  ELSE
	  END IF
     	END IF
     	  EFF_REF 	= 0.0D0
     	  EFF_REF_S 	= 0.0D0
     	  EFF_REF_P	= 0.0D0
     	IF (F_REFLEC.NE.0.AND.F_CRYSTAL.EQ.0)  THEN
     	  CALL REFLEC  &
     		(PPOUT,0.0D0,0.0D0,0.0D0,R_P,R_S,PHASEP,PHASES,ABSOR,izero)
! D
! D	  OPEN	(27,FILE='REPHASE',STATUS='NEW')	
! D
     	END IF
        ! read lens optical constants
        if ((f_refrac.eq.1).and.(f_crystal.eq.0).and.(f_r_ind.gt.0)) then
            call get_refraction_index (izero,0d0,tmp,tmp,tmp,tmp)
        end if
! C
! C If no reflectivity options are selected for crystal case, full polarization
! C dependence is assumed.
! C
     	IF (F_CRYSTAL.EQ.1.AND.F_REFLEC.EQ.0)  F_REFLEC = 1
	
! C We rotate now the 'source' as specified by the mirror rotations.

     	IF (F_MOVE.EQ.1) CALL ROT_FOR (RAY,AP)
! D
! D
! D	OPEN (23,FILE='RULING',STATUS='NEW')

! C
! C Return to Kumakhov if initializing, entrance point for subsequent
! C calls to MIRROR by KUMAKHOV
! C
	IF (F_KOMA.EQ.1) THEN
	   RETURN
	END IF
5009    CONTINUE
! C
! C Start the loop through the beam
! C
	DO 1099 ITIK=1,NPOINT


if (check_perp_comp( ray(4,itik), ray(5,itik), ray(6,itik), &
                     ray(7,itik), ray(8,itik), ray(9,itik), &
                     ap(1,itik), ap(2,itik), ap(3,itik)) .gt. 1d-6) then 
   print*,'MIRROR: Warning (before calculations): lack of perpendicularity in ray: ', itik
end if


! C
! C Counts the "good" and "bad" rays
! C
     	IF (RAY(10,ITIK).LT.0.0D0)	K_1 = K_1 + 1
     	IF (RAY(10,ITIK).GE.0.0D0)	K_2 = K_2 + 1
! * Check if the ray is acceptable
	IF (RAY(10,ITIK).LT.-1.0D6) 	GO TO 10000
! C
     	P_START(1)  =   RAY(1,ITIK)
     	P_START(2)  =   RAY(2,ITIK)
     	P_START(3)  =   RAY(3,ITIK)
	 VVIN(1)	 =   RAY(4,ITIK)
	 VVIN(2)  =   RAY(5,ITIK)
	 VVIN(3)  =   RAY(6,ITIK)
     	  AS_VEC(1) =   RAY(7,ITIK)
     	  AS_VEC(2) =   RAY(8,ITIK)
     	  AS_VEC(3) =   RAY(9,ITIK)
	   AP_VEC(1) =   AP(1,ITIK)
	   AP_VEC(2) =   AP(2,ITIK)
	   AP_VEC(3) =   AP(3,ITIK)
! C
! C  Check if it is segment mirror case
! C
        if(F_SEGMENT.EQ.1) then
        
         if(itik.eq.1) then
! Csrio           call segment_calc(vvin,p_start,itik,ppout,vnor,isega,tpar)
	      CALL LEAVE &
      ('SEGMENT','Not yet implemented in Shadow3',izero)

           ierr = isega
           IF    (ierr.NE.1) CALL LEAVE ('Mirror','Error ',IERR)
         end if
 
         ipro = 5 
         goto 717
        end if

! C
! C solve for intercepts
! C
	IFLAG	= 1
     	CALL	INTERCEPT (P_START, VVIN, TPAR, IFLAG)
! C
! C tests for return
! C
     	IF (IFLAG.GE.0) THEN
	  PPOUT(1) = P_START(1) + VVIN(1)*TPAR
	  PPOUT(2) = P_START(2) + VVIN(2)*TPAR
	  PPOUT(3) = P_START(3) + VVIN(3)*TPAR
     	ELSE
     	 DO 299 I_DEL=1,11
 299       RAY (I_DEL,ITIK) = 0.0D0
     	 RAY (10, ITIK) = -1.1D6*I_WHICH
	 I_KOMA = -1
     	 GO TO 10000
     	END IF
! C
! C Intercepts completed; proceed
! C
! C tests if surface errors are defined
! C
        IF (F_RIPPLE.EQ.1) THEN
! C
! C The case of a rippled surface is solved by successive approximations
! C Due to the small amplitudes involved, few iterations are enough.
! C
! C THE subroutine SURFACE has been modified to take another parameter. It
! C is now called with SURFERR as well. If a SURFERR of -9 is passed back 
! C from the SURFACE, the ray is tagged as lost.  This is because the -9 
! C error indicates that the intersection of the ray with the mirror 
! C could not be calculated correctly. That can occur at the mirror periphery 
! C when the mirror contour is specified as a spline error surface generated 
! C by the utility PRESURFACE and results in errant rays which appear to be
! C reflected from the base mirror figure (e.g. plane), *not* the error 
! C surface specified.

        !DO 300 I=1,3
        DO I=1,3
           CALL SURFACE (PPOUT,P_TRUE,VTEMP,SURFERR)
           ! srio@esrf.eu changed 20131120
           !IF (SURFERR.EQ.-9) THEN
           IF (SURFERR.LT.0) THEN
! C	   	WRITE(6,*)'SURFERR = ',SURFERR
                if (shadow3_verbose .eq. 1) then 
                    print *,"MIRROR: Warning: Unable to get interpolated coordinates."
                    print *,"                 ray number: ",itik," , Error code: ",surferr
                end if 
                !RAY(10,ITIK) = -9
                RAY(10,ITIK) = SURFERR
                go to 301
           END IF
! C
! C Evaluate now the intersection of the incoming beam with a plane 
! C tangent to the TRUE surface in P_TRUE.
! C
           CALL  DOT   ( VTEMP, VVIN, T_1)
           CALL  VECTOR ( P_START, P_TRUE, P_TEMP)
           CALL  DOT ( P_TEMP, VTEMP, T_2)
! C
           TPAR = T_2/T_1
! C
           STEMP(1) =   P_START(1) + TPAR*VVIN(1) ! 1st approx
           STEMP(2) =   P_START(2) + TPAR*VVIN(2) !
           STEMP(3) =   P_START(3) + TPAR*VVIN(3) !
! C
! C Computes ideal surface position above former approximation
! C
          IF (I.NE.3) THEN
            IFLAG = -1
            CALL INTERCEPT ( STEMP, Z_VRS, T_2, IFLAG)
            PPOUT(1)  =   STEMP(1)
            PPOUT(2)  =   STEMP(2)
            PPOUT(3)  =   STEMP(3) + T_2
          END IF
        END DO 
!300	   CONTINUE

        PPOUT(1) = STEMP(1)
         PPOUT(2) = STEMP(2)
          PPOUT(3) = STEMP(3)
        VNOR(1)  = VTEMP(1)
         VNOR(2)  = VTEMP(2)
          VNOR(3)  = VTEMP(3)
        CALL NORM     (VNOR, VNOR)
        ELSE   !non-ripple
! C
! C Computes the normal for the ideal surface. The normal (gradient) is 
! C defined as 'outward' for a concave surface, in our case it will be 
! C directed aloneg -Z (down). The formulae we need use however a normal
! C along the +Z direction. So:
! C
! C               from   NORMAL           set to:
! C              /            \
! C           concave        convex       result (always)
! C
! C            -Z             +Z            +Z
! C
! C                                         -Z   for a refractor
! C
          CALL NORMAL (PPOUT,VNOR)
          CALL NORM (VNOR, VNOR)
! C
! C Then the following test will insure that the normal is always UPWARD
! C
          IF (F_CONVEX.EQ.0)  CALL SCALAR (VNOR,-1.0D0,VNOR)
        END IF  !ripple/non-ripple

301     continue
! C
! C COMPUTES THE PHASE
! C
	IF (F_KOMA.EQ.1) THEN
	  CALL DOT(VVIN,VNOR,TEMP)
	  IF (TEMP.GT.0.0D0) THEN
	    CALL SCALAR(VNOR,-1.0D0,VNOR)
	  END IF
	END IF

	IF (F_FACET.NE.1.AND.F_KOMA.NE.1.AND.F_SEGMENT.NE.1) THEN
          ! changed srio@esrf.eu 2012-01-26
          ! the optical path must not be affected by the refraction index 
          ! if refractor is not set
          ! srio@esrf.eu 2012-06-06 removed ABS(TPAR)
          IF (F_REFRAC.EQ.0) THEN 
            PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR
          ELSE
            PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR*R_IND_OBJ
          END IF
     	  !PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR*R_IND_OBJ
	ELSE IF (F_KOMA.EQ.1) THEN
	  IF (VVIN(3).EQ.0.0) THEN
	    DO 995 I_DEL=1,11
	      RAY(I_DEL,ITIK) = 0.0D0
995	    CONTINUE
	    RAY (10,ITIK) = -1.1D6*I_WHICH
	    I_KOMA = -1
	    GOTO 10000
	  ELSE
	    TEMPKO = -1.0D0*PPOUT(3)

	    IF (TEMPKO.GE.ZKO_LENGTH) THEN
	       KOXX = 1
	       GOTO 10000
	    ELSE
               ! changed srio@esrf.eu 2012-01-26
               ! the optical path must not be affected by the refraction 
               ! index if refractor is not set
               ! srio@esrf.eu 2012-06-06 removes ABS(tpar)
               IF (F_REFRAC.EQ.0) THEN 
                 PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR
               ELSE 
                 if (f_crystal .eq. 0) then
                   PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR*R_IND_OBJ
                 else ! laue crystals
                   PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR
                 endif
               END IF
	       !PHASE(1,ITIK) = PHASE(1,ITIK) + TPAR*R_IND_OBJ
	    END IF
	  END IF
! C	ELSE

	END IF

! C
! C Project the incoming ray VVIN onto the normal; 
! C
     	IF (F_REFRAC.NE.0)  CALL SCALAR (VNOR,-1.0D0,VNOR)
! CSrio 
! Csriosrio     	IF (FMIRR.EQ.7)  CALL SCALAR (VNOR,-1.0D0,VNOR)
! CSrio For hyperbolic Laue crystals
! Csrio	write(13,*) ">>>VNOR: ",VNOR
!	write(13,*) ">>>PPOUT: ",PPOUT
! Csrio	CALL PROJ (VVIN,VNOR,VTEMP)


! TODO: For hyperbolic Laue crystals this may be commented
	CALL PROJ (VVIN,VNOR,VTEMP)
! C
! C Stores intercepts:
! C
	RAY(1,ITIK) 	= PPOUT(1)
	RAY(2,ITIK) 	= PPOUT(2)
	RAY(3,ITIK) 	= PPOUT(3)
! C
! C  Check to see if surface roughness has been turned on.  If so, if
! C  the ray is to be scattered rather than just be specularly reflected,
! C  set the flag F_SCATTER_ROUGH=1.
! C

!srio
        f_scatter_rough = 0 

	IF (F_ROUGHNESS.EQ.1) THEN
! C
! C  Calculate the root mean square roughness along the direction of the ray.
! C
	 CALL VECTOR(VTEMP,VVIN,K_PAR)
	 CALL NORM  (K_PAR,K_PAR)
	 IF (ABS(K_PAR(1)).LT.1D-37.AND.ABS(K_PAR(2)).LT.1D-37)  &
        THEN                                          !NORMAL INCIDENCE
	  ROUGH_RMS = SQRT(ROUGH_X**2+ROUGH_Y**2)/SQRT(2.0D0)
	 ELSE

	  ROUGH_RMS = SQRT((K_PAR(1)*ROUGH_X)**2 &
                    +(K_PAR(2)*ROUGH_Y)**2)
	 END IF
! C
! C  Calculate SCAT_FRAC, which gives the fraction of light that is
! C  scattered. 
! C
	 CALL DOT(VVIN,VNOR,SIN_VAL)      ! SINE (GRAZING ANGLE)

! C The expression for UNDSIGMA was previously used for SIGMAPHA, which
! C has now been replaced by SCAT_FRAC, a more meaningful name.  There
! C was no theoretical justification for this expression and it yielded
! C too small a number for the fraction of rays that were scattered.
! C
! C	 UNDSIGMA = 4*ROUGH_RMS*SIN_VAL/(TWOPI*1.0D8/RAY(11,ITIK))
! C	 UNDSIGMA = UNDSIGMA**2

! C Below is the new expression for SCAT_FRAC.  It uses the DeBye Waller
! C factor to determine the fraction of rays scattered. That expression
! C is as follows:  
! C
! C R_scat/R_tot = 1 - Exp(-(4*Pi*sigma_rms*cos(inc_ang)/lambda)^2)
! C
! C Shadow uses the wave number 2*Pi/lambda in cm^-1 so the
! C expression was rewritten as below.

	 RGH_TMP1 = 2*ROUGH_RMS*SIN_VAL*(RAY(11,ITIK)/1.0D8)
	 RGH_TMP2 = RGH_TMP1**2
	 SCAT_FRAC = 1-EXP(-1*RGH_TMP2)

! C For each ray, if a toss of the dice (with output values between 0 and 1) 
! C returns a value greater than SCAT_FRAC, then the ray is reflected 
! C specularly. If it yields a value less than or equal to SCAT_FRAC, the 
! C ray is scattered.

	 IF (WRAN(ISTAR1).GT.SCAT_FRAC) THEN
	  F_SCATTER_ROUGH = 0                ! SPECULAR REFLECTION
	 ELSE 
	  F_SCATTER_ROUGH = 1                ! SCATTERED LIGHT
	 END IF
	END IF
! C
     	IF (F_REFRAC.EQ.0) THEN
! C
! C Reflection Case; 
! C
	  RAY(4,ITIK) 	= VVIN(1) - 2*VTEMP(1)
	  RAY(5,ITIK) 	= VVIN(2) - 2*VTEMP(2)
	  RAY(6,ITIK) 	= VVIN(3) - 2*VTEMP(3)
 
     	ELSE
! C
! C Refraction case; the normal is NOT inverted as it is defined as 
! C 'outward' from the surface.
! C
! C
! C If the k-number is not specified, assume a default of 10000 cm-1.
! C
	  if (f_crystal.ne.1) then        !skip this part when laue crystals
	  IF (RAY(11,ITIK).NE.0.0D0) THEN
	    Q_IN_MOD	=   RAY(11,ITIK)
	  ELSE
	    Q_IN_MOD	=   10000
	  END IF

          ! this call returns rr_ind_* = r_ind_* if F_R_IND=0, otherwise it interpolates
          call get_refraction_index(ione,Q_IN_MOD,rr_ind_obj,rr_attenuation_obj,&
                                                  rr_ind_ima,rr_attenuation_ima)
          !recalculate ALFA as a function of the energy
          rr_alfa = rr_ind_ima/rr_ind_obj
!if (itik.le.10) print *,'>>>0 alfa_old,alfa_new,new_obj,new_ima: ',alfa,rr_alfa,rr_ind_obj,rr_ind_ima
 
     	  CALL SCALAR	(VVIN,Q_IN_MOD,Q_IN)
     	  CALL PROJ	(Q_IN,VNOR,VTEMP)
     	  CALL VECTOR 	(VTEMP,Q_IN,K_PAR)
	  CALL DOT	(VTEMP,VTEMP,Q1_PER)
	  CALL DOT 	(K_PAR,K_PAR,Q_PAR)
	  !Q2_PER	= ALFA**2*Q1_PER + (ALFA**2-1)*Q_PAR
	  Q2_PER	= rr_ALFA**2*Q1_PER + (rr_ALFA**2-1)*Q_PAR
	  CALL SCALAR	(VTEMP,SQRT(Q2_PER/Q1_PER),VTEMP)
	  CALL SUM	(VTEMP,K_PAR,Q_OUT)
	  CALL DOT	(Q_OUT,Q_OUT,TTEMP)
	  CALL NORM	(Q_OUT,VTEMP)

! 	  IF (RAY(11,ITIK).NE.0.0D0)  RAY(11,ITIK)  =   SQRT (TTEMP)
!     	  RAY(4,ITIK)	=   VTEMP(1)
!     	  RAY(5,ITIK)	=   VTEMP(2)
!     	  RAY(6,ITIK)	=   VTEMP(3)

          ! check for no solution (total reflectivity regime)
          ! added srio@esrf.eu 2011-12-08
          if (isnan(SQRT (TTEMP)) ) then
             RAY (10, ITIK) = -123456  !flag for imaginary solution
          else 
            IF (RAY(11,ITIK).NE.0.0D0)  RAY(11,ITIK)  =   SQRT (TTEMP)
          end if
          if (isnan(vtemp(1)) ) then
!print *,'itik,ttemp: ',itik,ttemp,vtemp
            RAY (10, ITIK) = -123456  !flag for imaginary solution
            RAY(4,ITIK) =   0.0D0 ! imaginary solution, arbitrary direction
          else 
            RAY(4,ITIK) =   VTEMP(1)
          end if
          if (isnan(vtemp(2)) ) then
            RAY (10, ITIK) = -123456  !flag for imaginary solution
            RAY(5,ITIK) =   1.0D0 ! imaginary solution, arbitrary direction
          else
            RAY(5,ITIK) =   VTEMP(2)
          end if
          if (isnan(vtemp(3)) ) then
            RAY (10, ITIK) = -123456  !flag for imaginary solution
            RAY(6,ITIK) =   0.0D0 ! imaginary solution, arbitrary direction
          else
            RAY(6,ITIK) =   VTEMP(3)
          end if


     	END IF
	  end if
! C
! C Check if the intercept is within the mirror limits. If not, the
! C ray will be assumed to be lost forever and not used again in any
! C calculation.
! C
     	IF (FHIT_C.EQ.1) THEN
     	 IF (FSHAPE.EQ.1) THEN
	  TESTX 	= (PPOUT(1) - XFIRST)*(XSECON - PPOUT(1))
	  TESTY 	= (PPOUT(2) - YFIRST)*(YSECON - PPOUT(2))
     	 ELSE 	IF (FSHAPE.EQ.2) THEN
     	  TESTX	= PPOUT(1)**2/RWIDX2**2 + PPOUT(2)**2/RLEN2**2  - 1.0D0
     	  TESTX	= - TESTX
     	  TESTY	= TESTX
     	 ELSE 	IF (FSHAPE.EQ.3) THEN
     	  TESTX	= PPOUT(1)**2/RWIDX2**2 + PPOUT(2)**2/RLEN2**2 - 1.0D0
     	  TESTX = - TESTX
     	  TESTY	= PPOUT(1)**2/RWIDX1**2 + PPOUT(2)**2/RLEN1**2 - 1.0D0
     	 END IF
     	ELSE
     	 TESTX	= 1.0D0
     	 TESTY	= 1.0D0
     	END IF

	IF (TESTX.LT.(0.0).OR.TESTY.LT.(0.0)) THEN
	 RAY (10,ITIK)	= - 1.1D4*I_WHICH ! Beam out of the mirror limits.
	END IF
! C
! C mosaic crystal calculation
! C
        IF (F_MOSAIC.EQ.1) THEN
            WAVEN  =   RAY(11,ITIK)
            if (f_refrac.eq.0) then                     !bragg
                CALL MOSAIC (VVIN,VNOR,WAVEN,VNORG)
            else if (f_refrac.eq.1) then                !laue
                call rotvector (vnor,x_vrs,pihalf,vtemp)
                call mosaic (vvin,vtemp,waven,vnorg)
            end if
            CALL PROJ (VVIN,VNORG,VTEMP)
            DO I=1,3
                VVOUT(I) = VVIN(I) - 2*VTEMP(I)
            END DO
            RAY(4,ITIK)  = VVOUT(1)
            RAY(5,ITIK)  = VVOUT(2)
            RAY(6,ITIK)  = VVOUT(3)
        END IF

! C
! C This is the loop that handles the facets case
	IF (F_FACET.EQ.1) THEN
! C
! C Find the index of the facet
! C We use double precision to IDNINT to find the nearest integer
! C Find the index of the facet
! C

	IFAC_X  = IDNINT(PPOUT(1)/RFAC_LENX)
	IFAC_Y  = IDNINT(PPOUT(2)/RFAC_LENY)
	PF_CENT(1)= IFAC_X*RFAC_LENX
	PF_CENT(2)= IFAC_Y*RFAC_LENY

! C
! C Determine the center coordinate of the facet
! C and the new coordinate system
! C
	PF_CENT(3)=0.0D0

! C
! C Seperate the Torodial baselin case from
! C the other baseline
! C

	IF (FMIRR.EQ.3) THEN
	 IF (F_TORUS.EQ.0) THEN
	    Z_VRS(3)=-1.0D0
            IFLAG=1
! C
! C  note: here we have to reset F_TORUS = 3 in order to
! C  find the proper distance from the z=0 plane to the
! C  baseline mirror. i.e. At F_TORUS = 0 we calculate
! C  the farthest distance while at F_TORUS = 3 we get
! C  the closest distance.
! C
	    FRUS = F_TORUS
	    F_TORUS = 3
	    CALL INTERCEPT (PF_CENT,Z_VRS,TPAR,IFLAG)
	      F_TORUS=FRUS
	      Z_VRS(3)=1.0D0
          IF (IFLAG.NE.0) THEN
! C 6/5/93
! C g.j.
! C
         DO 295 I_DEL=1,11
 295       RAY (I_DEL,ITIK) = 0.0D0
         RAY (10, ITIK) = -1.1D6*I_WHICH
         goto 10000
         END IF

	 ELSE
	      CALL LEAVE ('FACET','This part has not been considered yet',izero)
         END IF
! C
! C For the base line other than Tordial baseline
! C
	ELSE
	  IFLAG=-1
! C
! C Set the IFLAG = -1 for the closest intercepted length
! C

	      CALL INTERCEPT (PF_CENT,Z_VRS,TPAR,IFLAG)
	      IF (IFLAG.NE.0) THEN
! C 6/5/93
! C g.j.
! C
         DO 297 I_DEL=1,11
 297       RAY (I_DEL,ITIK) = 0.0D0
         RAY (10, ITIK) = -1.1D6*I_WHICH
         goto 10000
         END IF

        END IF
	    PF_CENT(3) = TPAR


! C Define the normal vector for the facet mirror
! C
	      CALL    NORMAL (PF_CENT,PF_NOR)
	      IF (F_CONVEX.EQ.0) CALL SCALAR (PF_NOR,-1.0D0,PF_NOR) 
	      CALL    NORM   (PF_NOR,PF_NOR)
! C
! C Define binormal and tangential vectors
! C
              PF_BNOR(2)=0.0D0
	      IF (PF_NOR(3).EQ.0) CALL LEAVE &
                    ('FACET','N_z should not be zero',izero)
	    IF (PF_NOR(1).NE.0.) THEN
	       PF_BNOR(1)=PF_NOR(3)
	       PF_BNOR(3)=-1.0D0*PF_NOR(1)
	    ELSE
	       PF_BNOR(1)=1.0D0
	       PF_BNOR(3)=0.0D0
	    END IF
	    CALL NORM(PF_BNOR,PF_BNOR)
	       CALL CROSS (PF_NOR,PF_BNOR,PF_TAU)
	       CALL NORM (PF_TAU,PF_TAU)

! C
! C Local refence frame has been set
! C
! C Rotate and translate the incident ray to the
! C new coordinate system
! C
! C The PF_START will be the new coordinate for the ray and
! C the PF_VIN the new direction.
! C

	CALL    FA_ROT (P_START,PF_START,PF_CENT,PF_BNOR,PF_TAU,PF_NOR)
	CALL    FA_ROT (VVIN,PF_VIN,ORIGIN,PF_BNOR,PF_TAU,PF_NOR)

! C
! C Find the intercept point of the facet surface
! C

	I_BASELINE=FMIRR
	FMIRR=9
	IFLAG=1
	      CALL    SPOLY  (PF_START,PF_VIN,TPAR,IFLAG)
	      IF (IFLAG.EQ.-1) THEN
	        FMIRR=I_BASELINE
! C 6/5/93
! C g.j.
! C
              DO 293 I_DEL=1,11
 293          RAY (I_DEL,ITIK) = 0.0D0
              RAY (10, ITIK) = -1.1D6*I_WHICH
              goto 10000
	      END IF

        ! changed srio@esrf.eu 2012-01-26
        ! the optical path is always positive (use ABS(TPAR)) and 
        ! must not be affected by the refraction index if refractor
        ! is not set
        IF (F_REFRAC.EQ.0) THEN 
          PHASE (1,ITIK) = PHASE(1,ITIK) + ABS(TPAR)
        ELSE 
          PHASE (1,ITIK) = PHASE(1,ITIK) + ABS(TPAR)*R_IND_OBJ
        END IF
	!PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR*R_IND_OBJ

	PF_OUT(1)=PF_START(1) + PF_VIN(1) * TPAR
	PF_OUT(2)=PF_START(2) + PF_VIN(2) * TPAR
	PF_OUT(3)=PF_START(3) + PF_VIN(3) * TPAR

! C
! C Find the normal vector on the intercepted point
! C

	CALL    NORMAL (PF_OUT,PFNORMAL)
	CALL    NORM   (PFNORMAL,PFNORMAL)

! C
! C Rotate and translate the normal vector and the intercepted
! C point back to the baseline frame system
! C
          IF (F_ANGLE.EQ.1) THEN
            CALL DOT(PF_VIN, PFNORMAL, TEMP)
            CALL DOT(PF_OUT, PFNORMAL, TEMP1)
!srio            ANGLE_IN = DACOSD(TEMP)
!srio            ANGLE_OUT = DACOSD(TEMP1)
            ANGLE_IN = TODEG*ACOS(TEMP)
            ANGLE_OUT = TODEG*ACOS(TEMP1)
!write(*,*) ">>>> in mirror: angle_in,angle_out: ",ANGLE_IN,ANGLE_OUT
          END IF

	CALL    FA_ROTBACK (PF_OUT,PPOUT,PF_CENT,PF_BNOR,PF_TAU,PF_NOR)
	CALL    FA_ROTBACK (PFNORMAL,VNOR,ORIGIN,PF_BNOR,PF_TAU,PF_NOR)

	CALL    PROJ (VVIN,VNOR,VTEMP)

        RAY(1,ITIK) = PPOUT(1)
	RAY(2,ITIK) = PPOUT(2)
	RAY(3,ITIK) = PPOUT(3)
	RAY(4,ITIK) = VVIN(1) - 2.0D0*VTEMP(1)
	RAY(5,ITIK) = VVIN(2) - 2.0D0*VTEMP(2)
	RAY(6,ITIK) = VVIN(3) - 2.0D0*VTEMP(3)

	FMIRR = I_BASELINE

	END IF    ! END OF FACET IF

! C
! C End of the facet calculation
! C

! C
! C begin of the segment mirror operation
! C       G.J. 4/19/93
! C
 
717     continue
 
        IF (F_SEGMENT.EQ.1)  then
        
! C isega flag of the segment calculation
! C
        isega = 0
 
! Csrio         call segment_calc(vvin,p_start,ipro,ppout,vnor,isega,tpar)
	      CALL LEAVE ('SEGMENT','Not yet implemented in Shadow3',izero)
 
        if(isega.ne.1)  then
         DO 296 I_DEL=1,11
296       RAY (I_DEL,ITIK) = 0.0D0
         RAY (10, ITIK) = -1.1D6*I_WHICH
        goto 10000
        end if
 
          IF (F_ANGLE.EQ.1) THEN
 
            VVOUT(1) = VVIN(1) - 2.0D0*VTEMP(1)
            VVOUT(2) = VVIN(2) - 2.0D0*VTEMP(2)
            VVOUT(3) = VVIN(3) - 2.0D0*VTEMP(3)
 
          END IF
 
        ! changed srio@esrf.eu 2012-01-26
        ! the optical path is always positive (use ABS(TPAR)) and 
        ! must not be affected by the refraction index if refractor
        ! is not set
        IF (F_REFRAC.EQ.0) THEN 
          PHASE (1,ITIK) = PHASE(1,ITIK) + ABS(TPAR)
        ELSE 
          PHASE (1,ITIK) = PHASE(1,ITIK) + ABS(TPAR)*R_IND_OBJ
        END IF
        !PHASE (1,ITIK) = PHASE(1,ITIK) + TPAR*R_IND_OBJ

        CALL    PROJ (VVIN,VNOR,VTEMP)
 
        RAY(1,ITIK) = PPOUT(1)
        RAY(2,ITIK) = PPOUT(2)
        RAY(3,ITIK) = PPOUT(3)
        RAY(4,ITIK) = VVIN(1) - 2.0D0*VTEMP(1)
        RAY(5,ITIK) = VVIN(2) - 2.0D0*VTEMP(2)
        RAY(6,ITIK) = VVIN(3) - 2.0D0*VTEMP(3)

        end if
! C
! C end of segment calculation
! C


     	  IF (F_GRATING.EQ.1.OR.F_BRAGG_A.EQ.1.OR.F_SCATTER_ROUGH.EQ.1) THEN
! C Grating case. Computes the diffracted beam **
! C Scattering vector. First we bring the normal upward, as this will be
! C useful. This is not done for a convex grating.
! C
! C     	IF (F_CONVEX.EQ.0)	CALL SCALAR (VNOR,-1.0D0,VNOR)
! C
! C Gscatter, the G vector used for scattering the ray, will be then
! C tangent to the surface. However, the ruling density will change if 
! C the ruling is uniform in the X-Y plane. This is the first case.
! C The second case is probably unrealistic, as it refers to a constant
! C ruling density on the grating surface. This could correspond to the
! C case of a bent plane grating.
! C The third is the case of an holographic grating.
! C These differences are taken care by the term G_FAC. It modifies the
! C surface line density at the intercept point by computing the angle
! C between the tangent plane and the basal plane at that point along Y.
! C This is correct indipendent of the grating figure since the angle is
! C obtained from the NORMAL (VNOR) and the Y versor (Y_VRS).
! C
! C The grating calculation is also used for asymmetrically cut 
! C crystals in the conventional mode (f_ruling=1) or in the 
! C Johansson mounting (f_ruling=5). If surface roughness is consider
! C the program uses the grating model to calculate the diffarction
! C with a stochastic diffracting vector
! C
     	IF (F_RULING.EQ.0) THEN
     		CALL	CROSS	(VNOR,X_VRS,VTAN)
     		CALL	NORM	(VTAN,VTAN)
     		CALL	DOT	(VNOR,Y_VRS,G_FAC)
     		G_FAC	=   SQRT (1.0D0 - G_FAC**2)
     		G_MODR	=   G_MOD*G_FAC
! C
! C This is the first implementation of the phase shift (m*N*lamda) due to 
! C grating diffraction, as it is the simplest one.
! C
		IF (ORDER.NE.0.AND.RAY(11,ITIK).NE.0) &
     PHASE(1,ITIK)	= PHASE(1,ITIK) + PPOUT(2)*G_MOD/RAY(11,ITIK)
	ELSE IF (F_RULING.EQ.1) THEN
     		CALL	CROSS	(VNOR,X_VRS,VTAN)
     		CALL	NORM	(VTAN,VTAN)
     		G_MODR	=   G_MOD
	ELSE IF (F_RULING.EQ.5) THEN
            !--------------------
            ! Ruben added for VLS grating phase shift
            IF (F_RUL_ABS.EQ.0) then
                DIST = ABS(PPOUT(2))
            else
                DIST = PPOUT(2)
            end If
            PHASE(1,ITIK) = PHASE(1,ITIK) + TWOPI*ORDER*PPOUT(2)*( Ruling &
                + DIST * RUL_A1 &
                + DIST**2 * RUL_A2 &
                + DIST**3 * RUL_A3)/RAY(11,ITIK)
            ! End Ruben added for VLS grating
            !--------------------

     		CALL	CROSS	(VNOR,X_VRS,VTAN)
     		CALL	NORM	(VTAN,VTAN)
! C
! C The case of the Johansson crystal is implemented by computing a local
! C surface line density on the basis of the d_spacing and of the local asymmetry
! C angle.
! C
	  IF (F_JOHANSSON.EQ.1) THEN
! *
! * arc fom origin to intercept point on the YZ plane
! *
	        IF (FMIRR.EQ.1) THEN                        !Spherical case
 	         ARC = RMIRR * ASIN (PPOUT(2)/RMIRR)
	        ELSE IF (FMIRR.EQ.5) THEN                   !Plane case
	         ARC = SQRT (PPOUT(2)**2 + PPOUT(3)**2)
	         IF (PPOUT(2).LT.0) ARC=-ARC
! * Add the toroidal case 94/11/23 * New part!!!! */
! C Written by MSR, added CLW 19 dec 1994.
                ELSE IF (FMIRR.EQ.3) THEN                   !Torus case
                 ARC = (R_MAJ + R_MIN) * ASIN (PPOUT(2)/RMIRR)
 

	        ELSE                                        !No more cases now
	         CALL LEAVE('Error in MIRROR', &
     'Johansson surface not implemented yet.Try spherical or plane',izero)
		END IF
! *
! * local planes cut angle
! *
	        A_BRAGG = ARC/R_JOHANSSON + A_BRAGG_OLD
	        IF (A_BRAGG.LT.0) ORDER = -1
	        IF (A_BRAGG.GE.0) ORDER = +1
	        RDENS   = ABS(SIN(A_BRAGG)/D_SPACING)
	        G_MODR  = RDENS*TWOPI*ORDER
	  ELSE
! C
! C Compute now the adjustment to the surface line density at the point
! C of intercept
! C
     		CALL	DOT	(VNOR,Y_VRS,G_FAC)
     		G_FAC	=   SQRT (1.0D0 - G_FAC**2)
! C
! C Computes distance of intercept projection on basal plane from
! C origin.
! C

! Bug in Varied Line Spherical Grating (polynomial ruling)
! Noticed by Vladimir N. Strocov, Giacomo Ghiringhelli, Ruben Renninger
! Fixed by Fan Jian and Franco Cerrina
! Checked by Ruben Renninger
! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/VLSgrating_fix2010-07-02.txt
!!     		TTEMP	=   PPOUT(3)/VVIN(3)
!!		DIST	=   PPOUT(2) + VVIN(2)*TTEMP
		DIST = PPOUT(2)

! C
! C Test for sign flag
! C
     		IF (F_RUL_ABS.EQ.0) DIST = ABS(DIST)
		RDENS	=   RULING + RUL_A1*DIST + RUL_A2*DIST**2 &
     				+ RUL_A3*DIST**3 + RUL_A4*DIST**4
		G_MODR	=   RDENS*TWOPI*ORDER*G_FAC
	  END IF
     	ELSE IF (F_RULING.EQ.2) THEN
! C
! C computes normal to laser wavefronts at intercept
! C
     	 IF (F_PW.EQ.0) THEN		! Both spherical sources
     	  IF (F_PW_C.EQ.0) THEN
     		CALL	VECTOR	(HOLO1,PPOUT,DIS1)
     		CALL	VECTOR	(HOLO2,PPOUT,DIS2)
     	  ELSE IF (F_PW_C.EQ.1) THEN
     		CALL	VECTOR  (HOLO2,PPOUT,DIS2)
     		CALL	VDIST	(PPOUT,HOLO1,VHOLO1,DIS1)
     	  ELSE IF (F_PW_C.EQ.2) THEN
     		CALL	VECTOR	(HOLO1,PPOUT,DIS1)
     		CALL	VDIST	(PPOUT,HOLO2,VHOLO2,DIS2)
     	  ELSE IF (F_PW_C.EQ.3) THEN
     		CALL	VDIST	(PPOUT,HOLO1,VHOLO1,DIS1)
     		CALL	VDIST	(PPOUT,HOLO2,VHOLO2,DIS2)
     	  END IF
     		CALL	NORM	(DIS1,DIS1)
     		CALL	NORM	(DIS2,DIS2)
     	 ELSE IF (F_PW.EQ.1) THEN	! plane/spherical
     	  IF (F_PW_C.EQ.2.OR.F_PW_C.EQ.3) THEN
     		CALL	VDIST	(PPOUT,HOLO2,VHOLO2,DIS2)
     	  ELSE
     		CALL	VECTOR	(HOLO2,PPOUT,DIS2)
     	  END IF
     		CALL	NORM	(DIS2,DIS2)
     	 ELSE IF (F_PW.EQ.2) THEN	! spherical/plane
     	  IF (F_PW_C.EQ.1.OR.F_PW_C.EQ.3) THEN
     		CALL	VDIST	(PPOUT,HOLO1,VHOLO1,DIS1)
     	  ELSE
     		CALL	VECTOR	(HOLO1,PPOUT,DIS1)
     	  END IF
     		CALL	NORM	(DIS1,DIS1)
     	 ELSE IF (F_PW.EQ.3) THEN	! plane/plane
! ** Nothing to do. DIS1,DIS2 are already defined.
     	 ELSE
     	 END IF
! C
! C Take in account REAL/ VIRTUAL recording sources
! C
     	IF (F_VIRTUAL.EQ.1) THEN
     	  CALL	SCALAR (DIS2,-1.0D0,DIS2)
     	ELSE IF (F_VIRTUAL.EQ.2) THEN
     	  CALL	SCALAR (DIS1,-1.0D0,DIS1)
     	ELSE IF (F_VIRTUAL.EQ.3) THEN
     	  CALL	SCALAR (DIS2,-1.0D0,DIS2)
     	  CALL	SCALAR (DIS1,-1.0D0,DIS1)
     	END IF
     		CALL	VECTOR	(DIS2,DIS1,HYPER)
     		CALL	NORM	(HYPER,HYPER)
! ** HYPER is the normal to the hyperboloid in PPOUT.
     		CALL	CROSS	(HYPER,VNOR,VTEMP)
     		CALL	NORM	(VTEMP,VTEMP)
     		CALL	CROSS	(VNOR,VTEMP,VTAN)
     		CALL	NORM	(VTAN,VTAN)
! ** VTAN is now a vector tangent to the grating surface and orthogonal
! ** to the groove. We compute now the ruling density at that point.
     		CALL	VECTOR	(DIS2,DIS1,HYPER1)
     		CALL	DOT	(VTAN,HYPER1,ADJUST)
     		RULING	=   ADJUST*1.0D8/HOLO_W
     		G_MODR	=   ORDER*TWOPI*RULING
! D	WRITE (23,*)    'ray = ',itik
! D	WRITE (23,*)	PPOUT
! D	WRITE (23,*)	VTAN
! D	WRITE (23,*)	RULING
! ** Check now  the direction of VTAN. If it is not pointing toward +y,
! ** it is reversed. This to get out of the problem of the convexity of
! ** the grooves.
     	 IF (VTAN(2).LT.0.0D0.AND.FZP.EQ.0) THEN
     	   CALL SCALAR (VTAN,-1.0D0,VTAN)
     	 END IF
	ELSE IF (F_RULING.EQ.3) THEN
	  SPACING = SQRT((DIST_FAN-PPOUT(2))**2+PPOUT(1)**2)/RULING
	  SPACING = SPACING/DIST_FAN
	  ARG	  = PPOUT(1) /(DIST_FAN-PPOUT(2))
	  PHI_XY  = ATAN (ARG)
	  VTAN(3) = 0.0D0
	  VTAN(1) = COS(PHI_XY)
	  VTAN(2) = SIN(PHI_XY)
! C
! C COMA_FAC  is the coma correction factor
! C
	  SPACING = SPACING*(1+PHI_XY**2*COMA_FAC)
	  G_MODR = TWOPI/SPACING*ORDER
	END IF
	    CALL	SCALAR	(VTAN,G_MODR,GSCATTER)
! D	WRITE (23,*)	GSCATTER
	if (f_scatter_rough.eq.1) then
	krough_count = krough_count + 1
! C
! C       if the surface roughness is selected calculate the scattering
! C       vector due to the roughness, and this vector is added to
! C       the scattering vector if we are working with gratings
! C       or is used alone if we work with mirrors
! C
! C
! C	Modifications to MIRROR to include effects of uncorrelated roughness.
! C	A random scattering vector of length and direction specified in
! C	a power spectral density file is used. VERSION FOR A PLANE MIRROR.
! C
            IPSFLAG = 1
            IERR = 0
            CALL PSPECT (X1,X2,IERR,IPSFLAG,file_rough)

		IF (IERR.NE.0) CALL LEAVE  &
     		('MIRROR', 'Error on return from roughness: PSPECT', izero)
	IF (WRAN(ISTAR1).LT.0.5D0) X1 = -X1
	IF (WRAN(ISTAR1).LT.0.5D0) X2 = -X2
 	RH_DENS = SQRT (X1**2 + X2**2)
	ORDER = 1.0D0
	IF (WRAN(ISTAR1).LE.0.5D0) ORDER = -1.0D0
	G_MODR = RH_DENS * ORDER
! C
! C	Now to build GSCATTER.  It must be in the tangent plane.
! C	Remember that VTAN and GSCATTER are parallel.
! C
	VTAN(1) = X1/RH_DENS
	VTAN(2) = X2/RH_DENS
	VTAN(3) = 0.0D0
! C	VNOR = Z_VRS
! C	CALL CROSS (VTAN,VNOR,BNOR)
! C 	CALL NORM (BNOR,BNOR)
! C
! C	Check to see that VTAN is not zero.
! C
	CALL DOT (VTAN,VTAN,TEST)
	IF (ABS(TEST).LE.1.0D-10) CALL LEAVE  &
     ('MIRROR', &
     'Impossible condition 1 in MIRROR during VTAN calculation',izero)
! C
! C now build the GSCATTER vector
! C
	CALL SCALAR	(VTAN,G_MODR,stemp)
	if (f_grating.eq.1.or.f_bragg_a.eq.1) then
	 call sum (gscatter,stemp,gscatter)
	else 
	 gscatter(1) = stemp(1)
	  gscatter(2) = stemp(2)
	   gscatter(3) = stemp(3)
	end if
! C
	end if
!sriodebug
! ** 2. Projects the incoming vector on the scattering plane
		IF (RAY(11,ITIK).NE.0.0D0) THEN
     		  Q_IN_MOD	=   RAY(11,ITIK)
		ELSE
		  IF (ORDER.EQ.0) THEN		! Arbitrarily assume k=10000cm-1
		    Q_IN_MOD	=   10000	! for zeroth order.
		  ELSE
		    WRITE(6,*) 'Warning! Photon energy of incoming ', &
     'ray is not defined. No diffractions computed.'
		    GO TO 10000
		  END IF
		END IF
     		CALL SCALAR	(VVIN,Q_IN_MOD,Q_IN)
     		CALL PROJ	(Q_IN,VNOR,VTEMP)
     		CALL VECTOR 	(VTEMP,Q_IN,K_PAR)

!		iskiplaue=0
	        if (f_refrac.eq.1) then                           !laue xtals
	          if (abs(a_bragg-pihalf).lt.1d-15) then            !laue symm
                    call rotvector (vnor,x_vrs,-a_bragg,stemp)
	            call proj (vvin,stemp,vtemp)
	            q_out(1) 	= VVIN(1) - 2*vtemp(1)
	            q_out(2) 	= VVIN(2) - 2*vtemp(2)
	            q_out(3) 	= VVIN(3) - 2*vtemp(3)
!				goto 8989
! srio@esrf.eu 2011-411 debugging Laue crystals
                    RAY(4,ITIK) =   Q_OUT(1)
                    RAY(5,ITIK) =   Q_OUT(2)
                    RAY(6,ITIK) =   Q_OUT(3)
                    GOTO 450
!		  iskiplaue=1
! end debugging
	          end if
! C			call sum	(vtemp,gscatter,q_out) 
! C     		else if (f_refrac.ne.1) then
	        end if

!srio
!-------------		IF (iskiplaue.ne.1) THEN

		CALL SUM	(K_PAR,GSCATTER,Q_OUT)
     		CALL DOT	(Q_OUT,Q_OUT,Q_OUT_MOD)
     		VALUE  =   Q_IN_MOD**2 - Q_OUT_MOD
     		  IF (VALUE.LT.0.0D0) THEN
			IF (f_scatter_rough.eq.1) THEN
			    GOTO 450
			ELSE
			    RAY(10,ITIK)	= - 1.010101D6
			    GO TO 10000
			END IF
     		  ELSE
     		VALUE  =   SQRT( VALUE )
     		CALL SCALAR	(VNOR,VALUE,VTEMP)
     		CALL SUM	(VTEMP,Q_OUT,Q_OUT)
     		CALL NORM	(Q_OUT,Q_OUT)
                krough_count2 = krough_count2 + 1

!---------------		END IF
! C
! C If it is Kumakhov case the value would not make sense
! C

!8989	    continue
     		RAY(4,ITIK) =   Q_OUT(1)
     		RAY(5,ITIK) =   Q_OUT(2)
     		RAY(6,ITIK) =   Q_OUT(3)
     		  END IF
! ** Resets VNOR to initial value.
! C     	IF (F_CONVEX.EQ.0)	CALL SCALAR (VNOR,-1.0D0,VNOR)
     	  ELSE
     	  END IF
450    	CONTINUE
    	IF (FMIRR.EQ.6)	THEN		! Codling slit case

     		T_SLIT	= - P_START(2)/VVIN(2)
     		X_SLIT	=   P_START(1) + VVIN(1)*T_SLIT
     		Z_SLIT	=   P_START(3) + VVIN(3)*T_SLIT

     		TEST_X  =   (X_C_P - X_SLIT)*(X_SLIT - X_C_M)
     		TEST_Z  =   (Z_C_P - Z_SLIT)*(Z_SLIT - Z_C_M)

     		CODLING(1,ITIK)	=   X_SLIT
     		CODLING(2,ITIK) =    .0D0
     		CODLING(3,ITIK) =   Z_SLIT
     		CODLING(4,ITIK) =    .0D0
     		CODLING(5,ITIK) =    .0D0
     		CODLING(6,ITIK) =    .0D0
	        CODLING(12,ITIK)=   RAY(12,ITIK)

     		IF (TEST_X.LT.0.0D0.OR.TEST_Z.LT.0.0D0)  THEN
     			RAY(10,ITIK) = - 1.1D+4*I_WHICH
     			CODLING(10,ITIK) = - 1.1D+4*I_WHICH
     		ELSE
     			CODLING(10,ITIK) =   RAY(10,ITIK)
     		END IF
     	ELSE
     	END IF	

! * First we bring the normal upwards
! C
! C     	IF (F_CONVEX.EQ.0)	CALL SCALAR (VNOR,-1.0D0,VNOR)
! C
! * Reflectivity
! * Check for reflectivity. If this mode is "on", we have to compute
! * some angles, namely the sine of the incidence angle and the sine
! * of the A vector with the normal. Also, the polarized light is 
! * treated as a superposition of two orthogonal A vectors with the appropriate
! * phase relation. These two incoming vectors have to be resolved into the
! * local S- and P- component with a new phase relation.
! * A_VEC will be rotated later, once the amplitude will have been determined.
     	CALL	CROSS 	(VVIN,VNOR,AS_TEMP)	! vector pp. to inc.pl.
     	IF (M_FLAG.EQ.1) THEN
	CALL	DOT	(AS_VEC,AS_VEC,AS2)
	CALL	DOT	(AP_VEC,AP_VEC,AP2)
	IF (AS2.NE.0)	THEN
     	 DO 499 I=1,3
 499   	   AS_TEMP(I) = AS_VEC(I)
	ELSE
	 DO 599 I=1,3
 599	   AS_TEMP(I) = AP_VEC(I)
	END IF
     	END IF
     	CALL	NORM  	(AS_TEMP,AS_TEMP)	! Local unit As vector
	CALL	CROSS	(AS_TEMP,VVIN,AP_TEMP)
	CALL	NORM	(AP_TEMP,AP_TEMP)	! Local unit Ap vector
	CALL	DOT	(AS_VEC,AS_TEMP,A11)	! matrix element of rotation
	CALL	DOT	(AP_VEC,AS_TEMP,A12)	! matrix element of rotation
	CALL	DOT	(AS_VEC,AP_TEMP,A21)	! matrix element of rotation
	CALL	DOT	(AP_VEC,AP_TEMP,A22)	! matrix element of rotation
	PHS	= PHASE(2,ITIK)
	PHP	= PHASE(3,ITIK)
! ** Now recompute the ampltitude and phase of the local S- and P- component.
	AS_NEW	= SQRT(ABS(A11**2 + A12**2 +  &
     					2.0D0*A11*A12*COS(PHS-PHP)))
	AP_NEW	= SQRT(ABS(A21**2 + A22**2 +  &
     					2.0D0*A21*A22*COS(PHS-PHP)))
	CALL	SCALAR	(AS_TEMP,AS_NEW,AS_VEC)	! Local As vector
	CALL	SCALAR	(AP_TEMP,AP_NEW,AP_VEC)	! Local Ap vector
	PHTS	= A11*SIN(PHS) + A12*SIN(PHP)
	PHBS	= A11*COS(PHS) + A12*COS(PHP)
	PHTP	= A21*SIN(PHS) + A22*SIN(PHP)
	PHBP	= A21*COS(PHS) + A22*COS(PHP)
	CALL	ATAN_2	(PHTS,PHBS,PHS)		! Phase of local As vector
	CALL	ATAN_2	(PHTP,PHBP,PHP)		! Phase of local Ap vector
! C
! C
     	CALL	DOT	(VVIN,VNOR,SIN_VAL)	! sin(graz. ang)
     	CALL	DOT	(Q_OUT,VNOR,SIN_REF)	! sin(graz.ref.ang)
! C
! C MLAYER thicknesses may be scaled to the angle from the pole.
! C
	IF (F_THICK.EQ.1) THEN
	  CALL	DOT	(VNOR,Z_VRS,COS_POLE)	! cos(ang. of normal from pole)
	ELSE
	  COS_POLE	= 1.0D0
	END IF
     	SIN_VAL	=   ABS(SIN_VAL)
	COS_POLE =	ABS(COS_POLE)
! * Computes now the reflectivity
     	WAVEN	=   RAY(11,ITIK)
     	IF (F_REFLEC.NE.0) THEN
	  IF (F_CRYSTAL.EQ.1) THEN
! C
! C Bragg case
! C
! * rotation of surface normal an angle A_BRAGG around the axis X 
! * for having the normal to Bragg planes in asymmetrical case, then
! * calculation of the angle between this normal and incident ray
! *
	IF (F_BRAGG_A.EQ.1.or.f_refrac.eq.1) THEN  !bragg asymm or any laue
	 CALL ROTVECTOR (VNOR,X_VRS,A_BRAGG,VTEMP)
	 CALL DOT  (VVIN,VTEMP,SIN_BRG)
	 SIN_BRG = ABS(SIN_BRG)
	ELSE                                       !bragg sym
	 SIN_BRG = SIN_VAL
	 SIN_REF = SIN_VAL
	END IF
! Csriosrio
! Csrio
! Csrio
! Csrio trick for calculation Hyperbolic Laue using external ccc conic coeffs
! Csrio
! Csrio see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/hyperbola_fixes_2008-10-22.txt
	IF (SIN_VAL*SIN_REF.LE.0) THEN 
          SIN_REF=SIN_REF*(-1.0D0)
        END IF

     	  CALL	CRYSTAL	(WAVEN, SIN_VAL, SIN_REF, SIN_BRG, REF_S, REF_P, &
      PHASES, PHASEP, DEPTH_MFP_S, DEPTH_MFP_P, DELTA_REF, THETA_B,ione)
	  ELSE
! C
! C "normal" mirror
! C
	   CALL	REFLEC 	(PPOUT,WAVEN,SIN_VAL,COS_POLE, &
     			REF_P,REF_S,PHASEP,PHASES,ABSOR,ione)
	  END IF
! C
! C       Mosaic crystal corrections in
! C       penetration depth and, consequently, phase shift
! C
	  IF (F_MOSAIC.EQ.1) THEN
	    IF (F_FACET.EQ.1) THEN
	      CALL LEAVE ('MIRROR', &
     		'CONFLICT BETWEEN MOSAIC AND FACET',izero)
	    END IF
	   CALL  DOT (AS_VEC,AS_VEC,AS_MOD)
	   CALL  DOT (AP_VEC,AP_VEC,AP_MOD)
	   A_DEG = AS_MOD/(AS_MOD+AP_MOD)
	   A_RND = WRAN (MOSAIC_SEED)
	   IF (A_RND.LE.A_DEG) THEN
	    DEPTH_MFP = DEPTH_MFP_S
	   ELSE IF (A_RND.GT.A_DEG) THEN
	    DEPTH_MFP = DEPTH_MFP_P
	   END IF
           ! very small, very large or "normal"
           IF (DEPTH_MFP.LT.1.0D-10) THEN   ! no penetration
              DEPTH_INC = 0.0d0
           ELSE IF (DEPTH_MFP.GT. 10.0*thickness/sin_val) THEN  ! uniform penetration
	      depth_inc = WRAN (MOSAIC_SEED)
              depth_inc = depth_inc*thickness/sin_val 
           ELSE
             ARG = THICKNESS/DEPTH_MFP/SIN_VAL
             tmp = 0.0d0
             CALL MFP(tmp,MOSAIC_SEED,i_two)
             CALL MFP(ARG,MOSAIC_SEED,i_one)
             CALL MFP(DEPTH_INC,MOSAIC_SEED,ione)
             DEPTH_INC = DEPTH_INC*DEPTH_MFP
           ENDIF

           if (depth_inc .gt. thickness/sin_val) then 
             print *,'MIRROR: Warning: Overestimated penetration',depth_inc,thickness/sin_val
           end if

           if (depth_inc .gt. 0.0d0) then 
               DO I=1,3
                  VTEMP(I) = PPOUT(I) + DEPTH_INC*VVIN(I)
               END DO
               !srio debugging: for spherical crystals, we need the closest 
               !interaction to the surface, and this is forced setting iflag=-1
               iflag = -1 

               CALL INTERCEPT (VTEMP,VVOUT,DEPTH_REF,IFLAG)
               DO I=1,3
                  RAY(I,ITIK) = VTEMP(I) + DEPTH_REF*VVOUT(I)
               END DO
               PHASE (1,ITIK) = PHASE (1,ITIK) + (DEPTH_INC+DEPTH_REF)*(1.0D0-DELTA_REF)
           end if
          END IF  ! end mosaic

! *
! *  Reset Johansson asymmetrical parameters to its original value
! *
	  IF (F_JOHANSSON.EQ.1) THEN
	   A_BRAGG = A_BRAGG_OLD
	   ORDER   = ORDER_OLD
	  END IF
! D	  ANG_INC	= ACOSD(SIN_VAL)
! D	  WREFS		= REF_S
! D	  WREFP		= REF_P
! D	  WPS		= PHASES
! D	  WPP		= PHASEP
! D	  WRITE	(27,*)	ANG_INC,WREFS,WREFP,WPS,WPP
     	 IF (F_REFLEC.EQ.1) THEN 			! Full polarization case
     	   CALL	SCALAR	(AS_VEC,REF_S,AS_VEC)
     	   CALL	SCALAR  (AP_VEC,REF_P,AP_VEC)
	   PHP	= PHP + PHASEP
	   PHS	= PHS + PHASES
     	   EFF_REF_S = EFF_REF_S + REF_S**2
           EFF_REF_P = EFF_REF_P + REF_P**2
     	 ELSE IF (F_REFLEC.EQ.2) THEN			! No interested in it
	   REF		= (REF_S**2 + REF_P**2)/2
     	   EFF_REF 	= EFF_REF + REF
	   REF		= SQRT(REF)
	   CALL	SCALAR	(AP_VEC,REF,AP_VEC)
     	   CALL	SCALAR	(AS_VEC,REF,AS_VEC)
     	 END IF
      	END IF
! C
! C Rotate the A vector so that its sign is no longer imbedded in the phase angle.
! C
	IF ((COS(PHS)).LT.0.0D0) THEN
	  PHS	= PHS - PI
	  CALL	SCALAR	(AS_VEC,-1.0D0,AS_VEC)
	END IF
	IF ((COS(PHP)).LT.0.0D0) THEN
	  PHP	= PHP - PI
	  CALL	SCALAR	(AP_VEC,-1.0D0,AP_VEC)
	END IF

! ** So far we have the new amplitude of the two components. We have now
! ** to 'reflect' A_VEC onto the mirror. For this, notice that the s-comp
! ** is geometrically unchanged, while the p-comp is changed. The angles
! ** are exchanged with respect to VVIN. Things are more complicated in
! ** the case of a grating, due to the vectorial nature of the diffraction,
! ** not treated here. We make the simplifying assumption that the
! ** diffraction will not change the degree of polarization. This mean that
! ** A_VEC will have the same components referred to the ray as before the
! ** diffraction. 

      VVOUT(1) =   RAY(4,ITIK)
      VVOUT(2) =   RAY(5,ITIK)
      VVOUT(3) =   RAY(6,ITIK)
!srio ! D	WRITE	(24,*)	ITIK
!srio ! D	WRITE	(24,*)	VNOR
!srio ! D	WRITE	(24,*)	VTEMP
!srio ! D	WRITE	(24,*)	A_VEC
!srio ! D	WRITE	(24,*)	A_S
!srio ! D	WRITE	(24,*)	A_P
!srio ! C 
!srio ! C The following IF block applies only to the GRATING case.
!srio ! C The binormal is redefined in terms of the diffraction
!srio ! C plane.
!srio ! C
!srio      	IF (F_GRATING.NE.0.OR.F_BRAGG_A.EQ.1) THEN
!srio 	  CALL	PROJ	(VVOUT,VNOR,VTEMP)
!srio 	  CALL	SCALAR	(VTEMP,-2.0D0,VTEMP)
!srio 	  CALL	SUM	(VTEMP,VVOUT,VTEMP)
!srio 	  CALL	CROSS	(VTEMP,VNOR,AS_TEMP)
!srio      	 IF (M_FLAG.EQ.1) THEN
!srio 	   CALL	DOT	(AS_VEC,AS_VEC,AS2)
!srio 	   CALL	DOT	(AP_VEC,AP_VEC,AP2)
!srio 	  IF (AS2.NE.0)	THEN
!srio      	    DO 899 I=1,3
!srio  899          AS_TEMP(I) = AS_VEC(I)
!srio 	  ELSE
!srio 	   DO 999 I=1,3
!srio  999	     AS_TEMP(I) = AP_TEMP(I)
!srio      	  END IF
!srio 	 END IF
!srio      	  CALL	NORM  	(AS_TEMP,AS_TEMP)	! Local unit As vector
!srio 	  CALL	CROSS	(AS_TEMP,VTEMP,AP_TEMP)
!srio 	  CALL	NORM	(AP_TEMP,AP_TEMP)	! Local unit Ap vector
!srio      	  CALL	DOT	(AS_VEC,AS_VEC,RES)
!srio      	  RES	=    SQRT (RES)
!srio      	  CALL	SCALAR	(AS_TEMP,RES,AS_VEC)
!srio 	  CALL	DOT	(AP_VEC,AP_VEC,RES)
!srio 	  RES	=    SQRT (RES)
!srio 	  CALL	SCALAR	(AP_TEMP,RES,AP_VEC)
!srio      	END IF
!srio      	CALL	PROJ	(AP_VEC,VNOR,VTEMP)
!srio 	CALL	VECTOR	(VTEMP,AP_VEC,VTEMP)
!srio      	CALL	SCALAR	(VTEMP,-2.0D0,VTEMP)
!srio      	CALL	SUM	(AP_VEC,VTEMP,AP_VEC)

!--------------------------------------------------------------------------------

! Electric vectors are changed to assure orthogonality with the new direction VVOUT
! To conserve intensity, the moduli of Es and Ep must not change
! AS_VEC and VVOUT are not orthogonal so a projection of S and P coordinates into the
! new ones do not work as it may be a component of the electric field along VVOUT

CALL CROSS_M_FLAG  (VVOUT,VNOR,AS_TEMP,M_FLAG) ! vector pp. to inc.pl.
CALL DOT (AS_VEC,AS_VEC,AS2)
CALL DOT (AP_VEC,AP_VEC,AP2)

IF (M_FLAG.EQ.1) THEN
 IF (AS2.NE.0) THEN
   DO I=1,3
     AS_TEMP(I) = AS_VEC(I)
   END DO
 ELSE
  DO I=1,3
   AS_TEMP(I) = AP_VEC(I)
  END DO
 END IF
END IF

CALL NORM   (AS_TEMP,AS_TEMP) ! Local unit As vector perp to vvout
CALL CROSS (AS_TEMP,VVOUT,AP_TEMP)
CALL NORM (AP_TEMP,AP_TEMP) ! Local unit Ap vector perp to vvout

do i=1,3
  as_vec(i) = as_temp(i) * sqrt(as2)
  ap_vec(i) = ap_temp(i) * sqrt(ap2)
end do
!--------------------------------------------------------------------------------


      ! attenuation in lens media
      rr_reflectivity = 1.0D0
      ! debugging srio@esrf.eu 20130910 added f_crystal.ne.0
      IF ( (F_REFRAC.EQ.1) .and. (F_CRYSTAL.EQ.0) ) THEN 
         IF (ABS(rr_attenuation_obj).GT.1e-15) THEN
           ! for amplitudes use sqrt(reflectivity)
           !rr_reflectivity = sqrt(exp(-ABS(r_attenuation_obj*TPAR)))
           rr_reflectivity = sqrt(exp(-ABS(rr_attenuation_obj*TPAR)))
         END IF
!if (itik.lt.10) print *,'>>>>0 obj: ',rr_attenuation_obj,TPAR,rr_reflectivity
      END IF



! D	WRITE	(24,*)	A_P
! D	WRITE	(24,*)	VNOR
! C
! C If full polarization is not selected, then only the As vector will be written
! C onto disk.  To have the As carry the correct magnitude, we must sum As and Ap.
! C
	IF (NCOL.NE.18)	THEN
	  CALL	SUM	(AS_VEC,AP_VEC,AS_VEC)
	ELSE
	  AP  (1,ITIK)	=   AP_VEC(1) * rr_reflectivity
	  AP  (2,ITIK)	=   AP_VEC(2) * rr_reflectivity
	  AP  (3,ITIK)	=   AP_VEC(3) * rr_reflectivity
	END IF
! C
     	RAY (7,ITIK)	=   AS_VEC(1) * rr_reflectivity
     	RAY (8,ITIK)	=   AS_VEC(2) * rr_reflectivity
     	RAY (9,ITIK)	=   AS_VEC(3) * rr_reflectivity

	PHASE(2,ITIK)	=   PHS
	PHASE(3,ITIK)	=   PHP



10000	CONTINUE

	IF (F_KOMA.NE.1) THEN
	IF (F_ANGLE.EQ.1) THEN
! C
! C Save index, incidence and reflection angles for all rays.
! C
	IF (F_FACET.NE.1) THEN		! already set them in facet
	  CALL DOT (VVIN,VNOR,SIN_IN)
	  CALL DOT (VVOUT,VNOR,SIN_OUT)

  	  ANGLE_IN = TODEG*ACOS (SIN_IN)
	  ANGLE_OUT = TODEG*ACOS (SIN_OUT)
	END IF

	  ANGLE(1,ITIK) = ITIK
	  ANGLE(2,ITIK) = 180 - ANGLE_IN
	  ANGLE(3,ITIK) = ANGLE_OUT
	  ANGLE(4,ITIK) = RAY(10,ITIK)

	END IF
	END IF

! C
! C Counts lost rays in this OE
! C
! C Good rays
     	IF (RAY(10,ITIK).GE.0.0D0)	K_3 = K_3 + 1
! C Lost rays
     	IF (RAY(10,ITIK).LT.0.0D0)	K_4 = K_4 + 1
! C Hard lost
	IF (RAY(10,ITIK).LT.-1.0D6)	K_6 = K_6 + 1

if (check_perp_comp( ray(4,itik), ray(5,itik), ray(6,itik), &
                     ray(7,itik), ray(8,itik), ray(9,itik), &
                     ap(1,itik), ap(2,itik), ap(3,itik)) .gt. 1d-6) then 
end if

 1099	CONTINUE  ! end loop on number of rays
! C
! C Kumakhov case skips the file writing ...
! C
	IF (F_KOMA.NE.1) THEN
! C
! C* Store the results for later examination or processing *
! C* Insert also an 'end of file' marker for external processor *
! C
     	IF ((FWRITE.EQ.0).OR.(FWRITE.EQ.1)) THEN
     	  CALL	FNAME	(FFILE, 'mirr', I_WHICH, izero)
	  IFLAG	= 0
	  CALL  WRITE_OFF   (FFILE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,izero,IERR)
          IF    (IERR.NE.0) CALL LEAVE  &
                        ('MIRROR','Error writing MIRR',IERR)

     	END IF
! D
! D	IF (F_REFL.EQ.2)	CLOSE(27)
! D
! C
! C If codling slit, write out
! C
     	IF (FMIRR.EQ.6) THEN
     	  CALL	FNAME	(FFILE, 'codl', I_WHICH, izero)
	  IFLAG	= 0
     	  CALL	WRITE_OFF (FFILE,CODLING,PHASE,AP,NCOL,NPOINT,IFLAG,izero, IERR)
     	  IF	(IERR.NE.0) CALL LEAVE &
     		 ('MIRROR','Error writing CODLING',IERR)
     	END IF
! C
! C Evaluates now the mirror efficiencies; two figures of merit are given, the
! C geometrical efficiency (i.e., reflectivity = 1) and the reflectivity. The
! C overall f.m. is the product of the two.
! C
! C
! C Total of rays lost out of the mirror
! C
     	K_5	=   K_4 - K_1
	K_6	=   NPOINT - K_6

     	IF (K_2.NE.0) THEN
     	  EFF_GEOM  =  DBLE(K_3)/DBLE(K_2)
     	ELSE
     	  END IF
     	IF (F_REFLEC.EQ.1) THEN
     	  ABS_REF_S 	=  (EFF_REF_S/K_6)
     	  ABS_REF_P 	=  (EFF_REF_P/K_6)
     	  ABS_REF	=  (ABS_REF_S + ABS_REF_P)/2
     	  OVERALL	=  ABS_REF*EFF_GEOM
     	ELSE IF (F_REFLEC.EQ.2) THEN
     	  ABS_REF	=  (EFF_REF/K_6)
     	  OVERALL	=  ABS_REF*EFF_GEOM
     	END IF
        ! srio@esrf.eu 20110412 avoid writing effic.xx if FWRITE=3
        IF (FWRITE.NE.3) THEN
     	  CALL	FNAME	(FFILE, 'effic', I_WHICH, izero)
	  OPEN (UNIT=20,FILE=FFILE,STATUS='UNKNOWN')
	  REWIND (20)
     	  IF (K_2.EQ.0)	WRITE (20,3000)
	  WRITE (20,2000) NPOINT,K_2,K_5,I_WHICH,EFF_GEOM
     	  IF (F_REFLEC.EQ.1) THEN
     	    WRITE (20,2010) ABS_REF_S,ABS_REF_P,ABS_REF
     	  ELSE IF (F_REFLEC.EQ.2) THEN
     	    WRITE (20,2020) ABS_REF
     	  END IF
          IF (F_ROUGHNESS.EQ.1) THEN
             WRITE(20,2040) krough_count2,krough_count
          END IF
     	  IF (F_REFLEC.NE.0) WRITE (20,2030) OVERALL
	  CLOSE (20)
        END IF ! fwrite

3000	FORMAT (1X,'WATCH OUT !! NO GOOD RAYS IN INPUT !!')
2000	FORMAT (1X,'Of a total of ',I6,' rays, of which ',I6,' formed ', &
      'the input set ',/,1X,I6,' were out of the mirror N. ',I4,/,1X, &
      'The mirror collects ',G12.5,' of the incoming flux.')
2010	FORMAT (1X,'The average reflectivities are :',/, &
                1X,'S-pol ',20X,G12.5,/, &
                1X,'P-pol ',20X,G12.5,/, &
                1x,'Total ',20X,G12.5)
2020	FORMAT (1X,'The average reflectivity is :',G12.5)
2030	FORMAT (1X,'The overall efficiency of the mirror is :',G12.5)
2040    FORMAT (1X,'There were',I6,'rays scattered out of the elastic', &
             ' peak',1X,/,'and',I6, &
             ' rays were decided to be scattered', &
             ' by the rho**2 factor')

	IF (F_ANGLE.EQ.1) THEN
! C write incidence and reflection information to file
 
        CALL FNAME (FFILE, 'angle', I_WHICH, izero)
        OPEN (UNIT=55,FILE=FFILE,STATUS='UNKNOWN')
        REWIND (55)
 
        DO 2525 J = 1,NPOINT
2525      WRITE(55,*) ANGLE(1,J),ANGLE(2,J),ANGLE(3,J),ANGLE(4,J)
        CLOSE(55)
	
	END IF  ! write angle


! bugged......
!     	IF ((FWRITE.EQ.0).OR.(FWRITE.EQ.1)) THEN
!     	  CALL	FNAME	(FFILE, 'rmir', I_WHICH, izero)
!     	 IF (F_MOVE.EQ.1)   THEN
!     	   CALL ROT_BACK (RAY,AP)
!	   IFLAG	= 0
!     	   CALL	WRITE_OFF	(FFILE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,izero,IERR)
!     	  IF (IERR.NE.0) CALL LEAVE ('MIRROR','Error writing RMIR',IERR)
!     	 END IF
!     	END IF

IF (F_MOVE.EQ.1)   THEN
    CALL ROT_BACK (RAY,AP)
END IF


IF ((FWRITE.EQ.0).OR.(FWRITE.EQ.1)) THEN
    CALL FNAME (FFILE, 'rmir', I_WHICH, izero)
    IFLAG = 0
    CALL  WRITE_OFF      (FFILE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,izero,IERR)
    IF (IERR.NE.0) CALL LEAVE ('MIRROR','Error writing RMIR',IERR)
END IF
        WRITE(6,*) 'Exit from MIRROR'
! D	CLOSE (23)

	END IF  ! no koma?
99999 continue  ! come here of f_refrac=2 (empty oe)

End Subroutine mirror1




! C+++
! C	SUBROUTINE	SWITCH_INP
! C
! C	PURPOSE		To select different I/O modes for SHADOW
! C
! C	ALGORITHM	Tests the symbol 'MENU_STAT' in the symbol table.
! C			Recognized values are
! C				MENU
! C				PROMPT
! C				BATCH
! C
! C---
! #if defined(unix) || HAVE_F77_CPP
SUBROUTINE SWITCH_INP (OUTP,IFLAG,iTerminate)

        ! todo: remove implicits
        ! implicit real(kind=skr) (a-e,g-h,o-z)
        ! implicit integer(kind=ski)        (f,i-n)
        implicit none

        character(len=*),intent(in out)  :: OUTP
        integer(kind=ski),intent(in out) :: iFlag
        integer(kind=ski),intent(out)    :: iTerminate

        character(len=sklen)  :: FILESOURCE
        character(len=sklen)  :: infile
        character(len=sklen)  :: arg
        LOGICAL               :: IRET
        !INTEGER                IPASS
        !DATA         IPASS        / 0 /
        !SAVE                IPASS
        !integer(kind=ski)            :: iTerminate
        integer(kind=ski),save       :: ipass=0


        !integer(kind=ski)            :: iTerminate, iFlag
        integer(kind=ski)            :: irestart, irest, f_new, iwhat, idumm, iend, nold


! C
! C Clears all variable to avoid cross talks
! C
! srio: MOVED TO TRACE LEVEL
!             CALL        RESET

! C
! C Tests for continuation or new optical system
! C
        iTerminate = 0

        IF (IFLAG.EQ.0) THEN
          WRITE(6,*)'Mode selected is: '//trim(OUTP)
          WRITE(6,*)' '
          WRITE(6,*)'Options: to start anew                 [ 0 ] ' 
          WRITE(6,*)'            to restart from a given OE [ 1 ] '
          WRITE(6,*)' '
          IRESTART = IRINT ('Then ? ')
          WRITE(6,*)' '
          IF (IRESTART.EQ.0) THEN
            IFLAG = 1
          ELSE
            IREST =   IRINT ('Previous element number: ')
            CALL READ_AXIS  (IREST)
            IFLAG = IREST + 1
            FILESOURCE = RSTRING ('Image file of the previous OE ? ')
            F_NEW = 0
          END IF
        ELSE
! C
! C Increments counter
! C
          IFLAG = IFLAG + 1
        END IF
             
! C
! C Parses for matches
! C
IF ((OUTP.EQ.'menu').OR.(OUTP.EQ.'systemfile'))  THEN
! C-------------------------------------------------------------
! C
! C   MENU  case
! C
        IF (IPASS.EQ.0) THEN
          OPEN (37, FILE='systemfile.dat', STATUS='old', IOSTAT=IWHAT)
! C
! C
! C First call; must seek the files list
! C
! C Tests if files found; if not, return to main program
! C
          IF (IWHAT.NE.0) THEN
              CALL  LEAVE  ('SWITCH_INP-E-error:', 'systemfile.dat not found', &
                               idumm)
          ELSE
              IPASS = 1
          END IF
        END IF ! (IPASS.EQ.0)
! C
! C System file found and opened succesfully; read in OE filename
! C
        READ (37, 1000, IOSTAT=IEND) INFILE
! C
! C No more files to input; end of OS
! C
        IF (IEND.NE.0) THEN
          !STOP
          iTerminate = 1
          RETURN
        END IF
! C
! C read in NAMELIST block
! C
        IDUMM = 0
        IF (IFLAG.GT.1) THEN
          NOLD = NPOINT
          CALL RWNAME (INFILE,'R_OE', IDUMM)
          NPOINT = NOLD
        ELSE
          CALL RWNAME (INFILE,'R_OE', IDUMM)
        END IF


        IF (IDUMM.NE.0) THEN
          WRITE(6,*)'Error reading from file '//trim(INFILE)
          CALL LEAVE ('SWITCH_INP', 'Check contents of SYSTEMFILE',IDUMM)
        END IF
        GO TO 2000
END IF ! (OUTP.EQ.'MENU')

! C
! C This cannot be a MENU case
! C

        IF (IFLAG.GT.1) THEN
          print *,''
          IDUMM        =   IYES ('Do you want to change input mode ? ')
          print *,''
            IF (IDUMM.EQ.1) THEN
10            WRITE(6,*)'Enter 1 for PROMPT, 2 for BATCH, 3 for SYSTEMFILE'
              IDUMM = IRINT ('Then ? ')
              IF (IDUMM.EQ.1) THEN
                OUTP = 'prompt'
              ELSE IF (IDUMM.EQ.2) THEN
                OUTP = 'batch'
              ELSE IF (IDUMM.EQ.3) THEN
                OUTP = 'systemfile'
              ELSE
                WRITE(6,*)'What ?? '
                GO TO 10
              END IF
            END IF
        END IF

IF (OUTP.EQ.'prompt') THEN
! C--------------------------------------------------------------
! C              
! C   PROMPT  CASE
! C
! C IFLAG represents the OE #
! C
          CALL        INPUT_OE (IFLAG,iTerminate)
! C
ELSE IF (OUTP.EQ.'batch') THEN
! C
! C--------------------------------------------------------------
! C
! C   NAMELIST CASE
! C
! C
          INFILE = RSTRING ('Input file [ EXIT terminates OS ] ?')
! C
! C If ^Z detected, assumes end of system
! C
!csrio              IF (INFILE(1:5).EQ.'EXIT'.OR.INFILE(1:5).EQ.'exit') THEN 
          IF (INFILE.EQ.'EXIT'.OR.INFILE.EQ.'exit') THEN 
            !STOP
            iTerminate=1
            RETURN
          END IF
! C
! C Valid element; read in NAMELIST block
! C
          IDUMM        = 0
          IF (IFLAG.GT.1) THEN
            NOLD = NPOINT
            CALL RWNAME (INFILE,'R_OE', IDUMM)
            NPOINT = NOLD
          ELSE
            CALL RWNAME (INFILE,'R_OE', IDUMM)
          END IF
          IF (IDUMM.NE.0) CALL LEAVE &
                ('SWITCH_INP','Error reading from file ',IDUMM)
! C
ELSE
! C
! C------------------------------------------------------------------
! C
! C   UNRECOGNIZED  INPUT
! C
          WRITE(6,*)'ERROR:: string was: ',OUTP
          WRITE(6,*)'ERROR:: SHADOW not activated properly.'
          CALL LEAVE ('SWITCH_INP','Error in MENU_STAT',izero)
END IF
! C
! C
2000        CONTINUE
! C 
! C If restart from the middle of an OS, then the source files should be the ones
! C specified in the beginning of this subroutine.
! C
        IF (IRESTART.EQ.1) FILE_SOURCE = FILESOURCE
! C
! C Data are ready; can start execution
! C
RETURN
1000 FORMAT (A)
End Subroutine switch_inp


! C -----------------------------------------------------------------------
! C
! C +++
! C	Subroutine 	TRACE_STEP
! C	Purpose		Traces the current state.
! C ---
! C
SUBROUTINE TRACE_STEP (NSAVE,ICOUNT, IPASS, RAY, PHASE, AP)
        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

	real(kind=skr),dimension(:,:)  :: ray,phase,ap
	!DIMENSION 	RAY(12,N_DIM), PHASE (3,N_DIM), AP(3,N_DIM)
! C
	NCOL	= NSAVE
! C
! C Let the user know where we are
! C
     	WRITE(6,*)'Tracing optical element # ',ICOUNT
! C
! C Defines the source parameters
! C
     	CALL SETSOUR
! C
! C Compute the (x,y) reference frame on the image plane
! C
	CALL IMREF
! C
! C Compute the parameters defining the central ray 
! C
	CALL OPTAXIS (ICOUNT)
! C
! C Compute the mirror, if needed
! C
	CALL MSETUP (ICOUNT)
! C
! C Reads in the file containing the source beam
! C

     	IF (IPASS.EQ.1) THEN
          !CALL RBEAM (FILE_SOURCE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IERR)
          CALL RBEAM (FILE_SOURCE,RAY,PHASE,AP,IERR)
	  IF (IERR.NE.0) THEN
	    CALL LEAVE ('TRACE_STEP', &
     		'Error reading source image "' // &
     		FILE_SOURCE(1:IBLANK(FILE_SOURCE)) // '".', &
     		IERR)
     	  END IF

          ! srio: get NCOL from source file. Up to here it is undefined
          CALL beamGetDim (FILE_SOURCE,NCOL,NP,IFLAG,IERR)

	  NSAVE	= NCOL
! C
! C If full polarization is not selected, AP should be initialized to zero.
! C
	  IF (NCOL.NE.18) THEN
	    DO  10 I = 1, NPOINT
	      AP(1,I)	= 0.0D0
	      AP(2,I)	= 0.0D0
	      AP(3,I)	= 0.0D0
 10	    CONTINUE
	  END IF
! C     	 IF (IERR.NE.0) THEN
! C     	  CALL  MSSG	('TRACE: Unable to open ',FILESOURCE, IERR)
! C     	  STOP
! C     	 END IF
     	  IPASS = 0
     	END IF
! C
! C This call rotates the last RAY file in the new MIRROR reference
! C frame
! C
	CALL RESTART (RAY,PHASE,AP)
! C
! C Check if any screens are present ahead of the mirror.
! C
     	DO  20 I=1,N_SCREEN
     	IF (F_SCREEN.EQ.1.AND.I_SCREEN(I).EQ.1)  &
       	CALL  SCREEN (RAY,AP,PHASE,I,ICOUNT)
 20    	CONTINUE
! C
! C Computes now the intersections onto the mirror and the new rays
! C
        IF (F_KOMA.EQ.1) THEN
! Csrio		CALL KUMAKHOV(RAY,AP,PHASE,ICOUNT)
	      CALL LEAVE &
      ('KUMAKHOV','Not yet implemented in Shadow3',izero)

	ELSE
		CALL MIRROR1 (RAY,AP,PHASE,ICOUNT)
	END IF
! C
! C Computes other screens and stops.
! C
     	DO  30 I=1,N_SCREEN
     	IF (F_SCREEN.EQ.1.AND.I_SCREEN(I).EQ.0)  &
     	CALL  SCREEN (RAY,AP,PHASE,I,ICOUNT)
 30    	CONTINUE
! C
! C Computes the intersections on the image plane
! C
	CALL IMAGE1 (RAY,AP,PHASE,ICOUNT)
! C
! C This file will contain the status of the parameters at the exit
! C from this optical element.
! C

! 
! deallocate arrays
!
	CALL DEALLOC


     	CALL	FNAME	(FFILE, 'end', ICOUNT, izero)
     	IDUMM = 0
     	CALL	RWNAME  ( FFILE, 'W_OE', IDUMM)
     	IF (IDUMM.NE.0) CALL LEAVE &
     		 ('TRACE','Error writing NAMELIST',IDUMM)
	RETURN
End Subroutine trace_step


!c+++
!c	SUBROUTINE	INPUT_OE
!c
!c	PURPOSE		Reads in the parameters of the optical element
!c
!c       Inputs:         I_OENUM: The OE we're dealing with
!c
!c---
SUBROUTINE INPUT_OE (I_OENUM,iTerminate)

implicit none

integer(kind=ski), intent(in) :: I_OENUM
integer(kind=ski), intent(out):: iTerminate
integer(kind=ski)             :: I_MIR,IVERB,F_PLANES,I,IDUMM
character(len=sklen) ::  TEXT
character(len=80)  ::  MSSG,MSSG2

iTerminate=0
WRITE(6,*)'Call to INPUT_OE'
MSSG (1:40) = '--------------------------------- S H A '
MSSG (41:80)= 'D O W  -------------------------------  '
WRITE (6,'(1X,A)')  trim(MSSG)
IF (I_OENUM.EQ.1) THEN
    WRITE(6,*)' '
    WRITE(6,*)'When prompted for a yes/no answer, you may enter:'
    WRITE(6,*)'for   YES  answer      Y*, y*, 1*'
    WRITE(6,*)'for   NO   answer      anything else'
    WRITE(6,*)' '
END IF
WRITE (6,2222) I_OENUM
2222  FORMAT (1X,/,1X,'Defining Optical Element: ', I2)
I_MIR  =   I_OENUM
TEXT  =   RSTRING('Continue ? [ EXIT to terminate OS ] ')
IF (TEXT(1:5).EQ.'EXIT'.OR.TEXT(1:5).EQ.'exit') THEN
    print *,''
    print *,'End of session'
    print *,''
    iTerminate=1
    !STOP
    RETURN
END IF
WRITE (6,'(1X,///)')


IVERB = IYES ('Do you want a verbose [ 1 ] or terse [ 0 ] output ?')
IF (IVERB.EQ.1) THEN
    WRITE(6,*)'You may save disk space by not writing out the ', &
      'intermediate STAR or MIRR data files. In general you will not', &
      'need them unless you have specific needs (footprints, etc.) '
END IF
WRITE(6,*)' '
WRITE(6,*)'Files to write out. Options: '
WRITE(6,*)'All............................ [ 0 ] '
WRITE(6,*)'Mirror only.....................[ 1 ] '
WRITE(6,*)'Image at CP only................[ 2 ] '
WRITE(6,*)'None............................[ 3 ] '
FWRITE = IRINT (' Then ? ')
WRITE(6,*)

!**---------------------------------------------------------------------
IF (IVERB.EQ.1) THEN
    print *,'Let''s define the optical or central axis of the system for this particular    '
    print *,'optical element. By this I mean a "virtual" line that runs throughout the   '
    print *,'optical system. Along this line are located the "continuation" planes, where'
    print *,'the OS is subdivided in the individual OE. This line does not have to coincide'
    print *,'with the true optical axis, as it is used mainly for bookkeeping the data,    '
    print *,'but it helps greatly in the data analysis if this identity is preserved as    '
    print *,'everything in the program is referred to it. Once established, you still have '
    print *,'complete freedom of "moving" around the mirrors. In the case of a grating,  '
    print *,'you will have several choices. The program may override your specifications   '
    print *,'for the central axis and locate the source and image at the ‘‘best’’ position.'
    print *,'You will be prompted later for this option. It is recommended to use CM as    '
    print *,'units. This is not critical for most cases, but it is in the case of          '
    print *,'diffraction elements.                                                         '
END IF

WRITE(6,*)'Optical Element definition:'
T_INCIDENCE = RNUMBER ('Incidence Angle ? ')
T_SOURCE    = RNUMBER ('Source Distance ? ')
T_REFLECTION= RNUMBER ('Reflection Angle? ')
T_IMAGE     = RNUMBER ('Image Distance  ? ')
F_REFRAC    = IRINT   ('Reflector [ 0 ] or refractor [ 1 ] ? ')
!**---------------------------------------------------------------------
WRITE(6,*)
!**---------------------------------------------------------------------

!c------------------------------------------------------------
!c Segmented Mirror  question
!c Added by G.J. 4/17/93
!c
IF (IVERB.EQ.1) THEN
    WRITE(6,*)'A segmented mirror is formed by M by N independent  mirrors'
END IF
 
F_SEGMENT = IYES('Is this a segmented mirror system?')
 
IF (F_SEGMENT.EQ.1) THEN  ! start block if-segmented
    FILE_SEGMENT = RSTRING ('File describing the orientation of the mirrors?')
    FILE_SEGP = RSTRING ('File with the polynomial describing the mirror?')
    WRITE(6,*)'Size and  number of the mirrors ?'
    SEG_LENX =RNUMBER(' x-length ?')
    SEG_LENY =RNUMBER(' y-length ?')
    !c       type *,'seg length',seg_lenx,seg_leny
    ISEG_XNUM =IRINT(' Number of rows(x- odd integer) ?')
    ISEG_YNUM =IRINT(' Number of columns(y- odd integer) ?')
    FMIRR = 5
ELSE
    !c
    !c------------------------------------------------------------
    !c KOMAKOFU  question
    !c Added by G.J. 8/7/92
    !c
    IF (IVERB.EQ.1) THEN
        WRITE(6,*)'Kumakhov lens are formed from tube arrays '
        WRITE(6,*)'their packing pattern are Wigner-Seitz type cell.'
        WRITE(6,*)'A capillary would be the central tube of a kumakhov lens.'
    END IF
    F_KOMA = IYES('Is this a Kumakhov system?')
    
    IF (F_KOMA.EQ.1) THEN  !<koma>
        !c	   F_INC_MNOR_ANG = 0
           
        IF (IVERB.EQ.1) THEN
           WRITE(6,*) 'For multiple reflection calculations, you may'
           WRITE(6,*) 'want to store the intercepts of each bounce.'
        END IF
        F_KOMA_BOUNCE = IYES('Store (X,Y,Z) for each bounce? ')
        
        IF (IVERB.EQ.1) THEN
            WRITE(6,*) 'Normally, the tube radii are specified as r(z).'
            WRITE(6,*) 'You may also specify r(z)^2.'
        END IF
        F_KOMA_CA = IYES ('Specify as r(z)^2 ? (Y/N) ')
        
        IF (F_KOMA_CA.NE.1) THEN
        
            IF (IVERB.EQ.1) THEN
                WRITE(6,*)'Modified means that you could change each ', &
                  'tube length as a function of position.'
            END IF
            F_EXIT_SHAPE = IYES('B: Is  this a modified exit plane?')
            !c	      F_EXIT_SHAPE = 0
            FILE_KOMA= RSTRING('File with the parameters?')
        ELSE
            FILE_KOMA_CA = RSTRING('File with the parameters? ')
            F_EXIT_SHAPE = 0
        END IF
        
        FMIRR = 5
        
    ELSE  !<koma>
        !c
        !c------------------------------------------------------------************
        !c Facet question
        !c Added by G.J. 5/12/92
        !c
        
        IF (IVERB.EQ.1) THEN
           WRITE(6,*) 'Compound mirrors ( or lenses) are formed'
           WRITE(6,*) 'by several smaller mirrors ( facets) combined together.'
        END IF
        F_FACET = IYES('A: Is this mirror faceted [Y/N] ?')
        IF (F_FACET.EQ.1) THEN
            FILE_FAC = RSTRING ('B: File with the polynomial describing the facet?')
            IF (IVERB.EQ.1) THEN
                WRITE(6,*) 'We need to define which side of the surface'
                WRITE(6,*) 'to use (see demo for further explanation).'
            END IF
            F_POLSEL=0
            WRITE(6,*)' 3      closest'
            WRITE(6,*)' 2      ...'
            WRITE(6,*)' 1      ...'
            WRITE(6,*)' 4      farthest'
            F_POLSEL = IRINT('Choice[1-4]?')
            WRITE(6,*)'Size of the Facet?'
            RFAC_LENX =RNUMBER(' x-length ?')
            RFAC_LENY =RNUMBER(' y-length ?')
        END IF
        
        IF (IVERB.EQ.1) THEN
            IF(F_FACET.EQ.1) THEN
                WRITE(6,*)'Choose the baseline for facet'
            END IF
            print *,''
            print *,'Lets define the mirror. I may compute its parameters, like the radius or the '
            print *,'axes. This will not affect the rest of the calculations; all the geometrical '
            print *,'parameters may be modified later. Or, you may wish to specify the mirror     '
            print *,'parameters yourself.                                                         '
        
                                                       
            WRITE (6,30)
        30  FORMAT (//,1X,'What kind of surface are we dealing with ?',/,1X,&
                'spherical    = 1',/,1X,&
                'elliptical   = 2',/,1X,&
                'toroidal     = 3',/,1X,&
                'paraboloid   = 4',/,1X,&
                'plane        = 5',/,1X,&
                'Codling slit = 6',/,1X,&
                'hyperbolical = 7',/,1X,&
                'cone	      = 8',/,1X,&
                'polynomial   = 9',/)
        END IF
        
        FMIRR = IRINT ('Mirror surface [ 1-9] ? ')
        IF (FMIRR.NE.9) THEN
            IF (FMIRR.EQ.4) THEN 
                WRITE(6,*)'Paraboloid Selected.'
                WRITE(6,*)'Enter 0 (1) if the image (source) is at infinity.'
                WRITE(6,*)'The inputs for the relative positions are disregarded.'
                F_SIDE = IRINT ('Focus location ? ')
            ELSE IF (FMIRR.EQ.3) THEN
                IF (F_FACET.EQ.1) THEN
                      WRITE(6,*)'In facet, only selection 0 has been developed'
                      WRITE(6,*)'Do not use Angle=0 0 The R_MAJ will be 0'
                END IF
                WRITE(6,*)'Toroidal Selected.'
                WRITE(6,*)'Enter 0, if mirror pole is at the lower outer '//&
                    'torus (usual case)'
                WRITE(6,*)'      1,                          lower inner'
                WRITE(6,*)'      2,                          upper inner'      
                WRITE(6,*)'      3,                          upper outer'
                F_TORUS = IRINT ('Mirror pole location ? ')
            END IF
            IF (FMIRR.EQ.8) THEN
                F_EXT = 1
            ELSE IF (FMIRR.NE.5.AND.FMIRR.NE.6) THEN
                F_EXT = IYES ('Do you want to specify the mirror parameters ?')
            END IF
            FCYL = IYES ('Is the mirror Cylindrical ? ')
            IF (FCYL.EQ.1) THEN
                CIL_ANG = RNUMBER ('Angle of cylinder axis from x axis [ 0 ] ?')
            END IF
        ELSE
                  FILE_MIR = RSTRING ('File with polynomial coefficients ? ')
        END IF
        
        !c
        !c end of the Kumakof IF block
        !c
    END IF  !<koma>
    !c
    !c end of the segmented mirror IF block
    !c
END IF  ! end block if-segmented
 

FZP = IYES('Is this optical element a Fresnel Zone Plate ? ')

IF (FZP.EQ.1) THEN
    F_GRATING = 1
ELSE
    F_GRATING=IYES ('Are we dealing with a Grating ? ')
END IF

IF (F_GRATING.EQ.1) THEN ! grating
    WRITE(6,*)
    !**-------------------------------------------------------------------
    IF (FZP.EQ.1) THEN
         WRITE(6,*)' For a Fresnel Zone Plate select the appropriate '//&
          'HOLOGRAPHIC case.'
    END IF
    WRITE(6,*)'Type of ruling.'
    WRITE(6,*) '0    ruling density constant on the X-Y plane'
    WRITE(6,*) '1    for ruling density constant onto the mirror surface.'
    WRITE(6,*) '2    for an holographic grating.'
    WRITE(6,*) '3    for an oriental-fan type'
    WRITE(6,*) '4    reserved'
    WRITE(6,*) '5    polynomial line density'
    F_RULING = IRINT ('Then: ')
    IF (F_RULING.EQ.0.OR.F_RULING.EQ.1) THEN
        RULING =   RNUMBER ('Lines per CM ? ')
    ELSE IF (F_RULING.EQ.2) THEN
        IF (IVERB.EQ.1) THEN
            WRITE(6,*)'Hologram Recording Parameters.'
            WRITE(6,*)'The angles are positive if the source is on the' &
              //' side of the Y-axis.'
            WRITE(6,*)'"EnS" means Entrance Slit Side Source'
            WRITE(6,*)'"Rotation" refers to rotation around Z'
            WRITE(6,*)'Distances [ cm! ] and angles [ degrees ] refer ', &
              'to hologram.'
        END IF
        HOLO_R1 =  RNUMBER('EnS distance: ')
        HOLO_DEL=  RNUMBER('EnS incidence angle:    ')
        HOLO_RT1=  RNUMBER('EnS rotation  angle:    ')
        HOLO_R2 =  RNUMBER('ExS distance: ')
        HOLO_GAM=  RNUMBER('ExS incidence angle:    ')
        HOLO_RT2=  RNUMBER('ExS rotation  angle:    ')
        HOLO_W  =  RNUMBER('Recording Wavelength [ Angs ] ? ')
        WRITE(6,*)'Type of recording sources. We have the choices: '
        WRITE(6,*)'Both    SPHERICAL                    0'
        WRITE(6,*)'Source  PLANE,     image SPHERICAL   1'
        WRITE(6,*)'Source  SPHERICAL, image PLANE       2'
        WRITE(6,*)'Source  PLANE,     image PLANE       3'
        F_PW =  IRINT ('Source type [ 0-3 ]. Then ? ')
        WRITE(6,*)'The SPHERICAL source can also be specified to be '// &
          'CYLINDRICAL, with axis perpendicular to recording plane.'
         WRITE(6,*)'Use: '
        WRITE(6,*)'None   Cylindrical                    0'
        WRITE(6,*)'Source Cylindrical, image Spherical   1'
        WRITE(6,*)'Source Spherical,   image Cylindrical 2'
        WRITE(6,*)'Both   Cylindrical                    3'
        F_PW_C =  IRINT ('Cylindrical [ 0-3 ] ? ')
        WRITE(6,*)'Sources REAL/ VIRTUAL: '
        WRITE(6,*)'EnS real,    ExS real        0'
        WRITE(6,*)'EnS real,    ExS virtual:    1'
        WRITE(6,*)'EnS virtual, ExS real:       2'
        WRITE(6,*)'EnS virtual, ExS virtual     3'
        F_VIRTUAL = IRINT ('Then ? ')
        WRITE(6,*)' '
    ELSE IF (F_RULING.EQ.3) THEN
        WRITE(6,*)' Position of ruling focus on mirror plane.'
        AZIM_FAN = RNUMBER ('Angle from Y axis         [ deg, CCW ] ? ')
        DIST_FAN = RNUMBER ('Distance from grating center    [ cm ] ? ')
        COMA_FAC = RNUMBER ('Coma correction factor  [ 0 for none ] ? ')
        RULING   = RNUMBER ('Line density at grat. cent.   [ l/cm ] ? ')
    ELSE IF (F_RULING.EQ.5) THEN
        WRITE(6,*)'Degree of polynomial is 4 :'
        WRITE(6,*)'   density = a0 + a1*w + a2*w^2 + a3*w^3 + a4*w^4'
        WRITE(6,*)'NOTICE : for a0, please enter the LINE DENSITY AT ORIGIN'
        WRITE(6,*)'units of density is LINES/CM'
        WRITE(6,*)'Please enter coefficients :'
        RULING = RNUMBER ('a0 : ')
        RUL_A1 = RNUMBER ('a1 : ')
        RUL_A2 = RNUMBER ('a2 : ')
        RUL_A3 = RNUMBER ('a3 : ')
        RUL_A4 = RNUMBER ('a4 : ')
        WRITE(6,*)'Use ABSOLUTE [ 0 ] or SIGNED [ 1 ] from origin ? '
        F_RUL_ABS = IRINT ('Then ? ')
        WRITE(6,*)'All set then.'
    END IF


    IF (IVERB.EQ.1) WRITE(6,*) 'We follow the European convention. NEGATIVE orders are inside.'
    ORDER    =  RNUMBER('Diffraction Order ? ')
    IF (IVERB.EQ.1) WRITE(6,*) &
        'Enter 1 if you want me to position the grating at the correct' &
           //'wavelength, 0 to leave everything as it is.'
    F_CENTRAL =  IRINT ('Auto Tuning of Grating [ Y/N ] ? ')
    IF (F_CENTRAL.EQ.1) THEN
        IF (IVERB.EQ.1) WRITE(6,*) &
            'You must then supply the wavelength (or photon energy) at '// &
            'which you want the grating tuned.'
        F_PHOT_CENT =  IRINT ('Energy, in eV, [ 0 ] or wavelength, in Angs., [ 1 ] ? ')
        IF (F_PHOT_CENT.EQ.0) PHOT_CENT = RNUMBER ('Photon Energy ? ')
        IF (F_PHOT_CENT.EQ.1) R_LAMBDA  = RNUMBER ('Wavelength    ? ')
        IF (IVERB.EQ.1) THEN
            WRITE(6,*) 'SHADOW recognizes several types of Grating mounts and/or '
            WRITE(6,*) 'monochromators. '
            WRITE(6,*) 'We have several choices, depending on what kind of mount you '
            WRITE(6,*) 'are using. They are :'
        
            WRITE(6,*) '	TGM/SEYA  mount (constant included angle).'
            WRITE(6,*) ' In this case the source/image distances are not changed from'
            WRITE(6,*) ' the one already specified and the grating is rotated to match'
            WRITE(6,*) ' the diffraction conditions.'

            WRITE(6,*) ' ERG/GRASSHOPPER (constant incidence angle). The image' 
            WRITE(6,*) ' plane is positioned on the Rowland circle at the appropriate' 
            WRITE(6,*) ' diffraction angle.'
            WRITE(6,*) ' '
            WRITE(6,*) ' CONSTANT INCIDENCE ANGLE and image plane at the' 
            WRITE(6,*) ' position already specified. Only the diffraction angle is'
            WRITE(6,*) ' modified.'
            WRITE(6,*) ' ' 
            WRITE(6,*) ' CONSTANT DIFFRACTION ANGLE. The incidence angle'
            WRITE(6,*) ' is modified to match the diffraction conditions. The planes'
            WRITE(6,*) ' are not moved.'
            WRITE(6,*) ' ' 
            WRITE(6,*) ' for a CONSTANT BLAZE mount (Hunter type)'
        END IF

        WRITE(6,*)'TGM/SEYA    0'
        WRITE(6,*)'ERG         1'
        WRITE(6,*)'Con Inc Ang 2'
        WRITE(6,*)'Con Dif Ang 3'
        WRITE(6,*)'Hunter      4'
        F_MONO   =   IRINT ('Mount type ? ')
        IF (F_MONO.EQ.4) THEN
            BLAZE   =   RNUMBER ('Blaze Angle [ Deg ] ? ')
            HUNT_L  =   RNUMBER ('Monochromator Length ? ')
            HUNT_H   =   RNUMBER ('Monochr. Height (Distance between Beams) ')
            F_HUNT  =   IRINT ('First [ 1 ] or Second [ 2 ] grating ? ')
        END IF
    ELSE
    END IF
END IF !grating
!**------------------------------------------------------------------
WRITE(6,*)
!**------------------------------------------------------------------

IF (F_GRATING.EQ.0) THEN  !no-grating
    F_CRYSTAL = IYES ('Are we dealing with a crystal [ Y/N ] ? ')
    IF (F_REFRAC.EQ.1.and.f_crystal.ne.1) THEN     !lens
        !-----
        print *,' Enter index of refraction in the two media (ONJECT and IMAGE spaces):'
        print *,'    '
        print *,' Index of refraction (optical constants) from: '
        print *,'     0: constant: keyword in both media'
        print *,'     1: file(from prerefl) in first medium keyboard in next medium'
        print *,'     2: keyboard in first medium and file (from prerefl) next medium'
        print *,'     3: file(from prerefl) in both media'
        print *,'     '
        f_r_ind = irint('?>')
        WRITE(6,*)

        select case (f_r_ind)
            case (0) 
                  !WRITE(6,*) 'Enter the index of refraction in OBJECT space:'
                  !R_IND_OBJ = RNUMBER ('Object space: ')
                  !WRITE(6,*) 'Enter the index of refraction in IMAGE space:'
                  !R_IND_IMA = RNUMBER ('Image space: ')
                  R_IND_OBJ = RNUMBER(' Enter the index of refraction in OBJECT space: ')
                  R_IND_IMA = RNUMBER(' Enter the index of refraction in IMAGE space: ')
                  R_ATTENUATION_OBJ = &
                       RNUMBER(' Enter attenuation coeff [UserLength^-1] in OBJECT space: ')
                  R_ATTENUATION_IMA = &
                       RNUMBER(' Enter attenuation coeff [UserLength^-1] in IMAGE space: ')
            case (1) 
                  FILE_R_IND_OBJ = rstring(" Enter file (from prerefl) for OBJECT space: ")
                  R_IND_IMA = RNUMBER(' Enter the index of refraction in IMAGE space: ')
                  R_ATTENUATION_IMA = &
                       RNUMBER(' Enter attenuation coeff [UserLength^-1] in IMAGE space: ')
            case (2) 
                  R_IND_OBJ = RNUMBER(' Enter the index of refraction in OBJECT space: ')
                  R_ATTENUATION_OBJ = &
                       RNUMBER(' Enter attenuation coeff [UserLength^-1] in OBJECT space: ')
                  FILE_R_IND_IMA = rstring(" Enter file (from prerefl) for IMAGE space: ")
            case (3) 
                  FILE_R_IND_OBJ = rstring(" Enter file (from prerefl) for OBJECT space: ")
                  FILE_R_IND_IMA = rstring(" Enter file (from prerefl) for IMAGE space: ")
        end select 
    !-----
    END IF !lens

    IF (F_CRYSTAL.EQ.1) THEN !crystal
        WRITE(6,*) 'File containing crystal parameters ?'
        READ    (5,111)    FILE_REFL
        F_MOSAIC = IYES ('Is it a mosaic crystal [ Y/N ] ? ')
        if (f_mosaic.eq.1.or.f_refrac.eq.1) THICKNESS = RNUMBER ('What is the crystal thickness [cm] ? ')
        IF (F_MOSAIC.EQ.1) THEN ! mosaic=yes
            IF (IVERB.EQ.1) THEN 
                WRITE(6,*) 'I assume that the mosaic crystal small blocks follow a gaussian'
                WRITE(6,*) 'distribution of a given FWHM spread.This spread must be much'
                WRITE(6,*) 'larger than the rock curve width. I suposse that the distance'
                WRITE(6,*) 'between intercepts on the mosaic surface will be larger enough'
                WRITE(6,*) 'than the small mosaic block size.I use a random seed to gene-'
                WRITE(6,*) 'rate the gaussian distribution of the blocks around the sur-'
                WRITE(6,*) 'face normal.I also need the thickness of the whole crystal'
                WRITE(6,*) '(faces must be parallel) for the reflectivity calculation.'
                WRITE(6,*) 'Mosaic crystal force us to use cm as unit.'
            END IF
            SPREAD_MOS = RNUMBER ('What is the mosaic spread FWHM [deg] ? ')
            MOSAIC_SEED  = IRINT ('Give me a random seed (integer, 0=get from clock) ? ')
        ELSE IF (F_MOSAIC.NE.1) THEN  ! mosaic-no
            F_BRAGG_A = IYES ('Is the crystal asymmetric [ Y/N ] ? ')
            IF (F_BRAGG_A.EQ.1) THEN
                IF (IVERB.EQ.1) THEN
                    WRITE(6,*) ' Enter the angle formed by the crystal planes with'
                    WRITE(6,*) ' the crystal surface. Use a positive value if the plane normal'
                    WRITE(6,*) ' lies along the +Y direction (focusing), negative otherwise.'
                END IF
                A_BRAGG = RNUMBER('Planes angle [ deg ] ? ')

                if (f_refrac.eq.1) then
                    WRITE(6,*) 'For the Laue case: is the beam arriving onto the'
                    WRITE(6,*) ' bragg planes [0] , or under the bragg planes [1] ?'
                    f_planes = iyes(' <?> ')
                    if (f_planes.eq.1) then 
                        order = -1
                    else
                        order = +1
                    end if
                end if
            END IF
            F_JOHANSSON = IYES ('Are we working in Johansson geometry [Y/N] ? ')
            IF (F_JOHANSSON.EQ.1) THEN
                IF (IVERB.EQ.1) THEN
                    WRITE(*,*) 'Johansson geometry is when you grind a crystal plate with a'
                    WRITE(*,*) 'spherical shape of a given radius (Johansson radius),and then,'
                    WRITE(*,*) 'the crystal is bent again with another different radius'
                    WRITE(*,*) '(surface radius). For the optimal curvature, the surface must'
                    WRITE(*,*) 'lie on the Rowland circle and the Johansson radius should be'
                    WRITE(*,*) 'twice the Rowland circle radius. Both are calculated automati-'
                    WRITE(*,*) 'cally if you have chosen automatic parameters calculation in'
                    WRITE(*,*) 'a spherical shape surface.    '  
                END IF
                IF (F_EXT.EQ.1) THEN
                    R_JOHANSSON = RNUMBER ('Johansson radius  ? ')
                END IF
            END IF
        END IF !mosaic-end
        F_CENTRAL = IYES ('Automatic Tuning of Crystal [ Y/N ] ?')
        IF (F_CENTRAL.EQ.1) THEN
            IF (IVERB.EQ.1) THEN 
                WRITE(6,*) 'You must then supply the wavelength (or photon energy) at '
                WRITE(6,*) 'which you want the crystal tuned.'
            END IF
            F_PHOT_CENT =  IRINT ('Energy, in eV, [ 0 ] or wavelength, in Angs., [ 1 ] ? ')
            IF (F_PHOT_CENT.EQ.0) PHOT_CENT = RNUMBER ('Photon Energy ? ')
            IF (F_PHOT_CENT.EQ.1) R_LAMBDA  = RNUMBER ('Wavelength    ? ')
        END IF  
    END IF !crystal
END IF  ! no-grating
!c
!c---------------------------------------------------------------------
!c
F_CONVEX =  IYES ('Is the mirror convex [ Y/N ] ? ')
IF (F_REFRAC.EQ.0) THEN
    IF (IVERB.EQ.1) THEN
        WRITE(6,*) 'Reflectivity of Surface. SHADOW may solve the Fresnel equations locally. '
        WRITE(6,*) 'Available options:'
        WRITE(6,*) 'No reflectivity              .......... 0 '
        WRITE(6,*) 'Full polarization dependence .......... 1 '
        WRITE(6,*) 'No        "           "      .......... 2 '
    END IF
    F_REFLEC  =  IRINT ('Reflectivity mode [ 0,1,2 ] ? ')
    IF (F_REFLEC.NE.0) THEN
        WRITE(6,*)
       IF (IVERB.EQ.1) THEN
           WRITE(6,*) &
              'Optical constants from file  ( for multi-line source ) ..... 0',&
              'Optical constants from keyboard  (single-line source ) ..... 1',&
              'Define multilayer from file                            ..... 2'
       END IF
       F_REFL = IRINT ('Optical Constant Source: [file=0,tt:=1], multilayer [2] ?')
       IF (F_REFL.EQ.1) THEN
           WRITE(6,*) &
            'Please enter the complex dielectric constant. ', &
            'The form I use is :', &
            '           Epsilon  = ( 1 - ALPHA) + i*GAMMA'
           ALFA = RNUMBER ('ALPHA = ')
           GAMMA= RNUMBER ('GAMMA = ')
       ELSE IF (F_REFL.EQ.0) THEN
           WRITE(6,*) 'File with optical constants ?'
           READ (5,111) FILE_REFL
       ELSE
           WRITE(6,*) 'File with thicknesses and refractive indices of multilayer ?'
           READ (5,111)FILE_REFL
           F_THICK = IYES ('Vary thicknesses as the cosine of the angle from the pole? ')
       END IF 
    ELSE
    END IF
END IF  

!c------------------------------------------------------------------------
WRITE(6,*)
!c---------------------------------------------------------------------

IF (IVERB.EQ.1) THEN
    print *,'Mirror orientation angle. Angles are measured CCW, in deg,     '
    print *,'referring to the mirror normal. Alpha=0 is the mirror          '
    print *,'sitting at the origin, facing up. Alpha = 90 is the            '
    print *,'mirror standing at your right and facing left when you         '
    print *,'look along the beam STANDING ON THE PREVIOUS MIRROR and so on. '
END IF
ALPHA = RNUMBER ('Orientation Angle [ Alpha ] ? ')

!c----------------------------------------------------------------------
WRITE(6,*)
!c----------------------------------------------------------------------

FHIT_C = IYES ('Mirror Dimensions finite [ Y/N ] ?')
IF (FHIT_C.EQ.1) THEN
    IF (IVERB.EQ.1) THEN
         WRITE(6,*)'Mirror shape. Options:'
         WRITE(6,*)'         rectangular :    1'
         WRITE(6,*)'   full  elliptical  :    2'
         WRITE(6,*)'  "hole" elliptical  :    3'
    END IF
    FSHAPE = IRINT ('Shape: [ 1, 2, 3] ?')
    IF (FSHAPE.EQ.1) THEN
        RWIDX1 = RNUMBER ('Mirror half-width x(+) ? ')
        RWIDX2 = RNUMBER ('Mirror half-width x(-) ? ')
        RLEN1  = RNUMBER ('Mirror half-length y(+) ? ')
        RLEN2  = RNUMBER ('Mirror half-length y(-) ? ')
    ELSE 
        RWIDX2 = RNUMBER ('External Outline Major axis ( x ) ? ')
        RLEN2  = RNUMBER ('External Outline Minor axis ( y ) ? ')
        IF (FSHAPE.EQ.3) THEN
            RWIDX1 = RNUMBER ('Internal Outline Major axis ( x ) ? ')
            RLEN1 = RNUMBER ('Internal Outline Minor axis ( y ) ? ')
        END IF
    END IF
END IF
!*----------------------------------------------------------------------
WRITE(6,*)
!*----------------------------------------------------------------------
IF (FMIRR.EQ.6) THEN
    COD_LEN = RNUMBER ('Codling Slit Length [ Sagittal ] ? ')
    COD_WID = RNUMBER ('Codling Slit Width  [ Effective, Tangential] ? ')
    GO TO 5001
END IF
IF (FMIRR.EQ.9.OR.FMIRR.EQ.5) GO TO 5001
!* Inquires about the mirror optical parameters.
IF (F_EXT.EQ.0) THEN
    IF (IVERB.EQ.1) THEN
        print *,'The mirror will be computed from the optical parameters'        
        print *,'that you supply. For example,in the case of a spherical mirror' 
        print *,'I will compute the radius of the mirror to satisfy the equation'
        print *,'      1/p + 1/q = 2/(R*cos(theta))  given p,q and theta.'         
        print *,'This will NOT affect in any way the placement of the mirror in' 
        print *,'the optical element.'
    END IF
    F_DEFAULT = IYES ('Focii placed at continuation planes [ Y/N ] ? ')
    IF (F_DEFAULT.EQ.0) THEN
        IF (IVERB.EQ.1) THEN
            print *,' '
            print *,'The mirror parameters will be specified by using either polar' 
            print *,'or cartesian coordinates for the source position. Then? '
        END IF
        !c
        !c Using polar coordinates.
        !c
        SSOUR      = RNUMBER ('Object Focus to Mirror Pole Distance: ')
        THETA      = RNUMBER ('Incidence Angle [ degrees ] ? ')
        SIMAG      =  RNUMBER ('Mirror pole to Image Focus Distance ? ')
    ELSE
    END IF
END IF
IF (F_EXT.EQ.1) THEN
    IF (FMIRR.EQ.1) THEN
        RMIRR =   RNUMBER ('Spherical Mirror Radius ? ')
    ELSE IF (FMIRR.EQ.2) THEN
        AXMAJ      = RNUMBER ('Elliptical Mirror Semi-Major Axis ? ')
        AXMIN      = RNUMBER ('Elliptical Mirror Semi-Minor Axis ? ')
        IF (IVERB.EQ.1) THEN
            WRITE(6,*) 'In the elliptical mirror case, it is also necessary to'
            WRITE(6,*) 'specify which part of the ellipse is being used. This '
            WRITE(6,*) 'is achieved by specifing the angle between the major '
            WRITE(6,*) 'axis and the vector from the ellipse center to the '
            WRITE(6,*) 'mirror pole.'
        END IF
        ELL_THE = RNUMBER ('CCW angle between semi-major axis and pole ? ')
    ELSE IF (FMIRR.EQ.3) THEN
        WRITE(6,*) 'Care !! Enter the torus MAXIMUM radius and the minor radius'
        R_MAJ      =  RNUMBER ('Tangential [ Major ] Radius ? ')
        R_MIN      =  RNUMBER ('Sagittal [ Minor ] Radius ? ')
    ELSE IF (FMIRR.EQ.4) THEN
        IF (IVERB.EQ.1) THEN
            WRITE(6,*)'Equation used is      2'
            WRITE(6,*)'                    Y    =   2 P X '
        END IF
        PARAM      =  RNUMBER ('Parabola Parameter ? ')
    ELSE IF (FMIRR.EQ.7) THEN
        AXMAJ      = RNUMBER ('Hyperbolical Mirror Semi-Major Axis ? ')
        AXMIN      = RNUMBER ('Hyperbolical Mirror Semi-Minor Axis ? ')
        IF (IVERB.EQ.1) THEN
            print *,'In the hyperbolical mirror case, it is also necessary '
            print *,'to specify which part of the ellipse is being used. '
            print *,'This is achieved by specifing the angle between the '
            print *,'major axis and the vector from the hyperbola center '
            WRITE(6,*)'to the mirror pole.'
        END IF
        ELL_THE = RNUMBER ('CCW angle between semi-major axis and pole ? ')
    ELSE IF (FMIRR.EQ.8) THEN
        CONE_A      = RNUMBER ('Cone Half-Opening [deg] ? ')
    END IF
ELSE
END IF
!**---------------------------------------------------------------------
WRITE(6,*)
!**-----------------------------------------------------------------------
5001      CONTINUE
IF (IVERB.EQ.1) THEN
    print *,'It may be helpful to save the exact incidence and reflection '
    print *,'angles for each ray.  The saved file contains the index of the '
    print *,'ray, the incidence angle (in degrees), and the reflection angle'
    print *,'for each ray hitting this element.'
END IF
F_ANGLE = IYES('Save incidence and reflection angles to disk? ')
IF (IVERB.EQ.1) THEN
    print *,' The Optical Element and the the relative mirror are now fully '
    print *,'defined. The mirror pole is now located at the "center" of the '
    print *,'optical element. It is possible to override this situation and '
    print *,'"move" the mirror without affecting the rest of the system.'    
    print *,' It is also possible to move the "source" without affecting the'
    print *,'rest of the system.'                                            
    print *,' The movements are expressed in the DEFAULT Mirror Ref. Frame.' 
    print *,' so that if you move BOTH source and mirror the relative'       
    print *,'movement is the vector sum of the individual ones.'             
    print *,'A word of caution: SOURCE movements and MIRROR movements are'   
    print *,'NOT equivalent from the point of view of the whole system.'
END IF
FSTAT =  IYES ('Do you want to move the Source [ Y/N ] ? ')
IF (FSTAT.EQ.1) THEN
    WRITE(6,*)'Source movements in SOURCE REFERENCE FRAME.'
    WRITE(6,*)'CW rotations are (+) angles.'
    X_SOUR            =         RNUMBER      ('X-offset ? ')
    Y_SOUR             =        RNUMBER      ('Y-offset ? ')
    Z_SOUR            =         RNUMBER      ('Z-offset ? ')
    X_SOUR_ROT      =      RNUMBER      ('X-rotation ? ')
    Y_SOUR_ROT      =      RNUMBER      ('Y-rotation ? ')
    Z_SOUR_ROT      =      RNUMBER      ('Z-rotation ? ')
    ALPHA_S = RNUMBER ('Source rotation around Z-axis. CCW is > 0 ')
    RDSOUR = RNUMBER ('Source Distance from Pole ? ')
    RTHETA = RNUMBER ('Incidence Angle [ degrees ] ? ')
    IF (IVERB.EQ.1) THEN
        WRITE(6,*)'The previous changes are in the SOURCE frame.'
        WRITE(6,*)'The following OFFSETS are applied in the MIRROR'
        WRITE(6,*)'reference frame.'
    END IF
    OFF_SOUX = RNUMBER ('Source offset in [ x ] ? ')
    OFF_SOUY = RNUMBER ('                 [ y ] ? ')
    OFF_SOUZ = RNUMBER ('                 [ z ] ? ')
ELSE
END IF
IF (IVERB.EQ.1) THEN
    WRITE(6,*)'   '
    WRITE(6,*)'--- Mirror rotations and position. ---'
    print *,'We define three angles, as rotations around the three axis.'   
    print *,'These rotation are  defined in the program as corrections to'  
    print *,'the mirror nominal position; that is, they modify the mirror'  
    print *,'position relative to the Default Mirror Reference Frame, where'
    print *,'all the calculations are performed. Remember that rotations'   
    print *,'do NOT commute. I apply them in the same order of entry.'      
    print *,'CW ROTATIONS are (+) angles. '                                 
    print *,'A translation can be also applied to the mirror.'
END IF
F_MOVE = IYES ('Do you want to move the mirror itself [ Y/N ] ? ')
IF (F_MOVE.EQ.1) THEN
    X_ROT      =  RNUMBER ('Rotation around X axis [ degrees ] ? ')
    Y_ROT      =  RNUMBER ('                Y                  ? ')
    Z_ROT      =  RNUMBER ('                Z                  ? ')
    OFFX      =  RNUMBER ('Mirror Offset. In X ? ')
    OFFY      =  RNUMBER ('                  Y ? ')
    OFFZ      =  RNUMBER ('                  Z ? ')
ELSE
END IF
F_RIPPLE = IYES ('Distorted surface [ Y/N ] ? ')
IF (F_RIPPLE.EQ.1) THEN             
    IF (IVERB.EQ.1) THEN
        WRITE(6,*)'Sinusoidal ripple (0)'
        WRITE(6,*)'Gaussian ripple   (1)'
        WRITE(6,*)'External spline   (2)'
    END IF
    F_G_S      = IRINT ('Type of distortion ? ')
    IF (F_G_S.NE.0) THEN      
        FILE_RIP      = RSTRING('File to read ? ')
    ELSE
        WRITE(6,*) 'Wavelength along the X-axis ?'
        READ(5,*)X_RIP_WAV
        WRITE(6,*) '             and the Y-axis ?'
        READ(5,*)Y_RIP_WAV
        WRITE(6,*) 'Amplitude along the X-axis ?'
        READ(5,*)X_RIP_AMP
        WRITE(6,*) '            and the Y-axis ?'
        READ(5,*)Y_RIP_AMP
        WRITE(6,*) 'Phase for X-axis. 0 means a maximum at the origin. Then ?'
        READ(5,*)X_PHASE
        WRITE(6,*) ' and  for Y-axis ?'
        READ(5,*)Y_PHASE
    END IF
ELSE                                    ! RIP 1
END IF
f_roughness = iyes('Do you want to include surface roughness [Y/N] ? ' )
if (f_roughness.eq.1) then
    file_rough  = rstring( 'File to read ? ')
    rough_y = rnumber &
      ('Roughness RMS in Y direction (along the mirror) [Angstroms] ? ')
    rough_x = rnumber &
      ('Roughness RMS in X direction (transversal) [Angstroms]? ')
end if
!c
!**-----------------------------------------------------------------------
WRITE(6,*)
!**--------------------------------------------------------------------
F_SCREEN = IYES ('Any screens in this OE [ Y/N ] ? ')
IF (F_SCREEN.NE.0) THEN
    N_SCREEN = IRINT ('How many in this OE [ total ] ? ')
    DO 11 I=1,N_SCREEN
        WRITE(6,*)'Screen N. ',I
        I_SCREEN(I) = IYES ('Is this screen before mirror [ Y/N ] ? ')
        SL_DIS(I) = RNUMBER('Distance from mirror [ absolute ] ? ')
        I_SLIT(I) = IYES ('Is Screen Carrying an Aperture Stop [ Y/N ] ? ')
        IF (I_SLIT(I).EQ.1) THEN
            I_STOP(I) = IRINT ('Obstruction [ 1 ] or Aperture [ 0 ] ? ')
            IF (IVERB.EQ.1) THEN
                WRITE(6,*)'Kind of slit. Use:'
                WRITE(6,*)'0            for a rectangular slit'
                WRITE(6,*)'1            for an elliptical slit'
                WRITE(6,*)'2            for an "external" slit.'
            END IF
            K_SLIT(I) = IRINT ('Stop shape [ 0 r, 1 e, 2 ex ] ? ')
            IF (K_SLIT(I).EQ.2) THEN
                !        XXFILSCR(I) = RSTRING('File containing the mask coordinates ? ')
                FILE_SCR_EXT(I) = RSTRING('File containing the mask coordinates ? ')
            ELSE
                RX_SLIT(I)= RNUMBER ('Dimension along X ? ')
                RZ_SLIT(I)= RNUMBER ('                Z ? ')
                CX_SLIT(I)= RNUMBER ('Center along X [0.0] ? ')
                CZ_SLIT(I)= RNUMBER ('             Z [0.0] ? ')
            END IF
        ELSE
        END IF
        I_ABS(I)= IYES ('Include absorption [ Y/N ] ? ')
        IF (I_ABS(I).EQ.1) THEN
            WRITE(6,*) 'File with optical constants ?'
            READ (5,111)      FILE_ABS(I)
            THICK(I) = RNUMBER ('Thickness of film [ cm ] ? ')
        END IF
11  CONTINUE

ELSE
END IF
!**---------------------------------------------------------------------
WRITE(6,*)
!**---------------------------------------------------------------------
FSLIT = IYES ('Slit at continuation plane [ Y/N ] ? ')
IF (FSLIT.EQ.1) THEN 
    SLLEN      = RNUMBER ('Slit Length in Sagittal Plane [ x ] ? ')
    SLWID   = RNUMBER ('Slit Width in Dispersion Plane [ z ] ? ')
    SLTILT      = RNUMBER ('Slit Tilt, CCW [ degrees ] ? ')
ELSE
END IF
N_PLATES= IYES ('Extra Image plates [ Y/N ] ? ')
IF (N_PLATES.EQ.1) N_PLATES = IRINT ('How many ? ')
IF (N_PLATES.GT.0) THEN
    I      =   1
21  continue
    IF (I.LE.N_PLATES) THEN
        WRITE(6,*)'Plate # ',I
        D_PLATE(I) = RNUMBER ('Distance Mirror Center Plate ? ')
        I       =   I + 1
        GOTO 21
    END IF
    F_PLATE = IYES ('Plates at orthogonal to Optical Axis ? ')
    IF (F_PLATE.EQ.0) THEN
        IF (IVERB.EQ.1) THEN
            print *,'Image plane orientation: the plane is defined by two angles.'
            print *,'Take the Y = 0 plane, then rotate it around the X-axis CCW.' 
            print *,'This is your first angle (elevation). Then rotate it around' 
            print *,'the Z-axis, still CCW (azimuth) and there you are.'
        END IF
        THETA_I = RNUMBER ('Elevation [ degrees ] ? ')
        ALPHA_I = RNUMBER ('Azimuth               ? ')
    ELSE
    END IF
ELSE
END IF
!c
!c---------------------------------------------------------------------
!c
!c                SOURCE   FILES
!c
!c This question applies only to the first OE of an optical system
!c
!c
IF (I_OENUM.EQ.1) THEN
    FILE_SOURCE = RSTRING ('File containing the source array [Default: begin.dat] ? ')
    IF (trim(FILE_SOURCE) == "") FILE_SOURCE="begin.dat"
END IF
10101	CONTINUE
!c
!c-----------------------------------------------------------------------
!c
!c                  SAVE     FILE
!c
!c
CALL  FNAME  (FFILE, 'start', I_OENUM, izero)
IDUMM = 0
CALL  RWNAME  (FFILE, 'W_OE', IDUMM)
IF (IDUMM.NE.0) CALL LEAVE ('INPUT_OE','Error writing NAMELIST',IDUMM)
111  FORMAT (A)
WRITE(6,*)'Exit from INPUT'

End Subroutine input_oe


!
!
!


    ! C+++
    ! C	SUBROUTINE	INPUT_SOURCE1
    ! C
    ! C	PURPOSE		Reads in the parameters of the optical system
    ! C
    ! C---

    !
    !  The old subroutine "input_source" has been moved to input_source1
    !  and the old main program r_inpsour.F (whose exucutable was 
    !  input_source) is now input_source
    !  
SUBROUTINE INPUT_SOURCE1
    
     implicit none
!      IMPLICIT REAL(kind=skr) (A-E,G-H,O-Z)
!      IMPLICIT INTEGER(kind=ski)        (F,I-N)
!     character(len=512)           :: FILE_SOURCE, FILE_BOUND, FILE_TRAJ, FFILE
     character(len=80)    :: MSSG1,MSSG2
     integer(kind=ski)      :: oe_number,iDefault,iVerb,i,iAnsw,i_Col,iErr,lun

    !
    ! define defaults (a geometrical source)
    !
         	WRITE(6,*)'Call to INPUT_SOURCE'


     !iDefault = IYES("Write default start.00 file? [Y/N]: ")
     iDefault = 0
     if (iDefault == 1) then 
       FDISTR =  2
       FGRID =  0
       FSOUR =  3
       FSOURCE_DEPTH =  1
       F_COHER =  0
       F_COLOR =  1
       F_PHOT =  0
       F_POL =  3
       F_POLAR =  0
       F_OPD =  1
       F_WIGGLER =  0
       F_BOUND_SOUR =  0
       F_SR_TYPE =  0
       ISTAR1 =  6775431
       NPOINT =  30000
       NCOL =  0
       N_CIRCLE =  0
       N_COLOR =  2
       N_CONE =  0
       IDO_VX =  1
       IDO_VZ =  1
       IDO_X_S =  1
       IDO_Y_S =  1
       IDO_Z_S =  1
       IDO_XL =  0
       IDO_XN =  0
       IDO_ZL =  0
       IDO_ZN =  0
       SIGXL1 =  0.00000D0
       SIGXL2 =  0.00000D0
       SIGXL3 =  0.00000D0
       SIGXL4 =  0.00000D0
       SIGXL5 =  0.00000D0
       SIGXL6 =  0.00000D0
       SIGXL7 =  0.00000D0
       SIGXL8 =  0.00000D0
       SIGXL9 =  0.00000D0
       SIGXL10 =  0.00000D0
       SIGZL1 =  0.00000D0
       SIGZL2 =  0.00000D0
       SIGZL3 =  0.00000D0
       SIGZL4 =  0.00000D0
       SIGZL5 =  0.00000D0
       SIGZL6 =  0.00000D0
       SIGZL7 =  0.00000D0
       SIGZL8 =  0.00000D0
       SIGZL9 =  0.00000D0
       SIGZL10 =  0.00000D0
       CONV_FACT =  0.00000D0
       CONE_MAX =  0.00000D0
       CONE_MIN =  0.00000D0
       EPSI_DX =  0.00000D0
       EPSI_DZ =  0.00000D0
       EPSI_X =  0.00000D0
       EPSI_Z =  0.00000D0
       HDIV1 =  0.5D-6
       HDIV2 =  0.5D-6
       PH1 =  1000.000D0
       PH2 =  1010.00000D0
       PH3 =  0.00000D0
       PH4 =  0.00000D0
       PH5 =  0.00000D0
       PH6 =  0.00000D0
       PH7 =  0.00000D0
       PH8 =  0.00000D0
       PH9 =  0.00000D0
       PH10 =  0.00000D0
       RL1 =  0.00000D0
       RL2 =  0.00000D0
       RL3 =  0.00000D0
       RL4 =  0.00000D0
       RL5 =  0.00000D0
       RL6 =  0.00000D0
       RL7 =  0.00000D0
       RL8 =  0.00000D0
       RL9 =  0.00000D0
       RL10 =  0.00000D0
       BENER =  0.00000D0
       POL_ANGLE =  0.0000D0
       POL_DEG =  1.0000D0
       R_ALADDIN =  0.00000D0
       R_MAGNET =  0.00000D0
       SIGDIX =  0.001D0
       SIGDIZ =  0.0001D0
       SIGMAX =  0.001D0
       SIGMAY =  0.001D0
       SIGMAZ =  0.001D0
       VDIV1 =  5D-6
       VDIV2 =  5D-6
       WXSOU =  0.1D0
       WYSOU =  0.2D0
       WZSOU =  0.2D0
       PLASMA_ANGLE =  0.00000D0
       FILE_TRAJ = "NONE SPECIFIED" 
       FILE_SOURCE =  "NONE SPECIFIED"
       FILE_BOUND =  "NONE SPECIFIED"
       OE_NUMBER =  0
       IDUMMY =  0
       DUMMY =  0.00000D0
       F_NEW =  0

       goto 10101
     end if

    ! C

    ! C     	DATA	IDO_X_S,IDO_Y_S,IDO_Z_S,IDO_VX,IDO_VZ	/1,1,1,1,1/
    ! C     	DATA	ISTAR1		/ 12345701 /
    ! C
    ! C
    ! C
    	!CALL CLSCREEN
    ! C		       123456789 123456789 123456789 123456789 1
    !     	MSSG1 (1:40) = '--------------------------------- S H A '
    !        MSSG1 (41:80)= ' D O W  -------------------------------'
    MSSG1 = '--------------------------------- S H A D O W  -------------------------------'
    MSSG2 = 'version 3beta'
    WRITE (6,'(1X,A)')  trim(MSSG1)
    ! C
    ! C Get the data file path using either SHADOW$DATA or Unix SHADOW_DATA_DIR
    ! C environment variable. Also, check for existence in the routine itself.
    ! C
    ! TODO: include shadow banner and version
    ! 	IFLAG = 1
    ! 	CALL DATAPATH ('VERSION', VERSION, IFLAG)
    !   IF (IFLAG .NE. 0) THEN
    ! 	    CALL LEAVE ('INPUT_SOURCE', 'VERSION file not found', 1)
    !   END IF
    !     	OPEN (11, FILE=VERSION, STATUS='OLD')
    !     	READ (11,*) I1,I1,I1
    !     	READ (11,'(1X,A)') MSSG2
    !     	CLOSE(11)
    !     	WRITE (6,'(1X,A)') MSSG2
         	WRITE (6,2222) 
    2222	FORMAT (1X,/,1X,'Defining source : ')
         	WRITE(6,*)'When prompted for a yes/no answer, you may enter:'
         	! WRITE(6,*)'for   YES  answer      Y, 1'
         	WRITE(6,*)'for   YES  answer      Y*, y*, 1*'
         	WRITE(6,*)'for   NO   answer      anything else'
         	WRITE (6,'(1X,///)')
         	IVERB = IYES ('Do you want a verbose [ 1 ] or terse [ 0 ] output ?')
    	!CALL CLSCREEN
    	!WRITE(6,'(1X,A)') trim(MSSG1)
    	!WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    
    ! **---------------------------------------------------------------------
         	WRITE(6,*)'------------- SOURCE SPECS ------------------'
           	IF (IVERB.EQ.1) THEN
         	WRITE(6,*)'Options available:'
         	WRITE(6,*)'Random in  BOTH REAL and MOMENTUM space	      0'
         	WRITE(6,*)'Grid       BOTH				      1'
         	WRITE(6,*)'Grid       REAL, random  MOMENTUM	              2'
         	WRITE(6,*)'Random     REAL,  grid   MOMENTUM		      3'
    	WRITE(6,*)'Ellipses in PHASE space,random around each ellipse 4'
    	WRITE(6,*)'Ellipses in PHASE space,  grid around each ellipse 5'
         	END IF
    	FGRID = IRINT ('Source modelling type [ 0-5 ] ? ')
    		IF (FGRID.EQ.0) THEN		!3
         	!  NPOINT= IRINT ('How many rays [ 1 - 5 000 ] ? ')
         	NPOINT= IRINT ('How many rays [Default=30000] ? ')
		IF (NPOINT == 0) NPOINT=30000
         	ISTAR1= IRINT ('Seed [ odd, 1000 - 1 000 000 ] ? ')
         		ELSE IF (FGRID.EQ.2.OR.FGRID.EQ.3.OR.FGRID.EQ.4) THEN
         	NPOINT= IRINT ('How many rays for RANDOM part ? ')
         	ISTAR1= IRINT ('Seed [ odd, 1000 - 1 000 000 ] ? ')
    		END IF				!3
    ! * Go now on to define the source.
                print *,'Do you want to optimize the source ? '
                print *,'   i.e., reject rays, a variance reduction technique'
                print *,'   0: no optimization'
                print *,'   1: optimization using a phase volume (via histo3)'
                print *,'   2: reject rays if hit outside a rectangular slit'
                print *,'      or angular acceptance acceptance'
                F_BOUND_SOUR =  IRINT('<?> ')
                IF ( F_BOUND_SOUR.EQ.1 ) THEN 
                  FILE_BOUND = RSTRING &
                  ('Please file name with acceptance phase volume (from histo3): ')
                END IF
                IF ( F_BOUND_SOUR.EQ.2 ) THEN 
                  print *,'   slit/acceptance info will be read from an ASCII file with: '
                  print *,'   distance_to_slit x_from x_to z_from z_to'
                  print *,'   or, alternatively '
                  print *,'   0 xprime_min xprime_max zprime_min zprime_max'
                  print *,'   '
                  FILE_BOUND = RSTRING &
                  ('Please input name of file with slit/acceptance info: ')
                END IF
                IF ( F_BOUND_SOUR.GT.0 ) THEN 
                  NTOTALPOINT =  IRINT('Max number of rejected rays (set 0 for infinity)')
                END IF
         	  
    ! **---------------------------------------------------------------------
    	!CALL CLSCREEN
    	!WRITE(6,'(1X,A)') trim(MSSG1)
    	!WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    	WRITE(6,*)'Source type : [ 0 ] regular (geometrical or bending magnet) source'
    	WRITE(6,*)'              [ 1 ] normal wiggler'
    	WRITE(6,*)'              [ 2 ] undulator'
    	WRITE(6,*)'              [ 3 ] elliptical wiggler'
    	F_WIGGLER	= IRINT ('Then ? ')
    	IF (F_WIGGLER.NE.0) THEN
    	  IF ((F_WIGGLER.EQ.1) .OR. (F_WIGGLER.EQ.3)) THEN
    	    FILE_TRAJ	= RSTRING ('File containing the electron trajectory ? ')
    	    CONV_FACT	= RNUMBER ('Conversion factor from meters to user units ? ')
         	    HDIV1 = RNUMBER ('Horizontal half-divergence [ (+)x, rads ] ? ')
         	    HDIV2 = RNUMBER ('                           [ (-)x, rads ] ? ')
         	    VDIV1 = RNUMBER ('Vertical                   [ (+)z, rads ] ? ')
         	    VDIV2 = RNUMBER ('                           [ (-)z, rads ] ? ')
    	  ELSE IF (F_WIGGLER.EQ.2) THEN
    	    FILE_TRAJ	= RSTRING ('File containing the CDF''s ? ')	
    	  END IF
    	  IF (FGRID.NE.0) THEN
         	    NPOINT= IRINT ('How many rays [ 1 - 5 000 ] ? ')
         	    ISTAR1= IRINT ('Seed [ odd, 1000 - 1 000 000 ] ? ')
    	  END IF
         	  SIGMAX	= RNUMBER ('Sigma along X ? ')
         	  SIGMAZ	= RNUMBER ('            Z ? ')
    ! C23456789112345678921234567893123456789412345678951234567896123456789712
    	  WRITE(6,*)'Electron beam emittance.  Units are: rads*[ units of length used so far ]'
         	  EPSI_X = rnumber ('Beam emittances in X [ at waist ] ? ')
         	  EPSI_DX= rnumber ('Distance of insertion device''s center from X waist [ signed ] ? ')
         	  EPSI_Z = rnumber ('Beam emittances in Z [ at waist ] ? ')
         	  EPSI_DZ= rnumber ('Distance of insertion device''s center from Z waist [ signed ] ? ')
         	  IF (iverb.eq.1) then
         			WRITE (6,540,ADVANCE='NO')
    540	FORMAT (/,1X,'Polarization component of interest. Enter parallel polarization    1',/,       &
         1x,'perpendicular            2',/,                                                          &
         1x,'total                    3',/,                                                          &
         1x,'                   then ? ')
         	end if
         	  F_POL = irint ('Polarization Selected [ 1-3 ] ? ')
    	  GO TO 10100
    	END IF
    ! **---------------------------------------------------------------------
    	!CALL CLSCREEN
    	!WRITE(6,'(1X,A)') trim(MSSG1)
    	!WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    	IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    	  IF (IVERB.EQ.1) THEN
    	    WRITE(6,*) 'Ellipses in PHASE space are accomplished by assumming '
    	    WRITE(6,*) 'a double Gaussian distribution in phase space and then'
    	    WRITE(6,*) 'generating data points at various concentric sigma '
    	    WRITE(6,*)'levels (1-sigma, 3-sigma etc.)'
    	  END IF
    	  SIGMAX = RNUMBER ('Sigma for X ? ')
    	  SIGDIX = RNUMBER ('Sigma for X'' (hori. divergence) [ rads ] ? ')
    	  IDO_XL = IRINT ('How many sigma levels ? ')
    	  DO 11 I = 1, IDO_XL
    	    WRITE(6,*)'Sigma level of ellipse ',I
    	    IF (I.EQ.1) SIGXL1 = RNUMBER (' ? ')
    	    IF (I.EQ.2) SIGXL2 = RNUMBER (' ? ')
    	    IF (I.EQ.3) SIGXL3 = RNUMBER (' ? ')
    	    IF (I.EQ.4) SIGXL4 = RNUMBER (' ? ')
    	    IF (I.EQ.5) SIGXL5 = RNUMBER (' ? ')
    	    IF (I.EQ.6) SIGXL6 = RNUMBER (' ? ')
    	    IF (I.EQ.7) SIGXL7 = RNUMBER (' ? ')
    	    IF (I.EQ.8) SIGXL8 = RNUMBER (' ? ')
    	    IF (I.EQ.9) SIGXL9 = RNUMBER (' ? ')
    	    IF (I.EQ.10) SIGXL10 = RNUMBER (' ? ')
    11	  CONTINUE
    	  IF (FGRID.EQ.5) IDO_XN = IRINT ('No. of rays within each level ?')
    	  SIGMAZ = RNUMBER ('Sigma for Z ? ')
         	  SIGDIZ = RNUMBER ('Sigma for Z'' (vert. divergence) [ rads ] ? ')
    	  IDO_ZL = IRINT ('How many sigma levels ? ')
    	  DO 21 I = 1, IDO_ZL
    	    WRITE(6,*)'Sigma level of ellipse ',I
    	    IF (I.EQ.1) SIGZL1 = RNUMBER (' ? ')
    	    IF (I.EQ.2) SIGZL2 = RNUMBER (' ? ')
    	    IF (I.EQ.3) SIGZL3 = RNUMBER (' ? ')
    	    IF (I.EQ.4) SIGZL4 = RNUMBER (' ? ')
    	    IF (I.EQ.5) SIGZL5 = RNUMBER (' ? ')
    	    IF (I.EQ.6) SIGZL6 = RNUMBER (' ? ')
    	    IF (I.EQ.7) SIGZL7 = RNUMBER (' ? ')
    	    IF (I.EQ.8) SIGZL8 = RNUMBER (' ? ')
    	    IF (I.EQ.9) SIGZL9 = RNUMBER (' ? ')
    	    IF (I.EQ.10) SIGZL10 = RNUMBER (' ? ')
    21	  CONTINUE
    	  IF (FGRID.EQ.5) IDO_ZN = IRINT  ('No. of rays within each level ?')
    	  GO TO 470
    	END IF
         	IF (IVERB.EQ.1) THEN
!    	WRITE (6,440)
!    ! C23456789112345678921234567893123456789412345678951234567896123456789712
!    440	FORMAT (1X,' The source is specified in the laboratory ',/,1X, &
!                'reference frame. The program will then rotate the set of ',/,1X, &
!                'rays in the mirror frame.',//,1X, &
!                'Type of source,now.',//,10X, &
!                'use  ( 0 ) for point source  ',/,10X, &
!                '     ( 1 ) for rectangular  s.',/,10X, &
!                '     ( 2 ) for elliptical   s.',/,10X, &
!                '     ( 3 ) for gaussian     s.',/,10X, &
!                '     ( 6 ) for dense plasma s.',/,10X)

                 print *,' The source is specified in the laboratory reference frame. '
                 print *,' The program will then rotate the set of rays in the mirror frame.'
                 print *,'Type of source,now.'        
                 print *,'use  ( 0 ) for point source  '  
                 print *,'     ( 1 ) for rectangular  s.'
                 print *,'     ( 2 ) for elliptical   s.'
                 print *,'     ( 3 ) for gaussian     s.'
                 print *,'     ( 6 ) for dense plasma s.'
         	END IF
    	FSOUR = IRINT ('X-Z plane source type [ 0-3, 6 ] ? ')
    	IF (FSOUR.EQ.1) THEN 
         	WXSOU	= RNUMBER ('Source Width  [ x ] ? ' )
         	WZSOU	= RNUMBER ('       Height [ z ] ? ')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many points along the source X ? ')
         	IDO_Z_S = IRINT ('                                 Z ? ')
         		END IF
    	ELSE IF (FSOUR.EQ.2) THEN          
         	WXSOU	= RNUMBER ('Source Width  [ x ] ? ' )
         	WZSOU	= RNUMBER ('       Height [ z ] ? ')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many concentric ellipses within the source ? ')
         	IDO_Z_S = IRINT ('How many points on each ellipse ? ')
         		END IF
    
    	ELSE IF (FSOUR.EQ.3) THEN          
         	SIGMAX	= RNUMBER ('Sigma along X ? ')
         	SIGMAZ	= RNUMBER ('            Z ? ')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many points along the source X ? ')
         	IDO_Z_S = IRINT ('                                 Z ? ')
         		END IF
    
    	ELSE IF (FSOUR.EQ.6) THEN
    	PLASMA_ANGLE = RNUMBER ('Source cone full opening (radians)')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many points along the source X ? ')
         	IDO_Z_S = IRINT ('                                 Z ? ')
         		END IF
    	END IF				   
    470	CONTINUE
    ! * Inquires now about the source depth.
    ! **---------------------------------------------------------------------
    	!CALL CLSCREEN
    	!WRITE(6,'(1X,A)') trim(MSSG1)
    	!WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! C23456789112345678921234567893123456789412345678951234567896123456789712
    ! **---------------------------------------------------------------------
    ! C
    ! C       dense plasma assumes flat depth distrib here for now. See later.
    ! C
    	IF (FSOUR.EQ.6) THEN
    	  FSOURCE_DEPTH = 2
    	ELSE
         	  IF (IVERB.EQ.1) THEN
!    	    WRITE (6,460,ADVANCE='NO')
!    460 	    FORMAT (1X,'Source depth. The actual source will be centered on', &
!           'the no-depth position. Use ',/,10x, &
!           '	(1) for no depth,',/,10x, &
!           '	(2) for flat depth distribution,',/,10x, &
!           '	(3) for gaussian depth distribution,',/,10x, &
!           '    (4) for a synchrotron source depth distr.',//,20x, &
!           '	Then ? ')

            print *,'Source depth. The actual source will be centered on'
            print *,'the no-depth position. Use '
            print *,'   (1) for no depth,'
            print *,'   (2) for flat depth distribution,'
            print *,'   (3) for gaussian depth distribution,'
            print *,'   (4) for a synchrotron source depth distr.'
            print *,'	Then ? '
         	  END IF
    	  FSOURCE_DEPTH = IRINT ('Source Depth [ 1-4 ] ? ')
    	END IF
    ! * The S.R. case will be dealt with aftewards, when asking the details of
    ! * the source.
         	IF (FSOURCE_DEPTH.EQ.2) THEN
    	 WYSOU = RNUMBER ('Source Depth ? ')
         	ELSE IF (FSOURCE_DEPTH.EQ.3) THEN
    	 SIGMAY	=  RNUMBER ('Sigma along depth ? ')
    	ELSE IF (FSOURCE_DEPTH.EQ.4.AND.(FGRID.EQ.4.OR.FGRID.EQ.5)) THEN
         	  WRITE(6,*) 'Notice: the ORBIT radius MUST be in the same units as the rest of the optical system.'
         	  WRITE(6,*) 'Use negative ORBIT radius argument for CCW storage ring.'
    	  R_ALADDIN=rnumber('Orbit Radius [ same as other units ] ?')
         	  HDIV1=RNUMBER('Horizontal half-divergence [ (+)x, rads ] ? ')
         	  HDIV2=RNUMBER('                           [ (-)x, rads ] ? ')
         	END IF
    1111	CONTINUE
         	IF ((FGRID.EQ.1.OR.FGRID.EQ.2).AND.FSOURCE_DEPTH.NE.1) THEN  !6
         	  IDO_Y_S = IRINT ('How many points along the depth ? ')
         	END IF					!6
    	IF (FGRID.EQ.4.OR.FGRID.EQ.5) GO TO 770
    
    ! **---------------------------------------------------------------------
    	!CALL CLSCREEN
    	!WRITE(6,'(1X,A)') trim(MSSG1)
    	!WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    ! C
    ! C       dense plasma assumes conical source distribution for now.
    ! C
    	IF (FSOUR.EQ.6) THEN
         	  FDISTR = 5
    	ELSE
         	  IF (IVERB.EQ.1) THEN
!    	    WRITE (6,530)	
!    530 	    FORMAT (/,1X,'O.K., got it so far.',/,10X, &
!                 'Source distribution now. We may use ',/,10X, &
!                 '    ( 1 ) for a flat source',/,10X, &
!                 '    ( 2 )       uniform   s.',/,10X, &
!                 '    ( 3 )       gaussian  s.',/,10X, &
!                 '    ( 4 )       synchrotron ',/,10X, &
!                 '    ( 5 )       conical ',/,10X, &
!                 '    ( 6 )       exact synchrotron ',/)
                 
	              print *, ' '
                  print *, 'O.K., got it so far.'
                  print *, 'Source distribution now. We may use '
                  print *, '    ( 1 ) for a flat source'   
                  print *, '    ( 2 )       uniform   s.'  
                  print *, '    ( 3 )       gaussian  s.'  
                  print *, '    ( 4 )       synchrotron '  
                  print *, '    ( 5 )       conical '      
                  print *, '    ( 6 )       exact synchrotron '      
                 
    	  END IF
         	  FDISTR = IRINT ('Source Angle Distribution [ 1-6 ] ? ')
    	END IF
         	IF ((FGRID.EQ.1.OR.FGRID.EQ.3).AND.FDISTR.NE.5) THEN
         	  IDO_VZ = IRINT ('How many points in the vertical ? ')
         	  IDO_VX = IRINT ('          and in the horizontal ? ')
         	ELSE IF ((FGRID.EQ.1.OR.FGRID.EQ.3).AND.FDISTR.EQ.5) THEN
         	  N_CONE = irint ('How many points along cone radius ? ')
         	  N_CIRCLE = irint ('                and along circles ? ')
         	END IF
    		IF (FDISTR.NE.5) THEN      	!1
         	HDIV1 = RNUMBER ('Horizontal half-divergence [ (+)x, rads ] ? ')
         	HDIV2 = RNUMBER ('                           [ (-)x, rads ] ? ')
         	VDIV1 = RNUMBER ('Vertical                   [ (+)z, rads ] ? ')
         	VDIV2 = RNUMBER ('                           [ (-)z, rads ] ? ')
    		ELSE            		!1
         	cone_max = rnumber ('Max half-divergence ? ')
         	cone_min = rnumber ('Min half-divergence ? ')
    		END IF				!1
    		IF (FDISTR.EQ.3) THEN		!2
         	sigdiz = rnumber ('Vertical sigma [ rads ] ? ')
         	sigdix = rnumber ('Horizontal              ? ')
    		ELSE IF (FDISTR.EQ.4.OR.FDISTR.EQ.6) THEN	!2.1
         	r_magnet  = rnumber ('Magnetic Radius [ m ] ? ')
         	WRITE(6,*) 'Notice: the ORBIT radius MUST be in the same units as the rest of the optical system.'
         	WRITE(6,*) 'Use negative ORBIT radius argument for CCW storage ring.'
    	r_aladdin = rnumber ('Orbit Radius [ same as other units ] ?')
         			IF (FSOUR.EQ.3) THEN
         	iansw = iyes ('Do you want to include electron beam emittances [ Y/N ] ? ')
         				IF (IANSW.EQ.1) THEN
         	  WRITE(6,*) 'Units are : rads*[ units of length used so far ]'
         	EPSI_X = rnumber ('Beam emittances in X [ at waist ] ? ')
         	EPSI_DX= rnumber ('Distance from waist [ signed ] ? ')
         	EPSI_Z = rnumber ('Beam emittances in Z [ at waist ] ? ')
         	EPSI_DZ= rnumber ('Distance from waist [ signed ] ? ')
         			     	END IF
         		     	END IF
         	BENER = RNUMBER ('Electron Beam Energy [ GeV ] ? ')
         	if (iverb.eq.1) then
         			WRITE (6,640,ADVANCE='NO')
    640	FORMAT (/,1X,'Polarization component of interest. Enter ',/, &
         1x,'parallel polarization    1',/, &
         1x,'perpendicular            2',/, &
         1x,'total                    3',/, &
         1x,'                   then ? ')
         	end if
         	f_pol = irint ('Polarization Selected [ 1-3 ] ? ')
    	  IF (FDISTR.EQ.4) THEN
    	    IF (IVERB.EQ.1) THEN
    		WRITE (6,*) 'The source can be generated according to either [0] photons or [1] power distribution.'
    	    END IF
    	    F_SR_TYPE	= IRINT ('Distribution type [0,1] ? ')
    	  END IF
    		END IF				!2
    ! C
    770	CONTINUE
         	IF (FDISTR.NE.4.AND.FDISTR.NE.6) THEN
         	  IANSW = IYES ('Do you want a Photon energy [ Y/N ] ? ')
         	ELSE
         	  IANSW = 1
         	END IF
         	IF (IANSW.NE.1) GO TO 10100
         	IF (FDISTR.NE.6) THEN
         	 IF (IVERB.EQ.1) THEN
         	   WRITE(6,*)'We have these choices :'
         	   WRITE(6,*)'Single line ............................ 1'
         	   WRITE(6,*)'Several lines .......................... 2'
         	   WRITE(6,*)'Uniform source.......................... 3'
    	   WRITE(6,*)'Several lines, different intensities.... 4'
         	 END IF
         	  F_COLOR = IRINT ('Energy distribution [ 1-4 ] ? ')
         	ELSE IF (FDISTR.EQ.6) THEN
         	  F_COLOR = 1
         	END IF
         	F_PHOT = IRINT ('Photon Energy [ 0 ] or Angstroms [ 1 ] ? ' )
         	IF (F_COLOR.EQ.1) THEN
         	 IF (F_PHOT.EQ.0) PH1 = RNUMBER ('Energy [ eV ] ? ')
         	 IF (F_PHOT.EQ.1) PH1 = RNUMBER ('Wavelength [ A ] ? ')
         	ELSE IF (F_COLOR.EQ.2) THEN
         	 n_color = irint('How many lines ? ')
         		  DO 31 I_COL=1,N_COLOR
            WRITE(6,*)'Photon energy or wavelength for line ',I_COL
         		IF (I_COL.EQ.1)  PH1 = RNUMBER (' ? ')
         		IF (I_COL.EQ.2)  PH2 = RNUMBER (' ? ')
         		IF (I_COL.EQ.3)  PH3 = RNUMBER (' ? ')
         		IF (I_COL.EQ.4)  PH4 = RNUMBER (' ? ')
         		IF (I_COL.EQ.5)  PH5 = RNUMBER (' ? ')
         		IF (I_COL.EQ.6)  PH6 = RNUMBER (' ? ')
         		IF (I_COL.EQ.7)  PH7 = RNUMBER (' ? ')
         		IF (I_COL.EQ.8)  PH8 = RNUMBER (' ? ')
         		IF (I_COL.EQ.9)  PH9 = RNUMBER (' ? ')
         		IF (I_COL.EQ.10)  PH10 = RNUMBER (' ? ')
    31     		  CONTINUE
         	ELSE IF (F_COLOR.EQ.3) THEN
         	 WRITE(6,*)'From photon energy or wavelength ... '
         	 PH1 = rnumber (' ? ')
         	 WRITE(6,*)'... to photon energy or wavelength :'
         	 PH2 = rnumber (' ? ')
         	ELSE IF (F_COLOR.EQ.4) THEN
         	 n_color = irint('How many lines ? ')
         		  DO 41 I_COL=1,N_COLOR
            WRITE(6,*)'Photon energy or wavelength for line ',I_COL
         		IF (I_COL.EQ.1)  PH1 = RNUMBER (' ? ')
         		IF (I_COL.EQ.2)  PH2 = RNUMBER (' ? ')
         		IF (I_COL.EQ.3)  PH3 = RNUMBER (' ? ')
         		IF (I_COL.EQ.4)  PH4 = RNUMBER (' ? ')
         		IF (I_COL.EQ.5)  PH5 = RNUMBER (' ? ')
         		IF (I_COL.EQ.6)  PH6 = RNUMBER (' ? ')
         		IF (I_COL.EQ.7)  PH7 = RNUMBER (' ? ')
         		IF (I_COL.EQ.8)  PH8 = RNUMBER (' ? ')
         		IF (I_COL.EQ.9)  PH9 = RNUMBER (' ? ')
         		IF (I_COL.EQ.10)  PH10 = RNUMBER (' ? ')
            WRITE(6,*)'Relative intensity for line ',I_COL
         		IF (I_COL.EQ.1)  RL1 = RNUMBER (' ? ')
         		IF (I_COL.EQ.2)  RL2 = RNUMBER (' ? ')
         		IF (I_COL.EQ.3)  RL3 = RNUMBER (' ? ')
         		IF (I_COL.EQ.4)  RL4 = RNUMBER (' ? ')
         		IF (I_COL.EQ.5)  RL5 = RNUMBER (' ? ')
         		IF (I_COL.EQ.6)  RL6 = RNUMBER (' ? ')
         		IF (I_COL.EQ.7)  RL7 = RNUMBER (' ? ')
         		IF (I_COL.EQ.8)  RL8 = RNUMBER (' ? ')
         		IF (I_COL.EQ.9)  RL9 = RNUMBER (' ? ')
         		IF (I_COL.EQ.10)  RL10 = RNUMBER (' ? ')
    	WRITE(6,*)'RL1 = ',RL1
    	WRITE(6,*)'RL5 = ',RL5
    41     		  CONTINUE
         	END IF
    10100	CONTINUE
    ! **---------------------------------------------------------------------
    	!CALL CLSCREEN
    	!WRITE(6,'(1X,A)') trim(MSSG1)
    	!WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    	  F_OPD		= IYES ('Do you want to store the optical paths (OPD) [Y/N] ? ')
    ! ** Inquire about the source polarization.
    	  F_POLAR	= IYES ('Do you want to generate the A vectors (electric field) [Y/N] ?')
    	IF (F_POLAR.EQ.0)	GO TO 10101
    ! C
    ! C For SR, all the polarization varibles will be defined by SOURCE internally.
    ! C
    	IF (FDISTR.EQ.4.OR.FDISTR.EQ.6.OR.F_WIGGLER.NE.0) GO TO 10101
      	IF (IVERB.EQ.1) THEN
         	  WRITE(6,*)'Source polarization is specified by degree of ', &
         'polarization (= AX/(AX+AZ)) and phase angle of AZ from ', &
         'AX, for instance ,'
         	  WRITE(6,*)'Circular polarized :'
         	  WRITE(6,*)'    phase diff.    = +90 (CW) or -90 (CCW) degree'
         	  WRITE(6,*)'    deg. of polar. = 0.5'
         	  WRITE(6,*)'Linear polarized   :'
    	  WRITE(6,*)'    phase diff.    = 0'
    	  WRITE(6,*)'    deg. of polar. = cos(phi)/(cos(phi)+sin(phi))'
    	  WRITE(6,*)'    where      phi = angle of polarization plane from X-axis.'
         	END IF
    	POL_ANGLE	= RNUMBER ('Phase difference ?')
    	POL_DEG		= RNUMBER ('Degree of polarization ?')
    	IF (IVERB.EQ.1) THEN
    	  WRITE(6,*) 'If the absolute phase of AX does not change from one ray',  &
         ' to another, the source is coherent.  If it is randomly ', &
         'distributed, the source is incoherent.'
    	END IF
    	F_COHER		= IRINT ('Incoherent [0] or coherent [1] ?')
    ! **---------------------------------------------------------------------
    ! *				Store the input data
    ! **---------------------------------------------------------------------
    ! ** The format is the same as that used by SILENT.	
    10101	CONTINUE
!        	CALL	FNAME	(FFILE, 'start', 0, 2)
!         	IDUMM = 0
!         	!! CALL	RWNAME	(FFILE, 'W_SOUR', IDUMM)
!print *,">> PLEASE write: ",trim(FFILE)
!         	!! IF (IDUMM.NE.0) CALL LEAVE ('INPUT_SOURCE','Error writing NAMELIST',IDUMM)
    ! 111	FORMAT (A)

    ! this is done outside (input_source)
    ! call rwname('start.00','W_SOUR',iErr)
    !   if (iErr /= 0 ) then
    !      call leave("INPUT_SOURCE1","Error writinge file: start.00",-55_4)
    !   end if

       WRITE(6,*)'Exit from INPUT_SOURCE'

End Subroutine input_source1


  !
  !
  !

  ! C+++
  ! C	SUBROUTINE	sourceGeom
  ! C
  ! C	PURPOSE		To generate a array describing a geometric source.
  ! C
  ! C	INPUTS		
  ! C       RAY         the allocated array to fill in the source rays
  ! C	    NPOINT1	number of rays in ray(ncol,npoint1)
  ! C
  ! C	OUTPUTS		The ray array filled. 
  ! C
  ! C---
  
!  SUBROUTINE 	SOURCE1B (infile, FNAME, IOFORM)
SUBROUTINE sourceGeom (pool00,ray,npoint1) !bind(C,NAME="sourceGeom")
    
    
    implicit    real(kind=skr)          (a-e,g-h,o-z)
    implicit    integer(kind=ski)       (f,i-n)
    
    integer(kind=ski), intent(in)    :: npoint1
    type (poolSource), intent(in out) :: pool00
    real(kind=skr), dimension(18,npoint1), intent(in out) :: ray

    integer(kind=ski) :: ioform
    integer(kind=ski) :: C_X,C_Y,C_Z,C_VX,C_VZ,C_XN,C_ZN
    character(len=sklen)       :: infile, FNAME
    character(len=sklen)       :: errmsg
    
    real(kind=skr), dimension(3)  :: XDUM, YDUM
    real(kind=skr), dimension(3)  ::  DIREC,AP_VEC,E_TEMP,SB_POS,VTEMP,A_VEC,A_TEMP,E_BEAM
    real(kind=skr), dimension(6,npoint1)  :: grid
    real(kind=skr), dimension(10) :: SIGXL,SIGZL
    
    real(kind=skr)  ::   YRAN,DPS_RAN1,DPS_RAN2
    real(kind=skr)  ::   TMP_A,TMP_B,DPS_RAN3
    
    real(kind=skr),dimension(2)  :: JI,DZ,THE_INT
    real(kind=skr),dimension(4)  :: II,DX,PHI_INT
    real(kind=skr),dimension(10) :: RELINT,PRELINT
    
    integer(kind=ski) :: n_rej=0, k_rej=0
    
    real(kind=skr)    :: xxx=0,yyy=0,zzz=0

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


    !
    ! put inputs (pool) into global variables
    !
    !TODO: work without globals!!!!
    CALL PoolSourceToGlobal(pool00)

    ISTAT = 0
    IDUMM = 0
    !! PROVISIONAL STUFF...
    IF ((FDISTR.EQ.4).OR.(FSOURCE_DEPTH.EQ.4).OR.(F_WIGGLER.GT.0)) THEN
       ITMP=1
       CALL LEAVE ('SOURCE1','Only geometrical source. No synchrotron source allowed here',ITMP)
    END IF
    
    
    KREJ = 0
    NREJ = 0
    ! C
    ! C Sets up some variables needed for the rest of the routine
    ! C
    ! C First figure out the number of columns written out for each ray.
    ! C
    IF (F_POLAR.EQ.1) THEN
       NCOL = 18
    ELSE IF (F_OPD.EQ.1) THEN
       NCOL = 13
    ELSE
       NCOL = 12
    END IF
    
    RAD_MIN = ABS(R_MAGNET)
    RAD_MAX = ABS(R_MAGNET)
    
    IF (FSOUR.EQ.3) THEN
       EPSI_XOLD = EPSI_X
       EPSI_ZOLD = EPSI_Z
       IF (SIGMAX.NE.0.0D0) THEN
          EPSI_X =   EPSI_X/SIGMAX
       ELSE
          EPSI_X =   0.0D0
       END IF
       IF (SIGMAZ.NE.0.0D0) THEN
          EPSI_Z =   EPSI_Z/SIGMAZ
       ELSE
          EPSI_Z =   0.0D0
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
    IF ( F_BOUND_SOUR.GT.0 .AND. FGRID.EQ.0 ) THEN
       ITMP=-1
       CALL  SOURCE_BOUND (XDUM,YDUM,ITMP)
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
       FSOUR = 4
       FDISTR = 7
    END IF
    
    IF (F_PHOT.EQ.1) THEN
       IF (F_COLOR.EQ.1) THEN
          PHOTON(1) =   TOANGS/PHOTON(1)
       ELSE IF (F_COLOR.EQ.2.OR.F_COLOR.EQ.4) THEN
          DO  21 I=1,N_COLOR
             PHOTON(I) =   TOANGS/PHOTON(I)
21        CONTINUE
       ELSE IF (F_COLOR.EQ.3) THEN
          DO 31 I=1,2
             PHOTON(I) =   TOANGS/PHOTON(I)
31        CONTINUE
       END IF
    END IF
102 CONTINUE
    
    IF (FDISTR.NE.5) THEN
       NMOM = IDO_VX * IDO_VZ
    ELSE
       NMOM = (N_CONE * N_CIRCLE) 
       IDO_VX = N_CIRCLE
       IDO_VZ = N_CONE
    END IF
    NSPACE = IDO_X_S * IDO_Y_S * IDO_Z_S
    
    IF (FGRID.EQ.0) THEN
       NTOTAL = NPOINT
    ELSE IF (FGRID.EQ.1) THEN
       NTOTAL = NSPACE * NMOM
    ELSE IF (FGRID.EQ.2) THEN
       NTOTAL = NSPACE * NPOINT
    ELSE IF (FGRID.EQ.3) THEN
       NTOTAL = NPOINT * NMOM
    ELSE IF (FGRID.EQ.4) THEN
       NTOTAL = IDO_XL * NPOINT * IDO_ZL * NPOINT
    ELSE IF (FGRID.EQ.5) THEN
       NTOTAL = IDO_XL * IDO_XN * IDO_ZL * IDO_ZN
    END IF
    
    ITMP=0
    IF (NTOTAL.LE.0) CALL LEAVE ('SOURCE','NPOINT = 0',ITMP)
    !!      IF (NTOTAL.GT.N_DIM) CALL LEAVE ('SOURCE','Too many rays.',ITMP)
    ! C
    ! C Compute the steps and iteration count limits for the grid generation.
    ! C
    IF (IDO_X_S.GT.1) STEP_X = 1.0D0/(IDO_X_S - 1)
    IF (IDO_Y_S.GT.1) STEP_Y = 1.0D0/(IDO_Y_S - 1)
    IF (IDO_Z_S.GT.1) STEP_Z = 1.0D0/(IDO_Z_S - 1)
    IF (IDO_VX.GT.1) STEP_VX = 1.0D0/(IDO_VX - 1)
    IF (IDO_VZ.GT.1) STEP_VZ = 1.0D0/(IDO_VZ - 1)
    IF (IDO_XN.GT.1) STEP_XN = 1.0D0/(IDO_XN - 1)
    IF (IDO_ZN.GT.1) STEP_ZN = 1.0D0/(IDO_ZN - 1)
    CL_X = (IDO_X_S - 1) / 2.0D0
    CL_Y = (IDO_Y_S - 1) / 2.0D0
    CL_Z = (IDO_Z_S - 1) / 2.0D0
    CL_VX = (IDO_VX - 1) / 2.0D0
    CL_VZ = (IDO_VZ - 1) / 2.0D0
    CL_XN = (IDO_XN - 1) / 2.0D0
    CL_ZN = (IDO_ZN - 1) / 2.0D0
    ! C
    ! C First fill out a "typical" part of the GRID direction.
    ! C
    INDEXMOM = 0 
    IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
       DO 41 I = 1, NPOINT
       GRID (4,I) = WRAN (ISTAR1)
       GRID (6,I) = WRAN (ISTAR1)
41     CONTINUE
       INDEXMOM = NPOINT
    ELSE IF (FGRID.EQ.1.OR.FGRID.EQ.3) THEN
    !!srio  DO 51 C_VX = -CL_VX, CL_VX
       DO 51 C_VX = -INT(CL_VX), INT(CL_VX)
    !!srio  DO 61 C_VZ = -CL_VZ, CL_VZ
          DO 61 C_VZ = -INT(CL_VZ), INT(CL_VZ)
!!print *,'C_VX C_VZ: ',C_VX ,C_VZ
             INDEXMOM = INDEXMOM + 1
             GRID (4,INDEXMOM) = C_VX * STEP_VX + 0.5D0
             GRID (6,INDEXMOM) = C_VZ * STEP_VZ + 0.5D0
61        CONTINUE
51     CONTINUE
       ELSE IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
          DO 71 I = 1, IDO_XL
             IF (FGRID.EQ.4) THEN
                DO 81 J = 1, NPOINT
                   INDEXMOM = INDEXMOM + 1
                   GRID(1,INDEXMOM) = SIGXL(I)
                   GRID(2,INDEXMOM) = WRAN (ISTAR1)
                   GRID(4,INDEXMOM) = WRAN (ISTAR1)
81              CONTINUE
             ELSE
          !!srio DO 91 C_XN = -CL_XN, CL_XN
                DO 91 C_XN = -INT(CL_XN), INT(CL_XN)
                   INDEXMOM = INDEXMOM + 1
                   GRID(1,INDEXMOM) = SIGXL(I)
                   GRID(2,INDEXMOM) = WRAN (ISTAR1)
                   GRID(4,INDEXMOM) = C_XN * STEP_XN + 0.5D0
91              CONTINUE
             END IF
71        CONTINUE
       END IF
    ! C
    ! C Now fill out the entire GRID.
    ! C
       INDEXSPA = 0
       IF (FGRID.EQ.0) THEN
          DO 103 I = 1, NPOINT
          GRID (1,I) = WRAN (ISTAR1)
          GRID (2,I) = WRAN (ISTAR1)
          GRID (3,I) = WRAN (ISTAR1)
103 CONTINUE
          INDEXSPA = NPOINT
       ELSE IF (FGRID.EQ.3) THEN
          DO 113 I = 1, NPOINT
             TEMPX = WRAN (ISTAR1)
             TEMPY = WRAN (ISTAR1)
             TEMPZ = WRAN (ISTAR1)
             DO 121 J = 1, INDEXMOM
                INDEXSPA = INDEXSPA + 1
                GRID(1,INDEXSPA) = TEMPX
                GRID(2,INDEXSPA) = TEMPY
                GRID(3,INDEXSPA) = TEMPZ
                GRID(4,INDEXSPA) = GRID (4,J)
                GRID(6,INDEXSPA) = GRID (6,J)
121          CONTINUE
113       CONTINUE
       ELSE IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
          !!srio DO 131 C_X = -CL_X, CL_X
          !!srio DO 141 C_Y = -CL_Y, CL_Y
          !!srio DO 151 C_Z = -CL_Z, CL_Z
          DO 131 C_X = -INT(CL_X), INT(CL_X)
             DO 141 C_Y = -INT(CL_Y), INT(CL_Y)
                DO 151 C_Z = -INT(CL_Z), INT(CL_Z)
                   DO 161 J = 1, INDEXMOM
                      INDEXSPA = INDEXSPA + 1
                      GRID (1,INDEXSPA) = C_X * STEP_X + 0.5D0
                      GRID (2,INDEXSPA) = C_Y * STEP_Y + 0.5D0
                      GRID (3,INDEXSPA) = C_Z * STEP_Z + 0.5D0
                      GRID (4,INDEXSPA) = GRID (4,J)
                      GRID (6,INDEXSPA) = GRID (6,J)
161                CONTINUE
151             CONTINUE
141          CONTINUE
131       CONTINUE
       ELSE IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
          DO 171 I = 1, IDO_ZL
             IF (FGRID.EQ.4) THEN
                DO 181 J = 1, NPOINT
                   TEMP = WRAN (ISTAR1)
                   DO 191 K = 1, IDO_XL*NPOINT
                      INDEXSPA = INDEXSPA + 1
                      GRID(1,INDEXSPA) = GRID(1,K)
                      GRID(2,INDEXSPA) = GRID(2,K)
                      GRID(4,INDEXSPA) = GRID(4,K)
                      GRID(3,INDEXSPA) = SIGZL(I)
                      GRID(6,INDEXSPA) = TEMP
191                CONTINUE
181             CONTINUE
             ELSE
    !!srio DO 201 C_ZN = -CL_ZN, CL_ZN
                DO 201 C_ZN = -INT(CL_ZN), INT(CL_ZN)
                   TEMP = C_ZN * STEP_ZN + 0.5D0
                   DO 211 K = 1, IDO_XL*IDO_XN
                      INDEXSPA = INDEXSPA + 1
                      GRID(1,INDEXSPA) = GRID(1,K)
                      GRID(2,INDEXSPA) = GRID(2,K)
                      GRID(4,INDEXSPA) = GRID(4,K)
                      GRID(3,INDEXSPA) = SIGZL(I)
                      GRID(6,INDEXSPA) = TEMP
211                CONTINUE
201             CONTINUE
             END IF
171       CONTINUE
       END IF
       ! C
       ! C---------------------------------------------------------------------
       ! C           POSITIONS
       ! C
       ! C
       KK =   0
       MM =   0
       DO 10000 ITIK=1,NTOTAL
          KK =  KK + 1
          !IF (KK.EQ.250) THEN
          IF (KK.EQ.NTOTAL/20) THEN
             !ITOTRAY = KK + MM*250
             ITOTRAY = KK + MM*(NTOTAL/20)
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
10001     CONTINUE
    
          GO TO (1,2,3,4,5,5,7), FSOUR+1
    
1         CONTINUE
          ! C
          ! C Point source **
          ! C
          xxx = 0.0d0
          zzz = 0.0d0
          GO TO 111
    
2         CONTINUE
          ! C
          ! C Rectangular source 
          ! C
          XXX  = (-1.0D0 + 2.0D0*GRID(1,ITIK))*WXSOU/2
          ZZZ  = (-1.0D0 + 2.0D0*GRID(3,ITIK))*WZSOU/2
          GO TO 111
    
3         CONTINUE
          ! C
          ! C Elliptical source **
          ! C Uses a transformation algorithm to generate a uniform variate distribution
          ! C
          IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
             PHI = TWOPI*GRID(1,ITIK)*(IDO_X_S-1)/IDO_X_S
          ELSE
             PHI  = TWOPI*GRID(1,ITIK)
          END IF
          RADIUS  = SQRT(GRID(3,ITIK))
          XXX  = WXSOU*RADIUS*COS(PHI)
          ZZZ  = WZSOU*RADIUS*SIN(PHI)
          GO TO 111
          
4         CONTINUE
          ! C
          ! C Gaussian -- In order to accomodate the generation nof finite emittance
          ! C beams, we had to remove the 'grid' case.
          ! C 
          ARG_X  = GRID(1,ITIK)
          ARG_Z  = GRID(3,ITIK)
          ! C
          ! C Compute the actual distance (EPSI_W*) from the orbital focus
          ! C
          EPSI_WX = EPSI_DX + EPSI_PATH
          EPSI_WZ = EPSI_DZ + EPSI_PATH
          CALL GAUSS (SIGMAX, EPSI_X, EPSI_WX, XXX, E_BEAM(1), istar1)
          CALL GAUSS (SIGMAZ, EPSI_Z, EPSI_WZ, ZZZ, E_BEAM(3), istar1)
          GO TO 111
          
5         CONTINUE
          ! C
          ! C Ellipses in phase space (spatial components).
          ! C
          IF (FGRID.EQ.4) THEN
             PHI_X = TWOPI * GRID(4,ITIK)
             PHI_Z = TWOPI * GRID(6,ITIK)
          ELSE
             PHI_X = TWOPI * GRID(4,ITIK) * (IDO_XN-1) / IDO_XN
             PHI_Z = TWOPI * GRID(6,ITIK) * (IDO_ZN-1) / IDO_ZN
          END IF
          XXX = GRID(1,ITIK)*SIGMAX*COS(PHI_X)
          ZZZ = GRID(3,ITIK)*SIGMAZ*COS(PHI_Z)
          GO TO 111
          
7         CONTINUE
          
          GOTO 550
          
111       CONTINUE
          ! C
          ! C---------------------------------------------------------------------
          ! C                      DEPTH
          ! C
          ! C
          GO TO (110,220,330,440)  FSOURCE_DEPTH
          ! C
          ! C No depth case.
          ! C
          YYY = 0.0d0
110       GO TO 550
          ! C
          ! C Uniform depth distribution
          ! C
220       YYY  = (-1.0D0 + 2.0D0*GRID(2,ITIK))*WYSOU/2
          GO TO 550
          ! C
          ! C Gaussian depth distribution 
          ! C
330       ARG_Y  = GRID(2,ITIK)
          
          CALL MDNRIS (ARG_Y,YYY,IER)
          IF (IER.NE.0) WRITE(6,*)'Warning ! Error in YYY,MNDRIS (SOURCE)'
       
       YYY = YYY*SIGMAY
       
       GO TO 550
440    CONTINUE
550    CONTINUE
       ! C
       ! C---------------------------------------------------------------------
       ! C             DIRECTIONS
       ! C
       ! C   Generates now the direction of the rays.
       ! C
       ! C
101    CONTINUE
       I_CHANGE = 1
       GO TO (11,11,33,44,55,44,77), FDISTR
       
11     CONTINUE
       ! C
       ! C   Uniform distribution ( Isotrope emitter ) and cosine  source
       ! C
       ! C   Distinction not ready yet. Not important for small apertures 
       ! C
       XMAX1  =   TAN(HDIV1)
       XMAX2 = - TAN(HDIV2)
       ZMAX1 =   TAN(VDIV1)
       ZMAX2 = - TAN(VDIV2)
       XRAND  = (GRID(4,ITIK)*(XMAX1 - XMAX2) + XMAX2)
       ZRAND  = (GRID(6,ITIK)*(ZMAX1 - ZMAX2) + ZMAX2)
       THETAR  = ATAN(SQRT(XRAND**2+ZRAND**2))
       CALL  ATAN_2 (ZRAND,XRAND,PHIR)
       DIREC(1)  = COS(PHIR)*SIN(THETAR)
       DIREC(2)  = COS(THETAR)
       DIREC(3)  = SIN(PHIR)*SIN(THETAR)
       ! C      ARG =   GRID(6,ITIK)*(SIN(VDIV1) + SIN(VDIV2)) - SIN(VDIV2)
       ! C      PHIR =   GRID(4,ITIK)*(HDIV1 + HDIV2) - HDIV1
       ! C      THETAR  =   ASIN(ARG)
       ! C      DIREC(1) =   SIN(PHIR)*COS(THETAR)
       ! C      DIREC(2) =   COS(PHIR)*COS(THETAR)
       ! C      DIREC(3) =   SIN(THETAR)
       GO TO 1111
       
33     CONTINUE
       ! C
       ! C Gaussian emitter 
       ! C Note : an emitter cannot have an angular gaussian distribution, as a
       ! C gaussian is defined from -infin. to + infin. It might be an useful
       ! C approximation in several cases. This program uses a gaussian 
       ! C distribution onto an ideal image plane, independent in x and z. This 
       ! C approximation will not break down for large sigma.
       ! C
       ARG_VX  = GRID(4,ITIK)
       ARG_VZ  = GRID(6,ITIK)
       
       CALL MDNRIS (ARG_VX,DIR_X,IER)
       IF (IER.NE.0) WRITE(6,*) 'Warning !Error in DIR_X:MNDRIS(SOURCE)'
       
       DIREC(1)  = DIR_X*SIGDIX
       
       CALL MDNRIS (ARG_VZ,DIR_Z,IER)
       IF (IER.NE.0) WRITE(6,*)'Warning !Error in DIR_Z:MNDRIS(SOURCE)'
       
       DIREC(3)  = DIR_Z*SIGDIZ
       DIREC(2)  = 1.0D0
       
       CALL NORM (DIREC,DIREC)
       
       GO TO 1111
       
44     CONTINUE
       GO TO 1111
55     CONTINUE
       ! C   Now generates a set of rays along a cone centered about the normal,
       ! C   plus a ray along the normal itself.
       ! C      
       IF (FGRID.EQ.1.OR.FGRID.EQ.3) THEN
          ANGLE =   TWOPI*GRID(4,ITIK)*(IDO_VX-1)/IDO_VX
    ELSE
       ANGLE =   TWOPI*GRID(4,ITIK)
    END IF
    ! C temp fix -- 16 Jan 1987
    ! C      ANG_CONE =   CONE_MIN + 
    ! C     $ (CONE_MAX - CONE_MIN)*GRID(6,ITIK)
    ANG_CONE =   COS(CONE_MIN) - GRID(6,ITIK)*(COS(CONE_MIN)-COS(CONE_MAX))
    ANG_CONE =  ACOS(ANG_CONE)
    DIREC(1) =   SIN(ANG_CONE)*COS(ANGLE)
    DIREC(2) =   COS(ANG_CONE)
    DIREC(3) =   SIN(ANG_CONE)*SIN(ANGLE)
    ! C
    GO TO 1111
    
77  CONTINUE
    ! C
    ! C Ellipses in phase space (momentum components).
    ! C
    ANGLEX = GRID(1,ITIK)*SIGDIX*SIN(PHI_X)
    ANGLEV = GRID(3,ITIK)*SIGDIZ*SIN(PHI_Z)
    DIREC(1) = SIN(ANGLEX)
    DIREC(3) = SIN(ANGLEV)
    DIREC(2) = SQRT(1.0D0 - DIREC(1)**2 - DIREC(3)**2)
    GO TO 1111
    
1111 CONTINUE
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
    A_VEC(1) =   1.0D0
    A_VEC(2) =   0.0D0
    A_VEC(3) =   0.0D0
    ! C
    ! C   Rotate A_VEC so that it will be perpendicular to DIREC and with the
    ! C   right components on the plane.
    ! C 
    CALL CROSS (A_VEC,DIREC,A_TEMP)
    CALL CROSS (DIREC,A_TEMP,A_VEC)
    CALL NORM (A_VEC,A_VEC)
    CALL CROSS (A_VEC,DIREC,AP_VEC)
    CALL NORM (AP_VEC,AP_VEC)
    
    IF (F_POLAR.EQ.1) THEN
       ! C
       ! C   WaNT A**2 = AX**2 + AZ**2 = 1 , instead of A_VEC**2 = 1 .
       ! C
       DENOM = SQRT(1.0D0 - 2.0D0*POL_DEG + 2.0D0*POL_DEG**2)
       AX = POL_DEG/DENOM
       CALL SCALAR (A_VEC,AX,A_VEC)
       ! C
       ! C   Same procedure for AP_VEC
       ! C
       AZ = (1-POL_DEG)/DENOM
       CALL SCALAR  (AP_VEC,AZ,AP_VEC)
    ELSE
    END IF
    ! C
    ! C Now the phases of A_VEC and AP_VEC.
    ! C
    IF (F_COHER.EQ.1) THEN
       PHASEX = 0.0D0
    ELSE
       PHASEX = WRAN(ISTAR1) * TWOPI
    END IF
    PHASEZ = PHASEX + POL_ANGLE*I_CHANGE
    GO TO (2020,2030,2040,2045) F_COLOR
    
2010 CONTINUE
    ! C
    ! C Not interested in the photon energy. Set at 0.0
    ! C
    GO TO 2050
    
2020 CONTINUE
    ! C
    ! CSingle line. 
    ! C
    Q_WAVE =   TWOPI*PHOTON(1)/TOCM
    GO TO 2050
    
2030 CONTINUE
    ! C
    ! C Several photon energies (up to 10) with same relative intensities.
    ! C
    N_TEST =   WRAN (ISTAR1)*N_COLOR + 1
    Q_WAVE =   TWOPI*PHOTON(N_TEST)/TOCM
    GO TO 2050
    
2040 CONTINUE
    ! C
    ! C Box photon distribution
    ! C
    PHOT_CH =   PHOTON(1) + (PHOTON(2) - PHOTON(1))*WRAN(ISTAR1)
    Q_WAVE =   TWOPI*PHOT_CH/TOCM
    GO TO 2050
    
2045 CONTINUE
    ! C
    ! C Several photon energies (up to 10) with different relative intensities.
    ! C
    RELINT(1) = RL1
    RELINT(2) = RL2
    RELINT(3) = RL3
    RELINT(4) = RL4
    RELINT(5) = RL5
    RELINT(6) = RL6
    RELINT(7) = RL7
    RELINT(8) = RL8
    RELINT(9) = RL9
    RELINT(10) = RL10
    
    ! C
    ! C Normalize so that each energy has a probability and so that the sum 
    ! C of the probabilities of all the energies is 1.
    ! C
    
    TMP_A = 0
    DO 2046 J=1,N_COLOR
       TMP_A = TMP_A + RELINT(J)  
2046 CONTINUE 
    DO 2047 J=1,N_COLOR
       RELINT(J)=RELINT(J)/TMP_A
2047 CONTINUE
          
       ! C
       ! C Arrange the probabilities so that they comprise the (0,1) interval,
       ! C e.g. (energy1,0.3), (energy2, 0.1), (energy3, 0.6) is translated to
       ! C 0.0, 0.3, 0.4, 1.0. Then a random number falling in an interval
       ! C assigned to a certain energy results in the ray being assigned that
       ! C photon energy.
       ! C
    TMP_B = 0
    DO 2048 J=1,N_COLOR
       TMP_B = TMP_B + RELINT(J) 
       PRELINT(J) = TMP_B
2048 CONTINUE
          
    DPS_RAN3 = WRAN(ISTAR1)
    IF (DPS_RAN3.GE.0.AND.DPS_RAN3.LE.PRELINT(1)) THEN
       Q_WAVE = TWOPI*PHOTON(1)/TOCM
    END IF
     
    DO 2049 J=2,N_COLOR
       IF (DPS_RAN3.GT.PRELINT(J-1).AND.DPS_RAN3.LE.PRELINT(J)) THEN
          Q_WAVE = TWOPI*PHOTON(J)/TOCM
       END IF
    
2049 CONTINUE
    
    GO TO 2050
    
    ! C
    ! C Create the final array 
    ! C
2050 CONTINUE
    ray (1,ITIK)  =   XXX
    ray (2,ITIK)  =   YYY
    ray (3,ITIK)  =   ZZZ
    ray (4,ITIK)  =    DIREC(1)
    ray (5,ITIK)  =    DIREC(2)
    !!srioTest BEGIN (5,ITIK)  =    1.0D0
    ray (6,ITIK)  =    DIREC(3)
    ray (7,ITIK) =   A_VEC(1)
    !!srioTest BEGIN (7,ITIK) =   1.0D0
    ray (8,ITIK) =   A_VEC(2)
    ray (9,ITIK) =   A_VEC(3)
    ray (10,ITIK) =   1.0D0
    ray (11,ITIK) =   Q_WAVE
    !!srio                BEGIN (12,ITIK) =   FLOAT (ITIK)
    ray (12,ITIK) =   dble(ITIK)
    IF (F_POLAR.EQ.1) THEN
       ray (13,ITIK) =   0.0D0
       ray (14,ITIK)   =   PHASEX
       ray (15,ITIK)   =   PHASEZ
       ray (16,ITIK) =   AP_VEC(1)
       ray (17,ITIK) =   AP_VEC(2)
       ray (18,ITIK) =   AP_VEC(3)
    END IF

    ! C
    ! C All rays are generated. Test for acceptance if optimized source is
    ! C specified.
    ! C
    IF (F_BOUND_SOUR.GT.0 .AND. FGRID.EQ.0 ) THEN
       SB_POS(1) = XXX
       SB_POS(2) = YYY 
       SB_POS(3) = ZZZ
       ITEST = 1
       CALL SOURCE_BOUND (SB_POS, DIREC, ITEST)
       IF (ITEST.LT.0) THEN
          K_REJ = K_REJ + 1
          N_REJ = N_REJ + 1
          ! C      WRITE(6,*) 'itest ===',ITEST
          !IF (K_REJ.EQ.500) THEN


          DO J=1,6
            GRID(J,ITIK) = WRAN(ISTAR1)
          END DO
          IF (K_REJ.EQ.NTOTAL) THEN
             WRITE(6,*)N_REJ,'   rays have been rejected so far.'
             WRITE(6,*)ITIK-1, '                  accepted.'
             K_REJ = 0
          END IF
          !?????
          !if (itik.eq.ntotal) goto 10005

          if ( (ntotalpoint.gt.0) .and. (n_rej.ge.ntotalpoint)) then
            PRINT *,'sourceGeom: too many rejected rays: ',ntotalpoint
            PRINT *,'sourceGeom:    check inputs (NTOTALPOINT) and/or file: '//trim(file_bound)
            PRINT *,'sourceGeom:    Exit'
            npoint = itik-1  ! the current index is a bad ray...
            ! exit
            goto 10005
          endif
          GOTO 10001
       END IF
    END IF
10000 CONTINUE
    
10005 CONTINUE
    IFLAG = 0
    IF (FSOUR.EQ.3) THEN
       ! C
       ! C Reset EPSI_X and EPSI_Z to the values input by the user.
       ! C
       EPSI_X = EPSI_XOLD
       EPSI_Z = EPSI_ZOLD
    END IF
    
    !
    ! put global variables into pool 
    !
    !TODO: work without globals!!!!
    CALL GlobalToPoolSource(pool00)

    ntotalpoint = N_REJ+NPOINT
    if (n_rej .gt. 0) then
      WRITE(6,*)'----------------------------------------------------------------'
      WRITE(6,*)'  source optimization (rejection, variance reduction) used: '
      WRITE(6,*)N_REJ+NPOINT,   '   total number of rays have been created.'
      WRITE(6,*)NPOINT   ,      '                  accepted (stored).'
      WRITE(6,*)N_REJ,          '                  rejected.'
      WRITE(6,*)real(N_REJ+NPOINT)/real(NPOINT), '      created/accepted ratio.'
      WRITE(6,*)'----------------------------------------------------------------'
    endif 

    WRITE(6,*)'Exit from SOURCE'
    RETURN
  END SUBROUTINE sourceGeom
  !
  !
  !

  Subroutine PoolSourceToGlobal(src) !bind(C,NAME="PoolSourceToGlobal")

    type(poolSource),intent(in)  :: src

#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) name = src%name
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) name = src%name
#include "shadow_source.def"

!  
!     FDISTR= pool00%FDISTR
!...
!     F_NEW= pool00%F_NEW
!     !! END CODE CREATED AUTOMATICALLY (makecode1.pro)
  End Subroutine PoolSourceToGlobal
  
  !
  !
  !
  
  
  Subroutine PoolOEToGlobal(oe) !bind(C,NAME="PoolOEToGlobal")
    
    type(poolOE),intent(in out) :: oe
    integer(kind=ski) :: i

#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) name = oe%name
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) name = oe%name
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) forall(i=1:arrdim) name(i) = oe%name(i)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) forall(i=1:arrdim) name(i) = oe%name(i)
#include "shadow_oe.def"


!     FMIRR= pool01%FMIRR
!...
!     THICK(10)= pool01%THICK(10)
    
  End Subroutine PoolOEToGlobal
  
  !
  !
  !
  
  
  Subroutine GlobalToPoolSource(src) 
    
    type(poolSource), intent(in out) :: src

#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) src%name = name
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) src%name = name
#include "shadow_source.def"   
 
!     pool00%FDISTR = FDISTR
!...
!     pool00%F_NEW = F_NEW
    
  End Subroutine GlobalToPoolSource
  
  !
  !
  !
  
  
  Subroutine GlobalToPoolOE(oe) 
    
    type(poolOE),intent(in out) :: oe
    integer(kind=ski) :: i

#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) oe%name = name
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) oe%name = name
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) forall(i=1:arrdim) oe%name(i) = name(i)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) forall(i=1:arrdim) oe%name(i) = name(i)
#include "shadow_oe.def"

!     
!     pool01%FMIRR = FMIRR
!...
!     pool01%THICK(10) = THICK(10)
  End Subroutine GlobalToPoolOE
  

! C+++
! C	SUBROUTINE	RESTART18
! C
! C	PURPOSE		Binding for RESTART. 
! C			Input and output is always RAY18(18,NPOINT) 
! C
! C	ARGUMENTS	[ I ]	RAY18	: the beam as obtained from the
! C					  last IMAGE plane.
! C			[ O ]   RAY18	: The same beam but in new RF
! C
! C	PARAMETERS	In Common blocks
! C
! C---
SUBROUTINE RESTART18 (RAY18,NCOL1,NPOINT1) !,RAY,PHASE,AP)

	implicit none

        integer(kind=ski), intent(in) :: NPOINT1
        integer(kind=ski), intent(in) :: NCOL1
        real(kind=skr),dimension(18,NPOINT1),    intent(in out) :: RAY18
        !real(kind=skr),dimension(NCOL1,NPOINT1), intent(in out) :: RAY
        !real(kind=skr),dimension(3,NPOINT1),     intent(in out) :: PHASE, AP
        real(kind=skr),dimension(NCOL1,NPOINT1)                  :: RAY
        real(kind=skr),dimension(3,NPOINT1)                      :: PHASE, AP

!
!       initialize
!
        RAY   = 0.0D0
        PHASE = 0.0D0
        AP    = 0.0D0
!
! cp
!
        RAY(:,:)   = RAY18(1:NCOL1,:)
        IF (NCOL1 == 18) THEN
          PHASE(:,:) = RAY18(13:15,:)
          AP(:,:)    = RAY18(16:18,:)
        END IF

!
! call the binded routine
!

	CALL RESTART(RAY,PHASE,AP)


!
! cp back
!
        RAY18(1:NCOL1,:)  = RAY  
        IF (NCOL1 == 18) THEN
          RAY18(13:15,:) = PHASE
          RAY18(16:18,:) = AP
	END IF

!TODO: remove, this is reduntant (for test)
!        RAY(:,:)   = RAY18(1:NCOL1,:)
!        IF (NCOL1 == 18) THEN
!          PHASE(:,:) = RAY18(13:15,:)
!          AP(:,:)    = RAY18(16:18,:)
!        END IF

End Subroutine restart18
  


! C+++
! C	SUBROUTINE	SCREEN18
! C
! C	PURPOSE		Binding for SCREEN. 
! C			Input and output is always RAY18(18,NPOINT) 
! C
! C	ARGUMENTS	[ I ]	RAY18	: the beam as obtained from the
! C					  last IMAGE plane.
! C			[ O ]   RAY18	: The same beam but in new RF
! C
! C	PARAMETERS	In Common blocks
! C
! C---
SUBROUTINE SCREEN18 (RAY18,NCOL1,NPOINT1, &
                     !RAY,AP,PHASE, &
                     i_what,i_element)

	implicit none

        integer(kind=ski), intent(in) :: NPOINT1,NCOL1
        integer(kind=ski), intent(in) :: i_what,i_element
        real(kind=skr),dimension(18,NPOINT1),    intent(in out) :: RAY18
        !real(kind=skr),dimension(NCOL1,NPOINT1), intent(in out) :: RAY
        !real(kind=skr),dimension(3,NPOINT1),     intent(in out) :: PHASE, AP
        real(kind=skr),dimension(NCOL1,NPOINT1) :: RAY
        real(kind=skr),dimension(3,NPOINT1)     :: PHASE, AP
        integer(kind=ski)                                   :: i,j

!
!       initialize
!
        RAY   = 0.0D0
        PHASE = 0.0D0
        AP    = 0.0D0
!
! cp
!
        RAY(:,:)   = RAY18(1:NCOL1,:)
        IF (NCOL1 == 18) THEN
          PHASE(:,:) = RAY18(13:15,:)
          AP(:,:)    = RAY18(16:18,:)
        END IF

!
! call the binded routine
!

	CALL SCREEN(RAY,AP,PHASE,I_WHAT,I_ELEMENT)


!
! cp back
!
        RAY18(1:NCOL1,:)  = RAY  
        IF (NCOL1 == 18) THEN
          RAY18(13:15,:) = PHASE
          RAY18(16:18,:) = AP
	END IF

End Subroutine screen18
  


! C+++
! C	SUBROUTINE	MIRROR18
! C
! C	PURPOSE		Binding for SCREEN. 
! C			Input and output is always RAY18(18,NPOINT) 
! C
! C	ARGUMENTS	[ I ]	RAY18	: the beam as obtained from the
! C					  last IMAGE plane.
! C			[ O ]   RAY18	: The same beam but in new RF
! C
! C	PARAMETERS	In Common blocks
! C
! C---
SUBROUTINE MIRROR18 (RAY18,NCOL1,NPOINT1, &
                     !RAY,AP,PHASE, &
                     i_which)

	implicit none

        integer(kind=ski), intent(in) :: NPOINT1,NCOL1
        integer(kind=ski), intent(in) :: i_which
        real(kind=skr),dimension(18,NPOINT1),    intent(in out) :: RAY18
        !real(kind=skr),dimension(NCOL1,NPOINT1), intent(in out) :: RAY
        !real(kind=skr),dimension(3,NPOINT1),     intent(in out) :: PHASE, AP
        real(kind=skr),dimension(NCOL1,NPOINT1) :: RAY
        real(kind=skr),dimension(3,NPOINT1)     :: PHASE, AP
        integer(kind=ski)                                   :: i,j

!
!       initialize
!
        RAY   = 0.0D0
        PHASE = 0.0D0
        AP    = 0.0D0
!
! cp
!
        RAY(:,:)   = RAY18(1:NCOL1,:)
        IF (NCOL1 == 18) THEN
          PHASE(:,:) = RAY18(13:15,:)
          AP(:,:)    = RAY18(16:18,:)
        END IF

!
! call the binded routine
!

	CALL MIRROR1(RAY,AP,PHASE,I_WHICH)

!
! cp back
!
        RAY18(1:NCOL1,:)  = RAY  
        IF (NCOL1 == 18) THEN
          RAY18(13:15,:) = PHASE
          RAY18(16:18,:) = AP
	END IF

End Subroutine MIRROR18
  


! C+++
! C	SUBROUTINE	IMAGE18
! C
! C	PURPOSE		Binding for IMAGE1. 
! C			Input and output is always RAY18(18,NPOINT) 
! C
! C	ARGUMENTS	[ I ]	RAY18	: the beam as obtained from the
! C					  last IMAGE plane.
! C			[ O ]   RAY18	: The same beam but in new RF
! C
! C	PARAMETERS	In Common blocks
! C
! C---
SUBROUTINE IMAGE18 (RAY18,NCOL1,NPOINT1, i_what)

        implicit none

        integer(kind=ski), intent(in) :: NPOINT1,NCOL1
        integer(kind=ski), intent(in) :: i_what
        real(kind=skr),dimension(18,NPOINT1),    intent(in out) :: RAY18
        !real(kind=skr),dimension(NCOL1,NPOINT1), intent(in out) :: RAY
        !real(kind=skr),dimension(3,NPOINT1),     intent(in out) :: PHASE, AP
        real(kind=skr),dimension(NCOL1,NPOINT1) :: RAY
        real(kind=skr),dimension(3,NPOINT1)     :: PHASE, AP
        integer(kind=ski)                       :: i,j

!
!       initialize
!
        RAY   = 0.0D0
        PHASE = 0.0D0
        AP    = 0.0D0
!
! cp
!
        RAY(:,:)   = RAY18(1:NCOL1,:)
        IF (NCOL1 == 18) THEN
          PHASE(:,:) = RAY18(13:15,:)
          AP(:,:)    = RAY18(16:18,:)
        END IF

!
! call the binded routine
!

        CALL IMAGE1(RAY,AP,PHASE,I_WHAT)


!
! cp back
!
        RAY18(1:NCOL1,:)  = RAY  
        IF (NCOL1 == 18) THEN
          RAY18(13:15,:) = PHASE
          RAY18(16:18,:) = AP
        END IF

End Subroutine image18

  
! C+++
! C	SUBROUTINE	DEALLOC
! C
! C	PURPOSE		Deallocate allocated arrays
! C
! C	ALGORITHM	
! C
! C	INPUTS		
! C
! C	OUTPUTS		
! C
! C---
  
SUBROUTINE DeAlloc
    
    implicit none

     integer(kind=ski) :: SERR,IFLAG
     real(kind=skr)    :: xin=0d0,yin=0d0,zout=0d0
     real(kind=skr),dimension(3)     :: vin=(/0.0,1.0,0.0/)
    
    WRITE(6,*)'Call to DEALLOC'


    IF (F_G_S.EQ.2) THEN
      ! C
      ! C External spline distortion selected.
      ! C
      IFLAG = -2
      SERR = 0
      !xin = 0.0D0
      !yin = 0.0D0
      zout = 0.0D0
      CALL SUR_SPLINE(XIN,YIN,ZOUT,VIN,IFLAG,SERR)
    END IF

    ! deallocate lens optical constants arrays
    if ((f_refrac.eq.1).and.(f_crystal.eq.0).and.(f_r_ind.gt.0)) then
       iflag = 2
       call get_refraction_index (iflag,xin,yin,yin,yin,yin)
    end if 
    
    WRITE(6,*)'Exit from DEALLOC'

    RETURN
         
END SUBROUTINE DeAlloc

! C+++
! C	SUBROUTINE	TRACEOE
! C
! C	PURPOSE		traces an optical element
! C			
! C
! C	ARGUMENTS	[ I ]	oeType	: the type with oe variables
! C                                       (from start.xx)
! C	         	[ I ]	ray18	: the array with source
! C	         	[ I ]	npoint1	: the number of rays in ray18
! C			[ O ]   oeType	: the type with oe variables (end.xx)
! C			[ O ]   RAY18	: the array with image (star.xx)
! C
! C
! C---
SUBROUTINE TraceOE (oeType,ray18,npoint1,icount) bind(C,NAME="TraceOE")


	implicit none

        integer(kind=ski),                     intent(in) :: npoint1,icount
        real(kind=skr),dimension(18,npoint1),  intent(in out) :: ray18
	type (poolOE),                         intent(in out) :: oeType

        integer(kind=ski)      :: i,ncol1


	if(ncol.ne.0) then
		ncol1 = ncol ! cp from existing one
	else
		ncol1 = 18
	endif

        call reset

        ! put variables of oe into global (ex-common blocks)
	call PoolOEToGlobal(oeType)

	NPOINT	= NPOINT1 ! overwrite the read NPOINT
	NCOL	= NCOL1   ! overwrite the read NCOL


	! C
	! C Defines the source parameters
	! C
     	CALL SETSOUR
	! C
	! C Compute the (x,y) reference frame on the image plane
	! C
	CALL IMREF
	! C
	! C Compute the parameters defining the central ray 
	! C
	CALL OPTAXIS (ICOUNT)
	! C
	! C Compute the mirror, if needed
	! C
	CALL MSETUP (ICOUNT)

	! C
	! C This call rotates the last RAY file in the new MIRROR reference
	! C frame
	! C
	!!CALL RESTART (RAY,PHASE,AP)
	CALL RESTART18 (RAY18,NCOL1,NPOINT1)
	! C
	! C Check if any screens are present ahead of the mirror.
	! C
     	DO  I=1,N_SCREEN
     	  IF (F_SCREEN.EQ.1.AND.I_SCREEN(I).EQ.1)  &
       	  !CALL  SCREEN (RAY,AP,PHASE,I,ICOUNT)
       	  CALL  SCREEN18 (RAY18,NCOL1,NPOINT1,I,ICOUNT)
	END DO
	! C
	! C Computes now the intersections onto the mirror and the new rays
	! C
        IF (F_KOMA.EQ.1) THEN
        ! Csrio	CALL KUMAKHOV(RAY,AP,PHASE,ICOUNT)
	      i = 0
	      CALL LEAVE ('KUMAKHOV','Not yet implemented in Shadow3',i)

	ELSE
		!!CALL MIRROR1 (RAY,AP,PHASE,ICOUNT)
		CALL MIRROR18(RAY18,NCOL1,NPOINT1,ICOUNT)
                ! writes mirr.xxB (if wanted)
                !CALL	FNAME	(FFILE, 'mirr', ICOUNT, izero)
                !IFLAG	= 0
                !CALL beamWrite(RAY18,iErr,NCOL,NPOINT,trim(ffile)//'B')
                !IF (IERR.NE.0) CALL LEAVE ('TRACE3','Error writing MIRR',IERR)
        END IF
	! C
	! C Computes other screens and stops.
	! C
     	DO  I=1,N_SCREEN
     	  IF (F_SCREEN.EQ.1.AND.I_SCREEN(I).EQ.0)  &
     	  !!CALL  SCREEN (RAY,AP,PHASE,I,ICOUNT)
     	  CALL  SCREEN18 (RAY18,NCOL1,NPOINT1,I,ICOUNT)
	END DO
	! C
	! C Computes the intersections on the image plane
	! C
	!!CALL IMAGE1 (RAY,AP,PHASE,ICOUNT)
	CALL IMAGE18 (RAY18,NCOL1,NPOINT1,ICOUNT)

       !
       ! deallocate arrays
       !
       CALL DEALLOC

       !CALL	FNAME (FFILE, 'star', ICOUNT, izero)
       !IFLAG	= 0
       !CALL beamWrite(RAY18,iErr,NCOL,NPOINT,trim(ffile)//'B')
       !IF (IERR.NE.0)	CALL LEAVE ('IMAGE','Error writing STAR',IERR)

	! cp global variables in input/output type
	call GlobalToPoolOE(oeType)

End Subroutine traceoe

!
! shadow3trace: driver for trace...
!  

! C+++
! C	SUBROUTINE	SHADOW3TRACE
! C
! C	PURPOSE		main driver for trace
! C	
! C			Note that this program is kept for compatibility
! C			with shadow2.x. A new more structurated program
! C			trace3 is also available.
! C	
! C
! C---
SUBROUTINE Shadow3Trace

        implicit none

        character(len=sklen)      :: mode !, arg
        integer(kind=ski)       :: icount,ipass,nsave,iTerminate
        integer(kind=ski)       :: numarg
        integer(kind=ski)       :: ncol1,np,iflag,ierr

        real(kind=skr),dimension(:,:),allocatable :: ray,phase,ap
	logical                                   :: logicalFlag=.true.

        print *,''
        print *,'Ray Tracing Selected. Begin procedure.'
        print *,''

        mode = RString('Mode selected [prompt OR batch OR systemfile] ?')

        IF ( (trim(mode) /= "prompt") .and. (trim(mode) /= "batch") .and. (trim(mode) /= "systemfile")) RETURN

        !call FStrUpCase(mode)

        icount = 0
        ipass  = 1
        nsave = 0

! C
! C Start by inquiring about the optical system
! C
        DO WHILE (logicalFlag)  ! enters in an infinite loop over oe's
!
! srio: this will change input mode, and load
! input variables (start.xx)
!
          CALL Reset
          CALL Switch_Inp (mode,icount,iTerminate)
          IF (iTerminate == 1) RETURN

!
! it is necessary to allocate main arrays (ray, phase, ap) here, 
!at the main level. 
! 

          IF ( .NOT.( ALLOCATED(Ray).AND.ALLOCATED(Ap).AND.ALLOCATED(Phase) ) ) THEN
            CALL beamGetDim (file_source,ncol1,np,iflag,ierr)

            IF ((iflag.NE.0).OR.(ierr.NE.0)) THEN
              PRINT *,'TRACE: beamGetDim: Error in file: '//TRIM(file_source)
              STOP
            ELSE
 
!
! allocate arrays
!
              IF (ALLOCATED(ray)) DEALLOCATE(ray)
              IF (ALLOCATED(ap)) DEALLOCATE(ap)
              IF (ALLOCATED(phase)) DEALLOCATE(phase)
              IF (.NOT. ALLOCATED(ray)) then
                ALLOCATE(ray(ncol1,np),STAT=ierr)
                IF (ierr /= 0) THEN
                  PRINT *,"TRACE: Error allocating ray" ; STOP 4
                END IF
              END IF
              IF (.NOT. ALLOCATED(ap)) THEN
                ALLOCATE(ap(3,np),STAT=ierr)
                IF (ierr /= 0) THEN
                  PRINT *,"TRACE: Error allocating ray" ; STOP 4
                END IF
              END IF
              IF (.NOT. ALLOCATED(phase)) THEN
                ALLOCATE(phase(3,np),STAT=ierr)
                IF (ierr /= 0) THEN
                  print *,"TRACE: Error allocating ray" ; STOP 4
                END IF
              END IF

!read source file (nor here... it is done in msetup)
            
! put dimensions in variable pool
              npoint=np
              ncol=ncol1
            END IF
          END IF
 

          CALL Trace_Step (nsave, icount, ipass, ray, phase, ap)
        END DO ! end do while infinite loop

        IF (ALLOCATED(ray)) DEALLOCATE(ray)
        IF (ALLOCATED(ap)) DEALLOCATE(ap)
        IF (ALLOCATED(phase)) DEALLOCATE(phase)

        RETURN

END SUBROUTINE Shadow3Trace


! C+++
! C	SUBROUTINE	GET_REFRACTION_INDEX
! C
! C	PURPOSE		To get the refraction index as a function of q (or energy)
! C                     for lenses.
! C
! C	FLAG		k_what:  .eq. 0 --> initialization call. Reads in data file.
! C			         .eq. 1 --> performs computations.
! C			         .eq. 2 --> deallocate arrays
! C			(see below)
! C
! C	ARGUMENTS	[ I ] wnum 	: wavenumber (cm-1) 
! C			[ O ] rr_ind_obj : real part of the refraction index (object)
! C			[ O ] rr_attenuation_obj : attenuation coeff (object)
! C			[ O ] rr_ind_ima : real part of the refraction index (image)
! C			[ O ] rr_attenuation_ima : attenuation coeff (image)
! C
! C---
SUBROUTINE get_refraction_index(k_what,WNUM,rr_ind_obj,rr_attenuation_obj, &
                                      rr_ind_ima,rr_attenuation_ima)

implicit none

integer(kind=ski),          intent(in)   :: k_what
real(kind=skr),             intent(in)   :: wnum
real(kind=skr),             intent(out)  :: rr_ind_obj,rr_attenuation_obj
real(kind=skr),             intent(out)  :: rr_ind_ima,rr_attenuation_ima

real(kind=skr)                           :: qmin_obj, qmax_obj, qstep_obj, depth0_obj
real(kind=skr)                           :: qmin_ima, qmax_ima, qstep_ima, depth0_ima
real(kind=skr),dimension(:),allocatable  :: zf1_obj,zf2_obj
real(kind=skr),dimension(:),allocatable  :: zf1_ima,zf2_ima
integer(kind=ski)                        :: nrefl_obj
integer(kind=ski)                        :: nrefl_ima

real(kind=skr)                  :: ratio, phot_ener, ratio1, ratio2
integer(kind=ski)               :: index1,i,iErr
real(kind=skr)                  :: wnum0,del_x
! note that alfa (watch the f!!) and gamma are called internally myALFA and myGAMMA
! to avoid conflict with the global ALFA and GAMMA.
real(kind=skr)                  :: myALFA,myGAMMA
integer(kind=ski)               :: i_debug=0


! C
! C SAVE the variables that need to be saved across subsequent invocations
! C of this subroutine.
! C
SAVE        QMIN_obj, QMAX_obj, QSTEP_obj, DEPTH0_obj, NREFL_obj, zf1_obj, zf2_obj
SAVE        QMIN_ima, QMAX_ima, QSTEP_ima, DEPTH0_ima, NREFL_ima, zf1_ima, zf2_ima

! C
! C Initialization call. 
! C WNUM is the WAVENUMBER (cm-1) of the ray.
! C ALFA and GAMMA are the complex dielectric function
! C		EPSILON	  = 1 - ALFA + i*GAMMA		[ i=sqrt(-1) ]
! C and are dependent ONLY on the material, while the reflectivities
! C depend of course on the geometry too.
! C
! C 	K_WHAT = 0	Initialization/arrays allocation
! C		 1	Calculation
! C		 2	arrays deallocation
! C

! initialize outputs to the constant values and return if interpolation vs E not needed.
rr_ind_obj = r_ind_obj
rr_attenuation_obj = r_attenuation_obj
rr_ind_ima = r_ind_ima
rr_attenuation_ima = r_attenuation_ima
IF ((K_WHAT.EQ.1).and.(F_R_IND.eq.0)) RETURN


select case (k_what)
case (0)  ! load files
    if (f_r_ind.eq.0) return

    if ((f_r_ind.eq.1).or.(f_r_ind.eq.3)) then 
        ! only new ascii output from prerefl is accepted for lenses
        OPEN  (23,FILE=FILE_R_IND_OBJ,STATUS='OLD', FORM='FORMATTED', IOSTAT=iErr)
        ! srio added test
        IF (ierr /= 0 ) then
             PRINT *,"GET_REFRACTION_INDEX: Error: File not found: "//TRIM(file_r_ind_obj)
             STOP ' Fatal error: aborted'
        END IF
        READ (23,*) QMIN_obj,QMAX_obj,QSTEP_obj,DEPTH0_obj
        READ (23,*) NREFL_obj
        IF (.NOT. ALLOCATED(zf1_obj)) THEN
          ALLOCATE(zf1_obj(nrefl_obj),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"GET_REFRACTION_INDEX: Error allocating array" ; STOP 4
          END IF
        END IF
        IF (.NOT. ALLOCATED(zf2_obj)) THEN
          ALLOCATE(zf2_obj(nrefl_obj),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"GET_REFRACTION_INDEX: Error allocating array" ; STOP 4
          END IF
        END IF
        READ (23,*) (zf1_obj(I),I=1,NREFL_obj)
        READ (23,*) (zf2_obj(I),I=1,NREFL_obj)
        CLOSE (23)
        if (i_debug.gt.0) print *,">>Debug: file read successfully: "//trim(FILE_R_IND_OBJ)
    end if


    if ((f_r_ind.eq.2).or.(f_r_ind.eq.3)) then 
        ! only new ascii output from prerefl is accepted for lenses
        OPEN  (23,FILE=FILE_R_IND_IMA,STATUS='OLD', FORM='FORMATTED', IOSTAT=iErr)
        ! srio added test
        IF (ierr /= 0 ) then
             PRINT *,"GET_REFRACTION_INDEX: Error: File not found: "//TRIM(file_r_ind_ima)
             STOP ' Fatal error: aborted'
        END IF
        READ (23,*) QMIN_ima,QMAX_ima,QSTEP_ima,DEPTH0_ima
        READ (23,*) NREFL_ima
        IF (.NOT. ALLOCATED(zf1_ima)) THEN
          ALLOCATE(zf1_ima(nrefl_ima),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"GET_REFRACTION_INDEX: Error allocating array" ; STOP 4
          END IF
        END IF
        IF (.NOT. ALLOCATED(zf2_ima)) THEN
          ALLOCATE(zf2_ima(nrefl_ima),STAT=ierr)
          IF (ierr /= 0) THEN
            print *,"GET_REFRACTION_INDEX: Error allocating array" ; STOP 4
          END IF
        END IF
        READ (23,*) (zf1_ima(I),I=1,NREFL_ima)
        READ (23,*) (zf2_ima(I),I=1,NREFL_ima)
        CLOSE (23)
        if (i_debug.gt.0) print *,">>Debug: file read successfully: "//trim(FILE_R_IND_IMA)
    end if
! C
! C This is the normal calculation part;
! C
case (1)

   ! interpolation in OBJECT space
   if ((f_r_ind.eq.1).or.(f_r_ind.eq.3)) then
       if (i_debug.gt.1) then 
          !
          ! write file (for test)
          !
          write(77,'(a)') "#S 1 image"
          write(77,'(a)') "#N 4"
          write(77,'(a)') "#L q[cm-1]  energy[eV]  delta  beta"
          do index1=1,nrefl_obj
             WNUM0  =   QSTEP_obj*(index1-1) + QMIN_obj
             write(77,*) wnum0,wnum0*tocm/twopi,zf1_obj(index1)/2,zf2_obj(index1)*wnum0
          end do
          print *,">>Debug: File fort.77 written"
       end if

       index1 =   (WNUM - QMIN_obj)/QSTEP_obj + 1
       IF (index1.LT.1) THEN 
          index1=1
          print *,"GET_REFRACTION_INDEX: Warning: Photon energy below lower limit. Rerun prerefl."
       END IF
       IF (index1.GT.NREFL_obj) THEN
          index1=NREFL_obj-1
          print *,"GET_REFRACTION_INDEX: Warning: Photon energy above upper limit. Rerun prerefl."
       END IF
       IF (index1.EQ.NREFL_obj)  index1  = index1 - 1
       WNUM0  =   QSTEP_obj*(index1-1) + QMIN_obj
       DEL_X  =   WNUM - WNUM0
       DEL_X  =   DEL_X/QSTEP_obj
       myALFA  =   zf1_obj(index1) + (zf1_obj(index1+1)-zf1_obj(index1))*DEL_X
       myGAMMA  =   zf2_obj(index1) + (zf2_obj(index1+1)-zf2_obj(index1))*DEL_X
       rr_ind_obj = 1d0 - myALFA/2
       rr_attenuation_obj = myGAMMA*wnum
   endif

   ! interpolation in image space
   if ((f_r_ind.eq.2).or.(f_r_ind.eq.3)) then
       if (i_debug.gt.1) then 
         !
         ! write file (for test)
         !
         write(88,'(a)') "#S 1 image"
         write(88,'(a)') "#N 4"
         write(88,'(a)') "#L q[cm-1]  energy[eV]  delta  beta"
         do index1=1,nrefl_ima
            WNUM0  =   QSTEP_ima*(index1-1) + QMIN_ima
            write(88,*) wnum0,wnum0*tocm/twopi,zf1_ima(index1)/2,zf2_ima(index1)*wnum0
         end do
         print *,">>Debug: File fort.88 written"
       end if

       index1 =   (WNUM - QMIN_ima)/QSTEP_ima + 1
       IF (index1.LT.1) THEN
          index1=1
          print *,"GET_REFRACTION_INDEX: Warning: Photon energy below lower limit. Rerun prerefl."
       END IF
       IF (index1.GT.NREFL_ima) THEN
          index1=NREFL_ima-1
          print *,"GET_REFRACTION_INDEX: Warning: Photon energy above upper limit. Rerun prerefl."
       END IF
       IF (index1.EQ.NREFL_ima)  index1  = index1 - 1
       WNUM0  =   QSTEP_ima*(index1-1) + QMIN_ima
       DEL_X  =   WNUM - WNUM0
       DEL_X  =   DEL_X/QSTEP_ima
       myALFA  =   zf1_ima(index1) + (zf1_ima(index1+1)-zf1_ima(index1))*DEL_X
       myGAMMA  =   zf2_ima(index1) + (zf2_ima(index1+1)-zf2_ima(index1))*DEL_X
       rr_ind_ima = 1d0 - myALFA/2
       rr_attenuation_ima = myGAMMA*wnum
   endif
   if (i_debug.gt.0) then
       print *,">debug: q,energy: ",wnum,wnum*tocm/twopi
       print *,">debug: delta obj,ima:       ",1d0-rr_ind_obj,1d0-rr_ind_ima
       print *,">debug: attenuation_obj,ima: ",rr_attenuation_obj,rr_attenuation_ima
   end if
!
! array deallocation
!
case (2)
   if(allocated(zf1_obj)) deallocate(zf1_obj)
   if(allocated(zf2_obj)) deallocate(zf2_obj)
   if(allocated(zf1_ima)) deallocate(zf1_ima)
   if(allocated(zf2_ima)) deallocate(zf2_ima)
   if (i_debug.gt.0) print *,">>Debug: arrays deallocated (prerefl for lens)"
case default
   continue
end select

return
End Subroutine get_refraction_index



!
! PREREFL_TEST
!
!
! this is a very simple routine that displays the refraction index obtained from
! a file created by prerefl. 
! It can be used for testing prerefl, or for simple calculations of refractive 
! index. 
!
! limitations: only (new) ascii files from prerefl are accepted 
!
! todo: move to shadow_preprocessors (needs direct access to file, not using 
!                                    get_refraction_index() )
SUBROUTINE prerefl_test ()

implicit none

integer(kind=ski)           :: k_what
real(kind=skr)              :: rr_ind_obj,rr_attenuation_obj
real(kind=skr)              :: rr_ind_ima,rr_attenuation_ima
real(kind=skr)              :: wnum,energy1

print *,"   prerefl_test: calculates refraction index for a given energy"
print *,"                 using a file created by the prerefl preprocessor."
print *,"   "
F_R_IND = 1  ! to read file in the object space 
FILE_R_IND_OBJ = RSTRING("File Name (from prerefl): ")
! read file
k_what=0
call GET_REFRACTION_INDEX (k_what,WNUM,rr_ind_obj,rr_attenuation_obj, &
                                      rr_ind_ima,rr_attenuation_ima)
energy1 = rnumber("Photon energy [eV]: ")
wnum = twopi*energy1/tocm
!calculate
k_what=1
call GET_REFRACTION_INDEX (k_what,WNUM,rr_ind_obj,rr_attenuation_obj, &
                                      rr_ind_ima,rr_attenuation_ima)
!print outputs
print *,"------------------------------------------------------------------------"
print *,"Inputs: "
print *,"   prerefl file: "//trim(FILE_R_IND_OBJ)//" gives for E=",energy1,"eV: "
print *,"   energy [eV]:                       ",energy1
print *,"   wavelength [A]:                    ",(1d0/wnum)*twopi*1e+8
print *,"   wavenumber (2 pi/lambda) [cm^-1]:  ",wnum
print *,"Outputs: "
print *,"   refraction index = (1-delta) + i*beta : "
print *,"   delta:                          ",1d0-rr_ind_obj
print *,"   beta:                           ",rr_attenuation_obj/(2*wnum)
print *,"   real(n):                        ",rr_ind_obj
print *,"   attenuation coef [cm^-1]:       ",rr_attenuation_obj
print *,"------------------------------------------------------------------------"

!cleaning
k_what=2
call GET_REFRACTION_INDEX (k_what,WNUM,rr_ind_obj,rr_attenuation_obj, &
                                      rr_ind_ima,rr_attenuation_ima)
RETURN
End Subroutine prerefl_test


!
! PRE_MLAYER_SCAN
!

!
! this is a simple routine that computes the multilayer reflectivity
! with a multilayer defined in a file created by pre_mlayer.
!
! It can be used for testing pre_mlayer, or for simple calculations of 
! ML reflectivity.
!
! todo: move to shadow_preprocessors (needs direct access to file, not using 
!                                    get_refraction_index() )
!
SUBROUTINE pre_mlayer_scan ()

implicit none

integer(kind=ski)           :: k_what, iscan=0, thetaN=1, energyN=1
integer(kind=ski)           :: i,j, iErr
real(kind=skr)              :: wnum
real(kind=skr)              :: energy1=0.0, theta1=0.0, energy2=0.0, theta2=0.0
real(kind=skr)              :: energy=0.0, theta=0.0, energyS = 0.0,thetaS = 0.0
real(kind=skr),dimension(3) :: pin
real(kind=skr)              :: sin_ref, cos_pole
real(kind=skr)              :: r_s, r_p, phaseS, phaseP, absor
character(len=sklen)        :: fileOut 

! initializa some variables
F_REFL = 2
pin(1) = 0.0
pin(2) = 0.0
pin(3) = 0.0
fileOut = "pre_mlayer_scan.dat"

!
! input section
!
print *,"   pre_mlayer_scan: calculates reflectivity of a multilayer"
print *,"                 using a file created by the pre_mlayer preprocessor."
print *,"   "
FILE_REFL  = RSTRING("File Name (from pre_mlayer): ")
!file_refl = "morawe.dat"

energyN = irint(' Number of energy points (1 for angle-scan): ')
thetaN = irint(' Number of anglular points (1 for energy-scan): ')
!energyN = 1
!thetaN = 5000

if (energyN .GT. 1) then 
     !energy1 = 12400.0
     !energy2 = 24800.0
     energy1 = rnumber("Photon energy from [eV]: ")
     energy2 = rnumber("              to [eV]: ")
else
     !energy1 = 17800.0
     energy1 = rnumber("Photon energy [eV]: ")
endif

if (thetaN .GT. 1) then 
     !theta1 = 0.05
     !theta2 = 3.0
     theta1 = rnumber("Incident grazing angle from [deg]: ")
     theta2 = rnumber("                       to [deg]: ")
else
     !theta1 = 0.75
     theta1 = rnumber("Incident grazing angle [deg]: ")
endif


! calculations

! initialization (read file)
k_what = 0 
sin_ref = 0.0
cos_pole = 1.0
wnum = twopi*energy1/tocm
CALL REFLEC (PIN,WNUM,SIN_REF,COS_POLE,R_P,R_S,PHASEP,PHASES,ABSOR,K_WHAT)

!calculate
k_what=1
if (energyN .GT. 1) energyS = (energy2-energy1)/float(energyN-1)
if (thetaN .GT. 1) thetaS = (theta2-theta1)/float(thetaN-1)

if ((energyN .GT. 1) .or. (thetaN .GT. 1) ) then
  OPEN  (23,FILE=fileOut,STATUS='unknown', FORM='FORMATTED', IOSTAT=iErr)
  write(23,'(a)') "#F "//trim(fileOut)
  write(23,'(a)') " "
  write(23,'(a)') "#S 1 pre_mlater_test results"
  write(23,'(a)') "#N 4" 
  write(23,'(a)') "#L energy[eV]  grazingAngle [deg]  R_S  R_P"
endif

do i=1,thetaN
  do j=1,energyN
    theta = theta1+ float(i-1)*thetaS
    energy = energy1+ float(j-1)*energyS
    sin_ref = sin ( theta * pi/180) 
    wnum = twopi*energy/tocm
    CALL REFLEC (PIN,WNUM,SIN_REF,COS_POLE,R_P,R_S,PHASEP,PHASES,ABSOR,K_WHAT)

    if ( (thetaN .eq. 1) .and. (energyN .eq. 1) ) then
       print *,"------------------------------------------------------------------------"
       print *,"Inputs: "
       print *,"   pre_mlayer file: "//trim(FILE_REFL)//" gives for E=",energy1,"eV: "
       print *,"   energy [eV]:                       ",energy
       print *,"   grazing angle [deg]:               ",theta
       print *,"   wavelength [A]:                    ",(1d0/wnum)*twopi*1e+8
       print *,"   wavenumber (2 pi/lambda) [cm^-1]:  ",wnum
       print *,"Outputs: "
       print *,"   R_S:                          ",R_S
       print *,"   R_P:                          ",R_P
       print *,"------------------------------------------------------------------------"
    else
      write(23,*) energy,theta,r_s,r_p
    endif
  end do
end do

!print outputs

!cleaning
k_what=2
CALL REFLEC (PIN,WNUM,SIN_REF,COS_POLE,R_P,R_S,PHASEP,PHASES,ABSOR,K_WHAT)

if ((energyN .GT. 1) .or. (thetaN .GT. 1) ) then
  CLOSE  (23)
  print *,"File "//trim(fileOut)//" written to disk. "
endif

RETURN
End Subroutine pre_mlayer_scan

! C+++
! C	SUBROUTINE	presurface_translate
! C
! C	PURPOSE		To compute the interpolated surface from a 
! C			bi-cubic spline.
! C
! C	INPUT		An unformatted file prepared by PRESURFACE.
! C
! C	ARGUMENTS	Input:
! C			 {x,y} 	coordinates
! C			 IFlag: -1, readin file from FILESURF
! C			         0, compute z
! C				-2, clear arrays
! C			Output:
! C			 z	value of z at {x.y}
! C			 v[3]	normal to surface at {x,y,z}
! C			 Iflag:  0, normal completion
! C			        -1, out of bounds
! C			serr: surface spline error (-9 is bad)
! C
! C---
SUBROUTINE presurface_translate ()


! C This routine now takes an additional parameter SERR which indicates
! C whether errors occur when calculating the ray's intersection with the
! C mirror as specified by a PRESURFACE spline file.

! C Below, the CSPL, X, and Y have been changed to allow a maximum of
! C 201 points instead of 101.
! C

implicit none
real(kind=skr),dimension(201) :: Xold
real(kind=skr),dimension(201) :: Yold
real(kind=skr),dimension(2,201,2,201) :: CSPLold
!
real(kind=skr),dimension(501) :: X
real(kind=skr),dimension(6)   :: PDS(6)
real(kind=skr),dimension(:),allocatable :: Y
real(kind=skr),dimension(:,:,:,:),allocatable ::  CSPL

character(len=sklen)      :: filein,fileout

INTEGER(KIND=ski) :: iostat_value, SERR,nx,ny,nxold,nyold,i,j,k,l
! C 
! C Replace OPEN calls with library routine FOPENR()
! C	  CALL FOPENR(20, FILE_RIP, 'UNFORMATTED', IFERR, IOSTAT)
! C
filein =  rstring('Input file (binary from presurface/SHADOW 2.x): ')
OPEN  (20, FILE=filein, STATUS='OLD', FORM='UNFORMATTED', IOSTAT=IOSTAT_value)
IF (IOSTAT_value.NE.0) THEN
  print *,'PRESURFACE_TRANSLATE: Error opening file '//trim(filein)
END IF
READ  (20) NXold, Nyold
READ  (20) Xold,Yold
READ  (20) CSPLold
CLOSE (20)

print *,'X: ',xold(1:3),'...',xold((nxold-3):nxold)
print *,'Y: ',yold(1:nyold)
print *,'NX: ',nxold
print *,'NY: ',nyold

!
! new file
!
fileout =  rstring('Output file (binary for SHADOW3): ')

nx = nxold
ny = nyold

allocate( Y(NY) )
allocate( CSPL(2,501,2,NY) )
! cp data
X = 0.0
cspl = 0.0
X(1:nx) = xold(1:nx)
Y = yold(1:ny)
do i=1,2 
do j=1,201
do k=1,2
do l=1,NY
cspl(i,j,k,l) = csplold(i,j,k,l)
end do
end do
end do
end do

OPEN  (20, FILE=fileout, STATUS='unknown', FORM='UNFORMATTED', IOSTAT=IOSTAT_value)
write  (20) nX,nY
write  (20) X,Y
write  (20) CSPL
CLOSE (20)
! C
! C Succesful completion
! C
! D	  WRITE(6,*)'Read ',NX,' by ',NY,' array.'
!     	  IERR = 0
!     	 RETURN
!     	ELSE IF (IERR.EQ.-2) THEN !deallocate arrays
!     	  RETURN
!     	ELSE 
!     	END IF

IF(ALLOCATED( Y ))    DEALLOCATE(Y)
IF(ALLOCATED( CSPL )) DEALLOCATE(CSPL)

return
End Subroutine presurface_translate

!
! Check perpendicularity of three vectors (the returned result must be zero)
!
real(kind=skr) FUNCTION check_perp_vect (v1,v2,v3)
!
implicit none
real(kind=skr),dimension(3),intent(in)  :: v1,v2,v3
!
check_perp_vect = abs(dot_product(v1,v2)) + abs(dot_product(v1,v3)) + abs(dot_product(v2,v3))
End Function check_perp_vect

!
!
! Check perpendicularity of three vectors (given coordinates). The returned result must be zero.
!
real(kind=skr) FUNCTION check_perp_comp (v11,v12,v13,  v21,v22,v23, v31,v32,v33 )
   !
   implicit none
   real(kind=skr),intent(in)  :: v11,v12,v13,v21,v22,v23,v31,v32,v33
   real(kind=skr),dimension(3)   :: v1,v2,v3
   !
   v1=(/v11,v12,v13/)
   v2=(/v21,v22,v23/)
   v3=(/v31,v32,v33/)
   check_perp_comp = check_perp_vect (v1,v2,v3)
End Function check_perp_comp

End Module shadow_kernel

