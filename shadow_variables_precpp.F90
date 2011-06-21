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
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ftype(kind=fkind) :: name
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ftype(kind=fkind,len=length) :: name
#include "ShadowMaskSource.def"
    end type poolSource
    
    
    type, public, bind(C) :: poolOE
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ftype(kind=fkind) :: name
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ftype(kind=fkind,len=length) :: name
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) ftype(kind=fkind), dimension(arrdim) :: name
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) ftype(kind=fkind, len=length), dimension(arrdim) :: name
#include "ShadowMaskOE.def"
    end type poolOE



    public  :: PoolOELoad,PoolOEWrite,PoolSourceLoad,PoolSourceWrite
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
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfForceSetValue(gf, #name, src%name) .and. iOut
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfForceSetValue(gf, #name, src%name) .and. iOut
#include "ShadowMaskSource.def"
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
    character(kind=skc,len=2), dimension(10)     :: str
    
    iOut = GfTypeAllocate(gf,zero,zero)

    do i=1, 10
       write(str(i),'(I2)') i
    end do

    !! START CODE CREATED AUTOMATICALLY (cpp)
    
    !srio danger
    !iOut= iOut .and. GfForceSetValue(gf,"NPOINT",pool01%NPOINTOE)

#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfForceSetValue(gf, #name, oe%name) .and. iOut
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfForceSetValue(gf, #name, oe%name) .and. iOut
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
    do i=1, arrdim newline \
       iOut=GfForceSetValue(gf, myConcat( #name, trim(str(i))), oe%name(i)) .and. iOut newline \
    end do
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \ 
    do i=1, arrdim newline \
       iOut=GfForceSetValue(gf, myConcat( #name, trim(str(i))), oe%name(i)) .and. iOut newline \
    end do
#include "ShadowMaskOE.def"
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

    SELECT CASE (OS_NAME)
      CASE ("Linux") 
           iout = gffilewrite(gf,"/dev/null")
      CASE ("Windows") 
           iout = gffilewrite(gf,"tmp.dat")
      CASE DEFAULT
           iout = gffilewrite(gf,"/dev/null")
    END SELECT

    !! START CODE CREATED AUTOMATICALLY (cpp)
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfGetValue(gf,#name,src%name)  .and. iOut
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfGetValue(gf,#name,src%name)  .and. iOut
#include "ShadowMaskSource.def"
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
    character(kind=skc,len=2), dimension(10) :: str
    
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

    SELECT CASE (OS_NAME)
      CASE ("Linux") 
           iout = gffilewrite(gf,"/dev/null")
      CASE ("Windows") 
           iout = gffilewrite(gf,"tmp.dat")
      CASE DEFAULT
           iout = gffilewrite(gf,"/dev/null")
    END SELECT

    do i=1, 10
       write(str(i),'(I2)') i
    end do

!     !! START CODE CREATED AUTOMATICALLY (makecode1.pro)
!     
!     iOut= GfGetValue(gf,"FMIRR",oe%FMIRR)                       .and. iOut 
!     iOut= GfGetValue(gf,"F_TORUS",oe%F_TORUS)                   .and. iOut 
!     iOut= GfGetValue(gf,"FCYL",oe%FCYL)                         .and. iOut 
!     iOut= GfGetValue(gf,"F_EXT",oe%F_EXT)                       .and. iOut 
!     iOut= GfGetValue(gf,"FSTAT",oe%FSTAT)                       .and. iOut 
!     iOut= GfGetValue(gf,"F_SCREEN",oe%F_SCREEN)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_PLATE",oe%F_PLATE)                   .and. iOut 
!     iOut= GfGetValue(gf,"FSLIT",oe%FSLIT)                       .and. iOut 
!     iOut= GfGetValue(gf,"FWRITE",oe%FWRITE)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_RIPPLE",oe%F_RIPPLE)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_MOVE",oe%F_MOVE)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_THICK",oe%F_THICK)                   .and. iOut 
!     iOut= GfGetValue(gf,"F_BRAGG_A",oe%F_BRAGG_A)               .and. iOut 
!     iOut= GfGetValue(gf,"F_G_S",oe%F_G_S)                       .and. iOut 
!     iOut= GfGetValue(gf,"F_R_RAN",oe%F_R_RAN)                   .and. iOut 
!     iOut= GfGetValue(gf,"F_GRATING",oe%F_GRATING)               .and. iOut 
!     iOut= GfGetValue(gf,"F_MOSAIC",oe%F_MOSAIC)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_JOHANSSON",oe%F_JOHANSSON)           .and. iOut 
!     iOut= GfGetValue(gf,"F_SIDE",oe%F_SIDE)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_CENTRAL",oe%F_CENTRAL)               .and. iOut 
!     iOut= GfGetValue(gf,"F_CONVEX",oe%F_CONVEX)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_REFLEC",oe%F_REFLEC)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_RUL_ABS",oe%F_RUL_ABS)               .and. iOut 
!     iOut= GfGetValue(gf,"F_RULING",oe%F_RULING)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_PW",oe%F_PW)                         .and. iOut 
!     iOut= GfGetValue(gf,"F_PW_C",oe%F_PW_C)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_VIRTUAL",oe%F_VIRTUAL)               .and. iOut 
!     iOut= GfGetValue(gf,"FSHAPE",oe%FSHAPE)                     .and. iOut 
!     iOut= GfGetValue(gf,"FHIT_C",oe%FHIT_C)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_MONO",oe%F_MONO)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_REFRAC",oe%F_REFRAC)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_DEFAULT",oe%F_DEFAULT)               .and. iOut 
!     iOut= GfGetValue(gf,"F_REFL",oe%F_REFL)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_HUNT",oe%F_HUNT)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_CRYSTAL",oe%F_CRYSTAL)               .and. iOut 
!     iOut= GfGetValue(gf,"F_PHOT_CENT",oe%F_PHOT_CENT)           .and. iOut 
!     iOut= GfGetValue(gf,"F_ROUGHNESS",oe%F_ROUGHNESS)           .and. iOut 
!     iOut= GfGetValue(gf,"F_ANGLE",oe%F_ANGLE)                   .and. iOut 
!     iOut= GfGetValue(gf,"NPOINT",oe%NPOINTOE)                   .and. iOut 
!     iOut= GfGetValue(gf,"NCOL",oe%NCOL)                         .and. iOut 
!     iOut= GfGetValue(gf,"N_SCREEN",oe%N_SCREEN)                 .and. iOut 
!     iOut= GfGetValue(gf,"ISTAR1",oe%ISTAR1)                     .and. iOut 
!     iOut= GfGetValue(gf,"CIL_ANG",oe%CIL_ANG)                   .and. iOut 
!     iOut= GfGetValue(gf,"ELL_THE",oe%ELL_THE)                   .and. iOut 
!     iOut= GfGetValue(gf,"N_PLATES",oe%N_PLATES)                 .and. iOut 
!     iOut= GfGetValue(gf,"IG_SEED",oe%IG_SEED)                   .and. iOut 
!     iOut= GfGetValue(gf,"MOSAIC_SEED",oe%MOSAIC_SEED)           .and. iOut 
!     iOut= GfGetValue(gf,"ALPHA",oe%ALPHA)                       .and. iOut 
!     iOut= GfGetValue(gf,"SSOUR",oe%SSOUR)                       .and. iOut 
!     iOut= GfGetValue(gf,"THETA",oe%THETA)                       .and. iOut 
!     iOut= GfGetValue(gf,"SIMAG",oe%SIMAG)                       .and. iOut 
!     iOut= GfGetValue(gf,"RDSOUR",oe%RDSOUR)                     .and. iOut 
!     iOut= GfGetValue(gf,"RTHETA",oe%RTHETA)                     .and. iOut 
!     iOut= GfGetValue(gf,"OFF_SOUX",oe%OFF_SOUX)                 .and. iOut 
!     iOut= GfGetValue(gf,"OFF_SOUY",oe%OFF_SOUY)                 .and. iOut 
!     iOut= GfGetValue(gf,"OFF_SOUZ",oe%OFF_SOUZ)                 .and. iOut 
!     iOut= GfGetValue(gf,"ALPHA_S",oe%ALPHA_S)                   .and. iOut 
!     iOut= GfGetValue(gf,"RLEN1",oe%RLEN1)                       .and. iOut 
!     iOut= GfGetValue(gf,"RLEN2",oe%RLEN2)                       .and. iOut 
!     iOut= GfGetValue(gf,"RMIRR",oe%RMIRR)                       .and. iOut 
!     iOut= GfGetValue(gf,"AXMAJ",oe%AXMAJ)                       .and. iOut 
!     iOut= GfGetValue(gf,"AXMIN",oe%AXMIN)                       .and. iOut 
!     iOut= GfGetValue(gf,"CONE_A",oe%CONE_A)                     .and. iOut 
!     iOut= GfGetValue(gf,"R_MAJ",oe%R_MAJ)                       .and. iOut 
!     iOut= GfGetValue(gf,"R_MIN",oe%R_MIN)                       .and. iOut 
!     iOut= GfGetValue(gf,"RWIDX1",oe%RWIDX1)                     .and. iOut 
!     iOut= GfGetValue(gf,"RWIDX2",oe%RWIDX2)                     .and. iOut 
!     iOut= GfGetValue(gf,"PARAM",oe%PARAM)                       .and. iOut 
!     iOut= GfGetValue(gf,"HUNT_H",oe%HUNT_H)                     .and. iOut 
!     iOut= GfGetValue(gf,"HUNT_L",oe%HUNT_L)                     .and. iOut 
!     iOut= GfGetValue(gf,"BLAZE",oe%BLAZE)                       .and. iOut 
!     iOut= GfGetValue(gf,"RULING",oe%RULING)                     .and. iOut 
!     iOut= GfGetValue(gf,"ORDER",oe%ORDER)                       .and. iOut 
!     iOut= GfGetValue(gf,"PHOT_CENT",oe%PHOT_CENT)               .and. iOut 
!     iOut= GfGetValue(gf,"X_ROT",oe%X_ROT)                       .and. iOut 
!     iOut= GfGetValue(gf,"D_SPACING",oe%D_SPACING)               .and. iOut 
!     iOut= GfGetValue(gf,"A_BRAGG",oe%A_BRAGG)                   .and. iOut 
!     iOut= GfGetValue(gf,"SPREAD_MOS",oe%SPREAD_MOS)             .and. iOut 
!     iOut= GfGetValue(gf,"THICKNESS",oe%THICKNESS)               .and. iOut 
!     iOut= GfGetValue(gf,"R_JOHANSSON",oe%R_JOHANSSON)           .and. iOut 
!     iOut= GfGetValue(gf,"Y_ROT",oe%Y_ROT)                       .and. iOut 
!     iOut= GfGetValue(gf,"Z_ROT",oe%Z_ROT)                       .and. iOut 
!     iOut= GfGetValue(gf,"OFFX",oe%OFFX)                         .and. iOut 
!     iOut= GfGetValue(gf,"OFFY",oe%OFFY)                         .and. iOut 
!     iOut= GfGetValue(gf,"OFFZ",oe%OFFZ)                         .and. iOut 
!     iOut= GfGetValue(gf,"SLLEN",oe%SLLEN)                       .and. iOut 
!     iOut= GfGetValue(gf,"SLWID",oe%SLWID)                       .and. iOut 
!     iOut= GfGetValue(gf,"SLTILT",oe%SLTILT)                     .and. iOut 
!     iOut= GfGetValue(gf,"COD_LEN",oe%COD_LEN)                   .and. iOut 
!     iOut= GfGetValue(gf,"COD_WID",oe%COD_WID)                   .and. iOut 
!     iOut= GfGetValue(gf,"X_SOUR",oe%X_SOUR)                     .and. iOut 
!     iOut= GfGetValue(gf,"Y_SOUR",oe%Y_SOUR)                     .and. iOut 
!     iOut= GfGetValue(gf,"Z_SOUR",oe%Z_SOUR)                     .and. iOut 
!     iOut= GfGetValue(gf,"X_SOUR_ROT",oe%X_SOUR_ROT)             .and. iOut 
!     iOut= GfGetValue(gf,"Y_SOUR_ROT",oe%Y_SOUR_ROT)             .and. iOut 
!     iOut= GfGetValue(gf,"Z_SOUR_ROT",oe%Z_SOUR_ROT)             .and. iOut 
!     iOut= GfGetValue(gf,"R_LAMBDA",oe%R_LAMBDA)                 .and. iOut 
!     iOut= GfGetValue(gf,"THETA_I",oe%THETA_I)                   .and. iOut 
!     iOut= GfGetValue(gf,"ALPHA_I",oe%ALPHA_I)                   .and. iOut 
!     iOut= GfGetValue(gf,"T_INCIDENCE",oe%T_INCIDENCE)           .and. iOut 
!     iOut= GfGetValue(gf,"T_SOURCE",oe%T_SOURCE)                 .and. iOut 
!     iOut= GfGetValue(gf,"T_IMAGE",oe%T_IMAGE)                   .and. iOut 
!     iOut= GfGetValue(gf,"T_REFLECTION",oe%T_REFLECTION)         .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SOURCE",oe%FILE_SOURCE)           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_RIP",oe%FILE_RIP)                 .and. iOut 
!     iOut= GfGetValue(gf,"FILE_REFL",oe%FILE_REFL)               .and. iOut 
!     iOut= GfGetValue(gf,"FILE_MIR",oe%FILE_MIR)                 .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ROUGH",oe%FILE_ROUGH)             .and. iOut 
!     iOut= GfGetValue(gf,"FZP",oe%FZP)                           .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_R1",oe%HOLO_R1)                   .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_R2",oe%HOLO_R2)                   .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_DEL",oe%HOLO_DEL)                 .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_GAM",oe%HOLO_GAM)                 .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_W",oe%HOLO_W)                     .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_RT1",oe%HOLO_RT1)                 .and. iOut 
!     iOut= GfGetValue(gf,"HOLO_RT2",oe%HOLO_RT2)                 .and. iOut 
!     iOut= GfGetValue(gf,"AZIM_FAN",oe%AZIM_FAN)                 .and. iOut 
!     iOut= GfGetValue(gf,"DIST_FAN",oe%DIST_FAN)                 .and. iOut 
!     iOut= GfGetValue(gf,"COMA_FAC",oe%COMA_FAC)                 .and. iOut 
!     iOut= GfGetValue(gf,"ALFA",oe%ALFA)                         .and. iOut 
!     iOut= GfGetValue(gf,"GAMMA",oe%GAMMA)                       .and. iOut 
!     iOut= GfGetValue(gf,"R_IND_OBJ",oe%R_IND_OBJ)               .and. iOut 
!     iOut= GfGetValue(gf,"R_IND_IMA",oe%R_IND_IMA)               .and. iOut 
!     iOut= GfGetValue(gf,"RUL_A1",oe%RUL_A1)                     .and. iOut 
!     iOut= GfGetValue(gf,"RUL_A2",oe%RUL_A2)                     .and. iOut 
!     iOut= GfGetValue(gf,"RUL_A3",oe%RUL_A3)                     .and. iOut 
!     iOut= GfGetValue(gf,"RUL_A4",oe%RUL_A4)                     .and. iOut 
!     iOut= GfGetValue(gf,"F_POLSEL",oe%F_POLSEL)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_FACET",oe%F_FACET)                   .and. iOut 
!     iOut= GfGetValue(gf,"F_FAC_ORIENT",oe%F_FAC_ORIENT)         .and. iOut 
!     iOut= GfGetValue(gf,"F_FAC_LATT",oe%F_FAC_LATT)             .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_LENX",oe%RFAC_LENX)               .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_LENY",oe%RFAC_LENY)               .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_PHAX",oe%RFAC_PHAX)               .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_PHAY",oe%RFAC_PHAY)               .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_DELX1",oe%RFAC_DELX1)             .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_DELX2",oe%RFAC_DELX2)             .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_DELY1",oe%RFAC_DELY1)             .and. iOut 
!     iOut= GfGetValue(gf,"RFAC_DELY2",oe%RFAC_DELY2)             .and. iOut 
!     iOut= GfGetValue(gf,"FILE_FAC",oe%FILE_FAC)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_SEGMENT",oe%F_SEGMENT)               .and. iOut 
!     iOut= GfGetValue(gf,"ISEG_XNUM",oe%ISEG_XNUM)               .and. iOut 
!     iOut= GfGetValue(gf,"ISEG_YNUM",oe%ISEG_YNUM)               .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SEGMENT",oe%FILE_SEGMENT)         .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SEGP",oe%FILE_SEGP)               .and. iOut 
!     iOut= GfGetValue(gf,"SEG_LENX",oe%SEG_LENX)                 .and. iOut 
!     iOut= GfGetValue(gf,"SEG_LENY",oe%SEG_LENY)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_KOMA",oe%F_KOMA)                     .and. iOut 
!     iOut= GfGetValue(gf,"FILE_KOMA",oe%FILE_KOMA)               .and. iOut 
!     iOut= GfGetValue(gf,"F_EXIT_SHAPE",oe%F_EXIT_SHAPE)         .and. iOut 
!     iOut= GfGetValue(gf,"F_INC_MNOR_ANG",oe%F_INC_MNOR_ANG)     .and. iOut 
!     iOut= GfGetValue(gf,"ZKO_LENGTH",oe%ZKO_LENGTH)             .and. iOut 
!     iOut= GfGetValue(gf,"RKOMA_CX",oe%RKOMA_CX)                 .and. iOut 
!     iOut= GfGetValue(gf,"RKOMA_CY",oe%RKOMA_CY)                 .and. iOut 
!     iOut= GfGetValue(gf,"F_KOMA_CA",oe%F_KOMA_CA)               .and. iOut 
!     iOut= GfGetValue(gf,"FILE_KOMA_CA",oe%FILE_KOMA_CA)         .and. iOut 
!     iOut= GfGetValue(gf,"F_KOMA_BOUNCE",oe%F_KOMA_BOUNCE)       .and. iOut 
!     iOut= GfGetValue(gf,"X_RIP_AMP",oe%X_RIP_AMP)               .and. iOut 
!     iOut= GfGetValue(gf,"X_RIP_WAV",oe%X_RIP_WAV)               .and. iOut 
!     iOut= GfGetValue(gf,"X_PHASE",oe%X_PHASE)                   .and. iOut 
!     iOut= GfGetValue(gf,"Y_RIP_AMP",oe%Y_RIP_AMP)               .and. iOut 
!     iOut= GfGetValue(gf,"Y_RIP_WAV",oe%Y_RIP_WAV)               .and. iOut 
!     iOut= GfGetValue(gf,"Y_PHASE",oe%Y_PHASE)                   .and. iOut 
!     iOut= GfGetValue(gf,"N_RIP",oe%N_RIP)                       .and. iOut 
!     iOut= GfGetValue(gf,"ROUGH_X",oe%ROUGH_X)                   .and. iOut 
!     iOut= GfGetValue(gf,"ROUGH_Y",oe%ROUGH_Y)                   .and. iOut 
!     iOut= GfGetValue(gf,"OE_NUMBER",oe%OE_NUMBER)               .and. iOut 
!     iOut= GfGetValue(gf,"IDUMMY",oe%IDUMMY)                     .and. iOut 
!     iOut= GfGetValue(gf,"DUMMY",oe%DUMMY)                       .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(1)",oe%CX_SLIT(1))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(2)",oe%CX_SLIT(2))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(3)",oe%CX_SLIT(3))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(4)",oe%CX_SLIT(4))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(5)",oe%CX_SLIT(5))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(6)",oe%CX_SLIT(6))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(7)",oe%CX_SLIT(7))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(8)",oe%CX_SLIT(8))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(9)",oe%CX_SLIT(9))             .and. iOut 
!     iOut= GfGetValue(gf,"CX_SLIT(10)",oe%CX_SLIT(10))           .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(1)",oe%CZ_SLIT(1))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(2)",oe%CZ_SLIT(2))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(3)",oe%CZ_SLIT(3))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(4)",oe%CZ_SLIT(4))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(5)",oe%CZ_SLIT(5))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(6)",oe%CZ_SLIT(6))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(7)",oe%CZ_SLIT(7))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(8)",oe%CZ_SLIT(8))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(9)",oe%CZ_SLIT(9))             .and. iOut 
!     iOut= GfGetValue(gf,"CZ_SLIT(10)",oe%CZ_SLIT(10))           .and. iOut 
!     iOut= GfGetValue(gf,"D_PLATE(1)",oe%D_PLATE(1))             .and. iOut 
!     iOut= GfGetValue(gf,"D_PLATE(2)",oe%D_PLATE(2))             .and. iOut 
!     iOut= GfGetValue(gf,"D_PLATE(3)",oe%D_PLATE(3))             .and. iOut 
!     iOut= GfGetValue(gf,"D_PLATE(4)",oe%D_PLATE(4))             .and. iOut 
!     iOut= GfGetValue(gf,"D_PLATE(5)",oe%D_PLATE(5))             .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(1)",oe%FILE_ABS(1))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(2)",oe%FILE_ABS(2))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(3)",oe%FILE_ABS(3))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(4)",oe%FILE_ABS(4))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(5)",oe%FILE_ABS(5))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(6)",oe%FILE_ABS(6))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(7)",oe%FILE_ABS(7))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(8)",oe%FILE_ABS(8))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(9)",oe%FILE_ABS(9))           .and. iOut 
!     iOut= GfGetValue(gf,"FILE_ABS(10)",oe%FILE_ABS(10))         .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(1)",oe%FILE_SCR_EXT(1))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(2)",oe%FILE_SCR_EXT(2))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(3)",oe%FILE_SCR_EXT(3))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(4)",oe%FILE_SCR_EXT(4))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(5)",oe%FILE_SCR_EXT(5))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(6)",oe%FILE_SCR_EXT(6))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(7)",oe%FILE_SCR_EXT(7))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(8)",oe%FILE_SCR_EXT(8))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(9)",oe%FILE_SCR_EXT(9))   .and. iOut 
!     iOut= GfGetValue(gf,"FILE_SCR_EXT(10)",oe%FILE_SCR_EXT(10)) .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(1)",oe%I_ABS(1))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(2)",oe%I_ABS(2))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(3)",oe%I_ABS(3))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(4)",oe%I_ABS(4))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(5)",oe%I_ABS(5))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(6)",oe%I_ABS(6))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(7)",oe%I_ABS(7))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(8)",oe%I_ABS(8))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(9)",oe%I_ABS(9))                 .and. iOut 
!     iOut= GfGetValue(gf,"I_ABS(10)",oe%I_ABS(10))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(1)",oe%I_SCREEN(1))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(2)",oe%I_SCREEN(2))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(3)",oe%I_SCREEN(3))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(4)",oe%I_SCREEN(4))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(5)",oe%I_SCREEN(5))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(6)",oe%I_SCREEN(6))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(7)",oe%I_SCREEN(7))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(8)",oe%I_SCREEN(8))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(9)",oe%I_SCREEN(9))           .and. iOut 
!     iOut= GfGetValue(gf,"I_SCREEN(10)",oe%I_SCREEN(10))         .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(1)",oe%I_SLIT(1))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(2)",oe%I_SLIT(2))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(3)",oe%I_SLIT(3))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(4)",oe%I_SLIT(4))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(5)",oe%I_SLIT(5))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(6)",oe%I_SLIT(6))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(7)",oe%I_SLIT(7))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(8)",oe%I_SLIT(8))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(9)",oe%I_SLIT(9))               .and. iOut 
!     iOut= GfGetValue(gf,"I_SLIT(10)",oe%I_SLIT(10))             .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(1)",oe%I_STOP(1))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(2)",oe%I_STOP(2))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(3)",oe%I_STOP(3))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(4)",oe%I_STOP(4))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(5)",oe%I_STOP(5))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(6)",oe%I_STOP(6))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(7)",oe%I_STOP(7))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(8)",oe%I_STOP(8))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(9)",oe%I_STOP(9))               .and. iOut 
!     iOut= GfGetValue(gf,"I_STOP(10)",oe%I_STOP(10))             .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(1)",oe%K_SLIT(1))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(2)",oe%K_SLIT(2))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(3)",oe%K_SLIT(3))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(4)",oe%K_SLIT(4))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(5)",oe%K_SLIT(5))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(6)",oe%K_SLIT(6))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(7)",oe%K_SLIT(7))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(8)",oe%K_SLIT(8))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(9)",oe%K_SLIT(9))               .and. iOut 
!     iOut= GfGetValue(gf,"K_SLIT(10)",oe%K_SLIT(10))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(1)",oe%RX_SLIT(1))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(2)",oe%RX_SLIT(2))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(3)",oe%RX_SLIT(3))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(4)",oe%RX_SLIT(4))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(5)",oe%RX_SLIT(5))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(6)",oe%RX_SLIT(6))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(7)",oe%RX_SLIT(7))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(8)",oe%RX_SLIT(8))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(9)",oe%RX_SLIT(9))             .and. iOut 
!     iOut= GfGetValue(gf,"RX_SLIT(10)",oe%RX_SLIT(10))           .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(1)",oe%RZ_SLIT(1))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(2)",oe%RZ_SLIT(2))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(3)",oe%RZ_SLIT(3))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(4)",oe%RZ_SLIT(4))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(5)",oe%RZ_SLIT(5))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(6)",oe%RZ_SLIT(6))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(7)",oe%RZ_SLIT(7))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(8)",oe%RZ_SLIT(8))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(9)",oe%RZ_SLIT(9))             .and. iOut 
!     iOut= GfGetValue(gf,"RZ_SLIT(10)",oe%RZ_SLIT(10))           .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(1)",oe%SCR_NUMBER(1))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(2)",oe%SCR_NUMBER(2))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(3)",oe%SCR_NUMBER(3))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(4)",oe%SCR_NUMBER(4))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(5)",oe%SCR_NUMBER(5))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(6)",oe%SCR_NUMBER(6))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(7)",oe%SCR_NUMBER(7))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(8)",oe%SCR_NUMBER(8))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(9)",oe%SCR_NUMBER(9))       .and. iOut 
!     iOut= GfGetValue(gf,"SCR_NUMBER(10)",oe%SCR_NUMBER(10))     .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(1)",oe%SL_DIS(1))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(2)",oe%SL_DIS(2))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(3)",oe%SL_DIS(3))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(4)",oe%SL_DIS(4))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(5)",oe%SL_DIS(5))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(6)",oe%SL_DIS(6))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(7)",oe%SL_DIS(7))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(8)",oe%SL_DIS(8))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(9)",oe%SL_DIS(9))               .and. iOut 
!     iOut= GfGetValue(gf,"SL_DIS(10)",oe%SL_DIS(10))             .and. iOut 
!     iOut= GfGetValue(gf,"THICK(1)",oe%THICK(1))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(2)",oe%THICK(2))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(3)",oe%THICK(3))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(4)",oe%THICK(4))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(5)",oe%THICK(5))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(6)",oe%THICK(6))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(7)",oe%THICK(7))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(8)",oe%THICK(8))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(9)",oe%THICK(9))                 .and. iOut 
!     iOut= GfGetValue(gf,"THICK(10)",oe%THICK(10))               .and. iOut 
!     !! END CODE CREATED AUTOMATICALLY (makecode1.pro)
    !! END CODE CREATED AUTOMATICALLY (makecode1.pro)

    !! START CODE CREATED AUTOMATICALLY (cpp)
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfGetValue(gf, #name, oe%name) .and. iOut
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfGetValue(gf, #name, oe%name) .and. iOut
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \ 
    do i=1,arrdim newline \
       iOut=GfGetValue(gf, myConcat( #name, trim(str(i)) ), oe%name(i)) .and. iOut newline \
    end do
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \ 
    do i=1,arrdim newline \
       iOut=GfGetValue(gf, myConcat( #name, trim(str(i)) ), oe%name(i)) .and. iOut newline \
    end do
#include "ShadowMaskOE.def"
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
  
!
!
!
    
End Module shadow_variables

