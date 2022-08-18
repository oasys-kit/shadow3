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

#define STR(X) "X"

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
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ftype(kind=fkind) :: name
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ftype(kind=fkind,len=1), dimension(length) :: name
#include "shadow_source.def"
    end type poolSource
    
    
    type, public, bind(C) :: poolOE
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ftype(kind=fkind) :: name
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ftype(kind=fkind,len=1), dimension(length) :: name
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) ftype(kind=fkind), dimension(arrdim) :: name
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) ftype(kind=fkind, len=1), dimension(length,arrdim) :: name
#include "shadow_oe.def"
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
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfForceSetValue(gf, STR(name), src%name) .and. iOut
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfForceSetValue(gf, STR(name), src%name) .and. iOut
#include "shadow_source.def"
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

#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfForceSetValue(gf, STR(name), oe%name) .and. iOut
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfForceSetValue(gf, STR(name), oe%name) .and. iOut
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) iOut=GfSetArrValue(gf, STR(name), oe%name) .and. iOut
!    do i=1, arrdim \
!       iOut=GfForceSetValue(gf, STR(name)//trim(str(i)), oe%name(i)) .and. iOut \
!    end do
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) iOut=GfSetArrValue(gf, STR(name),oe%name) .and. iOut
!    do i=1, arrdim \
!       iOut=GfForceSetValue(gf, STR(name)//trim(str(i)), oe%name(i)) .and. iOut \
!    end do
#include "shadow_oe.def"
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
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfGetValue(gf,STR(name),src%name)  .and. iOut
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfGetValue(gf,STR(name),src%name)  .and. iOut
#include "shadow_source.def"
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
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) iOut=GfGetValue(gf, STR(name), oe%name) .and. iOut
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) iOut=GfGetValue(gf, STR(name), oe%name) .and. iOut
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) iOut=GfGetArrValue(gf, STR(name), oe%name) .and. iOut
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) iOut=GfGetArrValue(gf, STR(name), oe%name) .and. iOut
#include "shadow_oe.def"
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
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) src%name=defvalue
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) src%name=defvalue
#include "shadow_source.def"
  end subroutine PoolSourceDefault

  subroutine PoolOEDefault(oe)
    type (poolOE), intent(inout) :: oe
    integer(kind=ski) :: i, j
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) oe%name=defvalue
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) oe%name=defvalue
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) oe%name=defvalue
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) oe%name=defvalue
#include "shadow_oe.def" 
  end subroutine PoolOEDefault


!
!
!
    
End Module shadow_variables

