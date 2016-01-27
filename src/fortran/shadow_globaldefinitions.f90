module shadow_globaldefinitions

!
! global assignments for shadow3
!
    use, intrinsic :: ISO_C_BINDING
    implicit none

!
! Numeric variable types (precision)
!
    integer, parameter, public :: ski = C_INT
    integer, parameter, public :: skr = C_DOUBLE
    integer, parameter, public :: skc = C_CHAR
    integer, parameter, public :: skx = C_DOUBLE_COMPLEX
    integer, parameter, public :: sklen = 1024
!

!!----
!!---- OS: Integer variable 1: Windows, 2: Mac, 3: Linux, 4:Sun
!!---- OS_DS: ASCII code of directory separator character
!!
   
#ifdef _COMPILE4WIN
! defined for both win32 and win64
   integer(kind=ski), parameter :: OS = 1
   character(len=1),  parameter :: OS_DS = "\"
#endif
#ifdef _COMPILE4NIX
   integer(kind=ski), parameter :: OS = 3
   character(len=1),  parameter :: OS_DS = "/"
#endif
#ifdef _COMPILE4MAX
   integer(kind=ski), parameter :: OS = 2
   character(len=1),  parameter :: OS_DS = "/"
#endif
#ifdef _COMPILE4SUN
   integer(kind=ski), parameter :: OS = 4
   character(len=1),  parameter :: OS_DS = "/"
#endif

!
! mathematical and physical constants
!
    real(kind=skr), parameter :: pi     = 3.141592653589793238462643
    real(kind=skr), parameter :: twopi  = 6.283185307179586467925287
    real(kind=skr), parameter :: pihalf = 1.570796326794896619231322
    real(kind=skr), parameter :: todeg  =57.295779513082320876798155
    real(kind=skr), parameter :: torad  = 0.017453292519943295769237

    ! TODO: Change these values with new codata values (see NIST)
    ! real(kind=skr), parameter :: tocm =   1.239852D-4
    ! real(kind=skr), parameter :: toangs = 1.239852D+4

    ! updated srio@esrf.eu 2012/05/24 
    ! for full compatibility with shadow2, use old values
    ! http://physics.nist.gov/cgi-bin/cuu/Value?h|search_for=plank
    ! h    =6.62606957e-34
    ! http://physics.nist.gov/cgi-bin/cuu/Value?c|search_for=light
    ! c    =2.99792458e8  
    ! http://physics.nist.gov/cgi-bin/cuu/Value?e|search_for=electron
    ! ec   =1.602176565e-19
    ! C_M_A    = 1.0e+10  m -> Angstrom
    ! 
    ! HC = H*C/EC*C_M_A ; conversion Angstrom <-> eV for particles with no mass
    real(kind=skr), parameter :: tocm =   12398.4192920042D-8
    real(kind=skr), parameter :: toangs = 12398.4192920042

    ! for debugging purposes, set this to 1 to get a verbose output
    integer(kind=ski), public :: shadow3_verbose = 0

    ! length units: note that this will be overwritten by source and oe DUMMY variable
    !  using m: user_units_to_cm=100, using cm: user_units_to_cm=1, usning mm: user_units_to_cm=0.1
    !  default is cm
    real(kind=skr), public :: user_units_to_cm = 1.0

end module shadow_globaldefinitions
