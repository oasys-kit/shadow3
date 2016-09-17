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
    real(kind=skr), parameter :: pi     = 3.141592653589793238462643D0
    real(kind=skr), parameter :: twopi  = 6.283185307179586467925287D0
    real(kind=skr), parameter :: pihalf = 1.570796326794896619231322D0
    real(kind=skr), parameter :: todeg  =57.295779513082320876798155D0
    real(kind=skr), parameter :: torad  = 0.017453292519943295769237D0


    ! introduced physical constants in globaldefinitions:  srio@esrf.eu 20160527 - NIST CODATA 2014
    real(kind=skr),parameter :: codata_c                     = 2.99792458D8      !speed of light, m/s
    real(kind=skr),parameter :: codata_rm                    = 9.10938356D-31    !electron rest mass  kg
    real(kind=skr),parameter :: codata_e                     = 1.6021766208D-19  !electron charge, C
    real(kind=skr),parameter :: codata_h                     = 6.626070040D-34   !Planck's constant   joules*sec
    real(kind=skr),parameter :: codata_mee                   = 0.5109989461D0      ! electrom Mass equivalent in MeV
    real(kind=skr),parameter :: codata_electric_permittivity = 8.854187817D-12   ! electric constant epsilon0
    ! conversion Angstrom <-> eV for particles with no mass
    ! codata_h*codata_c/codata_e*1d10
    real(kind=skr), parameter :: toangs = 12398.419739640718D0    ! 12398.4192920042

    real(kind=skr), parameter :: tocm =   12398.419739640718D-8 ! 12398.4192920042D-8



    ! for debugging purposes, set this to 1 to get a verbose output
    integer(kind=ski), public :: shadow3_verbose = 0

    ! length units: note that this will be overwritten by source and oe DUMMY variable
    !  using m: user_units_to_cm=100, using cm: user_units_to_cm=1, usning mm: user_units_to_cm=0.1
    !  default is cm
    real(kind=skr), public :: user_units_to_cm = 1.0D0

end module shadow_globaldefinitions
