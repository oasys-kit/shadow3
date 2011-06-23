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


!
! Operating system dependencies **Please Edit**
!

   !!----
   !!---- OS: Integer variable 1: Windows, 2: Mac, 3: Linux, ....
   !!---- OS_NAME: Character variable, name of the operating system
   !!---- OS_DS: ASCII code of directory separator character
   !!
   
   ! UNCOMMENT THIS BLOCK FOR WINDOWS
   !integer, parameter :: OS= 1    ! Windows
   !character(len=7), parameter :: OS_NAME="Windows"
   !character(len=1), parameter :: OS_DS="\"


   ! UNCOMMENT THIS BLOCK FOR LINUX
   integer(kind=ski), parameter :: OS= 2    ! Linux
   character(len=5), parameter :: OS_NAME="Linux"
   character(len=1), parameter :: OS_DS="/"


   ! UNCOMMENT THIS BLOCK FOR MACOS
   !integer, parameter :: OS= 3    ! MacOS
   !character(len=5), parameter :: OS_NAME="MacOS"
   !character(len=1), parameter :: OS_DS="/"


   ! UNCOMMENT THIS BLOCK FOR SOLARIS
   !integer, parameter :: OS= 4    ! Solaris
   !character(len=7), parameter :: OS_NAME="Solaris"
   !character(len=1), parameter :: OS_DS="/" 


end module shadow_globaldefinitions
