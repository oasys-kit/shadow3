module shadow_kind

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


!
! Operating system dependencies **Please Edit**
!

   !!----
   !!---- OS: Integer variable 1: Windows, 2: Mac, 3: Linux, ....
   !!---- OS_NAME: Character variable, name of the operating system
   !!---- OS_DS: ASCII code of directory separator character
   !!
   !integer, parameter :: OS= 1    ! Windows
   integer(kind=ski), parameter :: OS= 2    ! Linux
   !integer, parameter :: OS= 3    ! MacOS
   !integer, parameter :: OS= 4    ! Solaris
   !
   !character(len=7), parameter :: OS_NAME="Windows"
   character(len=5), parameter :: OS_NAME="Linux"
   !character(len=5), parameter :: OS_NAME="MacOS"
   !character(len=7), parameter :: OS_NAME="Solaris"
   !
   !character(len=1), parameter :: OS_DS="\"
   character(len=1), parameter :: OS_DS="/"
   !character(len=1), parameter :: OS_DS="/"
   !character(len=1), parameter :: OS_DS="/" 

end module shadow_kind
