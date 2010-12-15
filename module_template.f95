!----
!---- MODULE:  blabla
!----
!---- Enter here a few line description line, like:
!---- template for SHADOW module...
!----
!---- Historic provenance, work done, etc.
!----
!----
!---- Example of usage: see test_blabla.f95
!
!

Module BlaBla
    !---- Use Modules ----!

    !---- Variables ----!
    implicit none

    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    public :: blablaF1
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!


    !---- List of private functions ----!
    private :: blablaF2
    !---- List of private subroutines ----!



    !---- Definitions ----!
    ! this is an example of a type 
    !Type, public :: GfType
    !   character(len=512) :: fileName
    !   ! logical for allocation
    !   logical            :: alloc1
    !   integer            :: nLines
    !   integer            :: nVariables
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
    !---- Private Functions ----!
    !


    !
    !
    !
    Function blablaF2 (arg1,arg2) Result(iOut)

       !---- Arguments ----!
       integer(kind=4), intent (in)               :: arg1
       integer(kind=4), optional, intent (in)     :: arg2
       logical :: iOut

       !---- Local variables ----!
       integer(kind=4) :: n, nVar

       !
       ! put code here
       !
       iOut=.true.
    End function blablaF2


    !
    !---- Public Functions ----!
    !

    !
    !
    !
    Function blablaF1 (fileName) Result(iOut)

       !---- Arguments ----!
       character(len=512), intent(in)     :: fileName

       !---- Local variables ----!
       logical :: iOut


       ! 
       ! put code here
       !
       iOut=.true.
    End function blablaF1


End Module blabla

