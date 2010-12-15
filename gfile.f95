!----
!---- MODULE:  GFILE
!----
!---- i/o a file with a variable list (called gfile in Shadow)
!----
!---- Completely new f95 code (srio@esrf.eu 2009-06-24) 
!----
!---- Example of gfile:
!----
!---- ; this is a comment
!---- a = 1
!---- ! this is a comment
!---- b = 3.0
!---- c3 = hola que tal
!
!    Example of usage: see test_gfile.f95
!
!

Module GFile
    !---- Use Modules ----!

    !---- Variables ----!
    implicit none

    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    public :: GfFileLoad, GfFileWrite, GfTypePrint, GfIsDefined
    !---- List of public overloaded functions ----!
    public :: GfGetValue, GfSetValue
    !---- List of public subroutines ----!


    !---- List of private functions ----!
    private :: GfGetValueString, GfGetValueInteger, GfGetValueReal
    private :: GfSetValueString, GfSetValueInteger, GfSetValueReal
    private :: GfTypeAllocate
    private :: U_Case
    !---- List of private subroutines ----!



    !---- Definitions ----!
    Type, public :: GfType
       character(len=512) :: fileName
       ! logical for allocation
       logical            :: alloc1
       integer(kind=4)            :: nLines
       integer(kind=4)            :: nVariables
       character(len=512), dimension(:), allocatable :: fileLines
       character(len=512), dimension(:), allocatable :: variableNames
       character(len=512), dimension(:), allocatable :: variableValues
    End Type GfType


    !---- Interfaces ----!
    Interface  GfGetValue
       Module Procedure GfGetValueString
       Module Procedure GfGetValueInteger
       Module Procedure GfGetValueReal
    End Interface

    Interface  GfSetValue
       Module Procedure GfSetValueString
       Module Procedure GfSetValueInteger
       Module Procedure GfSetValueReal
    End Interface


  Contains
    !
    !---- Private Functions ----!
    !


    !
    !
    !
    Function GfTypeAllocate (g1,nLines,nVariables) Result(iOut)

       !---- Arguments ----!
       type(GfType), intent (in out) :: g1
       integer(kind=4), optional, intent (in)     :: nLines, nVariables
       logical :: iOut

       !---- Local variables ----!
       integer(kind=4) :: n, nVar

       iOut = .false.
       if (present(nLines)) then
          g1%nLines=nLines
          n=nLines
       else
          n=g1%nLines
       end if

       if (present(nVariables)) then
          g1%nVariables=nVariables
          nVar=nVariables
       else
          nVar=g1%nvariables
       end if



       if (allocated(g1%fileLines) ) deallocate(g1%fileLines)
       allocate(g1%fileLines(n))

       if (allocated(g1%variableNames) ) deallocate(g1%variableNames)
       allocate(g1%variableNames(nVar))

       if (allocated(g1%variableValues) ) deallocate(g1%variableValues)
       allocate(g1%variableValues(nVar))

       !print *,">> Allocated arrays in GfType, nLines,nVariables: ",n,nVar

       iOut=.true.
    End function GfTypeAllocate 


    !
    !
    !
    Function GfGetValueString (g1,variableName,variable) Result(iOut)
       !---- Arguments ----!
       type(GfType), intent(in)           :: g1
       character(len=*),  intent(in)      :: variableName
       character(len=*),  intent(in out)  :: variable


       !---- Local variables ----!
       logical                            :: iOut
       integer(kind=4)                            :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) read(g1%variableValues(j),fmt="(a)") variable

    End Function GfGetValueString

    !
    !
    !
    Function GfGetValueInteger (g1,variableName,variable) Result(iOut)
       !---- Arguments ----!
       type(GfType),      intent(in)      :: g1
       character(len=*),  intent(in)      :: variableName
       integer(kind=4),           intent(in out)  :: variable


       !---- Local variables ----!
       logical                            :: iOut
       integer(kind=4)                            :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) read(g1%variableValues(j),fmt=*) variable

    End Function GfGetValueInteger

    !
    !
    !
    Function GfGetValueReal (g1,variableName,variable) Result(iOut)
       !---- Arguments ----!
       type(GfType),             intent(in)      :: g1
       character(len=*),         intent(in)      :: variableName
       real(kind=kind(1.0d0)),   intent(in out)  :: variable


       !---- Local variables ----!
       logical                            :: iOut
       integer(kind=4)                            :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) read(g1%variableValues(j),fmt=*) variable

    End Function GfGetValueReal


    !
    !
    !
    Function GfSetValueString (g1,variableName,variable) Result(iOut)
       !---- Arguments ----!
       type(GfType), intent(in out)       :: g1
       character(len=*),  intent(in)      :: variableName
       character(len=*),  intent(in)      :: variable


       !---- Local variables ----!
       logical                            :: iOut
       integer(kind=4)                            :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) then 
         g1%variableValues(j) = variable
       endif 

    End Function GfSetValueString

    !
    !
    !
    Function GfSetValueInteger (g1,variableName,variable) Result(iOut)
       !---- Arguments ----!
       type(GfType),      intent(in out)  :: g1
       character(len=*),  intent(in)      :: variableName
       integer(kind=4),           intent(in)      :: variable


       !---- Local variables ----!
       logical                            :: iOut
       integer(kind=4)                            :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) then 
         write(g1%variableValues(j),fmt=*) variable
       endif 

    End Function GfSetValueInteger

    !
    !
    !
    Function GfSetValueReal (g1,variableName,variable) Result(iOut)
       !---- Arguments ----!
       type(GfType),           intent(in out)  :: g1
       character(len=*),        intent(in)     :: variableName
       real(kind=kind(1.0d0)), intent(in)      :: variable


       !---- Local variables ----!
       logical                            :: iOut
       integer(kind=4)                            :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) then 
         write(g1%variableValues(j),fmt="(f20.5)") variable
       endif 

    End Function GfSetValueReal





    !
    !---- Public Functions ----!
    !

    !
    !
    !
    Function GfFileLoad (g1,fileName) Result(iOut)

       !---- Arguments ----!
       type(GfType), intent (in out) :: g1
       character(len=512), optional, intent(in)     :: fileName

       !---- Local variables ----!
       logical :: iOut
       integer(kind=4) :: lun, iErr, i, j, nLines, nVariables, itmp
       character (len=512)                          :: line


       iOut = .false.
       if (present(fileName)) then
          g1%fileName=fileName
       end if

       lun = 33
       ! print *,"Opening file **"//trim(g1%fileName)//"**"
       !open(unit=lun,file=trim(g1%fileName),status="old",action="read", &
       open(unit=lun,file=trim(g1%fileName),status="old",action="read", &
           iostat=iErr)
       ! print *,">> iErr: ",iErr
       if (iErr /= 0 ) then
          print *," Error opening the file: "//trim(g1%fileName)
          !print *," Error opening the file: "//trim(fileName)
          return
       end if

       !
       ! get the number of lines and number of variables
       !
       nLines = 0
       nVariables = 0
       do
          read(unit=lun,fmt="(a)",iostat=iErr) line
          if (iErr /= 0) exit
          nLines = nLines+1
          line = adjustl(line)
          if ( (line(1:1) /= "!") .and. (line(1:1) /= ";")  ) nVariables=nVariables+1
          
       end do
       rewind(unit=lun)

     ! print *, ">> Number of lines in file: ",nLines

     ! print *, ">> Allocating type... "
     g1%nLines=nLines
     g1%nVariables=nVariables
     iOut = GfTypeAllocate(g1)
     ! print *, ">> Result (Allocating type...) ",iOut
     if (.not. iOut) stop


       j=0
       do i=1,nLines
         read(unit=lun,fmt="(a)",IOSTAT=iErr) line
         if (iErr /= 0) then
           print *,"Error reading gfile: "//trim(g1%fileName)
           close(unit=lun)
           exit
         end if
         g1%fileLines(i)=line
         line = adjustl(line)
         if ( (line(1:1) /= "!") .and. (line(1:1) /= ";")  ) then ! allocate variable
           j=j+1
           itmp = scan(line,"=")
           g1%variableNames(j) = line(1:itmp-1)
           g1%variableValues(j) = adjustl(line(itmp+1:))
         endif
       end do
       close(unit=lun)

       iOut=.true.
    End function GfFileLoad

    !
    !
    !
    Function GfFileWrite (g1,fileName) Result(iOut)

       !---- Arguments ----!
       type(GfType), intent (in out)                :: g1
       character(len=*), optional, intent(in)     :: fileName

       !---- Local variables ----!
       logical :: iOut
       integer(kind=4) :: lun, iErr, i 
       character (len=512)                          :: line


       iOut = .false.
       if (present(fileName)) then
          g1%fileName=fileName
       end if

       ! print *,"Opening file **"//trim(g1%fileName)//"**"
       lun = 22
       open(unit=lun,file=trim(g1%fileName),status="replace",action="write", &
           iostat=iErr)
       if (iErr /= 0 ) then
          print *,"Error opening the file: "//trim(g1%fileName)
          return
       end if

       !
       ! get the number of lines and number of variables
       !
       do i=1,g1%nvariables
          line = trim(g1%variableNames(i))//" = "//adjustl(g1%variableValues(i))
          write(unit=lun,fmt="(a)",iostat=iErr) trim(line)
          if (iErr /= 0) then 
            print *,"Problem writing file: "//trim(g1%fileName)
            exit
          endif
       end do
       close(unit=lun)

       print *,"File written to disk: "//trim(g1%fileName)
       iOut=.true.
    End function GfFileWrite


    !
    !
    !
    Function GfTypePrint (g1) Result(iOut)

       !---- Arguments ----!
       type(GfType), intent (in) :: g1

       !---- Local variables ----!
       logical :: iOut
       integer(kind=4) :: i, nLines
   
       integer(kind=4) :: itmp
       character (len=512)                          :: line,line1,line2


       iOut = .false.

       print *,"===================   file: "//trim(g1%fileName)
!       do i=1,g1%nLines
!           print *,"**"//trim( g1%fileLines(i) )//"**"
!           !line = adjustl(g1%fileLines(i))
!           !itmp = scan(line,"=")
!           !line1 = line(1:itmp-1)
!           !line2 = adjustl(line(itmp+1:))
!           !print *," ",itmp,"**"//trim(line1)//"**"//trim(line2)//"**"
!       end do
       print *,"===================   variables:  "
       do i=1,g1%nVariables
           print *,"      "//trim( g1%variableNames(i) )//"="//trim( g1%variableValues(i) )
       end do
       print *,"===================   "

       iOut=.true.
    End function GfTypePrint



    !
    !
    !
    Function GfIsDefined (g1,variableName,variableIndex) Result(iOut)

       !---- Arguments ----!
       type(GfType), intent(in)  :: g1
       character(len=*),  intent(in)  :: variableName
       integer(kind=4), optional, intent(out) :: variableIndex

       !---- Local variables ----!
       logical :: iOut
       integer(kind=4) :: i
   

       iOut = .false.

       if (present(variableIndex)) variableIndex=-1

       do i=1,g1%nVariables
           if ( trim(U_Case(g1%variableNames(i))) == trim(U_Case(variableName)) ) then 
             if (present(variableIndex)) variableIndex=i
             iOut=.true.
             exit
           end if
       end do

       return

    End function GfIsDefined

    !
    !
    !

    !! copied from crysFML lib (J. R. Carvajal et al.)

    !!----
    !!---- Character Function U_Case(Text) Result (Mtext)
    !!----    character (len=*), intent(in) :: text   !  In -> String:"Input Line"
    !!----    character (len=len(text))     :: mtext  ! Out -> String:"INPUT LINE"
    !!----
    !!----    Conversion to upper case, text is not modified
    !!----
    !!---- Update: February - 2005
    !!
    Function U_Case(Text) Result (Mtext)
       !---- Argument ----!
       character (len=*), intent(in) :: text
       character (len=len(text))     :: mtext

       !---- Local variables ----!
       integer, parameter :: inc = ICHAR("A") - ICHAR("a")
       integer            :: leng, pos

       mtext=text
       leng=len_trim(mtext)
       do pos=1,leng
          if (mtext(pos:pos) >= "a" .and. mtext(pos:pos) <= "z")           &
              mtext(pos:pos) = CHAR ( ICHAR(mtext(pos:pos)) + inc )
       end do

       return
    End Function U_Case

End Module GFile

