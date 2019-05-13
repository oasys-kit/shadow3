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

module GFile
!    use iso_varying_string, only : replace
    use stringio, only : u_case
    use shadow_globaldefinitions, only : ski, skr, skc, sklen
    implicit none

    public :: GfFileLoad, GfFileWrite, GfTypePrint, GfIsDefined
    public :: GfTypeAllocate
    public :: GfGetValue, GfSetValue, GfForceSetValue
    public :: GfGetArrValue, GfSetArrValue
    public :: GfConvertStringArrToString, GfConvertStringToStringArr

    private :: GfGetValueString, GfGetValueInteger, GfGetValueReal
    private :: GfSetValueString, GfSetValueInteger, GfSetValueReal
    private :: GfForceSetValueString, GfForceSetValueInteger, GfForceSetValueReal
    private :: GfSetArrayValueString, GfSetArrayValueInteger, GfSetArrayValueReal
    private :: GfGetArrayValueString, GfGetArrayValueInteger, GfGetArrayValueReal

    type, public :: GfType
       character(len=sklen) :: fileName
       ! logical for allocation
       logical            :: alloc1 = .false.
       integer(kind=ski)            :: nLines=0
       integer(kind=ski)            :: nVariables=0
       character(len=sklen), dimension(:), allocatable :: fileLines
       character(len=sklen), dimension(:), allocatable :: variableNames
       character(len=sklen), dimension(:), allocatable :: variableValues
    end type GfType

    interface  GfGetValue
       module procedure GfGetValueString
       module procedure GfGetValueInteger
       module procedure GfGetValueReal
    end interface

    interface  GfSetValue
       module procedure GfSetValueString
       module procedure GfSetValueInteger
       module procedure GfSetValueReal
    end interface

    interface  GfForceSetValue
      module procedure GfForceSetValueString
      module procedure GfForceSetValueInteger
      module procedure GfForceSetValueReal
    end interface

    interface GfGetArrValue
      module procedure GfGetArrayValueString
      module procedure GfGetArrayValueInteger
      module procedure GfGetArrayValueReal
    end interface

    interface GfSetArrValue
      module procedure GfSetArrayValueString
      module procedure GfSetArrayValueInteger
      module procedure GfSetArrayValueReal
    end interface

contains
    function GfConvertStringArrToString(inString) result(outString)
      character(len=1), intent(in) , dimension(:) :: inString
      character(len=SIZE(inString)) :: outString
      integer :: i
      do i = 1, SIZE(inString)
        outString(i:i) = inString(i)
      end do
    end function 

    function GfConvertStringToStringArr(inString) result(outString)
      character(len=*), intent(in) :: inString
      character(len=1), dimension(len(inString)) :: outString
      integer :: i
      do i = 1, LEN(inString)
        outString(i) = inString(i:i)
      end do
    end function

    function GfGetArrayValueString(g1,varname,varval) result(iOut)
      type(GfType), intent(in) :: g1
      character(kind=skc,len=*), intent(in) :: varname
      character(kind=skc,len=1), intent(inout) :: varval(:,:)
      logical :: iOut
      integer :: i
      character(len=5) :: f
      character(kind=skc) :: tempStringArr(sklen)
      character(kind=skc, len=sklen) :: tempString

      iOut = .true.
      do i=1,size(varval, dim=2)
        write(f,'(I2)') i
        iOut = GfGetValueString(g1,TRIM(varname)//"("//trim(adjustl(f))//")",varval(:,i)) .and. iOut
      end do
    end function

    function GfGetArrayValueInteger(g1,varname,varval) result(iOut)
      type(GfType), intent(in) :: g1
      character(kind=skc,len=*), intent(in) :: varname
      integer(kind=ski), intent(inout) :: varval(:)
      logical :: iOut
      integer :: i
      character(len=5) :: f

      iOut = .true.
      do i=1,size(varval)
        write(f,'(I2)') i
        iOut = GfGetValueInteger(g1,TRIM(varname)//"("//trim(adjustl(f))//")",varval(i)) .and. iOut
      end do
    end function

    function GfGetArrayValueReal(g1,varname,varval) result(iOut)
      type(GfType), intent(in) :: g1
      character(kind=skc,len=*), intent(in) :: varname
      real(kind=skr), intent(inout) :: varval(:)
      logical :: iOut
      integer :: i
      character(len=5) :: f

      iOut = .true.
      do i=1,size(varval)
        write(f,'(I2)') i
        iOut = GfGetValueReal(g1,TRIM(varname)//"("//trim(adjustl(f))//")",varval(i)) .and. iOut
      end do
    end function



    function GfSetArrayValueString(g1,varname,varval) result(iOut)
      type(GfType), intent(inout) :: g1
      character(kind=skc,len=*), intent(in) :: varname
      character(kind=skc,len=1), intent(in) :: varval(:,:)
      logical :: iOut
      integer :: i
      character(len=5) :: f

      iOut = .true.
      do i=1,size(varval, dim=2)
        write(f,'(I2)') i
        iOut = GfForceSetValueString(g1,TRIM(varname)//"("//trim(adjustl(f))//")",varval(:,i)) .and. iOut
      end do
    end function

    function GfSetArrayValueInteger(g1,varname,varval) result(iOut)
      type(GfType), intent(inout) :: g1
      character(kind=skc,len=*), intent(in) :: varname
      integer(kind=ski), intent(in) :: varval(:)
      logical :: iOut
      integer :: i
      character(len=5) :: f

      iOut = .true.
      do i=1,size(varval)
        write(f,'(I2)') i
        iOut = GfForceSetValueInteger(g1,TRIM(varname)//"("//trim(adjustl(f))//")",varval(i)) .and. iOut
      end do
    end function

    function GfSetArrayValueReal(g1,varname,varval) result(iOut)
      type(GfType), intent(inout) :: g1
      character(kind=skc,len=*), intent(in) :: varname
      real(kind=skr), intent(in) :: varval(:)
      logical :: iOut
      integer :: i
      character(len=5) :: f

      iOut = .true.
      do i=1,size(varval)
        write(f,'(I2)') i
        iOut = GfForceSetValueReal(g1,TRIM(varname)//"("//trim(adjustl(f))//")",varval(i)) .and. iOut
      end do
    end function





    function GfTypeAllocate (g1, nLines, nVariables) result(iOut)
       type(GfType), intent (inout):: g1
       integer(kind=ski), optional, intent (in)     :: nLines
       integer(kind=ski), optional, intent (in)     :: nVariables
       logical :: iOut

       integer(kind=ski) :: n, nVar

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

       iOut=.true.
    end function GfTypeAllocate



    function GfGetValueString (g1, variableName, variable) result(iOut)
       type(GfType), intent(in)         :: g1
       character(len=*),  intent(in)    :: variableName
       character(len=1),  intent(inout) :: variable(:)
       logical                          :: iOut

       integer(kind=ski)          :: j
       character(len=sklen)       :: var
       character(len=sklen) :: tempString
       integer :: i

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) then 
         read(g1%variableValues(j),fmt="(a)") var
         do i = 1, len_trim(var)
           variable(i) = var(i:i)
         end do
       end if

       if (.not. iOut) print *,"Warning GfGetValueString: "//trim(variableName)

    end function GfGetValueString



    function GfGetValueInteger (g1, variableName, variable) result(iOut)
       type(GfType),      intent(in)    :: g1
       character(len=*),  intent(in)    :: variableName
       integer(kind=ski), intent(inout) :: variable
       logical                          :: iOut

       integer(kind=ski) :: j
       integer(kind=ski) :: var
       character(len=sklen) :: tempString

       iOut = GfIsDefined(g1, variableName, j)

       if (iOut) then 
         read (g1%variableValues(j),fmt=*) var
         variable=var
       end if

       if (.not. iOut) print *,"Warning GfGetValueInteger: "//trim(variableName)

    end function GfGetValueInteger



    function GfGetValueReal (g1, variableName, variable) result(iOut)
       type(GfType),      intent(in)    :: g1
       character(len=*),  intent(in)    :: variableName
       real(kind=skr),    intent(inout) :: variable
       logical                          :: iOut

       integer(kind=ski)          :: j
       real(kind=skr)             :: var
       character(len=sklen) :: tempString

       iOut = GfIsDefined(g1, variableName, j)

       if (iOut) then 
         read (g1%variableValues(j),fmt=*) var
         variable=var
       end if

       if (.not. iOut) print *,"Warning GfGetValueReal: "//trim(variableName)


    end function GfGetValueReal



    function GfSetValueString (g1, variableName, variable) result(iOut)
       type(GfType),      intent(inout) :: g1
       character(len=*),  intent(in)    :: variableName
       character(len=1),  intent(in)    :: variable(:)
       logical                          :: iOut

       integer(kind=ski)                :: j, i

       iOut = GfIsDefined(g1, variableName, j)

       if (iOut) then
         do i = 1, SIZE(variable)
           g1%variableValues(j)(i:i) = variable(i)
         end do
       endif

       if (.not. iOut) print *,"Warning GfSetValueString: "//trim(variableName)


    end function GfSetValueString



    function GfSetValueInteger (g1, variableName, variable) result(iOut)
       type(GfType),      intent(inout)  :: g1
       character(len=*),  intent(in)     :: variableName
       integer(kind=ski), intent(in)     :: variable
       logical                           :: iOut

       integer(kind=ski)                 :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) then
         write(g1%variableValues(j),fmt=*) variable
       endif

       if (.not. iOut) print *,"Warning GfSetValueInteger: "//trim(variableName)


    end function GfSetValueInteger



    function GfSetValueReal (g1, variableName, variable) result(iOut)
       type(GfType),      intent(inout) :: g1
       character(len=*),  intent(in)    :: variableName
       real(kind=skr),    intent(in)    :: variable
       logical                          :: iOut

       integer(kind=ski)                :: j

       iOut = GfIsDefined(g1,variableName,j)

       if (iOut) then
         ! todo:  WARNING: The format may give problems!!!!
         ! write(g1%variableValues(j),fmt="(f20.5)") variable
         ! write(g1%variableValues(j),fmt="(g20.5)") variable
         write(g1%variableValues(j),fmt="(g30.15)") variable
       endif

       if (.not. iOut) print *,"Warning GfSetValueReal: "//trim(variableName)


    end function GfSetValueReal



    function GfForceSetValueString (g1, variableName, variable) result(iout)
        type (gftype),      intent(inout) :: g1
        character(len=*),   intent(in)    :: variableName
        character(len=1),   intent(in)    :: variable(:)
        logical                           :: iOut

        type (gftype)             :: g2
        integer(kind=ski)         :: i
        character(len=sklen) :: tempString 

        iOut = .true.

        tempString = GfConvertStringArrToString(variable)

        if ( .not. gfIsDefined(g1, variableName, i) ) then
                g2 = g1
                g1%nLines = g1%nLines+1
                g1%nVariables = g1%nVariables+1
                iOut = gfTypeAllocate(g1)

                do i=1, g1%nLines-1
                    g1%fileLines(i) = g2%fileLines(i)
                end do
                g1%fileLines(g1%nLines) = variableName//" = "//tempString

                do i=1, g1%nvariables-1
                    g1%variableNames(i) = g2%variableNames(i)
                    g1%variableValues(i) = g2%variableValues(i)
                end do
                g1%variableNames(g1%nVariables) = variableName
                g1%variableValues(g1%nVariables) = "  "
        end if
        iOut = gfSetValue(g1, variableName, variable)


       if (.not. iOut) print *,"Warning GfForceSetValueString: "//trim(variableName)


    end function GfForceSetValueString



    function GfForceSetValueReal (g1, variableName, variable) result(iOut)
        type (gftype), intent(inout) :: g1
        character(len=*), intent(in) :: variableName
        real(kind=skr),   intent(in) :: variable
        logical                      :: iOut

        type (gftype)     :: g2
        integer(kind=ski) :: i

        iOut = .true.

        if ( .not. gfIsDefined(g1, variableName, i) ) then
                g2 = g1
                g1%nLines = g1%nLines+1
                g1%nVariables = g1%nVariables+1
                iOut = gfTypeAllocate(g1)

                do i=1, g1%nLines-1
                   g1%fileLines(i)=g2%filelines(i)
                end do

                write(g1%fileLines(g1%nLines),fmt=*) "variableName = ", variable

                do i=1, g1%nVariables-1
                   g1%variableNames(i) = g2%variableNames(i)
                   g1%variableValues(i) = g2%variableValues(i)
                end do
                g1%variableNames(g1%nVariables) = variableName
                g1%variableValues(g1%nVariables)="  "
        end if
        iOut = gfSetValue(g1, variableName, variable)


       if (.not. iOut) print *,"Warning GfForceSetValueReal: "//trim(variableName)

    end function GfForceSetValueReal



    function GfForceSetValueInteger (g1, variableName, variable) result(iOut)
        type (gftype),      intent(inout) :: g1
        character(len=*),   intent(in)    :: variableName
        integer(kind=ski),  intent(in)    :: variable
        logical                           :: iOut

        type (gftype)                     :: g2
        integer(kind=ski)                 :: i

        iOut = .true.

        if ( .not. gfIsDefined(g1, variableName, i) ) then
            g2 = g1
            g1%nLines=g1%nLines+1
            g1%nVariables=g1%nVariables+1
            iOut = gfTypeAllocate(g1)

            do i=1, g1%nLines-1
               g1%fileLines(i)=g2%fileLines(i)
            end do

            write(g1%FileLines(g1%nLines),fmt=*) "variablename = ", variable

            do i=1, g1%nvariables-1
               g1%variableNames(i)=g2%variableNames(i)
               g1%variableValues(i)=g2%variableValues(i)
            end do
            g1%variableNames(g1%nVariables) = variableName
            g1%variableValues(g1%nVariables)="  "
        end if
        iOut = gfSetValue(g1, variablename, variable)


       if (.not. iOut) print *,"Warning GfForceSetValueInteger: "//trim(variableName)

    end function GfForceSetValueInteger



    function GfFileLoad (g1, fileName) result(iOut)
        type(GfType),     intent (inout)          :: g1
           character(len=*),optional,intent(in)   :: fileName
           logical                                :: iOut

           integer(kind=ski)     :: lun
           integer(kind=ski)     :: i, j
           integer(kind=ski)     :: iErr
           integer(kind=ski)     :: nLines
           integer(kind=ski)     :: nVariables
           integer(kind=ski)     :: iTmp
           character (len=sklen) :: line


         iOut = .false.
         if (present(fileName)) then
              g1%fileName=fileName
         end if

         lun = 33
         open (unit=lun, file=trim(g1%fileName), status="old", action="read", iostat=iErr)

         if (iErr /= 0 ) then
            print *,"GfFileLoad: Error opening the file: "//trim(g1%fileName)
            !stop
            return
         end if

         nLines = 0
         nVariables = 0
         do
            read (unit=lun, fmt="(a)", iostat=iErr) line
            if (iErr /= 0) exit
                nLines = nLines+1
                line = adjustl(line)
            if ( (line(1:1) /= "!") .and. (line(1:1) /= ";")  ) nVariables=nVariables+1
         end do
         rewind(unit=lun)

       g1%nLines = nLines
       g1%nVariables = nVariables
       iOut = GfTypeAllocate(g1)

       if (.not. iOut) return !stop

           j=0
           do i=1, nLines
             read (unit=lun, fmt="(a)", iostat=iErr) line
             if (iErr /= 0) then
               print *,"Error reading gfile: "//trim(g1%fileName)
               close (unit=lun)
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
       close (unit=lun)

       iOut=.true.
    end function GfFileLoad



    function GfFileWrite (g1, fileName) result(iOut)
       type(GfType), intent(in) :: g1
       character(len=*), optional, intent(in)   :: fileName
       logical :: iOut

       integer(kind=ski) :: lun
       integer(kind=ski) :: iErr
       integer(kind=ski) :: i
       character(len=sklen):: line


       iOut = .false.

       lun = 22
       open (unit=lun, file=trim(fileName), status="replace", action="write", iostat=iErr)
       if (iErr /= 0 ) then
          print *,"GfFileWrite: Error opening the file: "//trim(fileName)
          print *,"             Failed writing file."
          !stop
          return
       end if

       do i=1, g1%nVariables
          line = trim(g1%variableNames(i))//" = "//adjustl(g1%variableValues(i))
          write (unit=lun, fmt="(a)", iostat=iErr) trim(line)
          if (iErr /= 0) then
            print *,"Problem writing file: "//trim(fileName)
            exit
          endif
       end do
       close (unit=lun)

       iOut = .true.
    end function GfFileWrite


    !
    !
    !
    function GfTypePrint (g1) result(iOut)
       type(GfType), intent (in)               :: g1
       logical                                 :: iOut

       integer(kind=ski) :: i
       integer(kind=ski) :: nLines
       integer(kind=ski) :: iTmp
!  character (len=sklen)        :: line, line1, line2


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
       do i=1, g1%nVariables
           print *,"      "//trim( g1%variableNames(i) )//"="//trim( g1%variableValues(i) )
       end do
       print *,"===================   "

       iOut=.true.
    end function GfTypePrint



    !
    !
    !
    function GfIsDefined (g1,variableName,variableIndex) Result(iOut)

       !---- Arguments ----!
       type(GfType), intent(in)  :: g1
       character(len=*),  intent(in)  :: variableName
       integer(kind=ski), optional,intent(out) :: variableIndex
       logical :: iOut
       integer(kind=ski) :: i

       iOut = .false.

       if (present(variableIndex)) variableIndex=-1

       do i=1, g1%nVariables
          if ( trim(U_Case(g1%variableNames(i))) == trim(U_Case(variableName)) ) then
             if (present(variableIndex)) variableIndex=i
             iOut=.true.
             exit
           end if
       end do

       return

    end function GfIsDefined

    !
    !
    !

end module GFile

