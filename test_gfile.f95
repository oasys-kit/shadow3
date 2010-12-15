!----
!----  Program test_gfile
!----
!----  testing the module: gfile 
!---- 
!----  srio@esrf.eu 2009-06-24
!---- 
!
Program test_gfile
    !---- Use Modules ----!
    use GFile

    !---- Variables ----!
    implicit none

    Type(GfType)           :: g1
    integer(kind=4)                :: narg,i,j,k
    integer(kind=4)                :: lun=1
    character(len=512)     :: line,filcod,fileName
    Logical                :: arggiven=.false., esta, iOut

    ! 
    ! these are the "user" variables to be read from file via the gfile
    !
    character(len=512),dimension(18)     :: myVariables=  (/ &
       "a             ", &
       "b             ", &
       "c             ", &
       "file_koma_ca  ", &
       "f_koma_bounce ", &
       "x_rip_amp     ", &
       "x_rip_wav     ", &
       "x_phase       ", &
       "scr_number(1) ", &
       "scr_number(2) ", &
       "scr_number(3) ", &
       "scr_number(4) ", &
       "scr_number(5) ", &
       "scr_number(6) ", &
       "scr_number(7) ", &
       "scr_number(8) ", &
       "scr_number(9) ", &
       "scr_number(10)" /)
    real(kind=kind(1.0d0)) ::a,b,c,x_rip_amp,x_rip_wav,x_phase
    integer(kind=4)                ::f_koma_bounce,FDISTR
    character(len=512)     ::file_koma_ca
    integer(kind=4),dimension(10)  ::scr_number

!
! get the file name i) from command line or ii) inquire about it
!


     !---- Arguments on the command line ----!
     narg=command_argument_count()

     if (narg > 0) then
        call get_command_argument(1,filcod)
        arggiven=.true.
     end if


     if (.not. arggiven) then
        write(unit=*,fmt="(a)", advance='no') " => gfile: "
        read(unit=*,fmt="(a)") filcod
        if(len_trim(filcod) == 0) stop
     end if

     inquire(file=trim(filcod),exist=esta)

     if ( .not. esta) then
       print *, "Error reading file: "//trim(filcod)
       stop
     end if

!
! loads the contents of the file ito the GfType
!
     print *," "
     print *, "Loading file: "//trim(filcod)
     iOut = GfFileLoad(g1,trim(filcod))
     print *, "Result (Loading file): ",iOut
     if (.not. iOut) stop


!
! print all stored variables on screen
!
     iOut = GfTypePrint(g1)
     if (.not. iOut) stop

     print *," "
     print *,"Calling GfIsDefined  Active variables..."

     do i=1,size(myVariables)
       if (GfIsDefined(g1,myVariables(i),j)) then 
          print *,trim(myVariables(i))//"="//trim(g1%variableValues(j))
       end if
     end do

!
! load some variables into this main program
!
     print *," "
     print *,"Calling GfIsDefined  Loading some variables..."


!
! direct reading a variable (not used, use better GfGetValue() )
!
!     ! reads a string variable 
!     if (GfIsDefined(g1,"file_koma_ca",j)) then 
!       ! warning: fmt=* is not valid here (it reads the first word only)
!       read(g1%variableValues(j),fmt="(a)") file_koma_ca
!       print *,"Loaded file_koma_ca: ",trim(file_koma_ca)
!     endif 
!
!     ! reads a real variable 


!     if (GfIsDefined(g1,"x_rip_amp",j)) then 
!       read(g1%variableValues(j),fmt=*) x_rip_amp
!       print *,"Loaded x_rip_amp: ",x_rip_amp
!     endif 

     ! reads a string variable 
     iOut = GfGetValue(g1,"file_koma_ca",file_koma_ca) 
     if (iOut) print *,"Loaded file_koma_ca: ",trim(file_koma_ca)


     ! reads a integer variable 
     iOut = GfGetValue(g1,"f_koma_bounce",f_koma_bounce) 
     if (iOut) print *,"Loaded f_koma_bounce: ",f_koma_bounce


     ! reads a real variable 
     print *,">> FDISTR defined? : ",GfIsDefined(g1,"FDISTR") 
     iOut = GfGetValue(g1,"FDISTR",FDISTR) 
     print *,">> FDISTR iOut? : ",iOut
     print *,">> FDISTR iOut? : ",FDISTR
     if (iOut) print *,"Loaded FDISTR: ",FDISTR

     iOut = GfGetValue(g1,"x_rip_amp",x_rip_amp) 
     if (iOut) print *,"Loaded x_rip_amp: ",x_rip_amp

     iOut = GfGetValue(g1,"a",a) 
     if (iOut) print *,"Loaded a: ",a

     iOut = GfGetValue(g1,"b",b) 
     if (iOut) print *,"Loaded b: ",b

     iOut = GfGetValue(g1,"scr_number(1)",scr_number(1)) 
     if (iOut) print *,"Loaded scr_number(1): ",scr_number(1)

     iOut = GfGetValue(g1,"scr_number(10)",scr_number(1)) 
     if (iOut) print *,"Loaded scr_number(10): ",scr_number(1)

!
! redefining some variables and srtoring them in the GfType
!
     iOut = GfSetValue(g1,"file_koma_ca","veintisiete")
     if (iOut) print *,"modified string (file_koma_ca=veintisiete)"
     iOut = GfGetValue(g1,"file_koma_ca",file_koma_ca)
     if (iOut) print *,"Re-Loaded string file_koma_ca: ",trim(file_koma_ca)

     x_rip_amp=33.0
     iOut = GfSetValue(g1,"x_rip_amp",x_rip_amp)
     if (iOut) print *,"modified real (x_rip_amp=33.0)"
     iOut = GfGetValue(g1,"x_rip_amp",x_rip_amp)
     if (iOut) print *,"Re-Loaded real x_rip_amp: ",x_rip_amp

     f_koma_bounce = 1234567890
     iOut = GfSetValue(g1,"f_koma_bounce",f_koma_bounce)
     if (iOut) print *,"modified integer (f_koma_bounce=...)"
     iOut = GfGetValue(g1,"f_koma_bounce",f_koma_bounce)
     if (iOut) print *,"Re-Loaded integer f_koma_bounce: ",f_koma_bounce

     c = 83459.123
     write(*,fmt="(a,f30.10)") " New c: ",c
     iOut = GfSetValue(g1,"c",c)
     if (iOut) print *,"modified integer (c=83459.123)"
     iOut = GfGetValue(g1,"c",c)
     if (iOut) write(*,*) "Re-Loaded integer c: ",c
     if (iOut) write(*,fmt="(a,f30.10)") "Re-Loaded integer c: ",c
!
! dump the GfType into a new file
!

     fileName="tmp.00"
     iOut = GfFileWrite(g1,"tmp.00")

     print *,"Done test_gfile"

End Program test_gfile



