!----
!----  Program test_stringIO
!----
!----  testing the module: stringIO 
!---- 
!----  srio@esrf.eu 2009-09-21
!---- 
!

Program test_stringio
    !---- Use Modules ----!
    use stringio

    !---- Variables ----!
    implicit none

    integer(kind=4)                :: i,j
    real(kind=kind(1d0))   :: a
    character(len=512)     :: line,line2


!----     clscreen.F
   print *,">> Calling clscreen..."
   call clscreen()

!----     ** these are for entering variables from prompt
!----     F: iyes.F 
!----     F: rnumber.F 
!----     F: irint.F 
!----     F: rstring.F 

   i = iyes("Enter yes or no (Y*,y*,1* is YES; N*,n*,any number except 1* is NO): ")
   print *,">> I got: ",i

   a = rnumber("Enter a real number: ")
   print *,">> I got: ",a
  
   j = irint("Enter an integer number: ")
   print *,">> I got: ",j
  
   line = rstring("Enter a string of characters: ")
!   print *,">> I got: ","**"//line//"**"

!----     F: iblank.F 
   print *,">> I got(withouth trailing blanks -- using iblank) ",line(1:iblank(line))
! and this one with f95 intrinsic fuunction...
   print *,">> I got(withouth trailing blanks -- using trim) ",trim(line)
  
!----     ** these are utils for strings
!----     F: fname.F
   print *,">> Calling fname to get OPTAX.05 ..."
   call fname(line2,'OPTAX',5,2)
   print *,">> Testing fname. I got: ",trim(line2)


!----     S: despace.F
   print *,">> Calling despace with '     hola   que   tal' ..."
   call despace("     hola   que   tal   ",line2,j)
   print *,">> Testing despace. I got: ","**"//line2(1:j)//"**"


!----     ** these are for error io:
!----     S: mssg.F 
!----     S: leave.F 

   print *,">> Calling mssg: "
   call mssg("test_stringIO","Hi",27)

   print *,">> Calling leave: "
   call leave("test_stringIO","Bye",27)


End Program test_stringio



