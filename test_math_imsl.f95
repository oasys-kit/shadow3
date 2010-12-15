!----
!----  Program test_math_imsl
!----
!----  testing the module: math_imsl
!---- 
!----  srio@esrf.eu 2009-09-22
!---- 
!

Program test_math_imsl
    !---- Use Modules ----!
    use math_imsl

    !---- Variables ----!
    implicit none

    integer(kind=4)                :: i
    real(kind=kind(1d0))           :: a
    real(kind=kind(1d0))           :: out1
    integer(kind=4)                :: ierr


      print *,">> calling mdnris input,output,ierr "
   DO i=1,99
      a = real(i)/100
      call mdnris(a,out1,ierr)
      print *,">>",a,out1,ierr
   END DO

   a=1.1
   print *,">> calling mdnris with a:",a
   call mdnris(a,out1,ierr)
   print *,">>",a,out1,ierr

End Program test_math_imsl
