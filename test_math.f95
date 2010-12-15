!----
!----  Program test_math
!----
!----  testing the module: math
!---- 
!----  srio@esrf.eu 2009-09-22
!---- 
!

Program test_math
    !---- Use Modules ----!
    use math

    !---- Variables ----!
    implicit none

    real(kind=kind(1.0d0))  ::   X,Y,Z(15),p
    real(kind=kind(1.0d0)),dimension(3)  ::   v1,v2,v3

    integer(kind=4)                 ::   I
    integer(kind=4)                 ::   ISTAR1=87237661


        write(*,*) '---------------------------------'
        write(*,*) 'ISTAR1: ',ISTAR1
        write(*,*) ' '
        write(*,*) 'Using fortran 95 native random generator: '
        DO I=1,10
          write(*,*) "F: ",WRAN(ISTAR1),ISTAR1
        END DO

!! 
!! test the buil-in generator direct call
!!
        X=0.5
        write(*,*) "X: ",X
        CALL RANDOM_NUMBER(HARVEST=X)
        write(*,*) ' '
        write(*,*) 'Using native F95: '
        DO I=1,10
          CALL RANDOM_NUMBER(Y)
          write(*,*) Y,X
        END DO

        write(*,*) '---------------------------------'


!!
!! TODO test spl_int
!!

!!
!! TODO test gauss
!!

!!
!! vector operations
!!
   print *," vector operations "

   v1 = (/1.0,1.0,1.0/)
   v2 = (/1.0,0.0,0.0/)

   print *," v1= ",v1
   print *," v2= ",v2

   p=27
   call scalar (v1,p,v3)
   print *," 27 v1 = ",v3

   call dot (v1,v2,p)
   print *," v1.v2= ",p

   call cross (v1,v2,v3)
   print *," v1xv2= ",v3

   call norm (v1,v3)
   print *," v1/|v1|= ",v3

   call vector (v1,v2,v3)
   print *," v2-v1= ",v3

   call versor (v1,v2,v3)
   print *," versor from v1 to v2= ",v3

   call proj (v1,v2,v3)
   print *," v1 onto v2= ",v3

   call sum (v1,v2,v3)
   print *," v1 + v2= ",v3

   call atan_2 (0.0D0,1.0D0,p)
   print *," atan_2(0)= ",p


End Program test_math
