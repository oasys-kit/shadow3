!
!---- MODULE:  shadow_roughness
!----
!---- Module for roughness calculations, implementing a random sampling
!---- using a 2d tabulated function (power spectral density)
!
!---- Contains: 
!----
!---- Example of usage: 
!----
!----


Module shadow_roughness
!---- Use Modules ----!

use, intrinsic :: ISO_C_BINDING
use stringio
use gfile
use shadow_math
use shadow_globaldefinitions 
!use shadow_beamio
!use shadow_variables

implicit none
  

!---- Variables ----!
      !remover original common
      !common /spgs_info/ nx,ny,xoffset,yoffset,c1,c2,p1,p2,s0,s1,s2
      integer(kind=ski),parameter        :: nMaxRough=150

  !---- Everything FROM HERE is private unless explicitly made public ----!
      private
  
  
  !---- List of public functions ----!
  ! public :: rwname
  !---- List of public overloaded functions ----!
  !---- List of public subroutines ----!
      public :: pspect, pspect_test

  !---- List of private functions ----!
  !---- List of private subroutines ----!
  !private :: get_variables00, put_variables00, source_bound
  
  
  
!---- Definitions ----!
  ! this is an example of a type 
  !Type, public :: GfType
  !   character(len=sklen) :: fileName
  !   ! logical for allocation
  !   logical            :: alloc1
  !   integer(kind=ski)            :: nLines
  !   integer(kind=ski)            :: nVariables
  !   character(len=sklen), dimension(:), allocatable :: fileLines
  !   character(len=sklen), dimension(:), allocatable :: variableNames
  !   character(len=sklen), dimension(:), allocatable :: variableValues
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
  !---- Public Routines ----!
  !
  

Subroutine pspect (x1,x2,ierr,ipsflag,fileRoughness)

  implicit none
  integer(kind=ski),        intent(in)   :: ipsflag
  character(len=sklen),     intent(in)   :: fileRoughness
  integer(kind=ski),        intent(out)  :: ierr
  real(kind=skr),           intent(out)  :: x1,x2
  integer(kind=ski)        :: iseed=0


  !ex-common block
  real(kind=skr),dimension(nMaxRough)            :: c1,p1,s1
  real(kind=skr),dimension(nMaxRough,nMaxRough)  :: c2,p2,s2
  real(kind=skr)                                 :: s0, xoffset,yoffset
  integer(kind=ski)                              :: nx,ny

  save c1,p1,s1,c2,p2,s2,s0, xoffset,yoffset,nx,ny

!C
!C If IPSFLAG < 0, call from SETSOUR -- reading files etc.
!C
       IF (IPSFLAG.LT.0) THEN
            print*,'Reading file: '//trim(fileRoughness)
            !CALL SPGS_INIT (fileRoughness)
            CALL SPGS_INIT (fileRoughness, &
          c1,p1,s1,c2,p2,s2,s0, xoffset,yoffset,nx,ny)
            print*,'Done reading file: '//trim(fileRoughness)
            RETURN
      ENDIF
!C
!C Normal entry; Generate two random numbers
!C
      X1 = WRAN(ISEED)
      X2 = WRAN(ISEED)
!C
!C Call the stochastic process generator
!C
!srio: added variables in old common block: CALL GENERATE (X1,X2,IERR)
       CALL GENERATE_SPGS (X1,X2,IERR, &
          c1,p1,s1,c2,p2,s2,s0, xoffset,yoffset,nx,ny)
!C       X1 = 105
!C       X2 = 10523
      IF (IERR.NE.0) CALL LEAVE ('PSPECT','Error on return from GENERATE',IERR)
!C
!C X1, X2 now contain fx, fy.  Change to kx, ky
!C
      X1 = TWOPI*X1
      X2 = TWOPI*X2
      RETURN
End Subroutine pspect

  !
  !---- Private Routines ----!
  !

!C+++
!C
!C      Generate.
!C
!C      Take a 2 dimentional vector of normalized real 
!C      variables, convert to a vector in 2-d with the distribution
!C      corresponding to the CDF given in the parameter list
!C
!C---
Subroutine generate (Nmax, Kmax, C1, C2, P1, P2, S0, S1, S2, Vin, Vout)

implicit none

!todo: add intent
real(kind=skr), dimension(nMaxRough,nMaxRough) :: C2, S2, P2
real(kind=skr), dimension(nMaxRough)           :: C2Temp, P2Temp, S2Temp
real(kind=skr), dimension(nMaxRough)           :: C1, S1, p1 !, p2
real(kind=skr), dimension(2)      :: Vin, Vout
real(kind=skr)                    :: S0
integer(kind=ski)                 :: Nmax, Kmax, NLess, Kless

CALL Locate1(Nmax, C1, P1, S0, S1, Vin(1) , Vout(1), Nless) 
CALL Interpolate1(Kmax, Vout, Nless, C1, C2, P1, P2, S1, S2, &
         C2Temp, P2Temp, S2Temp )
CALL Locate1(Kmax, C2Temp, P2Temp, S2Temp(Kmax), S2Temp, &
         Vin(2), Vout(2), Kless)
RETURN
End Subroutine generate

!C+++
!C      With revised algorithm for interpolate and Getcoords.
!C
!C
!C---
!C        PROGRAM MAIN
subroutine pspect_test
      implicit none

      real(kind=skr),dimension(2) :: vin, vout
      real(kind=skr) :: x,y
      integer(kind=ski)    ::  i, ierr, iseed=0
      character(len=sklen) :: infilename


      !ex-common block
      real(kind=skr),dimension(nMaxRough)            :: c1,p1,s1
      real(kind=skr),dimension(nMaxRough,nMaxRough)  :: c2,p2,s2
      real(kind=skr)                                 :: s0, xoffset,yoffset
      integer(kind=ski)                              :: nx,ny

      infilename = 'jnt1.dat'
      !call spgs_init('jnt1.dat')
      call spgs_init(infilename, &
          c1,p1,s1,c2,p2,s2,s0, xoffset,yoffset,nx,ny)

      open(file = 'pspect.dat',status = 'unknown',unit = 10)
      do i = 1,10000
          x = wran(iseed)
          y = wran(iseed)
          vin(1) = x
          vin(2) = y
          !call generate(dx,dy,ierr)
          call generate (nx, nx, C1, C2, P1, P2, S0, S1, S2, Vin, Vout)
          write(10,*) vout(1),vout(2)
      end do
      close(unit = 10)
      print *,'Sampled points from 2D PSD in file :'//trim(infilename)//'. Output file: pspect.dat'
end subroutine pspect_test


!C+++
!C
!C     spgs_init - reads info from a file and initializes the stochastic
!C     process generator with it.
!C
!C---
subroutine spgs_init(infilename, &
          c1,p1,s1,c2,p2,s2,s0, xoffset,yoffset,nx,ny)

      implicit none
      character(len=sklen), intent(in) :: infilename

      real(kind=skr),dimension(nMaxRough),intent(out)            :: c1,p1,s1
      real(kind=skr),dimension(nMaxRough,nMaxRough),intent(out)  :: c2,p2,s2
      real(kind=skr),intent(out)         :: s0, xoffset,yoffset
      integer(kind=ski),intent(out)      :: nx,ny
      !common /spgs_info/ nx,ny,xoffset,yoffset,c1,c2,p1,p2,s0,s1,s2



      integer(kind=ski) ::  x,y,izero=0
      real(kind=skr)    ::  xstart,ystart, xstep, ystep

      open(unit=10,file = infilename,status = 'unknown')
      read(10,*) nx
      read(10,*) xstart
      read(10,*) xstep
      read(10,*) ny
      if ((nx.gt.nMaxRough) .or. (ny.gt.nMaxRough)) then
         print *,'SPGS_INIT: using points in Ppwer Spectral Density: ',nx,ny
         print *,'SPGS_INIT: Maximum number of points :',nMaxRough
         izero = 0
         CALL LEAVE ('SPGS_INIT','Change program inputs.',izero)
      endif
      read(10,*) ystart
      read(10,*) ystep
      xoffset = xstart
      yoffset = ystart
      do y = 1,ny
          do x = 1,nx
            ! reads probability
            read(10,*) p2(y,x)
            ! computes coordinates
            c1(x) = (x-1) * xstep
            c2(y,x) = (y-1) * ystep 
          end do
      end do
      close(unit= 10)

      ! calculate S, the CDF (cumulated distribution function)
      call accumulate(nx,ny,c1,c2,p1,p2,s0,s1,s2)
return
End subroutine spgs_init

!C+++
!C
!C       A two dimentional, linear integrating, parabolic interpolating
!C       version of the stochastic process generator.
!C
!C      C's are coordinate arrays, P's are probability, S's are
!C      CDF's
!C
!C+++
Subroutine accumulate ( nx, ny, C1, C2, P1, P2, S0, S1, S2 )

implicit none

real(kind=skr),dimension(nMaxRough,nMaxRough)  :: c2,p2,s2
real(kind=skr),dimension(nMaxRough)      :: c1,p1,s1
real(kind=skr)                     :: s0
integer(kind=ski) ::  X, Y, nx, ny
DO X = 1, nx
    DO Y = 1,ny
       CALL Integrate2(X,Y,C2,P2,S2)
    END DO
    P1(X) = S2(ny, X)
END DO
DO X = 1,nx
    CALL Integrate1(X,C1,P1,S1)
END DO
S0 = S1(nx)
RETURN
End Subroutine accumulate

!C+++
!C
!C
!C---
Subroutine integrate2(X,Y,C2,P2,S2)
implicit none
!C
!C
real(kind=skr),dimension(nMaxRough,nMaxRough) :: c2,p2,s2
integer(kind=ski)                 :: x,y
IF ( Y .eq. 1 ) THEN
    S2(Y,X) = 0
ELSE
    S2(Y,X) = S2(Y-1,X) + (C2(Y,X)-C2(Y-1,X))*(P2(Y,X) + P2(Y-1,X))/2
ENDIF
    C2(Y,X) = C2(Y,X)
RETURN
End Subroutine integrate2
              

!C+++
!C
!C
!C---
SUBROUTINE  Integrate1(X,C1,P1,S1)
implicit none
!C
!C
integer(kind=ski) ::  X
real(kind=skr), dimension(nMaxRough) ::  C1, P1, S1
!C
           If (X .eq. 1) THEN 
                 S1(X) = 0
           ELSE
                 S1(X) = S1(X-1) +  (C1(X) - C1(X-1))*(P1(X) + P1(X-1))/2
           ENDIF
           RETURN
End Subroutine Integrate1
 
!C+++
!C
!C      Generate.
!C
!C       take a pair of values x and y, which are initialized to 
!C       random variables on input, and use a cdf to map them
!C       into the domain space of the pdf supplied to the program.
!C
!C
!C---
Subroutine Generate_spgs (x,y,ierr, &
          c1,p1,s1,c2,p2,s2,s0, xoffset,yoffset,nx,ny)
implicit none

real(kind=skr), dimension(2)             ::  Vin,Vout
real(kind=skr), dimension(nMaxRough)     ::  p2temp,s2temp,c2temp

integer(kind=ski)  ::  xless,yless
integer(kind=ski)  ::  ierr
real(kind=skr)     ::  x,y

!ex-common block
real(kind=skr),dimension(nMaxRough)            :: c1,p1,s1
real(kind=skr),dimension(nMaxRough,nMaxRough)  :: c2,p2,s2
real(kind=skr)                                 :: s0, xoffset,yoffset
integer(kind=ski)                              :: nx,ny


!      common /spgs_info/ nx,ny,xoffset,yoffset,c1,c2,p1,p2,s0,s1,s2

      vin(1) = x
      vin(2) = y
      CALL Locate1(nx, C1, P1, S0, S1, Vin(1) , Vout(1), Xless) 
      CALL Interpolate1(ny, Vout, Xless, C1, C2, P1, P2, S1, S2, &
                  C2Temp, P2Temp, S2Temp )
      CALL Locate1(ny, C2Temp, P2Temp, S2Temp(ny), S2Temp, &
                   Vin(2), Vout(2), Yless)
      x = vout(1) + xoffset
      y = vout(2) + yoffset
      ierr = 0
RETURN
End Subroutine Generate_spgs

!C+++
!C
!C      Locate1, scans a line in the first dimension, looking
!C            for the intersection of Vin and  s1(x1) (x1 is
!C            is a space coordinate)
!C
!C            This is a parabolic interpolation algorithm
!C
!C---
SUBROUTINE Locate1(Xmax, C1, P1, S0, S1, Vin, Vout, Nless)
implicit none
!C
!C      NOTE: Vin is only a real number here, not an array, same with
!C            Vout
!C
integer(kind=ski)            ::  Xmax, Nless, Above, Below
real(kind=skr),dimension(42) ::  S1, C1, P1
real(kind=skr)               ::  S0, Vin, Vout
real(kind=skr)               ::  a,b,c

           Vin = Vin * S0
           IF ( Vin .lt. 0) THEN
                 Vin = 0
           ENDIF
           Below = 1
           Above = Xmax
100        continue 
           IF (Below .ne. Above) THEN
                 IF (Vin .lt. S0/2) THEN
                       IF (S1(Below + 1) .le. Vin) THEN 
                             Below = Below + 1
                             GOTO 100
                       ELSE IF (S1(Above - 1) .ge. Vin) THEN
                             Above = Above - 1
                             GOTO 100
                       ENDIF
                 ELSE
                       IF (S1(Above - 1) .ge. Vin) THEN
                             Above = Above - 1
                             GOTO 100
                        ELSE IF (S1(Below + 1) .le. Vin) THEN 
                             Below = Below + 1
                             GOTO 100
                       ENDIF
                 ENDIF       
           ENDIF
           IF (Above .eq. Below) THEN 
                 Vout = C1(Below)
           ELSE IF (abs(C1(above) - C1(below)).gt. 1e-28 ) then
                A = 0.5d0 * ( P1(above) - P1(below))/(C1(above) - C1(below) )
            B = (P1(below) * C1(above) - P1(above) * C1(below))/ &
                         (C1(above) - C1(below) )
                C = -1.0d0 *( A * C1(below) * C1(below) + B * C1(below))  
                IF (Abs(P1(above) - P1(below)) .gt. 1d-28 ) then
                  Vout = ((-1.0d0 * B ) + sqrt(B**2 - (4.0d0* A *  &
                         ( C + S1(Below) - Vin ))))/( 2* A)
                ELSE
                       Vout = 2.0d0 * ( Vin - S1(Below)) / &
                              (P1(above) - P1(below)) + C1(below)
                ENDIF
      else 
            Vout = C1(below)
           ENDIF
           Nless = Below
           IF ((vout .gt. c1(above)) .or. (vout .lt. c1(below))) THEN
!C                  WRITE (6,*) 'LF', C1(Below), Vout, C1(above), P1(below),
!C     $            P1(above)
            write(6,*) a,b,c, vout
           ENDIF
           RETURN
End subroutine  locate1

!C+++
!C
!C      Interpolate1
!C
!C      someday all these will be one routine, but no they are
!C      separate for ease of codification.
!C      This will take a point between C1(X) and C1(X+1) and generate 
!C      a CDF that is the interpolation of the two adjacent CDF's
!C      at the point specified.
!C
!C---
Subroutine interpolate1(Ymax, Vout, Xless, C1, C2, P1, P2, S1, S2, & 
      C2Temp, P2Temp, S2Temp)
implicit none
real(kind=skr),dimension(2)       ::  Vout
real(kind=skr),dimension(nMaxRough)     ::  c1,s1,p1,c2temp,s2temp,p2temp
real(kind=skr),dimension(nMaxRough,nMaxRough) ::  c2,s2,p2
!Real*8 Vout(2), C1(nMaxRough), C2(nMaxRough,nMaxRough), S1(nMaxRough)
!real*8 S2(nMaxRough,nMaxRough), P1(nMaxRough),
!     $                  P2(nMaxRough,nMaxRough),
!     $                  C2Temp(nMaxRough), S2Temp(nMaxRough), P2Temp(nMaxRough)
integer(kind=ski) ::  Ymax, Xless, Tab           
real(kind=skr)    ::  separation,delta

!C
!C      Note: this routine will have to be changed if the C's are ever
!C      shifted from the equal probability lines.
!C

DO Tab = 1,Ymax
    Separation = C1(XLess + 1) - C1(XLess)
    Delta = Vout(1) - C1(XLess)
    C2Temp(Tab) = C2(Tab, Xless) + ( C2(Tab, XLess + 1 )   &
                  - C2(Tab, XLess)) * ( Delta / Separation)
    S2Temp(Tab) = S2(Tab, XLess) + ( S2(Tab, XLess + 1 ) &
                  - S2(Tab, XLess)) * ( Delta / Separation)
    P2Temp(Tab) = P2(Tab, XLess) + ( P2(Tab, XLess + 1 ) &
                  - P2(Tab, XLess)) * ( Delta / Separation) 
END DO
RETURN
End subroutine interpolate1


End Module shadow_roughness
