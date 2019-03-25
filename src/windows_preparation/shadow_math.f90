












































!----
!----
!---- MODULE:  shadow_math
!----
!---- Contains the mathematical routines in Shadow
!----
!---- 
!---- todo: Much of the functionality of these routines is now in 
!----       fortran 95. It should be desirable to use then the intrinsic
!----       f95 routines, but this is left for the future.
!---- 
!----  Example of use: see test_math
!
!

Module shadow_math

    use shadow_globaldefinitions !, only : ski, skr, skc, sklen
    implicit none

!! uncomment this for PENELOPE random mumber generator
!!    integer(kind=4) ::  iseed1=2347,iseed2=2377

!---- Everything is private unless explicitly made public ----!
    private



!---- public routines :
!---- used in the geometrical source: wran, rotate, spl_int, atan_2, gauss
!---- the vectorial calculus tools:   scalar, dot, cross, norm, vector, 
!----                                 versor, proj, vsum, vdist

    public :: wran, init_random_seed, mysqrt
    public :: rotate, spl_int, lin_int, atan_2, gauss, binormal
    public :: scalar, dot, cross, norm, vector, versor, proj, vsum, vdist
    public :: gnormal, rotvector, mfp, cross_m_flag
    public :: qsf,cubspl
    ! used in shadow_preprocessors: ibcccu, ibcdcu
    public :: ibcccu, ibcdcu

    private :: gcf,gser
    private :: erfc,gammp,gammq,gammln

!----
!---- Some mathematical routines from IMSL library.
!----
!---- By now, it has only the routines used in the geometrical source: 
!----     MDNRIS
!----
!----     Note 1) the routine MERFI called by mdnris are set as private
!----     as it is not used directly in Shadow. 
!----
!----     Note 2) the error message routines in imsl have 
!----     been removed due to incompatibilities with f95
!----     They are: UERSET, UERTST, UGETIO, USPKD.
!----
!---- Example of use: see test_math_imsl
!
!
    public :: mdnris, zrpoly, dbcevl, pnpoly
    private :: merfi
    private :: zrpqlb, zrpqlc, zrpqld, zrpqle, zrpqlf, zrpqlg, zrpqlh, zrpqli



Contains


!!C +++
!!C	REAL FUNCTION	WRAN (ISEED)
!!C
!!C This function is simply a wrapper around the "real" (intrinsic) random
!!C number generator, RAN, supplied by all FTN compilers. It servers two
!!C purposes: filter out "bad" random numbers (== 0 in SHADOW) and allow
!!C SHADOW to read its random numbers out of a text file, so we can get 
!!C the results across platforms.
!!C
!!C 1/23/92 added the option to use the RAND function (IBM RS600
!!C specfically) in the absence of a RAN function.  Also, when the 6000
!!C compiler becomes able to handle the CALL EXIT, the STOP should be
!!C removed. (*update*).
!!C
!!C 		ISEED: the seed to give RAN.
!!C
!!C ---

        REAL(KIND=SKR)  FUNCTION WRAN (ISEED)

                implicit none

                INTEGER(KIND=SKI)              :: ISEED,K
                INTEGER(KIND=SKI),dimension(1) :: iseed2
                INTEGER(KIND=SKI)              :: first=1,wran_counter=0
                real(kind=skr)                 :: XX

                !first=1

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Uncomment this part is for the built-in f95 random generator
!!
!!
     !write(*,*) " "
     !write(*,*) "WRAN: first: ",first
     !write(*,*) "WRAN: wran_counter: ",wran_counter

         if (first.eq.1) then
                first = 0
                CALL init_random_seed(iseed)
         end if
         CALL RANDOM_NUMBER(WRAN)

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Uncomment for Penelope random number generator
!!
!!
!!           wran = RAND_PENELOPE(1.0D0)

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!
!! This part is for calling the C generator using the wranc.c wrapper
!! This part produces the same results than Shadow 2.x
!!

!! todo: MUST BE COMMENTED IN WINDOWS
!!      WRAN = WRANC(ISEED)
!!

      wran_counter=wran_counter+1
      RETURN
END FUNCTION WRAN


SUBROUTINE init_random_seed(iseed)
     INTEGER(kind=ski),intent(in) :: iseed
     call  init_random_seed_standard(iseed)
     ! TODO: to be written initializer for penelope random generator
     ! call  init_random_seed_penelope(iseed)
END SUBROUTINE 

SUBROUTINE init_random_seed_standard(iseed)
            INTEGER(kind=ski),intent(in) :: iseed
            INTEGER(kind=ski) :: i, clock

            !watch out the kind here !
            INTEGER           :: n
            INTEGER, DIMENSION(:), ALLOCATABLE :: seed

            CALL RANDOM_SEED(size = n)
            !print *,"n=",n
            ALLOCATE(seed(n))

            !CALL RANDOM_SEED(GET = oldseed)
            !print *,"INIT: old seed=",seed


            IF (iseed.eq.0) then
              CALL SYSTEM_CLOCK(COUNT=clock)
              seed = clock + 37 * (/ (i - 1, i = 1, n) /)
              print *,"INIT_RANDOM_SEED: random seed initialised using system clock"
            ELSE 
              seed = iseed
            END IF
            !print *,"INIT: seed=",seed
            CALL RANDOM_SEED(PUT = seed)

            DEALLOCATE(seed)
END SUBROUTINE


!
! this is an implementation of the complex square root. 
!
! ideally, the Fortran compiler implements it, but it gives me 
! "invalid memory reference" whan calling sqrt(FH*FH_BAR) in 
! subroutine crystal in windows with gfortran 4.8.0 20130302
!
! the implementation follows the formula posted by Didier Piau in: 
! http://math.stackexchange.com/questions/44406/how-do-i-get-the-square-root-of-a-complex-number

complex(kind=skx) function mysqrt(z)
   COMPLEX(KIND=skx) :: z,zout
   real(kind=skr)    :: r,zr,zi 
   r = abs(z)
   if (abs(z+r) .eq. 0) then 
      print*,'mysqrt: warning calculation using intrinsic function'
      mysqrt = cdsqrt(z)
   else
      mysqrt = sqrt(r) * (z+r)/abs(z+r) 
   endif
end function mysqrt



! C+++
! C SUBROUTINE ROTATE
! C
! C PURPOSE: GENERATE THE EULERIAN ANGLES FROM THE ELECTRON COORDINATES TO
! C THE SHADOW REFERENCE FRAME.
! C
! C----
        SUBROUTINE ROTATE (VIN,PSI,THETA,PHI,VOUT)
        !IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI) 	(I-N)
        implicit none

        !DIMENSION 				VIN(3), VOUT(3)
        real(kind=skr),dimension(3)  :: vin,vout
        real(kind=skr)               :: psi,theta,phi
        real(kind=skr)               :: cospsi,costhe,cosphi,sinpsi,sinthe,sinphi
!C
!C To avoid useles recalculations
!C
       COSPSI = COS (PSI)
       COSTHE = COS (THETA)
       COSPHI = COS (PHI)
       SINPSI = SIN (PSI)
       SINTHE = SIN (THETA)
       SINPHI = SIN (PHI)

       VOUT(1)= (COSPSI*COSPHI-COSTHE*SINPHI*SINPSI)*VIN(1)+&
                (-SINPSI*COSPHI-COSTHE*SINPHI*COSPSI)*VIN(2)+&
                (SINTHE*SINPHI)*VIN(3)

       VOUT(2)= (COSPSI*SINPHI+COSTHE*COSPHI*SINPSI)*VIN(1)+&
                (-SINPSI*SINPHI+COSTHE*COSPHI*COSPSI)*VIN(2)+&
                (-SINTHE*COSPHI)*VIN(3)

       VOUT(3)= (SINTHE*SINPSI)*VIN(1)+(SINTHE*COSPSI)*VIN(2) &
                +COSTHE*VIN(3)

    END SUBROUTINE ROTATE


! C+++
! C     SUBROUTINE    SPL_INT
! C
! C     PURPOSE    TO INTERPOLATE THE VALUE Y FOR A POINT X, USING THE ARRAY
! C                G(5,N) FROM THE PROGRAM CUBSPL.
! C
! C     INPUT      G(5,N) AS FROM CUBSPL.
! C                N, THE # OF DATA POINTS IN G(5,N).
! C                X, THE POINT WHERE YOU WANT THE INTERPOLATION TO BE MADE.   
! C 
! C     OUTPUT     Y, THE INTERPOLATED VALUE.
! C                IER =1 FOR ERROR, =0 OTHERWISE.
! C
! C---              
        SUBROUTINE SPL_INT(G,N,X,Y,IER)
  !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
       !IMPLICIT INTEGER(KIND=SKI)  (I-N)

       REAL(KIND=SKR),dimension(5,N),intent(in)  ::  G
       REAL(KIND=SKR),               intent(in)  ::  X
       INTEGER(KIND=SKI),            intent(in)  ::  N
       REAL(KIND=SKR),               intent(out) ::  Y
       REAL(KIND=SKR)                            ::  Z
       !REAL(KIND=SKR)     G(5,N), X, Y, Z
       INTEGER(KIND=SKI)              ::  I, IER
       real(kind=skr)                 ::  gmin,gmax

       GMAX = MAX(G(1,1),G(1,N))
       GMIN = MIN(G(1,1),G(1,N))
       IF ((X .LT. GMIN) .OR. (X .GT. GMAX)) THEN
! please note that an error here for BM or WIGGLER may be due
! to the use of an not-updated SRSPEC SRANG and SRDISTR
                WRITE(6,*) 'SPL_INT: x is outside the interpolation range.'
                WRITE(6,*) 'X, GMIN, GMAX: ',X,GMIN,GMAX
                IER = 1
                RETURN
       ELSE IF (X .EQ. G(1,N)) THEN
                I = N-1
                GO TO 10
       END IF
       I = 0
   21   IF (G(1,I+1) .LE. X) THEN
                I = I + 1
       GOTO 21
           END IF
   10  Z = X - G(1,I)
       Y = G(2,I) + Z*(G(3,I) + Z*(G(4,I) + Z*G(5,I)))
       IER = 0
       RETURN
        END SUBROUTINE SPL_INT


! C+++
! C     SUBROUTINE    LIN_INT
! C
! C     PURPOSE    TO INTERPOLATE LINEARLY THE VALUE Y FOR A POINT X, 
! C                USING ONLY THE ELEMENTS G(1,N) and G(2,N) OF THE ARRAY
! C                G(5,N) FROM THE PROGRAM CUBSPL.
! C
! C     INPUT      G(5,N) AS FROM CUBSPL.
! C                N, THE # OF DATA POINTS IN G(5,N).
! C                X, THE POINT WHERE YOU WANT THE INTERPOLATION TO BE MADE.   
! C 
! C     OUTPUT     Y, THE INTERPOLATED VALUE.
! C                IER =1 FOR ERROR, =0 OTHERWISE.
! C
! C---              
        SUBROUTINE LIN_INT(G,N,X,Y,IER)

       REAL(KIND=SKR),dimension(5,N),intent(in)  ::  G
       REAL(KIND=SKR),               intent(in)  ::  X
       INTEGER(KIND=SKI),            intent(in)  ::  N
       REAL(KIND=SKR),               intent(out) ::  Y
       REAL(KIND=SKR)                            ::  Z
       INTEGER(KIND=SKI)              ::  I, IER
       real(kind=skr)                 ::  gmin,gmax

       GMAX = MAX(G(1,1),G(1,N))
       GMIN = MIN(G(1,1),G(1,N))
       IF ((X .LT. GMIN) .OR. (X .GT. GMAX)) THEN
! please note that an error here for BM or WIGGLER may be due
! to the use of an not-updated SRSPEC SRANG and SRDISTR
                WRITE(6,*) 'LIN_INT: x is outside the interpolation range.'
                WRITE(6,*) 'X, GMIN, GMAX: ',X,GMIN,GMAX
                IER = 1
                RETURN
       ELSE IF (X .EQ. G(1,N)) THEN
                I = N-1
                GO TO 10
       END IF
       I = 0
   21   IF (G(1,I+1) .LE. X) THEN
                I = I + 1
       GOTO 21
           END IF
   10  Z = X - G(1,I)
       IF (I .EQ. N)  THEN
           WRITE(6,*) 'LIN_INT: End point. Set previous one'
           I = I-1
       END IF
       !cubic spline Y = G(2,I) + Z*(G(3,I) + Z*(G(4,I) + Z*G(5,I)))
       !liner interpolation
       Y = G(2,I) + Z* (G(2,I+1) -G(2,I))/(G(1,I+1)-G(1,I))
       IER = 0
       RETURN
        END SUBROUTINE LIN_INT



!C ++++
!C
!C       scalar multiplication
!C ----
        SUBROUTINE SCALAR( V1,ARG,V2)

        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI) (I-N)
        implicit none
        real(kind=skr),dimension(3),intent(in)  :: v1
        real(kind=skr),             intent(in)  :: arg
        real(kind=skr),dimension(3),intent(out) :: v2

        !DIMENSION    V1(3),V2(3)

        V2(1) =   V1(1)*ARG
        V2(2) =   V1(2)*ARG
        V2(3) =   V1(3)*ARG

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        IF (ABS(V2(1)).LT.1.0E-31) V2(1) = 0.0D0
        IF (ABS(V2(2)).LT.1.0E-31) V2(2) = 0.0D0
        IF (ABS(V2(3)).LT.1.0E-31) V2(3) = 0.0D0

    END SUBROUTINE SCALAR

! C ++++
! C
! C       scalar product 
! C ----
        SUBROUTINE DOT (V1,V2,RES)

        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)
        !DIMENSION    V1(3),V2(3)
        implicit none
        real(kind=skr),dimension(3),intent(in)  :: v1,v2
        real(kind=skr),             intent(out) :: res
        

        RES = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        IF (ABS(RES).LT.1.0E-31) RES = 0.0D0
      RETURN
    END SUBROUTINE DOT

! C ++++
! C
! C   	vector product :    vres = v1 x v2
! C ----
        SUBROUTINE CROSS (V1,V2,VRES)

        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI) (I-N)
        implicit none
        real(kind=skr),dimension(3),intent(in)   :: v1,v2
        real(kind=skr),dimension(3),intent(out)  :: vres
     
        integer(KIND=SKI)     :: m_flag
        character(len=sklen)  :: m_warning
        real(kind=skr)        :: ttest
        !DIMENSION   V1(3),V2(3),VRES (3)

        M_FLAG = 0
        VRES(1) = V1(2)*V2(3) - V1(3)*V2(2)
        VRES(2) = - ( V1(1)*V2(3) - V1(3)*V2(1) )
        VRES(3) =     V1(1)*V2(2) - V1(2)*V2(1)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        IF (ABS(VRES(1)).LT.1.0E-31) VRES(1) = 0.0D0
        IF (ABS(VRES(2)).LT.1.0E-31) VRES(2) = 0.0D0
        IF (ABS(VRES(3)).LT.1.0E-31) VRES(3) = 0.0D0

        TTEST  =  VRES(1)*VRES(1) + VRES(2)*VRES(2) + VRES(3)*VRES(3)
        IF (TTEST.LT.1.0E-31) THEN
         M_FLAG = 1
         M_WARNING = 'Error in CROSS: product is zero.'
        END IF
        END SUBROUTINE CROSS


! C ++++
! C
! C    vector product returning M_FLAG:    vres = v1 x v2
! C ----
!TODO: This is the same as CROSS() but also returning M_FLAG, to avoid
!      defining it as a global variable. To be fixed...

        SUBROUTINE CROSS_M_FLAG (V1,V2,VRES,M_FLAG)

        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI) (I-N)
        implicit none
        real(kind=skr),dimension(3),intent(in)   :: v1,v2
        real(kind=skr),dimension(3),intent(out)  :: vres
     
        integer(KIND=SKI)     :: m_flag
        character(len=sklen)  :: m_warning
        real(kind=skr)        :: ttest

        M_FLAG = 0
        VRES(1) = V1(2)*V2(3) - V1(3)*V2(2)
        VRES(2) = - ( V1(1)*V2(3) - V1(3)*V2(1) )
        VRES(3) =     V1(1)*V2(2) - V1(2)*V2(1)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        IF (ABS(VRES(1)).LT.1.0E-31) VRES(1) = 0.0D0
        IF (ABS(VRES(2)).LT.1.0E-31) VRES(2) = 0.0D0
        IF (ABS(VRES(3)).LT.1.0E-31) VRES(3) = 0.0D0

        TTEST  =  VRES(1)*VRES(1) + VRES(2)*VRES(2) + VRES(3)*VRES(3)
        IF (TTEST.LT.1.0E-31) THEN
         M_FLAG = 1
         M_WARNING = 'Error in CROSS: product is zero.'
        END IF
        END SUBROUTINE CROSS_M_FLAG




! C ++++
! C
! C       vector normalization 
! C ----

        SUBROUTINE NORM (V1,V2)
        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)
        !DIMENSION     V1(3),V2(3)
        implicit none
        real(kind=skr),dimension(3),intent(in)   :: v1
        real(kind=skr),dimension(3),intent(out)  :: v2

        real(kind=skr)   :: rnorm

        RNORM = V1(1)**2 + V1(2)**2 + V1(3)**2
        RNORM = SQRT(RNORM)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        IF (ABS(RNORM).LT.1.0E-31) RNORM = 0.0D0

        IF (RNORM.NE.0.0D0) THEN
           RNORM = 1/RNORM
           V2(1) = V1(1)*RNORM
           V2(2) = V1(2)*RNORM
           V2(3) = V1(3)*RNORM
        END IF
        RETURN

        END SUBROUTINE NORM

! C ++++
! C
! C       generate a vector  vres = p2 - p1    ( P1 -> P2 )
! C ----

        SUBROUTINE VECTOR (P1,P2,VRES)

        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)

        !DIMENSION       P1(3),P2(3),VRES(3)
        implicit none
        real(kind=skr),dimension(3),intent(in)   :: p1,p2
        real(kind=skr),dimension(3),intent(out)  :: vres

        integer(KIND=SKI) :: i

        DO I=1,3
             VRES(I)  =   P2(I) - P1(I)
!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
             IF (ABS(VRES(I)).LT.1.0E-31) VRES(I) = 0.0D0
        END DO
        END SUBROUTINE VECTOR


! C ++++
! C
! C generate a versor 
! C
! C ----
        SUBROUTINE VERSOR (P1,P2,VRES)

        !IMPLICIT REAL(KIND=SKR)   (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)    (I-N)

        !DIMENSION     P1(3),P2(3),VRES(3)

        implicit none
        real(kind=skr),dimension(3),intent(in)   :: p1,p2
        real(kind=skr),dimension(3),intent(out)  :: vres

        integer(KIND=SKI) :: I
        real(kind=skr)   :: rnorm
!C
!C **** CHECK FOR RNORM.EQ.0 SOMEBODY ******
!C
        RNORM =    .0D0
        DO I=1,3
            RNORM  =   RNORM + ( P1(I) - P2(I) )*( P1(I) - P2(I) )
        END DO
        RNORM  =   SQRT(RNORM)
        DO I=1,3
            VRES(I) =   (P2(I)-P1(I))/RNORM
        END DO
    END SUBROUTINE VERSOR

! C ++++
! C
! C  project v1 onto v2 
! C ----
        SUBROUTINE PROJ (V1,V2,VRES)

        implicit none
        real(kind=skr),dimension(3),intent(in)   :: v1,v2
        real(kind=skr),dimension(3),intent(out)  :: vres

        real(kind=skr)   :: rnorm,scalar1,prod
        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)
        !DIMENSION   V1(3),V2(3),VRES(3)

        RNORM = V2(1)**2 + V2(2)**2 + V2(3)**2

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        IF (ABS(RNORM).LT.1.0E-31) RNORM = 0.0D0

        IF (RNORM.EQ.0.0D0) THEN
            VRES(1) = 0.0D0
            VRES(2) = 0.0D0
            VRES(3) = 0.0D0
        ELSE
           ! srio renames SCALAR to scalar1
           scalar1 = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)
           PROD = scalar1/RNORM
           VRES(1) = V2(1)*PROD
           VRES(2) = V2(2)*PROD
           VRES(3) = V2(3)*PROD
        END IF
        END SUBROUTINE PROJ

! C ++++
! C
! C       Generates the sum of two vectors 
! C ----
        SUBROUTINE vSUM (P1,P2,RES)

        implicit none
        real(kind=skr),dimension(3),intent(in)   :: p1,p2
        real(kind=skr),dimension(3),intent(out)  :: res
        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)
        !DIMENSION   P1(3),P2(3),RES(3)

        RES(1) = P1(1) + P2(1)
        RES(2) = P1(2) + P2(2)
        RES(3) = P1(3) + P2(3)
    END SUBROUTINE vSUM

! C ++++
! C
! C Vector from a line to a point; line is specified by
! C       H0 (starting), VH (direction); point is P0, output is DIS
! C -----
    SUBROUTINE VDIST (P0, H0, VH, DIS)
    implicit none
    real(kind=skr),dimension(3),intent(in)   :: p0, h0, vh
    real(kind=skr),dimension(3),intent(out)  :: dis
    real(kind=skr),dimension(3)              :: vt, t_vh
    !IMPLICIT REAL(KIND=SKR)  (A-H, O-Z)
    !DIMENSION    P0(3), H0(3), VH(3), DIS(3)
    !DIMENSION    VT(3), T_VH(3)

    CALL VECTOR(H0, P0, VT)
    CALL PROJ(VT, VH, T_VH)
    CALL VECTOR(T_VH, VT, DIS)

    RETURN
    END SUBROUTINE VDIST

! C ++++
! C
! C This subroutine returns the value of the arctangent between 0-2*PI
! C ----
     SUBROUTINE ATAN_2 (SINE,COSINE,ANGLE)
     implicit none
     real(kind=skr),intent(in)   :: sine,cosine
     real(kind=skr),intent(out)  :: angle
     real(kind=skr)              :: arg

     !IMPLICIT REAL(KIND=SKR)  (A-E,G-H,O-Z)
     !IMPLICIT INTEGER(KIND=SKI)  (F,I-N)
     !moved to global_definitions DATA PI /3.141592653589793238462643D0/
     IF (COSINE.EQ.0.0D0.AND.SINE.EQ.0.0D0) THEN
       ANGLE = 0.0D0
       RETURN
     ELSE
     END IF
!C
!C Check if cosine is 0
!C
     IF (COSINE.NE.0) THEN
         ARG = SINE/COSINE
         ANGLE = ATAN (ABS(ARG))
!C
!C First quadrant: sine > 0, cosine > 0
!C
         IF (SINE.GT.0.0D0.AND.COSINE.GT.0.0D0) THEN
             ANGLE = ANGLE
!C
!C Second quadrant: sine > 0, cosine < 0
!C
         ELSE IF (SINE.GT.0.0D0.AND.COSINE.LT.0.0D0) THEN
             ANGLE = PI - ANGLE
!C
!C Third quadrant: sine < 0, cosine < 0
!C
         ELSE  IF (SINE.LT.0.0D0.AND.COSINE.LT.0.0D0) THEN
             ANGLE = ANGLE + PI
!C
!C Fourth quadrant: sine < 0, cosine > 0
!C
         ELSE  IF (SINE.LT.0.0D0.AND.COSINE.GT.0.0D0) THEN
             ANGLE = 2*PI - ANGLE
!C
!C Divide by zero cases
!C
         ELSE IF (SINE.EQ.0.0D0.AND.COSINE.GT.0.0D0) THEN
             ANGLE = 0.0D0
         ELSE IF (SINE.EQ.0.0D0.AND.COSINE.LT.0.0D0) THEN
             ANGLE = PI
         ELSE
         END IF
     ELSE IF (SINE.GT.0.0D0) THEN
         ANGLE = PI/2
     ELSE IF (SINE.LT.0.0D0) THEN
         ANGLE = 1.5D0*PI
     END IF

     END SUBROUTINE ATAN_2

! C +++
! C  SUBROUTINE GAUSS
! C
! C  PURPOSE  Generate a bivariate normal distribution with
! C    indipendent sigmas and a correlation.
! C    This will simulate a SR source of given emittance
! C    specified by the two sigmas. If dist < > 0, then
! C    the outputs will be correlated.
! C
! C  INPUT  sigma1, sigma2, distance
! C
! C  OUTPUT  X,X1 two binormal variate
! C
! C  ALGORITHM As described by R.H.Rubinstein, in Simulation and
! C    the MonteCarlo method, Wiley, NY 1981
! C
! C
! C ---
        SUBROUTINE GAUSS (S,SPRIM,DD,X,XPRIM,IS)

        implicit none
        real(kind=skr),       intent(in)   :: s, sprim, dd
        real(kind=skr),       intent(out)  :: x, xprim
        integer(kind=ski),    intent(out)  :: is

        real(kind=skr)                     :: c11,c22,c21,r1,r2,z1,z2
        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)
        !integer(KIND=SKI) :: is
        ! moved to shadow_globaldefinitions DATA  TWOPI /6.283185307179586467925287D0/
!C 
!C  Initialize by computing the covariance matrix
!C 
!   10 C11 =   SQRT (DD**2*SPRIM**2+S**2)
        C11 =   SQRT (DD**2*SPRIM**2+S**2)
        IF (C11.NE.0.0D0) THEN
            C21 = DD*SPRIM**2/C11
            C22 = S*SPRIM/C11
        ELSE
            C21 = 0.0D0
            C22 = 0.0D0
        END IF
!C 
!C  Entry for computation 
!C  Generates first the two normal variates with sigma=1 (Box & Muller)
!C 
!   20 CONTINUE
        R1 = WRAN(IS)
        R2 = WRAN(IS)
        Z1 = SQRT(-2*LOG(R1))*COS(TWOPI*R2)
        Z2 = SQRT(-2*LOG(R1))*SIN(TWOPI*R2)
!C 
!C  generates now the new varaites
!C 
        X = Z1*C11
        XPRIM = Z1*C21 + Z2*C22
!C 
!C  Completed
!C 
        RETURN
        END SUBROUTINE GAUSS


!------------------------------------------------------------------------------------------------

! C +++
! C  SUBROUTINE BINORMAL
! C
! C  PURPOSE  Generate a bivariate normal distribution with
! C           independent sigmas and a correlation
! C
! C  INPUT  sigma1, sigma2, rho (correlation)
! C
! C  OUTPUT  X,X1 two binormal variate
! C
! C  ALGORITHM  compute the Cholesky decomposition
! C
! C  TEST use emittance_test preprocessor
! C
! C  srio@esrf.eu 20140610: written, based on gauss()
! C
! C  
! C ---
        SUBROUTINE BINORMAL (sigma1,sigma2,rho,x1,x2,IS)

        implicit none
        real(kind=skr),       intent(in)   :: sigma1, sigma2, rho
        real(kind=skr),       intent(out)  :: x1, x2
        integer(kind=ski),    intent(out)  :: is

        real(kind=skr)                     :: r1,r2,z1,z2
        real(kind=skr)                     :: U11,U12,U21,U22

!C 
!C the covariance matrix is:
!C 
!     [  c11  c12  ]     [  sigma1^2           rho*sigma1*sigma2   ]
!     [  c21  c22  ]  =  [  rho*sigma1*sigma2  sigma2^2            ]
!
!
!        C11 =   sigma1**2 
!        C22 =   sigma2**2 
!        C21 =   rho*sigma1*sigma2

!
! calculate U bu Cholesky decomposition of C
!
! in Mathematica: 
! cov = {{S1^2, rho S1 S2}, {rho S1 S2, S2^2}}
! Uc = FullSimplify[CholeskyDecomposition[cov]]
! {{Sqrt[S1^2], (rho S1 S2)/Sqrt[S1^2]}, {0, Sqrt[ S2 (S2 - rho Conjugate[rho] Conjugate[S2])]}}
! 

        U11 = sigma1
        U21 = sigma2*rho
        U12 = 0.0
        U22 = sigma2 * sqrt( 1.0-rho**2 ) 
        
!C 
!C  Entry for computation 
!C  Generates first the two normal variates with sigma=1 (Box & Muller)
!C 
        R1 = WRAN(IS)
        R2 = WRAN(IS)
        Z1 = SQRT(-2*LOG(R1))*COS(TWOPI*R2)
        Z2 = SQRT(-2*LOG(R1))*SIN(TWOPI*R2)
!C 
!C  generates now the new variates
!C 
        x1 = Z1*U11 + Z2*U12
        x2 = Z1*U21 + Z2*U22

        RETURN
        END SUBROUTINE BINORMAL
!------------------------------------------------------------------------------------------------


! C
! C	SUBROUTINE GNORMAL
! C
! C     This subroutine give us a value following the gaussian
! C	distribution law. We initialize the subroutine (flag negative) 
! C	calling it with ARG the minimum and the maximun of the interval
! C	in which we want the result. After that, we call again the 
! C	subroutine with a flag no negative to have the result.
! C
! C
! C
SUBROUTINE GNORMAL (ARG,ISEED,IFLAG)

        implicit none
        integer(kind=ski), intent(in)    :: iflag
        integer(kind=ski), intent(inout) :: iseed
        real(kind=skr),    intent(out)   :: arg

        real(kind=skr)    :: ymin=0.0,ymax=0.0  ! initialization implies "SAVE"
        real(kind=skr)    :: yval
        integer(kind=ski) :: ierr
        !SAVE                YMIN, YMAX

        IF (IFLAG.LT.0) THEN
           IF (IFLAG.EQ.-2)  CALL GNORFUNC (ARG,YMIN)
           IF (IFLAG.EQ.-1)  CALL GNORFUNC (ARG,YMAX)
        ELSE
           YVAL = YMIN + WRAN(ISEED)*(YMAX-YMIN)
           CALL MDNRIS(YVAL,ARG,IERR)
           !  IF (IERR.NE.0) CALL MSSG &
           !  ('Error from GNORMAL','Value outside the interval',IERR) 
           IF (IERR.NE.0) print *,'GNORMAL: Error from MDNRIS.'
        END IF
        RETURN
End Subroutine gnormal

! C ++++
! C Subroutine GNORFUNC (Normal or gaussian probability distribution
! C function).
! C ----
        SUBROUTINE GNORFUNC (Y,P)
        implicit none

        real(KIND=SKR),intent(in)    :: y
        real(KIND=SKR),intent(out)   :: p

        real(KIND=SKR)                          :: y0

        Y0 =-Y/DSQRT(2.0D0)
        P  = 0.5D0*ERFC(Y0)

        RETURN
        END SUBROUTINE gnorfunc

! C ++++
! C FUNCTION ERFC (Complemented Error Fuction)
! C Reference: See Numerical Recipes in Fortran, Cambridge U. Press
! C (1989) for a description of this and following subroutines.
! C ----
        REAL(KIND=SKR)  FUNCTION ERFC(X)

        implicit none
        real(KIND=SKR),intent(in)  :: x

        IF(X.LT.0D0) THEN
                ERFC=1.0D0+GAMMP(0.5D0,X**2)
        ELSE
                ERFC=GAMMQ(0.5D0,X**2)
        ENDIF
        RETURN
        END FUNCTION erfc

! C
! C
! C
        REAL(KIND=SKR) FUNCTION GAMMP(A,X)

        implicit none
        real(KIND=SKR),intent(in) :: a,x
        real(KIND=SKR)            :: gamser,gln,gammcf
      

        IF(X.LT.0.0D0.OR.A.LE.0.0D0) THEN 
                  !PAUSE
                  print *,"PAUSE statement executed.  Hit Return to continue"
                  read (*,*) 
        END IF
        IF(X.LT.A+1.0D0) THEN
                CALL GSER(GAMSER,A,X,GLN)
        ELSE
                CALL GCF(GAMMCF,A,X,GLN)
                GAMMP=1.0D0-GAMMCF
        ENDIF
        RETURN

        End Function gammp
! C
! C
! C
        REAL(KIND=SKR) FUNCTION GAMMQ (A,X)

        implicit none
        real(KIND=SKR),intent(in)     :: a,x
        real(KIND=SKR)                  :: gamser,gln,gammcf

        IF(X.LT.0.0D0.OR.A.LE.0.0D0) THEN 
          !PAUSE
          print *,"PAUSE statement executed.  Hit Return to continue"
          read (*,*) 
        END IF
        IF(X.LT.A+1.0D0) THEN
          CALL GSER(GAMSER,A,X,GLN)
          GAMMQ=1.0D0-GAMSER
        ELSE
          CALL GCF(GAMMCF,A,X,GLN)
          GAMMQ=GAMMCF
        ENDIF
        RETURN
        End Function gammq
! C
! C
! C
       SUBROUTINE GCF (GAMMCF,A,X,GLN)

       implicit none
       !implicit real(KIND=SKR)  (a-h,o-z)
       !implicit integer(KIND=SKI)  (i-n)

       real(kind=skr)     :: gammcf, a, x, gln
       real(kind=skr)     :: gold, a0, a1, b0, b1, fac, an, ana, anf, g
       integer(kind=ski)  :: n,itmax=100
       real(kind=skr)     :: eps=3.0d-7

       !PARAMETER (ITMAX=100,EPS=3.D-7)

       GLN=GAMMLN(A)
       GOLD=0.D0
       A0=1.D0
       A1=X
       B0=0.D0
       B1=1.D0
       FAC=1.D0
       DO 11 N=1,ITMAX
                AN=DBLE(N)
                ANA=AN-A
                A0=(A1+A0*ANA)*FAC
                B0=(B1+B0*ANA)*FAC
                ANF=AN*FAC
                A1=X*A0+ANF*A1
                B1=X*B0+ANF*B1
                IF(A1.NE.0.) THEN
            FAC=1.0D0/A1
            G=B1*FAC
            IF(ABS((G-GOLD)/G).LT.EPS) GO TO 1
            GOLD=G
                ENDIF
   11   CONTINUE
        !PAUSE 'A too large, ITMAX too small'
        print *,"A too large, ITMAX too small"
        read (*,*) 
    1   GAMMCF=DEXP(-X+A*DLOG(X)-GLN)*G
        RETURN
        End Subroutine gcf

! C
! C
! C
        SUBROUTINE GSER (GAMSER,A,X,GLN)

        implicit real(KIND=SKR)  (a-h,o-z)
        implicit integer(KIND=SKI)  (i-n)

       PARAMETER (ITMAX=100,EPS=3.D-7)
       GLN=GAMMLN(A)
       IF(X.LE.0.0D0) THEN
                IF(X.LT.0.0D0) THEN 
                  !PAUSE
                  print *,"PAUSE statement executed.  Hit Return to continue"
                  read (*,*) 
                END IF
                GAMSER=0.0D0
                RETURN
       ENDIF
       AP=A
       SUM1=1.0D0/A
       DEL=SUM1
       DO 11 N=1,ITMAX
                AP=AP+1.0D0
                DEL=DEL*X/AP
                SUM1=SUM1+DEL
                IF(ABS(DEL).LT.ABS(SUM1)*EPS)GO TO 1
   11 CONTINUE
       !PAUSE 'A too large, ITMAX too small'
        print *,"A too large, ITMAX too small"
        read (*,*) 
        1   GAMSER=SUM1*DEXP(-X+A*DLOG(X)-GLN)
       RETURN
        End Subroutine gser
! C
! C
! C
REAL(KIND=SKR) FUNCTION GAMMLN(XX)

       real(KIND=SKR),intent(in)       :: XX
       real(KIND=SKR),dimension(6)     :: COF
       real(KIND=SKR)                  :: STP,HALF,ONE,FPF,X,TMP,SER
       integer(KIND=SKI)               :: J
      
       DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0, &
        -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
       DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/

       X=XX-ONE
       TMP=X+FPF
       TMP=(X+HALF)*DLOG(TMP)-TMP
       SER=ONE
       DO J=1,6
                X=X+ONE
                SER=SER+COF(J)/X
       END DO
       GAMMLN=TMP+DLOG(STP*SER)
       RETURN
End Function gammln

! C ++++
! C
! C       Rotation a vector VIN an angle ALPHA around an AXIS vector
! C ----
SUBROUTINE ROTVECTOR (VIN,AXIS,ALPHA,VOUT)

     implicit none

     real(kind=skr),dimension(3),    intent(in)  :: vin,axis
     real(kind=skr),                 intent(in)  :: alpha
     real(kind=skr),dimension(3),    intent(out) :: vout

     real(kind=skr),dimension(3) :: vctr0, vctr1, vctr2, vctr3
     real(kind=skr)              :: axis_mod2, eta, sa, ca
!   implicit real(KIND=SKR)  (a-h,o-z)
!   implicit integer(KIND=SKI)  (i-n)
!
!      DIMENSION VIN(3),AXIS(3),VOUT(3)
!      DIMENSION VCTR0(3),VCTR1(3),VCTR2(3),VCTR3(3)

                CALL DOT(AXIS,AXIS,AXIS_MOD2)
                CALL DOT(VIN,AXIS,ETA)
                ETA = ETA/AXIS_MOD2
                CALL SCALAR(AXIS,ETA,VCTR0)
                CALL VECTOR(VCTR0,VIN,VCTR1)
                CALL CROSS(VIN,AXIS,VCTR3)
                SA = SIN(ALPHA)/SQRT(AXIS_MOD2)
                CA = COS(ALPHA)
                CALL SCALAR(VCTR3,SA,VCTR3)
                CALL SCALAR(VCTR1,CA,VCTR1)
                CALL vSUM(VCTR1,VCTR3,VCTR2)
                CALL vSUM(VCTR0,VCTR2,VOUT)
     RETURN
End Subroutine rotvector


! C ++++
! C SUBROUTINE MFP  (mean free path)
! C
! C This subroutine return a value following the exponential decay
! C distribution law. We initialize the subroutine (flag negative) 
! C calling it with ARG the minimum and the maximun of the interval
! C in which we want the result. 
! C Iflag=-2 initializes the minimum and iflag=-1 the maximum
! C After that, we call again the subroutine with a non negative 
! C flag to have the result.
! C
! C
! C ----
       SUBROUTINE MFP (ARG,ISEED,IFLAG)

       implicit none

       real(kind=skr),     intent(inout) :: arg
       integer(kind=ski),  intent(inout) :: iseed
       integer(kind=ski),  intent(in)    :: iflag

       real(kind=skr)    :: ymin=0.0,ymax=0.0, aa0, yval

       !todo check this save
       !SAVE YMIN, YMAX

       IF (IFLAG.LT.0) THEN
           IF (IFLAG.EQ.-2)  YMIN = 1.0D0 - DEXP(-ARG)
           IF (IFLAG.EQ.-1)  YMAX = 1.0D0 - DEXP(-ARG)
       ELSE
           AA0  = WRAN(ISEED)
           YVAL = YMIN + AA0*(YMAX-YMIN)
           IF ((YVAL.GT.1.0D0).OR.(YVAL.LT.0.0D0)) THEN
               print *,'Error from MFP','Argument outside of [0,1] interval'
           END IF
           ARG  = -LOG(1-YVAL)
       END IF
       RETURN
       End Subroutine mfp


!C+++
!C     ..................................................................
!C
!C        SUBROUTINE QSF
!C
!C        PURPOSE
!C           TO COMPUTE THE VECTOR OF INTEGRAL VALUES FOR A GIVEN
!C           EQUIDISTANT TABLE OF FUNCTION VALUES.
!C
!C        USAGE
!C           CALL QSF (H,Y,Z,NDIM)
!C
!C        DESCRIPTION OF PARAMETERS
!C           H      - THE INCREMENT OF ARGUMENT VALUES.
!C           Y      - THE INPUT VECTOR OF FUNCTION VALUES.
!C           Z      - THE RESULTING VECTOR OF INTEGRAL VALUES. Z MAY BE
!C                    IDENTICAL WITH Y.
!C           NDIM   - THE DIMENSION OF VECTORS Y AND Z.
!C
!C        REMARKS
!C           NO ACTION IN CASE NDIM LESS THAN 3.
!C
!C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!C           NONE
!C
!C        METHOD
!C           BEGINNING WITH Z(1)=0, EVALUATION OF VECTOR Z IS DONE BY
!C           MEANS OF SIMPSONS RULE TOGETHER WITH NEWTONS 3/8 RULE OR A
!C           COMBINATION OF THESE TWO RULES. TRUNCATION ERROR IS OF
!C           ORDER H**5 (I.E. FOURTH ORDER METHOD). ONLY IN CASE NDIM=3
!C           TRUNCATION ERROR OF Z(2) IS OF ORDER H**4.
!C           FOR REFERENCE, SEE
!C           (1) F.B.HILDEBRAND, INTRODUCTION TO NUMERICAL ANALYSIS,
!C               MCGRAW-HILL, NEW YORK/TORONTO/LONDON, 1956, PP.71-76.
!C           (2) R.ZURMUEHL, PRAKTISCHE MATHEMATIK FUER INGENIEURE UND
!C               PHYSIKER, SPRINGER, BERLIN/GOETTINGEN/HEIDELBERG, 1963,
!C               PP.214-221.
!C
!C     ..................................................................
!C---
SUBROUTINE QSF(H,Y,Z,NDIM)
       implicit none
       real(kind=skr),                  intent(in) :: h
       real(kind=skr),dimension(ndim),  intent(in) :: y
       real(kind=skr),dimension(ndim),  intent(out):: z
       integer(kind=ski),               intent(in) :: ndim

       !implicit real(kind=skr) (a-h,o-z)
       !implicit integer(kind=ski)        (i-n)
       !DIMENSION  Y(NDIM),Z(NDIM)
       real(kind=skr)    :: sum1, aux1, sum2, aux2, aux, ht
       integer(kind=ski) :: i

 
!C
       HT=1.0D0/3.0D0*H
        IF(NDIM-5)7,8,1
!C
!C  Ndim is greater than 5. preparations of integration loop
!C
    1  SUM1 = 4.0D0 * Y(2)
       SUM1 = HT * (Y(1) + SUM1 + Y(3))
       AUX1 = 4.0D0 * Y(4)
       AUX1 = SUM1 + HT * (Y(3) + AUX1 + Y(5))
       AUX2 = HT *(Y(1)+3.875D0*(Y(2)+Y(5))+2.625D0*(Y(3)+Y(4))+Y(6))
       SUM2 = 4.0D0 * Y(5)
       SUM2 = AUX2-HT*(Y(4)+SUM2+Y(6))
       Z(1) = 0.0D0
       AUX  = 4.0D0 * Y(3)
       Z(2) = SUM2-HT*(Y(2)+AUX+Y(4))
       Z(3) = SUM1
       Z(4) = SUM2
       IF(NDIM-6)5,5,2
!C
!C  Integration loop
!C
    2  DO 4 I=7,NDIM,2
        SUM1   = AUX1
        SUM2   = AUX2
        AUX1   = 4.0D0*Y(I-1)
        AUX1   = SUM1+HT*(Y(I-2)+AUX1+Y(I))
        Z(I-2) = SUM1
        IF(I-NDIM)3,6,6
!C
    3   AUX2 = 4.0D0*Y(I)
        AUX2=SUM2+HT*(Y(I-1)+AUX2+Y(I+1))
    4   Z(I-1) = SUM2
    5   Z(NDIM-1) = AUX1
        Z(NDIM) = AUX2
       RETURN
    6  Z(NDIM-1) = SUM2
       Z(NDIM) = AUX1
       RETURN
!C
!C  End of integration loop
!C
    7  IF(NDIM-3)12,11,8
!C
!C  Ndim is equal to 4 or 5
!C
    8  SUM2 = 1.125D0*HT*(Y(1) + 3.0D0*Y(2) + 3.0D0*Y(3) + Y(4))
       SUM1 = 4.0D0*Y(2)
       SUM1 = HT*(Y(1)+SUM1+Y(3))
       Z(1) = 0.0D0
       AUX1 = 4.0D0*Y(3)
       Z(2) = SUM2-HT*(Y(2)+AUX1+Y(4))
       IF(NDIM-5)10,9,9
    9  AUX1 = 4.0D0*Y(4)
       Z(5) = SUM1+HT*(Y(3)+AUX1+Y(5))
   10  Z(3) = SUM1
       Z(4) = SUM2
       RETURN
!C
!C  Ndim is equal to 3
!C
   11  SUM1 = HT*(1.25D0*Y(1) + 2.0D0*Y(2) - .25D0*Y(3))
       SUM2 = 4.0D0*Y(2)
       Z(3) = HT*(Y(1)+SUM2+Y(3))
       Z(1) = 0.0D0
       Z(2) = SUM1
   12  RETURN
END SUBROUTINE QSF

!
!
!

!*******************************************************************************
!C+++
!C SUBROUTINE SORT_SPL (Real*8 version)
!C
!C PURPOSE  To sort a pair of array XVEC and YVEC.  The result is
!C   in ascending order according to XVEC.
!C---
SUBROUTINE SORT_SPL (XVEC,YVEC,ICOUNT)

        ! todo: remove implicits
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        !DIMENSION XVEC(ICOUNT),YVEC(ICOUNT)

        implicit none
        real(kind=skr),dimension(icount),   intent(inout) :: xvec, yvec
        integer(kind=ski),                  intent(in)    :: icount

        integer(kind=ski) :: i,j,imin
        real(kind=skr)    :: amin,xtemp,ytemp


        DO I = 1, ICOUNT
          IMIN = I
          AMIN = XVEC(I)
          DO J = I, ICOUNT
            IF (XVEC(J).LT.AMIN) THEN
              AMIN = XVEC(J)
              IMIN = J
            END IF
          END DO
          XTEMP  = XVEC(I)
          XVEC(I) = XVEC(IMIN)
          XVEC(IMIN) = XTEMP
          YTEMP  = YVEC(I)
          YVEC(I) = YVEC(IMIN)
          YVEC(IMIN) = YTEMP
        END DO
        RETURN
End Subroutine sort_spl 



!C+++
!C     PROGRAM    CUBSPL
!C
!C     PURPOSE    GENERATE THE COEFFICIENTS G(2,I), G(3,I), G(4,I) AND 
!C                G(5,I) FOR THE POLYNOMIAL Y=G(2,I)+X(I)*(G(3,I)+X(I)
!C                *(G(4,I)+X(I)*G(5,I))) WHICH IS VALID BETWEEN THE
!C                INTERVAL X(I) AND X(I+1).
!C
!C     INPUT      N     =# OF PAIR OF RAW DATA POINTS (X(I),Y(I)).
!C                Y(N)  =THE ARRAY WHICH CONTAINS THE DATA OF Y(1) TO Y(N).
!C                G(5,N)=THE FIRST ROW G(1,I) SHOULD CONTAIN THE DATA
!C                       OF X(1) TO X(N). THE OTHER FOUR ROWS WILL BE
!C                       FILLED UP WITH THE POLYNOMIAL COEFFICIENTS BY THIS
!C                       PROGRAM.
!C                IER=1(0), check (no check) for steep slope at the two ends.
!C
!C     OUTPUT     THE LAST FOUR ROWS OF G.
!C                IER=1 FOR ERROR, =0 OTHERWISE.
!C
!C---       
SUBROUTINE CUBSPL(G, Y, N, IER)

      implicit none
      integer(kind=ski),                intent(in)     :: n
      integer(kind=ski),                intent(inout)  :: ier
      real(kind=skr),dimension(5,N),    intent(inout)  :: G
      real(kind=skr),dimension(N),      intent(inout)  :: Y

      ! todo: remove implicits
      !implicit real(kind=skr) (a-h,o-z)
      !implicit integer(kind=ski)        (i-n)

      integer(kind=ski),dimension(2)   :: mpurge
      integer(kind=ski)                :: I, j, itmp, iend, istart
      real(kind=skr),dimension(N)      :: E1, E2
      real(kind=skr)                   :: R, smin

!srio danger
!      REAL*8 G(5,N), Y(N), E1(NPOINT), E2(NPOINT), R
!danger      real(kind=kind(1.0d0)),dimension(NPOINT)    :: E1, E2
!      REAL*8 G(5,N), Y(N), E1(N_DIM), E2(N_DIM), R
!      INTEGER I, IER, N
!C
!      IF (N.GT.N_DIM) CALL LEAVE &
!        ('CUBSPL','Splines cannot handle more than N_DIM points.' &
!srio      IF (N.GT.NPOINT) THEN 
!srio         print *,'N: ',N
!srio         print *,'NPOINT: ',NPOINT
!srio           CALL LEAVE &
!srio           ('CUBSPL','Splines cannot handle more than NPOINT points.',-1)
!srio      ENDIF
!srio danger
!srio      IF (N.GT.NPOINT) THEN
!srioprint *,'<><> N,NPOINT,IER: ',N,NPOINT,IER
!srio        CALL LEAVE &
!srio        ('CUBSPL','Splines cannot handle more than NPOINT points.' &
!srio       ,-1)
!srio      END IF

      itmp = -1
      !IF (N.LT.4) CALL LEAVE &
      !  ('CUBSPL','At least 4 data points are needed for splines.',itmp)
      IF (N.LT.4) THEN
        print *,'CUBSPL: At least 4 data points are needed for splines.',itmp
        print *,'Error: CUBSPL: Aborted'
        ! STOP 'Aborted'
      END IF
      DO J = 1, N-1
        IF (G(1,J).GT.G(1,J+1)) THEN
          DO I = 1, N
            E1(I) = G(1,I)
            E2(I) = Y(I)
          END DO
          CALL SORT_SPL (E1,E2,N)
          DO I = 1, N
            G(1,I) = E1(I)
            Y(I) = E2(I)
          END DO
          GO TO 101
        END IF
      END DO
101   SMIN = 1.0D+30

      DO I = 1, N-1 
         E1(I) = G(1,I+1) - G(1,I)

         !C check for zero here ...
         !C   IF (E1(I).EQ.0.0) THEN
         !C     E1(I) = 1.0E-30
         !C   ENDIF
         !C end changes.  may need at a later date for special wigglers. clw
         !C 7/22/93
         ! srio danger
         ! uncommented this to solve the problem in wiggler interpolation 
         ! shadow 2.3...
         ! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/compilation_fix2008-04-09.txt

         if (e1(i).eq.0.0) then
           e1(i) = 1.0d-12
         endif
!
         E2(I) = (Y(I+1) - Y(I)) / E1(I)
         SMIN = MIN(SMIN,ABS(E2(I)))
      END DO

      IF (IER.EQ.0) THEN
        ISTART = 1
        IEND = N
      ELSE
!C
!C Check if the slopes at the two ends (1 -> ISTART, IEND -> N) are too steep.
!C
        I = 1
!srio danger
! changed this to avoid problem in wiggler shadow 2.3....
! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/compilation_fix2008-04-09.txt
!51 IF (ABS(E2(I)).GT.SMIN*10) THEN

51      IF (ABS(E2(I)).GT.SMIN) THEN
            I = I + 1
            GOTO 51
        END IF
        ISTART = I
        I = N - 1
!srio danger
! changed this to avoid problem in wiggler shadow 2.3....
!61 IF (ABS(E2(I)).GT.SMIN*10) THEN
61     IF (ABS(E2(I)).GT.SMIN*1) THEN
          I = I - 1
        GOTO 61
       END IF
        IEND = I
       IF ((IEND-ISTART+1).LT.4) THEN
          ISTART = 1
          IEND =N
       END IF
      END IF
!C
!C Start computing the spline coefficients.
!C
      G(3,ISTART) = E1(ISTART+1)
      G(4,ISTART) = E1(ISTART) + E1(ISTART+1)
      G(5,ISTART) = ((E1(ISTART)+2.0D0*G(4,ISTART))*E1(ISTART+1)* &
        E2(ISTART)+(E1(ISTART)**2.0D0) &
               *E2(ISTART+1))/G(4,ISTART)          
      DO I = ISTART+1, IEND-1
        G(2,I) = E1(I)
        G(3,I) = 2.0D0*(E1(I-1) + E1(I))
        G(4,I) = E1(I-1)
        G(5,I) = 3.0D0*(E1(I)*E2(I-1) + E1(I-1)*E2(I))
      END DO
      G(2,IEND) = E1(IEND-1) + E1(IEND-2)
      G(3,IEND) = E1(IEND-2)
      G(5,IEND) = ((E1(IEND-1)+2.0D0*G(2,IEND))*E2(IEND-1)*E1(IEND-2)+ &
        (E1(IEND-1) &
               **2.0D0)*E2(IEND-2))/G(2,IEND)
      DO I = ISTART, IEND-1
        R = G(2,I+1) / G(3,I)
        G(3,I+1) = G(3,I+1) - R*G(4,I)
        G(5,I+1) = G(5,I+1) - R*G(5,I)
      END DO
      G(3,IEND) = G(5,IEND) / G(3,IEND)
      DO I = IEND-1, ISTART, -1
        G(3,I) = (G(5,I) - G(4,I)*G(3,I+1)) / G(3,I)
      END DO
      DO I = ISTART, IEND-1
        G(2,I) = Y(I)
        G(4,I) = (3.0D0*E2(I) - 2.0D0*G(3,I) - G(3,I+1)) / E1(I)
        G(5,I) = (-2.0D0*E2(I) + G(3,I) + G(3,I+1)) / (E1(I)**2.0D0)
      END DO
!C
!C Use broken line instead at the two ends if slope are too steep.
!C
      DO I = 1, ISTART-1
          G(2,I) = Y(I)
          G(3,I) = E2(I)
          G(4,I) = 0.0D0
          G(5,I) = 0.0D0
      END DO
      DO I = IEND, N-1
          G(2,I) = Y(I)
          G(3,I) = E2(I)
          G(4,I) = 0.0D0
          G(5,I) = 0.0D0
      END DO
      IER = 0
      RETURN
End Subroutine cubspl


!C  *********************************************************************
!C                         FUNCTION RAND_PENELOPE
!C  *********************************************************************
      FUNCTION RAND_PENELOPE(DUMMY)
!C
!C  This is an adapted version of subroutine RANECU written by F. James
!C  (Comput. Phys. Commun. 60 (1990) 329-344), which has been modified to
!C  give a single random number at each call.
!C
!C  The 'seeds' ISEED1 and ISEED2 must be initialised in the main program
!C  and transferred through the named common block /RSEED/.
!C
!C  Some compilers incorporate an intrinsic random number generator with
!C  the same name (but with different argument lists). To avoid conflict,
!C  it is advisable to declare RAND as an external function in all sub-
!C  programs that call it.
!C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I-N)
      PARAMETER (USCALE=1.0D0/2.147483563D9)
! defined and initialized in the global definitions
!C      COMMON/RSEED/ISEED1,ISEED2
!C
      I1=ISEED1/53668
      ISEED1=40014*(ISEED1-I1*53668)-I1*12211
      IF(ISEED1.LT.0) ISEED1=ISEED1+2147483563
!C
      I2=ISEED2/52774
      ISEED2=40692*(ISEED2-I2*52774)-I2*3791
      IF(ISEED2.LT.0) ISEED2=ISEED2+2147483399
!C
      IZ=ISEED1-ISEED2
      IF(IZ.LT.1) IZ=IZ+2147483562
      RAND_PENELOPE=IZ*USCALE
!C
      RETURN
END FUNCTION RAND_PENELOPE


!
! FROM HERE IMSL ROUTINES....
!

! C   IMSL ROUTINE NAME   - MERFI
! C
! C-----------------------------------------------------------------------
! C
! C   COMPUTER            - VAX/SINGLE
! C
! C   LATEST REVISION     - JANUARY 1, 1978
! C
! C   PURPOSE             - INVERSE ERROR FUNCTION
! C
! C   USAGE               - CALL MERFI (P,Y,IER)
! C
! C   ARGUMENTS    P      - INPUT VALUE IN THE EXCLUSIVE RANGE (-1.0,1.0)
! C                Y      - OUTPUT VALUE OF THE INVERSE ERROR FUNCTION
! C                IER    - ERROR PARAMETER (OUTPUT)
! C                         TERMINAL ERROR
! C                           IER = 129 INDICATES P LIES OUTSIDE THE LEGAL
! C                             RANGE. PLUS OR MINUS MACHINE INFINITY IS
! C                             GIVEN AS THE RESULT (SIGN IS THE SIGN OF
! C                             THE FUNCTION VALUE OF THE NEAREST LEGAL
! C                             ARGUMENT).
! C
! C   PRECISION/HARDWARE  - SINGLE/ALL
! C
! C   REQD. IMSL ROUTINES - UERTST,UGETIO
! C
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
! C
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
! C
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
! C
! C-----------------------------------------------------------------------
! C
        SUBROUTINE MERFI (P,Y,IER)
! C                                  SPECIFICATIONS FOR ARGUMENTS
! C      REAL               P,Y
     IMPLICIT REAL(kind=skr)    (A-H, O-Z)
     IMPLICIT INTEGER(kind=ski) (I-N)
     INTEGER(kind=ski)          IER
! C                    SPECIFICATIONS FOR LOCAL VARIABLES
      REAL(kind=skr)    A,B,X,Z,W,WI,SN,SD,F,Z2,RINFM,A1,A2,A3,B0,B1, &
                       B2,B3,C0,C1,C2,C3,D0,D1,D2,E0,E1,E2,E3,F0,F1, &
                      F2,G0,G1,G2,G3,H0,H1,H2,SIGMA
      DATA              A1/-.5751703/,A2/-1.896513/,A3/-.5496261E-1/
      DATA              B0/-.1137730/,B1/-3.293474/,B2/-2.374996/
      DATA              B3/-1.187515/
      DATA              C0/-.1146666/,C1/-.1314774/,C2/-.2368201/
      DATA              C3/.5073975E-1/
      DATA              D0/-44.27977/,D1/21.98546/,D2/-7.586103/
      DATA              E0/-.5668422E-1/,E1/.3937021/,E2/-.3166501/
      DATA              E3/.6208963E-1/
      DATA              F0/-6.266786/,F1/4.666263/,F2/-2.962883/
      DATA              G0/.1851159E-3/,G1/-.2028152E-2/
      DATA              G2/-.1498384/,G3/.1078639E-1/
      DATA              H0/.9952975E-1/,H1/.5211733/
      DATA              H2/-.6888301E-1/
      DATA              RINFM/1.7014E+38/
      DATA              ONE/1.0d0/
! C                    FIRST EXECUTABLE STATEMENT
          IER = 0
        X = P
       SIGMA = SIGN(ONE,X)
! C                    TEST FOR INVALID ARGUMENT
       IF (.NOT.(X.GT.-1. .AND. X.LT.1.)) GO TO 30
        Z = ABS(X)
       IF (Z.LE. .85) GO TO 20
       A = 1.-Z
       B = Z
! C                    REDUCED ARGUMENT IS IN (.85,1.),
! C                    OBTAIN THE TRANSFORMED VARIABLE
    5  W = SQRT(-LOG(A+A*B))
       IF (W.LT.2.5) GO TO 15
       IF (W.LT.4.) GO TO 10
! C                    W GREATER THAN 4., APPROX. F BY A
! C                    RATIONAL FUNCTION IN 1./W
       WI = 1./W
       SN = ((G3*WI+G2)*WI+G1)*WI
       SD = ((WI+H2)*WI+H1)*WI+H0
       F = W + W*(G0+SN/SD)
       GO TO 25
! C                    W BETWEEN 2.5 AND 4., APPROX. F
! C                    BY A RATIONAL FUNCTION IN W
    10  SN = ((E3*W+E2)*W+E1)*W
       SD = ((W+F2)*W+F1)*W+F0
       F = W + W*(E0+SN/SD)
       GO TO 25
! C                    W BETWEEN 1.13222 AND 2.5, APPROX.
! C                    F BY A RATIONAL FUNCTION IN W
    15  SN = ((C3*W+C2)*W+C1)*W
       SD = ((W+D2)*W+D1)*W+D0
       F = W + W*(C0+SN/SD)
       GO TO 25
! C                    Z BETWEEN 0. AND .85, APPROX. F
! C                    BY A RATIONAL FUNCTION IN Z
    20  Z2 = Z*Z
       F = Z+Z*(B0+A1*Z2/(B1+Z2+A2/(B2+Z2+A3/(B3+Z2))))
! C                    FORM THE SOLUTION BY MULT. F BY
! C                    THE PROPER SIGN
    25  Y = SIGMA*F
       IER = 0
       GO TO 9005
! C                    ERROR EXIT. SET SOLUTION TO PLUS
! C                    (OR MINUS) INFINITY
    30  IER = 129
       Y = SIGMA * RINFM
  9000  CONTINUE
    !    CALL UERTST(IER,6HMERFI )
       print *,"Error: MERFI: Error from math routine merfi. Called with: ",P
       return 
       ! stop
  9005  RETURN
        END SUBROUTINE MERFI
! C   IMSL ROUTINE NAME   - MERRC=ERFC


    !
    !---- Public Subroutines ----!
    !

    !
    !
    !

! C   IMSL ROUTINE NAME   - MDNRIS
! C
! C-----------------------------------------------------------------------
! C
! C   COMPUTER            - VAX/SINGLE
! C
! C   LATEST REVISION     - JUNE 1, 1981
! C
! C   PURPOSE             - INVERSE STANDARD NORMAL (GAUSSIAN)
! C                           PROBABILITY DISTRIBUTION FUNCTION
! C
! C   USAGE               - CALL MDNRIS (P,Y,IER)
! C
! C   ARGUMENTS    P      - INPUT VALUE IN THE EXCLUSIVE RANGE (0.0,1.0)
! C                Y      - OUTPUT VALUE OF THE INVERSE NORMAL (0,1)
! C                           PROBABILITY DISTRIBUTION FUNCTION
! C                IER    - ERROR PARAMETER (OUTPUT)
! C                         TERMINAL ERROR
! C                           IER = 129 INDICATES P LIES OUTSIDE THE LEGAL
! C                             RANGE. PLUS OR MINUS MACHINE INFINITY IS
! C                             GIVEN AS THE RESULT (SIGN IS THE SIGN OF
! C                             THE FUNCTION VALUE OF THE NEAREST LEGAL
! C                             ARGUMENT).
! C
! C   PRECISION/HARDWARE  - SINGLE/ALL
! C
! C   REQD. IMSL ROUTINES - MERFI,UERTST,UGETIO
! C
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
! C
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
! C
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
! C
! C-----------------------------------------------------------------------
! C
    SUBROUTINE MDNRIS (P,Y,IER)
! C                                  SPECIFICATIONS FOR ARGUMENTS
! C     REAL               P,Y
      ! IMPLICIT DOUBLE PRECISION   (A-H,O-Z)
     IMPLICIT REAL(kind=skr) (A-H, O-Z)
     IMPLICIT INTEGER(kind=ski) (I-N)
! C     DOUBLE PRECISION   P,Y
     INTEGER(kind=ski)            IER
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES
! C     REAL               EPS,G0,G1,G2,G3,H0,H1,H2,A,W,WI,SN,SD
! C     REAL               SIGMA,SQRT2,X,XINF
! C
    DATA               XINF/1.7014E+38/
    DATA               SQRT2/1.414214/
    DATA               EPS/1.1921E-07/
    DATA               G0/.1851159E-3/,G1/-.2028152E-2/
    DATA               G2/-.1498384/,G3/.1078639E-1/
    DATA               H0/.9952975E-1/,H1/.5211733/
    DATA               H2/-.6888301E-1/
! C                                  FIRST EXECUTABLE STATEMENT
     IER = 0
     IF (P .GT. 0.0 .AND. P .LT. 1.0) GO TO 5
     IER = 129
! C
! C Need the followin REAL() cast to make the intrinsic function work on VMS
! C
     SIGMA = REAL (SIGN(1.0, REAL(P)))
     Y = SIGMA * XINF
     GO TO 9000
    5 IF(P.LE.EPS) GO TO 10
     X = 1.0 -(P + P)
     CALL MERFI (X,Y,IER)
     Y = -SQRT2 * Y
     GO TO 9005
! C                                   P TOO SMALL, COMPUTE Y DIRECTLY
   10 A = P+P
     W = SQRT(-LOG(A+(A-A*A)))
! C                           USE A RATIONAL FUNCTION IN 1./W
     WI = 1./W
     SN = ((G3*WI+G2)*WI+G1)*WI
                SD = ((WI+H2)*WI+H1)*WI+H0
    Y = W + W*(G0+SN/SD)
    Y = -Y*SQRT2
     GO TO 9005
 9000  CONTINUE
    !      CALL UERTST(IER,6HMDNRIS)
     print *,"Error: MDNRIS: Error from math routine mdnris. Called with: ",P
     ! stop
 9005  RETURN
    END SUBROUTINE MDNRIS




! C   IMSL ROUTINE NAME   - ZRPOLY
! C
! C-----------------------------------------------------------------------
! C
! C   COMPUTER            - VAX/DOUBLE
! C
! C   LATEST REVISION     - JANUARY 1, 1978
! C
! C   PURPOSE             - ZEROS OF A POLYNOMIAL WITH DOUBLE PRECISION
! C                           COEFFICIENTS (JENKINS-TRAUB)
! C
! C   USAGE               - CALL ZRPOLY (A,NDEG,Z,IER)
! C
! C   ARGUMENTS    A      - INPUT REAL VECTOR OF LENGTH NDEG+1
! C                           CONTAINING THE COEFFICIENTS IN ORDER OF
! C                           DECREASING POWERS OF THE VARIABLE.
! C                NDEG   - INPUT INTEGER DEGREE OF POLYNOMIAL.
! C                           NDEG MUST BE GREATER THAN 0 AND LESS
! C                           THAN 101.
! C                Z      - OUTPUT COMPLEX VECTOR OF LENGTH NDEG
! C                           CONTAINING THE COMPUTED ROOTS OF THE
! C                           POLYNOMIAL.
! C                         NOTE - THE ROUTINE TREATS Z AS A DOUBLE PRECISION VECTOR
! C                           OF LENGTH 2*NDEG. AN APPROPRIATE
! C                           EQUIVALENCE STATEMENT MAY BE REQUIRED.
! C                           SEE DOCUMENT EXAMPLE.
! C                IER    - ERROR PARAMETER. (OUTPUT)
! C                         TERMINAL ERROR
! C                           IER=129, INDICATES THAT THE DEGREE OF THE
! C                             POLYNOMIAL IS GREATER THAN 100 OR LESS
! C                             THAN 1.
! C                           IER=130, INDICATES THAT THE LEADING
! C                             COEFFICIENT IS ZERO.
! C                           IER=131, INDICATES THAT ZRPOLY FOUND FEWER
! C                             THAN NDEG ZEROS. IF ONLY M ZEROS ARE
! C                             FOUND, Z(J),J=M+1,...,NDEG ARE SET TO
! C                             POSITIVE MACHINE INFINITY.
! C
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32
! C                       - SINGLE/H36,H48,H60
! C
! C   REQD. IMSL ROUTINES - UERTST,UGETIO,ZRPQLB,ZRPQLC,ZRPQLD,ZRPQLE,
! C                           ZRPQLF,ZRPQLG,ZRPQLH,ZRPQLI
! C
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
! C
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
! C
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
! C
! C-----------------------------------------------------------------------
! C
        SUBROUTINE ZRPOLY (A,NDEG,Z,IER)
! C                                  SPECIFICATIONS FOR ARGUMENTS
        INTEGER(kind=ski)      NDEG, IER
        REAL(kind=skr)     A(1),Z(1)
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES
        INTEGER(kind=ski)       N,NN,J,JJ,I,NM1,ICNT,N2,L,NZ,NPI
        INTEGER(kind=ski)       :: i20=20
        REAL(kind=skr)     ETA,RMRE,RINFP,REPSP,RADIX,RLO,XX,YY,SINR,      &
                  COSR,RMAX,RMIN,X,SC,XM,FF,DX,DF,BND,XXX,ARE
        REAL(kind=skr)     PT(101)
        REAL(kind=skr)     TEMP(101),P(101),QP(101),RK(101),QK(101),SVK(101)
        REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                  A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                  T,AA,BB,CC,FACTOR,REPSR1,ZERO,ONE,FN
        LOGICAL              ZEROK
        COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                  A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
! C                                  THE FOLLOWING STATEMENTS SET MACHINE
! C                                    CONSTANTS USED IN VARIOUS PARTS OF
! C                                    THE PROGRAM. THE MEANING OF THE
! C                                    FOUR CONSTANTS ARE - REPSR1 THE
! C                                    MAXIMUM RELATIVE REPRESENTATION
! C                                    ERROR WHICH CAN BE DESCRIBED AS
! C                                    THE SMALLEST POSITIVE FLOATING
! C                                    POINT NUMBER SUCH THAT 1.+REPSR1 IS
! C                                    GREATER THAN 1
! C                                  RINFP THE LARGEST FLOATING-POINT
! C                                    NUMBER
! C                                  REPSP THE SMALLEST POSITIVE
! C                                    FLOATING-POINT NUMBER IF THE
! C                                    EXPONENT RANGE DIFFERS IN SINGLE
! C                                    AND DOUBLE PRECISION THEN REPSP
! C                                    AND RINFP SHOULD INDICATE THE
! C                                    SMALLER RANGE
! C                                  RADIX THE BASE OF THE FLOATING-POINT
! C                                    NUMBER SYSTEM USED
                DATA               RINFP/1.7E+38/
       DATA               REPSP/2.9388E-39/
       DATA               RADIX/2.0/
       DATA               REPSR1/2.775557562D-17/
       DATA               ZERO/0.0D0/,ONE/1.0D0/
! C                                  ZRPOLY USES SINGLE PRECISION
! C                                    CALCULATIONS FOR SCALING, BOUNDS
! C                                    AND ERROR CALCULATIONS.
! C                                  FIRST EXECUTABLE STATEMENT
                IER = 0
       IF (NDEG .GT. 100 .OR. NDEG .LT. 1) GO TO 165
       ETA = REPSR1
       ARE = ETA
       RMRE = ETA
       RLO = REPSP/ETA
! C                                  INITIALIZATION OF CONSTANTS FOR
! C                                    SHIFT ROTATION
            XX = .7071068
       YY = -XX
       SINR = .9975641
       COSR = -.06975647
       N = NDEG
       NN = N+1
! C                                  ALGORITHM FAILS IF THE LEADING
! C                                    COEFFICIENT IS ZERO.
     IF (A(1).NE.ZERO) GO TO 5
       IER = 130
       GO TO 9000
! C                                  REMOVE THE ZEROS AT THE ORIGIN IF
! C                                    ANY
        5  IF (A(NN).NE.ZERO) GO TO 10
       J = NDEG-N+1
       JJ = J+NDEG
       Z(J) = ZERO
       Z(JJ) = ZERO
       NN = NN-1
       N = N-1
       IF (NN.EQ.1) GO TO 9005
       GO TO 5
! C                                  MAKE A COPY OF THE COEFFICIENTS
   10  DO 15 I=1,NN
        P(I) = A(I)
   15  CONTINUE
! C                                  START THE ALGORITHM FOR ONE ZERO
   20  IF (N.GT.2) GO TO 30
       IF (N.LT.1) GO TO 9005
! C                                  CALCULATE THE FINAL ZERO OR PAIR OF
! C                                    ZEROS
       IF (N.EQ.2) GO TO 25
       Z(NDEG) = -P(2)/P(1)
       Z(NDEG+NDEG) = ZERO
       GO TO 145
   25  CALL ZRPQLI (P(1),P(2),P(3),Z(NDEG-1),Z(NDEG+NDEG-1),Z(NDEG),Z(NDEG+NDEG))
       GO TO 145
! C                                  FIND LARGEST AND SMALLEST MODULI OF
! C                                    COEFFICIENTS.
   30  RMAX = 0.
       RMIN = RINFP
       DO 35 I=1,NN
         X = ABS(SNGL(P(I)))
          IF (X.GT.RMAX) RMAX = X
          IF (X.NE.0..AND.X.LT.RMIN) RMIN = X
   35  CONTINUE
! C                                  SCALE IF THERE ARE LARGE OR VERY
! C                                    SMALL COEFFICIENTS COMPUTES A
! C                                    SCALE FACTOR TO MULTIPLY THE
! C                                    COEFFICIENTS OF THE POLYNOMIAL.
! C                                    THE SCALING IS DONE TO AVOID
! C                                    OVERFLOW AND TO AVOID UNDETECTED
! C                                    UNDERFLOW INTERFERING WITH THE
! C                                    CONVERGENCE CRITERION.
! C                                  THE FACTOR IS A POWER OF THE BASE
                SC = RLO/RMIN
       IF (SC.GT.1.0) GO TO 40
       IF (RMAX.LT.10.) GO TO 55
       IF (SC.EQ.0.) SC = REPSP*RADIX*RADIX
       GO TO 45
   40  IF (RINFP/SC.LT.RMAX) GO TO 55
   45  L = DLOG(SC)/DLOG(RADIX)+.5
       IF (L .EQ. 0) GO TO 55
       FACTOR = DBLE(RADIX)**L
       DO 50 I=1,NN
        P(I) = FACTOR*P(I)
   50  CONTINUE
! C                                  COMPUTE LOWER BOUND ON MODULI OF
! C                                    ZEROS.
   55  DO 60 I=1,NN
      PT(I) = ABS(SNGL(P(I)))
   60  CONTINUE
       PT(NN) = -PT(NN)
! C                                  COMPUTE UPPER ESTIMATE OF BOUND
       X = DEXP((DLOG(-PT(NN))-DLOG(PT(1)))/N)
       IF (PT(N).EQ.0.) GO TO 65
! C                                  IF NEWTON STEP AT THE ORIGIN IS
! C                                    BETTER, USE IT.
       XM = -PT(NN)/PT(N)
       IF (XM.LT.X) X = XM
! C                                  CHOP THE INTERVAL (0,X) UNTIL FF.LE.0
   65  XM = X*.1
       FF = PT(1)
       DO 70 I=2,NN
        FF = FF*XM+PT(I)
   70  CONTINUE
       IF (FF.LE.0.) GO TO 75
       X = XM
       GO TO 65
   75  DX = X
! C                                  DO NEWTON ITERATION UNTIL X
! C                                    CONVERGES TO TWO DECIMAL PLACES
   80  IF (ABS(DX/X).LE..005) GO TO 90
       FF = PT(1)
       DF = FF
       DO 85 I=2,N
                FF = FF*X+PT(I)
          DF = DF*X+FF
   85  CONTINUE
       FF = FF*X+PT(NN)
       DX = FF/DF
       X = X-DX
       GO TO 80
   90  BND = X
! C                                  COMPUTE THE DERIVATIVE AS THE INTIAL
! C                                    K POLYNOMIAL AND DO 5 STEPS WITH
! C                                    NO SHIFT
       NM1 = N-1
       FN = ONE/N
       DO 95 I=2,N
        RK(I) = (NN-I)*P(I)*FN
   95  CONTINUE
       RK(1) = P(1)
       AA = P(NN)
       BB = P(N)
       ZEROK = RK(N).EQ.ZERO
       DO 115 JJ=1,5
          CC = RK(N)
          IF (ZEROK) GO TO 105
! C                                  USE SCALED FORM OF RECURRENCE IF
! C                                    VALUE OF K AT 0 IS NONZERO
          T = -AA/CC
          DO 100 I=1,NM1
             J = NN-I
             RK(J) = T*RK(J-1)+P(J)
  100     CONTINUE
          RK(1) = P(1)
          ZEROK = DABS(RK(N)).LE.DABS(BB)*ETA*10.
        GO TO 115
! C                                  USE UNSCALED FORM OF RECURRENCE
  105    DO 110 I=1,NM1
                 J = NN-I
             RK(J) = RK(J-1)
  110    CONTINUE
         RK(1) = ZERO
        ZEROK = RK(N).EQ.ZERO
  115  CONTINUE
! C                                  SAVE K FOR RESTARTS WITH NEW SHIFTS
       DO 120 I=1,N
        TEMP(I) = RK(I)
  120  CONTINUE
! C                                  LOOP TO SELECT THE QUADRATIC
! C                                    CORRESPONDING TO EACH NEW SHIFT
       DO 140 ICNT=1,20
! C                                  QUADRATIC CORRESPONDS TO A DOUBLE
! C                                    SHIFT TO A NON-REAL POINT AND ITS
! C                                    COMPLEX CONJUGATE. THE POINT HAS
! C                                    MODULUS BND AND AMPLITUDE ROTATED
! C                                    BY 94 DEGREES FROM THE PREVIOUS
! C                                    SHIFT
         XXX = COSR*XX-SINR*YY
          YY = SINR*XX+COSR*YY
          XX = XXX
          SR = BND*XX
          SI = BND*YY
          U = -SR-SR
          V = BND*BND
! C                                  SECOND STAGE CALCULATION, FIXED
! C                                    QUADRATIC
          CALL ZRPQLB (i20*ICNT,NZ)
          IF (NZ.EQ.0) GO TO 130
! C                                  THE SECOND STAGE JUMPS DIRECTLY TO
! C                                    ONE OF THE THIRD STAGE ITERATIONS
! C                                    AND RETURNS HERE IF SUCCESSFUL.
! C                                  DEFLATE THE POLYNOMIAL, STORE THE
! C                                    ZERO OR ZEROS AND RETURN TO THE
! C                                    MAIN ALGORITHM.
          J = NDEG-N+1
          JJ = J+NDEG
          Z(J) = SZR
          Z(JJ) = SZI
          NN = NN-NZ
          N = NN-1
          DO 125 I=1,NN
           P(I) = QP(I)
  125     CONTINUE
          IF (NZ.EQ.1) GO TO 20
          Z(J+1) = RLZR
          Z(JJ+1) = RLZI
          GO TO 20
! C                                  IF THE ITERATION IS UNSUCCESSFUL
! C                                    ANOTHER QUADRATIC IS CHOSEN AFTER
! C                                    RESTORING K
  130     DO 135 I=1,N
      RK(I) = TEMP(I)
  135     CONTINUE
  140  CONTINUE
! C                                  RETURN WITH FAILURE IF NO
! C                                    CONVERGENCE WITH 20 SHIFTS
                IER = 131
! C                                  CONVERT ZEROS (Z) IN COMPLEX FORM
  145  DO 150 I=1,NDEG
        NPI= NDEG+I
         P(I) = Z(NPI)
  150  CONTINUE
       N2 = NDEG+NDEG
       J = NDEG
       DO 155 I=1,NDEG
         Z(N2-1) = Z(J)
                Z(N2) = P(J)
          N2 = N2-2
          J = J-1
  155  CONTINUE
       IF (IER .EQ. 0) GO TO 9005
! C                                  SET UNFOUND ROOTS TO MACHINE INFINITY
       N2 = 2*(NDEG-NN)+3
       DO 160 I=1,N
                Z(N2) = RINFP
          Z(N2+1) = RINFP
          N2 = N2+2
  160  CONTINUE
       GO TO 9000
  165  IER = 129
 9000  CONTINUE
        !srio      CALL UERTST (IER,6HZRPOLY)
      print *,"Error from math routine zrpoly. Called with NDEG,A: ",NDEG,A
 9005  RETURN
        END SUBROUTINE ZRPOLY


! C   IMSL ROUTINE NAME   - ZRPQLB                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - ZRPQLC,ZRPQLD,ZRPQLE,ZRPQLF,ZRPQLG,ZRPQLH,    
! C                           ZRPQLI                                      
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
    SUBROUTINE ZRPQLB (L2,NZ)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
       INTEGER(kind=ski)      L2,NZ
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)      N,NN,J,ITYPE,I,IFLAG
       REAL(kind=skr)     ARE,BETAS,BETAV,ETA,OSS,OTS,OTV,OVV,RMRE,SS,    &
                          TS,TSS,TV,TVV,VV
       REAL(kind=skr)     P(101),QP(101),RK(101),QK(101),SVK(101)
       REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                          A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                          SVU,SVV,UI,VI,S,ZERO
       LOGICAL              VPASS,SPASS,VTRY,STRY
       COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                          A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
       DATA                 ZERO/0.0D0/
! C                                  FIRST EXECUTABLE STATEMENT           
       NZ = 0
! C                                  COMPUTES UP TO L2 FIXED SHIFT        
! C                                    K-POLYNOMIALS, TESTING FOR         
! C                                    CONVERGENCE IN THE LINEAR OR       
! C                                    QUADRATIC CASE. INITIATES ONE OF   
! C                                    THE VARIABLE SHIFT ITERATIONS AND  
! C                                    RETURNS WITH THE NUMBER OF ZEROS   
! C                                    FOUND.                             
! C                                  L2 - LIMIT OF FIXED SHIFT STEPS      
! C                                  NZ -NUMBER OF ZEROS FOUND            
       BETAV = .25
       BETAS = .25
       OSS = SR
       OVV = V
! C                                  EVALUATE POLYNOMIAL BY SYNTHETIC     
! C                                    DIVISION                           
       CALL ZRPQLH (NN,U,V,P,QP,RA,RB)
       CALL ZRPQLE (ITYPE)
       DO 40 J=1,L2
! C                                  CALCULATE NEXT K POLYNOMIAL AND      
! C                                    ESTIMATE V                         
      CALL ZRPQLF (ITYPE)
          CALL ZRPQLE (ITYPE)
          CALL ZRPQLG (ITYPE,UI,VI)
          VV = VI
! C                                  ESTIMATE S                           
          SS = 0.
          IF (RK(N).NE.ZERO) SS = -P(NN)/RK(N)
          TV = 1.
          TS = 1.
          IF (J.EQ.1.OR.ITYPE.EQ.3) GO TO 35
! C                                  COMPUTE RELATIVE MEASURES OF         
! C                                    CONVERGENCE OF S AND V SEQUENCES   
          IF (VV.NE.0.) TV = ABS((VV-OVV)/VV)
          IF (SS.NE.0.) TS = ABS((SS-OSS)/SS)
! C                                  IF DECREASING, MULTIPLY TWO MOST     
! C                                    RECENT CONVERGENCE MEASURES        
          TVV = 1.
          IF (TV.LT.OTV) TVV = TV*OTV
          TSS = 1.
          IF (TS.LT.OTS) TSS = TS*OTS
! C                                  COMPARE WITH CONVERGENCE CRITERIA    
          VPASS = TVV.LT.BETAV
          SPASS = TSS.LT.BETAS
          IF (.NOT.(SPASS.OR.VPASS)) GO TO 35
! C                                  AT LEAST ONE SEQUENCE HAS PASSED THE 
! C                                    CONVERGENCE TEST. STORE VARIABLES  
! C                                    BEFORE ITERATING                   
          SVU = U
          SVV = V
          DO 5 I=1,N
           SVK(I) = RK(I)
    5     CONTINUE
          S = SS
! C                                  CHOOSE ITERATION ACCORDING TO THE    
! C                                    FASTEST CONVERGING SEQUENCE        
          VTRY = .FALSE.
          STRY = .FALSE.
          IF (SPASS.AND.((.NOT.VPASS).OR.TSS.LT.TVV)) GO TO 20
   10     CALL ZRPQLC (UI,VI,NZ)
          IF (NZ.GT.0) RETURN
! C                                  QUADRATIC ITERATION HAS FAILED. FLAG 
! C                                    THAT IT HAS BEEN TRIED AND         
! C                                    DECREASE THE CONVERGENCE           
! C                                    CRITERION.                         
          VTRY = .TRUE.
          BETAV = BETAV*.25
! C                                  TRY LINEAR ITERATION IF IT HAS NOT   
! C                                    BEEN TRIED AND THE S SEQUENCE IS   
! C                                    CONVERGING                         
          IF (STRY.OR.(.NOT.SPASS)) GO TO 25
          DO 15 I=1,N
           RK(I) = SVK(I)
   15     CONTINUE
   20     CALL ZRPQLD (S,NZ,IFLAG)
          IF (NZ.GT.0) RETURN
! C                                  LINEAR ITERATION HAS FAILED. FLAG    
! C                                    THAT IT HAS BEEN TRIED AND         
! C                                    DECREASE THE CONVERGENCE CRITERION 
          STRY = .TRUE.
          BETAS = BETAS*.25
          IF (IFLAG.EQ.0) GO TO 25
! C                                  IF LINEAR ITERATION SIGNALS AN       
! C                                    ALMOST DOUBLE REAL ZERO ATTEMPT    
! C                                    QUADRATIC INTERATION               
          UI = -(S+S)
          VI = S*S
          GO TO 10
! C                                  RESTORE VARIABLES                    
   25     U = SVU
          V = SVV
          DO 30 I=1,N
           RK(I) = SVK(I)
   30     CONTINUE
! C                                  TRY QUADRATIC ITERATION IF IT HAS    
! C                                    NOT BEEN TRIED AND THE V SEQUENCE  
! C                                    IS CONVERGING                      
          IF (VPASS.AND.(.NOT.VTRY)) GO TO 10
! C                                  RECOMPUTE QP AND SCALAR VALUES TO    
! C                                    CONTINUE THE SECOND STAGE          
          CALL ZRPQLH (NN,U,V,P,QP,RA,RB)
          CALL ZRPQLE (ITYPE)
   35     OVV = VV
          OSS = SS
          OTV = TV
          OTS = TS
   40  CONTINUE
       RETURN
        END SUBROUTINE ZRPQLB


! C   IMSL ROUTINE NAME   - ZRPQLC                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - ZRPQLE,ZRPQLF,ZRPQLG,ZRPQLH,ZRPQLI            
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
        SUBROUTINE ZRPQLC (UU,VV,NZ)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
       INTEGER(kind=ski)       NZ
       REAL(kind=skr)     UU,VV
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)       N,NN,J,I,ITYPE
       REAL(kind=skr)     ARE,EE,ETA,OMP,RELSTP,RMP,RMRE,T,ZM
       REAL(kind=skr)     P(101),QP(101),RK(101),QK(101),SVK(101)
       REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                          A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                          UI,VI,ZERO,PT01,ONE
       LOGICAL              TRIED
       COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                          A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
       DATA                 ZERO,PT01,ONE/0.0D0,0.01D0,1.0D0/
! C                                  FIRST EXECUTABLE STATEMENT           
       NZ = 0
! C                                  VARIABLE-SHIFT K-POLYNOMIAL          
! C                                    ITERATION FOR A QUADRATIC FACTOR   
! C                                    CONVERGES ONLY IF THE ZEROS ARE    
! C                                    EQUIMODULAR OR NEARLY SO           
! C                                  UU,VV - COEFFICIENTS OF STARTING     
! C                                    QUADRATIC                          
! C                                  NZ - NUMBER OF ZERO FOUND            
       TRIED = .FALSE.
       U = UU
       V = VV
       J = 0
! C                                  MAIN LOOP                            
    5  CALL ZRPQLI (ONE,U,V,SZR,SZI,RLZR,RLZI)
! C                                  RETURN IF ROOTS OF THE QUADRATIC ARE 
! C                                    REAL AND NOT CLOSE TO MULTIPLE OR  
! C                                    NEARLY EQUAL AND OF OPPOSITE SIGN  
       IF ( DABS(DABS(SZR)-DABS(RLZR)).GT.PT01*DABS(RLZR)) RETURN
! C                                  EVALUATE POLYNOMIAL BY QUADRATIC     
! C                                    SYNTHETIC DIVISION                 
       CALL ZRPQLH (NN,U,V,P,QP,RA,RB)
       RMP = DABS(RA-SZR*RB)+DABS(SZI*RB)
! C                                  COMPUTE A RIGOROUS BOUND ON THE      
! C                                    ROUNDING ERROR IN EVALUTING P      
       ZM = SQRT(ABS(SNGL(V)))
       EE = 2.*ABS(SNGL(QP(1)))
       T = -SZR*RB
       DO 10 I=2,N
        EE = EE*ZM+ABS(SNGL(QP(I)))
   10  CONTINUE
       EE = EE*ZM+ABS(SNGL(RA)+T)
       EE = (5.*RMRE+4.*ARE)*EE-(5.*RMRE+2.*ARE)*(ABS(SNGL(RA)+T)+        &
                ABS(SNGL(RB))*ZM)+2.*ARE*ABS(T)
! C                                  ITERATION HAS CONVERGED SUFFICIENTLY 
! C                                    IF THE POLYNOMIAL VALUE IS LESS    
! C                                    THAN 20 TIMES THIS BOUND           
       IF (RMP.GT.20.*EE) GO TO 15
       NZ = 2
       RETURN
   15  J = J+1
! C                                  STOP ITERATION AFTER 20 STEPS        
       IF (J.GT.20) RETURN
       IF (J.LT.2) GO TO 25
       IF (RELSTP.GT..01.OR.RMP.LT.OMP.OR.TRIED) GO TO 25
! C                                  A CLUSTER APPEARS TO BE STALLING THE 
! C                                    CONVERGENCE. FIVE FIXED SHIFT      
! C                                    STEPS ARE TAKEN WITH A U,V CLOSE   
! C                                    TO THE CLUSTER                     
       IF (RELSTP.LT.ETA) RELSTP = ETA
       RELSTP = SQRT(RELSTP)
       U = U-U*RELSTP
       V = V+V*RELSTP
       CALL ZRPQLH (NN,U,V,P,QP,RA,RB)
       DO 20 I=1,5
                CALL ZRPQLE (ITYPE)
          CALL ZRPQLF (ITYPE)
   20  CONTINUE
       TRIED = .TRUE.
       J = 0
   25  OMP = RMP
! C                                  CALCULATE NEXT K POLYNOMIAL AND NEW  
! C                                    U AND V                            
       CALL ZRPQLE (ITYPE)
       CALL ZRPQLF (ITYPE)
       CALL ZRPQLE (ITYPE)
       CALL ZRPQLG (ITYPE,UI,VI)
! C                                  IF VI IS ZERO THE ITERATION IS NOT   
! C                                    CONVERGING                         
       IF (VI.EQ.ZERO) RETURN
       RELSTP = DABS((VI-V)/VI)
       U = UI
       V = VI
       GO TO 5

        END SUBROUTINE ZRPQLC

! C   IMSL ROUTINE NAME   - ZRPQLD                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - NONE REQUIRED                                 
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       

        SUBROUTINE ZRPQLD (SSS,NZ,IFLAG)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     INTEGER(kind=ski)       NZ,IFLAG
       REAL(kind=skr)     SSS
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)      N,NN,J,I
       REAL(kind=skr)     ARE,EE,ETA,OMP,RMP,RMS,RMRE
       REAL(kind=skr)     P(101),QP(101),RK(101),QK(101),SVK(101)
       REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                          A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                          PV,RKV,T,S,ZERO,PT001
       COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                          A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
       DATA                 ZERO/0.0D0/,PT001/0.001D0/
! C                                  VARIABLE-SHIFT H POLYNOMIAL          
! C                                    ITERATION FOR A REAL ZERO SSS -    
! C                                    STARTING ITERATE                   
! C                                  NZ - NUMBER OF ZERO FOUND            
! C                                  IFLAG - FLAG TO INDICATE A PAIR OF   
! C                                    ZEROS NEAR REAL AXIS               
! C                                  FIRST EXECUTABLE STATEMENT           
       NZ = 0
       S = SSS
       IFLAG = 0
       J = 0
! C                                  MAIN LOOP                            
    5  PV = P(1)
! C                                  EVALUATE P AT S                      
       QP(1) = PV
       DO 10 I=2,NN
                PV = PV*S+P(I)
          QP(I) = PV
   10  CONTINUE
       RMP = DABS(PV)
! C                                  COMPUTE A RIGOROUS BOUND ON THE      
! C                                    ERROR IN EVALUATING P              
       RMS = DABS(S)
       EE = (RMRE/(ARE+RMRE))*ABS(SNGL(QP(1)))
       DO 15 I=2,NN
        EE = EE*RMS+ABS(SNGL(QP(I)))
   15  CONTINUE
! C                                  ITERATION HAS CONVERGED SUFFICIENTLY 
! C                                    IF THE POLYNOMIAL VALUE IS LESS    
! C                                    THAN 20 TIMES THIS BOUND           
       IF (RMP.GT.20.*((ARE+RMRE)*EE-RMRE*RMP)) GO TO 20
       NZ = 1
       SZR = S
       SZI = ZERO
       RETURN
   20  J = J+1
! C                                  STOP ITERATION AFTER 10 STEPS        
       IF (J.GT.10) RETURN
       IF (J.LT.2) GO TO 25
       IF (DABS(T).GT.PT001*DABS(S-T).OR.RMP.LE.OMP) GO TO 25
! C                                  A CLUSTER OF ZEROS NEAR THE REAL     
! C                                    AXIS HAS BEEN ENCOUNTERED RETURN   
! C                                    WITH IFLAG SET TO INITIATE A       
! C                                   QUADRATIC ITERATION
       IFLAG = 1
       SSS = S
       RETURN
! C                                  RETURN IF THE POLYNOMIAL VALUE HAS   
! C                                    INCREASED SIGNIFICANTLY            
   25  OMP = RMP
! C                                  COMPUTE T, THE NEXT POLYNOMIAL, AND  
! C                                    THE NEW ITERATE                    
       RKV = RK(1)
       QK(1) = RKV
       DO 30 I=2,N
          RKV = RKV*S+RK(I)
          QK(I) = RKV
   30  CONTINUE
       IF (DABS(RKV).LE.DABS(RK(N))*10.*ETA) GO TO 40
! C                                  USE THE SCALED FORM OF THE           
! C                                    RECURRENCE IF THE VALUE OF K AT S  
! C                                    IS NONZERO                         
       T = -PV/RKV
       RK(1) = QP(1)
       DO 35 I=2,N
        RK(I) = T*QK(I-1)+QP(I)
   35  CONTINUE
       GO TO 50
! C                                  USE UNSCALED FORM                    
   40  RK(1) = ZERO
       DO 45 I=2,N
        RK(I) = QK(I-1)
   45  CONTINUE
   50 RKV = RK(1)
       DO 55 I=2,N
        RKV = RKV*S+RK(I)
   55  CONTINUE
       T = ZERO
       IF (DABS(RKV).GT.DABS(RK(N))*10.*ETA) T = -PV/RKV
       S = S+T
       GO TO 5
        END SUBROUTINE ZRPQLD
                                                               
! C   IMSL ROUTINE NAME   - ZRPQLE                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - ZRPQLH                                        
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       

        SUBROUTINE ZRPQLE (ITYPE)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     INTEGER(kind=ski)      ITYPE
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)      N,NN
       REAL(kind=skr)     ARE,ETA,RMRE
       REAL(kind=skr)     P(101),QP(101),RK(101),QK(101),SVK(101)
       REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3, &
                          A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI
       COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                          A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
! C                                  THIS ROUTINE CALCULATES SCALAR       
! C                                    QUANTITIES USED TO COMPUTE THE     
! C                                    NEXT K POLYNOMIAL AND NEW          
! C                                    ESTIMATES OF THE QUADRATIC         
! C                                    COEFFICIENTS                       
! C                                  ITYPE - INTEGER VARIABLE SET HERE    
! C                                    INDICATING HOW THE CALCULATIONS    
! C                                    ARE NORMALIZED TO AVOID OVERFLOW   
! C                                  SYNTHETIC DIVISION OF K BY THE       
! C                                    QUADRATIC 1,U,V                    
! C                                  FIRST EXECUTABLE STATEMENT           
       CALL ZRPQLH (N,U,V,RK,QK,C,D)
       IF (DABS(C).GT.DABS(RK(N))*100.*ETA) GO TO 5
       IF (DABS(D).GT.DABS(RK(N-1))*100.*ETA) GO TO 5
       ITYPE = 3
! C                                  TYPE=3 INDICATES THE QUADRATIC IS    
! C                                    ALMOST A FACTOR OF K               
       RETURN
    5  IF (DABS(D).LT.DABS(C)) GO TO 10
       ITYPE = 2
! C                                  TYPE=2 INDICATES THAT ALL FORMULAS   
! C                                    ARE DIVIDED BY D                   
       E = RA/D
       F = C/D
       G = U*RB
       H = V*RB
       A3 = (RA+G)*E+H*(RB/D)
       A1 = RB*F-RA
       A7 = (F+U)*RA+H
       RETURN
   10  ITYPE = 1
! C                                  TYPE=1 INDICATES THAT ALL FORMULAS   
! C                                    ARE DIVIDED BY C                   
       E = RA/C
       F = D/C
       G = U*E
       H = V*RB
       A3 = RA*E+(H/C+G)*RB
       A1 = RB-RA*(D/C)
       A7 = RA+G*D+H*F
       RETURN
        END SUBROUTINE ZRPQLE

! C   IMSL ROUTINE NAME   - ZRPQLF                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - NONE REQUIRED                                 
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       

        SUBROUTINE ZRPQLF (ITYPE)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     INTEGER(kind=ski)      ITYPE
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)      N,NN,I
       REAL(kind=skr)     ARE,ETA,RMRE
       REAL(kind=skr)     P(101),QP(101),RK(101),QK(101),SVK(101)
       REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                          A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,TEMP,ZERO
       COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                          A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
       DATA                 ZERO/0.0D0/
! C                                  COMPUTES THE NEXT K POLYNOMIALS      
! C                                    USING SCALARS COMPUTED IN ZRPQLE   
! C                                  FIRST EXECUTABLE STATEMENT           
       IF (ITYPE.EQ.3) GO TO 20
       TEMP = RA
       IF (ITYPE.EQ.1) TEMP = RB
       IF (DABS(A1).GT.DABS(TEMP)*ETA*10.) GO TO 10
! C                                  IF A1 IS NEARLY ZERO THEN USE A      
! C                                    SPECIAL FORM OF THE RECURRENCE     
       RK(1) = ZERO
       RK(2) = -A7*QP(1)
      DO 5 I=3,N
       RK(I) = A3*QK(I-2)-A7*QP(I-1)
    5  CONTINUE
       RETURN
! C                                  USE SCALED FORM OF THE RECURRENCE    
   10  A7 = A7/A1
      A3 = A3/A1
       RK(1) = QP(1)
       RK(2) = QP(2)-A7*QP(1)
       DO 15 I=3,N
        RK(I) = A3*QK(I-2)-A7*QP(I-1)+QP(I)
   15  CONTINUE
       RETURN
! C                                  USE UNSCALED FORM OF THE RECURRENCE  
! C                                    IF TYPE IS 3                       
   20  RK(1) = ZERO
       RK(2) = ZERO
       DO 25 I=3,N
        RK(I) = QK(I-2)
   25  CONTINUE
       RETURN
        END SUBROUTINE ZRPQLF

! C   IMSL ROUTINE NAME   - ZRPQLG                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - NONE REQUIRED                                 
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
        SUBROUTINE ZRPQLG (ITYPE,UU,VV)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     INTEGER(kind=ski)      ITYPE
       REAL(kind=skr)     UU,VV
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)      N,NN
      REAL(kind=skr)         ARE,ETA,RMRE
       REAL(kind=skr)     P(101),QP(101),RK(101),QK(101),SVK(101)
       REAL(kind=skr)     SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                          A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                          A4,A5,B1,B2,C1,C2,C3,C4,TEMP,ZERO
       COMMON /ZRPQLJ/      P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                          A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
       DATA                 ZERO/0.0D0/
! C                                  COMPUTE NEW ESTIMATES OF THE         
! C                                    QUADRATIC COEFFICIENTS USING THE   
! C                                    SCALARS COMPUTED IN ZRPQLE         
! C                                  USE FORMULAS APPROPRIATE TO SETTING  
! C                                    OF TYPE.                           
! C                                  FIRST EXECUTABLE STATEMENT           
       IF (ITYPE.EQ.3) GO TO 15
       IF (ITYPE.EQ.2) GO TO 5
       A4 = RA+U*RB+H*F
       A5 = C+(U+V*F)*D
       GO TO 10
    5  A4 = (RA+G)*F+H
       A5 = (F+U)*C+V*D
! C                                  EVALUATE NEW QUADRATIC COEFFICIENTS. 
! C                                                                       
   10  B1 = -RK(N)/P(NN)
       B2 = -(RK(N-1)+B1*P(N))/P(NN)
       C1 = V*B2*A1
       C2 = B1*A7
       C3 = B1*B1*A3
       C4 = C1-C2-C3
       TEMP = A5+B1*A4-C4
       IF (TEMP.EQ.ZERO) GO TO 15
       UU = U-(U*(C3+C2)+V*(B1*A1+B2*A7))/TEMP
       VV = V*(1+C4/TEMP)
       RETURN
! C                                  IF TYPE=3 THE QUADRATIC IS ZEROED    
   15  UU = ZERO
       VV = ZERO
       RETURN
        END SUBROUTINE ZRPQLG
! C   IMSL ROUTINE NAME   - ZRPQLH                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - NONE REQUIRED                                 
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       

        SUBROUTINE ZRPQLH (NN,U,V,P,Q,RA,RB)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     INTEGER(kind=ski)       NN
       REAL(kind=skr)     P(NN),Q(NN),U,V,RA,RB
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)      I
       REAL(kind=skr)     C
! C                                  DIVIDES P BY THE QUADRATIC 1,U,V     
! C                                    PLACING THE QUOTIENT IN Q AND THE  
! C                                    REMAINDER IN A,B                   
! C                                  FIRST EXECUTABLE STATEMENT           
       RB = P(1)
       Q(1) = RB
       RA = P(2)-U*RB
       Q(2) = RA
       DO 5 I=3,NN
          C = P(I)-U*RA-V*RB
          Q(I) = C
          RB = RA
          RA = C
    5  CONTINUE
       RETURN
        END SUBROUTINE ZRPQLH

! C   IMSL ROUTINE NAME   - ZRPQLI                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JANUARY 1, 1978                               
! C                                                                       
! C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE        
! C                           ZRPOLY                                      
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - NONE REQUIRED                                 
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       

        SUBROUTINE ZRPQLI (RA,B1,C,SR,SI,RLR,RLI)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     REAL(kind=skr)     RA,B1,C,SR,SI,RLR,RLI
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       REAL(kind=skr)     RB,D,E,ZERO,ONE,TWO
       DATA                 ZERO,ONE,TWO/0.0D0,1.0D0,2.0D0/
! C                                  CALCULATE THE ZEROS OF THE QUADRATIC 
! C                                    A*Z**2 + B1*Z + C. THE QUADRATIC   
! C                                    FORMULA, MODIFIED TO AVOID         
! C                                    OVERFLOW, IS USED TO FIND THE      
! C                                    LARGER ZERO IF THE ZEROS ARE REAL  
! C                                    AND BOTH ZEROS ARE COMPLEX.        
! C                                  THE SMALLER REAL ZERO IS FOUND       
! C                                    DIRECTLY FROM THE PRODUCT OF THE   
! C                                    ZEROS C/A                          
! C                                  FIRST EXECUTABLE STATEMENT           
       IF (RA.NE.ZERO) GO TO 10
       SR = ZERO
       IF (B1.NE.ZERO) SR = -C/B1
       RLR = ZERO
    5  SI = ZERO
       RLI = ZERO
       RETURN
   10  IF (C.NE.ZERO) GO TO 15
       SR = ZERO
       RLR = -B1/RA
       GO TO 5
! C                                  COMPUTE DISCRIMINANT AVOIDING        
! C                                    OVERFLOW                           
   15  RB = B1/TWO
       IF (DABS(RB).LT.DABS(C)) GO TO 20
       E = ONE-(RA/RB)*(C/RB)
       D = DSQRT(DABS(E))*DABS(RB)
       GO TO 25
   20  E = RA
       IF (C.LT.ZERO) E = -RA
       E = RB*(RB/DABS(C))-E
       D = DSQRT(DABS(E))*DSQRT(DABS(C))
   25  IF (E.LT.ZERO) GO TO 30
! C                                 REAL ZEROS
       IF (RB.GE.ZERO) D = -D
       RLR = (-RB+D)/RA
       SR = ZERO
       IF (RLR.NE.ZERO) SR = (C/RLR)/RA
       GO TO 5
! C                                  COMPLEX CONJUGATE ZEROS              
   30  SR = -RB/RA
       RLR = SR
       SI = DABS(D/RA)
       RLI = -SI
       RETURN
        END SUBROUTINE ZRPQLI


! C   IMSL ROUTINE NAME   - DBCEVL                                        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
! C   COMPUTER            - VAX/DOUBLE                                    
! C                                                                       
! C   LATEST REVISION     - JUNE 1, 1982                                  
! C                                                                       
! C   PURPOSE             - BICUBIC SPLINE MIXED PARTIAL DERIVATIVE       
! C                           EVALUATOR                                   
! C                                                                       
! C   USAGE               - CALL DBCEVL (X,NX,Y,NY,C,IC,XL,YL,PDS,IER)    
! C                                                                       
! C   ARGUMENTS    X      - VECTOR OF LENGTH NX. (INPUT) X MUST BE        
! C                           ORDERED SO THAT X(I) .LT. X(I+1) FOR        
! C                           I=1,...,NX-1.                               
! C                NX     - NUMBER OF ELEMENTS IN X. (INPUT) NX MUST BE   
! C                           .GE. 2.                                     
! C                Y      - VECTOR OF LENGTH NY. (INPUT) Y MUST BE        
! C                           ORDERED SO THAT Y(J) .LT. Y(J+1) FOR        
! C                           J=1,...,NY-1.                               
! C                NY     - NUMBER OF ELEMENTS IN Y. (INPUT) NY MUST BE   
! C                           .GE. 2.                                     
! C                         NOTE - THE COORDINATE PAIRS (X(I),Y(J)), FOR  
! C                           I=1,...,NX AND J=1,...,NY, GIVE THE POINTS  
! C                           WHERE THE FUNCTION VALUES ARE DEFINED.      
! C                C      - ARRAY OF SPLINE COEFFICIENTS. (INPUT)         
! C                           C IS OF DIMENSION 2 BY NX BY 2 BY NY.       
! C                           THE SPLINE COEFFICIENTS CAN BE COMPUTED BY  
! C                           IMSL SUBROUTINE IBCCCU.                     
! C                           (NOTE - C IS TREATED INTERNALLY AS A        
! C                            2 BY NX BY 2*NY ARRAY BECAUSE CERTAIN      
! C                            ENVIRONMENTS DO NOT PERMIT QUADRUPLY-      
! C                            DIMENSIONED ARRAYS.  IN THESE              
! C                            ENVIRONMENTS THE CALLING PROGRAM MAY       
! C                            DIMENSION C IN THE SAME MANNER.)           
! C                IC     - SECOND DIMENSION OF ARRAY C EXACTLY AS        
! C                           SPECIFIED IN THE DIMENSION STATEMENT        
! C                           (INPUT).  IC MUST BE .GE. NX.               
! C                XL,YL  - (XL,YL) IS THE POINT AT WHICH THE MIXED       
! C                           PARTIAL DERIVATIVES OF THE SPLINE ARE TO BE 
! C                           EVALUATED. (INPUT)                          
! C                PDS    - VECTOR OF LENGTH 6 CONTAINING THE PARTIAL     
! C                           DERIVATIVES OF THE BICUBIC SPLINE, S(X,Y),  
! C                           EVALUATED AT X=XL AND Y=YL. (OUTPUT)        
! C                             PDS(1) = S(XL,YL)                         
! C                             PDS(2) = DS/DX                            
! C                             PDS(3) = DS/DY                            
! C                             PDS(4) = D(DS/DX)/DY                      
! C                             PDS(5) = D(DS/DX)/DX                      
! C                             PDS(6) = D(DS/DY)/DY.                     
! C                IER    - ERROR PARAMETER. (OUTPUT)                     
! C                         WARNING ERROR                                 
! C                           IER = 33, XL IS LESS THAN X(1).             
! C                           IER = 34, YL IS LESS THAN Y(1).             
! C                           IER = 35, XL IS GREATER THAN X(NX).         
! C                           IER = 36, YL IS GREATER THAN Y(NY).         
! C                                                                       
! C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
! C                       - SINGLE/H36,H48,H60                            
! C                                                                       
! C   REQD. IMSL ROUTINES - UERTST,UGETIO                                 
! C                                                                       
! C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
! C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
! C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
! C                                                                       
! C   COPYRIGHT           - 1982 BY IMSL, INC. ALL RIGHTS RESERVED.       
! C                                                                       
! C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
! C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
! C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
! C                                                                       
! C-----------------------------------------------------------------------
! C                                                                       
        SUBROUTINE DBCEVL (X,NX,Y,NY,C,IC,XL,YL,PDS,IER)
! C                                  SPECIFICATIONS FOR ARGUMENTS         
     INTEGER(kind=ski)  :: NX,NY,IC,IER
       REAL(kind=skr)     :: X(1),Y(1),C(2,IC,1),XL,YL,PDS(6)
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
       INTEGER(kind=ski)  :: I,J,K,KM1,KP1,KP2,LXPL,LX,LY,L,LXP1
       INTEGER(kind=ski)  :: WARNINGDISPLAY=0
       REAL(kind=skr)     :: HX,HY,SUX(2),SUY(2),SU(2),SVX(2),SV(2),SXY(2),  &
                          U,V,SPLN0,SPLN1,SPLN2,S0,SH,SP0,SPH,H,D
       SPLN0(S0,SH,SP0,SPH,H,D) = S0+D*(H*SP0+D*(3.D0*(SH-S0)-            &
        (SPH+2.D0*SP0)*H+D*(2.D0*(S0-SH)+(SPH+SP0)*H)))
       SPLN1(S0,SH,SP0,SPH,H,D) = SP0+D*(6.D0*(SH-S0)/H-2.D0*             &
        (SPH+2.D0*SP0)+3.D0*D*(2.D0*(S0-SH)/H+(SPH+SP0)))
       SPLN2(S0,SH,SP0,SPH,H,D) = 6.D0*(SH-S0)/H**2-2.D0*                 &
        (SPH+2.D0*SP0)/H+D*(2.D0*(S0-SH)/H**2+(SPH+SP0)/H)*6.D0
! C                                  FIRST EXECUTABLE STATEMENT           
       IER = 0
       IF (XL.LT.X(1)) IER = 33
       DO 5 I=2,NX
          LX = I-1
          IF (XL.LE.X(I)) GO TO 10
    5  CONTINUE
       IER = 35
   10  IF (YL.LT.Y(1)) IER = 34
       DO 15 J=2,NY
                LY = J-1
          IF (YL.LE.Y(J)) GO TO 20
   15  CONTINUE
       IER = 36
   20  LXP1 = LX+1
       HX = X(LXP1)-X(LX)
       HY = Y(LY+1)-Y(LY)
       U = (XL-X(LX))/HX
       V = (YL-Y(LY))/HY
       K = 2*LY
       KP1 = K+1
       KP2 = K+2
       KM1 = K-1
       DO 25 L=1,2
                LXPL = LX-1+L
          I = 2*(LY-1+L)
          J = I-1
          SUX(L) = SPLN1(C(1,LX,J),C(1,LXP1,J),C(2,LX,J),C(2,LXP1,J),HX,U)
          SXY(L) = SPLN1(C(1,LX,I),C(1,LXP1,I),C(2,LX,I),C(2,LXP1,I),HX,U)
          SU(L) = SPLN0(C(1,LX,J),C(1,LXP1,J),C(2,LX,J),C(2,LXP1,J),HX,U)
          SUY(L) = SPLN0(C(1,LX,I),C(1,LXP1,I),C(2,LX,I),C(2,LXP1,I),HX,U)
          SV(L) = SPLN0(C(1,LXPL,KM1),C(1,LXPL,KP1),C(1,LXPL,K),C(1,LXPL,KP2),HY,V)
          SVX(L) = SPLN0(C(2,LXPL,KM1),C(2,LXPL,KP1),C(2,LXPL,K),C(2,LXPL,KP2),HY,V)
   25  CONTINUE
       PDS(1) = SPLN0(SV(1),SV(2),SVX(1),SVX(2),HX,U)
       PDS(2) = SPLN1(SV(1),SV(2),SVX(1),SVX(2),HX,U)
       PDS(3) = SPLN1(SU(1),SU(2),SUY(1),SUY(2),HY,V)
       PDS(4) = SPLN1(SUX(1),SUX(2),SXY(1),SXY(2),HY,V)
       PDS(5) = SPLN2(SV(1),SV(2),SVX(1),SVX(2),HX,U)
       PDS(6) = SPLN2(SU(1),SU(2),SUY(1),SUY(2),HY,V)
      !srio IF (IER.GT.0) CALL UERTST(IER,6HDBCEVL)                           
       IF ((IER.GT.0).AND.(WARNINGDISPLAY.NE.0)) THEN 
           print *,"              "
           print *,"Warning: Out of range in math routine dbcevl. "
           SELECT CASE (iEr)
             CASE(33)
               print *,"              IER = 33, XL IS LESS THAN X(1)."
               print *,"              XL,X(1),X(NX): ",XL,X(1),X(NX)
             CASE(34)
               print *,"              IER = 34, YL IS LESS THAN Y(1)."
               print *,"              YL,Y(1),Y(NY): ",YL,Y(1),Y(NY)
             CASE(35)
               print *,"              IER = 35, XL IS GREATER THAN X(NX)."
               print *,"              XL,X(1),X(NX): ",XL,X(1),X(NX)
             CASE(36)
               print *,"              IER = 36, YL IS GREATER THAN Y(NY)."
               print *,"              YL,Y(1),Y(NY): ",YL,Y(1),Y(NY)
             CASE DEFAULT
           END SELECT
           !print *,"              XL,X(1),X(NX): ",XL,X(1),X(NX)
           !print *,"              YL,Y(1),Y(NY): ",YL,Y(1),Y(NY)
           print *,"              "
        END IF

       RETURN
        END SUBROUTINE DBCEVL
! C>>>PNP2                                                                
! C     ..................................................................
! C                                                                       
! C        SUBROUTINE PNPOLY                                              
! C                                                                       
! C        PURPOSE                                                        
! C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON            
! C                                                                       
! C        USAGE                                                          
! C           CALL PNPOLY (PX, PY, X, Y, N, INOUT )                       
! C                                                                       
! C        DESCRIPTION OF THE PARAMETERS                                  
! C           PX      - X-COORDINATE OF POINT IN QUESTION.                
! C           PY      - Y-COORDINATE OF POINT IN QUESTION.                
! C           X       - N LONG VECTOR CONTAINING X-COORDINATES OF         
! C                     VERTICES OF POLYGON.                              
! C           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF         
! C                     VERTICES OF POLYGON.                              
! C           N       - NUMBER OF VERTICES IN THE POLYGON.                
! C           INOUT   - THE SIGNAL RETURNED:                              
! C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,        
! C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,     
! C                      1 IF THE POINT IS INSIDE OF THE POLYGON.         
! C                                                                       
! C        REMARKS                                                        
! C           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE    
! C           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE    
! C           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED   
! C           BY THE LINE FORMING THE POLYGON.                            
! C           THE FIRST POINT MAY OPTIONALLY BE REPEATED, IF SO N MAY     
! C           OPTIONALLY BE INCREASED BY 1.                               
! C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING      
! C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX    
! C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING   
! C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.              
! C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.         
! C           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.    
! C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.   
! C                                                                       
! C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  
! C           NONE                                                        
! C                                                                       
! C        METHOD                                                         
! C           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT    
! C           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF     
! C           TIMES, THEN THE POINT IS INSIDE THE POLYGON.                
! C                                                                       
! C     ..................................................................
! C                                                                       
        SUBROUTINE PNPOLY (PX,PY,X,Y,N,INOUT)
     IMPLICIT REAL(kind=skr)  (A-E,G-H,O-Z)
       IMPLICIT INTEGER(kind=ski)  (F,I-N)
! C
       DIMENSION     X(N),Y(N)
       LOGICAL     IX,IY,JX,JY,EOR
! C     EXCLUSIVE OR FUNCTION.                                          
       EOR(IX,IY)=(IX.OR.IY).AND..NOT.(IX.AND.IY)
       INOUT=-1
       DO 4 I=1,N
        XI=X(I)-PX
        YI=Y(I)-PY
! C     CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.          
        IF (XI.EQ.0.0.AND.YI.EQ.0.0) GO TO 2
! C     J IS NEXT VERTEX NUMBER OF POLYGON.                             
        J=1+MOD(I,N)
        XJ=X(J)-PX
        YJ=Y(J)-PY
! C     IS THIS LINE OF 0 LENGTH ?                                      
        IF (XI.EQ.XJ.AND.YI.EQ.YJ) GO TO 4
        IX=XI.GE.0.0
        IY=YI.GE.0.0
        JX=XJ.GE.0.0
        JY=YJ.GE.0.0
! C     CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.           
        IF (XI.EQ.0.0.AND.XJ.EQ.0.0.AND.EOR(IY,JY)) GO TO 2
! C     CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.         
        IF (YI.EQ.0.0.AND.YJ.EQ.0.0.AND.EOR(IX,JX)) GO TO 2
! C     CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT 
! C     OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).                         
        IF (.NOT.((IY.OR.JY).AND.EOR(IX,JX))) GO TO 4
! C     DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY)
        IF (.NOT.(IY.AND.JY.AND.EOR(IX,JX))) GO TO 1
        INOUT=-INOUT
      GO TO 4
        1 IF ((YI*XJ-XI*YJ)/(XJ-XI)) 4,2,3
        2  INOUT=0
       RETURN
3      INOUT=-INOUT
4      CONTINUE
       RETURN
        END SUBROUTINE PNPOLY

!
! ibcccu and ibcdcu used by shadow_preprocessors
!

!C   IMSL ROUTINE NAME   - IBCCCU                                        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
!C   COMPUTER            - VAX/DOUBLE                                    
!C                                                                       
!C   LATEST REVISION     - JUNE 1, 1982                                  
!C                                                                       
!C   PURPOSE             - BICUBIC SPLINE TWO-DIMENSIONAL COEFFICIENT    
!C                           CALCULATOR                                  
!C                                                                       
!C   USAGE               - CALL IBCCCU (F,X,NX,Y,NY,C,IC,WK,IER)         
!C                                                                       
!C   ARGUMENTS    F      - NX BY NY MATRIX CONTAINING THE FUNCTION       
!C                           VALUES. (INPUT) F(I,J) IS THE FUNCTION VALUE
!C                           AT THE POINT (X(I),Y(J)) FOR I=1,...,NX AND 
!C                           J=1,...,NY.                                 
!C                X      - VECTOR OF LENGTH NX. (INPUT) X MUST BE        
!C                           ORDERED SO THAT X(I) .LT. X(I+1) FOR        
!C                           I=1,...,NX-1.                               
!C                NX     - NUMBER OF ELEMENTS IN X. (INPUT) NX MUST BE   
!C                           .GE. 4.                                     
!C                Y      - VECTOR OF LENGTH NY. (INPUT) Y MUST BE        
!C                           ORDERED SO THAT Y(J) .LT. Y(J+1) FOR        
!C                           J=1,...,NY-1.                               
!C                NY     - NUMBER OF ELEMENTS IN Y. (INPUT) NY MUST BE   
!C                           .GE. 4.                                     
!C                         NOTE - THE COORDINATE PAIRS (X(I),Y(J)), FOR  
!C                           I=1,...,NX AND J=1,...,NY, GIVE THE POINTS  
!C                           WHERE THE FUNCTION VALUES F(I,J) ARE        
!C                           DEFINED.                                    
!C                C      - ARRAY OF SPLINE COEFFICIENTS. (OUTPUT)        
!C                           C IS OF DIMENSION 2 BY NX BY 2 BY NY.       
!C                           AT THE POINT (X(I),Y(J))                    
!C                             C(1,I,1,J) = S                            
!C                             C(2,I,1,J) = DS/DX                        
!C                             C(1,I,2,J) = DS/DY                        
!C                             C(2,I,2,J) = D(DS/DX)/DY                  
!C                           WHERE S(X,Y) IS THE SPLINE APPROXIMATION.   
!C                           (NOTE - C IS TREATED INTERNALLY AS A        
!C                             2 BY NX BY 2*NY ARRAY BECAUSE CERTAIN     
!C                             ENVIRONMENTS DO NOT PERMIT QUADRUPLY-     
!C                             DIMENSIONED ARRAYS.  IN THESE             
!C                             ENVIRONMENTS THE CALLING PROGRAM MAY      
!C                             DIMENSION C IN THE SAME MANNER.)          
!C                IC     - ROW DIMENSION OF MATRIX F AND SECOND          
!C                           DIMENSION OF ARRAY C EXACTLY AS             
!C                           SPECIFIED IN THE DIMENSION STATEMENT.       
!C                           (INPUT). IC MUST BE .GE. NX.                
!C                WK     - WORK VECTOR OF LENGTH                         
!C                           2*NX*NY+2*MAX(NX,NY)                        
!C                IER    - ERROR PARAMETER. (OUTPUT)                     
!C                         TERMINAL ERROR                                
!C                           IER = 129, IC IS LESS THAN NX               
!C                           IER = 130, NX IS LESS THAN 4                
!C                           IER = 131, NY IS LESS THAN 4                
!C                           IER = 132, X OR Y ARE NOT ORDERED SO THAT   
!C                             X(I) .LT. X(I+1) AND                      
!C                             Y(I) .LT. Y(I+1)                          
!C                                                                       
!C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
!C                       - SINGLE/H36,H48,H60                            
!C                                                                       
!C   REQD. IMSL ROUTINES - IBCDCU,UERTST,UGETIO                          
!C                                                                       
!C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
!C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
!C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
!C                                                                       
!C   COPYRIGHT           - 1982 BY IMSL, INC. ALL RIGHTS RESERVED.       
!C                                                                       
!C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
!C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
!C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
SUBROUTINE IBCCCU (F,X,NX,Y,NY,C,IC,WK,IER)                       
      integer(kind=ski) :: NX,NY,IC,IER,iTmp,iTmp2
      real(kind=skr),dimension(1)      :: X,Y,WK
      real(kind=skr),dimension(IC,1)   :: F
      real(kind=skr),dimension(2,IC,1) :: C
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      integer(kind=ski) :: IWK
!C                                  FIRST EXECUTABLE STATEMENT           
      IER = 129                                                         
      IF (IC .LT. NX) GO TO 9000                                        
      IER = 130                                                         
      IF (NX .LT. 4) GO TO 9000                                         
      IER = 131                                                         
      IF (NY .LT. 4) GO TO 9000                                         
      IWK = 2*NY*NX                                                     
      CALL IBCDCU(X,F,NX,NY,WK(IWK+1),WK,IC,NY,IER)                     
      IF (IER .GT. 0) GO TO 9000                                        
      !CALL IBCDCU(Y,WK,NY,2*NX,WK(IWK+1),C,NY,2*IC,IER)                 
      iTmp = 2*NX
      iTmp2 = 2*IC
      CALL IBCDCU(Y,WK,NY,iTmp,WK(IWK+1),C,NY,iTmp2,IER)                 
      IF (IER .EQ. 0) GO TO 9005                                        
 9000 CONTINUE                                                          
      !CALL UERTST(IER,6HIBCCCU)                                         
      PRINT *,'Error: IBCCCU: Error in math routine ibcccu.'
      !STOP 'Aborted'
 9005 RETURN                                                            
END SUBROUTINE IBCCCU                                                              
!C   IMSL ROUTINE NAME   - IBCDCU                                        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
!C   COMPUTER            - VAX/DOUBLE                                    
!C                                                                       
!C   LATEST REVISION     - JUNE 1, 1982                                  
!C                                                                       
!C   PURPOSE             - NUCLEUS CALLED ONLY BY IMSL SUBROUTINE IBCCCU 
!C                                                                       
!C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
!C                       - SINGLE/H36,H48,H60                            
!C                                                                       
!C   REQD. IMSL ROUTINES - NONE REQUIRED                                 
!C                                                                       
!C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
!C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
!C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
!C                                                                       
!C   COPYRIGHT           - 1982 BY IMSL, INC. ALL RIGHTS RESERVED.       
!C                                                                       
!C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
!C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
!C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
SUBROUTINE IBCDCU (TAU,GTAU,N,M,W,VS,IC1,IC2,IER)                 
!C                                  SPECIFICATIONS FOR ARGUMENTS         
      integer(kind=ski) :: N,M,IC1,IC2,IER                                
      real(kind=skr),dimension(N)      :: TAU
      real(kind=skr),dimension(IC1,1)  :: GTAU
      real(kind=skr),dimension(N,2)    :: W
      real(kind=skr),dimension(IC2,2,2):: VS
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      integer(kind=ski) :: I,JJ,JM1,JP1,J,K,LIM,LL,LP1,NM1
      real(kind=skr) :: AA,BB,C1,C2,CC,DD,DTAU,G,H,RATIO,U,XILIM       
!C                                  FIRST EXECUTABLE STATEMENT           
      LIM = N-3                                                         
      NM1 = N-1                                                         
      LP1 = LIM+1                                                       
      IER = 132                                                         
      W(2,1) = TAU(3)-TAU(1)                                            
      IF (W(2,1).LE.0.0D0) RETURN                                       
      DO 5 K=1,M                                                        
         VS(K,1,1) = GTAU(1,K)                                          
    5 CONTINUE                                                          
      XILIM = TAU(1)                                                    
      IF (LIM.LT.2) GO TO 20                                            
      XILIM = TAU(N-2)                                                  
      DO 15 I=2,LIM                                                     
         J = I+1                                                        
         W(J,1) = TAU(I+2)-TAU(J)                                       
         IF (W(J,1).LE.0.0D0) RETURN                                    
         DO 10 K=1,M                                                    
   10    VS(K,1,I) = GTAU(J,K)                                          
   15 CONTINUE                                                          
   20 W(LP1,1) = TAU(N)-XILIM                                           
      IF (W(LP1,1).LE.0.0D0) RETURN                                     
      DO 25 K=1,M                                                       
   25 VS(K,1,LP1) = GTAU(N,K)                                           
      DO 35 I=2,LP1                                                     
         DO 30 K=1,M                                                    
   30    VS(K,2,I) = (VS(K,1,I)-VS(K,1,I-1))/W(I,1)                     
   35 CONTINUE                                                          
      DTAU = TAU(2)-TAU(1)                                              
      RATIO = DTAU/W(2,1)                                               
      W(1,2) = (RATIO-1.D0)**2                                          
      W(1,1) = RATIO*(RATIO-1.D0)                                       
      C1 = RATIO*(2.D0*RATIO-3.D0)                                      
      DO 40 K=1,M                                                       
   40 VS(K,2,1) = (GTAU(2,K)-GTAU(1,K))/DTAU+VS(K,2,2)*C1               
      IF (LIM.LT.2) GO TO 55                                            
      DO 50 I=2,LIM                                                     
         J = I+1                                                        
         JJ = I-1                                                       
         G = -W(J,1)/W(JJ,2)                                            
         C1 = 3.D0*W(I,1)                                               
         C2 = 3.D0*W(J,1)                                               
         DO 45 K=1,M                                                    
   45    VS(K,2,I) = G*VS(K,2,JJ)+C1*VS(K,2,J)+C2*VS(K,2,I)             
         W(I,2) = G*W(JJ,1)+2.D0*(W(I,1)+W(J,1))                        
   50 CONTINUE                                                          
   55 DTAU = TAU(N-1)-XILIM                                             
      RATIO = DTAU/W(LP1,1)                                             
      G = -(RATIO-1.D0)**2/W(LIM,2)                                     
      W(LP1,2) = RATIO*(RATIO-1.D0)                                     
      C1 = RATIO*(2.D0*RATIO-3.D0)                                      
      DO 60 K=1,M                                                       
   60 VS(K,2,LP1) = (GTAU(N-1,K)-VS(K,1,LIM))/DTAU+VS(K,2,LP1)*C1       
      W(LP1,2) = G*W(LIM,1)+W(LP1,2)                                    
      DO 65 K=1,M                                                       
   65 VS(K,2,LP1) = (G*VS(K,2,LIM)+VS(K,2,LP1))/W(LP1,2)                
      J = LIM                                                           
   70 DO 75 K=1,M                                                       
   75 VS(K,2,J) = (VS(K,2,J)-W(J,1)*VS(K,2,J+1))/W(J,2)                 
      J = J-1                                                           
      IF (J.GT.0) GO TO 70                                              
      DO 95 K=1,M                                                       
         DO 85 JJ=1,N                                                   
            J = N+1-JJ                                                  
            JM1 = J-1                                                   
            IF (J.EQ.N) JM1 = J-2                                       
            IF (J.EQ.1) JM1 = J                                         
            DO 80 LL=1,2                                                
               VS(K,LL,J) = VS(K,LL,JM1)                                
   80       CONTINUE                                                    
   85    CONTINUE                                                       
         DO 90 J=2,NM1,LIM                                              
            JM1 = J-1                                                   
            JP1 = J+1                                                   
            IF (JM1.EQ.2) JM1 = 1                                       
            IF (JP1.EQ.NM1) JP1 = N                                     
            H = TAU(JP1)-TAU(JM1)                                       
            U = TAU(J)-TAU(JM1)                                         
            AA = VS(K,1,JM1)                                            
            BB = VS(K,2,JM1)                                            
            CC = (3.D0*(VS(K,1,JP1)-VS(K,1,JM1))/H-(VS(K,2,JP1)+ &
            2.D0*VS(K,2,JM1)))/H                                        
            DD = (2.D0*(VS(K,1,JM1)-VS(K,1,JP1))/H+(VS(K,2,JP1)+ &
            VS(K,2,JM1)))/H**2                                          
            VS(K,1,J) = AA+U*(BB+U*(CC+DD*U))                           
            VS(K,2,J) = BB+U*(2.D0*CC+3.D0*DD*U)                        
   90    CONTINUE                                                       
   95 CONTINUE                                                          
      IER = 0                                                           
      RETURN                                                            
END SUBROUTINE IBCDCU

End Module shadow_math
