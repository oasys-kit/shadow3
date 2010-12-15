!----
!---- MODULE:  math
!----
!---- Contains the mathematical routines in Shadow
!----
!---- By now, it has only the routines used in the geometrical source: 
!----      wran, rotate, spl_int, atan_2, gauss
!----      and the vectorial calculus tools:
!----      scalar, dot, cross, norm, vector, versor, proj, sum
!----      vdist
!---- 
!---- TODO: Much of the functionality of these routines is now in 
!----       fortran 95. It should be desirable to use then the intrinsic
!----       f95 routines, but this is left for the future.
!---- 
!----  Example of use: see test_math
!
!

Module math
    !---- Use Modules ----!

    !---- Variables ----!
    implicit none

    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    public :: wran

    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public :: rotate, spl_int, atan_2, gauss
    public :: scalar, dot, cross, norm, vector, versor, proj, sum, vdist


    !---- List of private functions ----!
    !---- List of private subroutines ----!



    !---- Definitions ----!


    !---- Interfaces ----!

  Contains
    !
    !---- Public Routines (Functions and Subroutines) ----!
    !


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


	!DOUBLE PRECISION FUNCTION WRAN (ISEED)
	REAL(KIND=KIND(1.0D0))  FUNCTION WRAN (ISEED)
     	! IMPLICIT	REAL*8		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

	INTEGER(KIND=4)	ISEED
	INTEGER(KIND=4)	:: first=1

!!
!! This part is for the built-in f95 random generator
!!


        if (first.eq.1) then
	   XX = dble(ISEED)
           XX = XX/(10**int(1+log10(XX)))
           ! write(*,*) "WRAN: Initializing generator with: ",XX
           CALL RANDOM_NUMBER(HARVEST=XX)
           first = 0
        end if
        CALL RANDOM_NUMBER(WRAN)

!!
!! This part is for calling the C generator using the wranc.c wrapper
!! This part produces the same results than Shadow 2.x
!!
!! TODO: COMMENTED IN WINDOWS
!!	WRAN = WRANC(ISEED)

	RETURN
    END FUNCTION WRAN
    !
    !
    !

! C+++
! C SUBROUTINE ROTATE
! C
! C PURPOSE: GENERATE THE EULERIAN ANGLES FROM THE ELECTRON COORDINATES TO
! C THE SHADOW REFERENCE FRAME.
! C
! C----
         SUBROUTINE ROTATE (VIN,PSI,THETA,PHI,VOUT)
          ! IMPLICIT  REAL*8 (A-H,O-Z)
          ! IMPLICIT INTEGER(KIND=4) (I-N)

	  IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	  IMPLICIT INTEGER(KIND=4)        (I-N)

          DIMENSION VIN(3), VOUT(3)
! C
! C To avoid useles recalculations
! C
          COSPSI = COS (PSI)
          COSTHE = COS (THETA)
          COSPHI = COS (PHI)
          SINPSI = SIN (PSI)
          SINTHE = SIN (THETA)
          SINPHI = SIN (PHI)
! C
          VOUT(1)= (COSPSI*COSPHI-COSTHE*SINPHI*SINPSI)*VIN(1)+&
         (-SINPSI*COSPHI-COSTHE*SINPHI*COSPSI)*VIN(2)+&
         (SINTHE*SINPHI)*VIN(3)
! C
          VOUT(2)= (COSPSI*SINPHI+COSTHE*COSPHI*SINPSI)*VIN(1)+&
         (-SINPSI*SINPHI+COSTHE*COSPHI*COSPSI)*VIN(2)+&
         (-SINTHE*COSPHI)*VIN(3)
! C
          VOUT(3)= (SINTHE*SINPSI)*VIN(1)+(SINTHE*COSPSI)*VIN(2) &
         +COSTHE*VIN(3)

    END SUBROUTINE ROTATE

    !
    !
    !

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
      ! IMPLICIT	REAL*8	(A-H,O-Z)
      ! IMPLICIT INTEGER(KIND=4) (I-N)

      IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
      IMPLICIT INTEGER(KIND=4)        (I-N)

      REAL(KIND=KIND(1.0D0))   G(5,N), X, Y, Z
      INTEGER(KIND=4)  I, IER, N
      GMAX	= MAX(G(1,1),G(1,N))
      GMIN	= MIN(G(1,1),G(1,N))
      IF ((X .LT. GMIN) .OR. (X .GT. GMAX)) THEN
        WRITE(6,*) 'SPL_INT: x is outside the interpolation range.'
        IER = 1
        RETURN
      ELSE IF (X .EQ. G(1,N)) THEN
        I = N-1 
        GO TO 10
      END IF  
      I = 0
21    IF (G(1,I+1) .LE. X) THEN
        I = I + 1
      GOTO 21
	  END IF
10    Z = X - G(1,I)
      Y = G(2,I) + Z*(G(3,I) + Z*(G(4,I) + Z*G(5,I)))
      IER = 0
      RETURN
    END SUBROUTINE SPL_INT
    !
    !
    !

! C
! C----------------------------------------------------------
! C
! C       scalar multiplication 
! C
! C
     	SUBROUTINE	SCALAR	( V1,ARG,V2)

     	! IMPLICIT	REAL*8		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

     	DIMENSION	V1(3),V2(3)

     	V2(1)	=   V1(1)*ARG
     	V2(2)	=   V1(2)*ARG
     	V2(3)	=   V1(3)*ARG

! C
! C If the numbers are *very* small, zero them out. This is not done
! C for VMS, since it seems to work out fine. Why mess with something
! C that already works.
! C
	IF (ABS(V2(1)).LT.1.0E-31) V2(1) = 0.0D0
	IF (ABS(V2(2)).LT.1.0E-31) V2(2) = 0.0D0
	IF (ABS(V2(3)).LT.1.0E-31) V2(3) = 0.0D0

    END SUBROUTINE SCALAR

    !
    !
    !

! C
! C------------------------------------------------------------
! C
! C       scalar product 
! C
! C
	SUBROUTINE DOT (V1,V2,RES)

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

	DIMENSION 	V1(3),V2(3)

	RES = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)

! C
! C If the numbers are *very* small, zero them out. This is not done
! C for VMS, since it seems to work out fine. Why mess with something
! C that already works.
! C
	IF (ABS(RES).LT.1.0E-31) RES = 0.0D0
     	RETURN
    END SUBROUTINE DOT

    !
    !
    !

! C
! C-----------------------------------------------------------
! C
! C   	vector product :    vres = v1 x v2
! C
! C
	SUBROUTINE CROSS (V1,V2,VRES)

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

        integer(kind=4)              ::    m_flag
        character(len=512)   :: m_warning
! C

!     	COMMON	/ERRFLAG	/IFLAG
	DIMENSION 	V1(3),V2(3),VRES (3)
     	M_FLAG	 = 0
	VRES(1)  =     V1(2)*V2(3) - V1(3)*V2(2)
	VRES(2)  = - ( V1(1)*V2(3) - V1(3)*V2(1) )
	VRES(3)  =     V1(1)*V2(2) - V1(2)*V2(1)

! C
! C If the numbers are *very* small, zero them out. This is not done
! C for VMS, since it seems to work out fine. Why mess with something
! C that already works.
! C
	IF (ABS(VRES(1)).LT.1.0E-31) VRES(1) = 0.0D0
	IF (ABS(VRES(2)).LT.1.0E-31) VRES(2) = 0.0D0
	IF (ABS(VRES(3)).LT.1.0E-31) VRES(3) = 0.0D0

     	TTEST  =  VRES(1)*VRES(1) + VRES(2)*VRES(2) + VRES(3)*VRES(3)
     	IF (TTEST.LT.1.0E-31) THEN
     	  M_FLAG = 1
     	  M_WARNING = 'Error in CROSS: product is zero.'
     	END IF
	END SUBROUTINE CROSS

    !
    !
    !

! C
! C-----------------------------------------------------------
! C
! C       vector normalization 
! C
! C
	SUBROUTINE NORM (V1,V2)

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

	DIMENSION 	V1(3),V2(3)
! C
	RNORM = V1(1)**2 + V1(2)**2 + V1(3)**2
	RNORM = SQRT(RNORM)

! C
! C If the numbers are *very* small, zero them out. This is not done
! C for VMS, since it seems to work out fine. Why mess with something
! C that already works.
! C
	IF (ABS(RNORM).LT.1.0E-31) RNORM = 0.0D0

	IF (RNORM.NE.0.0D0) THEN
	  RNORM = 1/RNORM
	  V2(1) = V1(1)*RNORM
	  V2(2) = V1(2)*RNORM
	  V2(3) = V1(3)*RNORM
	END IF
     	RETURN

    END SUBROUTINE NORM

    !
    !
    !

! C
! C-----------------------------------------------------------
! C
! C       generate a vector  vres = p2 - p1    ( P1 -> P2 )
! C
! C
	SUBROUTINE VECTOR (P1,P2,VRES)

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

        integer(kind=4)         ::  i
	DIMENSION 	P1(3),P2(3),VRES(3)
	DO 100 I=1,3
	    VRES(I)  =   P2(I) - P1(I)
! C
! C If the numbers are *very* small, zero them out. This is not done
! C for VMS, since it seems to work out fine. Why mess with something
! C that already works.
! C
	    IF (ABS(VRES(I)).LT.1.0E-31) VRES(I) = 0.0D0
100	CONTINUE	
    END SUBROUTINE VECTOR


    !
    !
    !

! C
! C-----------------------------------------------------------
! C
! C	generate a versor 
! C
! C
	SUBROUTINE VERSOR (P1,P2,VRES)
	! IMPLICIT 	REAL*8 		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

        integer(kind=4)         ::   i
	DIMENSION 	P1(3),P2(3),VRES(3)
! C
! C **** CHECK FOR RNORM.EQ.0 SOMEBODY ****** 
! C
	RNORM =    .0D0
	DO 100 I=1,3
100	  RNORM  =   RNORM + ( P1(I) - P2(I) )*( P1(I) - P2(I) )
	RNORM  =   SQRT(RNORM)
	DO 200 I=1,3
200	  VRES(I) =   (P2(I)-P1(I))/RNORM
    END SUBROUTINE VERSOR

    !
    !
    !

! C
! C--------------------------------------------------------------------
! C
! C 	project v1 onto v2 
! C
! C
	SUBROUTINE PROJ (V1,V2,VRES)

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)

        DIMENSION 	V1(3),V2(3),VRES(3)
	RNORM = V2(1)**2 + V2(2)**2 + V2(3)**2

! C
! C If the numbers are *very* small, zero them out. This is not done
! C for VMS, since it seems to work out fine. Why mess with something
! C that already works.
! C
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

    !
    !
    !

! C
! C------------------------------------------------------------------
! C
! C       Generates the sum of two vectors 
! C
! C
	SUBROUTINE SUM (P1,P2,RES)
	!IMPLICIT 	REAL*8 		(A-E,G-H,O-Z)

        IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
        IMPLICIT INTEGER(KIND=4)        (I-N)

	DIMENSION 	P1(3),P2(3),RES(3)

		RES(1) = P1(1) + P2(1)
		RES(2) = P1(2) + P2(2)
		RES(3) = P1(3) + P2(3)
    END SUBROUTINE SUM

    !
    !
    !

! C
! C-------------------------------------------------------------
! C
! C	Vector from a line to a point; line is specified by
! C       H0 (starting), VH (direction); point is P0, output is DIS
! C
! C
     	SUBROUTINE	VDIST	(P0, H0, VH, DIS)
     	IMPLICIT	REAL*8	(A-H, O-Z)
     	DIMENSION	P0(3), H0(3), VH(3), DIS(3)
     	DIMENSION	VT(3), T_VH(3)

     	CALL	VECTOR	(H0, P0, VT)
     	CALL	PROJ	(VT, VH, T_VH)
     	CALL	VECTOR	(T_VH, VT, DIS)

     	RETURN
    END SUBROUTINE VDIST
    !
    !
    !

! C
! C------------------------------------------------------------------
! C
! C This subroutine returns the value of the arctangent between 0-2*PI
! C
! C
     	SUBROUTINE	ATAN_2	(SINE,COSINE,ANGLE)
     	! IMPLICIT	REAL*8		(A-E,G-H,O-Z)
     	! IMPLICIT	INTEGER*4	(F)
	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (F,I-N)
     	DATA     PI  /3.141592653589793238462643D0/
     	IF (COSINE.EQ.0.0D0.AND.SINE.EQ.0.0D0)	THEN
     		ANGLE	= 0.0D0
     		RETURN
     	ELSE
     	END IF
! C
! C Check if cosine is 0
! C
     	IF (COSINE.NE.0) THEN
     	  ARG	=    SINE/COSINE
     	  ANGLE   =    ATAN (ABS(ARG))
! C
! C First quadrant: sine > 0, cosine > 0
! C
     	 IF (SINE.GT.0.0D0.AND.COSINE.GT.0.0D0) THEN
     	   ANGLE	=   ANGLE
! C
! C Second quadrant: sine > 0, cosine < 0
! C
         ELSE  IF (SINE.GT.0.0D0.AND.COSINE.LT.0.0D0) THEN
     	   ANGLE	=   PI - ANGLE
! C
! C Third quadrant: sine < 0, cosine < 0
! C
     	 ELSE  IF (SINE.LT.0.0D0.AND.COSINE.LT.0.0D0) THEN
     	   ANGLE	=   ANGLE + PI
! C
! C Fourth quadrant: sine < 0, cosine > 0
! C
     	 ELSE  IF (SINE.LT.0.0D0.AND.COSINE.GT.0.0D0) THEN
     	   ANGLE	=   2*PI - ANGLE
! C
! C Divide by zero cases
! C
     	 ELSE IF (SINE.EQ.0.0D0.AND.COSINE.GT.0.0D0) THEN
     	   ANGLE	=   0.0D0
     	 ELSE IF (SINE.EQ.0.0D0.AND.COSINE.LT.0.0D0) THEN
     	   ANGLE	=   PI
     	 ELSE
     	 END IF
     	ELSE IF (SINE.GT.0.0D0) THEN
     	  ANGLE	=   PI/2
     	ELSE IF (SINE.LT.0.0D0) THEN
     	  ANGLE   =   1.5D0*PI
     	END IF

    END   SUBROUTINE ATAN_2

    !
    !
    !

!C +++
!C 	SUBROUTINE	GAUSS
!C 
!C 	PURPOSE		Generate a bivariate normal distribution with
!C 			indipendent sigmas and a correlation.
!C 			This will simulate a SR source of given emittance
!C 			specified by the two sigmas. If dist < > 0, then
!C 			the outputs will be correlated.
!C 
!C 	INPUT		sigma1, sigma2, distance
!C 
!C 	OUTPUT		X,X1 two binormal variate
!C 
!C 	ALGORITHM	As described by R.H.Rubinstein, in Simulation and
!C 			the MonteCarlo method, Wiley, NY 1981
!C 
!C 
!C ---
	SUBROUTINE	GAUSS	( S, SPRIM, DD, X, XPRIM, IS)
	! IMPLICIT	REAL*8		(A-H,O-Z)
	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (I-N)
        integer(kind=4)         ::  is
     	DATA	TWOPI 	/6.283185307179586467925287D0/
!C 
!C  Initialize by computing the covariance matrix
!C 
10	C11	=   SQRT (DD**2*SPRIM**2+S**2)
	IF (C11.NE.0.0D0) THEN
	  C21	=   DD*SPRIM**2/C11
	  C22	=   S*SPRIM/C11
	ELSE
	  C21	= 0.0D0
	  C22	= 0.0D0
	END IF
!C 
!C  Entry for computation 
!C  Generates first the two normal variates with sigma=1 (Box & Muller)
!C 
20	CONTINUE
	R1	=   WRAN(IS)
	R2	=   WRAN(IS)
	Z1	=   SQRT(-2*LOG(R1))*COS(TWOPI*R2)
	Z2	=   SQRT(-2*LOG(R1))*SIN(TWOPI*R2)
!D	WRITE	(33,*) Z1,Z2
!C 
!C  generates now the new varaites
!C 
	X	=   Z1*C11
	XPRIM	=   Z1*C21 + Z2*C22
!C 
!C  Completed
!C 
	RETURN
    END SUBROUTINE GAUSS

End Module math
