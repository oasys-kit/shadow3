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

	use shadow_kind, only : ski, skr, skc, sklen
    implicit none

    public :: wran
    public :: rotate, spl_int, atan_2, gauss
    public :: scalar, dot, cross, norm, vector, versor, proj, sum, vdist
    public :: gnorfunc, rotvector, mfp
    public :: qsf,cubspl
    private :: gcf,gser
    private :: erfc,gammp,gammq,gammln

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
     	! IMPLICIT	REAL*8		(A-H,O-Z)

                implicit none
		!IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		!IMPLICIT INTEGER(KIND=SKI) 	(I-N)

		INTEGER(KIND=SKI)	:: ISEED
		INTEGER(KIND=SKI)	:: first=1,wran_counter=0
                real(kind=skr)          :: XX

		!first=1

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Uncomment this part is for the built-in f95 random generator
!!

     !write(*,*) " "
     !write(*,*) "WRAN: first: ",first
     !write(*,*) "WRAN: wran_counter: ",wran_counter
!todo this initializes ONLY if first=1!!!!!!!!!!!!!!!

         if (first.eq.1) then
 	   		XX = dble(ISEED)
       		XX = XX/(10**int(1+log10(XX)))
     !write(*,*) " "
     !write(*,*) "WRAN: Initializing generator with ISEED: ", ISEED
     !write(*,*) "                                     XX: ",XX
       		CALL RANDOM_NUMBER(HARVEST=XX)
       		first = 0
         end if
         CALL RANDOM_NUMBER(WRAN)

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!! Uncomment for Penelope random number generator
!!

!!           wran = RAND_PENELOPE(1.0D0)

!!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!
!! This part is for calling the C generator using the wranc.c wrapper
!! This part produces the same results than Shadow 2.x
!!

!! TODO: MUST BE COMMENTED IN WINDOWS
!!      WRAN = WRANC(ISEED)
!!

      wran_counter=wran_counter+1
      RETURN
END FUNCTION WRAN

! C+++
! C SUBROUTINE ROTATE
! C
! C PURPOSE: GENERATE THE EULERIAN ANGLES FROM THE ELECTRON COORDINATES TO
! C THE SHADOW REFERENCE FRAME.
! C
! C----
	SUBROUTINE ROTATE (VIN,PSI,THETA,PHI,VOUT)
          ! IMPLICIT  REAL*8 (A-H,O-Z)
          ! IMPLICIT INTEGER(KIND=SKI) (I-N)

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
	  	IMPLICIT INTEGER(KIND=SKI) 	(I-N)

        DIMENSION 				VIN(3), VOUT(3)
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
      ! IMPLICIT	REAL*8	(A-H,O-Z)
      ! IMPLICIT INTEGER(KIND=SKI) (I-N)

 		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
      	IMPLICIT INTEGER(KIND=SKI) 	(I-N)

      	REAL(KIND=SKR)   		G(5,N), X, Y, Z
      	INTEGER(KIND=SKI)  		I, IER, N

      	GMAX = MAX(G(1,1),G(1,N))
      	GMIN = MIN(G(1,1),G(1,N))
      	IF ((X .LT. GMIN) .OR. (X .GT. GMAX)) THEN
        	WRITE(6,*) 'SPL_INT: x is outside the interpolation range.'
        	WRITE(6,*) 'X, GMIN, GMAX: ',X,GMIN,GMAX
        	IER = 1
        	RETURN
      	ELSE IF (X .EQ. G(1,N)) THEN
        	I = N-1
        	GO TO 10
      	END IF
      	I = 0
   21  	IF (G(1,I+1) .LE. X) THEN
        	I = I + 1
      	GOTO 21
	  	END IF
   10 	Z = X - G(1,I)
      	Y = G(2,I) + Z*(G(3,I) + Z*(G(4,I) + Z*G(5,I)))
     	IER = 0
      	RETURN
	END SUBROUTINE SPL_INT

!C ++++
!C
!C       scalar multiplication
!C ----
	SUBROUTINE SCALAR( V1,ARG,V2)

     	! IMPLICIT	REAL*8		(A-H,O-Z)

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI)	(I-N)

     	DIMENSION				V1(3),V2(3)

     	V2(1)	=   V1(1)*ARG
     	V2(2)	=   V1(2)*ARG
     	V2(3)	=   V1(3)*ARG

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

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI) 	(I-N)

		DIMENSION				V1(3),V2(3)

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

	! IMPLICIT 	REAL*8 		(A-H,O-Z)

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI)	(I-N)

        integer(KIND=SKI)     	m_flag
        character(len=sklen)   	m_warning
!     	COMMON	/ERRFLAG	/IFLAG
		DIMENSION 				V1(3),V2(3),VRES (3)
     	M_FLAG	= 0
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
! C       vector normalization 
! C ----

	SUBROUTINE NORM (V1,V2)
		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI) 	(I-N)

		DIMENSION 				V1(3),V2(3)

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

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI) 	(I-N)

        integer(KIND=SKI)		i
		DIMENSION 				P1(3),P2(3),VRES(3)
		DO 100 I=1,3
	    	VRES(I)  =   P2(I) - P1(I)
!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
	    	IF (ABS(VRES(I)).LT.1.0E-31) VRES(I) = 0.0D0
  100	CONTINUE
	END SUBROUTINE VECTOR


! C ++++
! C
! C	generate a versor 
! C
! C ----
	SUBROUTINE VERSOR (P1,P2,VRES)

		IMPLICIT REAL(KIND=SKR) 		(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI)   	(I-N)

        integer(KIND=SKI)		I
		DIMENSION 				P1(3),P2(3),VRES(3)
!C
!C **** CHECK FOR RNORM.EQ.0 SOMEBODY ******
!C
		RNORM =    .0D0
		DO 100 I=1,3
			RNORM  =   RNORM + ( P1(I) - P2(I) )*( P1(I) - P2(I) )
  100	CONTINUE
		RNORM  =   SQRT(RNORM)
		DO 200 I=1,3
			VRES(I) =   (P2(I)-P1(I))/RNORM
  200	CONTINUE
    END SUBROUTINE VERSOR

! C ++++
! C
! C 	project v1 onto v2 
! C ----
	SUBROUTINE PROJ (V1,V2,VRES)

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI) 	(I-N)

        DIMENSION 				V1(3),V2(3),VRES(3)

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
	SUBROUTINE SUM (P1,P2,RES)

        IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
        IMPLICIT INTEGER(KIND=SKI) 	(I-N)

		DIMENSION 				P1(3),P2(3),RES(3)

		RES(1) = P1(1) + P2(1)
		RES(2) = P1(2) + P2(2)
		RES(3) = P1(3) + P2(3)

    END SUBROUTINE SUM

! C ++++
! C
! C	Vector from a line to a point; line is specified by
! C       H0 (starting), VH (direction); point is P0, output is DIS
! C -----
    SUBROUTINE VDIST (P0, H0, VH, DIS)
     	IMPLICIT REAL(KIND=SKR)		(A-H, O-Z)
     	DIMENSION				P0(3), H0(3), VH(3), DIS(3)
     	DIMENSION				VT(3), T_VH(3)

     	CALL VECTOR(H0, P0, VT)
     	CALL PROJ(VT, VH, T_VH)
     	CALL VECTOR(T_VH, VT, DIS)

     	RETURN
    END SUBROUTINE VDIST

! C ++++
! C
! C This subroutine returns the value of the arctangent between 0-2*PI
! C ----
     SUBROUTINE	ATAN_2	(SINE,COSINE,ANGLE)
     	! IMPLICIT	REAL*8		(A-E,G-H,O-Z)
     	! IMPLICIT	INTEGER*4	(F)
		IMPLICIT REAL(KIND=SKR) 	(A-E,G-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI) 	(F,I-N)
     	DATA PI /3.141592653589793238462643D0/
     	IF (COSINE.EQ.0.0D0.AND.SINE.EQ.0.0D0)	THEN
     		ANGLE = 0.0D0
     		RETURN
     	ELSE
     	END IF
!C
!C Check if cosine is 0
!C
     	IF (COSINE.NE.0) THEN
     		ARG	= SINE/COSINE
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
! C 	SUBROUTINE	GAUSS
! C
! C 	PURPOSE		Generate a bivariate normal distribution with
! C 			indipendent sigmas and a correlation.
! C 			This will simulate a SR source of given emittance
! C 			specified by the two sigmas. If dist < > 0, then
! C 			the outputs will be correlated.
! C
! C 	INPUT		sigma1, sigma2, distance
! C
! C 	OUTPUT		X,X1 two binormal variate
! C
! C 	ALGORITHM	As described by R.H.Rubinstein, in Simulation and
! C 			the MonteCarlo method, Wiley, NY 1981
! C
! C
! C ---
	SUBROUTINE GAUSS (S,SPRIM,DD,X,XPRIM,IS)

		IMPLICIT REAL(KIND=SKR) 	(A-H,O-Z)
		IMPLICIT INTEGER(KIND=SKI) 	(I-N)
        integer(KIND=SKI) 		is
     	DATA 					TWOPI /6.283185307179586467925287D0/
!C 
!C  Initialize by computing the covariance matrix
!C 
   10	C11	=   SQRT (DD**2*SPRIM**2+S**2)
		IF (C11.NE.0.0D0) THEN
	  		C21	= DD*SPRIM**2/C11
	  		C22	= S*SPRIM/C11
		ELSE
	  		C21 = 0.0D0
	  		C22 = 0.0D0
		END IF
!C 
!C  Entry for computation 
!C  Generates first the two normal variates with sigma=1 (Box & Muller)
!C 
   20	CONTINUE
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

! C ++++
! C	Subroutine GNORFUNC (Normal or gaussian probability distribution
! C	function).
! C ----
	SUBROUTINE GNORFUNC (Y,P)

        real(KIND=SKR),intent(in)  		:: y
        real(KIND=SKR),intent(out) 		:: p

        real(KIND=SKR)          y0

		Y0 =-Y/DSQRT(2.0D0)
		P  = 0.5D0*ERFC(Y0)

		RETURN
	End Subroutine gnorfunc

! C ++++
! C	FUNCTION ERFC (Complemented Error Fuction)
! C	Reference: See Numerical Recipes in Fortran, Cambridge U. Press
! C	(1989) for a description of this and following subroutines.
! C ----
	REAL(KIND=SKR)  FUNCTION ERFC(X)

    	real(KIND=SKR),intent(in)  		:: x

      	IF(X.LT.0D0) THEN
        	ERFC=1.0D0+GAMMP(0.5D0,X**2)
      	ELSE
        	ERFC=GAMMQ(0.5D0,X**2)
      	ENDIF
      	RETURN
	End Function erfc

! C
! C
! C
   	REAL(KIND=SKR) FUNCTION GAMMP(A,X)

  		real(KIND=SKR),intent(in)  		:: a,x
  		real(KIND=SKR)          gamser,gln,gammcf
      

  		IF(X.LT.0.0D0.OR.A.LE.0.0D0) THEN 
                  !PAUSE
                  print *,"PAUSE statement executed.  Hit Return to continue"
                  read (*,*) 
                END IF
      	IF(X.LT.A+1.0D0) THEN
        	CALL GSER(GAMSER,A,X,GLN)
        	GAMMP=GAMSER
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

   		real(KIND=SKR),intent(in)    	:: a,x
      	real(KIND=SKR)         	gamser,gln,gammcf

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

      	implicit real(KIND=SKR) 	(a-h,o-z)
      	implicit integer(KIND=SKI) 	(i-n)

      	PARAMETER (ITMAX=100,EPS=3.D-7)

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
   11  	CONTINUE
      	!PAUSE 'A too large, ITMAX too small'
        print *,"A too large, ITMAX too small"
        read (*,*) 
    1 	GAMMCF=DEXP(-X+A*DLOG(X)-GLN)*G
      	RETURN
	End Subroutine gcf

! C
! C
! C
	SUBROUTINE GSER (GAMSER,A,X,GLN)

		implicit real(KIND=SKR) 	(a-h,o-z)
		implicit integer(KIND=SKI) 	(i-n)

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
   11	CONTINUE
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

   		real(KIND=SKR),intent(in)   	:: XX
      	real(KIND=SKR) 			COF(6)
      	real(KIND=SKR)         	STP,HALF,ONE,FPF,X,TMP,SER
      	integer(KIND=SKI)      	J
      
      	DATA 					COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0, &
        						-1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      	DATA 					HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/

      	X=XX-ONE
      	TMP=X+FPF
      	TMP=(X+HALF)*DLOG(TMP)-TMP
      	SER=ONE
      	DO 11 J=1,6
        	X=X+ONE
        	SER=SER+COF(J)/X
   11	CONTINUE
      	GAMMLN=TMP+DLOG(STP*SER)
      	RETURN
	End Function gammln

! C ++++
! C
! C       Rotation a vector VIN an angle ALPHA around an AXIS vector
! C ----
	SUBROUTINE ROTVECTOR (VIN,AXIS,ALPHA,VOUT)

		implicit real(KIND=SKR) 	(a-h,o-z)
		implicit integer(KIND=SKI) 	(i-n)

     	DIMENSION				VIN(3),AXIS(3),VOUT(3)
     	DIMENSION				VCTR0(3),VCTR1(3),VCTR2(3),VCTR3(3)

		CALL DOT(AXIS,AXIS,AXIS_MOD2)
		CALL DOT(VIN,AXIS,ETA)
		ETA	= ETA/AXIS_MOD2
		CALL SCALAR(AXIS,ETA,VCTR0)
		CALL VECTOR(VCTR0,VIN,VCTR1)
		CALL CROSS(VIN,AXIS,VCTR3)
		SA	= SIN(ALPHA)/SQRT(AXIS_MOD2)
		CA	= COS(ALPHA)
		CALL SCALAR(VCTR3,SA,VCTR3)
		CALL SCALAR(VCTR1,CA,VCTR1)
		CALL SUM(VCTR1,VCTR3,VCTR2)
		CALL SUM(VCTR0,VCTR2,VOUT)
		RETURN
	End Subroutine rotvector


! C ++++
! C	SUBROUTINE MFP  (mean free path)
! C
! C       This subroutine return a value following the exponential decay
! C	distribution law. We initialize the subroutine (flag negative) 
! C	calling it with ARG the minimum and the maximun of the interval
! C	in which we want the result. After that, we call again the 
! C	subroutine with a flag no negative to have the result.
! C
! C
! C ----
	SUBROUTINE MFP (ARG,ISEED,IFLAG)

    	real(KIND=SKR)   		arg
      	integer(KIND=SKI)   	iflag, iseed

      	real(KIND=SKR)  		ymin,ymax, aa0, yval
		SAVE YMIN, YMAX

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
!TODO: mv to math
SUBROUTINE QSF(H,Y,Z,NDIM)
	!IMPLICIT REAL*8		(A-H,O-Z)
        implicit real(kind=skr) (a-h,o-z)
        implicit integer(kind=ski)        (i-n)

      	DIMENSION		Y(NDIM),Z(NDIM)
!C
      	HT=1.0D0/3.0D0*H
	IF(NDIM-5)7,8,1
!C
!C  Ndim is greater than 5. preparations of integration loop
!C
    1 	SUM1 = 4.0D0 * Y(2)
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
    2 	DO 4 I=7,NDIM,2
      		SUM1   = AUX1
      		SUM2   = AUX2
      		AUX1   = 4.0D0*Y(I-1)
      		AUX1   = SUM1+HT*(Y(I-2)+AUX1+Y(I))
      		Z(I-2) = SUM1
      		IF(I-NDIM)3,6,6
!C
    3 		AUX2 = 4.0D0*Y(I)
      		AUX2=SUM2+HT*(Y(I-1)+AUX2+Y(I+1))
    4 		Z(I-1) = SUM2
    5 		Z(NDIM-1) = AUX1
      		Z(NDIM) = AUX2
      	RETURN
    6 	Z(NDIM-1) = SUM2
      	Z(NDIM) = AUX1
      	RETURN
!C
!C  End of integration loop
!C
    7 	IF(NDIM-3)12,11,8
!C
!C  Ndim is equal to 4 or 5
!C
    8 	SUM2 = 1.125D0*HT*(Y(1) + 3.0D0*Y(2) + 3.0D0*Y(3) + Y(4))
      	SUM1 = 4.0D0*Y(2)
      	SUM1 = HT*(Y(1)+SUM1+Y(3))
      	Z(1) = 0.0D0
      	AUX1 = 4.0D0*Y(3)
      	Z(2) = SUM2-HT*(Y(2)+AUX1+Y(4))
      	IF(NDIM-5)10,9,9
    9 	AUX1 = 4.0D0*Y(4)
      	Z(5) = SUM1+HT*(Y(3)+AUX1+Y(5))
   10 	Z(3) = SUM1
      	Z(4) = SUM2
      	RETURN
!C
!C  Ndim is equal to 3
!C
   11 	SUM1 = HT*(1.25D0*Y(1) + 2.0D0*Y(2) - .25D0*Y(3))
      	SUM2 = 4.0D0*Y(2)
      	Z(3) = HT*(Y(1)+SUM2+Y(3))
      	Z(1) = 0.0D0
      	Z(2) = SUM1
   12 	RETURN
END SUBROUTINE QSF

    !
    !
    !

!*******************************************************************************
!C+++
!C	SUBROUTINE	SORT_SPL	(Real*8 version)
!C
!C	PURPOSE		To sort a pair of array XVEC and YVEC.  The result is
!C			in ascending order according to XVEC.
!C---
SUBROUTINE SORT_SPL	(XVEC,YVEC,ICOUNT)

        ! todo: remove implicits
	implicit real(kind=skr) (a-h,o-z)
	implicit integer(kind=ski)        (i-n)

!	IMPLICIT	REAL*8	(A-H,O-Z)
	DIMENSION	XVEC(ICOUNT),YVEC(ICOUNT)
	DO 11 I = 1, ICOUNT
	  IMIN	= I
	  AMIN	= XVEC(I)
	  DO 21 J = I, ICOUNT
	    IF (XVEC(J).LT.AMIN) THEN
	      AMIN	= XVEC(J)
	      IMIN	= J
	    END IF
21	  CONTINUE
	  XTEMP		= XVEC(I)
	  XVEC(I)	= XVEC(IMIN)
	  XVEC(IMIN)	= XTEMP
	  YTEMP		= YVEC(I)
	  YVEC(I)	= YVEC(IMIN)
	  YVEC(IMIN)	= YTEMP
11	CONTINUE
	RETURN
End Subroutine sort_spl 
    !
    !
    !
    !
    !
    !

!#if defined(unix) || HAVE_F77_CPP
!#	include		<header.txt>
!#elif defined(vms)
!     	INCLUDE		'SHADOW$INC:HEADER.TXT/LIST'
!#endif
!
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
!C		 IER=1(0), check (no check) for steep slope at the two ends.
!C
!C     OUTPUT     THE LAST FOUR ROWS OF G.
!C                IER=1 FOR ERROR, =0 OTHERWISE.
!C
!C---       
SUBROUTINE CUBSPL(G, Y, N, IER)

        ! todo: remove implicits
	implicit real(kind=skr) (a-h,o-z)
	implicit integer(kind=ski)        (i-n)

!      IMPLICIT	REAL*8	(A-H,O-Z)
!#if defined(unix) || HAVE_F77_CPP
!#     include	<dim.par>
!#elif defined(vms)
!      INCLUDE	'SHADOW$INC:DIM.PAR/LIST'
!srio      INTEGER	MPURGE(2)
      integer(kind=ski),dimension(2)   :: mpurge
!#endif

!srio danger
!      REAL*8	G(5,N), Y(N), E1(NPOINT), E2(NPOINT), R
      integer(kind=ski)                        :: I,IER,N
      real(kind=skr),dimension(5,N)  :: G
      real(kind=skr),dimension(N)         :: Y
!!!DANGER      real(kind=kind(1.0d0)),dimension(NPOINT)    :: E1, E2

      real(kind=skr),dimension(N)    :: E1, E2

      real(kind=skr)                 :: R
!      REAL*8	G(5,N), Y(N), E1(N_DIM), E2(N_DIM), R
!      INTEGER	I, IER, N
!C
!      IF (N.GT.N_DIM) CALL LEAVE &
!       	('CUBSPL','Splines cannot handle more than N_DIM points.' &
!srio      IF (N.GT.NPOINT) THEN 
!srio         print *,'N: ',N
!srio         print *,'NPOINT: ',NPOINT
!srio           CALL LEAVE &
!srio       	   ('CUBSPL','Splines cannot handle more than NPOINT points.',-1)
!srio      ENDIF
!srio danger
!srio      IF (N.GT.NPOINT) THEN
!srioprint *,'<><> N,NPOINT,IER: ',N,NPOINT,IER
!srio        CALL LEAVE &
!srio       	('CUBSPL','Splines cannot handle more than NPOINT points.' &
!srio      	,-1)
!srio      END IF

      itmp = -1
      !IF (N.LT.4) CALL LEAVE &
      ! 	('CUBSPL','At least 4 data points are needed for splines.',itmp)
      IF (N.LT.4) THEN
       	print *,'CUBSPL: At least 4 data points are needed for splines.',itmp
        STOP 'Aborted'
      END IF
      DO 11 J = 1, N-1
        IF (G(1,J).GT.G(1,J+1)) THEN
	  DO 21 I = 1, N
	    E1(I)	= G(1,I)
	    E2(I)	= Y(I)
21	  CONTINUE
	  CALL SORT_SPL	(E1,E2,N)
	  DO 31 I = 1, N
	    G(1,I)	= E1(I)
	    Y(I)	= E2(I)
31	  CONTINUE
	  GO TO 101
        END IF
11    CONTINUE
101	SMIN	= 1.0D+30
	  DO 41 I = 1, N-1 
	  E1(I) = G(1,I+1) - G(1,I)

!C check for zero here ...
!C	  IF (E1(I).EQ.0.0) THEN
!C	    E1(I) = 1.0E-30
!C	  ENDIF
!C end changes.  may need at a later date for special wigglers. clw
!C 7/22/93
! srio danger
! uncommented this to solve the problem in wiggler interpolation 
! shadow 2.3...
! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/compilation_fix2008-04-09.txt

         if (e1(i).eq.0.0) then
           e1(i) = 1.0e-12
         endif
!
	  E2(I) = (Y(I+1) - Y(I)) / E1(I)
	SMIN	= MIN(SMIN,ABS(E2(I)))
41    CONTINUE
      IF (IER.EQ.0) THEN
	ISTART	= 1
	IEND	= N
      ELSE
!C
!C Check if the slopes at the two ends (1 -> ISTART, IEND -> N) are too steep.
!C
	I = 1
!srio danger
! changed this to avoid problem in wiggler shadow 2.3....
! see http://ftp.esrf.fr/pub/scisoft/shadow/user_contributions/compilation_fix2008-04-09.txt
!51	IF (ABS(E2(I)).GT.SMIN*10) THEN
51	IF (ABS(E2(I)).GT.SMIN) THEN
	  I = I + 1
	GOTO 51
	END IF
	ISTART	= I
	I = N - 1
!srio danger
! changed this to avoid problem in wiggler shadow 2.3....
!61	IF (ABS(E2(I)).GT.SMIN*10) THEN
61	IF (ABS(E2(I)).GT.SMIN*1) THEN
	  I = I - 1
	GOTO 61
	END IF
	IEND	= I
	IF ((IEND-ISTART+1).LT.4) THEN
	  ISTART	= 1
	  IEND	=N
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
      DO 71 I = ISTART+1, IEND-1
        G(2,I) = E1(I)
	G(3,I) = 2.0D0*(E1(I-1) + E1(I))
	G(4,I) = E1(I-1)
	G(5,I) = 3.0D0*(E1(I)*E2(I-1) + E1(I-1)*E2(I))
71     CONTINUE
      G(2,IEND) = E1(IEND-1) + E1(IEND-2)
      G(3,IEND) = E1(IEND-2)
      G(5,IEND) = ((E1(IEND-1)+2.0D0*G(2,IEND))*E2(IEND-1)*E1(IEND-2)+ &
      		(E1(IEND-1) &
               **2.0D0)*E2(IEND-2))/G(2,IEND)
      DO 81 I = ISTART, IEND-1
	R = G(2,I+1) / G(3,I)
	G(3,I+1) = G(3,I+1) - R*G(4,I)
	G(5,I+1) = G(5,I+1) - R*G(5,I)
81    CONTINUE
      G(3,IEND) = G(5,IEND) / G(3,IEND)
      DO 91 I = IEND-1, ISTART, -1
	G(3,I) = (G(5,I) - G(4,I)*G(3,I+1)) / G(3,I)
91    CONTINUE
      DO 141 I = ISTART, IEND-1
	G(2,I) = Y(I)
	G(4,I) = (3.0D0*E2(I) - 2.0D0*G(3,I) - G(3,I+1)) / E1(I)
	G(5,I) = (-2.0D0*E2(I) + G(3,I) + G(3,I+1)) / (E1(I)**2.0D0)
141     CONTINUE
!C
!C Use broken line instead at the two ends if slope are too steep.
!C
	DO 111 I = 1, ISTART-1
	  G(2,I)	= Y(I)
	  G(3,I)	= E2(I)
	  G(4,I)	= 0.0D0
	  G(5,I)	= 0.0D0
111	CONTINUE
	DO 121 I = IEND, N-1
	  G(2,I)	= Y(I)
	  G(3,I)	= E2(I)
	  G(4,I)	= 0.0D0
	  G(5,I)	= 0.0D0
121	CONTINUE
      IER = 0
!#if defined(vms)
!	MPURGE(1)	= %LOC(E1(1))
!	MPURGE(2)	= %LOC(E1(N_DIM))
!	CALL	SYS$PURGWS	(MPURGE)
!	MPURGE(1)	= %LOC(E2(1))
!	MPURGE(2)	= %LOC(E2(N_DIM))
!	CALL	SYS$PURGWS	(MPURGE)
!#endif
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


End Module math
