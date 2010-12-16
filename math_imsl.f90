!----
!---- MODULE:  math_imsd
!----
!---- Contains the mathematical routines from IMSL library.
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

MODULE MATH_IMSL
	use shadow_kind, only : ski, skr, skc
    IMPLICIT NONE
    public :: mdnris, zrpoly, dbcevl, pnpoly
    private :: merfi
    private :: zrpqlb, zrpqlc, zrpqld, zrpqle, zrpqlf, zrpqlg, zrpqlh, zrpqli
Contains
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
    5 	W = SQRT(-LOG(A+A*B))
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
    10 	SN = ((E3*W+E2)*W+E1)*W
      	SD = ((W+F2)*W+F1)*W+F0
      	F = W + W*(E0+SN/SD)
      	GO TO 25
! C                    W BETWEEN 1.13222 AND 2.5, APPROX.
! C                    F BY A RATIONAL FUNCTION IN W
    15 	SN = ((C3*W+C2)*W+C1)*W
      	SD = ((W+D2)*W+D1)*W+D0
      	F = W + W*(C0+SN/SD)
      	GO TO 25
! C                    Z BETWEEN 0. AND .85, APPROX. F
! C                    BY A RATIONAL FUNCTION IN Z
    20 	Z2 = Z*Z
      	F = Z+Z*(B0+A1*Z2/(B1+Z2+A2/(B2+Z2+A3/(B3+Z2))))
! C                    FORM THE SOLUTION BY MULT. F BY
! C                    THE PROPER SIGN
    25 	Y = SIGMA*F
      	IER = 0
      	GO TO 9005
! C                    ERROR EXIT. SET SOLUTION TO PLUS
! C                    (OR MINUS) INFINITY
    30 	IER = 129
      	Y = SIGMA * RINFM
  9000 	CONTINUE
    !    CALL UERTST(IER,6HMERFI )
      	print *,"Error from math routine merfi. Called with: ",P
      	stop
  9005 	RETURN
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
    5	IF(P.LE.EPS) GO TO 10
   		X = 1.0 -(P + P)
   		CALL MERFI (X,Y,IER)
   		Y = -SQRT2 * Y
   		GO TO 9005
! C                                   P TOO SMALL, COMPUTE Y DIRECTLY
   10	A = P+P
    	W = SQRT(-LOG(A+(A-A*A)))
! C                           USE A RATIONAL FUNCTION IN 1./W
   		WI = 1./W
   		SN = ((G3*WI+G2)*WI+G1)*WI
		SD = ((WI+H2)*WI+H1)*WI+H0
  		Y = W + W*(G0+SN/SD)
  		Y = -Y*SQRT2
   		GO TO 9005
 9000 	CONTINUE
    !      CALL UERTST(IER,6HMDNRIS)
    	print *,"Error from math routine mdnris. Called with: ",P
    	stop
 9005 	RETURN
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
    	INTEGER(kind=ski)     	NDEG, IER
        REAL(kind=skr)   		A(1),Z(1)
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES
        INTEGER(kind=ski)      	N,NN,J,JJ,I,NM1,ICNT,N2,L,NZ,NPI
        INTEGER(kind=ski)      	:: i20=20
        REAL(kind=skr)   		ETA,RMRE,RINFP,REPSP,RADIX,RLO,XX,YY,SINR,      &
             					COSR,RMAX,RMIN,X,SC,XM,FF,DX,DF,BND,XXX,ARE
        REAL(kind=skr)   		PT(101)
        REAL(kind=skr)   		TEMP(101),P(101),QP(101),RK(101),QK(101),SVK(101)
        REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
             					A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
             					T,AA,BB,CC,FACTOR,REPSR1,ZERO,ONE,FN
        LOGICAL            		ZEROK
        COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
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
	5 	IF (A(NN).NE.ZERO) GO TO 10
      	J = NDEG-N+1
      	JJ = J+NDEG
      	Z(J) = ZERO
      	Z(JJ) = ZERO
      	NN = NN-1
      	N = N-1
      	IF (NN.EQ.1) GO TO 9005
      	GO TO 5
! C                                  MAKE A COPY OF THE COEFFICIENTS
   10 	DO 15 I=1,NN
      		P(I) = A(I)
   15 	CONTINUE
! C                                  START THE ALGORITHM FOR ONE ZERO
   20 	IF (N.GT.2) GO TO 30
      	IF (N.LT.1) GO TO 9005
! C                                  CALCULATE THE FINAL ZERO OR PAIR OF
! C                                    ZEROS
      	IF (N.EQ.2) GO TO 25
      	Z(NDEG) = -P(2)/P(1)
      	Z(NDEG+NDEG) = ZERO
      	GO TO 145
   25 	CALL ZRPQLI (P(1),P(2),P(3),Z(NDEG-1),Z(NDEG+NDEG-1),Z(NDEG),Z(NDEG+NDEG))
      	GO TO 145
! C                                  FIND LARGEST AND SMALLEST MODULI OF
! C                                    COEFFICIENTS.
   30 	RMAX = 0.
      	RMIN = RINFP
      	DO 35 I=1,NN
       		X = ABS(SNGL(P(I)))
         	IF (X.GT.RMAX) RMAX = X
         	IF (X.NE.0..AND.X.LT.RMIN) RMIN = X
   35 	CONTINUE
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
   40 	IF (RINFP/SC.LT.RMAX) GO TO 55
   45 	L = DLOG(SC)/DLOG(RADIX)+.5
      	IF (L .EQ. 0) GO TO 55
      	FACTOR = DBLE(RADIX)**L
      	DO 50 I=1,NN
      		P(I) = FACTOR*P(I)
   50 	CONTINUE
! C                                  COMPUTE LOWER BOUND ON MODULI OF
! C                                    ZEROS.
   55 	DO 60 I=1,NN
   			PT(I) = ABS(SNGL(P(I)))
   60 	CONTINUE
      	PT(NN) = -PT(NN)
! C                                  COMPUTE UPPER ESTIMATE OF BOUND
      	X = DEXP((DLOG(-PT(NN))-DLOG(PT(1)))/N)
      	IF (PT(N).EQ.0.) GO TO 65
! C                                  IF NEWTON STEP AT THE ORIGIN IS
! C                                    BETTER, USE IT.
      	XM = -PT(NN)/PT(N)
      	IF (XM.LT.X) X = XM
! C                                  CHOP THE INTERVAL (0,X) UNTIL FF.LE.0
   65 	XM = X*.1
      	FF = PT(1)
      	DO 70 I=2,NN
      		FF = FF*XM+PT(I)
   70 	CONTINUE
      	IF (FF.LE.0.) GO TO 75
      	X = XM
      	GO TO 65
   75 	DX = X
! C                                  DO NEWTON ITERATION UNTIL X
! C                                    CONVERGES TO TWO DECIMAL PLACES
   80 	IF (ABS(DX/X).LE..005) GO TO 90
      	FF = PT(1)
      	DF = FF
      	DO 85 I=2,N
        	FF = FF*X+PT(I)
         	DF = DF*X+FF
   85 	CONTINUE
      	FF = FF*X+PT(NN)
      	DX = FF/DF
      	X = X-DX
      	GO TO 80
   90 	BND = X
! C                                  COMPUTE THE DERIVATIVE AS THE INTIAL
! C                                    K POLYNOMIAL AND DO 5 STEPS WITH
! C                                    NO SHIFT
      	NM1 = N-1
      	FN = ONE/N
      	DO 95 I=2,N
      		RK(I) = (NN-I)*P(I)*FN
   95 	CONTINUE
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
  100    	CONTINUE
         	RK(1) = P(1)
         	ZEROK = DABS(RK(N)).LE.DABS(BB)*ETA*10.
      		GO TO 115
! C                                  USE UNSCALED FORM OF RECURRENCE
  105  		DO 110 I=1,NM1
        		J = NN-I
           		RK(J) = RK(J-1)
  110  		CONTINUE
       		RK(1) = ZERO
      		ZEROK = RK(N).EQ.ZERO
  115 	CONTINUE
! C                                  SAVE K FOR RESTARTS WITH NEW SHIFTS
      	DO 120 I=1,N
      		TEMP(I) = RK(I)
  120 	CONTINUE
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
  125    	CONTINUE
         	IF (NZ.EQ.1) GO TO 20
         	Z(J+1) = RLZR
         	Z(JJ+1) = RLZI
         	GO TO 20
! C                                  IF THE ITERATION IS UNSUCCESSFUL
! C                                    ANOTHER QUADRATIC IS CHOSEN AFTER
! C                                    RESTORING K
  130    	DO 135 I=1,N
  				RK(I) = TEMP(I)
  135    	CONTINUE
  140 	CONTINUE
! C                                  RETURN WITH FAILURE IF NO
! C                                    CONVERGENCE WITH 20 SHIFTS
		IER = 131
! C                                  CONVERT ZEROS (Z) IN COMPLEX FORM
  145 	DO 150 I=1,NDEG
      		NPI= NDEG+I
       		P(I) = Z(NPI)
  150 	CONTINUE
      	N2 = NDEG+NDEG
      	J = NDEG
      	DO 155 I=1,NDEG
       		Z(N2-1) = Z(J)
        	Z(N2) = P(J)
         	N2 = N2-2
         	J = J-1
  155 	CONTINUE
      	IF (IER .EQ. 0) GO TO 9005
! C                                  SET UNFOUND ROOTS TO MACHINE INFINITY
      	N2 = 2*(NDEG-NN)+3
      	DO 160 I=1,N
        	Z(N2) = RINFP
         	Z(N2+1) = RINFP
         	N2 = N2+2
  160 	CONTINUE
      	GO TO 9000
  165 	IER = 129
 9000 	CONTINUE
	!srio      CALL UERTST (IER,6HZRPOLY)
     	print *,"Error from math routine merfi. Called with: ",P
 9005 	RETURN
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
    	INTEGER(kind=ski)     	L2,NZ
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)     	N,NN,J,ITYPE,I,IFLAG
      	REAL(kind=skr)   		ARE,BETAS,BETAV,ETA,OSS,OTS,OTV,OVV,RMRE,SS,    &
                        		TS,TSS,TV,TVV,VV
      	REAL(kind=skr)   		P(101),QP(101),RK(101),QK(101),SVK(101)
      	REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                        		A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                        		SVU,SVV,UI,VI,S,ZERO
      	LOGICAL            		VPASS,SPASS,VTRY,STRY
      	COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                        		A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
      	DATA               		ZERO/0.0D0/
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
    5    	CONTINUE
         	S = SS
! C                                  CHOOSE ITERATION ACCORDING TO THE    
! C                                    FASTEST CONVERGING SEQUENCE        
         	VTRY = .FALSE.
         	STRY = .FALSE.
         	IF (SPASS.AND.((.NOT.VPASS).OR.TSS.LT.TVV)) GO TO 20
   10    	CALL ZRPQLC (UI,VI,NZ)
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
   15    	CONTINUE
   20    	CALL ZRPQLD (S,NZ,IFLAG)
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
   25    	U = SVU
         	V = SVV
         	DO 30 I=1,N
         		RK(I) = SVK(I)
   30    	CONTINUE
! C                                  TRY QUADRATIC ITERATION IF IT HAS    
! C                                    NOT BEEN TRIED AND THE V SEQUENCE  
! C                                    IS CONVERGING                      
         	IF (VPASS.AND.(.NOT.VTRY)) GO TO 10
! C                                  RECOMPUTE QP AND SCALAR VALUES TO    
! C                                    CONTINUE THE SECOND STAGE          
         	CALL ZRPQLH (NN,U,V,P,QP,RA,RB)
         	CALL ZRPQLE (ITYPE)
   35    	OVV = VV
         	OSS = SS
         	OTV = TV
         	OTS = TS
   40 	CONTINUE
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
    	REAL(kind=skr)   		UU,VV
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)      	N,NN,J,I,ITYPE
      	REAL(kind=skr)   		ARE,EE,ETA,OMP,RELSTP,RMP,RMRE,T,ZM
      	REAL(kind=skr)   		P(101),QP(101),RK(101),QK(101),SVK(101)
      	REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                        		A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                        		UI,VI,ZERO,PT01,ONE
      	LOGICAL            		TRIED
      	COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                        		A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
      	DATA               		ZERO,PT01,ONE/0.0D0,0.01D0,1.0D0/
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
    5 	CALL ZRPQLI (ONE,U,V,SZR,SZI,RLZR,RLZI)
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
   10 	CONTINUE
      	EE = EE*ZM+ABS(SNGL(RA)+T)
      	EE = (5.*RMRE+4.*ARE)*EE-(5.*RMRE+2.*ARE)*(ABS(SNGL(RA)+T)+        &
        	ABS(SNGL(RB))*ZM)+2.*ARE*ABS(T)
! C                                  ITERATION HAS CONVERGED SUFFICIENTLY 
! C                                    IF THE POLYNOMIAL VALUE IS LESS    
! C                                    THAN 20 TIMES THIS BOUND           
      	IF (RMP.GT.20.*EE) GO TO 15
      	NZ = 2
      	RETURN
   15 	J = J+1
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
   20 	CONTINUE
      	TRIED = .TRUE.
      	J = 0
   25 	OMP = RMP
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
    	INTEGER(kind=ski)      	NZ,IFLAG
      	REAL(kind=skr)   		SSS
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)     	N,NN,J,I
      	REAL(kind=skr)   		ARE,EE,ETA,OMP,RMP,RMS,RMRE
      	REAL(kind=skr)   		P(101),QP(101),RK(101),QK(101),SVK(101)
      	REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                        		A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                        		PV,RKV,T,S,ZERO,PT001
      	COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                        		A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
      	DATA               		ZERO/0.0D0/,PT001/0.001D0/
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
    5 	PV = P(1)
! C                                  EVALUATE P AT S                      
      	QP(1) = PV
      	DO 10 I=2,NN
        	PV = PV*S+P(I)
         	QP(I) = PV
   10 	CONTINUE
      	RMP = DABS(PV)
! C                                  COMPUTE A RIGOROUS BOUND ON THE      
! C                                    ERROR IN EVALUATING P              
      	RMS = DABS(S)
      	EE = (RMRE/(ARE+RMRE))*ABS(SNGL(QP(1)))
      	DO 15 I=2,NN
      		EE = EE*RMS+ABS(SNGL(QP(I)))
   15 	CONTINUE
! C                                  ITERATION HAS CONVERGED SUFFICIENTLY 
! C                                    IF THE POLYNOMIAL VALUE IS LESS    
! C                                    THAN 20 TIMES THIS BOUND           
      	IF (RMP.GT.20.*((ARE+RMRE)*EE-RMRE*RMP)) GO TO 20
      	NZ = 1
      	SZR = S
      	SZI = ZERO
      	RETURN
   20 	J = J+1
! C                                  STOP ITERATION AFTER 10 STEPS        
      	IF (J.GT.10) RETURN
      	IF (J.LT.2) GO TO 25
      	IF (DABS(T).GT.PT001*DABS(S-T).OR.RMP.LE.OMP) GO TO 25
! C                                  A CLUSTER OF ZEROS NEAR THE REAL     
! C                                    AXIS HAS BEEN ENCOUNTERED RETURN   
! C                                    WITH IFLAG SET TO INITIATE A       
! C   	                               QUADRATIC ITERATION
      	IFLAG = 1
      	SSS = S
      	RETURN
! C                                  RETURN IF THE POLYNOMIAL VALUE HAS   
! C                                    INCREASED SIGNIFICANTLY            
   25 	OMP = RMP
! C                                  COMPUTE T, THE NEXT POLYNOMIAL, AND  
! C                                    THE NEW ITERATE                    
      	RKV = RK(1)
      	QK(1) = RKV
      	DO 30 I=2,N
         	RKV = RKV*S+RK(I)
         	QK(I) = RKV
   30 	CONTINUE
      	IF (DABS(RKV).LE.DABS(RK(N))*10.*ETA) GO TO 40
! C                                  USE THE SCALED FORM OF THE           
! C                                    RECURRENCE IF THE VALUE OF K AT S  
! C                                    IS NONZERO                         
      	T = -PV/RKV
      	RK(1) = QP(1)
      	DO 35 I=2,N
      		RK(I) = T*QK(I-1)+QP(I)
   35 	CONTINUE
      	GO TO 50
! C                                  USE UNSCALED FORM                    
   40 	RK(1) = ZERO
      	DO 45 I=2,N
      		RK(I) = QK(I-1)
   45 	CONTINUE
   50	RKV = RK(1)
      	DO 55 I=2,N
      		RKV = RKV*S+RK(I)
   55 	CONTINUE
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
    	INTEGER(kind=ski)     	ITYPE
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)     	N,NN
      	REAL(kind=skr)   		ARE,ETA,RMRE
      	REAL(kind=skr)   		P(101),QP(101),RK(101),QK(101),SVK(101)
      	REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3, &
                        		A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI
      	COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
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
    5 	IF (DABS(D).LT.DABS(C)) GO TO 10
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
   10 	ITYPE = 1
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
    	INTEGER(kind=ski)     	ITYPE
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)     	N,NN,I
      	REAL(kind=skr)   		ARE,ETA,RMRE
      	REAL(kind=skr)   		P(101),QP(101),RK(101),QK(101),SVK(101)
      	REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                       			A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,TEMP,ZERO
      	COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                        		A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
      	DATA               		ZERO/0.0D0/
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
    5 	CONTINUE
      	RETURN
! C                                  USE SCALED FORM OF THE RECURRENCE    
   10 	A7 = A7/A1
     	A3 = A3/A1
      	RK(1) = QP(1)
      	RK(2) = QP(2)-A7*QP(1)
      	DO 15 I=3,N
      		RK(I) = A3*QK(I-2)-A7*QP(I-1)+QP(I)
   15 	CONTINUE
      	RETURN
! C                                  USE UNSCALED FORM OF THE RECURRENCE  
! C                                    IF TYPE IS 3                       
   20 	RK(1) = ZERO
      	RK(2) = ZERO
      	DO 25 I=3,N
      		RK(I) = QK(I-2)
   25 	CONTINUE
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
    	INTEGER(kind=ski)     	ITYPE
      	REAL(kind=skr)   		UU,VV
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)     	N,NN
     	REAL(kind=skr)        	ARE,ETA,RMRE
      	REAL(kind=skr)   		P(101),QP(101),RK(101),QK(101),SVK(101)
      	REAL(kind=skr)   		SR,SI,U,V,RA,RB,C,D,A1,A2,A3,                   &
                        		A6,A7,E,F,G,H,SZR,SZI,RLZR,RLZI,                &
                        		A4,A5,B1,B2,C1,C2,C3,C4,TEMP,ZERO
      	COMMON /ZRPQLJ/    		P,QP,RK,QK,SVK,SR,SI,U,V,RA,RB,C,D,A1,A2,A3,A6, &
                        		A7,E,F,G,H,SZR,SZI,RLZR,RLZI,ETA,ARE,RMRE,N,NN
      	DATA               		ZERO/0.0D0/
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
    5 	A4 = (RA+G)*F+H
      	A5 = (F+U)*C+V*D
! C                                  EVALUATE NEW QUADRATIC COEFFICIENTS. 
! C                                                                       
   10 	B1 = -RK(N)/P(NN)
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
   15 	UU = ZERO
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
    	INTEGER(kind=ski)      	NN
      	REAL(kind=skr)   		P(NN),Q(NN),U,V,RA,RB
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	INTEGER(kind=ski)     	I
      	REAL(kind=skr)   		C
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
    5 	CONTINUE
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
    	REAL(kind=skr)   		RA,B1,C,SR,SI,RLR,RLI
! C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      	REAL(kind=skr)   		RB,D,E,ZERO,ONE,TWO
      	DATA               		ZERO,ONE,TWO/0.0D0,1.0D0,2.0D0/
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
    5 	SI = ZERO
      	RLI = ZERO
      	RETURN
   10 	IF (C.NE.ZERO) GO TO 15
      	SR = ZERO
      	RLR = -B1/RA
      	GO TO 5
! C                                  COMPUTE DISCRIMINANT AVOIDING        
! C                                    OVERFLOW                           
   15 	RB = B1/TWO
      	IF (DABS(RB).LT.DABS(C)) GO TO 20
      	E = ONE-(RA/RB)*(C/RB)
      	D = DSQRT(DABS(E))*DABS(RB)
      	GO TO 25
   20 	E = RA
      	IF (C.LT.ZERO) E = -RA
      	E = RB*(RB/DABS(C))-E
      	D = DSQRT(DABS(E))*DSQRT(DABS(C))
   25 	IF (E.LT.ZERO) GO TO 30
! C   	                             REAL ZEROS
      	IF (RB.GE.ZERO) D = -D
      	RLR = (-RB+D)/RA
      	SR = ZERO
      	IF (RLR.NE.ZERO) SR = (C/RLR)/RA
      	GO TO 5
! C                                  COMPLEX CONJUGATE ZEROS              
   30 	SR = -RB/RA
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
    5 	CONTINUE
      	IER = 35
   10 	IF (YL.LT.Y(1)) IER = 34
      	DO 15 J=2,NY
        	LY = J-1
         	IF (YL.LE.Y(J)) GO TO 20
   15 	CONTINUE
      	IER = 36
   20 	LXP1 = LX+1
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
   25 	CONTINUE
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
    	IMPLICIT REAL(kind=skr) 	(A-E,G-H,O-Z)
      	IMPLICIT INTEGER(kind=ski) 	(F,I-N)
! C
      	DIMENSION 				X(N),Y(N)
      	LOGICAL 				IX,IY,JX,JY,EOR
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
	1	IF ((YI*XJ-XI*YJ)/(XJ-XI)) 4,2,3
	2 	INOUT=0
      	RETURN
3     	INOUT=-INOUT
4     	CONTINUE
      	RETURN
	END SUBROUTINE PNPOLY
END MODULE MATH_IMSL
