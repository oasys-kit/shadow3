!----
!----
!---- MODULE:  shadow_sourcesync
!----
!---- Main module for shadow synchrotron sources
!---- Contains: 
!----
!----
!----
!---- Example of usage: see test_sync.f95 for some tests.
!----                   see gen_source (main program for creating a shadow source)
!----
!----

Module shadow_synchrotron
    !---- Use Modules ----!

    use shadow_globaldefinitions
    use stringio
    use gfile
    use shadow_beamio
    use shadow_math
    use shadow_variables
    use shadow_kernel

    !---- Variables ----!
    implicit none

    !dimensions
    integer(kind=ski),parameter :: NDIM_TRAJ=1001 ! number of points of electron trajectory

    ! updated and moved to globaldefinitionssrio@esrf.eu 20160527
    ! real(kind=skr),parameter :: codata_mee  = 0.51099892   ! electrom Mass equivalent in MeV

    real(kind=skr),dimension(NDIM_TRAJ,2)   :: PPAR,PPER,PTOT

    real(kind=skr) :: PHOT,STEP,X,BK,GAMM
    integer(kind=ski)        :: NP,IPAD1,N,IPAD2,IER,IPAD3
    ! DANGER: THETA here (in /SECON/, defined in SETUP,  has been renamed 
    ! to THETABM as another variable with the same name is used in trace
    real(kind=skr) :: THETABM

    real(kind=skr) :: RAD,RLCR,RL,FCT,CONST,ARG,PSIMAX

    real(kind=skr) :: PSI
    integer(kind=ski)        :: IC,IPAD4,IDATA


    real(kind=skr),dimension(NDIM_TRAJ)   :: PPAR_INT,PPER_INT,PTOT_INT

    real(kind=skr),dimension(NDIM_TRAJ)   :: ANG_ARR
    real(kind=skr),dimension(10013)  :: SPLI_1,SPLI_2,SPLI_3

    real(kind=skr)   :: R_INPUT
    !C
    !C The SOURCE and ALADDIN are also in ../../include/common.blk.in. KEEP
    !C THESE IN SYNC. Watch the B_ENER vs BENER *and* PHOTON_ vs PHOTON in 
    !C ALADDIN however.
    !C

    !	COMMON /SPL_ARR/	PHOT,PHOT_INV,XPHOT,PSI_INV,PSI_POL
    !srio	REAL*8		PHOT(5,1010),PHOT_INV(5,1010)
    !srio	REAL*8		XPHOT(251),PSI_INV(5,21,251),PSI_POL(2,21,251)
    real(kind=skr),dimension(5,1010)   :: PHOT_SPLINE,PHOT_INV
    real(kind=skr),dimension(251)      :: XPHOT
    real(kind=skr),dimension(5,21,251) :: PSI_INV
    real(kind=skr),dimension(2,21,251) :: PSI_POL
    !	COMMON  /CDFINDEX/	IMAX_1,IMIN_1,IINT_1,NKOL,
    !     $				IMAX_2,IMIN_2,IINT_2,IST
    integer(kind=ski)    :: IMAX_1,IMIN_1,IINT_1,NKOL,IMAX_2,IMIN_2,IINT_2,IST



    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    public :: bskm
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public :: aladdin1, white, SOURCESYNC, SrCdf, SrFunc,shadow3source


    !---- List of private functions ----!
    !---- List of private subroutines ----!


    !---- Definitions ----!


    !---- Interfaces ----!


  Contains
    !
    !---- Routines ----!
    !

    !todo: mv (and clean) datapath                to stringio
    !todo: mv icsevu icsccu zbrent                to math_imsl
    !todo: mv bskm cubspl sort_spl                to math

!C   IMSL ROUTINE NAME   - ICSEVU                                        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
!C   COMPUTER            - VAX/DOUBLE                                    
!C                                                                       
!C   LATEST REVISION     - JANUARY 1, 1978                               
!C                                                                       
!C   PURPOSE             - EVALUATION OF A CUBIC SPLINE                  
!C                                                                       
!C   USAGE               - CALL ICSEVU(X,Y,NX,C,IC,U,S,M,IER)            
!C                                                                       
!C   ARGUMENTS    X      - VECTOR OF LENGTH NX CONTAINING THE ABSCISSAE  
!C                           OF THE NX DATA POINTS (X(I),Y(I)) I=1,...,  
!C                           NX (INPUT). X MUST BE ORDERED SO THAT       
!C                           X(I) .LT. X(I+1).                           
!C                Y      - VECTOR OF LENGTH NX CONTAINING THE ORDINATES  
!C                           (OR FUNCTION VALUES) OF THE NX DATA POINTS  
!C                           (INPUT).                                    
!C                NX     - NUMBER OF ELEMENTS IN X AND Y (INPUT).        
!C                           NX MUST BE .GE. 2.                          
!C                C      - SPLINE COEFFICIENTS (INPUT). C IS AN NX-1 BY  
!C                           3 MATRIX.                                   
!C                IC     - ROW DIMENSION OF MATRIX C EXACTLY AS          
!C                           SPECIFIED IN THE DIMENSION STATEMENT        
!C                           IN THE CALLING PROGRAM (INPUT).             
!C                           IC MUST BE .GE. NX-1.                       
!C                U      - VECTOR OF LENGTH M CONTAINING THE ABSCISSAE   
!C                           OF THE M POINTS AT WHICH THE CUBIC SPLINE   
!C                           IS TO BE EVALUATED (INPUT).                 
!C                S      - VECTOR OF LENGTH M (OUTPUT).                  
!C                           THE VALUE OF THE SPLINE APPROXIMATION AT    
!C                           U(I) IS                                     
!C                           S(I) = ((C(J,3)*D+C(J,2))*D+C(J,1))*D+Y(J)  
!C                           WHERE X(J) .LE. U(I) .LT. X(J+1) AND        
!C                           D = U(I)-X(J).                              
!C                M      - NUMBER OF ELEMENTS IN U AND S (INPUT).        
!C                IER    - ERROR PARAMETER (OUTPUT).                     
!C                         WARNING ERROR                                 
!C                           IER = 33, U(I) IS LESS THAN X(1).           
!C                           IER = 34, U(I) IS GREATER THAN X(NX).       
!C                                                                       
!C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
!C                       - SINGLE/H36,H48,H60                            
!C                                                                       
!C   REQD. IMSL ROUTINES - UERTST,UGETIO                                 
!C                                                                       
!C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
!C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
!C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
!C                                                                       
!C   REMARKS  1.  THE ROUTINE ASSUMES THAT THE ABSCISSAE OF THE NX       
!C                DATA POINTS ARE ORDERED SUCH THAT X(I) IS LESS THAN    
!C                X(I+1) FOR I=1,...,NX-1. NO CHECK OF THIS CONDITION    
!C                IS MADE IN THE ROUTINE. UNORDERED ABSCISSAE WILL CAUSE 
!C                THE ALGORITHM TO PRODUCE INCORRECT RESULTS.            
!C            2.  THE ROUTINE GENERATES TWO WARNING ERRORS. ONE ERROR    
!C                OCCURS IF U(I) IS LESS THAN X(1), FOR SOME I IN THE    
!C                THE INTERVAL (1,M) INCLUSIVELY. THE OTHER ERROR OCCURS 
!C                IF U(I) IS GREATER THAN X(NX), FOR SOME I IN THE       
!C                INTERVAL (1,M) INCLUSIVELY.                            
!C            3.  THE ORDINATE Y(NX) IS NOT USED BY THE ROUTINE. FOR     
!C                U(K) .GT. X(NX-1), THE VALUE OF THE SPLINE, S(K), IS   
!C                GIVEN BY                                               
!C                 S(K)=((C(NX-1,3)*D+C(NX-1,2))*D+C(NX-1,1))*D+Y(NX-1)  
!C                WHERE D=U(K)-X(NX-1).                                  
!C                                                                       
!C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.       
!C                                                                       
!C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
!C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
!C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
SUBROUTINE ICSEVU  (X,Y,NX,C,IC,U,S,M,IER)                        
      implicit none
      integer(kind=ski) ::             NX,IC,M,IER                                    
      real(kind=skr)    ::    X(NX),Y(NX),C(IC,3),U(M),S(M)                  
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      integer(kind=ski) ::            I,JER,KER,NXM1,K                               
      real(kind=skr) ::    D,DD,ZERO                                      
      DATA               I/1/,ZERO/0.0D0/                               
!C                                  FIRST EXECUTABLE STATEMENT           
      JER = 0                                                           
      KER = 0                                                           
      IF (M .LE. 0) GO TO 9005                                          
      NXM1 = NX-1                                                       
      IF (I .GT. NXM1) I = 1                                            
!C                                  EVALUATE SPLINE AT M POINTS          
      DO 40 K=1,M                                                       
!C                                  FIND THE PROPER INTERVAL             
         D = U(K)-X(I)                                                  
         IF (D) 5,25,15                                                 
    5    IF (I .EQ. 1) GO TO 30                                         
         I = I-1                                                        
         D = U(K)-X(I)                                                  
         IF (D) 5,25,20                                                 
   10    I = I+1                                                        
         D = DD                                                         
   15    IF (I .GE. NX) GO TO 35                                        
         DD = U(K)-X(I+1)                                               
         IF (DD .GE. ZERO) GO TO 10                                     
         IF (D .EQ. ZERO) GO TO 25                                      
!C                                  PERFORM EVALUATION                   
   20    S(K) = ((C(I,3)*D+C(I,2))*D+C(I,1))*D+Y(I)                     
         GO TO 40                                                       
   25    S(K) = Y(I)                                                    
         GO TO 40                                                       
!C                                  WARNING - U(I) .LT. X(1)             
   30    JER = 33                                                       
         GO TO 20                                                       
!C                                  IF U(I) .GT. X(NX) - WARNING         
!C
!C floating point is a little too good on the 6000. instead of comparing
!C to zero, compare the absolute value to 1.0E-18D0.  otherwise every 
!C ray will generate an error message for the incoming value of
!C +PSIMAX.   
!C 1/23/92 decide to keep above change for all machines.
!C
   35    IF (DABS(DD).GT.(1.0E-18)) KER = 34                            
         D = U(K)-X(NXM1)                                               
         I = NXM1                                                       
         GO TO 20                                                       
   40 CONTINUE                                                          
      IER = MAX0(JER,KER)                                               
 9000 CONTINUE                                                          
!      IF (JER .GT. 0) CALL UERTST(JER,6HICSEVU)                         
!      IF (KER .GT. 0) CALL UERTST(KER,6HICSEVU)                         
      IF (JER .GT. 0) THEN
        print *,"Error from math_imsl icsevu. Called with JER: ",JER
        !stop 
        return
      ENDIF
      IF (KER .GT. 0) THEN
        print *,"Error from math_imsl icsevu. Called with KER: ",KER
        !stop 
        return
      ENDIF
 9005 RETURN                                                            
End Subroutine icsevu

    !
    !
    !

!C   IMSL ROUTINE NAME   - ICSCCU                                        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
!C   COMPUTER            - VAX/DOUBLE                                    
!C                                                                       
!C   LATEST REVISION     - JUNE 1, 1980                                  
!C                                                                       
!C   PURPOSE             - CUBIC SPLINE INTERPOLATION                    
!C                           (EASY-TO-USE VERSION)                       
!C                                                                       
!C   USAGE               - CALL ICSCCU (X,Y,NX,C,IC,IER)                 
!C                                                                       
!C   ARGUMENTS    X      - VECTOR OF LENGTH NX CONTAINING THE ABSCISSAE  
!C                           OF THE NX DATA POINTS (X(I),Y(I)) I=1,...,  
!C                           NX. (INPUT) X MUST BE ORDERED SO THAT       
!C                           X(I) .LT. X(I+1).                           
!C                Y      - VECTOR OF LENGTH NX CONTAINING THE ORDINATES  
!C                           (OR FUNCTION VALUES) OF THE NX DATA POINTS. 
!C                           (INPUT)                                     
!C                NX     - NUMBER OF ELEMENTS IN X AND Y. (INPUT) NX     
!C                           MUST BE .GE. 2.                             
!C                C      - SPLINE COEFFICIENTS. (OUTPUT) C IS AN NX-1 BY 
!C                           3 MATRIX. THE VALUE OF THE SPLINE           
!C                           APPROXIMATION AT T IS                       
!C                           S(T) = ((C(I,3)*D+C(I,2))*D+C(I,1))*D+Y(I)  
!C                           WHERE X(I) .LE. T .LT. X(I+1) AND           
!C                           D = T-X(I).                                 
!C                IC     - ROW DIMENSION OF MATRIX C EXACTLY AS          
!C                           SPECIFIED IN THE DIMENSION STATEMENT IN     
!C                           THE CALLING PROGRAM. (INPUT)                
!C                IER    - ERROR PARAMETER. (OUTPUT)                     
!C                         TERMINAL ERROR                                
!C                           IER = 129, IC IS LESS THAN NX-1.            
!C                           IER = 130, NX IS LESS THAN 2.               
!C                           IER = 131, INPUT ABSCISSA ARE NOT ORDERED   
!C                             SO THAT X(1) .LT. X(2) ... .LT. X(NX).    
!C                                                                       
!C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
!C                       - SINGLE/H36,H48,H60                            
!C                                                                       
!C   REQD. IMSL ROUTINES - UERTST,UGETIO                                 
!C                                                                       
!C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
!C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
!C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
!C                                                                       
!C   COPYRIGHT           - 1980 BY IMSL, INC. ALL RIGHTS RESERVED.       
!C                                                                       
!C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
!C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
!C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
SUBROUTINE ICSCCU (X,Y,NX,C,IC,IER)                               
!C                                  SPECIFICATIONS FOR ARGUMENTS         
      implicit none
      integer(kind=ski) ::            NX,IC,IER                                      
      real(kind=skr) ::    X(NX),Y(NX),C(IC,3)                            
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      integer(kind=ski) ::            IM1,I,JJ,J,MM1,MP1,M,NM1,NM2                   
      real(kind=skr) ::    DIVDF1,DIVDF3,DTAU,G,CNX(3)                    
!C                                  FIRST EXECUTABLE STATEMENT           
      NM1 = NX-1                                                        
      IER = 129                                                         
      IF (IC .LT. NM1) GO TO 9000                                       
      IER = 130                                                         
      IF (NX .LT. 2) GO TO 9000                                         
      IER = 131                                                         
      IF (NX .EQ. 2) GO TO 45                                           
!C                                  COMPUTE NOT-A-KNOT SPLINE            
      DO 5 M = 2,NM1                                                    
         MM1=M-1                                                        
         C(M,2) = X(M)-X(MM1)                                           
         IF (C(M,2).LE.0.0D0) GO TO 9000                                
         C(M,3) = (Y(M)-Y(MM1))/C(M,2)                                  
    5 CONTINUE                                                          
      CNX(2) = X(NX)-X(NM1)                                             
      IF (CNX(2).LE.0.0D0) GO TO 9000                                   
      CNX(3) = (Y(NX)-Y(NM1))/CNX(2)                                    
      IER = 0                                                           
      NM2 = NX-2                                                        
      IF (NX .GT. 3) GO TO 10                                           
      C(1,3) = CNX(2)                                                   
      C(1,2) = C(2,2)+CNX(2)                                            
      C(1,1) = ((C(2,2)+2.D0*C(1,2))*C(2,3)*CNX(2)+C(2,2)**2*CNX(3)) &
      /C(1,2)                                                           
      GO TO 20                                                          
   10 C(1,3) = C(3,2)                                                   
      C(1,2) = C(2,2)+C(3,2)                                            
      C(1,1) = ((C(2,2)+2.D0*C(1,2))*C(2,3)*C(3,2)+C(2,2)**2*C(3,3)) &
      /C(1,2)                                                           
      DO 15 M=2,NM2                                                     
         MP1=M+1                                                        
         MM1=M-1                                                        
         G = -C(MP1,2)/C(MM1,3)                                         
         C(M,1) = G*C(MM1,1)+3.D0*C(M,2)*C(MP1,3)+3.D0*C(MP1,2)*C(M,3)  
         C(M,3) = G*C(MM1,2)+2.D0*C(M,2)+2.D0*C(MP1,2)                  
   15 CONTINUE                                                          
   20 G = -CNX(2)/C(NM2,3)                                              
      C(NM1,1) = G*C(NM2,1)+3.D0*C(NM1,2)*CNX(3)+3.D0*CNX(2)*C(NM1,3)   
      C(NM1,3) = G*C(NM2,2)+2.D0*C(NM1,2)+2.D0*CNX(2)                   
      IF (NX.GT.3) GO TO 25                                             
      CNX(1)=2.D0*CNX(3)                                                
      CNX(3)=1.D0                                                       
      G=-1.D0/C(NM1,3)                                                  
      GO TO 30                                                          
   25 G = C(NM1,2)+CNX(2)                                               
      CNX(1) = ((CNX(2)+2.D0*G)*CNX(3)*C(NM1,2)+CNX(2)**2* &
      (Y(NM1)-Y(NX-2))/C(NM1,2))/G                                      
      G = -G/C(NM1,3)                                                   
      CNX(3) = C(NM1,2)                                                 
   30 CNX(3) = G*C(NM1,2)+CNX(3)                                        
      CNX(1) = (G*C(NM1,1)+CNX(1))/CNX(3)                               
      C(NM1,1) = (C(NM1,1)-C(NM1,2)*CNX(1))/C(NM1,3)                    
      DO 35 JJ=1,NM2                                                    
         J = NM1-JJ                                                     
         C(J,1) = (C(J,1)-C(J,2)*C(J+1,1))/C(J,3)                       
   35 CONTINUE                                                          
      DO 40 I=2,NM1                                                     
         IM1 = I-1                                                      
         DTAU = C(I,2)                                                  
         DIVDF1 = (Y(I)-Y(IM1))/DTAU                                    
         DIVDF3 = C(IM1,1)+C(I,1)-2.D0*DIVDF1                           
         C(IM1,2) = (DIVDF1-C(IM1,1)-DIVDF3)/DTAU                       
         C(IM1,3) = DIVDF3/DTAU**2                                      
   40 CONTINUE                                                          
      DTAU = CNX(2)                                                     
      DIVDF1 = (Y(NX)-Y(NM1))/DTAU                                      
      DIVDF3 = C(NM1,1)+CNX(1)-2.D0*DIVDF1                              
      C(NM1,2) = (DIVDF1-C(NM1,1)-DIVDF3)/DTAU                          
      C(NM1,3) = DIVDF3/DTAU**2                                         
      GO TO 9005                                                        
   45 IF (X(1) .GE. X(2)) GO TO 9000                                    
      IER = 0                                                           
      C(1,1) = (Y(2)-Y(1))/(X(2)-X(1))                                  
      C(1,2) = 0.0D0                                                    
      C(1,3) = 0.0D0                                                    
      GO TO 9005                                                        
 9000 CONTINUE                                                          
!      CALL UERTST(IER,6HICSCCU)                                         
      print *,"Error from math_imsl icsccu. Called with: ",IER
      !stop 
      return
 9005 RETURN                                                            
End  Subroutine icsccu                                                              
    !
    !
    !

!C   IMSL ROUTINE NAME   - ZBRENT                                        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
!C   COMPUTER            - VAX/DOUBLE                                    
!C                                                                       
!C   LATEST REVISION     - JANUARY 1, 1978                               
!C                                                                       
!C   PURPOSE             - ZERO OF A FUNCTION WHICH CHANGES SIGN IN A    
!C                           GIVEN INTERVAL (BRENT ALGORITHM)            
!C                                                                       
!C   USAGE               - CALL ZBRENT (F,EPS,NSIG,A,B,MAXFN,IER)        
!C                                                                       
!C   ARGUMENTS    F      - AN EXTERNAL FUNCTION SUBPROGRAM F(X)          
!C                           PROVIDED BY THE USER WHICH COMPUTES F FOR   
!C                           ANY X IN THE INTERVAL (A,B). (INPUT)        
!C                           F MUST APPEAR IN AN EXTERNAL STATEMENT IN   
!C                           THE CALLING PROGRAM                         
!C                EPS    - FIRST CONVERGENCE CRITERION (INPUT).  A ROOT, 
!C                           B, IS ACCEPTED IF ABS(F(B)) IS LESS THAN OR 
!C                           EQUAL TO EPS.  EPS MAY BE SET TO ZERO.      
!C                NSIG   - SECOND CONVERGENCE CRITERION (INPUT).  A ROOT,
!C                           B, IS ACCEPTED IF THE CURRENT APPROXIMATION 
!C                           AGREES WITH THE TRUE SOLUTION TO NSIG       
!C                           SIGNIFICANT DIGITS.                         
!C                A,B    - ON INPUT, THE USER MUST SUPPLY TWO POINTS, A  
!C                           AND B, SUCH THAT F(A) AND F(B) ARE OPPOSITE 
!C                           IN SIGN.                                    
!C                           ON OUTPUT, BOTH A AND B ARE ALTERED.  B     
!C                           WILL CONTAIN THE BEST APPROXIMATION TO THE  
!C                           ROOT OF F. SEE REMARK 1.                    
!C                MAXFN  - ON INPUT, MAXFN SHOULD CONTAIN AN UPPER BOUND 
!C                           ON THE NUMBER OF FUNCTION EVALUATIONS       
!C                           REQUIRED FOR CONVERGENCE.  ON OUTPUT, MAXFN 
!C                           WILL CONTAIN THE ACTUAL NUMBER OF FUNCTION  
!C                           EVALUATIONS USED.                           
!C                IER    - ERROR PARAMETER. (OUTPUT)                     
!C                         TERMINAL ERROR                                
!C                           IER = 129 INDICATES THE ALGORITHM FAILED TO 
!C                             CONVERGE IN MAXFN EVALUATIONS.            
!C                           IER = 130 INDICATES F(A) AND F(B) HAVE THE  
!C                             SAME SIGN.                                
!C                                                                       
!C   PRECISION/HARDWARE  - SINGLE AND DOUBLE/H32                         
!C                       - SINGLE/H36,H48,H60                            
!C                                                                       
!C   REQD. IMSL ROUTINES - UERTST,UGETIO                                 
!C                                                                       
!C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND           
!C                           CONVENTIONS IS AVAILABLE IN THE MANUAL      
!C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP  
!C                                                                       
!C   REMARKS  1.  ON EXIT FROM ZBRENT, WHEN IER=0, A AND B SATISFY THE   
!C                FOLLOWING,                                             
!C                F(A)*F(B) .LE.0,                                       
!C                ABS(F(B)) .LE. ABS(F(A)), AND                          
!C                EITHER ABS(F(B)) .LE. EPS OR                           
!C                ABS(A-B) .LE. MAX(ABS(B),0.1)*10.0**(-NSIG).           
!C                THE PRESENCE OF 0.1 IN THIS ERROR CRITERION CAUSES     
!C                LEADING ZEROES TO THE RIGHT OF THE DECIMAL POINT TO BE 
!C                COUNTED AS SIGNIFICANT DIGITS. SCALING MAY BE REQUIRED 
!C                IN ORDER TO ACCURATELY DETERMINE A ZERO OF SMALL       
!C                MAGNITUDE.                                             
!C            2.  ZBRENT IS GUARANTEED TO REACH CONVERGENCE WITHIN       
!C                K = (ALOG((B-A)/D)+1.0)**2 FUNCTION EVALUATIONS WHERE  
!C                  D=MIN(OVER X IN (A,B) OF                             
!C                    MAX(ABS(X),0.1)*10.0**(-NSIG)).                    
!C                THIS IS AN UPPER BOUND ON THE NUMBER OF EVALUATIONS.   
!C                RARELY DOES THE ACTUAL NUMBER OF EVALUATIONS USED BY   
!C                ZBRENT EXCEED SQRT(K). D CAN BE COMPUTED AS FOLLOWS,   
!C                  P = AMIN1(ABS(A),ABS(B))                             
!C                  P = AMAX1(0.1,P)                                     
!C                  IF ((A-0.1)*(B-0.1).LT.0.0) P = 0.1                  
!C                  D = P*10.0**(-NSIG)                                  
!C                                                                       
!C   COPYRIGHT           - 1977 BY IMSL, INC. ALL RIGHTS RESERVED.       
!C                                                                       
!C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN 
!C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,    
!C                           EXPRESSED OR IMPLIED, IS APPLICABLE.        
!C                                                                       
!C-----------------------------------------------------------------------
!C                                                                       
SUBROUTINE ZBRENT (F,EPS,NSIG,A,B,MAXFN,IER)                      
!C                                  SPECIFICATIONS FOR ARGUMENTS         
      implicit none
      integer(kind=ski)         :: NSIG,MAXFN,IER                                 
      real(kind=skr)  ::    F,EPS,A,B 
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      integer(kind=ski)         :: IC                                             
      real(kind=skr)  :: ZERO=0.0D0,HALF=0.5D0,ONE=1.0D0,&
                                 THREE=3.0D0,TEN=10.0D0
      real(kind=skr)  :: T,FA,FB,C,FC,D,E,TOL,RM,S,P,Q,R,RONE,TEMP      
!C                                  FIRST EXECUTABLE STATEMENT           
      IER = 0                                                           
      T = TEN**(-NSIG)                                                  
      IC = 2                                                            
      S = A                                                             
      FA = F(S)                                                         
      S = B                                                             
      FB = F(S)                                                         
!C                                  TEST FOR SAME SIGN                   
      IF (FA*FB.GT.ZERO) GO TO 50                                       
    5 C = A                                                             
      FC = FA                                                           
      D = B-C                                                           
      E = D                                                             
   10 IF (DABS(FC).GE.DABS(FB)) GO TO 15                                
      A = B                                                             
      B = C                                                             
      C = A                                                             
      FA = FB                                                           
      FB = FC                                                           
      FC = FA                                                           
   15 CONTINUE                                                          
      TOL = T*DMAX1(DABS(B),0.1D0)                                      
      RM = (C-B)*HALF                                                   
!C                                  TEST FOR FIRST CONVERGENCE CRITERIA  
      IF (DABS(FB).LE.EPS) GO TO 40                                     
!C                                  TEST FOR SECOND CONVERGENCE CRITERIA 
      IF (DABS(C-B).LE.TOL) GO TO 40                                    
!C                                  CHECK EVALUATION COUNTER             
      IF (IC.GE.MAXFN) GO TO 45                                         
!C                                  IS BISECTION FORCED                  
      IF (DABS(E).LT.TOL) GO TO 30                                      
      IF (DABS(FA).LE.DABS(FB)) GO TO 30                                
      S = FB/FA                                                         
      IF (A.NE.C) GO TO 20                                              
!C                                  LINEAR INTERPOLATION                 
      P = (C-B)*S                                                       
      Q = ONE-S                                                         
      GO TO 25                                                          
!C                                  INVERSE QUADRATIC INTERPOLATION      
   20 Q = FA/FC                                                         
      R = FB/FC                                                         
      RONE = R-ONE                                                      
      P = S*((C-B)*Q*(Q-R)-(B-A)*RONE)                                  
      Q = (Q-ONE)*RONE*(S-ONE)                                          
   25 IF (P.GT.ZERO) Q = -Q                                             
      IF (P.LT.ZERO) P = -P                                             
      S = E                                                             
      E = D                                                             
!C                                  IF ABS(P/Q).GE.75*ABS(C-B) THEN      
!C                                     FORCE BISECTION                   
      IF (P+P.GE.THREE*RM*Q) GO TO 30                                   
!C                                  IF ABS(P/Q).GE..5*ABS(S) THEN FORCE  
!C                                     BISECTION. S = THE VALUE OF P/Q   
!C                                     ON THE STEP BEFORE THE LAST ONE   
      IF (P+P.GE.DABS(S*Q)) GO TO 30                                    
      D = P/Q                                                           
      GO TO 35                                                          
!C                                  BISECTION                            
   30 E = RM                                                            
      D = E                                                             
!C                                  INCREMENT B                          
   35 A = B                                                             
      FA = FB                                                           
      TEMP = D                                                          
      IF (DABS(TEMP).LE.HALF*TOL) TEMP = DSIGN(HALF*TOL,RM)             
      B = B+TEMP                                                        
      S = B                                                             
      FB = F(S)                                                         
      IC = IC+1                                                         
      IF (FB*FC.LE.ZERO) GO TO 10                                       
      GO TO 5                                                           
!C                                  CONVERGENCE OF B                     
   40 A = C                                                             
      MAXFN = IC                                                        
      GO TO 9005                                                        
!C                                  MAXFN EVALUATIONS                    
   45 IER = 129                                                         
      A = C                                                             
      MAXFN = IC                                                        
      GO TO 9000                                                        
!C                                  TERMINAL ERROR - F(A) AND F(B) HAVE  
!C                                  THE SAME SIGN                        
   50 IER = 130                                                         
      MAXFN = IC                                                        
 9000 CONTINUE                                                          
!      CALL UERTST (IER,6HZBRENT)                                        
      print *,"Error from math_imsl zbrent. Called with IER: ",IER
      print *,"   EPS,NSIG,A,B,MAXFN,IER:", EPS,NSIG,A,B,MAXFN,IER
      !stop 
      return
 9005 RETURN                                                            
End  Subroutine zbrent
    !
    !
    !

!C+++
!C	SUBROUTINE	BSKM
!C
!C	PURPOSE		Computes the modified Bessel functions of
!C			order 1/2, 3/2 and 5/2
!C
!C---
SUBROUTINE BSKM(X,BK,IQ,ORD)

implicit none
! todo: remove implicits
!       implicit real(kind=skr) (a-h,o-z)
!       implicit integer(kind=ski)        (i-n)

real(kind=skr),intent(in)    :: x,ord
integer(kind=ski),intent(in) :: iq
real(kind=skr),intent(out)   :: bk

real(kind=skr)     :: g13, g23, pp, ppm, ppp, rmu, zz
integer(kind=ski)  :: k,j

G13 = 2.678938534707748D0
G23 = 1.354117939426400D0
K = 0
BK = 0.0D0

IF (X.LT.10.1) THEN
    IF (IQ.EQ.1)THEN
        PP = (X/2)**(-ORD)/G23-(X/2)**ORD/G13*3
        GO TO 250
        230 DO 240 J = 1,K
            PPP = PPP*(X/2.D0)**2/J/FLOAT(3*J-2)*3.0D0
        240 PPM = PPM*(X/2.D0)**2/J/FLOAT(3*J-1)*3.0D0
        PPP = PPP/G13*(X/2.D0)**ORD*3.0D0/(3*J-2)
        PPM = PPM/G23*(X/2.D0)**(-ORD)
        PP = PPM-PPP
        250 PP = PP*PI/2.D0/SIN(ORD*PI)
        BK = BK+PP
        IF(ABS(PP).LT.1.0D-20) RETURN
        K = K+1
        PPP = 1.0D0
        PPM = 1.0D0
        GOTO 230
    ELSE
        PP = (X/2.D0)**(-ORD)/G13-(X/2.D0)**ORD/G23*3.0D0/2.D0
        GO TO 350
        330 continue
        DO 340 J = 1,K
            PPP = PPP*(X/2.D0)**2/J/FLOAT(3*J-1)*3.0D0
        340 PPM = PPM*(X/2.D0)**2/J/FLOAT(3*J-2)*3.0D0
        PPP = PPP/G23*(X/2.D0)**ORD*3.0D0/(3*J-1)
        PPM = PPM/G13*(X/2.D0)**(-ORD)
        PP = PPM-PPP
        350 PP = PP*PI/2.D0/SIN(ORD*PI)
        BK = BK+PP
        IF(ABS(PP).LT.1.0D-20) RETURN
        K = K+1
        PPP = 1.0D0
        PPM = 1.0D0
        GO TO 330
    END IF
ELSE
    ZZ = 8*X
    BK = 1.0D0
    RMU = 4*ORD**2
    BK = (PI/2.D0/X)**0.5D0*EXP(-X)
    BK = BK*(1+(RMU-1)/ZZ+(RMU-1)*(RMU-9)/2/ZZ**2+(RMU-1)*(RMU-9)*&
               (RMU-25)/6.D0/ZZ**3)
END IF
RETURN

End Subroutine bskm

    !
    ! This routine is used by the wiggler
    !

!C+++
!C	SUBROUTINE	PIECESPL
!C
!C	PURPOSE		Check for large increases in slope of the data.  
!C			If found, cubic spline interpolation is used for those
!C			data BEFORE the big jump, while linear interpolation 
!C			is used for those AFTER the increase, i.e. several
!C			segmented splines will be used for the data set.
!C			If not found, a single spline will be used for all data.
!C---
Subroutine PIECESPL (G, Y, N, IER)

! todo: remove implicits
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

real(kind=skr),dimension(5,N) :: G
real(kind=skr),dimension(N)   :: Y
integer(kind=ski)   :: N,IER

real(kind=skr),dimension(5,N) :: GTEMP
real(kind=skr),dimension(N)   :: YTEMP, SLOPE


integer(kind=ski)   :: i, ibottom, ifirst, itop,ntemp
real(kind=skr)      :: slope1, slope2


!C
!C First calculate the slope for each pair of data point.  
!C
DO 11 I = 1, N-1
    IF (G(1,I).EQ.G(1,I+1)) THEN
        SLOPE(I) = 1.0D20
    ELSE
        SLOPE(I) = ABS( (Y(I+1)-Y(I)) / (G(1,I+1)-G(1,I)) )
    END IF
11 CONTINUE

!C
!C Check if the slope increase 2.5 times more than adjacent one.
!C
SLOPE1 = SLOPE(1)
DO 21 I = 2, N-1
    SLOPE2 = SLOPE(I)
    IF (SLOPE2.GT.2.5D0*SLOPE1) THEN
        IFIRST = 1
        IBOTTOM = I
        GO TO 25
    END IF
    SLOPE1 = SLOPE2
21 CONTINUE
!C
!C Normal case. Can get by with one single spline.
!C
CALL CUBSPL (G, Y, N, IER)
100 CONTINUE

RETURN
!C
!C Piecewise splines case.
!C
!C Looks for limit of the spline (IFIRST -> IBOTTOM)
!C
20 SLOPE1 = SLOPE(IFIRST)
DO 31 I = IFIRST+1, N-1 
    SLOPE2 = SLOPE(I)
    IF (SLOPE2.GT.2.5D0*SLOPE1) THEN
        IBOTTOM = I ! Index at start of slope increase
        GO TO 25
    ELSE
        SLOPE1 = SLOPE2
    END IF
31 CONTINUE
IBOTTOM = I

!C
!C Looks for limit of the linear part (IBOTTOM -> ITOP).
!C
25 DO 41 I = IBOTTOM+1, N-1
    SLOPE2 = SLOPE(I)
    IF (SLOPE2.LT.SLOPE1) THEN
        ITOP = I ! Index at finish of slope increase
        GO TO 30
    END IF
41 CONTINUE
ITOP = N

30 NTEMP = IBOTTOM - IFIRST + 1
!C
!C The spline needs at least 4 points.  If less, might as well use linear.
!C
IF (NTEMP.LT.4) THEN
    IBOTTOM = IFIRST
    GO TO 40
END IF
!C
!C If at least 4 points, use the spline.
!C
DO 51 I = IFIRST, IBOTTOM
    GTEMP(1,I-IFIRST+1) = G(1,I)
    YTEMP(I-IFIRST+1) = Y(I)
51 CONTINUE
!C
!C Build the spline and store it in G.
!C
CALL CUBSPL (GTEMP, YTEMP, NTEMP, IER)
DO 61 I = IFIRST, IBOTTOM-1
    G(2,I) = GTEMP(2,I-IFIRST+1)
    G(3,I) = GTEMP(3,I-IFIRST+1)
    G(4,I) = GTEMP(4,I-IFIRST+1)
    G(5,I) = GTEMP(5,I-IFIRST+1)
61 CONTINUE
!C
!C Build the linear part
!C
40 DO 71 I = IBOTTOM, ITOP-1
    G(2,I) = Y(I)
    G(3,I) = SLOPE(I)
    G(4,I) = 0.0D0
    G(5,I) = 0.0D0
71 CONTINUE
!C
!C Move to the next segment of the data
!C
IFIRST = ITOP
IF (IBOTTOM.GE.N.OR.ITOP.GE.N) THEN
    GO TO 100 ! Finish
ELSE
    GO TO 20 ! Continue search
END IF

End Subroutine piecespl


!***************************************************************************
REAL(KIND=SKR) FUNCTION POL_PAR(XPAR_IN)

        ! todo: remove implicits
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        !DIMENSION X_IN(1),Y_OUT(1)

        implicit none

        real(kind=skr),   intent(inout) :: xpar_in
        real(kind=skr),dimension(1)     :: X_IN,Y_OUT
        integer(kind=ski)               :: N_ELEM,M_VECTOR,L_SPLINE

        N_ELEM   = NDIM_TRAJ
        M_VECTOR = 1
        L_SPLINE = NDIM_TRAJ-1 !1000
        X_IN(1)  = XPAR_IN

        CALL ICSEVU (ANG_ARR,PPAR_INT,N_ELEM,SPLI_1,L_SPLINE,X_IN,Y_OUT, &
                     M_VECTOR,IER)

        IF (IER.NE.0) WRITE(6,*)'Error in ICSEVU = ',IER
        
        POL_PAR = R_INPUT - Y_OUT(1)

        RETURN
End Function pol_par
    !
    !
    !

!  ***************************************************************************
REAL(KIND=SKR) FUNCTION POL_PER(XPAR_IN)

        ! todo: remove implicits
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        !DIMENSION X_IN(1),Y_OUT(1)

        implicit none

!C
        real(kind=skr),   intent(inout) :: xpar_in
        real(kind=skr),dimension(1)     :: X_IN,Y_OUT
        integer(kind=ski)               :: N_ELEM,M_VECTOR,L_SPLINE

        N_ELEM   = NDIM_TRAJ
        M_VECTOR = 1
        L_SPLINE = NDIM_TRAJ-1 !1000
        X_IN(1)  = XPAR_IN

        CALL ICSEVU (ANG_ARR,PPER_INT,N_ELEM,SPLI_2,L_SPLINE,X_IN,Y_OUT, &
                     M_VECTOR,IER)

        IF (IER.NE.0) WRITE(6,*)'Error in ICSEVU = ',IER

        POL_PER = R_INPUT - Y_OUT(1)

        RETURN
End Function pol_per
    !
    !
    !

!***************************************************************************
REAL(KIND=SKR) FUNCTION POL_TOT(XPAR_IN)

        ! todo: remove implicits
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none

!C
        real(kind=skr),   intent(inout) :: xpar_in
        real(kind=skr),dimension(1)     :: X_IN,Y_OUT
        integer(kind=ski)               :: N_ELEM,M_VECTOR,L_SPLINE
        !DIMENSION X_IN(1),Y_OUT(1)

        N_ELEM   = NDIM_TRAJ
        M_VECTOR = 1
        L_SPLINE = NDIM_TRAJ-1  !1000
        X_IN(1)  = XPAR_IN

        CALL ICSEVU (ANG_ARR,PTOT_INT,N_ELEM,SPLI_3,L_SPLINE,X_IN,Y_OUT, &
                     M_VECTOR,IER)

        IF (IER.NE.0) WRITE(6,*)'Error in ICSEVU = ',IER

        POL_TOT = R_INPUT - Y_OUT(1)

        RETURN
End Function pol_tot

!C+++
!C	SUBROUTINE	SETUP
!C
!C	PURPOSE		Initialize a set of variable to compute a SR
!C			spectrum.
!C
!C---
SUBROUTINE SETUP
!C

        ! todo: remove implicits
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none


        NP  = NDIM_TRAJ
        !srio !! THETA  = 1.0D0
        THETABM  = 1.0D0
!C ***** uses a 1 mrad angle (horizontal) ******
!C ***** band-pass is 1 eV   ******
        !srio !! CONST  = 3.951D+28*PHOT*THETA/1000.0D0*RAD**2.0D0
        CONST  = 3.951D+28*PHOT*THETABM/1000.0D0*RAD**2.0D0
        !GAMM  = 1957.0D0*BENER
        GAMM  = BENER/(codata_mee*1d-3)
        CONST  = CONST/GAMM**4
!C ***** THIS USES A CURRENT OF 1 mA ******
        CONST  = CONST*0.001D0
        RLCR  = 5.59D0*RAD/BENER**3
        !RL  = 12398.0D0/PHOT
        RL  = TOANGS/PHOT
        STEP  = 2.0D0*PSIMAX/(NP-1)
        FCT  = RLCR/2/RL

        RETURN
End Subroutine setup

!C+++
!C	SUBROUTINE	COMPUTE
!C
!C	PURPOSE		Performs actual SR computations.
!C
!C---
SUBROUTINE COMPUTE

! todo: remove implicits
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none
real(kind=skr)    :: arg2, arg3, cr_ener
integer(kind=ski) :: i, i1000, i1001, ier1, ier2, ier3, iq
real(kind=skr)    :: ord, par_max, perp_max, ppar_min, pper_min, ptot_max, ptot_min
real(kind=skr)    :: r_lcrit, ring_gamma, sig_eq, sum_par, sum_perp, sum_ptot, tot_pwr

DO 100 IC=1,NDIM_TRAJ

  PSI = - PSIMAX + (IC-1)*STEP 
  ARG = (1 + GAMM**2*PSI**2)
  X = ARG**1.5D0*FCT
  IQ = 1
  ORD = 1.0D0/3.0D0

  CALL BSKM(X,BK,IQ,ORD)

  ARG2 = BK**2*ARG*CONST*(ARG - 1)
  PPER(IC,2) = ARG2
  PPER(IC,1) = PSI*1000
  IQ = 2
  ORD = 2/3.0D0

  CALL BSKM(X,BK,IQ,ORD)

  ARG3 = BK**2*CONST*ARG**2
  PPAR(IC,2) = ARG3
  PPAR(IC,1) = PSI*1000
  PTOT(IC,2) = ARG2 + ARG3
  PTOT(IC,1) = PSI*1000

100 CONTINUE
!** Generates the cumulative distrib. functions by a tapezoidal integration **
 
SUM_PAR   = 0.0D0
SUM_PERP  = 0.0D0
SUM_PTOT  = 0.0D0

ANG_ARR(1) = - PSIMAX

DO 200 I=2,NDIM_TRAJ
  PSI = - PSIMAX + (I-1)*STEP
  ANG_ARR(I) = PSI
  SUM_PAR  = SUM_PAR  + (PPAR(I,2) + PPAR(I-1,2))/2*STEP
  SUM_PERP = SUM_PERP + (PPER(I,2) + PPER(I-1,2))/2*STEP
  SUM_PTOT = SUM_PTOT + (PTOT(I,2) + PTOT(I-1,2))/2*STEP
  PPAR_INT(I) = SUM_PAR
  PPER_INT(I) = SUM_PERP
200  PTOT_INT(I) = SUM_PTOT
!C
!C Computes some useful data
!C
CR_ENER =   2218*BENER**3/RAD
R_LCRIT =   12398.4D0/CR_ENER
TOT_PWR =   88.5D0*BENER**4/RAD
RING_GAMMA = 1957*BENER
SIG_EQ =   1.54D0*(CR_ENER/PHOT)**0.38D0/RING_GAMMA
OPEN (22,FILE='FLUX',STATUS='UNKNOWN')
 REWIND (22)
 WRITE (22,*) '-----------------------------------------------'
 WRITE (22,1001) BENER,RING_GAMMA,RAD
 WRITE (22,1002) CR_ENER,R_LCRIT
 WRITE (22,1003) TOT_PWR
 WRITE (22,*) 'Integrated flux at ',PHOT,' eV'
 WRITE (22,*) 'within limiting vertical angles (rads): '
 WRITE (22,*) -PSIMAX,PSIMAX
 WRITE (22,*) 'Units:  Photons/sec/mrad/ma/eV'
 WRITE (22,1000) 'parallel      : ',SUM_PAR
 WRITE (22,1000) 'perpendicular : ',SUM_PERP
 WRITE (22,1000) 'total         : ',SUM_PTOT
 WRITE (22,1004) SIG_EQ
CLOSE (22)

print *,'File written to disk: FLUX'
1000 FORMAT (1X,A,2X,E12.5)
1001 FORMAT (1X,'Machine Energy: ',G12.5,' GeV. GAMMA: ', &
                G12.5,'    Radius: ',G12.5,' m')
1002 FORMAT (1X,'Critical Energy= ',G12.5, &
                   ' eV, Critical Wavelength= ',G12.5,' Angs.')
1003 FORMAT (1X,'Total Radiated Power= ',G12.5,' kW')
1004 FORMAT (1X,'Approximated Gaussian width: ',g12.5,' rads')
!** Adjust now the c.d.f. between 0-1

PPAR_MIN = PPAR_INT(1)
PPER_MIN = PPER_INT(1)
PTOT_MIN = PTOT_INT(1)

DO 250 I=1,NDIM_TRAJ
 PPAR_INT(I) = PPAR_INT(I) - PPAR_MIN
 PPER_INT(I) = PPER_INT(I) - PPER_MIN
250 PTOT_INT(I) = PTOT_INT(I) - PTOT_MIN

PAR_MAX  = PPAR_INT(NDIM_TRAJ)
PERP_MAX = PPER_INT(NDIM_TRAJ)
PTOT_MAX = PTOT_INT(NDIM_TRAJ)

DO 300 I=1,NDIM_TRAJ
 PPAR_INT(I) = PPAR_INT(I)/PAR_MAX
 PPER_INT(I) = PPER_INT(I)/PERP_MAX
300 PTOT_INT(I) = PTOT_INT(I)/PTOT_MAX

!i1000 = 1000
!i1001 = 1001
i1000 = NDIM_TRAJ-1
i1001 = NDIM_TRAJ
CALL ICSCCU (ANG_ARR,PPAR_INT,i1001,SPLI_1,i1000,IER1)
CALL ICSCCU (ANG_ARR,PPER_INT,i1001,SPLI_2,i1000,IER2)
CALL ICSCCU (ANG_ARR,PTOT_INT,i1001,SPLI_3,i1000,IER3)

IF (IER1.NE.0.OR.IER2.NE.0.OR.IER3.NE.0)  &
          WRITE(6,*)'Error in ICSCCU ',IER1,IER2,IER3

RETURN
End Subroutine compute

!C+++
!C	SUBROUTINE	FILESAVE
!C
!C	PURPOSE		Write to disk the SR spectrum and flux data
!C
!C---
SUBROUTINE FILESAVE

implicit none

character(len=9),dimension(3)  :: filsav
character(len=5)               :: value
!CHARACTER *9 	FILSAV(3)
!CHARACTER *5	VALUE
!integer (kind=ski)  ::  
real(kind=skr)      :: x_in, pol_tot
integer(kind=ski)   :: i,k, iphot

IPHOT =   PHOT

WRITE (VALUE,100)  IPHOT
100 FORMAT (I5.5)
FILSAV(1) = 'SPAR'//VALUE
FILSAV(2) = 'SPER'//VALUE
FILSAV(3) = 'STOT'//VALUE

DO 200 IC = 1,3
    OPEN(UNIT = 20,FILE=FILSAV(IC),STATUS = 'UNKNOWN')
    REWIND (20)
    IF (IC.EQ.1)THEN
        WRITE(20,110) ((PPAR(I,K), K = 1,2), I = 1,NP,10)
    ELSE IF (IC.EQ.2) THEN
        WRITE(20,110) ((PPER(I,K),K = 1,2),I = 1,NP,10)
    ELSE
        WRITE(20,110) ((PTOT(I,K),K = 1,2),I = 1,NP,10)
    END IF
200 CLOSE(20)
110 FORMAT(1X,G12.5,10X,G12.5)

RETURN 
End Subroutine filesave
 

    !
    !
    !

!C+++
!C	SUBROUTINE	SOLVE
!C
!C	PURPOSE		Finds the zero of an equation
!C
!C---
SUBROUTINE SOLVE(Y_IN,X_OUT,I_FLAG)

! todo: remove implicits
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

real(kind=skr),intent(in)      :: y_in
integer(kind=ski),intent(in)   :: i_flag
real(kind=skr),intent(out)     :: x_out

real(kind=skr)      :: eps, var_1, var_2
integer(kind=ski)   :: max_call, n_digits


R_INPUT = Y_IN
EPS      = 0.0D0
N_DIGITS = 4
VAR_1 = - PSIMAX
VAR_2 =   PSIMAX
MAX_CALL = 50

IF (I_FLAG.EQ.1) THEN
    CALL ZBRENT (POL_PAR,EPS,N_DIGITS,VAR_1,VAR_2,MAX_CALL,IER)
    IF (IER.NE.0) WRITE(6,*)'Err.',IER,'in ZBRENT'
ELSE IF (I_FLAG.EQ.2) THEN
    CALL ZBRENT (POL_PER,EPS,N_DIGITS,VAR_1,VAR_2,MAX_CALL,IER)
    IF (IER.NE.0) WRITE(6,*)'Err.',IER,'in ZBRENT'
ELSE IF (I_FLAG.EQ.3) THEN
    CALL ZBRENT (POL_TOT,EPS,N_DIGITS,VAR_1,VAR_2,MAX_CALL,IER)
    IF (IER.NE.0) WRITE(6,*)'Err.',IER,'in ZBRENT'
ELSE
    CALL LEAVE ('ALADDIN','Error in Flag call to ALADDIN',I_FLAG)
END IF

X_OUT = VAR_2

RETURN
End Subroutine solve


!C+++
!C	SUBROUTINE	ALADDIN1
!C
!C	PURPOSE		a) To compute the SR vertical distribution at a
!C			   given photon energy and machine.
!C			b) to generate a random variate with the above
!C			   distribution.
!C
!C	ALGORITHM	Formulae published by K.Green in the BNL internal
!C			report. The Bessel function are computed directly
!C
!C	FLAGS		i_flag: if .lt. 0, initializtion call. The SR
!C			        distribuiton is computed using the data
!C                               in the common blocks /source/, /aladdin/.
!C
!C				if .gt. 0, the random variate is generated
!C				according to the former computation.
!C
!C	INPUT		r_var:  uniform random variate
!C
!C	OUTPUT		ang_out:elevation angle.
!C
!C           to disk:    4 files:
!C			 SPARxxxx.DAT  \
!C			 SPERxxxx.DAT  |--> SR vertical distribution.
!C			 STOTxxxx.DAT  /
!C			 FLUX.DAT      ---> Integrated Flux.
!C
!C----
!
SUBROUTINE ALADDIN1 (R_VAR,ANG_OUT,I_FLAG,IERROR)

        ! todo: remove implicits
	implicit real(kind=skr) (a-h,o-z)
	implicit integer(kind=ski)        (i-n)

			CHARACTER *6 	FILSAV,Q
!srio danger
!	COMMON	/FOURT/	FILSAV,Q
!C
     	IERROR	=  0
	IF (I_FLAG.LT.0) THEN

                ! srio renames photon_ to photon_underscore
     		!PHOT	=    PHOTON_(1)
     		!srio danger
                !  PHOT	=    PHOTON_underscore(1)
                PHOT	=    PH1
     		PSIMAX	=    MAX(VDIV1,VDIV2)
     		RAD	=    ABS(R_MAGNET)
!print *,'ALADDIN1: Setting RAD=',RAD,R_MAGNET
!srio commented this: I do not why this game BENER<->B_ENER
!srio BENER is in start.00 and should be used here.
!srio     		BENER	=    B_ENER

		CALL SETUP
		CALL COMPUTE
		CALL FILESAVE
	ELSE
!** The values of I_FLAG are :
!**	1. Parallel,
!**      2. Perpendicular,
!**	3. Total polarization.
		CALL SOLVE (R_VAR,ANG_OUT,I_FLAG)
     		IF (IER.NE.0) THEN
     		  IERROR = IER
     		  RETURN
     		END IF
!** If the total polarization is selected, R_VAR will contain on
!** return the degree of polarization at an angle ANG_OUT above the
!** machine plane. The definition is slightly different from usual, as
!** we define = 1 for pure parallel pol., = 0 for pure perpendicular,
!** so that 0.5 will be pure circular. The module SOURCE will then 
!** generate a randomly choosen polarization between the two.
     		IF (I_FLAG.EQ.3) THEN
     		 DO 100	I=1,NDIM_TRAJ
     		  TEST   =   ABS(ANG_OUT - PTOT(I,1)/1000)
     		  IF (TEST.LE.STEP)  GO TO 200
100		 CONTINUE
200		 CONTINUE
     		   IF (PTOT(I,2).NE.0.0D0) THEN
!C
!C Fix May 9, 1990.   SOURCE defined the degree of polarization as Ax/(Ax+Az),
!C instead of Ax^2/(Ax^2+Az^2).  So to calculate the degree of polarization, 
!C we need to take the square root of the powers (PPAR, PPER).
!C
!C OLD: 		     DEGREE	=   PPAR(I,2)/PTOT(I,2) 
     		     DEGREE	=   sqrt(PPAR(I,2)) &
     				/(sqrt(PPAR(I,2))+sqrt(PPER(I,2))) 
     		   ELSE
     		     DEGREE	=   1.0D0
     		   END IF
     		 R_VAR	=   DEGREE
     		ELSE IF (I_FLAG.EQ.2) THEN
     		  R_VAR	=   0.0D0   
     		ELSE
     		  R_VAR	=   1.0D0
     		END IF
	END IF
	RETURN
End Subroutine aladdin1



    !
    ! NOW THE ROUTINES NEEDED BY WHITE
    !

! srio danger
! INTSPL REMOVED (IT IS ALREADY IN shadow_math)

!C ------------------------------------------------------------------------
!C+++
!C	SUBROUTINE	WHTRCDF
!C
!C	PURPOSE		To read the unformatted CDF for SR radiation, and
!C			produce the spline coefficients for the 
!C			original photon energy CDF (PHOT), the
!C			inverted photon energy CDF (PHOT_INV) and the
!C			inverted vertical angle CDF (PSI_INV).
!C
!C---
SUBROUTINE WHTRCDF(RAD_MIN,RAD_MAX)

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

!srio move to shadow_variables
!srio	REAL*8		PHOT(5,1010),PHOT_INV(5,1010)
!srio	REAL*8		XPHOT(251),PSI_INV(5,21,251),PSI_POL(2,21,251)

!srio danger
!	COMMON /SPL_ARR/	PHOT,PHOT_INV,XPHOT,PSI_INV,PSI_POL

     	REAL*8		Y(1010)
        REAL*8		PORDER(10),RAD_MIN,RAD_MAX
     	REAL*8		WORKG(5,21),WORKY(21)
	REAL*8		EMAX,EMIN,CMAX,CMIN,EDIFF,CDIFF,PDIFF,DUMM

!srio danger
!	COMMON  /CDFINDEX/	IMAX_1,IMIN_1,IINT_1,NKOL,
!     $				IMAX_2,IMIN_2,IINT_2,IST

	character(len=1024)      :: SRSPEC, SRDISTR
!c
!c Get the data file path using either SHADOW$DATA or Unix SHADOW_DATA_DIR
!c environment variable. Also, check for existence in the routine itself.
!c
	IFLAG = 1
	CALL DATAPATH ('SRSPEC', SRSPEC, IFLAG) 
	IF (IFLAG.NE.0) THEN
          PRINT *,'File SRSPEC not found: Creating it!'
          CALL SrCdf
          SRSPEC='SRSPEC'
	ENDIF
!	IFLAG = 1
	CALL DATAPATH ('SRDISTR', SRDISTR, IFLAG) 
	IF (IFLAG.NE.0) THEN
          PRINT *,'File SRDISTR not found: Creating it!'
          CALL SrCdf
          SRDISTR='SRDISTR'
	ENDIF
!c
!c Define the useful parameters. Note that we now set the maximum energy
!c to 100*lam_c, instead of 10*Lam_C (EX_UPP = 1.0D) as it used to be.
!c 
	EX_LOW	= -5.0D0	
	EX_UPP	= 2.0D0
	EX_STEP	= (EX_UPP - EX_LOW)*1.0D-3
	GAMMA	= 1957.0D0*BENER
	RAD_MIN	= ABS(RAD_MIN)
	RAD_MAX	= ABS(RAD_MAX)
!c	R_LAM	= 4.0D0*PI*RAD/3.0D0/GAMMA**3*1.0D10	!Angstroms
	IF (F_COLOR.NE.3) THEN
	  C_PHOT	= TOANGS/4.0D0/PI/RAD_MIN*3.0D0*GAMMA**3*1.0D-10
	  DO  10 I = 1, NKOL
	    PORDER(I)	= PHOTON(I)/C_PHOT
10	  CONTINUE
	  EMAX	= PORDER(NKOL)
	  EMIN	= PORDER(1)	
	ELSE 
	  C_PHOT_MIN	= TOANGS/4.0D0/PI/RAD_MAX*3.0D0*GAMMA**3*1.0D-10
	  C_PHOT_MAX	= TOANGS/4.0D0/PI/RAD_MIN*3.0D0*GAMMA**3*1.0D-10
	  EMAX	= PHOTON(2)/C_PHOT_MIN
	  EMIN	= PHOTON(1)/C_PHOT_MAX
	END IF

!c Calculate the appropriate index for EMAX and EMIN
!c	IF (EMAX.GT.10.0**EX_UPP) CALL LEAVE
!c     $	  ('WHTRCDF','Maximum photon energy is too large.',0)
!c	IF (EMIN.LT.10.0**EX_LOW) CALL LEAVE
!c     $	  ('WHTRCDF','Minimum photon energy is too small.',0)
!c
     	!OPEN	(20, FILE=SRDISTR(1:IBLANK(SRDISTR)), STATUS='OLD', &
      	!	FORM='UNFORMATTED')
     	open	(unit=20, file=trim(SRDISTR), status='OLD', &
      		FORM='UNFORMATTED',IOSTAT=iErr)
        IF (iErr .ne. 0) THEN
           call leave ('WHTRCDF','Could not open file: '//trim(SRDISTR),&
                iErr)
        ENDIF
	READ 	(20)	ICOL,NP
	IMIN	= NP - (EX_UPP - LOG10(EMIN))/EX_STEP
	IMAX	= NP - (EX_UPP - LOG10(EMAX))/EX_STEP
	IMAX_1	= IMAX + 20
	IMIN_1	= IMIN - 20
	IF (IMAX_1.GT.NP) 	IMAX_1 = NP
	IF (IMIN_1.LT.1)	IMIN_1 = 1
	IINT_1	= IMAX_1 - IMIN_1 + 1
!c Reads in the total flux distribution
     	IF (F_COLOR.EQ.1)	GO TO 200
	DO  15 I = IMAX_1+1, NP
	  READ 	(20) 	
15	CONTINUE
	IF (F_SR_TYPE.EQ.0) THEN
	  DO 25 I = IINT_1, 1, -1
!srio	    READ(20)	PHOT(1,I),Y(I),DUMM
	    READ(20)	PHOT_SPLINE(1,I),Y(I),DUMM
	    Y(I)	= 1.0D0 - Y(I)
25	  CONTINUE
	ELSE
	  DO 35 I = IINT_1, 1, -1
!srio	    READ	(20)	PHOT(1,I),DUMM,Y(I)
	    READ	(20)	PHOT_SPLINE(1,I),DUMM,Y(I)
	    Y(I)	= 1.0D0 - Y(I)
35	  CONTINUE
	END IF
	CLOSE 	(20)

!c
!c Produces the original cdf curve of photon energy (PHOT).
!c
	IER	= 0
!srio	CALL CUBSPL  (PHOT,Y,IINT_1,IER)
	CALL CUBSPL  (PHOT_SPLINE,Y,IINT_1,IER)
	IF (F_COLOR.EQ.2) THEN
	  DO 45 I = 2, NKOL
	    WORKY(I)	= PORDER(I) - PORDER(I-1)
45	  CONTINUE
!srio	  WORKY(1)	= PORDER(1) - PHOT(1,1)
	  WORKY(1)	= PORDER(1) - PHOT_SPLINE(1,1)
!srio	  WORKY(NKOL+1)	= PHOT(1,IINT_1) - PORDER(NKOL)
	  WORKY(NKOL+1)	= PHOT_SPLINE(1,IINT_1) - PORDER(NKOL)
	  PDIFF		= WORKY(1)
	  DO 55 I = 2, NKOL+1
	    PDIFF	= MIN(PDIFF,WORKY(I))
55	  CONTINUE
	  PDIFF		= PDIFF/2.0D0
	  WORKY(1)	= 0.0D0
	  Y(1)		= 1.0D0
!c
!c For discrete energy, find the probability within +/- PDIFF of each energy.
!c
	  DO 65 I = 1, NKOL
!srio	    CALL SPL_INT (PHOT,IINT_1,PORDER(I)-PDIFF,CMIN,IER)
!print *,'WHTRCDF calls 1 SPL_INT (phot_spline) ...'
	    CALL SPL_INT (PHOT_SPLINE,IINT_1,PORDER(I)-PDIFF,CMIN,IER)
!srio	    CALL SPL_INT (PHOT,IINT_1,PORDER(I)+PDIFF,CMAX,IER)
!print *,'WHTRCDF calls 2 SPL_INT (phot_spline) ...'
	    CALL SPL_INT (PHOT_SPLINE,IINT_1,PORDER(I)+PDIFF,CMAX,IER)
	    WORKY(I+1)	= WORKY(I) + CMAX - CMIN
	    Y(I+1)	= I + 1.0D0
65	  CONTINUE
	  DO 75 I = 1, NKOL+1
	    PHOT_INV(1,I)	= WORKY(I)/WORKY(NKOL+1)
75	  CONTINUE
	ELSE
!C	  IF (EMAX.GT.10.0**EX_UPP) THEN
!C	    CMAX	= 1.0D0
!C	  ELSE	
!C	    CALL SPL_INT (PHOT,IINT_1,EMAX,CMAX,IER)
!C	  END IF
!C	  IF (EMAX.LT.10.0**EX_LOW) THEN
!C	    CMIN	= 0.0D0
!C	  ELSE
!C	    CALL SPL_INT (PHOT,IINT_1,EMIN,CMIN,IER)
!C	  END IF
!C	  CDIFF	= CMAX - CMIN
!C	  EDIFF	= EMAX - EMIN
!C	  IF (CDIFF.LT.1.0E-8) THEN
!C	    IF (EDIFF.LT.1.0E-8) THEN
!C	      DO I = 1, IINT_1
!C	        Y(I)		= (EMAX + EMIN)/2.0D0
!C	        PHOT_INV(1,I)	= I - 2
!C	      CONTINUE
!C	    ELSE	
!C	      DO I = 1, IINT_1
!C	        Y(I)		= PHOT(1,I)
!C	        PHOT_INV(1,I)	= (PHOT(1,I) - EMIN)/EDIFF
!C	      CONTINUE
!C	    END IF
!C	  ELSE
	    DO 85 I = 1, IINT_1
	      PHOT_INV(1,I) 	= Y(I)
!srio	      Y(I)		= PHOT(1,I)
	      Y(I)		= PHOT_SPLINE(1,I)
85	    CONTINUE
!C	  END IF
	  IER	= 0
	  CALL CUBSPL  (PHOT_INV,Y,IINT_1,IER)
	END IF
!c------------------------------------------------------------------------------
!c Now the angle data !
!c------------------------------------------------------------------------------
!c First calculate the appropriate indices
200	CONTINUE
     	OPEN	(30, FILE=SRSPEC(1:IBLANK(SRSPEC)), STATUS='OLD', &
      		FORM='UNFORMATTED',IOSTAT=iErr)
        IF (iErr .ne. 0) THEN
           call leave ('WHTRCDF','Could not open file: '//trim(SRSPEC),&
                iErr)
        ENDIF
     	READ	(30)	NPHOT,ICOL,IST
	ITRY	= ((NP - IMIN_1)/IST) + 2
	IMIN_2  = ((NP - IMAX_1)/IST) + 1
	ITOT	= NP/IST + 1
	IF (ITRY.GT.ITOT) THEN
	  IMAX_2	= ITOT
	ELSE
	  IMAX_2	= ITRY
	END IF
	IINT_2	= IMAX_2 - IMIN_2 + 1
!c Reads in the angle data for the desired polarization
	DO 90 I = 1, IMIN_2-1
	  DO 95 J = 1, 22
	    READ (30)
95	  CONTINUE
90	CONTINUE
	IF (F_POL.EQ.1) THEN
	  DO 110 I = IINT_2, 1, -1
	    READ (30)	XPHOT(I),DUMM
	    DO 105 J = 1, 21
	      READ (30)PSI_POL(1,J,I), PSI_INV(1,J,I),DUMM, DUMM,  &
      				PSI_POL(2,J,I)
105	    CONTINUE
110	  CONTINUE
	ELSE IF (F_POL.EQ.2) THEN
	  DO 120 I = IINT_2, 1, -1
	    READ (30)	XPHOT(I),DUMM
	    DO 115 J = 1, 21
	      READ (30)PSI_POL(1,J,I), DUMM, PSI_INV(1,J,I),DUMM, &
      				PSI_POL(2,J,I)
115	    CONTINUE
120	  CONTINUE
	ELSE
	  DO 130 I = IINT_2, 1, -1
	    READ (30)	XPHOT(I),DUMM
	    DO 125 J = 1, 21
	      READ (30)PSI_POL(1,J,I), DUMM, DUMM, PSI_INV(1,J,I), &
      				PSI_POL(2,J,I)
125	    CONTINUE
	    PSI_INV(1,21,I)	= 0.5D0
130	  CONTINUE
	END IF
	CLOSE	(30)
!c For each photon energy (XPHOT), generate the spline on angles
	DO 140 I = 1, IINT_2
	  DO 135 J = 1, 21
	    PSI_POL(1,J,I)	= PSI_POL(1,J,I)/GAMMA		!Radians
	    WORKG(1,J)	= PSI_INV(1,J,I)
	    WORKY(J)	= PSI_POL(1,J,I)
135	  CONTINUE
	  IER	= 0
	  i21 = 21
	  CALL CUBSPL (WORKG,WORKY,i21,IER)
	  DO 136 J = 1, 21
	    DO 137 K = 2, 5
	      PSI_INV(K,J,I)	= WORKG(K,J)
137	    CONTINUE
136	  CONTINUE
140	CONTINUE
	RETURN 
End Subroutine whtrcdf

    !
    !
    !

!C+++
!C	SUBROUTINE	WHTICDF
!C
!C	PURPOSE		To interpolate the photon energy from random 
!C			number and then the vertical angle at that 
!C			photon energy.
!C---
!srio changes this argument from RAD (global variable) to RAD33
!to avoid conflict!
SUBROUTINE WHTICDF (RAD33,CORREC,PSEED,ASEED,WAVE_NO,PSI,POLAR)

        ! todo: remove implicits
	implicit real(kind=skr) (a-h,o-z)
	implicit integer(kind=ski)        (i-n)

!srio mv to shadow_variables
!srio	REAL*8		PHOT(5,1010),PHOT_INV(5,1010)
!srio	REAL*8		XPHOT(251),PSI_INV(5,21,251),PSI_POL(2,21,251)
!srio danger
!	COMMON /SPL_ARR/	PHOT,PHOT_INV,XPHOT,PSI_INV,PSI_POL
!csrio	REAL*8		EPOLAR(2),WORKG(5,21),WORKP(4),WORKX(5,4)	
!csrio	REAL*8		PSEED,ASEED,ASEEDH,ATRY,PSI,POLAR,SLOPE, &
!csrio      			PDIFF
        real(kind=skr),dimension(2)    :: EPOLAR
        real(kind=skr),dimension(5,21) :: WORKG
        real(kind=skr),dimension(4)    :: WORKP
        real(kind=skr),dimension(5,4)  :: WORKX
        real(kind=skr) :: RAD33
        real(kind=skr) :: PSEED,ASEED,ASEEDH,ATRY,PSI,POLAR,SLOPE,PDIFF
!srio danger
!	COMMON  /CDFINDEX/	IMAX_1,IMIN_1,IINT_1,NKOL, &
!     $				IMAX_2,IMIN_2,IINT_2,IST
!C Define some useful data
	real(kind=skr) :: EX_UPP=2.0D0,EX_LOW=-5.0D0

	EX_STEP	= (EX_UPP - EX_LOW)*1.0D-3
!C	RAD	= ABS(R_MAGNET)
	GAMMA	= 1957.0D0*BENER
	R_LAM	= 4.0D0*PI*RAD33/3.0D0/GAMMA**3*1.0D2		!cm
	C_PHOT	= TOCM/R_LAM

!C Interpolate for the photon energy
     	IF (F_COLOR.EQ.1) THEN
          WAVE_NO	= TWOPI*PHOTON(1)/TOCM			!cm-1
     	  PE		= PHOTON(1)/C_PHOT/CORREC
	ELSE IF (F_COLOR.EQ.2) THEN
	  I = 1
11	  IF (PHOT_INV(1,I).LT.PSEED) THEN
	    I = I + 1
	  GOTO 11
      END IF
	  WAVE_NO	= TWOPI*PHOTON(I-1)/TOCM		!cm-1
	  PE		= PHOTON(I-1)/C_PHOT/CORREC
	ELSE
!C
!C Re-scale PSEED so that it only cover the range EMIN to EMAX.
!C

          EMIN = PHOTON(1)/C_PHOT
          EMAX = PHOTON(2)/C_PHOT
          !IF (EMIN.LT.PHOT_SPLINE(1,1)) EMIN = PHOT_SPLINE(1,1)
          !IF (EMAX.GT.PHOT_SPLINE(1,IINT_1)) EMAX = PHOT_SPLINE(1,IINT_1)
          IF (     (EMIN.LT.PHOT_SPLINE(1,1)) &
              .OR. (EMIN.GT.PHOT_SPLINE(1,IINT_1))  ) &
              EMIN = PHOT_SPLINE(1,1)
          IF (     (EMAX.GT.PHOT_SPLINE(1,IINT_1)) &
              .OR. (EMAX.LT.PHOT_SPLINE(1,1))   ) &
              EMAX = PHOT_SPLINE(1,IINT_1)

          CALL SPL_INT (PHOT_SPLINE,IINT_1,EMIN,CMIN,IER)
          CALL SPL_INT (PHOT_SPLINE,IINT_1,EMAX,CMAX,IER)
          PSEED = CMIN + PSEED*(CMAX-CMIN)
          CALL SPL_INT (PHOT_INV,IINT_1,PSEED,PE,IER)
          WAVE_NO = TWOPI*PE/R_LAM*CORREC     !cm-1
	END IF
!C For the angle, first find the indices of 'neighboring erergy' on XPHOT
	IPE	= IMAX_2 - (EX_UPP - LOG10(PE))/EX_STEP/IST
	IF (IPE-1.LT.1) THEN
	  ISTART = 1
	  IEND   = 4
	  IPOL	 = 1
	ELSE IF (IPE+2.GT.IINT_2) THEN
	  IEND   = IINT_2
	  ISTART = IEND - 3
	  IPOL   = IINT_2 - 1
	ELSE
	  ISTART = IPE - 1
	  IEND   = IPE + 2
	  IPOL	 = IPE
	END IF
	IF (ASEED.GT.0.5) THEN
	  ASEEDH = 1.0D0 - ASEED
	ELSE
	  ASEEDH = ASEED
	END IF
!C Interpolate the angles correspond to the four 'neighboring energy'
	DO 21 I = ISTART, IEND
	  DO  31 J = 1, 21 
	    DO 41 K = 1, 5
	      WORKG(K,J) = PSI_INV(K,J,I)
41	    CONTINUE
31	  CONTINUE

	  i21 = 21
	  CALL SPL_INT (WORKG,i21,ASEEDH,ATRY,IER)
	  WORKP(I-ISTART+1)	= ATRY
	  WORKX(1,I-ISTART+1)	= XPHOT(I)
21	CONTINUE
!C Use the four energy and their corresponding angles to interpolate for 
!C the angle at PE
	IER	= 0
	i4 = 4
	CALL CUBSPL  (WORKX,WORKP,i4,IER)
	CALL SPL_INT (WORKX,i4,PE,PSI,IER)
	IF (ASEED.GT.0.5)	PSI = -PSI		!Radians
!C Now the polarization
	IF (F_POL.EQ.1) THEN
	  POLAR	= 1.0D0
	ELSE IF (F_POL.EQ.2) THEN
	  POLAR	= 0.0D0
	ELSE
	  DO 51 I = IPOL, IPOL+1
	    J = 1
61 	    IF (-ABS(PSI).GT.PSI_POL(1,J,I)) THEN
	      J = J + 1
		GOTO 61
		END IF
	    IF (J.EQ.1) THEN
	      EPOLAR(I-IPOL+1)  = PSI_POL(2,1,I)
	    ELSE
	      SLOPE = (PSI_POL(2,J,I)-PSI_POL(2,J-1,I))/ &
      			(PSI_POL(1,J,I)-PSI_POL(1,J-1,I))
	      PDIFF = -ABS(PSI) - PSI_POL(1,J-1,I)
	      EPOLAR(I-IPOL+1)	= SLOPE*PDIFF + PSI_POL(2,J-1,I)
	    END IF
51	  CONTINUE
	  SLOPE	= (EPOLAR(2)-EPOLAR(1))/(XPHOT(IPOL+1)-XPHOT(IPOL))
	  PDIFF = PE - XPHOT(IPOL)
	  POLAR = SLOPE*PDIFF + EPOLAR(1)
	END IF 
	RETURN
End Subroutine whticdf

!C+++
!C	SUBROUTINE	WHITE
!C
!C	PURPOSE		IER = 0, initialization and set up the splines of
!C				 inverted cdf.
!C			IER > 0, interpolate for the photon energy,vertical
!C				 angle,and the degree of polarization.
!C			IER < 0, purge away the longer arrays.
!C
!C---
SUBROUTINE WHITE (RAD1,RAD2,A_PSEED,A_ASEED,A_WAVE_NO,A_PSI,A_POLAR,IER)

!srio danger
!	COMMON /CDFINDEX/	IMAX_1,IMIN_1,IINT_1,NKOL,
!     $				IMAX_2,IMIN_2,IINT_2,IST

        ! todo: remove implicits
	implicit real(kind=skr) (a-e,g-h,o-z)
	implicit integer(kind=ski)        (f,i-n)

!srio mv to shadow_variables
!srio	REAL*8		PHOT(5,1010),PHOT_INV(5,1010)
!srio	REAL*8		XPHOT(251),PSI_INV(5,21,251),PSI_POL(2,21,251)

!srio danger
!	COMMON /SPL_ARR/	PHOT,PHOT_INV,XPHOT,PSI_INV,PSI_POL

        real(kind=skr),dimension(10) :: porder
        real(kind=skr) :: RAD1,RAD2,PSEED,ASEED,PSI,POLAR

	IF (IER.EQ.0.0D0)	GO TO 100
	IF (IER.LT.0.0D0)	GO TO 200
     	PSEED	=  A_PSEED
     	ASEED   =  A_ASEED
	CALL WHTICDF (RAD1,RAD2,PSEED,ASEED,A_WAVE_NO,PSI,POLAR)
     	A_PSI	=  PSI
     	A_POLAR	=  POLAR
	RETURN
!C
!C First arrange the photon energy in ascending order.
!C
100	IF (F_COLOR.NE.1) THEN
     	  IF (F_COLOR.EQ.2) THEN
	    NKOL	= N_COLOR
	  ELSE IF (F_COLOR.EQ.3) THEN
	    NKOL 	= 2
	  END IF
	  DO  10 J = 1, NKOL
	    IPMIN = 1
	    DO 15 I = 2, NKOL
	      IF (PHOTON(I).LT.PHOTON(IPMIN))	IPMIN = I
15	    CONTINUE
	    PORDER(J)	 = PHOTON(IPMIN)
	    PHOTON(IPMIN)  = 1D20
10	  CONTINUE
          DO 20 J = 1, NKOL
            PHOTON(J)	= PORDER(J)
20          CONTINUE
     	ELSE
          NKOL	= 2
          PHOTON(2)	= PHOTON(1)
     	END IF
	CALL WHTRCDF (RAD1,RAD2)
	RETURN
200	CONTINUE
	RETURN
End Subroutine white


! C+++
! C	SUBROUTINE	SOURCE
! C
! C	PURPOSE		To generate a (18,ndim) array describing the system source.
! C
! C	INPUTS		From modules SETSOUR, MSETUP, through COMMON.BLK
! C			The following two input parameters are meant to
! C			be passed to the WRITE_OFF subroutine to control
! C			the output format.
! C	    INFILE	Name of the input start file. 
! C	    FNAME	Name of the output file. This enables us to use
! C			use ASCII or BINARY output files (BEGIN.DAT's).
! C	    IOFORM = 0, Binary output file.
! C		   = 1, ASCII output file. (only unix version)
! C
! C	OUTPUTS		File BEGIN.DAT
! C
! C	MODIFICATIONS	Included wiggler and undulator source (2/26/88)
! C	                srio@esrf.eu 20140617 fixed emittance effects. Implicit none: Viva!!
! C
! C---
SUBROUTINE SOURCESYNC (pool00, ray, npoint1) bind(C,NAME="SourceSync")

implicit none

integer(kind=ski), intent(in)                         :: npoint1
real(kind=skr), dimension(18,npoint1), intent(in out) :: ray
type (poolSource), intent(inout)                     ::  pool00

integer(kind=ski)         :: IOFORM
integer(kind=ski)         :: C_X,C_Y,C_Z,C_VX,C_VZ,C_XN,C_ZN

character(len=sklen)       :: errmsg
    
!!srio for SR, force 18 columns

!! needed for calling source_bound
real(kind=skr),dimension(3)  :: XDUM, YDUM
real(kind=skr),dimension(3)  :: DIREC,AP_VEC,E_TEMP,SB_POS
real(kind=skr),dimension(3)  :: VTEMP,A_VEC,A_TEMP, E_BEAM
real(kind=skr), dimension(6,NPOINT1) :: grid
real(kind=skr),dimension(10)  :: SIGXL,SIGZL
    
! insertion device arrays
real(kind=skr), dimension(:,:),allocatable :: seed_y,y_x,y_xpri, y_z,y_zpri
real(kind=skr), dimension(:,:),allocatable :: y_curv,y_path

real(kind=skr),dimension(:),allocatable :: y_temp,c_temp,x_temp,z_temp,ang_temp
real(kind=skr),dimension(:),allocatable :: p_temp,ang2_temp,abd2_temp 
    
    
real(kind=skr) :: YRAN,DPS_RAN1,DPS_RAN2
real(kind=skr) :: TMP_A,TMP_B,DPS_RAN3

real(kind=skr),dimension(:,:,:),allocatable :: CDFX,D_POL,UPHI
real(kind=skr),dimension(:,:),allocatable   :: CDFZ,UTHETA
real(kind=skr),dimension(:),allocatable     :: CDFW,UENER

real(kind=skr),dimension(10)       :: RELINT,PRELINT
real(kind=skr),dimension(4)        :: II,DX,PHI_INT
real(kind=skr),dimension(2)        :: JI,DZ,THE_INT 
   
integer(kind=ski) :: n_rej=0, k_rej=0, use_wiggler_binary_files, use_undulator_binary_files
real(kind=skr) :: xxx=0.0,yyy=0.0,zzz=0.0

! removing implicits...
real(kind=skr)    :: ax_a_z,ang_cone,angin,angin1,angin2,angle,angle1,angle2,angle3,anglev
real(kind=skr)    :: anglex, arg_ang, arg_ener, arg_vx, arg_vz, arg_x, arg_y, arg_z
real(kind=skr)    :: ax, az, cin, cl_vx, cl_vz, cl_x, a_x, a_z, cl_xn, cl_y, cl_z, cl_zn
real(kind=skr)    :: correc, curv, cxmax, czmax, deltai, deltaj, deltak, denom, dir_x, dir_z
real(kind=skr)    :: dk, epsi_path, epsi_wx, epsi_wz, epsi_xold, epsi_zold, eseed
integer(kind=ski) :: i,i0,i1,i_change,iangle,idumm,iflag,index,indexmom,indexspa,istat,itest
integer(kind=ski) :: itik, itmp, itotray, j, jnow, k, ki, kk, mm, n_test, ne, nmom
integer(kind=ski) :: np_sy, np_traj, krej, nrej, nspace, nt, ntotal

real(kind=skr)    :: path0, path_step, phasex, phasez, phi, penergy, phi1, phi2, phi_x, phi_z
real(kind=skr)    :: phir, phot_ch, q_wave, rad_max, rad_min, radius, rhox, rhoz 
real(kind=skr)    :: rsigmax, rsigmaz, rsigmaxp, rsigmazp, seedin, sigmaxp, sigmazp
real(kind=skr)    :: step_vx, step_vz, step_x, step_xn, step_y, step_z, step_zn
real(kind=skr)    :: temp, tempx, tempy, tempz, theta1, theta2, theta3, theta4, thetar
real(kind=skr)    :: x_traj, xin, xmax1, xmax2, xrand, xseed, y_traj, y_traj_old, yin
real(kind=skr)    :: z_traj, zin, zmax1, zmax2, zrand, zseed

! load gfile (moved from gen_source)
    
n_rej=0
k_rej=0
xxx=0.0
yyy=0.0
zzz=0.0
!
! put inputs (pool) into global variables
!
!todo: work without globals!!!!
CALL PoolSourceToGlobal(pool00)

    
ISTAT = 0
IDUMM = 0
   
KREJ = 0
NREJ = 0

call init_random_seed(ISTAR1)
! C
! C Sets up some variables needed for the rest of the routine
! C
! C First figure out the number of columns written out for each ray.
! C
IF (F_POLAR.EQ.1) THEN
   NCOL = 18
ELSE IF (F_OPD.EQ.1) THEN
   NCOL = 13
ELSE
   NCOL = 12
END IF
    
    
IF (F_WIGGLER.EQ.1) THEN
    ! C
    ! C Normal wigger case:
    ! C read in the wiggler trajectory, tangent and radius, and other parameters
    ! C
    !OPEN (29, FILE=FILE_TRAJ, STATUS='OLD', FORM='UNFORMATTED')

    use_wiggler_binary_files = 0
    OPEN (29, FILE=FILE_TRAJ, STATUS='OLD', FORM='FORMATTED')
    READ (29,*,err=898) NP_TRAJ,PATH_STEP,BENER,RAD_MIN,RAD_MAX,PH1,PH2

go to 899

898  continue
    ! try binary file
    close(29)
    OPEN (29, FILE=FILE_TRAJ, STATUS='OLD', FORM='UNFORMATTED')
    READ (29) NP_TRAJ,PATH_STEP,BENER,RAD_MIN,RAD_MAX,PH1,PH2
    use_wiggler_binary_files = 1

899  continue

    !array allocation
    allocate( y_temp(NP_TRAJ) )
    allocate( c_temp(NP_TRAJ) )
    allocate( x_temp(NP_TRAJ) )
    allocate( z_temp(NP_TRAJ) )
    allocate( ang_temp(NP_TRAJ) )
    allocate( p_temp(NP_TRAJ) )
    allocate( ang2_temp(NP_TRAJ) )
    allocate( abd2_temp(NP_TRAJ) )

    allocate( seed_y(5,NP_TRAJ) )
    allocate( y_x(5,NP_TRAJ) )
    allocate( y_xpri(5,NP_TRAJ) )
    allocate( y_z(5,NP_TRAJ) )
    allocate( y_zpri(5,NP_TRAJ) )
    allocate( y_curv(5,NP_TRAJ) )
    allocate( y_path(5,NP_TRAJ) )
     

    DO 13 I = 1, NP_TRAJ
        if (use_wiggler_binary_files .EQ. 0) then
            READ (29,*) XIN,YIN,SEEDIN,ANGIN,CIN
        else
            READ (29) XIN,YIN,SEEDIN,ANGIN,CIN
        endif
        ! C+++
        ! C The program will build the splines for generating the stocastic source.
        ! C the splines are defined by:
        ! C
        ! C      Y(X) = G(2,I)+X(I)*(G(3,I)+X(I)*(G(4,I)+X(I)*G(5,I)))
        ! C
        ! C which is valid between the interval X(I) and X(I+1)
        ! C
        ! C We define the 5 arrays:
        ! C    Y_X(5,N)    ---> X(Y)
        ! C    Y_XPRI(5,N) ---> X'(Y)
        ! C    Y_CURV(5,N) ---> CURV(Y)
        ! C    Y_PATH(5,N) ---> PATH(Y)
        ! C    F(1,N) contains the array of Y values where the nodes are located.
        ! C+++
        Y_TEMP(I)   = YIN*CONV_FACT ! Convert to user units
        X_TEMP(I)   = XIN*CONV_FACT ! Convert to user units
        SEED_Y(1,I) = SEEDIN
        ANG_TEMP(I) = ANGIN
        C_TEMP(I)   = CIN
        P_TEMP(I)   = (I-1)*PATH_STEP*CONV_FACT ! Convert to user units
        ! C
        ! C Array initialization:
        ! C
        Y_X(1,I)    = Y_TEMP(I)
        Y_XPRI(1,I) = Y_TEMP(I)
        Y_CURV(1,I) = Y_TEMP(I)
        Y_PATH(1,I) = Y_TEMP(I)
    13 CONTINUE   ! end loop on trajectrory points
    CLOSE (29)

    ! C
    ! C Generate the (5) splines. Notice that the nodes are always in the first
    ! C element already.
    ! C      Y_X     : on input, first row contains nodes.
    ! C      X_TEMP  : input array to which to fit the splines
    ! C      NP_TRAJ : # of spline points
    ! C      IER     : status flag
    ! C On output:
    ! C      Y_X(1,*)    : spline nodes
    ! C      Y_X(2:5,*)  : spline coefficients (relative to X_TEMP)
    ! C
    NP_SY = NP_TRAJ
    IER = 1
    ! C*************************************

    CALL PIECESPL(SEED_Y, Y_TEMP,   NP_SY,   IER)
    IER = 0
    CALL CUBSPL (Y_X,    X_TEMP,   NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_XPRI, ANG_TEMP, NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_CURV, C_TEMP,   NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_PATH, P_TEMP,   NP_TRAJ, IER)
    ! C+++
    ! C Compute the path length to the middle (origin) of the wiggler.
    ! C We need to know the "center" of the wiggler coordinate.
    ! C input:     Y_PATH  ---> spline array
    ! C            NP_TRAJ ---> # of points
    ! C            Y_TRAJ  ---> calculation point (ind. variable)
    ! C output:    PATH0   ---> value of Y_PATH at X = Y_TRAJ. If
    ! C                         Y_TRAJ = 0, then PATH0 = 1/2 length 
    ! C                         of trajectory.
    ! C+++
    Y_TRAJ = 0.0D0
    CALL SPL_INT (Y_PATH, NP_TRAJ, Y_TRAJ, PATH0, IER)
    ! C
    ! C These flags are set because of the original program structure.
    ! C
    F_PHOT  = 0
    F_COLOR  = 3
    ! C FGRID  = 0
    FSOUR  = 3
    FDISTR  = 4
ELSE IF (F_WIGGLER.EQ.3) THEN
    ! C
    ! C Elliptical wiggler case:
    ! C
    OPEN (29, FILE=FILE_TRAJ, STATUS='OLD', FORM='UNFORMATTED')
    READ (29) NP_TRAJ,PATH_STEP,BENER,RAD_MIN,RAD_MAX,PH1,PH2
 
    !array allocation
    allocate( y_temp(NP_TRAJ) )
    allocate( c_temp(NP_TRAJ) )
    allocate( x_temp(NP_TRAJ) )
    allocate( z_temp(NP_TRAJ) )
    allocate( ang_temp(NP_TRAJ) )
    allocate( p_temp(NP_TRAJ) )
    allocate( ang2_temp(NP_TRAJ) )
    allocate( abd2_temp(NP_TRAJ) )

    allocate( seed_y(5,NP_TRAJ) )
    allocate( y_x(5,NP_TRAJ) )
    allocate( y_xpri(5,NP_TRAJ) )
    allocate( y_z(5,NP_TRAJ) )
    allocate( y_zpri(5,NP_TRAJ) )
    allocate( y_curv(5,NP_TRAJ) )
    allocate( y_path(5,NP_TRAJ) )
     
    DO 14 I = 1, NP_TRAJ
        READ (29) XIN,YIN,ZIN,SEEDIN,ANGIN1,ANGIN2,CIN 
        ! C+++
        ! C The program will build the splines for generating the stocastic source.
        ! C the splines are defined by:
        ! C
        ! C      Y(X) = G(2,I)+X(I)*(G(3,I)+X(I)*(G(4,I)+X(I)*G(5,I)))
        ! C
        ! C which is valid between the interval X(I) and X(I+1)
        ! C
        ! C We define the 7 arrays:
        ! C    Y_X(5,N)    ---> X(Y)
        ! C    Y_XPRI(5,N) ---> X'(Y)
        ! C    Y_Z(5,N)    ---> Z(Y)
        ! C    Y_ZPRI(5,N) ---> Z'(Y)
        ! C    Y_CURV(5,N) ---> CURV(Y)
        ! C    Y_PATH(5,N) ---> PATH(Y)
        ! C    F(1,N) contains the array of Y values where the nodes are located.
        ! C+++
        Y_TEMP(I) = YIN*CONV_FACT! Convert to user units
        X_TEMP(I) = XIN*CONV_FACT! Convert to user units
        Z_TEMP(I) = ZIN*CONV_FACT! Convert to user units
        SEED_Y(1,I) = SEEDIN
        ANG_TEMP(I) = ANGIN1
        ANG2_TEMP(I) = ANGIN2
        C_TEMP(I) = CIN
        P_TEMP(I) = (I-1)*PATH_STEP*CONV_FACT! Convert to user units
        ! C
        ! C Array initialization:
        ! C
        Y_X(1,I) = Y_TEMP(I)
        Y_XPRI(1,I) = Y_TEMP(I)
        Y_Z(1,I) = Y_TEMP(I)
        Y_ZPRI(1,I) = Y_TEMP(I)
        Y_CURV(1,I) = Y_TEMP(I)
        Y_PATH(1,I) = Y_TEMP(I)
    14	  CONTINUE
    CLOSE (29)
    ! C
    ! C Generate the (7) splines. Notice that the nodes are always in the first
    ! C element already.
    ! C      Y_X (or Y_Z)       : on input, first row contains nodes.
    ! C      X_TEMP (or Z_TEMP) : input array to which fit the splines
    ! C      NP_TRAJ : # of spline points
    ! C      IER     : status flag
    ! C On output:
    ! C      Y_X(1,*) or (Y_Z(1,*))     : spline nodes
    ! C      Y_X(2:5,*) (or Y_Z(2:5,*)) : spline coefficients (relative to
    ! C                                   X_TEMP (or Z_TEMP))
    ! C
    NP_SY = NP_TRAJ
    IER = 1
    ! C*************************************
    CALL PIECESPL(SEED_Y, Y_TEMP,   NP_SY,   IER)
    IER = 0
    CALL CUBSPL (Y_X,    X_TEMP,   NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_Z,    Z_TEMP,   NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_XPRI, ANG_TEMP, NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_ZPRI, ANG2_TEMP, NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_CURV, C_TEMP,   NP_TRAJ, IER)
    IER = 0
    CALL CUBSPL (Y_PATH, P_TEMP,   NP_TRAJ, IER)
    ! C+++
    ! C Compute the path length to the middle (origin) of the wiggler.
    ! C We need to know the "center" of the wiggler coordinate.
    ! C input:     Y_PATH  ---> spline array
    ! C            NP_TRAJ ---> # of points
    ! C            Y_TRAJ  ---> calculation point (ind. variable)
    ! C output:    PATH0   ---> value of Y_PATH at X = Y_TRAJ. If
    ! C                         Y_TRAJ = 0, then PATH0 = 1/2 length 
    ! C                         of trajectory.
    ! C+++
    Y_TRAJ = 0.0D0
    CALL SPL_INT (Y_PATH, NP_TRAJ, Y_TRAJ, PATH0, IER)
    ! C
    ! C These flags are set because of the original program structure.
    ! C
    F_PHOT  = 0
    F_COLOR  = 3
    ! C FGRID  = 0
    FSOUR  = 3
    FDISTR  = 4
ELSE IF (F_WIGGLER.EQ.2) THEN
    ! C
    ! C Uudulator case : first read in the CDF's and degree of polarization.
    ! C


    use_undulator_binary_files = 0

    OPEN(30, FILE=FILE_TRAJ, STATUS='OLD',FORM='FORMATTED')
    READ(30,*,err=1313) NE,NT,NP,IANGLE
    GO TO 1414

    1313 continue
    close(30)
    OPEN(30, FILE=FILE_TRAJ, STATUS='OLD',FORM='UNFORMATTED')
    READ(30) NE,NT,NP,IANGLE
    use_undulator_binary_files = 1

    1414 continue


    allocate( CDFX (NP,NT,NE) )
    allocate( D_POL(NP,NT,NE) )
    allocate( UPHI (NP,NT,NE) )
    allocate( CDFZ   (NT,NE) )
    allocate( UTHETA (NT,NE) )
    allocate( CDFW   (NE) )
    allocate( UENER  (NE) )

    IF (use_undulator_binary_files .EQ. 1) THEN
        DO K = 1,NE
            READ (30) UENER(K)
        END DO

        DO K = 1,NE
            DO J = 1,NT
                READ (30)UTHETA(J,K)
            END DO
        END DO

        DO K = 1,NE
            DO J = 1,NT
                DO I = 1,NP
                    READ (30) UPHI(I,J,K)
                END DO
            END DO
        END DO

        DO K = 1,NE
            READ (30) CDFW(K)
        END DO

        DO K = 1,NE
            DO J = 1,NT
                READ (30) CDFZ(J,K)
            END DO
        END DO

        DO K = 1,NE
            DO J = 1,NT
                DO I = 1,NP
                    READ (30) CDFX(I,J,K)
                END DO
            END DO
        END DO

        DO K = 1,NE
            DO J = 1,NT
                DO I = 1,NP
                    READ (30) D_POL(I,J,K)
                END DO
            END DO
        END DO
    ELSE
        DO K = 1,NE
            READ (30,*) UENER(K)
        END DO

        DO K = 1,NE
            DO J = 1,NT
                READ (30,*)UTHETA(J,K)
            END DO
        END DO

        DO K = 1,NE
            DO J = 1,NT
                DO I = 1,NP
                    READ (30,*) UPHI(I,J,K)
                END DO
            END DO
        END DO

        DO K = 1,NE
            READ (30,*) CDFW(K)
        END DO

        DO K = 1,NE
            DO J = 1,NT
                READ (30,*) CDFZ(J,K)
            END DO
        END DO

        DO K = 1,NE
            DO J = 1,NT
                DO I = 1,NP
                    READ (30,*) CDFX(I,J,K)
                END DO
            END DO
        END DO

        DO K = 1,NE
            DO J = 1,NT
                DO I = 1,NP
                    READ (30,*) D_POL(I,J,K)
                END DO
            END DO
        END DO

    END IF
    
    CLOSE(30)
    ! C
    ! C These flags are set because of the original program structure.
    ! C
    F_PHOT= 0
    F_COLOR= 3
    ! C  FGRID= 0
    FSOUR= 3
    F_COHER= 1
ELSE    
    ! C
    ! C Bending magnet case
    ! C
    RAD_MIN= ABS(R_MAGNET)
    RAD_MAX= ABS(R_MAGNET)
END IF



! C
! C Prepares some SR variables; the vertical divergence replaces the emittance
! C
IF (FDISTR.EQ.4.OR.FDISTR.EQ.6) THEN
    F_COHER = 0
    IF (R_ALADDIN.LT.0.0D0) THEN
        POL_ANGLE = -90.0D0
    ELSE
        POL_ANGLE = 90.0D0
    END IF
END IF
POL_ANGLE     =   TORAD*POL_ANGLE
! C
! C Saved values of emittance that user inputs.  Write these out to 
! C namelist instead of EPSI/SIGMA.  6/25/93 clw.
! C
    
IF (FSOUR.EQ.3) THEN   ! warning.... This is done for BM and ID's also
    EPSI_XOLD = EPSI_X
    EPSI_ZOLD = EPSI_Z
    IF (SIGMAX.NE.0.0D0) THEN
        EPSI_X =   EPSI_X/SIGMAX
    ELSE
        EPSI_X =   0.0D0
    END IF
    IF (SIGMAZ.NE.0.0D0) THEN
        EPSI_Z =   EPSI_Z/SIGMAZ
    ELSE
        EPSI_Z =   0.0D0
    END IF
END IF
    
PHOTON(1) = PH1
PHOTON(2) = PH2
PHOTON(3) = PH3
PHOTON(4) = PH4
PHOTON(5) = PH5
PHOTON(6) = PH6
PHOTON(7) = PH7
PHOTON(8) = PH8
PHOTON(9) = PH9
PHOTON(10) = PH10
! C
! C sets up the acceptance/rejection method for optimizing source
! C notice that this is acceptable ONLY for random sources
! C
IF ( F_BOUND_SOUR.GT.0 .AND. FGRID.EQ.0 ) THEN
    ITMP=-1
    CALL SOURCE_BOUND (XDUM,YDUM,ITMP)
END IF
! C
! C tests for grids
! C
IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    SIGXL(1) = SIGXL1
    SIGXL(2) = SIGXL2
    SIGXL(3) = SIGXL3
    SIGXL(4) = SIGXL4
    SIGXL(5) = SIGXL5
    SIGXL(6) = SIGXL6
    SIGXL(7) = SIGXL7
    SIGXL(8) = SIGXL8
    SIGXL(9) = SIGXL9
    SIGXL(10) = SIGXL10
    ! C
    SIGZL(1) = SIGZL1
    SIGZL(2) = SIGZL2
    SIGZL(3) = SIGZL3
    SIGZL(4) = SIGZL4
    SIGZL(5) = SIGZL5
    SIGZL(6) = SIGZL6
    SIGZL(7) = SIGZL7
    SIGZL(8) = SIGZL8
    SIGZL(9) = SIGZL9
    SIGZL(10) = SIGZL10
    ! C
    ! C The next two assignments are just for convenience of the original program 
    ! C structure.
    ! C
    FSOUR = 4
    FDISTR = 7
END IF
    
IF (F_PHOT.EQ.1) THEN
    IF (F_COLOR.EQ.1) THEN
        PHOTON(1) =   TOANGS/PHOTON(1)
    ELSE IF (F_COLOR.EQ.2.OR.F_COLOR.EQ.4) THEN
         DO  21 I=1,N_COLOR
             PHOTON(I) =   TOANGS/PHOTON(I)
         21  CONTINUE
    ELSE IF (F_COLOR.EQ.3) THEN
         DO 31 I=1,2
             PHOTON(I) =   TOANGS/PHOTON(I)
         31 CONTINUE
    END IF
END IF

! C
! C If the S.R. case has been chosen, set up the subroutine for the
! C  vertical distribution.
! C
IF (FDISTR.EQ.6) THEN
    itmp = -1
    CALL ALADDIN1 (DUMMY,DUMMY,itmp,IER)
END IF
IF (FDISTR.EQ.4)  THEN 
    i0 = 0
    CALL WHITE (RAD_MIN,RAD_MAX,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,i0)
END IF


! ==============================  GRID SETUP ================================
!   TODO: remove this section, useless for Synchrotron sources. 
! C
! C Calculate the total number of rays.
! C
102 CONTINUE
    
IF (FDISTR.NE.5) THEN
    NMOM = IDO_VX * IDO_VZ
ELSE
    NMOM = (N_CONE * N_CIRCLE) 
    IDO_VX = N_CIRCLE
    IDO_VZ = N_CONE
END IF
NSPACE = IDO_X_S * IDO_Y_S * IDO_Z_S
    
IF (FGRID.EQ.0) THEN
    NTOTAL = NPOINT
ELSE IF (FGRID.EQ.1) THEN
    NTOTAL = NSPACE * NMOM
ELSE IF (FGRID.EQ.2) THEN
    NTOTAL = NSPACE * NPOINT
ELSE IF (FGRID.EQ.3) THEN
    NTOTAL = NPOINT * NMOM
ELSE IF (FGRID.EQ.4) THEN
    NTOTAL = IDO_XL * NPOINT * IDO_ZL * NPOINT
ELSE IF (FGRID.EQ.5) THEN
    NTOTAL = IDO_XL * IDO_XN * IDO_ZL * IDO_ZN
END IF
    
ITMP=0
IF (NTOTAL.LE.0) CALL LEAVE ('SOURCE','NPOINT = 0',ITMP)
!!      IF (NTOTAL.GT.N_DIM) CALL LEAVE ('SOURCE','Too many rays.',ITMP)
! C
! C Compute the steps and iteration count limits for the grid generation.
! C
IF (IDO_X_S.GT.1) STEP_X  = 1.0D0/(IDO_X_S - 1)
IF (IDO_Y_S.GT.1) STEP_Y  = 1.0D0/(IDO_Y_S - 1)
IF (IDO_Z_S.GT.1) STEP_Z  = 1.0D0/(IDO_Z_S - 1)
IF (IDO_VX.GT.1)  STEP_VX = 1.0D0/(IDO_VX - 1)
IF (IDO_VZ.GT.1)  STEP_VZ = 1.0D0/(IDO_VZ - 1)
IF (IDO_XN.GT.1)  STEP_XN = 1.0D0/(IDO_XN - 1)
IF (IDO_ZN.GT.1)  STEP_ZN = 1.0D0/(IDO_ZN - 1)
CL_X  = (IDO_X_S - 1) / 2.0D0
CL_Y  = (IDO_Y_S - 1) / 2.0D0
CL_Z  = (IDO_Z_S - 1) / 2.0D0
CL_VX = (IDO_VX - 1) / 2.0D0
CL_VZ = (IDO_VZ - 1) / 2.0D0
CL_XN = (IDO_XN - 1) / 2.0D0
CL_ZN = (IDO_ZN - 1) / 2.0D0
! C
! C First fill out a "typical" part of the GRID direction.
! C
INDEXMOM = 0 
IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
     DO 41 I = 1, NPOINT
         GRID (4,I) = WRAN (ISTAR1)
         GRID (6,I) = WRAN (ISTAR1)
     41 CONTINUE
     INDEXMOM = NPOINT
ELSE IF (FGRID.EQ.1.OR.FGRID.EQ.3) THEN
     !!srio DO 51 C_VX = -CL_VX, CL_VX
     DO 51 C_VX = -INT(CL_VX), INT(CL_VX)
         !!srio  DO 61 C_VZ = -CL_VZ, CL_VZ
         DO 61 C_VZ = -INT(CL_VZ), INT(CL_VZ)
             INDEXMOM = INDEXMOM + 1
             GRID (4,INDEXMOM) = C_VX * STEP_VX + 0.5D0
             GRID (6,INDEXMOM) = C_VZ * STEP_VZ + 0.5D0
         61 CONTINUE
     51 CONTINUE
     ! C IF (FDISTR.EQ.5) THEN
     ! C INDEXMOM = INDEXMOM + 1
     ! C GRID (4,INDEXMOM) = 0.0D0
     ! C GRID (6,INDEXMOM) = -1.0D0
     ! C END IF
ELSE IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
     DO 71 I = 1, IDO_XL
         IF (FGRID.EQ.4) THEN
             DO 81 J = 1, NPOINT
                 INDEXMOM= INDEXMOM + 1
                 GRID(1,INDEXMOM)= SIGXL(I)
                 GRID(2,INDEXMOM)= WRAN (ISTAR1)
                 GRID(4,INDEXMOM)= WRAN (ISTAR1)
             81 CONTINUE
         ELSE
             !!srio      DO 91 C_XN = -CL_XN, CL_XN
             DO 91 C_XN = -INT(CL_XN), INT(CL_XN)
                 INDEXMOM= INDEXMOM + 1
                 GRID(1,INDEXMOM)= SIGXL(I)
                 GRID(2,INDEXMOM)= WRAN (ISTAR1)
                 GRID(4,INDEXMOM)= C_XN * STEP_XN + 0.5D0
             91 CONTINUE
         END IF
     71   CONTINUE
END IF
! C
! C Now fill out the entire GRID.
! C
INDEXSPA = 0
IF (FGRID.EQ.0) THEN
    DO 103 I = 1, NPOINT
        GRID (1,I) = WRAN (ISTAR1)
        GRID (2,I) = WRAN (ISTAR1)
        GRID (3,I) = WRAN (ISTAR1)
    103 CONTINUE
    INDEXSPA = NPOINT
ELSE IF (FGRID.EQ.3) THEN
    DO 113 I = 1, NPOINT
        TEMPX = WRAN (ISTAR1)
        TEMPY = WRAN (ISTAR1)
        TEMPZ = WRAN (ISTAR1)
        DO 121 J = 1, INDEXMOM
            INDEXSPA = INDEXSPA + 1
            GRID(1,INDEXSPA) = TEMPX
            GRID(2,INDEXSPA) = TEMPY
            GRID(3,INDEXSPA) = TEMPZ
            GRID(4,INDEXSPA) = GRID (4,J)
            GRID(6,INDEXSPA) = GRID (6,J)
        121 CONTINUE
    113   CONTINUE
ELSE IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
    DO 131 C_X = -INT(CL_X), INT(CL_X)
        DO 141 C_Y = -INT(CL_Y), INT(CL_Y)
            DO 151 C_Z = -INT(CL_Z), INT(CL_Z)
                DO 161 J = 1, INDEXMOM
                    INDEXSPA = INDEXSPA + 1
                    GRID (1,INDEXSPA) = C_X * STEP_X + 0.5D0
                    GRID (2,INDEXSPA) = C_Y * STEP_Y + 0.5D0
                    GRID (3,INDEXSPA) = C_Z * STEP_Z + 0.5D0
                    GRID (4,INDEXSPA) = GRID (4,J)
                    GRID (6,INDEXSPA) = GRID (6,J)
                161 CONTINUE
            151 CONTINUE
        141 CONTINUE
    131   CONTINUE
ELSE IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    DO 171 I = 1, IDO_ZL
        IF (FGRID.EQ.4) THEN
            DO 181 J = 1, NPOINT
                TEMP = WRAN (ISTAR1)
                DO 191 K = 1, IDO_XL*NPOINT
                    INDEXSPA  = INDEXSPA + 1
                    GRID(1,INDEXSPA) = GRID(1,K)
                    GRID(2,INDEXSPA) = GRID(2,K)
                    GRID(4,INDEXSPA) = GRID(4,K)
                    GRID(3,INDEXSPA) = SIGZL(I)
                    GRID(6,INDEXSPA) = TEMP
                191 CONTINUE
            181 CONTINUE
        ELSE
            !!srio       DO 201 C_ZN = -CL_ZN, CL_ZN
            DO 201 C_ZN = -INT(CL_ZN), INT(CL_ZN)
                TEMP = C_ZN * STEP_ZN + 0.5D0
                DO 211 K = 1, IDO_XL*IDO_XN
                    INDEXSPA  = INDEXSPA + 1
                    GRID(1,INDEXSPA) = GRID(1,K)
                    GRID(2,INDEXSPA) = GRID(2,K)
                    GRID(4,INDEXSPA) = GRID(4,K)
                    GRID(3,INDEXSPA) = SIGZL(I)
                    GRID(6,INDEXSPA) = TEMP
                211         CONTINUE
            201       CONTINUE
        END IF
    171   CONTINUE
END IF

!
! ===============================   END GRID SETUP ===============================
!

KK = 0
MM = 0

DO 10000 ITIK=1,NTOTAL  !start mega-loop on number of rays
    ! C
    ! C---------------------------------------------------------------------
    ! C           POSITIONS
    ! C
    ! C
    KK = KK + 1
    !IF (KK.EQ.250) THEN
    IF (KK.EQ.NTOTAL/20) THEN
        ITOTRAY = KK + MM*(NTOTAL/20)
        IF (MM.EQ.0) THEN
            WRITE(6,*)'Generated ',ITOTRAY,' rays out of ',NTOTAL
        ELSE
            WRITE(6,*)'          ',ITOTRAY
        END IF
        KK = 0
        MM = MM + 1
    END IF
    ! C
    ! C The following entry point is for the "optimized" source
    ! C
    ! C
    10001	CONTINUE
    IF ((F_WIGGLER.EQ.1).OR.(F_WIGGLER.EQ.3)) THEN !-------! wiggler
        ! C
        ! C Normal wiggler case
        ! C
        IF (F_WIGGLER.EQ.1) THEN
            ARG_Y = GRID(2,ITIK)
            CALL SPL_INT (SEED_Y, NP_SY,   ARG_Y,  Y_TRAJ,    IER)
            ! srio@esrf.eu 2014-05-19
            ! in wiggler some problems arise because spl_int
            ! does not return a Y value in the correct range. 
            ! In those cases, we make a linear interpolation instead. 
            if ((y_traj.le.y_temp(1)).or.(y_traj.gt.y_temp(NP_SY))) then 
                y_traj_old = y_traj
                CALL LIN_INT (SEED_Y, NP_SY,   ARG_Y,  Y_TRAJ,    IER)
                print*,'SOURCESYNC: bad y_traj from SPL_INT, corrected with LIN_SPL: ',y_traj_old,'=>',y_traj
            endif

            CALL SPL_INT (Y_X,    NP_TRAJ, Y_TRAJ, X_TRAJ,    IER)
            CALL SPL_INT (Y_XPRI, NP_TRAJ, Y_TRAJ, ANGLE,     IER)
            CALL SPL_INT (Y_CURV, NP_TRAJ, Y_TRAJ, CURV,      IER)
            CALL SPL_INT (Y_PATH, NP_TRAJ, Y_TRAJ, EPSI_PATH, IER)
        END IF
        ! C
        ! C Elliptical wiggler case
        ! C
        IF (F_WIGGLER.EQ.3) THEN
            ARG_Y = GRID(2,ITIK)
            CALL SPL_INT (SEED_Y, NP_SY,   ARG_Y,  Y_TRAJ,    IER)
            CALL SPL_INT (Y_X,    NP_TRAJ, Y_TRAJ, X_TRAJ,    IER)
            CALL SPL_INT (Y_Z,    NP_TRAJ, Y_TRAJ, Z_TRAJ,    IER)
            CALL SPL_INT (Y_XPRI, NP_TRAJ, Y_TRAJ, ANGLE1,    IER)
            CALL SPL_INT (Y_ZPRI, NP_TRAJ, Y_TRAJ, ANGLE2,    IER)
            CALL SPL_INT (Y_CURV, NP_TRAJ, Y_TRAJ, CURV,      IER)
            CALL SPL_INT (Y_PATH, NP_TRAJ, Y_TRAJ, EPSI_PATH, IER)
        END IF
        EPSI_PATH = EPSI_PATH - PATH0 ! now refer to wiggler's origin
        IF (CURV.LT.0) THEN
            POL_ANGLE = 90.0D0  ! instant orbit is CW
        ELSE
            POL_ANGLE = -90.0D0  !     CCW
        END IF
        IF (CURV.EQ.0) THEN
            R_MAGNET = 1.0D+20
        ELSE
            R_MAGNET = ABS(1.0D0/CURV)
        END IF
        POL_ANGLE  = TORAD*POL_ANGLE
    ! C above statement added 24 march 1992 to change POL_ANGLE to radians. clw.
    ! C
    ELSE IF (FSOURCE_DEPTH.EQ.4) THEN  !-------! Bending magnet
        ! Synchrontron depth
        ANGLE  =  GRID(2,ITIK) * (HDIV1 + HDIV2) - HDIV2
        EPSI_PATH =  ABS(R_ALADDIN)*ANGLE
    ELSE IF (F_WIGGLER.EQ.2) THEN !-------! Undulator
        ! C
        ! C Undulator case : first interpolate for the photon energy.
        ! C

        ESEED  = GRID(2,ITIK)* CDFW(NE)
        DO 221 K = 1, NE-1
            IF (ESEED.LE.CDFW(K+1)) GO TO 510
        221   CONTINUE
        510 DK  = (ESEED - CDFW(K))/(CDFW(K+1) - CDFW(K))
        KI = K
        PENERGY = UENER(K) + DK*(UENER(K+1) - UENER(K))
        Q_WAVE = TWOPI*PENERGY/TOCM
        ! C
        ! C then interpolate for theta (Z').
        ! C
        ZSEED  = GRID(6,ITIK) 
        
        INDEX = 1
        DO 231 K = KI, KI+1
            CZMAX = ZSEED*CDFZ(NT,K)
            DO 241 J = 1, NT-1
                IF (CZMAX.LE.CDFZ(J+1,K)) THEN
                    JI(INDEX) = J
                    DZ(INDEX) = (CZMAX - CDFZ(J,K))/(CDFZ(J+1,K) - CDFZ(J,K))
                    THE_INT(INDEX) = UTHETA(J,K) + DZ(INDEX)* (UTHETA(J+1,K) - UTHETA(J,K))
                    GO TO 520
                END IF 
            241 CONTINUE
            520 INDEX = INDEX + 1
        231 CONTINUE
        
        !   THETA = THE_INT(1) + DK*(THE_INT(2)-THE_INT(1))
        !   warning use this instead : 
        THETABM=THE_INT(1) + DK*(THE_INT(2)-THE_INT(1))
        
        ! C
        ! C Finally interpolate for phi (X').
        ! C
        XSEED  = GRID(4,ITIK) 
        
        INDEX = 1
        DO 251 K = KI, KI+1
            JNOW = JI(K-KI+1)
            DO 261 J = JNOW, JNOW + 1
                CXMAX = XSEED * CDFX(NP,J,K)
                    DO 271 I = 1, NP-1
                        IF (CXMAX.LE.CDFX(I+1,J,K)) THEN
                            II(INDEX) = I
                            DX(INDEX) = (CXMAX - CDFX(I,J,K)) /(CDFX(I+1,J,K) - CDFX(I,J,K))
                            PHI_INT(INDEX) = UPHI(I,J,K) + DX(INDEX)* (UPHI(I+1,J,K) - UPHI(I,J,K))
                            GO TO 530
                        END IF 
                    271 CONTINUE
                    530 INDEX = INDEX + 1
            261 CONTINUE
        251 CONTINUE
        
        PHI1 = PHI_INT(1) + DZ(1)*(PHI_INT(2) - PHI_INT(1))
        PHI2 = PHI_INT(3) + DZ(2)*(PHI_INT(4) - PHI_INT(3))
        PHI  = PHI1 + DK*(PHI2 - PHI1)
        
        ! C
        ! C Also the degree of polarization.
        ! C
        
        ! C ++++
        ! C
        ! C BEGIN BUG BUG BUG BUG (Tue Apr  8 21:25:51 CDT 1997)
        ! C
        ! C DELTAI, DELTAJ and DELTAK are used uninitialized here, and I have no 
        ! C idea what these are supposed represent. I'm setting it to zero for 
        ! C now, which is what most compilers do (not F77 standard however, and
        ! C on some systems, you understandably get garbage). Also, fixed THETA3
        ! C calculation (was THETA4 -- typo). -- MK
        ! C
        DELTAI = 0.0D0
        DELTAJ = 0.0D0
        DELTAK = 0.0D0
        ! C
        ! C END BUG  BUG
        ! C
        ! C ---
        THETA1 = D_POL(I,J,K) + (D_POL(I,J,K+1)     - D_POL(I,J,K))*  DELTAK
        THETA2 = D_POL(I,J+1,K) + (D_POL(I,J+1,K+1)   - D_POL(I,J+1,K))*  DELTAK
        THETA3 = D_POL(I+1,J,K) + (D_POL(I+1,J,K+1)   - D_POL(I+1,J,K))*  DELTAK
        THETA4 = D_POL(I+1,J+1,K) + (D_POL(I+1,J+1,K+1) - D_POL(I+1,J+1,K))*  DELTAK
        PHI1  = THETA1 + (THETA2-THETA1)*DELTAJ 
        PHI2  = THETA3 + (THETA4-THETA3)*DELTAJ 
        POL_DEG = PHI1 + (PHI2-PHI1)*DELTAI
        ! C
        POL_ANGLE = 90.0D0
        EPSI_PATH = 0.0D0
        I_CHANGE = 1
        POL_ANGLE  = TORAD*POL_ANGLE
        ! C above statement added 24 march 1992 to change POL_ANGLE to radians. clw.
        ! C
        ! C If the cdf's are in polar coordinates switch them to cartesian angles.
        ! C
        IF (IANGLE.EQ.1) THEN
            !!      A_Z = ASIN(SIN(THETA)*SIN(PHI))
            !!      A_X = ACOS(COS(THETA)/COS(A_Z))
            !!      THETA = A_Z
            !!   warning use this instead : 
            A_Z = ASIN(SIN(THETABM)*SIN(PHI))
            A_X = ACOS(COS(THETABM)/COS(A_Z))
            THETABM = A_Z
            
            PHI  = A_X
        END IF
        ! C
        ! C Decide in which quadrant THETA and PHI are.
        ! C
        IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
            !!      IF (WRAN(ISTAR1).LT.0.5) PHI = -PHI
            !!      IF (WRAN(ISTAR1).LT.0.5) THETA = -THETA
            
            !!   warning use this instead : 
            IF (WRAN(ISTAR1).LT.0.5) THETABM = -THETABM
            IF (WRAN(ISTAR1).LT.0.5) PHI = -PHI
            
        END IF
    END IF  !-------! Undulator ends.

    !!
    
    ! fsour  =  0 - spatial source type/shape in X-Z plane.  Options are:
    !               point (0), rectangle (1), ellipse (2), gaussian (3).

    GO TO (1,2,3,4,5,5,7), FSOUR+1
    
    !
    !    POSITIONS: start sampling position coordinates (XXX,YYY,ZZZ)
    1	CONTINUE
    ! C
    ! C Point source **
    ! C
    xxx = 0.0d0
    zzz = 0.0d0
    GO TO 111
    
    2	CONTINUE
    ! C
    ! C Rectangular source 
    ! C
    XXX = (-1.0D0 + 2.0D0*GRID(1,ITIK))*WXSOU/2
    ZZZ = (-1.0D0 + 2.0D0*GRID(3,ITIK))*WZSOU/2
    GO TO 111
    
    3	CONTINUE
    ! C
    ! C Elliptical source **
    ! C Uses a transformation algorithm to generate a uniform variate distribution
    ! C
    IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
      PHI = TWOPI*GRID(1,ITIK)*(IDO_X_S-1)/IDO_X_S
    ELSE
      PHI = TWOPI*GRID(1,ITIK)
    END IF
    RADIUS = SQRT(GRID(3,ITIK))
    XXX = WXSOU*RADIUS*COS(PHI)
    ZZZ = WZSOU*RADIUS*SIN(PHI)
    GO TO 111
    
    4	CONTINUE
    ! C
    ! C Gaussian -- In order to accomodate the generation nof finite emittance
    ! C beams, we had to remove the 'grid' case.
    ! C Includes IDs
    ! C 
    ARG_X = GRID(1,ITIK)
    ARG_Z = GRID(3,ITIK)
    ! C
    ! C Compute the actual distance (EPSI_W*) from the orbital focus
    ! C
    EPSI_WX = EPSI_DX + EPSI_PATH
    EPSI_WZ = EPSI_DZ + EPSI_PATH

    ! BUG srio@esrf.eu found that these routine does not make the 
    ! calculation correctly. Changed to new one BINORMAL
    !CALL GAUSS (SIGMAX, EPSI_X, EPSI_WX, XXX, E_BEAM(1), istar1)
    !CALL GAUSS (SIGMAZ, EPSI_Z, EPSI_WZ, ZZZ, E_BEAM(3), istar1)
    !
    ! calculation of the electrom beam moments at the current position 
    ! (sX,sZ) = (epsi_wx,epsi_ez): 
    ! <x2> = sX^2 + sigmaX^2
    ! <x x'> = sX sigmaXp^2
    ! <x'2> = sigmaXp^2                 (same for Z)
 
    ! then calculate the new recalculated sigmas (rSigmas) and correlation rho of the 
    ! normal bivariate distribution at the point in the electron trajectory
    ! rsigmaX  = sqrt(<x2>)
    ! rsigmaXp = sqrt(<x'2>)
    ! rhoX =  <x x'>/ (rsigmaX rsigmaXp)      (same for Z)
        
    if (abs(sigmaX) .lt. 1e-15) then  !no emittance
        sigmaXp = 0.0d0
        XXX = 0.0
        E_BEAM(1) = 0.0
    else
        sigmaXp = epsi_Xold/sigmaX    ! true only at waist, use epsi_xOld as it has been redefined :(
        rSigmaX = sqrt( (epsi_wX**2) * (sigmaXp**2) + sigmaX**2 ) 
        rSigmaXp = sigmaXp
        if (abs(rSigmaX*rSigmaXp) .lt. 1e-15) then  !no emittance
            rhoX = 0.0
        else
            rhoX = epsi_wx * sigmaXp**2 / (rSigmaX * rSigmaXp) 
        endif
        
        CALL BINORMAL (rSigmaX, rSigmaXp, rhoX, XXX, E_BEAM(1), istar1)
    endif

    if (abs(sigmaZ) .lt. 1e-15) then  !no emittance
        sigmaZp = 0.0d0
        ZZZ = 0.0
        E_BEAM(3) = 0.0
    else
        sigmaZp = epsi_Zold/sigmaZ
        rSigmaZ = sqrt( (epsi_wZ**2) * (sigmaZp**2) + sigmaZ**2 ) 
        rSigmaZp = sigmaZp
        if (abs(rSigmaZ*rSigmaZp) .lt. 1e-15) then  !no emittance
            rhoZ = 0.0
        else
            rhoZ = epsi_wZ * SigmaZp**2 /(rSigmaZ*rSigmaZp)
        end if
        CALL BINORMAL (rSigmaZ, rSigmaZp, rhoZ, ZZZ, E_BEAM(3), istar1)
    endif

    ! C
    ! C For normal wiggler, XXX is perpendicular to the electron trajectory at 
    ! C the point defined by (X_TRAJ,Y_TRAJ,0).
    ! C
    IF (F_WIGGLER.EQ.1) THEN   ! normal wiggler
        YYY = Y_TRAJ - XXX*SIN(ANGLE)
        XXX = X_TRAJ + XXX*COS(ANGLE)
        GO TO 550
    ELSE IF (F_WIGGLER.EQ.2) THEN ! undulator
        ANGLEX = E_BEAM(1) + PHI
        !!  ANGLEV = E_BEAM(3) + THETA
        !! warning: use this inestead
        ANGLEV = E_BEAM(3) + THETABM
        DIREC(1) = TAN(ANGLEX)
        DIREC(2) = 1.0D0
        DIREC(3) = TAN(ANGLEV)/COS(ANGLEX)
        CALL NORM (DIREC,DIREC)
        GO TO 1111 
    ELSE IF (F_WIGGLER.EQ.3) THEN  ! eliptical wiggler
        VTEMP(1) = XXX
        VTEMP(2) = 0.0D0
        VTEMP(3) = ZZZ
        ANGLE1= -ANGLE1
        ANGLE3= 0.0D0
        CALL ROTATE(VTEMP,ANGLE3,ANGLE2,ANGLE1,VTEMP)
        XXX=X_TRAJ + VTEMP(1)
        YYY=Y_TRAJ + VTEMP(2)
        ZZZ=Z_TRAJ + VTEMP(3)
        !added srio@esrf.eu 20131105
        go to 550
    END IF
    GO TO 111
    
    5 	CONTINUE
    ! C
    ! C Ellipses in phase space (spatial components).
    ! C
    IF (FGRID.EQ.4) THEN
      PHI_X = TWOPI * GRID(4,ITIK)
      PHI_Z = TWOPI * GRID(6,ITIK)
    ELSE
      PHI_X = TWOPI * GRID(4,ITIK) * (IDO_XN-1) / IDO_XN
      PHI_Z = TWOPI * GRID(6,ITIK) * (IDO_ZN-1) / IDO_ZN
    END IF
    XXX = GRID(1,ITIK)*SIGMAX*COS(PHI_X)
    ZZZ = GRID(3,ITIK)*SIGMAZ*COS(PHI_Z)
    GO TO 111
    
    7	CONTINUE
    
    GOTO 550
    
    111	CONTINUE
    ! C
    ! C---------------------------------------------------------------------
    ! C                      DEPTH
    ! C
    ! C
    GO TO (110,220,330,440)  FSOURCE_DEPTH
    ! C
    ! C No depth case.
    ! C
    YYY = 0.0d0
    110 GO TO 550
    ! C
    ! C Uniform depth distribution
    ! C
    220 YYY = (-1.0D0 + 2.0D0*GRID(2,ITIK))*WYSOU/2
    GO TO 550
    ! C
    ! C Gaussian depth distribution 
    ! C
    330 ARG_Y  = GRID(2,ITIK)
    
    CALL MDNRIS (ARG_Y,YYY,IER)
    IF (IER.NE.0) WRITE(6,*)'Warning ! Error in YYY,MNDRIS (SOURCE)'
    
    YYY  = YYY*SIGMAY
    
    GO TO 550
    ! C
    ! C Synchrotron depth distribution
    ! C
    440	CONTINUE
    ! CC	R_ALADDIN NEGATIVE FOR COUNTER-CLOCKWISE SOURCE	
    IF (R_ALADDIN.LT.0) THEN
        YYY = (ABS(R_ALADDIN) + XXX) * SIN(ANGLE)
    ELSE
        YYY = ( R_ALADDIN - XXX) * SIN(ANGLE)
    END IF
    XXX  =   COS(ANGLE) * XXX + R_ALADDIN * (1.0D0 - COS(ANGLE))
    


    550	CONTINUE

    ! C
    ! C---------------------------------------------------------------------
    ! C             DIRECTIONS
    ! C
    ! C   Generates now the direction of the rays.
    ! C   Stire them un DIREC(1-3)
    ! C
    ! C
    101 CONTINUE
    I_CHANGE = 1
    GO TO (11,11,33,44,55,44,77), FDISTR
    
    11 CONTINUE
    ! C
    ! C   Uniform distribution ( Isotrope emitter ) and cosine  source
    ! C
    ! C   Distinction not ready yet. Not important for small apertures 
    ! C
    XMAX1 =   TAN(HDIV1)
    XMAX2 = - TAN(HDIV2)
    ZMAX1 =   TAN(VDIV1)
    ZMAX2 = - TAN(VDIV2)
    XRAND  = (GRID(4,ITIK)*(XMAX1 - XMAX2) + XMAX2)
    ZRAND  = (GRID(6,ITIK)*(ZMAX1 - ZMAX2) + ZMAX2)
    THETAR = ATAN(SQRT(XRAND**2+ZRAND**2))
    CALL ATAN_2 (ZRAND,XRAND,PHIR)
    DIREC(1) = COS(PHIR)*SIN(THETAR)
    DIREC(2) = COS(THETAR)
    DIREC(3) = SIN(PHIR)*SIN(THETAR)
    ! C     	ARG	=   GRID(6,ITIK)*(SIN(VDIV1) + SIN(VDIV2)) - SIN(VDIV2)
    ! C     	PHIR	=   GRID(4,ITIK)*(HDIV1 + HDIV2) - HDIV1
    ! C     	THETAR  =   ASIN(ARG)
    ! C     	DIREC(1)	=   SIN(PHIR)*COS(THETAR)
    ! C     	DIREC(2)	=   COS(PHIR)*COS(THETAR)
    ! C     	DIREC(3)	=   SIN(THETAR)
    GO TO 1111
    
    33  CONTINUE
    ! C
    ! C Gaussian emitter 
    ! C Note : an emitter cannot have an angular gaussian distribution, as a
    ! C gaussian is defined from -infin. to + infin. It might be an useful
    ! C approximation in several cases. This program uses a gaussian 
    ! C distribution onto an ideal image plane, independent in x and z. This 
    ! C approximation will not break down for large sigma.
    ! C
    ARG_VX = GRID(4,ITIK)
    ARG_VZ = GRID(6,ITIK)
    
    CALL MDNRIS (ARG_VX,DIR_X,IER)
    IF (IER.NE.0) WRITE(6,*) 'Warning !Error in DIR_X:MNDRIS(SOURCE)'
    
    DIREC(1) = DIR_X*SIGDIX
    
    CALL MDNRIS (ARG_VZ,DIR_Z,IER)
    IF (IER.NE.0) WRITE(6,*)'Warning !Error in DIR_Z:MNDRIS(SOURCE)'
    
    DIREC(3) = DIR_Z*SIGDIZ
    DIREC(2) = 1.0D0
    
    CALL NORM (DIREC,DIREC)
    
    GO TO 1111
    
    44 CONTINUE
    ! C
    ! C Synchrotron source 
    ! C Note. The angle of emission IN PLANE is the same as the one used
    ! C before. This will give rise to a source curved along the orbit.
    ! C The elevation angle is instead characteristic of the SR distribution.
    ! C The electron beam emittance is included at this stage. Note that if
    ! C EPSI = 0, we'll have E_BEAM = 0.0, with no changes.
    ! C
    IF (F_WIGGLER.EQ.3) ANGLE=0        ! Elliptical Wiggler.
    ANGLEX =   ANGLE + E_BEAM(1)
    DIREC(1)  =   TAN(ANGLEX)
    IF (R_ALADDIN.LT.0.0D0) DIREC(1) = - DIREC(1)
    DIREC(2)  =   1.0D0
    ARG_ANG  =   GRID(6,ITIK)
    ! C
    ! C In the case of SR, we take into account the fact that the electron
    ! C trajectory is not orthogonal to the field. This will give a correction
    ! C to the photon energy.  We can write it as a correction to the 
    ! C magnetic field strength; this will linearly shift the critical energy
    ! C and, with it, the energy of the emitted photon.
    ! C
    E_TEMP(3) =   TAN(E_BEAM(3))/COS(E_BEAM(1))
    E_TEMP(2) =   1.0D0
    E_TEMP(1) =   TAN(E_BEAM(1))
    CALL NORM (E_TEMP,E_TEMP)
    CORREC =   SQRT(1.0D0-E_TEMP(3)**2)
    4400 CONTINUE
    IF (FDISTR.EQ.6) THEN
        CALL ALADDIN1 (ARG_ANG,ANGLEV,F_POL,IER)
        Q_WAVE =   TWOPI*PHOTON(1)/TOCM*CORREC
        POL_DEG =   ARG_ANG
    ELSE IF (FDISTR.EQ.4) THEN
        ARG_ENER =   WRAN (ISTAR1)
        RAD_MIN =   ABS(R_MAGNET)

        i1 = 1
        CALL WHITE  &
        (RAD_MIN,CORREC,ARG_ENER,ARG_ANG,Q_WAVE,ANGLEV,POL_DEG,i1)
    END IF
    IF (ANGLEV.LT.0.0) I_CHANGE = -1
    ANGLEV =   ANGLEV + E_BEAM(3)
    ! C
    ! C Test if the ray is within the specified limits
    ! C
    IF (FGRID.EQ.0.OR.FGRID.EQ.2) THEN
        IF (ANGLEV.GT.VDIV1.OR.ANGLEV.LT.-VDIV2) THEN
            ARG_ANG = WRAN(ISTAR1)
            ! C
            ! C If it is outside the range, then generate another ray.
            ! C
            GO TO 4400
        END IF
    END IF
    DIREC(3)  =   TAN(ANGLEV)/COS(ANGLEX)
    IF (F_WIGGLER.EQ.3) THEN
        CALL ROTATE (DIREC, ANGLE3,ANGLE2,ANGLE1,DIREC)
    END IF
    CALL NORM (DIREC,DIREC)
    GO TO 1111
    55 CONTINUE
    ! C   Now generates a set of rays along a cone centered about the normal,
    ! C   plus a ray along the normal itself.
    ! C      
    IF (FGRID.EQ.1.OR.FGRID.EQ.3) THEN
        ANGLE =   TWOPI*GRID(4,ITIK)*(IDO_VX-1)/IDO_VX
    ELSE
        ANGLE =   TWOPI*GRID(4,ITIK)
    END IF
    ! C temp fix -- 16 Jan 1987
    ! C      ANG_CONE =   CONE_MIN + 
    ! C     $ (CONE_MAX - CONE_MIN)*GRID(6,ITIK)
    ANG_CONE =   COS(CONE_MIN) - GRID(6,ITIK)*(COS(CONE_MIN)-COS(CONE_MAX))
    ANG_CONE =  ACOS(ANG_CONE)
    DIREC(1) =   SIN(ANG_CONE)*COS(ANGLE)
    DIREC(2) =   COS(ANG_CONE)
    DIREC(3) =   SIN(ANG_CONE)*SIN(ANGLE)
    ! C
    GO TO 1111
    
    77 CONTINUE
    ! C
    ! C Ellipses in phase space (momentum components).
    ! C
    ANGLEX = GRID(1,ITIK)*SIGDIX*SIN(PHI_X)
    ANGLEV = GRID(3,ITIK)*SIGDIZ*SIN(PHI_Z)
    DIREC(1) = SIN(ANGLEX)
    DIREC(3) = SIN(ANGLEV)
    DIREC(2) = SQRT(1.0D0 - DIREC(1)**2 - DIREC(3)**2)
    GO TO 1111
    


    1111  CONTINUE

    ! C
    ! C  ---------------------------------------------------------------------
    ! C                 POLARIZATION
    ! C
    ! C   Generates the polarization of the ray. This is defined on the
    ! C   source plane, so that A_VEC is along the X-axis and AP_VEC is along Z-axis.
    ! C   Then care must be taken so that A will be perpendicular to the ray 
    ! C   direction.
    ! C
    ! C   In the case of SR, the value of POL_DEG is set by the call to
    ! C   the module ALADDIN, so that it is possible to take into account the
    ! C   angular dependence of the source polarization.
    ! C
    A_VEC(1) = 1.0D0
    A_VEC(2) = 0.0D0
    A_VEC(3) = 0.0D0
    ! C
    ! C   Rotate A_VEC so that it will be perpendicular to DIREC and with the
    ! C   right components on the plane.
    ! C 
    CALL CROSS (A_VEC,DIREC,A_TEMP)
    CALL CROSS (DIREC,A_TEMP,A_VEC)
    CALL NORM (A_VEC,A_VEC)
    CALL CROSS (A_VEC,DIREC,AP_VEC)
    CALL NORM (AP_VEC,AP_VEC)
    
    IF (F_POLAR.EQ.1) THEN
        ! C
        ! C   WaNT A**2 = AX**2 + AZ**2 = 1 , instead of A_VEC**2 = 1 .
        ! C
        DENOM = SQRT(1.0D0 - 2.0D0*POL_DEG + 2.0D0*POL_DEG**2)
        AX = POL_DEG/DENOM
        CALL SCALAR (A_VEC,AX,A_VEC)
        ! C
        ! C   Same procedure for AP_VEC
        ! C
        AZ = (1-POL_DEG)/DENOM
        CALL SCALAR  (AP_VEC,AZ,AP_VEC)
    ELSE
        !srio ! C
        !srio ! C If don't want the full polarization, then POL_DEG is only defined in the 
        !srio ! C case of synchrotron radiation.
        !srio ! C
        IF (FDISTR.EQ.4.OR.FDISTR.EQ.6.OR.F_WIGGLER.NE.0) THEN
            IF (WRAN(ISTAR1).GT.POL_DEG) THEN
                ! C
                ! C A_VEC is along x or z -axis according to POL_DEG.
                ! C
                A_VEC(1) = AP_VEC(1)
                A_VEC(2) = AP_VEC(2)
                A_VEC(3) = AP_VEC(3)
            END IF
        END IF
    END IF
    ! C
    ! C Now the phases of A_VEC and AP_VEC.
    ! C
    IF (F_COHER.EQ.1) THEN
        PHASEX = 0.0D0
    ELSE
        PHASEX = WRAN(ISTAR1) * TWOPI
    END IF
    PHASEZ = PHASEX + POL_ANGLE*I_CHANGE
    ! C
    ! C---------------------------------------------------------------------
    ! C            PHOTON   ENERGY
    ! C
    ! C Generates the wavevector of the ray. Depending on the choice, a
    ! C single or a set of Q is created.NOTE: units are cm -1
    ! C
    ! C
    ! C
    ! C In the case of SR, Q_WAVE is already known
    ! C
    IF (FDISTR.EQ.4.OR.FDISTR.EQ.6.OR.F_WIGGLER.NE.0) GO TO 2050
    GO TO (2020,2030,2040,2045) F_COLOR
    
    2010 CONTINUE
    ! C
    ! C Not interested in the photon energy. Set at 0.0
    ! C
    GO TO 2050
    
    2020 CONTINUE
    ! C
    ! CSingle line. 
    ! C
    Q_WAVE =   TWOPI*PHOTON(1)/TOCM
    GO TO 2050
    
    2030 CONTINUE
    ! C
    ! C Several photon energies (up to 10) with same relative intensities.
    ! C
    N_TEST =   WRAN (ISTAR1)*N_COLOR + 1
    Q_WAVE =   TWOPI*PHOTON(N_TEST)/TOCM
    GO TO 2050
    
    2040 CONTINUE
    ! C
    ! C Box photon distribution
    ! C
    PHOT_CH =   PHOTON(1) + (PHOTON(2) - PHOTON(1))*WRAN(ISTAR1)
    Q_WAVE =   TWOPI*PHOT_CH/TOCM
    GO TO 2050
    
    2045 CONTINUE
    ! C
    ! C Several photon energies (up to 10) with different relative intensities.
    ! C
    RELINT(1) = RL1
    RELINT(2) = RL2
    RELINT(3) = RL3
    RELINT(4) = RL4
    RELINT(5) = RL5
    RELINT(6) = RL6
    RELINT(7) = RL7
    RELINT(8) = RL8
    RELINT(9) = RL9
    RELINT(10) = RL10
    
    ! C
    ! C Normalize so that each energy has a probability and so that the sum 
    ! C of the probabilities of all the energies is 1.
    ! C
    
    TMP_A = 0
    DO 2046 J=1,N_COLOR
        TMP_A = TMP_A + RELINT(J)  
    2046 CONTINUE 
    DO 2047 J=1,N_COLOR
        RELINT(J)=RELINT(J)/TMP_A
    2047 CONTINUE
    
    ! C
    ! C Arrange the probabilities so that they comprise the (0,1) interval,
    ! C e.g. (energy1,0.3), (energy2, 0.1), (energy3, 0.6) is translated to
    ! C 0.0, 0.3, 0.4, 1.0. Then a random number falling in an interval
    ! C assigned to a certain energy results in the ray being assigned that
    ! C photon energy.
    ! C
    TMP_B = 0
    DO 2048 J=1,N_COLOR
        TMP_B = TMP_B + RELINT(J) 
        PRELINT(J) = TMP_B
    2048 CONTINUE
    
    DPS_RAN3 = WRAN(ISTAR1)
    IF (DPS_RAN3.GE.0.AND.DPS_RAN3.LE.PRELINT(1)) THEN
        Q_WAVE = TWOPI*PHOTON(1)/TOCM
    END IF
    
    DO 2049 J=2,N_COLOR
        IF (DPS_RAN3.GT.PRELINT(J-1).AND.DPS_RAN3.LE.PRELINT(J)) THEN
            Q_WAVE = TWOPI*PHOTON(J)/TOCM
        END IF
    2049 CONTINUE
   
    GO TO 2050
    
    ! C
    ! C Create the final array 
    ! C
    2050 CONTINUE

    ray (1,ITIK)        =   XXX
    ray (2,ITIK)        =   YYY
    ray (3,ITIK)        =   ZZZ
    ray (4,ITIK)        =    DIREC(1)
    ray (5,ITIK)        =    DIREC(2)
    ray (6,ITIK)        =    DIREC(3)
    ray (7,ITIK)        =   A_VEC(1)
    ray (8,ITIK)        =   A_VEC(2)
    ray (9,ITIK)        =   A_VEC(3)
    ray (10,ITIK)       =   1.0D0
    ray (11,ITIK)       =   Q_WAVE
    ray (12,ITIK)       =   dble(ITIK)
    IF (F_POLAR.EQ.1) THEN
        ray (13,ITIK)   =   0.0D0
        ray (14,ITIK)   =   PHASEX
        ray (15,ITIK)   =   PHASEZ
        ray (16,ITIK)   =   AP_VEC(1)
        ray (17,ITIK)   =   AP_VEC(2)
        ray (18,ITIK)   =   AP_VEC(3)
    END IF



    ! C
    ! C All rays are generated. Test for acceptance if optimized source is
    ! C specified.
    ! C
    IF (F_BOUND_SOUR.GT.0 .AND. FGRID.EQ.0 ) THEN
       SB_POS(1) = XXX
       SB_POS(2) = YYY 
       SB_POS(3) = ZZZ
       ITEST = 1
       CALL SOURCE_BOUND (SB_POS, DIREC, ITEST)
       IF (ITEST.LT.0) THEN
          K_REJ = K_REJ + 1
          N_REJ = N_REJ + 1
          ! C     	   WRITE(6,*) 'itest ===',ITEST
          !IF (K_REJ.EQ.500) THEN


          DO J=1,6
             GRID(J,ITIK) = WRAN(ISTAR1)
          END DO
          IF (K_REJ.EQ.NTOTAL) THEN
             WRITE(6,*)N_REJ,'   rays have been rejected so far.'
             WRITE(6,*)ITIK-1, '                  accepted.'
             K_REJ = 0
          END IF
          !?????
          !if (itik.eq.ntotal) goto 10005

          if ( (ntotalpoint.gt.0) .and. (n_rej.ge.ntotalpoint)) then
            PRINT *,'sourceSync: too many rejected rays: ',ntotalpoint
            PRINT *,'sourceSync:    check inputs (NTOTALPOINT) and/or file: '//trim(file_bound)
            PRINT *,'sourceSync:    Exit'
            npoint = itik - 1 ! the current index is a bad ray
            ! exit
            goto 10005
          endif
          GOTO 10001
       END IF
    END IF
10000       CONTINUE   ! end loop on number of rays (ITIK)
    
10005 continue


IFLAG = 0
IF (FSOUR.EQ.3) THEN
    ! C
    ! C Reset EPSI_X and EPSI_Z to the values input by the user.
    ! C
    EPSI_X = EPSI_XOLD
    EPSI_Z = EPSI_ZOLD
ENDIF
    
!
! put global variables into pool 
!
!TODO: work without globals!!!!


ntotalpoint = N_REJ+NPOINT
if (n_rej .gt. 0) then
      WRITE(6,*)'----------------------------------------------------------------'
      WRITE(6,*)'  source optimization (rejection, variance reduction) used: '
      WRITE(6,*)N_REJ+NPOINT,   '   total number of rays have been created.'
      WRITE(6,*)NPOINT,         '                  accepted (stored).'
      WRITE(6,*)N_REJ,          '                  rejected.'
      WRITE(6,*)real(N_REJ+NPOINT)/real(NPOINT), '      created/accepted ratio.'
      WRITE(6,*)'----------------------------------------------------------------'
endif

CALL GlobalToPoolSource(pool00)

! deallocate arrays (wiggler)
if (allocated(seed_y)) deallocate( seed_y)
if (allocated(y_x)) deallocate( y_x)
if (allocated(y_xpri)) deallocate( y_xpri)
if (allocated(y_z)) deallocate( y_z)
if (allocated(y_zpri)) deallocate( y_zpri)
if (allocated(y_curv)) deallocate( y_curv)
if (allocated(y_path)) deallocate( y_path)
if (allocated(y_temp)) deallocate( y_temp)
if (allocated(c_temp)) deallocate( c_temp)
if (allocated(x_temp)) deallocate( x_temp)
if (allocated(z_temp)) deallocate( z_temp)
if (allocated(ang_temp)) deallocate( ang_temp)
if (allocated(p_temp)) deallocate( p_temp)
if (allocated(ang2_temp)) deallocate( ang2_temp)
if (allocated(abd2_temp )) deallocate( abd2_temp)

! deallocate arrays (undulator)
if (allocated( cdfx )) deallocate( cdfx )
if (allocated( d_pol )) deallocate( d_pol )
if (allocated( uphi )) deallocate( uphi )
if (allocated( cdfz )) deallocate( cdfz )
if (allocated( utheta )) deallocate( utheta )
if (allocated( cdfw )) deallocate( cdfw )
if (allocated( uener )) deallocate( uener )

WRITE(6,*)'Exit from SOURCE'
RETURN

End Subroutine SOURCESYNC
!
!
!

    
!C+++
!C	SUBROUTINE		SRCDF
!C
!C	PURPOSE		Generate a table of CDF for both the
!C			G0 and the angular part.
!C
!C	RANGE		from 100*lam_c to 10**-5 lam_c
!C                       [Note change from 10*lam_c as in srfunc.F]
!C
!C	OUTPUT		Files: SRANG, SRSPEC, SRDIST
!C
!C---
SUBROUTINE SrCdf

        implicit none 

	REAL(kind=skr) :: X_WRI,Y_WRI,Z_WRI
	REAL(kind=skr) :: X1_WRI,X2_WRI,X3_WRI,X4_WRI,X5_WRI
	INTEGER(kind=ski) :: NP
	real(kind=skr),dimension(10000)   :: ARRX,G0
	real(kind=skr),dimension(10000,3) :: ARR
     	real(kind=skr),dimension(41)      :: F_PAR,F_TOT,F_PER,DEG_POL
     	real(kind=skr),dimension(4,41)    :: CDF,PDF
     	real(kind=skr),dimension(3,10000) :: CDF_G0
	real(kind=skr)   :: ord23=2.0D0/3.0D0, ORD13=1.0D0/3.0D0
	real(kind=skr)   :: ex_low,ex_upp,ex_step,x_ex,x,bk,x_j,x_j1
	real(kind=skr)   :: xstep,cdf_max,pwr_max,coeff,val,xcall
	real(kind=skr)   :: y23,y13,c2_max,c3_max,c4_max,c2_min,c3_min,c4_min
	integer(kind=ski):: i,j,nst

!C
!     	ORD23	=  2.0D0/3.0D0
!     	ORD13	=  1.0D0/3.0D0
!C
     	NP	=  NDIM_TRAJ
     	EX_LOW	=  -5.0D0
!C
!C Way back the maximum energy used to be 20*lam_c (EX_UPP = 1.30103),
!C then FC changed it to 10*lam_c to avoid underflowing REAL*4 variables.
!C Now that it's all REAL*8, it's 100*lam_c (EX_UPP = 2.0).
!C
 	EX_UPP	=   2.0D0
    	EX_STEP =  (EX_UPP - EX_LOW)*1.0D-3
     	DO I=1,NP
     	  X_EX 	=  EX_UPP - EX_STEP*(I-1)
     	  X 	=  10.0D0**X_EX
     	  !CALL	BSKM(X,BK,1,ORD13)
     	  CALL	BSKM(X,BK,iOne,ORD13)
	  ARR(I,1)=BK
	  !CALL	BSKM(X,BK,2,ORD23)
	  CALL	BSKM(X,BK,iTwo,ORD23)
	  ARR(I,2)=BK
          !C
          !C Added the 5/3 case by recursion formula -- Nov 26 1983
          !C						F.C.
          !C
     	  ARR(I,3) = ARR(I,1) + ARR(I,2)*4/X/3
	  ARRX(I)=X
        END DO
     	!WRITE(6,*)'Bessel Functions completed'
!C
!C Function G0 -- April 1984, F.C.
!C Use a trapezoidal integration.
!C
     	G0(1)	=  ARR(1,3)
     	X_J	=  10.0D0**EX_UPP
     	ARRX(1)	=	X_J
     	DO J=2,NP
     	  X_J1 	=  EX_UPP - EX_STEP*(J-1)
     	  X_J 	=  EX_UPP - EX_STEP*(J-2)
     	  X_J1 	=  10.0D0**X_J1
     	  X_J	=  10.0D0**X_J
     	  XSTEP =  ABS(X_J1 - X_J)
     	  G0(J) =  G0(J-1) + ( ARR(J-1,3) + ARR(J,3) )*0.5D0*XSTEP
     	  ARRX(J)	=   X_J1
        END DO
     	!WRITE(6,*)'G0 completed'
!C
!C This is the unformatted file that will eventually contain the
!C whole synchrotron radiation spectrum.
!C
     	OPEN 	(23, FILE='SRDISTR',STATUS='UNKNOWN', &
                 FORM='UNFORMATTED')
	REWIND 	(23)

     	 WRITE (23)	2,NP
     	 CDF_G0 (1,1) = ARRX (1)
     	 CDF_G0 (2,1) = 0.0D0
     	 CDF_G0 (3,1) = 0.0D0
     	 DO J=2,NP
     	   CDF_G0 (1,J) =   ARRX(J)
     	   CDF_G0 (2,J) =   (G0(J-1)+G0(J))*0.5D0*(ARRX(J-1)-ARRX(J)) + &
      			     CDF_G0 (2,J-1)
     	   CDF_G0 (3,J) =   (G0(J-1)*ARRX(J-1)+G0(J)*ARRX(J))*0.5D0* &
      			    (ARRX(J-1)-ARRX(J)) + &
      			     CDF_G0 (3,J-1)
         END DO
     	 CDF_MAX  =  CDF_G0 (2,NP)
     	 PWR_MAX  =  CDF_G0 (3,NP)
     	 DO J=1,NP
     	   CDF_G0 (2,J) =   CDF_G0 (2,J)/CDF_MAX
     	   CDF_G0 (3,J) =   CDF_G0 (3,J)/PWR_MAX
           !C
           !C Write out E/Ec, CDF of the # of photons, CDF of the power.
           !C 
     	  WRITE (23)	CDF_G0(1,J),CDF_G0(2,J),CDF_G0(3,J)
           !CD	  WRITE (25,*)	X_WRI,Y_WRI
        END DO
     	CLOSE	(23)
        print *,'File written to disk: SRDISTR'
     	!WRITE(6,*)'G0 CDF completed and written'
	!WRITE(6,*)'Total area of G0  = ',CDF_MAX
!C	I_CONT	= IRINT	('Continue with angular part [1/0] ? ')
!C	IF (I_CONT.EQ.0) GO TO 111
!C
!C Computes now the vertical distribution at the same values of Y.
!C The range is desumed from the approximated formula for sigma_z'
!C as published by S.Krinsky in its paper in SR (North Holland).
!C This fromula works well for high photon energies, but is too large
!C for low h_nu.
!C

     	OPEN	(23, FILE='SRSPEC',STATUS='UNKNOWN', FORM='UNFORMATTED')
	REWIND	(23)
     	OPEN	(24, FILE='SRANG',STATUS='UNKNOWN', FORM='UNFORMATTED')
	REWIND	(24)
!C
     	!WRITE(6,*)'Step (E.G., 4) ? '
     	!READ(5,*)IST
        ist = 4 
     	NST	=  (NP-1)/IST + 1
     	WRITE	(23)	NST,5,IST
     	WRITE	(24)	NST,4,IST
	WRITE	(24)	EX_UPP,EX_LOW,EX_STEP
     	COEFF	=  4*0.57D0/1957.0D0
     	DO J=1,NP,IST
!C
!C ARRX(J) is lambdac/lambda, or energy/energyc.
!C
     	  VAL	=  1/ARRX(J)
     	  PSIMAX =  2*COEFF*VAL**0.43D0/3.0D0	!Radians
     	  IF (ARRX(J).LT.1.0E-4) PSIMAX = PSIMAX*0.75D0
     	  STEP	=  PSIMAX/20
     	!WRITE(6,*)'Start J= ',J,' for e/ec= ',ARRX(J)
     	 DO I=1,41
!C
!C Range from -psimax to +psimax
!C
     	   PSI	=  -PSIMAX + (I-1)*STEP
     	   PSI	=   PSI*1957.0D0
     	   CDF (1,I) = PSI
      	   ARG  =  (1 + PSI**2)
     	   XCALL=   0.5D0*ARG**1.5D0*ARRX(J)
     	  !CALL	BSKM (XCALL,Y23,2,ORD23)
     	  !CALL	BSKM (XCALL,Y13,1,ORD13)
     	  CALL	BSKM (XCALL,Y23,iTwo,ORD23)
     	  CALL	BSKM (XCALL,Y13,iOne,ORD13)
     	   F_PAR(I)=  ARG**2*Y23**2
     	   F_PER(I)=  PSI**2*ARG*Y13**2
     	   F_TOT(I)=  F_PAR(I) + F_PER(I)
!C
!C Fix May 9, 1990.    SHADOW defined the degree of polarization as Ax/(Ax+Az),
!C instead of Ax^2/(Ax^2+Az^2).  So here we need to take the square root of the
!C powers (F_PAR, F_PER).
!C
!C Old:     DEG_POL(I)   = F_PAR(I)/F_TOT(I)
	   DEG_POL(I)   = sqrt(F_PAR(I))/ (sqrt(F_PAR(I)) + sqrt(F_PER(I)))

     	END DO
!50     	 CONTINUE
!C
!C Computes the three CDF's
!C
     	CDF(2,1) =  F_PAR(1)*STEP
     	CDF(3,1) =  F_PER(1)*STEP
     	CDF(4,1) =  F_TOT(1)*STEP
     	DO I=2,41
     	  CDF(2,I) = CDF(2,I-1) + (F_PAR(I)+F_PAR(I-1))*.5D0*STEP
     	  CDF(3,I) = CDF(3,I-1) + (F_PER(I)+F_PER(I-1))*.5D0*STEP
     	  CDF(4,I) = CDF(4,I-1) + (F_TOT(I)+F_TOT(I-1))*.5D0*STEP
        END DO
     	C2_MAX	=   CDF(2,41) - CDF(2,1)
     	C3_MAX  =   CDF(3,41) - CDF(3,1)
     	C4_MAX  =   CDF(4,41) - CDF(4,1)
     	C2_MIN	=   CDF(2,1)
     	C3_MIN	=   CDF(3,1)
     	C4_MIN	=   CDF(4,1)
!C
!C The integral of PDF over the angle(in radian) equal to G0, for an 1 Gev 
!C machine.
!C
     	DO I=1,41
	  PDF(1,I) = CDF(1,I)
	  PDF(2,I) = F_PAR(I)/C4_MAX*G0(J)
	  PDF(3,I) = F_PER(I)/C4_MAX*G0(J)
	  PDF(4,I) = F_TOT(I)/C4_MAX*G0(J)
     	  CDF(2,I) = (CDF(2,I)-C2_MIN)/C2_MAX
     	  CDF(3,I) = (CDF(3,I)-C3_MIN)/C3_MAX
     	  CDF(4,I) = (CDF(4,I)-C4_MIN)/C4_MAX
       	END DO
!C
!C Write out; file is real*4
!C
     	  X_WRI =   ARRX(J)
     	  Z_WRI =   PSIMAX
     	 WRITE (23)	X_WRI,Z_WRI
	 WRITE (24)	X_WRI,Z_WRI,G0(J)
     	 DO  80 I=1,21
     	   X1_WRI =   CDF(1,I)
     	   X2_WRI =   CDF(2,I)
     	   X3_WRI =   CDF(3,I)
     	   X4_WRI =   CDF(4,I)
	   X5_WRI =   DEG_POL(I)
     	  WRITE (23)	X1_WRI,X2_WRI,X3_WRI,X4_WRI,X5_WRI
	  WRITE (24)	PDF(1,I),PDF(2,I),PDF(3,I),PDF(4,I)
80     	 CONTINUE
     	END DO
111	CONTINUE
	
	CLOSE(23)
        print *,'File written to disk: SRSPEC'
	CLOSE(24)
        print *,'File written to disk: SRANG'
END SUBROUTINE SrCdf

!C+++
!C	SUBROUTINE	SRFUNC
!C
!C	PURPOSE		Generate a table of values of functions frequently
!C			used in Synchrotron Radiation calculations.
!C			
!C			Bessel Functions of order 1/2, 3/2, 5/2
!C
!C			Green's G0 and H0 functions
!C
!C	RANGE		from 100*lam_c to 10**-5 lam_c
!C                       [Note change from 10*lam_c Mon Apr 28 19:45:17 1997]
!C
!C	OUTPUT		Files: G0FUNC and BSKM
!C
!C---

!TODO: Check whether the files created here (G0FUNC and BSKM) are used or not
SUBROUTINE SrFunc

        implicit none
     	real(kind=skr),dimension(1000) :: PPAR,PPER,PTOT,ANGARR
	real(kind=skr)  :: ORD23=2.0D0/3.0D0,ORD13=1.0D0/3.0D0
	real(kind=skr),dimension(10000)   :: ARRX,G0
	real(kind=skr),dimension(10000,3) :: ARR


	real(kind=skr) :: ex_step,ex_upp,ex_low,x_ex,x_j,x_j1,xstep
        integer(kind=ski) :: np,i,j,kl

     	NP	=  NDIM_TRAJ
     	EX_LOW	=  -5.0D0
     	EX_UPP  =   2.0D0
     	EX_STEP =  (EX_UPP - EX_LOW)/(NP - 1)
     	DO I=1,NP
     	  X_EX 	=  EX_UPP - EX_STEP*(I-1)
     	  X 	=  10.0D0**X_EX
     	 !CALL	BSKM(X,BK,1,ORD13)
     	 CALL	BSKM(X,BK,iOne,ORD13)
	  ARR(I,1)=BK
	 !CALL	BSKM(X,BK,2,ORD23)
	 CALL	BSKM(X,BK,iTwo,ORD23)
	  ARR(I,2)=BK
!C
!C Added the 5/3 case by recursion formula -- Nov 26 1983
!C						F.C.
!C
     	  ARR(I,3) = ARR(I,1) + ARR(I,2)*4/X/3
	  ARRX(I)=X
     	END DO 
!C
	OPEN	(2, FILE='BSKM', STATUS='UNKNOWN')
	REWIND (2)
	WRITE	(2,1000)
1000	FORMAT	(1X,' MODIFIED BESSEL FUNCTION OF ORDER N ',//)
	WRITE	(2,1100)
1100 	FORMAT	(1X,' X= ',T20,' N=1/3 ',T40,' N=2/3 ',T60,' N=5/3 ',//)
	WRITE	(2,1200) (ARRX(I),(ARR(I,KL),KL=1,3),I=1,NP)
1200	FORMAT	(1X,G14.7,T20,G14.7,T40,G14.7,T60,G14.7)
	CLOSE	(2)
	print *,'File written to disk: BSKM'
!C
!C Function G0 -- April 1984, F.C.
!C Use a trapezoidal integration.
!C
     	OPEN 	(22, FILE='G0FUNC', STATUS='UNKNOWN')
	REWIND (22)
     	G0(1)	=  ARR(1,3)
     	X_J	=  10.0D0**EX_UPP
     	WRITE (22,*)	X_J,G0(1)
     	ARRX(1)	=	X_J
     	DO J=2,NP
     	  X_J1 	=  EX_UPP - EX_STEP*(J-1)
     	  X_J 	=  EX_UPP - EX_STEP*(J-2)
     	  X_J1 	=  10.0D0**X_J1
     	  X_J	=  10.0D0**X_J
     	  XSTEP =  ABS(X_J1 - X_J)
     	  G0(J) =  G0(J-1) + ( ARR(J-1,3) + ARR(J,3) )*0.5D0*XSTEP
     	  WRITE (22,*)	X_J1,G0(J)
     	  ARRX(J)	=   X_J1
	END DO
     	CLOSE 	(22)
	print *,'File written to disk: G0FUNC'
!C
!C This is the unformatted file that will eventually contain the
!C whole synchrotron radiation spectrum.
!C
     	OPEN 	(23, FILE='G0UNF', STATUS='UNKNOWN', FORM='UNFORMATTED')
	REWIND (23)
     	  WRITE (23)	NP,EX_LOW,EX_UPP
     	 DO J=1,NP
     	  WRITE (23)	ARRX(J),G0(J)
         END DO
     	CLOSE	(23)
	print *,'File written to disk: G0UNF'
END SUBROUTINE SrFunc


!
!
!
SUBROUTINE Shadow3Source

implicit none
  
character(len=1024)    ::  mode
character(len=1024)    ::  bgnfile,inFile
integer (kind=ski)     ::  iSynchrotron=0,iErr

type (poolSource)      ::  pool00
real(kind=skr), allocatable, dimension(:,:) :: ray

print *,' '
print *,'SOURCE selected. Begin procedure.'
print *,'   Select mode, enter :'
print *,'prompt      : prompted session'
print *,'batch       : file-oriented session'
print *,'systemfile  : file-oriented session using start.00'
print *,' '


mode = RString('Select mode (prompt OR batch OR systemfile): ')

SELECT CASE (mode)
   CASE("prompt")
     CALL input_source1
     CALL GlobalToPoolSource(pool00)
     CALL PoolSourceWrite(pool00,"start.00")
   CASE("batch")
     inFile = RString("File containing source description [ Default: start.00 ] ?:")
     IF (trim(inFile) == "") THEN 
        inFile="start.00"
     END IF
     CALL PoolSourceLoad(pool00,inFile) 
   CASE("systemfile")
     inFile="start.00"
     CALL PoolSourceLoad(pool00,inFile) 
   CASE DEFAULT
      print *,'Shadow3Source: Mode not found: '//trim(mode)
      RETURN
END SELECT

!
! allocate ray 
!
!print *,'Allocating array with pool00%npoint: ',pool00%npoint
  IF (allocated(ray)) deallocate(ray)
  ALLOCATE( ray(18,pool00%npoint) )
  ray=0.0d0
!
!  
  IF ((pool00%fdistr.EQ.4).OR.(pool00%fsource_depth.EQ.4).OR.(pool00%f_wiggler.GT.0)) THEN
    iSynchrotron = 1
  ENDIF
!
  IF (iSynchrotron .eq. 1) THEN 
    CALL  SourceSync (pool00,ray,pool00%npoint)
  ELSE 
    ! Note that the routine sourceGeom (geometrical source) in 
    ! shadow_kernel is a subset of SourceSync (in shadow_sourcesync)
    ! therefore this call is unnecessary because the same work can be
    ! done by SourceSync.
    ! The reason for keeping the two functions is for simplicity and 
    ! to structurate better the shadow3 code: sourceGeom uses a relatively
    ! small number of subroutines and functions, all available in the
    ! kernel, whereas SourceSync is much more complex and is included in
    ! a separated synchrotron module. 
    ! Therefore, users that do not want synchrotron, they just comment 
    ! the "USE shadow_sourcesync" and "CALL SourceSync"
    CALL  sourceGeom(pool00, ray,pool00%npoint)
  ENDIF 
  
  ! write file begin.dat
  bgnfile  = "begin.dat"
  CALL beamWrite(ray,ierr,pool00%ncol,pool00%npoint,bgnfile)
  IF (ierr.NE.0) PRINT *, "GEN_SOURCE: beamWrite failed to write file: "//TRIM(bgnfile)

  ! write end.00 file
  CALL GlobalToPoolSource(pool00)
  CALL PoolSourceWrite(pool00,"end.00")

  IF (allocated(ray)) deallocate(ray)

  print *,' '
  print *,'Source has been successfully generated.'
  print *,' '
  print *,'SOURCE procedure completed.'
  print *,' '
RETURN

END SUBROUTINE Shadow3Source

End Module shadow_synchrotron
