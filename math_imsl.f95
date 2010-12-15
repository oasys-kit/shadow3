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

Module math_imsl
    !---- Use Modules ----!

    !---- Variables ----!
    implicit none

    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    ! public :: 
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public :: mdnris


    !---- List of private functions ----!
    !---- List of private subroutines ----!
    private :: merfi



    !---- Definitions ----!


    !---- Interfaces ----!

  Contains
    !
    !---- Private Functions ----!
    !

    !
    !---- Public Functions ----!
    !

    !
    !---- Private Subroutines ----!
    !

    !
    !
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
          !IMPLICIT DOUBLE PRECISION (A-H, O-Z)
          IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H, O-Z)
          IMPLICIT INTEGER(KIND=4) (I-N)
          INTEGER(KIND=4)            IER
    ! C                                  SPECIFICATIONS FOR LOCAL VARIABLES
          REAL               A,B,X,Z,W,WI,SN,SD,F,Z2,RINFM,A1,A2,A3,B0,B1, &
                            B2,B3,C0,C1,C2,C3,D0,D1,D2,E0,E1,E2,E3,F0,F1, &
                            F2,G0,G1,G2,G3,H0,H1,H2,SIGMA               
          DATA               A1/-.5751703/,A2/-1.896513/,A3/-.5496261E-1/
          DATA               B0/-.1137730/,B1/-3.293474/,B2/-2.374996/  
          DATA               B3/-1.187515/                             
          DATA               C0/-.1146666/,C1/-.1314774/,C2/-.2368201/
          DATA               C3/.5073975E-1/                         
          DATA               D0/-44.27977/,D1/21.98546/,D2/-7.586103/
          DATA               E0/-.5668422E-1/,E1/.3937021/,E2/-.3166501/
          DATA               E3/.6208963E-1/                           
          DATA               F0/-6.266786/,F1/4.666263/,F2/-2.962883/ 
          DATA               G0/.1851159E-3/,G1/-.2028152E-2/        
          DATA               G2/-.1498384/,G3/.1078639E-1/          
          DATA               H0/.9952975E-1/,H1/.5211733/          
          DATA               H2/-.6888301E-1/                     
          DATA               RINFM/1.7014E+38/                   
    ! C                                  FIRST EXECUTABLE STATEMENT           
          IER = 0
          X = P 
          SIGMA = SIGN(1.0,X)
    ! C                                  TEST FOR INVALID ARGUMENT            
          IF (.NOT.(X.GT.-1. .AND. X.LT.1.)) GO TO 30
          Z = ABS(X)                                
          IF (Z.LE. .85) GO TO 20                  
          A = 1.-Z                                
          B = Z                                  
    ! C                                  REDUCED ARGUMENT IS IN (.85,1.),     
    ! C                                     OBTAIN THE TRANSFORMED VARIABLE   
    5 W = SQRT(-ALOG(A+A*B))                
      IF (W.LT.2.5) GO TO 15               
      IF (W.LT.4.) GO TO 10               
    ! C                                  W GREATER THAN 4., APPROX. F BY A    
    ! C                                     RATIONAL FUNCTION IN 1./W         
      WI = 1./W                          
      SN = ((G3*WI+G2)*WI+G1)*WI        
      SD = ((WI+H2)*WI+H1)*WI+H0          
      F = W + W*(G0+SN/SD)               
      GO TO 25                          
    ! C                                  W BETWEEN 2.5 AND 4., APPROX. F      
    ! C                                     BY A RATIONAL FUNCTION IN W       
    10 SN = ((E3*W+E2)*W+E1)*W          
      SD = ((W+F2)*W+F1)*W+F0          
      F = W + W*(E0+SN/SD)             
      GO TO 25                         
    ! C                                  W BETWEEN 1.13222 AND 2.5, APPROX.   
    ! C                                     F BY A RATIONAL FUNCTION IN W     
    15 SN = ((C3*W+C2)*W+C1)*W         
      SD = ((W+D2)*W+D1)*W+D0         
      F = W + W*(C0+SN/SD)            
      GO TO 25                        
    ! C                                  Z BETWEEN 0. AND .85, APPROX. F      
    ! C                                     BY A RATIONAL FUNCTION IN Z       
    20 Z2 = Z*Z                        
      F = Z+Z*(B0+A1*Z2/(B1+Z2+A2/(B2+Z2+A3/(B3+Z2)))) 
    ! C                                  FORM THE SOLUTION BY MULT. F BY      
    ! C                                     THE PROPER SIGN                   
    25 Y = SIGMA*F                                     
      IER = 0                                        
      GO TO 9005                                    
    ! C                                  ERROR EXIT. SET SOLUTION TO PLUS     
    ! C                                     (OR MINUS) INFINITY               
    30 IER = 129                                    
      Y = SIGMA * RINFM                            
     9000 CONTINUE                                     
    !      CALL UERTST(IER,6HMERFI )                    
      print *,"Error from math_imsl merfi. Called with: ",P
      stop 
     9005 RETURN                                       
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
      IMPLICIT REAL(KIND=KIND(1.0D0)) (A-H, O-Z)
      IMPLICIT INTEGER(KIND=4) (I-N)
    ! C     DOUBLE PRECISION   P,Y                                            
      INTEGER(KIND=4)            IER
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
    ! C                                  P TOO SMALL, COMPUTE Y DIRECTLY      
    10 A = P+P
      W = SQRT(-ALOG(real(A+(A-A*A))))
    ! C                                  USE A RATIONAL FUNCTION IN 1./W      
      WI = 1./W
      SN = ((G3*WI+G2)*WI+G1)*WI
      SD = ((WI+H2)*WI+H1)*WI+H0
      Y = W + W*(G0+SN/SD)
      Y = -Y*SQRT2
      GO TO 9005
    9000 CONTINUE
    !      CALL UERTST(IER,6HMDNRIS)
      print *,"Error from math_imsl mdnris. Called with: ",P
      stop
    9005 RETURN
    END SUBROUTINE MDNRIS

End Module math_imsl
