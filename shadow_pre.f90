!----
!---- MODULE:  shadow_Pre
!----
!---- Preprocessors for Shadow
!---- Contains: 
!----
!----
!----
!---- Example of usage: 
!----
!----

Module shadow_Pre
    !---- Use Modules ----!

    use stringio
    use math
!    use math_imsl
    use shadow_kind
!use shadow_variables
!use shadow_kernel
!    use shadow_sourcesync, only : srcdf ! needed by nphoton only

    !---- Variables ----!
    implicit none

    ! these variables are used by pre_mlayer
    !COMMON	/OPCON	/  ISTART,IFINAL
    !COMMON	/DAT	/  ENER(420),DELTA(420),BETA(420)
    real(kind=skr),dimension(420) :: ENER,DELTA,BETA
    integer(kind=ski)             :: ISTART,IFINAL


!todo: fix this repetition
!
! the global variables here are only used for undulator and not for wiggler
! 
!      	real(kind=ski),parameter :: PI=3.141592653589793238462643D0
!     	real(kind=ski),parameter :: TWOPI=6.283185307179586476925287D0
!     	real(kind=ski),parameter :: PIHALF=1.570796326794896619231322D0
!     	real(kind=ski),parameter :: TOANGS=1.239852D+4


    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
!    public :: 
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public ::  presurface   
    public ::  prerefl,pre_mlayer,grade_mlayer,bragg

    !---- List of private functions ----!
    !---- List of private subroutines ----!


    !---- Definitions ----!


    !---- Interfaces ----!


  Contains
    !
    !---- Routines ----!
    !


!todo: mv ibccu and the other to math_imsl

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
!C                                  SPECIFICATIONS FOR ARGUMENTS         
      !INTEGER            NX,NY,IC,IER                                   
      !DOUBLE PRECISION   F(IC,1),X(1),Y(1),C(2,IC,1),WK(1)              
      integer(kind=ski) :: NX,NY,IC,IER                                   
      real(kind=skr),dimension(1)      :: X,Y,WK
      real(kind=skr),dimension(IC,1)   :: F
      real(kind=skr),dimension(2,IC,1) :: C
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      !INTEGER            IWK                                            
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
      CALL IBCDCU(Y,WK,NY,2*NX,WK(IWK+1),C,NY,2*IC,IER)                 
      IF (IER .EQ. 0) GO TO 9005                                        
 9000 CONTINUE                                                          
      !CALL UERTST(IER,6HIBCCCU)                                         
      PRINT *,'IBCCCU Error'
      STOP 'Aborted'
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
      !INTEGER            N,M,IC1,IC2,IER                                
      !DOUBLE PRECISION   TAU(N),GTAU(IC1,1),W(N,2),VS(IC2,2,1)          
      integer(kind=ski) :: N,M,IC1,IC2,IER                                
      real(kind=skr),dimension(N)      :: TAU
      real(kind=skr),dimension(IC1,1)  :: GTAU
      real(kind=skr),dimension(N,2)    :: W
      !!real(kind=skr),dimension(IC2,2,1):: VS
      !! changed srio. Warning!!!
      real(kind=skr),dimension(IC2,2,2):: VS
!C                                  SPECIFICATIONS FOR LOCAL VARIABLES   
      !INTEGER            I,JJ,JM1,JP1,J,K,LIM,LL,LP1,NM1                
      integer(kind=ski) :: I,JJ,JM1,JP1,J,K,LIM,LL,LP1,NM1
      !DOUBLE PRECISION   AA,BB,C1,C2,CC,DD,DTAU,G,H,RATIO,U,XILIM       
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
!C+++
!C	PROGRAM		PRESURFACE
!C
!C	PURPOSE		To compute a bi-cubic spline fitting a user
!C			supplied surface
!C
!C	INPUT		A file containing the surface specification, in
!C			the format:
!C				Nx, Ny
!C				Y(1),...,Y(Ny)
!C				X(1),Z(1,1),...,Z(1,Ny)
!C				.....
!C				X(Nx),Z(Nx,1),...,Z(Nx,Ny)
!C
!C	Output		A file containing the spline array prepared by
!C			IMSL function IBCCCU
!C---
SUBROUTINE PRESURFACE
	implicit none

	character(len=1024) :: inFile,outFile


!C The dimensions of WK should be 2*NX*NY + 2*MAX(NX,NY).  If so, the
!C size of WK for NX=101, NY=101 should be 20604 *not* 20602 as above.
!C Below I have used the correct formula and have dimensioned WK for 201
!C points, as I have CSPL, X, Y, and Z.

	!DIMENSION	CSPL (2,201,2,201), WK (81204)

	!real(kind=skr),dimension(2,201,2,201) :: CSPL 
        !real(kind=skr),dimension(81204)       :: WK
	!real(kind=skr),dimension(:,:,:,:),allocatable :: CSPL
	real(kind=skr),dimension(:),allocatable       :: WK
	real(kind=skr),dimension(:,:,:,:),allocatable :: CSPL
	integer(kind=ski)   :: iErr,iWhat,nx,ny,i,j,iEr,ic,nw

!C     	DIMENSION	X(101),Y(101),Z(101,101)
     	!DIMENSION	X(201),Y(201),Z(201,201)
     	!real(kind=skr),dimension(201,201) :: Z
        !real(kind=skr),dimension(201)     :: X,Y
! todo: by now, only allocatable in Y direction. To continue with X
!       (problema in calling imsl stuff...)
        real(kind=skr),dimension(201)     :: X
     	real(kind=skr),dimension(:,:),allocatable :: Z
        !real(kind=skr),dimension(:),allocatable   :: X,Y
        real(kind=skr),dimension(:),allocatable   :: Y



1     	INFILE	=   RSTRING ('File containing the surface mesh ? ')
        OPEN	(20, FILE=INFILE, STATUS='OLD', IOSTAT = IERR)

    	IF (IERR.NE.0) THEN
    	  WRITE(6,*)'Cannot access ',INFILE
    	  IWHAT = IYES ('Retry ? ')
    	 IF (IWHAT.EQ.1) GOTO 1
    	  STOP
    	END IF
     	READ (20,*) NX, NY

     	IF (NX.LT.4.OR.NY.LT.4) THEN
     	  WRITE(6,*)'Not enough points to define arrays. Must be at',&
      		' least 4 points in each direction.'
     	  STOP 'Please retry with larger arrays.'
     	END IF

     	IF (NX.GT.201) THEN
     	  WRITE(6,*)'Arrays X too large: '
     	  WRITE(6,*)'Maximum allowed is 201 points in X, unlimited in Y.'
     	  STOP 'Please retry with smaller arrays.'
     	END IF

	! allocating arrays
        !nw = 2*nx*ny+2*max(nx,ny)
        nw = 2*201*ny+2*max(201,ny)
!print *,'nx: ',nx
!print *,'ny: ',ny
!print *,'nw: ',nw
        !allocate(Z(NX,NY))	
        allocate(Z(201,NY))	
        allocate(Y(NY))
        !allocate(X(NX))
	!allocate(CSPL(2,NX,2,NY))
	allocate(CSPL(2,201,2,NY))
	allocate(WK(nw))
	!ic = 1+max(nx,ny)
	ic = 1+max(201,ny)

!C     	IF (NX.GT.101.OR.NY.GT.101) THEN
!C Below, we now allow a maximum of 101 points instead of 201.
!C
!     	IF (NX.GT.201.OR.NY.GT.201) THEN
!!C     	  WRITE(6,*)'Arrays too large. Maximum allowed is 101 points.'
!     	  WRITE(6,*)'Arrays too large. Maximum allowed is 201 points.'
!     	  STOP 'Please retry with smaller arrays.'
!     	END IF
     	WRITE(6,*)'Setting up ',NX,' by ',NY,' array.'
!C
!C Reads in Y array
!C
     	READ (20,*) (Y(I),I=1,NY)
!C
!C Now read X and Z arrays
!C
     	DO 10 I=1,NX
     	  READ (20,*) X(I),(Z(I,J),J=1,NY)
10    	CONTINUE
     	WRITE(6,*)'Array read correctly. Compute spline.'
!C
!C Call IMSL routine to compute spline. Now use 201 points instead of 101.
!C
!C     	CALL	IBCCCU ( Z, X, NX, Y, NY, CSPL, 101, WK, IER)
     	!CALL	IBCCCU ( Z, X, NX, Y, NY, CSPL, 201, WK, IER)
     	CALL	IBCCCU ( Z, X, NX, Y, NY, CSPL, 201, WK, IER)
     	
	IF (IER.EQ.132) THEN
!C     	  WRITE(6,*)'The X and/or Y array are not ordered properly. 
!C     $    Please check data in ',INFILE
     	  WRITE(6,*)'The X and/or Y array are/is not ordered properly.' 
          WRITE(6,*)'Please check data in ',INFILE
	  STOP
     	END IF
     	WRITE(6,*)'Spline succesfully completed.'
     	OUTFILE	=   RSTRING ('Please enter file-name for storage: ')
!#ifdef vms
!     	OPEN (20, FILE=OUTFILE, STATUS='NEW', FORM='UNFORMATTED')
!#else
     	OPEN (20, FILE=OUTFILE, STATUS='UNKNOWN', FORM='UNFORMATTED')
	REWIND (20)
!#endif 
     	 WRITE (20)	NX, NY
     	 WRITE (20)	X,Y
     	 WRITE (20)	CSPL
     	CLOSE (20)
        !deallocate(Z)
        !deallocate(Y)
        !deallocate(X)
	!deallocate(CSPL)
	!deallocate(WK)
     	WRITE(6,*)'Task completed. Spline stored in ',OUTFILE
     	!STOP
	RETURN
END SUBROUTINE PreSurface

!
! Optics preprocessors
!



!C+++
!C	SUBROUTINE	OPT_ELE
!C
!C	Purpose		Returns alpha and gamma for an element
!C			
!C---
SUBROUTINE OPT_ELE (RF1,RF2,ENERGY,DENSITY)

	!REAL*4		ATWT,RMU,EMF,DENSITY
     	!REAL*4		ENERGY(420),RF1(420),RF2(420)
     	!REAL*4		avog,atoms
     	!INTEGER *4 	NUMBER1
     	!INTEGER *4 	i
	!CHARACTER*2	ELEMENT
	real(kind=skr)  :: ATWT,RMU,EMF,DENSITY
     	real(kind=skr),dimension(420) :: ENERGY,RF1,RF2
     	real(kind=skr)    :: avog,atoms
     	integer(kind=ski) :: NUMBER1
     	integer(kind=ski) :: i
	character(len=2)  :: ELEMENT
	!CHARACTER*80	RSTRING
!todo Sort out constants and physical constants...
     	DATA	AVOG	/  6.022098E+23 		     /


     	ELEMENT	= RSTRING('Enter atomic symbol (capitalized) : ')
!** Get the data
	CALL 	READLIB(ELEMENT,NUMBER1,ATWT,RMU,EMF,ENERGY,RF1,RF2)
!** Computes atomic concentration
     	ATOMS	=   DENSITY/ATWT*AVOG
!C 
!C Return the scattering factor weighed by atomic concentration.
!C
	DO 12 I = 1, 420
	  RF1(I)	= ATOMS*RF1(I)
12 	CONTINUE
	DO 13 I = 1, 420
	  RF2(I)	= ATOMS*RF2(I)
13 	CONTINUE
     	RETURN
END SUBROUTINE OPT_ELE


!C+++
!C	SUBROUTINE	OPT_COM
!C
!C---
SUBROUTINE OPT_COM (RF1,RF2,ENERGY,DENSITY)
	real(kind=skr) :: ATWT,RMU,EMF,DENSITY
        real(kind=skr),dimension(420) :: RF1,RF2,ENERGY
	!DIMENSION	IREL(5)
	integer(kind=ski),dimension(5) :: IREL
	integer(kind=ski)              :: i,nAtoms,nTot
	real(kind=skr)                 :: rMol,rMolec,at1,at2,at3,at4,at5,f1,f2
     	integer(kind=ski)              :: NUMBER1
     	integer(kind=ski),dimension(5) :: NATOM
	CHARACTER(len=2),dimension(5)  :: ELEMENT
	CHARACTER(len=2)               :: ELE
	!CHARACTER*80	RSTRING
     	!REAL*4		OUTFIL11(420),OUTFIL12(420)
     	!REAL*4		OUTFIL21(420),OUTFIL22(420)
     	!REAL*4		OUTFIL31(420),OUTFIL32(420)
     	!REAL*4		OUTFIL41(420),OUTFIL42(420)
     	!REAL*4		OUTFIL51(420),OUTFIL52(420)
     	real(kind=skr),dimension(420) :: OUTFIL11,OUTFIL12
     	real(kind=skr),dimension(420) :: OUTFIL21,OUTFIL22
     	real(kind=skr),dimension(420) :: OUTFIL31,OUTFIL32
     	real(kind=skr),dimension(420) :: OUTFIL41,OUTFIL42
     	real(kind=skr),dimension(420) :: OUTFIL51,OUTFIL52
     	real(kind=skr),dimension(5)   :: AT,RREL
     	real(kind=skr)                :: avog

     	AVOG	=   6.022098E+23
     	DO 14 I = 1,5
     	  IREL(I) = 0
     	  RREL(I) = 0.0
14      CONTINUE
     	 WRITE(6,*) 'The program is setup to compute optical constants',&
      		' of compounds with up to 5 elements.'
     	NATOMS	= IRINT('How many atomic species : ')
     	NTOT = 0
     	 WRITE(6,*) 'H2O would be: H, 2 and O, 1.'
     	DO 16 I = 1,NATOMS
     	   WRITE(6,*) 'Enter atomic symbol (capitalized) and ', &
          'formula index for : ',I
	  ELEMENT(I)	= RSTRING(' ')
     	  IREL(I)	= IRINT(' ')
     	  NTOT 		= NTOT + IREL(I)
16      CONTINUE
     	DO 17 I=1,NATOMS
     	  RREL(I) = FLOAT(IREL(I))/NTOT
17     	CONTINUE
!** Get the data. F1 and F2 are then 'averaged' together
     	RMOL	= 0.0
     	GO TO (55,44,33,22,11)	NATOMS
11	ELE	= ELEMENT(5)
	CALL READLIB(ELE,NUMBER1,ATWT,RMU,EMF,ENERGY,OUTFIL51,OUTFIL52)
	NATOM(5)	= NUMBER1
     	RMOL	=          ATWT*IREL(5)
22	ELE	= ELEMENT(4)
	CALL READLIB(ELE,NUMBER1,ATWT,RMU,EMF,ENERGY,OUTFIL41,OUTFIL42)
	NATOM(4)	= NUMBER1
     	RMOL	= RMOL+          ATWT*IREL(4)
33	ELE	= ELEMENT(3)
	CALL READLIB(ELE,NUMBER1,ATWT,RMU,EMF,ENERGY,OUTFIL31,OUTFIL32)
	NATOM(3)	= NUMBER1
     	RMOL	= RMOL+         ATWT*IREL(3)
44	ELE	= ELEMENT(2)
	CALL READLIB(ELE,NUMBER1,ATWT,RMU,EMF,ENERGY,OUTFIL21,OUTFIL22)
	NATOM(2)	= NUMBER1
     	RMOL	= RMOL+         ATWT*IREL(2)
55	ELE	= ELEMENT(1)
	CALL READLIB(ELE,NUMBER1,ATWT,RMU,EMF,ENERGY,OUTFIL11,OUTFIL12)
	NATOM(1)	= NUMBER1
     	RMOL	= RMOL+         ATWT*IREL(1)
!** Computes atomic concentrations and molecular weigth
     	RMOLEC	=   DENSITY/RMOL*AVOG	! This is the number of molecules
     	WRITE (6,*)  &
      'Molecular weight is : ',RMOL,' Number of molecules/cm3: ',RMOLEC
     	AT1	=   RMOLEC*irel(1)
     	AT(1)	=   AT1
     	AT2	=   RMOLEC*irel(2)
     	AT(2)	=   AT2
     	AT3	=   RMOLEC*irel(3)
     	AT(3)	=   AT3
     	AT4	=   RMOLEC*irel(4)
     	AT(4)	=   AT4
     	AT5	=   RMOLEC*irel(5)
     	AT(5)	=   AT5
!** Computes now the effective F1 and F2
     	DO 18 I = 1,420
     	F1	=   0.0
     	F2	=   0.0
     	GO TO (155,144,133,122,111)	NATOMS
111	F1	=        OUTFIL51(I)*AT5
     	F2	=        OUTFIL52(I)*AT5
122	F1	=   F1 + OUTFIL41(I)*AT4
     	F2	=   F2 + OUTFIL42(I)*AT4
133	F1	=   F1 + OUTFIL31(I)*AT3
     	F2	=   F2 + OUTFIL32(I)*AT3
144	F1	=   F1 + OUTFIL21(I)*AT2
     	F2	=   F2 + OUTFIL22(I)*AT2
155	F1	=   F1 + OUTFIL11(I)*AT1
     	F2	=   F2 + OUTFIL12(I)*AT1
     	RF1(I)	=   F1
     	RF2(I)	=   F2
18      CONTINUE
	RETURN
END SUBROUTINE OPT_COM
!
!
!

SUBROUTINE WriteF12LibIndex

implicit none

integer(kind=ski) :: iErr

OPEN (20, FILE="F12LIB.INDEX", STATUS='UNKNOWN', FORM='UNFORMATTED', IOSTAT=iErr)

IF (iErr /= 0) THEN 
     CALL LEAVE ('WriteF12LibIndex', 'Cannot write F12LIB.INDEX ', 1)
ENDIF
!REWIND (20)
write(20,'(A)')  "AC 89"
write(20,'(A)')  "AG 47"
write(20,'(A)')  "AL 13"
write(20,'(A)')  "AR 18"
write(20,'(A)')  "AS 33"
write(20,'(A)')  "AT 85"
write(20,'(A)')  "AU 79"
write(20,'(A)')  "B   5"
write(20,'(A)')  "BA 56"
write(20,'(A)')  "BE  4"
write(20,'(A)')  "BI 83"
write(20,'(A)')  "BR 35"
write(20,'(A)')  "C   6"
write(20,'(A)')  "CA 20"
write(20,'(A)')  "CD 48"
write(20,'(A)')  "CE 58"
write(20,'(A)')  "CL 17"
write(20,'(A)')  "CO 27"
write(20,'(A)')  "CR 24"
write(20,'(A)')  "CS 55"
write(20,'(A)')  "CU 29"
write(20,'(A)')  "DY 66"
write(20,'(A)')  "ER 68"
write(20,'(A)')  "EU 63"
write(20,'(A)')  "F   9"
write(20,'(A)')  "FE 26"
write(20,'(A)')  "FR 87"
write(20,'(A)')  "GA 31"
write(20,'(A)')  "GD 64"
write(20,'(A)')  "GE 32"
write(20,'(A)')  "H   1"
write(20,'(A)')  "HE  2"
write(20,'(A)')  "HF 72"
write(20,'(A)')  "HG 80"
write(20,'(A)')  "HO 67"
write(20,'(A)')  "I  53"
write(20,'(A)')  "IN 49"
write(20,'(A)')  "IR 77"
write(20,'(A)')  "K  19"
write(20,'(A)')  "KR 36"
write(20,'(A)')  "LA 57"
write(20,'(A)')  "LI  3"
write(20,'(A)')  "LU 71"
write(20,'(A)')  "MG 12"
write(20,'(A)')  "MN 25"
write(20,'(A)')  "MO 42"
write(20,'(A)')  "N   7"
write(20,'(A)')  "NA 11"
write(20,'(A)')  "NB 41"
write(20,'(A)')  "ND 60"
write(20,'(A)')  "NE 10"
write(20,'(A)')  "NI 28"
write(20,'(A)')  "O   8"
write(20,'(A)')  "OS 76"
write(20,'(A)')  "P  15"
write(20,'(A)')  "PA 91"
write(20,'(A)')  "PB 82"
write(20,'(A)')  "PD 46"
write(20,'(A)')  "PM 61"
write(20,'(A)')  "PO 84"
write(20,'(A)')  "PR 59"
write(20,'(A)')  "PT 78"
write(20,'(A)')  "RA 88"
write(20,'(A)')  "RB 37"
write(20,'(A)')  "RE 75"
write(20,'(A)')  "RH 45"
write(20,'(A)')  "RN 86"
write(20,'(A)')  "RU 44"
write(20,'(A)')  "S  16"
write(20,'(A)')  "SB 51"
write(20,'(A)')  "SC 21"
write(20,'(A)')  "SE 34"
write(20,'(A)')  "SI 14"
write(20,'(A)')  "SM 62"
write(20,'(A)')  "SN 50"
write(20,'(A)')  "SR 38"
write(20,'(A)')  "TA 73"
write(20,'(A)')  "TB 65"
write(20,'(A)')  "TC 43"
write(20,'(A)')  "TE 52"
write(20,'(A)')  "TH 90"
write(20,'(A)')  "TI 22"
write(20,'(A)')  "TL 81"
write(20,'(A)')  "TM 69"
write(20,'(A)')  "U  92"
write(20,'(A)')  "V  23"
write(20,'(A)')  "W  74"
write(20,'(A)')  "XE 54"
write(20,'(A)')  "Y  39"
write(20,'(A)')  "YB 70"
write(20,'(A)')  "ZN 30"
write(20,'(A)')  "ZR 40"


WRITE(6,*)'File written to disk: F12LIB.INDEX'
RETURN
END SUBROUTINE WriteF12LibIndex

!
!
!


!C+++
!C	SUBROUTINE	READLIB
!C
!C	PURPOSE		To read f1 and f2 of an element from the indexed 
!C			library F12LIB.FULL generated by the program GENLIB.FOR
!C
!C---

!SUBROUTINE ReadLib (ELEMENT,NZ,ATWT,C1,C2,ENG,F1,F2)
SUBROUTINE ReadLib (ELE,NZ,ATWT,C1,C2,ENG,F1,F2)

!C
!C DEFINITION OF ARGUMENTS
!C ELEMENT - 2 LETTER ATOMIC SYMBOL (INPUT)
!C NZ      - ATOMIC NUMBER (RETURNED)
!C ATWT    -ATOMIC WEIGHT (RETURNED)
!C C1      MU(BARNS/ATOM)/MU(CM2/GM) (RETURNED)
!C C2	  E*MU(E)/F2                (RETURNED)
!C ENG     - ENERGY ARRAY(EV)  (RETURNED)
!C F1      - F1 ARRAY      (RETURNED)
!C F2      - F2 ARRAY      (RETURNED)
!C

	!DIMENSION ENG(420),F1(420),F2(420)
	!CHARACTER*2	ELEMENT
        character(len=2),intent(in)                 ::  ele
        integer(kind=ski),intent(out)               :: nz
        real(kind=skr)   ,intent(out)               :: atwt,c1,c2
	real(kind=skr),dimension(420),intent(out)   ::  ENG,F1,F2

        character(len=2)                  ::  element

	!!!!!Watch out this is single precision
	real(kind=4),dimension(844)       ::  REALBUF

        character(len=80),dimension(92)   ::  LIST
        integer(kind=ski),dimension(92)   ::  LOOKUP
        integer(kind=ski)                 ::  POS, R
        integer(kind=ski)                 ::  iFlag,i,fUnit,iErr
	character(len=1024) :: F12LIB, INDEXF

	!DIMENSION REALBUF(844)
        !CHARACTER*2     ELE

!C       NEW ADDITIONS FOR BINARY SEARCH
        !CHARACTER*80 LIST(92)
        !character(len=80),dimension(92) ::  LIST
        !INTEGER LOOKUP(92)
!C
!C This remembers if this routines already been visited at least once. Then
!C Skip a few of the environment management steps.
!C
!C	LOGICAL		LVISITED
!C	COMMON		/ RDLIBCM /	LVISITED
!C	DATA		LVISITED	/ .FALSE. /
!C
	!CHARACTER*132	F12LIB, INDEXF
!C
!C	LVISITED = .TRUE.
!C
!C If user specified 'HH' according to old version, change it to 'H ' instead.
!C
        element = ele
	IF (ELEMENT(1:1).EQ.ELEMENT(2:2))	ELEMENT(2:2)	= ' '

!C
!C OPEN TkE LIBRARY FILE
!C
!C Get the data file path using either SHADOW$DATA or Unix SHADOW_DATA_DIR
!C environment variable. Also, check for existence in the routine itself.
!C
	IFLAG = 1
	CALL DATAPATH ('F12LIB.INDEX', INDEXF, IFLAG)
	IF (IFLAG .NE. 0) THEN
            print *,'File F12LIB.INDEX not fount. I create it!'
            CALL WriteF12LibIndex
	    !CALL LEAVE ('READLIB', 'F12LIB.INDEX not found', 1)
	ENDIF
	IFLAG = 1
	CALL DATAPATH ('F12LIB.FULL', F12LIB, IFLAG)
	IF (IFLAG .NE. 0) THEN
            print *,' '
            print *,'File F12LIB.FULL not fount. I CANNOT create it!'
            print *,'Please copy this file from old SHADOW distribution (data dir)'
            print *,'or download it from the SHADOW distribution website. '
            print *,' '
	    CALL LEAVE ('READLIB', 'F12LIB.FULL not found', 1)
	ENDIF

!C OPEN AND READ THE FILE STORING CHEMICAL SYMBOLS OF ELEMENTS
        fUnit=11
	!open (unit=fUnit, file=INDEXF) !, status = 'OLD') !, ACTION='READ')
	open (unit=fUnit, file='F12FULL.INDEX', status = 'OLD', iostat=iErr )
        IF (iErr /= 0) THEN 
          STOP 'File not found: F12FULL.INDEX'
        ENDIF

5	format (A2, I15)
	read (11, 5) (LIST(I), LOOKUP(I), I = 1, 92)

        CLOSE (11)

!#ifndef vms
!	OPEN(UNIT=71,FILE=F12LIB,
!     $	ACCESS='DIRECT',RECL=3376, STATUS = 'OLD')
!
!C OPEN AND READ THE FILE STORING CHEMICAL SYMBOLS OF ELEMENTS
!	open (unit=11, file=INDEXF, status = 'OLD')
!#else
	OPEN(UNIT=71,FILE="F12LIB.FULL", ioStat=iErr, &
           ACCESS='DIRECT',RECL=3376, STATUS = 'OLD', ACTION='READ')
!	OPEN(UNIT=71,FILE=F12LIB, &
!           !ACCESS='DIRECT',RECL=3376, STATUS = 'OLD', 'readonly')

        IF (iErr /= 0) THEN 
          STOP 'File not found: F12FULL.FULL'
        ENDIF


!C
!C First read the energy array. It is assumed that the first record is  
!C the energy array.
!C
	READ	(71,REC=1)	 REALBUF
	DO 11 I = 1, 420
	  ENG(I)	= REALBUF(I)
11 	CONTINUE
!C
!C READ THE DATA OUT OF THE LIBRARY FILE
!C DATA IS ASSUMED TO BE SORTED IN ORDER OF ENTERIES IN 'FILES' IN LIBRARY FILE
!C
        CALL BINARY(LIST, 1, 92, ELEMENT, POS)
!C	READ	(71, REC= (LOOKUP(POS)+1)) REALBUF
	R = LOOKUP(POS) + 1
	READ	(71, REC= R) REALBUF
!C
!C PUT THE DATA INTO THE SUBROUTINE ARGUMENTS
!C
	NZ=REALBUF(1)
	ATWT=REALBUF(2)
	C1=REALBUF(3)
	C2=REALBUF(4)
!C
!C F1 ARRAY
!C
	DO 20 I=1, 420
	F1(I)=REALBUF(I+4)
   20	CONTINUE
!C F2 ARRAY
	DO 30 I=1, 420
	F2(I)=REALBUF(I+424)
   30	CONTINUE
	CLOSE (71)

	RETURN

END SUBROUTINE ReadLib


!
!
!
SUBROUTINE Binary(data, lb, ub, item, loc)

!C   Here data is a sorted array with lower bound lb and upper bound ub,
!C   and item is a given item of information.  The variables beg, fin and
!C   mid denote, respectively, the beginning, end and middle locations
!C   of a segment of elements of data.  This algorithm finds the location
!C   loc of item in data or set loc = null.
	 integer lb, ub, loc, mid, beg, fin, i
	 character*(*) item
	 character*(*) data(ub)

	 beg = lb
	 fin = ub
	 mid = (beg+fin)/2
30	 do 10 i = 1, 10
!C   Check if search successful
	  if (item .EQ. data(mid)) then
	     go to 20
!C   If search unsuccessful, find new range
	  else if ( item  .LT. data(mid) ) then
	       fin = mid - 1
	  else
	       beg = mid + 1
	  endif

	  mid = (beg+fin)/2

!C   Check if termination condition reached
	  if (beg .LE. fin) then
	     go to 30
	  else
	     go to 40
	  endif

10       continue

!C   Successful search, return the location of element
20       loc = mid
	 return
!C   Unsuccessful search, return NULL for location of element
40	 loc = 0
	 return
END SUBROUTINE Binary

!***********************************************************************
!*	program 	PREREFL					      *
!*								      *
!*	F.Cerrina,	SRC - June 1983				      *
!*			Modified July 1986
!*								      *
!* This program is based on the compilation of atomic scattering factors
!* of B.Henke, Low Energy X-Ray Diagnostics - 1981 and formulaes therein
!* The photon energy range is 30-100000 eV, for all elements.	      *
!*
!* This program will generate a file of the right structure for Shadow *
!* The output will be an array in energy				      *
!*								      *
!C	Link with READLIB.OBJ
!***********************************************************************
SUBROUTINE PREREFL
     	!IMPLICIT	REAL*8	(A-H,O-Z)
        implicit real(kind=skr) (a-h,o-z)
        implicit integer(kind=ski)        (i-n)
!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!	INCLUDE		'SHADOW$INC:DIM.PAR/LIST'
!#endif
	real(kind=skr),dimension(420)   :: RF1,RF2,ENERGY
	real(kind=skr),dimension(420,2) :: OUTFIL
	real(kind=skr) :: DENSITY
	!CHARACTER*80	OUT_FILE,RSTRING
	character(len=1024) :: OUT_FILE
	!DIMENSION	AF1(N_DIM),AF2(N_DIM)
	integer(kind=ski),parameter     :: N_DIM=10000
	real(kind=skr),dimension(N_DIM) :: AF1,AF2
	EQUIVALENCE	(OUTFIL(1,1),RF1(1))	
	EQUIVALENCE	(OUTFIL(1,2),RF2(1))	
     	DATA	PI     	/  3.141592653589793238462643D0 /
     	DATA	PIHALF 	/  1.570796326794896619231322D0 /
     	DATA	TWOPI 	/  6.283185307179586476925287D0 /
     	DATA	TODEG 	/ 57.295779513082320876798155D0 /
     	DATA	TORAD	/  0.017453292519943295769237D0 /
	DATA	TOCM	/  1.239852D-4		     /
	DATA	TOANGS 	/  1.239852D+4		     /

     	RADIUS	=   2.817939D-13
	I_TYPE	= IRINT ('Element [0] or compound [1] ? ')
     	DENSITY	= RNUMBER('Density [ g/cm3 ] ?')
	IF (I_TYPE.EQ.0) THEN
	  CALL	OPT_ELE	(RF1,RF2,ENERGY,DENSITY)
	ELSE IF (I_TYPE.EQ.1) THEN
	  CALL	OPT_COM	(RF1,RF2,ENERGY,DENSITY)
	ELSE 
	  STOP	'Error !  Invalid response.'
	END IF
     	 WRITE(6,*) 'Enter starting photon energy,end and step'
     	READ(*,*) ESTART,EFINAL,ESTEP
     	OUT_FILE	= RSTRING('Output file : ')

     	QMIN	=   ESTART/TOCM*TWOPI
     	QMAX	=   EFINAL/TOCM*TWOPI
     	QSTEP	=   ESTEP/TOCM*TWOPI
     	NPOINT	=  (EFINAL-ESTART)/ESTEP + 1
	DEPTH0	=   DENSITY/2.0D0
	IF (NPOINT.GT.N_DIM) STOP	'Too many points (*N_DIM* max.)'
!** Computes the ALPHA and gamma coefficients.
!*** Finds and interpolate for the photon energy.
!#ifdef vms
!     	OPEN (20,FILE=OUT_FILE,STATUS='NEW',FORM='UNFORMATTED')
!#else
     	OPEN (20,FILE=OUT_FILE,STATUS='UNKNOWN',FORM='UNFORMATTED')
	REWIND (20)
!#endif
     	WRITE (20)	QMIN,QMAX,QSTEP,DEPTH0
     	WRITE (20)	NPOINT
	ELFACTOR	= LOG10(1.0D4/30.0D0)/300.0D0
     	DO 11 I=1,NPOINT
     	PHOTON	=   ESTART + (I-1)*ESTEP
	NENER	=   LOG10(PHOTON/30.0E0)/ELFACTOR + 1
     	F1	=   OUTFIL(NENER,1) +  &
      	(OUTFIL(NENER+1,1) - OUTFIL(NENER,1))*(PHOTON - ENERGY(NENER))/ &
      	(ENERGY(NENER+1) - ENERGY(NENER))
     	F2	=   OUTFIL(NENER,2) +  &
      	(OUTFIL(NENER+1,2) - OUTFIL(NENER,2))*(PHOTON - ENERGY(NENER))/ &
      	(ENERGY(NENER+1) - ENERGY(NENER))
!*** Computes now ALPHA and gamma
     	WAVE	=   TOCM/PHOTON
     	ALPHA	=   RADIUS/PI*(WAVE**2)*F1
     	GAMMA	=   RADIUS/PI*(WAVE**2)*F2
     	AF1(I)	=   ALPHA
     	AF2(I)  =   GAMMA
11     	CONTINUE
     	WRITE (20)	(AF1(I),I=1,NPOINT)
     	WRITE (20)	(AF2(I),I=1,NPOINT)
     	CLOSE (20)
     	!CALL	EXIT (0)
	RETURN
END SUBROUTINE PREREFL


!
! Multilayers
!




!C+++
!C	SUBROUTINE	OPTPROPCOMP
!C
!C---
SUBROUTINE OptPropComp	
	implicit none
        !REAL *8 	RNUMBER
        REAL(kind=skr)  :: DENSITY
     	!DIMENSION	ENERGY(420),IREL(5),AT(5)
	real(kind=skr),dimension(420) :: ENERGY
	integer(kind=ski),dimension(5):: IREL
	real(kind=skr),dimension(5)   :: AT

     	!INTEGER *4 	NUMBER
	integer(kind=ski)   :: NUMBER
	!CHARACTER*1	ELEMENT(5,2),ELE(2)
	character(len=2)                :: ELE
	character(len=2),dimension(5)   :: ELEMENT
     	!COMMON	/OPCON	/  ISTART,IFINAL
     	!COMMON	/DAT	/  ENER(420),DELTA(420),BETA(420)
     	!DIMENSION	OUTFIL11(420),OUTFIL12(420)
     	!DIMENSION	OUTFIL21(420),OUTFIL22(420)
     	!DIMENSION	OUTFIL31(420),OUTFIL32(420)
     	!DIMENSION	OUTFIL41(420),OUTFIL42(420)
     	!DIMENSION	OUTFIL51(420),OUTFIL52(420)
     	real(kind=skr),dimension(420) ::OUTFIL11,OUTFIL12
     	real(kind=skr),dimension(420) ::OUTFIL21,OUTFIL22
     	real(kind=skr),dimension(420) ::OUTFIL31,OUTFIL32
     	real(kind=skr),dimension(420) ::OUTFIL41,OUTFIL42
     	real(kind=skr),dimension(420) ::OUTFIL51,OUTFIL52

     	real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
     	real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     

	real(kind=skr)    :: RADIUS,AVOG,RMOL,RMOLEC,RMu,AtWt,Emf
	real(kind=skr)    :: at1,at2,at3,at4,at5,f1,f2,wave,alpha,gamma
	integer(kind=ski) :: I,J,K,IDK,NATOMS

     	RADIUS	=   2.817939E-13
     	AVOG	=   6.022098E+23

    	DENSITY = RNUMBER('Density [ g/cm3 ] ? ')
     	DO 12 I = 1,5
     	  IREL(I) = 0
12     	CONTINUE
     	NATOMS = IRINT('How many atomic species (up to 5) ? ')
     	WRITE(6,*)'H2O would be: HH,2 and OO,1. Then ? '
     	DO 13 I = 1,NATOMS
     	WRITE(6,*)'Enter 2-letters (capitalized) atomic symbol and ',&
      'formula index for : ',I
	!!READ	(5,2)	(ELEMENT(I,K),K=1,2)
	READ	(5,2)	ELEMENT(I)
!!2	FORMAT	(2A1)
2	FORMAT	(A2)
     	READ(5,*)IREL(I)
13     	CONTINUE
!** Get the data. F1 and F2 are then 'averaged' together
     	RMOL	= 0.0
	IDK	=22
     	GO TO (55,44,33,22,11)	NATOMS
!11	ELE(1)	= ELEMENT(5,1)
!	ELE(2)	= ELEMENT(5,2)
11	ELE	= ELEMENT(5)
	CALL READLIB(ELE,NUMBER,ATWT,RMU,EMF,ENERGY,OUTFIL51,OUTFIL52)
     	RMOL	=          ATWT*IREL(5)
!22	ELE(1)	= ELEMENT(4,1)
!	ELE(2)	= ELEMENT(4,2)
22	ELE	= ELEMENT(4)
	CALL READLIB(ELE,NUMBER,ATWT,RMU,EMF,ENERGY,OUTFIL41,OUTFIL42)
     	RMOL	= RMOL+          ATWT*IREL(4)
!33	ELE(1)	= ELEMENT(3,1)
!	ELE(2)	= ELEMENT(3,2)
33	ELE	= ELEMENT(3)
	CALL READLIB(ELE,NUMBER,ATWT,RMU,EMF,ENERGY,OUTFIL31,OUTFIL32)
     	RMOL	= RMOL+         ATWT*IREL(3)
!44	ELE(1)	= ELEMENT(2,1)
!	ELE(2)	= ELEMENT(2,2)
44	ELE	= ELEMENT(2)
	CALL READLIB(ELE,NUMBER,ATWT,RMU,EMF,ENERGY,OUTFIL21,OUTFIL22)
     	RMOL	= RMOL+         ATWT*IREL(2)
!55	ELE(1)	= ELEMENT(1,1)
!	ELE(2)	= ELEMENT(1,2)
55	ELE	= ELEMENT(1)
	CALL READLIB(ELE,NUMBER,ATWT,RMU,EMF,ENERGY,OUTFIL11,OUTFIL12)
     	RMOL	= RMOL+         ATWT*IREL(1)
     	WRITE (6,1110)
     	DO 14 I=1,NATOMS
     	!WRITE (6,1111) ELEMENT(I,1),ELEMENT(I,2),IREL(I)
     	WRITE (6,1111) ELEMENT(I),IREL(I)
14     	CONTINUE
1110	FORMAT (1X,'Formula:  ',$)
!1111	FORMAT ('+',A1,A1,'(',I2,') ',$)
1111	FORMAT ('+',A2,'(',I2,') ',$)
     	WRITE (6,*)
!** Computes atomic concentrations and molecular weigth
     	RMOLEC	=   DENSITY/RMOL*AVOG	! This is the number of molecules
     	WRITE (6,*) &
      'Molecular weight is : ',RMOL,' Number of molecules/cm3: ',RMOLEC
     	AT1	=   RMOLEC*irel(1)
     	AT(1)	=   AT1
     	AT2	=   RMOLEC*irel(2)
     	AT(2)	=   AT2
     	AT3	=   RMOLEC*irel(3)
     	AT(3)	=   AT3
     	AT4	=   RMOLEC*irel(4)
     	AT(4)	=   AT4
     	AT5	=   RMOLEC*irel(5)
     	AT(5)	=   AT5
     	WRITE (6,*) 'Atoms/Cm3 of each species:'
     	!WRITE (6,1112) (ELEMENT(I,1),ELEMENT(I,2),I=1,NATOMS)
     	WRITE (6,1112) (ELEMENT(I),I=1,NATOMS)
     	WRITE (6,1113) (AT(I),I=1,NATOMS)
1112	FORMAT (5X,A1,A1,T20,A1,A1,T35,A1,A1,T50,A1,A1,T65,A1,A1)
1113	FORMAT (1X,G12.5,T15,G12.5,T30,G12.5,T45,G12.5,T60,G12.5)
     	WRITE (6,*) ' Density ',DENSITY,' g/cm3'
!** Computes now the effective F1 and F2
	J = 0
     	DO 16 I = ISTART,IFINAL
     	F1	=   0.0
     	F2	=   0.0
     	GO TO (155,144,133,122,111)	NATOMS
111	F1	=        OUTFIL51(I)*AT5
     	F2	=        OUTFIL52(I)*AT5
122	F1	=   F1 + OUTFIL41(I)*AT4
     	F2	=   F2 + OUTFIL42(I)*AT4
133	F1	=   F1 + OUTFIL31(I)*AT3
     	F2	=   F2 + OUTFIL32(I)*AT3
144	F1	=   F1 + OUTFIL21(I)*AT2
     	F2	=   F2 + OUTFIL22(I)*AT2
155	F1	=   F1 + OUTFIL11(I)*AT1
     	F2	=   F2 + OUTFIL12(I)*AT1
!*** Computes now ALPHA and gamma
	J = J + 1
     	WAVE	=   TOCM/ENERGY(I)
     	ALPHA	=   RADIUS/PI*(WAVE**2)*F1
     	GAMMA	=   RADIUS/PI*(WAVE**2)*F2
	ENER(J)	=   ENERGY(I)
	DELTA(J) =  ALPHA/2
	BETA(J) =   GAMMA/2
16     	CONTINUE
	RETURN
END SUBROUTINE OptPropComp

!C+++
!C	PROGRAM		PRE_MLAYER
!C
!C	Purpose		To prepare a multilayers file for input to SHADOW.
!C
!C---
SUBROUTINE Pre_Mlayer
	implicit none

     	character(len=1024) :: FILEOUT,FGRADE
	real(kind=skr),dimension(1001)  :: THICK,GAMMA
        real(kind=skr) :: ESTART, EFINAL 
	real(kind=skr)    :: ElFactor
	integer(kind=ski)    :: np,i,j,npair,iGrade
        !REAL *8 RNUMBER
     	!COMMON	/OPCON	/  ISTART,IFINAL
     	!COMMON	/DAT	/  ENER(420),DELTA(420),BETA(420)

	FILEOUT	= RSTRING ('Name of output file : ')
10     	ESTART = RNUMBER ('Photon energy (eV) from : ')
	IF (ESTART.LT. DBLE(30)) THEN
	  WRITE(6,*)'Minimum energy is 30 eV.  Try again.'
	  GO TO 10
	END IF
11     	EFINAL = RNUMBER ('                     to : ')
	IF (EFINAL.GT.100000) THEN
	  WRITE(6,*)'Maximum energy is 100,000 eV.  Try again.'
	  GO TO 11
	END IF
	ELFACTOR = LOG10(1.0E4/30.0)/300.0
	ISTART = LOG10(ESTART/30.0E0)/ELFACTOR + 1
	IFINAL = LOG10(EFINAL/30.0E0)/ELFACTOR + 2
	NP = IFINAL - ISTART + 1
	OPEN	(20,FILE=FILEOUT,STATUS='UNKNOWN')
	REWIND (20)
	WRITE	(20,*)	NP
!C
!C Get the refractive index of the substrate.
!C
	WRITE(6,*)'***************************************************'
	WRITE(6,*)'Specify the substrate material :'
	CALL	OptPropComp
	WRITE	(20,*)	(ENER(I), I = 1, NP)
	DO 19 I = 1, NP
 19	    WRITE	(20,*)	DELTA(I), BETA(I)
!C
!C Get the refractive index of the even layer.
!C
	WRITE(6,*)'***************************************************'
	WRITE(6,*)'Right above the substrate is the even layer material'
	WRITE(6,*)'Specify the even layer material :'
	CALL	OptPropComp
	DO 29 I = 1, NP
 29	    WRITE	(20,*)	DELTA(I), BETA(I)
!C
!C Get the refractive index of the odd layer.
!C
	WRITE(6,*)'***************************************************'
	WRITE(6,*)'Odd layer material is on top of the even layer.'
	WRITE(6,*)'Specify the odd layer material :'
	CALL	OptPropComp
	DO 39 I = 1, NP
 39    	    WRITE	(20,*)	DELTA(I), BETA(I)
	WRITE(6,*)'***************************************************'
	NPAIR	= IRINT ('No. of layer pairs : ')
	WRITE	(20,*)	NPAIR
   	WRITE(6,*) ' '
   	WRITE(6,*)  &
      'Starting from the substrate surface, specify the thickness t :'
   	WRITE(6,*) '      t = t(odd) + t(even)        in Angstroms,'
   	WRITE(6,*) 'and the gamma ratio :'
   	WRITE(6,*) '      t(even) / (t(odd) + t(even))'
   	WRITE(6,*) 'for EACH layer pairs.'
   	WRITE(6,*) ' '
   	WRITE(6,*)  &
      'Type two -1 whenever you want the remaining layers ', &
      'to assume the thickness and gamma ratio of the previous one.'
   	WRITE(6,*) ' '
   	DO 219 I = 1, NPAIR
   	  WRITE(6,*) 't and gamma ratio of layer ',I,' :'
   	  READ(5,*) THICK(I),GAMMA(I)
   	  IF (THICK(I).EQ.-1.AND.GAMMA(I).EQ.-1) THEN
   	    DO 319 J = I, NPAIR
   	      THICK(J) = THICK(I-1)
   	      GAMMA(J) = GAMMA(I-1)
319   	    CONTINUE
   	    GO TO 15
   	  END IF
219   	CONTINUE
15	DO 119 I = 1, NPAIR
   	  WRITE	(20,*)	THICK(I),GAMMA(I)
119   	CONTINUE
   
   	WRITE(6,*) '***************************************************'
   	IGRADE	= IYES('Is t and/or gamma graded over the surface ? ')
   	WRITE(6,*) ' '
   	WRITE	(20,*)	IGRADE
   	IF (IGRADE.EQ.1) THEN
   	WRITE(6,*)  &
      'Then GRADE_MLAYER should be run to generate the spline ', &
      'coefficients for the t and gamma factors over the surface.', &
      '  Here just type in the file name that WILL be used to store',  &
      ' the spline coefficients :'
   	FGRADE	= RSTRING(' ')
   	WRITE	(20,*)	FGRADE
   	END IF
   	CLOSE 	(20)
END SUBROUTINE Pre_Mlayer

!C+++
!C	PROGRAM		GRADE_MLAYER
!C
!C	PURPOSE		To compute bi-cubic splines fitting a user
!C			supplied multilayer.  The spline fitting is adapted
!C			directly from PRESURFACE.FOR.
!C
!C	INPUT		Files (2) in the format:
!C				Nx, Ny
!C				Y(1),...,Y(Ny)
!C				X(1),F(1,1),...,F(1,Ny)
!C				.....
!C				X(Nx),F(Nx,1),...,F(Nx,Ny)
!C			where F(i,j) will be the thickness factor for the first 
!C			file, and the gamma factor for the second file.
!C
!C	Output		A file containing the spline array prepared by
!C			IMSL function IBCCCU
!C---
SUBROUTINE Grade_Mlayer
     	!IMPLICIT	REAL*8	(A-H,O-Z)
        implicit real(kind=skr) (a-h,o-z)
        implicit integer(kind=ski)        (i-n)

     	!CHARACTER*80	INFILE,OUTFILE,RSTRING
     	character(len=1024) :: INFILE,OUTFILE
     	!DIMENSION	CSPL (2,101,2,101), WK (20602)
     	!DIMENSION	X(101),Y(101),F(101,101)
     	real(kind=skr),dimension(2,101,2,101) :: CSPL 
     	real(kind=skr),dimension(20602)       :: WK 
     	real(kind=skr),dimension(101)         :: X,Y
     	real(kind=skr),dimension(101,101)     :: F

	WRITE(6,*) 'For a graded multilayer, you will specify the '
        WRITE(6,*) 'factor MULTIPLYING the thickness'
     	WRITE(6,*) 't and the gamma ratio, across the optical surface.'
     	WRITE(6,*) 'In PRE_MLAYER, you have already defined the nominal'
        WRITE(6,*) ' t and gamma AT THE ORIGIN.  Here you will specify'
	WRITE(6,*) 'their multiplication factor F(x,y).  For non-graded'
	WRITE(6,*) 'multilayer, F(x,y) = 1.0.'
	WRITE(6,*) '***************************************************'

	ITER	= 1
     	OUTFILE	=   RSTRING ('Enter file-name for output: ')
!#ifdef vms
!     	OPEN (21, FILE=OUTFILE, STATUS='NEW', FORM='UNFORMATTED')
!#else
     	OPEN (21, FILE=OUTFILE, STATUS='UNKNOWN', FORM='UNFORMATTED')
	REWIND (21)
!#endif

15	WRITE(6,*) ' '
	IF (ITER.EQ.1) THEN
     	  INFILE = RSTRING ('File containing the t factor mesh : ')
	ELSE
	  INFILE = RSTRING ('File containing the gamma factor mesh : ')
	END IF

!#ifdef vms
!     	OPEN	(20, FILE=INFILE, STATUS='OLD', READONLY, IOSTAT = IERR)
!#else
     	OPEN	(20, FILE=INFILE, STATUS='OLD', IOSTAT = IERR)
!#endif

     	IF (IERR.NE.0) THEN
     	  WRITE(6,*) 'Cannot access ',INFILE
     	  IWHAT = IYES ('Retry ? ')
     	 IF (IWHAT.EQ.1) GOTO 15
     	  CALL EXIT
     	END IF
     	READ (20,*) NX, NY
!     	IF (NX.GT.101.OR.NY.GT.101) THEN
!     	  WRITE(6,*) 'Arrays too large. Maximum allowed is 101 points.'
!     	  STOP 'Please retry with smaller arrays.'
!     	END IF
     	IF (NX.LT.4.OR.NY.LT.4) THEN
     	  WRITE(6,*) 'Not enough points to define arrays. Must be at', &
      		' least 4 points in each direction.'
     	  STOP 'Please retry with larger arrays.'
     	END IF
     	WRITE(6,*) 'Setting up ',NX,' by ',NY,' array.'
!C
!C Reads in Y array
!C
     	READ (20,*) (Y(I),I=1,NY)
!C
!C Now read X and F arrays
!C
     	DO I=1,NX
     	  READ (20,*) X(I),(F(I,J),J=1,NY)
     	END DO
     	CLOSE (20)
     	WRITE(6,*) 'Array read correctly. Compute spline.'
!C
!C Call IMSL routine to compute spline
!C
     	CALL	IBCCCU ( F, X, NX, Y, NY, CSPL, 101, WK, IER)
     	IF (IER.EQ.132) THEN
     	  WRITE(6,*) 'The X and/or Y array are not ordered properly.',  &
       'Please check data in '//trim(INFILE)
     	 CALL EXIT
     	END IF
     	WRITE(6,*) 'Spline succesfully completed.'
     	 WRITE (21)	NX, NY
     	 WRITE (21)	X,Y
	 DO 299 I = 1, NX
	 DO 299 J = 1, NY
     	 WRITE (21)	CSPL(1,I,1,J), CSPL(1,I,2,J),  &
      		CSPL(2,I,1,J), CSPL(2,I,2,J)
299	 CONTINUE
!C     	 WRITE (21)	(((CSPL(1,I,1,J), CSPL(1,I,2,J), 
!C     $		CSPL(2,I,1,J), CSPL(2,I,2,J)), J = 1,NY), I = 1,NX)

	IF (ITER.EQ.1) THEN
	  ITER   = 2
	  GO TO 15
	END IF
	CLOSE	(21)
     	WRITE(6,*) 'Task completed. Spline stored in ',OUTFILE
     	!CALL	EXIT
     	RETURN
END SUBROUTINE Grade_Mlayer

!
! crystals
!



!todo: mv to math
!******************************************************************************
!C+++
!C	SUBROUTINE	POLY_2
!C
!C	PURPOSE		INPUT	3 pairs of data points (x,y)
!C
!C			OUTPUT	coefficients of a second degree polynomial
!C				which passes through the 3 points (x,y)
!C---
SUBROUTINE POLY_2 (X,Y,A,IFLAG)
	!REAL*8		X(3),Y(3),A(3),C1,C2,C3
	real(kind=skr),dimension(3) :: X,Y,A
	real(kind=skr)              :: C1,C2,C3
        integer(kind=ski)           :: IFLAG

	IF (X(1).EQ.X(2).OR.X(1).EQ.X(3).OR.X(2).EQ.X(3)) THEN
	  IFLAG	= -1
	  RETURN
	END IF
	C1	= Y(1)/(X(1) - X(2))/(X(1) - X(3))
	C2	= Y(2)/(X(2) - X(3))/(X(2) - X(1))
	C3	= Y(3)/(X(3) - X(1))/(X(3) - X(2))
!C
!C Quadratic term
!C
	A(3)	= C1 + C2 + C3
!C
!C Linear term
!C
	A(2)	= -C1*(X(2)+X(3)) - C2*(X(3)+X(1)) - C3*(X(1)+X(2))
!C
!C Constant term
!C
	A(1)	= C1*X(2)*X(3) + C2*X(3)*X(1) + C3*X(1)*X(2)
	IFLAG	= 1
	RETURN
END SUBROUTINE POLY_2


!C+++
!C	PROGRAM		BRAGG
!C
!C	PURPOSE		To generate the reflectivity of a Bragg
!C			reflection for an ideal crystal, in a format
!C			to be used by SHADOW.
!C
!C	ALGORITHM	Uses formulaes from Zachariasen, Warren,
!C			and Handbook of SR books.
!C
!C---
SUBROUTINE BRAGG

     	!IMPLICIT	REAL*8	(A-H,O-Z)
        implicit real(kind=skr) (a-h,o-z)
        implicit integer(kind=ski)        (i-n)

     	real(kind=skr),parameter ::PI=3.141592653589793238462643D0
     	real(kind=skr),parameter ::PIHALF=1.570796326794896619231322D0 
     	real(kind=skr),parameter ::TWOPI=6.283185307179586467925287D0 
     	real(kind=skr),parameter ::TODEG=57.295779513082320876798155D0 
     	real(kind=skr),parameter ::TORAD=0.017453292519943295769237D0 
	real(kind=skr),parameter ::TOCM=1.239852D-4	    
	real(kind=skr),parameter ::TOANGS=1.239852D+4
     	real(kind=skr),parameter ::E2_MC2=2.817939D-13
	real(kind=skr),parameter ::AVOG=6.022098D+23

     	!CHARACTER *80	OUTFIL,RSTRING
     	character(len=1024) :: OUTFIL
	COMPLEX*16	CI,FA,FB,STRUCT,F_0,REFRAC
	COMPLEX*16	RCS1_O,RCP1_O,RCS2_O,RCP2_O,RCS_O,RCP_O
	COMPLEX*16	RCS1_H,RCP1_H,RCS2_H,RCP2_H,RCS_H,RCP_H
	COMPLEX*16	SSVAR,SPVAR,QS,QP,QS1,QP1,QS2,QP2
	COMPLEX*16	GA,GA_BAR,GB,GB_BAR,FH,FH_BAR
	complex*16	psi_h,psi_hbar,psi_0,psi_conj
	complex*16	cry_q,cry_z,ctemp
	complex*16	br_c1,br_c2,br_delta1,br_delta2,br_x1,br_x2
	complex*16	br_ref

     	real(kind=skr),dimension(3)   :: M_REF,XA,YA,CA,XB,YB,CB
	real(kind=skr),dimension(420) :: ENERGY,FP_A,FPP_A,FP_B,FPP_B
	real(kind=skr)                :: ATWT,RMU,EMF
	character(len=2)              :: ELEA,ELEB
	integer(kind=ski)             :: ATNUM_A,ATNUM_B

	CI	= (0.0D0,1.0D0)
!C
!C Inquires about the cell geometry
!C
	WRITE(6,*)  &
      	'All crystal structures are refered to a cubic unit cell.'
     	WRITE(6,*) 'Bravais lattice type : '
     	WRITE(6,*) '0	for 	ZincBlende'
     	WRITE(6,*) '1	for 	Rocksalt'
     	WRITE(6,*) '2	for	simple FCC'
	WRITE(6,*) '3       for     CsCl structure'
        WRITE(6,*) 'hexagonal Bravais lattice type: '
        WRITE(6,*) '4       for     Hexagonal Close-Packed structure'
        WRITE(6,*) '5       for     Hexagonal Graphite structure'
        I_LATT  = IRINT ('Then ? ')
        IF ((I_LATT.EQ.4).OR.(I_LATT.EQ.5) ) THEN
         D_LATT_A = RNUMBER('Lattice constant a (Angs) ? ')
         D_LATT_C = RNUMBER('Lattice constant c (Angs) ? ')
         D_LATT_A = D_LATT_A*1.0D-8
         D_LATT_C = D_LATT_C*1.0D-8
        ELSE
         D_LATT = RNUMBER('Lattice constant (Angs) ? ')
         D_LATT = D_LATT*1.0D-8
        END IF
!C
!C Computes # of unit cell per cm^3
!C
        IF ((I_LATT.EQ.4).OR.(I_LATT.EQ.5) ) THEN
        R_NATOM = 1.0D0/(D_LATT_C*SQRT(3.0D0/4.0D0)*D_LATT_A**2)
        ELSE
        R_NATOM =  D_LATT**(-3)
        END IF
!C
!C inquires about reflection indeces
!C
     	WRITE(6,*) 'Index of crystal plane of reflection H,K,L : '
     	READ(*,*) M_REF(1),M_REF(2),M_REF(3)
!C
!C Computes lattice planes spacing
!C
     	RMILL	=  M_REF(1)**2 + M_REF(2)**2 + M_REF(3)**2
        IF ((I_LATT.EQ.4).OR.(I_LATT.EQ.5)) THEN
        SP_HKL  =  1.0D0/SQRT(4.0D0/3.0D0*( &
      (M_REF(1)**2+M_REF(2)**2+M_REF(1)*M_REF(2))/D_LATT_A**2 &
      + 3.0D0/4.0D0*M_REF(3)**2/D_LATT_C**2 ))
        ELSE
        SP_HKL  =  D_LATT/SQRT(RMILL)
        END IF
!C
     	IF (I_LATT.EQ.0) THEN
	  WRITE(6,*)  &
      	  '***********************************************************'
	  WRITE(6,*)  &
      	  'The ZINCBLENDE structure is defined by atom A located at '
	  WRITE(6,*)  &
      	  '(0,0,0) and atom B at (1/4,1/4,1/4) of the fcc lattice.'
	  WRITE(6,*)  &
      	  '***********************************************************'
	  ELEA	= RSTRING &
      		('Enter atomic symbol (capitalized) for atom A : ')
	  ELEB	= RSTRING &
      		('Enter atomic symbol (capitalized) for atom B : ')
	ELSE IF (I_LATT.EQ.1) THEN
	  WRITE(6,*)  &
      	  '***********************************************************'
	  WRITE(6,*)  &
      	  'The ROCKSALT structure is defined by atom A located at '
	  WRITE(6,*)  &
      	  '(0,0,0) and atom B at (1/2,1/2,1/2) of the fcc lattice.'
	  WRITE(6,*)  &
      	  '***********************************************************'
	  ELEA	= RSTRING &
      		('Enter atomic symbol (capitalized) for atom A : ')
	  ELEB	= RSTRING &
      		('Enter atomic symbol (capitalized) for atom B : ')
	ELSE IF (I_LATT.EQ.2) THEN
	  WRITE(6,*)  &
      	  '***********************************************************'
	  WRITE(6,*) 'Simple FCC structure'
	  WRITE(6,*)  &
      	  '***********************************************************'
	  ELEA	= RSTRING &
      	    ('Enter atomic symbol (capitalized) for the basis atom : ')
	ELSE IF (I_LATT.EQ.3) THEN
	  WRITE(6,*)  &
      	  '***********************************************************'
	  WRITE(6,*)  &
          'The CsCl structure is defined by atom A located at '
	  WRITE(6,*)  &
      	  '(0,0,0) and atom B at (1/2,1/2,1/2) of the cubic lattice.'
	  WRITE(6,*) 'If atom A = B, then it is a bcc lattice.'
	  WRITE(6,*)  &
      	  '***********************************************************'
	  ELEA	= RSTRING &
      		('Enter atomic symbol (capitalized) for atom A : ')
	  ELEB	= RSTRING &
      		('Enter atomic symbol (capitalized) for atom B : ')
        ELSE IF (I_LATT.EQ.4) THEN
	  WRITE(6,*)  &
      	  '***********************************************************'
          WRITE(6,*)  &
          'The HEXAGONAL CLOSED-PACKED structure is defined by', &
          'an atom A located at (1/3,2/3,1/4) and (2/3,1/3,3/4) of the prism ', &
          'cell'
	  WRITE(6,*)  &
      	  '**********************************************************'
          ELEA  = RSTRING &
              ('Enter atomic symbol (capitalized) for the basis atom :')
        ELSE IF (I_LATT.EQ.5) THEN
	  WRITE(6,*)  &
      	  '**********************************************************'
          WRITE(6,*) ' GRAPHITE structure '
	  WRITE(6,*)  &
      	  '**********************************************************'
          ELEA  = RSTRING &
              ('Enter atomic symbol (capitalized) for the basis atom :')
     	END IF
!C
!C Inquires about fo for the basis atoms.
!C
	RATIO	= 0.5D0/SP_HKL*1.0D-8
	WRITE(6,*)  &
      	'***********************************************************'
	WRITE(6,*)  &
      	'Atomic scattering factor is defined by fo + f'' + if", where'
	WRITE(6,*)  &
      	'    fo = fo(SIN(theta)/Lambda) is the non-dispersive part'
	WRITE(6,*) '    fp, fpp (Lambda) are the dispersive part.'
	WRITE(6,*)  &
      	'***********************************************************'
	WRITE(6,*)  &
      	'We need fo at 3 different values of SIN(theta)/Lambda, which'
	WRITE(6,*)  &
      	'should cover the range of interest and center around :'
     	WRITE(6,*) 'SIN(theta)/Lambda = ',RATIO,' ratio.'
	WRITE(6,*)  &
      	'***********************************************************'
	WRITE(6,*) 'Please enter 1) SIN(theta)/Lambda, 2) fo ,'
        IF ((I_LATT.NE.2).AND.(I_LATT.NE.4).AND.(I_LATT.NE.5)) THEN
105	  WRITE(6,*) 'For atom A, first set : '
	  READ(*,*) XA(1),YA(1)
	  WRITE(6,*) '          , second set : '
	  READ(*,*) XA(2),YA(2)
	  WRITE(6,*) '          , third set : '
	  READ(*,*) XA(3),YA(3)
	  CALL	POLY_2	(XA,YA,CA,IFLAG)
	  IF (IFLAG.EQ.-1) THEN
	    WRITE(6,*) 'Error in range of input value. Try again.'
	    GO TO 105
	  END IF
106	  WRITE(6,*) 'For atom B, first set : '
	  READ(*,*) XB(1),YB(1)
	  WRITE(6,*) '          , second set : '
	  READ(*,*) XB(2),YB(2)
	  WRITE(6,*) '          , third set : '
	  READ(*,*) XB(3),YB(3)
	  CALL	POLY_2	(XB,YB,CB,IFLAG)
	  IF (IFLAG.EQ.-1) THEN
	    WRITE(6,*) 'Error in range of input value. Try again.'
	    GO TO 106
	  END IF
	ELSE
107	  WRITE(6,*) 'For basis atom, first set : '
	  READ(*,*) XA(1),YA(1)
	  WRITE(6,*) '              , second set : '
	  READ(*,*) XA(2),YA(2)
	  WRITE(6,*) '              , third set : '
	  READ(*,*) XA(3),YA(3)
	  CALL	POLY_2	(XA,YA,CA,IFLAG)
	  IF (IFLAG.EQ.-1) THEN
	    WRITE(6,*) 'Error in range of input value. Try again.'
	    GO TO 107
	  END IF
	  CB(1)	= 0.0D0
	  CB(2)	= 0.0D0
	  CB(3)	= 0.0D0
	END IF
!C
!C defines wavelengths
!C
	WRITE(6,*)  &
      	'***********************************************************'
	WRITE(6,*)  &
      	'f'', f" is furnished from optical constant library within ...'
 	EMIN	= RNUMBER ('minimum photon energy (eV) : ')
 	EMAX	= RNUMBER ('maximum photon energy (eV) : ')
	ESTEP	= RNUMBER ('energy step (eV) : ')
50	NPOINT	= (EMAX - EMIN)/ESTEP + 1
	IF (NPOINT.GT.1000) THEN
	  WRITE(6,*)  &
      	  'Too many points (1000 max).  Please increase step size.'
	  GO TO 50
	END IF
	I_ABSORP = IYES  &
      	    ('Do you want to include crystal absorption [1/0] ? ')
	TEMPER	= RNUMBER ('Temperature (Debye-Waller) factor : ')
!C
!C Compute the geometrical part G's of the structure factor and get f', f"
!C from the optical constant library.
!C
	IF (I_LATT.EQ.0) THEN
	  GA = (1.0D0,0.0D0) + CDEXP(CI*PI*(M_REF(1)+M_REF(2)))  &
      	  		     + CDEXP(CI*PI*(M_REF(1)+M_REF(3)))  &
      			     + CDEXP(CI*PI*(M_REF(2)+M_REF(3)))
	  GB = GA * CDEXP(CI*PIHALF*(M_REF(1)+M_REF(2)+M_REF(3)))
	  CALL	READLIB(ELEA,ATNUM_A,ATWT,RMU,EMF,ENERGY,FP_A,FPP_A)
	  CALL	READLIB(ELEB,ATNUM_B,ATWT,RMU,EMF,ENERGY,FP_B,FPP_B)
	ELSE IF (I_LATT.EQ.1) THEN
	  GA = (1.0D0,0.0D0) + CDEXP(CI*PI*(M_REF(1)+M_REF(2)))  &
      			     + CDEXP(CI*PI*(M_REF(1)+M_REF(3)))  &
      			     + CDEXP(CI*PI*(M_REF(2)+M_REF(3)))
	  GB = GA * CDEXP(CI*PI*(M_REF(1)+M_REF(2)+M_REF(3)))
	  CALL	READLIB(ELEA,ATNUM_A,ATWT,RMU,EMF,ENERGY,FP_A,FPP_A)
	  CALL	READLIB(ELEB,ATNUM_B,ATWT,RMU,EMF,ENERGY,FP_B,FPP_B)
	ELSE IF (I_LATT.EQ.2) THEN
	  GA = (1.0D0,0.0D0) + CDEXP(CI*PI*(M_REF(1)+M_REF(2)))  &
      			     + CDEXP(CI*PI*(M_REF(1)+M_REF(3)))  &
      			     + CDEXP(CI*PI*(M_REF(2)+M_REF(3)))
	  GB = (0.0D0,0.0D0)
	  CALL	READLIB(ELEA,ATNUM_A,ATWT,RMU,EMF,ENERGY,FP_A,FPP_A)
	ELSE IF (I_LATT.EQ.3) THEN
	  GA = (1.0D0,0.0D0) 
	  GB = CDEXP(CI*PI*(M_REF(1)+M_REF(2)+M_REF(3)))
	  CALL	READLIB(ELEA,ATNUM_A,ATWT,RMU,EMF,ENERGY,FP_A,FPP_A)
	  CALL	READLIB(ELEB,ATNUM_B,ATWT,RMU,EMF,ENERGY,FP_B,FPP_B)
        ELSE IF (I_LATT.EQ.4) THEN
          GA = CDEXP(CI*TWOPI*((1.0D0/3.0D0)*M_REF(1)+ &
                    (2.0D0/3.0D0)*M_REF(2)+(1.0D0/4.0D0)*M_REF(3)))+ &
        CDEXP(CI*TWOPI*((2.0D0/3.0D0)*M_REF(1)+(1.0D0/3.0D0)*M_REF(2)+ &
                    (3.0D0/4.0D0)*M_REF(3)))
          GB = (0.0D0,0.0D0)
          CALL  READLIB(ELEA,ATNUM_A,ATWT,RMU,EMF,ENERGY,FP_A,FPP_A)
        ELSE IF (I_LATT.EQ.5) THEN
          GA=(1.0d0,0.0d0) + CDEXP(CI*PI*M_REF(3)) + &
       CDEXP(CI*TWOPI*((1.0D0/3.0D0)*M_REF(1)+(2.0D0/3.0D0)*M_REF(2))) + &
       CDEXP(CI*TWOPI*((2.0D0/3.0D0)*M_REF(1)+(1.0D0/3.0D0)*M_REF(2)+ &
       (1.0D0/2.0D0)*M_REF(3)))
          GB = (0.0D0,0.0D0)
          CALL  READLIB(ELEA,ATNUM_A,ATWT,RMU,EMF,ENERGY,FP_A,FPP_A)
	END IF
	GA_BAR	= CONJG(GA)
	GB_BAR	= CONJG(GB)
	RN	= E2_MC2*R_NATOM
!C
!C Crystal absorption.
!C
	IF (I_ABSORP.EQ.0) THEN
	  DO 199 I = 1,420
	    FPP_A(I)	= 0.0
	    FPP_B(I)	= 0.0
 199	  CONTINUE
	END IF
!C
!C Now prepare the file for SHADOW.
!C
	OUTFIL	= RSTRING ('Output file name (for SHADOW) : ')
!#ifdef vms
!	OPEN	(25,FILE=OUTFIL,STATUS='NEW',FORM='FORMATTED')
!#else
	OPEN	(25,FILE=OUTFIL,STATUS='UNKNOWN',FORM='FORMATTED')
	REWIND (25)
!#endif
	WRITE	(25,*)	I_LATT,RN,SP_HKL
	WRITE	(25,*)	ATNUM_A,ATNUM_B,TEMPER
	WRITE	(25,*)	GA
	WRITE   (25,*)  GA_BAR
	WRITE	(25,*)  GB
	WRITE	(25,*)  GB_BAR
	WRITE	(25,*)	CA(1),CA(2),CA(3)
	WRITE   (25,*)  CB(1),CB(2),CB(3)
	WRITE	(25,*)	NPOINT

	ELFACTOR	= LOG10(1.0E4/30.0)/300.0
	DO 299 I = 1, NPOINT
	  PHOT	= EMIN + (I-1)*ESTEP
	  NENER	= LOG10(PHOT/30.0)/ELFACTOR + 1
	  F1A	= FP_A(NENER) + (FP_A(NENER+1) - FP_A(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	  F2A	= FPP_A(NENER) + (FPP_A(NENER+1) - FPP_A(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	  F1B	= FP_B(NENER) + (FP_B(NENER+1) - FP_B(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	  F2B	= FPP_B(NENER) + (FPP_B(NENER+1) - FPP_B(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	  WRITE	(25,*)	PHOT, F1A-ATNUM_A, F2A 
	  WRITE (25,*)  F1B-ATNUM_B, F2B
 299	  CONTINUE
	CLOSE	(25)
!C
!C Rocking curve for immediate check.
!C
	I_ROCK	=  &
      	    IYES ('Do you want to generate a rocking curve [1/0] ? ')
	IF (I_ROCK.EQ.0) go to 20
	write (6,*) 'What do you want to calculate ?: '
	write (6,*)  &
      	    '[1] Diffracted beam in Transmission (Laue) geometry '
	write (6,*)  &
      	    '[2] Diffracted beam in Reflection (Bragg) geometry  '
	write (6,*) '[3] like [2] using thick crystal approximation'
!Cc	write (6,*) '[4] Transmitted beam in Laue case'
!Cc	write (6,*) '[5] Transmitted beam in Bragg case'
	i_mode = irint(' <?> ') 
30	PHOT	= RNUMBER ('... at what energy (eV) ? ')
	NENER	= LOG10(PHOT/30.0)/ELFACTOR + 1
	F1A	= FP_A(NENER) + (FP_A(NENER+1) - FP_A(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	F2A	= FPP_A(NENER) + (FPP_A(NENER+1) - FPP_A(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	F1B	= FP_B(NENER) + (FP_B(NENER+1) - FP_B(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	F2B	= FPP_B(NENER) + (FPP_B(NENER+1) - FPP_B(NENER)) *  &
      	    (PHOT - ENERGY(NENER)) / (ENERGY(NENER+1) - ENERGY(NENER))
	FOA	= CA(3)*RATIO**2 + CA(2)*RATIO + CA(1)
	FOB	= CB(3)*RATIO**2 + CB(2)*RATIO + CB(1)
	FA	= FOA + (F1A - ATNUM_A) + CI*F2A
	FB	= FOB + (F1B - ATNUM_B) + CI*F2B
	R_LAM0 	= TOCM/PHOT
     	SIN_GRA	= R_LAM0/SP_HKL/2
     	GRAZE	= TODEG*(ASIN(SIN_GRA))
!C
!C Compute the absorption coefficient and Fo.
!C
	IF (I_LATT.EQ.0) THEN
	  ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*(DIMAG(FA)+DIMAG(FB)))
     	  F_0 = 4*((F1A + F1B) + CI*(F2A + F2B))
	ELSE IF (I_LATT.EQ.1) THEN
	  ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*(DIMAG(FA)+DIMAG(FB)))
     	  F_0 = 4*((F1A + F1B) + CI*(F2A + F2B))
	ELSE IF (I_LATT.EQ.2) THEN
	  FB	 = (0.0D0,0.0D0)
	  ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*DIMAG(FA))
     	  F_0 = 4*(F1A + CI*F2A)
	ELSE IF (I_LATT.EQ.3) THEN
	  ABSORP = 2.0D0*RN*R_LAM0*(DIMAG(FA)+DIMAG(FB))
     	  F_0 = (F1A + F1B) + CI*(F2A + F2B)
        ELSE IF (I_LATT.EQ.4) THEN
          FB     = (0.0D0,0.0D0)
          ABSORP = 2.0D0*RN*R_LAM0*(2.0D0*(DIMAG(FA)))
          F_0 = 2*(F1A+ CI*F2A )
        ELSE IF (I_LATT.EQ.5) THEN
          FB     = (0.0D0,0.0D0)
          ABSORP = 2.0D0*RN*R_LAM0*(4.0D0*(DIMAG(FA)))
          F_0 = 4*(F1A + CI*F2A )
	END IF
!C	
!C FH and FH_BAR are the structure factors for (h,k,l) and (-h,-k,-l).
!C
	FH 	= (GA * FA) + (GB * FB)
	FH_BAR	= (GA_BAR * FA) + (GB_BAR * FB)
	STRUCT 	= SQRT(FH * FH_BAR) 
!C
!C computes refractive index.
!C
     	REFRAC = (1.0D0,0.0D0) - R_LAM0**2*RN*F_0/TWOPI
	DeltaBragg  = 1.0D0 - DREAL(REFRAC)
	BetaBragg   = -DIMAG(REFRAC)
!C
!C THETA_B is the Bragg angle corrected for refraction
!C
	THETA_B = R_LAM0/(1 - (DeltaBragg/SIN_GRA**2))/2.0D0/SP_HKL
	THETA_B = TODEG*(ASIN(THETA_B))
	WRITE(6,*) ' '
     	WRITE(6,*) 'So far, we are working with:'
     	WRITE(6,*) 'Lambda 		 = ',R_LAM0*1.0D8,' Angstroms'
     	WRITE(6,*) 'Theta (graz) 	 = ',GRAZE,' degrees'
	WRITE(6,*) 'Bragg angle      = ',THETA_B,' degrees'
	WRITE(6,*) 'Structure factor  = ',STRUCT
	WRITE(6,*) 'Refraction index = 1 - delta - i*beta :'
	WRITE(6,*) '           delta = ',DeltaBragg
	WRITE(6,*) '            beta = ',BETABragg
	WRITE(6,*) 'Absorption coeff = ',ABSORP,' cm-1'
     	WRITE(6,*) ' '
!C
!C S%VAR is the variable "s" of C BVAR is the variable "b" of Warren.
!C
	SSVAR	= RN*(R_LAM0**2)*STRUCT*TEMPER/PI/SIN(TORAD*2.0D0*GRAZE)
	SPVAR	= SSVAR*ABS(COS(TORAD*2.0D0*GRAZE))
	SSR	= DREAL(SSVAR)
	SPR	= DREAL(SPVAR)
	BVAR	= ABSORP*R_LAM0/TWOPI/SIN(TORAD*2.0D0*GRAZE)
!C
!C Inquires about mosaic crystal calculation
!C
        i_mosaic = irint('Do you want to calculate a mosaic crystal ?')
        if (i_mosaic.eq.1) then
         spread  = RNUMBER('mosaic angle spread (FWHM) [deg] ? ')
         spread = torad*spread/2.35d0
        else
         i_mosaic = 0
!C        thick    = 0.0d0
         spread   = 0.0d0
        end if
!C
!C Inquires crystal Thickness
!C
	if (i_mode.ne.3) then
	  thick = RNUMBER('thickness of the crystal [cm] ? ')
	endif
!C
!C Inquires about asymmetrical diffraction
!C
	IF (I_MOSAIC.NE.1) THEN
     	  WRITE(6,*) 'Asymmetric cut angle (deg) between face ', &
      'and bragg planes (CW)= '
	  READ(*,*) A_BRAGG 
	if (a_bragg.eq.0.0) then
	  i_asym = 0
	else 
	  i_asym = 1
	endif
	ENDIF
!C
!C <<calculation for perfect crystal>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!C
	IF (I_MOSAIC.NE.1) THEN
!C
!C Define the angles
!C
	if (i_mode.eq.5.or.i_mode.eq.3.or.i_mode.eq.2) then !bragg
	   THETA_O	= GRAZE + A_BRAGG
	   THETA_H	= GRAZE - A_BRAGG
	else if (i_mode.eq.4.or.i_mode.eq.1) then           !laue
	   theta_o	= abs( - graze + a_bragg )
	   theta_h	= abs(   graze + a_bragg )
	endif
	SIN_O	= SIN(TORAD*THETA_O)
	SIN_H	= SIN(TORAD*THETA_H)
	COS_A   = COS(TORAD*A_BRAGG)
	SIN_G   = SIN(TORAD*GRAZE)
	COS_G   = COS(TORAD*GRAZE)
	ASS_FAC	= abs(SIN_O/SIN_H)
	SQR_FAC = SQRT(ASS_FAC)
	if (i_mode.eq.2.or.i_mode.eq.3.or.i_mode.eq.5)  &
            ass_fac = -1.0d0*ass_fac
!C
!C Define the q variables of Warren, useful lfor calculating the RC width.
!C
	QS	= RN*R_LAM0*STRUCT*TEMPER*SP_HKL
	QS1	= QS*COS_A/SIN_O
	QS2	= QS*COS_A/SIN_H
	QP	= QS*ABS(COS(TORAD*2.0D0*GRAZE))
	QP1	= QP*COS_A/SIN_O
	QP2	= QP*COS_A/SIN_H

	SSS	= DREAL(SSVAR)*1.0D6
	PPP	= DREAL(SPVAR)*1.0D6

	IF (I_ASYM.NE.1) THEN
!#ifdef vms
!	 OPEN	(20,FILE='ROCK_CURVE.S',STATUS='NEW')
!	 OPEN	(21,FILE='ROCK_CURVE.P',STATUS='NEW')
!#else
	 OPEN	(20,FILE='rock_curve.s',STATUS='UNKNOWN')
	 REWIND (20)
	 OPEN	(21,FILE='rock_curve.p',STATUS='UNKNOWN')
	 REWIND (21)
!#endif
	 WRITE(6,*) '1/2 width of Rock Curve  s-pol  =  : ', &
               	    SSS,' microradians'
	 WRITE(6,*) '1/2 width of Rock Curve  p-pol  =  : ', &
               	    PPP,' microradians'
	ELSE IF (I_ASYM.EQ.1) THEN
!#ifdef vms
!	 OPEN	(20,FILE='ROCK_CURVE_INC.S',STATUS='NEW')
!	 OPEN	(21,FILE='ROCK_CURVE_INC.P',STATUS='NEW')
!	 OPEN	(27,FILE='ROCK_CURVE_REF.S',STATUS='NEW')
!	 OPEN	(28,FILE='ROCK_CURVE_REF.P',STATUS='NEW')
!#else
	 OPEN	(20,FILE='rock_curve_inc.s',STATUS='UNKNOWN')
	 REWIND (20)
	 OPEN	(21,FILE='rock_curve_inc.p',STATUS='UNKNOWN')
	 REWIND (21)
	 OPEN	(27,FILE='rock_curve_ref.s',STATUS='UNKNOWN')
	 REWIND (27)
	 OPEN	(28,FILE='rock_curve_ref.p',STATUS='UNKNOWN')
	 REWIND (28)
!#endif
	 WRITE(6,*)  &
      	 'The width of the Rock Curve in function of incident angle is'
	 WRITE(6,*)  &
      	 '1/2 width for s-pol  =  : ',SSS/SQR_FAC,' microradians'
	 WRITE(6,*)  &
      	 '1/2 width for p-pol  =  : ',PPP/SQR_FAC,' microradians'
	 WRITE(6,*)  &
      	 'The width of the Rock Curve in function of reflected angle is'
	 WRITE(6,*)  &
      	 '1/2 width for s-pol  =  : ',SSS*SQR_FAC,' microradians'
	 WRITE(6,*)  &
      	 '1/2 width for p-pol  =  : ',PPP*SQR_FAC,' microradians'
	 WRITE(6,*) 'Asymmetric factor b=  : ',ass_fac
	END IF
	WRITE(6,*) '    '
!C
!C
!C
	EPRANGE	= RNUMBER ('+/- how many microradians : ')
	EPRANGE	= EPRANGE * 1.0D-6
	NPOINT	= IRINT ('How many points : ')
	ESTEP	= 2.0D0*EPRANGE/(NPOINT-1)
!C
!C Calculation of corrected angles for asymmetrical diffraction
!C following Handbook of SR
!C
	THETA_INC = (THETA_B - GRAZE)	
	THETA_INC_O = 0.5D0*(1.0D0-1.0D0/ASS_FAC)*THETA_INC
	THETA_INC_H = 0.5D0*(1.0D0-ASS_FAC)*THETA_INC
	if (i_mode.eq.5.or.i_mode.eq.3.or.i_mode.eq.2) then 
	   THETA_B_O	= GRAZE + A_BRAGG + THETA_INC_O
	   THETA_B_H	= GRAZE - A_BRAGG + THETA_INC_H
	else if (i_mode.eq.4.or.i_mode.eq.1) then
	   theta_b_o	= abs( - graze + a_bragg - theta_inc_o)
	   theta_b_h	= abs(   graze + a_bragg + theta_inc_h)
	endif
!C
!C RCS, RCP are the complex reflection coefficient for s- and p- component.
!C The two reflectivities are written out.
!C
!C
!C Inquire about units
!C
	MUL_FAC = 1.0D0
	I_SEC = IYES ('Do you want to use sec [1/0] ? ')
	IF (I_SEC.EQ.1) MUL_FAC=180.0d0*3600.0d0/PI
	I_DES = IYES ('Do you want the R.C. centered [1/0] ? ')
	DES_FAC_O=THETA_INC_O*TORAD
	DES_FAC_H=THETA_INC_H*TORAD
!C
!C Begins loop along the angular points
!C
	DO 399 I = 1, NPOINT
	  EP	= (I-1)*ESTEP - EPRANGE
!C
!C New crystal formulation Aug 92 G.J.Chen , M. S. del Rio Nov 92.
!C
	if (i_mode.ne.3) then
	
!C
!C definition of sin_q_ang, sin_q_ref and sin_brg to be consistent
!C with the CRYSTAL module.
!C
	sin_brg = -1.0d0*dsin(graze*torad-ep)
!C
!C sin_q_ang is the sine of the angle between the incident ray and
!C the crystal surface: 
!C Laue case: q_ang = [90 - (ep+graze+90-a_bragg) =>
!C sin_q_ang = -sin(ep+graze-a_bragg)
!C Bragg case: q_ang = graze + a_bragg
!C the EP is not critical
!C
	if (i_mode.eq.1.or.i_mode.eq.4) then                ! laue
	  sin_q_ang =-1.0d0*dsin(ep+(graze-a_bragg)*torad) 
	else if (i_mode.eq.2.or.i_mode.eq.5) then           ! bragg
	  sin_q_ang = dsin(ep+(graze+a_bragg)*torad)
	endif
!C
!C sin_q_ref is the sine of the angle between the outcoming ray and
!C the crystal surface: q_ref = ep' + graze + a_bragg  =>
!C
	if (i_mode.eq.1.or.i_mode.eq.4) then              ! laue
	  sin_q_ref = -1.0D0*dsin(torad*(graze+a_bragg))
	else if (i_mode.eq.2.or.i_mode.eq.5) then         ! bragg
	  sin_q_ref = dsin(torad*(graze-a_bragg))
	endif
!C
!C gamma_0 is the director cosine between the incident ray and the
!C surface normal: cos (180 - q_ang) = cos (180 - [graze + 90 - a_bragg])
!C = sin (graze-a_bragg) = -sin_a_ang
!C gamma_h = cos (90+a_bragg+graze)=-sin(graze+a_bragg)=sin_g_ref
!C
	gamma_0 = -1.0d0*sin_q_ang
	gamma_h = sin_q_ref
	cry_b = gamma_0/gamma_h
	d_spacing = sp_hkl
	cry_t0 = thick
	cry_t = 0.5d0*(1.d0/abs(gamma_0) +1.d0/abs(gamma_h))*cry_t0 
	cry_a = pi/r_lam0*(cry_t0/gamma_0)
	cry_alpha = ((r_lam0/d_spacing)**2+2*r_lam0* &
      sin_brg/d_spacing)

	psi_h = rn*r_lam0**2/pi*fh
	psi_hbar = rn*r_lam0**2/pi*fh_bar
	psi_0 = rn*r_lam0**2/pi*f_0
	psi_conj = rn*r_lam0**2/pi*dconjg(fh)

	cry_q = cry_b*psi_h*psi_hbar
	cry_z = (1.d0-cry_b)*0.5d0*psi_0 + cry_b*0.5d0*cry_alpha

!C
!C	new formulation based on formulae [3.137] (laue) and
!C	[3.130] (bragg) of Zachariasen's book. MSR Nov 1992.
!C	We allow also to calculate the transmitted beam in both
!C	laue case [3.131] and bragg case [3.138] (i_mode 4 and 5
!C	respectively)
!C

!C
!C s-polarization
!C
	ctemp = cdsqrt(cry_q  + cry_z**2)
	br_x1 = (-1.0d0*cry_z+ctemp)/psi_hbar
	br_x2 = (-1.0d0*cry_z-ctemp)/psi_hbar
	br_delta1 = 0.5d0*(psi_0-cry_z+ctemp)
	br_delta2 = 0.5d0*(psi_0-cry_z-ctemp)
	br_c1 = -1.0d0*ci*thick*twopi/(-1.0d0*abs(gamma_0)) / &
      						r_lam0*br_delta1
	br_c2 = -1.0d0*ci*thick*twopi/(-1.0d0*abs(gamma_0)) / &
      						r_lam0*br_delta2
!C
!C a very big exponential produces numerical overflow. If so, the value
!C is changed artificially to avoid the overflow. This is equivalent to 
!C use the thick crystal approximation
!C
!C 700 -> 100 to reduce overflow on HP.
!C
	if (dreal(br_c1).gt.100.or.dreal(br_c2).gt.100) then 
	  if (dreal(br_c1).gt.100) br_c1 = 100.0d0+ci*dimag(br_c1)
	  if (dreal(br_c2).gt.100) br_c2 = 100.0d0+ci*dimag(br_c2)
	endif

	br_c1 = cdexp(br_c1)
	br_c2 = cdexp(br_c2)

	if (i_mode.eq.1) then 
	  br_ref = br_x1*br_x2*(br_c1-br_c2)/(br_x2-br_x1)             ! laue
	else if (i_mode.eq.2) then
	  br_ref = br_x1*br_x2*(br_c1-br_c2)/(br_c2*br_x2-br_c1*br_x1) ! bragg
	else if (i_mode.eq.4) then
	  br_ref = (br_x2*br_c1-br_x1*br_c2)/(br_x2-br_x1)             ! laueT
	else if (i_mode.eq.5) then
	  br_ref = br_c1*br_c2*(br_x2-br_x1)/(br_c2*br_x2-br_c1*br_x1) ! braggT
	endif

	r_s = br_ref*dconjg(br_ref)
	if (i_mode.eq.1.or.i_mode.eq.2) r_s = (1.0d0/abs(cry_b))*r_s
!C
!C p-polarization
!C
	c_ppol = abs(cos(torad*2.0d0*graze))

	ctemp = cdsqrt(cry_q*c_ppol**2  + cry_z**2)
	br_x1 = (-1.0d0*cry_z+ctemp)/(psi_hbar*c_ppol)
	br_x2 = (-1.0d0*cry_z-ctemp)/(psi_hbar*c_ppol)
	br_delta1 = 0.5d0*(psi_0-cry_z+ctemp)
	br_delta2 = 0.5d0*(psi_0-cry_z-ctemp)
	br_c1 = -1.0d0*ci*thick*twopi/(-1.0d0*abs(gamma_0)) /  &
      						r_lam0*br_delta1
	br_c2 = -1.0d0*ci*thick*twopi/(-1.0d0*abs(gamma_0)) /  &
      						r_lam0*br_delta2
!C
!C a very big exponential produces numerical overflow. If so the value
!C is changed to avoid the overflow. This is equivalent to the thick
!C crystal approximation
!C
!C 700 -> 100 to reduce overflow on HP.
!C
	if (dreal(br_c1).gt.100.or.dreal(br_c2).gt.100) then 
	  if (dreal(br_c1).gt.100) br_c1 = 100.0d0+ci*dimag(br_c1)
	  if (dreal(br_c2).gt.100) br_c2 = 100.0d0+ci*dimag(br_c2)
	endif

	br_c1 = cdexp(br_c1)
	br_c2 = cdexp(br_c2)

	if (i_mode.eq.1) then
	  br_ref = br_x1*br_x2*(br_c1-br_c2)/(br_x2-br_x1)             ! laue
	else if (i_mode.eq.2) then
	  br_ref = br_x1*br_x2*(br_c1-br_c2)/(br_c2*br_x2-br_c1*br_x1) ! bragg
	else if (i_mode.eq.4) then
	  br_ref = (br_x2*br_c1-br_x1*br_c2)/(br_x2-br_x1)             ! laueT
	else if (i_mode.eq.5) then
	  br_ref = br_c1*br_c2*(br_x2-br_x1)/(br_c2*br_x2-br_c1*br_x1) ! braggT
	endif

	r_p = br_ref*dconjg(br_ref)
	if (i_mode.eq.1.or.i_mode.eq.2) r_p = (1.0d0/abs(cry_b))*r_p



	if (i_des.eq.1) then                         !centered
	  WRITE	(20,*)	(EP-des_fac_o)*MUL_FAC,r_s
	  WRITE	(21,*)	(EP-des_fac_o)*MUL_FAC,r_p
	else                                         !not centered
	  WRITE	(20,*)	EP*MUL_FAC,r_s
	  WRITE	(21,*)	EP*MUL_FAC,r_p
	endif
!C
!C if asymmetric, the exit rock curve
!C
	if (i_asym.eq.1) then
	   epp = (ep-des_fac_o)*abs(cry_b)
	   if (i_des.ne.1) epp = epp + des_fac_h
	   WRITE  (27,*)  EPP*MUL_FAC,r_s
 	   WRITE  (28,*)  EPP*MUL_FAC,r_p
	endif
!C
!C OLD BRAGG CASE ===============================================
!C Main calculation (symmetrical case and asym incident case)
!C
	else if (i_mode.eq.3) then
!C
!C Variable h (for absorption)
!C
	HH	= ABSORP*SP_HKL/2.0D0
	SIN_OO	= 1.0D0/SIN_O
	SIN_HH	= 1.0D0/SIN_H
	H	= 0.5D0*HH*COS_A*(SIN_OO+SIN_HH)
!C
	  UVE_O	= PI*SP_HKL*COS_G*(1.0D0-ASS_FAC)*EP/R_LAM0
	  RCS1_O= CI*QS2/(H+CI*UVE_O+SQRT(QS1*QS2+(H+CI*UVE_O)**2))
	  RCP1_O= CI*QP2/(H+CI*UVE_O+SQRT(QP1*QP2+(H+CI*UVE_O)**2))
	  RCS2_O= CI*QS2/(H+CI*UVE_O-SQRT(QS1*QS2+(H+CI*UVE_O)**2))
	  RCP2_O= CI*QP2/(H+CI*UVE_O-SQRT(QP1*QP2+(H+CI*UVE_O)**2))
!C
	  IF (((CDABS(RCS1_O))**2).LE.-1.d0*ASS_FAC) THEN
	    RCS_O	= RCS1_O/SQR_FAC
	  ELSE 
	    RCS_O	= RCS2_O/SQR_FAC
	  END IF
	  RCS_O	= RCS_O*SQRT(FH/FH_BAR)
	  IF (((CDABS(RCP1_O))**2).LE.-1.d0*ASS_FAC) THEN
	    RCP_O	= RCP1_O/SQR_FAC
	  ELSE 
	    RCP_O	= RCP2_O/SQR_FAC
	  END IF
	  RCP_O	= RCP_O*SQRT(FH/FH_BAR)
!C
	  IF (GRAZE.GT.45) 	RCP_O = -RCP_O
	if (i_des.eq.1) then                         !centered
	  WRITE	(20,*)	EP*MUL_FAC,(CDABS(RCS_O))**2
	  WRITE	(21,*)	EP*MUL_FAC,(CDABS(RCP_O))**2
	else                                         !not centered
	  WRITE	(20,*)	(EP+DES_FAC_O)*MUL_FAC,(CDABS(RCS_O))**2
	  WRITE	(21,*)	(EP+DES_FAC_O)*MUL_FAC,(CDABS(RCP_O))**2
	endif
!C
!C Only for asymmetrical case (the reflected R.C)
!C
	IF (I_ASYM.EQ.1) THEN
	 UVE_H	= PI*SP_HKL*COS_G*(1.0D0-1.0D0/ASS_FAC)*EP/R_LAM0
	 RCS1_H= CI*QS2/(H+CI*UVE_H+SQRT(QS1*QS2+(H+CI*UVE_H)**2))
	 RCP1_H= CI*QP2/(H+CI*UVE_H+SQRT(QP1*QP2+(H+CI*UVE_H)**2))
	 RCS2_H= CI*QS2/(H+CI*UVE_H-SQRT(QS1*QS2+(H+CI*UVE_H)**2))
	 RCP2_H= CI*QP2/(H+CI*UVE_H-SQRT(QP1*QP2+(H+CI*UVE_H)**2))
!C
	  IF (((CDABS(RCS1_H))**2).LE.-1.d0*ASS_FAC) THEN
	    RCS_H	= RCS1_H/SQR_FAC
	  ELSE 
	    RCS_H	= RCS2_H/SQR_FAC
	  END IF
	 RCS_H	= RCS_H*SQRT(FH/FH_BAR)
	  IF (((CDABS(RCP1_H))**2).LE.-1.d0*ASS_FAC) THEN
	    RCP_H	= RCP1_H/SQR_FAC
	  ELSE 
	    RCP_H	= RCP2_H/SQR_FAC
	  END IF
!C
	 RCP_H	= RCP_H*SQRT(FH/FH_BAR)
	  IF (GRAZE.GT.45) 	RCP_H = -RCP_H
!C
	if (i_des.eq.1) then                         !centered
	 WRITE	(27,*)	EP*MUL_FAC,(CDABS(RCS_H))**2
 	 WRITE	(28,*)	EP*MUL_FAC,(CDABS(RCP_H))**2
	else                                         !not centered
	 WRITE	(27,*)	(EP+DES_FAC_H)*MUL_FAC,(CDABS(RCS_H))**2
 	 WRITE	(28,*)	(EP+DES_FAC_H)*MUL_FAC,(CDABS(RCP_H))**2
	endif
	END IF
	endif
 399    CONTINUE
!C
!C <<calculation for mosaic crystal>>
!C
        ELSE if (i_mosaic.eq.1) then
!C
!#ifdef vms
!	 OPEN	(20,FILE='ROCK_CURVE.S',STATUS='NEW')
!	 OPEN	(21,FILE='ROCK_CURVE.P',STATUS='NEW')
!#else
	 OPEN	(20,FILE='rock_curve.s',STATUS='UNKNOWN')
	 REWIND (20)
	 OPEN	(21,FILE='rock_curve.p',STATUS='UNKNOWN')
	 REWIND (21)
!#endif
        qs_mosaic = (sin(torad*2.0d0*graze))*(ssr*pi)**2/r_lam0
        qp_mosaic = (sin(torad*2.0d0*graze))*(spr*pi)**2/r_lam0
	if (i_mode.eq.3.or.i_mode.eq.2) then
           a_mosaic = thick*absorp/sin(torad*graze)    !bragg
	else if (i_mode.eq.1) then
           a_mosaic = thick*absorp/cos(torad*graze)    !laue(alpha=90)
	endif

          omega_0      = (1/sqrt(twopi))*(1/spread)
          aas_mosaic_0 = omega_0*qs_mosaic/absorp
          aap_mosaic_0 = omega_0*qp_mosaic/absorp
          refmax_s  = aas_mosaic_0/(1+aas_mosaic_0+ &
      sqrt(1+2*aas_mosaic_0))
          refmax_p  = aap_mosaic_0/(1+aap_mosaic_0+ &
      sqrt(1+2*aap_mosaic_0))
          tmax      = cos(torad*graze)*dexp(1+2*aas_mosaic_0)/2/ &
      aas_mosaic_0/absorp
        texts= r_lam0**2/pi/sin(torad*2*graze)/ssr/2/sp_hkl
        textp= r_lam0**2/pi/sin(torad*2*graze)/spr/2/sp_hkl
        tabs_mosaic = sin(torad*graze)/absorp
	ABSSECS_MOSAIC = OMEGA_0*QS_MOSAIC
	ABSSECP_MOSAIC = OMEGA_0*QP_MOSAIC
	ABSEXTS_MOSAIC = SIN(TORAD*GRAZE)/TEXTS
	ABSEXTP_MOSAIC = SIN(TORAD*GRAZE)/TEXTP
	TSECS_MOSAIC = SIN(TORAD*GRAZE)/ABSSECS_MOSAIC
	TSECP_MOSAIC = SIN(TORAD*GRAZE)/ABSSECP_MOSAIC
        ratio_mosaic = tabs_mosaic/texts
	EPRANGE_mosaic  = 1.5d0*2.35d0*spread
        NPOINT_mosaic   = 200
        ESTEP_mosaic    = 2.0D0*EPRANGE_mosaic/(NPOINT_mosaic-1)

        DO I = 1, NPOINT_mosaic
         EP    = (I-1)*ESTEP_mosaic - EPRANGE_mosaic
         omega = (1/sqrt(twopi))*(1/spread)*dexp(-ep*ep/2/spread/spread)
         aas_mosaic = omega*qs_mosaic/absorp
         aap_mosaic = omega*qp_mosaic/absorp
!C
!C reflection (bragg) case
!C
	if (i_mode.eq.3.or.i_mode.eq.2) then
         rs_mosaic = 1+aas_mosaic+(sqrt(1+2*aas_mosaic))/ &
                     tanh(a_mosaic*sqrt(1+2*aas_mosaic))
         rp_mosaic = 1+aap_mosaic+(sqrt(1+2*aap_mosaic))/ &
                     tanh(a_mosaic*sqrt(1+2*aap_mosaic))
         rs_mosaic = aas_mosaic / rs_mosaic
         rp_mosaic = aap_mosaic / rp_mosaic
!C
!C transmission (laue) case
!C
	else if (i_mode.eq.1) then
	 rs_mosaic = sinh(aas_mosaic*a_mosaic)* &
                     exp(-a_mosaic*(1.0d0+aas_mosaic))
	 rp_mosaic = sinh(aap_mosaic*a_mosaic)* &
                     exp(-a_mosaic*(1.0d0+aap_mosaic))
	endif

         write (20,*) ep,rs_mosaic
         write (21,*) ep,rp_mosaic
        end do
!C <<end of mosaic and perfect calculations>>
        END IF
!C
!C Write out the parameters used.
!C
!#ifdef vms
!	OPEN	(23,FILE='ROCK_CURVE.PAR',STATUS='NEW')
!#else
	OPEN	(23,FILE='rock_curve.par',STATUS='UNKNOWN')
	REWIND (23)
!#endif
	IF (I_LATT.EQ.0) THEN
	  WRITE	(23,*)	'ZincBlende structure :'
	  WRITE	(23,*)	'For atom A, fo + f'' + if" = ',FA
	  WRITE	(23,*)	'         B,               = ',FB
	ELSE IF (I_LATT.EQ.1) THEN
	  WRITE	(23,*)	'Rocksalt structure :'
	  WRITE	(23,*)	'For atom A, fo + f'' + if" = ',FA
	  WRITE	(23,*)	'         B,               = ',FB
	ELSE IF (I_LATT.EQ.2) THEN
	  WRITE	(23,*)	'Simple FCC structure :'
	  WRITE	(23,*)	'For basis atom, fo + f'' + if" = ',FA
	ELSE IF (I_LATT.EQ.3) THEN
	  WRITE	(23,*)	'CsCl structure :'
	  WRITE	(23,*)	'For atom A, fo + f'' + if" = ',FA
	  WRITE	(23,*)	'         B,               = ',FB
        ELSE IF (I_LATT.EQ.4) THEN
          WRITE (23,*)  'Hexagonal close-packed structure :'
          WRITE (23,*)  'For atom A, fo + f'' + if" = ',FA
          WRITE (23,*)  '         B,               = ',FB
        ELSE IF (I_LATT.EQ.5) THEN
          WRITE (23,*)  'Graphite structure :'
          WRITE (23,*)  'For atom basis, fo + f'' + if" = ',FA
        END IF
        IF ((I_LATT.EQ.4).OR.(I_LATT.EQ.5)) THEN
         WRITE (23,*) 'Lattice constant A =',D_LATT_A*1.0D8,' Angstroms'
         WRITE (23,*) 'Lattice constant C =',D_LATT_C*1.0D8,' Angstroms'
        ELSE
         WRITE  (23,*)  'Lattice constant = ',D_LATT*1.0D8,' Angstroms'
        END IF
	WRITE	(23,*)	'crystal thickness = ',thick,' cm'
	WRITE	(23,*)	'd-spacing        = ',SP_HKL*1.0D8,' Angstroms'
	WRITE	(23,*)	'Photon energy  = ',PHOT,' eV'
     	WRITE	(23,*)	'Lambda 		 = ', &
       					R_LAM0*1.0D8, ' Angstroms'
     	WRITE	(23,*)	'SIN(theta)/Lambda = ', &
      					0.5D0/SP_HKL*1.0D-8,' ratio.'
	WRITE	(23,*)	'Refraction index = 1 - delta - i*beta :'
	WRITE	(23,*)	'           delta = ',DeltaBragg
	WRITE	(23,*)	'            beta = ',BetaBragg
	WRITE	(23,*)	'Absorption coeff = ',ABSORP,' cm-1'
	WRITE 	(23,*)  'Temperature factor = ',TEMPER
	WRITE	(23,*)	'Structure factor F(000) = ',F_0
	WRITE	(23,*)	'Structure factor F(hkl) = ',STRUCT
	if (i_mosaic.ne.1) &
        WRITE	(23,*)	'Asymmetric factor b=  : ',ass_fac
        write	(23,*)  'lenght primary extinction (s-pol) =', &
      			textp*1d4,' microns'
        write	(23,*)  'lenght primary extinction (p-pol) =', &
      			texts*1d4,' microns'
	IF (I_ASYM.NE.1) THEN
	 if (i_mosaic.ne.1) then
	 WRITE  (23,*) 'The width of the Rock Curve is'
	 WRITE  (23,*) '1/2 width for s-pol  =  : ',SSS,' microradians'
	 WRITE  (23,*) '1/2 width for p-pol  =  : ',PPP,' microradians'
	 endif
      	 WRITE	(23,*)	'Theta (graz) 	     = ',GRAZE,' degrees'
	 WRITE	(23,*)	'Bragg angle(corr)   = ',THETA_B,' degrees'
	ELSE IF (I_ASYM.EQ.1) THEN
	 WRITE(23,*) ' '
	 WRITE(23,*)  &
      		'Rocking Curve parameters for the symmetrical case:'
	 WRITE(23,*) '1/2 width for s-pol  =  : ',SSS,' microradians'
	 WRITE(23,*) '1/2 width for p-pol  =  : ',PPP,' microradians'
      	 WRITE(23,*) 'Theta (graz) 	     = ',GRAZE,' degrees'
	 WRITE(23,*) 'Bragg angle(corr)   = ',THETA_B,' degrees'
	 WRITE(23,*) ' '
	 WRITE(23,*)'Width of Rock Curve in function of incident angle:'
	 WRITE(23,*)'1/2 width for s-pol = : ',SSS/SQR_FAC,' microrad'
	 WRITE(23,*)'1/2 width for p-pol = : ',PPP/SQR_FAC,' microrad'
	 WRITE(23,*)'Incident Grazing angle  = ',theta_o,' degrees'
	 WRITE(23,*)'Incident corrected angle   = ',THETA_B_O,' degrees'
	 WRITE(23,*) ' '
	 WRITE(23,*) &
      		'Width of Rock Curve in function of reflected angle:'
	 WRITE(23,*)'1/2 width for s-pol = : ',SSS*SQR_FAC,' microrad'
	 WRITE(23,*)'1/2 width for p-pol = : ',PPP*SQR_FAC,' microrad'
	 WRITE(23,*)'Reflected Grazing angle = ',theta_h,' degrees'
	 WRITE(23,*)'Reflected corrected angle  = ',THETA_B_H,' degrees'
	 WRITE(23,*)'Angle between face and bragg planes (CW)=',A_BRAGG, &
              ' degrees'
	 CLOSE	(27)
	 CLOSE	(28)
	END IF
        IF (I_MOSAIC.EQ.1) THEN
         write (23,*) '  '
         write (23,*) '***********  MOSAIC PARAMETERS  ***************'
         write (23,*) '  '
         write (23,*) 'spread= ',2.35d0*spread/TORAD ,'deg fwhm'
         write (23,*) 'true absorp lenght = ',tabs_mosaic*1d4,' microns'
         write (23,*) 'peak thickness = ',tmax,' cm'
         write (23,*) 'For parallel polarization we have: '
         write (23,*) '   Q 	 = ',qs_mosaic,' cm-1 '
         write (23,*) '   lenght secn ext =',tsecs_mosaic*1d4,' microns'
         write (23,*) '   abs coef prim ext =',absexts_mosaic,'cm-1'
         write (23,*) '   abs coef secn ext =',abssecs_mosaic,'cm-1'
         write (23,*) '   peak refl = ',refmax_s
         write (23,*) 'For perpendicular polarization we have: '
         write (23,*) '   Q     = ',qp_mosaic,' cm-1 '
         write (23,*) '   lenght secn ext =',tsecp_mosaic*1d4,' microns'
         write (23,*) '   abs coef prim ext =',absextp_mosaic,'cm-1'
         write (23,*) '   abs coef secn ext =',abssecp_mosaic,'cm-1'
         write (23,*) '   peak refl = ',refmax_p
         write (23,*) '  '
!D        CLOSE  (31)
!D        CLOSE  (32)
        END IF
	CLOSE	(20)
	CLOSE	(21)
	CLOSE	(23)
	I_AGAIN	= IYES ('Do you want to try another energy [1/0] ? ')
	IF (I_AGAIN.EQ.1) GO TO 30
20	CONTINUE
END SUBROUTINE Bragg

!
!
!


End Module shadow_Pre
