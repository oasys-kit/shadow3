C
C ***************************************************************************
C                             CDF_Z
C
C       A program to generate the CDF of the photons for SHADOW by
C	calculating the flux and the degree of polarization at given
C	polar angles (theta,phi) and energy.
C
C       Modified version of URGENT made by F. Zontone.
C
C       Usage: to call in the SHADOW module "MAKE_ID", option 'undulator',
C              as user's program.
C
C	INPUT:  i)  the input file UNDUL_Z.PAR with the undulator
C		    parameters;
C	OUTPUT: i)  the file for SHADOW UPHOT.DAT used for generating
C		    the CDF of the undulator radiation
C
C ***************************************************************************
C
	SUBROUTINE CDF_Z
        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)
	REAL*8 ENER,THETA_S,PHI_S,RNO,POL_DEG
	DIMENSION ENER(51),THETA_S(31,51),PHI_S(31,31,51)
	DIMENSION RNO(31,31,51),POL_DEG(31,31,51)
        INTEGER CODE
        COMMON/INPUT/GAMMA,K3,KX,KY,LAMDAR,LEN,N
        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,NPHI1,NPHI2,
     $  NALPHA,DALPHA,ISG(400)
        COMMON/CONST/F3,APMIN,APMAX,MODE,ICALC,IHARM,IANG
        COMMON/SCREEN/D,XPMIN,DXP,NXP,YPMIN,DYP,NYP,FAC
	COMMON/SCALE/E_MIN,DE
        COMMON/CONVL/H(5001),WMIN,DW,NW
        COMMON/CALC/BRI1(101,101),BRI2(101,101),BRI3(101,101),
     $  BRI0(101,101),RAD1(51,51),RAD2(51,51),RAD3(51,51),
     $  RAD0(51,51),I1(5001),I2(5001),L1,L2,L3,L4,
     $  SPEC1(5001),SPEC2(5001),SPEC3(5001),SPEC0(5001),WK(5001)
        DATA PI/3.141592653589/,IDEBUG/0/
C
C       READ UNDULATOR PARAMETERS FROM A INPUT FILE
C
C
C srio gets input from shadow namelists
C
	call GetFromNml(PERIOD,KX,KY,N,ENERGY,CUR,NPHI,DALPHA)
C	OPEN(21,FILE='UNDUL_Z.PAR',STATUS='OLD')
C	READ(21,*) PERIOD,KX,KY,N,ENERGY,CUR,NPHI,DALPHA
C	CLOSE (21)
C
C	READ THE ANGLES FROM THE SHADOW BINARY FILE
C
	OPEN (10,FILE='uphot.dat',STATUS='UNKNOWN',FORM='UNFORMATTED')
	READ (10) NE_S,NT_S,NP_S
	DO 15 M=1,NE_S
	   READ (10) ENER(M)
15	CONTINUE
	DO 25 M=1,NE_S
	   DO 35 J=1,NT_S
	      READ (10) THETA_S(J,M)
35	   CONTINUE
25	CONTINUE
	DO 45 M=1,NE_S
	   DO 55 J=1,NT_S
	      DO 65 I=1,NP_S
	         READ (10) PHI_S(I,J,M)
65	      CONTINUE
55	   CONTINUE
45	CONTINUE
C
C       CONSTANTS
C
        GAMMA=ENERGY/0.000511
        LAMDAR=PERIOD*1.0D+10/(2.0*GAMMA*GAMMA)
        K2=KX*KX+KY*KY
        K3=1.0+K2/2.0
        LEN=N*PERIOD
        LAMDA1=LAMDAR*K3
        E1=12398.5/LAMDA1
        PTOT=0.0725*N*K2*ENERGY*ENERGY*CUR/PERIOD
        GK=0.0
        IF(KX.LT.1E-3.OR.KY.LT.1E-3)THEN
        IF(KX.LT.1E-3)K=KY
        IF(KY.LT.1E-3)K=KX
        GK=K*((K**6)+(24.*(K**4)/7.)+(4.*K*K)+(16./7.))
     $  /((1.+(K*K))**3.5)
        ENDIF
        IF(ABS(KX-KY).LT.1E-3)THEN
        K=KX
        GK=32.0/7.0*K/(1.0+K*K)**3.0
        ENDIF
        PD=0.1161*(ENERGY**4)*N*K*GK*CUR/PERIOD
C
C       DEFAULT CALCULATION
C
	MODE=2
	ICALC=3
	IHARM=-1
	D=0.0
        XPS=0.0
        YPS=0.0
        NXP=0
        NYP=0
        IF(NPHI.EQ.0)NPHI=20
        IF(NSIG.EQ.0)NSIG=3
        IF(NALPHA.EQ.0)NALPHA=15
        IF(DALPHA.EQ.0.0)DALPHA=2.0
	nomega=0
	write (*,*) '<><> NOMEGA: ',nomega
        IF(NOMEGA.EQ.0)NOMEGA=16
	write (*,*) '<><> NOMEGA: ',nomega
        IF(DOMEGA.EQ.0.0)DOMEGA=2.0
C	
C	ENERGY SCALE
C
	write (*,*) '<><> NOMEGA: ',nomega
	DE=2.0*DOMEGA*E1/(NOMEGA*N)
	write (*,*) '<><> NOMEGA: ',nomega
C
C       ANGULAR (IANG=1) UNITS
C
        IANG=1
	D=1.0
C
C       ERROR CHECK
C
	write (*,*) '<><> NOMEGA: ',nomega
	write (*,*) "NXP,NYP,NPHI,NALPHA,NOMEGA: ",NXP,NYP,NPHI,
     $NALPHA,NOMEGA
        IF(NXP.LT.0.OR.NXP.GT.50.OR.NYP.LT.0.OR.NYP.GT.50)GOTO 900
        IF(NPHI.GT.100.OR.NALPHA.GT.100)GOTO 900
        IF(NOMEGA.GT.5000)GOTO 900
C
C	START CALCULATIONS
C
	WRITE(6,500)
	DO M=1,NE_S
	   write(*,*) 'N of iteration in energy: ',M
	   DO J=1,NT_S
	      DO I=1,NP_S
		 E_MIN=DBLE(ENER(M))
		 TH_ANG=DBLE(THETA_S(J,M))
		 PH_ANG=DBLE(PHI_S(I,J,M))
		 XPC=TH_ANG*DCOS(PH_ANG)*1D+03
		 YPC=TH_ANG*DSIN(PH_ANG)*1D+03
C
C       PINHOLE : DETERMINE MIN. AND MAX. EMISSION ANGLES WITHIN PINHOLE
C
        	 XE=XPC*1.0D-03/D
        	 YE=YPC*1.0D-03/D
        	 APMAX=GAMMA*GAMMA*((XE*XE)+(YE*YE))
        	 APMIN=GAMMA*GAMMA*((XE*XE)+(YE*YE))
        	 IF(XPC.EQ.0.0.AND.YPC.EQ.0.0)THEN
          	 FAC=4.0
          	 XPMIN=0.0
          	 YPMIN=0.0
        	ELSE
          	 FAC=1.0
          	 XPMIN=XPC
          	 YPMIN=YPC
        	 ENDIF
        	NXP=1
        	NYP=1
C
C       SET UP ARRAY OF COSPHI AND SINPHI
C
        	 NPHI1=(4*NPHI)
        	 NPHI2=NPHI+1
        	 INC=NPHI
        	 ISIGN=1
        	 DPHI=2.0*PI/FLOAT(NPHI1)
        	DO 10 IE=1,NPHI1
        	 IF(IE.EQ.1)CODE(IE)=1
        	 IF(IE.NE.1)CODE(IE)=CODE(IE-1)+ISIGN
        	 IF(IE.EQ.(INC+1))ISIGN=-1
        	 IF(IE.EQ.((2*INC)+1))ISIGN=1
        	 IF(IE.EQ.((3*INC)+1))ISIGN=-1
        	 ISG(IE)=ISIGN
        	 PHI=(IE-1)*DPHI
        	 SINPHI(IE)=DSIN(PHI)
        	 COSPHI(IE)=DCOS(PHI)
10      	CONTINUE
C
C       FACTOR TO SCALE FINAL RESULTS
C
        	 F3=4.55D+07*N*N*GAMMA*GAMMA*CUR/(D*D)
C
C       CALL ANALYSIS PROGRAM
C
     		 CALL SUB4
		 RAD0(1,1)=RAD0(1,1)*(1000/E_MIN)*1D06
		 RNO(I,J,M)=REAL(RAD0(1,1))
		 IF(RNO(I,J,M).LT.10.0) RNO(I,J,M)=1.0
		 L1_S=REAL(L1)
		 POL_DEG(I,J,M)=(1+L1_S)/2
      		 END DO
	   END DO
	END DO
	DO 46 M=1,NE_S
	   DO 56 J=1,NT_S
	      DO 66 I=1,NP_S
	         WRITE (10) RNO(I,J,M)
66	      CONTINUE
56	   CONTINUE
46	CONTINUE
	DO 47 M=1,NE_S
	   DO 57 J=1,NT_S
	      DO 67 I=1,NP_S
	         WRITE (10) POL_DEG(I,J,M)
67	      CONTINUE
57	   CONTINUE
47	CONTINUE
	CLOSE (10)
	write(*,*) 'Calculations has been successfully 
     $completed (...maybe...) !!!'
	RETURN
        !STOP
900     WRITE(6,9000)
500     FORMAT('         ******  CDF_Z - VERSION 1.2 - 29/6/94  ******')
9000    FORMAT(//' *** INVALID INPUT PARAMETERS ***')
        END
C
C       ---------------
        SUBROUTINE SUB4
C       ---------------
        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)
        INTEGER CODE
        COMMON/INPUT/GAMMA,K3,KX,KY,LAMDAR,LEN,N
        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,NPHI1,NPHI2,
     $  NALPHA,DALPHA,ISG(400)
        COMMON/CONST/F3,APMIN,APMAX,MODE,ICALC,IHARM,IANG
        COMMON/SCREEN/D,XPMIN,DXP,NXP,YPMIN,DYP,NYP,FAC
	COMMON/SCALE/E_MIN,DE
        COMMON/CONVL/H(5001),WMIN,DW,NW
        COMMON/CALC/BRI1(101,101),BRI2(101,101),BRI3(101,101),
     $  BRI0(101,101),RAD1(51,51),RAD2(51,51),RAD3(51,51),
     $  RAD0(51,51),I1(5001),I2(5001),L1,L2,L3,L4,
     $  SPEC1(5001),SPEC2(5001),SPEC3(5001),SPEC0(5001),WK(5001)
        DATA PI/3.141592653589/,TWOPI/6.283185307/,EPI2/78.95683521/
C
C       ZERO EMITTANCE
C       MODE  = 2 ANGULAR (IANG=1) DENSITY SPECTRUM
C       ICALC = 3
C
C
C       VARY PHOTON ENERGY
C
        PTOT=0.0
	NE=1
        IA=NE
        E=E_MIN
        LAMDA=12398.5/E
        R=LAMDA/LAMDAR
        I1(IA)=0
        I2(IA)=0
        RADMAX=0.0
C
C       VARY POSITION IN PINHOLE
C
        IB=NXP
        XPMM=XPMIN+((IB-1)*DXP)
        XP=XPMM/1000.0
        IC=NYP
        YPMM=YPMIN+((IC-1)*DYP)
        YP=YPMM/1000.0
        RAD1(IB,IC)=0.0
        RAD2(IB,IC)=0.0
        RAD3(IB,IC)=0.0
        RAD0(IB,IC)=0.0
C
C       FIND HARMONIC NO. CONTRIBUTING AT THE
C       GIVEN WAVELENGTH AND PINHOLE POSITION
C
        XE1=XP*GAMMA/D
        YE1=YP*GAMMA/D
        ALP2=(XE1*XE1)+(YE1*YE1)
        I=0
20      I=I+1
        ALP2I=(R*I)-K3
        A2MAX=ALP2I+(DALPHA*R/N)
        IF(A2MAX.LT.ALP2)GOTO 20
        A2MIN=ALP2I-(DALPHA*R/N)
	IF(A2MIN.GT.ALP2) GOTO 30
        IF(I1(IA).EQ.0)I1(IA)=I
        I2(IA)=I
        ALP=DSQRT(ALP2)
        COSPH=0.0
        SINPH=1.0
        IF(ALP.GT.1.0D-06)THEN
        COSPH=XE1/ALP
        SINPH=YE1/ALP
        ENDIF
        CALL BRIGHT(ALP,COSPH,SINPH,KX,KY,I,S0,S1,S2,S3)
        RAD1(IB,IC)=F3*S1*SINC(ALP2,ALP2I,R,N)
        RAD2(IB,IC)=F3*S2*SINC(ALP2,ALP2I,R,N)
        RAD3(IB,IC)=F3*S3*SINC(ALP2,ALP2I,R,N)
        RAD0(IB,IC)=F3*S0*SINC(ALP2,ALP2I,R,N)
C
C       PRINT IRRADIANCE DISTRIBUTION AT THE GIVEN WAVELENGTH
C
        IF(RAD0(IB,IC).NE.0.0)THEN
        L1=RAD1(IB,IC)/RAD0(IB,IC)
        L2=RAD2(IB,IC)/RAD0(IB,IC)
        L3=RAD3(IB,IC)/RAD0(IB,IC)
        L4=SQRT(ABS(1.0-L1*L1-L2*L2-L3*L3))
        ELSE
        L1=0.0
        L2=0.0
        L3=0.0
        L4=0.0
      	ENDIF
30      IF(RAD0(IB,IC).GT.RADMAX)RADMAX=RAD0(IB,IC)
        POWER=1.602D-16*RAD0(1,1)
        IF(RAD0(1,1).NE.0.0)THEN
          L1=RAD1(1,1)/RAD0(1,1)
          L2=RAD2(1,1)/RAD0(1,1)
          L3=RAD3(1,1)/RAD0(1,1)
          L4=SQRT(ABS(1.0-L1*L1-L2*L2-L3*L3))
          ELSE
          L1=0.0
          L2=0.0
          L3=0.0
          L4=0.0
          ENDIF
        IF(IA.EQ.1.OR.IA.EQ.NE)THEN
          PTOT=PTOT+(POWER/2.0)
        ELSE
          PTOT=PTOT+POWER
        ENDIF
        PTOT=PTOT*DE
        RETURN
        END
C
C       ----------------------------------------------------------
        SUBROUTINE BRIGHT(ALPHA,COSPHI,SINPHI,KX,KY,I,S0,S1,S2,S3)
C       ----------------------------------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,K,O-Z)
        DATA KMIN/0.001/
        IF(I.LE.0.OR.ALPHA.LT.0.0)GOTO 40
        IF(KX.LT.KMIN.AND.KY.LT.KMIN)GOTO 40
        IF(KX.LT.KMIN.AND.KY.GT.KMIN)GOTO 10
        IF(KX.GT.KMIN.AND.KY.LT.KMIN)GOTO 20
        IF(KX.GT.KMIN.AND.KY.GT.KMIN)GOTO 30
10      CALL BRIGH1(ALPHA,COSPHI,SINPHI,KY,I,S0,S1,S2,S3)
        RETURN
20      CALL BRIGH1(ALPHA,SINPHI,-COSPHI,KX,I,S0,S1,S2,S3)
        S1=-S1
        S2=-S2
        RETURN
30      CALL BRIGH3(ALPHA,COSPHI,SINPHI,KX,KY,I,S0,S1,S2,S3)
        RETURN
40      WRITE(6,9000)
        STOP
9000    FORMAT(//' *** INVALID INPUT PARAMETERS IN SUBROUTINE
     $  BRIGHT ***')
        END
C
C       -------------------------------------------------------
        SUBROUTINE BRIGH1(ALPHA,COSPHI,SINPHI,K,I,S0,S1,S2,S3)
C       -------------------------------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,J,K,O-Z)
        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
        DATA NMAX/1000/,TOL1/1.0D-05/,TOL2/1.0D-05/
C
C       CALCULATE THE UNDULATOR RADIATION BRIGHTNESS FUNCTION
C       FOR A PLANE TRAJECTORY
C       USING THE BESSEL FUNCTION APPROXIMATION
C
C       CALCULATE VARIABLES X, Y
C
        A=1.0+((K*K)/2.0)+(ALPHA*ALPHA)
        X=I*2.0*K*ALPHA*COSPHI/A
        Y=I*K*K/(4.0*A)
        IF(DABS(X).LT.TOL1)GOTO 10
C
C       CALCULATE A0, A1 IN THE GENERAL CASE
C
        CALL JSET(JNX,J0X,X,TOL2,MAXX,NMAX)
        CALL JSET(JNY,J0Y,Y,TOL2,MAXY,NMAX)
        CALL JSUM1(X,Y,SUM1,SUM2,I,MAXX,MAXY)
        A0=SUM1
        A1=((2.0*I*SUM1)+(4.0*SUM2))/X
        GOTO 30
C
C       CALCULATE A0, A1 WHEN X = 0.0
C
10      CALL JSET(JNY,J0Y,Y,TOL2,MAXY,NMAX)
        IF(((I+1)/2).GT.MAXY)RETURN
        IF(I.EQ.((I/2)*2))GOTO 20
        N1=(-I-1)/2
        N2=(-I+1)/2
        A0=0.0
        A1=JY(N1)+JY(N2)
        GOTO 30
20      N=-I/2
        A0=JY(N)
        A1=0.0
C
C       CALCULATE STOKES PARAMETER
C
30      AXR=2.0*I/A*(A0*ALPHA*COSPHI-K*A1/2.0)
        AYR=2.0*I/A*(A0*ALPHA*SINPHI)
        S0=AXR*AXR+AYR*AYR
        S1=AXR*AXR-AYR*AYR
        S2=2.0*AXR*AYR
        S3=0.0
        RETURN
        END
C
C       -----------------------------------------------------------
        SUBROUTINE BRIGH3(ALPHA,COSPHI,SINPHI,KX,KY,I,S0,S1,S2,S3)
C       -----------------------------------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,J,K,O-Z)
        INTEGER Q
        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
        DATA NMAX/1000/,TOL1/1.0D-05/,TOL2/1.0D-05/
C
C       CALCULATE THE UNDULATOR RADIATION BRIGHTNESS FUNCTION
C       FOR A GENERAL ELLIPTICAL/HELICAL TRAJECTORY
C       USING THE BESSEL FUNCTION APPROXIMATION
C
        S0R =0.0
        S0I =0.0
        S1R =0.0
        S1I =0.0
        SM1R=0.0
        SM1I=0.0
C
C       CALCULATE VARIABLES X, Y, PHI
C
        A=1.0+((KX*KX)/2.0)+((KY*KY)/2.0)+(ALPHA*ALPHA)
        X=I*2.0*ALPHA*DSQRT(((KX*SINPHI)**2)+((KY*COSPHI)**2))/A
        Y=I*((KY*KY)-(KX*KX))/(4.0*A)
        PHI=DATAN2((KX*SINPHI),(KY*COSPHI))
        IF(X.LT.TOL1.AND.DABS(Y).LT.TOL1)GOTO 30
        IF(X.LT.TOL1)GOTO 10
        IF(DABS(Y).LT.TOL1)GOTO 20
C
C       SET UP BESSEL FUNCTIONS IN THE GENERAL CASE
C
        CALL JSET(JNX,J0X,X,TOL2,MAXX,NMAX)
        CALL JSET(JNY,J0Y,Y,TOL2,MAXY,NMAX)
C
C       DO THE SUMS
C
        Q=0
        CALL JSUM2(S0R,S0I,PHI,I,Q)
        Q=-1
        CALL JSUM2(SM1R,SM1I,PHI,I,Q)
        Q=1
        CALL JSUM2(S1R,S1I,PHI,I,Q)
        GOTO 40
C
C       CALCULATE S0,S1,SM1 WHEN X = 0.0
C
10      CALL JSET(JNY,J0Y,Y,TOL2,MAXY,NMAX)
        IF(I.EQ.((I/2)*2))GOTO 15
        N1=(-I-1)/2
        N2=(-I+1)/2
        S1R =JY(N1)
        SM1R=JY(N2)
        GOTO 40
15      N=-I/2
        S0R =JY(N)
        GOTO 40
C
C       CALCULATE S0,S1,SM1 WHEN Y = 0.0
C
20      N1=I
        N2=I+1
        N3=I-1
        CALL JSET(JNX,J0X,X,TOL2,MAXX,NMAX)
        S0R = DCOS(N1*PHI)*JX(N1)
        S0I =-DSIN(N1*PHI)*JX(N1)
        S1R = DCOS(N2*PHI)*JX(N2)
        S1I =-DSIN(N2*PHI)*JX(N2)
        SM1R= DCOS(N3*PHI)*JX(N3)
        SM1I=-DSIN(N3*PHI)*JX(N3)
        GOTO 40
C
C       CALCULATE S0,S1,SM1 WHEN X = 0.0 AND Y = 0.0
C
30      IF(I.EQ.1)SM1R=1.0
C
C       CALCULATE STOKES PARAMETER
C
40      AXR=((2.0*S0R*ALPHA*COSPHI)-(KY*(S1R+SM1R)))*I/A
        AXI=((2.0*S0I*ALPHA*COSPHI)-(KY*(S1I+SM1I)))*I/A
        AYR=((2.0*S0R*ALPHA*SINPHI)+(KX*(S1I-SM1I)))*I/A
        AYI=((2.0*S0I*ALPHA*SINPHI)-(KX*(S1R-SM1R)))*I/A
        S0=AXR*AXR+AXI*AXI+AYR*AYR+AYI*AYI
        S1=AXR*AXR+AXI*AXI-AYR*AYR-AYI*AYI
        S2=2.0*(AXR*AYR+AXI*AYI)
        S3=2.0*(AXI*AYR-AXR*AYI)
        RETURN
        END
C
C       --------------------------------------
        SUBROUTINE JSUM1(X,Y,S1,S2,I,MAXX,MAXY)
C       --------------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)
C
C       CALCULATE SUMS S1 AND S2
C
        S1=0.0
        IF(I.LE.MAXX)S1=JY(0)*JX(I)
        S2=0.0
        SIGN=1.0
        DO 10 N=1,MAXY
        SIGN=-SIGN
        N1=(2*N)+I
        N2=(-2*N)+I
        J1=JY(N)
        J2=JX(N1)
        J3=JX(N2)
        S1=S1+(J1*(J2+(J3*SIGN)))
        S2=S2+(N*J1*(J2-(J3*SIGN)))
10      CONTINUE
        RETURN
        END
C
C       ------------------------------
        SUBROUTINE JSUM2(SR,SI,PHI,I,Q)
C       ------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,J,K,O-Z)
        INTEGER P,Q
        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
        SR=0.0
        SI=0.0
        DO 10 P=-MAXY,MAXY
        N=I+(2*P)+Q
        IF(IABS(N).GT.MAXX)GOTO 10
        F=JX(N)*JY(P)
        SR=SR+(DCOS(N*PHI)*F)
        SI=SI-(DSIN(N*PHI)*F)
10      CONTINUE
        RETURN
        END
C
C       -----------------------------
        FUNCTION SINC(ALP2,ALP2I,R,N)
C       -----------------------------
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DATA PI/3.141592653589/
C
C       CALCULATE THE LINESHAPE FUNCTION
C
        SINC=1.0
        X=N*PI*(ALP2-ALP2I)/R
        IF(DABS(X).LT.1.0D-06)RETURN
        SINC=DSIN(X)/X
        SINC=SINC*SINC
        RETURN
        END
C
C       --------------
        FUNCTION JX(N)
C       --------------
        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)
        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
        JX=0.0
        IF(IABS(N).GT.MAXX)RETURN
        IF(N.NE.0)GOTO 10
        JX=J0X
        RETURN
10      SIGN=1.0
        N1=N
        IF(N.GT.0)GOTO 20
        N1=-N
        IF(N1.NE.((N1/2)*2))SIGN=-1.0
20      JX=JNX(N1)*SIGN
        RETURN
        END
C
C       --------------
        FUNCTION JY(N)
C       --------------
        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)
        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
        JY=0.0
        IF(IABS(N).GT.MAXY)RETURN
        IF(N.NE.0)GOTO 10
        JY=J0Y
        RETURN
10      SIGN=1.0
        N1=N
        IF(N.GT.0)GOTO 20
        N1=-N
        IF(N1.NE.((N1/2)*2))SIGN=-1.0
20      JY=JNY(N1)*SIGN
        RETURN
        END
C
C       ----------------------------------------
        SUBROUTINE JSET(JNX,J0X,X0,TOL,MAX,NMAX)
C       ----------------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)
        DIMENSION JNX(1)
        DATA BIGNO/1.0D+10/,BIGNI/1.0D-10/
C
C       SET UP ARRAY OF BESSEL FUNCTIONS UP TO ORDER MAX
C       SUCH THAT JMAX (X) < TOL AND MAX > X
C       USING MILLER'S DOWNWARD RECURRENCE ALGORITHM
C       MODIFIED FORM OF NUMERICAL RECIPIES ROUTINE BESSJN
C       NB] ARGUMENT CAN BE NEGATIVE
C
        X=DABS(X0)
        IF(X.LE.0.1)THEN
          M=4
        ELSE
          IF(X.LE.1.0)THEN
            M=8
          ELSE
            M=2*((INT(1.18*X)+13)/2)
          ENDIF
        ENDIF
        IF(M.GT.NMAX)GOTO 999
C
        TOX=2.0/X
        ISUM=0
        SUM=0.
        BJP=0.
        BJ=1.
        JNX(M)=1.0
        DO 10 N1=M,1,-1
        N=N1-1
        BJN=N1*TOX*BJ-BJP
        BJP=BJ
        BJ=BJN
        IF(DABS(BJ).GT.BIGNO)THEN
          BJ=BJ*BIGNI
          BJP=BJP*BIGNI
          SUM=SUM*BIGNI
          DO 20 I=N1,M
          JNX(I)=JNX(I)*BIGNI
20        CONTINUE
        ENDIF
        IF(N.NE.0)JNX(N)=BJ
        IF(ISUM.NE.0)SUM=SUM+BJ
        ISUM=1-ISUM
10      CONTINUE
        SUM=(2.0*SUM)-BJ
        J0X=BJ/SUM
        SIGN=1.0
        DO 30 N=1,M
        SIGN=-SIGN
        JNX(N)=JNX(N)/SUM
        IF(X0.LT.0.0)JNX(N)=JNX(N)*SIGN
        IF(N.LE.DABS(X).OR.JNX(N).GT.TOL)MAX=N
30      CONTINUE
        RETURN
999     WRITE(6,9999)
        STOP
9999    FORMAT(//' *** OVERFLOW OF BESSEL FUNCTION ARRAY ***')
        END

	SUBROUTINE GetFromNml( PERIOD,KX,KY,N,ENERGY,CUR,NPHI,DALPHA)

	!IMPLICIT	REAL*8	(A-H,O-Z)
        implicit none
	real*8, intent(inout)    :: period,KX,KY,ENERGY,CUR,DALPHA
	integer, intent(inout) :: N,NPHI


	integer :: NCOMP,ICOMP
	real*8 :: RCURR,BPASS
        integer :: IANGLE,IAPERTURE,IEXTERNAL
        character(len=1024) :: FOUT,FIN,FTRAJ,FINT
        real*8 :: EMIN,EMAX
        real*8 :: THEMIN,THEMAX,PHIMIN,PHIMAX
        integer :: NE,NT,NP,NCHECK,IOPT,ITER,IPASS
        integer :: I_EDIV,IINT
        real*8  :: EDIVX,EDIVY

	integer :: i_device, n0
	real*8 :: rlau, oldener, rk, rkx, rlen

	NAMELIST	/PARAIN/	NCOMP,RCURR,ICOMP,BPASS, 
     $     IANGLE,IAPERTURE,IEXTERNAL, 
     $     FOUT,FIN,FTRAJ,EMIN,EMAX, 
     $     THEMIN,THEMAX,PHIMIN,PHIMAX, 
     $     NE,NT,NP,NCHECK,IOPT,ITER,IPASS, 
     $     I_EDIV,EDIVX,EDIVY,FINT,IINT
           
        NAMELIST /EPATH1/ I_DEVICE, N0, RLAU, OLDENER, RK, RKX, RLEN
!

!C
!C Read in the parameters from namelist files
!C
	OPEN	(21, FILE='uphot.nml', STATUS='OLD')
	READ	(21, NML=PARAIN)
	write(6,NML=PARAIN) 
	CLOSE	(21)

	OPEN	(21, FILE='epath.nml', STATUS='OLD')
	READ	(21, NML=EPATH1)
	write(6,NML=EPATH1) 
	CLOSE	(21)

	period = RLAU
        kx = 0
        ky = rk
        n = ncomp
        energy = oldener
	cur = rcurr
        nphi = 0
        dalpha = 0.0D0
        
!C
!C Read in the (energy, theta, phi) array
!C
	END SUBROUTINE GetFromNml

