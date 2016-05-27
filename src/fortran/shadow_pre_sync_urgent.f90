module shadow_pre_sync_urgent

!
!  This module include code from URGENT, a code from  R.P.Walker and B.Diviacco
!  to compute Undulator Radiation properties.
!
!  Reference:   
!          R.P. Walker and B. Diviacco, 
!          URGENT-A computer program for calculating the undulator radiation 
!          spectral, angular, polarization, and power density properties,
!          Rev. Sci. Instrum. 63, pp. 392-395, 1992.
!        
!  Purpose: 
!          The purpose of this module is to implement the photon angular 
!          distribution as a function of the photon energy for an Undulator.
!          It is an alternative mechanism for creating a shadow3 
!          undulator source, thus to be used instead the SHADOW's 
!          undul_phot preprocessor.
! 
!  History: 
!         First version was created by Federico Zontone at ESRF in 1994
!         using Urgent code. The code cdf_z.f was used in SHADOW's
!         ESRF version for years, and has been proposed as an optional 
!         module in shadow3. 
!
!         In december 2013 M. Sanchez del Rio revisited the undulator codes
!         in SHADOW3 and converted cdf_z.f in a fortran 9x module urgent.F90
!         that is used in shadow3 (by default now). 
!
!         The called subroutine is renamed from cdf_z to undul_phot_urgent
!         to be more consistemt with the names. In fact, this code calculates
!         the photon flux distributions and not the cdf (that is calculated 
!         in the SHADOW preprocessor undul_cdf). 
!
!         To convert cdf_z in a f9x module some changes have been done in the
!         code. Mainly the common blocks are now global variables, and some 
!         variables have been renamed to avoid fortran reserved names and 
!         conflict between global variables and variables in routines scope. 
!         Also, all variables are declared (implicit none).
!         Most modification are in lowercase. 
!
! Inputs: 
!        Undulator inputs are read from files created by other 
!        shadow3 prerocessors: 
!
!        - epath.nml (written by shadow3/epath)
!        - uphot.nml (written by shadow3/undul_set)
!        - uphot.dat containing the (energy,theta,phi) grid 
!                    (written by shadow3/undul_set)
!        
! Outputs: 
!        - uphot.dat the flux for different photon energies are appended
!                    in the uphot.dat file (binary). Note that this 
!                    binary file can be converted to ascii output using 
!                    shadow3/undul_phot_dump utility.
!
! TODO: 
!         Continue cleaning the code: 
!             - check and manage dimensions
!             - make variable declarations compatible with shadow3
!             - implement the full functionality of the main URGENT
!               (some functions are missing here). 
!
!
    !---- Use Modules ----!

    ! for compatibility with shadow3 (just in case!)
    use, intrinsic :: ISO_C_BINDING


    !---- Variables ----!
    implicit none


    !C
    !C declarations
    !C
    
    double precision gamma1,K3,KX,KY,LAMDAR,len1
    double precision SINPHI(400),COSPHI(400)
    double precision DPHI,DALPHA
    double precision CODE(400)
    integer isg(400)
    integer NPHI1,NPHI2,NALPHA
    integer Nund

    double precision F3,APMIN,APMAX
    integer MODE,ICALC,IHARM,IANG
    double precision D,XPMIN,DXP,YPMIN,DYP,FAC
    integer NXP,NYP

    double precision E_MIN,DE
    double precision H(5000)
    double precision WMIN,DW
    integer NW

    double precision BRI1(101,10),BRI2(101,10)
    double precision BRI3(101,10),BRI0(101,10)
    double precision RAD1(51,51),RAD2(51,51)
    double precision RAD3(51,51),RAD0(51,51)
    integer I1(5001),I2(5001)
    integer L1,L2,L3,L4
    double precision SPEC1(5001),SPEC2(5001)
    double precision SPEC3(5001),SPEC0(5001)
    double precision WK(5001)

    integer maxx,maxy
    double precision jnx(1000),jny(1000)
    double precision j0x,j0y

!C
!C common/global block 1
!C
!         COMMON/INPUT/gamma1,K3,KX,KY,LAMDAR,len1,Nund
!
!C        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,DALPHA,
!C     $  ISG(400),NPHI1,NPHI2,NALPHA
!
!         COMMON/CONST/F3,APMIN,APMAX,MODE,ICALC,IHARM,IANG
!         COMMON/SCREEN/D,XPMIN,DXP,YPMIN,DYP,FAC,NXP,NYP
!         COMMON/SCALE/E_MIN,DE
!
!C         COMMON/CONVL/H(5001),WMIN,DW,NW
!
!C         COMMON/CALC/BRI1(101,101),BRI2(101,101),BRI3(101,101),
!C     $  BRI0(101,101),RAD1(51,51),RAD2(51,51),RAD3(51,51),
!C     $  RAD0(51,51),I1(5001),I2(5001),L1,L2,L3,L4,
!C     $  SPEC1(5001),SPEC2(5001),SPEC3(5001),SPEC0(5001),WK(5001)
! 
!
!C
!C common/global block 2
!C
!C        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
!C


    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
!    public :: 
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public ::  undul_phot_urgent
 

 Contains

        subroutine undul_phot_urgent
        implicit none

        double precision PERIOD_1,KX_1,KY_1, ENERGY_1,CUR_1, DALPHA_1
        integer NPERIOD_1, NPHI_1, NE_1,NT_1,NP_1

        call GetFromNml(PERIOD_1,KX_1,KY_1,NPERIOD_1,ENERGY_1, &
        CUR_1,NPHI_1,DALPHA_1,NE_1,NT_1,NP_1)
        call cdf_z_calc(PERIOD_1,KX_1,KY_1,NPERIOD_1,ENERGY_1, &
        CUR_1,NPHI_1,DALPHA_1,NE_1,NT_1,NP_1)

        end subroutine undul_phot_urgent

!C
!C ***************************************************************************
!C
!C                             CDF_Z
!C
!C       A program to generate the CDF of the photons for SHADOW by
!C	calculating the flux and the degree of polarization at given
!C	polar angles (theta,phi) and energy.
!C
!C       Modified version of URGENT made by F. Zontone.
!C             Further modified my M. Sanchez del Rio to adapt to shadow3
!C
!C       Usage: to call in the SHADOW module "MAKE_ID", option 'undulator',
!C              as user's program.
!C
!C	INPUT:  i)  the input file UNDUL_Z.PAR with the undulator
!C		    parameters;
!C	OUTPUT: i)  the file for SHADOW uphot.dat used for generating
!C		    the CDF of the undulator radiation
!C
        SUBROUTINE CDF_Z_CALC(PERIOD_1,KX_1,KY_1,NPERIOD_1,ENERGY_1, &
        CUR_1,NPHI_1,DALPHA_1,NE_1,NT_1,NP_1)

        implicit none

        double precision PERIOD_1,KX_1,KY_1, ENERGY_1,CUR_1, DALPHA_1
        integer NPERIOD_1, NPHI_1, NE_1,NT_1,NP_1
        double precision ENER,THETA_S,PHI_S,RNO,POL_DEG


        double precision cur,domega,e1,energy,gk,k,k2
        double precision l1_s,lamda1,pd,period,ph_ang
        integer i,isign1,inc,idebug,ie,j,m,n,nomega,np_s,ne_s,nt_s
        integer nphi,nsig
        integer use_undulator_binary_file

        double precision ptot,phi,xe,xpc,xps,th_ang,ye,ypc,yps
        double precision pi

        DIMENSION ENER(NE_1),THETA_S(NT_1,NE_1),PHI_S(NP_1,NT_1,NE_1)
        DIMENSION RNO(NP_1,NT_1,NE_1),POL_DEG(NP_1,NT_1,NE_1)

        !DIMENSION ENER(51),THETA_S(31,51),PHI_S(31,31,51)
        !DIMENSION RNO(31,31,51),POL_DEG(31,31,51)
!C        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)
!C        INTEGER CODE, Nund
!C        COMMON/INPUT/gamma1,K3,KX,KY,LAMDAR,len1,Nund
!CCsrio        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,NPHI1,NPHI2,
!CCsrio     $  NALPHA,DALPHA,ISG(400)
!C        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,DALPHA,
!C     $  ISG(400),NPHI1,NPHI2,NALPHA
!C        COMMON/CONST/F3,APMIN,APMAX,MODE,ICALC,IHARM,IANG
!CCsrio        COMMON/SCREEN/D,XPMIN,DXP,NXP,YPMIN,DYP,NYP,FAC
!C        COMMON/SCREEN/D,XPMIN,DXP,YPMIN,DYP,FAC,NXP,NYP
!C        COMMON/SCALE/E_MIN,DE
!C        COMMON/CONVL/H(5001),WMIN,DW,NW
!C        COMMON/CALC/BRI1(101,101),BRI2(101,101),BRI3(101,101),
!C     $  BRI0(101,101),RAD1(51,51),RAD2(51,51),RAD3(51,51),
!C     $  RAD0(51,51),I1(5001),I2(5001),L1,L2,L3,L4,
!C     $  SPEC1(5001),SPEC2(5001),SPEC3(5001),SPEC0(5001),WK(5001)

        DATA PI/3.141592653589/,IDEBUG/0/
!C
!C       READ UNDULATOR PARAMETERS FROM A INPUT FILE
!C
!C
!C gets main inputs from arguments
!C
        PERIOD = period_1
        KX = kx_1
        KY = ky_1 
        Nund = nperiod_1
        ENERGY = energy_1
        CUR = cur_1
        NPHI = nphi_1
        DALPHA = dalpha_1


!C
!C gets additional input from shadow namelists
!C
!C	OPEN(21,FILE='UNDUL_Z.PAR',STATUS='OLD')
!C	READ(21,*) PERIOD,KX,KY,N,ENERGY,CUR,NPHI,DALPHA
!C	CLOSE (21)
!C
!C	READ THE ANGLES FROM THE SHADOW BINARY FILE
!C
      use_undulator_binary_file = 0  ! from now, use ASCII files. If change, do the same in shadow_pre_sync.f90

      if (use_undulator_binary_file .eq. 1) then
          OPEN (10,FILE='uphot.dat',STATUS='UNKNOWN',FORM='UNFORMATTED')
          READ (10) NE_S,NT_S,NP_S
      else
          OPEN (10,FILE='uphot.dat',STATUS='UNKNOWN',FORM='FORMATTED')
          READ (10,*) NE_S,NT_S,NP_S
      endif

      IF ( (NE_1 .ne. NE_S) .or.  &
           (NT_1 .ne. NT_S) .or.  &
           (NP_1 .ne. NP_S) ) THEN
          write(*,*) "ERROR: Inconsistent dimensions: "
          write(*,*) "    NE_1,NE_S",NE_1,NE_S
          write(*,*) "    NT_1,NT_S",NT_1,NT_S
          write(*,*) "    NP_1,NP_S",NP_1,NP_S
          print *,'Error: CDF_Z_CALC'
          return
          ! STOP
      END IF

      if (use_undulator_binary_file .eq. 1) then
        DO M=1,NE_S
	        READ (10) ENER(M)
        END DO

        DO M=1,NE_S
	        DO J=1,NT_S
	            READ (10) THETA_S(J,M)
            END DO
        END DO

        DO M=1,NE_S
	        DO J=1,NT_S
	            DO I=1,NP_S
	                READ (10) PHI_S(I,J,M)
                END DO
            END DO
        END DO
      else
        DO M=1,NE_S
	        READ (10,*) ENER(M)
        END DO

        DO M=1,NE_S
	        DO J=1,NT_S
	            READ (10,*) THETA_S(J,M)
            END DO
        END DO

        DO M=1,NE_S
	        DO J=1,NT_S
	            DO I=1,NP_S
	                READ (10,*) PHI_S(I,J,M)
                END DO
            END DO
        END DO
      end if



!C
!C       CONSTANTS
!C
        gamma1=ENERGY/0.000511
        LAMDAR=PERIOD*1.0D+10/(2.0*gamma1*gamma1)
        K2=KX*KX+KY*KY
        K3=1.0+K2/2.0
        len1=Nund*PERIOD
        LAMDA1=LAMDAR*K3
        E1=12398.5/LAMDA1
        PTOT=0.0725*Nund*K2*ENERGY*ENERGY*CUR/PERIOD
        GK=0.0
        IF(KX.LT.1E-3.OR.KY.LT.1E-3)THEN
        IF(KX.LT.1E-3)K=KY
        IF(KY.LT.1E-3)K=KX
        GK=K*((K**6)+(24.*(K**4)/7.)+(4.*K*K)+(16./7.)) &
        /((1.+(K*K))**3.5)
        ENDIF
        IF(ABS(KX-KY).LT.1E-3)THEN
        K=KX
        GK=32.0/7.0*K/(1.0+K*K)**3.0
        ENDIF
        PD=0.1161*(ENERGY**4)*Nund*K*GK*CUR/PERIOD
!C
!C       DEFAULT CALCULATION
!C
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
        IF(NOMEGA.EQ.0)NOMEGA=16
        IF(DOMEGA.EQ.0.0)DOMEGA=2.0
!C	
!C	ENERGY SCALE
!C
        DE=2.0*DOMEGA*E1/(NOMEGA*N)
!C
!C       ANGULAR (IANG=1) UNITS
!C
        IANG=1
        D=1.0
!C
!C       ERROR CHECK
!C
        write (*,*) "NXP,NYP,NPHI,NALPHA,NOMEGA: ",NXP,NYP,NPHI, &
      NALPHA,NOMEGA
        IF(NXP.LT.0.OR.NXP.GT.50.OR.NYP.LT.0.OR.NYP.GT.50)GOTO 900
        IF(NPHI.GT.100.OR.NALPHA.GT.100)GOTO 900
        IF(NOMEGA.GT.5000)GOTO 900
!C
!C	START CALCULATIONS
!C
        WRITE(6,500)
        DO M=1,NE_S
	   write(*,*) 'N of iteration in energy: ',M,' of ',NE_S
	   DO J=1,NT_S
	      DO I=1,NP_S
		 E_MIN=DBLE(ENER(M))
		 TH_ANG=DBLE(THETA_S(J,M))
		 PH_ANG=DBLE(PHI_S(I,J,M))
		 XPC=TH_ANG*DCOS(PH_ANG)*1D+03
		 YPC=TH_ANG*DSIN(PH_ANG)*1D+03
!C
!C       PINHOLE : DETERMINE MIN. AND MAX. EMISSION ANGLES WITHIN PINHOLE
!C
        	 XE=XPC*1.0D-03/D
        	 YE=YPC*1.0D-03/D
        	 APMAX=gamma1*gamma1*((XE*XE)+(YE*YE))
        	 APMIN=gamma1*gamma1*((XE*XE)+(YE*YE))
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
!C
!C       SET UP ARRAY OF COSPHI AND SINPHI
!C
        	 NPHI1=(4*NPHI)
        	 NPHI2=NPHI+1
        	 INC=NPHI
        	 Isign1=1
        	 DPHI=2.0*PI/FLOAT(NPHI1)
        	DO 10 IE=1,NPHI1
        	 IF(IE.EQ.1)CODE(IE)=1
        	 IF(IE.NE.1)CODE(IE)=CODE(IE-1)+Isign1
        	 IF(IE.EQ.(INC+1))Isign1=-1
        	 IF(IE.EQ.((2*INC)+1))Isign1=1
        	 IF(IE.EQ.((3*INC)+1))Isign1=-1
        	 ISG(IE)=Isign1
        	 PHI=(IE-1)*DPHI
        	 SINPHI(IE)=DSIN(PHI)
        	 COSPHI(IE)=DCOS(PHI)
10      	CONTINUE
!C
!C       FACTOR TO SCALE FINAL RESULTS
!C
        	 F3=4.55D+07*Nund*Nund*gamma1*gamma1*CUR/(D*D)
!C
!C       CALL ANALYSIS PROGRAM
!C
     		 CALL SUB4
		 RAD0(1,1)=RAD0(1,1)*(1000/E_MIN)*1D06
		 RNO(I,J,M)=REAL(RAD0(1,1))
		 IF(RNO(I,J,M).LT.10.0) RNO(I,J,M)=1.0
		 L1_S=REAL(L1)
		 POL_DEG(I,J,M)=(1+L1_S)/2
      		 END DO
	   END DO
	END DO

	if (use_undulator_binary_file .eq. 1) then
	    DO M=1,NE_S
	       DO J=1,NT_S
	          DO I=1,NP_S
	             WRITE (10) RNO(I,J,M)
              END DO
           END DO
        END DO
	    DO M=1,NE_S
	       DO J=1,NT_S
	          DO I=1,NP_S
	             WRITE (10) POL_DEG(I,J,M)
              END DO
           END DO
        END DO
    else
	    DO M=1,NE_S
	       DO J=1,NT_S
	          DO I=1,NP_S
	             WRITE (10,*) RNO(I,J,M)
              END DO
           END DO
        END DO
	    DO M=1,NE_S
	       DO J=1,NT_S
	          DO I=1,NP_S
	             WRITE (10,*) POL_DEG(I,J,M)
              END DO
           END DO
        END DO
    end if


	CLOSE (10)
	write(*,*) 'Calculations have been successfully completed !'
	RETURN


        !STOP
900     WRITE(6,9000)
500     FORMAT('         ******  UNDUL_PHOT_URGENT - VERSION 1.3 - ******')
9000    FORMAT(//' *** INVALID INPUT PARAMETERS ***')
        END subroutine
!C
!C       ---------------
        SUBROUTINE SUB4
!C       ---------------
!C        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)
        implicit none

        integer i,ia,ne,ib,ic
        double precision ptot,a2max,a2min,alp,alp2,alp2i, cosph
        double precision e,lamda,power,r,radmax,s0,s1,s2,s3,sinph
        double precision xe1,xp,xpmm,ye1,yp,ypmm
        double precision pi,twopi,epi2
!        double precision sinc ! function
        DATA PI/3.141592653589/,TWOPI/6.283185307/,EPI2/78.95683521/

!C        INTEGER CODE,Nund

!C        COMMON/INPUT/gamma1,K3,KX,KY,LAMDAR,len1,Nund
!CCsrio        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,NPHI1,NPHI2,
!CCsrio     $  NALPHA,DALPHA,ISG(400)
!C        COMMON/ANGLE/CODE(400),SINPHI(400),COSPHI(400),DPHI,DALPHA,
!C     $  ISG(400),NPHI1,NPHI2,NALPHA
!C        COMMON/CONST/F3,APMIN,APMAX,MODE,ICALC,IHARM,IANG
!CCsrio        COMMON/SCREEN/D,XPMIN,DXP,NXP,YPMIN,DYP,NYP,FAC
!C        COMMON/SCREEN/D,XPMIN,DXP,YPMIN,DYP,FAC,NXP,NYP
!C	COMMON/SCALE/E_MIN,DE
!C        COMMON/CONVL/H(5001),WMIN,DW,NW
!C        COMMON/CALC/BRI1(101,101),BRI2(101,101),BRI3(101,101),
!C     $  BRI0(101,101),RAD1(51,51),RAD2(51,51),RAD3(51,51),
!C     $  RAD0(51,51),I1(5001),I2(5001),L1,L2,L3,L4,
!C     $  SPEC1(5001),SPEC2(5001),SPEC3(5001),SPEC0(5001),WK(5001)

!C
!C       ZERO EMITTANCE
!C       MODE  = 2 ANGULAR (IANG=1) DENSITY SPECTRUM
!C       ICALC = 3
!C
!C
!C       VARY PHOTON ENERGY
!C
        PTOT=0.0
        NE=1
        IA=NE
        E=E_MIN
        LAMDA=12398.5/E
        R=LAMDA/LAMDAR
        I1(IA)=0
        I2(IA)=0
        RADMAX=0.0
!C
!C       VARY POSITION IN PINHOLE
!C
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
!C
!C       FIND HARMONIC NO. CONTRIBUTING AT THE
!C       GIVEN WAVELENGTH AND PINHOLE POSITION
!C
        XE1=XP*gamma1/D
        YE1=YP*gamma1/D
        ALP2=(XE1*XE1)+(YE1*YE1)
        I=0
20      I=I+1
        ALP2I=(R*I)-K3
        A2MAX=ALP2I+(DALPHA*R/Nund)
        IF(A2MAX.LT.ALP2)GOTO 20
        A2MIN=ALP2I-(DALPHA*R/Nund)
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
        RAD1(IB,IC)=F3*S1*SINC(ALP2,ALP2I,R,Nund)
        RAD2(IB,IC)=F3*S2*SINC(ALP2,ALP2I,R,Nund)
        RAD3(IB,IC)=F3*S3*SINC(ALP2,ALP2I,R,Nund)
        RAD0(IB,IC)=F3*S0*SINC(ALP2,ALP2I,R,Nund)
!C
!C       PRINT IRRADIANCE DISTRIBUTION AT THE GIVEN WAVELENGTH
!C
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
        END subroutine
!C
!C       ----------------------------------------------------------
        SUBROUTINE BRIGHT(ALPHA,COSphi1,SINphi1,kx1,ky1,I,S0,S1,S2,S3)
!C       ----------------------------------------------------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,K,O-Z)
!C        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)
        implicit none
        double precision ALPHA,COSphi1,SINphi1,kx1,ky1,S0,S1,S2,S3
        integer i

        double precision kmin

        DATA KMIN/0.001/
        IF(I.LE.0.OR.ALPHA.LT.0.0)GOTO 40
        IF(kx1.LT.KMIN.AND.ky1.LT.KMIN)GOTO 40
        IF(kx1.LT.KMIN.AND.ky1.GT.KMIN)GOTO 10
        IF(kx1.GT.KMIN.AND.ky1.LT.KMIN)GOTO 20
        IF(kx1.GT.KMIN.AND.ky1.GT.KMIN)GOTO 30
10      CALL BRIGH1(ALPHA,COSphi1,SINphi1,ky1,I,S0,S1,S2,S3)
        RETURN
20      CALL BRIGH1(ALPHA,SINphi1,-COSphi1,kx1,I,S0,S1,S2,S3)
        S1=-S1
        S2=-S2
        RETURN
30      CALL BRIGH3(ALPHA,COSphi1,SINphi1,kx1,ky1,I,S0,S1,S2,S3)
        RETURN
40      WRITE(6,9000)
        print *,'Error: BRIGHT'
        return
        !STOP
9000    FORMAT(//' *** INVALID INPUT PARAMETERS IN SUBROUTINE BRIGHT ***')
        END subroutine
!C
!C       -------------------------------------------------------
        SUBROUTINE BRIGH1(ALPHA,COSphi1,SINphi1,K,I,S0,S1,S2,S3)
!C       -------------------------------------------------------
!C        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)

        implicit none

        double precision alpha,cosphi1,sinphi1,k,s0,s1,s2,s3
        integer i

        double precision tol1,tol2
        double precision a,a0,a1,axr,ayr
        integer nmax,n,n1,n2
        double precision sum1,sum2 ,x,y
!       double precision jx,jy ! functions
!C        integer maxx,maxy
!C        double precision jnx,jny,j0x,j0y
!C        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
!C        include 'tmp.h'

        DATA NMAX/1000/,TOL1/1.0D-05/,TOL2/1.0D-05/
!C
!C       CALCULATE THE UNDULATOR RADIATION BRIGHTNESS FUNCTION
!C       FOR A PLANE TRAJECTORY
!C       USING THE BESSEL FUNCTION APPROXIMATION
!C
!C       CALCULATE VARIABLES X, Y
!C
        A=1.0+((K*K)/2.0)+(ALPHA*ALPHA)
        X=I*2.0*K*ALPHA*COSphi1/A
        Y=I*K*K/(4.0*A)
        IF(DABS(X).LT.TOL1)GOTO 10
!C
!C       CALCULATE A0, A1 IN THE GENERAL CASE
!C
        CALL JSET(JNX,J0X,X,TOL2,MAXX,NMAX)
        CALL JSET(JNY,J0Y,Y,TOL2,MAXY,NMAX)
        CALL JSUM1(X,Y,SUM1,SUM2,I,MAXX,MAXY)
        A0=SUM1
        A1=((2.0*I*SUM1)+(4.0*SUM2))/X
        GOTO 30
!C
!C       CALCULATE A0, A1 WHEN X = 0.0
!C
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
!C
!C       CALCULATE STOKES PARAMETER
!C
30      AXR=2.0*I/A*(A0*ALPHA*COSphi1-K*A1/2.0)
        AYR=2.0*I/A*(A0*ALPHA*SINphi1)
        S0=AXR*AXR+AYR*AYR
        S1=AXR*AXR-AYR*AYR
        S2=2.0*AXR*AYR
        S3=0.0
        RETURN
        END subroutine
!C
!C       -----------------------------------------------------------
        SUBROUTINE BRIGH3(ALPHA,COSphi1,SINphi1,kx1,ky1,I,S0,S1,S2,S3)
!C       -----------------------------------------------------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,J,K,O-Z)

        implicit none

        double precision alpha,cosphi1,sinphi1,kx1,ky1,s0,s1,s2,s3
        integer i

        integer nmax
        double precision tol1,tol2
        integer q
        integer n,n1,n2,n3
        double precision sum1,sum2 ,x,y
        double precision s0r,s0i,s1r,s1i,sm1r,sm1i
        double precision a,phi,axi,axr,ayi,ayr
!       double precision jx,jy ! functions
!C        double precision jnx,jny,j0x,j0y
!C        INTEGER Q
!C        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
!C        include 'tmp.h'

        DATA NMAX/1000/,TOL1/1.0D-05/,TOL2/1.0D-05/

!C
!C       CALCULATE THE UNDULATOR RADIATION BRIGHTNESS FUNCTION
!C       FOR A GENERAL ELLIPTICAL/HELICAL TRAJECTORY
!C       USING THE BESSEL FUNCTION APPROXIMATION
!C
        S0R =0.0
        S0I =0.0
        S1R =0.0
        S1I =0.0
        SM1R=0.0
        SM1I=0.0
!C
!C       CALCULATE VARIABLES X, Y, PHI
!C
        A=1.0+((kx1*kx1)/2.0)+((ky1*ky1)/2.0)+(ALPHA*ALPHA)
        X=I*2.0*ALPHA*DSQRT(((kx1*SINphi1)**2)+((ky1*COSphi1)**2))/A
        Y=I*((ky1*ky1)-(kx1*kx1))/(4.0*A)
        PHI=DATAN2((kx1*SINphi1),(ky1*COSphi1))
        IF(X.LT.TOL1.AND.DABS(Y).LT.TOL1)GOTO 30
        IF(X.LT.TOL1)GOTO 10
        IF(DABS(Y).LT.TOL1)GOTO 20
!C
!C       SET UP BESSEL FUNCTIONS IN THE GENERAL CASE
!C
        CALL JSET(JNX,J0X,X,TOL2,MAXX,NMAX)
        CALL JSET(JNY,J0Y,Y,TOL2,MAXY,NMAX)
!C
!C       DO THE SUMS
!C
        Q=0
        CALL JSUM2(S0R,S0I,PHI,I,Q)
        Q=-1
        CALL JSUM2(SM1R,SM1I,PHI,I,Q)
        Q=1
        CALL JSUM2(S1R,S1I,PHI,I,Q)
        GOTO 40
!C
!C       CALCULATE S0,S1,SM1 WHEN X = 0.0
!C
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
!C
!C       CALCULATE S0,S1,SM1 WHEN Y = 0.0
!C
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
!C
!C       CALCULATE S0,S1,SM1 WHEN X = 0.0 AND Y = 0.0
!C
30      IF(I.EQ.1)SM1R=1.0
!C
!C       CALCULATE STOKES PARAMETER
!C
40      AXR=((2.0*S0R*ALPHA*COSphi1)-(ky1*(S1R+SM1R)))*I/A
        AXI=((2.0*S0I*ALPHA*COSphi1)-(ky1*(S1I+SM1I)))*I/A
        AYR=((2.0*S0R*ALPHA*SINphi1)+(kx1*(S1I-SM1I)))*I/A
        AYI=((2.0*S0I*ALPHA*SINphi1)-(kx1*(S1R-SM1R)))*I/A
        S0=AXR*AXR+AXI*AXI+AYR*AYR+AYI*AYI
        S1=AXR*AXR+AXI*AXI-AYR*AYR-AYI*AYI
        S2=2.0*(AXR*AYR+AXI*AYI)
        S3=2.0*(AXI*AYR-AXR*AYI)
        RETURN
        END subroutine
!C
!C       --------------------------------------
        SUBROUTINE JSUM1(X,Y,S1,S2,I,maxx1,maxy1)
!C       --------------------------------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)

        implicit none

        double precision x,y,s1,s2
        integer i,maxx1,maxy1

!        double precision jx,jy  ! functions
        double precision sign1,j1,j2,j3
        integer n,n1,n2

!C
!C       CALCULATE SUMS S1 AND S2
!C
        S1=0.0
        IF(I.LE.maxx1)S1=JY(0)*JX(I)
        S2=0.0
        sign1=1.0
        DO 10 N=1,maxy1
        sign1=-sign1
        N1=(2*N)+I
        N2=(-2*N)+I
        J1=JY(N)
        J2=JX(N1)
        J3=JX(N2)
        S1=S1+(J1*(J2+(J3*sign1)))
        S2=S2+(N*J1*(J2-(J3*sign1)))
10      CONTINUE
        RETURN
        END subroutine
!C
!C       ------------------------------
        SUBROUTINE JSUM2(SR,SI,PHI,I,Q)
!C       ------------------------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,J,K,O-Z)

        implicit none

        double precision sr,si,phi
        integer i,q

!       double precision jx,jy  ! functions
        double precision f
        integer n,p

!C        DOUBLE PRECISION jnx,jny,j0x,j0y
!C        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
!C        include 'tmp.h'

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
        END subroutine
!C
!C       -----------------------------
        double precision FUNCTION SINC(ALP2,ALP2I,R,N)
!C       -----------------------------
!C        IMPLICIT DOUBLE PRECISION (A-H,K,L,O-Z)
        implicit none
        double precision alp2,alp2i,r
        integer n

        double precision x,pi

        DATA PI/3.141592653589/
!C
!C       CALCULATE THE LINESHAPE FUNCTION
!C
        SINC=1.0
        X=N*PI*(ALP2-ALP2I)/R
        IF(DABS(X).LT.1.0D-06)RETURN
        SINC=DSIN(X)/X
        SINC=SINC*SINC
        RETURN
        END function
!C
!C       --------------
        double precision FUNCTION JX(N)
!C       --------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)

        implicit none 
        integer n
        integer n1
        double precision sign1

!C        DOUBLE PRECISION jnx,jny,j0x,j0y
!C        DOUBLE PRECISION jx
!C        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY
!C        include 'tmp.h'

        n1 = -1111
        JX=0.0
!C        IF(IABS(N).GT.MAXX)RETURN
        IF(IABS(N).GT.MAXX) GO TO 8000
        IF(N.NE.0)GOTO 10
        JX=J0X
!C        RETURN
        GO TO 8000
10      sign1=1.0
        N1=N
        IF(N.GT.0)GOTO 20
        N1=-N
        IF(N1.NE.((N1/2)*2))sign1=-1.0
20      JX=JNX(N1)*sign1
8000    continue
        RETURN
        END function
!C
!C       --------------
        double precision FUNCTION JY(N)
!C       --------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)

        implicit none 
        integer n
        integer n1
        double precision sign1

!C        DOUBLE PRECISION jnx,jny,j0x,j0y
!C        DOUBLE PRECISION jy
!C        COMMON/JDATA/JNX(1000),JNY(1000),J0X,J0Y,MAXX,MAXY

        JY=0.0
        IF(IABS(N).GT.MAXY)RETURN
        IF(N.NE.0)GOTO 10
        JY=J0Y
        RETURN
10      sign1=1.0
        N1=N
        IF(N.GT.0)GOTO 20
        N1=-N
        IF(N1.NE.((N1/2)*2))sign1=-1.0
20      JY=JNY(N1)*sign1
        RETURN
        END function
!C
!C       ----------------------------------------
        SUBROUTINE JSET(jnx1,j0x1,X0,TOL,max11,NMAX)
!C       ----------------------------------------
!Csrio        IMPLICIT DOUBLE PRECISION (A-H,J,O-Z)

        implicit none 
        double precision jnx1,j0x1
        double precision x0,tol
        integer max11,nmax

        double precision bigno,bigni
        double precision bj,bjn,bjp,sign1,tox,x,sum1
        integer i,isum,m,n,n1

!C        DOUBLE PRECISION jnx,jny,j0x,j0y

        DIMENSION jnx1(1)
        DATA BIGNO/1.0D+10/,BIGNI/1.0D-10/
!C
!C       SET UP ARRAY OF BESSEL FUNCTIONS UP TO ORDER MAX
!C       SUCH THAT JMAX (X) < TOL AND MAX > X
!C       USING MILLER'S DOWNWARD RECURRENCE ALGORITHM
!C       MODIFIED FORM OF NUMERICAL RECIPIES ROUTINE BESSJN
!C       NB] ARGUMENT CAN BE NEGATIVE
!C
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
!C
        TOX=2.0/X
        ISUM=0
        sum1=0.
        BJP=0.
        BJ=1.
        jnx1(M)=1.0
        DO 10 N1=M,1,-1
        N=N1-1
        BJN=N1*TOX*BJ-BJP
        BJP=BJ
        BJ=BJN
        IF(DABS(BJ).GT.BIGNO)THEN
          BJ=BJ*BIGNI
          BJP=BJP*BIGNI
          sum1=sum1*BIGNI
          DO 20 I=N1,M
          jnx1(I)=jnx1(I)*BIGNI
20        CONTINUE
        ENDIF
        IF(N.NE.0)jnx1(N)=BJ
        IF(ISUM.NE.0)sum1=sum1+BJ
        ISUM=1-ISUM
10      CONTINUE
        sum1=(2.0*sum1)-BJ
        j0x1=BJ/sum1
        sign1=1.0
        DO 30 N=1,M
        sign1=-sign1
        jnx1(N)=jnx1(N)/sum1
        IF(X0.LT.0.0)jnx1(N)=jnx1(N)*sign1
        IF(N.LE.DABS(X).OR.jnx1(N).GT.TOL) then 
            max11=N
        endif
30      CONTINUE
        RETURN
999     WRITE(6,9999)
        print *,'Error: JSET'
        return
        ! STOP
9999    FORMAT(//' *** OVERFLOW OF BESSEL FUNCTION ARRAY ***')
        END subroutine JSET

!
! added auxiliary routine by srio
!
        SUBROUTINE GetFromNml( period1,kx1,ky1,N,ENERGY, &
           CUR,NPHI,dalpha1,NE,NT,NP)

        !IMPLICIT REAL*8 (A-H,O-Z)
        implicit none

        double precision, intent(inout)    :: period1,kx1,ky1
        double precision, intent(inout)    :: ENERGY,CUR,dalpha1
        integer, intent(inout) :: N,NPHI

        integer :: NCOMP,ICOMP
        double precision :: RCURR,BPASS
        integer :: IANGLE,IAPERTURE,IEXTERNAL
        character(len=1024) :: FOUT,FIN,FTRAJ,FINT
        double precision :: EMIN,EMAX
        double precision :: THEMIN,THEMAX,PHIMIN,PHIMAX
        integer :: NE,NT,NP,NCHECK,IOPT,ITER,IPASS
        integer :: I_EDIV,IINT
        double precision  :: EDIVX,EDIVY

        integer :: i_device, n0
        double precision :: rlau, oldener, rk, rkx, rlen

        NAMELIST /PARAIN/ NCOMP,RCURR,ICOMP,BPASS,  &
           IANGLE,IAPERTURE,IEXTERNAL,  &
           FOUT,FIN,FTRAJ,EMIN,EMAX,  &
           THEMIN,THEMAX,PHIMIN,PHIMAX,  &
           NE,NT,NP,NCHECK,IOPT,ITER,IPASS,  &
           I_EDIV,EDIVX,EDIVY,FINT,IINT
           
        NAMELIST /EPATH1/ I_DEVICE, N0, RLAU, OLDENER, RK, RKX, RLEN
!

!C
!C Read in the parameters from namelist files
!C
        OPEN (21, FILE='uphot.nml', STATUS='OLD')
        READ (21, NML=PARAIN)
        write(6,NML=PARAIN) 
        CLOSE (21)

        OPEN (21, FILE='epath.nml', STATUS='OLD')
        READ (21, NML=EPATH1)
        write(6,NML=EPATH1) 
        CLOSE (21)

        period1 = RLAU
        kx1 = 0
        ky1 = rk
        n = ncomp
        energy = oldener
        cur = rcurr
        nphi = 0
        dalpha1 = 0.0D0
        
!C
!C Read in the (energy, theta, phi) array
!C
        END SUBROUTINE GetFromNml


end module shadow_pre_sync_urgent
