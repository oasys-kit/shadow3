












































!----
!----
!---- MODULE:  shadow_Pre_Sync
!----
!---- Preprocessors for synchrotron insertion devices
!---- Contains: 
!----
!----
!----
!---- Example of usage: 
!----
!----

Module shadow_Pre_Sync
    !---- Use Modules ----!

    use stringio
    use shadow_math
    use shadow_beamio, only: beamWrite ! for emittance_test
    use shadow_globaldefinitions
    use shadow_synchrotron, only : srcdf ! needed by nphoton only

    !---- Variables ----!
    implicit none

    !todo: fix this repetition
    !
    ! the global variables here are only used for undulator and not for wiggler
    !

    !todo: check physical constants
    !DONE srio@esrf.eu 20131211 NIST codata values. Bug fixed (inconsistent
    ! values of c in undulators).

    ! UPDATED AND MOVED TO GLOBAL DEFINITIONS srio@esrf.eu 20160527
    ! real(kind=skr),parameter :: codata_c  = 2.99792458D8 !speed of light, m/s
    ! real(kind=skr),parameter :: codata_rm = 9.10938291D-31   !electron rest mass  kg
    ! real(kind=skr),parameter :: codata_e  = 1.602176565D-19  !electron charge, C
    ! real(kind=skr),parameter :: codata_h  = 6.62606957D-34   !Planck's constant   joules*sec
    ! real(kind=skr),parameter :: codata_mee  = 0.51099892   ! electrom Mass equivalent in MeV
    ! real(kind=skr),parameter :: codata_electric_permittivity = 8.854187817D-12 ! electric constant epsilon0


    !TODO: make NDIM* allocatable
    !      (it is already done for NDIM_{A,E} in shadow_synchrotron.F90)
    ! IF CHANGED THESE NUMBERS, CHANGE ALSO cdf_z.f (Urgent code)!!
    !DONE allocatable arrays!
    integer(kind=ski),parameter :: NDIM_TRAJ=1001 ! nr of points of e- trajectory
    integer(kind=ski),parameter :: use_undulator_binary_files = 0 ! 0=ASCII, 1=BINARIES

    character(len=sklen) :: FOUT,FIN,FTRAJ,FINT

    integer(kind=ski)  :: N0,NPointId
    real(kind=skr)     :: RLAU,ENERGY1,RLA1,RK,GA0
    real(kind=skr)     :: BETA0,BETAX0,BETAY0,BETAZ0,B0,ER,RLEN
    real(kind=skr)     :: PHI_E,THE_E,TAU,Z0,ZSTEP,ETAU,EZ0
    real(kind=skr)     :: EZSTEP
    integer(kind=ski)  :: NCOMP,ICOMP,IANGLE,IAPERTURE,IEXTERNAL,IOPT
    integer(kind=ski)  :: IPASS,ITER,IINT,I_EDIV
    real(kind=skr)     :: RCURR,BPASS,BDEL
    real(kind=skr)     :: EDIVX,EDIVY
    integer(kind=ski)  :: NE,NT,NP,NCHECK
    real(kind=skr),dimension(NDIM_TRAJ) :: XOFZ,TOFZ,Z,BETAX,BETAZ
    real(kind=skr),dimension(NDIM_TRAJ) :: XOFZ1,TOFZ1,Z1,BETAX1,BETAZ1
    real(kind=skr),dimension(NDIM_TRAJ) :: XOFZ2,TOFZ2,Z2,BETAX2,BETAZ2
    real(kind=skr)     :: emin,emax,estep,phimin,phimax,phistep
    real(kind=skr)     :: themin,themax,thestep,TOTPOWER
    real(kind=skr)     :: mx11,mx22,mx12,mz11,mz22,mz12
    !

    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    !    public ::
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public ::  epath   ! wiggler+undulator
    public ::  epath_b ! wiggler
    public ::  nphoton ! wiggler
    public ::  undul_set, undul_phot, undul_cdf  ! undulator
    public ::  undul_phot_dump ! undulator, create nphoton.spec, and ascii version of nphoton.dat
    public ::  wiggler_spectrum, emittance_test ! wiggler


    !---- List of private functions ----!
    !---- List of private subroutines ----!


    !---- Definitions ----!


    !---- Interfaces ----!


  Contains
    !
    !---- Routines ----!
    !


!
! START WIGGLER AND UNDULATOR PREPROCESSORS
!

!C+++
!C	SUBROUTINE	EPATH
!C
!C	PURPOSE		To compute the trajectory of an electron through
!C			a wiggler/undulator. Uses only one period.
!C
!C	ALGORITHM	Uses Simpson rule as implementesd in QSF
!C
!C	COMMENTS	Notice that z is along the axis of the undulator,
!C			y is vertical.
!C
!C	CREATION DATE	7/87
!C
!C--
SUBROUTINE EPath(i_device)
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none

        CHARACTER(len=sklen) :: OUTFILE,UNOUTFILE,PARFILE,TRAJFILE
        CHARACTER(len=sklen) :: UNEXAM,stmp
        real(kind=skr),dimension(NDIM_TRAJ) :: XOFZ,Z,TOFZ,YOFZ
        real(kind=skr),dimension(NDIM_TRAJ) :: BETAX, BETAZ,BETAY
        real(kind=skr),dimension(NDIM_TRAJ) :: YX,YT,CURV,YY
        real(kind=skr),dimension(NDIM_TRAJ) :: EXOFZ,EZ,ETOFZ
        real(kind=skr),dimension(NDIM_TRAJ) :: EBETAX,EBETAZ
        real(kind=skr),dimension(3)    :: VEL,ACC
        real(kind=skr),dimension(3)    :: B,EN,T
        real(kind=skr),dimension(NDIM_TRAJ) :: TAUX,TAUY,TAUZ
        real(kind=skr),dimension(NDIM_TRAJ) :: ENX,ENY,ENZ
        real(kind=skr),dimension(NDIM_TRAJ) :: BX,BY,BZ

        real(kind=skr)    ::  b0x, b1x, b2x, bbb, beta02, betaxmax, betaymax, bf
        real(kind=skr)    ::  c, c2, c3, con, d2dxd2dy, d2xdz2, d2ydz2, dnum, dxdz, dydz
        real(kind=skr)    ::  e, emc, ener, h, oldener, phi, phif, rb0, rkx, rlen_pi, rm
        real(kind=skr)    ::  start_len, ttt, u_kvec, xmax, xtemp, zcorr, zf

        integer(kind=ski) ::  i_device, i, i_wig, j, nc, nfile
!
! srio: writes also epath.nml for further use these parameters 
! in urgent_cdf
!
!TODO: change namelist by gfile

        NAMELIST /EPATH1/  I_DEVICE, N0, RLAU, OLDENER, RK, RKX, RLEN
!C
!C
!C
        c  = codata_c
        rm = codata_rm
        e  = codata_e
        h  = codata_h


        c2 = e/(TWOPI*rm*c)   !unit(coul*sec/(kg*m))
! srio moved this to upper level
!!c
!!c	Specify Undulator
!!c
        IF ((I_DEVICE /= 1) .and. (I_DEVICE /= 2)) THEN
            WRITE(6,*) ' '
            WRITE(6,*) 'Type of Insertion Device.'
            WRITE(6,*) 'Enter: '
            WRITE(6,*) 'for wiggler   (large K)      [ 1 ]'
            WRITE(6,*) 'for undulator (small K)      [ 2 ]'
            I_DEVICE = IRINT ('Then ? ')
        END IF
        WRITE(6,*) ' '
        !WRITE(6,*) '----------------------------------------------------------------------'
        !WRITE(6,*) ' '
        !WRITE(6,*) ' '
        WRITE(6,*) 'Define Insertion Device parameters:'
        WRITE(6,*) ' '
        if (i_device.eq.1) then
            n0 = irint  ('Enter number of periods : ')
!C
!C       (I_WIG.EQ.1) implies normal wiggler
!C       (I_WIG.EQ.2) implies elliptical wiggler
!C
            WRITE(6,*) ' '
            WRITE(6,*) 'Type of Wiggler.'
            WRITE(6,*) 'Enter: '
            WRITE(6,*) 'for normal wiggler     [1]'
            WRITE(6,*) 'for elliptical wiggler [2]'
            I_WIG = IRINT ('Then ? ')
        end if
        rlau = rnumber('      wavelength of insertion device (m) : ')
        if (i_wig.eq.2) then
            RK = rnumber('   deflection parameter KZ for the vertical field conponent: ')
            RKX = rnumber('   deflection parameter KX for the horizontal field component: ')
        else
            RK = rnumber('      deflection parameter K : ')
        end if
        ener =  rnumber('      electron energy (GeV) : ')
        WRITE(6,*) '      Enter the number of points to be used in the trajectory calculation.'
        write(stmp,'(I8)') NDIM_TRAJ
        np  =  irint  ('      ( max = '//trim(stmp)//', suggested 101 ) : ')
        Rb0 =  rnumber('End correction field factor (0-1) : ')
!c	Electron Trajectory Parameters
!c
        WRITE(6,*) ' '
        WRITE(6,*) ' '
        WRITE(6,*) 'Two files will be created. One will contain a ', &
            ' record of the parameters used in the calculation, the other', &
            ' the trajectory itself. The names of the files can be specified', &
            ' by the user, e.g., xshwig.par and xshwig.traj.'
        WRITE(6,*) ' '
        WRITE(6,*) 'Output files specification:'
        WRITE(6,*) ' '
        parfile     = rstring('      Name for parameter file: ')
        if (i_device.eq.1) then
            TRAJFILE  = RSTRING('      Name for WIGGLER trajectory file: ')
        else
            unoutfile = rstring('      Name for UNDULATOR trajectory file: ')
            nfile     = IYES('      Do you want a plottable file [ Y/N ] ? ')
            if (nfile.eq.1) unexam = rstring ('      Name for plottable file: ')
        end if
        WRITE(6,*) ' '
!c
!c
!c	Compute General Parameters
!c
!c	n0	number of periods
!c	rlau	wavelength of undulator	(m)
!c	energy1 fundamental energy (joule)
!c	rla1	fundamental wavelength (m)
!c	rk	deflection parameter
!c	rkx     deflection parameter in the x direction (i_wig.eq.2)
!c	rlen    length of undulator along z (m)
!c	ener	electron energy (joule)	
!c	b0	peak magnetic field (Tesla)
!c       b0x     trasverse peak magnetic field (Tesla)(i_wig.eq.2)
!c
!c   in the following we will be using the PEAK value of B0 explicitely.
!c
        OLDENER = ENER         !GeV
        ENER    = E*ENER*1.0D9      !CHANGE UNITS (TO JOULE)
        GA0     = ENER/RM/C**2      !Gamma_0, in electron rest mass
                                    !energy (mc^2) units,
                                    !adimensional
        RLEN   = N0*RLAU
        BETA02 = 1.0D0 - (RM*C**2/ENER)**2
        BETA0  = SQRT(BETA02)
        B0     = RK*(TWOPI*RM*C*BETA0)/E/RLAU
        if (i_wig.eq.2) then
            B0X = RKX*(TWOPI*RM*C*BETA0)/E/RLAU
        end if
!C   RK     = E/(TWOPI*RM*C*BETA0)*B0*RLAU !As defined by S.Krinsky; reduces
!C                     !to simpler form when v ~ c
        RLA1   = RLAU/(2*GA0**2)*(1.D0 +(RK**2)/2)
        ENERGY1= H*C/RLA1      !units (joules)
        emc    = e/(ga0*rm*c)      !units ( coul*sec/(kg*m) )
        betaxmax  = -emc*b0*rlau/twopi   !constant used in external function
        betaymax  = -emc*b0x*rlau/twopi   !constant used in external function
        U_KVEC  = twopi/rlau      !   "         "
        RLEN_PI  = pi*rlen/rlau      !   "         "
        betax0 = 0.0d0 
        if (i_wig.eq.2) then
            betay0 = 0.0d0
        end if
        betaz0 = sqrt (beta02)
!C
!C np must be an odd number for Simpson rule.
!C
        i = np/2
        np = i*2 + 1
!C
!C Write out parameter file
!C
        OPEN   (20, FILE=PARFILE, STATUS='UNKNOWN')
        REWIND  (20)
        WRITE (20,*) 'Parameters used for run creating'
        IF (I_DEVICE.EQ.1) THEN
            WRITE (20,*) 'WIGGLER case. Trajectory stored in:'
            WRITE (20,*) TRAJFILE
            WRITE (20,*) 'Number of periods               = ',N0
        ELSE
            WRITE (20,*) 'UNDULATOR case. Trajectory stored in:'
            WRITE (20,*) UNOUTFILE
            if (nfile.eq.1) then
                WRITE (20,*) 'and in (formatted file): '
                WRITE (20,*) UNEXAM
            end if
            WRITE (20,*) 'The UNDULATOR case uses 1 period only.'
        END IF
        WRITE (20,*) '------   INPUT    ---------'
        WRITE (20,*) 'Insertion device  Wavel.  [ m ] = ',RLAU
        WRITE (20,*) 'Beam Energy             [ GeV ] = ',OLDENER
        WRITE (20,*) 'K                               = ',RK
        IF (I_WIG.EQ.2) THEN
            WRITE (20,*) 'KX                              = ',RKX
        END IF
        WRITE (20,*) 'Field correction factor         = ',rb0
        WRITE (20,*) '------   OUTPUT    ---------'
        WRITE (20,*) 'Gamma                           = ',GA0
        IF (I_WIG.EQ.2) THEN
            WRITE (20,*) 'Vertical Peak Magnetic field   [ Tesla ] = ',B0
            WRITE (20,*) 'Horizontal Peak Magnetic Field [ Tesla ] = ',B0X
        ELSE 
            WRITE (20,*) 'Peak Magnetic field   [ Tesla ] = ',B0
        END IF
        WRITE (20,*) 'Fundamental              [ Ev ] = ',ENERGY1/E
        WRITE (20,*) 'Fundamental       [ Angstroms ] = ',RLA1*1.0d10
        XTEMP   =  186.0D0/(10.D0*B0*OLDENER**2)
        WRITE (20,*) 'Equivalent SR C.W.     [ Angs ] = ',XTEMP
        WRITE (20,*) '              C.E.       [ eV ] = ',TOANGS/XTEMP
        CLOSE (20)
        print *,'File written to disk: '//trim(parFile)

!
! srio: writes also epath.nml for further use these parameters 
! in urgent_cdf
!
        OPEN   (21, FILE='epath.nml', STATUS='UNKNOWN')
        WRITE   (21, NML=EPATH1)
        CLOSE   (21)
        print *,'File written to disk: epath.nml'


!C
!C   Calculate x, y, t as functions of z
!C       Initial values of parameters for both wiggler cases.
!C
if (i_device.eq.1) then         !Wiggler
        !C
        Z(1)    = 0.0D0
        if (i_wig.eq.2) then                  !Elliptical Wiggler
            BETAX(1) = BETAX0
            BETAY(1) = BETAYMAX
            BETAZ(1) = SQRT(BETA0**2-BETAX(1)**2-BETAY(1)**2)
            XOFZ(1)  =0.0D0
            YOFZ(1)  =0.0D0
            TOFZ(1)  =0.0D0
            YX(1)    = BETAX(1)/BETAZ(1)
            YY(1)    = BETAY(1)/BETAZ(1)
            YT(1)    = 1/BETAZ(1)
            DXDZ     = BETAX(1)/BETAZ(1)
            DYDZ     = BETAY(1)/BETAZ(1)
            D2XDZ2   = BETAZ(1)**4
            D2DXD2DY = (BETAZ(1)*BETAY(1))**2
            DNUM     = SQRT(D2XDZ2+D2DXD2DY)*EMC*B0/(BETAZ(1)**3)
            CURV(1)  = DNUM/(1.0D0+DXDZ**2+DYDZ**2)**1.5D0
            VEL(1)   = DXDZ
            VEL(2)   = DYDZ
            VEL(3)   = 1.0D0
            CALL NORM (VEL,T)
            TAUX(1)  = T(1)
            TAUY(1)  = T(2)
            TAUZ(1)  = T(3)
            ACC(1)   = -EMC*B0*BETAZ(1)
            ACC(2)   = 0.0D0 
            ACC(3)   = 0.0D0
            CALL CROSS (VEL,ACC,B)
            CALL NORM (B,B)
            BX(1)    = B(1)
            BY(1)    = B(2)
            BZ(1)    = B(3)
            CALL CROSS (B,T,EN)
            ENX(1)    = EN(1)
            ENY(1)    = EN(2)
            ENZ(1)    = EN(3)
        else                    !Normal Wiggler
            !C  BETAX(1) = 0.0D0
            BETAX(1) = BETAX0
            BETAZ(1) = SQRT(BETA0**2-BETAX(1)**2)
            XOFZ(1)  = 0.0D0
            TOFZ(1)  = 0.0D0
            YX(1) = BETAX(1)/BETAZ(1)
            YT(1) = 1/BETAZ(1)
            DXDZ     = BETAX(1)/BETAZ(1)
            D2XDZ2   = BETA0**2/(BETAZ(1)**3)*EMC*B0
            CURV(1)  = D2XDZ2/(1.0D0+DXDZ**2)**1.5D0
        end if
        !C
        !C  Loop generates trajectory from Z=0 to rlau.
        !C  The base vectors T, B, and N of the edges of a trihedron
        !C  moving along the trajectory define the coordinate system for
        !C  the cross field wiggler.
        !C
        ZSTEP = RLAU/(NP-1)
        WRITE(6,*) ' '
        WRITE(6,*) 'Trajectory Calculations begins.'
        WRITE(6,*) ' '
        DO I  = 2,NP
            Z(I) = Z(1) + (I-1)*ZSTEP
            !C
            !C  Calculate betax, betay, betaz as functions of z (here z is the 
            !C  direction of propagation).
            !C
            IF (I_WIG.EQ.2)  THEN
                BETAX(I) = BETAXMAX*SIN(U_KVEC*Z(I)) + BETAX0
                BETAY(I) = BETAYMAX*COS(U_KVEC*Z(I)) + BETAY0
                BETAZ(I) = SQRT(BETA0**2-BETAX(I)**2-BETAY(I)**2)
                YX(I)    = BETAX(I)/BETAZ(I)
                YY(I)    = BETAY(I)/BETAZ(I)
                YT(I)    = 1.0D0/BETAZ(I)
                DXDZ     = BETAX(I)/BETAZ(I)
                DYDZ     = BETAY(I)/BETAZ(I)
                D2XDZ2   = (BETAZ(I)*B0*COS(TWOPI*Z(I)/RLAU))**2
                D2YDZ2   = (BETAZ(I)*B0X*SIN(TWOPI*Z(I)/RLAU))**2
                D2DXD2DY = (BETAX(I)*B0X*SIN(TWOPI*Z(I)/RLAU)+BETAY(I)*B0* &
                COS(TWOPI*Z(I)/RLAU))**2
                DNUM     = SQRT(D2XDZ2+D2YDZ2+D2DXD2DY)/(BETAZ(I)**2)*EMC
                CURV(I)  = DNUM/(1.0D0+DXDZ**2+DYDZ**2)**1.5D0
                VEL(1)   = DXDZ
                VEL(2)   = DYDZ
                VEL(3)   = 1.0D0
                CALL NORM (VEL,T)
                TAUX(I)  = T(1)
                TAUY(I)  = T(2)
                TAUZ(I)  = T(3)
                ACC(1)   = -EMC*B0*BETAZ(I)*COS(TWOPI*Z(I)/RLAU)/BETAZ(I)
                ACC(2)   = EMC*B0*BETAZ(I)*SIN(TWOPI*Z(I)/RLAU)/BETAZ(I) 
                ACC(3)   = 0.0D0
                CALL CROSS (VEL,ACC,B)
                CALL NORM (B,B)
                BX(I)    = B(1)
                BY(I)    = B(2)
                BZ(I)    = B(3)
                CALL CROSS (B,T,EN)
                ENX(I)    = EN(1)
                ENY(I)    = EN(2)
                ENZ(I)    = EN(3)
                !C  TAU=SQRT(TAUX(I)**2+TAUY(I)**2+TAUZ(I)**2)
                !C  BI=SQRT(BX(I)**2+BY(I)**2+BZ(I)**2)
                !C  ENNE=SQRT(ENX(I)**2+ENY(I)**2+ENZ(I)**2)
                !C
          ELSE
                BETAX(I) = BETAXMAX*SIN(U_KVEC*Z(I)) + BETAX0
                BETAZ(I) = SQRT(BETA0**2-BETAX(I)**2)
                YX(I)    = BETAX(I)/BETAZ(I)
                YT(I)    = 1.0D0/BETAZ(I)
                DXDZ     = BETAX(I)/BETAZ(I)
                D2XDZ2   = BETA0**2/(BETAZ(I)**3)*EMC*B0*COS(TWOPI*Z(I)/RLAU)
                CURV(I)  = D2XDZ2/(1.0D0+DXDZ**2)**1.5D0
          ENDIF
        END DO 
!19	CONTINUE
!C
        IF (I_WIG.EQ.2) THEN 
            CALL QSF ( ZSTEP,YX,XOFZ,NP)
            CALL QSF ( ZSTEP,YY,YOFZ,NP)
            CALL QSF ( ZSTEP,YT,TOFZ,NP)
        ELSE
            CALL QSF ( ZSTEP,YX,XOFZ,NP)
            CALL QSF ( ZSTEP,YT,TOFZ,NP)
        END IF
!C
!C Write out trajectory file in SHADOW's frame. To transform to SHADOW frame, 
!C -X -> X, Y -> Z, Z -> Y. All periods are written out.
!C
        WRITE(6,*) ' '
        WRITE(6,*) 'Calculation Completed. File out results.'     
        WRITE(6,*) ' '
        OPEN        (40,FILE=TRAJFILE,STATUS='UNKNOWN')
        REWIND (40)
        !DO 49 J = 1, N0
        DO J = 1, N0
             START_LEN        = ((J-1)-N0*0.5D0)*RLAU
             !DO 59 I = 1, NP-1
             DO I = 1, NP-1
                 IF (I_WIG.EQ.2) THEN
                     WRITE    (40,*) &
                        -XOFZ(I),Z(I)+START_LEN,YOFZ(I), &
                        -BETAX(I),BETAZ(I),BETAY(I),CURV(I)
                 ELSE
                     WRITE        (40,*)         &
                      -XOFZ(I),Z(I)+START_LEN,0.0d0, &
                      -BETAX(I),BETAZ(I),0.0d0,CURV(I)
                 ENDIF
             END DO 
!59           CONTINUE
        END DO 
!49      CONTINUE
        IF (I_WIG.EQ.2) THEN
            WRITE (40,*) &
                -XOFZ(NP),Z(NP)+START_LEN,YOFZ(NP), &
                -BETAX(NP),BETAZ(NP),BETAY(NP),CURV(NP)
        ELSE
            WRITE (40,*)         &
                -XOFZ(NP),Z(NP)+START_LEN,0.0d0, &
                -BETAX(NP),BETAZ(NP),0.0d0,CURV(NP)
        ENDIF
        CLOSE (40)
        print *,'File written to disk: '//trim(trajFile)

        RETURN
!C
else if (i_device.eq.2) then    !Undulator
        !C
        !C	write to file for use in ERAD
        !C
        OPEN (32,FILE=UNOUTFILE,STATUS='UNKNOWN',FORM='UNFORMATTED')
        REWIND (32)
        WRITE(32) N0,RLAU,ENERGY1
        WRITE(32) RLA1,RK,GA0,BETA0
        WRITE(32) BETAX0,BETAY0,BETAZ0
        WRITE(32) B0,ENER,RLEN,NP,rb0
        WRITE(32) PHI_E,THE_E
!C
!C (N-1) periods assume ideal sinusoidal field and trajectory.
!C
        c3        = -e/(ga0*rm*c)*rlau/twopi
        Z0        = RLAU/2.0D0
        ZSTEP        = RLAU/(NP-1)
        !C
        !DO 95 I        = 1,NP
        DO I = 1,NP
           Z(I)        = (I-1)*ZSTEP
           BETAX(I) = C3*B0*SIN(U_KVEC*Z(I))
           BETAZ(I) = SQRT(BETA0**2-BETAX(I)**2)
           YT(I)    = 1/BETAZ(I)
           YX(I)    = BETAX(I)/BETAZ(I)
        END DO
!95        CONTINUE
!C
        CALL QSF (ZSTEP,YX,XOFZ,NP)
        CALL QSF (ZSTEP,YT,TOFZ,NP)

        TAU = (TOFZ(NP) - TOFZ(1))/C
!C
!C The two ends of the undulator are combined into the remaining (one)
!C period, with the length of RLAU (1+sqrt(2)).
!C

        c3        = -e/(ga0*rm*c)*rlau/twopi
        ZF        = RLAU/4.0D0
        ZCORR     = SQRT(2.0D0)*RLAU/4.0D0
        EZ0       = ZF + ZCORR
        EZSTEP  = 2*EZ0/(NP-1)
        BF      = -B0/SQRT(2.0D0)

        PHIF    = U_KVEC*ZF

        !DO 29 I  = 1,NP
        DO I  = 1,NP
            EZ(I)  = -EZ0 + ( I-1 )*EZSTEP
 !C
 !C include modifications for fringe field.
 !C
            PHI = U_KVEC*EZ(I)
            IF (ABS(EZ(I)).GT.ZF) THEN
                con = c3*b0/2.0d0
                B1X = C3*BF/SQRT(2.0D0)* &
                               ( -COS(SQRT(2.0D0)*(ABS(PHI)-PHIF)) ) + con
                IF (EZ(I).LT.0.0)        B1X = -B1X
                B2X = C3*B0*SIN(U_KVEC*EZ(I))
 
                IF (ABS(EZ(I)).GT.(2.0D0*ZF)) THEN
                    EBETAX(I)        = RB0*B1X
                ELSE
                    EBETAX(I)        = RB0*B1X + (1.0D0-RB0)*B2X
                ENDIF
            ELSE
                 EBETAX(I) = C3*B0*SIN( U_KVEC*EZ(I) )
            END IF
            EBETAZ(I) = SQRT(BETA0**2-EBETAX(I)**2)
            YT(I)    = 1/EBETAZ(I)
            YX(I)    = EBETAX(I)/EBETAZ(I)
        END DO
!29	CONTINUE
!C
        CALL QSF ( EZSTEP,YX,EXOFZ,NP)
        CALL QSF ( EZSTEP,YT,ETOFZ,NP)
           
        ETAU = ( ETOFZ(NP) - ETOFZ(1) )/C
        NC        = (NP+1)/2.0D0
        XMAX = EXOFZ(NC)                ! maximum fluctuation in X.
!C
        WRITE(32) TAU,Z0,ZSTEP
        DO 69 I = 1, NP
            WRITE(32) XOFZ(I)+XMAX,0.0d0,Z(I)
            WRITE(32) BETAX(I),0.0d0,BETAZ(I)
            WRITE(32) TOFZ(I)/C,1.0D0 -BETAZ(I)
69      CONTINUE
        WRITE(6,*) parfile
          
        WRITE(32) ETAU,EZ0,EZSTEP,NC
!C
!C The first RLAU/2 (approx.) of the undulator:
!C
        DO 25 I = 1,NC
            WRITE(32) EXOFZ(I),0.0D0,EZ(I)        !X,Y,Z
            WRITE(32) EBETAX(I),0.0D0,EBETAZ(I)        !BETAX,BETAY,BETAZ
            WRITE(32) ETOFZ(I)/C-0.5D0*ETAU,1.0D0-EBETAZ(I)
25      CONTINUE
!C
!C The last RLAU/2 (approx.) of the undulator:
!C
        DO 35 I = NC,NP
            WRITE(32) EXOFZ(I),0.0D0,EZ(I)             !X,Y,Z
            WRITE(32) EBETAX(I),0.0D0,EBETAZ(I)        !BETAX,BETAY,BETAZ
            WRITE(32) ETOFZ(I)/C-0.D05*ETAU,1.0D0-EBETAZ(I)
35      CONTINUE

        CLOSE(32)
        print *,'File written to disk: '//trim(unOutFile)
!C
        IF (NFILE.EQ.1) THEN
            WRITE(6,*) unexam
            open (33, file=unexam, status='unknown')
            rewind(33)
            DO 79 I = 1, NP
                TTT = ETOFZ(I)/C
                BBB = 1.0D0 - EBETAZ(I)
                WRITE(33,1010) EXOFZ(I), EBETAX(I), EZ(I), BBB, TTT
79          CONTINUE
        END IF
        print *,'File written to disk: '//trim(unExam)
END IF
        CLOSE(33)
        !CLOSE(32)
1000    FORMAT (4(1X,G19.12))
1010    FORMAT (5(1X,G19.12))
1070    FORMAT (7(1X,G19.12))
1090    FORMAT (9(1X,G19.12))

        RETURN
END SUBROUTINE EPath


!
! START WIGGLER PREPROCESSORS
!


!C+++
!C	SUBROUTINE	EPATH_B
!C
!C	PURPOSE		To compute the trajectory of an electron through
!C			a wiggler given the magnetic field.
!C
!C	ALGORITHM	See formulae in ESRF red book pag. CIV-297
!C
!C	COMMENTS	Old btraj from ESRF.
!C
!C	CREATION DATE	10/91 M. Sanchez del Rio, C. Vettier
!C                      5/2014 srio@esrf.eu adapted to Shadow3
!C                      20150918 srio@esrf.eu changed sign of magnetic field
!C                               to get the right curvature for ELECTRONS!
!C
!C--
SUBROUTINE epath_b(i_device)
        implicit none

        integer(kind=ski), intent(in) ::  i_device

        real(kind=skr),dimension(:),allocatable    ::  betax,betaz,betay
        real(kind=skr),dimension(:),allocatable    ::  yx, yz, curv, yy, bx, bz
        real(kind=skr),dimension(:),allocatable    ::  bh
        integer(kind=ski),dimension(:),allocatable ::  nh

        real(kind=skr)       :: e,h,c,rm,c2
        character(len=sklen) :: outfilePar,outfileTraj,infile
        integer(kind=ski)    :: i_file,i,j,npoints,nharm,nper,i_write,n,nn
        real(kind=skr)       :: per,beta0,start_len,beta02,emc,ener
        real(kind=skr)       :: oldener,phase,phase0,yint,ystep,tmp

        c  = codata_c
        rm = codata_rm
        e  = codata_e
        h  = codata_h
        c2 = e/(TWOPI*rm*c)   !unit(coul*sec/(kg*m))

!
! input section
!
        write(6,*) 'Wiggler trajectory calculation from: '
        write(6,*) '  [0] An input file (2-column) with the magnetic field'
        write(6,*) '      of one period. 1st column: y[m]; 2nd column: B[T]'
        write(6,*) '  [1] A file with magnetic field hatmonic data:'
        write(6,*) '      1st column: n (harmonic number); 2nd column: Bn[T]' 
        i_file = irint('> ')
        infile = rstring ('Input file ? ')
        nper = irint ('Enter number of periods : ')
        oldener = rnumber  ('Electron energy [GeV] ? ')
        outfilePar  = rstring ('Output file with parameters ? ')
        outfileTraj = rstring ('Output file with the trajectory ? ')


        if (i_file.eq.0) then      !file with magnetic field Bz[T](y[m]) (vertical)
        else if (i_file.eq.1) then !file with period [m], number of periods, harmonics [T]
            npoints = irint('Enter number of points per period : ')
            per = rnumber('Enter wiggler wavelength [m] : ')
        endif


        if (i_file.eq.0) then !reads file with magnetic field Bz[T](y[m]) (vertical)
            ! gets NPOINTS from file
            open (21,file=infile,status='old')
            ! first scan the file to get the number of points
            I = 0
            DO WHILE (.true.) 
                READ (21,*,END=11)   tmp,tmp
                I = I + 1
            END DO 
11          npoints = I 
            CLOSE   (21)
            WRITE(6,*) 'Found ',npoints,' points from input file : '//trim(infile)
        else
            ! gets NHARM from file
            open (21,file=infile,status='old')
            ! first scan the file to get the number of points
            nharm = 0
            DO WHILE (.true.) 
                READ (21,*,END=110)   nn,tmp
                IF (nn .GT. nharm) nharm=nn
            END DO 
110         CONTINUE
            CLOSE   (21)
            WRITE(6,*) 'Found ',nharm,' maximum harmonic in input file : '//trim(infile)
            allocate( nh(nharm) )
            allocate( bh(nharm) )
        end if
    
        ! allocate arrays
        allocate( BETAX(NPOINTS*NPER) )
        allocate( BETAY(NPOINTS*NPER) )
        allocate( BETAZ(NPOINTS*NPER) )
        allocate( YX(NPOINTS*NPER) )
        allocate( YY(NPOINTS*NPER) )
        allocate( YZ(NPOINTS*NPER) )
        allocate( CURV(NPOINTS*NPER) )
        allocate( BX(NPOINTS*NPER) )
        allocate( BZ(NPOINTS*NPER) )

        !
        !load Bz(Y) data from file
        !
        if (i_file.eq.0) then !reads file with magnetic field Bz[T](y[m]) (vertical)
            open (21,file=infile,status='old')
            do i=1,npoints
                read (21,*) YY(i),BZ(i)
            end do
            close (21)
            ystep = yy(2)-yy(1)
            per  = yy(npoints)-yy(1)
            write(6,*) npoints ,' points read from file: '//trim(infile)
            write(6,*) ' Period of the ID is : ',per
        else if (i_file.eq.1) then !read file with period [m], number of periods, harmonics [T]
            open (21,file=infile,status='old')
            !c get harmonics from file
            do i=1,nharm
                nh(i) = i
                bh(i) = 0.0d0
            end do
            do i=1,3001
                read (21,*,end=34) n,tmp
                if (n .gt. 0) bh(n) = tmp 
            end do
34          continue
            close (21)
            !test
            !do i=1,nharm
            !    print*,'>>>>>>>>>>>>>>> ',i,nh(i),bh(i)
            !    bh(i) = 0.0d0
            !end do



            ystep = per/(npoints-1)
        end if
!c
!c	calculates initial electron speed from its energy
!c
        ENER    = E*oldENER*1.0D9        !CHANGE UNITS (TO JOULE)
        GA0     = ENER/RM/C**2                !Gamma_0, in electron rest mass
                                             !energy (mc^2) units,
                                             !adimensional
        emc = e/(ga0*rm*c)
        beta02 = 1.0d0 - (rm*c**2/ener)**2
        beta0 = sqrt(beta02)
        write(6,*) ' beta = v/c =',beta0
        start_len=per*(nper-1)/2.d0                
!c
!c        calculates the integral of the magnetic field bz (proportional
!c        to speeds)
!c        
        if (i_file.eq.0) then
         yint = 0.0d0
         betax(1) = 0.0d0
         do i=2,npoints
           ! for electron yint = yint + bz(i)
           yint = yint - bz(i)
           betax (i) = yint*ystep
         end do 
        else if (i_file.eq.1) then
         phase0=-pi
phase0=0.0
         do i=1,npoints
                yy(i)=(i-1)*ystep-per/2.d0
                bz(i)=0.d0
                betax(i)=0.d0
                do n=1,nharm
                        phase=twopi*(yy(i)/per)
                        bz(i)=bz(i)+bh(n)*dcos(phase*n)
                        ! for e-  betax(i)=betax(i)+bh(n)*(dsin(phase*n)-dsin(phase0*n))/n
                        betax(i)=betax(i)-bh(n)*(dsin(phase*n)-dsin(phase0*n))/n
                enddo
                betax(i)=betax(i)*per/twopi
         end do 
        end if
        write(6,*) ' Integral of B = ',betax(npoints), '  [Tm]'
!c
!c        rescale to speeds v/c = 0.3/E[GeV] * integral (Bz [T] ds[m])
!c
        do i=1,npoints
         betax (i) = - (0.3/oldener) * betax(i) 
         betay (i) = sqrt(beta0**2 - betax(i)**2)
         !for e-  curv(i) = -emc*bz(i)/beta0
         curv(i) = emc*bz(i)/beta0
        end do        
!c
!c        calculates positions as the integral of speeds
!c
        if (i_file.eq.0) then
         yint = 0.0d0
         do i=1,npoints
          yint = yint + betax(i)
          yx(i) = yint * ystep 
         end do 
        else if (i_file.eq.1) then
         do i=1,npoints
                yx(i)=0.d0
                do n=1,nharm
                        phase=twopi*(yy(i)/per)
                        yx(i)=yx(i)-bh(n)*(dcos(phase*n)-dcos(phase0*n))/n/n
                enddo
                yx(i)=yx(i)*(-3.d-1/oldener)*(per/twopi)**2
         end do 
        end if
!c
!c        creates parameters file
!c
        open(22,file=outfilePar,status='unknown')
          write (22,*) '########### Data from epath_b ##############'
          write (22,*) 'Period of the ID is : ',per ,' [m]'
          write (22,*) 'Number of periods : ',nper
          write (22,*) 'Number of points per periods : ',npoints
          write (22,*) 'beta = v/c =',beta0
          write (22,*) 'Electron energy [GeV] = ',oldener
        if (i_file.eq.0) then
          write(22,*) 'Magnetic field profile from file:   ',infile
          write(22,*) 'Integral of B = ',betax(npoints), '[T m]'
        else if (i_file.eq.1) then 
          write(22,*) 'Magnetic field harmonics from file '//trim(infile)
          write(22,*) 'Number of harmonics: ',nharm
        end if
        close(22)
        write(6,*) "File written to disk: "//trim(outfilePar)
!c
!c        creates trajectory file
!c
        open(22,file=outfileTraj,status='unknown')
        do j=1,nper
                nn=1
                if(j.gt.1) nn=2         ! to avoid overlap
                do i=nn,npoints
                write (22,*) YX(i),yy(i)+(j-1)*per-start_len,0.0,betax(i),betay(i),0.0d0,curv(i),bz(i)
                end do
        end do
        close(22)
        write(6,*) "File written to disk: "//trim(outfileTraj)//" containing: "
        write(6,*) "   Cols:  1      2      3      5      6      7      8          9"
        write(6,*) "          X[m]   Y[m]   Z[m]   betaX  betaY  betaZ  curv[m^-1] Bz[T]"
        !
        ! deallocate arrays
        !
        if (allocated( BETAX ) ) deallocate( BETAX )
        if (allocated( BETAY ) ) deallocate( BETAY )
        if (allocated( BETAZ ) ) deallocate( BETAZ )
        if (allocated( YX ) ) deallocate( YX )
        if (allocated( YY ) ) deallocate( YY )
        if (allocated( YZ ) ) deallocate( YZ )
        if (allocated( CURV ) ) deallocate( CURV )
        if (allocated( BX ) ) deallocate( BX )
        if (allocated( BZ ) ) deallocate( BZ )
        if (allocated( nh ) ) deallocate( nh )
        if (allocated( bh ) ) deallocate( bh )

        write(6,*) 'Done'
        RETURN
END SUBROUTINE epath_b




!C+++
!C	SUBROUTINE	NPhotonCalc
!C
!C	PURPOSE		IFLAG = -1, to read the unformatted CDF for SR 
!C			radiation, and produce the spline coefficients for it.
!C			
!C			IFLAG = 1, to compute the number of photons emitted 
!C			between	EMIN and EMAX.
!C
!C       ARGUMENTS       TOT_NUM, output, contains the # of photons
!C                       per mrad of orbit.
!C
!C                       RAD, input, radius in m of the trajectory.
!C
!C                       BENER, beam energy in GeV.
!C
!C                       EMIN, input, initial photon energy
!C
!C                       EMAX, input, final photon energy
!C
!C                       IFLAG, on input, if = -1 sets up calculations;  
!C                       if = 0, performs calculations; on output, if = 0
!C                       everything is all right; if (IFLAG.NE.0), error status.
!C---
SUBROUTINE NPhotonCalc (TOT_NUM,RAD,BENER,EIMIN,EIMAX,IFLAG)
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none

        real(kind=skr),intent(in)        :: RAD, BENER, EIMIN, EIMAX
        integer(kind=ski),intent(inout)  :: IFLAG
        real(kind=skr),intent(out)       :: TOT_NUM

        real(kind=skr),dimension(5,1010) :: PHOT_INV
        real(kind=skr),dimension(1010)   :: Y
        real(kind=skr)                   :: EMAX,EMIN,CMAX,CMIN,CDIFF

        real(kind=skr)    :: c_phot, ex_low, ex_upp, ex_step, gamma1
        real(kind=skr)    :: pnum, ang_num, r_lam
        integer(kind=ski) :: i, np, icol, ier, izero
             
        CHARACTER(len=sklen) :: SRDISTR
        SAVE        PHOT_INV,NP
!C
!C Define the useful parameters
!C
        EX_LOW   = -5.0D0        
        EX_UPP   = 2.0D0
        EX_STEP  = (EX_UPP - EX_LOW)*1.0D-3
        
        IF (IFLAG.EQ.-1) THEN
            !C
            !C Get the data file path using either SHADOW$DATA or Unix SHADOW_DATA_DIR
            !C environment variable. Also, check for existence in the routine itself.
            !C
            IFLAG = 1
            SRDISTR=""
            CALL DATAPATH ('SRDISTR', SRDISTR, IFLAG) 
            IF (IFLAG.NE.0) THEN
                print *,'File not found: SRDISTR. Creating it!'
                !CALL LEAVE ('NPhotonCalc', 'SRDISTR file not found', IFLAG)
                call srcdf
                SRDISTR = "SRDISTR"
            ENDIF
            OPEN (20, FILE=SRDISTR, STATUS='OLD',FORM='UNFORMATTED')
            READ (20) ICOL,NP
            !C
            !C Reads in the total flux distribution
            !C
            !DO 99 I = NP, 1, -1
            DO I = NP, 1, -1
                READ (20) PHOT_INV(1,I),Y(I)
                Y(I) = 1.0D0 - Y(I)
            END DO
            !99  CONTINUE
            CLOSE (20)
            !C
            !C Produces the inverted cdf curve of photon energy
            !C
            CALL CUBSPL(PHOT_INV,Y,NP,IER)
            RETURN
        ELSE
            !C
            !C Calculate the appropriate index for EIMAX and EIMIN
            !C
            !gamma1       = 1957.0D0*BENER
            gamma1       = BENER/(codata_mee*1d-3)
            R_LAM       = 4.0D0*PI*RAD/3.0D0/gamma1**3*1.0D10        !Angstroms
            C_PHOT      = TOANGS/R_LAM
            EMIN        = EIMIN/C_PHOT
            EMAX        = EIMAX/C_PHOT
            !C
            !C Interpolate for the probability between EMIN and EMAX.
            !C
            IF (EMAX.GT.10.0D0**EX_UPP) THEN
                CMAX = 1.0D0
            ELSE IF (EMAX.LT.10.0D0**EX_LOW) THEN
                iZero = 0
                CALL LEAVE ('NPhotonCalc','Maximum photon energy is too small.',iZero)
                IFLAG = -1
                RETURN
            ELSE
                CALL SPL_INT (PHOT_INV,NP,EMAX,CMAX,IER)
            END IF
            IF (EMIN.GT.10.0D0**EX_UPP) THEN
                CMIN = 1.0D0
            ELSE IF (EMIN.LT.10.0D0**EX_LOW) THEN
                iZero = 0
                CALL LEAVE ('NPhotonCalc','Minimum photon energy is too small.',iZero)
                IFLAG = -1
                RETURN
            ELSE
                CALL SPL_INT (PHOT_INV,NP,EMIN,CMIN,IER)
            END IF
            CDIFF = CMAX - CMIN
            !C
            !C G0_TOT is the total area of the G0 curve.
            !C TOT_NUM is the Photons/sec/mA/mrad between EMIN and EMAX.
            !C
            CDIFF    = CDIFF * 5.097721285019080D0
            TOT_NUM  = CDIFF * 1.013D6 * TOANGS * gamma1 
            IFLAG    = 1
            RETURN
        END IF
END SUBROUTINE NPhotonCalc

!C+++
!C	SUBROUTINE		NPHOTON
!C
!C	PURPOSE		To take the output from EPATH and compute the no. of
!C			photons generated along the trajectory. The output is
!C			a file for input to SOURCE running the wiggler case.
!C
!C	NOTE		Everything is in SHADOW's referance frame.
!C---
SUBROUTINE NPhoton
        !IMPLICIT   REAL*8   (A-H,O-Z)
        implicit none
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)

        !integer(kind=ski),parameter     ::  N_DIM=10000
        real(kind=skr),dimension(:),allocatable ::  Y,BETAY,X,BETAX
        real(kind=skr),dimension(:),allocatable ::  CURV,PHOT_NUM,PHOT_CDF
        real(kind=skr),dimension(:),allocatable ::  Z,BETAZ,DS,S
        real(kind=skr),dimension(:),allocatable ::  DX, DY, DZ
        !real(kind=skr),dimension(NDIM_TRAJ)  ::  TAUX,TAUY,TAUZ
        !real(kind=skr),dimension(NDIM_TRAJ)  ::  BX,BY,BZ
        !real(kind=skr),dimension(NDIM_TRAJ)  ::  ENX,ENY,ENZ
        character(len=sklen)            ::  INFILE,OUTFILE

        real(kind=skr)    :: ang_num, angle, angle1, angle2, bener, cdf, curv_max, curv_min
        real(kind=skr)    :: gamma1, phot_max, phot_min, pnum, rad, step, tot_num
        real(kind=skr)    :: tmp
        integer(kind=ski) :: i, i_wig, iflag, i_wig_binary 

        !C
        !C Read in the CDF of G0, and generated the spline coefficients.
        !C
        IFLAG   = -1
        CALL   NPhotonCalc (PNUM,RAD,BENER,EMIN,EMAX,IFLAG)

        WRITE(6,*) ' '
        WRITE(6,*) '************************ WIGGLER RADIATION *************************'
        WRITE(6,*) ' '
        INFILE   = RSTRING ('Name of input file with electron trajectory: ')
        !C        
        !C        (I_WIG.EQ.1) implies normal wiggler.
        !C        (I_WIG.EQ.2) implies elliptical wiggler.
        !C
        WRITE(6,*) ' '
        WRITE(6,*) 'Type of Wiggler.'
        WRITE(6,*) 'Enter:'
        WRITE(6,*) 'for normal wiggler [1] (-1 for ascii output)'
        WRITE(6,*) 'for elliptical wiggler [2] (-2 for ascii output)'
        I_WIG=IRINT('Then? ')

        if (i_wig .le. 0) then
            i_wig_binary = 0
        else
            i_wig_binary = 1
        endif

        i_wig = abs(i_wig)


        ! first scan the file to get the number of points
        OPEN   (20,FILE=INFILE,STATUS='OLD')
        I = 0
        DO WHILE (.true.) 
            READ (20,*,END=11)   tmp,tmp,tmp,tmp,tmp,tmp,tmp
            I = I + 1
        END DO 
11      NP   = I 
        CLOSE   (20)
        WRITE(6,*) 'Found ',NP,' points from input file : '//trim(infile)

        
        ! allocate arrays
        allocate( Y(NP) )
        allocate( BETAY(NP) )
        allocate( X(NP) )
        allocate( BETAX(NP) )
        allocate( CURV(NP) )
        allocate( PHOT_NUM(NP) )
        allocate( PHOT_CDF(NP) )
        allocate( Z(NP) )
        allocate( BETAZ(NP) )
        allocate( DS(NP) )
        allocate( S(NP) )
        allocate( DX(NP) )
        allocate( DY(NP) )
        allocate( DZ(NP) )


        OPEN   (20,FILE=INFILE,STATUS='OLD')

        !  DO 99 I = 1, N_DIM+1
        ! DO I = 1, N_DIM+1
        DO I = 1, NP
            !READ (20,*,END=101)   X(I),Y(I),Z(I),BETAX(I),BETAY(I),BETAZ(I),CURV(I)
            READ (20,*)   X(I),Y(I),Z(I),BETAX(I),BETAY(I),BETAZ(I),CURV(I)
        END DO 
        ! 99        CONTINUE
!        STOP    'NPhoton: Too many points from input file.'
!101     NP   = I - 1
        CLOSE   (20)
        WRITE(6,*) 'Read ',NP,' points from input file: '//trim(infile)

        STEP   = SQRT((Y(2)-Y(1))**2 + (X(2)-X(1))**2 + (Z(2)-Z(1))**2)

        !C
        !C Compute gamma and the beam energy
        !C
        gamma1   = 1/SQRT(1-(BETAY(1)**2)-(BETAX(1)**2)-(BETAZ(1)**2))
        !BENER   = gamma1*(9.109D-31)*(2.998d8**2)/(1.602d-19)*1.0d-9
        BENER   = gamma1*(codata_rm)*(codata_c**2)/(codata_e)*1.0d-9
        

         WRITE(6,*) 'Beam energy (GeV) = ',BENER
        !C
        !C Figure out the limit of photon energy.
        !C
        CURV_MAX   = 0.0D0
        CURV_MIN   = 1.0D20
        !DO 199 I = 1, NP
        DO I = 1, NP
            CURV_MAX   = MAX(ABS(CURV(I)),CURV_MAX)
            CURV_MIN   = MIN(ABS(CURV(I)),CURV_MIN)
        END DO 
        !199        CONTINUE
        WRITE(6,*) 'Radius of curvature (max.) = ',1/CURV_MIN,' m'
        WRITE(6,*) '                    (min.) = ',1/CURV_MAX,' m'
        PHOT_MIN   = TOANGS*3.0D0*gamma1**3/4.0D0/PI/1.0D10*CURV_MIN
        PHOT_MAX   = TOANGS*3.0D0*gamma1**3/4.0D0/PI/1.0D10*CURV_MAX
        WRITE(6,*) 'Critical Energy (max.) = ',PHOT_MAX,' eV'
        WRITE(6,*) '                (min.) = ',PHOT_MIN,' eV'
        !C        WRITE(6,*) 'Use photon energy between ',
        !C     $           PHOT_MAX*10,' eV and ',PHOT_MIN*1.0D-5,' eV'
        !C
        EMIN   = RNUMBER ('Initial photon energy [ eV ] : ')
        EMAX   = RNUMBER ('Final photon energy [ eV ]   : ')
        OUTFILE   = RSTRING ('Name of output file : ')
        !C
        !C NPhotonCalc computes the no. of photons per mrad (ANG_NUM) at each point.  
        !C It is then used to generate the no. of photons per axial length (PHOT_NUM)
        !C along the trajectory S.
        !C
        !DO 299 I = 1, NP
        DO I = 1, NP
          IF (ABS(CURV(I)).LT.1.0D-10) THEN
            ANG_NUM   = 0.0D0   
          ELSE
            RAD   = ABS(1.0D0/CURV(I))
            IFLAG = 1
            CALL   NPhotonCalc (ANG_NUM,RAD,BENER,EMIN,EMAX,IFLAG)
          END IF
          PHOT_NUM(I) =  &
            ANG_NUM*ABS(CURV(I))*SQRT(1+(BETAX(I)/BETAY(I))**2+ &
          (BETAZ(I)/BETAY(I))**2)*1.0D3
        END DO 
        !299     CONTINUE
        !C
        !C Computes CDF of the no. of photon along the trajectory S.
        !C In the elliptical case, the entire traversed path length (DS) is computed.
        !C In the normal case, only the component (Y) in the direction of propagation
        !C is computed.
        !C
        !DO 399 I = 2, NP
        DO I = 2, NP
        IF (I_WIG.EQ.2) THEN
            DS(1) = 0.0D0
            DX(I) = X(I) - X(I-1)
            DY(I) = Y(I) - Y(I-1)
            DZ(I) = Z(I) - Z(I-1)
            DS(I) = SQRT(DX(I)**2 + DY(I)**2 + DZ(I)**2) + DS(I-1)
            PHOT_CDF(I)   = PHOT_CDF(I-1) +  &
                (PHOT_NUM(I-1) + PHOT_NUM(I))*0.5D0*(DS(I) - DS(I-1))
        ELSE
            PHOT_CDF(I)   = PHOT_CDF(I-1) +  &
                (PHOT_NUM(I-1) + PHOT_NUM(I))*0.5D0*(Y(I) - Y(I-1))
        END IF
        ! 399     CONTINUE
        END DO 
        TOT_NUM   = PHOT_CDF(NP)
        WRITE(6,*) 'Total no.of photons = ',TOT_NUM
!C
!C Creates the binary file that serves as input to SHADOW.
!C
        if (i_wig_binary .eq. 1) then
            OPEN   (21,FILE=OUTFILE,STATUS='UNKNOWN',FORM='UNFORMATTED')
            REWIND   (21)
            WRITE (21) NP,STEP,BENER,1.0D0/CURV_MAX, 1.0D0/CURV_MIN,EMIN,EMAX
        else
            OPEN (21,FILE=OUTFILE,STATUS='UNKNOWN',FORM='FORMATTED')
            REWIND (21)
            WRITE (21,*) NP,STEP,BENER,1.0D0/CURV_MAX, 1.0D0/CURV_MIN,EMIN,EMAX
        endif
        !DO 499 I = 1, NP
        DO I = 1, NP
            CDF      = PHOT_CDF(I)/TOT_NUM
            IF (I_WIG.EQ.2) THEN
                ANGLE1  = ATAN2 (BETAX(I),BETAY(I))
                ANGLE2  = ASIN  (BETAZ(I))
                if (i_wig_binary .eq. 1) then
                    WRITE (21) X(I),Y(I),Z(I),CDF,ANGLE1,ANGLE2, CURV(I)
                else
                    WRITE (21,*) X(I),Y(I),Z(I),CDF,ANGLE1,ANGLE2, CURV(I)
                endif
            ELSE
                ANGLE = ATAN2 (BETAX(I),BETAY(I))
                if (i_wig_binary .eq. 1) then
                    WRITE (21) X(I),Y(I),CDF,ANGLE,CURV(I)
                else
                    WRITE (21,*) X(I),Y(I),CDF,ANGLE,CURV(I)
                endif
                !C
            ENDIF
        ! 499         CONTINUE
        END DO 
        CLOSE   (21)
        print *,'File written to disk: '//trim(outFile)

        ! deallocate arrays
        if (allocated(Y )) deallocate( Y)
        if (allocated(BETAY )) deallocate( BETAY)
        if (allocated(X )) deallocate( X)
        if (allocated(BETAX )) deallocate( BETAX)
        if (allocated(CURV )) deallocate( CURV)
        if (allocated(PHOT_NUM )) deallocate( PHOT_NUM)
        if (allocated(PHOT_CDF )) deallocate( PHOT_CDF)
        if (allocated(Z )) deallocate( Z)
        if (allocated(BETAZ )) deallocate( BETAZ)
        if (allocated(DS )) deallocate( DS)
        if (allocated(S )) deallocate( S)
        if (allocated(DX )) deallocate( DX)
        if (allocated(DY )) deallocate( DY)
        if (allocated(DZ )) deallocate( DZ)

        RETURN
END SUBROUTINE NPhoton

!
! START UNDULATOR PREPROCESSORS
!

!C+++
!C      SUBROUTINE              UNDUL_SHADOW_IO
!C
!C      PURPOSE                 Act as a user friendly input routine 
!C                              to obtain parameters 
!C
!C      INPUT                   from trajectory file
!C
!C      OUTPUT                  Parameters passed through global vars in this module
!C
!C---
        SUBROUTINE UNDUL_SHADOW_IO

        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none

        real(kind=skr)       :: hh,hbar,hhbar,epsi,epsi_fac
        character(len=sklen) :: stmp

!        hh    = H/E             !  "         "	    eV*sec
!        hbar  = H/TWOPI         !  "         "      joule*sec
!        hhbar = HH/TWOPI        !  "	     "	    eV*sec

        hh    = codata_H/codata_E             !  "         "	    eV*sec
        hbar  = codata_H/TWOPI         !  "         "      joule*sec
        hhbar = HH/TWOPI        !  "	     "	    eV*sec

        !EPSI    = 8.854D-12	!FARAD/METER
        EPSI    = codata_electric_permittivity
        EPSI_FAC = 16.0D0*PI**3*EPSI*codata_C
        EPSI_FAC = 1/EPSI_FAC  ! {4 pi epsi_0 * 4 pi^2}
!C
!C Internal or external computation base
!C
        WRITE(6,*) 'Computation done by :'
        WRITE(6,*) '       supplied program        (0)'
        WRITE(6,*) '       user''s program          (1)'
        IEXTERNAL = IRINT ('Choice : ')
!C
             WRITE(6,*) ' '
             WRITE(6,*) '----------------------------',&
      '-----------------------------------------------'
             WRITE(6,*) ' '
        WRITE(6,*) ' '
        WRITE(6,*) 'Define Radiation Computational parameters:'
        WRITE(6,*) ' '
        WRITE(6,*) 'Polar coordinate is used here.'
        !write(stmp,'(I5)') NDIM_E
        NE = IRINT ('Number of points in energy : ')
        !write(stmp,'(I5)') NDIM_A
        NT = IRINT ('                     theta : ')
        NP = IRINT ('                       phi : ')
        WRITE(6,*) ' '
        WRITE(6,*) '----------------------------',&
      '-----------------------------------------------'
        WRITE(6,*) ' '
!C
!C  read file generated from EPATH
!C
        WRITE(6,*) '----------------------------',&
      '-----------------------------------------------'
        WRITE(6,*) ' '
        FTRAJ   = RSTRING('Enter name of trajectory file from EPATH (input ):')
        WRITE(6,*) ' '
        WRITE(6,*) '----------------------------', &
              '-----------------------------------------------'
!C
!C Read in trajectory file
!C
        CALL UREAD
!C
!C Let the user know the parameters from the trajectory file.
!C
        WRITE(6,*) ' '
        WRITE(6,*) 'Undulator case.'
        WRITE(6,*) 'Trajectory computed by EPATH with following parameters:'
        WRITE(6,*) ' '
        WRITE(6,*) 'Number of points       :',NPointId
        WRITE(6,*) 'Undulator wavelength   :',RLAU,'  meters'
        WRITE(6,*) 'Deflection parameter   :',RK
        WRITE(6,*) 'Peak magnetic field    :',B0,'  tesla'
        WRITE(6,*) 'Electron energy	       :',ER/1.602D-19/1.D9,'  GeV'
        WRITE(6,*) 'Gamma		       :',GA0
        WRITE(6,*) '      '
        WRITE(6,*) 'Fundamental energy     :',ENERGY1/1.602D-19,'  eV'
        WRITE(6,*) 'Fundamental wavelength :',RLA1*1.0d10,'  Angstrom'
        WRITE(6,*) '      '
!C
!C  band pass case:  energy range: EMIN,EMAX,ESTEP
!C
        WRITE(6,*) ' '
        WRITE(6,*) '----------------------------',&
      '-----------------------------------------------'
        WRITE(6,*) ' Parameters for Radiation Computation.'
        IF (IEXTERNAL.EQ.0) WRITE(6,*)  &
       'Please note value of first harmonic from above table.'
        WRITE(6,*) ' '
        NCOMP   = IRINT  ('  Enter number of periods: ')
        EMIN    = RNUMBER('        spectrum starting energy [ eV ] : ')
        EMAX    = RNUMBER('                 final           [ eV ] : ')
        RCURR   = RNUMBER('        electron current         [ A  ] : ')
!C
!C Sets ICOMP to # of photons per energy interval (=1), bandpass case is (=0).
!C
        ICOMP = 1
!C
!C Selects polar angles (=1), cartesian angle (=2).
!C
        IANGLE = 1
!C
!C Region in which to generate angles (first quadrant of x',z')
!C
        IAPERTURE = 2
!C
        THEMAX = RNUMBER('Maximum angle between radiation and undulator axis [ mrad ]: ')
        THEMIN    = 0.0d0
!C
!C      WRITE(6,*) 'Maximum angle between radiation and undulator axis 
!C     $ NORMAL to undulator plane (i.e., parallel to magnetic field) '
!C      WRITE(6,*) '(i.e., in the vertical plane)'
        PHIMIN    = 0.0d0
        PHIMAX    = PIHALF
!C
!C  convert units (mrad to rad)
!C
        THEMAX   = THEMAX*1.D-3
!C
!C No. of optimization
!C
        WRITE(6,*) 'All undulator parameters defined.'
        IOPT = IRINT ('How many times of optimization ? ')

        RETURN
END SUBROUTINE Undul_Shadow_Io



!C+++
!C
!C	SUBROUTINE	UREAD
!C
!C	PURPOSE:	To read in the trajectory file generated by EPATH, and
!C			the parameter namelist file.
!C
!C	OUTPUT:		It defines all the common blocks
!C
!C---
SUBROUTINE URead 
        ! implicit real(kind=skr) (a-h,o-z)
        ! implicit integer(kind=ski)        (i-n)
        implicit none
        integer(kind=ski) :: i,nc
        real(kind=skr)    :: betay,betazprime,yofz,rb0dumm


!C
!C  read file generated from EPATH
!C
        OPEN (21,FILE=FTRAJ,STATUS='OLD',FORM='UNFORMATTED')
        READ (21) N0,RLAU,ENERGY1
        READ (21) RLA1,RK,GA0,BETA0
        READ (21) BETAX0,BETAY0,BETAZ0
        READ (21) B0,ER,RLEN,NPointId,rb0dumm
        READ (21) PHI_E,THE_E  !Initial velocity angles for EPATH
!C
!C  This is the part for the (N-1) periods of ideal sinusoidal trajectory.
!C
        READ (21) TAU,Z0,ZSTEP
        DO 19 I = 1,NPointId
             READ (21) XOFZ(I),YOFZ,Z(I)
            READ (21) BETAX(I),BETAY,BETAZ(I)
            READ (21) TOFZ(I),BETAZPRIME
19	CONTINUE
!C
!C  Now the part for the two ends of the undulator.
!C
!C	NC 	= 0.5*(NPointId+1)
        READ (21) ETAU, EZ0, EZSTEP, NC
        DO 29 I=1,NC
            READ (21) XOFZ1(I), YOFZ, Z1(I)
            READ (21) BETAX1(I), BETAY, BETAZ1(I)
            READ (21) TOFZ1(I), BETAZPRIME
29	CONTINUE

        DO 39 I = 1,NC
            READ (21) XOFZ2(I), YOFZ, Z2(I)
            READ (21) BETAX2(I), BETAY, BETAZ2(I)
            READ (21) TOFZ2(I), BETAZPRIME
            Z2(I) = Z2(I) + (NCOMP-1)*RLAU
            TOFZ2(I) = TOFZ2(I) + (NCOMP-1)*TAU
39	CONTINUE
        CLOSE (21)

! dump ascii file


!C
!C  This is the part for the (N-1) periods of ideal sinusoidal trajectory.
!C
!        DO I = 1,NPointId
!            WRITE (111,*) XOFZ(I),YOFZ,Z(I)
!            WRITE (111,*) BETAX(I),BETAY,BETAZ(I)
!            WRITE (111,*) TOFZ(I),BETAZPRIME
!        END DO
!C
!C  Now the part for the two ends of the undulator.
!C
!        DO I=1,NC
!            WRITE (111,*) XOFZ1(I), YOFZ, Z1(I)
!            WRITE (111,*) BETAX1(I), BETAY, BETAZ1(I)
!            WRITE (111,*) TOFZ1(I), BETAZPRIME
!        END DO

!        DO I = 1,NC
!            WRITE (111,*) XOFZ2(I), YOFZ, Z2(I)
!            WRITE (111,*) BETAX2(I), BETAY, BETAZ2(I)
!            WRITE (111,*) TOFZ2(I), BETAZPRIME
!        END DO


! end dump ascii file

!C
        RETURN

END SUBROUTINE URead



!C+++
!C
!C	PROGRAM		UNDUL_SET
!C
!C	PURPOSE		This is the program to
!C			a) define all the parameters
!C			b) write the (energy, theta, phi) array
!C			c) write the parameters in a namelist
!C
!C
!C---
SUBROUTINE Undul_Set
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none

        !DIMENSION    UPHI(31,31,51),UTHETA(31,51),UENER(51)
        !real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E)  ::     UPHI
        !real(kind=skr),dimension(NDIM_A,NDIM_E)     ::     UTHETA
        !real(kind=skr),dimension(NDIM_E)        ::     UENER
        real(kind=skr),dimension(:,:,:),allocatable  ::     UPHI
        real(kind=skr),dimension(:,:),allocatable    ::     UTHETA
        real(kind=skr),dimension(:),allocatable      ::     UENER
        
        !DIMENSION    TSTART(10),TEND(10)
        real(kind=skr),dimension(10) ::     tstart, tend
        real(kind=skr)               ::     ener, rlamda, tav, the_1, the_2, tstep,di
        integer(kind=ski)            ::     i,ii,iname,j,jstart,k,nd,nd1,nd2
        integer(kind=ski)            ::     nhar,ntemp,srio_jump
        LOGICAL                      ::     FLAG1,FLAG2

        CHARACTER(len=sklen) :: FNAME

        NAMELIST    /PARAIN/    NCOMP,RCURR,ICOMP,BPASS, &
                          IANGLE,IAPERTURE,IEXTERNAL, &
                          FOUT,FIN,FTRAJ,EMIN,EMAX, &
                          THEMIN,THEMAX,PHIMIN,PHIMAX, &
                          NE,NT,NP,NCHECK,IOPT,ITER,IPASS, &
                          I_EDIV,EDIVX,EDIVY,FINT,IINT
!C
!C Read in namelist and trajectory file from EPATH
!C
        WRITE(6,*) ' '
        WRITE(6,*) '*********************** UNDULATOR RADIATION ',&
      '***********************'
        WRITE(6,*) ' '
        write(6,*) 'Parameters from : '
        write(6,*) '    User interactive process   (0) '
        write(6,*) '    NAMELIST file		   (1) '
        INAME = IYES ('Choice : ')
!C
        if (INAME.eq.1) then
!C
!C Read in the namelist file of parameters
!C
           FNAME = RSTRING ('Namelist file : ')
           OPEN (31, FILE=FNAME, STATUS='OLD')
           READ (31,NML=PARAIN)
           CLOSE (31)
        else
           CALL UNDUL_SHADOW_IO
           ITER = 0
        end if
!C
!C Get parameters for REPORT
!C
        WRITE(6,*) 'How often do you want a report on calculations ?'
        NCHECK = IRINT ('E.G., 20, 50,... ? ')
!C
!C Set the counters
!C
        IPASS = 0
!C
!C Read in the trajectory parameters.
!C
        CALL UREAD
!C
!C  compute the number of points and step size
!C
        IF (EMAX.EQ.EMIN) THEN
           NE    = 1
           ESTEP = 0.0D0
        ELSE
           ESTEP  = (EMAX-EMIN)/(NE-1)
        END IF
!C     	BDEL	= ABS(ESTEP)
!C
        IF (PHIMAX.EQ.PHIMIN) THEN
          NP      = 1
          PHISTEP = 0.0D0
        ELSE
          IF (NP.GT.1) THEN
              PHISTEP = (PHIMAX-PHIMIN)/(NP-1) ! open string of points for 1 quadrant.
          ELSE
              PHISTEP = ABS ( PHIMAX - PHIMIN )
          END IF
        END IF
!C
        IF (THEMAX.EQ.THEMIN) THEN
            NT      = 1
            THESTEP = 0.0D0
        ELSE
            IF (NT.GT.1) THEN
                THESTEP   = (THEMAX-THEMIN)/(NT-1) ! open string of points
            ELSE
                THESTEP = ABS ( THEMAX - THEMIN )
            END IF
        END IF

        !C
        !C allocate the arrays
        !C
        allocate( UENER(NE) )
        allocate( UTHETA(NT,NE) )
        allocate( UPHI(NP,NT,NE) )

        !C
        !C Fill the arrays
        !C
        DO K = 1, NE
            UENER(K) = EMIN + (K-1)*ESTEP
            DO J = 1, NT
                UTHETA(J,K) = THEMIN + (J-1)*THESTEP
                DO I = 1, NP
                    UPHI(I,J,K) = PHIMIN + (I-1)*PHISTEP
                END DO
            END DO
        END DO
        !C
        !C For theta, we put them at the ith harmonics.  
        !C The number of available points in theta, NTEMP, is normally NT minus the 2 
        !C end points (THEMIN and THEMAX).
        !C

        ! srio@esrf.eu 20131213
        ! I have decided to jump this section that rearranges the angular
        ! arrays depending on the energy. 
        ! I think this creates problems and it is preferred, in my opinion, 
        ! to keep the arrays linear and increment the number of points. 

        srio_jump = 1
        if (srio_jump .eq. 0) then
                DO 49 K = 1, NE
                    ENER = UENER(K)
                    !RLAMDA  = 12398.52D0/ENER*1.0D-10 ! meter
                    RLAMDA = toangs/ENER*1.0D-10  ! meter
                    DI = 2.0D0/NCOMP
                    NHAR = 0
                    NTEMP = NT - 2
                    JSTART = 2
                    !C
                    !C Fill in TSTART(i) and TEND(i), the boundary of the ith harmonics.  It 
                    !C includes up to the 2nd minimum on either side of the harmonics.
                    !C
                    DO 59 I = 1, 10
                        THE_1 = (I-DI)*RLAMDA/RLAU*(2.0D0*GA0**2) -  &
                            1.0D0 - 0.5D0*(RK**2)
                        THE_2 = (I+DI)*RLAMDA/RLAU*(2.0D0*GA0**2) -  &
                            1.0D0 - 0.5D0*(RK**2)
                        IF (THE_1.GE.0.0) THE_1 = SQRT(THE_1/GA0**2)
                        IF (THE_2.GE.0.0) THE_2 = SQRT(THE_2/GA0**2)
                        FLAG1 = (THE_1.GE.THEMIN).AND.(THE_1.LE.THEMAX)
                        FLAG2 = (THE_2.GE.THEMIN).AND.(THE_2.LE.THEMAX)
                        IF (FLAG1.OR.FLAG2) THEN
                            NHAR = NHAR + 1
                            IF (FLAG1) THEN
                                TSTART(NHAR) = THE_1
                            ELSE
                                TSTART(NHAR) = THEMIN
                                NTEMP  = NTEMP + 1
                                JSTART  = 1
                            END IF
                            IF (FLAG2) THEN
                                TEND(NHAR) = THE_2
                            ELSE
                                TEND(NHAR) = THEMAX
                                NTEMP  = NTEMP + 1
                            END IF
                        END IF
                    59  CONTINUE
                    !C
                    !C If catch no harmonics, just keep the uniform distribution.
                    !C
                    IF (NHAR.EQ.0) THEN
                    ELSE
                        !C
                        !C Check for the unlikely case of overlap.
                        !C
                        DO 69 I = 2, NHAR
                            IF (TSTART(I).LT.TEND(I-1))THEN
                                TAV = (TSTART(I) + TEND(I-1))/2.0D0
                                TSTART(I) = TAV + 1.0D-6
                                TEND(I-1) = TAV
                            END IF
                        69 CONTINUE
                        !C
                        !C Spread the available points NTEMP to the various harmonics.
                        !C
                        ND2 = NTEMP/NHAR
                        ND1 = NTEMP - ND2*(NHAR-1)
                        DO 79 I = 1, NHAR
                            IF (I.EQ.1) THEN
                                ND  = ND1
                            ELSE
                                ND  = ND2
                            END IF
                            TSTEP = (TEND(I) - TSTART(I))/(ND-1)
                        
                            DO 89 II = 1, ND
                                UTHETA(JSTART,K) = TSTART(I) + (II-1)*TSTEP
                                JSTART = JSTART + 1
                            89 CONTINUE
                        79 CONTINUE
                        UTHETA(1,K) = THEMIN
                        UTHETA(NT,K) = THEMAX
                    END IF
                49 CONTINUE
        end if ! srio_jump
        
        !C
        !C If starting from scratch, we need to write out the initial 
        !C (energy, theta, phi) array; otherwise, it already exists.
        !C
        IF (ITER.EQ.0) THEN
          IF (use_undulator_binary_files .EQ. 1) THEN
              OPEN (20, FILE='uphot.dat', STATUS='UNKNOWN', FORM='UNFORMATTED')
              REWIND (20)
              WRITE (20) NE, NT, NP
              DO K = 1,NE
                  WRITE (20) UENER(K)
              END DO
              DO K = 1,NE
                 DO J = 1,NT
                    WRITE (20) UTHETA(J,K)
                 END DO
              END DO
              DO K = 1,NE
                 DO J = 1,NT
                    DO I = 1,NP
                       WRITE (20) UPHI(I,J,K)
                    END DO
                 END DO
              END DO
              CLOSE (20)
              PRINT *,'File written to disk (binary): uphot.dat'
          ELSE
              OPEN (20, FILE='uphot.dat', STATUS='UNKNOWN', FORM='FORMATTED')
              REWIND (20)
              WRITE (20,*) NE, NT, NP
              DO K = 1,NE
                  WRITE (20,*) UENER(K)
              END DO
              DO K = 1,NE
                 DO J = 1,NT
                    WRITE (20,*) UTHETA(J,K)
                 END DO
              END DO
              DO K = 1,NE
                 DO J = 1,NT
                    DO I = 1,NP
                       WRITE (20,*) UPHI(I,J,K)
                    END DO
                 END DO
              END DO
              CLOSE (20)
              PRINT *,'File written to disk (ascii): uphot.dat'
          ENDIF


        END IF
!C
!C Finally the namelist file
!C
        OPEN    (21, FILE='uphot.nml', STATUS='UNKNOWN')
        REWIND  (21)
        WRITE   (21, NML=PARAIN)
        CLOSE   (21)
        PRINT *,'File writtem to disk: uphot.nml'

!C
!C deallocate the arrays
!C
        if (allocated( UENER )) deallocate( UENER )
        if (allocated( UTHETA )) deallocate( UTHETA )
        if (allocated( UPHI   )) deallocate( UPHI ) 

END SUBROUTINE Undul_Set


!C+++
!C	SUBROUTINE	REPORT
!C
!C	PURPOSE		To update the screen with progress reports
!C
!C---
SUBROUTINE Report (E,T,P,TT,PERC,IVAL)
        !IMPLICIT	REAL*8	(A-H,O-Z)
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        implicit none

        !real,intent(in) :: TT
        real(kind=skr),       intent(in) :: E,T,P,PERC
        real(kind=4),         intent(in) :: TT
        integer(kind=ski),    intent(in) :: IVAL

        IF (IVAL.LT.0) THEN
            WRITE (6,1010) 'Phi / Horz: ', 'Theta / Vert : ', &
                'Energy: ','CPU Time: ','% Completed: '
        ELSE
            WRITE (6,1000)  P, T, E, TT, PERC
        END IF
1000    FORMAT (1X,G10.3,T15,G10.3,T30,G10.3,T45,G10.3,T60,F9.2)
1010    FORMAT (1X,A    ,T15,A    ,T30,A    ,T45,A    ,T60,A)
        RETURN
END SUBROUTINE Report


!C+++
!C
!C	SUBROUTINE		UPHOTON
!C
!C	PURPOSE:		To compute the photon distribution of an 
!C				undulator for a certain (ener,theta,phi).
!C
!C	INPUT:			ENER, photon energy (eV)
!C				THETA, angle (rad)
!C				PHI, angle (rad)
!C				The calling program should define the common 
!C				block PARA2
!C
!C	OUTPUT:			PHOT, # of photons
!C				POL_DEG, degree of polarization
!C---
SUBROUTINE UPhoton (ENER,THETA,PHI,PHOT,POL_DEG)
        
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

!C
real(kind=skr),intent(in)   :: ener,theta,phi
real(kind=skr),intent(out)  :: pol_deg,phot

real(kind=skr),dimension(NDIM_TRAJ) :: axr,axi,ayr,ayi,azr,azi
real(kind=skr),dimension(3)    :: n,p_pi,ep

real(kind=skr)   :: hh,hbar,hhbar,epsi,epsi_fac
real(kind=skr)    ::   aax,aay,aaz,atempxi,atempxr,atempyi,atempyr,atempzi,atempzr
real(kind=skr)    ::   atot,atxi,atxr,atyi,atyr,atzi,atzr, comega, cpsi, dot_i, dot_r
real(kind=skr)    ::   axi_i,axi_i1,axi_i2,axr_i,axr_i1,axr_i2
real(kind=skr)    ::   ayi_i,ayi_i1,ayi_i2,ayr_i,ayr_i1,ayr_i2
real(kind=skr)    ::   azi_i,azi_i1,azi_i2,azr_i,azr_i1,azr_i2
real(kind=skr)    ::   eatempxi,eatempxr
real(kind=skr)    ::   eatempyi,eatempyr
real(kind=skr)    ::   eatempzi,eatempzr
real(kind=skr)    ::   efac,fs,g0,gn,gg, omega, powert, powerx, powery, powerz
real(kind=skr)    ::   psi, r_nph_t, r_nph_x, r_nph_y, r_nph_z, rnx, rny, rnz
real(kind=skr)    ::   spsi
integer(kind=ski) ::   l,nc

!DIMENSION    AXR(1001),AXI(1001)
!DIMENSION    AYR(1001),AYI(1001)
!DIMENSION    AZR(1001),AZI(1001)
!DIMENSION    N(3),P_PI(3),EP(3)
!C

hh    = codata_H/codata_E      !  "         "  eV*sec
hbar  = codata_H/TWOPI  !  "         "      joule*sec
hhbar = HH/TWOPI !  "         "  eV*sec
epsi    = codata_electric_permittivity ! 8.854D-12  !FARAD/METER
epsi_fac = 16.0D0*PI**3*EPSI*codata_C
epsi_fac = 1/EPSI_FAC  ! {4 pi epsi_0 * 4 pi^2}
!C
!C  observation vector  n = ( rnx, rny, rnz )
!C  electron trajectory   rr = ( xofz, yofz, z )
!C
IF (IANGLE.EQ.2) THEN
    RNX = COS(THETA)*SIN(PHI)    !cartesian
    RNY = SIN(THETA)
    RNZ = COS(THETA)*COS(PHI)
ELSE IF (IANGLE.EQ.1) THEN
    RNX = SIN(THETA)*COS(PHI)    !polar
    RNY = SIN(THETA)*SIN(PHI)
    RNZ = COS(THETA)
END IF
!C
!C Begin Integration loop.
!C First integrate over the (N-1) periods of ideal sinusodal trajectory.
!C
!C  omega is frequency of radiation, unit:  (SEC-1)
!C
OMEGA  = ENER/HHBAR
COMEGA = OMEGA/codata_C        !CONSTANT FOR EXTERNAL
!C
!C  compute integral: A = beta exp(ipsi) 1/(c*betaz) dz
!C
!C Begins integration loop.
!C
DO 19 L=1,NPointId                  !Begin loop 4
    !C
    PSI   = - COMEGA*( TOFZ(L)*codata_C &
        - RNX*XOFZ(L)  &
        - 0.0d0 &      !      RNY*YOFZ(L)
        - RNZ*Z(L) )
    !C
    CPSI= COS(PSI)
    SPSI= SIN(PSI)
    !C
    !C The following elements should be divided by "C", but due to overflow
    !C errors we do this later.
    !C X component
    !C
    AXR(L) = CPSI*BETAX(L)/BETAZ(L)
    AXI(L) = SPSI*BETAX(L)/BETAZ(L)
    !C
    !C Y component
    !C
    !C               AYR(L) = 0.0d0 !       CPSI*BETAY(L)/BETAZ(L)
    !C               AYI(L) = 0.0d0 !      SPSI*BETAY(L)/BETAZ(L)
    !C
    !C Z component
    !C
    AZR(L) = CPSI
    AZI(L) = SPSI
19  CONTINUE                  !End loop 4



!C
!C Perform integration.
!C
CALL SIMPSON (ZSTEP,AXR,AXR_I,NPointId)
CALL SIMPSON (ZSTEP,AXI,AXI_I,NPointId)
CALL SIMPSON (ZSTEP,AZR,AZR_I,NPointId)
CALL SIMPSON (ZSTEP,AZI,AZI_I,NPointId)
!C    CALL SIMPSON (ZSTEP,AYR,AYR_I,NPointId)
!C     CALL SIMPSON (ZSTEP,AYI,AYI_I,NPointId)
AYR_I = 0.0D0
AYI_I = 0.0D0
!C
!C  A = N X (N X BETA) = N (N dot BETA)-BETA (N dot N)
!C
DOT_R   = RNX*AXR_I + RNY*AYR_I + RNZ*AZR_I
DOT_I   = RNX*AXI_I + RNY*AYI_I + RNZ*AZI_I
!C Divide by "C"
DOT_R   = DOT_R/codata_C
DOT_I   = DOT_I/codata_C
ATXR = RNX*DOT_R - AXR_I/codata_C
ATXI = RNX*DOT_I - AXI_I/codata_C
ATYR = RNY*DOT_R - AYR_I/codata_C
ATYI = RNY*DOT_I - AYI_I/codata_C
ATZR = RNZ*DOT_R - AZR_I/codata_C
ATZI = RNZ*DOT_I - AZI_I/codata_C
!C
!C  Now include G(omega), grating term.
!C
GN  = SIN((NCOMP-1)*OMEGA/2.0d0*( TAU - RLAU*RNZ/codata_C))
G0  = SIN(          OMEGA/2.0d0*( TAU - RLAU*RNZ/codata_C))
IF (ABS(G0).GT.1.0E-15) THEN
    GG = GN/G0
ELSE
    GG = NCOMP - 1
END IF
!C
!C FS is the extra e^(i*(N-1)*psi/2) factor in front of the (sinNx/sinx) term.
!C 
FS = -0.5D0*(NCOMP-2)*OMEGA*(TAU - RLAU*RNZ/codata_C)
ATEMPXR = GG * (COS(FS)*ATXR - SIN(FS)*ATXI)
ATEMPXI = GG * (COS(FS)*ATXI + SIN(FS)*ATXR)
ATEMPYR = GG * (COS(FS)*ATYR - SIN(FS)*ATYI)
ATEMPYI = GG * (COS(FS)*ATYI + SIN(FS)*ATYR)
ATEMPZR = GG * (COS(FS)*ATZR - SIN(FS)*ATZI)
ATEMPZI = GG * (COS(FS)*ATZI + SIN(FS)*ATZR)
!C
!C Integration for the (N-1) periods is completed.
!C

!C
!C Now the part for the two ends of the undulator :
!C
!C The entrance to the undulator:
!C
NC = 0.5D0*(NPointId+1)


DO 29 L=1,NC    
    !C 
    PSI   = - COMEGA*( TOFZ1(L)*codata_C &
        - RNX*XOFZ1(L)  &
        - 0.0d0 & ! RNY*YOFZ(L)
        - RNZ*Z1(L))
    !C
    CPSI= COS(PSI)
    SPSI= SIN(PSI)
    !C
    !C The following elements should be divided by "C", but due to overflow
    !C errors we do this later.
    !C X component
    !C
    AXR(L) = CPSI*BETAX1(L)/BETAZ1(L)
    AXI(L) = SPSI*BETAX1(L)/BETAZ1(L)
    !C
    !C Y component
    !C
    !C     AYR(L) = 0.0d0 !  CPSI*BETAY(L)/BETAZ(L)
    !C     AYI(L) = 0.0d0 ! SPSI*BETAY(L)/BETAZ(L)
    !C
    !C Z component
    !C
    AZR(L) = CPSI
    AZI(L) = SPSI
29  CONTINUE     !End loop 4

!C
!C Perform integration.
!C
CALL SIMPSON (EZSTEP,AXR,AXR_I1,NC)
CALL SIMPSON (EZSTEP,AXI,AXI_I1,NC)
CALL SIMPSON (EZSTEP,AZR,AZR_I1,NC)
CALL SIMPSON (EZSTEP,AZI,AZI_I1,NC)
!C		  CALL SIMPSON (EZSTEP,AYR,AYR_I1,NC)
!C     CALL SIMPSON (EZSTEP,AYI,AYI_I1,NC)
AYR_I1 = 0.0D0
AYI_I1 = 0.0D0
!C
!C The exit of the undulator:
!C
DO 39 L=1,NC    
    !C
    PSI   = - COMEGA*( TOFZ2(L)*codata_C &
        - RNX*XOFZ2(L) &
        - 0.0d0 & ! RNY*YOFZ(L)
        - RNZ*Z2(L))
    !C
    CPSI= COS(PSI)
    SPSI= SIN(PSI)
    !C
    !C The following elements should be divided by "C", but due to overflow
    !C errors we do this later.
    !C X component
    !C
    AXR(L) = CPSI*BETAX2(L)/BETAZ2(L)
    AXI(L) = SPSI*BETAX2(L)/BETAZ2(L)
    !C
    !C Y component
    !C
    !C     AYR(L) = 0.0d0 !  CPSI*BETAY(L)/BETAZ(L)
    !C     AYI(L) = 0.0d0 ! SPSI*BETAY(L)/BETAZ(L)
    !C
    !C Z component
    !C
    AZR(L) = CPSI
    AZI(L) = SPSI
39   CONTINUE     !End loop 4

!C
!C Perform integration.
!C
CALL SIMPSON (EZSTEP,AXR,AXR_I2,NC)
CALL SIMPSON (EZSTEP,AXI,AXI_I2,NC)
CALL SIMPSON (EZSTEP,AZR,AZR_I2,NC)
CALL SIMPSON (EZSTEP,AZI,AZI_I2,NC)
!C    CALL SIMPSON (EZSTEP,AYR,AYR_I2,NC)
!C     CALL SIMPSON (EZSTEP,AYI,AYI_I2,NC)
AYR_I2 = 0.0D0
AYI_I2 = 0.0D0
!C
!C Add contributions from the two ends:
!C
AXR_I = AXR_I1 + AXR_I2
AYR_I = AYR_I1 + AYR_I2
AZR_I = AZR_I1 + AZR_I2
AXI_I = AXI_I1 + AXI_I2
AYI_I = AYI_I1 + AYI_I2
AZI_I = AZI_I1 + AZI_I2
!C
!C  A = N X (N X BETA) = N (N dot BETA)-BETA (N dot N)
!C
DOT_R   = RNX*AXR_I + RNY*AYR_I + RNZ*AZR_I
DOT_I   = RNX*AXI_I + RNY*AYI_I + RNZ*AZI_I
!C Divide by "C"
DOT_R   = DOT_R/codata_C
DOT_I   = DOT_I/codata_C
EATEMPXR = RNX*DOT_R - AXR_I/codata_C
EATEMPXI = RNX*DOT_I - AXI_I/codata_C
EATEMPYR = RNY*DOT_R - AYR_I/codata_C
EATEMPYI = RNY*DOT_I - AYI_I/codata_C
EATEMPZR = RNZ*DOT_R - AZR_I/codata_C
EATEMPZI = RNZ*DOT_I - AZI_I/codata_C
!C
!C  Integration over the entire undulator is completed.  
!C
!C  Now sum up the two contributions:
!C
ATEMPXR = ATEMPXR + EATEMPXR
ATEMPXI = ATEMPXI + EATEMPXI
ATEMPYR = ATEMPYR + EATEMPYR
ATEMPYI = ATEMPYI + EATEMPYI
ATEMPZR = ATEMPZR + EATEMPZR
ATEMPZI = ATEMPZI + EATEMPZI

!C
!C  compute modulus sqared:  A(omega)*Astar(omega)
!C
AAX     = ATEMPXR**2 + ATEMPXI**2
AAY    = ATEMPYR**2 + ATEMPYI**2
AAZ   = ATEMPZR**2 + ATEMPZI**2
!C
!C  Include polarization.  The coord system is defined so that z is parallel
!C  to the normal vector at the observation point.  Polarization written in 
!C  terms of this coord system.  sigma:: horizontal, pi:: vertical
!C
!C  the polarization is defined as:   |E(parallel)|^2/|E(total)|^2
!C
ATOT   = AAX + AAY + AAZ
IF (ATOT.EQ.AAX) THEN
    POL_DEG = 1.0D0
ELSE
    POL_DEG = AAX/ATOT
END IF
!C
!C  Compute energy radiated by a single electron from values of integral; 
!C  This energy is in units of J/cm-1/solid angle
!C
EFAC = (OMEGA*codata_E)**2*EPSI_FAC
POWERX = AAX*EFAC
POWERY = AAY*EFAC
POWERZ = AAZ*EFAC
!C
!C Change to Joules/eV/solid angles (energy radiated by 1 electron along
!C the trajectory)
!C
POWERX = POWERX/HHBAR
POWERY = POWERY/HHBAR
POWERZ = POWERZ/HHBAR
!C
!C Include current (N electrons/sec) and converts at the same time to 
!C		/mrad**2
!C Units are now Watts/eV/mrad**2
!C
POWERX = POWERX*RCURR/codata_e*1.0D-6
POWERY = POWERY*RCURR/codata_e*1.0D-6
POWERZ = POWERZ*RCURR/codata_e*1.0D-6
!C
!C Compute number of photons/sec/eV/mrad**2
!C
R_NPH_X = POWERX/(ENER*codata_E)
R_NPH_Y = POWERY/(ENER*codata_E)
R_NPH_Z = POWERZ/(ENER*codata_E)
!C
!C Compute number of photons/sec/eV/rad**2
!C
R_NPH_X = R_NPH_X*1.0D6
R_NPH_Y = R_NPH_Y*1.0D6
R_NPH_Z = R_NPH_Z*1.0D6
!C
!C Bandpass or constant dE case
!C
IF (ICOMP.EQ.0) THEN
    POWERX = POWERX*BPASS*ENER
    POWERY = POWERY*BPASS*ENER
    POWERZ = POWERZ*BPASS*ENER
    POWERT = POWERX + POWERY + POWERZ
    R_NPH_X = R_NPH_X*BPASS*ENER
    R_NPH_Y = R_NPH_Y*BPASS*ENER
    R_NPH_Z = R_NPH_Z*BPASS*ENER
    R_NPH_T = R_NPH_X + R_NPH_Y + R_NPH_Z
ELSE
    POWERX = POWERX
    POWERY = POWERY
    POWERZ = POWERZ
    POWERT = POWERX + POWERY + POWERZ
    R_NPH_X = R_NPH_X
    R_NPH_Y = R_NPH_Y
    R_NPH_Z = R_NPH_Z
    R_NPH_T = R_NPH_X + R_NPH_Y + R_NPH_Z
END IF
!C
!C PHOT is either in photon/sec/rad^2/eV or photon/sec/rad^2/bandpass.
!C
PHOT = R_NPH_T
!C
RETURN
END SUBROUTINE UPhoton


!C+++
!C     ..................................................................
!C
!C        SUBROUTINE SIMPSON
!C
!C        PURPOSE
!C           To compute the integral using Simpson's rule.
!C
!C        USAGE
!C           CALL QSF (H,Y,Z,NDIM)
!C
!C        DESCRIPTION OF PARAMETERS
!C           H      - THE INCREMENT OF ARGUMENT VALUES.
!C           Y      - THE INPUT VECTOR OF FUNCTION VALUES.
!C           Z      - THE RESULTING INTEGRAL
!C           NDIM   - THE DIMENSION OF VECTORS Y 
!C
!C     ..................................................................
!C---
!TODO: Move to math?
SUBROUTINE Simpson(H,Y,Z,NDIM)
        implicit none
        !implicit real(kind=skr) (a-h,o-z)
        !implicit integer(kind=ski)        (i-n)
        !DIMENSION Y(NDIM)

        real(kind=skr),intent(in)                 :: h
        integer(kind=ski),intent(in)              :: ndim
        real(kind=skr),dimension(ndim),intent(in) :: y
        real(kind=skr),intent(out)                :: z

        real(kind=skr)     :: ht, sum1
        integer(kind=ski)  :: i
!C
        HT=1.0D0/3.0D0*H

        sum1 = Y(1) 
        DO 29 I = 2, NDIM-1, 2
            sum1 = sum1 + 4.0D0*Y(I)
            sum1 = sum1 + 2.0D0*Y(I+1)
29      CONTINUE
        sum1  = sum1 - Y(NDIM)
        Z = HT*sum1

        RETURN
END SUBROUTINE Simpson

!C+++
!C
!C	PROGRAM		UNDUL_PHOT
!C
!C	PURPOSE		This is the main calling program to
!C			a) read in the (energy, theta, phi) array
!C			b) compute the # of photon
!C			c) write out all the arrays
!C
!C---
SUBROUTINE Undul_Phot

!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

real :: ttime,time0

real(kind=skr),dimension(:,:,:),allocatable :: uphi,rn0,pol_deg
real(kind=skr),dimension(:,:),allocatable   :: utheta
real(kind=skr),dimension(:),allocatable     :: uener

real(kind=skr)    :: ener,perc,phi, phot,pol,theta,totpoints
integer(kind=ski) :: i,j,k,iOne,ipoints,iTmp,kCheck

NAMELIST /PARAIN/ NCOMP,RCURR,ICOMP,BPASS,&
    IANGLE,IAPERTURE,IEXTERNAL,&
    FOUT,FIN,FTRAJ,EMIN,EMAX,&
    THEMIN,THEMAX,PHIMIN,PHIMAX,&
    NE,NT,NP,NCHECK,IOPT,ITER,IPASS,&
    I_EDIV,EDIVX,EDIVY,FINT,IINT


!C
!C Read in the parameters from namelist file
!C
OPEN (21, FILE='uphot.nml', STATUS='OLD')
READ (21, NML=PARAIN)
CLOSE (21)
!C
!C Read in the (energy, theta, phi) array
!C
IF (use_undulator_binary_files .EQ. 1) THEN
    OPEN (40, FILE='uphot.dat', STATUS='OLD', FORM='UNFORMATTED')
    READ (40) NE, NT, NP
ELSE
    OPEN (40, FILE='uphot.dat', STATUS='OLD', FORM='FORMATTED')
    READ (40,*) NE, NT, NP
ENDIF
!
! allocate arrays
!
allocate( UENER(NE) )
allocate( UTHETA(NT,NE) )
allocate( UPHI(NP,NT,NE) )
allocate( RN0(NP,NT,NE) )
allocate( POL_DEG(NP,NT,NE) )


IF (use_undulator_binary_files .EQ. 1) THEN
    DO K = 1, NE
        READ (40) UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            READ (40) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) UPHI(I,J,K)
            END DO
        END DO
    END DO
ELSE
    DO K = 1, NE
        READ (40,*) UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            READ (40,*) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) UPHI(I,J,K)
            END DO
        END DO
    END DO
ENDIF

  
!C
!C Read in the trajectory file
!C
CALL UREAD
!C
write(6,*) ' '
write(6,*) '----------------------------', &
    '-----------------------------------------------'
write(6,*) ' '
write(6,*) ' '
write(6,*) '----------------------------', &
    '-----------------------------------------------'
write(6,*) ' '
write(6,*) ' '
write(6,*) 'Begin calculations.'
write(6,*) ' '
!C
!C Sets up REPORT
!C
!          CALL     REPORT ( UENER(1), UTHETA(1,1), UPHI(1,1,1), TTIME, PERC, -1)
iTmp=-1
CALL REPORT ( UENER(1), UTHETA(1,1), UPHI(1,1,1), TTIME, PERC, iTmp)
!C
!C  Compute the # of photons from internal routine UPHOTON
!C  All preliminaries completed. Starts real calculations.
!C
!TIME0 = CPUTIM()
CALL CPU_TIME(TIME0)
TOTPOINTS = NP*NT*NE
KCHECK = 0
IPOINTS = 0
!C
!C Compute the # of photons at each (energy, theta, phi).
!C
DO K = 1,NE
    ENER  = UENER(K)
    DO J = 1, NT
        THETA = UTHETA(J,K)
        DO I = 1, NP
            PHI = UPHI(I,J,K)
            CALL UPHOTON (ENER, THETA, PHI, PHOT, POL)
            RN0(I,J,K) = PHOT
            POL_DEG(I,J,K) = POL
            !C Status report.
            KCHECK = KCHECK + 1
            IPOINTS = IPOINTS + 1
            IF (KCHECK.EQ.NCHECK) THEN
                CALL CPU_TIME(ttime)
                TTIME = TTIME - TIME0
                PERC = IPOINTS/TOTPOINTS*100
                iOne = 1
                !CALL REPORT ( ENER, THETA, PHI, TTIME, PERC, 1)
                CALL REPORT ( ENER, THETA, PHI, TTIME, PERC, iOne)
                KCHECK = 0
            END IF
            !C
        END DO
    END DO
END DO
!C
IF (IPASS.EQ.0) THEN
    write(6,*) ' '
    write(6,*) '----------------------------', &
        '-----------------------------------------------'
    write(6,*) ' '
    write(6,*) 'Spectra Computations completed.'
    write(6,*) ' '
    write(6,*) ' '
    write(6,*) '----------------------------',&
        '-----------------------------------------------'
    write(6,*) ' '
END IF
!C
!C Write out all arrays.  APPENDED!!!!
!C
IF (use_undulator_binary_files .EQ. 1) THEN
    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                WRITE (40) RN0(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                WRITE (40) POL_DEG(I,J,K)
            END DO
        END DO
    END DO
ELSE
    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                WRITE (40,*) RN0(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                WRITE (40,*) POL_DEG(I,J,K)
            END DO
        END DO
    END DO
ENDIF

CLOSE (40)


!
! deallocate arrays
!
if (allocated( UENER   )) deallocate( UENER )
if (allocated( UTHETA  )) deallocate( UTHETA )
if (allocated( UPHI    )) deallocate( UPHI ) 
if (allocated( RN0     )) deallocate( RN0 ) 
if (allocated( POL_DEG )) deallocate( POL_DEG ) 

END SUBROUTINE Undul_Phot


!C+++
!C
!C	SUBROUTINE	RNS
!C
!C	PURPOSE		Integrate #photons/sec/eV/rad**2 first over phi,
!C			then over theta.
!C
!C	ALGORITHM	Integration performed using QSF
!C			RECTANGULAR APERTURE (cartesian angles)
!C			PINHOLE APERTURE (polar angles)
!C
!C	INPUT:		RN0(phi, theta, ener)
!C			Common block PARA3 should be defined by the calling 
!C			program
!C
!C	OUTPUT:		RN1(theta, ener)
!C			RN2(ener)
!C---
SUBROUTINE Rns(RN0,RN1,RN2,UPHI,UTHETA,UENER,np,nt,ne)
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

integer(kind=ski), intent(in)  :: np,nt,ne

!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E), intent(in)  :: rn0
!real(kind=skr),dimension(NDIM_A,NDIM_E),    intent(out) :: rn1
!real(kind=skr),dimension(NDIM_E),       intent(out) :: rn2
!
!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E) , intent(in) :: uphi
!real(kind=skr),dimension(NDIM_A,NDIM_E)    , intent(in) :: utheta
!real(kind=skr),dimension(NDIM_E)       , intent(in) :: uener

real(kind=skr),dimension(np,nt,ne), intent(in)  :: rn0
real(kind=skr),dimension(nt,ne),    intent(out) :: rn1
real(kind=skr),dimension(ne),       intent(out) :: rn2

real(kind=skr),dimension(np,nt,ne) , intent(in) :: uphi
real(kind=skr),dimension(nt,ne)    , intent(in) :: utheta
real(kind=skr),dimension(ne)       , intent(in) :: uener

!DIMENSION RN0(31,31,51),RN1(31,51),RN2(51)
!DIMENSION UPHI(31,31,51),UTHETA(31,51),UENER(51)

real(kind=skr),dimension(NDIM_TRAJ) :: yrn0,yrnn0,yrn1,yrnn1

real(kind=skr)    :: arn
integer(kind=ski) :: i,j,k

!DIMENSION YRN0(1001),YRNN0(1001)
!DIMENSION YRN1(1001),YRNN1(1001)
         
!real(kind=skr),parameter :: PI=3.141592653589793238462643D0

!C
IF (IANGLE.EQ.1) THEN  !Polar coords
    !C   
    !C    TYPE *,'BEGIN INTEGRATION OVER PHI'
    !C
    DO 155 K = 1,NE
        DO 165 J = 1,NT
            DO 175 I = 1,NP
                YRN0(I) = RN0(I,J,K) * UTHETA(J,K)
                !C      IF (J.EQ.1) THEN
                !C   YRN0(I) = YRN0(I)/(UPHI(NP,J,K)-UPHI(1,J,K))*
                !C     $    (UTHETA(2,K)-UTHETA(1,K))*PI/16
                !C      ELSE IF (J.NE.1) THEN
                !C   IF (I.EQ.1.OR.I.EQ.NP) THEN
                !C        YRN0(I) = YRN0(I)/2.0D0*UTHETA(J,K) 
                !C   ELSE
                !C        YRN0(I) = YRN0(I)*UTHETA(J,K)
                !C   END IF
                !C      END IF
            175   CONTINUE
            
            IF (J.EQ.1) THEN
                ARN = 0.0D0
                DO 185 I = 1, NP
                    ARN = ARN + 0.5D0*(RN0(I,1,K) + RN0(I,2,K))
                185     CONTINUE
                ARN = ARN/NP
                DO 195 I = 1, NP
                    YRN0(I) = ARN*0.5D0*(UTHETA(2,K)-UTHETA(1,K))/2.0D0
                195     CONTINUE
            END IF
            
            !C  Use trapezoidal integration
            YRNN0(1) = 0.0D0
            DO 205 I=2,NP
                YRNN0(I) = YRNN0(I-1) + (UPHI(I,J,K)-UPHI(I-1,J,K))* &
                    0.5D0*( YRN0(I-1) + YRN0(I) )
            205   CONTINUE
            !C
            RN1(J,K) = YRNN0(NP)
        165  CONTINUE
    155  CONTINUE
    !C
    !C TYPE *,'BEGIN INTEGRATION OVER THETA'
    !C
    DO 215 K = 1,NE
        DO 225 J = 1,NT
            YRN1(J) = RN1(J,K) 
        225       CONTINUE
        !C  Use trapezoidal integration
        YRNN1(1) = 0.0D0
        DO 235 J=2,NT
            YRNN1(J) = YRNN1(J-1) + (UTHETA(J,K)-UTHETA(J-1,K))* &
                0.5D0*( YRN1(J-1) + YRN1(J) )
        235   CONTINUE
        !C
        IF (IAPERTURE.EQ.1) THEN
            RN2(K) = YRNN1(NT)*4.0D0
        ELSE IF (IAPERTURE.EQ.2) THEN
            RN2(K) = YRNN1(NT)
        END IF
    215    CONTINUE
ELSE IF (IANGLE.EQ.2) THEN  ! Cartesian coords
    !C
    !C         TYPE *,'BEGIN INTEGRATION OVER  PHI'
    !C
    DO 245 K=1,NE    ! # ENER
        DO 255 J=1,NT    ! # THE
            DO 265 I=1,NP   ! # PHI
                YRN0(I) = RN0(I,J,K) ! Prepare integration over phi
                !C         IF (I.EQ.1.AND.J.EQ.1)THEN
                !C                  YRN0(I) = YRN0(I)/4.0D0
                !C         ELSE IF (I.EQ.1.OR.J.EQ.1) THEN
                !C                  YRN0(I) = YRN0(I)/2.0D0
                !C         ELSE
                !C            YRN0(I) = YRN0(I)
                !C         END IF
            265      CONTINUE
            !C  Use trapezoidal integration
            YRNN0(1) = 0.0D0
            DO 275 I=2,NP
                YRNN0(I) = YRNN0(I-1) + (UPHI(I,J,K)-UPHI(I-1,J,K))* &
                    0.5D0*( YRN0(I-1) + YRN0(I) )
            275   CONTINUE
            !C
            RN1(J,K) = YRNN0(NP)
        255       CONTINUE
    245    CONTINUE
    !C
    !C Begin integration over theta
    !C 
    !C         TYPE *,'BEGIN INTEGRATION OVER THETA'
    !C
    !C
    DO 285 K=1,NE
        DO 295 J=1,NT
            YRN1(J) = RN1(J,K)
        295           CONTINUE
        !C  Use trapezoidal integration
        YRNN1(1) = 0.0D0
        DO 305 J=2,NT
            YRNN1(J) = YRNN1(J-1)+(UTHETA(J,K) - UTHETA(J-1,K))* &
                0.5D0*( YRN1(J-1) + YRN1(J) )
        305   CONTINUE
        !C
        !C Multiply by 4 for the entire rectangular region.
        !C
        IF (IAPERTURE.EQ.3) THEN  !rectangle centered in xy plane 
            RN2(K) = YRNN1(NT)*4.0D0
        ELSE IF (IAPERTURE.EQ.4) THEN
            RN2(K) = YRNN1(NT)  !rectangle in 1st quadrant.
        END IF
        285        CONTINUE
    !C
END IF
!C
IF (IPASS.NE.0) RETURN
!C
!C Include total power calculation
!C
WRITE(6,*) 'Calculation completed.'
WRITE(6,*) '----------------------------',&
    '-----------------------------------------------'
WRITE(6,*) ' '
WRITE(6,*) '----------------------------', &
    '-----------------------------------------------'
WRITE(6,*) ' '
WRITE(6,*) 'Begin computation of total power.'
TOTPOWER = 0.0D0
!C
!C Notice: RN2 is either of: #phot/eV/sec 
!C    or: #phot/bpass/sec
!C
!d do kc=1,ne
!d write (90,*) ener(kc),rn2(kc)
!d end do
!d
DO 315 K=1,NE-1
    IF (ICOMP.EQ.0) THEN  ! Constant Bpass
        TOTPOWER = TOTPOWER + 0.5D0*(RN2(K)+RN2(K+1))/BPASS &
            *(UENER(K+1)-UENER(K))*1.602D-19
    ELSE    ! Constant dE
        TOTPOWER = TOTPOWER +  &
            0.5D0*(RN2(K)*UENER(K)+RN2(K+1)*UENER(K+1)) &
            *(UENER(K+1)-UENER(K))*1.602D-19
    END IF
315 CONTINUE

WRITE(6,*)'Total Power emitted in the specified angles is: '
WRITE(6,*)totpower
WRITE(6,*)'Watts.'
WRITE(6,*)' '
WRITE(6,*)'Preliminary calculations completed.'
WRITE(6,*)' '
WRITE(6,*)' '
!C
RETURN
END SUBROUTINE Rns

!C+++
!C	
!C	SUBROUTINE		UCDF
!C
!C	PURPOSE			To compute the CDF's from the RN's
!C
!C	INPUT:			RN0(phi, theta, ener)
!C				RN1(     theta, ener)
!C				RN2(            ener)
!C
!C	OUTPUT:			CDF0(phi, theta, ener)
!C				CDF1(     theta, ener)
!C				CDF2(            ener)
!C---
SUBROUTINE UCDF(RN0,RN1,RN2,CDF0,CDF1,CDF2,UPHI,UTHETA,UENER,np,nt,ne)

implicit none
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
integer(kind=ski), intent(in)  :: np,nt,ne

!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E), intent(in) :: rn0
!real(kind=skr),dimension(NDIM_A,NDIM_E),    intent(in) :: rn1
!real(kind=skr),dimension(NDIM_E),       intent(in) :: rn2
!
!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E), intent(out) :: cdf0
!real(kind=skr),dimension(NDIM_A,NDIM_E),    intent(out) :: cdf1
!real(kind=skr),dimension(NDIM_E),       intent(out) :: cdf2
!
!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E)             :: uphi
!real(kind=skr),dimension(NDIM_A,NDIM_E)                :: utheta
!real(kind=skr),dimension(NDIM_E)                   :: uener

real(kind=skr),dimension(np,nt,ne), intent(in) :: rn0
real(kind=skr),dimension(nt,ne),    intent(in) :: rn1
real(kind=skr),dimension(ne),       intent(in) :: rn2

real(kind=skr),dimension(np,nt,ne), intent(out) :: cdf0
real(kind=skr),dimension(nt,ne),    intent(out) :: cdf1
real(kind=skr),dimension(ne),       intent(out) :: cdf2

real(kind=skr),dimension(np,nt,ne)             :: uphi
real(kind=skr),dimension(nt,ne)                :: utheta
real(kind=skr),dimension(ne)                   :: uener

real(kind=skr),dimension(NDIM_TRAJ)  :: yrn0,yrnn0,yrn1,yrnn1,yrn2,yrnn2
integer(kind=ski)  :: i,j,k

!DIMENSION  UPHI(31,31,51),UTHETA(31,51),UENER(51) 
!DIMENSION  RN0(31,31,51),RN1(31,51),RN2(51) 
!DIMENSION  CDF0(31,31,51),CDF1(31,51),CDF2(51)

!DIMENSION  YRN0(1001),YRNN0(1001)
!DIMENSION  YRN1(1001),YRNN1(1001)
!DIMENSION  YRN2(1001),YRNN2(1001)

!C 
!C  HERE WE CALCULATE THE CDF's.  
!C
DO 15 K = 1,NE
    DO 25 J = 1,NT
        DO 35 I = 1,NP
            YRN0(I) = RN0(I,J,K)
        35       CONTINUE
        !C  Use trapezoidal integration
        YRNN0(1) = 0.0D0  
        DO 45 I=2,NP
            YRNN0(I) = YRNN0(I-1) + (UPHI(I,J,K)-UPHI(I-1,J,K))* &
                0.5D0*( YRN0(I-1) + YRN0(I) )
        45    CONTINUE
        !C      ! integrate over phi
        DO 55 I = 1,NP   ! Note, just integration,
            CDF0(I,J,K) = YRNN0(I) ! variable not eliminated.
        55 CONTINUE
    25 CONTINUE
15 CONTINUE
!C 
DO 65 K = 1,NE
    DO 75 J = 1,NT
        YRN1(J) = RN1(J,K)
    75    CONTINUE
    !C  Use trapezoidal integration
    YRNN1(1) = 0.0D0
    DO 85 J=2,NT
        YRNN1(J) = YRNN1(J-1) + (UTHETA(J,K)-UTHETA(J-1,K))* &
            0.5D0*( YRN1(J-1) + YRN1(J) )
    85   CONTINUE
    !C      ! integrate over theta
    DO 95 J = 1,NT
        CDF1(J,K) = YRNN1(J)
    95     CONTINUE
65  CONTINUE
!C
DO 115 K=1,NE
    YRN2(K) = RN2(K)
115   CONTINUE
!C  Use trapezoidal integration
YRNN2(1) = 0.0D0
DO 125 K=2,NE
    YRNN2(K) = YRNN2(K-1) + (UENER(K)-UENER(K-1))* &
        0.5D0*( YRN2(K-1) + YRN2(K) )
125  CONTINUE
!C
DO 135 K=1,NE
    CDF2(K) = YRNN2(K)
135   CONTINUE
!C 
RETURN
END SUBROUTINE Ucdf

!C+++
!C
!C	SUBROUTINE		UWRITE
!C
!C	PURPOSE			To write the RN and CDF files.
!C
!C	INPUT:			RN0,RN1,RN2,POL_DEG
!C				CDF0,CDF1,CDF2
!C---
SUBROUTINE UWrite (RN0,RN1,RN2,POL_DEG,CDF0,CDF1,CDF2,UPHI,UTHETA,UENER,np,nt,ne)

!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

integer(kind=ski), intent(in)  :: np,nt,ne

real(kind=skr),dimension(np,nt,ne), intent(in) :: rn0,cdf0,uphi
real(kind=skr),dimension(nt,ne),    intent(in) :: rn1,cdf1,utheta
real(kind=skr),dimension(ne),       intent(in) :: rn2,cdf2,uener

real(kind=skr),dimension(np,nt,ne), intent(out) :: pol_deg


character(len=sklen) :: FNAME1,FNAME2,SPECFILE
character(len=60)    :: name
character(len=17)    :: date

integer(kind=ski) :: i,j,k,iflag,itype
real(kind=skr)    :: temp

!C
!C Read in parameters of trajectory file.
!C
CALL UREAD
!C
!C Write RN file
!C
WRITE(6,*) 'Number of optimizations finished : ',ITER
WRITE(6,*) ' '
ITYPE = 1
IFLAG = IYES ('Do you want to write out spectra ? ')
IF (IFLAG.EQ.0) GO TO 10
FNAME1 = RSTRING ('Name of file for storing spectra: ')
CALL RW_UNDUL ( NP,NT,NE, UPHI, UTHETA, UENER, &
    RN0, RN1, RN2, POL_DEG, &
    FNAME1, ITYPE, IANGLE)
!C

10   CONTINUE

!C
!C Write CDF file
!C
ITYPE = 2
IFLAG = IYES ('Do you want to create a SHADOW file ? ')
IF (IFLAG.EQ.0) GO TO 30
FNAME2 = RSTRING ('Name of file for SHADOW: ')

!C
!C SHADOW defines the degree of polarization by |E| instead of |E|^2
!C i.e.  P = |Ex|/(|Ex|+|Ey|)   instead of   |Ex|^2/(|Ex|^2+|Ey|^2)
!C
DO 19 K = 1, NE
    DO 29 J = 1, NT
        DO 39 I = 1, NP
            TEMP = POL_DEG(I,J,K)
            POL_DEG(I,J,K) = SQRT(TEMP)/(SQRT(TEMP)+SQRT(1.D0-TEMP))
        39 CONTINUE
    29 CONTINUE
19 CONTINUE

CALL RW_UNDUL ( NP,NT,NE, UPHI, UTHETA, UENER, &
    CDF0, CDF1, CDF2, POL_DEG, FNAME2, ITYPE, IANGLE)

!C
!C Create and write a log file.
!C
30  continue

SPECFILE = RSTRING('File name for parameter info : ')
OPEN  (29,FILE=SPECFILE,STATUS='UNKNOWN')
REWIND (29)
WRITE (29,99)
WRITE (29,*) 'Trajectory computed by EPATH with following parameters:'
WRITE (29,*) 'Number of points : ',NPointId
WRITE (29,*) 'Wavelen. (und)   : ',RLAU,             ' meters'
WRITE (29,*) 'Fundamental wvl  : ',RLA1*1.0d10,      ' angstroms'
WRITE (29,*) 'Fund.    energy  : ',ENERGY1/1.602D-19,' eV'
WRITE (29,*) 'K is             : ',RK
WRITE (29,*) 'Gamma            : ',GA0
WRITE (29,*) 'Beta0            : ',BETA0,            ' C units'    
WRITE (29,*) 'Field B0         : ',B0,               ' tesla'
WRITE (29,*) 'Electron energy  : ',ER/1.602d-19/1.D9,'GeV'
WRITE (29,99)
WRITE (29,*) 'Read ',NPointId,' trajectory records from ',trim(FTRAJ)
WRITE (29,*) 'Number periods used in ERAD: ',NCOMP
WRITE (29,*) 'Total power radiated in the limits [ W ]: ',TOTPOWER
IF (ICOMP.EQ.0) THEN
    write (29,99)
    write (29,*) 'Working with band-pass case (units eV) .'
    write (29,*) 'band-pass = ',bpass
    write (29,*) 'Limits: ',EMIN,EMAX
    !C         write (29,*) 'Step  : ',ESTEP,' Number of points: ',NE
    write (29,*) 'Number of points: ',NE
    write (29,99)
    ELSE
    write (29,99)
    write (29,*) 'Working with constant dE (units eV) .'
    write (29,*) 'Energy interval = ',ESTEP
    write (29,*) 'Limits: ',EMIN,EMAX
    !C         write (29,*) 'Step  : ',ESTEP,' Number of points: ',NE
    write (29,*) 'Number of points: ',NE
    write (29,99)
    !C         write (29,*) 'Piecewise spectrum choosen: ', KIND(IHARM+1)
    !C         write (29,*) 'From Harmonic: ',N_HARM1,' to ',N_HARM2
    !C         write (29,*) 'Width ',WIDTH,' and Number of points ',NCOMP
END IF
IF (IANGLE.EQ.1) THEN
    WRITE (29,*) 'POLAR ANGLES CHOSEN    '
    WRITE (29,*)'Azimutal angle (units rad) .'
    WRITE (29,*) 'Limits: ',PHIMIN,PHIMAX
    !C        WRITE (29,*) 'Step  : ',PHISTEP,' Number of points: ',NP
    WRITE (29,*) 'Number of points: ',NP
    WRITE (29,99)
    WRITE (29,*)'Polar angle (units mrad) .'
    WRITE (29,*) 'Limits: ',THEMIN*1.0D3,THEMAX*1.0D3
    !C        WRITE (29,*) 'Step  : ',THESTEP*1.0D3,' Number of points: ',NT
    WRITE (29,*) 'Number of points: ',NT
    WRITE (29,'(a)') 'Spectra written into (binary) file: '//trim(FNAME1)
    ELSE IF (IANGLE.EQ.2) THEN
    WRITE (29,*) 'CARTESIAN ANGLES CHOSEN '
    WRITE (29,*)'Horizontal angle (units mrad) .'
    WRITE (29,*) 'Limits: ',PHIMIN*1.0D3,PHIMAX*1.0D3
    !C        WRITE (29,*) 'Step  : ',PHISTEP*1.0D3,' Number of points: ',NP
    WRITE (29,*) 'Number of points: ',NP
    WRITE (29,99)
    WRITE (29,*)'Vertical angle (units mrad) .'
    WRITE (29,*) 'Limits: ',THEMIN*1.0D3,THEMAX*1.0D3
    !C        WRITE (29,*) 'Step  : ',THESTEP*1.0D3,' Number of points: ',NT
    WRITE (29,*) 'Number of points: ',NT
    WRITE (29,*) 'File for SHADOW (binary) written to:'
    WRITE (29,*) trim(FNAME2)
END IF
IF (ICOMP.EQ.0) THEN
    WRITE (29,*) 'in units: PHOTONS/SEC/%Bandpass/RAD**2 '
ELSE
    WRITE (29,*) 'in units: PHOTONS/SEC/eV/RAD**2 '
END IF
WRITE (29,99)
!C
!C
WRITE (29,99)
CLOSE (29)
PRINT *,'File written to disk: '//trim(specFile)

99 FORMAT (1X,/,'---------------------------------------------',/)
1000 FORMAT (1X, 3(1X,G12.5), 1X, G15.8)
1010 FORMAT (1X, 3(1X,G12.5), 3(1X, G15.8) )
1020 FORMAT (1X, 3(1X,G12.5), 2(1X, G15.8) )
!C
RETURN
END SUBROUTINE UWrite

!C+++
!C
!C	SUBROUTINE 		RW_UNDUL
!C
!C	PURPOSE			write output files for source generation using SHADOW.
!C
!C	INPUT			arrays to write out (U*, ZERO,ONE,TWO,POL_DEG)
!C                              itype: 1=write spectrum, 2=write SHADOW file.
!C
!C	OUTPUT			A file containing CDF's and POLARIZATION or RN's
!C---

SUBROUTINE RW_UNDUL ( NP,NT,NE, UPHI, UTHETA, UENER, &
                      ZERO, ONE, TWO, POL_DEG, FNAME, ITYPE, IANGLE)

implicit none

integer(kind=ski), intent(in)    :: np,nt,ne,itype,iangle
real(kind=skr),dimension(np,nt,ne), intent(in) :: uphi, zero, pol_deg
real(kind=skr),dimension(nt,ne),    intent(in) :: utheta, one
real(kind=skr),dimension(ne),       intent(in) :: uener, two

character(len=sklen) :: FNAME
integer(kind=ski)  :: i,j,k

IF (use_undulator_binary_files .EQ. 1) THEN
    OPEN (31,FILE=FNAME,STATUS='UNKNOWN', FORM='UNFORMATTED')
    REWIND (31)
    !C  IF (ITYPE.EQ.2) THEN  !cartesian for SHADOW
    WRITE(31) NE,NT,NP,IANGLE

    DO K = 1,NE
        WRITE(31) UENER(K)
    END DO

    DO K = 1,NE
        DO J = 1,NT
            WRITE(31) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1,NT
            DO I = 1,NP
                WRITE(31) UPHI(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1,NE
        WRITE(31) TWO(K)
    END DO

    DO K = 1,NE
        DO J = 1,NT
            WRITE(31) ONE(J,K)
        END DO
    END DO


    DO K = 1,NE
        DO J = 1,NT
            DO I = 1,NP
                WRITE(31) ZERO(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1,NE
        DO J = 1,NT
            DO I = 1,NP
                WRITE(31) POL_DEG(I,J,K)
            END DO
        END DO
    END DO

    CLOSE(31)
    PRINT *,'File written to disk (binary): '//trim(fname)
ELSE
    OPEN (31,FILE=FNAME,STATUS='UNKNOWN', FORM='FORMATTED')
    REWIND (31)
    !C  IF (ITYPE.EQ.2) THEN  !cartesian for SHADOW
    WRITE(31,*) NE,NT,NP,IANGLE

    DO K = 1,NE
        WRITE(31,*) UENER(K)
    END DO

    DO K = 1,NE
        DO J = 1,NT
            WRITE(31,*) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1,NT
            DO I = 1,NP
                WRITE(31,*) UPHI(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1,NE
        WRITE(31,*) TWO(K)
    END DO

    DO K = 1,NE
        DO J = 1,NT
            WRITE(31,*) ONE(J,K)
        END DO
    END DO


    DO K = 1,NE
        DO J = 1,NT
            DO I = 1,NP
                WRITE(31,*) ZERO(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1,NE
        DO J = 1,NT
            DO I = 1,NP
                WRITE(31,*) POL_DEG(I,J,K)
            END DO
        END DO
    END DO

    CLOSE(31)
    PRINT *,'File written to disk (ascii): '//trim(fname)

END IF

RETURN
END SUBROUTINE RW_Undul

!C+++
!C
!C	SUBROUTINE		UINVERT
!C
!C	PURPOSE			1) invert the CDFs so that they are equal space 
!C				   in the probability (Y-axis)
!C				2) create a new set of (energy, theta, phi) 
!C				   corresponds to the probability.
!C
!C	INPUT:			CDF0,CDF1,CDF2,UPHI,UTHETA,UENER
!C
!C	OUTPUT:			AENER,ATHETA,APHI
!C
!C---
SUBROUTINE UInvert (CDF0,CDF1,CDF2,UPHI,UTHETA, UENER,APHI,ATHETA,AENER,np,nt,ne)

implicit none
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)

integer(kind=ski), intent(in)  :: np,nt,ne

!integer(kind=ski), intent(in)    :: np,nt,ne,itype,iangle

!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E), intent(in) :: cdf0,uphi
!real(kind=skr),dimension(NDIM_A,NDIM_E),    intent(in) :: cdf1,utheta
!real(kind=skr),dimension(NDIM_E),       intent(in) :: cdf2,uener
!
!real(kind=skr),dimension(NDIM_A,NDIM_A,NDIM_E), intent(out) :: aphi
!real(kind=skr),dimension(NDIM_A,NDIM_E),    intent(out) :: atheta
!real(kind=skr),dimension(NDIM_E),       intent(out) :: aener

real(kind=skr),dimension(np,nt,ne), intent(in) :: cdf0,uphi
real(kind=skr),dimension(nt,ne),    intent(in) :: cdf1,utheta
real(kind=skr),dimension(ne),       intent(in) :: cdf2,uener

real(kind=skr),dimension(np,nt,ne), intent(out) :: aphi
real(kind=skr),dimension(nt,ne),    intent(out) :: atheta
real(kind=skr),dimension(ne),       intent(out) :: aener

!DIMENSION  UPHI(31,31,51),UTHETA(31,51),UENER(51)
!DIMENSION  CDF0(31,31,51),CDF1(31,51),CDF2(51)
!DIMENSION  APHI(31,31,51),ATHETA(31,51),AENER(51)

!DIMENSION  XY(2,201),X_NEW(201)
real(kind=skr),dimension(2,201)  :: xy
real(kind=skr),dimension(201)    :: x_new

integer(kind=ski) :: i,j,k,jstart,kstart
real(kind=skr)    :: ener,adjust,theta

!C
!C ADJUST is the allowed movement for INV_LINEAR.
!C
ADJUST = 1.0D0/NCOMP

IF (IPASS.EQ.0) THEN
    GO TO 10
ELSE IF (IPASS.EQ.1) THEN
    DO K = 1, NE
        AENER(K) = UENER(K)
    END DO
    GO TO 20
ELSE IF (IPASS.EQ.2) THEN
    DO K = 1, NE
        AENER(K) = UENER(K)
        DO J = 1, NT
            ATHETA(J,K) = UTHETA(J,K)
        END DO
    END DO
    GO TO 30
END IF

!C
!C First invert the energy.
!C 
10 continue

DO 39 K = 1, NE
    XY(1,K) = UENER(K)
    XY(2,K) = CDF2(K)
39 CONTINUE
CALL INV_LINEAR (XY,NE,X_NEW,ADJUST)
DO K = 1, NE
    AENER(K) = X_NEW(K)
END DO
!C
!C Then theta.
!C
20 continue

KSTART = 1
DO 59 K = 1, NE
    ENER = AENER(K)
    !C +++
    !C Replace DO ... WHILE with standard stuff.
    !C
    !C   DO WHILE (ENER.GT.UENER(KSTART))
    !C     KSTART = KSTART + 1
    !C   END DO
    !C
    !C ---
    69 CONTINUE
    
    IF (ENER.GT.UENER(KSTART)) THEN
        KSTART = KSTART + 1
        GOTO 69
    END IF
    
    DO 79 J = 1, NT
        XY(1,J) = UTHETA(J,KSTART)
        XY(2,J) = CDF1(J,KSTART)
    79 CONTINUE
    CALL INV_LINEAR (XY,NT,X_NEW,ADJUST)
    DO J = 1, NT
        ATHETA(J,K) = X_NEW(J)
    END DO
59 CONTINUE
!C
!C Now phi.
!C

30 continue

KSTART = 1
DO 99 K = 1, NE
    ENER = AENER(K)
    !C +++
    !C Replace DO ... WHILE with standard stuff.
    !C   DO WHILE (ENER.GT.UENER(KSTART))
    !C     KSTART = KSTART + 1
    !C   END DO
    !C ---
    !C
    109   CONTINUE
    IF (ENER.GT.UENER(KSTART)) THEN
        KSTART = KSTART + 1
        GOTO 109
    ENDIF
    !C
    JSTART = 1
    DO 119 J = 1, NT
        THETA = ATHETA(J,K)
        !C +++
        !C Replace DO ... WHILE with standard stuff.
        !C
        !C     DO WHILE (THETA.GT.UTHETA(JSTART,KSTART))
        !C       JSTART = JSTART + 1
        !C     END DO
        !C ---
        !C
        129     CONTINUE
        IF (THETA.GT.UTHETA(JSTART,KSTART)) THEN
            JSTART = JSTART + 1
            GOTO 129
        ENDIF
        !C
        DO 139 I = 1, NP
            XY(1,I) = UPHI(I,JSTART,KSTART)
            XY(2,I) = CDF0(I,JSTART,KSTART)
        139       CONTINUE
        CALL INV_LINEAR (XY,NP,X_NEW,ADJUST)
        DO I = 1, NP
            APHI(I,J,K) = X_NEW(I) 
        END DO
    119 CONTINUE
99 CONTINUE

RETURN

END SUBROUTINE UInvert

!C+++
!C
!C	SUBROUTINE		INV_LINEAR
!C
!C	PURPOSE			Use linear interpolation to make an array 
!C				equal space in Y-axis
!C
!C	INPUT:			XY(2,201) array.
!C				
!C	OUTPUT:			X_NEW(201), the new X array.
!C
!C---
SUBROUTINE Inv_Linear (XY,NE,X_NEW,ADJUST)

implicit none
!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
!DIMENSION XY(2,201),X_NEW(201)

real(kind=skr),dimension(2,201),intent(in)  :: xy
real(kind=skr),                 intent(in)  :: adjust
integer(kind=ski),              intent(in)  :: ne

real(kind=skr),dimension(201),  intent(out) :: x_new

real(kind=skr)     :: astep,xstep,ystep,y_new
integer(kind=ski)  :: i,istart

YSTEP = (XY(2,NE)-XY(2,1))/(NE-1)
ISTART = 1
X_NEW(1) = XY(1,1)
X_NEW(NE) = XY(1,NE)

DO 15 I = 2, NE-1
    Y_NEW = XY(2,1) + (I-1)*YSTEP
    
    !C +++
    !C Replace DO ... WHILE with GOTO statements.
    !C
    !C   DO WHILE (Y_NEW.GT.XY(2,ISTART))
    !C     ISTART = ISTART + 1
    !C   END DO
    !C ---
    19 CONTINUE
    IF (Y_NEW.GT.XY(2,ISTART)) THEN
        ISTART = ISTART + 1
        GOTO 19
    ENDIF
    
    IF (XY(2,ISTART).NE.XY(2,ISTART-1)) THEN
        X_NEW(I) = XY(1,ISTART-1) + (XY(1,ISTART) - XY(1,ISTART-1))/ &
            (XY(2,ISTART) - XY(2,ISTART-1))* &
            (Y_NEW - XY(2,ISTART-1))
    ELSE
        !C
        !C If the probability is the same, then just use the average value.
        !C
        X_NEW(I) = (XY(1,ISTART-1)+XY(1,ISTART))/2.0D0
    END IF

15 CONTINUE

!C
!C Now check whether any 2 adjacant points are spaced too far apart in X.
!C
ASTEP = (XY(1,NE)-XY(1,1))/(NE-1)
DO 25 I = 1, NE-1
    XSTEP = X_NEW(I+1) - X_NEW(I)
    IF (XSTEP.GT.ASTEP) THEN
        X_NEW(I)  = X_NEW(I) + ADJUST*XSTEP
        X_NEW(I+1)  = X_NEW(I+1) - ADJUST*XSTEP
    END IF
25 CONTINUE

X_NEW(1) = XY(1,1)
X_NEW(NE) = XY(1,NE)
!C
RETURN
END SUBROUTINE Inv_Linear

!C+++
!C
!C	PROGRAM		UNDUL_CDF
!C
!C	PURPOSE		This is the main calling program to
!C			a) read in the # of photon
!C			b) build the CDFs
!C			c) invert the CDFs or write them out
!C
!C---
SUBROUTINE Undul_Cdf

!implicit real(kind=skr) (a-h,o-z)
!implicit integer(kind=ski)        (i-n)
implicit none

NAMELIST /PARAIN/ NCOMP,RCURR,ICOMP,BPASS, &
    IANGLE,IAPERTURE,IEXTERNAL, &
    FOUT,FIN,FTRAJ,EMIN,EMAX, &
    THEMIN,THEMAX,PHIMIN,PHIMAX, &
    NE,NT,NP,NCHECK,IOPT,ITER,IPASS, &
    I_EDIV,EDIVX,EDIVY,FINT,IINT

!DIMENSION UPHI(31,31,51),UTHETA(31,51),UENER(51)
!DIMENSION RN0(31,31,51),RN1(31,51),RN2(51)
!DIMENSION POL_DEG(31,31,51)
!DIMENSION CDF0(31,31,51),CDF1(31,51),CDF2(51)
!DIMENSION APHI(31,31,51),ATHETA(31,51),AENER(51)

real(kind=skr),dimension(:,:,:),allocatable  :: cdf0,uphi,aphi,rn0,pol_deg
real(kind=skr),dimension(:,:),allocatable    :: cdf1,utheta,atheta,rn1
real(kind=skr),dimension(:),allocatable      :: cdf2,uener,aener,rn2

integer(kind=ski)  :: i,j,k
!C
!C Read in the parameters from namelist file
!C

OPEN (21, FILE='uphot.nml', STATUS='OLD')
READ (21, NML=PARAIN)
CLOSE (21)
!C
!C Read in the arrays
!C

IF (use_undulator_binary_files .EQ. 1) THEN
    OPEN (40, FILE='uphot.dat', STATUS='OLD', FORM='UNFORMATTED')
    READ (40) NE, NT, NP
ELSE
    OPEN (40, FILE='uphot.dat', STATUS='OLD', FORM='FORMATTED')
    READ (40,*) NE, NT, NP
END IF

!
! allocate arrays
!
allocate( cdf2(NE) )
allocate( UENER(NE) )
allocate( aener(NE) )
allocate( rn2(NE) )

allocate( cdf1(NT,NE) )
allocate( UTHETA(NT,NE) )
allocate( aTHETA(NT,NE) )
allocate( rn1(NT,NE) )

allocate( cdf0(NP,NT,NE) )
allocate( UPHI(NP,NT,NE) )
allocate( aPHI(NP,NT,NE) )
allocate( rn0(NP,NT,NE) )
allocate( pol_deg(NP,NT,NE) )

!
IF (use_undulator_binary_files .EQ. 1) THEN
    DO K = 1, NE
        READ (40) UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            READ (40) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) UPHI(I,J,K)
            END DO
        END DO
    END DO


    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) RN0(I,J,K)
            END DO
        END DO
    END DO


    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) POL_DEG(I,J,K)
            END DO
        END DO
    END DO
ELSE
    DO K = 1, NE
        READ (40,*) UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            READ (40,*) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) UPHI(I,J,K)
            END DO
        END DO
    END DO


    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) RN0(I,J,K)
            END DO
        END DO
    END DO


    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) POL_DEG(I,J,K)
            END DO
        END DO
    END DO
END IF

CLOSE (40)

!C
!C Integrate RN0 to get RN1 and RN2
!C  
CALL RNS (RN0,RN1,RN2,UPHI,UTHETA,UENER,np,nt,ne)
!C
!C Generate the 3 CDFs
!C
CALL UCDF (RN0,RN1,RN2,CDF0,CDF1,CDF2,UPHI,UTHETA,UENER,np,nt,ne)
!C
!C Check if the user want to write out RNs
!C
IF (IPASS.EQ.0) THEN
    CALL UWRITE (RN0,RN1,RN2,POL_DEG,CDF0,CDF1,CDF2,UPHI,UTHETA,UENER,np,nt,ne)
    !C
    !C See if the no. of times of optimization is finished
    !C
    IF (ITER.EQ.IOPT) THEN
        !CALL EXIT (0)
        RETURN
    END IF
END IF

!
! srio@esrf.eu 
! the following part (till the end proroutine) is only run if 
! OPTIMIZATION is set, an option not recommended (and not tested!). 
!
print*,''
print*,'----------------------------------------------------------------'
print*,''
print*,'undul_cdf: Warning: undulator optimization set (not recommended)'
print*,''
print*,'----------------------------------------------------------------'
print*,''

!C
!C Invert the CDFs so that they are equal space in probability (Y-axis). A new 
!C set of (energy, theta, phi) is created.
!C
CALL UINVERT (CDF0,CDF1,CDF2,UPHI,UTHETA,UENER,APHI,ATHETA,AENER,np,nt,ne)

IF (IPASS.EQ.0) THEN
    DO K = 1, NE
        UENER(K)  = AENER(K)
        DO J = 1, NT
            UTHETA(J,K) = ATHETA(J,K)
            DO I = 1, NP
                UPHI(I,J,K) = APHI(I,J,K)
            END DO
        END DO
    END DO
    IPASS = IPASS + 1
ELSE IF (IPASS.EQ.1) THEN
    DO K = 1, NE
        DO J = 1, NT
            UTHETA(J,K) = ATHETA(J,K)
            DO I = 1, NP
                UPHI(I,J,K) = APHI(I,J,K)
            END DO
        END DO
    END DO
    IPASS = IPASS + 1
ELSE IF (IPASS.EQ.2) THEN
    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                UPHI(I,J,K) = APHI(I,J,K)
            END DO
        END DO
    END DO
    IPASS = 0
    ITER = ITER + 1
END IF
!C
!C Write the new (energy, theta, phi) array
!C

IF (use_undulator_binary_files .EQ. 1) THEN
    OPEN (45, FILE='uphot.dat', STATUS='UNKNOWN', FORM='UNFORMATTED')
    REWIND (45)
    WRITE (45) NE, NT, NP
ELSE
    OPEN (45, FILE='uphot.dat', STATUS='UNKNOWN', FORM='FORMATTED')
    REWIND (45)
    WRITE (45,*) NE, NT, NP
END IF

IF (use_undulator_binary_files .EQ. 1) THEN
    DO K = 1, NE
        WRITE (45) UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            WRITE (45) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                WRITE (45) UPHI(I,J,K)
            END DO
        END DO
    END DO
ELSE
    DO K = 1, NE
        WRITE (45,*) UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            WRITE (45,*) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                WRITE (45,*) UPHI(I,J,K)
            END DO
        END DO
    END DO
END IF

CLOSE (45)
!C
!C Write out the parameters to namelist file
!C
OPEN (31, FILE='uphot.nml', STATUS='UNKNOWN')
REWIND (31)
WRITE (31, NML=PARAIN)
CLOSE (31)
PRINT *,'File (namelist) written to disk: uphot.nml'
!
! deallocate arrays
!
if (allocated( cdf2   )) deallocate( cdf2 )
if (allocated( uener  )) deallocate( uener )
if (allocated( aener  )) deallocate( aener )
if (allocated( rn2  )) deallocate( rn2 )
if (allocated( cdf1  )) deallocate( cdf1 )
if (allocated( utheta  )) deallocate( utheta )
if (allocated( atheta  )) deallocate( atheta )
if (allocated( rn1  )) deallocate( rn1 )
if (allocated( cdf0  )) deallocate( cdf0 )
if (allocated( uphi  )) deallocate( uphi )
if (allocated( aphi  )) deallocate( aphi )
if (allocated( rn0  )) deallocate( rn0 )
if (allocated( pol_deg  )) deallocate( pol_deg )

END SUBROUTINE Undul_Cdf

!
!
!  Subroutine undul_phot_dump
!
!  Creates an ascii file with the contents of nphoton.dat (created by undul_phot)  
!
!  input file:  nphoton.dat
!  output file: nphoton.spec
!
!
SUBROUTINE undul_phot_dump
implicit none

integer(kind=ski):: i,j,k,ne,nt,np,itmp
real(kind=skr),dimension(:),allocatable          :: uener
real(kind=skr),dimension(:,:),allocatable        :: utheta
real(kind=skr),dimension(:,:,:),allocatable      :: uphi,rn0,pol_deg
real(kind=skr) :: xx,zz,tmp_p,tmp_i

!C
!
!
! get the arrays
!
!
IF (use_undulator_binary_files .EQ. 1) THEN
    OPEN    (40, FILE='uphot.dat', STATUS='OLD', FORM='UNFORMATTED')
    READ    (40)    NE, NT, NP
ELSE
    OPEN    (40, FILE='uphot.dat', STATUS='OLD', FORM='FORMATTED')
    READ    (40,*)    NE, NT, NP
ENDIF

    !
    ! allocate arrays
    !
    allocate( UENER(NE) )
    allocate( UTHETA(NT,NE) )
    allocate( UPHI(NP,NT,NE) )
    allocate( RN0(NP,NT,NE) )
    allocate( POL_DEG(NP,NT,NE) )


IF (use_undulator_binary_files .EQ. 1) THEN
    DO K = 1, NE
        READ (40)    UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            READ (40) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) UPHI(I,J,K)
            END DO
        END DO
    END DO
    !
    !
    ! get the intesity and pol_deg
    !
    !
    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) RN0(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40) POL_DEG(I,J,K)
            END DO
        END DO
    END DO
ELSE
    DO K = 1, NE
        READ (40,*)    UENER(K)
    END DO

    DO K = 1, NE
        DO J = 1, NT
            READ (40,*) UTHETA(J,K)
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) UPHI(I,J,K)
            END DO
        END DO
    END DO
    !
    !
    ! get the intesity and pol_deg
    !
    !
    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) RN0(I,J,K)
            END DO
        END DO
    END DO

    DO K = 1, NE
        DO J = 1, NT
            DO I = 1, NP
                READ (40,*) POL_DEG(I,J,K)
            END DO
        END DO
    END DO
ENDIF


close(40)


print*,"Dumping arrays Theta Phi Energy: ",nt,np,ne

open (unit=29,file='uphot.spec',status='UNKNOWN')
write(29,'(a18,2X,f12.5)') "#F uphot.spec",uener(k)
write(29,'(a)') "#U Undulator flux calculated by shadow3/undul_phot"

!energy spectrum
write(29,'(a)') ""
write(29,'(a18,2x,f13.5)') "#S 1 ENERGY SPECTRUM"
write(29,'(a)') "#N 3"
write(29,'(a)') "#L ENERGY [eV]  POL_DEG  INTENSITY [a.u.]"
do k=1,ne
    itmp = 0
    tmp_p = 0e0
    tmp_i = 0e0
    DO I=1,NP
        DO J=1,NT
           itmp = itmp + 1
           tmp_p = tmp_p + pol_deg(i,j,k)
           tmp_i = tmp_i + rn0(i,j,k)
        END DO
    END DO
    !write(29,*) uener(k),tmp_p/itmp,tmp_i/itmp
    write(29,*) uener(k),tmp_p/itmp,tmp_i/itmp
                !(0.001*uener(k))* &    ! from eV to 0.1%bw
                !(utheta(nt,1))**2     ! integral in phi (2pi) and theta

end do

    

do k=1,ne
    ! THETA PHI INT
    write(29,'(a)') ""
    write(29,'(a51,2x,f13.5)') "#S 2 FLUX [Ph/s/eV/rad^2] vs ANGLE [rad] for E[eV]=",uener(k)
    write(29,'(a)') "#N 4"
    write(29,'(a)') "#L THETA [rad]  PHI [rad]  POL_DEG  INTENSITY [Ph/s/eV/rad^2]"
    DO I=1,NP
        DO J=1,NT
            write(29,*) utheta(j,k),uphi(i,j,k),pol_deg(i,j,k),rn0(i,j,k)
        END DO
    END DO
end do

close(unit=29)
print *,'File written to disk: uphot.spec'

!
! deallocate arrays
!
if (allocated( UENER   )) deallocate( UENER )
if (allocated( UTHETA  )) deallocate( UTHETA )
if (allocated( UPHI    )) deallocate( UPHI ) 
if (allocated( RN0     )) deallocate( RN0 ) 
if (allocated( POL_DEG )) deallocate( POL_DEG ) 

    
END SUBROUTINE undul_phot_dump



! +++
!         SUBROUTINE      WIGGLER_SPECTRUM
! 
!         PURPOSE         To take the output from EPATH and compute the no. of
!                         photons generated along the trajectory. The output is
!                         a file containing the energy spectrum of the wiggler
! 
!         NOTE            Everything is in SHADOW's referance frame.
! ---

subroutine wiggler_spectrum
        implicit none

        integer(kind=ski)                 ::  N_DIM=10000
        real(kind=skr),dimension(10000)   ::  Y,BETAY,X,BETAX,CURV,PHOT_NUM,PHOT_CDF
        real(kind=skr),dimension(10000)   ::  Z,BETAZ,DS,S, DX, DY, DZ
        real(kind=skr),dimension(NDIM_TRAJ)    ::  TAUX,TAUY,TAUZ, BX,BY,BZ, ENX,ENY,ENZ
        character(len=sklen)              ::  INFILE,OUTFILE
        real(kind=skr)                    ::  pnum,rad,bener,emin,emax,gamma1,step
        real(kind=skr)                    ::  phot_min,phot_max,eloopmin, eloopmax, estep 
        real(kind=skr)                    ::  r_intens, energy, deltae, coeff, ang_nim
        real(kind=skr)                    ::  ang_num, curv_max, curv_min, tot_num
        integer(kind=ski)                 ::  iflag,i_wig,nstep, i_units, np, i, ii
! 
!  Read in the CDF of G0, and generated the spline coefficients.
! 
        IFLAG        = -1
        CALL        NPHOTONcalc (PNUM,RAD,BENER,EMIN,EMAX,IFLAG)
! 
        WRITE(6,*) ' '
        WRITE(6,*) '************************ WIGGLER RADIATION *************************'
        WRITE(6,*) '************************ ENERGY SPECTRUM   *************************'
        WRITE(6,*) ' '
        INFILE        = RSTRING ('Name of input file : ')
!         
!         (I_WIG.EQ.1) implies normal wiggler.
!         (I_WIG.EQ.2) implies elliptical wiggler.
! 
        WRITE(6,*) ' '
        WRITE(6,*) 'Type of Wiggler.'
        WRITE(6,*) 'Enter:'
        WRITE(6,*) 'for normal wiggler   [1]'
        WRITE(6,*) 'for elliptical wiggler [2]'
        I_WIG=IRINT('Then? ')
! 
        OPEN (20,FILE=INFILE,STATUS='OLD')
! 
        DO 99 I = 1, N_DIM+1
          READ (20,*,END=101) X(I),Y(I),Z(I),BETAX(I),BETAY(I),BETAZ(I),CURV(I)
 99        CONTINUE
        !STOP         'Too many points from input file.'
        print *,'Too many points from input file.'
        return
101        NP        = I - 1
        CLOSE        (20)
        WRITE(6,*) 'Read ',NP,' points from input file.'
        STEP        = SQRT((Y(2)-Y(1))**2 + (X(2)-X(1))**2 + (Z(2)-Z(1))**2)
! 
!  Compute gamma and the beam energy
! 
        gamma1        = 1/SQRT(1-(BETAY(1)**2)-(BETAX(1)**2)-(BETAZ(1)**2))
        !TODO:
        !BENER        = gamma1*(9.109D-31)*(2.998d8**2)/(1.602e-19)*1.0d-9
        BENER        = gamma1*(codata_rm)*(codata_c**2)/(codata_e)*1.0d-9
         WRITE(6,*) 'Beam energy (GeV) = ',BENER
! 
!  Figure out the limit of photon energy.
! 
        CURV_MAX        = 0.0D0
        CURV_MIN        = 1.0D20
        DO 199 I = 1, NP
          CURV_MAX        = MAX(ABS(CURV(I)),CURV_MAX)
          CURV_MIN        = MIN(ABS(CURV(I)),CURV_MIN)
 199        CONTINUE
        WRITE(6,*) 'Radius of curvature (max.) = ',1/CURV_MIN,' m'
        WRITE(6,*) '                    (min.) = ',1/CURV_MAX,' m'
        PHOT_MIN        = TOANGS*3.0D0*gamma1**3/4.0D0/PI/1.0D10*CURV_MIN
        PHOT_MAX        = TOANGS*3.0D0*gamma1**3/4.0D0/PI/1.0D10*CURV_MAX
        WRITE(6,*) 'Critical Energy (max.) = ',PHOT_MAX,' eV'
        WRITE(6,*) '                (min.) = ',PHOT_MIN,' eV'
!         WRITE(6,*) 'Use photon energy between ',
!      $                PHOT_MAX*10,' eV and ',PHOT_MIN*1.0D-5,' eV'
! 
        ELOOPMIN= RNUMBER ('Initial photon energy [ eV ] : ')
        ELOOPMAX= RNUMBER ('Final photon energy [ eV ]   : ')
        ESTEP        = RNUMBER ('Step  [ eV ]   : ')
        OUTFILE        = RSTRING ('Name of output file : ')
        WRITE(6,*) 'Units for the result file:'
        WRITE(6,*) '  eV, Phot/sec/eV     [1]'
        WRITE(6,*) '  eV, Phot/sec/0.1%bw [2]'
        WRITE(6,*) '  eV, Watts/eV        [3]'
        WRITE(6,*) '  eV, Watts/0.1%bw    [4]'
        I_UNITS=IRINT(' Then? ')
        R_INTENS= RNUMBER ('Electron beam current [mA]  : ')
        open (33, file=outfile, status='unknown')
! 
!  starts the loop in energy
! 
        nstep = (eloopmax-eloopmin)/estep + 1
        !do 555 energy=eloopmin,eloopmax,estep
        do 555 ii=1,nstep
        energy = eloopmin + (ii-1)*estep
        if (i_units.eq.1) then 
          deltae = 0.5D0
          coeff  = 1.0D0
        else if (i_units.eq.2) then 
          deltae = 0.5D0*0.001*energy
          coeff  = 1.0D0
        else if (i_units.eq.3) then 
          deltae = 0.5D0
          coeff  = energy*1.602189D-19
        else if (i_units.eq.4) then 
          deltae = 0.5D0*0.001*energy
          coeff  = energy*1.602189D-19
        endif
        emin = energy-deltae
        emax = energy+deltae
! 
!  NPHOTON computes the no. of photons per mrad (ANG_NUM) at each point.  
!  It is then used to generate the no. of photons per axial length (PHOT_NUM)
!  along the trajectory S.
! 
        DO 299 I = 1, NP
          IF (ABS(CURV(I)).LT.1.0D-10) THEN
            ANG_NUM        = 0.0D0        
          ELSE
            RAD        = ABS(1.0D0/CURV(I))
            IFLAG = 1
            CALL        NPHOTONcalc (ANG_NUM,RAD,BENER,EMIN,EMAX,IFLAG)
          END IF
          PHOT_NUM(I) = ANG_NUM*ABS(CURV(I))*SQRT(1+(BETAX(I)/BETAY(I))**2+ &
                       (BETAZ(I)/BETAY(I))**2)*1.0D3
299     CONTINUE
! 
!  Computes CDF of the no. of photon along the trajectory S.
!  In the elliptical case, the entire traversed path length (DS) is computed.
!  In the normal case, only the component (Y) in the direction of propagation
!  is computed.
! 
         DO 399 I = 2, NP
        IF (I_WIG.EQ.2) THEN
          DS(1) = 0.0D0
                DX(I) = X(I) - X(I-1)
                DY(I) = Y(I) - Y(I-1)
                DZ(I) = Z(I) - Z(I-1)
                DS(I) = SQRT(DX(I)**2 + DY(I)**2 + DZ(I)**2) + DS(I-1)
           PHOT_CDF(I)        = PHOT_CDF(I-1) + (PHOT_NUM(I-1) + PHOT_NUM(I))*0.5D0*(DS(I) - DS(I-1))
        ELSE
           PHOT_CDF(I)        = PHOT_CDF(I-1) + (PHOT_NUM(I-1) + PHOT_NUM(I))*0.5D0*(Y(I) - Y(I-1))
        END IF
 399     CONTINUE
         TOT_NUM        = PHOT_CDF(NP)
         WRITE(6,*)  energy,coeff*TOT_NUM*R_INTENS
         WRITE(33,*) energy,coeff*TOT_NUM*R_INTENS
555        continue
        close(33)

end subroutine wiggler_spectrum

!
! emittance_test: wiggler emittance
!            test the wiggler emittance
!
! inputs (keyboard): 
!            data for emittance (sigma, sigma' and waist position OR
!                                Twiss parameters)
! outputs: 
!        shadow binary : a shadow binary file with the x,x',z,z' 
!
!
subroutine emittance_test()
     
implicit none 

character(len=sklen)                         :: outFile
real(kind=skr), dimension(:,:), allocatable  :: ray

real(kind=skr)    :: sigmaX,sigmaXp,sigmaZ,sigmaZp,rhoX,rhoZ, yCoor
real(kind=skr)    :: x1,xp1,z1,zp1, dX, dZ
real(kind=skr)    :: alphaX,betaX,gammaX,emittX, alphaZ,betaZ,gammaZ,emittZ
real(kind=skr)    :: alphaX_old,betaX_old, alphaZ_old,betaZ_old
real(kind=skr)    :: fpoint1,sX,sXp,sZ,sZp
integer(kind=ski) :: nPoint1,ncol1,iFlag,iErr,i,j,IS,input_type
    
ncol1 = 18
!IS = 57
!
! input section
!
print *,' '
print *,'    *****       emittance_test     ****      '
print *,' This application samples rays containing position and directions of the '   
print *,' electrons at a given point of the storage ring, and writes them in a '
print *,' SHADOW formatted file. '
print *,' The input is either the electron beam data at waist (sigmas and position) '
print *,' or electron beam data at the wanted position (sigmas and correlation OR '
print *,' Twiss parameters). '
print *,' '
print *,' '


print *,' This application samples rays containing position and directions of the '   
outFile = rstring('     output file name (SHADOW binary, e.g. begin.dat): ')
npoint1 = irint(  '     number of points (e.g. 20000): ') 
print *,'Please enter input type: '
print *,'    [0] second moment matrix elements'
print *,'    [1] sigmas at waists and distances from them'
print *,'    [2] optical functions (alpha, beta, gamma)'
input_type = irint(  '     >?')

select case(input_type)
    case(0)
        sigmaX =  rnumber("     sigmaX: ")
        sigmaXp = rnumber("     sigmaXp: ")
        rhoX =    rnumber("     rhoX: ")
        sigmaZ =  rnumber("     sigmaZ: ")
        sigmaZp = rnumber("     sigmaZp: ")
        rhoZ =    rnumber("     rhoZ: ")
        print *,'----- inputs --------------------------'
        print *,"         "
        print *,"         sigmaX,sigmaXp",sigmaX,sigmaXp
        print *,"         sigmaZ,sigmaZp",sigmaZ,sigmaZp
        print *,"         "
    case(1)
        yCoor =  rnumber("     y coordinate (y=0 for the device center): ")
        sigmaX =  rnumber("     sigma X at waist: ")
        sigmaXp = rnumber("     sigma prime X at waist: ")
        dX =    rnumber("     distance from X waist to ID center (where y=0): ")
        sigmaZ =  rnumber("     sigma Z at waist: ")
        sigmaZp = rnumber("     sigma prime Z at waist: ")
        dZ =    rnumber("     distance from Z waist to ID center (where y=0): ")

        print *,'----- inputs (at waist)--------------------------'
        print *,"         "
        print *,"         sigmaX,sigmaXp",sigmaX,sigmaXp
        print *,"         distance from X waist to ID center (where y=0): ",dX
        print *,"         sigmaZ,sigmaZp",sigmaZ,sigmaZp
        print *,"         distance from Z waist to ID center (where y=0): ",dZ
        print *,"         "
        print *,"         epsiX=sigmaX.sigmaXp: ",sigmaX*sigmaXp
        print *,"         epsiZ=sigmaZ.sigmaZp: ",sigmaZ*sigmaZp


        ! calculate sigmas and correlations at given point: 
        ! TODO: check negative distance
        sigmaX = sqrt( sigmaX**2 + ( (dX+yCoor)*sigmaXp)**2 )
        sigmaXp = sigmaXp ! no change
        rhoX = (dX+yCoor) * sigmaXp**2
        rhoX = rhoX/sigmaX/sigmaXp

        sigmaZ = sqrt( sigmaZ**2 + ((dZ+yCoor)*sigmaZp)**2 )
        sigmaZp = sigmaZp ! no change
        rhoZ = (dZ+yCoor) * sigmaZp**2
        rhoZ = rhoZ/sigmaZ/sigmaZp

        print *,'----- values at working point in the trajectory: '
        print *,"         yCoor: ",yCoor
        print *,"         "
        print *,"         \sqrt{<x^2>}: ",sigmaX
        print *,"               <x x'>: ",rhoX*sigmaX*sigmaXp
        print *,"                 rhoX: ",rhoX
        print *,"        \sqrt{<x'^2>}: ",sigmaXp
        print *,"         "
        print *,"         \sqrt{<z^2>}: ",sigmaZ
        print *,"               <z z'>: ",rhoZ*sigmaZ*sigmaZp
        print *,"                 rhoZ: ",rhoZ
        print *,"        \sqrt{<z'^2>}: ",sigmaZp

    case(2)
        yCoor  =  rnumber("     y coordinate (y=0 for the ID center): ")
        emittX =  rnumber("     emittance X (at y=0): ")
        alphaX =  rnumber("     alpha X (at y=0): ")
        betaX =  rnumber("     beta X (at y=0): ")
        gammaX =  rnumber("     gamma X (at y=0): ")
        emittZ =  rnumber("     emittance Z (at y=0): ")
        alphaZ =  rnumber("     alpha Z (at y=0): ")
        betaZ =  rnumber("     beta Z (at y=0): ")
        gammaZ =  rnumber("     gamma Z (at y=0): ")

        print *,''
        print *,'----- inputs (at y=0)-----------------------------'
        print *,"         "
        print *,"         X,Z emittances: ",emittX,emittZ
        print *,"         X alpha,beta,gamma: ",alphaX,betaX,gammaX
        print *,"         Z alpha,beta,gamma: ",alphaZ,betaZ,gammaZ

        ! if y!=0, move these values to the new position
        if (abs(yCoor) .gt. 1e-12) then 
            ! gamma and emittances do not change! 
            alphaX_old = alphaX
            alphaZ_old = alphaZ
            betaX_old = betaX
            betaZ_old = betaZ

            betaX = betaX_old - 2*alphaX_old*yCoor+gammaX*yCoor*yCoor
            betaZ = betaZ_old - 2*alphaZ_old*yCoor+gammaZ*yCoor*yCoor
            alphaX = alphaX_old - gammaX*yCoor
            alphaZ = alphaZ_old - gammaZ*yCoor
            print *,''
            print *,'----- Twiss parameters (at new y)------------------'
            print *,"         "
            print *,"         yCoor: ",yCoor
            print *,"         X,Z emittances: ",emittX,emittZ
            print *,"         X alpha,beta,gamma: ",alphaX,betaX,gammaX
            print *,"         Z alpha,beta,gamma: ",alphaZ,betaZ,gammaZ
        endif 

        ! calculate sigmas and correlations at given point: 
        sigmaX = sqrt( emittX*betaX)
        sigmaXp = sqrt(emittX*gammaX)
        rhoX = -emittX*alphaX
        rhoX = rhoX/sigmaX/sigmaXp
        sigmaZ = sqrt( emittZ*betaZ)
        sigmaZp = sqrt(emittZ*gammaZ)
        rhoZ = -emittZ*alphaZ
        rhoZ = rhoZ/sigmaZ/sigmaZp

        print *,'----- values at working point in the trajectory: '
        print *,"         yCoor: ",yCoor
        print *,"         "
        print *,"         \sqrt{<x^2>}: ",sigmaX
        print *,"               <x x'> : ",rhoX*sigmaX*sigmaXp
        print *,"                 rhoX : ",rhoX
        print *,"         \sqrt{<x'^2>}: ",sigmaXp
        print *,"         "
        print *,"         \sqrt{<z^2>}: ",sigmaZ
        print *,"               <z z'> : ",rhoZ*sigmaZ*sigmaZp
        print *,"                 rhoZ : ",rhoZ
        print *,"         \sqrt{<z'^2>}: ",sigmaZp
    case default
end select
 
!
! allocate arrays
!
if (ALLOCATED(RAY)) DEALLOCATE(RAY)
    if (.not. ALLOCATED(RAY)) then
        ALLOCATE(RAY(18,NPOINT1),stat=ierr)
        if (ierr /= 0) then
            call leave ('EMITTANCE_TEST','Error allocating array',IERR)
        end if
        ray = 0.0d0
end if
 
!
! fill array
!
do i=1,npoint1
   ray(10,i) = 1.0 ! lost ray flag
   ray(11,i) = 1.0 ! k (energy)
   ray(12,i) = i   ! ray counter
   ray(7,i)  = 1.0 ! Ex_sigma
   !TODO: check if call is with rho or rho^2
   call binormal(sigmax,sigmaxp,rhoX,x1,xp1,IS)
   !OLD call gauss(sigmax,sigmaxp,yCoor,x1,xp1,IS)
   ray(1,i) = x1
   ray(4,i) = xp1
   call binormal(sigmaz,sigmazp,rhoZ,z1,zp1,IS)
   ! OLD call gauss(sigmaz,sigmazp,yCoor,z1,zp1,IS)

   ray(3,i) = z1
   ray(6,i) = zp1
end do

!compute moments
mx11 = 0.0
mx22 = 0.0 
mx12 = 0.0
mz11 = 0.0
mz22 = 0.0
mz12 = 0.0
do i=1,npoint1
   mx11 = mx11 + ray(1,i)*ray(1,i)
   mx22 = mx22 + ray(4,i)*ray(4,i)
   mz11 = mz11 + ray(3,i)*ray(3,i)
   mz22 = mz22 + ray(6,i)*ray(6,i)

   mx12 = mx12 + ray(1,i)*ray(4,i)
   mz12 = mz12 + ray(3,i)*ray(6,i)
end do

fpoint1 = float(npoint1)
print*,' '
print*,' '
print*,' '
print*,'<x2>, <xp2>, <x xp>: ',mx11/fpoint1,mx22/fpoint1,mx12/fpoint1
sX = sqrt(mx11/fpoint1)
sXp = sqrt(mx22/fpoint1)
print*,'sigmaX, sigmaXp, rhoX: ',sX,sXp,mx12/fpoint1/sX/sXp
print*,'original sigmaX, sigmaXp, rhoX: ',sigmaX,sigmaXp,rhoX

print*,' '
print*,'<z2>, <zp2>, <z zp>: ',mz11/fpoint1,mz22/fpoint1,mz12/fpoint1
sZ = sqrt(mz11/fpoint1)
sZp = sqrt(mz22/fpoint1)
print*,'sigmaZ, sigmaZp, rhoZ: ',sZ,sZp,mz12/fpoint1/sz/szp
print*,'original sigmaZ, sigmaZp, rhoZ: ',sigmaZ,sigmaZp,rhoZ
print*,' '
print*,' '
print*,' '

CALL beamWrite(ray,ierr,ncol1,npoint1,outFile)
print*,'File written to disk: '//trim(outfile)

if (allocated( ray ) ) deallocate( ray )


end subroutine emittance_test

End Module shadow_Pre_Sync
