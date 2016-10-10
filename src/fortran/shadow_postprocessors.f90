!----
!----
!---- MODULE:  shadow_post
!----
!---- Postprocessors for Shadow
!---- Contains: 
!----
!----
!----
!---- Example of usage: 
!----
!----

Module Shadow_PostProcessors
    !---- Use Modules ----!

    use stringio
    use shadow_beamio
    use shadow_math, only : wran
    use shadow_globaldefinitions
    use shadow_variables
! DONE: resdistribute variables in shadow_variables. Here only the
!       pool variables should be accessed and not Global variables. 
!       Also, the PoolSourceLoad routine should not be on shadow_kernel
!    use shadow_kernel
!    use shadow_sourcesync

    !---- Variables ----!
    implicit none

!todo: fix this repetition
!
! the global variables here are only used for undulator and not for wiggler
 
    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    !    public :: 
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public :: SourcInfo,MirInfo,SysInfo,Translate,PlotXY
    public :: histo1,histo1_calc, histo1_calc_easy, intens_calc
    public :: FFresnel,FFresnel2D,FFresnel2D_Interface,ReColor,Intens,FocNew
    public :: sysplot, retrace, retrace_interface, shrot, shtranslation
    public :: minmax, reflag, histo3

    !---- List of private functions ----!
    !---- List of private subroutines ----!


    !---- Definitions ----!


    !---- Interfaces ----!


  Contains
    !
    !---- Routines ----!
    !


!C+++
!C	PROGRAM		SOURCE_INFO
!C
!C	PURPOSE		Create a 'nice' printable file with complete
!C			source specifications.
!C
!C	INPUT		A STARTxx.DAT file.
!C
!C---
SUBROUTINE SourcInfo
        implicit none
       
        type (poolSource)      ::  pool00

        character(len=sklen) :: inFile1,file_Out,cd
        character(len=80) :: comment,title
        character(len=79),parameter :: topLin= &
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        character(len=20),dimension(5) :: tSpatial,tDepth,tPhot
        character(len=20),dimension(7) :: tAng
        character(len=20),dimension(3) :: tPol
        character(len=20)              :: break
        real(kind=skr),dimension(10)   :: photonArray,wave

        ! for date_and_time
        character(8)  :: date
        character(10) :: time
        character(5)  :: zone
        integer,dimension(8) :: values

     	!mv to global_definitions real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
	!mv to global_definitions real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
	integer(kind=ski)        :: J


TSPATIAL(1)	='POINT       '
TSPATIAL(4)	='GAUSSIAN    '
TSPATIAL(2)	='RECTANGULAR '
TSPATIAL(3)	='ELLIPTICAL  '
TSPATIAL(5)	='PHASE SPACE ELLIPSE'
TDEPTH(1)	='DEPTH OFF   '
TDEPTH(2)	='DEPTH ON    '
TDEPTH(3) 	='RECTANGULAR '
TDEPTH(4) 	='GAUSSIAN    '
TDEPTH(5)	='SYNCHROTRON '
TANG(1)		='UNIFORM     '
TANG(2)		='LAMBERTIAN  '
TANG(3)		='GAUSSIAN    '
TANG(4)		='SYNCHROTRON '
TANG(5)		='CONICAL     '
TANG(6)		='SYNCHROTRON (exact)'
TANG(7)		='PHASE SPACE ELLIPSE'
TPHOT(1)	='PHOTON OFF  '
TPHOT(2)	='PHOTON ON   '
TPHOT(3)	='SINGLE LINE '
TPHOT(4)	='MULTI LINE  '
TPHOT(5)	='BOX DISTR.  '
TPOL(1)		='SR PARALLEL '
TPOL(2)		='SR PERPEND  '
TPOL(3)		='SR TOTAL    '
BREAK	='    ----------------'

     	WRITE(6,*) &
      '------------------ S O U R C I N F O --------------------'
     	WRITE(6,*) ' '
	inFile1 = RString('SourcInfo: File containing source specs ?')
	title = RString('SourcInfo: Title ? ')
	comment = RString('SourcInfo: Comment ?')
	File_Out = RString('SourcInfo: Output file-name ? ')

! 
! read input file
!
	CALL PoolSourceLoad(pool00,inFile1)


     	OPEN (30,FILE=FILE_OUT,STATUS='UNKNOWN')
	REWIND (30)
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) &
      '**************  S O U R C E       ', &
      'D E S C R I P T I O N  **************'
     	WRITE (30,*) trim(TITLE) 
     	WRITE (30,*) trim(COMMENT)
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) 'Input file specified: '//trim(inFile1)
	CALL GETCWD(CD)
     	WRITE (30,*) 'Full file specification: '//trim(CD)//OS_DS//trim(inFile1)
	call date_and_time(TIME=time,DATE=date,ZONE=zone)
     	WRITE (30,*) 'Creation date: '//date(1:4)//' '//date(5:6)//' '//& 
            date(7:8)//', '//time(1:2)//'h '//time(3:4)//'min '//time(5:6)//' s'

     	  WRITE (30,*)	TOPLIN
!C
!C Prepares now the output to the file.
!C
!C
!C Type of computation
!C
     	IF (pool00%FGRID.EQ.0) THEN
     	  WRITE (30,2000)  
2000	FORMAT (1X,'Random Source.')
     	ELSE IF (pool00%FGRID.EQ.1) THEN
     	  WRITE (30,2001) 
2001	FORMAT (1X,'Grid Source.')
     	ELSE IF (pool00%FGRID.EQ.2) THEN
     	  WRITE (30,2002)
2002	FORMAT (1X,'Mixed Type Source. Spatial: GRID  , directions: RANDOM')
     	ELSE IF (pool00%FGRID.EQ.3) THEN
     	  WRITE (30,2003)
2003	FORMAT (1X,'Mixed Type Source. Spatial: RANDOM, directions: GRID')
	ELSE IF (pool00%FGRID.EQ.4) THEN
	  WRITE (30,2004)
2004	FORMAT (1X,'Phase space ellipses. RANDOM around each ellipse.')
	ELSE IF (pool00%FGRID.EQ.5) THEN
	  WRITE (30,2005)
2005	FORMAT (1X,'Phase space ellipses. GRID around each ellipse.')
     	END IF
	WRITE (30,2006) pool00%NPOINT
2006	FORMAT (1X,'Generated total ',I12,' rays.')
!C
!C Spatial type and values
!C
     	IF (pool00%FSOURCE_DEPTH.EQ.1) THEN
     	  WRITE (30,2008)
2008	FORMAT (1X,'Source assumed BIDIMENSIONAL (flat).')
     	ELSE
     	  WRITE (30,2009)
2009	FORMAT (1X,'Source assumed TRIDIMENSIONAL.')
     	END IF
     	WRITE (30,2010) 	TSPATIAL (pool00%FSOUR+1)
2010	FORMAT (1X,'Source Spatial Characteristics: ',A20)
     	IF (pool00%FSOUR.EQ.1.OR.pool00%FSOUR.EQ.2) THEN 
     	  WRITE (30,2020) pool00%WXSOU,pool00%WZSOU
2020	FORMAT (1X,'Source Width: ',G17.9,' and Height: ',G17.9)
     	ELSE IF (pool00%FSOUR.EQ.3.OR.pool00%FSOUR.EQ.4) THEN
     	  WRITE (30,2030) pool00%SIGMAX,pool00%SIGMAZ
2030	FORMAT (1X,'Sigma X     : ',G17.9,' Sigma Z   : ',G17.9)
     	END IF
     	IF (pool00%FSOURCE_DEPTH.EQ.2) THEN
     	  WRITE (30,2040) pool00%WYSOU
2040	FORMAT (1X,'Depth:  UNIFORM      Value   : ',G17.9)
     	ELSE IF (pool00%FSOURCE_DEPTH.EQ.3) THEN
     	  WRITE (30,2050) pool00%SIGMAY
2050	FORMAT (1X,'Depth:  GAUSSIAN.    Sigma-y : ',G17.9)
     	ELSE IF (pool00%FSOURCE_DEPTH.EQ.4) THEN
     	  WRITE (30,2055)
2055	FORMAT (1X,'Depth:  SYNCHROTRON SOURCE.')
     	END IF
!C
!C Source Emission
!C
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) 'Source Emission Characteristics'
     	WRITE (30,3000)  TANG (pool00%FDISTR)
3000	FORMAT (1X,'Distribution Type: ',A20)
     	IF (pool00%FDISTR.NE.5) THEN
     	  WRITE (30,3010)	 pool00%HDIV1,pool00%HDIV2
     	  WRITE (30,3020)  	 pool00%VDIV1,pool00%VDIV2
3010	FORMAT (1X,'Distribution Limits. +X : ',G17.9,' -X: ',G17.9, ' rad')
3020	FORMAT (1X,'                     +Z : ',G17.9,' -Z: ',G17.9, ' rad')
     	IF (pool00%FDISTR.EQ.3.OR.pool00%FDISTR.EQ.7) THEN
     	  WRITE (30,3026)	pool00%SIGDIX
     	  WRITE (30,3027) 	pool00%SIGDIZ
3026	FORMAT	(1X,'Horiz. StDev : ',G17.9)
3027	FORMAT	(1X,'Verti. StDev : ',G17.9)
	END IF
     	ELSE IF (pool00%FDISTR.EQ.5) THEN
     	  WRITE (30,3025)	 pool00%CONE_MAX,pool00%CONE_MIN
3025	FORMAT (1X,'Cone Outer Aperture : ',G17.9, ' Inner Aperture : ',G17.9)
     	END IF
!C
!C Synchrotron Case
!C
     	IF (pool00%FDISTR.EQ.4.OR.pool00%FDISTR.EQ.6) THEN
     	  WRITE (30,3030) pool00%R_MAGNET,pool00%BENER
3030	FORMAT (1X,'Magnetic Radius = ',G17.9,' m.  Beam Energy = ', &
      G12.5,' GeV.')
     	  WRITE (30,3040) pool00%EPSI_X,pool00%EPSI_Z
     	  WRITE (30,3050) pool00%EPSI_DX,pool00%EPSI_DZ
3040	FORMAT (1X,'Beam Emittancies. EPSI_X: ',G17.9,' EPSI_Z: ', &
      G17.9)
3050	FORMAT (1X,'Distance from Waist.   X: ',G17.9,'      Z: ', &
      G17.9)
     	  WRITE (30,3070) TPOL(pool00%F_POL)
3070	FORMAT (1X,'Polarization Used: ',A12)
     	END IF
!C
!C Photon Energy 
!C
     	 IF (pool00%F_COLOR.NE.0) THEN
     	   WRITE (30,*) TOPLIN
     	   WRITE (30,3080) TPHOT (pool00%F_COLOR+2)
     	   photonArray(1) = pool00%PH1
     	   photonArray(2) = pool00%PH2
     	   photonArray(3) = pool00%PH3
     	   photonArray(4) = pool00%PH4
     	   photonArray(5) = pool00%PH5
     	   photonArray(6) = pool00%PH6
     	   photonArray(7) = pool00%PH7
     	   photonArray(8) = pool00%PH8
     	   photonArray(9) = pool00%PH9
     	   photonArray(10) = pool00%PH10
     	   WAVE(1) = pool00%PH1
     	   WAVE(2) = pool00%PH2
     	   WAVE(3) = pool00%PH3
     	   WAVE(4) = pool00%PH4
     	   WAVE(5) = pool00%PH5
     	   WAVE(6) = pool00%PH6
     	   WAVE(7) = pool00%PH7
     	   WAVE(8) = pool00%PH8
     	   WAVE(9) = pool00%PH9
     	   WAVE(10) = pool00%PH10
3080	FORMAT (1X,'Source Photon Energy Distribution: ',A12)
     	  IF (pool00%F_COLOR.EQ.1) THEN
     	   IF (pool00%F_PHOT.EQ.0) WAVE(1) = TOANGS/photonArray(1)
     	   IF (pool00%F_PHOT.EQ.1) photonArray(1) = TOANGS/WAVE(1)
     	    WRITE (30,3090) photonArray(1),WAVE(1)
3090	FORMAT (1X,'Photon Energy: ',G12.5,' eV, or ',G12.5,' Angs.')
     	  ELSE IF (pool00%F_COLOR.EQ.2) THEN
     	   DO 11 J=1,pool00%N_COLOR
     	    IF (pool00%F_PHOT.EQ.0) WAVE(J) = TOANGS/photonArray(J)
     	    IF (pool00%F_PHOT.EQ.1) photonArray(J) = TOANGS/WAVE(J)
     	     WRITE (30,3090) photonArray(J),WAVE(J)
11        CONTINUE
     	  ELSE
     	   IF (pool00%F_PHOT.EQ.0) WAVE(1) = TOANGS/photonArray(1)
     	   IF (pool00%F_PHOT.EQ.1) photonArray(1) = TOANGS/WAVE(1)
     	    WRITE (30,3100) photonArray(1),WAVE(1)
     	   IF (pool00%F_PHOT.EQ.0) WAVE(2) = TOANGS/photonArray(2)
     	   IF (pool00%F_PHOT.EQ.1) photonArray(2) = TOANGS/WAVE(2)
     	    WRITE (30,3110) photonArray(2),WAVE(2)
3100	FORMAT (1X,'From Photon Energy: ',G17.9,' or ',G17.9,' Angs.')
3110	FORMAT (1X,' to  Photon Energy: ',G17.9,' or ',G17.9,' Angs.')
     	  END IF
	END IF
     	IF (pool00%F_POLAR.EQ.1) THEN
     	    WRITE (30,3130) pool00%POL_ANGLE*TODEG
3130	FORMAT (1X,'Angular difference in phase is ',G12.5)
     	    WRITE (30,3140) pool00%POL_DEG
3140	FORMAT (1X,'Degree of polarization is ',G12.5)
	    IF (pool00%F_COHER.EQ.0) THEN
	      WRITE (30,*) 'Source points have INCOHERENT phase.'
	    ELSE
	      WRITE (30,*) 'Source points have COHERENT phase.'
	    END IF
     	  END IF
          ! source optimization/rejection
          IF (pool00%F_BOUND_SOUR.GT.0) THEN
             write(30,*) 'Source optimization (rejection, variance reduction) used: '
             write(30,*) '    total number of rays been created: ',pool00%NTOTALPOINT
             write(30,*) '    accepted rays (stored): ',pool00%NPOINT
             write(30,*) '    rejected:               ', & 
                         pool00%NTOTALPOINT-pool00%NPOINT
             write(30,*) '    created/accepted ratio: ', & 
                         real(pool00%NTOTALPOINT)/real(pool00%NPOINT)
             IF (pool00%F_BOUND_SOUR.EQ.1) THEN
              write(30,*) '    file with phase-space volume: '//trim(pool00%FILE_BOUND)
             ELSE
              write(30,*) '    file with slit/acceptance: '//trim(pool00%FILE_BOUND)
             ENDIF
          END IF
!C
!C All completed.
!C
     	WRITE (30,*)	TOPLIN
     	WRITE (30,*) &
      '***************                 E N', &
      ' D                  ***************'
     	WRITE (30,*)	TOPLIN
	CLOSE (30)
	print *,'File written to disk: '//trim(file_out)
     	!CALL EXIT (0)
	RETURN
END SUBROUTINE SourcInfo
!
!
!

!C+++
!C	PROGRAM		MIRINFO
!C
!C	PURPOSE		Returns a readable file of a mirror definition
!C			in shadow.
!C
!C---
SUBROUTINE MirInfo

        implicit none ! beautiful!


        type (poolOE)      ::  p1
        character(len=sklen) :: mirFil,file_Out,cd
        character(len=80) :: comment,title
        character(len=20),dimension(12) :: type1
        character(len=79),parameter :: topLin= &
        '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
        ! for date_and_time
        character(len=8)  :: date
        character(len=10) :: time
        character(len=5)  :: zone
        character(len=2)  :: stmp
        integer,dimension(8) :: values

! todo: check that this is also defined in shadow_kernel...
        !mv to global_definitions real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
        !mv to global_definitions real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
        real(kind=skr)           ::ECCENT,AFOCI
        integer(kind=ski)        :: J

        type1(1)        ='SPHERICAL   ' 
        type1(2)        ='ELLIPTICAL  ' 
        type1(3) ='TOROIDAL    ' 
        type1(4) ='PARABOLICAL ' 
        type1(5)        ='PLANE       ' 
        type1(6) ='CODLING SLIT' 
        type1(7)        ='HYPERBOLICAL' 
        type1(8)        ='CONICAL     ' 
        type1(9)        ='POLYNOMIAL  ' 
        type1(10)        ='CONIC EXTNAL' 
        type1(11)        ='            ' 
        type1(12)        ='            ' 

        WRITE(6,*)  &
        '-------------------- M I R I N F O ----------------------'
        WRITE(6,*) ' '
        WRITE(6,*)  &
        'MirInfo: Mirror descriptor file. It must be an end.xx type.'
        WRITE(6,*) 'MirInfo: Please input filename: '
        READ (5,'(A)')   MirFil
        WRITE(6,*) 'MirInfo: Title ?'
        READ (5,'(A)') TITLE
        WRITE(6,*) 'MirInfo: Comment ?'
        READ (5,'(A)') COMMENT
        WRITE(6,*) 'MirInfo: Output file ?'
        READ (5,'(A)')   FILE_OUT
!
! read file
!
        CALL PoolOELoad(p1,mirFil)

        OPEN (20,FILE=FILE_OUT,STATUS='UNKNOWN')
        REWIND(20)
        WRITE (20,*) TOPLIN
        WRITE (20,*) '********************   MIRROR  DESCRIPTION   ********************'
        WRITE (20,*) TOPLIN
        WRITE (20,*) trim(TITLE)
        WRITE (20,*) trim(COMMENT)
        WRITE (20,*) TOPLIN
        WRITE (20,*) 'Input file specified: '//trim(MIRFIL)
        CALL GETCWD(CD)
        WRITE (20,*) 'Full file specification: '//trim(CD)//OS_DS//trim(mirFil)
        call date_and_time(TIME=time,DATE=date,ZONE=zone)
        WRITE (20,*) 'Creation date: '//date(1:4)//' '//date(5:6)//' '//&
            date(7:8)//' '//time(1:2)//'h '//time(3:4)//'min '//time(5:6)//' s'


        WRITE (20,*)   TOPLIN
        WRITE (20,2001) type1(p1%FMIRR)
2001    FORMAT (/,1X,'Surface figure was defined as:',T40,A)

        IF (p1%FCYL.EQ.0) THEN
           WRITE (20,*) 'Cylindrical figure                      NO'
        ELSE
            WRITE (20,*) 'Cylindrical figure                      YES'
            WRITE (20,*) 'Cylinder axis angle from X-axis        ',p1%CIL_ANG*TODEG
        END IF

        IF (p1%F_ROUGHNESS.EQ.1) THEN
        WRITE (20,*) 'Roughness on from '//trim(p1%FILE_ROUGH)
        WRITE (20,*) 'RMS in Y (angstroms)                   ',p1%ROUGH_Y
        WRITE (20,*) 'RMS in X (angstroms)                   ',p1%ROUGH_X
        ENDIF

        IF (p1%F_REFRAC.EQ.0) THEN
            WRITE (20,*) 'Element type                            REFLECTOR'
        ELSE
            WRITE (20,*) 'Element type                            REFRACTOR'
        END IF

        IF (p1%F_GRATING.EQ.0.AND.p1%F_CRYSTAL.EQ.0) THEN
            IF (p1%F_FACET.EQ.1) THEN
                WRITE (20,*) 'Element type                       Faceted Mirror'
                WRITE (20,*) 'Facet size (X)                         ',p1%RFAC_LENX
                WRITE (20,*) 'Facet size (Y)                         ',p1%RFAC_LENY
                WRITE (20,*) 'Facet polynomial from                  ',p1%FILE_FAC
                IF (p1%F_POLSEL.EQ.3) THEN
                    WRITE (20,*) 'Intercept used                         CLOSEST'
                ELSE IF (p1%F_POLSEL.EQ.2) THEN
                    WRITE (20,*) 'Intercept used                         2nd CLOSEST'
                ELSE IF (p1%F_POLSEL.EQ.1) THEN
                    WRITE (20,*) 'Intercept used                         2nd FARTHEST'
                ELSE IF (p1%F_POLSEL.EQ.4) THEN
                    WRITE (20,*) 'Intercept used                         FARTHEST'
                ENDIF
            ELSE IF (p1%F_KOMA.EQ.1) THEN
                WRITE (20,*) 'Element type                Multi-bounce Tube Array'
                IF (p1%F_KOMA_CA.EQ.1) THEN
                    WRITE (20,*) 'Paramters from ',p1%FILE_KOMA_CA
                    WRITE (20,*) 'Tube radii specified as (r(Z))**2'
                ELSE
                    WRITE (20,*) 'Paramters from ',p1%FILE_KOMA
                    WRITE (20,*) 'Tube radii specified as r(Z)'
                ENDIF
            ENDIF
        ELSE IF (p1%F_CRYSTAL.EQ.1) THEN
            WRITE (20,*) 'Element type                            CRYSTAL'
            WRITE (20,*) 'Lattice Spacing                        ',p1%D_SPACING
            WRITE (20,*) 'Bragg Reflection from '//trim(p1%FILE_REFL)
            IF (p1%F_MOSAIC.EQ.1) THEN
                WRITE (20,*) 'MOSAIC Crystal selected                '
                WRITE (20,*) 'Mosaic crystal spread (st. dev)  [DEG] ',p1%SPREAD_MOS*TODEG
                WRITE (20,*) 'Mosaic crystal thickness [cm]          ',p1%THICKNESS
            ELSE IF (p1%F_MOSAIC.NE.1) THEN
                IF (p1%F_BRAGG_A.EQ.1) THEN
                    WRITE (20,*) 'Asymmetric Cut angle                   ',p1%A_BRAGG*TODEG
                END IF
                IF (p1%F_JOHANSSON.EQ.1) THEN
                    WRITE (20,*) 'JOHANSSON Geometry selected            '
                    WRITE (20,*) 'Johansson radius                        ',p1%R_JOHANSSON
                END IF
            END IF
        ELSE IF (p1%F_GRATING.EQ.1) THEN
            IF (p1%FZP.EQ.1) THEN
                WRITE (20,*) 'Element type                 Fresnel Zone Plate'
            END IF
            WRITE (20,*) 'Element type                            GRATING'
            WRITE (20,*) 'Order choosen ( inside are < 0 )       ',p1%ORDER
            IF (p1%F_CENTRAL.EQ.1) THEN
                WRITE (20,*) 'Automatic Tuning                        YES'
                IF (p1%F_MONO.EQ.0.AND.p1%F_CRYSTAL.EQ.0) THEN
                    WRITE (20,*) 'Mount                                   SEYA / TGM'
                ELSE IF (p1%F_MONO.EQ.0.AND.p1%F_CRYSTAL.EQ.1) THEN
                    WRITE (20,*) 'Mount                                   BRAGG'
                ELSE IF (p1%F_MONO.EQ.1) THEN
                WRITE (20,*) 'Mount                                   ERG'
                ELSE IF (p1%F_MONO.EQ.2) THEN
                    WRITE (20,*) 'Mount                                   Const. INCIDENCE'
                ELSE IF (p1%F_MONO.EQ.3) THEN
                    WRITE (20,*) 'Mount                                   Const. DIFFRACTION'
                ELSE IF (p1%F_MONO.EQ.4) THEN
                    WRITE (20,*) 'Mount                                   Const. BLAZE'
                END IF
                IF (p1%F_PHOT_CENT.EQ.0) WRITE (20,*) 'Grating tuned at  [ eV ]              ',p1%PHOT_CENT
                IF (p1%F_PHOT_CENT.EQ.1) WRITE (20,*) &
                    'Grating tuned at  [ Angstroms ]       ',p1%R_LAMBDA
            ELSE
                WRITE (20,*) 'Automatic Tuning                        NO'
            END IF
            IF (p1%F_RULING.EQ.0.AND.p1%F_CRYSTAL.EQ.0) THEN
                WRITE (20,*) 'Constant ruling [ lines/cm ]           ',p1%RULING
            ELSE IF (p1%F_RULING.EQ.1) THEN
                WRITE (20,*) 'Uniform ruling. At pole [ lines/cm ]   ',p1%RULING
            ELSE IF (p1%F_RULING.EQ.2) THEN
                WRITE (20,*) 'Holographic grating. Recording Wavelength: ',p1%HOLO_W
                WRITE (20,2002)
2002            FORMAT (1X,'Input Slit Dist.',T20,'Exit Slit Dist.', &
                        T40,'Input Slit Angle',T60,'Exit Slit Angle')
                WRITE (20,2003)  p1%HOLO_R1,p1%HOLO_R2,p1%HOLO_DEL,p1%HOLO_GAM
2003            FORMAT (1X,G16.9,T20,G16.9,T40,G16.9,T60,G16.9)
                WRITE (20,*) 'Input  Slit rotation angle ',p1%HOLO_RT1*TODEG
                WRITE (20,*) 'Output Slit rotation angle ',p1%HOLO_RT2*TODEG
                IF (p1%F_PW.EQ.0) WRITE (20,*) 'Spherical / Spherical'
                IF (p1%F_PW.EQ.1) WRITE (20,*) 'Plane     / Spherical'
                IF (p1%F_PW.EQ.2) WRITE (20,*) 'Spherical / Plane'
                IF (p1%F_PW.EQ.3) WRITE (20,*) 'Plane     / Plane'
                IF (p1%F_PW_C.EQ.0) WRITE (20,*)  'Spherical   / Spherical'
                IF (p1%F_PW_C.EQ.1) WRITE (20,*)  'Cylindrical / Spherical'
                IF (p1%F_PW_C.EQ.2) WRITE (20,*)  'Spherical   / Cylindrical'
                IF (p1%F_PW_C.EQ.3) WRITE (20,*)  'Cylindrical / Cylindrical'
                IF (p1%F_VIRTUAL.EQ.0) WRITE (20,*) 'Real      / Real'
                IF (p1%F_VIRTUAL.EQ.1) WRITE (20,*) 'Real      / Virtual'
                IF (p1%F_VIRTUAL.EQ.2) WRITE (20,*) 'Virtual   / Real'
                IF (p1%F_VIRTUAL.EQ.3) WRITE (20,*) 'Virtual   / Virtual'
            ELSE IF (p1%F_RULING.EQ.5) THEN
                WRITE (20,*) 'Mechanically ruled grating. Polinomial Coefficients: '
                WRITE (20,*) 'Zero order term Coefficient: ',p1%RULING
                WRITE (20,*) 'First                        ',p1%RUL_A1
                WRITE (20,*) 'Second                       ',p1%RUL_A2
                WRITE (20,*) 'Third                        ',p1%RUL_A3
                WRITE (20,*) 'Fourth                       ',p1%RUL_A4
            ELSE IF (p1%F_RULING.EQ.3) THEN
                WRITE (20,*) '"Oriental fan" type grating.'
                WRITE (20,*) 'Fan pole angle from Y axis         ',p1%AZIM_FAN
                WRITE (20,*) '        distance from grating pole ',p1%DIST_FAN
                WRITE (20,*) 'Coma correction factor             ',p1%COMA_FAC
                WRITE (20,*) 'Line density at grating pole       ',p1%RULING
            END IF
        END IF

        IF (p1%F_REFRAC.EQ.1) THEN
            if (p1%F_R_IND .EQ. 0) then
                write(20,*) "Index of refraction in object space: ",p1%R_IND_OBJ," Attenuation coeff: ",p1%R_ATTENUATION_OBJ
                write(20,*) "Index of refraction in image space: ",p1%R_IND_IMA," Attenuation coeff: ",p1%R_ATTENUATION_IMA
           else if (p1%F_R_IND .EQ. 1) then
                write(20,*) "Index of refraction in object space from file "//trim(p1%FILE_R_IND_OBJ)
                write(20,*) "Index of refraction in image space: ",p1%R_IND_IMA," Attenuation coeff: ",p1%R_ATTENUATION_IMA
           else if (p1%F_R_IND .EQ. 2) then
                write(20,*) "Index of refraction in object space: ",p1%R_IND_OBJ," Attenuation coeff: ",p1%R_ATTENUATION_OBJ
                write(20,*) "Index of refraction in image space from file "//trim(p1%FILE_R_IND_IMA)
           else if (p1%F_R_IND .EQ. 3) then
               write(20,*) "Index of refraction in object space from file "//trim(p1%FILE_R_IND_OBJ)
               write(20,*) "Index of refraction in image space from file "//trim(p1%FILE_R_IND_IMA)
           end if
        END IF

        IF (p1%F_REFLEC.EQ.0) THEN
            WRITE (20,*) 'Reflectivity                            OFF'
        ELSE
            IF (p1%F_REFL.EQ.0) THEN
                WRITE (20,*) 'Reflectivity      ON     coefficients from: ',p1%FILE_REFL
            ELSE IF (p1%F_REFL.EQ.1) THEN
                WRITE (20,*) 'Reflectivity      ON     coefficients from TT:'
            ELSE IF (p1%F_REFL.EQ.2) THEN
                WRITE (20,*) 'Multilayer        ON     coefficients and geometry from : '//trim(p1%FILE_REFL)
            END IF
            IF (p1%F_REFLEC.EQ.1) WRITE (20,*) &
            'Polarization dependence                 YES'
            IF (p1%F_REFLEC.EQ.2) WRITE (20,*) &
            'Polarization dependence                 NO'
        END IF
        IF (p1%FHIT_C.EQ.0) THEN
            WRITE (20,*) 'Mirror dimensions                       UNLIMITED'
        ELSE
            IF (p1%FSHAPE.EQ.1) THEN
                WRITE (20,*) 'Mirror dimensions ( rectangular ):'
                WRITE (20,2005)
2005            FORMAT (1X,T10,'X plus',T30,'X minus',T50,'Y plus',T70,'Y minus')
                WRITE (20,2004) p1%RWIDX1,p1%RWIDX2,p1%RLEN1,p1%RLEN2
2004            FORMAT (1X,T10,G12.5,T30,G12.5,T50,G12.5,T70,G12.5)
            ELSE IF (p1%FSHAPE.EQ.2) THEN
                WRITE (20,*) 'Mirror dimensions ( elliptical ) :'
                WRITE (20,2006)
2006            FORMAT (1X,T10,'Major Axis',T30,'Minor axis')
                WRITE (20,2004) p1%RWIDX2,p1%RLEN2
            ELSE IF (p1%FSHAPE.EQ.3) THEN
                WRITE (20,*) 'Mirror dimensions ( elliptical + hole )'
                WRITE (20,*) 'A. Outside border:'
                WRITE (20,2006)
                WRITE (20,2004) p1%RWIDX2,p1%RLEN2
                WRITE (20,*) 'B. Inner border :'
                WRITE (20,2006)
                WRITE (20,2004) p1%RWIDX1,p1%RLEN1
            END IF
        END IF
        WRITE (20,*) TOPLIN
        WRITE (20,*) 'Central Axis parameters :'
        WRITE (20,*) 'Source Plane Distance                   ',p1%T_SOURCE
        WRITE (20,*) 'Image  Plane                            ',p1%T_IMAGE
        WRITE (20,*) 'Incidence Angle                         ',p1%T_INCIDENCE*TODEG
        WRITE (20,*) 'Reflection/Diffraction Angle            ',p1%T_REFLECTION*TODEG
        WRITE (20,*) ' '
        IF (p1%F_EXT.EQ.1) THEN
            WRITE (20,*) 'Mirror parameters                       EXTERNAL'
        ELSE
            WRITE (20,*) 'Mirror parameters                       COMPUTED'
            IF (p1%F_DEFAULT.EQ.1) THEN
                WRITE (20,*) 'Same configuration as Central Axis      YES'
            ELSE
                WRITE (20,*) 'Same configuration as Central Axis      NO'
            END IF
            WRITE (20,*) 'Objective focus at                      ',p1%SSOUR
            WRITE (20,*) 'Image focus at                          ',p1%SIMAG
            WRITE (20,*) 'Incidence angle                         ',p1%THETA*TODEG
        END IF
        WRITE (20,*) 'Parameters used follow:'

        IF (p1%FMIRR.EQ.1) THEN
            WRITE (20,2010) p1%RMIRR
2010        FORMAT (1X,'Spherical Radius ',T40,G16.9)
        ELSE IF (p1%FMIRR.EQ.2) THEN
            WRITE (20,*)'   Semi-major axis  ', p1%AXMAJ
            WRITE (20,*)'   Semi-minor axis  ', p1%AXMIN
            WRITE (20,*)'   Semi-focal-length', SQRT(p1%AXMAJ**2-p1%AXMIN**2)
            ECCENT = SQRT(p1%AXMAJ**2-p1%AXMIN**2)/p1%AXMAJ
            WRITE (20,*)'   Eccentricity     ', ECCENT
        ELSE IF (p1%FMIRR.EQ.3) THEN
            WRITE (20,*)'   Major Radius (optical)    ', p1%R_MAJ+p1%R_MIN
            WRITE (20,*)'   Minor Radius              ', p1%R_MIN
        ELSE IF (p1%FMIRR.EQ.4) THEN
            WRITE (20,*)'   Parabola Param. ', p1%PARAM
        ELSE IF (p1%FMIRR.EQ.5) THEN
            WRITE (20,*)'   Plane mirror '
        ELSE IF (p1%FMIRR.EQ.6) THEN
            WRITE (20,*)'   Codling Slit'
        ELSE IF (p1%FMIRR.EQ.7) THEN
            WRITE (20,*)'   Semi-major axis  ', p1%AXMAJ
            WRITE (20,*)'   Semi-minor axis  ', p1%AXMIN
            AFOCI = SQRT(p1%AXMIN**2+p1%AXMAJ**2)
            ECCENT = AFOCI/ABS(p1%AXMAJ)
            WRITE (20,*)'   Semi-focal-length',AFOCI
            WRITE (20,*)'   Eccentricity     ', ECCENT
        ELSE IF (p1%FMIRR.EQ.8) THEN
            WRITE (20,*)'   Cone half-angle  ',p1%CONE_A*TODEG
        ELSE IF (p1%FMIRR.EQ.9) THEN
            WRITE (20,*)'   Polynomial Coeff file   '//trim(p1%FILE_MIR)
        END IF

        IF (p1%FSTAT.EQ.0) THEN
            WRITE (20,*) 'Source of this O.E. moved               NO'
        ELSE
            WRITE (20,*) 'Source of this O.E. moved               YES'
            WRITE (20,*) 'In SOURCE reference frame: '
            WRITE (20,*) 'Source Movement X: ',p1%X_SOUR
            WRITE (20,*) '                Y: ',p1%Y_SOUR
            WRITE (20,*) '                Z: ',p1%Z_SOUR
            WRITE (20,*) 'Source rotat  X: ',p1%X_SOUR_ROT*TODEG
            WRITE (20,*) '              Y: ',p1%Y_SOUR_ROT*TODEG
            WRITE (20,*) '              Z: ',p1%Z_SOUR_ROT*TODEG
            WRITE (20,*) 'In MIRROR reference frame: '
            WRITE (20,*) 'Source distance    ',p1%RDSOUR
            WRITE (20,*) '       rotation    ',p1%ALPHA_S*TODEG
            WRITE (20,*) 'Incidence angle    ',p1%RTHETA*TODEG
            WRITE (20,*) 'Source offset X: ',p1%OFF_SOUX
            WRITE (20,*) '              Y: ',p1%OFF_SOUY
            WRITE (20,*) '              Z: ',p1%OFF_SOUZ
        END IF
        IF (p1%F_MOVE.EQ.0) THEN
            WRITE (20,*) 'Mirror at pole position ( no mov. )     YES'
        ELSE
            WRITE (20,*) 'Mirror moved from pole. Parameters :'
            WRITE (20,*) 'Displacement along X:   ',p1%OFFX
            WRITE (20,*) '                   Y:   ',p1%OFFY
            WRITE (20,*) '                   Z:   ',p1%OFFZ
            WRITE (20,*) 'Rotation around X:   ',p1%X_ROT*TODEG
            WRITE (20,*) '                Y:   ',p1%Y_ROT*TODEG
            WRITE (20,*) '                Z:   ',p1%Z_ROT*TODEG
        END IF

!
! write conic coefficients
!
       IF ((p1%FMIRR.EQ.1).OR.(p1%FMIRR.EQ.2).OR.(p1%FMIRR.EQ.4).OR. &
           (p1%FMIRR.EQ.5).OR.(p1%FMIRR.EQ.7).OR.(p1%FMIRR.EQ.8).OR. &
           (p1%FMIRR.EQ.9).OR.(p1%FMIRR.EQ.10)) THEN
        
        WRITE (20,*) ' '
        WRITE (20,*) TOPLIN
        WRITE (20,*) 'OE surface in form of conic equation: '
        WRITE (20,*) '    c[1]*X^2 + c[2]*Y^2 + c[3]*Z^2 + '
        WRITE (20,*) '    c[4]*X*Y + c[5]*Y*Z + c[6]*X*Z  + '
        WRITE (20,*) '    c[7]*X + c[8]*Y + c[9]*Z + c[10] = 0  '
        WRITE (20,*) ' with '
        DO j=1,10  
           Write( stmp, '(i2)' ) j
           stmp = adjustl(stmp)
        WRITE (20,*) ' c['//trim(stmp)//'] =',p1%ccc(j)
        END DO
       END IF

       WRITE (20,*) TOPLIN
       WRITE (20,*) '***************                 E N D                  ***************'
       WRITE (20,*) TOPLIN
       CLOSE (20)
       RETURN
END SUBROUTINE MirInfo

!
!
!

!C+++
!C	PROGRAM		SYSINFO
!C
!C	PURPOSE		Returns a readable file of a mirror definition
!C			in shadow.
!C
!C---
SUBROUTINE SysInfo

	implicit none ! beautiful!

	type (poolOE)      ::  p2
	character(len=sklen) :: mirFil,file_Out,cd,optFile
	character(len=80) :: comment,title
	character(len=79),parameter :: topLin= &
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
	! for date_and_time
	character(len=8)  :: date
	character(len=10) :: time
	character(len=5)  :: zone
	integer,dimension(8) :: values
	integer           :: iDef,i,j,jj,ij,kk,nf,ind1,ind2,iElem,iErr
	real(kind=skr)    :: x,y,z

! todo: check that this is also defined in shadow_kernel...
     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 


	character(len=20),dimension(12) :: type1
	character(len=20),dimension(2)  :: tScr
	character(len=20),dimension(5)  :: tSlit
	character(len=20)               :: break
	character(len=80)               :: text
	character(len=sklen),dimension(20)  :: file_In

TSLIT(1)='SLIT       ' 
TSLIT(2)='STOP       ' 
TSLIT(3)='RECTANGULAR' 
TSLIT(4)='ELLIPTICAL ' 
TSLIT(5)='EXTERNAL   ' 

TSCR(1)	='AFTER Mirror' 
TSCR(2) ='BEFORE  Mirror' 

TYPE1(1)='SPHERICAL   ' 
TYPE1(2)='ELLIPTICAL  ' 
TYPE1(3)='TOROIDAL    ' 
TYPE1(4)='PARABOLICAL ' 
TYPE1(5)='PLANE       ' 
TYPE1(6)='CODLING SLIT' 
TYPE1(7)='HYPERBOLICAL' 
TYPE1(8)='CONICAL     ' 
TYPE1(9)='            ' 
TYPE1(10)='            ' 
TYPE1(11)='            ' 
TYPE1(12)='            ' 
BREAK	='    ----------------' 

     	WRITE(6,*)  &
        '-------------------- S Y S I N F O ----------------------'
     	WRITE(6,*) ' '
      	IDEF = IYES ('SysInfo: Default filenames [ Y/N ] ?')
     	IF (IDEF.EQ.1) THEN
     	  NF	=   IRINT ('SysInfo: How many OE''s ? ')
     	  IF (NF.EQ.0) NF = 1
     	  DO J=1,NF
     	   WRITE (FILE_IN(J),999) 'end.',J
	  END DO
     	  WRITE (OPTFILE,998)	'optax.',NF
998	  FORMAT (A6,I2.2)
999	  FORMAT (A4,I2.2)
     	ELSE
     	  WRITE(6,*) 'SysInfo: List of files describing the system. Use TT: ',&
      'for keyboard input.'
	  MIRFIL = RSTRING ('SysInfo: File containing ERFs ? ')
     	  OPEN (20, FILE=MIRFIL, STATUS='OLD', IOSTAT=iERR)
	  IF (iErr /= 0) THEN
            print *,'Error: SYSINFO: Error reading file: '//trim(mirFil)
            return
            ! STOP  'Aborted'
	  END IF
	  I=1
30     	  CONTINUE
     	  READ (20,'(A)',ERR=40,END=40)  FILE_IN(I)
          WRITE(6,*) 'File # ',I,' = '//trim(file_in(i))
     	  I=I+1
     	  GO TO 30
40	  CLOSE (20)
     	
     	  WRITE(6,*) 'SysInfo> Will use ',I-1,' ERF files.'
     	  NF	= I - 1
     	  optFile=RString('SysInfo: Optaxis file-name : ')
     	END IF
	title = RString('SysInfo: Title ?  ')
	comment = RString('SysInfo: Comment ? ')
	file_Out = RString('SysInfo: Output file : ')
     	OPEN (30,FILE=FILE_OUT,STATUS='UNKNOWN')
	REWIND (30)
     	WRITE (30,*) ''
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) '**************  S Y S T E M      ',&
      'D E S C R I P T I O N  **************'
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) trim(TITLE)
     	WRITE (30,*) trim(COMMENT)
     	WRITE (30,*) TOPLIN
	IF (iDef /= 1) WRITE (30,*) 'Input file with end.xx files:'//trim(MIRFIL)

          CALL GETCWD(CD)

     	WRITE (30,*)	TOPLIN
     	WRITE (30,*) ' #    Optical Element: '

     	DO J=1,NF
	  WRITE (30,*)	J,trim(cd)//OS_DS//trim(FILE_IN(J))
	END DO
     	WRITE (30,*)	TOPLIN
     	
     	DO 13 J=1,NF
        CALL PoolOELoad(p2,file_in(j) )
     	WRITE (30,*) ' '
     	WRITE (30,*)	'Optical Element # ',J,'      System Number: '
     	DO 14 I=1,80
     	TEXT(I:I)=' '
14      CONTINUE
     	IF (p2%F_CRYSTAL.EQ.1) THEN
     		TEXT(1:5) = 'BRAGG'
     	ELSE IF (p2%F_GRATING.EQ.1) THEN
     		TEXT(1:7) = 'GRATING'
     	ELSE
     		TEXT(1:6) = 'MIRROR'
     	END IF
     	DO 16 JJ=1,14
     	TEXT (9+JJ:9+JJ) = TYPE1(p2%FMIRR)(JJ:JJ)
16      CONTINUE
     	IF (p2%FHIT_C.EQ.1) THEN
     		TEXT(30:39)  =  'DIM CHECK'
     	ELSE
     		TEXT(30:39)  =  'UNLIMITED'
     	END IF
     	IF (p2%F_EXT.EQ.1) THEN
     		TEXT(45:53)  =  'EXTERNAL'
     	ELSE
     		TEXT(45:53)  =  'COMPUTED'
     	END IF
     	IF (p2%F_REFLEC.EQ.0) THEN
     		TEXT(60:71)	=  'REFLEC. OFF'
     	ELSE
     		TEXT(60:71)	=  'REFLEC. ON '
     	END IF
     	WRITE (30,*) ' '
     	WRITE (30,*) TEXT(1:71)
     	WRITE (30,*) ' '
     	WRITE (30,*) '  Orientation        ',p2%ALPHA*TODEG,' deg.'
     	WRITE (30,*) '  Source Plane       ',p2%T_SOURCE
     	WRITE (30,*) '  Incidence Ang.     ',p2%T_INCIDENCE*TODEG,' deg.'
     	WRITE (30,*) '  Reflection Ang.    ',p2%T_REFLECTION*TODEG,' deg.'
     	WRITE (30,*) '  Image Plane        ',p2%T_IMAGE
     	WRITE (30,*)	BREAK
     	IF (p2%F_SCREEN.EQ.1) THEN
	WRITE(6,*) p2%N_SCREEN,p2%I_SCREEN,p2%SL_DIS
     	  WRITE (30,*)	'  SCREENS: ',p2%N_SCREEN,' defined.'
     	 DO 17 I=1,p2%N_SCREEN
     	   IJ = p2%I_SCREEN(I) + 1
     	   WRITE (30,*)	TSCR(IJ),' at ',p2%SL_DIS(I)
     	  IF (p2%I_SLIT(I).NE.0) THEN
     	    IND1 =  p2%I_STOP(I) + 1
     	    IND2  =  p2%K_SLIT(I) + 3
     	    WRITE (30,*)	'  Type :',TSLIT(IND1),'  ',TSLIT(IND2)
     	  END IF
17      CONTINUE
     	  WRITE (30,*)	BREAK
     	END IF
13     	CONTINUE
     	OPEN (20, FILE=OPTFILE, STATUS='OLD')
     	WRITE (30,3005)
3005	FORMAT (1X,/,1X,T26,'OPTICAL SYSTEM CONFIGURATION')
     	WRITE (30,3000)
3000	FORMAT (1x,T27,'Laboratory Reference Frame.',/)
     	WRITE (30,3010)
3010	FORMAT (1X,'OPT. Elem # ',T20,'X = ',T40,'Y =',T60,'Z =',/)
     	KK = 0
110	READ (20,*,ERR=100,END=100) IELEM
     	READ (20,*)	X,Y,Z				!Source
     	IF (KK.EQ.0)	WRITE (30,3020) 0,X,Y,Z
     	READ (20,*)	X,Y,Z				!Mirror
     	WRITE (30,3020) IELEM,X,Y,Z
     	READ (20,*)	X,Y,Z				!Image
     	WRITE (30,3030) IELEM,X,Y,Z
     	WRITE (30,*)	' '
     	READ (20,*)
     	READ (20,*)
     	READ (20,*)
     	READ (20,*)
     	READ (20,*)
     	KK = KK + 1
     	GO TO 110
3020	FORMAT (1X,T5,I4,T18,G18.11,T38,G18.11,T58,G18.11)
3030	FORMAT (1X,T8,I4,'''',T21,G18.11,T41,G18.11,T61,G18.11)
100	CONTINUE
     	WRITE (30,*)	TOPLIN
     	WRITE (30,*) '***************                 E N',&
      ' D                  ***************'
     	WRITE (30,*)	TOPLIN
     	CLOSE (30)
	RETURN
END SUBROUTINE SysInfo
!
!  graphics!!
!

! C+++
! C	PROGRAM		TRANSLATE
! C
! C	PURPOSE		To translate a binary ray file to a readable ASCII format.
! C---
SUBROUTINE Translate
    
	implicit none 

        CHARACTER(len=sklen)      :: FILEIN,FILEOUT
        real(kind=skr), dimension(:,:), allocatable  :: Ray
    	integer(kind=ski) :: IUNIT,OUTFLAG
    	integer(kind=ski) :: ICSV,nCol1,nPoint1,iFlag,iErr,nRay,i,j
    
    	FILEIN = RSTRING('File for input ? ')
    
        !
        ! allocate RAY and read binary file
        ! 

        ! before calling beamLoad, we must allocate arrays
        ! get information on number of columns and rays from the header 
        CALL    beamGetDim (FILEIN,nCol1,NPOINT1,IFLAG,IERR)
        IF ((IFLAG.ne.0).OR.(IERR.ne.0)) THEN
           print *,'Error: TRANSLATE: Error accessing file: '//trim(FILEIN)
           return
           ! STOP 'Aborted'
        ELSE
            !
            ! allocate arrays
            !
            if (ALLOCATED(RAY)) DEALLOCATE(RAY)
            if (.not. ALLOCATED(RAY)) then
               ALLOCATE(RAY(18,NPOINT1),stat=ierr)
               if (ierr /= 0) then
                  call leave ('TRANSLATE','Error allocating array',IERR)
               end if
            end if
            ! beamLoad must be calles once RAY is allocated
            CALL beamLoad(ray,ierr,nCol1,npoint1,fileIn)
         END IF


        ! CALL	beamLoad	(FILEIN,RAY,nCol1,NPOINT1,IFLAG,IERR)
    
        WRITE(6,*)'Translate: Read         ',NPOINT1,' rays.'
    	WRITE(6,*)'Translate: Each ray has ',nCol1,' entries.'

    	WRITE(6,*) 'Translate: Enter filename or - for standard output.'
    	FILEOUT = RSTRING('Output filename or device ? ')

    	NRAY = IRINT('Translate: How many rays to translate ? ')
    	ICSV = IYES('Translate: Create comma separated values (CSV) output [y/N] ? ')
    	IF (FILEOUT(1:1).EQ.'-') THEN
    	   IUNIT = 6
    	   OUTFLAG = 1
    	ELSE
    	   IUNIT = 22
         	   OPEN (IUNIT,FILE=FILEOUT,STATUS='UNKNOWN')
    	   REWIND (IUNIT)
    	   OUTFLAG = 0
    	ENDIF
    ! C
    ! C If ICSV is 0, then add newlines after every 3 numbers to nicely
    ! C fit on a line. If 1, all numbers for the ray goes on a single line,
    ! C separated by commas.  Harder to look at, but easy to load using "foreign" 
    ! C programs.
    ! C
    	DO  111 I=1,NRAY
    	  IF (ICSV.EQ.0) THEN
    
    	    WRITE (IUNIT,98)		(RAY(J,I),J=1,3)
    	    WRITE (IUNIT,98)		(RAY(J,I),J=4,6)
    	    WRITE (IUNIT,98)		(RAY(J,I),J=7,9)
    	    WRITE (IUNIT,98)		(RAY(J,I),J=10,12)
    	    IF (nCol1.EQ.13) THEN
    	      WRITE	(IUNIT,98)		RAY(13,I)
    	    ELSE IF (nCol1.EQ.18) THEN
    	      WRITE (IUNIT,98)		(RAY(J,I),J=13,15)
    	      WRITE (IUNIT,98)		(RAY(J,I),J=16,18)
    	    END IF
    	    WRITE (IUNIT,*)
    	  ELSE
    	    DO 112 J=1,nCol1
    	      WRITE (IUNIT, 99)		RAY(J,I)
    	      IF (J.LT.nCol1) THEN
    		WRITE (IUNIT, '(a1,$)')		','
    	      ENDIF
    112	    CONTINUE
    	    WRITE (IUNIT,*)
    	  ENDIF
    111	CONTINUE
    
    	IF (OUTFLAG.EQ.0) THEN
    	   CLOSE (IUNIT)
         	   WRITE(6,*)'Translate: File written to disk: '//trim(fileOut)
    	ENDIF
     if (ALLOCATED(RAY)) DEALLOCATE(RAY)
 
     RETURN
    
     98	FORMAT(G21.14)
     99	FORMAT(G21.14,$)
END SUBROUTINE TRANSLATE
!
!
!

!C+++
!C
!C	subroutine	histo1
!C
!C	purpose		this program will generate an histogram of the
!C			distribution of the rays in a given column.
!C
!C	input (prompt)	a RAY file from SHADOW
!C
!C	output		a plottable file with gnuplot
!C
!C---

SUBROUTINE Histo1

	implicit none 

	character(len=sklen)  :: fileIn,fileIn2
	integer(kind=ski)  :: n_Col,iEner,iFlag,iErr,i,iAnsw
	integer(kind=ski)  :: nCol1,nPoint1,nCol2,nPoint2,iNorm
	
	integer(kind=ski)  :: iLost,iRefl,iFile,jBin,nBin
	real(kind=skr)     :: center,width,zero=0.0
	real(kind=skr)     :: xmin,xmax,xArg,xLow,sigma

        real(kind=skr),dimension(:,:),allocatable :: ray,ray2
        real(kind=skr),dimension(:),allocatable   :: xArray,yArray,y2Array

     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     

	WRITE(6,*)'File for analysis ?'
	READ(5,'(A)') FILEIN

     	N_COL = IRINT ('Column to analyze ? ')
	IF (N_COL.EQ.11) THEN
	  WRITE(6,*)'Options : [0] Angstroms'
	  WRITE(6,*)'        : [1] eV'
	  WRITE(6,*)'        : [2] cm-1'
	  IENER	= IRINT ('<?> ')
	END IF
!
!
! reads input binary file
!
	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    beamGetDim (fileIn,ncol1,npoint1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Error: HISTO1: beamGetDim: Error in file: '//trim(fileIn)
            return
            !stop
        END IF

        IF (allocated(ray)) deallocate(ray)
        IF (allocated(xArray)) deallocate(xArray)
        IF (allocated(yArray)) deallocate(yArray)
        IF (allocated(y2Array)) deallocate(y2Array)
  	ALLOCATE( RAY(18,NPOINT1) )
  	ray=0.0d0

	CALL beamLoad(ray,ierr,ncol1,npoint1,fileIn)

!C
!C Find the min and max (for the user)
!C
 	XMIN	=  1.0E+20
	XMAX	= -1.0E+20
	DO I = 1, NPOINT1
	  XARG	= RAY(N_COL,I)
	  XMIN	= MIN (XMIN,XARG)
	  XMAX	= MAX (XMAX,XARG)
        END DO
	IF (N_COL.EQ.11) THEN
	   IF (IENER.EQ.0) THEN
	     XMIN	= TWOPI/XMIN*1.0E+8
	     XMAX	= TWOPI/XMAX*1.0E+8
	   ELSE IF (IENER.EQ.1) THEN
	     XMIN	= XMIN/TWOPI*TOCM
	     XMAX	= XMAX/TWOPI*TOCM
	   END IF
	END IF
	WRITE(6,*)'Read    : ',NPOINT1,' rays'
	WRITE(6,*)'Maximum : ',XMAX
	WRITE(6,*)'Minimum : ',XMIN
     	CENTER= RNUMBER ('Distribution center (set zero for default) ? ')
     	WIDTH = RNUMBER ('             width  (set zero for default) ? ')


     	NBIN = IRINT ('Number of bins (odd, please. Default=31) ? ')
        IF (NBIN.EQ.0) NBIN=31

        ! allocate results array
  	ALLOCATE( xArray(NBIN) )
  	ALLOCATE( yArray(NBIN) )
  	ALLOCATE( y2Array(NBIN) )
        xArray=0.0d0
        yArray=0.0d0
        y2Array=0.0d0


     	WRITE(6,*)'Flag checks. Enter :'
     	WRITE(6,*)'0	to exclude lost rays'
     	WRITE(6,*)'1	to include lost rays too'
     	WRITE(6,*)'2	to use only lost rays'
     	ILOST = IRINT ('<?> ')
     	WRITE(6,*)'Normalization kind. Enter :'
     	WRITE(6,*)'0	for no normalization'
     	WRITE(6,*)'1	to normalize to 1'
     	WRITE(6,*)'2	area normalized to 1'
     	INORM = IRINT ('<?> ')
	IREFL = IRINT ('Include reflectivity (|A|**2 as weighing factor)? ')

!C
!C Fill in the bins
!C
	call histo1_calc(ray,NPOINT1,NCOL1, &
                         XARRAY,YARRAY,Y2ARRAY,NBIN,&
                         N_COL,IENER,CENTER,WIDTH,ILOST,INORM,IREFL)

!C
!C Ready for output.
!C
     	  OPEN (20,FILE='histo1.dat',STATUS='UNKNOWN') !,INITIALSIZE=5)
 
            ! write a first empty bin, for helping gnuplot to make the plot
     	    WRITE (20,*)	XARRAY(1)-(XARRAY(2)-XARRAY(1)),0.0D0,0.0D0
     	    DO I=1,NBIN
!	      SIGMA = sqrt(max(Y2ARRAY(I)-(YARRAY(I)**2)/NPOINT1,0))
	      SIGMA = sqrt(max(Y2ARRAY(I)-(YARRAY(I)**2)/NPOINT1,zero))
     	      WRITE (20,*)	XARRAY(I),YARRAY(I),SIGMA
            END DO
            ! write a last empty bin, for helping gnuplot to make the plot
     	    WRITE (20,*)	XARRAY(NBIN)+(XARRAY(2)-XARRAY(1)),0.0D0,0.0D0
     	  CLOSE (20)
	  print *,'File (data) written to disk: histo1.dat'
     	  OPEN (21,FILE='histo1.gpl',STATUS='UNKNOWN') !,INITIALSIZE=5)
            WRITE (21,*)  "#"
            WRITE (21,*)  "# Gnuplot script for shadow3"
            WRITE (21,*)  "# Created by histo1"
            WRITE (21,*)  "#"
            IF (OS==1) THEN
                WRITE(21,'(A)')  'set terminal win '
            ELSE
                WRITE(21,'(A)')  'set terminal x11 '
            END IF
            WRITE (21,*)  "#Note that the abscissas in histo1.dat correspond to the CENTER of the bin"
            WRITE (21,*)  " "
            WRITE (21,*)  "# by default, do not display error bars"
            WRITE (21,*)  "plot 'histo1.dat' using 1:2 with boxes linetype -1"
            WRITE (21,*)  " "
            WRITE (21,*)  "# for a plot with error bars comment the previous line"
            WRITE (21,*)  "#     and uncomment the next two lines"
            WRITE (21,*)  "# plot 'histo1.dat' using 1:2:3 with yerrorbars linestyle 3, \"
            WRITE (21,*)  "# 'histo1.dat' using 1:2 with boxes linetype -1" 
            WRITE (21,*)  " "
            WRITE (21,*)  "# for a plot with a Gaussian fit, uncomment the following lines"
            WRITE (21,*)  "# a=0.0 "
            WRITE (21,*)  "# b=0.1 "
            WRITE (21,*)  "# c=0.9 "
            WRITE (21,*)  "# f(x) = a*exp(-(x-b)*(x-b)/2/c/c) "
            WRITE (21,*)  "# fit f(x) 'histo1.dat' via a, b, c "
            WRITE (21,*)  "# plot f(x),'histo1.dat' using 1:2 with boxes linetype -1 "
            WRITE (21,*)  "# print 'Fit FWHM=',c*2.35 "
            WRITE (21,*)  " "
            WRITE (21,*)  "pause -1 'Press <Enter> to end graphic '"
     	  CLOSE (21)
	  print *,'File (gnuplot script) written to disk: histo1.gpl'
	  
        IF (allocated(ray)) deallocate(ray)
        IF (allocated(ray2)) deallocate(ray2)
        IF (allocated(xArray)) deallocate(xArray)
        IF (allocated(yArray)) deallocate(yArray)
        IF (allocated(y2Array)) deallocate(y2Array)

	RETURN
END SUBROUTINE Histo1


!C+++
!C
!C	subroutine	histo1_calc
!C
!C	purpose		to compute the histogram needed by histo1
!C
!C	input 		a RAY array
!C
!C	output		three arrays: xArray and yArray with histogram
!C                      and y2Array with square of counts for error calculations
!C
!C      warning         Arrays must be allocated at caller lever. 
!C                      Counts are added to yArray and y2Array (for loops). 
!C
!C---
SUBROUTINE histo1_calc(ray,     & ! in: ray 
                       NPOINT1, & ! in: number of rays
                       NCOL1,   & ! in: number of cols used
                       XARRAY,  & ! inout: x array of the histogram
                       YARRAY,  & ! inout: y array of the histigram
                       Y2ARRAY, & ! inout: array with the square of counts (for errors)
                       NBIN,    & ! in: number of bins (odd)
                       N_COL,   & ! in: col to analyze
                       IENER,   & ! in: if col=11, units: 0=A, 1=eV, 2=cm-1
                       CENTER,  & ! inout: the distribution center 
                       WIDTH,   & ! inout: the distribution width 
                                  !        (zero=recalculate width and center)
                       ILOST,   & ! in: lost ray flag 0=Good, 1=All, 2=LostOnly
                       INORM,   & ! in: normalize to: 0=None, 1=MaxHisto, 2=Area
                       IREFL    & ! in: if 1, weith rays with reflectivity A**2
                      )

	implicit none 
	integer(kind=ski), intent(in)  :: npoint1,ncol1
        real(kind=skr), dimension(18,npoint1), intent(in)   :: ray
	integer(kind=ski), intent(in)  :: n_col,iener,nbin
	integer(kind=ski), intent(in)  :: ilost,inorm,irefl
	real(kind=skr), intent(inout)     :: center,width

	real(kind=skr), dimension(nbin), intent(inout)     :: xarray,yarray,y2array

     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     

	integer(kind=ski)  ::  i,jbin
	real(kind=skr)     ::  xarg,xmin,xmax,xlow,step,xstart
	real(kind=skr)     ::  arg, val, counts, ymax, rnorm, y1upp


         if (width.eq.0) then   ! default values
            XMIN	=  1.0E+20
            XMAX	= -1.0E+20
            DO I = 1, NPOINT1
              XARG	= RAY(N_COL,I)
              XMIN	= MIN (XMIN,XARG)
              XMAX	= MAX (XMAX,XARG)
            END DO
            IF (N_COL.EQ.11) THEN
              IF (IENER.EQ.0) THEN
                XMIN	= TWOPI/XMIN*1.0E+8
                XMAX	= TWOPI/XMAX*1.0E+8
              ELSE IF (IENER.EQ.1) THEN
                XMIN	= XMIN/TWOPI*TOCM
                XMAX	= XMAX/TWOPI*TOCM
              END IF
            END IF
            CENTER = 0.5*(XMAX+XMIN)
            WIDTH = 1.001*(XMAX-XMIN)
         end if

     	  XLOW	=   CENTER - WIDTH/2
     	  !srio@esrf.eu 20110415 changed. Why 1 bin should not work? 
     	  !STEP	=   WIDTH/(NBIN - 1)
     	  STEP	=   WIDTH/NBIN
     	  !XSTART	=   XLOW - STEP/2
     	  XSTART	=   XLOW 

     	DO I=1,NPOINT1
	 XARG	= RAY(N_COL,I)
!C
!C Use the appropriate units for column 11, if it is selected.
!C
	 IF (N_COL.EQ.11) THEN
	   IF (XARG.EQ.0.0D0) CYCLE
	   IF (IENER.EQ.0) THEN
	     XARG	= TWOPI/XARG*1.0E+8
	   ELSE IF (IENER.EQ.1) THEN
	     XARG	= XARG/TWOPI*TOCM
	   END IF
	 END IF
     	 IF (ILOST.EQ.0) THEN
     	  IF (RAY(10,I).LT.0.0D0) CYCLE
     	 ELSE	IF (ILOST.EQ.2) THEN
     	  IF (RAY(10,I).GT.0.0D0) CYCLE
      	 END IF

      	 ARG	=   (XARG - XSTART)/STEP
     	 JBIN	=   INT (ARG) + 1
      	 IF (JBIN.LT.1.OR.JBIN.GT.NBIN) CYCLE
	   IF (IREFL.EQ.1) THEN
	     VAL  =   RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2
	     IF (ncol1.EQ.18)  VAL = VAL + RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2
             !VAL =   SQRT(VAL)
	   ELSE
             VAL = 1.0D0
	   END IF
    	YARRAY(JBIN) = YARRAY(JBIN) + VAL
    	Y2ARRAY(JBIN) = Y2ARRAY(JBIN) + VAL**2
        END DO
!C
!C Prepare the arrays for writing
!C
     	DO I=1,NBIN
     	  !XARRAY(I) =   (I-1)*STEP + XLOW
          ! put abscissa value at the center of the bin
     	  XARRAY(I) =   (I-0.5)*STEP + XLOW
        END DO

!C
!C Normalize the arrays
!C
     	COUNTS	=   0.0
     	YMAX		= - 1.0
     	DO I=1,NBIN
     	  YMAX	=   MAX(YMAX,YARRAY(I))
     	  COUNTS=   COUNTS + YARRAY(I)
        END DO
!C
     	IF (INORM.NE.0) THEN
     	  IF (INORM.EQ.1) THEN
	    RNORM  =   YMAX
	    Y1UPP  = 1.2
	  END IF
     	  IF (INORM.EQ.2) THEN
	    RNORM  =   COUNTS
	    Y1UPP  = 1.2*YMAX/COUNTS
	  END IF
     	  DO I=1,NBIN
     	     YARRAY(I) =   YARRAY(I)/RNORM
          END DO
	ELSE IF (INORM.EQ.0) THEN
	  Y1UPP = 1.2*YMAX
     	END IF

END SUBROUTINE histo1_calc



!C+++
!C
!C	subroutine	histo1_calc_easy
!C
!C	purpose		to compute the histogram needed by histo1
!C                      It uses keywords for optional arguments to 
!C                      make life easier for the programmer user.
!C
!C	input 		a RAY array
!C
!C	output		two arrays xArray and yArray with histogram
!C
!C      warning         Arrays must be allocated at caller lever. 
!C                      Counts are added to yArray (for loops). 
!C
!C---
SUBROUTINE histo1_calc_easy (ray,     & ! in: ray 
                       N_COL,   & ! in: col to analyze
                       NBIN,    & ! in: number of bins (odd)
                       XARRAY,  & ! inout: x array of the histogram
                       YARRAY,  & ! inout: y array of the histigram
                       Y2ARRAY, & ! inout: array with the square of counts (for errors)

                       CENTER,  & ! opt inout: the distribution center [0]
                       WIDTH,   & ! opt inout: the distribution width [0]
                                  !   (set to zero=recalculate width and center)

                       NPOINT1, & ! opt in: number of rays [size(ray,1)]
                       NCOL1,   & ! opt in: number of cols used [size(ray,2)]
                       IENER,   & ! opt in: if col=11, units:[0]=A, 1=eV, 2=cm-1
                       ILOST,   & ! opt in: lost ray flag [0]=Good, 1=All, 2=LostOnly
                       INORM,   & ! opt in: normalize to: [0]=None, 1=MaxHisto, 2=Area
                       IREFL    & ! opt in: [0]=NoWeight, 1=weigth rays with reflectivity A**2
                      )

	implicit none 
	integer(kind=ski), intent(in)  :: n_col,nBin
        real(kind=skr), dimension(:,:), intent(in)     :: ray
	real(kind=skr), dimension(nbin), intent(inout) :: xarray,yarray,y2array

	real(kind=skr),    optional, intent(inout)     :: center,width
	integer(kind=ski), optional, intent(inout)  :: npoint1,ncol1
	integer(kind=ski), optional, intent(inout)  :: iener,ilost,inorm,irefl

! auxiliar variables
	integer(kind=ski) :: npoint2, ncol2
	real(kind=skr)    :: center2=0.0,width2=0.0
	integer(kind=ski) :: iener2=0,ilost2=0,inorm2=0,irefl2=0

        !compute histogram
        !
        ! note that center and width are set to zero for i=1, so they 
        ! are computed and stored for i>1
        ! Also, yarray is zero for i=1, but for i>1 it accumulates the counts. 
        ncol2=size(ray,1)
        npoint2=size(ray,2)

IF (present(center)) center2 = center
IF (present(width))  width2 = width
IF (present(iener))  iener2 = iener
IF (present(ilost))  ilost2 = ilost
IF (present(inorm))  inorm2 = inorm
IF (present(irefl))  irefl2 = irefl
IF (present(ncol1))  ncol2 = ncol1
IF (present(npoint1)) npoint2 = npoint1

call histo1_calc(ray,npoint2,ncol2, &
                   xarray,yarray,y2array,nBin,&
                   n_col,iener2,center2,width2,&
                   ilost2,inorm2,irefl2)

IF (present(center)) center=center2
IF (present(width))  width = width2

END SUBROUTINE histo1_calc_easy

! 
!
! now plotxy...
!

!C+++
!C	SUBROUTINE	ROUNDOUT
!C
!C	PURPOSE		To find 'nice' scale for plots
!C
!C	ALGORITHM	Direct calculation
!C
!C---
SUBROUTINE RoundOut (X1,X2,X3,X4)
	implicit none
	!IMPLICIT        REAL*8          (A-E,G-H,O-Z)
	real(kind=skr), intent(inout) :: X1,X2,X3,X4
     	CALL	ROUND	(X1,X2)
     	CALL	ROUND	(X3,X4)
     	RETURN
END SUBROUTINE RoundOut

!C+++
!C	SUBROUTINE	ROUND
!C
!C---
     	SUBROUTINE	ROUND	(XMIN,XMAX)
	!IMPLICIT        REAL*8          (A-E,G-H,O-Z)
        implicit real(kind=skr)          (a-e,g-h,o-z)
        implicit integer(kind=ski)       (f,i-n)

	XDEL	= ABS(XMAX-XMIN)
     	IF (XDEL.LT.1.0D-15)	RETURN
!C
!C Computes the exponent of the difference
!C
     	I0	=   LOG10 (XDEL/2.0D0)
     	IF (I0.LT.1.0) I0 = I0 - 1
	XRANGE	= 10.0D0**(I0)
	IMAX	= XMAX/XRANGE + 1
	IMIN	= XMIN/XRANGE - 1
	IF (IMAX.LT.0.0) THEN
	  IMAX	= IMAX/5.0D0
	ELSE
	  IMAX	= IMAX/5.0D0 + 1
	END IF
	IF (IMIN.LE.0.0) THEN
	  IMIN	= IMIN/5.0D0 - 1
	ELSE
	  IMIN 	= IMIN/5.0D0
	END IF
	IMAX	= IMAX*5.0D0
	IMIN	= IMIN*5.0D0
	XMAX	= DBLE(IMAX)*XRANGE
	XMIN	= DBLE(IMIN)*XRANGE
     	RETURN
END SUBROUTINE Round



!C++++
!C
!C	PROGRAM		PLOT_XY
!C
!C	PURPOSE		To generate an x-y plot of any two columns of
!C			SHADOW output.
!C
!C	INPUT		A "ray" file.
!C
!C	OUTPUT		A file suitable for TOPDRAWER
!C			A file suitable for Primvs
!C
!C	Link using GRA:GRALIB.LNK or GRA:TDSHARE.LNK
!C----


SUBROUTINE PlotXY
        implicit real(kind=skr) (a-e,g-h,o-z)
        implicit integer(kind=ski)        (f,i-n)

	real(kind=skr)   :: PLOT
	integer,parameter  :: MAXGS=101
        real(kind=skr),dimension(:,:),allocatable :: ray,ray2
        real(kind=skr),dimension(:),allocatable   :: A_SQUARE,A2_SQUARE
        real(kind=skr),dimension(:),allocatable   :: XPLOT,YPLOT,XLOSS,YLOSS,weightPlot
        logical :: lExists

     	!mv to global_definitions real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
	!mv to global_definitions real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	!mv to global_definitions real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
     	!mv to global_definitions real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
     	character(len=sklen) :: FILE_IN, FILE_I0, dataDir
     	character(len=80) :: TEXT,TEXT1,TEXT2,TEXT3,TEXT4,TEXT5
     	character(len=80) :: TEXT6,TEXT7,TEXT8
     	character(len=20) :: TODAY
     	character(len=80) :: COMMENT
        integer(kind=ski),parameter :: i_td=0
	real(kind=skr),dimension(30) :: TEST,R_LOW,R_UPP
	real(kind=skr),dimension(100) :: X_ARRAY,Y_ARRAY
	real(kind=skr),dimension(4)   :: XYLIM
	real(kind=skr),dimension(30)  :: XMEAN,STDEV,VAR
	real(kind=skr),dimension(MAXGS)       :: XSID,YSID
	real(kind=skr),dimension(MAXGS,MAXGS) :: PIXEL,PIXNEW
	real(kind=skr),dimension(MAXGS*MAXGS) :: ZDATA
	real(kind=skr),dimension(20)          :: CVALUE

        !attention to this: it must be the default integer
	integer  nStr

i22 =0 ! flag to file
	FILE_IN = RSTRING('PLOT> Input file? ')

if (trim(file_in) == "") file_in="begin.dat"


!     	CALL	beamLoad	(FILE_IN,RAY,NCOL,NPOINT,IFLAG,IERR)
!	IF (IERR.NE.0)	STOP	'Error in reading ray file.'

!
! reads input binary file
!

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    beamGetDim (file_In,ncol1,npoint1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Error: PLOTXY: beamGetDim: Error in file: '//trim(file_in)
            return
            !stop
        END IF

  	ALLOCATE( RAY(18,NPOINT1) )
  	ALLOCATE( RAY2(18,NPOINT1) )
  	ALLOCATE( A_SQUARE(NPOINT1) )
  	ALLOCATE( A2_SQUARE(NPOINT1) )
  	ALLOCATE( XPLOT(NPOINT1) )
  	ALLOCATE( YPLOT(NPOINT1) )
  	ALLOCATE( weightPlot(NPOINT1) )
  	ALLOCATE( XLOSS(NPOINT1) )
  	ALLOCATE( YLOSS(NPOINT1) )

  	ray=0.0d0
	CALL beamLoad(ray,ierr,ncol1,npoint1,file_In)

	npoint = npoint1
	ncol = ncol1

	WRITE(6,*) 'PLOT> Options --- Enter'
     	WRITE(6,*) 'PLOT>   0   for excluding the losses'
     	WRITE(6,*) 'PLOT>   1   for including only the losses'
     	WRITE(6,*) 'PLOT>   2   for including all the rays.'
     	ILOST = IRINT ('PLOT> Then ? ')
	WRITE(6,*) 'PLOT> Comment for plot [ 80 char ] ?'
	WRITE(6,*) '*******************/*******************/',&
      '*******************/*******************/'
     	READ (5,1000)	COMMENT
1000	FORMAT (A)

	NLOSS	=   0
     	DO 11 I=1,NPOINT
     	IF (RAY(10,I).LT.0.0) NLOSS = NLOSS + 1
11      CONTINUE
100     CONTINUE
     	WRITE(6,*) ' '
     	NGOOD = NPOINT - NLOSS
	WRITE(6,*) 'PLOT>  Found ',NGOOD, ' good points out of',NPOINT
!C********1*********2*********3*********4*********5*********6*********7**
	WRITE(6,*) 'PLOT>  The following columns are defined for each ray'
	IF (NCOL.GE.12) THEN
	  WRITE(6,*) '       1)  the regular columns [1-12]'
	  IF (NCOL.GE.13) THEN
	    WRITE(6,*) '       2)  optical path [13]'
	    IF (NCOL.EQ.18) THEN
	      WRITE(6,*) '       3)  phase angle of As[14], Ap[15], and'&
      ,' the Ap vector [16-18]'
	    END IF
	  END IF
	END IF
!C
     DO j=1,30
       R_LOW(j) =   1.0D+20
       R_UPP(j) = - 1.0D+20
     END DO

!     	R_LOW(1) =   1.0D+20
!     	R_UPP(1) = - 1.0D+20
!     	R_LOW(2) =   1.0D+20
!     	R_UPP(2) = - 1.0D+20
!     	R_LOW(3) =   1.0D+20
!     	R_UPP(3) = - 1.0D+20
!
!     	R_LOW(4) =   1.0D+20
!     	R_UPP(4) = - 1.0D+20
!     	R_LOW(5) =   1.0D+20
!     	R_UPP(5) = - 1.0D+20
!     	R_LOW(6) =   1.0D+20
!     	R_UPP(6) = - 1.0D+20
!
!     	R_LOW(11) =   1.0D+20
!     	R_UPP(11) = - 1.0D+20
!
!     	R_LOW(13) =   1.0D+20
!     	R_UPP(13) = - 1.0D+20
!
!     	R_LOW(20) =   1.0D+20
!	R_UPP(20) = - 1.0D+20
!     	! new columns
!	DO I=21,30
!     	  R_LOW(I) =   1.0D+20
!	  R_UPP(I) = - 1.0D+20
!	END DO

!initialize test
	TEST = 0.0D0

     	DO 300 I=1,NPOINT
	 IF (ILOST.EQ.0) THEN
	   IF (RAY(10,I).LT.0.0D0)   GO TO 300
	 ELSE IF (ILOST.EQ.1) THEN
     	   IF (RAY(10,I).GE.0.0D0)   GO TO 300
     	 ELSE
     	 END IF
         DO j=1,18
           TEST(j)= RAY(j,I)
         END DO
!     	TEST(1)	=   RAY(1,I)
!     	TEST(2)	=   RAY(2,I)
!     	TEST(3)	=   RAY(3,I)
!     	TEST(4)	=   RAY(4,I)
!     	TEST(5)	=   RAY(5,I)
!     	TEST(6)	=   RAY(6,I)
!     	TEST(11) =   RAY(11,I)
!     	TEST(13) =   RAY(13,I)

     	TEST(20) =   RAY(4,I)**2 + RAY(6,I)**2
     	TEST(20) =   ABS( SQRT(TEST(20))/RAY(5,I) )
!new columns
     	TEST(21) =   SQRT(RAY(1,I)**2 + RAY(2,I)**2 + RAY(3,I)**2)
     	TEST(22) =   ACOS(RAY(5,I))
     	TEST(23) =   RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2
	IF (NCOL.EQ.18) THEN
     	 TEST(24) =   RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2
        END IF
     	TEST(25) =   TEST(23) + TEST(24)
!intensities
     	TEST(23) =   SQRT(TEST(23))
     	TEST(24) =   SQRT(TEST(24))
     	TEST(25) =   SQRT(TEST(25))



!     	R_LOW(1)= MIN(R_LOW(1),TEST(1))
!     	R_UPP(1)= MAX(R_UPP(1),TEST(1))
!     	R_LOW(2)= MIN(R_LOW(2),TEST(2))
!     	R_UPP(2)= MAX(R_UPP(2),TEST(2))
!     	R_LOW(3)= MIN(R_LOW(3),TEST(3))
!        R_UPP(3)= MAX(R_UPP(3),TEST(3))
!
!     	R_LOW(4)= MIN(R_LOW(4),TEST(4))
!     	R_UPP(4)= MAX(R_UPP(4),TEST(4))
!     	R_LOW(5)= MIN(R_LOW(5),TEST(5))
!     	R_UPP(5)= MAX(R_UPP(5),TEST(5))
!     	R_LOW(6)= MIN(R_LOW(6),TEST(6))
!     	R_UPP(6)= MAX(R_UPP(6),TEST(6))
!
!     	R_LOW(11)= MIN(R_LOW(11),TEST(11))
!	R_UPP(11)= MAX(R_UPP(11),TEST(11))
!
!     	R_LOW(13)= MIN(R_LOW(13),TEST(13))
!	R_UPP(13)= MAX(R_UPP(13),TEST(13))
!
!     	R_LOW(20)= MIN(R_LOW(20),TEST(20))
!	R_UPP(20)= MAX(R_UPP(20),TEST(20))

        !DO II=21,30 
        DO II=1,30 
          R_LOW(II)= MIN(R_LOW(II),TEST(II))
          R_UPP(II)= MAX(R_UPP(II),TEST(II))
        END DO
300	CONTINUE
!
! computes intensity
!


        !C
        !C We ALWAYS need A**2 later on then.
        !C
	IF (NCOL.EQ.18) THEN
	     DO I = 1, NPOINT
	       A_SQUARE(I)= RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2 &
      			+ RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2
             END DO
	ELSE
	     DO I = 1, NPOINT
	       A_SQUARE(I)= RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2
             END DO
	END IF

        TOT_INTENSITY=0.0D0
     	DO I=1,NPOINT
     	  IDOIT = 1
     	  IF (ILOST.EQ.0.AND.RAY(10,I).LT.0.0) IDOIT = 0
     	  IF (ILOST.EQ.1.AND.RAY(10,I).GE.0.0) IDOIT = 0
     	  IF (IDOIT.EQ.1) TOT_INTENSITY=TOT_INTENSITY+A_SQUARE(i)
        END DO

!C
!C computes centers and variance
!C
     	DO 12 JCOL=1,6
     	  XMEAN(JCOL)	= 0.0D0
     	  VAR(JCOL)	= 0.0D0
     	  NPCOL		= 0
     	 DO 13 I=1,NPOINT
     	  IDOIT = 1
     	  IF (ILOST.EQ.0.AND.RAY(10,I).LT.0.0) IDOIT = 0
     	  IF (ILOST.EQ.1.AND.RAY(10,I).GE.0.0) IDOIT = 0
     	  IF (IDOIT.EQ.1) THEN
     	    XMEAN(JCOL) 	= XMEAN(JCOL) + RAY(JCOL,I)
     	    VAR(JCOL)	= VAR(JCOL) + RAY(JCOL,I)**2
     	    NPCOL	=   NPCOL + 1
     	  END IF
13	 CONTINUE
     	  IF (NPCOL.EQ.0) THEN
     	    WRITE (6,*) 'NPCOL zero for column ',JCOL
	    GO TO 12
     	  END IF
     	  XMEAN(JCOL) = XMEAN(JCOL)/NPCOL
     	  VAR(JCOL)   = VAR(JCOL)/NPCOL - XMEAN(JCOL)**2
     	 IF (VAR(JCOL).GE.0.0D0) THEN
     	   STDEV(JCOL) = SQRT(VAR(JCOL))
     	 ELSE
     	   STDEV(JCOL) = 0.0D0
     	 END IF
12     	CONTINUE


     	WRITE (6,*)
     	WRITE (6,2000)
     	WRITE (6,*)
     	WRITE (6,2010) 1,'X ',R_LOW(1),R_UPP(1),XMEAN(1),STDEV(1)
     	WRITE (6,2010) 2,'Y ',R_LOW(2),R_UPP(2),XMEAN(2),STDEV(2)
     	WRITE (6,2010) 3,'Z ',R_LOW(3),R_UPP(3),XMEAN(3),STDEV(3)
     	WRITE (6,2010) 4,'X''',R_LOW(4),R_UPP(4),XMEAN(4),STDEV(4)
     	WRITE (6,2010) 5,'Y''',R_LOW(5),R_UPP(5),XMEAN(5),STDEV(5)
     	WRITE (6,2010) 6,'Z''',R_LOW(6),R_UPP(6),XMEAN(6),STDEV(6)
        WRITE (6,2020) 11,'Photon Energy  (eV)',TOCM*R_LOW(11)/TWOPI &
      ,R_UPP(11)*TOCM/TWOPI
     	WRITE (6,2020) 20,'Numerical Aperture',R_LOW(20),R_UPP(20)
! new columns srio@esrf.eu 2011-01-05
     	WRITE (6,2020) 21,'R=SQRT(X**2+Y**2+Z**2)',  R_LOW(21),R_UPP(21)
     	WRITE (6,2020) 22,'angle [rad] from Y-axis', R_LOW(22),R_UPP(22)
     	WRITE (6,2020) 23,'Intensity-s',             R_LOW(23),R_UPP(23)
     	WRITE (6,2020) 24,'Intensity-p',             R_LOW(24),R_UPP(24)
     	WRITE (6,2020) 25,'Intensity  ',             R_LOW(25),R_UPP(25)
2000    FORMAT (//,T2,'Col',T5,' Par',T10,'Minimum:',T25,'Maximum:', &
                T40,'Center:',T55,'St. Dev.:')

2010	FORMAT (1X,T2,I2,T5,A3,T10,G12.5,T25,G12.5,T40,G12.5,T55,G12.5)
2020	FORMAT (1X,T2,I2,T7,A,T30,G12.5,T45,G12.5)

     	WRITE(6,*) 'Total Intensity:  ',TOT_INTENSITY
     	WRITE(6,*) ' '
     	WRITE(6,*) 'PLOT> Options. You may plot any two rows from '
	WRITE(6,*) '      the above list versus each other. '
	WRITE(6,*) '      You may also plot any of them versus the '
	WRITE(6,*) '      ray Numerical Aperture. N.A. -- enter 20.'
     	WRITE(6,*) ' '
     	WRITE(6,*) 'PLOT> Rows to use for plot :'
     	IX	=   IRINT ('PLOT>   for horizontal axis ? ')
IF (IX == 0) IX=1
     	IY	=   IRINT ('PLOT>   for vertical axis   ? ')
IF (IY == 0) IY=3
     	IF (IX.EQ.11.OR.IY.EQ.11) THEN
     	  WRITE(6,*) 'PLOT> Units for plot: '
     	  WRITE(6,*) '      0   For  cm-1'
     	  WRITE(6,*) '      1   For  eV'
     	  WRITE(6,*) '      2   For  Angs'
     	  IUNIT = IRINT ('PLOT> Then ? ')
     	ENDIF
     	WRITE(6,*) ' '
     	WRITE(6,*) 'PLOT> Scaling options. Enter '
     	WRITE(6,*) ' '
     	WRITE(6,*) '       0   For automatic scaling'
     	WRITE(6,*) '       1   For cartesian scaling'
     	WRITE(6,*) '       2   For external limits'
     	IPLOT = IRINT ('PLOT> Then ? ')
!srio!C
!srio!C takes care of the Optical Path Case -- since the variation is small
!srio!C we must avoid precision problems in the plotting, so that we will be
!srio!C plotting the OPD.
!srio!C 
!srio     	  IF (IX.EQ.13.OR.IY.EQ.13) THEN
!srio     	    P_CENT = (R_LOW(13)+R_UPP(13))/2.0D0
!srio     	    P_DEL  = R_UPP(13) - R_LOW(13)
!srio     	    R_LOW(13) = - 0.5D0*P_DEL
!srio     	    R_UPP(13) = + 0.5D0*P_DEL
!srio     	  END IF
     	  X_LOW = R_LOW(IX)
     	  Y_LOW = R_LOW(IY)
     	  X_UPP = R_UPP(IX)
     	  Y_UPP = R_UPP(IY)
     	  IF (IUNIT.EQ.1) THEN
     	   IF (IX.EQ.11) THEN
     	     X_LOW = TOCM*X_LOW/TWOPI
     	     X_UPP = TOCM*X_UPP/TWOPI
     	   ELSE IF (IY.EQ.11) THEN
     	     Y_LOW = TOCM*Y_LOW/TWOPI
     	     Y_UPP = TOCM*Y_UPP/TWOPI
     	   END IF
     	  ELSE IF (IUNIT.EQ.2) THEN
     	   IF (IX.EQ.11) THEN
     	     X_TMP = X_LOW
     	     X_LOW = TWOPI/X_UPP*1.0D8
     	     X_UPP = TWOPI/X_TMP*1.0D8
     	   ELSE IF (IY.EQ.11) THEN
     	     Y_TMP = Y_LOW
     	     Y_LOW = TWOPI/Y_UPP*1.0D8
     	     Y_UPP = TWOPI/Y_TMP*1.0D8
     	   END IF
     	  END IF
     	IF (IPLOT.EQ.0) THEN
     	  CALL	ROUNDOUT (X_LOW,X_UPP,Y_LOW,Y_UPP)
     	ELSE IF (IPLOT.EQ.1) THEN
     	  X_CEN = 0.5D0*(X_LOW+X_UPP)
     	  Y_CEN = 0.5D0*(Y_LOW+Y_UPP)
     	  X_DEL = X_UPP - X_LOW
     	  Y_DEL = Y_UPP - Y_LOW
     	  DEL	= MAX(X_DEL,Y_DEL)
     	  X_LOW = X_CEN - 0.5D0*DEL
     	  X_UPP = X_CEN + 0.5D0*DEL
     	  Y_LOW = Y_CEN - 0.5D0*DEL
     	  Y_UPP = Y_CEN + 0.5D0*DEL
     	  CALL	ROUNDOUT (X_LOW,X_UPP,Y_LOW,Y_UPP)
     	ELSE
     	  WRITE(6,*) 'PLOT> Found limits: '
     	  WRITE(6,*) 'PLOT>    Horizontal: ',X_LOW,X_UPP
     	  WRITE(6,*) 'PLOT>    vertical: ',Y_LOW,Y_UPP
     	  WRITE(6,*) 'PLOT>   '
     	  WRITE(6,*) 'PLOT> Enter new limits. '
     	  WRITE(6,*) 'PLOT> Horizontal min.: '
	  READ(5,*)  X_LOW
!C     	  X_LOW	= RNUMBER ('PLOT> Horizontal min.: ')
     	  WRITE(6,*) 'PLOT> Horizontal max.: '
	  READ(5,*)  X_UPP
!C     	  X_UPP = RNUMBER ('PLOT> Horizontal max.: ')
     	  WRITE(6,*) 'PLOT> Vertical min.: '
	  READ(5,*)  Y_LOW
!C     	  Y_LOW = RNUMBER ('PLOT> Vertical min.  : ')
     	  WRITE(6,*) 'PLOT> Vertical max.: '
	  READ(5,*)  Y_UPP
!C     	  Y_UPP = RNUMBER ('PLOT> Vertical max.  : ')
     	END IF
!C
     	IGRID = 0
     	WRITE(6,*) 'PLOT> Plotting options : '
	WRITE(6,*) '       0   For scattered plot'
	!WRITE(6,*) '       1   For connected plot'
	WRITE(6,*) '       1   For pixelized plot'
	WRITE(6,*) '       2   For contour plot'
     	IGRID = IRINT ('PLOT> Then ? ' )

iGridFile=0
IF (iGrid > 0) iGridFile=1

     	IF (IGRID.GE.1) THEN
     	 WRITE(6,*) 'PLOT> Number of bins [Nx by Ny] to prepare the grids.'
     	 NGX	=   IRINT ('PLOT> Nx [Default=101]: ')
     	 NGY	=   IRINT ('PLOT> Ny [Default=101]: ')
         IF (ngx == 0) ngx=101
         IF (ngy == 0) ngy=101
	 IF (igrid == 2) THEN
	   NCONT	=   IRINT ('PLOT> Number of contours [Default=10] : ')
           IF (nCont == 0) nCont=10
	 END IF
	 K_REFL	=   IRINT ('PLOT> Included reflectivity ? ')
  
  


         INORMAL = 0




     	 ISMOOTH = IYES ('Smoothing [ Y/N ] ? ')
	 iGridFile = 1
i22 =1
     	END IF





iCross=0
     	IF (ICROSS.EQ.1) THEN
	 EXT	= 0.4
	ELSE
	 EXT	= 0.8
     	END IF
     	Y_C = (Y_LOW + Y_UPP)/2
     	YU_C = Y_C + EXT*(Y_UPP-Y_C)
     	YL_C = Y_C - EXT*(Y_C-Y_LOW)
     	X_C = (X_LOW + X_UPP)/2
     	XU_C = X_C + EXT*(X_UPP-X_C)
     	XL_C = X_C - EXT*(X_C-X_LOW)
iMirr=0
!C
!C starts computing the histograms
!C
     	WRITE(6,*) 'PLOT> Ready for histograms. Enter:'
     	WRITE(6,*) '     -1         to skip '
     	WRITE(6,*) '      0     for same limits as plot'
!     	WRITE(6,*) '      1                     3*stdev'
!     	WRITE(6,*) '      2                     external'
     	IANSW	=   IRINT ('PLOT> ? ')
     	IF (IANSW.EQ.0) THEN
     	  X_CENT = 0.5*(X_LOW + X_UPP)
     	  XWIDTH = X_UPP - X_LOW
     	  Y_CENT = 0.5*(Y_LOW + Y_UPP)
     	  YWIDTH = Y_UPP - Y_LOW
     	ELSE IF (IANSW.EQ.1) THEN
     	  X_CENT = XMEAN(IX)
     	  Y_CENT = XMEAN(IY)
     	  XWIDTH = 6*STDEV(IX)
     	  YWIDTH = 6*STDEV(IY)
     	ELSE IF (IANSW.EQ.2) THEN
!C     	  X_CENT =  RNUMBER ('PLOT> Center for [ X ] axis: ')
     	  WRITE(6,*) 'PLOT> Center for [ X ] axis: '
	  READ(5,*) X_CENT
!C     	  XWIDTH =  RNUMBER ('PLOT> Width            : ')
     	  WRITE(6,*) 'PLOT> Width            : '
	  READ(5,*) XWIDTH
!C     	  Y_CENT =  RNUMBER ('PLOT> Center for [ Y ] axis: ')
     	  WRITE(6,*) 'PLOT> Center for [ Y ] axis: '
	  READ(5,*) Y_CENT
!C     	  YWIDTH =  RNUMBER ('PLOT> Width            : ')
     	  WRITE(6,*) 'PLOT> Width            : '
	  READ(5,*) YWIDTH
     	END IF
	IF (IANSW.NE.-1) THEN
          if (iGrid > 0) then
            n_binX = ngx
            n_binY = ngy
          else
	  !IF (IGRID.NE.2) THEN
	    N_BINX =  IRINT   &
      		('PLOT> Number of bins for X axis [default = 55] : ')
	    IF (N_BINX.LT.1)	N_BINX	= 55
	    N_BINY =  IRINT   &
      		('PLOT> Number of bins for Y axis [default = 55] : ')
	    IF (N_BINY.LT.1)	N_BINY	= 55
          end if
	  !ELSE
	  !  N_BINX	= NGX
	  !  N_BINY	= NGY
	  !END IF
	END IF
	IF (IGRID.GT.0) THEN
!C
!C Prepare the side of the grid.
!C
	  X_STEP = (X_UPP - X_LOW)/NGX
	  Y_STEP = (Y_UPP - Y_LOW)/NGY
	  AREA	 = X_STEP*Y_STEP
!C
!C Scale power or # of rays to area
!C
!	  IF (ISCALE.NE.0) THEN
!   	    FACTOR = FACTOR/(PUNITS**2*AREA)
!	  ELSE
	    FACTOR = 1.0D0
!	  END IF
!C
	  DO 21 I = 1,NGX
	    XSID(I)	= X_LOW + X_STEP*(I-1) + X_STEP/2
21	  CONTINUE
	  DO 22 J = 1,NGY
	    YSID(J)	= Y_LOW + Y_STEP*(J-1) + Y_STEP/2
22	  CONTINUE
	END IF
!C
!C Loop thru all the rays.
!C
     	IPASS = 0
! starts loop
9999	KX = 0
     	KY = 0
	KPLOT = 0
	KLOSS = 0
     	IPASS = IPASS + 1
     	DO 8989 I=1,NPOINT
     	  !IF (IX.NE.20) XWRI	=  RAY(IX,I)
     	  !IF (IY.NE.20) YWRI  	=  RAY(IY,I)
     	  IF (IX.LT.19) XWRI	=  RAY(IX,I)
     	  IF (IY.LT.19) YWRI  	=  RAY(IY,I)
     	  IF (IX.EQ.11) THEN
             IF (XWRI.LT.1D-8) GO TO 8989
     	     IF (IUNIT.EQ.1) THEN
     	       XWRI = XWRI*TOCM/TWOPI
     	     ELSE IF (IUNIT.EQ.2) THEN
     	       XWRI = TWOPI/XWRI*1.0E8
     	     END IF
     	  END IF
     	  IF (IY.EQ.11) THEN
            IF (YWRI.LT.1D-8) GO TO 8989
     	     IF (IUNIT.EQ.1) THEN
     	       YWRI = YWRI*TOCM/TWOPI
     	     ELSE IF (IUNIT.EQ.2) THEN
     	       YWRI = TWOPI/YWRI*1.0E8
     	     END IF
     	  END IF
     	  IF (IX.EQ.20) THEN
     	      XWRI = ABS( SQRT( RAY(4,I)**2 + RAY(6,I)**2 )/RAY(5,I) )
     	  ELSE IF (IY.EQ.20) THEN
     	      YWRI = ABS( SQRT( RAY(4,I)**2 + RAY(6,I)**2 )/RAY(5,I) )
     	  END IF
!     	  IF (IX.EQ.13) THEN
!     	      XWRI = XWRI - P_CENT
!     	  ELSE IF (IY.EQ.13) THEN
!     	      YWRI = YWRI - P_CENT
!     	  END IF

!new columns
     	  IF (IX.EQ.23) THEN
     	      XWRI =  sqrt(RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2)
     	  ELSE IF (IY.EQ.23) THEN
     	      YWRI =  sqrt(RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2)
     	  END IF

	  IF (NCOL.EQ.18) THEN
     	    IF (IX.EQ.24) THEN
     	        XWRI =  sqrt(RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2)
     	    ELSE IF (IY.EQ.24) THEN
     	        YWRI =  sqrt(RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2)
     	    END IF
     	    IF (IX.EQ.25) THEN
     	        XWRI =  sqrt(RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2 + RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2)
     	    ELSE IF (IY.EQ.25) THEN
     	        YWRI =  sqrt(RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2 + RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2)
     	    END IF
          ELSE
     	    IF (IX.EQ.24) THEN
     	        XWRI =  0.0D0
     	    ELSE IF (IY.EQ.24) THEN
     	        YWRI =  0.0D0
     	    END IF
     	    IF (IX.EQ.25) THEN
     	        XWRI =  sqrt(RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2 )
     	    ELSE IF (IY.EQ.25) THEN
     	        YWRI =  sqrt(RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2 )
     	    END IF

          END IF


!C
!C Scattered plot :
!C
     	 IF (IGRID.EQ.0) THEN
          IF (ILOST.EQ.0) THEN
           IF (RAY(10,I).GE.0.0D0) THEN
		KPLOT		= KPLOT + 1
		XPLOT (KPLOT)	= XWRI
		YPLOT (KPLOT)	= YWRI
		weightPlot (KPLOT)	= sqrt(A_SQUARE(I))
	   END IF
     	  ELSE IF (ILOST.EQ.1) THEN
    	   IF (RAY(10,I).LT.0.0D0) THEN
		KPLOT		= KPLOT + 1
		XPLOT (KPLOT)	= XWRI
		YPLOT (KPLOT)	= YWRI
		weightPlot (KPLOT)	= sqrt(A_SQUARE(I))
	   END IF
     	  ELSE IF (ILOST.EQ.2) THEN
    	   IF (RAY(10,I).GE.0.0D0) THEN
		KPLOT		= KPLOT + 1
		XPLOT (KPLOT)	= XWRI
		YPLOT (KPLOT)	= YWRI
		weightPlot (KPLOT)	= sqrt(A_SQUARE(I))
	   ELSE
		KLOSS		= KLOSS + 1
		XLOSS (KLOSS)	= XWRI
		YLOSS (KLOSS)	= YWRI
		weightPlot (KPLOT)	= sqrt(A_SQUARE(I))
	   END IF
     	  END IF
!C
!C Pixelized and Contour plot :
!C
         ELSE ! IF (IGRID.EQ.2) THEN
	   IF (ILOST.EQ.0.AND.RAY(10,I).LT.0.0D0) GO TO 8989
	   IF (ILOST.EQ.1.AND.RAY(10,I).GE.0.0D0) GO TO 8989
	   KPLOT	= KPLOT + 1
	   ARG_X	= (XWRI - X_LOW)/X_STEP
	   ARG_Y	= (YWRI - Y_LOW)/Y_STEP
	   I_BIN	= INT(ARG_X) + 1
	   J_BIN	= INT(ARG_Y) + 1
	   IF (I_BIN.GT.MAXGS.OR.I_BIN.LT.1) GO TO 8989
	   IF (J_BIN.GT.MAXGS.OR.J_BIN.LT.1) GO TO 8989
	   if (k_refl.eq.1) then
	       ARG	= A_SQUARE(I)
           ELSE
	     ARG	= 1.0D0
	   END IF
	   PIXEL(I_BIN,J_BIN)	= PIXEL(I_BIN,J_BIN) + ARG
     	 END IF  ! end scatter OR connected OR contour
8989   	CONTINUE
     	IF (IGRID.EQ.1.AND.IPASS.EQ.1) GO TO 9999
! loop ended





	IF (IGRID.EQ.0)	THEN
!
! write file with scatter points
!
	  OPEN(36,FILE='plotxy_scatter.dat',STATUS='UNKNOWN')
	  WRITE(36,*) "# plotxy scatter data for plotxy.gpl"
	  WRITE(36,*) "# Xplot Yplot WeightPlot"
	  IF (KPLOT.NE.0) THEN
	    DO 3031 I = 1,KPLOT
	      WRITE(36,*) XPLOT(I),YPLOT(I),weightPlot(I)
3031	    CONTINUE
	  END IF
	  CLOSE(36)
	  print *,'File written to disk: plotxy_scatter.dat'
	  IF (ILOST.EQ.2) THEN
	   IF (KLOSS.GT.0) THEN
	     OPEN(40,FILE='plotxy_lost.dat',STATUS='UNKNOWN')
	     DO 3032 I = 1,KLOSS
	       WRITE(40,*) XLOSS(I),YLOSS(I)
3032	     CONTINUE
	   END IF
	  END IF !iLost
	ELSE !IF (IGRID.EQ.2) THEN
!C
!C Smooths the picture for contour plots
!C
     	  IF (ISMOOTH.EQ.1) THEN
     	   DO 26 L=2,NGX-1
     	    DO 27 J=2,NGY-1
     	      PIXNEW (L,J) = 4*PIXEL(L,J) +   &
      			2*PIXEL(L,J-1) + 2*PIXEL(L,J+1) +  &
      			2*PIXEL(L+1,J) + 2*PIXEL(L-1,J) +  &
      			PIXEL(L+1,J+1) + PIXEL(L+1,J-1) +  &
      			PIXEL(L-1,J+1) + PIXEL(L-1,J-1)
     	      PIXNEW (L,J) = PIXNEW (L,J)/16
27	    CONTINUE
26	   CONTINUE
!C
!C Sides
!C
     	   DO 28 L=2,NGX-1
     	     PIXNEW (L,1) = 4*PIXEL(L,1) +  &
      			2*PIXEL(L-1,1) + 2*PIXEL(L,2) + 2*PIXEL(L+1,1) + &
      			PIXEL(L-1,2) + PIXEL(L+1,2)
     	     PIXNEW (L,NGY) = 4*PIXEL(L,NGY) +  &
      			2*PIXEL(L-1,NGY) + 2*PIXEL(L,NGY-1) +  &
      			2*PIXEL(L+1,NGY) + &
      			PIXEL(L-1,NGY-1) + PIXEL(L+1,NGY-1)
     	     PIXNEW (L,1) = PIXNEW(L,1)/12
     	     PIXNEW (L,NGY) = PIXNEW (L,NGY)/12
28	   CONTINUE
     	   DO 29 J=2,NGY-1
     	     PIXNEW (1,J) = 4*PIXEL(1,J) +  &
      			2*PIXEL(1,J-1) + 2*PIXEL(2,J) + 2*PIXEL(1,J+1) + &
      			PIXEL(2,J-1) + PIXEL(2,J+1)
     	     PIXNEW (NGX,J) = 4*PIXEL(NGX,J) +  &
      			2*PIXEL(NGX,J-1) + 2*PIXEL(NGX-1,J) +  &
      			2*PIXEL(NGX,J+1) + &
      			PIXEL(NGX-1,J-1) + PIXEL(NGX-1,J+1)
     	     PIXNEW (1,J) = PIXNEW(1,J)/12
     	     PIXNEW (NGX,J) = PIXNEW(NGX,J)/12
29	   CONTINUE
!C
!C Corners
!C
     	    PIXNEW (1,1) = 4*PIXEL(1,1) + 2*PIXEL(1,2) + 2*PIXEL(2,1) + &
       			PIXEL(2,2)
      	    PIXNEW (1,NGY) = 4*PIXEL(1,NGY) + 2*PIXEL(1,NGY-1) + &
      			2*PIXEL(2,NGY) + PIXEL(2,NGY-1)
     	    PIXNEW (NGX,1) = 4*PIXEL(NGX,1) + 2*PIXEL(NGX,2) + &
      			2*PIXEL(NGX-1,1) + PIXEL(NGX-1,2)
     	    PIXNEW (NGX,NGY) = 4*PIXEL(NGX,NGY) + 2*PIXEL(NGX,NGY-1) + &
      			2*PIXEL(NGX-1,NGY) + PIXEL(NGX-1,NGY-2)
     	    PIXNEW (1,1) = PIXNEW(1,1)/9
     	    PIXNEW (1,NGY) = PIXNEW(1,NGY)/9
     	    PIXNEW (1,NGX) = PIXNEW(1,NGX)/9
     	    PIXNEW (NGX,NGY) = PIXNEW(NGX,NGY)/9
     	   DO 31 L=1,NGX
     	    DO 32 J=1,NGY
     	      PIXEL(L,J) = PIXNEW(L,J)
32	    CONTINUE
31	   CONTINUE 
	  END IF
!C
!C save grid file
!C
     	IF (iGridFile.EQ.1) THEN
     	  OPEN (21, FILE='plotxy_grid.dat', STATUS='UNKNOWN')
     	  WRITE (21,*) '# plotxy grid data for plotxy.gpl'
     	  WRITE (21,*) '# Xbin Ybin Weight'
!     	  WRITE (21,*) '; grid Nx Ny:',NGX,NGY
     	 DO 33 L=1,NGX
     	  DO 34 J=1,NGY
     	    WRITE (21,*) XSID(L),YSID(J),PIXEL(L,J)
34	  CONTINUE
     	    WRITE (21,*)  " "
33	 CONTINUE
     	  CLOSE(21)
	  print *,'File written to disk: plotxy_grid.dat'
     	END IF
!C
!C prepare for plotting
!C
	  ZMAX	= -1.0D20
	  ZMIN	=  1.0D20
	  DO 35 I = 1,NGX
	    DO 36 J = 1,NGY
	      ZMAX	= MAX(ZMAX,PIXEL(I,J))
	      ZMIN	= MIN(ZMIN,PIXEL(I,J))
36	    CONTINUE
35	  CONTINUE
	  IF (INORMAL.EQ.1) THEN
	    DO 37 I = 1,NGX
	      DO 38 J = 1,NGY
	        PIXEL(I,J)	= PIXEL(I,J)/ZMAX
38	      CONTINUE
37	    CONTINUE
	    ZMIN	= ZMIN/ZMAX
	    ZMAX	= 1.0D0
	  ELSE IF (INORMAL.EQ.2) THEN
	    DO 39 I = 1,NGX
	      DO 41 J = 1,NGY
	        PIXEL(I,J)	= PIXEL(I,J)/KPLOT
41	      CONTINUE
39	    CONTINUE
	    ZMAX	= ZMAX/KPLOT
	    ZMIN	= ZMIN/KPLOT
	  END IF
!	  DO 42 I = 1,NCONT
!	    CVALUE(I)	= ZMIN + I*(ZMAX-ZMIN)/(NCONT+1)
!42	  CONTINUE
	  K = 0
	  DO 43 J = 1,NGY
	    DO 44 I = 1,NGX
	      K		= K + 1
	      ZDATA(K)	= PIXEL(I,J)
44	    CONTINUE
43	  CONTINUE
!	  ICONT(1)	= NCONT
!	  ICONT(2)	= 1
!	  ICONT(3)	= 27
!	  ICONT(4)	= ITERM
!	  IPLT(1)	= I_TD
!	  IPLT(3)	= 0
	  XYLIM(1)      = X_LOW
	  XYLIM(2)	= X_UPP
	  XYLIM(3)  	= Y_LOW
	  XYLIM(4)	= Y_UPP
	END IF
!C
!C Now the histogram.
!C
xplot=0
yplot=0
     	IF (IANSW.EQ.-1) GO TO 5001
     	INORM = 0
     	IREFL	=   0
     	DO 47 KKK=1,2
     	 IF (KKK.EQ.1) THEN
     	   N_COL = IX
	   N_BIN = N_BINX
     	   STEP	=   XWIDTH/(N_BINX - 1)
     	   X_START	=   X_CENT - XWIDTH/2 - STEP/2
     	  DO 48 I=1,N_BIN
     	    X_ARRAY(I) =   (I-1)*STEP + X_START + STEP/2
	    Y_ARRAY(I) =   0.0D0
48	  CONTINUE
     	 ELSE
     	   N_COL = IY
	   N_BIN = N_BINY
     	   STEP	=   YWIDTH/(N_BINY - 1)
     	   X_START	=   Y_CENT- YWIDTH/2  - STEP/2
     	  DO 49 I=1,N_BIN
     	    X_ARRAY(I) =   (I-1)*STEP + X_START + STEP/2
	    Y_ARRAY(I) =   0.0D0
49	  CONTINUE
     	 END IF
!C
!C If plotting contours, then the histogram values are obtained by just 
!C integrating the grid (PIXEL) on each side.
!C
!	IF (IGRID.EQ.2) THEN
	if (IGRID.gt.0) then
	  IF (KKK.EQ.1) THEN
	    DO 51 I = 1, NGX
	      DO 52 J = 1, NGY
	        Y_ARRAY(I) = Y_ARRAY(I) + PIXEL(I,J)
52	      CONTINUE
51	    CONTINUE
	  ELSE
	    DO 53 J = 1, NGY
	      DO 54 I = 1, NGX
	        Y_ARRAY(J) = Y_ARRAY(J) + PIXEL(I,J)
54	      CONTINUE
53	      CONTINUE
	  END IF
	  GO TO 998
	END IF
     	DO 999 I=1,NPOINT
         IF (N_COL.EQ.11) THEN
	   IF (RAY(11,I).LT.1D-8) GO TO 999
           IF (IUNIT.EQ.1) THEN
     	     RAYTEST = TOCM*RAY(11,I)/TWOPI
     	   ELSE IF (IUNIT.EQ.2) THEN
     	     RAYTEST = TWOPI/RAY(11,I)*1.0E8
           END IF
         ELSE
           RAYTEST   = RAY(N_COL,I)
         END IF	 
     	 IF (ILOST.EQ.0) THEN
     	  IF (RAY(10,I).LT.0.0D0) GO TO 999
     	 ELSE	IF (ILOST.EQ.1) THEN
     	  IF (RAY(10,I).GT.0.0D0) GO TO 999
     	 ELSE	IF (ILOST.EQ.2) THEN
	 END IF
      	 ARG	=   (RAYTEST - X_START)/STEP
     	 J_BIN	=   INT (ARG) + 1
      	 IF (J_BIN.LT.1.OR.J_BIN.GT.N_BIN) GO TO 999
	 !IF (IREFL.EQ.1) THEN
	 IF (k_refl.EQ.1) THEN
	       VAL  =   A_SQUARE(I)
	 ELSE
		VAL = 1.0
	 END IF
    	 Y_ARRAY(J_BIN) = Y_ARRAY(J_BIN) + VAL
!998	CONTINUE
999	CONTINUE


! srio inverted the order
998	continue
     	IF (KKK.EQ.1) THEN
	  OPEN(37,FILE='plotxy_histtop.dat',STATUS='UNKNOWN')
	X1UPP = -1.0D20
     	DO 56 I=1,N_BIN
     	  XPLOT(I) = X_ARRAY(I)
     	  YPLOT(I) = Y_ARRAY(I)
	  X1UPP = MAX(X1UPP,Y_ARRAY(I))
56	CONTINUE
	X1UPP = X1UPP*1.1
	  X1LOW = 0.0D0
	  WRITE(37,*) XSTART,0.0
	  DO 3035 I = 1,N_BIN
	    WRITE(37,*) X_ARRAY(I)-STEP/2, Y_ARRAY(I)
	    WRITE(37,*) X_ARRAY(I)+STEP/2, Y_ARRAY(I)
3035	  CONTINUE
	  WRITE(37,*) X_ARRAY(N_BIN)+STEP/2, 0.0
	XMAX	= 0
	DO 3041 I=1,N_BIN
	  XMAX	= MAX(XMAX,Y_ARRAY(I))
3041	CONTINUE
	Y1LOW = 0.0
	Y1UPP = XMAX*1.1
        ELSE IF (KKK.EQ.2) THEN
	OPEN(38,FILE='plotxy_histside.dat',STATUS='UNKNOWN')
	XPLOT(1)	= X_ARRAY(1) - STEP/2
	YPLOT(1)	= 0.0
	XPLOT(2)	= X_ARRAY(1) - STEP/2
	YPLOT(2)	= Y_ARRAY(1)
	WRITE(38,*) YPLOT(1),XPLOT(1)
	WRITE(38,*) YPLOT(2),XPLOT(2)
	Y_ARRAY(N_BIN+1)	= 0.0
     	DO 59 I=1,N_BIN
     	  XWRI_L = X_ARRAY(I)-STEP/2
     	  XWRI_U = X_ARRAY(I)+STEP/2
     	 IF (IY.EQ.11) THEN
     	  IF (IUNIT.EQ.1) THEN
     	    XWRI_L = TOCM*XWRI_L/TWOPI
     	    XWRI_U = TOCM*XWRI_U/TWOPI
     	  ELSE IF (IUNIT.EQ.2) THEN
     	    XWRI_L = TWOPI/XWRI_L*1.0E8
     	    XWRI_U = TWOPI/XWRI_U*1.0E8
     	  END IF
     	 END IF
	 XPLOT (1)	= XWRI_L
	 YPLOT (1)	= Y_ARRAY(I)
	 XPLOT (2)	= XWRI_U
	 YPLOT (2)	= Y_ARRAY(I)
	 XPLOT (3)	= XWRI_U
	 YPLOT (3)	= Y_ARRAY(I+1)
	 WRITE(38,*) YPLOT(1),XPLOT(1)
	 WRITE(38,*) YPLOT(2),XPLOT(2)
	 WRITE(38,*) YPLOT(3),XPLOT(3)
59	CONTINUE
     	END IF
47	CONTINUE

5001	CONTINUE
     	IF (ILOST.EQ.0) THEN
	  TEXT1	= '--GOOD ONLY'
     	ELSE IF (ILOST.EQ.1) THEN
	  TEXT1	= '--LOST ONLY'
     	ELSE
	  TEXT1	= '--ALL RAYS'
     	END IF
	WRITE (TEXT2,11001)	NPOINT
11001	FORMAT	('TOT  = ',I7.1)
	WRITE (TEXT3,11102)	NLOSS
11102	FORMAT	('LOST = ',I7.1)

	WRITE (TEXT4,11103)	NPOINT-NLOSS
11103	FORMAT	('GOOD = ',I7.1)

	WRITE (TEXT5,11104)	TOT_INTENSITY
11104	FORMAT	('INTENS = ',G12.5)

	IF (k_refl ==0) THEN 
           TEXT8='WEIGHT: RAYS'
        ELSE 
           TEXT8='WEIGHT: INTENSITY'
        ENDIF

	IF (IPLOT.EQ.0) THEN
	  TEXT(1:19) = 'AUTOSCALING         '
	ELSE IF (IPLOT.EQ.1) THEN
	  TEXT(1:19) = 'CARTESIAN           '
	ELSE
	  TEXT(1:19) = 'EXTERNAL            '
	END IF
	IF (ILOST.EQ.0) THEN
	  TEXT(1:19) = '- good only         '
	ELSE IF (ILOST.EQ.1) THEN
	  TEXT(1:19) = '- lost only         '
	ELSE
	  TEXT(1:19) = '- all rays          '
	END IF
	IF (IGRID.EQ.2) THEN
	END IF

	IF (IANSW.EQ.-1) THEN
	END IF
	CLOSE(37)
	print *,'File written to disk: plotxy_histtop.dat'
	CLOSE(38)
	print *,'File written to disk: plotxy_histside.dat'
        IF (KLOSS.GT.0) THEN
	  CLOSE(40)
	  print *,'File written to disk: plotxy_lost.dat'
        END IF

! 
! write gnuplot file
!
    OPEN (35,FILE='plotxy.gpl',STATUS='UNKNOWN', FORM='FORMATTED')
    ! 
    epsilon=1e-10
    WRITE(35,'(A)') '#GnuPlot command file for PLOTXY'
    WRITE(35,'(A)') '#Minimum version: gnuplot 4.2 patchlevel 6'
    WRITE(35,'(A)') '#'
    WRITE(35,'(A)') ' '
    IF (iGrid == 2) THEN
       WRITE(35,'(A)') '                                           '
       WRITE(35,'(A)') '#                                          '
       WRITE(35,'(A)') '# IF CONTOUR UNCOMMENT HERE                '
       WRITE(35,'(A)') '#                                          '
       WRITE(35,'(A)') 'set table "./pippo.tmp"                   '
       WRITE(35,'(A)') 'set contour                               '
       WRITE(35,'(A)') 'set cntrparam levels incremental 0, 4, 40 '
       WRITE(35,'(A)') 'set contour base                          '
       WRITE(35,'(A)') 'unset surface                             '
       WRITE(35,'(A)') 'set view 0, 0                             '
       WRITE(35,'(A)') 'splot "plotxy_grid.dat" u 1:2:3 w pm3d    '
       WRITE(35,'(A)') 'unset contour                             '
       WRITE(35,'(A)') 'set output                                '
       WRITE(35,'(A)') 'unset table                               '
    END IF
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  '                                              '
IF (OS==1) THEN
    WRITE(35,'(A)')  'set terminal win size 900,600                 '
ELSE
    WRITE(35,'(A)')  'set terminal x11 size 900,600                 '
END IF
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  'set multiplot                                 '
    WRITE(35,'(A)')  '                                              '

        CALL GET_ENVIRONMENT_VARIABLE ('PWD', datadir, nStr)
IF (IANSW /= -1) THEN
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# top histogram                               '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'set lmargin screen 0.2125                     '
    WRITE(35,'(A)')  'set rmargin screen 0.70                       '
    WRITE(35,'(A)')  'set bmargin screen 0.75                       '
    WRITE(35,'(A)')  'set tmargin screen 0.90                       '
    WRITE(35,'(A)')  'unset xtics                                   '
    WRITE(35,'(A)')  'unset x2tics                                  '
    WRITE(35,'(A)')  'unset ytics                                   '
    WRITE(35,'(A)')  'unset y2tics                                  '
    WRITE(35,'(A)')  'unset key                                     '
    WRITE(35,'(A)')  'unset xlabel                                  '
    WRITE(35,'(A)')  'unset ylabel                                  '
    WRITE(35,'(A)')  'unset x2label                                 '
    WRITE(35,'(A)')  'unset y2label                                 '
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  'set x2tics mirror                             '
    !WRITE(35,'(A)')  'set x2label " 1234567890 1234567890 1234567890 "'
    !WRITE(35,'(A)')  'set x2label "'//trim(file_in)//'"'
        IF (nStr .gt. 0) THEN
         WRITE(35,'(A)')  'set x2label "'//trim(comment)//'  '//trim(datadir)//OS_DS//trim(file_in)//'"'
        ELSE
         WRITE(35,'(A)')  'set x2label "'//trim(comment)//'  '//trim(file_in)//'"'
        END IF 
    WRITE(35,*)  'set xrange[',X_LOW+epsilon,':',X_UPP-epsilon,']'
    WRITE(35,*)  'set yrange[*:*]'
    WRITE(35,'(A)')  'plot "plotxy_histtop.dat" u 1:2 w lines lt -1 notitle' 
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# side histogram                              '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'set lmargin screen 0.10                       '
    WRITE(35,'(A)')  'set rmargin screen 0.2125                     '
    WRITE(35,'(A)')  'set bmargin screen 0.10                       '
    WRITE(35,'(A)')  'set tmargin screen 0.75                       '
    WRITE(35,'(A)')  'unset xtics                                   '
    WRITE(35,'(A)')  'unset x2tics                                  '
    WRITE(35,'(A)')  'unset ytics                                   '
    WRITE(35,'(A)')  'unset y2tics                                  '
    WRITE(35,'(A)')  'unset key                                     '
    WRITE(35,'(A)')  'unset xlabel                                  '
    WRITE(35,'(A)')  'unset ylabel                                  '
    WRITE(35,'(A)')  'unset x2label                                 '
    WRITE(35,'(A)')  'unset y2label                                 '
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  'set ytics                                     '
    !WRITE(35,'(A)')  'set ylabel "Column Y"                         '
    WRITE(35,1211) IY

    WRITE(35,'(A)')  '                                              '
    WRITE(35,*)  'set xrange[*:*]'
    WRITE(35,*)  'set yrange[',Y_LOW+epsilon,':',Y_UPP-epsilon,']'
    WRITE(35,'(A)')  'plot "plotxy_histside.dat" u (-$1):2 w lines lt -1 notitle' 
END IF
1211    FORMAT ('set ylabel "Column ',I2,'"                         ')
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# scattered/contour plot                      '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'set lmargin screen 0.2125                     '
    WRITE(35,'(A)')  'set rmargin screen 0.70                       '
    WRITE(35,'(A)')  'set bmargin screen 0.10                       '
    WRITE(35,'(A)')  'set tmargin screen 0.75                       '
    IF (IANSW.NE.-1) WRITE(35,'(A)')  'unset xtics                                   '
    WRITE(35,'(A)')  'unset x2tics                                  '
    IF (IANSW.NE.-1) WRITE(35,'(A)')  'unset ytics                                   '
    WRITE(35,'(A)')  'unset y2tics                                  '
    WRITE(35,'(A)')  'unset key                                     '
    WRITE(35,'(A)')  'unset xlabel                                  '
    WRITE(35,'(A)')  'unset ylabel                                  '
    WRITE(35,'(A)')  'unset x2label                                 '
    WRITE(35,'(A)')  'unset y2label                                 '
    WRITE(35,'(A)')  '                                              '
    !WRITE(35,'(A)')  'set xlabel "Column X"                         '
    WRITE(35,1212) IX
1212    FORMAT ('set xlabel "Column ',I2,'"                         ')

    WRITE(35,'(A)')  '                                              '

IF (IANSW == -1) THEN 
    WRITE(35,1211) IY
    IF (nStr .gt. 0) THEN
         WRITE(35,'(A)')  'set x2label "'//trim(comment)//'  '//trim(datadir)//OS_DS//trim(file_in)//'"'
    ELSE
         WRITE(35,'(A)')  'set x2label "'//trim(comment)//'  '//trim(file_in)//'"'
    END IF 
ENDIF



    WRITE(35,*)  'set xrange[',X_LOW+epsilon,':',X_UPP-epsilon,']'
    WRITE(35,*)  'set yrange[',Y_LOW+epsilon,':',Y_UPP-epsilon,']'
    SELECT CASE (iGrid) 
    CASE(1) ! pixel
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# IF PIXEL UNCOMMENT THIS                     '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'set pm3d map                                  '
    WRITE(35,'(A)')  'set palette gray                              '
    WRITE(35,'(A)')  'splot "./plotxy_grid.dat" u 1:2:3 notitle     '
    CASE(0) ! scatter 
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# IF SCATTER UNCOMMENT THIS                   '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'plot "./plotxy_scatter.dat" u 1:2 w d lt -1 notitle' 
    CASE(2) ! contour
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# IF CONTOUR UNCOMMENT THIS                   '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'plot "./pippo.tmp" u 1:2 w li lt -1 notitle   '
    WRITE(35,'(A)')  '# !/bin/rm -rf pippo.tmp                      '
    CASE DEFAULT
    END SELECT
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  '# info column                                 '
    WRITE(35,'(A)')  '#                                             '
    WRITE(35,'(A)')  'set obj 10 rect from graph 1.20, graph 1 to graph 1.61, graph 0 '

        CALL GET_ENVIRONMENT_VARIABLE ('USER', TEXT6, nStr)
        CALL GET_ENVIRONMENT_VARIABLE ('HOST', TEXT7, nStr)
        WRITE(35,'(A)')  'set label "'//trim(text6)//'@'//trim(text7)//'" at graph 1.21, graph 0.9         '


      WRITE(35,'(A)')  'set label "'//trim(TEXT1)//'" at graph 1.21, graph 0.5         '


      WRITE(35,'(A)')  'set label "'//trim(TEXT2)//'" at graph 1.21, graph 0.30         '
      WRITE(35,'(A)')  'set label "'//trim(TEXT3)//'" at graph 1.21, graph 0.25        '
      WRITE(35,'(A)')  'set label "'//trim(TEXT4)//'" at graph 1.21, graph 0.20        '
      WRITE(35,'(A)')  'set label "'//trim(TEXT5)//'" at graph 1.21, graph 0.15        '
      WRITE(35,'(A)')  'set label "'//trim(TEXT8)//'" at graph 1.21, graph 0.10        '


    WRITE(35,'(A)')  'replot                                        '
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  'unset multiplot                               '
    WRITE(35,'(A)')  '                                              '
    WRITE (35,*)  "pause -1 'Press <Enter> to end graphic '"
    CLOSE(35) 
    print *,'File written to disk: plotxy.gpl'

	IFLAG = 0

  	IF(ALLOCATED( RAY ))       DEALLOCATE(RAY)
  	IF(ALLOCATED( RAY2 ))      DEALLOCATE(RAY2)
  	IF(ALLOCATED( A_SQUARE ))  DEALLOCATE(A_SQUARE)
  	IF(ALLOCATED( A2_SQUARE )) DEALLOCATE(A2_SQUARE)
  	IF(ALLOCATED( XPLOT ))     DEALLOCATE(XPLOT)
  	IF(ALLOCATED( YPLOT ))     DEALLOCATE(YPLOT)
  	IF(ALLOCATED( weightPlot ))     DEALLOCATE(weightPlot)
  	IF(ALLOCATED( XLOSS ))     DEALLOCATE(XLOSS)
  	IF(ALLOCATED( YLOSS ))     DEALLOCATE(YLOSS)

END SUBROUTINE PlotXY
!
!
!

!C+++
!C	PROGRAM		FFRESNEL
!C
!C	PURPOSE		To compute the diffracted image generated by the interf.
!C			of a given ray file.
!C
!C---
!     	PROGRAM		FFRESNEL
SUBROUTINE FFresnel

        implicit none

	character(len=sklen) :: inFile1
	real(kind=skr),dimension(:,:),allocatable :: ray
	real(kind=skr),dimension(2048)            :: rDis
        integer(kind=ski)   :: iErr,iFlag,ncol,np1,np,kLoss,iOver
        integer(kind=ski)   :: f_inc,kount,i,j,k
	real(kind=skr)      :: rMin,rMax,step,dist,wave,qNew,factor,qvec
	real(kind=skr)      :: rr_x,rr_z,vec1,vec2,rr,r,factor_inc,factor_nor
	real(kind=skr)      :: rImag,alpha,err
        complex(kind=skr)                  :: ARG_S,ARG_P,JEI
        complex(kind=skr),dimension(2048)  :: STOREX,STOREY,STOREZ
        complex(kind=skr),dimension(2048)  :: STOREX2,STOREY2,STOREZ2
        complex(kind=skr)                  :: errX, errY, errZ
     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
!C
     	JEI	=   (0.0D0,1.0D0)
     	
	!CALL CLSCREEN
     	WRITE(6,*) 'Define integration limits [ cm ].'
1     	RMIN	=   RNUMBER	('From               ')
     	RMAX	=   RNUMBER	(' to                ')
	NP	=   IRINT	('No. of points (max 2048) ')
	STEP	=   ABS(RMAX-RMIN)/(NP-1)
     	IF (NP.GT.2048) THEN
     	  STEP = (RMAX-RMIN)/1000
     	  WRITE(6,*) 'Too small a step. Smallest allowed is ',STEP
     	  GO TO 1
     	END IF
     	ALPHA	=  RNUMBER('Integration direction [ 0 = x, 90 = z ] ? ')
     	DIST	=  RNUMBER('Distance from plane [ cm ] ? ')
     	INFILE1 =  RSTRING('Ray input file ? ')
     	KLOSS	=  IRINT('[ 0 ] to discard losses, [ 1 ] to keep them. ')
!C

! 
! read input file 
!
	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    beamGetDim (inFile1,ncol,np1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Error: FFresnel: beamGetDim: Error in file: '//trim(inFile1)
            return
            ! stop
        END IF

  	ALLOCATE( RAY(18,NP1) )
  	ray=0.0d0

	CALL beamLoad(ray,ierr,ncol,np1,inFile1)

!

	IOVER = IYES('Do you want to override the wavelength in the file ? ')
	IF (IOVER.EQ.1) THEN
     	  WAVE	=  RNUMBER	('New wavelength [ Angs ]  ? ')
     	  QNEW	=   WAVE*1.0D-8
     	  QNEW	=   TWOPI/QNEW
	END IF

!C
!C Starts integration loop
!C
     	WRITE(6,*) 'Files read OK. '
     	FACTOR	=   RNUMBER('Conversion to [ cm ] factor (*) ? ')
	F_INC	=   IRINT('Include the S- and P- phase shifts in the computations ?')
	KOUNT	=   0
!C First set up the array of image location 
	DO I = 1, NP
	  RDIS(I)	= RMIN + (I-1)*STEP
	  STOREX(I)	= (0.0D0,0.0D0)
	  STOREY(I)	= (0.0D0,0.0D0)
	  STOREZ(I)	= (0.0D0,0.0D0)
	  STOREX2(I)	= (0.0D0,0.0D0)
	  STOREY2(I)	= (0.0D0,0.0D0)
	  STOREZ2(I)	= (0.0D0,0.0D0)
	END DO

     	DO J=1,NP1

     	 IF (KLOSS.EQ.0) THEN
     	   IF (RAY(10,J).LT.0.0D0) GO TO 100
     	 END IF
	 IF (IOVER.EQ.0) THEN
	   QVEC	= RAY(11,J)
	 ELSE
	   QVEC = QNEW
	 END IF
     	 KOUNT = KOUNT + 1

     	 DO K=1,NP
     	   RR_X	=   RDIS(K)*COS(TORAD*ALPHA)
     	   RR_Z	=   RDIS(K)*SIN(TORAD*ALPHA)
     	   VEC1 =   RR_X - RAY(1,J)*FACTOR
     	   VEC2 =   RR_Z - RAY(3,J)*FACTOR
     	   RR	=   SQRT(VEC1**2 + DIST**2 + VEC2**2)
     	   R 	=   RAY (13,J)*FACTOR
           ! 
           ! added srio@esrf.eu 2011-12-07 to avoid crashing when 
           ! using begin.dat as source (optical path zero)
           if (abs(R).lt.1d-10) R=1 
!** Common factor in front of the exponential :
	   FACTOR_INC	= RAY(5,J) + DIST/RR
	   FACTOR_NOR	= FACTOR_INC*QVEC/TWOPI/R/RR
!** Exponential factor :
	   IF (F_INC.EQ.1) THEN
     	     ARG_S	=   ((RR+R)*QVEC + RAY(14,J))*JEI
     	     ARG_P	=   ((RR+R)*QVEC + RAY(15,J))*JEI
	   ELSE
	     ARG_S	=   (RR+R)*QVEC*JEI
     	     ARG_P	=   (RR+R)*QVEC*JEI
	   END IF

     	   ARG_S	=   EXP(ARG_S)
	   ARG_S	=   FACTOR_NOR*ARG_S
     	   ARG_P	=   EXP(ARG_P)
	   ARG_P	=   FACTOR_NOR*ARG_P

     	   STOREX(K) = STOREX(K) + (ARG_S*RAY(7,J) + ARG_P*RAY(16,J))
     	   STOREY(K) = STOREY(K) + (ARG_S*RAY(8,J) + ARG_P*RAY(17,J))
     	   STOREZ(K) = STOREZ(K) + (ARG_S*RAY(9,J) + ARG_P*RAY(18,J))

           ! to compute errors
     	   STOREX2(K) = STOREX2(K) + (ARG_S*RAY(7,J) + ARG_P*RAY(16,J))**2
     	   STOREY2(K) = STOREY2(K) + (ARG_S*RAY(8,J) + ARG_P*RAY(17,J))**2
     	   STOREZ2(K) = STOREZ2(K) + (ARG_S*RAY(9,J) + ARG_P*RAY(18,J))**2

     	 END DO
100	CONTINUE
	END DO
     	OPEN	(20, FILE='FFPAR', STATUS='UNKNOWN')
	REWIND (20)
     	  WRITE (20,*) 'From ',RMIN,' to ',RMAX
	  WRITE (20,*) '     Step ',STEP
     	  WRITE (20,*) 'Wavelength ',WAVE,' angs.'
     	  WRITE (20,*) 'Image located at ',DIST
	  WRITE (20,*) 'Integration direction ',ALPHA
     	  WRITE (20,*) 'Scale factor [ units ] * ',FACTOR,' = [ cm ]'
     	 IF (KLOSS.EQ.0) THEN
     	  WRITE (20,*) 'Losses: NOT INCLUDED'
     	 ELSE
     	  WRITE (20,*) 'Losses:     INCLUDED'
     	 END IF
	 IF (F_INC.EQ.1) THEN
	  WRITE (20,*) 'S- and P- phase shifts :     INCLUDED'
	 ELSE
	  WRITE (20,*) 'S- and P- phase shifts : NOT INCLUDED'
	 END IF
     	  WRITE (20,*) 'Files used: ',INFILE1
     	CLOSE   (20)
        print *,'File FFPAR written to disk.'
     	OPEN	(20, FILE='FFRESNEL', STATUS='UNKNOWN')
	REWIND (20)
     	WRITE (20,*) "#S 1  File written by shadow3 ffresnel"
     	WRITE (20,*) "#N 3"
     	WRITE (20,*) "#L  X   Intensity  Error(3sigma)"
     	DO K=1,NP
     	  RIMAG = (ABS(STOREX(K)))**2 + (ABS(STOREY(K)))**2  &
      		  + (ABS(STOREZ(K)))**2
          ! storex = storex +/- errX
          errX  =  sqrt( storex2(k) - (STOREX(K)**2)/KOUNT )
          errY  =  sqrt( storey2(k) - (STOREY(K)**2)/KOUNT )
          errZ  =  sqrt( storez2(k) - (STOREZ(K)**2)/KOUNT )
     	  !err  = (ABS(errX))**2 + (ABS(errY))**2  + (ABS(errZ))**2
          ! a=s**2 -> da = 2*s*ds
     	  err  = &
            2*ABS(STOREX(K))*ABS(errX) + &
            2*ABS(STOREY(K))*ABS(errY) + &
            2*ABS(STOREZ(K))*ABS(errZ)

     	  WRITE (20,*) RDIS(K),RIMAG/KOUNT,3*err/KOUNT
     	END DO
     	CLOSE(20)
        print *,'File FFRESNEL written to disk.'

        IF (allocated(ray)) deallocate(ray)

END SUBROUTINE FFresnel






subroutine FFresnel2D(ray,npt,dist,image,nxpixel,xmax,xmin,nzpixel,zmax,zmin)

  implicit none

  real(kind=skr), dimension(18,npt), intent(in)                  :: ray
  integer(kind=ski), intent(in)                                  :: npt
  real(kind=skr), intent(in)                                     :: dist
  complex(kind=skx), dimension(3,nxpixel,nzpixel), intent(inout) :: image
  integer(kind=ski), intent(in)                                  :: nxpixel, nzpixel
  real(kind=skr), intent(in)                                     :: xmax, zmax
  real(kind=skr), intent(in)                                     :: xmin, zmin

  ! counters
  integer(kind=ski)                               :: i, j, k
  real(kind=skr)                                  :: qvec
  real(kind=skr)                                  :: rr_x, rr_z, vec1, vec2, xpixelsize, zpixelsize
  real(kind=skr)                                  :: rr, r, factor_inc, factor_nor, dist1
  complex(kind=skx)                               :: arg_s, arg_p
  real(kind=skr), dimension(nxpixel)              :: xmesh
  real(kind=skr), dimension(nzpixel)              :: zmesh

  real(kind=skr), parameter                       :: TWOPI = 6.283185307179586467925287D0

  real(kind=skr), parameter                       :: TORAD = 0.017453292519943295769237D0
  real(kind=skr), parameter                       :: ZERO = 0.0d0
  complex(kind=skx), parameter                    :: CZERO = (0.0d0,0.0d0)
  complex(kind=skx), parameter                    :: JEI = (0.0D0,1.0D0)

  xpixelsize = (xmax - xmin) / nxpixel
  zpixelsize = (zmax - zmin) / nzpixel
  if(xpixelsize.ne.zpixelsize) print *, "attention pixel are not squared \xpixelsize ", xpixelsize, "\zpixelsize ", zpixelsize

  do j=1, nxpixel
     xmesh(j) = xmin + (j-1)*xpixelsize
  end do

  do j=1, nzpixel
     zmesh(j) = zmin + (j-1)*zpixelsize
  end do

  do 7 i=1, npt
     if ( ray(10,i).le.ZERO ) goto 8
     qvec = ray(11,i)
     dist1= dist - ray(2,i)
     do j=1, nxpixel
        rr_x = xmesh(j)
        vec1 = rr_x - ray(1,i)
        do k=1, nzpixel
           rr_z = zmesh(k)
           vec2 = rr_z - ray(3,i)
           rr   = sqrt( vec1*vec1 + vec2*vec2 + dist1*dist1 )
           r    = ray(13,i)
           ! 
           ! added srio@esrf.eu 2011-12-07 to avoid crashing when 
           ! using begin.dat as source (optical path zero)
           if (abs(r).lt.1d-10) r=1 

           factor_inc = ray(5,i) + dist/rr
           factor_nor = factor_inc*qvec/twopi/r/rr

           arg_s = ( (rr+r)*qvec + ray(14,i) ) * jei
           arg_p = ( (rr+r)*qvec + ray(15,i) ) * jei
           arg_s = exp(arg_s)
           arg_s = factor_nor * arg_s
           arg_p = exp(arg_p)
           arg_p = factor_nor * arg_p

           image(1,j,k) = image(1,j,k) + ( arg_s*ray(7,i) + arg_p*ray(16,i) )
           image(2,j,k) = image(2,j,k) + ( arg_s*ray(8,i) + arg_p*ray(17,i) )
           image(3,j,k) = image(3,j,k) + ( arg_s*ray(9,i) + arg_p*ray(18,i) )
        end do
     end do
 8   continue
 7 end do

end subroutine FFresnel2D



!C+++
!C	PROGRAM		FFRESNEL2D_INTERFACE
!C
!C	PURPOSE		To prepare inputs for ffresnel2d()
!C
!C---
SUBROUTINE FFresnel2D_INTERFACE

implicit none

character(len=sklen) :: inFile1
real(kind=skr),dimension(:,:),allocatable :: ray
real(kind=skr),dimension(:,:),allocatable :: imageIntensity
complex(kind=skr),dimension(:,:,:),allocatable :: image
integer(kind=ski)  ::  ncol,np1,iflag,ierr, iover
integer(kind=ski)  ::  i,j,npx,npz,stot
real(kind=skr)     ::  wave,qnew
real(kind=skr)     ::  dist,xmin,xmax,zmin,zmax
           
      WRITE(6,*) 'Define integration limits [ cm ].'
      XMIN      =   RNUMBER      ('X From               ')
      XMAX      =   RNUMBER      ('  to                 ')
      ZMIN      =   RNUMBER      ('Z From               ')
      ZMAX      =   RNUMBER      ('  to                 ')
      NPX      =   IRINT      ('No. of pixels in X ')
      NPZ      =   IRINT      ('No. of pixels in Z')
      DIST      =  RNUMBER('Distance from plane [ cm ] ? ')
      INFILE1 =  RSTRING('Ray input file ? ')

! 
! read input file 
!
      !
      ! it is necessary to allocate ray array here, at the main level. 
      ! 
        CALL    beamGetDim (inFile1,ncol,np1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'FFresnel_Interface: beamGetDim: Error in file: '//trim(inFile1)
            return
        END IF

        ALLOCATE( RAY(18,NP1) )
        ray=0.0d0
        ALLOCATE( imageIntensity(npx,npz) )
        imageIntensity=0.0d0
        ALLOCATE( image(3,npx,npz) )
        image=(0.0d0,0.0d0)

      CALL beamLoad(ray,ierr,ncol,np1,inFile1)

!

      IOVER = IYES('Do you want to override the wavelength in the file ? ')
      IF (IOVER.EQ.1) THEN
             WAVE      =  RNUMBER      ('New wavelength [ Angs ]  ? ')
             QNEW      =   WAVE*1.0D-8
             QNEW      =   TWOPI/QNEW
             ray(11,:) =  QNEW
      END IF

      print *,'Calling FFresnel2D (please be patient...)'
      CALL FFresnel2D(ray,np1,dist,image,npx,xmax,xmin,npz,zmax,zmin)
      do i=1,npx
        do j=1,npz
        imageIntensity(i,j) = (ABS(image(1,i,j)))**2 + & 
                              (ABS(image(2,i,j)))**2 + &
                              (ABS(image(3,i,j)))**2
        imageIntensity(i,j) = imageIntensity(i,j) / np1
 
        end do
      end do


! write file

stot = skr*npx*npz

!print *,'type: ',skr
!print *,'size1: ',npx
!print *,'size2: ',npz
!print *,'full size: ',stot

open(unit=11,file='ffresnel2d.edf',status='unknown',form='formatted')

!print *,'File opened.'

write(11,'(A)') '{'
write(11,'(A)') 'HeaderID = EH:000001:000000:000000;'
write(11,'(A)') 'Image = 1 ;'
write(11,'(A)') 'ByteOrder = LowByteFirst ;'
write(11,'(A)') 'DataType = DoubleValue ;'
write(11,'(A,I15,A)') 'Dim_1 = ',npx,' ; '
write(11,'(A,I15,A)') 'Dim_2 = ',npz,' ; '
write(11,'(A,I30,A)') 'Size = ',stot,' ; '
write(11,'(A)') 'Title = EDF file written by shadow3/ffresnel2d;'
write(11,'(A)') '}'
close(11)

! watch the access='stream' to avoid fortran control records
open(unit=11,file='ffresnel2d.edf',status='unknown',form='unformatted', &
      position='append',access='stream')

write(11,err=20) imageIntensity

close(11)
print *,'File written to disk: ffresnel2d.edf'


IF (allocated(ray)) deallocate(ray)
IF (allocated(image)) deallocate(image)
IF (allocated(imageIntensity)) deallocate(imageIntensity)

RETURN

20 print *,'FFRESNEL2D_INTERFACE: Error'

END SUBROUTINE FFresnel2d_Interface



!
!
!

!C+++
!C
!C	PROGRAM		RECOLOR
!C
!C	This program will take a RAY file and modify the photon energies
!C	of each ray. The new file will then be used as a BEGIN file.
!C
!C---
SUBROUTINE ReColor
	implicit none

     	!mv to global_definitions real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
     	!mv to global_definitions real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
     	!mv to global_definitions real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
     	!mv to global_definitions real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
     	!mv to global_definitions real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
	!mv to global_definitions real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	!mv to global_definitions real(kind=skr),parameter ::TOANGS=  1.239852D+4		     

	character(len=sklen) :: inFile,outFile
	integer(kind=ski)   :: iErr,iFlag,npoint,ncol,iWhat,nLines,i,iSeed
	integer(kind=ski)   :: nn,iForm
	real(kind=skr)      :: line,cMin,cmax,cDelta,cVal
	real(kind=skr),dimension(10) :: wave
	real(kind=skr),dimension(:,:),allocatable :: ray

10     	WRITE(6,*)'File for input ?'
     	READ (5,'(a)')	INFILE
     	WRITE(6,*)'and for output ?'
     	READ (5,'(a)')	OUTFILE


	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    beamGetDim (inFile,ncol,npoint,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Error: ReColor: beamGetDim: Error in file: '//trim(inFile)
            return
            ! stop
        END IF

  	ALLOCATE( RAY(18,NPOINT) )
  	ray=0.0d0

	CALL beamLoad(ray,ierr,ncol,npoint,inFile)




     	WRITE(6,*)'Data ready. Read ',NPOINT,' points.'
     	WRITE(6,*)'Are you going to specify energies in eV [ 0 ] or'// &
      ' wavelengths in angstroms [ 1 ] ?'
     	READ(5,*)IWHAT
     	WRITE(6,*)'Available options :'
     	WRITE(6,*)'1	single line'
     	WRITE(6,*)'2	multi lines'
     	WRITE(6,*)'3	box distribution'
     	WRITE(6,*)'Then ?'
     	READ(5,*)LINE
     	IF (LINE.EQ.1) THEN
     	 WRITE(6,*)' Value ?'
     	 READ(5,*)WAVE(1)
     	 IF (IWHAT.EQ.0) WAVE(1) = TOANGS/WAVE(1)
     	ELSE IF (LINE.EQ.2) THEN
     	 WRITE(6,*)'How many lines ?'
     	 READ(5,*)NLINES
     	  DO 20 I=1,NLINES
     	   WRITE(6,*)'Line n. ',I,'. Then ?'
     	   READ(5,*)WAVE(I)
     	   IF (IWHAT.EQ.0) WAVE(I) = TOANGS/WAVE(I)
20     	  CONTINUE
     	ELSE IF (LINE.EQ.3) THEN
     	 WRITE(6,*)'Minimum value ?'
     	 READ(5,*)WAVE(1)
     	 WRITE(6,*)'Maximum value ?'
     	 READ(5,*)WAVE(2)
     	 IF (IWHAT.EQ.0) THEN
     	  WAVE(1) = TOANGS/WAVE(1)
     	  WAVE(2) = TOANGS/WAVE(2)
     	 END IF
     	 CMIN = TWOPI/WAVE(1)*1.0D8
     	 CMAX = TWOPI/WAVE(2)*1.0D8
     	 CDELTA = CMAX - CMIN
     	END IF
	IF (LINE.NE.1) THEN
     	  WRITE(6,*)'Enter seed for random number generator :'
     	  READ(5,*)ISEED
	END IF
     	DO 30 I=1,NPOINT
     	 IF (LINE.EQ.1) THEN
     	  RAY (11,I) = TWOPI/WAVE(1)*1.0D8
     	 ELSE IF (LINE.EQ.2) THEN
     	  NN	=   1 + NLINES*WRAN(ISEED)
     	  RAY(11,I)  =  TWOPI/WAVE(NN)*1.0D8
     	 ELSE IF (LINE.EQ.3) THEN
     	  CVAL	=   CMIN + WRAN(ISEED)*CDELTA
     	  RAY(11,I) = CVAL
     	 END IF
30     	CONTINUE
        IFORM = 0

	CALL beamWrite(ray,ierr,ncol,npoint,outFile)
	!IF (IERR.NE.0)	STOP	'Error in writting output file.'
	IF (IERR.NE.0) print *,'Error in writting output file.'


        IF (allocated(ray)) deallocate(ray)

     	WRITE(6,*)'All done.'
END SUBROUTINE ReColor

!
!
!


!C+++
!C
!C	PROGRAM		INTENS
!C
!C	PURPOSE		To plot the transmitted/absorbed intensity
!C			at a filter or mirror
!C
!C	INPUT		a RAY file from SHADOW
!C
!C	OUTPUT		a plottable file.
!C
!C---
	!PROGRAM		INTENS
	!IMPLICIT	REAL*8	(A-H,O-Z)
     	!DATA	TWOPI 	/  6.2831 85307 17958 64679 25287 D0 /
	!DATA	TOCM	/  1.239 852	D-4		     /
	!DATA	TOANGS  /  1.239 852    D+4		     /


SUBROUTINE Intens
	implicit none

     	!mv to shadow_globaldefinitions real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
	!mv to shadow_globaldefinitions real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	!mv to shadow_globaldefinitions real(kind=skr),parameter ::TOANGS=  1.239852D+4		     

	character(len=sklen)  :: file1,file2,outFil
	real(kind=skr),dimension(:,:),allocatable :: ray1,ray2
	real(kind=skr)   :: sigma,r_tot,r_tot2,pwFac,rFac,xpl,ypl,zpl,zpl0
	real(kind=skr)   :: zero=0.0

        integer(kind=ski) :: ncol1,ncol2,no,iFlag,iErr,kUnit,kPower,np
        integer(kind=ski) :: i_tot,i_lost,i,iLost,kind1,kPow,kPlt,kx,ky
        integer(kind=ski) :: kScaX,kScaY


     	FILE1	=   RSTRING ('File for Intensity calculations ? ' )

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    beamGetDim (file1,ncol1,np,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Erro: Intens: beamGetDim: Error in file: '//trim(file1)
            return
            !stop
        END IF

  	ALLOCATE( RAY1(18,NP) )
  	ray1=0.0d0

	CALL beamLoad(ray1,ierr,ncol1,np,file1)

!C
!C Also calculate the number of lost rays.
!C
     	I_TOT = 0
     	I_LOST = 0
	DO 29 I = 1, NP
	    IF (RAY1(10,I) .LT. 0.0D0) THEN
	       I_LOST = I_LOST + 1
	    ENDIF
	    I_TOT  = I_TOT + 1
 29	CONTINUE
	WRITE(6,*) 'Total rays :', I_TOT 
	WRITE(6,*) 'Good rays :', I_TOT - I_LOST
	WRITE(6,*) 'Each ray has ',NCOL1,' entries.'
!C
!C  Modified so that the info provided below *is* used for generating 
!C  the plot files.
!C
	WRITE(6,*) 'Which rays should be used?'
	WRITE(6,*) 'Enter 0 for only good rays'
	WRITE(6,*) '      1 for all rays'
	WRITE(6,*) '      2 for only lost rays'
	ILOST = IRINT ('<?>')
!C
!       initialize counters
        R_TOT = 0.0D0
        R_TOT2 = 0.0D0
        ! calculate total intensity 
	CALL INTENS_CALC(ray1,np,ncol1,ilost,r_tot,r_tot2)
	SIGMA = sqrt(max(R_TOT2-(R_TOT**2)/NP,zero))
     	WRITE(6,*) 'Total intensity is :',R_TOT
     	WRITE(6,*) 'Standard deviation in total intensity ',sigma
     	WRITE(6,*) ' '
     	WRITE(6,*) 'Normalized intensity is :',R_TOT/NP
     	WRITE(6,*) 'Standard deviation for normalized intensity',sigma/NP
     	WRITE(6,*) ' '

     	WRITE(6,*) 'Options:'
	WRITE(6,*) 'EXIT the application ................. -1'
     	WRITE(6,*) 'Intensity transmitted/reflected ....... 0'
     	WRITE(6,*) 'Intensity absorbed .................... 1'
     	WRITE(6,*) 'Local reflectivity/transmission ....... 2'
     	kind1	=   IRINT (' Then ? ')
        IF (kind1.EQ.-1) RETURN

     	IF (kind1.NE.2) THEN
	  WRITE(6,*)'Spectrum type :'
	  WRITE(6,*)'  [0]    # photons/sec.'
	  WRITE(6,*)'  [1]    Watt'
     	  KUNIT	=   IRINT ('Then ? ')
     	  KPOW  = IYES ('Normalize to source power [ Y/N ] ? ')
     	  IF (KPOW.EQ.1)  &
      		PWFAC = RNUMBER ('Total source power emitted ? ')
	  RFAC	= PWFAC/NP
     	END IF
     	IF (kind1.NE.0) THEN
     	  FILE2 = RSTRING ('Input I0 file : ' )
     	  !CALL beamLoad (FILE2,RAY2,NCOL2,NP,IFLAG,IERR)
	  !
	  ! it is necessary to allocate ray array here, at the main level. 
	  ! 
          CALL    beamGetDim (file2,ncol2,np,iflag,ierr)
          IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
              print *,'Error: Intens: beamGetDim: Error in file: '//trim(file2)
              return
              !stop
          END IF
  	  ALLOCATE( RAY2(18,NP) )
  	  ray2=0.0d0
	  CALL beamLoad(ray2,ierr,ncol2,np,file2)
     	  !IF (IERR.NE.0) STOP	'Error reading ray file.'
     	END IF
20     	  FILE2 =  RSTRING ('File-name  for [x,y] file ? ')
     	  KPLT  =  IRINT ('Bi [ 0 ] or Tri-dimensional plot [ 1 ] ? ')
     	   IF (KPLT.EQ.1) THEN
     	    KX = IRINT ('Row for plot x-axis ? ')
     	    KY = IRINT ('             y-axis ? ')
	    KSCAX = 0
     	    KSCAY = 0
	     IF (KX.EQ.11) THEN
	       WRITE(6,*) 'Enter :'
	       WRITE(6,*) '	0		cm-1'
	       WRITE(6,*) '	1		eV'
	       WRITE(6,*) '	2		angst'
	       KSCAX = IRINT ('Then ? ')
	     END IF
	     IF (KY.EQ.11) THEN
	       WRITE(6,*) 'Enter :'
	       WRITE(6,*) '	0		cm-1'
	       WRITE(6,*) '	1		eV'
	       WRITE(6,*) '	2		angst'
	       KSCAY = IRINT ('Then ? ')
	     END IF
     	   ELSE
		    KX	=  IRINT ('Row for plot x-axis ? ')
		    KSCAX = 0
		     IF (KX.EQ.11) THEN
		       WRITE(6,*) 'Enter :'
		       WRITE(6,*) '	0		cm-1'
		       WRITE(6,*) '	1		eV'
		       WRITE(6,*) '	2		angst'
		       KSCAX = IRINT ('Then ? ')
		     END IF
		   END IF


		 OPEN (20,FILE=FILE2,STATUS='UNKNOWN',ERR=20)
		 REWIND (20)
		 DO I=1,NP
		   XPL	=    RAY1(KX,I)
		   YPL	=    RAY1(KY,I)
		   ZPL	=    RAY1(7,I)**2 + RAY1(8,I)**2 + RAY1(9,I)**2
		   IF (NCOL1.EQ.18) &
		     ZPL = ZPL + RAY1(16,I)**2 + RAY1(17,I)**2 + RAY1(18,I)**2
		  IF (kind1.EQ.1) THEN
		    ZPL0=    RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2 
		    IF (NCOL2.EQ.18) &
		      ZPL0 = ZPL0 + RAY2(16,I)**2 + RAY2(17,I)**2 +  &
								RAY2(18,I)**2
		    ZPL	=    ZPL0 - ZPL
		  ELSE IF (kind1.EQ.2) THEN
		    ZPL0=    RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2
		    IF (NCOL2.EQ.18) &
		      ZPL0 = ZPL0 + RAY2(16,I)**2 + RAY2(17,I)**2 +  &
								RAY2(18,I)**2
		    ZPL	=    ZPL/ZPL0
		  END IF

		  IF (kind1.NE.2) THEN
		    IF (KUNIT.EQ.1) THEN		! Watt
		      IF (KPOW.EQ.0) THEN
			ZPL = ZPL*TOCM*RAY1(11,I)/TWOPI*1.602D-19
		      ELSE
			ZPL = ZPL*RFAC
		      END IF
		    ELSE				! Photons/sec
		      IF (KPOW.EQ.0) THEN
			ZPL = ZPL
		      ELSE
			ZPL = ZPL*RFAC/(TOCM*RAY1(11,I)/TWOPI*1.602D-19)
		      END IF
		    END IF
		  END IF
	!C
	!C For the 3-D plot case, write out plot files that take
	!C into account whether the user wishes to use all the rays, only the
	!C good rays, or only the lost rays.
	!C
		   IF (KPLT.EQ.1) THEN
		     IF (ILOST.EQ.1) THEN 
			IF (KSCAX.EQ.0.AND.KSCAY.EQ.0)	THEN
				WRITE (20,*)	XPL,YPL,ZPL
			ELSE	IF (KSCAX.EQ.1)		THEN
				WRITE (20,*)    TOCM*XPL/TWOPI,YPL,ZPL
			ELSE	IF (KSCAX.EQ.2)		THEN
				WRITE (20,*)    TWOPI/XPL*1.0D8,YPL,ZPL
			ELSE	IF (KSCAY.EQ.1)		THEN
				WRITE (20,*)    XPL,TOCM*YPL/TWOPI,ZPL
			ELSE	IF (KSCAY.EQ.2)		THEN
				WRITE (20,*)    XPL,TWOPI/YPL*1.0D8,ZPL
			END IF
		     ELSE IF (ILOST.EQ.0.AND.RAY1(10,I).GE.0.0D0) THEN
			IF (KSCAX.EQ.0.AND.KSCAY.EQ.0)	THEN
				WRITE (20,*)	XPL,YPL,ZPL
			ELSE	IF (KSCAX.EQ.1)		THEN
				WRITE (20,*)    TOCM*XPL/TWOPI,YPL,ZPL
			ELSE	IF (KSCAX.EQ.2)		THEN
				WRITE (20,*)    TWOPI/XPL*1.0D8,YPL,ZPL
			ELSE	IF (KSCAY.EQ.1)		THEN
				WRITE (20,*)    XPL,TOCM*YPL/TWOPI,ZPL
			ELSE	IF (KSCAY.EQ.2)		THEN
				WRITE (20,*)    XPL,TWOPI/YPL*1.0D8,ZPL
			END IF
			
		     ELSE IF (ILOST.EQ.2.AND.RAY1(10,I).LT.0.0D0) THEN
			IF (KSCAX.EQ.0.AND.KSCAY.EQ.0)	THEN
					WRITE (20,*)	XPL,YPL,ZPL
			ELSE	IF (KSCAX.EQ.1)		THEN
					WRITE (20,*)    TOCM*XPL/TWOPI,YPL,ZPL
			ELSE	IF (KSCAX.EQ.2)		THEN
				WRITE (20,*)    TWOPI/XPL*1.0D8,YPL,ZPL
			ELSE	IF (KSCAY.EQ.1)		THEN
				WRITE (20,*)    XPL,TOCM*YPL/TWOPI,ZPL
			ELSE	IF (KSCAY.EQ.2)		THEN
				WRITE (20,*)    XPL,TWOPI/YPL*1.0D8,ZPL
			END IF
		     END IF
	!C
	!C For the 2-D plot case too, now write out plot files that take
	!C into account whether the user wishes to use all the rays, only the
	!C good rays, or only the lost rays.
	!C
		   ELSE	IF (KPLT.EQ.0) THEN
		     IF (ILOST.EQ.1) THEN
			IF (KSCAX.EQ.0)	WRITE (20,*)	XPL,ZPL
			IF (KSCAX.EQ.1)	WRITE (20,*)    TOCM*XPL/TWOPI,ZPL
			IF (KSCAX.EQ.2)	WRITE (20,*)    TWOPI/XPL*1.0D8,ZPL
		     ELSE IF (ILOST.EQ.0.AND.RAY1(10,I).GE.0.0D0) THEN
			IF (KSCAX.EQ.0)	WRITE (20,*)	XPL,ZPL
			IF (KSCAX.EQ.1)	WRITE (20,*)    TOCM*XPL/TWOPI,ZPL
			IF (KSCAX.EQ.2)	WRITE (20,*)    TWOPI/XPL*1.0D8,ZPL
	     ELSE IF (ILOST.EQ.2.AND.RAY1(10,I).LT.0.0D0) THEN
	     	IF (KSCAX.EQ.0)	WRITE (20,*)	XPL,ZPL
	    	IF (KSCAX.EQ.1)	WRITE (20,*)    TOCM*XPL/TWOPI,ZPL
	    	IF (KSCAX.EQ.2)	WRITE (20,*)    TWOPI/XPL*1.0D8,ZPL
	     END IF
	   END IF
     	 END DO
     	 CLOSE (20)

        IF (allocated(ray1)) deallocate(ray1)
        IF (allocated(ray2)) deallocate(ray2)
END SUBROUTINE Intens


SUBROUTINE Intens_calc(ray1,np,ncol1,ilost,r_tot,r_tot2)
	implicit none

	real(kind=skr),dimension(:,:), intent(in):: ray1
        integer(kind=ski),intent(in)  :: np,ncol1,ilost
	real(kind=skr), intent(inout):: r_tot,r_tot2

        integer(kind=ski)  :: i
	real(kind=skr)     :: w


! not initialized, to accumulate results if called in a loop
!        R_TOT = 0.0D0
!        R_TOT2 = 0.0D0
         DO I = 1, NP
	    IF (ILOST .EQ. 0) THEN
	       IF (RAY1 (10, I) .LT. 0.0D0) CYCLE
	    ELSE IF (ILOST .EQ. 2) THEN
		IF (RAY1 (10, I) .GT. 0.0D0) CYCLE
	    ENDIF
     	  W = RAY1(7,I)**2 + RAY1(8,I)**2 + RAY1(9,I)**2
	  IF (NCOL1.EQ.18) W = W + RAY1(16,I)**2 + RAY1(17,I)**2 + RAY1(18,I)**2
     	  R_TOT = R_TOT + W
          ! srio@esrf.eu 20110414 store also the squares for error.
     	  R_TOT2 = R_TOT2 + W**2
         END DO
END SUBROUTINE Intens_calc

!
!
!

!C+++
!C
!C	SUBROUTINE	FINDOUT
!C
!C	INPUT	:	KOL	Column to be analyzed
!C			NT	number of points
!C			RAY	Data array
!C
!C	OUTPUT	:	CHI-Square coefficients for that data array,
!C			referred to the origin
!C
!C--
SUBROUTINE FindOut (KOL,NT,A1,A2,A3,A4,A5,A6,RAY,NPOINT)

	implicit none
     	integer(kind=ski) :: kOl,NPOINT,nt,k,i
     	real(kind=skr),dimension(18,NPOINT) :: RAY
     	real(kind=skr)    :: a1,a2,a3,a4,a5,a6,dVector

     	A1	=	0.0D0
     	A2	=	0.0D0
     	A3	=	0.0D0
     	A4	=	0.0D0
     	A5	=	0.0D0
     	A6	=	0.0D0
     	K	=	0
    	  DO 100 I=1,NT
     	  IF (RAY(10,I).LT.0.0) GO TO 100
     	  K	=	K + 1
	  DVECTOR	=	RAY(KOL+3,I)/RAY(5,I)
     	  A1	=   A1 + DVECTOR**2
     	  A2	=   A2 + RAY(KOL,I)*DVECTOR
     	  A3	=   A3 + RAY(KOL,I)**2
     	  A4	=   A4 + RAY(KOL,I)
     	  A6	=   A6 + DVECTOR
100	  CONTINUE
          A1	=   A1/K
          A2	=   A2/K
          A3	=   A3/K
          A4	=   A4/K
          A6	=   A6/K
          A5	=   A6*A4
          A4	=   A4**2
          A6	=   A6**2
      	RETURN
END SUBROUTINE FindOut

!
!
!


!C+++
!C 	Program		F0CUS
!C
!C	21-JAN-85	VRS.	5.3
!C
!C This program locates the waists in the output of SHADOW. This is done
!C by minimizing the variance of the beam respect to the optical axis.
!C
!C If Xi is the coordinate of the i-th ray on the image plane and Vi its
!C direction vector, then the variance will be:
!C
!C		SIG(t)	=  (SUM ( Xi + Vi*t )**2)/NPOINT
!C
!C By expanding the sum, we have
!C		SIG(t)  =  { A1*t**2 + A2*t + A3 } /NPOINT
!C
!C It is then a simple matter to find the minimum, as SIG(t) is just a
!C parabola.
!C
!C An option switch lets the user to choose the origin as center of the
!C distribution, or specify an alternative.
!C---

SUBROUTINE FocNew

        implicit real(kind=skr) (a-h,o-z)
        implicit integer(kind=ski)        (i-n)
	real(kind=skr),dimension(6) :: xmean,stDev,var
	real(kind=skr)              :: xExter, zExter
	real(kind=skr),dimension(:,:),allocatable :: ray
	real(kind=skr),dimension(6)               :: rLow,rUpp,test
	!real(kind=skr),dimension(1000)            :: x,y
	real(kind=skr),dimension(:),allocatable   :: x,y
	character(len=sklen)         :: iFile
	character(len=sklen)   :: filOut
	character(len=2)   :: header
	character(len=60)   :: fileText
	character(len=17)   :: dateText
	character(len=10),dimension(3)   :: nMode
	integer(kind=ski) :: kLost

    	NMODE(1)='Origin'
    	NMODE(2)='Baricenter'
    	NMODE(3)='External'
!C
!C KLOSS is really never used here, but I'm setting it to zero in case
!C the program expects it to be, which it probably does. Only check for
!C good rays as a result -- MK
!C
	KLOSS = 0

102    	IFILE	=   RSTRING ('Input file ? ')
	filetext = ' '
	datetext = ' '

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    beamGetDim (iFile,ncol,nn,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Error: FocNew: beamGetDim: Error in file: '//trim(iFile)
            return
            ! stop
        END IF

  	ALLOCATE( RAY(18,nn) )
  	ray=0.0d0

	CALL beamLoad(ray,ierr,ncol,nn,iFile)

     	!CALL	RBEAM (IFILE,RAY,PHASE,AP,NCOL,NN,IFLAG,IERR)
     	! IF (IERR.NE.0) STOP	'Error reading ray file.'
     	IF (IERR.NE.0) print *,'Error reading ray file.'

     	KK	=  0
     	DO 99 I=1,NN
     	IF (RAY(10,I).LT.0.0) 	KK = KK + 1
99     	CONTINUE

200	CONTINUE
     	NN = I-1
     	KK = NN - KK
     	WRITE(6,*)'We have ',KK,' good points out of ',NN
!C
!C Computes the center of gravity of the ray
!C
     	RLOW(1) =   1.0D+20
     	RUPP(1) = - 1.0D+20
     	RLOW(2) =   1.0D+20
     	RUPP(2) = - 1.0D+20
     	RLOW(3) =   1.0D+20
     	RUPP(3) = - 1.0D+20

     	RLOW(4) =   1.0D+20
     	RUPP(4) = - 1.0D+20
     	RLOW(5) =   1.0D+20
     	RUPP(5) = - 1.0D+20
     	RLOW(6) =   1.0D+20
     	RUPP(6) = - 1.0D+20
     	DO 300 I=1,NN
	 IF (KLOSS.EQ.0) THEN
	   IF (RAY(10,I).LT.0.0D0)   GO TO 300
	 ELSE IF (KLOSS.EQ.1) THEN
     	   IF (RAY(10,I).GE.0.0D0)   GO TO 300
     	 ELSE
     	 END IF
     	TEST(1)	=   RAY(1,I)
     	TEST(2)	=   RAY(2,I)
     	TEST(3)	=   RAY(3,I)
     	TEST(4)	=   RAY(4,I)
     	TEST(5)	=   RAY(5,I)
     	TEST(6)	=   RAY(6,I)
     	RLOW(1)= MIN(RLOW(1),TEST(1))
     	RUPP(1)= MAX(RUPP(1),TEST(1))
     	RLOW(2)= MIN(RLOW(2),TEST(2))
     	RUPP(2)= MAX(RUPP(2),TEST(2))
     	RLOW(3)= MIN(RLOW(3),TEST(3))
        RUPP(3)= MAX(RUPP(3),TEST(3))
     	RLOW(4)= MIN(RLOW(4),TEST(4))
     	RUPP(4)= MAX(RUPP(4),TEST(4))
     	RLOW(5)= MIN(RLOW(5),TEST(5))
     	RUPP(5)= MAX(RUPP(5),TEST(5))
     	RLOW(6)= MIN(RLOW(6),TEST(6))
300     RUPP(6)= MAX(RUPP(6),TEST(6))
!C
!C computes centers and variance
!C
     	NPOINT = NN
     	DO 11 JCOL=1,6
     	  XMEAN(JCOL)	= 0.0D0
     	  VAR(JCOL)	= 0.0D0
     	 DO 12 I=1,NPOINT
     	   XMEAN(JCOL) 	= XMEAN(JCOL) + RAY(JCOL,I)
     	   VAR(JCOL)	= VAR(JCOL) + RAY(JCOL,I)**2
12     	 CONTINUE
     	  XMEAN(JCOL) = XMEAN(JCOL)/NPOINT
     	  VAR(JCOL)   = VAR(JCOL)/NPOINT - XMEAN(JCOL)**2
     	 IF (VAR(JCOL).GE.0.0) THEN
     	   STDEV(JCOL) = SQRT(VAR(JCOL))
     	 ELSE
     	   STDEV(JCOL) = 0.0D0
     	 END IF
11     	CONTINUE
     	WRITE (6,*)
     	WRITE (6,2001)
     	WRITE (6,*)
     	WRITE (6,2010) 1,'X ',RLOW(1),RUPP(1),XMEAN(1),STDEV(1)
     	WRITE (6,2010) 2,'Y ',RLOW(2),RUPP(2),XMEAN(2),STDEV(2)
     	WRITE (6,2010) 3,'Z ',RLOW(3),RUPP(3),XMEAN(3),STDEV(3)
     	WRITE (6,2010) 4,'X''',RLOW(4),RUPP(4),XMEAN(4),STDEV(4)
     	WRITE (6,2010) 5,'Y''',RLOW(5),RUPP(5),XMEAN(5),STDEV(5)
     	WRITE (6,2010) 6,'Z''',RLOW(6),RUPP(6),XMEAN(6),STDEV(6)

2001	FORMAT (//,T2,'Row',T5,' Par',T10,'Minimum:',T25,'Maximum:', &
                T40,'Center:',T55,'St. Dev.:')
2010	FORMAT (1X,T2,I2,T5,A3,T10,G12.5,T25,G12.5,T40,G12.5,T55,G12.5)
     	WRITE(6,*)'Options: '
     	WRITE(6,*)'center at origin              [ 0 ] '
     	WRITE(6,*)'center at center of gravity   [ 1 ] '
     	WRITE(6,*)'external                      [ 2 ] '
     	ICENTER = IRINT ('Then ?')
     	XCENTER = 0.0D0
     	ZCENTER = 0.0D0
     	IF (ICENTER.EQ.2) THEN
     	  XEXTER = RNUMBER ('X-center : ')
     	  ZEXTER = RNUMBER ('Z-center : ')
     	 DO 13 JJ=1,NN
     	   RAY(1,JJ) = RAY(1,JJ) - XEXTER
     	   RAY(3,JJ) = RAY(3,JJ) - ZEXTER
13     	 CONTINUE
     	END IF
        iTmp=3
     	CALL FINDOUT (iTmp,NN,AZ1,AZ2,AZ3,AZ4,AZ5,AZ6,RAY,nn)
	ZBAR	=  AZ4
	VZBAR	=  AZ6
	IF (ICENTER.NE.1) THEN
	  AZ4	=  0
	  AZ5	=  0
	  AZ6	=  0
	END IF
	IF (ABS(AZ1-AZ6).GT.1.0E-30) THEN
     	  TPARZ	=  (AZ5 - AZ2) / (AZ1 - AZ6)
	ELSE
	  TPARZ	=  0.0D0
	END IF
        iTmp=1
     	CALL FINDOUT (iTmp,NN,AX1,AX2,AX3,AX4,AX5,AX6,RAY,nn)
	XBAR	=  AX4
	VXBAR	=  AX6
	TBAR	=  ZBAR + XBAR
	VTBAR	=  VZBAR + VXBAR
	IF (ICENTER.NE.1) THEN
	  AX4	=  0
	  AX5	=  0
	  AX6	=  0
	END IF
	IF (ABS(AX1-AX6).GT.1.0E-30) THEN
     	  TPARX	=  (AX5 - AX2) / (AX1 - AX6)
	ELSE
	  TPARX =   0.0D0
	END IF
     	AT1	=   AX1 + AZ1
     	AT2	=   AX2 + AZ2
     	AT3	=   AX3 + AZ3
     	AT4	=   AX4 + AZ4
     	AT5	=   AX5 + AZ5
     	AT6	=   AX6 + AZ6
	IF (ABS(AT1-AT6).GT.1.0E-30) THEN
     	  TPART	=  (AT5 - AT2) / (AT1 - AT6) 
	ELSE
	  TPART =   0.0D0
	END IF
	SIGX	=   DSQRT(DABS( AX1*TPARX**2 + 2.0D0*AX2*TPARX + AX3  &
      			- ( AX4 + 2.0D0*AX5*TPARX + AX6*TPARX**2)))
	SIGZ	=   DSQRT(DABS( AZ1*TPARZ**2 + 2.0D0*AZ2*TPARZ + AZ3  &
      			- ( AZ4 + 2.0D0*AZ5*TPARZ + AZ6*TPARZ**2)))
	SIGT	=   DSQRT(DABS( AT1*TPART**2 + 2.0D0*AT2*TPART + AT3  &
      			- ( AT4 + 2.0D0*AT5*TPART + AT6*TPART**2)))
     	SIGX0	=   DSQRT(DABS(AX3 - AX4))
     	SIGZ0	=   DSQRT(DABS(AZ3 - AZ4))
     	SIGT0	=   DSQRT(DABS(AT3 - AT4))
     	WRITE (6,*)' '
	WRITE (6,1035)
	WRITE (6,*) 'Searching file : '//trim(IFILE)
	WRITE (6,1055) FILETEXT,DATETEXT
	WRITE (6,1035)
     	WRITE (6,*) 'Center at :', NMODE(ICENTER+1)
     	WRITE (6,*) 'X = ',XEXTER,'    Z = ',ZEXTER
     	WRITE (6,1035)
     	WRITE (6,*) '.............   S A G I T T A L   ............'
     	WRITE (6,1000) AX1,AX2,AX3
     	WRITE (6,1012) SQRT(ABS(XBAR)),SQRT(ABS(VXBAR))
     	WRITE (6,1010)	TPARX
	WRITE (6,1015)	SIGX
     	WRITE (6,1060)  SIGX0
     	WRITE (6,*) '.............  T A N G E N T I A L  .............'
     	WRITE (6,1020) AZ1,AZ2,AZ3
     	WRITE (6,1012) SQRT(ABS(ZBAR)),SQRT(ABS(VZBAR))
     	WRITE (6,1030)	TPARZ
	WRITE (6,1015)	SIGZ
     	WRITE (6,1060)	SIGZ0
     	WRITE (6,*) '.............  L E A S T  C O N F U S I O N  ...............'
     	WRITE (6,1040) AT1,AT2,AT3
     	WRITE (6,1012) SQRT(ABS(TBAR)),SQRT(ABS(VTBAR))
     	WRITE (6,1050) TPART
	WRITE (6,1015)	SIGT
     	WRITE (6,1060)	SIGT0
     	WRITE(6,*)'All done. File out data.'
     	WRITE(6,*)' '

     	OPEN (23,FILE='focnew.txt',STATUS='UNKNOWN')
	REWIND (23)
	WRITE (23,1035)
	WRITE (23,*) 'Searching file : '//trim(IFILE)
	WRITE (23,1055) FILETEXT,DATETEXT
	WRITE (23,1035)
     	WRITE (23,*) 'Center at :', NMODE(ICENTER+1)
     	WRITE (23,*) 'X = ',XEXTER,'    Z = ',ZEXTER
	WRITE (23,1035)
     	WRITE (23,1070) KK,NN
     	WRITE (23,*) '.............. S A G I T T A L  ..............'
     	WRITE (23,1000) AX1,AX2,AX3
     	WRITE (23,1012) SQRT(ABS(XBAR)),SQRT(ABS(VXBAR))
     	WRITE (23,1010)	TPARX
	WRITE (23,1015) SIGX
     	WRITE (23,1060)	SIGX0
     	WRITE (23,*) '.............. T A N G E N T I A L .............'
     	WRITE (23,1020) AZ1,AZ2,AZ3
     	WRITE (23,1012) SQRT(ABS(ZBAR)),SQRT(ABS(VZBAR))
     	WRITE (23,1030)	TPARZ
	WRITE (23,1015) SIGZ
     	WRITE (23,1060)	SIGZ0
     	WRITE (23,*) '..............   L E A S T  C O N F U S I O N  .............'
     	WRITE (23,1040) AT1,AT2,AT3
     	WRITE (23,1012) SQRT(ABS(TBAR)),SQRT(ABS(VTBAR))
     	WRITE (23,1050) TPART
	WRITE (23,1015) SIGT
     	WRITE (23,1060)	SIGT0
     	CLOSE (23)
	print *,'File written to disk: focnew.txt'

1000	FORMAT	(1X,'X coefficients : ',3(1X,G17.10))
1012	FORMAT  (1X,'Center :',G17.10,T30,'Average versor :',G17.10)
1010	FORMAT (1X,'Sagittal   focus at       : ',G17.10)
1015	FORMAT (1X,'Waist size at best focus (rms)	: ',G17.10)
1060	FORMAT (1X,'Waist size at origin                : ',G17.10)
1020	FORMAT	(1X,'Z coefficients : ',3(1x,G17.10))
1030	FORMAT (1X,'Tangential focus at       : ',G17.10)
1040	FORMAT	(1X,'T coefficients : ',3(1x,G17.10))
1050	FORMAT (1X,'Circle of least confusion : ',G17.10)
1035	FORMAT (1X,'-----------------------------------------------------------------------------')
1055	FORMAT (1X,A60,T62,A17)
1070	FORMAT (1X,'Working with ',I4,' "good" rays out of ',I4)
     	
     	IANSW = IYES ('Do you want a plottable file ? ')
101	CONTINUE
     	IF (IANSW.EQ.1) THEN
     	WRITE(6,*)'Enter :'
     	WRITE(6,*)'1	for X'
     	WRITE(6,*)'2	for Z'
     	WRITE(6,*)'3	for T'
     	WRITE(6,*)'4	for all'
     	IPLOT = IRINT (' Then ? ')
     	WRITE(6,*)'Please specify Limits [ YMIN, YMAX, STEP ] ?'
     	YMIN	= RNUMBER ('Ymin ? ')
     	YMAX 	= RNUMBER ('Ymax ? ')
     	YSTEP 	= RNUMBER ('Ystep ? ')
     	NPL	=   (YMAX-YMIN)/YSTEP + 1
     	IF (IPLOT.NE.4) THEN
     	 IOLD	=  0
         KOUNT = IPLOT
     	ELSE
     	 IOLD	=  4
     	 IPLOT	=  1
     	 KOUNT	=  1
     	END IF


  	ALLOCATE( X(NPL) )
  	X=0.0d0
  	ALLOCATE( Y(NPL) )
  	Y=0.0d0

105	CONTINUE
        DO 16 I=1,NPL
     	 X(I) = YMIN + (I-1)*YSTEP
     	 IF (IPLOT.EQ.1) THEN
     	  Y(I) = SQRT(ABS( AX1*X(I)**2 + 2.0D0*AX2*X(I) + AX3  &
      			-(AX4 + 2.0D0*AX5*X(I) + AX6*X(I)**2)))
     	 ELSE IF (IPLOT.EQ.2) THEN
     	  Y(I) = SQRT(ABS( AZ1*X(I)**2 + 2.0D0*AZ2*X(I) + AZ3 &
      			-(AZ4 + 2.0D0*AZ5*X(I) + AZ6*X(I)**2)))
     	 ELSE IF (IPLOT.EQ.3) THEN
     	  Y(I) = SQRT(ABS( AT1*X(I)**2 + 2.0D0*AT2*X(I) + AT3 &
      			-(AT4 + 2.0D0*AT5*X(I) + AT6*X(I)**2))) 
     	 END IF
16     	CONTINUE
     	!IF (IOLD.EQ.4) THEN
     	 !IF (KOUNT.EQ.1)	FILOUT	= 'FX'//ROOT
     	 !IF (KOUNT.EQ.2)	FILOUT	= 'FZ'//ROOT
     	 !IF (KOUNT.EQ.3) 	FILOUT	= 'FT'//ROOT
     	 IF (KOUNT.EQ.1)	FILOUT	= 'focnew_X.dat'
     	 IF (KOUNT.EQ.2)	FILOUT	= 'focnew_Z.dat'
     	 IF (KOUNT.EQ.3) 	FILOUT	= 'focnew_T.dat'
     	 OPEN (23,FILE=FILOUT,STATUS='UNKNOWN')
	 REWIND (23)
     	!ELSE
     	! OPEN (23,FILE=IFILE,STATUS='UNKNOWN')
	! REWIND (23)
     	!END IF
     	DO 14 I=1,NPL
     	WRITE (23,*)	X(I),Y(I)
14     	CONTINUE
     	CLOSE (23)
     	!IF (IOLD.EQ.4) THEN
	  print *,'File written to disk: '//trim(FilOut)
        !ELSE
	!  print *,'File written to disk: '//trim(iFile)
        !END IF
     	IF (IOLD.EQ.4.AND.KOUNT.LT.3) THEN
     	  KOUNT = KOUNT + 1
     	  IPLOT = IPLOT + 1
     	  GO TO 105
     	END IF
     	END IF

!write gnuplot files
        IF (IANSW == 1) THEN
        OPEN (35,FILE='focnew.gpl',STATUS='UNKNOWN')
           write(35,'(A)') '# '
           write(35,'(A)') '# Gnuplot script for shadow3 '
           write(35,'(A)') '# Created by focnew '
           write(35,'(A)') '# '

           IF (OS==1) THEN
             write(35,'(A)') 'set terminal win  '
           ELSE
             write(35,'(A)') 'set terminal x11  '
           END IF


           write(35,'(A)') 'set style line 1 lt 1 lw 3 pt 3 linecolor rgb "red"'
           write(35,'(A)') 'set style line 2 lt 1 lw 3 pt 3 linecolor rgb "blue"'
           write(35,'(A)') 'set style line 3 lt 1 lw 3 pt 3 linecolor rgb "black"'

           IF (IOLD == 4) THEN
             write(35,'(A)') "plot 'focnew_X.dat' using 1:2 with line ls 1,\"
             write(35,'(A)') "     'focnew_Z.dat' using 1:2 with line ls 2,\"
             write(35,'(A)') "     'focnew_T.dat' using 1:2 with line ls 3"
           ELSE 
             write(35,'(A)') "plot '"//trim(filout)//"' using 1:2 with line ls 1"
           ENDIF
  
           write(35,'(A)') '  '
           write(35,'(A)') "pause -1 'Press <Enter> to end graphic '"
        CLOSE (35)
        print *,'File writen to disk: focnew.gpl'
        END IF

	!KGO = IYES('Another plot? ')
	!IF (KGO == 1) GO TO 102
        
        IF (allocated(ray)) deallocate(ray)
        IF (allocated(X)) deallocate(X)
        IF (allocated(Y)) deallocate(Y)
	RETURN
END SUBROUTINE FocNew


!***********************************************************************
!* This program will plot the whole optical system, in a suitable scale*
!* The inputs consist of a source starting point, in the standard format
!* of a BEGIN.DAT or STAR.xx file. Also, OPTAXIS.xx is used to deter.*
!* the physical location of the elements in the lab. reference frame.  *
!* 
!* Adapted to shadow3 srio@esrf.eu 2011-11-24. Output to gnuplot.
!* 
!***********************************************************************

subroutine sysplot

  implicit none 

  real(kind=skr),dimension(20,24) :: AXIS
  real(kind=skr),dimension(3)     :: UVEC,VVEC,WVEC

  !DATA (12,N_DIM),PHASE(3,N_DIM),AP(3,N_DIM)
  real(kind=skr),dimension(:,:),allocatable :: data1

  real(kind=skr)       :: VERSOR(20,3,3),RAY(20,500,4),OLD (20,500,4)
  character(len=sklen) :: FILESOUR,FILEIMAGE,FILEAXIS,TEMP
  character(len=sklen),dimension(25)  :: FILES

  integer(kind=ski) :: imode, kdim, nmirr, npoint, i, j
  integer(kind=ski) :: iflag, kx, ky, kz=0, ifl, ierr, np
  integer(kind=ski) :: i1, icheck, itest, last, ncol
  real(kind=skr)    :: xp,xpp,yp,ypp,zp

!
! input mode
!
             WRITE(6,*)'Input from : '
             WRITE(6,*)'     0 File: sysplot.inp'
             WRITE(6,*)'     1 Terminal'
             !READ(5,*)IMODE
             IMODE = irint(' <?> ')

!
! terminal mode
!
IF (IMODE.EQ.1) THEN
             WRITE(6,*)'Bidimensional [ 0 ] or 3d plot [ 1 **not yet working** ] ?'
             READ(5,*)KDIM
             WRITE(6,*)'How many optical elements ?'
             READ(5,*)NMIRR
             WRITE(6,*) 'How many rays [ suggest no more than 200 (500 max) ] ?'
             READ(5,*)  NPOINT
             WRITE(6,*)'Optaxis file ?'
             READ (5,'(a)') FILEAXIS
             WRITE(6,*)'Source file ?'
             READ (5,'(a)') FILESOUR
             WRITE(6,*)'Image (final) file ?'
             READ (5,'(a)') FILEIMAGE
             DO I=1,NMIRR
               WRITE(6,*)'Optical element N. ',I,' ?'
               READ (5,'(a)')        FILES(I)
             END DO
             IFLAG        =  IYES ('Plot all the losses too [ Y/N ] ? ' )
             IF (KDIM.EQ.0) THEN
               WRITE(6,*)'Columns to plot out (first is horizontal) ?'
               READ(5,*)KX,KY
             ELSE
               KX        =  1
               KY        =  2
               KZ        =  3
             END IF
ELSE
             OPEN        (20, FILE='sysplot.inp', STATUS='OLD')
             READ (20,*) KDIM
             READ (20,*) NMIRR
             READ (20,*) NPOINT,IFLAG
             READ (20,'(a)') FILEAXIS
!C  DELETE 1 LEADING BLANK SPACE FROM FILENAME
         TEMP = FILEAXIS
         FILEAXIS = TEMP(2: )
         READ (20,'(a)') FILESOUR
         TEMP = FILESOUR
         FILESOUR = TEMP(2: )
         READ (20,'(a)') FILEIMAGE
         TEMP = FILEIMAGE
         FILEIMAGE = TEMP(2: )
         DO I=1,NMIRR
            READ (20,'(a)') FILES(I)
            TEMP = FILES(I)
            FILES(I) = TEMP(2: )
         END DO
         IF (KDIM.EQ.1) THEN
                READ (20,*) KX,KY,KZ
         ELSE
                READ (20,*) KX,KY
         END IF
         CLOSE (20)
END IF

IF (NPOINT.GT.500) NPOINT = 500
         OPEN (20,FILE=FILEAXIS,STATUS='OLD')
         DO I=1,NMIRR
             READ (20,*)        I1
             READ (20,*)        (AXIS (I,J),J=1,3)
             READ (20,*)        (AXIS (I,J),J=4,6)
             READ (20,*)        (AXIS (I,J),J=7,9)
             READ (20,*)        (AXIS (I,J),J=10,12)
5            READ (20,*)        (AXIS (I,J),J=13,15)
             READ (20,*)        (AXIS (I,J),J=16,18)
             READ (20,*)        (AXIS (I,J),J=19,21)
             READ (20,*)        (AXIS (I,J),J=22,24)
         END DO
         CLOSE (20)

      !
      ! it is necessary to allocate ray array here, at the main level. 
      ! 
        CALL    beamGetDim (filesour,ncol,np,ifl,ierr)
        IF ((ifl.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Error: Sysplot: beamGetDim: Error in file: '//trim(filesour)
            return
            ! stop
        END IF

        ALLOCATE( data1(18,NP) )
        data1=0.0d0

      CALL beamLoad(data1,ierr,ncol,np,filesour)

       DO I=1,NPOINT
             OLD (1,I,1)        =   data1 (1,I)
             OLD (1,I,2)        =   data1 (2,I)
             OLD (1,I,3)        =   data1 (3,I)
             OLD (1,I,4)        =   1.0D0
       END DO

        data1=0.0d0
        CALL beamLoad(data1,ierr,ncol,np,fileimage)
        IF (ierr.ne.0) THEN
            print *,'Sysplot: beamLoad: Error in file: '//trim(fileimage)
            return
        END IF

        LAST        =   NMIRR + 2

        DO I=1,NPOINT
             OLD (LAST,I,1)        =   data1 (1,I)
             OLD (LAST,I,2)        =   data1 (2,I)
             OLD (LAST,I,3)        =   data1 (3,I)
             IF (IFLAG.EQ.0) THEN
               OLD (LAST,I,4)        =   data1 (10,I)
             ELSE
              IF (data1(10,I).GT.-1000000) THEN
               OLD (LAST,I,4)        =   1
              END IF
             END IF
        END DO

       DO J=1,NMIRR
        data1=0.0d0
        CALL beamLoad(data1,ierr,ncol,np,files(j))
        IF (IERR.NE.0) THEN 
            print *,'Error: Sysplot: beamLoad: Error in reading intermediate ray file.'
            return
            ! stop
        END IF

        DO I=1,NPOINT
             OLD (J+1,I,1)        =   data1 (1,I)
             OLD (J+1,I,2)        =   data1 (2,I)
             OLD (J+1,I,3)        =   data1 (3,I)
             IF (IFLAG.EQ.0) THEN
               OLD (J+1,I,4)        =   data1 (10,I)
             ELSE
              IF (data1(10,I).GT.-1000000) THEN
               OLD (J+1,I,4)  =  1
              END IF
             END IF
        END DO 
       END DO 

!** Computes now the set of versor to describe the ray in real space

      DO I=1,NMIRR

             VERSOR (I,1,1)        =   AXIS (I,10)        ! UVECT
             VERSOR (I,1,2)        =   AXIS (I,11)
             VERSOR (I,1,3)        =   AXIS (I,12)
             VERSOR (I,2,1)        =   AXIS (I,13)        ! VVEC
             VERSOR (I,2,2)        =   AXIS (I,14)
             VERSOR (I,2,3)        =   AXIS (I,15)
             VERSOR (I,3,1)        =   AXIS (I,16)        ! WVEC
             VERSOR (I,3,2)        =   AXIS (I,17)
             VERSOR (I,3,3)        =   AXIS (I,18)
      END DO 

!** Generates now the set of plotting points, from the source to each
!** mirror.

      DO I=1,NPOINT
             RAY (1,I,1)        =   OLD (1,I,1)
             RAY (1,I,2)        =   OLD (1,I,2)
             RAY (1,I,3)        =   OLD (1,I,3)
             RAY (1,I,4)        =   OLD (1,I,4)
        DO J=1,NMIRR
             ITEST        =   -11000*J
             RAY (J+1,I,1)        =   AXIS (J,4) + & 
                      OLD (J+1,I,1)*VERSOR (J,1,1) +  & 
                      OLD (J+1,I,2)*VERSOR (J,2,1) + & 
                      OLD (J+1,I,3)*VERSOR (J,3,1)

             RAY (J+1,I,2)        =   AXIS (J,5) + & 
                      OLD (J+1,I,1)*VERSOR (J,1,2) + & 
                      OLD (J+1,I,2)*VERSOR (J,2,2) + & 
                      OLD (J+1,I,3)*VERSOR (J,3,2)

             RAY (J+1,I,3)        =   AXIS (J,6) + & 
                      OLD (J+1,I,1)*VERSOR (J,1,3) + & 
                      OLD (J+1,I,2)*VERSOR (J,2,3) + & 
                      OLD (J+1,I,3)*VERSOR (J,3,3)

             ICHECK        =   OLD(J+1,I,4)
              IF (IFLAG.EQ.0) THEN
                IF (ICHECK.GE.0) THEN
                  RAY (J+1,I,4)        =  1.0D0
                ELSE IF (ICHECK.LT.-1000000) THEN
                 IF (RAY (J,I,4).GT.0.0) THEN
                   RAY (J+1,I,4)        = - 1.0D0
                 ELSE
                   RAY (J+1,I,4)        = - 2.0D0
                 END IF
                ELSE IF (ITEST.EQ.ICHECK) THEN
                   RAY (J+1,I,4)        = - 1.0D0
                ELSE IF (ITEST.GT.ICHECK.AND.ITEST.LT.0) THEN
                   RAY (J+1,I,4)        = -2.0D0
                END IF
              ELSE
                IF (ICHECK.GT.-1000000) THEN
                  RAY (J+1,I,4)        =  1.0D0
                ELSE
                  RAY(J+1,I,4) = -1.0D0
                END IF
              END IF
        END DO !J
      END DO !I

!** Computes the last set of rays, to the final image point.
             UVEC (1)        =   AXIS (NMIRR,10)        ! UVEC >> X
             UVEC (2)        =   AXIS (NMIRR,11)
             UVEC (3)        =   AXIS (NMIRR,12)
             VVEC (1)        =   AXIS (NMIRR,19)        ! VREF >> Y
             VVEC (2)        =   AXIS (NMIRR,20)
             VVEC (3)        =   AXIS (NMIRR,21)
             WVEC (1)        =   AXIS (NMIRR,22)        ! WREF >> Z
             WVEC (2)        =   AXIS (NMIRR,23)
             WVEC (3)        =   AXIS (NMIRR,24)

     DO I=1,NPOINT
             RAY (LAST,I,1)        =   AXIS (NMIRR,7) + &
                      OLD (LAST,I,1)*UVEC(1) + &
                      OLD (LAST,I,2)*VVEC(1) + &
                      OLD (LAST,I,3)*WVEC(1)
             RAY (LAST,I,2)        =   AXIS (NMIRR,8) + &
                      OLD (LAST,I,1)*UVEC(2) + &
                      OLD (LAST,I,2)*VVEC(2) + &
                      OLD (LAST,I,3)*WVEC(2) 
             RAY (LAST,I,3)        =   AXIS (NMIRR,9) + &
                      OLD (LAST,I,1)*UVEC(3) + &
                      OLD (LAST,I,2)*VVEC(3) + &
                      OLD (LAST,I,3)*WVEC(3)
             RAY (LAST,I,4)        =   OLD (LAST,I,4)
     END DO 

!** The array is now ready for plotting.
        OPEN (34,FILE='sysplot.dat',STATUS='UNKNOWN')
        REWIND (34)
        DO I=1,NPOINT
          DO J=1,NMIRR + 2
             XP        =   RAY (J,I,KX)
             YP        =   RAY (J,I,KY)
             ZP        =   RAY (J,I,KZ)
             IF (ABS(XP).LT.1.0E-10)        XP        =  0.0D0
             IF (ABS(YP).LT.1.0E-10)        YP        =  0.0D0
             IF (ABS(ZP).LT.1.0E-10)        ZP        =  0.0D0
             IF (KDIM.EQ.0) THEN
              IF (RAY (J,I,4).GE.0.0)  THEN
                IF (J.EQ.1) THEN
                  !WRITE (34,*) 'line(',XP,',',YP,',',XP,',',YP,')'
                  WRITE (34,*) XP,YP,XP,YP
                  XPP = XP
                  YPP = YP
                ELSE
                  !WRITE (34,*) 'line(',XPP,',',YPP,',',XP,',',YP,')'
                  WRITE (34,*) XPP,YPP,XP,YP
                  XPP = XP
                  YPP = YP
                ENDIF
              ELSE 
                 GO TO 400
              END IF
             ELSE
              IF (RAY (J,I,4).GE.0.0)  THEN
                 WRITE (34,*)        XP,YP,ZP
              ELSE
                 GO TO 400
              END IF
             END IF
          END DO !J
          WRITE (34,*) ' '
400     CONTINUE
        END DO !I
        CLOSE (34)
        print *,'File writen to disk: sysplot.dat'


        OPEN (35,FILE='sysplot.gpl',STATUS='UNKNOWN')
           write(35,1000) '# '
           write(35,1000) '# Gnuplot script for shadow3 '
           write(35,1000) '# Created by histo1 '
           write(35,1000) '# '

           IF (OS==1) THEN
             write(35,1000) 'set terminal win  '
           ELSE
             write(35,1000) 'set terminal x11  '
           END IF
           !write(35,1000) 'set terminal x11  '
           write(35,1000) 'set pointsize 3.0 '
           write(35,1000) 'plot "sysplot.dat" using 3:4 with lines notitle linecolor rgb "green" '
           write(35,1000) '  '
           write(35,1000) "pause -1 'Press <Enter> to end graphic ' "
        CLOSE (35)
        print *,'File writen to disk: sysplot.gpl'


        IF (IMODE.EQ.1) THEN
          OPEN        (20, FILE='sysplot.inp', STATUS='UNKNOWN')
          REWIND  (20)
              WRITE (20,*) KDIM
              WRITE (20,*) NMIRR
              WRITE (20,*) NPOINT,IFLAG
              WRITE (20,1010) trim(FILEAXIS)
              WRITE (20,1010) trim(FILESOUR)
              WRITE (20,1010) trim(FILEIMAGE)
              DO I=1,NMIRR
                 WRITE (20,1010) trim(FILES(I))
              END DO 
              IF (KDIM.EQ.1) THEN
                WRITE (20,*) KX,KY,KZ
              ELSE
                WRITE (20,*) KX,KY
              END IF
             CLOSE (20)
             print *,'File writen to disk: sysplot.inp'
        END IF

        IF (allocated(data1)) deallocate(data1)
        return

1000        FORMAT (A)
1010        FORMAT (1X,A)
END Subroutine sysplot

!
! SHROT: rotates the beam an angle theta around the axis X,Y or Z
!
! inputs: 
!        ray: the beam array
!        np: number of points in ray
!        theta: the rotation angle in rad
!        axis: 1,2 or 3 for rotation around X,Y or Z, respectively
! outputs: 
!        ray (overwritten): the array with the rotated beam
!
!

subroutine shrot(ray,np,theta,axis)
    !use shadow_globaldefinitions
      real(kind=skr), dimension(18,np), intent(inout) :: ray
      integer(kind=ski), intent(in)                   :: np,axis
      real(kind=skr), intent(in)                      :: theta
      
      real(kind=skr), dimension(np) :: x,y,xx,yy
      real(kind=skr)                :: sinth,costh
      integer(kind=ski),dimension(4):: tstart
      integer(kind=ski)             :: i1,i2,j

      tstart = (/ 1,4,7,16 /)
      costh =   cos(theta)
      sinth =   sin(theta)
         
      DO j=1,4
        select case (axis)
        case (1)  ! rotation around X
          i1 = tstart(j)+1
          i2 = tstart(j)+2
        case (2)  ! rotation around Y
          i1 = tstart(j)-1
          i2 = tstart(j)+1
        case (3)  ! rotation around Z
          i1 = tstart(j)-2
          i2 = tstart(j)-1
        case default
          print *,'Error: SHROT: invalid rotation axis: ',axis
          return
          ! stop
        end select 

        x = ray(i1,:)
        y = ray(i2,:)
        xx = 0.0
        yy = 0.0
  
        xx =  x*costh+y*sinth
        yy = -x*sinth+y*costh
  
        ray(i1,:) = xx
        ray(i2,:) = yy
      END DO
      
      return      
end subroutine shrot

!
! SHTRANSLATION: translates the beam by a vector
!
! inputs: 
!        ray: the beam array
!        np: number of points in ray
!        translation: the translation vector
! outputs: 
!        ray (overwritten): the array with the rotated beam
!
!
subroutine shtranslation(ray,np,translation)
    !use shadow_globaldefinitions
      real(kind=skr), dimension(18,np), intent(inout) :: ray
      integer(kind=ski), intent(in)                   :: np
      real(kind=skr),dimension(3), intent(in)      :: translation
      
      ray(1,:) = ray(1,:)+translation(1)
      ray(2,:) = ray(1,:)+translation(2)
      ray(3,:) = ray(1,:)+translation(3)
      return      
end subroutine shtranslation

!C+++
!C    PROGRAM        MINMAX
!C
!C    purpose        To locate maxima and minima in our ray arrays.
!C
!C---
   subroutine minmax
    implicit none

    character(len=sklen) :: fileIn
    real(kind=skr),dimension(:,:),allocatable :: ray
    integer(kind=ski) :: ncol,npoint,iflag,ierr
    integer(kind=ski) :: kloss,kount,i
    real(kind=skr),dimension(6) :: test
    real(kind=skr) :: check
    real(kind=skr) :: xmin, xmax, ymin, ymax, zmin, zmax
    real(kind=skr) :: x1min, x1max, y1min, y1max, z1min, z1max

               
    fileIn =  RSTRING('MINMAX> Beam input file ? ')

    ! 
    ! read input file 
    !
    CALL    beamGetDim (fileIn,ncol,npoint,iflag,ierr)
    IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
      print *,'MinMax: beamGetDim: Error in file: '//trim(fileIn)
      return
    END IF

    ALLOCATE( RAY(18,npoint) )
    ray=0.0d0
    
    CALL beamLoad(ray,ierr,ncol,npoint,fileIn)
    
    WRITE(6,*)'Options --- Enter'
    WRITE(6,*)'0 for excluding the losses'
    WRITE(6,*)'1 for including losses at a particular O.E.'
    WRITE(6,*)'2 for including all the losses.'
    KLOSS    =   IRINT ('Then ? ')
    IF (KLOSS.EQ.1) THEN
           WRITE(6,*) 'Flag value ( -11000 first O.E., -22000 sec O.E., etc.) ?'
           CHECK    =   RNUMBER ('Then ? ')
    ELSE
    END IF
    KOUNT    =   0
    DO I=1,NPOINT
          IF (RAY(10,I).LT.0.0) KOUNT = KOUNT + 1
    END DO
100 CONTINUE
    WRITE(6,*)'***********'
    WRITE(6,*)'File examined is :'//trim(fileIn)
    WRITE(6,*)'Found ',(NPOINT-KOUNT),' good points out of',NPOINT
    XMIN =   1.0D+20
    XMAX = - 1.0D+20
    YMIN =   1.0D+20
    YMAX = - 1.0D+20
    ZMIN =   1.0D+20
    ZMAX = - 1.0D+20

    X1MIN =   1.0D+20
    X1MAX = - 1.0D+20
    Y1MIN =   1.0D+20
    Y1MAX = - 1.0D+20
    Z1MIN =   1.0D+20
    Z1MAX = - 1.0D+20
    DO 300 I=1,NPOINT
       IF (KLOSS.EQ.0) THEN
             IF (RAY(10,I).LT.0.0D0)   GO TO 300
       ELSE IF (KLOSS.EQ.1) THEN
             IF (RAY(10,I).LT.0.0D0.AND.RAY(10,I).NE.CHECK) GO TO 300
       ELSE
       END IF
       TEST(1)    =   RAY(1,I)
       TEST(2)    =   RAY(2,I)
       TEST(3)    =   RAY(3,I)
       TEST(4)    =   RAY(4,I)
       TEST(5)    =   RAY(5,I)
       TEST(6)    =   RAY(6,I)
       XMIN = MIN(XMIN,TEST(1))
       XMAX = MAX(XMAX,TEST(1))
       YMIN = MIN(YMIN,TEST(2))
       YMAX = MAX(YMAX,TEST(2))
       ZMIN = MIN(ZMIN,TEST(3))
       ZMAX = MAX(ZMAX,TEST(3))
       X1MIN = MIN(X1MIN,TEST(4))
       X1MAX = MAX(X1MAX,TEST(4))
       Y1MIN = MIN(Y1MIN,TEST(5))
       Y1MAX = MAX(Y1MAX,TEST(5))
       Z1MIN = MIN(Z1MIN,TEST(6))
       Z1MAX = MAX(Z1MAX,TEST(6))
300 CONTINUE

    WRITE(6,*)'Here we are.'

    WRITE(6,*)'X max is',XMAX
    WRITE(6,*)'X min is',XMIN
    WRITE(6,*)'Y max is',YMAX
    WRITE(6,*)'Y min is',YMIN
    WRITE(6,*)'Z max is',ZMAX
    WRITE(6,*)'Z min is',ZMIN

    WRITE(6,*)'X prime max is',X1MAX
    WRITE(6,*)'X prime min is',X1MIN
    WRITE(6,*)'Y prime max is',Y1MAX
    WRITE(6,*)'Y prime min is',Y1MIN
    WRITE(6,*)'Z prime max is',Z1MAX
    WRITE(6,*)'Z prime min is',Z1MIN
    WRITE(6,*)'***********'

    !
    ! clean
    !
    IF (allocated(ray)) deallocate(ray)
   end subroutine minmax


!C+++
!C       PROGRAM         REFLAG
!C
!C       PURPOSE         To modify the good/lost flag in a given ray file
!C                       accordingly to those of another one. Typically
!C                       used to study the rays that will be lost from a
!C                       given source.
!C
!C       ALGORITHM       None
!C
!C       INPUTS          Two RAY  files
!C
!C       OUTPUT          Another ray file
!C
!C+++

   subroutine reflag
    implicit none

    character(len=sklen) :: fileIn1, fileIn2,fileOut
    real(kind=skr),dimension(:,:),allocatable :: ray1, ray2
    integer(kind=ski) :: ncol1,npoint1,iflag,ierr,i
    integer(kind=ski) :: ncol2,npoint2

               
    fileIn1 = RSTRING('REFLAG> File to be modified [ e.g. begin.dat ] ? ')
    fileIn2 = RSTRING('REFLAG> File to use as modifier [ e.g. screen.0503 ] ? ')
    fileOut = RSTRING('REFLAG> Output file ? ')


    ! 
    ! read input file 
    !
    CALL    beamGetDim (fileIn1,ncol1,npoint1,iflag,ierr)
    IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
      print *,'Reflag: beamGetDim: Error in file: '//trim(fileIn2)
      return
    END IF
    ALLOCATE( RAY1(18,npoint1) )
    ray1=0.0d0
    CALL beamLoad(ray1,ierr,ncol1,npoint1,fileIn1)
    
    CALL    beamGetDim (fileIn2,ncol2,npoint2,iflag,ierr)
    IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
      print *,'Reflag: beamGetDim: Error in file: '//trim(fileIn2)
      return
    END IF
    ALLOCATE( RAY2(18,npoint2) )
    ray2=0.0d0
    CALL beamLoad(ray2,ierr,ncol2,npoint2,fileIn2)
    
    IF (npoint1.NE.npoint2) THEN
      WRITE(6,*) 'Sorry, different number of rays.'
      WRITE(6,*)'You most likely have used wrong files. Please try again.'
      return
    END IF
    IF (ncol1.NE.ncol1) THEN
      WRITE(6,*)'Sorry, different number of columns.'
      WRITE(6,*)'You most likely have used wrong files. Please try again.'
      return
    END IF

    DO I=1,npoint1
          RAY1 (10,I) = RAY2 (10,I)
    END DO

    call beamWrite(ray1, iErr, nCol1, nPoint1, fileOut)
    if (ierr.eq.0) then
      print *,'File written to disk: '//trim(fileOut)
    else
      print *,'Reflag: ERROR writing file: '//trim(fileOut)
    end if

    !
    ! clean
    !
    IF (allocated(ray1)) deallocate(ray1)
    IF (allocated(ray2)) deallocate(ray2)
   end subroutine reflag

!C+++
!C
!C      PROGRAM     HISTO3
!C
!C      PURPOSE     Generates a three-dimensional histogram from an
!C                  output file of SHADOW. The purpose is that of
!C                  optimizing the source. In some optical systems the
!C                  number of rays traced through may be very small
!C                  because of the mismatch between a standard source
!C                  and the system etendue. This program defines the
!C                  fraction of phase space that generates ``good''
!C                  rays and limits the source generation to that 
!C                  volume. Notice that HISTO3 assumes that the
!C                  variables (x,y,z) are independent.
!C
!C      INPUT       A ray file generated by SHADOWIT or REFLAG containing the
!C                  begin.dat file with the lost rays so labeled. 
!C
!C      OUTPUT      A file for use by SHADOW in the SOURCE generation.
!C
!C      ALGORITHM   The program will create three arrays that
!C                  are filled with 1's when at least one ray falls
!C            within the limits.
!C---

    subroutine histo3

    implicit none

    character(len=sklen) :: infile, outfile

    real(kind=skr), dimension (101,101) :: ix, iy, iz

    real(kind=skr),dimension(:,:),allocatable :: ray
    integer(kind=ski) :: ncol,npoint,iflag,ierr,i,j
    integer(kind=ski) :: kount, nx, ny, nz, nx1, ny1, nz1

    real(kind=skr) :: xmin, xmax, ymin, ymax, zmin, zmax
    real(kind=skr) :: x1min, x1max, y1min, y1max, z1min, z1max
    real(kind=skr) :: xs, ys, zs, xs1, ys1, zs1
    integer(kind=ski) :: jx, jy, jz, jx1, jy1, jz1
    real(kind=skr),dimension(6) :: test

             
    infile =  RSTRING('HISTO3> Beam input file ? ')

    ! 
    ! read input file 
    !
    CALL    beamGetDim (infile,ncol,npoint,iflag,ierr)
    IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
      print *,'Histo3: beamGetDim: Error in file: '//trim(infile)
      return
    END IF

    ALLOCATE( RAY(18,npoint) )
    ray=0.0d0
    CALL beamLoad(ray,ierr,ncol,npoint,infile)
  
    OUTFILE =  RSTRING ('Output file      ? ')

!C
!C Prepare arrays
!C
    KOUNT = 0
    DO I=1,NPOINT
          IF (RAY(10,I).LT.0.0) KOUNT = KOUNT + 1
    END DO
    WRITE(6,*)'Found ',(NPOINT-KOUNT),' good points out of',NPOINT
         XMIN =   1.0D+20
         XMAX = - 1.0D+20
         YMIN =   1.0D+20
         YMAX = - 1.0D+20
         ZMIN =   1.0D+20
         ZMAX = - 1.0D+20

         X1MIN =   1.0D+20
         X1MAX = - 1.0D+20
         Y1MIN =   1.0D+20
         Y1MAX = - 1.0D+20
         Z1MIN =   1.0D+20
         Z1MAX = - 1.0D+20
         DO 300 I=1,NPOINT
       IF (RAY(10,I).LT.0.0D0)   GO TO 300
         TEST(1)    =   RAY(1,I)
         TEST(2)    =   RAY(2,I)
         TEST(3)    =   RAY(3,I)
         TEST(4)    =   RAY(4,I)
         TEST(5)    =   RAY(5,I)
         TEST(6)    =   RAY(6,I)
         XMIN = MIN(XMIN,TEST(1))
         XMAX = MAX(XMAX,TEST(1))
         YMIN = MIN(YMIN,TEST(2))
         YMAX = MAX(YMAX,TEST(2))
         ZMIN = MIN(ZMIN,TEST(3))
         ZMAX = MAX(ZMAX,TEST(3))
         X1MIN = MIN(X1MIN,TEST(4))
         X1MAX = MAX(X1MAX,TEST(4))
         Y1MIN = MIN(Y1MIN,TEST(5))
         Y1MAX = MAX(Y1MAX,TEST(5))
         Z1MIN = MIN(Z1MIN,TEST(6))
300      Z1MAX = MAX(Z1MAX,TEST(6))

         WRITE(6,*)'Here we are.'

         WRITE(6,*)'X max is',XMAX
         WRITE(6,*)'X min is',XMIN
         WRITE(6,*)'Y max is',YMAX
         WRITE(6,*)'Y min is',YMIN
         WRITE(6,*)'Z max is',ZMAX
         WRITE(6,*)'Z min is',ZMIN

         WRITE(6,*)'X prime max is',X1MAX
         WRITE(6,*)'X prime min is',X1MIN
         WRITE(6,*)'Y prime max is',Y1MAX
         WRITE(6,*)'Y prime min is',Y1MIN
         WRITE(6,*)'Z prime max is',Z1MAX
         WRITE(6,*)'Z prime min is',Z1MIN
!C
!C Inquire about the histogram size
!C
         NX  = IRINT ('How many bins in X  [ default 25 ] ? ')
         IF (NX.EQ.0) NX = 25
         NX1 = IRINT ('How many bins in X`          ? ')
         IF (NX1.EQ.0) NX1 = 25
         NY  = IRINT ('How many bins in Y           ? ')
         IF (NY.EQ.0) NY = 25
         NY1 = IRINT ('How many bins in Y`          ? ')
         IF (NY1.EQ.0) NY1 = 25
         NZ  = IRINT ('How many bins in Z           ? ')
         IF (NZ.EQ.0) NZ = 25
         NZ1 = IRINT ('How many bins in Z`          ? ')
         IF (NZ1.EQ.0) NZ1 = 25
!C
!C Compute histogram steps
!C
         XS    =  (XMAX - XMIN)    /NX
         XS1    =  (X1MAX - X1MIN)/NX1
         YS    =  (YMAX - YMIN)    /NY
         YS1    =  (Y1MAX - Y1MIN)/NY1
         ZS    =  (ZMAX - ZMIN)    /NZ
         ZS1    =  (Z1MAX - Z1MIN)/NZ1
!C
!C Build now the arrays
!C
         DO I=1,NPOINT
          IF (RAY(10,I).GE.0.0) THEN
             JX  = 1
            JY  = 1
            JZ  = 1
            JX1 = 1
            JY1 = 1
            JZ1 = 1
           IF (ABS(XS).GT.1.0D-10)  JX  = (RAY(1,I) - XMIN)   /XS  + 1
           IF (ABS(XS1).GT.1.0D-10) JX1 = (RAY(4,I) - X1MIN) /XS1 + 1
           IF (ABS(YS).GT.1.0D-10)  JY  = (RAY(2,I) - YMIN)   /YS  + 1
           IF (ABS(YS1).GT.1.0D-10) JY1 = (RAY(5,I) - Y1MIN) /YS1 + 1
           IF (ABS(ZS).GT.1.0D-10)  JZ  = (RAY(3,I) - ZMIN)   /ZS  + 1
           IF (ABS(ZS1).GT.1.0D-10) JZ1 = (RAY(6,I) - Z1MIN) /ZS1 + 1
            IX (JX,JX1) = 1
            IY (JY,JY1) = 1
            IZ (JZ,JZ1) = 1
          END IF
         END DO
!C
!C Write out arrays
!C
         !OPEN (21, FILE=OUTFILE, STATUS='UNKNOWN', FORM='UNFORMATTED')
         ! changed to formatted, at leas we can see what is there!
         ! srio@esrf.eu 20120525
         OPEN (21, FILE=OUTFILE, STATUS='UNKNOWN', FORM='FORMATTED')
         REWIND (21)
         WRITE (21,*) NX,  XMIN,   XS
         WRITE (21,*) NX1, X1MIN, XS1
         WRITE (21,*) NY,  YMIN,   YS
         WRITE (21,*) NY1, Y1MIN, YS1
         WRITE (21,*) NZ,  ZMIN,   ZS
         WRITE (21,*) NZ1, Z1MIN, ZS1
         DO I=1,NX
           WRITE (21,*) (IX(I,J),J=1,NX1)
         END DO
         DO I=1,NY
           WRITE (21,*) (IY(I,J),J=1,NY1)
         END DO
         DO I=1,NZ
           WRITE (21,*) (IZ(I,J),J=1,NZ1)
         END DO
         CLOSE (21)
         print *,'File written to disk: '//trim(outfile)

    ! 
    ! clean 
    !
    if (allocated(ray)) deallocate(ray)
    return

   end subroutine histo3

!
! retrace: propagates the beam in vacuum to a plane perpendiculat to the 
!            optical axis
!
! inputs: 
!        ray: the beam array
!        np: number of points in ray
!        distance: the distance to the image plane
!        resetY: reset all the Y values to zero (0=no, 1=yes)
! outputs: 
!        ray (overwritten): the array with the rotated beam
!
!
subroutine retrace(ray,np,distance,resetY)
    !use shadow_globaldefinitions
      real(kind=skr), dimension(18,np), intent(inout) :: ray
      integer(kind=ski), intent(in)                   :: np,resetY
      real(kind=skr), intent(in)                      :: distance

      real(kind=skr),dimension(3)      :: tmpv
      real(kind=skr),dimension(np)     :: rdist
      integer(kind=ski)                :: i
      
      rdist = -ray(2,:)
      rdist = (rdist+distance)/ray(5,:)

      ! check for perpendicular rays
      do i=1,np
         if (ray(5,i).lt.1d-16) rdist(i) = 0.0
      end do
      ray(1,:) = ray(1,:)+rdist*ray(4,:)
      ray(2,:) = ray(2,:)+rdist*ray(5,:)
      ray(3,:) = ray(3,:)+rdist*ray(6,:)
      if (resetY.eq.1) ray(2,:) = 0.0
      return      
end subroutine retrace

!
! this is an prompt interface to call retrace from shadow3
!
SUBROUTINE retrace_interface

implicit none

character(len=sklen) :: inFile1,outFile1
real(kind=skr),dimension(:,:),allocatable :: ray
integer(kind=ski) :: ncol,np1,iflag,ierr,resetY
real(kind=skr) :: dist
           
INFILE1 =  RSTRING('RETRACE> Beam input file ? ')
OUTFILE1 = RSTRING ('RETRACE> Beam output file ? ')
DIST    =  RNUMBER ('RETRACE> Distance ? ')
RESETY  =  IRINT('RETRACE> reset to zero all Y coordinates [0/1]?')

! 
! read input file 
!
CALL    beamGetDim (inFile1,ncol,np1,iflag,ierr)
IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
  print *,'Retrace: beamGetDim: Error in file: '//trim(inFile1)
  return
END IF

ALLOCATE( RAY(18,NP1) )
ray=0.0d0

CALL beamLoad(ray,ierr,ncol,np1,inFile1)

!
! do the job
!
call retrace(ray,np1,dist,resetY)
!
! write results
!
call beamWrite(ray, iErr, ncol, np1, outfile1)
if (ierr.eq.0) then
  print *,'File written to disk: '//trim(outfile1)
else
  print *,'ERROR writing file: '//trim(outfile1)
end if

!
! clean
!
IF (allocated(ray)) deallocate(ray)

RETURN

END SUBROUTINE retrace_interface


End Module Shadow_PostProcessors
