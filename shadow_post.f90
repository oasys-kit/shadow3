!----
!---- MODULE:  shadow_Post
!----
!---- Postprocessors for Shadow
!---- Contains: 
!----
!----
!----
!---- Example of usage: 
!----
!----

Module Shadow_Post
    !---- Use Modules ----!

    use stringio
    use beamio
    use math, only : wran
!    use math_imsl
    use shadow_kind
! todo: resdistribute variables in shadow_variables. Here only the
!       pool variables should be accessed and not Global variables. 
!       Also, the PoolSourceLoad routine should not be on shadow_kernel
    use shadow_variables
    use shadow_kernel
!    use shadow_sourcesync

    !---- Variables ----!
    implicit none

!todo: fix this repetition
!
! the global variables here are only used for undulator and not for wiggler
 
!     	real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
!     	real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
!     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
!     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
!     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
!	real(kind=skr),parameter ::TOCM=  1.239852D-4		     
!	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     


    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
!    public :: 
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!
    public :: SourcInfo,MirInfo,SysInfo,Translate,Histo1,PlotXY
    public :: FFresnel,ReColor,Intens,FocNew

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
!C
!C This causes problems with F77 drivers, since can't use -I directive.
!C so I'll use the standard cpp directive instead.
!C
!C	INCLUDE         './../../include/common.blk'
!C	INCLUDE         './../../include/namelist.blk'
!C
!C
!#	include		<common.blk>
!#	include		<namelist.blk>
!#elif defined(vms)
!     	INCLUDE		'SHADOW$INC:COMMON.BLK/LIST'
!     	INCLUDE		'SHADOW$INC:NAMELIST.BLK/LIST'
!#endif
	implicit none

	type (poolSource)      ::  pool00

	character(len=sklen) :: inFile1,file_Out,cd
	character(len=80) :: comment,title
!	character(len=63),parameter :: topLin= &
!     '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
	character(len=79),parameter :: topLin= &
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
!     	CHARACTER *80	COMMENT,TITLE
!     	DIMENSION	WAVE(10)
!     	CHARACTER *71	TOPLIN
!     	DATA	TOPLIN	/
!     $'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'/
!     	CHARACTER * 32 		FILE_OUT
!     	CHARACTER * 60		FILETEXT
!     	CHARACTER * 17		DATETEXT
!     	CHARACTER *20		BREAK, TANG(12), TSPATIAL(12)
!     	CHARACTER *12		TPOL (3), TPHOT(12), TDEPTH(12)
!     	CHARACTER *80		INFILE
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

     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
	integer(kind=ski)        :: J

!     	DATA	TSPATIAL(1)	/'POINT       '/
!     	DATA	TSPATIAL(4)	/'GAUSSIAN    '/
!     	DATA	TSPATIAL(2)	/'RECTANGULAR '/
!     	DATA	TSPATIAL(3)	/'ELLIPTICAL  '/
!	DATA	TSPATIAL(5)	/'PHASE SPACE ELLIPSE'/
!     	DATA	TDEPTH(1)	/'DEPTH OFF   '/
!     	DATA	TDEPTH(2)	/'DEPTH ON    '/
!     	DATA	TDEPTH(3) 	/'RECTANGULAR '/
!     	DATA	TDEPTH(4) 	/'GAUSSIAN    '/
!     	DATA	TDEPTH(5)	/'SYNCHROTRON '/
!     	DATA	TANG(1)		/'UNIFORM     '/
!     	DATA	TANG(2)		/'LAMBERTIAN  '/
!     	DATA	TANG(3)		/'GAUSSIAN    '/
!     	DATA	TANG(4)		/'SYNCHROTRON '/
!     	DATA	TANG(5)		/'CONICAL     '/
!	DATA	TANG(6)		/'SYNCHROTRON (exact)'/
!	DATA	TANG(7)		/'PHASE SPACE ELLIPSE'/
!     	DATA	TPHOT(1)	/'PHOTON OFF  '/
!     	DATA	TPHOT(2)	/'PHOTON ON   '/
!     	DATA	TPHOT(3)	/'SINGLE LINE '/
!     	DATA	TPHOT(4)	/'MULTI LINE  '/
!     	DATA	TPHOT(5)	/'BOX DISTR.  '/
!     	DATA	TPOL(1)		/'SR PARALLEL '/
!     	DATA	TPOL(2)		/'SR PERPEND  '/
!     	DATA	TPOL(3)		/'SR TOTAL    '/
!     	DATA	BREAK	/'    ----------------'/

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

!C#if vms
!C     	CALL	LIB$ERASE_PAGE(1,1)
!C#elif unix
!C        CALL system('clear') 
!C#endif
!	CALL CLSCREEN
     	WRITE(6,*) &
      '------------------ S O U R C I N F O --------------------'
!     	WRITE(6,*)  &
!      '                  vs. 2.01   Feb 1989'
     	WRITE(6,*) ' '
!     	GO TO 1
!20	WRITE(6,*) 'Error reading the NAMELIST.'
!     	STOP
!10     	WRITE(6,*) 'Error opening: ',INFILE
!1     	CALL ALINE ('INPUT> File containing source specs ?',INFILE)
!     	CALL	ALINE ('INPUT> Title ? ',TITLE)
!     	CALL	ALINE ('INPUT> Comment ? ',COMMENT)
!     	CALL	ALINE ('INPUT> Output file-name ? ',FILE_OUT)
!     	WRITE(6,*) 'SYSINF> Prepare output to file : ',FILE_OUT
	inFile1 = RString('SourcInfo: File containing source specs ?')
	title = RString('SourcInfo: Title ? ')
	comment = RString('SourcInfo: Comment ?')
	File_Out = RString('SourcInfo: Output file-name ? ')

! 
! read input file
!
!     	IDUMM = 1
!     	CALL	RWNAME ( INFILE,'R_SOUR', IDUMM)
!     	IF (IDUMM.EQ.-1) STOP 'Error opening file: '//trim(inFile)
!     	IF (IDUMM.EQ.-2) STOP 'Error opening file: '//trim(inFile)
!     	WRITE(6,*) 'SourcInfo: File ',INFILE,' read correctly.'
	CALL PoolSourceLoad(pool00,inFile1)


!     	WRITE(6,*) 'SYSINF> Prepare output to file : ',FILE_OUT
!#ifdef vms
!     	OPEN (30,FILE=FILE_OUT,STATUS='NEW',CARRIAGECONTROL='LIST')
!#else
     	OPEN (30,FILE=FILE_OUT,STATUS='UNKNOWN')
	REWIND (30)
!#endif
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) &
      '**************  S O U R C E       ', &
      'D E S C R I P T I O N  **************'
     	WRITE (30,*) trim(TITLE) 
     	WRITE (30,*) trim(COMMENT)
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) 'Input file specified: '//trim(inFile1)
!#ifdef vms
!     	  CALL	FILEINFO  (INFILE)
!     	  CALL	NEXTFILE  (FILETEXT,DATETEXT)
!#else
!	filetext = ' '
!	call get_file_text(filetext,INFILE)
!	DATETEXT = ' '
!#endif
!	  WRITE (30,1500)	FILETEXT
!1500	FORMAT (1X,'Full file Specification :',A)
!     	  WRITE (30,1501)	DATETEXT
	CALL GETCWD(CD)
     	WRITE (30,*) 'Full file specification: '//trim(CD)//OS_DS//trim(inFile1)
!1501	FORMAT (1X,'Creation Date           :',A)
	call date_and_time(TIME=time,DATE=date,ZONE=zone)
     	WRITE (30,*) 'Creation date: '//date(1:4)//' '//date(5:6)//' '//& 
            date(7:8)//' '//time(1:2)//'h '//time(2:3)//'min '//time(4:8)//' s'

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
!C SOURCE as of Nov. 1985 is independent of SHADOW and always generates a 
!C new source.
!C
!C Old/New SOURCE
!C
!C     	IF (F_NEW.EQ.0) THEN
!C     	WRITE (30,*) 'Used an OLD source file. Complete file specs
!C     $ follow.'
!C     	  CALL	FILEINFO  (FILE_SOURCE)
!C     	  CALL	NEXTFILE  (FILETEXT,DATETEXT)
!C	  WRITE (30,1500)	FILETEXT
!C     	  WRITE (30,1501)	DATETEXT
!C     	WRITE (30,*) TOPLIN
!C     	CALL	EXIT
!C     	END IF
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
     	   IF (F_PHOT.EQ.0) WAVE(2) = TOANGS/photonArray(2)
     	   IF (F_PHOT.EQ.1) photonArray(2) = TOANGS/WAVE(2)
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
!	character(len=63),parameter :: topLin= &
!     '+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
	character(len=79),parameter :: topLin= &
'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
	! for date_and_time
	character(len=8)  :: date
	character(len=10) :: time
	character(len=5)  :: zone
	integer,dimension(8) :: values

! todo: check that this is also defined in shadow_kernel...
     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
	real(kind=skr)           ::ECCENT,AFOCI
	integer(kind=ski)        :: J

!C
!C This causes problems with F77 drivers, since can't use -I directive.
!C so I'll use the standard cpp directive instead.
!C
!C	INCLUDE         './../../include/common.blk'
!C	INCLUDE         './../../include/namelist.blk'
!C
!C
!#	include		<common.blk>
!#	include		<namelist.blk>
!#elif defined(vms)
!     	INCLUDE		'SHADOW$INC:COMMON.BLK/LIST'
!     	INCLUDE		'SHADOW$INC:NAMELIST.BLK/LIST'
!#endif
!     	CHARACTER *40	MIRFIL
!     	CHARACTER *80	COMMENT,TITLE
!     	CHARACTER *71	TOPLIN
!     	DATA	TOPLIN	/
!     $'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'/
!!     	CHARACTER * 32 		FILE_IN,FILE_OUT
!     	CHARACTER * 60		FILETEXT
!     	CHARACTER * 17		DATETEXT
!     	CHARACTER *20		TYPE (12)
!     	DATA	TYPE(1)	/'SPHERICAL   '/
!     	DATA	TYPE(2)	/'ELLIPTICAL  '/
!     	DATA	TYPE(3) /'TOROIDAL    '/
!     	DATA	TYPE(4) /'PARABOLICAL '/
!     	DATA	TYPE(5)	/'PLANE       '/
!     	DATA	TYPE(6) /'CODLING sLIT'/
!     	DATA	TYPE(7)	/'HYPERBOLICAL'/
!     	DATA	TYPE(8)	/'CONICAL     '/
!     	DATA	TYPE(9)	/'POLYNOMIAL  '/

type1(1)	='SPHERICAL   ' 
type1(2)	='ELLIPTICAL  ' 
type1(3) ='TOROIDAL    ' 
type1(4) ='PARABOLICAL ' 
type1(5)	='PLANE       ' 
type1(6) ='CODLING SLIT' 
type1(7)	='HYPERBOLICAL' 
type1(8)	='CONICAL     ' 
type1(9)	='POLYNOMIAL  ' 
type1(10)	='            ' 
type1(11)	='            ' 
type1(12)	='            ' 

!     	GO TO 1
!20	WRITE(6,*) 'Error reading the NAMELIST.'
!     	STOP
!10     	WRITE(6,*) 'Error opening: ',MIRFIL
!1	CONTINUE
!!C#if vms
!!C     	CALL	LIB$ERASE_PAGE (1,1)
!!C#endif 
!	CALL CLSCREEN
    	WRITE(6,*)  &
      	'-------------------- M I R I N F O ----------------------'
!     	WRITE(6,*) 
!     $	'                  vs. 3.0 - May 1993 '
     	WRITE(6,*) ' '
     	WRITE(6,*)  &
      'MirInfo: Mirror descriptor file. It must be an end.xx type.'
     	WRITE(6,*) 'MirInfo: Please input filename: '
     	READ (5,'(A)')	MirFil
!1000	FORMAT	(A)

!     	IDUMM = 1
!     	CALL	RWNAME ( MIRFIL,'R_OE', IDUMM)
!     	IF (IDUMM.EQ.-1) GO TO 20
!     	IF (IDUMM.EQ.-2) GO TO 10
!     	IF (IDUMM.NE.0) STOP 'File non in standard format'
!     	WRITE(6,*) 'MINFO> File read correctly.'
     	WRITE(6,*) 'MirInfo: Title ?'
     	READ (5,'(A)') TITLE
     	WRITE(6,*) 'MirInfo: Comment ?'
     	READ (5,'(A)') COMMENT
     	WRITE(6,*) 'MirInfo: Output file ?'
     	READ (5,'(A)')	FILE_OUT
!
! read file
!
	CALL PoolOELoad(p1,mirFil)

!     	WRITE(6,*) 'MINFO> Prepare output to file : ',FILE_OUT
!#ifdef vms
!     	OPEN (20,FILE=FILE_OUT,STATUS='NEW',CARRIAGECONTROL='LIST')
!#else
     	OPEN (20,FILE=FILE_OUT,STATUS='UNKNOWN')
	REWIND(20)
!#endif
     	WRITE (20,*) TOPLIN
     	WRITE (20,*) '********************   MIRROR  DESCRIPTION   ',&
      '********************'
     	WRITE (20,*) TOPLIN
     	WRITE (20,*) trim(TITLE)
     	WRITE (20,*) trim(COMMENT)
     	WRITE (20,*) TOPLIN
     	WRITE (20,*) 'Input file specified: '//trim(MIRFIL)
!#ifdef vms
!     	CALL	FILEINFO  (MIRFIL)
!     	CALL	NEXTFILE  (FILETEXT,DATETEXT)
!#else
!	filetext = ' '
!	datetext = ' '
!	call get_file_text(filetext,MIRFIL)
!#endif 
!	WRITE (20,1500)	FILETEXT
!1500	FORMAT (1X,'Full file Specification :',A)
!     	WRITE (20,1501)	DATETEXT
!1501	FORMAT (1X,'Creation Date           :',A)
        CALL GETCWD(CD)
        WRITE (20,*) 'Full file specification: '//trim(CD)//OS_DS//trim(mirFil)
        call date_and_time(TIME=time,DATE=date,ZONE=zone)
        WRITE (20,*) 'Creation date: '//date(1:4)//' '//date(5:6)//' '//&
            date(7:8)//' '//time(1:2)//'h '//time(2:3)//'min '//time(4:8)//' s'


     	WRITE (20,*)	TOPLIN
     	WRITE (20,2001) type1(FMIRR)
2001	FORMAT (/,1X,'Surface figure was defined as:',T40,A)
!C      123456789 123456789 123456789 123456789 

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
	  WRITE (20,*) 'Paramters from                              ',FILE_KOMA_CA
	  WRITE (20,*) 'Tube radii specified as (r(Z))**2'
	    ELSE
	  WRITE (20,*) 'Paramters from                              ',FILE_KOMA
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
2002	FORMAT (1X,'Input Slit Dist.',T20,'Exit Slit Dist.', &
                T40,'Input Slit Angle',T60,'Exit Slit Angle')
     	   WRITE (20,2003)  p1%HOLO_R1,p1%HOLO_R2,p1%HOLO_DEL,p1%HOLO_GAM
2003	FORMAT (1X,G16.9,T20,G16.9,T40,G16.9,T60,G16.9)
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
     	  WRITE (20,*) 'Relative Index of Refraction           ',p1%ALFA
     	END IF
     	IF (p1%F_REFLEC.EQ.0) THEN
     	  WRITE (20,*) 'Reflectivity                            OFF'
     	ELSE
	 IF (p1%F_REFL.EQ.0) THEN
     	  WRITE (20,*) 'Reflectivity      ON     coefficients from: ',FILE_REFL
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
2005	FORMAT (1X,T10,'X plus',T30,'X minus',T50,'Y plus',T70,'Y minus')
     	WRITE (20,2004) p1%RWIDX1,p1%RWIDX2,p1%RLEN1,p1%RLEN2
2004	FORMAT (1X,T10,G12.5,T30,G12.5,T50,G12.5,T70,G12.5)
     	 ELSE IF (p1%FSHAPE.EQ.2) THEN
     	  WRITE (20,*) 'Mirror dimensions ( elliptical ) :'
        WRITE (20,2006)
2006	FORMAT (1X,T10,'Major Axis',T30,'Minor axis')
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
2010	FORMAT (1X,'Spherical Radius ',T40,G16.9)
     	ELSE IF (p1%FMIRR.EQ.2) THEN
     	WRITE (20,*)'   Semi-major axis  ', p1%AXMAJ
     	WRITE (20,*)'   Semi-minor axis  ', p1%AXMIN
     	WRITE (20,*)'   Semi-focal-length', SQRT(p1%AXMAJ**2-p1%AXMIN**2)
	ECCENT = SQRT(p1%AXMAJ**2-p1%AXMIN**2)/p1%AXMAJ
     	WRITE (20,*)'   Eccentricity     ', ECCENT
!C     	WRITE (20,*)'   Semi-focal-length',AFOCI
!C     	WRITE (20,*)'   Eccentricity     ', ECCENT
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
     	WRITE (20,*)'   Semi-minor axis  ', AXMIN
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
!C       123456789 123456789 123456789 123456789 123456789
     	ELSE
     	WRITE (20,*) 'Mirror moved from pole. Parameters :'
     	WRITE (20,*) 'Displacement along X:   ',p1%OFFX
     	WRITE (20,*) '                   Y:   ',p1%OFFY
     	WRITE (20,*) '                   Z:   ',p1%OFFZ
     	WRITE (20,*) 'Rotation around X:   ',p1%X_ROT*TODEG
     	WRITE (20,*) '                Y:   ',p1%Y_ROT*TODEG
     	WRITE (20,*) '                Z:   ',p1%Z_ROT*TODEG
     	END IF
     	WRITE (20,*) TOPLIN
     	WRITE (20,*) '***************                 E N', &
      ' D                  ***************'
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

!#if defined(unix) || HAVE_F77_CPP
!C
!C This causes problems with F77 drivers, since can't use -I directive.
!C so I'll use the standard cpp directive instead.
!C
!C	INCLUDE         './../../include/common.blk'
!C	INCLUDE         './../../include/namelist.blk'
!C
!C
!#	include		<common.blk>
!#	include		<namelist.blk>
!#elif defined(vms)
!     	INCLUDE		'SHADOW$INC:COMMON.BLK/LIST'
!     	INCLUDE		'SHADOW$INC:NAMELIST.BLK/LIST'
!#endif
!     	CHARACTER *40	MIRFIL
!     	CHARACTER *80	COMMENT,TITLE
!     	CHARACTER *71	TOPLIN
!     	DATA	TOPLIN	/
!     $'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'/
!     	CHARACTER * 32 		FILE_OUT
!     	CHARACTER * 60		FILETEXT
!     	CHARACTER * 17		DATETEXT

	character(len=20),dimension(12) :: type1
	character(len=20),dimension(2)  :: tScr
	character(len=20),dimension(5)  :: tSlit
	character(len=20)               :: break
	character(len=80)               :: text
	character(len=sklen),dimension(20)  :: file_In
!	CHARACTER *20		TYPE (12),BREAK, TSLIT(5) *11
!     	CHARACTER *21		TSCR (2)
!     	CHARACTER *80		FILE_IN(20),OPTFILE,TEXT,RSTRING
!	SAVE OPTFILE


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

!	CALL CLSCREEN
     	WRITE(6,*)  &
        '-------------------- S Y S I N F O ----------------------'
!     	WRITE(6,*) 
!     $  '                  vs. 2.02  June 1989'
     	WRITE(6,*) ' '
!     	GO TO 1
!20	WRITE(6,*) 'Error reading the NAMELIST.'
!     	STOP
!10     	WRITE(6,*) 'Error opening: ',MIRFIL
!1     	IDEF = IYES ('Default filenames [ Y/N ] ?')
      	IDEF = IYES ('SysInfo: Default filenames [ Y/N ] ?')
     	IF (IDEF.EQ.1) THEN
     	  NF	=   IRINT ('SysInfo: How many OE''s ? ')
     	  IF (NF.EQ.0) NF = 1
     	  DO J=1,NF
     	   WRITE (FILE_IN(J),999) 'end.',J
!D	   write(6,*) file_in(j)
	  END DO
     	  WRITE (OPTFILE,998)	'optax.',NF
998	  FORMAT (A6,I2.2)
999	  FORMAT (A4,I2.2)
!D	write(6,*) opt_fil
     	ELSE
     	  WRITE(6,*) 'SysInfo: List of files describing the system. Use TT: ',&
      'for keyboard input.'
	  MIRFIL = RSTRING ('SysInfo: File containing ERFs ? ')
!     	 IF (MIRFIL(1:3).EQ.'TT:') WRITE(6,*)  &
!      'SysInfo> Please enter filenames: ?'
!#ifdef vms
!     	  OPEN (20, FILE=MIRFIL, STATUS='OLD', READONLY, ERR=10)
!#else
     	  !OPEN (20, FILE=MIRFIL, STATUS='OLD', ERR=10)
     	  OPEN (20, FILE=MIRFIL, STATUS='OLD', IOSTAT=iERR)
	  IF (iErr /= 0) THEN
            print *,'SysInfo: Error reading file: '//trim(mirFil)
            STOP  'Aborted'
	  END IF
!#endif
	  I=1
30     	  CONTINUE
     	  READ (20,'(A)',ERR=40,END=40)  FILE_IN(I)
          WRITE(6,*) 'File # ',I,' = '//trim(file_in(i))
     	  I=I+1
     	  GO TO 30
40	  CLOSE (20)
     	
     	  WRITE(6,*) 'SysInfo> Will use ',I-1,' ERF files.'
     	  NF	= I - 1
     	  !CALL	ALINE ('INPUT> Optaxis file-name : ',OPTFILE)
     	  optFile=RString('SysInfo: Optaxis file-name : ')
!1000	  FORMAT	(A)
     	END IF
     	!CALL	ALINE ('INPUT> Title ? ',TITLE)
     	!CALL	ALINE ('INPUT> Comment ? ',COMMENT)
     	!CALL	ALINE ('INPUT> Output file-name ? ',FILE_OUT)
     	!WRITE(6,*) 'SYSINF> Prepare output to file : ',FILE_OUT
	title = RString('SysInfo: Title ?  ')
	comment = RString('SysInfo: Comment ? ')
	file_Out = RString('SysInfo: Output file : ')
!#ifdef vms
!     	OPEN (30,FILE=FILE_OUT,STATUS='NEW',CARRIAGECONTROL='LIST')
!#else
     	OPEN (30,FILE=FILE_OUT,STATUS='UNKNOWN')
	REWIND (30)
!#endif
     	WRITE (30,*) ''
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) '**************  S Y S T E M      ',&
      'D E S C R I P T I O N  **************'
     	WRITE (30,*) TOPLIN
     	WRITE (30,*) trim(TITLE)
     	WRITE (30,*) trim(COMMENT)
     	WRITE (30,*) TOPLIN
	IF (iDef /= 1) WRITE (30,*) 'Input file with end.xx files:'//trim(MIRFIL)

!     	IF (MIRFIL(1:3).NE.'TT:'.AND.IDEF.NE.1) THEN
!     	  CALL	FILEINFO  (MIRFIL)
!     	  CALL	NEXTFILE  (FILETEXT,DATETEXT)
!	  WRITE (30,1500)	FILETEXT
!1500	FORMAT (1X,'Full file Specification :',A)
!     	  WRITE (30,1501)	DATETEXT
!1501	FORMAT (1X,'Creation Date           :',A)
!     	END IF
!     	IF (IDEF.NE.1) THEN
          CALL GETCWD(CD)
!          WRITE (30,*) 'Full file specification: '// & 
!                       trim(CD)//OS_DS//trim(mirFil)
!          call date_and_time(TIME=time,DATE=date,ZONE=zone)
!          WRITE (30,*) 'Creation date: '//date(1:4)//' '//date(5:6)//' '//&
!            date(7:8)//' '//time(1:2)//'h '//time(2:3)//'min '//time(4:8)//' s'
!	END IF


     	WRITE (30,*)	TOPLIN
!     	WRITE (30,1502)
!1502	FORMAT (1X,' #    Optical Element: ',T62,'Creation Time:')
     	WRITE (30,*) ' #    Optical Element: '

     	DO J=1,NF
     	  !TEXT(1:80) = FILE_IN(J)(1:80)
     	  !CALL DESPACE (TEXT,TEXT,ILEN)
     	  !CALL	FILEINFO  (TEXT(1:ILEN))
     	  !CALL	NEXTFILE  (FILETEXT,DATETEXT)
	  !WRITE (30,1503)	J,FILETEXT,DATETEXT
	  WRITE (30,*)	J,trim(cd)//OS_DS//trim(FILE_IN(J))
!12      CONTINUE
	END DO
!1503	FORMAT (1X,I4,3X,A59,T63,A)
     	WRITE (30,*)	TOPLIN
     	
     	DO 13 J=1,NF
     	!IDUMM = 0
     	!CALL	RWNAME ( FILE_IN(J),'R_OE', IDUMM)
	!WRITE(6,*) N_SCREEN
     	!IF (IDUMM.EQ.-1) GO TO 20
     	!IF (IDUMM.EQ.-2) GO TO 10
     	!IF (IDUMM.NE. 0) STOP 'Fatal Error.'
     	!WRITE(6,*) 'SYSINF> File ',FILE_IN(J),' read correctly.'
        CALL PoolOELoad(p2,file_in(j) )
     	WRITE (30,*) ' '
     	WRITE (30,*)	'Optical Element # ',J,'      System Number: '
     	DO 14 I=1,80
     	TEXT(I:I)=' '
14      CONTINUE
     	IF (p2%F_CRYSTAL.EQ.1) THEN
     		TEXT(1:5) = 'BRAGG'
     	ELSE IF (F_GRATING.EQ.1) THEN
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
!#ifdef vms
!     	OPEN (20, FILE=OPTFILE, STATUS='OLD', READONLY)
!#else
     	OPEN (20, FILE=OPTFILE, STATUS='OLD')
!#endif
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

	!IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
	!IMPLICIT INTEGER(KIND=4)        (F,I-N)

        CHARACTER(len=sklen)      :: FILEIN,FILEOUT
        real(kind=skr), dimension(:,:), allocatable  :: Ray
    	integer(kind=ski) :: IUNIT,OUTFLAG
    	integer(kind=ski) :: ICSV,nCol1,nPoint1,iFlag,iErr,nRay,i,j
    
    	FILEIN = RSTRING('File for input ? ')
    
        !
        ! allocate RAY and read binary file
        ! 

        ! before calling rbeam18, we must allocate arrays
        ! get information on number of columns and rays from the header 
        CALL    RBEAMANALYZE (FILEIN,nCol1,NPOINT1,IFLAG,IERR)
        IF ((IFLAG.ne.0).OR.(IERR.ne.0)) THEN
           print *,'TRANSLATE: Error accessing file: '//trim(FILEIN)
           STOP 'Aborted'
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
            ! RBEAM18 must be calles once RAY is allocated
            CALL RBeam18(ray,ierr,nCol1,npoint1,fileIn)
         END IF


        ! CALL	RBEAM18	(FILEIN,RAY,nCol1,NPOINT1,IFLAG,IERR)
    
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
!C	program		histo1
!C
!C	purpose		this program will generate an histogram of the
!C			distribution of the rays in a given column.
!C
!C	input		a RAY file from SHADOW
!C
!C	output		a plottable file
!C
!C	Link using GRA:GRALIB.LNK or GRA:TDSHARE.LNK
!C---

SUBROUTINE Histo1

!	IMPLICIT        REAL*8          (A-E,G-H,O-Z)
!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!        INCLUDE	        'SHADOW$INC:DIM.PAR/LIST'
!C
!C CHECK/FIXME:
!C The X and Y vectors need to "REAL" for VMS for the Topdrawer calls,
!C but REAL*8 for Unix and the rest of the civilized world.
!C
	implicit none 

	character(len=sklen)  :: fileIn,fileIn2
	integer(kind=ski)  :: n_Col,iEner,iFlag,iErr,i,iAnsw
	integer(kind=ski)  :: nCol1,nPoint1,nCol2,nPoint2
	
	integer(kind=ski)  :: iLost,iRefl,iUnits,iFile,iScale,jBin,nBin
	real(kind=skr)     :: xmin,xmax,center,width,norm,xArg,xLow
	real(kind=skr)     :: step,xStart,arg,val,val0,counts,ymax,y1upp
	real(kind=skr)     :: rNorm

        real(kind=skr),dimension(:,:),allocatable :: ray,ray2
        real(kind=skr),dimension(:),allocatable   :: xArray,yArray

!     	real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
!     	real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
!     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
!     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     
!	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
!	REAL		XARRAY, YARRAY
!#endif
!	CHARACTER*80	FILEIN,FILEIN2,RSTRING
!
!     	REAL*8		RAY(18,N_DIM),VAL,TCONV
!     	REAL*8		RAY2(18,N_DIM)
!        REAL*8          RNUMBER
!     	DIMENSION	XARRAY(500),YARRAY(500),XTEMP(101)

!#ifndef vms
!	CHARACTER*1024	PRIMVS
!	CHARACTER*1024	PRIMVSPATH
!#endif
     	
!     	DATA	PI     	/  3.1415 92653 58979 32384 62643 D0 /
!     	DATA	PIHALF 	/  1.5707 96326 79489 66192 31322 D0 /
!     	DATA	TWOPI 	/  6.2831 85307 17958 64769 25287 D0 /
!     	DATA	TODEG 	/ 57.2957 79513 08232 08767 98155 D0 /
!!     	DATA	TORAD	/  0.0174 53292 51994 32957 69237 D0 /
!	DATA	TOCM	/  1.239 852	D-4		     /
!	DATA	TOANGS 	/  1.239 852    D+4		     /
!
!1	WRITE(6,*)'File for analysis ?'
1	WRITE(6,*)'File for analysis ?'
	READ(5,3333) FILEIN

3333	FORMAT (A)

     	N_COL = IRINT ('Column to analyze ? ')
	IF (N_COL.EQ.11) THEN
	  WRITE(6,*)'Options : [0] Angstroms'
	  WRITE(6,*)'        : [1] eV'
	  WRITE(6,*)'        : [2] cm-1'
	  IENER	= IRINT ('<?> ')
	END IF
!C
!
! reads input binary file
!
!     	CALL	RBEAM18	(FILEIN,RAY,NCOL,NPOINT,IFLAG,IERR)
!	IF (IERR.NE.0)	STOP	'Error in reading ray file.'

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (fileIn,ncol1,npoint1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'TRACE3: RBeamAnalyze: Error in file: '//trim(fileIn)
            stop
        END IF

  	ALLOCATE( RAY(18,NPOINT1) )
  	ALLOCATE( xArray(NPOINT1) )
  	ALLOCATE( yArray(NPOINT1) )
  	ray=0.0d0

	CALL RBeam18(ray,ierr,ncol1,npoint1,fileIn)



!C
!C Find the min and max.
!C
 	XMIN	=  1.0E+20
	XMAX	= -1.0E+20
	DO 11 I = 1, NPOINT1
	  XARG	= RAY(N_COL,I)
	  XMIN	= MIN (XMIN,XARG)
	  XMAX	= MAX (XMAX,XARG)
11      CONTINUE
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
     	CENTER= RNUMBER ('Distribution center ? ')
     	WIDTH = RNUMBER ('             width ? ')
     	NBIN = IRINT ('Number of bins (odd, please) ? ')
     	WRITE(6,*)'Flag checks. Enter :'
     	WRITE(6,*)'0	to exclude lost rays'
     	WRITE(6,*)'1	to include lost rays too'
     	WRITE(6,*)'2	to use only lost rays'
     	ILOST = IRINT ('<?> ')
     	WRITE(6,*)'Normalization kind. Enter :'
     	WRITE(6,*)'0	for no normalization'
     	WRITE(6,*)'1	to normalize to 1'
     	WRITE(6,*)'2	area normalized to 1'
     	NORM = IRINT ('<?> ')
	IREFL = IRINT ('Include reflectivity (|A|**2 as weighing factor)? ')
     	IF (IREFL.EQ.1) THEN
!     	  WRITE(6,*)'Options. Enter: '
!     	  WRITE(6,*)'0     to use |A| as weighing factor'
!     	  WRITE(6,*)'1     to use |A|**2 as weighing factor (transmitted)'
!     	  WRITE(6,*)'2     to use 1-|A|**2 as weighing factor (absorbed )'
!     	  IUNITS = IRINT ('<?> ')
          iUnits=1
!	  IF (IUNITS.EQ.2) THEN
!     	    IFILE  = IYES ('Use another file for incoming power [ Y/N ] ? ')
!     	    IF (IFILE.EQ.1) THEN
!     	      FILEIN2 = RSTRING ('File-name ? ')
!     	      !CALL RBEAM18 (FILEIN2,RAY2,NCOL2,NPOINT,IFLAG,IERR)
!	      !IF (IERR.NE.0)	STOP	'Error in reading ray file.'
!              CALL    RBeamAnalyze (fileIn2,ncol2,npoint2,iflag,ierr)
!  	      ALLOCATE( RAY(18,NPOINT2) )
!  	      ray=0.0d0
!	      CALL RBeam18(ray2,ierr,ncol2,npoint2,fileIn2)
!     	    END IF
!	  END IF
!	  WRITE(6,*)'Scale to :'
!	  WRITE(6,*)'  0      no scaling (cm-1)'
!	  WRITE(6,*)'  1      Watt'
!	  WRITE(6,*)'  2      eV/sec.'
!	  ISCALE 	= IRINT ('<?> ')
          iScale=0
     	END IF
!#if defined (vms)
!	WRITE(6,*)'Output options :'
!	WRITE(6,*)'  [ 0 ] store histogram in a file'
!	WRITE(6,*)'  [ 1 ] plot histogram on screen ' 
!	WRITE(6,*)'  [ 2 ] both' 
!	IOUT	= IRINT ('Then ? ')
!	IF (IOUT.EQ.1.OR.IOUT.EQ.2) THEN
!	  CALL	SET_SCREEN	(' ',0,ITERM)
!	END IF
!#else 
!	  WRITE(6,*) 'Display type:'
!#if HAVE_XWINDOWS
!	  WRITE(6,*) '  [ 0 ] Xwindow'
!#endif
!	  WRITE(6,*) '  [ 1 ] Tektronix'
!	  WRITE(6,*) '  [ 2 ] Postscript file'
!	  ITERM = IRINT ('Terminal type:  ')
!#if !HAVE_XWINDOWS
!	IF (ITERM.EQ.0) THEN
!	    WRITE (*,*) 'No X Windows support. Using Postscript file'
!	    ITERM=2
!	ENDIF
!#endif
!	IF (ITERM.LT.0 .OR. ITERM.GT.2) THEN
!	    WRITE (*,*) 'Invalid device Id. Using Postscript file'
!	    ITERM=2
!	ENDIF
!#endif

!C
!C Clear the arrays
!C
!     	DO 12 I=1,100
!     	  XARRAY(I)  =   0.0
!     	  YARRAY(I)  =   0.0
!	  XTEMP(I)   =   0.0
!12     	CONTINUE
!C
!C Fill in the bins
!C
     	  XLOW	=   CENTER - WIDTH/2
     	  STEP	=   WIDTH/(NBIN - 1)
     	  XSTART	=   XLOW - STEP/2
     	DO 999 I=1,NPOINT1
	 XARG	= RAY(N_COL,I)
!C
!C Use the appropriate units for column 11, if it is selected.
!C
	 IF (N_COL.EQ.11) THEN
	   IF (XARG.EQ.0.0D0) GO TO 999
	   IF (IENER.EQ.0) THEN
	     XARG	= TWOPI/XARG*1.0E+8
	   ELSE IF (IENER.EQ.1) THEN
	     XARG	= XARG/TWOPI*TOCM
	   END IF
	 END IF
     	 IF (ILOST.EQ.0) THEN
     	  IF (RAY(10,I).LT.0.0D0) GO TO 999
     	 ELSE	IF (ILOST.EQ.2) THEN
     	  IF (RAY(10,I).GT.0.0D0) GO TO 999
      	 END IF
      	 ARG	=   (XARG - XSTART)/STEP
     	 JBIN	=   INT (ARG) + 1
      	 IF (JBIN.LT.1.OR.JBIN.GT.NBIN) GO TO 999
	   IF (IREFL.EQ.1) THEN
	     VAL  =   RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2
	     IF (NCOL.EQ.18)  &
                VAL = VAL + RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2
!	     IF (IUNITS.EQ.0) THEN
		VAL =   SQRT(VAL)
!     	     ELSE IF (IUNITS.EQ.1) THEN
!     		VAL  =   VAL
!     	     ELSE IF (IUNITS.EQ.2) THEN
!     		IF (IFILE.EQ.0) THEN
!     		  VAL  =  (1.0D0-VAL)
!     		ELSE
!	          VAL0 =   RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2
!	          IF (NCOL2.EQ.18) THEN
!     		  	VAL0 = VAL0 + RAY2(16,I)**2 + RAY2(17,I)**2 +  &
!      							RAY2(18,I)**2
!		  ENDIF
!     		  VAL  =  (VAL0-VAL)
!     		END IF
!     	     END IF
!	     IF (ISCALE.EQ.1) THEN
!     	  	VAL	=  VAL*RAY(11,I)*TOCM*1.6021892D-19/TWOPI
!	     ELSE IF (ISCALE.EQ.2) THEN
!		VAL	=  VAL*RAY(11,I)*TOCM/TWOPI
!	     END IF
	   ELSE
		VAL = 1.0D0
	   END IF
    	YARRAY(JBIN) = YARRAY(JBIN) + VAL
999	CONTINUE
!C
!C Prepare the arrays for writing
!C
     	DO 13 I=1,NBIN
     	  XARRAY(I) =   (I-1)*STEP + XLOW
13     	CONTINUE
!C
!C Write the xtemp array for primvs purposes.
!C
!#if !defined(vms)
!	  X1LOW = CENTER - 0.6125*WIDTH
!	  X1UPP = CENTER + 0.6125*WIDTH
!	  Y1LOW = 0.0
!	  XTEMP(1) = XSTART
!	  DO 51 I = 2,NBIN+1
!	    XTEMP(I) = XTEMP(I-1) + STEP
!51	  CONTINUE
!#endif

!C
!C Normalize the arrays
!C
     	  COUNTS	=   0.0
     	  YMAX		= - 1.0
     	DO 14 I=1,NBIN
     	  YMAX	=   MAX(YMAX,YARRAY(I))
     	  COUNTS=   COUNTS + YARRAY(I)
14     	CONTINUE
!C
     	 IF (NORM.NE.0) THEN
     	  IF (NORM.EQ.1) THEN
	    RNORM  =   YMAX
	    Y1UPP  = 1.2
	  END IF
     	  IF (NORM.EQ.2) THEN
	    RNORM  =   COUNTS
	    Y1UPP  = 1.2*YMAX/COUNTS
	  END IF
     	   DO 16 I=1,NBIN
     	   YARRAY(I) =   YARRAY(I)/RNORM
16     	  CONTINUE
	ELSE IF (NORM.EQ.0) THEN
	  Y1UPP = 1.2*YMAX
     	END IF
!C
!C Ready for output.
!C
!#ifdef vms
!	IF (IOUT.EQ.0.OR.IOUT.EQ.2) THEN
     	  OPEN (20,FILE='histo1.dat',STATUS='UNKNOWN') !,INITIALSIZE=5)
     	    DO 17 I=1,NBIN
     	      WRITE (20,*)	XARRAY(I),YARRAY(I)
17     	    CONTINUE
     	  CLOSE (20)
	  print *,'File (data) written to disk: histo1.dat'
     	  OPEN (21,FILE='histo1.gpl',STATUS='UNKNOWN') !,INITIALSIZE=5)
            WRITE (21,*)  "#"
            WRITE (21,*)  "# Gnuplot script for shadow3"
            WRITE (21,*)  "# Created by histo1"
            WRITE (21,*)  "#"
IF (trim(OS_NAME) == "Windows") THEN
    WRITE(21,'(A)')  'set terminal win '
ELSE
    WRITE(21,'(A)')  'set terminal x11 '
END IF
            WRITE (21,*)  "plot 'histo1.dat' using 1:2 with boxes linetype -1"
            WRITE (21,*)  "pause -1 'Press <Enter> to end graphic '"
     	  CLOSE (21)
	  print *,'File (gnuplot script) written to disk: histo1.gpl'
	  
!     	  IANSW = IYES ( 'Execute gnoplot script [Y/N ] ?')
!	  IF (IANSW == 1) THEN
!            CALL SYSTEM("gnuplot histo1.gnu")
!	  END IF 
!	END IF
!	IF (IOUT.EQ.1.OR.IOUT.EQ.2) THEN
!	  CALL TDNEWP
!	  CALL TDHIST (NBIN,XARRAY,YARRAY)
!	  CALL SET_SCREEN	(' ',1,ITERM)
!	END IF
!#else
!	  OPEN (22,FILE='histo1.dat',STATUS='UNKNOWN')
!	  WRITE(22,*) XTEMP(1), 0.0
!	  DO 53 I = 1,NBIN+1
!	    WRITE(22,*) XTEMP(I), YARRAY(I)
!	    WRITE(22,*) XTEMP(I+1), YARRAY(I)
!53	  CONTINUE
!	  WRITE(22,*) XTEMP(NBIN+1), 0.0
!	  CLOSE(22)
!
!	  OPEN (23,FILE='histo1.prm',STATUS='UNKNOWN')
!	  WRITE(23,*) '# Primvs command file to plot output of HISTO1'
!	  IF (ITERM.EQ.0) THEN
!	    WRITE(23,*) '# Initialize Xwindow display'
!	    WRITE(23,*) ' '
!	    WRITE(23,*) 'initpage(xwin)'
!	  ELSE IF (ITERM.EQ.1) THEN
!	    WRITE(23,*) '# Initialize Tektronix display'
!	    WRITE(23,*) ' '
!	    WRITE(23,*) 'initpage(tekt)'
!	  ELSE IF (ITERM.EQ.2) THEN
!	    WRITE(23,*) '# Initialize postscript file '
!	    WRITE(23,*) ' '
!	    WRITE(23,*) 'setcolor(0)'
!	    WRITE(23,*) 'initpage(ps,"histo1.ps")'
!	  ENDIF
!	  WRITE(23,*) '# Set limits on viewing window, plot, and '
!	  WRITE(23,*) '# character size.'
!	  WRITE(23,*) 'regionr(0.1,0.1,0.9,0.9,1.0)'
!	  WRITE(23,3030) X1LOW,X1UPP,Y1LOW,Y1UPP
!	  WRITE(23,*) 'color(green)'
!	  WRITE(23,*) 'scalechr(0.8)'
!	  WRITE(23,*) ' '
!	  WRITE(23,*) '# Plot histogram from histo1.dat and draw axes'
!	  WRITE(23,*) 'plotl("histo1.dat")'
!	  WRITE(23,*) 'box("bcnst",0,0,"bcnstv",0,0)'
!	  WRITE(23,*) 'closepage'
!	  WRITE(23,*) 'exit'
!
!3030    FORMAT ('xyrange(',E15.8,',',E15.8,',',E15.8,',',E15.8,')')
!
!	IFLAG = 0
!	CALL PROGPATH ('primvs', PRIMVS, IFLAG)
!	PRIMVSPATH = PRIMVS(1:IBLANK(PRIMVS)) // ' -i histo1.prm'
!	WRITE(*,*) 'Executing program: ' // 
!     $		PRIMVSPATH(1:IBLANK(PRIMVSPATH))
!#if !defined(_WIN32)
!	CALL SYSTEM (PRIMVSPATH)
!#else
!	IFLAG = 0
!	CALL RUNPRIMVS (PRIMVS(1:IBLANK(PRIMVS)),'histo1.prm', iflag)
!#endif
!#endif

!     	IANSW = IYES ( 'Another run  [ Y/N ] ?')
!     	IF (IANSW.EQ.1) GO TO 1
!     	WRITE(6,*)'Return to operating system'
!     	STOP
        IF (allocated(ray)) deallocate(ray)
        IF (allocated(ray2)) deallocate(ray2)
        IF (allocated(xArray)) deallocate(xArray)
        IF (allocated(yArray)) deallocate(yArray)
	RETURN
END SUBROUTINE Histo1

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
	!IMPLICIT        REAL*8          (A-E,G-H,O-Z)


        implicit real(kind=skr) (a-e,g-h,o-z)
        implicit integer(kind=ski)        (f,i-n)

!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!	INCLUDE		'SHADOW$INC:DIM.PAR/LIST'
	real(kind=skr)   :: PLOT
!#endif
	integer,parameter  :: MAXGS=101
     	!REAL*8		RAY(18,N_DIM),RAY2(18,N_DIM)
        real(kind=skr),dimension(:,:),allocatable :: ray,ray2
	!REAL*8		A_SQUARE(N_DIM),A2_SQUARE(N_DIM)
	!DIMENSION	XPLOT(N_DIM),YPLOT(N_DIM)
	!DIMENSION	XLOSS(N_DIM),YLOSS(N_DIM)
        real(kind=skr),dimension(:),allocatable   :: A_SQUARE,A2_SQUARE
        real(kind=skr),dimension(:),allocatable   :: XPLOT,YPLOT,XLOSS,YLOSS,weightPlot
        logical :: lExists
!C
!C Save the large arrays so as not to overflow stack (eg., on DEC ALPHA)
!C
!	SAVE		RAY, RAY2, A_SQUARE, A2_SQUARE
!     	DATA	TWOPI 	/  6.2831 85307 17958 64679 25287 D0 /
!	DATA	TOCM	/  1.239 852	D-4		     /
!	DATA	TOANGS 	/  1.239 852    D+4		     /
!	DATA    TORAD   /  0.0174 53292 51994 32957 69237 D0 /

     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
!     	real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
!     	real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
!     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 

!     	CHARACTER * 80 	FILE_IN
     	character(len=sklen) :: FILE_IN, FILE_I0, dataDir
!     	CHARACTER * 60	FILETEXT
!     	CHARACTER * 17	DATETEXT
	!CHARACTER * 80	COMMENT
     	!CHARACTER * 80	TEXT,TEXT1,TEXT2,TEXT3,TEXT4,TEXT5
     	!CHARACTER * 80	RSTRING,FILE_I0,TEXT6,TEXT7,TEXT8
     	character(len=80) :: TEXT,TEXT1,TEXT2,TEXT3,TEXT4,TEXT5
     	character(len=80) :: TEXT6,TEXT7,TEXT8
     	!CHARACTER * 20	TODAY
     	character(len=20) :: TODAY
     	character(len=80) :: COMMENT
     	!DATA	COMMENT	/'   '/
     	!DATA	TODAY	/'                    '/
     	!DATA	COMMENT	/'   '/
     	!DATA	TODAY	/'                    '/
        integer(kind=ski),parameter :: i_td=0
	!data	i_td / 0 /
     	!DIMENSION	TEST(30),R_LOW(30),R_UPP(30)
	real(kind=skr),dimension(30) :: TEST,R_LOW,R_UPP
     	!DIMENSION	X_ARRAY(100),Y_ARRAY(100),XYLIM(4)
	real(kind=skr),dimension(100) :: X_ARRAY,Y_ARRAY
	real(kind=skr),dimension(4)   :: XYLIM
     	!DIMENSION	XMEAN(30),STDEV(30),VAR(30)
	real(kind=skr),dimension(30)  :: XMEAN,STDEV,VAR
	!DIMENSION	PIXEL(MAXGS,MAXGS),XSID(MAXGS)
     	!DIMENSION	YSID(MAXGS), PIXNEW(MAXGS,MAXGS)
     	!DIMENSION	ZDATA(MAXGS*MAXGS),CVALUE(20)
	real(kind=skr),dimension(MAXGS)       :: XSID,YSID
	real(kind=skr),dimension(MAXGS,MAXGS) :: PIXEL,PIXNEW
	real(kind=skr),dimension(MAXGS*MAXGS) :: ZDATA
	real(kind=skr),dimension(20)          :: CVALUE


	!INTEGER		ICONT(10),IPLT(10)
	!integer(kind=ski),dimension(10) :: ICONT !,IPLT


!#ifndef vms
!	CHARACTER*1024	PRIMVS
!	CHARACTER*1024	PRIMVSPATH
!#endif

i22 =0 ! flag to file
!1     	CALL	ALINE	('PLOT> Input file? ',FILE_IN)
	FILE_IN = RSTRING('PLOT> Input file? ')

if (trim(file_in) == "") file_in="begin.dat"


!     	CALL	RBEAM18	(FILE_IN,RAY,NCOL,NPOINT,IFLAG,IERR)
!	IF (IERR.NE.0)	STOP	'Error in reading ray file.'

!
! reads input binary file
!

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (file_In,ncol1,npoint1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'PLOTXY: RBeamAnalyze: Error in file: '//trim(file_in)
            stop
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
	CALL RBeam18(ray,ierr,ncol1,npoint1,file_In)

	npoint = npoint1
	ncol = ncol1

!C
!    	CALL	FILEINFO  (FILE_IN)
!    	CALL	NEXTFILE  (FILETEXT,DATETEXT)
	WRITE(6,*) 'PLOT> Options --- Enter'
     	WRITE(6,*) 'PLOT>   0   for excluding the losses'
     	WRITE(6,*) 'PLOT>   1   for including only the losses'
     	WRITE(6,*) 'PLOT>   2   for including all the rays.'
     	ILOST = IRINT ('PLOT> Then ? ')
	WRITE(6,*) 'PLOT> Comment for plot [ 80 char ] ?'
	WRITE(6,*) '*******************/*******************/',&
      '*******************/*******************/'
     	READ (5,1000)	COMMENT
!C1000	FORMAT (Q,A)
1000	FORMAT (A)

	NLOSS	=   0
     	DO 11 I=1,NPOINT
     	IF (RAY(10,I).LT.0.0) NLOSS = NLOSS + 1
11      CONTINUE
100     CONTINUE
!C     	CALL	LIB$ERASE_PAGE(1,1)
!     	WRITE(6,*) 'PLOT> File read OK. Full specifications:'
!     	WRITE(6,*) '     ',FILETEXT
!     	WRITE(6,*) '     ','Was created : ',DATETEXT
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
     	R_LOW(1) =   1.0D+20
     	R_UPP(1) = - 1.0D+20
     	R_LOW(2) =   1.0D+20
     	R_UPP(2) = - 1.0D+20
     	R_LOW(3) =   1.0D+20
     	R_UPP(3) = - 1.0D+20

     	R_LOW(4) =   1.0D+20
     	R_UPP(4) = - 1.0D+20
     	R_LOW(5) =   1.0D+20
     	R_UPP(5) = - 1.0D+20
     	R_LOW(6) =   1.0D+20
     	R_UPP(6) = - 1.0D+20

     	R_LOW(11) =   1.0D+20
     	R_UPP(11) = - 1.0D+20

     	R_LOW(13) =   1.0D+20
     	R_UPP(13) = - 1.0D+20

     	R_LOW(20) =   1.0D+20
     	R_UPP(20) = - 1.0D+20

     	DO 300 I=1,NPOINT
	 IF (ILOST.EQ.0) THEN
	   IF (RAY(10,I).LT.0.0D0)   GO TO 300
	 ELSE IF (ILOST.EQ.1) THEN
     	   IF (RAY(10,I).GE.0.0D0)   GO TO 300
     	 ELSE
     	 END IF
     	TEST(1)	=   RAY(1,I)
     	TEST(2)	=   RAY(2,I)
     	TEST(3)	=   RAY(3,I)
     	TEST(4)	=   RAY(4,I)
     	TEST(5)	=   RAY(5,I)
     	TEST(6)	=   RAY(6,I)
     	TEST(11) =   RAY(11,I)
     	TEST(13) =   RAY(13,I)
     	TEST(20) =   RAY(4,I)**2 + RAY(6,I)**2
     	TEST(20) =   ABS( SQRT(TEST(20))/RAY(5,I) )
     	R_LOW(1)= MIN(R_LOW(1),TEST(1))
     	R_UPP(1)= MAX(R_UPP(1),TEST(1))
     	R_LOW(2)= MIN(R_LOW(2),TEST(2))
     	R_UPP(2)= MAX(R_UPP(2),TEST(2))
     	R_LOW(3)= MIN(R_LOW(3),TEST(3))
        R_UPP(3)= MAX(R_UPP(3),TEST(3))

     	R_LOW(4)= MIN(R_LOW(4),TEST(4))
     	R_UPP(4)= MAX(R_UPP(4),TEST(4))
     	R_LOW(5)= MIN(R_LOW(5),TEST(5))
     	R_UPP(5)= MAX(R_UPP(5),TEST(5))
     	R_LOW(6)= MIN(R_LOW(6),TEST(6))
     	R_UPP(6)= MAX(R_UPP(6),TEST(6))

     	R_LOW(11)= MIN(R_LOW(11),TEST(11))
	R_UPP(11)= MAX(R_UPP(11),TEST(11))

     	R_LOW(13)= MIN(R_LOW(13),TEST(13))
	R_UPP(13)= MAX(R_UPP(13),TEST(13))

     	R_LOW(20)= MIN(R_LOW(20),TEST(20))
	R_UPP(20)= MAX(R_UPP(20),TEST(20))
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
!#if defined (vms)
!2000	FORMAT (//,T2,'Column',T5,' Par',T10,'Minimum:',T25,'Maximum:',
!     $		T40,'Center:',T55,'St. Dev.:')
!#else
2000    FORMAT (//,T2,'Col',T5,' Par',T10,'Minimum:',T25,'Maximum:', &
                T40,'Center:',T55,'St. Dev.:')
!#endif

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
!C
!C takes care of the Optical Path Case -- since the variation is small
!C we must avoid precision problems in the plotting, so that we will be
!C plotting the OPD.
!C 
     	  IF (IX.EQ.13.OR.IY.EQ.13) THEN
     	    P_CENT = (R_LOW(13)+R_UPP(13))/2.0D0
     	    P_DEL  = R_UPP(13) - R_LOW(13)
     	    R_LOW(13) = - 0.5D0*P_DEL
     	    R_UPP(13) = + 0.5D0*P_DEL
     	  END IF
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
     	  WRITE(6,*) 'PLOT> Enter limits. '
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
!!;;         !C
!!;;         !C We ALWAYS need A**2 later on then.
!!;;         !C
!!;; 	IF (NCOL.EQ.18) THEN
!!;; 	     DO I = 1, NPOINT
!!;; 	       A_SQUARE(I)= RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2 &
!!;;       			+ RAY(16,I)**2 + RAY(17,I)**2 + RAY(18,I)**2
!!;;              END DO
!!;; 	ELSE
!!;; 	     DO I = 1, NPOINT
!!;; 	       A_SQUARE(I)= RAY(7,I)**2 + RAY(8,I)**2 + RAY(9,I)**2
!!;;              END DO
!!;; 	END IF

     	IF (IGRID.GE.1) THEN
     	!IF (IGRID.EQ.1) THEN
     	! WRITE(6,*) 'PLOT> Grid size [ Nx by Ny ].'
     	! NGX	=   IRINT ('PLOT> Nx [Default=50]: ')
     	! NGY	=   IRINT ('PLOT> Ny [Default=50]: ')
        ! IF (ngx == 0) ngx=50
        ! IF (ngy == 0) ngy=50
	!ELSE IF (IGRID.EQ.2) THEN
     	 WRITE(6,*) 'PLOT> Number of bins [Nx by Ny] to prepare the grids.'
     	 NGX	=   IRINT ('PLOT> Nx [Default=101]: ')
     	 NGY	=   IRINT ('PLOT> Ny [Default=101]: ')
         IF (ngx == 0) ngx=101
         IF (ngy == 0) ngy=101
	 IF (igrid == 2) THEN
	   NCONT	=   IRINT ('PLOT> Number of contours [Default=10] : ')
           IF (nCont == 0) nCont=10
	 END IF
!	 IPLT(2) =  IRINT  &
!      	 ('PLOT> Degree of polynomial used for joining [0=default] : ')
	 K_REFL	=   IRINT ('PLOT> Included reflectivity ? ')
!  k_act=0
  
  


!!	 IF (K_REFL.EQ.1) THEN
!!	   NORMAL 	= 0
!!	   WRITE(6,*) 'PLOT> Options : '
!!	   WRITE(6,*) '  0   Power density [J/area] reflected/' // &
!!      		'transmitted'
!!	   WRITE(6,*) '  1   Power density [J/area] absorbed'
!!	   WRITE(6,*) '  2   Local reflectivity/transmission'
!!	   K_ACT = IRINT('PLOT> Then ? ')


!!--!!	   IF (K_ACT.EQ.1.OR.K_ACT.EQ.2) THEN
!!--!!	     FILE_I0 	= RSTRING ('PLOT> File to use for Io : ')
!!--!!
!!--!!!	     CALL	RBEAM18
!!--!!!     $		(FILE_I0,RAY2,NCOL2,NPOINT2,IFLAG,IERR)
!!--!!!	     IF (IERR.NE.0)	STOP 'Error in reading Io file.'
!!--!!
!!--!!             CALL    RBeamAnalyze (FILE_I0,ncol2,npoint2,iflag,ierr)
!!--!!             IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
!!--!!               print *,'TRACE3: RBeamAnalyze: Error in file: '//trim(FILE_I0)
!!--!!                  stop 'Aborted'
!!--!!             END IF
!!--!!	     IF (NPOINT2.NE.NPOINT) STOP &
!!--!!      'Io file does not have the same number of rays as the input file.'
!!--!!
!!--!!  	     ! already allocated! ALLOCATE( RAY(18,NPOINT1) )
!!--!!  	     ray2=0.0d0
!!--!!
!!--!!	     CALL RBeam18(ray2,ierr,ncol2,npoint2,FILE_I0)
!!--!!
!!--!!
!!--!!	    IF (NCOL2.EQ.18) THEN
!!--!!	      DO 17 I = 1, NPOINT2
!!--!!	       A2_SQUARE(I)= RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2 &
!!--!!      			+ RAY2(16,I)**2 + RAY2(17,I)**2 + RAY2(18,I)**2
!!--!!17	      CONTINUE
!!--!!	    ELSE
!!--!!	      DO 18 I = 1, NPOINT2
!!--!!	       A2_SQUARE(I)= RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2
!!--!!18	      CONTINUE
!!--!!	    END IF
!!--!!	   END IF



!!++!!	   IF (K_ACT.EQ.0.OR.K_ACT.EQ.1) THEN
!!++!!     	     WRITE(6,*) 'Scaling options: '
!!++!!     	     WRITE(6,*) 'No scaling                           [ 0 ] '
!!++!!     	     WRITE(6,*) 'Scaling with user-supplied number    [ 1 ] '
!!++!!     	     WRITE(6,*) 'Auto-scaling for SR source           [ 2 ] '
!!++!!     		ISCALE = IRINT ('PLOT> Then ? ')
!!++!!     	   END IF
!!++!!	   IF (ISCALE.EQ.1) THEN
!!++!!!C     	     TPOWER	= RNUMBER ('PLOT> Scaling factor : ')
!!++!!	     WRITE(6,*) 'PLOT> Scaling factor: '
!!++!!	     READ(5,*) TPOWER
!!++!!     	     WRITE(6,*)  &
!!++!!      'We need the conversion factor from the units used in the ', &
!!++!!      'calculations to CM. e.g., if mm were used, enter .1'
!!++!!	     WRITE(6,*) 'Conversion factor: '
!!++!!	     READ(5,*) PUNITS
!!++!!!C     	     PUNITS	= RNUMBER ('Then ? ')
!!++!!!C
!!++!!!C compute beam energy
!!++!!!C
!!++!!     	     TENER = 0.0
!!++!!     	    DO 19 I=1,NPOINT
!!++!!     	      TENER = TENER + RAY(11,I)/TWOPI*TOCM*1.602D-19
!!++!!19	    CONTINUE
!!++!!     	     FACTOR = TPOWER/TENER
!!++!!     	   ELSE IF (ISCALE.EQ.2) THEN
!!++!!!C     	     ORBIT = RNUMBER ('Machine radius [ m ] ? ')
!!++!!!C     	     ENER  = RNUMBER ('Machine energy [ GeV ] ? ')
!!++!!!C     	     CURR  = RNUMBER ('Machine current[ A ] ? ')
!!++!!!C     	     ACCEPTANCE = RNUMBER ('milliradians of orbit ? ')
!!++!!	     WRITE(6,*) 'Machine radius [ m ] ?'
!!++!!	     READ(5,*) ORBIT
!!++!!	     WRITE(6,*) 'Machine energy [ GeV ] ? '
!!++!!	     READ(5,*) ENER
!!++!!	     WRITE(6,*) 'Machine current[ A ] ? '
!!++!!	     READ(5,*) CURR
!!++!!	     WRITE(6,*) 'Milliradians of orbit ? '
!!++!!	     READ(5,*) ACCEPTANCE
!!++!!     	     WRITE(6,*)  &
!!++!!      'We need the conversion factor from the units used in the ', &
!!++!!      'calculations to CM. e.g., if mm were used, enter .1'
!!++!!	     WRITE(6,*) 'Conversion factor: '
!!++!!	     READ(5,*) PUNITS
!!++!!!C     	     PUNITS	= RNUMBER ('Then ? ')
!!++!!!C
!!++!!!C Following power is in KWH. It is the total power incoming on the
!!++!!!C beamline. Notice that the power is given in Watts.
!!++!!!C
!!++!!     	     TPOWER = ENER**4/ORBIT*88.5*CURR*ACCEPTANCE/TWOPI
!!++!!     	     WRITE(6,*) 'Total power accepted: ',TPOWER
!!++!!!C
!!++!!!C Each ray carries hv energy. The scaling factor is obtained 
!!++!!!C by computing the total energy carried by the beam.
!!++!!!C
!!++!!!C compute beam energy
!!++!!!C
!!++!!     	     TENER = 0.0
!!++!!     	    DO 3037 I=1,NPOINT
!!++!!     	      TENER = TENER + RAY(11,I)/TWOPI*TOCM*1.602D-19
!!++!!3037	    CONTINUE
!!++!!     	     FACTOR = TPOWER/TENER
!!++!!     	   END IF
!!	 ELSE
!!	   WRITE(6,*) 'PLOT> Data normalization : '
!!	   WRITE(6,*) '        0   For no normalization'
!!	   WRITE(6,*) '        1   For normalized to 1'
!!	   WRITE(6,*) '        2   For normalized to total counts'
!!	   NORMAL = IRINT ('PLOT> Then ? ')
!!	 END IF




     	 ISMOOTH = IYES ('Smoothing [ Y/N ] ? ')
!!     	 iGridFile	= IYES ('Save grid file [ Y/N ] ? ')
	 iGridFile = 1
!	   OPEN(22,FILE='test',STATUS='UNKNOWN')
i22 =1
     	END IF





!C
!     	ICROSS = IRINT ('PLOT> Hairline at [ 0,0 ] ? ')
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
!C
!     	IMIRR = IRINT ('PLOT> Overlay a mirror/slit ? ')
iMirr=0
!      	IF (IMIRR.EQ.1) THEN
!      	 WRITE(6,*) 'PLOT> Mirror/Slit type: '
!      	 WRITE(6,*) '      0 .... rectangular '
!      	 WRITE(6,*) '      1 .... elliptical '
!      	 ISLIT	=   IRINT ('Then ? ')
!      	  IF (ISLIT.EQ.0) THEN
! !C     	   XS_MIN =  RNUMBER ('PLOT > X min : ')
!      	   WRITE(6,*) 'PLOT > X min : '
! 	   READ(5,*) XS_MIN
! !C     	   XS_MAX =  RNUMBER ('PLOT >   max : ')
!      	   WRITE(6,*) 'PLOT >   max : '
! 	   READ(5,*) XS_MAX
! !C     	   YS_MIN =  RNUMBER ('PLOT > Y min : ')
!      	   WRITE(6,*) 'PLOT > Y min : '
! 	   READ(5,*) YS_MIN
! !C     	   YS_MAX =  RNUMBER ('PLOT >   max : ')
!      	   WRITE(6,*) 'PLOT >   max : '
! 	   READ(5,*) YS_MAX
!      	  ELSE
! !C     	   AMAJ =  RNUMBER ('PLOT > Semi-axis along [ x ] : ')
!      	   WRITE(6,*) 'PLOT > Semi-axis along [ x ] : '
! 	   READ(5,*) AMAJ
! !C     	   AMIN =  RNUMBER ('PLOT >                 [ y ] : ')
!      	   WRITE(6,*) 'PLOT >                 [ y ] : '
! 	   READ(5,*) AMIN
! !C     	   ELLX	=  RNUMBER ('PLOT > center [ x ] : ')
!      	   WRITE(6,*) 'PLOT > center [ x ] : '
! 	   READ(5,*) ELLX
! !C     	   ELLY	=  RNUMBER ('PLOT >        [ y ] : ')
!      	   WRITE(6,*) 'PLOT >        [ y ] : '
! 	   READ(5,*) ELLY
!      	  END IF
!      	END IF
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
!C
!C User can save the TD.FIL to regenerate the plot at a later time.
!C
!#if defined(vms)
!	I_TD  = IRINT 
!     $		('PLOT> Do you want to save the TD command file ? ')
!	IF (I_TD.EQ.1) OPEN (27,FILE='TD.FIL',STATUS='NEW')
!C
!C Set up the terminal for plotting.
!C  
!	CALL	SET_SCREEN	('PLOT>',0,ITERM)
!	CALL 	TDNEWP
!	TEXT	= 'MODE NOECHO ;'
!     	CALL 	TDSET	(%REF(TEXT))
!	TEXT	= 'TICKS SIZE 0.05 ;'
!     	CALL	TDSET	(%REF(TEXT))
!	TEXT	= 'LABELS ALL OFF ;'
!     	CALL	TDSET	(%REF(TEXT))
!#else
!C
!C Check unix display and start creating primvs command file
!C
!	ITD = 0
!C	WRITE(6,*)'This will create PRIMVS input files'
!	WRITE(6,*) 'Display type:'
!#if HAVE_XWINDOWS
!	WRITE(6,*) '  [ 0 ] Xwindow'
!#endif
!	WRITE(6,*) '  [ 1 ] Tektronix'
!	WRITE(6,*) '  [ 2 ] Postscript file'
!	ITERM = IRINT ('Terminal type:  ')
!iterm = 0
!C now write out primvs command file
! 	OPEN (35,FILE='plotxy1.gnu',STATUS='UNKNOWN', FORM='FORMATTED')
! 	!WRITE(35,*) '#Primvs command file for PLOTXY'
! 	! 
! 	WRITE(35,*) '#GnuPlot command file for PLOTXY'
! 
!         WRITE(35,*) 'set xtic font "FreeSans,8" '
!         WRITE(35,*) 'set ytic font "FreeSans,8" '
!         WRITE(35,*) 'set size 1,1 '
!         WRITE(35,*) 'set origin 0,0 '
!         WRITE(35,*) 'set lmargin 5 '
!         WRITE(35,*) 'set rmargin 3 '
!         WRITE(35,*) 'set tmargin 1 '
!         WRITE(35,*) 'set bmargin 2.5 '
!         WRITE(35,*) ' '
!         WRITE(35,*) ' '
!         WRITE(35,*) 'set multiplot'


!#endif
!#if !HAVE_XWINDOWS
!	IF (ITERM.EQ.0) THEN
!	    WRITE (*,*) 'No X Windows support. Using Postscript file'
!	    ITERM=2
!	ENDIF
!#endif
!	IF (ITERM.LT.0 .OR. ITERM.GT.2) THEN
!	    WRITE (*,*) 'Invalid device Id. Using Postscript file'
!	    ITERM=2
!	ENDIF
!	IF (IANSW.EQ.-1) THEN
!#if !defined(vms)
!	  IF      (ITERM.EQ.0) THEN
!	     WRITE (35,*) '# Initialize Xwindow display'
!	     WRITE (35,*) '#'
!	     WRITE (35,*) 'initpage(xwin)'
!	    ELSE IF (ITERM.EQ.1) THEN
!	     WRITE (35,*) '# Initialize Tektronix display'
!	     WRITE (35,*) '#'
!	     WRITE (35,*) 'initpage(tekt)'
!	  ELSE IF (ITERM.EQ.2) THEN
!	     WRITE (35,*) '# Initialize Postscript file (plotxy.ps)'
!	     WRITE (35,*) '#'
!	     WRITE (35,*) 'setcolor(0)'
!	     WRITE (35,*) 'initpage(ps,"plotxy.ps")'
!	  END IF
!#endif
!	  TEXT	= 'WINDOW X 0 9.2 Y 0 9.2 ;'
!	ELSE
!#if !defined(vms)
!	  IF      (ITERM.EQ.0) THEN
!	     WRITE (35,*) '# Initialize Xwindow display'
!	     WRITE (35,*) '#'
!	     WRITE (35,*) 'initpage(xwin)'
!	  ELSE IF (ITERM.EQ.1) THEN
!	     WRITE (35,*) '# Initialize Tektronix display'
!	     WRITE (35,*) '#'
!	     WRITE (35,*) 'initpage(tekt)'
!	  ELSE IF (ITERM.EQ.2) THEN
!	     WRITE (35,*) '# Initialize Postscript file'
!	     WRITE (35,*) '#'
!	     WRITE (35,*) 'setcolor(0)'
!	     WRITE (35,*) 'initpage(ps,"plotxy.ps")'
!	  END IF
!#endif
!	  TEXT	= 'WINDOW X 0 7 Y 0 7 ;'
!	END IF
!#if defined(vms)
!     	CALL	TDSET	(%REF(TEXT))
!	WRITE	(TEXT,1010) 	X_LOW,X_UPP,Y_LOW,Y_UPP
!1010	FORMAT	('LIMITS X ',G13.6,' ',G13.6,' Y ',G13.6,' ',G13.6,' ;')
!     	CALL 	TDSET	(%REF(TEXT))
!	IF (I_TD.EQ.1) THEN
!	  WRITE (27,*)	'MODE NOECHO'
!	  WRITE (27,*)	'TICKS SIZE 0.05'
!	  WRITE (27,*)	'LABELS ALL OFF'
!	  IF (IANSW.EQ.-1) THEN
!	    WRITE (27,*) 'WINDOW X 0 9.2 Y 0 9.2'
!	  ELSE
!	    WRITE (27,*) 'WINDOW X 0 7 Y 0 7'
!	  END IF
!	  WRITE (27,*)	TEXT(1:66)
!	END IF
!#else
! 	IF (IANSW.EQ.-1) THEN
! 	  WRITE (35,*) ' '
! 	  WRITE (35,*) '# Set page and plot limits'
! !	  WRITE (35,*) '#1 regionr(0.05,0.05,0.727,0.857,1.0)'
! 	  WRITE (35,*) 'set size 0.75 0.75'
! 	  WRITE (35,*) 'set origin 0,0'
! !	  WRITE(35,3030) '#1 ',X_LOW,X_UPP,Y_LOW,Y_UPP
! !	  WRITE (35,*) '#1 scalechr(0.5)'
! !	  WRITE(35,*) '#1 color(green)'
! 	ELSE
! 	  WRITE (35,*) ' '
! 	  WRITE (35,*) '# Set page and plot limits'
! !	  WRITE(35,*) '#1 regionr(0.05,0.05,0.55,0.7,1.0)'
! 	  WRITE (35,*) 'set size 0.75,0.75'
! 	  WRITE (35,*) 'set origin 0,0'
! !	  WRITE(35,3030) '#1 ',X_LOW,X_UPP,Y_LOW,Y_UPP
! !	  WRITE (35,*) '#1 scalechr(0.5)'
! !	  WRITE(35,*) '#1 color(green)'
! 	ENDIF
!3030    FORMAT ('#1 xyrange(',SP,G15.8,',',G15.8,',',G15.8,',',G15.8,')')
!#endif

!C
!C Mark the origin.
!C
!	  XPLOT (1)	= 0.0
!	  XPLOT (2)	= 0.0
!	  YPLOT (1)	= Y_UPP
!	  YPLOT (2)	= YU_C
!#if defined(vms)
!	  CALL	JOIN	(2,XPLOT,YPLOT,I_TD,1)
!#else
!	  WRITE(35,*) ' '
!	  WRITE(35,*) '# Mark the origin '
!	  WRITE(35,4040) XPLOT(1),YPLOT(1),XPLOT(2),YPLOT(2)
!4040	  FORMAT ('#line(',G15.8,',',G15.8,',',G15.8,',',G15.8,')')
!#endif
!	  XPLOT (1)	= 0.0
!	  XPLOT (2)	= 0.0
!	  YPLOT (1)	= YL_C
!	  YPLOT (2)	= Y_LOW
!#if defined(vms)
!	  CALL  JOIN	(2,XPLOT,YPLOT,I_TD,1)
!#else
!	  WRITE(35,4040) XPLOT(1),YPLOT(1),XPLOT(2),YPLOT(2)
!#endif
!	  XPLOT (1)	= X_LOW
!	  XPLOT (2)	= XL_C
!	  YPLOT (1)	= 0.0
!	  YPLOT (2)	= 0.0
!#if defined(vms)
!	  CALL  JOIN	(2,XPLOT,YPLOT,I_TD,1)
!#else
!	  WRITE(35,4040) XPLOT(1),YPLOT(1),XPLOT(2),YPLOT(2)
!#endif
!	  XPLOT (1)	= XU_C
!	  XPLOT (2)	= X_UPP
!	  YPLOT (1)	= 0.0
!	  YPLOT (2)	= 0.0
!#if defined(vms)
!	  CALL  JOIN	(2,XPLOT,YPLOT,I_TD,1)
!#else
!	  WRITE(35,4040) XPLOT(1),YPLOT(1),XPLOT(2),YPLOT(2)
!#endif
!C
!C Before plotting, set up the contour first.
!C
!	IF (IGRID.EQ.2) THEN
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
!#if defined(vms)
!	  TEXT	= 'SET ORDER X Y DUMM ;'
!	  CALL	TDSET	(%REF(TEXT))
!	  IF (I_TD.EQ.1)	WRITE (27,*) 'SET ORDER X Y DUMM'
!#else
!	  WRITE(35,*) '#1 format(x:1,y:2)'
!
!#endif
	END IF
!C
!C Loop thru all the rays.
!C
!	WRITE(35,*) 'plot "plotxy_scatter.dat" u 1:2 w d lt -1 notitle'
!	IF (IGRID.EQ.1) THEN
!	  OPEN(36,FILE='plotxy_scatter.dat',STATUS='UNKNOWN')
!	ENDIF

     	IPASS = 0
! starts loop
9999	KX = 0
     	KY = 0
	KPLOT = 0
	KLOSS = 0
     	IPASS = IPASS + 1
     	DO 8989 I=1,NPOINT
     	  IF (IX.NE.20) XWRI	=  RAY(IX,I)
     	  IF (IY.NE.20) YWRI  	=  RAY(IY,I)
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
     	  IF (IX.EQ.13) THEN
     	      XWRI = XWRI - P_CENT
     	  ELSE IF (IY.EQ.13) THEN
     	      YWRI = YWRI - P_CENT
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
		weightPlot (KPLOT)	= A_SQUARE(I)
	   END IF
     	  ELSE IF (ILOST.EQ.1) THEN
    	   IF (RAY(10,I).LT.0.0D0) THEN
		KPLOT		= KPLOT + 1
		XPLOT (KPLOT)	= XWRI
		YPLOT (KPLOT)	= YWRI
		weightPlot (KPLOT)	= A_SQUARE(I)
	   END IF
     	  ELSE IF (ILOST.EQ.2) THEN
    	   IF (RAY(10,I).GE.0.0D0) THEN
		KPLOT		= KPLOT + 1
		XPLOT (KPLOT)	= XWRI
		YPLOT (KPLOT)	= YWRI
		weightPlot (KPLOT)	= A_SQUARE(I)
	   ELSE
		KLOSS		= KLOSS + 1
		XLOSS (KLOSS)	= XWRI
		YLOSS (KLOSS)	= YWRI
		weightPlot (KPLOT)	= A_SQUARE(I)
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
!	   IF (K_REFL.EQ.1) THEN
	   if (k_refl.eq.1) then
!	     IF (K_ACT.EQ.0) THEN
	       ARG	= A_SQUARE(I)
!	       ARG	= ARG*FACTOR*TOCM/TWOPI*1.602D-19*RAY(11,I)
!	     ELSE
!	       ARG1	= A_SQUARE(I)
!	       ARG2	= A2_SQUARE(I)
!	       IF (K_ACT.EQ.1) THEN
!	         ARG	= ARG2 - ARG1
!!C
!!C the following, r_ener, is the energy carried by each ray
!!C
!     		 R_ENER = TOCM/TWOPI*1.602D-19*RAY(11,I)
!!C
!!C after scaling and taking in account the decrease in |A|, we obtain
!!C
!	         ARG	= ARG*FACTOR*R_ENER
!	       ELSE IF (K_ACT.EQ.2) THEN
!	         ARG	= ARG1/ARG2
!	       END IF
!	     END IF
           ELSE
	     ARG	= 1.0D0
	   END IF
	   PIXEL(I_BIN,J_BIN)	= PIXEL(I_BIN,J_BIN) + ARG
!	   WRITE(22,*) XWRI,YWRI,ARG
!!!   !C	
!!!   !C Connected plot :
!!!   !C
!!!        	 ELSE IF (IGRID.EQ.1.AND.IPASS.EQ.1) THEN  ! connected, initial
!!!        	  KX = KX + 1
!!!        	  IF (IX.NE.20) XWRI	=  RAY(IX,I)
!!!        	  IF (IY.NE.20) YWRI	=  RAY(IY,I)
!!!        	  IF (IX.EQ.20) THEN
!!!        	    XWRI = ABS( SQRT( RAY(4,I)**2 + RAY(6,I)**2 )/RAY(5,I) )
!!!        	  ELSE IF (IY.EQ.20) THEN
!!!        	    YWRI = ABS( SQRT( RAY(4,I)**2 + RAY(6,I)**2 )/RAY(5,I) )
!!!        	  END IF
!!!        	  IF (IX.EQ.13) THEN
!!!        	    XWRI = XWRI - P_CENT
!!!        	  ELSE IF (IY.EQ.13) THEN
!!!        	    YWRI = YWRI - P_CENT
!!!        	  END IF
!!!             IF (ILOST.EQ.0) THEN
!!!              IF (RAY(10,I).GE.0.0D0) THEN
!!!   	     KPLOT		= KPLOT + 1
!!!   	     XPLOT (KPLOT)	= XWRI
!!!   	     YPLOT (KPLOT)	= YWRI	
!!!        	     ITST = 0
!!!        	   ELSE IF (RAY(10,I).LT.0.0D0.AND.ITST.EQ.0) THEN
!!!   	     IF (KPLOT.NE.0) THEN
!!!   !#if defined(vms)
!!!   !	       CALL JOIN	(KPLOT,XPLOT,YPLOT,I_TD,0)
!!!   !#else
!!!   !	       WRITE(36,*) XPLOT,YPLOT
!!!   !#endif
!!!    	       KPLOT	= 0
!!!        	       ITST = 1
!!!   	     END IF
!!!        	   END IF
!!!        	  ELSE IF (ILOST.EQ.1) THEN
!!!       	   IF (RAY(10,I).LT.0.0D0) THEN
!!!   	     KPLOT		= KPLOT + 1
!!!   	     XPLOT (KPLOT)	= XWRI
!!!   	     YPLOT (KPLOT)	= YWRI	
!!!        	     ITST = 0
!!!        	   ELSE IF (RAY(10,I).GE.0.0D0.AND.ITST.EQ.0) THEN
!!!   	     IF (KPLOT.NE.0) THEN
!!!   !#if defined(vms)
!!!   !	       CALL JOIN	(KPLOT,XPLOT,YPLOT,I_TD,0)
!!!   !#else
!!!   !	       WRITE(36,*) XPLOT,YPLOT
!!!   !#endif
!!!    	       KPLOT	= 0
!!!        	       ITST = 1
!!!   	     END IF
!!!        	   END IF
!!!        	  ELSE IF (ILOST.EQ.2) THEN
!!!   	     KPLOT		= KPLOT + 1
!!!   	     XPLOT (KPLOT)	= XWRI
!!!   	     YPLOT (KPLOT)	= YWRI	
!!!        	  END IF
!!!        	  IF (KX.EQ.NGX) THEN
!!!   	     IF (KPLOT.NE.0) THEN
!!!   !#if defined(vms)
!!!   !	       CALL JOIN	(KPLOT,XPLOT,YPLOT,I_TD,0)
!!!   !#else
!!!   !	       WRITE(36,*) XPLOT,YPLOT
!!!   !#endif
!!!    	       KPLOT	= 0
!!!   	     END IF
!!!   	     KX	= 0
!!!        	  END IF
!!!        	 ELSE IF (IGRID.EQ.1.AND.IPASS.EQ.2) THEN !connected  (more)
!!!        	  KY = KY + 1
!!!        	  INDEX = NGX*(KY-1)+KX+1
!!!        	  IF (IX.NE.20) XWRI	=  RAY(IX,INDEX)
!!!        	  IF (IY.NE.20) YWRI	=  RAY(IY,INDEX)
!!!        	  IF (IX.EQ.20) THEN
!!!        	    XWRI = ABS( SQRT( RAY(4,I)**2 + RAY(6,I)**2 )/RAY(5,I) )
!!!        	  ELSE IF (IY.EQ.20) THEN
!!!        	    YWRI = ABS( SQRT( RAY(4,I)**2 + RAY(6,I)**2 )/RAY(5,I) )
!!!        	  END IF
!!!        	  IF (IX.EQ.13) THEN
!!!        	    XWRI = XWRI - P_CENT
!!!        	  ELSE IF (IY.EQ.13) THEN
!!!        	    YWRI = YWRI - P_CENT
!!!        	  END IF
!!!             IF (ILOST.EQ.0) THEN
!!!              IF (RAY(10,INDEX).GE.0.0D0) THEN
!!!   	     KPLOT		= KPLOT + 1
!!!   	     XPLOT (KPLOT)	= XWRI
!!!   	     YPLOT (KPLOT)	= YWRI	
!!!        	     ITST = 0
!!!        	   ELSE IF (RAY(10,INDEX).LT.0.0D0.AND.ITST.EQ.0) THEN
!!!   	     IF (KPLOT.NE.0) THEN
!!!   !#if defined(vms)
!!!   !	       CALL JOIN	(KPLOT,XPLOT,YPLOT,I_TD,0)
!!!   !#else
!!!   !	       WRITE(36,*) XPLOT,YPLOT
!!!   !#endif
!!!    	       KPLOT	= 0
!!!        	       ITST = 1
!!!   	     END IF
!!!        	   END IF
!!!        	  ELSE IF (ILOST.EQ.1) THEN
!!!       	   IF (RAY(10,INDEX).LT.0.0D0) THEN
!!!   	     KPLOT		= KPLOT + 1
!!!   	     XPLOT (KPLOT)	= XWRI
!!!   	     YPLOT (KPLOT)	= YWRI	
!!!        	     ITST = 0
!!!        	   ELSE IF (RAY(10,INDEX).GE.0.0D0.AND.ITST.EQ.0) THEN
!!!   	     IF (KPLOT.NE.0) THEN
!!!   !#if defined(vms)
!!!   !	       CALL JOIN	(KPLOT,XPLOT,YPLOT,I_TD,0)
!!!   !#else
!!!   !	       WRITE(36,*) XPLOT,YPLOT
!!!   !#endif
!!!    	       KPLOT	= 0
!!!        	       ITST = 1
!!!   	     END IF
!!!        	   END IF
!!!        	  ELSE IF (ILOST.EQ.2) THEN
!!!   	     KPLOT		= KPLOT + 1
!!!   	     XPLOT (KPLOT)	= XWRI
!!!   	     YPLOT (KPLOT)	= YWRI	
!!!        	  END IF
!!!        	  IF (KY.EQ.NGY) THEN
!!!        	     KX = KX + 1
!!!   	     IF (KPLOT.NE.0) THEN
!!!   !#if defined(vms)
!!!   !	       CALL JOIN	(KPLOT,XPLOT,YPLOT,I_TD,0)
!!!   !#else
!!!   !	       WRITE(36,*) XPLOT,YPLOT
!!!   !#endif
!!!    	       KPLOT	= 0
!!!   	     END IF
!!!      	     KY = 0
!!!        	  END IF
     	 END IF  ! end scatter OR connected OR contour
8989   	CONTINUE
     	IF (IGRID.EQ.1.AND.IPASS.EQ.1) GO TO 9999
! loop ended





	IF (IGRID.EQ.0)	THEN
!#if defined(vms)
!	  IF (ITERM.EQ.3) 	CALL SET_COLOR (9)
!	  IF (KPLOT.NE.0) CALL TDPLOT (KPLOT,XPLOT,YPLOT)
!#else
!	  WRITE(35,*) ' '
!	  WRITE(35,*) '# Draw axes and plot scatter plot '
!	  WRITE(35,*) '#1 symbol(1)'
!	  WRITE(35,*) '#1 box("bcnst",0,0,"bcnstv",0,0)'
!	  WRITE(35,*) '#1 Good rays are shown in green'
!	  WRITE(35,*) '#1 color(green)'
!	  WRITE(35,*) '#1 plotp("scatter.dat")'
!!	  WRITE(35,*) 'plot "plotxy_scatter.dat" u 1:2 w d lt -1 notitle'
!	  WRITE(35,*) '#1 color(green)'

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
!#endif
!	  IF (I_TD.EQ.1) THEN
!	    DO I = 1, KPLOT
!	      IF (MOD(I,1000).EQ.0)	WRITE	(27,*)	'PLOT'
!	      WRITE 	(27,*)	XPLOT(I),YPLOT(I)
!	    END DO
!	    WRITE	(27,*)	'PLOT'
!	  END IF
	  IF (ILOST.EQ.2) THEN
!#if defined(vms)
!	   IF (ITERM.EQ.3) 	CALL SET_COLOR (6)
!	   IF (KLOSS.GT.0)	CALL TDPLOT (KLOSS,XLOSS,YLOSS)
!#else
	   IF (KLOSS.GT.0) THEN
	     OPEN(40,FILE='plotxy_lost.dat',STATUS='UNKNOWN')
!	     WRITE(35,*) '# Lost rays are shown in red'
!	     WRITE(35,*) 'color(red)'
!	     WRITE(35,*) 'plotp("lost.dat")'
!	     WRITE(35,*) 'color(green)'
	     DO 3032 I = 1,KLOSS
	       WRITE(40,*) XLOSS(I),YLOSS(I)
3032	     CONTINUE
	   END IF
!#endif
!	   IF (I_TD.EQ.1) THEN
!	    DO I = 1, KLOSS
!	      IF (MOD(I,1000).EQ.0)	WRITE	(27,*)	'PLOT'
!	      WRITE 	(27,*)	XLOSS(I),YLOSS(I)
!	    END DO
!	    WRITE	(27,*)	'PLOT'
!	   END IF
	  END IF !iLost
!#if defined(vms)
!	  IF (ITERM.EQ.3) 	CALL SET_COLOR (1)
!#endif
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
!print *,'<><><> iGridFile: ',iGridFile
     	IF (iGridFile.EQ.1) THEN
!print *,'<><><> opening plotxy_grid.dat'
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
	  IF (NORMAL.EQ.1) THEN
	    DO 37 I = 1,NGX
	      DO 38 J = 1,NGY
	        PIXEL(I,J)	= PIXEL(I,J)/ZMAX
38	      CONTINUE
37	    CONTINUE
	    ZMIN	= ZMIN/ZMAX
	    ZMAX	= 1.0D0
	  ELSE IF (NORMAL.EQ.2) THEN
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
!! THIS IS TOPDRAWER???? srio!
	  !!CALL	CONTOUR(XSID,YSID,ZDATA,XYLIM,NGX,NGY,ICONT,CVALUE,IPLT)
!#ifndef vms
!	  WRITE(35,*) '#1 color(green)'

!! 	ELSE IF (IGRID.EQ.1) THEN
!! 	  WRITE(35,*) ' '
!! 	  WRITE(35,*) '# Draw axes and connected plot '
!! !          WRITE(35,*) '#1 symbol(1)'
!! !	  WRITE(35,*) '#1 box("bcnst",0,0,"bcnstv",0,0)'
!! !	  WRITE(35,*) '#1 plotl("scatter.dat")'
!! !#endif
	END IF
!C
!C Overlay the slit or mirror.
!C
!	IF (IMIRR.EQ.1) THEN
!     	  IF (ISLIT.EQ.0) THEN
!	   XPLOT(1)	= XS_MIN
!	   YPLOT(1)	= YS_MIN
!	   XPLOT(2)	= XS_MIN
!	   YPLOT(2)	= YS_MAX
!	   XPLOT(3)	= XS_MAX
!	   YPLOT(3)	= YS_MAX
!	   XPLOT(4)	= XS_MAX
!	   YPLOT(4)	= YS_MIN
!	   XPLOT(5)	= XS_MIN
!	   YPLOT(5)	= YS_MIN
!!#if defined(vms)
!!	   CALL		JOIN	(5,XPLOT,YPLOT,I_TD,1)
!!#else
!	   WRITE(35,*) ' '
!	   WRITE(35,*) '# Overlay the slit or mirror.'
!!	   WRITE(35,4040) '#1 ',XS_MIN,YS_MIN,XS_MIN,YS_MAX
!!	   WRITE(35,4040) '#1 ',XS_MIN,YS_MAX,XS_MAX,YS_MAX
!!	   WRITE(35,4040) '#1 ',XS_MAX,YS_MAX,XS_MAX,YS_MIN
!!	   WRITE(35,4040) '#1 ',XS_MAX,YS_MIN,XS_MIN,YS_MIN
!!#endif
!     	  ELSE
!!#if !defined(vms)
!!	    OPEN(39,FILE='ellipse',STATUS='UNKNOWN')
!!	    WRITE(35,*) ' '
!!	    WRITE(35,*) '# Overlay the slit or mirror.'
!!	    WRITE(35,*) 'plotl("ellipse")'
!!#endif
!     	    DO 46 I=1,61
!     	    ARG = 6*(I-1)
!     	    XPLOT(I) = ELLX + AMAJ*COS(TORAD*ARG)
!     	    YPLOT(I) = ELLY + AMIN*SIN(TORAD*ARG)
!!#if !defined(vms)
!!	    WRITE(39,*) XPLOT(I),YPLOT(I)
!!#endif
!46	    CONTINUE
!!#if defined(vms)
!!	   CALL	JOIN	(61,XPLOT,YPLOT,I_TD,0)
!!#endif
!     	  END IF
!     	END IF



!C
!C Now the histogram.
!C
xplot=0
yplot=0
     	IF (IANSW.EQ.-1) GO TO 5001
     	NORM = 0
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
!#if defined(vms)
!     	  CALL TDSET	(%REF('WINDOW X 0 7 Y 7.2 9.2; '))
!	  WRITE	(TEXT,1020)	X_LOW,X_UPP
!1020	  FORMAT ('LIMITS X ',G13.6,' ',G13.6,'; ')
!	  CALL TDSET	(%REF(TEXT))
!#else
!	  WRITE(35,*) ' '
!	  WRITE(35,*) '# Define plotting area for top histogram '
!	  WRITE(35,*) '#1 regionr(0.05,0.70,0.55,0.927,0.3077)'
!	  WRITE(35,*) 'set size 0.75,0.25'
!	  WRITE(35,*) 'set origin 0,0.75'
!	  WRITE(35,*) 'set noxtics'
!	  WRITE(35,*) 'set bmargin 0'

print *,'<><> opening : plotxy_histtop.dat'
	  OPEN(37,FILE='plotxy_histtop.dat',STATUS='UNKNOWN')
!#endif
	X1UPP = -1.0D20
     	DO 56 I=1,N_BIN
     	  XPLOT(I) = X_ARRAY(I)
     	  YPLOT(I) = Y_ARRAY(I)
	  X1UPP = MAX(X1UPP,Y_ARRAY(I))
56	CONTINUE
	X1UPP = X1UPP*1.1
!#if defined(vms)
!	  CALL	TDHIST  (N_BIN,XPLOT,YPLOT)
!#else
	  X1LOW = 0.0D0
!	  WRITE(35,3030) X_LOW,X_UPP,X1LOW,X1UPP
!	  WRITE(35,*) 'plot "plotxy_histtop.dat" u 1:2 with lines lt -1 notitle'
!	  WRITE(35,*) '#1 plotl("plotxy_histtop.dat")'
!	  WRITE(35,*) '#1 box("bcst",0,0,"bcnstv",0,0)'
!	  WRITE(35,*) '#1 label("","","',FILETEXT,'")'
	  WRITE(37,*) XSTART,0.0
	  DO 3035 I = 1,N_BIN
	    WRITE(37,*) X_ARRAY(I)-STEP/2, Y_ARRAY(I)
	    WRITE(37,*) X_ARRAY(I)+STEP/2, Y_ARRAY(I)
3035	  CONTINUE
	  WRITE(37,*) X_ARRAY(N_BIN)+STEP/2, 0.0
!#endif
!	  IF (I_TD.EQ.1) THEN
!	    WRITE	(27,*)	'WINDOW X 0 7 Y 7.2 9.2'
!	    WRITE	(27,*)	TEXT(1:36)
!	      DO 3042 I = 1, N_BIN
!		WRITE	(27,*)	XPLOT(I),YPLOT(I)
!3042	      CONTINUE
!	    WRITE	(27,*)	'HIST'
!	  END IF
     	ELSE
!#if defined(vms)
!     	  CALL TDSET	(%REF('WINDOW X 7.2 9.2 Y 0 7; '))
!	  WRITE	(TEXT,1030)	Y_LOW,Y_UPP
!1030	  FORMAT ('LIMITS Y ',G13.6,' ',G13.6,'; ')
!	  CALL TDSET	(%REF(TEXT))
!#else
!	  WRITE(35,*) ' '
!	  WRITE(35,*) '# Define plotting area for side histogram'
!!	  WRITE(35,*) '#1 regionr(0.55,0.05,0.727,0.7,3.25)'
!	  WRITE(35,*) 'set size 0.25,0.75'
!	  WRITE(35,*) 'set origin 0.75,0'
!	  WRITE(35,*) 'set noytics'
!	  WRITE(35,*) 'set xtics font "FreeSans,8" mirror rotate by -90'
!	  WRITE(35,*) 'set bmargin 2.5'
!	  WRITE(35,*) 'set lmargin 0'
!#endif
	XMAX	= 0
	DO 3041 I=1,N_BIN
	  XMAX	= MAX(XMAX,Y_ARRAY(I))
3041	CONTINUE
!#if defined(vms)
!	WRITE	(TEXT1,1031)	XMAX*1.1
!1031	FORMAT	('LIMIT X 0 ',G13.6,'; ')
!	CALL TDSET	(%REF(TEXT1))
!#else
	Y1LOW = 0.0
	Y1UPP = XMAX*1.1
!	WRITE(35,3030) Y1LOW,Y1UPP,Y_LOW,Y_UPP
!	WRITE(35,*) 'plot "plotxy_histside.dat" u 1:2 with lines lt -1 notitle'
!	WRITE(35,*) '#1 plotl("histside.dat")'
!	WRITE(35,*) '#1 box("bcnst",0,0,"bcst",0,0)'
print *,'<><> opening : plotxy_histside.dat'
	OPEN(38,FILE='plotxy_histside.dat',STATUS='UNKNOWN')
!#endif
!	IF (I_TD.EQ.1)	THEN
!	  WRITE	(27,*)	'WINDOW X 7.2 9.2 Y 0 7'
!	  WRITE (27,*)	TEXT(1:36)
!	  WRITE (27,*)	TEXT1(1:23)
!	END IF
	XPLOT(1)	= X_ARRAY(1) - STEP/2
	YPLOT(1)	= 0.0
	XPLOT(2)	= X_ARRAY(1) - STEP/2
	YPLOT(2)	= Y_ARRAY(1)
!#if defined(vms)
!	CALL JOIN	(2,YPLOT,XPLOT,I_TD,1)
!#else
	WRITE(38,*) YPLOT(1),XPLOT(1)
	WRITE(38,*) YPLOT(2),XPLOT(2)
!#endif
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
!#if defined(vms)
!	 CALL	JOIN	(3,YPLOT,XPLOT,I_TD,1)
!#else
	 WRITE(38,*) YPLOT(1),XPLOT(1)
	 WRITE(38,*) YPLOT(2),XPLOT(2)
	 WRITE(38,*) YPLOT(3),XPLOT(3)
!#endif
59	CONTINUE
     	END IF
47	CONTINUE

5001	CONTINUE
!#if defined(vms)
!	CALL TDSET	(%REF('WINDOW X 0 13 Y 0 10; '))
!	TEXT	= FILETEXT//'; '
!	CALL TDTSET	(2.0,0,0,0)
!     	CALL TDTITL	(%REF(TEXT),0.2,9.6)
!	TEXT	= DATETEXT//'; '
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT),9.5,9.6)
!	TEXT	= COMMENT(1:20)//'; '
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT),9.5,8.9)
!     	IF (LCOMM.GT.20) THEN
!	  TEXT	= COMMENT(21:40)//'; '
!	  CALL TDTSET	(2.0,0,0,0)
!	  CALL TDTITL	(%REF(TEXT),9.5,8.4)
!	  IF (LCOMM.GT.40) THEN
!	    TEXT	= COMMENT(41:60)//'; '
!	    CALL TDTSET	(2.0,0,0,0)
!	    CALL TDTITL	(%REF(TEXT),9.5,7.9)
!	    IF (LCOMM.GT.60) THEN
!	      TEXT	= COMMENT(61:80)//'; '
!	      CALL TDTSET	(2.0,0,0,0)
!	      CALL TDTITL (%REF(TEXT),9.5,7.4)
!	    END IF
!	  END IF
!     	END IF
!     	CALL	DATE	(TODAY(1:9))
!     	CALL	TIME	(TODAY(12:20))
!	CALL TDSET	(%REF('ORDER X Y ; '))
!	CALL TDSET	(%REF('AXES ALL OFF; '))
!     	CALL TDSET	(%REF('LIMITS X 0 13 Y 0 10; '))
!	IF (I_TD.EQ.1) THEN
!	  WRITE	(27,*)  'WINDOW X 0 13 Y 0 10'
!	  WRITE	(27,*)  'SET TITLE SIZE -2'
!	  WRITE	(27,*)  'TITLE 0.2 9.6 '''//FILETEXT//''''
!	  WRITE	(27,*)  'TITLE 9.5 9.6 '''//DATETEXT//''''
!	  WRITE	(27,*)  'TITLE 9.5 8.9 '''//COMMENT(1:20)//''''
!	  IF (LCOMM.GT.20) THEN
!	    WRITE	(27,*)  'TITLE 9.5 8.4 '''//COMMENT(21:40)//''''
!	    IF (LCOMM.GT.40)
!     $	    WRITE	(27,*)  'TITLE 9.5 7.9 '''//COMMENT(41:60)//''''
!	    IF (LCOMM.GT.60)
!     $	    WRITE	(27,*)  'TITLE 9.5 7.4 '''//COMMENT(61:80)//''''
!	  END IF
!	  WRITE	(27,*)  'ORDER X Y'
!	  WRITE	(27,*)  'AXIS ALL OFF'
!	  WRITE	(27,*)  'LIMITS X 0 13 Y 0 10'
!	END IF
!     	XPLOT(1)	= 9.3
!	YPLOT(1)	= 0
!     	XPLOT(2)	= 9.3
!	YPLOT(2)	= 7
!     	XPLOT(3)	= 13
!	YPLOT(3)	= 7
!     	XPLOT(4)	= 13
!	YPLOT(4)	= 0
!     	XPLOT(5)	= 9.3
!	YPLOT(5)	= 0
!	CALL JOIN	(5,XPLOT,YPLOT,I_TD,1)
!     	XPLOT(1)	= 9.3
!	YPLOT(1)	= 6.4
!     	XPLOT(2)	= 13
!	YPLOT(2)	= 6.4
!	CALL JOIN	(2,XPLOT,YPLOT,I_TD,1)
!	TEXT		= TODAY//'; '
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT),9.5,6.7)
!     	WHAT	=   X_UPP-X_LOW
!	WRITE (TEXT,1050) WHAT
!1050	FORMAT	('H Length ',G12.5,'; ')
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT),9.5,6.1)
!     	WHAT	=   (X_UPP+X_LOW)/2
!	WRITE (TEXT1,1060) WHAT
!1060	FORMAT	('H center ',G12.5,'; ')
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT1),9.5,5.7)
!     	WHAT	=   (Y_UPP-Y_LOW)
!	WRITE (TEXT2,1070) WHAT
!1070	FORMAT	('V Length ',G12.5,'; ')
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT2),9.5,5.3)
!     	WHAT	=   (Y_UPP+Y_LOW)/2
!	WRITE (TEXT3,1080) WHAT
!1080	FORMAT	('V center ',G12.5,'; ')
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT3),9.5,4.9)
!	IF (I_TD.EQ.1) THEN
!	  WRITE	(27,*)	'TITLE 9.5 6.7 '''//TODAY//''''
!	  WRITE (27,*)	'SET TITLE SIZE 2'
!	  WRITE (27,*)	'TITLE 9.5 6.1 '''//TEXT(1:21)//''''
!	  WRITE (27,*)	'TITLE 9.5 5.7 '''//TEXT1(1:21)//''''
!	  WRITE (27,*)	'TITLE 9.5 5.3 '''//TEXT2(1:21)//''''
!	  WRITE (27,*)	'TITLE 9.5 4.9 '''//TEXT3(1:21)//''''
!	END IF
!	XPLOT(1)	= 9.3
!	YPLOT(1)	= 4.5
!	XPLOT(2)	= 13
!	YPLOT(2)	= 4.5
!	CALL JOIN	(2,XPLOT,YPLOT,I_TD,1)
!     	IF (IPLOT.EQ.0) THEN
!	  TEXT(1:14)	= 'AUTOSCALING ; '
!     	ELSE IF (IPLOT.EQ.1) THEN
!	  TEXT(1:14)	= 'CARTESIAN   ; '
!     	ELSE
!	  TEXT(1:14)	= 'EXTERNAL    ; '
!     	END IF
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT),9.5,4.2)
!	XPLOT(1)	= 9.3
!	YPLOT(1)	= 3.9
!	XPLOT(2)	= 13
!	YPLOT(2)	= 3.9
!	CALL JOIN	(2,XPLOT,YPLOT,I_TD,1)
     	IF (ILOST.EQ.0) THEN
	  TEXT1	= '--GOOD ONLY'
     	ELSE IF (ILOST.EQ.1) THEN
	  TEXT1	= '--LOST ONLY'
     	ELSE
	  TEXT1	= '--ALL RAYS'
     	END IF
!	CALL TDTSET	(2.0,0,0,0)
!	CALL TDTITL	(%REF(TEXT1),9.5,3.6)
!	XPLOT(1)	= 9.3
!	YPLOT(1)	= 3.3
!	XPLOT(2)	= 13
!	YPLOT(2)	= 3.3
!	CALL JOIN	(2,XPLOT,YPLOT,I_TD,1)
	WRITE (TEXT2,11001)	NPOINT
11001	FORMAT	('TOT  = ',I7.1)
!	CALL TDTSET	(2.0,0,0,0)
!     	CALL TDTITL	(%REF(TEXT2),9.5,3.0)
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

!	CALL TDTSET	(2.0,0,0,0)
!     	CALL TDTITL	(%REF(TEXT3),9.5,2.6)
!	XPLOT(1)	= 9.3
!	YPLOT(1)	= 2.3
!	XPLOT(2)	= 13
!	YPLOT(2)	= 2.3
!	CALL JOIN	(2,XPLOT,YPLOT,I_TD,1)
!	WRITE (TEXT4,1120)	IX
!1120	FORMAT	('Horizontal: ',I7.1,'; ')
!	CALL TDTSET	(2.0,0,0,0)
!     	CALL TDTITL	(%REF(TEXT4),9.5,2.0)
!	WRITE (TEXT5,1130)	IY
!1130	FORMAT	('Vertical:   ',I7.1,'; ')
!	CALL TDTSET	(2.0,0,0,0)
!     	CALL TDTITL	(%REF(TEXT5),9.5,1.6)
!	XPLOT(1)	= 9.3
!	YPLOT(1)	= 1.3
!	XPLOT(2)	= 13
!	YPLOT(2)	= 1.3
!	CALL JOIN	(2,XPLOT,YPLOT,I_TD,1)
!	IF (IGRID.EQ.2) THEN
!	  TEXT6	= 'Contour Values : ; '
!	  CALL	TDTSET	(2.0,0,0,0)
!	  CALL	TDTITL	(%REF(TEXT6),9.5,1.0)
!	  WRITE	(TEXT7,1140)	NCONT,CVALUE(NCONT)
!1140	  FORMAT	('  ',I2,' -- ',G12.5,'; ')
!	  CALL	TDTSET	(2.0,0,0,0)
!	  CALL	TDTITL	(%REF(TEXT7),9.5,0.65)
!	  WRITE	(TEXT8,1140)	1,CVALUE(1)
!	  CALL	TDTSET	(2.0,0,0,0)
!	  CALL	TDTITL	(%REF(TEXT8),9.5,0.3)
!	END IF
!	CALL TDEND
!	CALL	SET_SCREEN	(' ',1,ITERM)
!	IF (I_TD.EQ.1) THEN
!	  WRITE (27,*)	'TITLE 9.5 4.2 '''//TEXT(1:11)//''''
!	  WRITE (27,*)	'TITLE 9.5 3.6 '''//TEXT1(1:11)//''''
!	  WRITE (27,*)	'TITLE 9.5 3.0 '''//TEXT2(1:14)//''''
!	  WRITE (27,*)	'TITLE 9.5 2.6 '''//TEXT3(1:14)//''''
!	  WRITE (27,*)	'TITLE 9.5 2.0 '''//TEXT4(1:19)//''''
!	  WRITE (27,*)	'TITLE 9.5 1.6 '''//TEXT5(1:19)//''''
!	  IF (IGRID.EQ.2) THEN
!	    WRITE (27,*)	'TITLE 9.5 1.0 '''//TEXT6(1:15)//''''
!	    WRITE (27,*)	'TITLE 9.5 0.65 '''//TEXT7(1:20)//''''
!	    WRITE (27,*)	'TITLE 9.5 0.3 '''//TEXT8(1:20)//''''
!	  END IF
!	  WRITE (27,*)  'END'
!	END IF
!#else
!	WRITE(35,*) ' '
!	WRITE(35,*) '# Fill in some useful information '
!	WRITE(35,*) '#1 regionp(0.73,0.05,1.0,0.7)'
!	WRITE(35,*) '#1 xyrange(0.6,1.0,0.1,1.0)'
!	WRITE(35,*) '#1 scalechr(0.7)'
!	WRITE(35,*) '#1 box("bc",0,0,"bc",0,0)'
!	WRITE(35,*) '#1 line(0.6,0.93,1.0,0.93)'
!	WRITE(35,*) '#1 line(0.6,0.72,1.0,0.72)'
!	WRITE(35,*) '#1 line(0.6,0.65,1.0,0.65)'
!	WRITE(35,*) '#1 line(0.6,0.55,1.0,0.55)'
!	WRITE(35,*) '#1 line(0.6,0.40,1.0,0.40)'
!	WRITE(35,*) '#1 line(0.6,0.28,1.0,0.28)'
!#if !defined rs6000 && !defined(F2C) && !defined(G77)
!	CALL DATE(TODAY(1:9))
!#endif
!#if !defined(F2C) && !defined(G77)
!	CALL TIME(TODAY(12:20))
!#endif
!	WRITE(35,*) '#1 text("t",-1.5,0.05,0,"',TODAY(1:16),'")'
!	WRITE(35,3050) X_UPP-X_LOW
!3050	FORMAT	('#1 gtext(0.61,0.90,0,0,0,"H Length ',G10.5,'")')
!	WRITE(35,3060) (X_UPP+X_LOW)/2
!3060	FORMAT	('#1 gtext(0.61,0.85,0,0,0,"H Center ',G10.5,'")')
!	WRITE(35,3070) Y_UPP-Y_LOW
!3070	FORMAT	('#1 gtext(0.61,0.80,0,0,0,"V Length ',G10.5,'")')
!	WRITE(35,3080) (Y_UPP+Y_LOW)/2
!3080	FORMAT	('#1 gtext(0.61,0.75,0,0,0,"V Center ',G10.5,'")')
	IF (IPLOT.EQ.0) THEN
	  TEXT(1:19) = 'AUTOSCALING         '
	ELSE IF (IPLOT.EQ.1) THEN
	  TEXT(1:19) = 'CARTESIAN           '
	ELSE
	  TEXT(1:19) = 'EXTERNAL            '
	END IF
!	WRITE(35,*) '#1 gtext(0.61,0.68,0,0,0,"',TEXT(1:19),'")'
	IF (ILOST.EQ.0) THEN
	  TEXT(1:19) = '- good only         '
	ELSE IF (ILOST.EQ.1) THEN
	  TEXT(1:19) = '- lost only         '
	ELSE
	  TEXT(1:19) = '- all rays          '
	END IF
!	WRITE(35,*) '#1 gtext(0.61,0.58,0,0,0,"',TEXT(1:19),'")'
!	WRITE(35,3090) NPOINT
!3090	FORMAT ('#1 gtext(0.61,0.48,0,0,0,"Total = ',I7.1,'")')
!	IF (ILOST.EQ.2) WRITE(35,*) '#1 color(red)'
!	WRITE(35,3110) NLOSS
!3110	FORMAT ('#1 gtext(0.61,0.43,0,0,0,"Lost = ',I7.1,'")')
!	IF (ILOST.EQ.2) WRITE(35,*) '#1 color(green)'
!	WRITE(35,3120) IX
!3120	FORMAT ('#1 gtext(0.61,0.35,0,0,0,"Horizontal = ',I7.1,'")')
!	WRITE(35,3130) IY
!3130	FORMAT ('#1 gtext(0.61,0.30,0,0,0,"Vertical = ',I7.1,'")')
	IF (IGRID.EQ.2) THEN
!	  WRITE(35,*) '#1 gtext(0.61,0.25,0,0,0,"Contour Values :")'
!	  WRITE(35,3140) NCONT,CVALUE(NCONT)
!3140	  FORMAT ('#1 gtext(0.61,0.20,0,0,0,"',I2,' -- ',G15.8,'")')
!	  WRITE(35,3150) 1,CVALUE(1)
!3150	  FORMAT ('#1 gtext(0.61,0.15,0,0,0,"',I2,' -- ',G15.8,'")')
	END IF
!C       IF (IANSW.EQ.-1) THEN
!	  WRITE(35,*) '#1 regionp(0.727,0.7,1,1)'
!	  WRITE(35,*) '#1 text("t",-4,0,0,"',COMMENT(1:20),'")'
!	  WRITE(35,*) '#1 text("t",-6,0,0,"',COMMENT(21:40),'")'
!	  WRITE(35,*) '#1 text("t",-8,0,0,"',COMMENT(41:60),'")'
!	  WRITE(35,*) '#1 text("t",-10,0,0,"',COMMENT(61:80),'")'
!	  WRITE(35,*) '#1 text("b",-1.0,0,0,"Prepared:")'
!C  WRITE(35,*) 'regionp(0.11,0.72,0.65,1)'
!C  WRITE(35,*) 'text("t",-1,0,0,"',FILETEXT,'")'
!CELSE
!C  WRITE(35,*) 'subpage(2)'
!C  WRITE(35,*) 'regionp(0.55,0,1,0.5)'
!C  WRITE(35,*) 'text("t",-1,0,0,"',COMMENT(1:20),'")'
!C  WRITE(35,*) 'text("t",-3,0,0,"',COMMENT(21:40),'")'
!C  WRITE(35,*) 'text("t",-5,0,0,"',COMMENT(41:60),'")'
!C  WRITE(35,*) 'text("t",-7,0,0,"',COMMENT(61:80),'")'
!C  WRITE(35,*) 'text("b",-1.0,0,0,"Prepared:")'
!CEND IF


	IF (IANSW.EQ.-1) THEN
!	  WRITE(35,*) '#1 regionp(0.11,0.72,0.65,1)'
!	  WRITE(35,*) '#1 text("t",-2,0,0,"',FILETEXT,'")'
	END IF
!	WRITE(35,*) '#1 closepage'
!	WRITE(35,*) '#1 exit'
!	WRITE(35,*) 'unset multiplot'
!	WRITE(35,*) "pause -1 'Press <Enter> to end graphic ' "
!	CLOSE(35)
!	print *,'File written to disk: plotxy.gnu'
	CLOSE(37)
	print *,'File written to disk: plotxy_histtop.dat'
	CLOSE(38)
	print *,'File written to disk: plotxy_histside.dat'
!	CLOSE(39)
!	print *,'File written to disk: 39'
        IF (KLOSS.GT.0) THEN
	  CLOSE(40)
	  print *,'File written to disk: plotxy_lost.dat'
        END IF
!	IF (i22 == 1) THEN
!	  CLOSE(22)
!	  print *,'File written to disk: test (22)'
!        END IF


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
    END IF
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  '                                              '
IF (trim(OS_NAME) == "Windows") THEN
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

    !WRITE(35,'(A)')  'set label "comment here Y=0.9" at graph 1.21, graph 0.9         '
        CALL GET_ENVIRONMENT_VARIABLE ('USER', TEXT6, nStr)
        CALL GET_ENVIRONMENT_VARIABLE ('HOST', TEXT7, nStr)
        WRITE(35,'(A)')  'set label "'//trim(text6)//'@'//trim(text7)//'" at graph 1.21, graph 0.9         '

    !WRITE(35,'(A)')  'set label "comment here Y=0.8" at graph 1.21, graph 0.8         '
    !WRITE(35,'(A)')  'set label "comment here Y=0.7" at graph 1.21, graph 0.7         '
    !WRITE(35,'(A)')  'set label "comment here Y=0.6" at graph 1.21, graph 0.6         '

    !WRITE(35,'(A)')  'set label "comment here Y=0.5" at graph 1.21, graph 0.5         '
      WRITE(35,'(A)')  'set label "'//trim(TEXT1)//'" at graph 1.21, graph 0.5         '

    !WRITE(35,'(A)')  'set label "comment here Y=0.4" at graph 1.21, graph 0.4         '

    !WRITE(35,'(A)')  'set label "comment here Y=0.3" at graph 1.21, graph 0.3         '
      WRITE(35,'(A)')  'set label "'//trim(TEXT2)//'" at graph 1.21, graph 0.30         '
      WRITE(35,'(A)')  'set label "'//trim(TEXT3)//'" at graph 1.21, graph 0.25        '
      WRITE(35,'(A)')  'set label "'//trim(TEXT4)//'" at graph 1.21, graph 0.20        '
      WRITE(35,'(A)')  'set label "'//trim(TEXT5)//'" at graph 1.21, graph 0.15        '
      WRITE(35,'(A)')  'set label "'//trim(TEXT8)//'" at graph 1.21, graph 0.10        '

    !WRITE(35,'(A)')  'set label "comment here Y=0.2" at graph 1.21, graph 0.2         '
    !WRITE(35,'(A)')  'set label "comment here Y=0.1" at graph 1.21, graph 0.1         '

    WRITE(35,'(A)')  'replot                                        '
    WRITE(35,'(A)')  '                                              '
    WRITE(35,'(A)')  'unset multiplot                               '
    WRITE(35,'(A)')  '                                              '
    WRITE (35,*)  "pause -1 'Press <Enter> to end graphic '"
    !WRITE(35,'(A)')  'pause -1                                      '
    CLOSE(35) 
    print *,'File written to disk: plotxy.gpl'

!print *,'<><><> TEXT1: **'//trim(text1)//'**'
!print *,'<><><> TEXT2: **'//trim(text2)//'**'
!print *,'<><><> TEXT3: **'//trim(text3)//'**'
!print *,'<><><> TEXT4: **'//trim(text4)//'**'
!print *,'<><><> TEXT5: **'//trim(text5)//'**'
!print *,'<><><> TEXT6: **'//trim(text6)//'**'
!print *,'<><><> TEXT7: **'//trim(text7)//'**'
!print *,'<><><> TEXT8: **'//trim(text8)//'**'


!C
!C Get the program name relative to SHADOW_ROOT/bin.
!C
	IFLAG = 0
!	CALL PROGPATH ('primvs', PRIMVS, IFLAG)
!	PRIMVSPATH = PRIMVS(1:IBLANK(PRIMVS)) // ' -i plotxy.prm'
!	WRITE(*,*) 'Executing program: ' // 
!     $		PRIMVSPATH(1:IBLANK(PRIMVSPATH))
!#if !defined(_WIN32)
!	CALL SYSTEM (PRIMVSPATH)
!#else
!	IFLAG = 0
!	CALL RUNPRIMVS (PRIMVS(1:IBLANK(PRIMVS)),'plotxy.prm',iflag)
!#endif

!#endif



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

!        implicit real(kind=skr) (a-h,o-z)
!        implicit integer(kind=ski)        (i-n)
        implicit none

     	!IMPLICIT	REAL*8	(A-H,O-Z)
!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!        INCLUDE	        'SHADOW$INC:DIM.PAR/LIST'
!#endif
!C     	IMPLICIT	REAL*8	(A-H,O-Z)
!     	CHARACTER*80	INFILE1,RSTRING
	character(len=sklen) :: inFile1
	real(kind=skr),dimension(:,:),allocatable :: ray
	real(kind=skr),dimension(1001)            :: rDis
        integer(kind=ski)   :: iErr,iFlag,ncol,np1,np,kLoss,iOver
        integer(kind=ski)   :: f_inc,kount,i,j,k
	real(kind=skr)      :: rMin,rMax,step,dist,wave,qNew,factor,qvec
	real(kind=skr)      :: rr_x,rr_z,vec1,vec2,rr,r,factor_inc,factor_nor
	real(kind=skr)      :: rImag
     	!DIMENSION	RAY(18,N_DIM)
     	!DIMENSION	RDIS(1001)
!     	COMPLEX*16	ARG_S,ARG_P,JEI,
!     $			STOREX(1001),STOREY(1001),STOREZ(1001)
        complex(kind=skr)                  :: ARG_S,ARG_P,JEI
        complex(kind=skr),dimension(1001)  :: STOREX,STOREY,STOREZ

        !REAL*8      	TWOPI, TORAD
     	!DATA	TWOPI 	/  6.2831 85307 17958 64769 25287 D0 /
	!DATA    TORAD   /  0.0174 53292 51994 32957 69237 D0 /

     	!real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
     	!real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
     	!real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
	!real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	!real(kind=skr),parameter ::TOANGS=  1.239852D+4		     
!C
     	JEI	=   (0.0D0,1.0D0)
     	
	!CALL CLSCREEN
     	WRITE(6,*) 'Define integration limits [ cm ].'
1     	RMIN	=   RNUMBER	('From               ')
     	RMAX	=   RNUMBER	(' to                ')
	NP	=   IRINT	('No. of points      ')
	STEP	=   ABS(RMAX-RMIN)/(NP-1)
     	IF (NP.GT.1001) THEN
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
!	CALL	RBEAM18	(INFILE1,RAY,NCOL,NP1,IFLAG,IERR)
!	IF (IERR.NE.0)	STOP 'Error in reading ray file.'
!	IF (NCOL.NE.18)	STOP 'The ray file doesnt contain polarization and phase information.'
!


	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (inFile1,ncol,np1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'FFresnel: RBeamAnalyze: Error in file: '//trim(inFile1)
            stop
        END IF

  	ALLOCATE( RAY(18,NP1) )
  	ray=0.0d0

	CALL RBeam18(ray,ierr,ncol,np1,inFile1)

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

     	 END DO
100	CONTINUE
	END DO
!#ifdef vms
!	OPEN	(20, FILE='FFPAR',STATUS='NEW',CARRIAGECONTROL='LIST')
!#else
     	OPEN	(20, FILE='FFPAR', STATUS='UNKNOWN')
	REWIND (20)
!#endif
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
!#ifdef vms
!     	OPEN	(20, FILE='FFRESNEL', STATUS='NEW')
!#else
     	OPEN	(20, FILE='FFRESNEL', STATUS='UNKNOWN')
	REWIND (20)
!#endif
     	DO K=1,NP
     	  RIMAG = (ABS(STOREX(K)))**2 + (ABS(STOREY(K)))**2  &
      		  + (ABS(STOREZ(K)))**2
     	  WRITE (20,*) RDIS(K),RIMAG/KOUNT
     	END DO
     	CLOSE(20)

        IF (allocated(ray)) deallocate(ray)

END SUBROUTINE FFresnel


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
     	!IMPLICIT	REAL*8	(A-H,O-Z)
     	!DATA	PI     	/  3.1415 92653 58979 32384 62643 D0 /
     	!DATA	PIHALF 	/  1.5707 96326 79489 66192 31322 D0 /
     	!DATA	TWOPI 	/  6.2831 85307 17958 64679 25287 D0 /
     	!DATA	TODEG 	/ 57.2957 79513 08232 08767 98155 D0 /
     	!DATA	TORAD	/  0.0174 53292 51994 32957 69237 D0 /
	!DATA	TOCM	/  1.239 852	D-4		     /
	!DATA	TOANGS 	/  1.239 852    D+4		     /

     	real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
     	real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
     	real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
     	real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     

!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!        INCLUDE	        'SHADOW$INC:DIM.PAR/LIST'
!#endif
!     	DIMENSION	RAY(12,N_DIM),AP(3,N_DIM),PHASE(3,N_DIM),
!     $			WAVE (10)
!     	CHARACTER*60	INFILE,OUTFILE
	character(len=sklen) :: inFile,outFile
	integer(kind=ski)   :: iErr,iFlag,npoint,ncol,iWhat,nLines,i,iSeed
	integer(kind=ski)   :: nn,iForm
	real(kind=skr)      :: line,cMin,cmax,cDelta,cVal
	real(kind=skr),dimension(10) :: wave
	real(kind=skr),dimension(:,:),allocatable :: ray

10     	WRITE(6,*)'File for input ?'
     	!READ (5,1000)	INFILE
     	READ (5,'(a)')	INFILE
     	WRITE(6,*)'and for output ?'
     	!READ (5,1000)	OUTFILE
     	READ (5,'(a)')	OUTFILE


!	CALL	RBEAM	(INFILE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IERR)

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (inFile,ncol,npoint,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'ReColor: RBeamAnalyze: Error in file: '//trim(inFile)
            stop
        END IF

  	ALLOCATE( RAY(18,NPOINT) )
  	ray=0.0d0

	CALL RBeam18(ray,ierr,ncol,npoint,inFile)




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
!C       IFORM = 0 FOR BINARY OUTPUT AND 1 FOR FORMATTED OUTPUT
        IFORM = 0

!	CALL	WRITE_OFF(OUTFILE,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,
!     $	IFORM,IERR)
!	IF (IERR.NE.0)	STOP	'Error in writting output file.'
	CALL Write_off18(ray,ierr,ncol,npoint,outFile)
	IF (IERR.NE.0)	STOP	'Error in writting output file.'


        IF (allocated(ray)) deallocate(ray)

     	WRITE(6,*)'All done.'
!     	STOP
!1000	FORMAT (A)
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

     	!real(kind=skr),parameter ::PI=  3.141592653589793238462643D0 
     	!real(kind=skr),parameter ::PIHALF=  1.570796326794896619231322D0 
     	real(kind=skr),parameter ::TWOPI=  6.283185307179586467925287D0 
     	!real(kind=skr),parameter ::TODEG= 57.295779513082320876798155D0 
     	!real(kind=skr),parameter ::TORAD=  0.017453292519943295769237D0 
	real(kind=skr),parameter ::TOCM=  1.239852D-4		     
	real(kind=skr),parameter ::TOANGS=  1.239852D+4		     


!C
!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!	INCLUDE		'SHADOW$INC:DIM.PAR/LIST'
!#endif
!C
	character(len=sklen)  :: file1,file2,outFil
	real(kind=skr),dimension(:,:),allocatable :: ray1,ray2
	real(kind=skr)   :: r_tot,pwFac,rFac,xpl,ypl,zpl,zpl0

        integer(kind=ski) :: ncol1,ncol2,no,iFlag,iErr,kUnit,kPower,np
        integer(kind=ski) :: i_tot,i_lost,i,iLost,kind,kPow,kPlt,kx,ky
        integer(kind=ski) :: kScaX,kScaY


     	!CHARACTER*80	FILE1,FILE2,OUTFIL
     	!REAL*8		RAY1(18,N_DIM),RAY2(18,N_DIM),R_TOT
!C Statements to define the use of RSTRING
!     	CHARACTER*80	RSTRING




!10     	CONTINUE
     	FILE1	=   RSTRING ('File for Intensity calculations ? ' )

     	!CALL	RBEAM18 (FILE1,RAY1,NCOL1,NP,IFLAG,IERR)
     	!IF ( IERR.NE.0) STOP	'Error reading ray file.'

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (file1,ncol1,np,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'Intens: RBeamAnalyze: Error in file: '//trim(file1)
            stop
        END IF

  	ALLOCATE( RAY1(18,NP) )
  	ray1=0.0d0

	CALL RBeam18(ray1,ierr,ncol1,np,file1)

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
	WRITE(6,*)  'Total rays :', I_TOT 
	WRITE(6,*)  'Good rays :', I_TOT - I_LOST
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
	DO 39 I = 1, NP
	    IF (ILOST .EQ. 0) THEN
	       IF (RAY1 (10, I) .LT. 0.0D0) GOTO 39
	    ELSE IF (ILOST .EQ. 2) THEN
		IF (RAY1 (10, I) .GT. 0.0D0) GOTO 39
	    ENDIF
     	  R_TOT = R_TOT + RAY1(7,I)**2 + RAY1(8,I)**2 + RAY1(9,I)**2
 39	CONTINUE
	IF (NCOL1.EQ.18) THEN
	DO 49 I = 1, NP
	    IF (ILOST .EQ. 0) THEN
	       IF (RAY1 (10, I) .LT. 0.0D0) GOTO 49
	    ELSE IF (ILOST .EQ. 2) THEN
		IF (RAY1 (10, I) .GT. 0.0D0) GOTO 49
	    ENDIF
	  R_TOT = R_TOT + RAY1(16,I)**2 + RAY1(17,I)**2 + RAY1(18,I)**2
 49	CONTINUE
	END IF	
     	WRITE(6,*) 'Total intensity is :',R_TOT
     	WRITE(6,*) 'Normalized intensity is :',R_TOT/NP
30     	CONTINUE
     	WRITE(6,*) 'Options:'
     	WRITE(6,*) 'Intensity transmitted/reflected ....... 0'
     	WRITE(6,*) 'Intensity absorbed .................... 1'
     	WRITE(6,*) 'Local reflectivity/transmission ....... 2'
     	KIND	=   IRINT (' Then ? ')
     	IF (KIND.NE.2) THEN
	  WRITE(6,*)'Spectrum type :'
	  WRITE(6,*)'  [0]    # photons/sec.'
	  WRITE(6,*)'  [1]    Watt'
     	  KUNIT	=   IRINT ('Then ? ')
     	  KPOW  = IYES ('Normalize to source power [ Y/N ] ? ')
     	  IF (KPOW.EQ.1)  &
      		PWFAC = RNUMBER ('Total source power emitted ? ')
	  RFAC	= PWFAC/NP
     	END IF
     	IF (KIND.NE.0) THEN
     	  FILE2 = RSTRING ('Input I0 file : ' )
     	  !CALL RBEAM18 (FILE2,RAY2,NCOL2,NP,IFLAG,IERR)
	  !
	  ! it is necessary to allocate ray array here, at the main level. 
	  ! 
          CALL    RBeamAnalyze (file2,ncol2,np,iflag,ierr)
          IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
              print *,'Intens: RBeamAnalyze: Error in file: '//trim(file2)
              stop
          END IF
  	  ALLOCATE( RAY2(18,NP) )
  	  ray2=0.0d0
	  CALL RBeam18(ray2,ierr,ncol2,np,file2)
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


!#ifdef vms
!     	 OPEN (20,FILE=FILE2,STATUS='NEW',ERR=20)
!#else
     	 OPEN (20,FILE=FILE2,STATUS='UNKNOWN',ERR=20)
	 REWIND (20)
!#endif
     	 DO I=1,NP
     	   XPL	=    RAY1(KX,I)
     	   YPL	=    RAY1(KY,I)
     	   ZPL	=    RAY1(7,I)**2 + RAY1(8,I)**2 + RAY1(9,I)**2
	   IF (NCOL1.EQ.18) &
      	     ZPL = ZPL + RAY1(16,I)**2 + RAY1(17,I)**2 + RAY1(18,I)**2
     	  IF (KIND.EQ.1) THEN
     	    ZPL0=    RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2 
	    IF (NCOL2.EQ.18) &
      	      ZPL0 = ZPL0 + RAY2(16,I)**2 + RAY2(17,I)**2 +  &
      							RAY2(18,I)**2
     	    ZPL	=    ZPL0 - ZPL
     	  ELSE IF (KIND.EQ.2) THEN
     	    ZPL0=    RAY2(7,I)**2 + RAY2(8,I)**2 + RAY2(9,I)**2
	    IF (NCOL2.EQ.18) &
      	      ZPL0 = ZPL0 + RAY2(16,I)**2 + RAY2(17,I)**2 +  &
      							RAY2(18,I)**2
     	    ZPL	=    ZPL/ZPL0
     	  END IF

	  IF (KIND.NE.2) THEN
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
!	ITRY	= IRINT ('Another run [1/0] ? ')
!     	IF (ITRY.EQ.1)	GO TO 10
END SUBROUTINE Intens


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
!	IMPLICIT	REAL*8	(A-H,O-Z)
!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!        INCLUDE	        'SHADOW$INC:DIM.PAR/LIST'
!#endif
     	!REAL*8		RAY(12,N_DIM)
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

        !implicit none  

        implicit real(kind=skr) (a-h,o-z)
        implicit integer(kind=ski)        (i-n)
	!IMPLICIT	REAL*8	(A-H,O-Z)
!#if defined(unix) || HAVE_F77_CPP
!#       include	        <dim.par>
!#elif defined(vms)
!        INCLUDE	        'SHADOW$INC:DIM.PAR/LIST'
!#endif
     	!REAL*8		RAY (12,N_DIM),AP(3,N_DIM),PHASE(3,N_DIM)
     	!DIMENSION	X(N_DIM),Y(N_DIM)

     	!DIMENSION	TEST(6),RLOW(12),RUPP(12)
	real(kind=skr),dimension(6) :: xmean,stDev,var
	real(kind=skr)              :: xExter, zExter
	real(kind=skr),dimension(:,:),allocatable :: ray
	real(kind=skr),dimension(6)               :: rLow,rUpp,test
	real(kind=skr),dimension(1000)            :: x,y
	character(len=sklen)         :: iFile
	character(len=7)   :: root
	character(len=9)   :: filOut
	character(len=2)   :: header
	character(len=60)   :: fileText
	character(len=17)   :: dateText
	character(len=10),dimension(3)   :: nMode
     	!REAL*8		XMEAN(6),STDEV(6),VAR(6),XEXTER,ZEXTER
     	!CHARACTER *80	IFILE,RSTRING
     	!EXTERNAL	RSTRING
     	!CHARACTER *7	ROOT
     	!CHARACTER *9	FILOUT
     	!CHARACTER *2	HEADER
     	!CHARACTER * 60		FILETEXT
     	!CHARACTER * 17		DATETEXT
     	!CHARACTER * 10		NMODE (3)
    	!DATA	NMODE(1)	/'Origin'/
    	!DATA	NMODE(2)	/'Baricenter'/
    	!DATA	NMODE(3)	/'External'/
	integer(kind=ski) :: kLost
	!real(kind=skr)    :: 

    	NMODE(1)='Origin'
    	NMODE(2)='Baricenter'
    	NMODE(3)='External'
!C
!C KLOSS is really never used here, but I'm setting it to zero in case
!C the program expects it to be, which it probably does. Only check for
!C good rays as a result -- MK
!C
	KLOSS = 0

	!CALL CLSCREEN
102    	IFILE	=   RSTRING ('Input file ? ')
!#ifdef vms
!     	CALL	FILEINFO  (IFILE)
!     	CALL	NEXTFILE  (FILETEXT,DATETEXT)
!#else
	filetext = ' '
	datetext = ' '
!	call get_file_text(filetext,IFILE)
!#endif

	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (iFile,ncol,nn,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'FocNew: RBeamAnalyze: Error in file: '//trim(iFile)
            stop
        END IF

  	ALLOCATE( RAY(18,nn) )
  	ray=0.0d0

	CALL RBeam18(ray,ierr,ncol,nn,iFile)

     	!CALL	RBEAM (IFILE,RAY,PHASE,AP,NCOL,NN,IFLAG,IERR)
     	IF (IERR.NE.0) STOP	'Error reading ray file.'

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
     	CALL FINDOUT (3,NN,AZ1,AZ2,AZ3,AZ4,AZ5,AZ6,RAY,nn)
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
     	CALL FINDOUT (1,NN,AX1,AX2,AX3,AX4,AX5,AX6,RAY,nn)
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
	WRITE (6,1035)
	WRITE (6,*) 'Searching file : ',IFILE
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
     	WRITE (6,*) '.............  LEAST CONFUSION  ...............'
     	WRITE (6,1040) AT1,AT2,AT3
     	WRITE (6,1012) SQRT(ABS(TBAR)),SQRT(ABS(VTBAR))
     	WRITE (6,1050) TPART
	WRITE (6,1015)	SIGT
     	WRITE (6,1060)	SIGT0
     	WRITE(6,*)'All done. File out data.'
!#ifdef vms
!     	OPEN (23,FILE='FOCUS',STATUS='NEW',CARRIAGECONTROL='LIST')
!#else
     	OPEN (23,FILE='focus',STATUS='UNKNOWN')
	REWIND (23)
!#endif
	WRITE (23,1035)
	WRITE (23,*) 'Searching file : ',IFILE
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
     	WRITE (23,*) '..............   LEAST CONFUSION  .............'
     	WRITE (23,1040) AT1,AT2,AT3
     	WRITE (23,1012) SQRT(ABS(TBAR)),SQRT(ABS(VTBAR))
     	WRITE (23,1050) TPART
	WRITE (23,1015) SIGT
     	WRITE (23,1060)	SIGT0
     	CLOSE (23)
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
     	IOLD	=  0
     	IF (IPLOT.NE.4) THEN
     	 IFILE = RSTRING ('File-name ? ')
     	ELSE
     	 ROOT = RSTRING (' File root [ 7 letters max ] ? ')
     	 IOLD	=  4
     	 IPLOT	=  1
     	 KOUNT	=  1
     	END IF
105	DO 16 I=1,NPL
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
     	IF (IOLD.EQ.4) THEN
     	 IF (KOUNT.EQ.1)	FILOUT	= 'FX'//ROOT
     	 IF (KOUNT.EQ.2)	FILOUT	= 'FZ'//ROOT
     	 IF (KOUNT.EQ.3) 	FILOUT	= 'FT'//ROOT
!#ifdef vms
!     	 OPEN (23,FILE=FILOUT,STATUS='NEW')
!#else
     	 OPEN (23,FILE=FILOUT,STATUS='UNKNOWN')
	 REWIND (23)
!#endif
     	ELSE
!#ifdef vms
!     	 OPEN (23,FILE=IFILE,STATUS='NEW')
!#else
     	 OPEN (23,FILE=IFILE,STATUS='UNKNOWN')
	 REWIND (23)
!#endif
     	END IF
     	DO 14 I=1,NPL
     	WRITE (23,*)	X(I),Y(I)
14     	CONTINUE
     	CLOSE (23)
	print *,'File written to disk: '//trim(iFile)
     	IF (IOLD.EQ.4.AND.KOUNT.LT.3) THEN
     	KOUNT = KOUNT + 1
     	IPLOT = IPLOT + 1
     	GO TO 105
     	END IF
     	END IF

	KGO = IYES('Another plot? ')
	IF (KGO == 1) GO TO 102
        
     	!WRITE(6,*)'Enter :'
     	!WRITE(6,*)'0	for another plot'
     	!WRITE(6,*)'1	to restart'
     	!WRITE(6,*)'2	to exit'
     	!KGO = IRINT (' Then ? ')
     	!GO TO (101,102,103)	KGO+1
!103	STOP

        IF (allocated(ray)) deallocate(ray)
	RETURN
END SUBROUTINE FocNew



End Module Shadow_Post
