    ! C+++
    ! C	PROGRAM	INPUT_SOURCE
    ! C
    ! C	PURPOSE		Reads in the parameters of the optical system
    ! C
    ! C---

    !
    !  The old subroutine "input_source" has been moved to a main program
    !  in order to separate completely the prompt-interface (or, in general
    !  any user interface) from the shadow source and trace, which will 
    !  receive the input ONLY from start.xx files. 
    !  
    PROGRAM INPUT_SOURCE
    
      use stringio

      IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
      IMPLICIT INTEGER(KIND=4)        (F,I-N)


     character(len=512)           :: FILE_SOURCE, FILE_BOUND, FILE_TRAJ, FFILE

     character(len=512)       :: MSSG1,MSSG2
     integer(kind=4)          :: oe_number

    !
    ! define defaults (a geometrical source)
    !

    FDISTR =  2
    FGRID =  0
    FSOUR =  3
    FSOURCE_DEPTH =  1
    F_COHER =  0
    F_COLOR =  1
    F_PHOT =  0
    F_POL =  3
    F_POLAR =  0
    F_OPD =  1
    F_WIGGLER =  0
    F_BOUND_SOUR =  0
    F_SR_TYPE =  0
    ISTAR1 =  6775431
    NPOINT =  30000
    NCOL =  0
    N_CIRCLE =  0
    N_COLOR =  2
    N_CONE =  0
    IDO_VX =  1
    IDO_VZ =  1
    IDO_X_S =  1
    IDO_Y_S =  1
    IDO_Z_S =  1
    IDO_XL =  0
    IDO_XN =  0
    IDO_ZL =  0
    IDO_ZN =  0
    SIGXL1 =  0.00000D0
    SIGXL2 =  0.00000D0
    SIGXL3 =  0.00000D0
    SIGXL4 =  0.00000D0
    SIGXL5 =  0.00000D0
    SIGXL6 =  0.00000D0
    SIGXL7 =  0.00000D0
    SIGXL8 =  0.00000D0
    SIGXL9 =  0.00000D0
    SIGXL10 =  0.00000D0
    SIGZL1 =  0.00000D0
    SIGZL2 =  0.00000D0
    SIGZL3 =  0.00000D0
    SIGZL4 =  0.00000D0
    SIGZL5 =  0.00000D0
    SIGZL6 =  0.00000D0
    SIGZL7 =  0.00000D0
    SIGZL8 =  0.00000D0
    SIGZL9 =  0.00000D0
    SIGZL10 =  0.00000D0
    CONV_FACT =  0.00000D0
    CONE_MAX =  0.00000D0
    CONE_MIN =  0.00000D0
    EPSI_DX =  0.00000D0
    EPSI_DZ =  0.00000D0
    EPSI_X =  0.00000D0
    EPSI_Z =  0.00000D0
    HDIV1 =  0.5D-6
    HDIV2 =  0.5D-6
    PH1 =  1000.000D0
    PH2 =  1010.00000D0
    PH3 =  0.00000D0
    PH4 =  0.00000D0
    PH5 =  0.00000D0
    PH6 =  0.00000D0
    PH7 =  0.00000D0
    PH8 =  0.00000D0
    PH9 =  0.00000D0
    PH10 =  0.00000D0
    RL1 =  0.00000D0
    RL2 =  0.00000D0
    RL3 =  0.00000D0
    RL4 =  0.00000D0
    RL5 =  0.00000D0
    RL6 =  0.00000D0
    RL7 =  0.00000D0
    RL8 =  0.00000D0
    RL9 =  0.00000D0
    RL10 =  0.00000D0
    BENER =  0.00000D0
    POL_ANGLE =  0.0000D0
    POL_DEG =  1.0000D0
    R_ALADDIN =  0.00000D0
    R_MAGNET =  0.00000D0
    SIGDIX =  0.001D0
    SIGDIZ =  0.0001D0
    SIGMAX =  0.001D0
    SIGMAY =  0.001D0
    SIGMAZ =  0.001D0
    VDIV1 =  5D-6
    VDIV2 =  5D-6
    WXSOU =  0.1D0
    WYSOU =  0.2D0
    WZSOU =  0.2D0
    PLASMA_ANGLE =  0.00000D0
    FILE_TRAJ = "NONE SPECIFIED" 
    FILE_SOURCE =  "NONE SPECIFIED"
    FILE_BOUND =  "NONE SPECIFIED"
    OE_NUMBER =  0
    IDUMMY =  0
    DUMMY =  0.00000D0
    F_NEW =  0

     iDefault = IYES("Write default start.00 file? [Y/N]: ")
     if (iDefault == 1) goto 10101

    ! C

    ! C     	DATA	IDO_X_S,IDO_Y_S,IDO_Z_S,IDO_VX,IDO_VZ	/1,1,1,1,1/
    ! C     	DATA	ISTAR1		/ 12345701 /
    ! C
    ! C
         	WRITE(6,*)'Call to INPUT'
    ! C
    	CALL CLSCREEN
    ! C		       123456789 123456789 123456789 123456789 1
    !     	MSSG1 (1:40) = '--------------------------------- S H A '
    !        MSSG1 (41:80)= ' D O W  -------------------------------'
    MSSG1 = '--------------------------------- S H A  D O W  -------------------------------'
    MSSG2 = 'version 3alpha0'
    WRITE (6,'(1X,A)')  trim(MSSG1)
    ! C
    ! C Get the data file path using either SHADOW$DATA or Unix SHADOW_DATA_DIR
    ! C environment variable. Also, check for existence in the routine itself.
    ! C
    ! TODO: include shadow banner and version
    ! 	IFLAG = 1
    ! 	CALL DATAPATH ('VERSION', VERSION, IFLAG)
    !   IF (IFLAG .NE. 0) THEN
    ! 	    CALL LEAVE ('INPUT_SOURCE', 'VERSION file not found', 1)
    !   ENDIF
    !     	OPEN (11, FILE=VERSION, STATUS='OLD')
    !     	READ (11,*) I1,I1,I1
    !     	READ (11,'(1X,A)') MSSG2
    !     	CLOSE(11)
    !     	WRITE (6,'(1X,A)') MSSG2
         	WRITE (6,2222) 
    2222	FORMAT (1X,/,1X,'Defining source : ')
         	WRITE(6,*)'When prompted for a yes/no answer, you may enter:'
         	! WRITE(6,*)'for   YES  answer      Y, 1'
         	WRITE(6,*)'for   YES  answer      Y*, y*, 1*'
         	WRITE(6,*)'for   NO   answer      anything else'
         	WRITE (6,'(1X,///)')
         	IVERB = IYES ('Do you want a verbose [ 1 ] or terse [ 0 ] output ?')
    	CALL CLSCREEN
    	WRITE(6,'(1X,A)') trim(MSSG1)
    	WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    
    ! **---------------------------------------------------------------------
         	WRITE(6,*)'------------- SOURCE SPECS ------------------'
           	IF (IVERB.EQ.1) THEN
         	WRITE(6,*)'Options available:'
         	WRITE(6,*)'Random in  BOTH REAL and MOMENTUM space	      0'
         	WRITE(6,*)'Grid       BOTH				      1'
         	WRITE(6,*)'Grid       REAL, random  MOMENTUM	              2'
         	WRITE(6,*)'Random     REAL,  grid   MOMENTUM		      3'
    	WRITE(6,*)'Ellipses in PHASE space,random around each ellipse 4'
    	WRITE(6,*)'Ellipses in PHASE space,  grid around each ellipse 5'
         	END IF
    	FGRID = IRINT ('Source modelling type [ 0-5 ] ? ')
    		IF (FGRID.EQ.0) THEN		!3
         	!  NPOINT= IRINT ('How many rays [ 1 - 5 000 ] ? ')
         	NPOINT= IRINT ('How many rays ? ')
         	ISTAR1= IRINT ('Seed [ odd, 1000 - 1 000 000 ] ? ')
         		ELSE IF (FGRID.EQ.2.OR.FGRID.EQ.3.OR.FGRID.EQ.4) THEN
         	NPOINT= IRINT ('How many rays for RANDOM part ? ')
         	ISTAR1= IRINT ('Seed [ odd, 1000 - 1 000 000 ] ? ')
    		END IF				!3
    ! * Go now on to define the source.
         	F_BOUND_SOUR 	=  IYES('Do you want to optimize the source ? ')
         	IF ( F_BOUND_SOUR.EQ.1 ) FILE_BOUND = RSTRING ('Please input name of file with acceptance: ')
         	  
    ! **---------------------------------------------------------------------
    	CALL CLSCREEN
    	WRITE(6,'(1X,A)') trim(MSSG1)
    	WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    	WRITE(6,*)'Source type : [ 0 ] regular source'
    	WRITE(6,*)'              [ 1 ] normal wiggler'
    	WRITE(6,*)'              [ 2 ] undulator'
    	WRITE(6,*)'              [ 3 ] elliptical wiggler'
    	F_WIGGLER	= IRINT ('Then ? ')
    	IF (F_WIGGLER.NE.0) THEN
    	  IF ((F_WIGGLER.EQ.1) .OR. (F_WIGGLER.EQ.3)) THEN
    	    FILE_TRAJ	= RSTRING ('File containing the electron trajectory ? ')
    	    CONV_FACT	= RNUMBER ('Conversion factor from meters to user units ? ')
         	    HDIV1 = RNUMBER ('Horizontal half-divergence [ (+)x, rads ] ? ')
         	    HDIV2 = RNUMBER ('                           [ (-)x, rads ] ? ')
         	    VDIV1 = RNUMBER ('Vertical                   [ (+)z, rads ] ? ')
         	    VDIV2 = RNUMBER ('                           [ (-)z, rads ] ? ')
    	  ELSE IF (F_WIGGLER.EQ.2) THEN
    	    FILE_TRAJ	= RSTRING ('File containing the CDF''s ? ')	
    	  END IF
    	  IF (FGRID.NE.0) THEN
         	    NPOINT= IRINT ('How many rays [ 1 - 5 000 ] ? ')
         	    ISTAR1= IRINT ('Seed [ odd, 1000 - 1 000 000 ] ? ')
    	  END IF
         	  SIGMAX	= RNUMBER ('Sigma along X ? ')
         	  SIGMAZ	= RNUMBER ('            Z ? ')
    ! C23456789112345678921234567893123456789412345678951234567896123456789712
    	  WRITE(6,*)'Electron beam emittance.  Units are: rads*[ units of length used so far ]'
         	  EPSI_X = rnumber ('Beam emittances in X [ at waist ] ? ')
         	  EPSI_DX= rnumber ('Distance of insertion device''s center from X waist [ signed ] ? ')
         	  EPSI_Z = rnumber ('Beam emittances in Z [ at waist ] ? ')
         	  EPSI_DZ= rnumber ('Distance of insertion device''s center from Z waist [ signed ] ? ')
         	  IF (iverb.eq.1) then
         			WRITE (6,540)
    540	FORMAT (/,1X,'Polarization component of interest. Enter parallel polarization    1',/, &
         1x,'perpendicular            2',/, &
         1x,'total                    3',/, &
         1x,'                   then ? ',$)
         	end if
         	  F_POL = irint ('Polarization Selected [ 1-3 ] ? ')
    	  GO TO 10100
    	END IF
    ! **---------------------------------------------------------------------
    	CALL CLSCREEN
    	WRITE(6,'(1X,A)') trim(MSSG1)
    	WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    	IF (FGRID.EQ.4.OR.FGRID.EQ.5) THEN
    	  IF (IVERB.EQ.1) THEN
    	    WRITE(6,*) 'Ellipses in PHASE space are accomplished by assumming '
    	    WRITE(6,*) 'a double Gaussian distribution in phase space and then'
    	    WRITE(6,*) 'generating data points at various concentric sigma '
    	    WRITE(6,*)'levels (1-sigma, 3-sigma etc.)'
    	  END IF
    	  SIGMAX = RNUMBER ('Sigma for X ? ')
    	  SIGDIX = RNUMBER ('Sigma for X'' (hori. divergence) [ rads ] ? ')
    	  IDO_XL = IRINT ('How many sigma levels ? ')
    	  DO 11 I = 1, IDO_XL
    	    WRITE(6,*)'Sigma level of ellipse ',I
    	    IF (I.EQ.1) SIGXL1 = RNUMBER (' ? ')
    	    IF (I.EQ.2) SIGXL2 = RNUMBER (' ? ')
    	    IF (I.EQ.3) SIGXL3 = RNUMBER (' ? ')
    	    IF (I.EQ.4) SIGXL4 = RNUMBER (' ? ')
    	    IF (I.EQ.5) SIGXL5 = RNUMBER (' ? ')
    	    IF (I.EQ.6) SIGXL6 = RNUMBER (' ? ')
    	    IF (I.EQ.7) SIGXL7 = RNUMBER (' ? ')
    	    IF (I.EQ.8) SIGXL8 = RNUMBER (' ? ')
    	    IF (I.EQ.9) SIGXL9 = RNUMBER (' ? ')
    	    IF (I.EQ.10) SIGXL10 = RNUMBER (' ? ')
    11	  CONTINUE
    	  IF (FGRID.EQ.5) IDO_XN = IRINT ('No. of rays within each level ?')
    	  SIGMAZ = RNUMBER ('Sigma for Z ? ')
         	  SIGDIZ = RNUMBER ('Sigma for Z'' (vert. divergence) [ rads ] ? ')
    	  IDO_ZL = IRINT ('How many sigma levels ? ')
    	  DO 21 I = 1, IDO_ZL
    	    WRITE(6,*)'Sigma level of ellipse ',I
    	    IF (I.EQ.1) SIGZL1 = RNUMBER (' ? ')
    	    IF (I.EQ.2) SIGZL2 = RNUMBER (' ? ')
    	    IF (I.EQ.3) SIGZL3 = RNUMBER (' ? ')
    	    IF (I.EQ.4) SIGZL4 = RNUMBER (' ? ')
    	    IF (I.EQ.5) SIGZL5 = RNUMBER (' ? ')
    	    IF (I.EQ.6) SIGZL6 = RNUMBER (' ? ')
    	    IF (I.EQ.7) SIGZL7 = RNUMBER (' ? ')
    	    IF (I.EQ.8) SIGZL8 = RNUMBER (' ? ')
    	    IF (I.EQ.9) SIGZL9 = RNUMBER (' ? ')
    	    IF (I.EQ.10) SIGZL10 = RNUMBER (' ? ')
    21	  CONTINUE
    	  IF (FGRID.EQ.5) IDO_ZN = IRINT  ('No. of rays within each level ?')
    	  GO TO 470
    	END IF
         	IF (IVERB.EQ.1) THEN
    	WRITE (6,440)
    ! C23456789112345678921234567893123456789412345678951234567896123456789712
    440	FORMAT (1X,' The source is specified in the laboratory ',/,1X, &
                'reference frame. The program will then rotate the set of ',/,1X, &
                'rays in the mirror frame.',//,1X, &
                'Type of source,now.',//,10X, &
                'use  ( 0 ) for point source  ',/,10X, &
                '     ( 1 ) for rectangular  s.',/,10X, &
                '     ( 2 ) for elliptical   s.',/,10X, &
                '     ( 3 ) for gaussian     s.',/,10X, &
                '     ( 6 ) for dense plasma s.',/,10X)
         	END IF
    	FSOUR = IRINT ('X-Z plane source type [ 0-3, 6 ] ? ')
    	IF (FSOUR.EQ.1) THEN 
         	WXSOU	= RNUMBER ('Source Width  [ x ] ? ' )
         	WZSOU	= RNUMBER ('       Height [ z ] ? ')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many points along the source X ? ')
         	IDO_Z_S = IRINT ('                                 Z ? ')
         		END IF
    	ELSE IF (FSOUR.EQ.2) THEN          
         	WXSOU	= RNUMBER ('Source Width  [ x ] ? ' )
         	WZSOU	= RNUMBER ('       Height [ z ] ? ')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many concentric ellipses within the source ? ')
         	IDO_Z_S = IRINT ('How many points on each ellipse ? ')
         		END IF
    
    	ELSE IF (FSOUR.EQ.3) THEN          
         	SIGMAX	= RNUMBER ('Sigma along X ? ')
         	SIGMAZ	= RNUMBER ('            Z ? ')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many points along the source X ? ')
         	IDO_Z_S = IRINT ('                                 Z ? ')
         		END IF
    
    	ELSE IF (FSOUR.EQ.6) THEN
    	PLASMA_ANGLE = RNUMBER ('Source cone full opening (radians)')
         		IF (FGRID.EQ.1.OR.FGRID.EQ.2) THEN
         	IDO_X_S = IRINT ('How many points along the source X ? ')
         	IDO_Z_S = IRINT ('                                 Z ? ')
         		END IF
    	END IF				   
    470	CONTINUE
    ! * Inquires now about the source depth.
    ! **---------------------------------------------------------------------
    	CALL CLSCREEN
    	WRITE(6,'(1X,A)') trim(MSSG1)
    	WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! C23456789112345678921234567893123456789412345678951234567896123456789712
    ! **---------------------------------------------------------------------
    ! C
    ! C       dense plasma assumes flat depth distrib here for now. See later.
    ! C
    	IF (FSOUR.EQ.6) THEN
    	  FSOURCE_DEPTH = 2
    	ELSE
         	  IF (IVERB.EQ.1) THEN
    	    WRITE (6,490)
    490 	    FORMAT (1X,'Source depth. The actual source will be centered on', &
           'the no-depth position. Use ',/,10x, &
           '	(1) for no depth,',/,10x, &
           '	(2) for flat depth distribution,',/,10x, &
           '	(3) for gaussian depth distribution,',/,10x, &
           '       (4) for a synchrotron source depth distr.',//,20x, &
           '	Then ? ',$)
         	  END IF
    	  FSOURCE_DEPTH = IRINT ('Source Depth [ 1-4 ] ? ')
    	END IF
    ! * The S.R. case will be dealt with aftewards, when asking the details of
    ! * the source.
         	IF (FSOURCE_DEPTH.EQ.2) THEN
    	 WYSOU = RNUMBER ('Source Depth ? ')
         	ELSE IF (FSOURCE_DEPTH.EQ.3) THEN
    	 SIGMAY	=  RNUMBER ('Sigma along depth ? ')
    	ELSE IF (FSOURCE_DEPTH.EQ.4.AND.(FGRID.EQ.4.OR.FGRID.EQ.5)) THEN
         	  WRITE(6,*) 'Notice: the ORBIT radius MUST be in the same units as the rest of the optical system.'
         	  WRITE(6,*) 'Use negative ORBIT radius argument for CCW storage ring.'
    	  R_ALADDIN=rnumber('Orbit Radius [ same as other units ] ?')
         	  HDIV1=RNUMBER('Horizontal half-divergence [ (+)x, rads ] ? ')
         	  HDIV2=RNUMBER('                           [ (-)x, rads ] ? ')
         	END IF
    1111	CONTINUE
         	IF ((FGRID.EQ.1.OR.FGRID.EQ.2).AND.FSOURCE_DEPTH.NE.1) THEN  !6
         	  IDO_Y_S = IRINT ('How many points along the depth ? ')
         	END IF					!6
    	IF (FGRID.EQ.4.OR.FGRID.EQ.5) GO TO 770
    
    ! **---------------------------------------------------------------------
    	CALL CLSCREEN
    	WRITE(6,'(1X,A)') trim(MSSG1)
    	WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    ! C
    ! C       dense plasma assumes conical source distribution for now.
    ! C
    	IF (FSOUR.EQ.6) THEN
         	  FDISTR = 5
    	ELSE
         	  IF (IVERB.EQ.1) THEN
    	    WRITE (6,530)	
    530 	    FORMAT (/,1X,'O.K., got it so far.',/,10X, &
                 'Source distribution now. We may use ',/,10X, &
                 '    ( 1 ) for a flat source',/,10X, &
                 '    ( 2 )       uniform   s.',/,10X, &
                 '    ( 3 )       gaussian  s.',/,10X, &
                 '    ( 4 )       synchrotron ',/,10X, &
                 '    ( 5 )       conical ',/,10X, &
                 '    ( 6 )       exact synchrotron ',/)
    	  END IF
         	  FDISTR = IRINT ('Source Angle Distribution [ 1-6 ] ? ')
    	ENDIF
         	IF ((FGRID.EQ.1.OR.FGRID.EQ.3).AND.FDISTR.NE.5) THEN
         	  IDO_VZ = IRINT ('How many points in the vertical ? ')
         	  IDO_VX = IRINT ('          and in the horizontal ? ')
         	ELSE IF ((FGRID.EQ.1.OR.FGRID.EQ.3).AND.FDISTR.EQ.5) THEN
         	  N_CONE = irint ('How many points along cone radius ? ')
         	  N_CIRCLE = irint ('                and along circles ? ')
         	END IF
    		IF (FDISTR.NE.5) THEN      	!1
         	HDIV1 = RNUMBER ('Horizontal half-divergence [ (+)x, rads ] ? ')
         	HDIV2 = RNUMBER ('                           [ (-)x, rads ] ? ')
         	VDIV1 = RNUMBER ('Vertical                   [ (+)z, rads ] ? ')
         	VDIV2 = RNUMBER ('                           [ (-)z, rads ] ? ')
    		ELSE            		!1
         	cone_max = rnumber ('Max half-divergence ? ')
         	cone_min = rnumber ('Min half-divergence ? ')
    		END IF				!1
    		IF (FDISTR.EQ.3) THEN		!2
         	sigdiz = rnumber ('Vertical sigma [ rads ] ? ')
         	sigdix = rnumber ('Horizontal              ? ')
    		ELSE IF (FDISTR.EQ.4.OR.FDISTR.EQ.6) THEN	!2.1
         	r_magnet  = rnumber ('Magnetic Radius [ m ] ? ')
         	WRITE(6,*) 'Notice: the ORBIT radius MUST be in the same units as the rest of the optical system.'
         	WRITE(6,*) 'Use negative ORBIT radius argument for CCW storage ring.'
    	r_aladdin = rnumber ('Orbit Radius [ same as other units ] ?')
         			IF (FSOUR.EQ.3) THEN
         	iansw = iyes ('Do you want to include electron beam emittances [ Y/N ] ? ')
         				IF (IANSW.EQ.1) THEN
         	  WRITE(6,*) 'Units are : rads*[ units of length used so far ]'
         	EPSI_X = rnumber ('Beam emittances in X [ at waist ] ? ')
         	EPSI_DX= rnumber ('Distance from waist [ signed ] ? ')
         	EPSI_Z = rnumber ('Beam emittances in Z [ at waist ] ? ')
         	EPSI_DZ= rnumber ('Distance from waist [ signed ] ? ')
         			     	END IF
         		     	END IF
         	BENER = RNUMBER ('Electron Beam Energy [ GeV ] ? ')
         	if (iverb.eq.1) then
         			WRITE (6,640)
    640	FORMAT (/,1X,'Polarization component of interest. Enter ',/, &
         1x,'parallel polarization    1',/, &
         1x,'perpendicular            2',/, &
         1x,'total                    3',/, &
         1x,'                   then ? ',$)
         	end if
         	f_pol = irint ('Polarization Selected [ 1-3 ] ? ')
    	  IF (FDISTR.EQ.4) THEN
    	    IF (IVERB.EQ.1) THEN
    		WRITE (6,*) 'The source can be generated according to either [0] photons or [1] power distribution.'
    	    END IF
    	    F_SR_TYPE	= IRINT ('Distribution type [0,1] ? ')
    	  END IF
    		END IF				!2
    ! C
    770	CONTINUE
         	IF (FDISTR.NE.4.AND.FDISTR.NE.6) THEN
         	  I_ANSW = IYES ('Do you want a Photon energy [ Y/N ] ? ')
         	ELSE
         	  I_ANSW = 1
         	END IF
         	IF (I_ANSW.NE.1) GO TO 10100
         	IF (FDISTR.NE.6) THEN
         	 IF (IVERB.EQ.1) THEN
         	   WRITE(6,*)'We have these choices :'
         	   WRITE(6,*)'Single line ............................ 1'
         	   WRITE(6,*)'Several lines .......................... 2'
         	   WRITE(6,*)'Uniform source.......................... 3'
    	   WRITE(6,*)'Several lines, different intensities.... 4'
         	 END IF
         	  F_COLOR = IRINT ('Energy distribution [ 1-4 ] ? ')
         	ELSE IF (FDISTR.EQ.6) THEN
         	  F_COLOR = 1
         	END IF
         	F_PHOT = IRINT ('Photon Energy [ 0 ] or Angstroms [ 1 ] ? ' )
         	IF (F_COLOR.EQ.1) THEN
         	 IF (F_PHOT.EQ.0) PH1 = RNUMBER ('Energy [ eV ] ? ')
         	 IF (F_PHOT.EQ.1) PH1 = RNUMBER ('Wavelength [ A ] ? ')
         	ELSE IF (F_COLOR.EQ.2) THEN
         	 n_color = irint('How many lines ? ')
         		  DO 31 I_COL=1,N_COLOR
            WRITE(6,*)'Photon energy or wavelength for line ',I_COL
         		IF (I_COL.EQ.1)  PH1 = RNUMBER (' ? ')
         		IF (I_COL.EQ.2)  PH2 = RNUMBER (' ? ')
         		IF (I_COL.EQ.3)  PH3 = RNUMBER (' ? ')
         		IF (I_COL.EQ.4)  PH4 = RNUMBER (' ? ')
         		IF (I_COL.EQ.5)  PH5 = RNUMBER (' ? ')
         		IF (I_COL.EQ.6)  PH6 = RNUMBER (' ? ')
         		IF (I_COL.EQ.7)  PH7 = RNUMBER (' ? ')
         		IF (I_COL.EQ.8)  PH8 = RNUMBER (' ? ')
         		IF (I_COL.EQ.9)  PH9 = RNUMBER (' ? ')
         		IF (I_COL.EQ.10)  PH10 = RNUMBER (' ? ')
    31     		  CONTINUE
         	ELSE IF (F_COLOR.EQ.3) THEN
         	 WRITE(6,*)'From photon energy or wavelength ... '
         	 PH1 = rnumber (' ? ')
         	 WRITE(6,*)'... to photon energy or wavelength :'
         	 PH2 = rnumber (' ? ')
         	ELSE IF (F_COLOR.EQ.4) THEN
         	 n_color = irint('How many lines ? ')
         		  DO 41 I_COL=1,N_COLOR
            WRITE(6,*)'Photon energy or wavelength for line ',I_COL
         		IF (I_COL.EQ.1)  PH1 = RNUMBER (' ? ')
         		IF (I_COL.EQ.2)  PH2 = RNUMBER (' ? ')
         		IF (I_COL.EQ.3)  PH3 = RNUMBER (' ? ')
         		IF (I_COL.EQ.4)  PH4 = RNUMBER (' ? ')
         		IF (I_COL.EQ.5)  PH5 = RNUMBER (' ? ')
         		IF (I_COL.EQ.6)  PH6 = RNUMBER (' ? ')
         		IF (I_COL.EQ.7)  PH7 = RNUMBER (' ? ')
         		IF (I_COL.EQ.8)  PH8 = RNUMBER (' ? ')
         		IF (I_COL.EQ.9)  PH9 = RNUMBER (' ? ')
         		IF (I_COL.EQ.10)  PH10 = RNUMBER (' ? ')
            WRITE(6,*)'Relative intensity for line ',I_COL
         		IF (I_COL.EQ.1)  RL1 = RNUMBER (' ? ')
         		IF (I_COL.EQ.2)  RL2 = RNUMBER (' ? ')
         		IF (I_COL.EQ.3)  RL3 = RNUMBER (' ? ')
         		IF (I_COL.EQ.4)  RL4 = RNUMBER (' ? ')
         		IF (I_COL.EQ.5)  RL5 = RNUMBER (' ? ')
         		IF (I_COL.EQ.6)  RL6 = RNUMBER (' ? ')
         		IF (I_COL.EQ.7)  RL7 = RNUMBER (' ? ')
         		IF (I_COL.EQ.8)  RL8 = RNUMBER (' ? ')
         		IF (I_COL.EQ.9)  RL9 = RNUMBER (' ? ')
         		IF (I_COL.EQ.10)  RL10 = RNUMBER (' ? ')
    	WRITE(6,*)'RL1 = ',RL1
    	WRITE(6,*)'RL5 = ',RL5
    41     		  CONTINUE
         	END IF
    10100	CONTINUE
    ! **---------------------------------------------------------------------
    	CALL CLSCREEN
    	WRITE(6,'(1X,A)') trim(MSSG1)
    	WRITE(6,'(1X,A)') trim(MSSG2)
    	WRITE(6,*)
    ! **---------------------------------------------------------------------
    	  F_OPD		= IYES ('Do you want to store the optical paths (OPD) [Y/N] ? ')
    ! ** Inquire about the source polarization.
    	  F_POLAR	= IYES ('Do you want to generate the A vectors (electric field) [Y/N] ?')
    	IF (F_POLAR.EQ.0)	GO TO 10101
    ! C
    ! C For SR, all the polarization varibles will be defined by SOURCE internally.
    ! C
    	IF (FDISTR.EQ.4.OR.FDISTR.EQ.6.OR.F_WIGGLER.NE.0) GO TO 10101
      	IF (IVERB.EQ.1) THEN
         	  WRITE(6,*)'Source polarization is specified by degree of ', &
         'polarization (= AX/(AX+AZ)) and phase angle of AZ from ', &
         'AX, for instance ,'
         	  WRITE(6,*)'Circular polarized :'
         	  WRITE(6,*)'    phase diff.    = +90 (CW) or -90 (CCW) degree'
         	  WRITE(6,*)'    deg. of polar. = 0.5'
         	  WRITE(6,*)'Linear polarized   :'
    	  WRITE(6,*)'    phase diff.    = 0'
    	  WRITE(6,*)'    deg. of polar. = cos(phi)/(cos(phi)+sin(phi))'
    	  WRITE(6,*)'    where      phi = angle of polarization plane from X-axis.'
         	END IF
    	POL_ANGLE	= RNUMBER ('Phase difference ?')
    	POL_DEG		= RNUMBER ('Degree of polarization ?')
    	IF (IVERB.EQ.1) THEN
    	  WRITE(6,*) 'If the absolute phase of AX does not change from one ray',  &
         ' to another, the source is coherent.  If it is randomly ', &
         'distributed, the source is incoherent.'
    	END IF
    	F_COHER		= IRINT ('Incoherent [0] or coherent [1] ?')
    ! **---------------------------------------------------------------------
    ! *				Store the input data
    ! **---------------------------------------------------------------------
    ! ** The format is the same as that used by SILENT.	
    10101	CONTINUE
!        	CALL	FNAME	(FFILE, 'start', 0, 2)
!         	IDUMM = 0
!         	!! CALL	RWNAME	(FFILE, 'W_SOUR', IDUMM)
!print *,">> PLEASE write: ",trim(FFILE)
!         	!! IF (IDUMM.NE.0) CALL LEAVE ('INPUT_SOURCE','Error writing NAMELIST',IDUMM)
    ! 111	FORMAT (A)


   ! 
   ! and here write the variables to the file...
   !

      open(unit=lun,file='start.00',status="replace",action="write", &
           iostat=iErr)
       if (iErr /= 0 ) then
          call leave("INPUT_SOURCE","Error opening the file: start.00",-55_4)
       end if


    write(unit=lun,fmt=*) "fdistr         = ",fdistr             
    write(unit=lun,fmt=*) "fgrid          = ",fgrid              
    write(unit=lun,fmt=*) "fsour          = ",fsour              
    write(unit=lun,fmt=*) "fsource_depth  = ",fsource_depth      
    write(unit=lun,fmt=*) "f_coher        = ",f_coher            
    write(unit=lun,fmt=*) "f_color        = ",f_color            
    write(unit=lun,fmt=*) "f_phot         = ",f_phot             
    write(unit=lun,fmt=*) "f_pol          = ",f_pol              
    write(unit=lun,fmt=*) "f_polar        = ",f_polar            
    write(unit=lun,fmt=*) "f_opd          = ",f_opd              
    write(unit=lun,fmt=*) "f_wiggler      = ",f_wiggler          
    write(unit=lun,fmt=*) "f_bound_sour   = ",f_bound_sour       
    write(unit=lun,fmt=*) "f_sr_type      = ",f_sr_type          
    write(unit=lun,fmt=*) "istar1         = ",istar1             
    write(unit=lun,fmt=*) "npoint         = ",npoint             
    write(unit=lun,fmt=*) "ncol           = ",ncol               
    write(unit=lun,fmt=*) "n_circle       = ",n_circle           
    write(unit=lun,fmt=*) "n_color        = ",n_color            
    write(unit=lun,fmt=*) "n_cone         = ",n_cone             
    write(unit=lun,fmt=*) "ido_vx         = ",ido_vx             
    write(unit=lun,fmt=*) "ido_vz         = ",ido_vz             
    write(unit=lun,fmt=*) "ido_x_s        = ",ido_x_s            
    write(unit=lun,fmt=*) "ido_y_s        = ",ido_y_s            
    write(unit=lun,fmt=*) "ido_z_s        = ",ido_z_s            
    write(unit=lun,fmt=*) "ido_xl         = ",ido_xl             
    write(unit=lun,fmt=*) "ido_xn         = ",ido_xn             
    write(unit=lun,fmt=*) "ido_zl         = ",ido_zl             
    write(unit=lun,fmt=*) "ido_zn         = ",ido_zn             
    write(unit=lun,fmt=*) "sigxl1         = ",sigxl1             
    write(unit=lun,fmt=*) "sigxl2         = ",sigxl2             
    write(unit=lun,fmt=*) "sigxl3         = ",sigxl3             
    write(unit=lun,fmt=*) "sigxl4         = ",sigxl4             
    write(unit=lun,fmt=*) "sigxl5         = ",sigxl5             
    write(unit=lun,fmt=*) "sigxl6         = ",sigxl6             
    write(unit=lun,fmt=*) "sigxl7         = ",sigxl7             
    write(unit=lun,fmt=*) "sigxl8         = ",sigxl8             
    write(unit=lun,fmt=*) "sigxl9         = ",sigxl9             
    write(unit=lun,fmt=*) "sigxl10        = ",sigxl10            
    write(unit=lun,fmt=*) "sigzl1         = ",sigzl1             
    write(unit=lun,fmt=*) "sigzl2         = ",sigzl2             
    write(unit=lun,fmt=*) "sigzl3         = ",sigzl3             
    write(unit=lun,fmt=*) "sigzl4         = ",sigzl4             
    write(unit=lun,fmt=*) "sigzl5         = ",sigzl5             
    write(unit=lun,fmt=*) "sigzl6         = ",sigzl6             
    write(unit=lun,fmt=*) "sigzl7         = ",sigzl7             
    write(unit=lun,fmt=*) "sigzl8         = ",sigzl8             
    write(unit=lun,fmt=*) "sigzl9         = ",sigzl9             
    write(unit=lun,fmt=*) "sigzl10        = ",sigzl10            
    write(unit=lun,fmt=*) "conv_fact      = ",conv_fact          
    write(unit=lun,fmt=*) "cone_max       = ",cone_max           
    write(unit=lun,fmt=*) "cone_min       = ",cone_min           
    write(unit=lun,fmt=*) "epsi_dx        = ",epsi_dx            
    write(unit=lun,fmt=*) "epsi_dz        = ",epsi_dz            
    write(unit=lun,fmt=*) "epsi_x         = ",epsi_x             
    write(unit=lun,fmt=*) "epsi_z         = ",epsi_z             
    write(unit=lun,fmt=*) "hdiv1          = ",hdiv1              
    write(unit=lun,fmt=*) "hdiv2          = ",hdiv2              
    write(unit=lun,fmt=*) "ph1            = ",ph1                
    write(unit=lun,fmt=*) "ph2            = ",ph2                
    write(unit=lun,fmt=*) "ph3            = ",ph3                
    write(unit=lun,fmt=*) "ph4            = ",ph4                
    write(unit=lun,fmt=*) "ph5            = ",ph5                
    write(unit=lun,fmt=*) "ph6            = ",ph6                
    write(unit=lun,fmt=*) "ph7            = ",ph7                
    write(unit=lun,fmt=*) "ph8            = ",ph8                
    write(unit=lun,fmt=*) "ph9            = ",ph9                
    write(unit=lun,fmt=*) "ph10           = ",ph10               
    write(unit=lun,fmt=*) "rl1            = ",rl1                
    write(unit=lun,fmt=*) "rl2            = ",rl2                
    write(unit=lun,fmt=*) "rl3            = ",rl3                
    write(unit=lun,fmt=*) "rl4            = ",rl4                
    write(unit=lun,fmt=*) "rl5            = ",rl5                
    write(unit=lun,fmt=*) "rl6            = ",rl6                
    write(unit=lun,fmt=*) "rl7            = ",rl7                
    write(unit=lun,fmt=*) "rl8            = ",rl8                
    write(unit=lun,fmt=*) "rl9            = ",rl9                
    write(unit=lun,fmt=*) "rl10           = ",rl10               
    write(unit=lun,fmt=*) "bener          = ",bener              
    write(unit=lun,fmt=*) "pol_angle      = ",pol_angle          
    write(unit=lun,fmt=*) "pol_deg        = ",pol_deg            
    write(unit=lun,fmt=*) "r_aladdin      = ",r_aladdin          
    write(unit=lun,fmt=*) "r_magnet       = ",r_magnet           
    write(unit=lun,fmt=*) "sigdix         = ",sigdix             
    write(unit=lun,fmt=*) "sigdiz         = ",sigdiz             
    write(unit=lun,fmt=*) "sigmax         = ",sigmax             
    write(unit=lun,fmt=*) "sigmay         = ",sigmay             
    write(unit=lun,fmt=*) "sigmaz         = ",sigmaz             
    write(unit=lun,fmt=*) "vdiv1          = ",vdiv1              
    write(unit=lun,fmt=*) "vdiv2          = ",vdiv2              
    write(unit=lun,fmt=*) "wxsou          = ",wxsou              
    write(unit=lun,fmt=*) "wysou          = ",wysou              
    write(unit=lun,fmt=*) "wzsou          = ",wzsou              
    write(unit=lun,fmt=*) "plasma_angle   = ",plasma_angle       
    write(unit=lun,fmt=*) "file_traj      = ",trim(file_traj    )
    write(unit=lun,fmt=*) "file_source    = ",trim(file_source  )
    write(unit=lun,fmt=*) "file_bound     = ",trim(file_bound   )
    write(unit=lun,fmt=*) "oe_number      = ",oe_number          
    write(unit=lun,fmt=*) "idummy         = ",idummy             
    write(unit=lun,fmt=*) "dummy          = ",dummy              
    write(unit=lun,fmt=*) "f_new          = ",f_new              

    close(unit=lun)
       WRITE(6,*)'Exit from INPUT_SOURCE'

    END PROGRAM INPUT_SOURCE
