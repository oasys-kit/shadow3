    ! C+++
    ! C	PROGRAM		TRANSLATE
    ! C
    ! C	PURPOSE		To translate a binary ray file to a readable ASCII format.
    ! C---
    	PROGRAM		TRANSLATE
    
        use stringio
        use beamio

	IMPLICIT REAL(KIND=KIND(1.0D0)) (A-E,G-H,O-Z)
	IMPLICIT INTEGER(KIND=4)        (F,I-N)

        CHARACTER(len=512)                                   :: FILEIN,FILEOUT
        real(kind=kind(1.0d0)), dimension(:,:), allocatable  :: Ray
    	INTEGER(kind=4)                                      :: IUNIT,OUTFLAG
    	INTEGER(kind=4)                                      :: ICSV
    
    	FILEIN = RSTRING('File for input ? ')
    
        !
        ! allocate RAY and read binary file
        ! 

         ! before calling rbeam18, we must allocate arrays
         ! get information on number of columns and rays from the header 
         CALL    RBEAMANALYZE (FILEIN,NCOL,NPOINT,IFLAG,IERR)
         IF ((IFLAG.ne.0).OR.(IERR.ne.0)) THEN
            call leave ('TRANSLATE','Error accessing file: '//trim(FILEIN),IERR)
         ELSE
            !
            ! allocate arrays
            !
            if (ALLOCATED(RAY)) DEALLOCATE(RAY)
            if (.not. ALLOCATED(RAY)) then
               ALLOCATE(RAY(18,NPOINT),stat=ierr)
               if (ierr /= 0) then
                  call leave ('TRANSLATE','Error allocating array',IERR)
               end if
            end if
            ! RBEAM18 must be calles once RAY is allocated
            CALL RBEAM18(FILEIN,Ray,iErr)
         END IF


        ! CALL	RBEAM18	(FILEIN,RAY,NCOL,NPOINT,IFLAG,IERR)
    
        WRITE(6,*)'Read         ',NPOINT,' rays.'
    	WRITE(6,*)'Each ray has ',NCOL,' entries.'

    	WRITE(6,*) 'Enter filename or - for standard output.'
    	FILEOUT = RSTRING('Output filename or device ? ')

    	NRAY = IRINT('How many rays to translate ? ')
    	ICSV = IYES('Create comma separated values (CSV) output [y/N] ? ')
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
    	    IF (NCOL.EQ.13) THEN
    	      WRITE	(IUNIT,98)		RAY(13,I)
    	    ELSE IF (NCOL.EQ.18) THEN
    	      WRITE (IUNIT,98)		(RAY(J,I),J=13,15)
    	      WRITE (IUNIT,98)		(RAY(J,I),J=16,18)
    	    END IF
    	    WRITE (IUNIT,*)
    	  ELSE
    	    DO 112 J=1,NCOL
    	      WRITE (IUNIT, 99)		RAY(J,I)
    	      IF (J.LT.NCOL) THEN
    		WRITE (IUNIT, '(a1,$)')		','
    	      ENDIF
    112	    CONTINUE
    	    WRITE (IUNIT,*)
    	  ENDIF
    111	CONTINUE
    
    	IF (OUTFLAG.EQ.0) THEN
    	   CLOSE (IUNIT)
         	   WRITE(6,*)'All done.'
    	ENDIF
    
     98	FORMAT(G21.14)
     99	FORMAT(G21.14,$)
    END PROGRAM TRANSLATE
