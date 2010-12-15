!----
!---- MODULE:  BEAMIO
!----
!---- i/o a file with a shadow beam (begin.dat, star.xx, mirr.xx, screen.xx)
!----
!---- Module grouping the adapted f77 routines rbeam, rbeam18, write_off 
!----        newly added: 
!----        write_off18, rbeamanalyze (to deal with allocatable arrays)
!----
!----     srio@esrf.eu 2009-09-18
!----
!----
!---- Example of usage: see test_beamio.f95
!----
!----

Module beamio
    !---- Use Modules ----!

    !---- Variables ----!
    implicit none

    !---- Everything is private unless explicitly made public ----!
    private 

    !---- List of public functions ----!
    public :: rbeamanalyze, rbeam, rbeam18, write_off, write_off18 
    !---- List of public overloaded functions ----!
    !---- List of public subroutines ----!


    !---- List of private functions ----!
    !private :: GfGetValueString, GfGetValueInteger, GfGetValueReal
    !---- List of private subroutines ----!



    !---- Definitions ----!
    !Type, public :: GfType
    !   character(len=512) :: fileName
    !   ! logical for allocation
    !   logical            :: alloc1
    !   integer            :: nLines
    !   integer            :: nVariables
    !   character(len=512), dimension(:), allocatable :: fileLines
    !   character(len=512), dimension(:), allocatable :: variableNames
    !   character(len=512), dimension(:), allocatable :: variableValues
    !End Type GfType


    !---- Interfaces ----!
    !Interface  GfGetValue
    !   Module Procedure GfGetValueString
    !   Module Procedure GfGetValueInteger
    !   Module Procedure GfGetValueReal
    !End Interface

    !Interface  GfSetValue
    !   Module Procedure GfSetValueString
    !   Module Procedure GfSetValueInteger
    !   Module Procedure GfSetValueReal
    !End Interface


  Contains
    !
    !---- Private Functions ----!
    !

    !
    !---- Private Subroutines ----!
    !

    !
    !---- Public Functions ----!
    !

    !
    !---- Public Subroutines ----!
    !

    !!+++
    !!	SUBROUTINE      RBEAMANALYZE  (FNAME,NCOL,NPOINT,IFLAG,IERR)
    !!
    !!	purpose		Returns the number of columns and number of
    !!                       points of a SHADOW binary file. 
    !!
    !!	Input	:	FNAME	(character*(*))	Input file name
    !!	
    !!	Output  :	NCOL	Number of columns for each ray 
    !!			NPOINT	Number of rays
    !!			IFLAG	A flag which is stored on the input file
    !!			IERR = 0, normal return
    !!			     = 1, error opening file
    !!			     = 2, error reading file
    !!
    !!---
    
    subroutine rbeamanalyze (fname,ncol,npoint,iflag,ierr)
    
       !---- Arguments ----!
       character(len=512),     intent(in)      :: fname
       integer(kind=4),        intent(out)     :: ncol, npoint, iflag, ierr
       !---- Local variables ----!
       integer(kind=4)         :: lun
    
       ierr 	=  0
       ncol = 0
       npoint = 0
       iflag = 0
    
       lun = 21
    
       ! print *,">> Opening: ",trim(fname)
       open(unit=lun,file=fname,status="old",action="read",form="unformatted", iostat=iErr)
       ! print *,">> (open) iErr: ",iErr
       if (iErr /= 0 ) then
         print *,"RBEAMANALYZE Error opening image file: : "//trim(fName)
         iErr = 1
         return
       end if
    
       read(unit=lun,iostat=iErr) NCOL, NPOINT, IFLAG
       ! print *,">> rbeamanalyze iErr: ",iErr
       ! print *,">> rbeamanalyze NCOL: ",NCOL
       ! print *,">> rbeamanalyze NPOINT: ",NPOINT
       ! print *,">> rbeamanalyze IFLAG: ",IFLAG
       if (iErr /= 0) then
              print *, "RBEAMANALYZE Error reading file: "//trim(fname)
              iErr = 2
              return
       end if
    
       close (unit=lun)
       return
    end subroutine rbeamanalyze

    !
    !
    !


    !!+++
    !!	SUBROUTINE	RBEAM	(FNAME,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IERR)
    !!
    !!	purpose		reads in a BEAM file from SHADOW and returns it
    !!			to the calling program. This is an intermediate
    !!			subroutine that will be easily modified to suit
    !!			the format of SHADOW.
    !!
    !!	Input	:	FNAME	(character*(*))	Input file name
    !!	
    !!	Output  :	RAY	Array which contains the basic 12 columns that
    !!				defined each ray
    !!                            It must be allocated as input
    !!			PHASE	Array which contains the OPD and the 2 phase 
    !!				angles of the A vector
    !!                            It must be allocated as input
    !!			AP	Array which contains the Ap vector for each ray
    !!                            It must be allocated as input
    !!			IERR = 0, normal return
    !!			     = 1, error opening file
    !!			     = 2, error reading file
    !!    		     = 3, array dimensions do not agree with 
    !!                            file dimensions
    !!    		     = 4, wrong number of columns
    !!
    !!---

	subroutine rbeam (fname,ray,phase,ap,ierr)

        !---- Arguments ----!
     	character(len=512),          intent(in)         :: fname
        real(kind=kind(1.0d0)),dimension(:,:), intent(in out)     :: ray
        real(kind=kind(1.0d0)),dimension(:,:), intent(in out)     :: phase,ap
     	integer(kind=4),             intent(out)        :: ierr

        !---- Local variables ----!
     	integer(kind=4)     :: 	lun,i,j,k,l
        ! variables for input arrays
     	integer(kind=4)     :: 	ncol,npoint
        ! variables for file 
     	integer(kind=4)     :: 	ncol1,npoint1,iflag1,ierr1


        ncol = size(ray,1)
        npoint = size(ray,2)

        ! print *,">> in RBEAM size(ray): ",size(ray)
        ! print *,">> in RBEAM size(ray,1): ",size(ray,1)
        ! print *,">> in RBEAM size(ray,2): ",size(ray,2)
        ! print *,">> in RBEAM size(ap,1): ",size(ap,1)
        ! print *,">> in RBEAM size(ap,2): ",size(ap,2)

        lun = 21
        ! print *,">> Opening: ",trim(fname)
        open(unit=lun,file=fname,status="old",action="read",form="unformatted", iostat=iErr)
        ! print *,"(open) iErr: ",iErr
        if (iErr /= 0 ) then
          print *,"RBEAM Error opening image file: "//trim(fName)
          iErr=1
          return
        end if


        read(unit=lun,iostat=iErr) ncol1, npoint1, iflag1
          ! print *,">> iErr: ",iErr
          ! print *,">> NCOL,NCOL1: ",NCOL,NCOL1
          ! print *,">> NPOINT,NPOINT1: ",NPOINT,NPOINT1
          ! print *,">> IFLAG1: ",IFLAG1

        if (iErr /= 0) then
            print *, "RBEAM Error reading file: "//trim(fname)
	    close (unit=lun)
       	    ierr = 2
       	    return
        end if


        if (iFlag1 /= 0) then
            print *, "RBEAM Error (Flag /=0) in file: "//trim(fname)
	    close (unit=lun)
       	    ierr = 2
       	    return
        end if

 	if (ncol /= ncol1) then
            print *, "RBEAM Error with number of columns"
            print *, "      in input variables: ",NCOL
            print *, "      in file: ",NCOL1
	    close (unit=lun)
       	    ierr = 3
       	    return
        end if


 	if (npoint /= npoint1) then
            print *, "RBEAM Error with number of rays"
            print *, "   in input variables: ",NPOINT
            print *, "   in file: ",NPOINT1
	    close (unit=lun)
       	    ierr = 3
       	    return
        end if

        select case (ncol)
         case (12)
 	  DO I = 1, NPOINT
 	      READ (unit=lun,iostat=iErr) (RAY(J,I), J = 1,12)
              if (iErr /= 0) then
                 print *, "RBEAM Error reading file: "//trim(fname)
	         close (unit=lun)
       	         ierr = 2
       	         return
              end if
          END DO
           
 	 case (13)
 	  DO I =1,NPOINT
 	    READ (unit=lun,iostat=iErr) (RAY(J,I), J = 1,12), PHASE(1,I)
              if (iErr /= 0) then
       	         ierr = 2
                 print *, "RBEAM Error reading file: "//trim(fname)
       	         exit
              end if
          END DO 

  	 case (18) 
 	  DO I = 1,NPOINT
 	    READ (unit=lun,iostat=iErr) &
            (RAY(J,I), J = 1,12), (PHASE(K,I), K = 1,3), (AP(L,I), L = 1,3)
              if (iErr /= 0) then
       	         ierr = 2
                 print *, "RBEAM Error reading file: "//trim(fname)
       	         exit
              end if
          END DO
 	 case default
       	    ierr = 4
            print *, "RBEAM Invalid number of columns: ",ncol
        end select

        close(unit=lun)
      	return
    end subroutine rbeam

    !
    !
    !


    !!+++
    !!	SUBROUTINE	RBEAM18	(FNAME,RAY,IERR)
    !!
    !!	purpose		The same as RBEAM, except return everything in
    !!			one single array RAY(18,N_DIM) instead of three
    !!			separate arrays.
    !!
    !! 	Input	:	FNAME	Input file name
    !!	
    !!	Output  :	RAY	Array which contains the 18 columns that
    !!				defined each ray
    !!                            It must be dimensionated as input.
    !!			NCOL	Number of columns for each ray 
    !!			NPOINT	Number of rays
    !!			IFLAG	A flag which is stored on the input file
    !!			IERR = 0, normal return
    !!			     = 1, error opening file
    !!			     = 2, error reading file
    !!  		     = 3, array dimensions do not agree with 
    !!                            file dimensions
    !! 			     = 4, wrong number of columns
    !!
    !!
    !!---
    
	subroutine rbeam18 (fname,ray,ierr)

        !---- Arguments ----!
     	character(len=512),          intent(in)         :: fname
        real(kind=kind(1.0d0)),dimension(:,:), intent(in out)     :: ray
     	integer(kind=4),             intent(out)        :: ierr

        !---- Local variables ----!
     	integer(kind=4)     :: 	lun,i,j,k,l
        ! variables for input arrays
     	integer(kind=4)     :: 	ncol,npoint
        ! variables for file 
     	integer(kind=4)     :: 	ncol1,npoint1,iflag1,ierr1


        ncol = size(ray,1)
        npoint = size(ray,2)

        ! print *,">> in RBEAM18 size(ray): ",size(ray)
        ! print *,">> in RBEAM18 size(ray,1): ",size(ray,1)
        ! print *,">> in RBEAM18 size(ray,2): ",size(ray,2)

 	if (ncol /= 18) then
            print *, "RBEAM18 Error with number of columns in input variable: ",NCOL
       	    ierr = 4
       	    return
        end if


        lun = 21

        ! print *,">> Opening: ",trim(fname)
        open(unit=lun,file=fname,status="old",action="read",form="unformatted", iostat=iErr)
        ! print *,"(open) iErr: ",iErr
        if (iErr /= 0 ) then
          print *,"RBEAM18 Error opening image file: "//trim(fName)
          iErr=1
          return
        end if


        read(unit=lun,iostat=iErr) ncol1, npoint1, iflag1
          ! print *,">> iErr: ",iErr
          ! print *,">> NCOL,NCOL1: ",NCOL,NCOL1
          ! print *,">> NPOINT,NPOINT1: ",NPOINT,NPOINT1
          ! print *,">> IFLAG1: ",IFLAG1

        if (iErr /= 0) then
            print *, "RBEAM18 Error reading file: "//trim(fname)
	    close (unit=lun)
       	    ierr = 2
       	    return
        end if

        if (iFlag1 /= 0) then
            print *, "RBEAM18 Error (Flag /=0) in file: "//trim(fname)
	    close (unit=lun)
       	    ierr = 2
       	    return
        end if

 	if (npoint /= npoint1) then
            print *, "RBEAM18 Error with number of rays"
            print *, "   in input variables: ",NPOINT
            print *, "   in file: ",NPOINT1
	    close (unit=lun)
       	    ierr = 3
       	    return
        end if

  	if (ncol1.ne.12.and.ncol1.ne.13.and.ncol1.ne.18) then
            print *, "RBEAM18 Error with number of columns in file: ",NCOL
	    close (unit=lun)
       	    ierr = 4
       	    return
        end if


        !!C
        !!C Read in the rays.
        !!C
	DO I = 1, NPOINT
	  read(unit=lun,iostat=iErr) (RAY(J,I), J = 1,ncol1)
          if (iErr /= 0) then
       	    ierr = 2
            print *, "RBEAM18 Error reading file: "//trim(fname)
       	    exit
          end if
        END DO

        close(unit=lun)
      	return
    end subroutine rbeam18

    !
    !
    !

    !!+++
    !!	SUBROUTINE	WRITE_OFF (FNAME,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IERR)
    !!
    !!	Input	:	FNAME	(character*(*))	Output file name
    !!			RAY	Array which contains the basic 12 columns that
    !!				defined each ray
    !!			PHASE	Array which contains the OPD and the 2 phase 
    !!				angles of the A vector
    !!			AP	Array which contains the Ap vector for each ray
    !!			NCOL	Number of columns for each ray 
    !!				(valid NCOL = 12, 13, or 18)
    !!			NPOINT	Number of rays
    !!			IFLAG	A flag which is written to the output file.
    !!			IFORM = 0, Binary output
    !!			      = 1, Formatted output
    !!	
    !!	Output	:	IERR = 0, normal return
    !!			     = 1, error opening file
    !!			     = 2, error writing file
    !!
    !!---
	SUBROUTINE WRITE_OFF (FNAME,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IFORM,IERR)

!!sriosrio          	CALL WRITE_OFF(FNAME,BEGIN,PHASE,AP,NCOL,NTOTAL,IFLAG,IOFORM,IERR)
        !---- Arguments ----!
     	character(len=512),          intent(in)   :: FNAME
     	REAL(kind=kind(1.0d0)),dimension(:,:), intent(in)   :: RAY, PHASE, AP
	integer(kind=4),             intent(in)   :: NCOL, NPOINT, IFLAG, IFORM
	integer(kind=4),             intent(out)  :: IERR


        !---- Local variables ----!
	integer(kind=4),parameter                 :: IOUNIT=20
	character(len=80)                         :: FFORMAT
	integer(kind=4)                           :: i,j,k,l


        !!
        !! Check for valid argument of NCOL.
        !!
	IF (NCOL.NE.12.AND.NCOL.NE.13.AND.NCOL.NE.18) THEN
          print *, "BEAMIO-WRITE_OFF Invalid argument of NCOL: ",NCOL
          STOP
        ENDIF

        !!
        !! Decide if output is formatted or unformatted.
        !!

	IF (IFORM .EQ. 0) THEN
	  FFORMAT = 'UNFORMATTED'
	ELSEIF (IFORM .EQ. 1) THEN
	  FFORMAT = 'FORMATTED'
	ELSE
          print *, "BEAMIO-WRITE_OFF Error Invalid argument of IO Format: ",IFORM
          STOP
	ENDIF

        !!
        !! Now do the dirty deed. Also, Remove the INITIALSIZE hint used 
        !! for the VMS version.
        !!

     	OPEN (unit=IOUNIT,FILE=FNAME,STATUS='unknown',FORM=FFORMAT,iostat=iErr)

        if (iErr /= 0 ) then
          print *,"BEAMIO-WRITE_OFF Error opening file: "//trim(fName)
          iErr=1
          return
        end if

	REWIND (IOUNIT)

	IF (IFORM .EQ. 0) THEN
	  WRITE (IOUNIT,ERR=20) NCOL, NPOINT, IFLAG
	ELSE
	  WRITE (IOUNIT,*, ERR=20) NCOL, NPOINT, IFLAG
	ENDIF

	IF (NCOL.EQ.12) THEN
	  IF (IFORM .EQ. 0) THEN
	      DO I = 1, NPOINT
		  WRITE (IOUNIT,ERR=20) (RAY(J,I), J=1,12)
              END DO
	  ELSE
	      DO I = 1, NPOINT
		  WRITE (IOUNIT,*,ERR=20) (RAY(J,I), J=1,12)
              END DO
	  ENDIF
	ELSE IF (NCOL.EQ.13) THEN
	  IF (IFORM.EQ.0) THEN
	      DO I = 1, NPOINT
		  WRITE (IOUNIT,ERR=20) (RAY(J,I), J=1,12), PHASE(1,I)
              END DO
	  ELSE
	      DO I = 1, NPOINT
		  WRITE (IOUNIT,*,ERR=20) (RAY(J,I), J=1,12), PHASE(1,I)
              END DO
	  ENDIF
	ELSE IF (NCOL.EQ.18) THEN
	  IF (IFORM .EQ. 0) THEN
	      DO I = 1, NPOINT
		  WRITE (20,ERR=20) & 
     	              (RAY(J,I), J = 1,12), (PHASE(K,I), K = 1,3),  & 
     	              (AP(L,I), L = 1,3)
              END DO
	  ELSE
	      DO I = 1, NPOINT
		  WRITE (20,*,ERR=20) &
     	              (RAY(J,I), J = 1,12), (PHASE(K,I), K = 1,3), &
     	              (AP(L,I), L = 1,3)
              END DO
	  ENDIF
	ENDIF

        !!
        !! Close file.
        !!
    	CLOSE	(unit=IOUNIT)

        !! successful end
     	IERR	=  0
     	RETURN
        !! unsuccessful end
    20	IERR	=  2
     	RETURN
    END SUBROUTINE WRITE_OFF

    !
    !
    !

    !! 
    !! 	SUBROUTINE	WRITE_OFF18 (FNAME,RAY,IERR)
    !! 
    !! 	Input	:	FNAME	Output file name
    !! 			RAY	Array which contains the basic 12,13 or 18 
    !!                          columns that defined each ray
    !! 	
    !! 	Output	:	IERR = 0, normal return
    !! 			     = 1, error opening file
    !! 			     = 2, error writing file
    !! 
    !! 

	SUBROUTINE	WRITE_OFF18 (FNAME,RAY,IERR)

        !---- Arguments ----!
     	character(len=512),            intent(in)   :: FNAME
     	REAL(kind=kind(1.0d0)),dimension(:,:),   intent(in)   :: RAY
	integer(kind=4),               intent(out)  :: IERR


        !---- Local variables ----!
	integer(kind=4),parameter                   :: IOUNIT=20
	integer(kind=4)                             :: i,j,k,l,ncol,npoint
	integer(kind=4)                             :: iform, iflag
        character(len=80)                           :: fformat


        iflag=0

        ncol = size(ray,1)
        npoint = size(ray,2)

        !! just a warning...
        if (ncol.ne.12.and.ncol.ne.13.and.ncol.ne.18) then
            print *, "WRITE_OFF18 Warning: number of columns: ",ncol
        end if

        ! print *,">> in WRITE_OFF18 size(ray): ",size(ray)
        ! print *,">> in WRITE_OFF18 size(ray,1): ",ncol
        ! print *,">> in WRITE_OFF18 size(ray,2): ",npoint

        !!
        !! Decide if output is formatted or unformatted.
        !!

  
        iform=0
	IF (IFORM .EQ. 0) THEN
	  FFORMAT = 'UNFORMATTED'
	ELSE 
	  FFORMAT = 'FORMATTED'
	END IF

     	OPEN (unit=IOUNIT,FILE=FNAME,STATUS='unknown',FORM=FFORMAT,iostat=iErr)

        if (iErr /= 0 ) then
          print *,"WRITE_OFF18 Error opening file: "//trim(fName)
          iErr=1
          return
        end if

        !!	REWIND (IOUNIT)

	IF (IFORM .EQ. 0) THEN
	  WRITE (IOUNIT,ERR=20) NCOL, NPOINT, IFLAG
	ELSE
	  WRITE (IOUNIT,*, ERR=20) NCOL, NPOINT, IFLAG
	ENDIF


	IF (IFORM .EQ. 0) THEN
	      DO I = 1, NPOINT
		  WRITE (IOUNIT,ERR=20) (RAY(J,I), J=1,NCOL)
              END DO
	ELSE
	      DO I = 1, NPOINT
		  WRITE (IOUNIT,*,ERR=20) (RAY(J,I), J=1,NCOL)
              END DO
	ENDIF

        !! Close file.
    	CLOSE	(unit=IOUNIT)

        !! succesful end
     	IERR	=  0
     	RETURN
        !! unsuccesful end
    20	IERR	=  2
     	RETURN
    END SUBROUTINE WRITE_OFF18
    !
    !
    !

End Module beamio

