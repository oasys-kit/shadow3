! C+++
! C	PROGRAM		TRACE
! C
! C	PURPOSE		Main trace program. Handles the switchboard.
! C	
! C			Note that this program is kept for compatibility
! C			with shadow2.x. A new more structurated program
! C			trace3 is also available.
! C	
! C
! C---
Program	Trace

        use shadow_globaldefinitions
        use stringio
        use shadow_beamio
        use shadow_variables
        use shadow_kernel 

	implicit none

        character(len=sklen)      :: mode, arg
	integer(kind=ski)       :: icount,ipass,nsave
	integer(kind=ski)       :: numarg
	integer(kind=ski)       :: ncol1,np,iflag,ierr,iTerminate
	integer(kind=ski)       :: iSysFileOpened=0

        real(kind=skr),dimension(:,:),allocatable :: ray,phase,ap
	logical				          :: logicalFlag=.true.

! C
! C
! C Get the command line of the program to find out what the mode is. The
! C allowed modes are:
! C 		MENU
! C 		BATCH
! C		PROMPT
! C If no mode is specified, the default mode is PROMPT.
! C


        numArg = COMMAND_ARGUMENT_COUNT()

        SELECT CASE (numarg) 
          CASE (0)
              mode = 'menu'
          CASE (2)
 	    CALL GETARG (1, ARG)
            IF (ARG (1:2) .NE. '-m') THEN 
               print *,'Usage: trace [-m menu/prompt/batch]'
               call exit(1) 
            END IF
 	    CALL GETARG (2, ARG)
            mode = arg
          CASE DEFAULT
              PRINT *,'Usage: trace [-m menu/prompt/batch]'
              CALL EXIT(1) 
        END SELECT

        !call FStrUpCase(mode)

  	icount = 0
       	ipass  = 1
  	nsave = 0
  	iTerminate = 0

! C
! C Start by inquiring about the optical system
! C
	DO WHILE (logicalFlag)  ! enters in an infinite loop over oe's
        !
        ! srio: this will change input mode, and load
        ! input variables (start.xx)
        !
        CALL Reset
   	CALL Switch_Inp (mode,icount,iTerminate,iSysFileOpened)
        !IF (iTerminate == 1) STOP 
        IF (iTerminate == 1) return

        !
        ! it is necessary to allocate main arrays (ray, phase, ap) here, 
        ! at the main level. 
        ! 

       IF  (  (.NOT. ALLOCATED(Ray)) &
            .OR.   (.NOT. ALLOCATED(Ap)) &
            .OR.   (.NOT. ALLOCATED(Phase)) ) THEN

        CALL beamGetDim (file_source,ncol1,np,iflag,ierr)

         IF ((iflag.NE.0).OR.(ierr.NE.0)) THEN
            PRINT *,'Error: TRACE: beamGetDim: Error in file: '//TRIM(file_source)
            return
            ! STOP
         ELSE
 
            !
            ! allocate arrays
            !
            IF (ALLOCATED(ray)) DEALLOCATE(ray)
            IF (ALLOCATED(ap)) DEALLOCATE(ap)
            IF (ALLOCATED(phase)) DEALLOCATE(phase)
            IF (.NOT. ALLOCATED(ray)) then
               ALLOCATE(ray(ncol1,np),STAT=ierr)
               IF (ierr /= 0) THEN
                  !PRINT *,"TRACE: Error allocating ray" ; STOP 4
                  PRINT *,"Error: TRACE: Error allocating ray" 
                  return
               END IF
            END IF
            IF (.NOT. ALLOCATED(ap)) THEN
               ALLOCATE(ap(3,np),STAT=ierr)
               IF (ierr /= 0) THEN
                  ! PRINT *,"TRACE: Error allocating ray" ; STOP 4
                  PRINT *,"Error: TRACE: Error allocating ray" 
                  return
               END IF
            END IF
            IF (.NOT. ALLOCATED(phase)) THEN
               ALLOCATE(phase(3,np),STAT=ierr)
               IF (ierr /= 0) THEN
                  !print *,"TRACE: Error allocating ray" ; STOP 4
                  print *,"Error: TRACE: Error allocating ray" 
                  return
               END IF
            END IF

	    !read source file (nor here... it is done in msetup)
	    !  print *,">> TRACE calling  RBEAM for reading "//trim(file_source)
	    !  CALL    RBEAM(file_source,Ray,Phase,Ap,iErr)
	    !  write(*,*) '>>>> TRACE Ray(1,5): ',Ray(1,5)
	    !  write(*,*) '>>>> TRACE IERR: ',IERR
	    !  print *,">>>> TRACE BACK FROM RBEAM"
	    
            ! put dimensions in variable pool
            npoint=np
            ncol=ncol1
         ENDIF
       END IF
 

  	CALL Trace_Step (nsave, icount, ipass, ray, phase, ap)
	END DO ! end do while infinite loop

END Program trace


