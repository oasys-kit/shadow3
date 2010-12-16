! C+++
! C	PROGRAM		TRACE3
! C
! C	PURPOSE		Main program. Simplified version of trace
! C			that works only for menu mode (i.e., with fixed
! C			file names: 
! C			begin.dat for source
! C			systemfile.dat for system 
! C
! C---
Program	Trace3

        use shadow_kind
        use stringio,        only: fname
        use beamio,          only: RBeamAnalyze, RBeam18
        use shadow_variables
        use shadow_kernel,   only: PoolOELoad, TraceOE, PoolOEWrite


	implicit none

        type (poolOE)                             :: pool0i
	integer(kind=ski)                         :: icount 
	integer(kind=ski)                         :: eof,noe, lun
	integer(kind=ski)                         :: itwo=2, izero=0
	integer(kind=ski)                         :: ncol1,npoint1,iflag,ierr
	character(len=512),dimension(100)         :: infile
        real(kind=skr),dimension(:,:),allocatable :: ray18


!
! loads source
!
	file_source = 'begin.dat'
	!
	! it is necessary to allocate ray array here, at the main level. 
	! 
        CALL    RBeamAnalyze (file_source,ncol1,npoint1,iflag,ierr)
        IF ((iflag.ne.0).OR.(ierr.ne.0)) THEN
            print *,'TRACE3: RBeamAnalyze: Error in file: '//trim(file_source)
            stop
        END IF

  	ALLOCATE( RAY18(18,NPOINT1) )
  	ray18=0.0d0

	
	CALL RBeam18(ray18,ierr,ncol1,npoint1,file_source)

	! copy some variables to variable pool
        npoint=npoint1
        ncol=ncol1

!
! reads systemfile.dat
!

	OPEN (UNIT=lun, FILE='systemfile.dat', STATUS='old', IOSTAT=eof)
        if (eof /= 0 ) then
          print *,"TRACE3: File not found: systemfile.dat "
          stop
        else
	  !
	  ! loops over start.xx files contained in systemfile.dat
	  !
	  icount = 0
	  DO WHILE (eof == 0)  ! enters in an infinite loop over oe's
            icount = icount+1
            READ (unit=lun, fmt='(A)', iostat=eof) infile(icount)
	  END DO
	  noe = icount-1
	endif
	close (unit=lun)

!
! loop over oe's
!
DO icount=1,noe   ! enters in an infinite loop over oe's

        ! reads start.xx into pool0i

      	WRITE(6,*)'Tracing optical element # ',icount

	call PoolOELoad(pool0i,infile(iCount)) 
	call TraceOE(pool0i,ray18,npoint1,iCount)

	! write end.xx
     	CALL	FName	(ffile, 'end', icount, itwo)
	CALL PoolOEWrite(pool0i,ffile)

END DO 

END Program trace3


