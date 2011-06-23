! C+++
! C	PROGRAM		GEN_SOURCE
! C
! C	PURPOSE		Reads in a start.00 file and create the specified
! C			source (file begin.dat)
! C
! C	INPUT		Variables in start.00 type file
! C
! C	OUTPUT		A BINARY file begin.dat
! C
! C	COMMAND LINE	gen_source [start_file_name]
! C
! C     NOTE: This program mimics the gen_source program in shadow2
! C           It is maintained for compatibility reasons. 
! C           The same functionality is also in shadow3.f90
!
! C---
PROGRAM  Gen_Source

  use shadow_globaldefinitions    ! Global definitions
  use stringio, only : RString
  use shadow_beamio,   only : beamWrite
  use shadow_variables
  use shadow_kernel
  use shadow_synchrotron

  implicit none
  
  character(len=sklen)     ::  infile,outfile,bgnfile,arg
  integer(kind=ski)      ::  indx, numArg, iErr
  integer (kind=ski)     ::  iSynchrotron=0, ioform=0

  type (poolSource)      ::  pool00
  real(kind=skr), allocatable, dimension(:,:) :: ray

  infile   = " "
  bgnfile  = "begin.dat"
 
! C
! C Look at the command line for user parameters.
! C
  numArg = COMMAND_ARGUMENT_COUNT()
  IF (numarg.NE.0) THEN
     indx = 1
     DO WHILE (indx .LE. numarg)
        !! Curiously get_command_argument is not recognised by g95 in 64bit
        !! Back to OLD way, which seems to work
        !! CALL get_command_argument (INDX, ARG)
        CALL GETARG (indx, arg)
        IF (arg (1:1) .NE. '-') infile = arg
        ! this option (ascii output) has been eliminated in Shadow3 (srio)
        !  	    IF (ARG (1:2) .EQ. '-a' .OR. ARG (1:2) .EQ. '-A') THEN
        !  		IOFORM = 1
        !  		BGNFILE = 'begin.dat.ascii'
        indx = indx + 1
     END DO
  ENDIF

! C
! C If the INFILE was not supplied in the command line, ask for it.
! C
  IF (infile(1:1).EQ." ") THEN
     infile  =  RString ('GEN_SOURCE => File with source specifications ? ')
  ENDIF
  

!
! load variables from start.00
!
  CALL PoolSourceLoad(pool00,infile) 

! 
! allocate ray 
!
!print *,'Allocating array with pool00%npoint: ',pool00%npoint
  ALLOCATE( ray(18,pool00%npoint) )
  ray=0.0d0

  
  IF ((pool00%fdistr.EQ.4).OR.(pool00%fsource_depth.EQ.4).OR.(pool00%f_wiggler.GT.0)) THEN
    iSynchrotron = 1
  ENDIF

  IF (iSynchrotron .eq. 1) THEN 
    CALL  SourceSync (pool00,ray,pool00%npoint)
  ELSE 
    ! Note that the routine sourceGeom (geometrical source) in 
    ! shadow_kernel is a subset of SourceSync (in shadow_sourcesync)
    ! therefore this call is unnecessary because the same work can be
    ! done by SourceSync.
    ! The reason for keeping the two functions is for simplicity and 
    ! to structurate better the shadow3 code: sourceGeom uses a relatively
    ! small number of subroutines and functions, all available in the
    ! kernel, whereas SourceSync is much more complex and is included in
    ! a separated synchrotron module. 
    ! Therefore, users that do not want synchrotron, they just comment 
    ! the "USE shadow_sourcesync" and "CALL SourceSync"
    CALL  sourceGeom(pool00, ray,pool00%npoint)
  ENDIF 
  
  ! write file begin.dat
  CALL beamWrite(ray,ierr,ncol,npoint,bgnfile)
  IF (ierr.NE.0) PRINT *, "GEN_SOURCE: beamWrite failed to write file: "//TRIM(bgnfile)

  ! write end.00 file
  CALL GlobalToPoolSource(pool00)
  CALL PoolSourceWrite(pool00,"end.00")

END PROGRAM Gen_Source
