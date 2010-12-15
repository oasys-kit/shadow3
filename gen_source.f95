! C+++
! C	PROGRAM		GEN_SOURCE
! C
! C	PURPOSE		Reads in a NAMELIST file and create the specified
! C			source
! C
! C	INPUT		A NAMELIST type file
! C
! C	OUTPUT		A [BINARY|ASCII] file [BEGIN.DAT|BEGIN.DAT.ASCII]
! C
! C	COMMAND LINE	[-a|b] [start_file_name]
! C		-b:	Create binary output BEGIN.DAT (default)
! C		-a:	Create ASCII output BEGIN.DAT.ASCII (for debugging?)
! C		start_file_name:
! C			Default START.00, unless specified explicitly.
! C
! C---
    PROGRAM  GEN_SOURCE

!        use source_g
        use stringio, only: rstring
        use shadow

        implicit none

     	character(len=512)   ::  INFILE,OUTFILE,BGNFILE,ARG
	integer(kind=4)              ::   IOFORM=0
	integer(kind=4)              ::   INDEX
	integer(kind=4)              ::   numArg



	INFILE  = " "
	BGNFILE  = "begin.dat"

! C
! C Look at the command line for user parameters. only for unix machines.
! C Let's the user choose ASCII option on the output BEGIN file.
! C
        numArg = command_argument_count()
 	IF (NUMARG.NE.0) THEN
 	  INDEX = 1
          DO WHILE (INDEX .LE. NUMARG)
            !! Curiously get_command_argument is not recognised by g95 in 64bit
            !! Bacll to OLD way, which seems to work
 	    !! CALL get_command_argument (INDEX, ARG)
 	    CALL getarg (INDEX, ARG)
            IF (ARG (1:1) .NE. '-') INFILE = ARG
            ! this option (ascii output) has been eliminated (srio)
            !  	    IF (ARG (1:2) .EQ. '-a' .OR. ARG (1:2) .EQ. '-A') THEN
            !  		IOFORM = 1
            !  		BGNFILE = 'begin.dat.ascii'
 	    INDEX = INDEX + 1
          END DO
        ENDIF

! C
! C If the INFILE was not supplied in the command line, ask for it.
! C
	IF (INFILE(1:1).EQ." ") THEN
     	  INFILE  =  RSTRING ('SOURCE => File with source specifications ? ')
	ENDIF

        ! print *,">> calling source1 with :"
        ! print *,">>    parameters file: ",trim(infile)
        ! print *,">>    output file name: ",trim(bgnFile)
        ! print *,">>    format(0=binary): ",ioform
        CALL  SOURCE1 (infile,BGNFILE, IOFORM)
        ! print *,">> back from source1"

    END PROGRAM GEN_SOURCE
