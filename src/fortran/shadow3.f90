
! C+++
! C
! C	PROGRAM		shadow3
! C
! C	PURPOSE		Run shadow3 
! C                     This is the main command interface to shadow3
! C                     All tools (pre,post-processorrs, tools, kernel)
! C                     are available here. 
! C
! C	INPUT           command line
! C
! C	OUTPUT          shadow files
! C
! C
! C---
PROGRAM  Shadow3
  use shadow_globaldefinitions
  use stringio
  use shadow_version
  use shadow_beamio
  use shadow_variables
  use shadow_kernel
  use shadow_crl
  use shadow_synchrotron
  use shadow_pre_sync    ! undulator preprocessors
  use shadow_pre_sync_urgent ! undulator preprocessor undul_phot_urgent
  use shadow_preprocessors       ! general preprocessors
  use shadow_postprocessors      ! general postprocessors

  implicit none

  
  character(len=sklen)     ::  inCommand,inCommandLow,arg,mode
  integer(kind=ski)      :: numArg,indx,iErr,i_Device,itmp

 
inCommand = "" 
inCommandLow = "" 

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
        IF (arg (1:1) .NE. '-') THEN
          inCommand = arg
          inCommandLow = arg
          CALL FStrLoCase(inCommandLow)
        END IF
        ! this option (ascii output) has been eliminated in Shadow3 (srio)
        !  	    IF (ARG (1:2) .EQ. '-a' .OR. ARG (1:2) .EQ. '-A') THEN
        !  		IOFORM = 1
        !  		BGNFILE = 'begin.dat.ascii'
        indx = indx + 1
     END DO
  ELSE ! if no argument is entered, display banner
        print *,'                                                                           '
        print *,'                                                                           '
        print *,'                                                                           '
        print *,'  :::::::  :::    ::            :::    :::::::        :::::     :::  ::  ::'
        print *,' ::::::::  :::    ::           ::::    ::::::::      :::::::    :::  ::  ::'
        print *,':::        :::    ::          ::: :    :::   :::    :::    ::   :::  ::  ::'
        print *,':::        :::    ::         ::: ::    :::    ::    :::    ::   :::  ::  ::'
        print *,' :::::::   :::::::::        :::  ::    :::    ::    :::    ::   :::  ::  ::'
        print *,'  :::::::  :::::::::       :::   ::    :::    ::    :::    ::   :::  ::  ::'
        print *,'       ::  :::    ::      :::    ::    :::    ::    :::    ::   :::  ::  ::'
        print *,'      :::  :::    ::     :::     ::    :::   :::    :::   :::   ::: ::: :::'
        print *,'::::::::   :::    ::    :::      ::    ::::::::      :::::::     ::::::::: '
        print *,':::::::    :::    ::   :::       ::    :::::::        :::::       ::: :::  '
        print *,'                                                                           '
        print *,'                                                                           '
        print *,'                                                                           '
        print *,'SHADOW v3.0                                                                '
        print *,'                                                                           '

  END IF 

! C
! C If the inCommand was not supplied in the command line, ask for it.
! C
!  IF (inCommand(1:1).EQ." ") THEN
!     inCommand  =  RString ('GO => Enter command: ')
!  ENDIF
  

!10 CONTINUE
DO   ! infinite loop starts

inCommandLow=inCommand
CALL FStrLoCase(inCommandLow)

SELECT CASE (inCommandLow)
  CASE ("make_id")
     !c
     !c	Specify Undulator
     !c
     WRITE(6,*) ' '
     WRITE(6,*) ' '
     WRITE(6,*) 'Type of Insertion Device.'
     WRITE(6,*) 'Enter: '
     WRITE(6,*) 'for wiggler   (large K)      [ 1 ]'
     WRITE(6,*) 'for undulator (small K)      [ 2 ]'
     I_DEVICE = IRINT ('Then ? ')
 
     SELECT CASE (i_Device)
       CASE(1)  ! wiggler
          WRITE(6,*) 'Type of wiggler: ' 
          WRITE(6,*) ' [0] conventional (sinosoidal field)'
          WRITE(6,*) ' [1] external magnetic field'
          itmp = IRINT ('Then ? ')
          if (itmp .eq. 1) then
              CALL epath_b(i_Device)
          else
              CALL epath(i_Device)
          endif
          CALL nphoton
          CALL input_source1
          CALL RWNAME('start.00','W_SOUR',iErr)
          IF (iErr /= 0) THEN 
             print *,'Error writing file start.00'
             stop 'Aborted'
          END IF
       CASE(2)  ! undulator
          CALL epath(i_Device)
          CALL Undul_Set
          WRITE(6,*) 'Code used for Undulator emission: ' 
          WRITE(6,*) 'SHADOW  [ 0 ]'
          WRITE(6,*) 'URGENT  [ 1 ]'
          itmp = IRINT ('Then ? ')
          if (itmp .eq.1) then
              CALL Undul_Phot_Urgent
          else
              CALL Undul_Phot
          endif
          CALL Undul_Cdf
          CALL input_source1
          CALL RWNAME('start.00','W_SOUR',iErr)
          IF (iErr /= 0) THEN 
             print *,'Error writing file start.00'
             stop 'Aborted'
          END IF
       CASE DEFAULT
         STOP 'ID not yet implemented'
     END SELECT
     print *,'------------------------------------------------------------------------------'
     print *,'GO make_id: '
     print *,'            all files created.'
     print *,'            make source by running: gen_source start.00'
     print *,'------------------------------------------------------------------------------'
     inCommand=""

  CASE ("trace")
     CALL shadow3trace
     inCommand=""
  CASE ("source")
     CALL shadow3source
     inCommand=""
  CASE ("focnew")
     CALL focnew
     inCommand=""
  CASE ("intens")
     CALL intens
     inCommand=""
  CASE ("recolor")
     CALL recolor
     inCommand=""
  CASE ("ffresnel")
     CALL ffresnel
     inCommand=""
  CASE ("ffresnel2d")
     CALL ffresnel2d_interface
     inCommand=""
  CASE ("plotxy")
     CALL plotxy
     inCommand=""
  CASE ("histo1")
     CALL histo1
     inCommand=""
  CASE ("translate")
     CALL translate
     inCommand=""
  CASE ("sysinfo")
     CALL sysinfo
     inCommand=""
  CASE ("mirinfo")
     CALL mirinfo
     inCommand=""
  CASE ("sourcinfo")
     CALL sourcinfo
     inCommand=""
  CASE ("bragg")
     CALL bragg
     inCommand=""
  !CASE ("grade_mlayer")
  !   CALL grade_mlayer
  !   inCommand=""
  CASE ("pre_mlayer")
     CALL pre_mlayer
     inCommand=""
  CASE ("prerefl")
     CALL prerefl
     inCommand=""
  CASE ("presurface")
     CALL presurface
     inCommand=""
  !CASE ("cdf_z")
  !   CALL cdf_z
  !   inCommand=""
  CASE ("undul_phot_urgent")
     CALL undul_phot_urgent
     inCommand=""
  CASE ("epath")
     i_device = 0
     CALL epath(i_Device)
     inCommand=""
  CASE ("epath_b")
     i_device = 1 ! always wiggler
     CALL epath_b(i_Device)
     inCommand=""
  CASE ("nphoton")
     i_device = 0
     CALL nPhoton
     inCommand=""
  CASE ("undul_set")
     CALL undul_set
     inCommand=""
  CASE ("undul_phot")
     CALL undul_phot
     inCommand=""
  CASE ("undul_phot_dump")
     CALL undul_phot_dump
     inCommand=""
  CASE ("undul_cdf")
     CALL undul_cdf
     inCommand=""
  CASE ("mkdatafiles")
     CALL genlib
     CALL WriteF12LibIndex
     CALL srcdf
     inCommand=""
  CASE ("input_source")
     CALL input_source1
     CALL RWNAME('start.00','W_SOUR',iErr)
     IF (iErr /= 0) THEN 
             print *,'Error writing file start.00'
             stop 'GO Aborted'
     END IF
     inCommand=""

  ! CRL beta 2011-12-22 srio@esrf.eu - NOT YET LISTED UNDER ?
  ! TODO: list and clean 
  CASE ("precrl")
     print *,'PRECRL: Create a stack of lenses: '
     print *,'0 - single CRL'
     print *,'1 - transfocator (stack of CRLs)'
     itmp  = irint('?>')
     select case(itmp)
       case (0) 
          CALL precrl()
       case (1) 
          CALL pretransfocator()
       case default
     end select
     inCommand=""
  CASE ("runcrl")
     CALL runcrl(0)
     inCommand=""


!  CASE ("srfunc")
!     CALL SrFunc
!     inCommand=""
!  CASE ("srcdf")
!     CALL SrCdf
!     inCommand=""
!  CASE ("genlib")
!     CALL genlib
!     inCommand=""
  CASE ("sysplot")
     CALL sysplot()
     inCommand=""
  CASE ("retrace")
     CALL retrace_interface()
     inCommand=""
  CASE ("minmax")
     CALL minmax()
     inCommand=""
  CASE ("reflag")
     CALL reflag()
     inCommand=""
  CASE ("histo3")
     CALL histo3()
     inCommand=""
  CASE ("exit")
     go to 333 !EXIT
  CASE ("quit")
     go to 333 !EXIT
  CASE ("exit()")
     go to 333 !EXIT
  CASE ("citation")
     print *,' 1)                                                          '
     print *,' F. Cerrina and M. Sanchez del Rio                         '
     print *,' "Ray Tracing of X-Ray Optical Systems"                    '
     print *,' Ch. 35 in Handbook of Optics (volume  V, 3rd edition),    '
     print *,' edited by M. Bass, Mc Graw Hill, New York, 2009.          '
     print *,'                                                           '
     print *,' ISBN: 0071633138 / 9780071633130                          '
     print *,' http://www.mhprofessional.com/handbookofoptics/vol5.php   '
     print *,'                                                           '
     print *,' 2)                                                          '
     print *,' M. Sanchez del Rio, N. Canestrari, F. Jiang and F. Cerrina'
     print *,' "SHADOW3: a new version of the synchrotron X-ray optics modelling package" '
     print *,' J. Synchrotron Rad. (2011). 18, 708-716                   '
     print *,' http://dx.doi.org/10.1107/S0909049511026306               '
     print *,'                                                           '
     inCommand="" 
  CASE ("license")
     print *,'    SHADOW3  ray-tracing code for optics                                      '
     print *,'    Copyright (C) 2010  F.Cerrina, M.Sanchez del Rio, F. Jiang, N. Canestrari '
     print *,'                                                                              '
     print *,'    This program is free software: you can redistribute it and/or modify      '
     print *,'    it under the terms of the GNU General Public License as published by      '
     print *,'    the Free Software Foundation, either version 3 of the License, or         '
     print *,'    (at your option) any later version.                                       '
     print *,'                                                                              '
     print *,'    This program is distributed in the hope that it will be useful,           '
     print *,'    but WITHOUT ANY WARRANTY; without even the implied warranty of            '
     print *,'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             '
     print *,'    GNU General Public License for more details.                              '
     print *,'                                                                              '
     print *,'    You should have received a copy of the GNU General Public License         '
     print *,'    along with this program.  If not, see <http://www.gnu.org/licenses/>.     '
     print *,'                                                                              '
     inCommand="?"    
  CASE ("help")
     inCommand="?"
  CASE ("?")
     print *,' '
     print *,' '
     print *,'                            S H A D O W'
     print *,' '
     print *,' '
     print *,'Commands Available at this level: '
     print *,'  [MAIN]            : source trace'
     print *,'  [PRE-PROCESSORS]  : prerefl bragg presurface'
     !print *,'                    : input_source pre_mlayer grade_mlayer'
     print *,'                    : input_source pre_mlayer '
     print *,'                    : make_id { epath epath_b nphoton undul_set'
     print *,'                    :   undul_phot (undul_phot_urgent) undul_phot_dump'
     print *,'                    :   undul_cdf}' 
     print *,'                    : jntpscalc mkdatafiles' 
     print *,'  [POST-PROCESSORS] : histo1 plotxy sysplot translate '
     print *,'                    : sourcinfo mirinfo sysinfo'
     print *,'                    : focnew intens recolor ffresnel ffresnel2d'
     print *,'                    : retrace minmax reflag histo3'
     print *,'                    : prerefl_test pre_mlayer_scan emittance_test '
     print *,'  [OTHER]           : exit exit() quit help ? version license citation'
     print *,'  [OP SYSTEM ACCESS]: $<command>'
     print *,''
     print *,''
     inCommand=""
  CASE ("version")
     call shadow_version_info
     inCommand="?"
  CASE ("prerefl_test")
     CALL prerefl_test()
     inCommand=""
  CASE ("pre_mlayer_scan")
     CALL pre_mlayer_scan()
     inCommand=""
  CASE ("presurface_translate")
     CALL presurface_translate()
     inCommand=""
  CASE ("wiggler_spectrum")
     CALL wiggler_spectrum()
     inCommand=""
  CASE ("emittance_test")
     CALL emittance_test()
     inCommand=""
  CASE ("jntpscalc")
     CALL jntpscalc()
     inCommand=""
  !undocumented, for test purposes only
  CASE ("pspect_test")
     CALL pspect_test()
     inCommand=""
  CASE DEFAULT
     IF (inCommand(1:1) == "$") THEN 
        CALL SYSTEM( inCommand(2:sklen) )
     ENDIF
     inCommand  =  RString ('shadow3> ')
END SELECT 

END DO ! infinite loop

333 continue
print *,'Exit shadow3'

END PROGRAM Shadow3
