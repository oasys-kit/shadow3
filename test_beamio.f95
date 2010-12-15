
!----
!----  Program test_beamio
!----
!----  testing the module: beamio
!---- 
!----  srio@esrf.eu 2009-09-18
!---- 
!
Program test_beamio
    !---- Use Modules ----!
    use beamio

    !---- Variables ----!
    implicit none


!!  declarations (new way, with allocatable arrays)
        real(kind=kind(1.0d0)), dimension(:,:), allocatable :: aRay,aRay18,aAp,aPhase
        CHARACTER(len=512)                        ::          fileIn, fileOut
        INTEGER(kind=4)                           :: nCol1,np,iFlag,iErr,iForm,i,j


!! read with rbeam

        filein = 'begin.dat'
        iform=0

        write(*,*) 'Calling beamAnalyzer... '

        CALL    RBEAMANALYZE (FILEIN,NCOL1,NP,IFLAG,IERR)
           write(*,*) 'test_beamio:rbeamAnalyze: NCOL: ',NCOL1
           write(*,*) 'test_beamio:rbeamAnalyze: NP: ',NP
           write(*,*) 'test_beamio:rbeamAnalyze: IFLAG: ',IFLAG
           write(*,*) 'test_beamio:rbeamAnalyze: IERR: ',IERR

         IF ((IFLAG.ne.0).OR.(IERR.ne.0)) THEN
            print *,'rBeamAnalyze: Error in file: ',FILEIN
         ELSE
 
            !
            ! allocate arrays
            !
            if (ALLOCATED(aRay)) DEALLOCATE(aRay18)
            if (ALLOCATED(aAp)) DEALLOCATE(aAp)
            if (ALLOCATED(aPhase)) DEALLOCATE(aPhase)
            if (.not. ALLOCATED(aRay)) then
               ALLOCATE(aRay(12,NP),stat=ierr)
               if (ierr /= 0) then
                  print *,"TEST_BEAMIO: Error allocating aray" ; stop 4
               end if
            end if
            if (.not. ALLOCATED(aAp)) then
               ALLOCATE(aAp(3,NP),stat=ierr)
               if (ierr /= 0) then
                  print *,"TEST_BEAMIO: Error allocating aray" ; stop 4
               end if
            end if
            if (.not. ALLOCATED(aPhase)) then
               ALLOCATE(aPhase(3,NP),stat=ierr)
               if (ierr /= 0) then
                  print *,"TEST_BEAMIO: Error allocating aray" ; stop 4
               end if
            end if

           print *,"TEST_BEAMIO:  size(aRay): ",size(aRay)
           print *,"TEST_BEAMIO:  size(aRay,1): ",size(aRay,1)
           print *,"TEST_BEAMIO:  size(aRay,2): ",size(aRay,2)
           print *,"TEST_BEAMIO:  size(aPhase,1): ",size(aPhase,1)
           print *,"TEST_BEAMIO:  size(aPhase,2): ",size(aPhase,2)
           print *,"TEST_BEAMIO:  size(aAp,1): ",size(aAp,1)
           print *,"TEST_BEAMIO:  size(aAp,2): ",size(aAp,2)
 
           print *,"CALLING RBEAM"
           CALL    RBEAM(FILEIN,aRay,aPhase,aAp,iErr)
           print *,"BACK FROM RBEAM"

           write(*,*) '---------------------------------'
           write(*,*) '   Read begin.dat (using rBeamA): '
           write(*,*) 'in test_beamio NCOL: ',NCOL1
           write(*,*) 'in test_beamio NP: ',NP
           write(*,*) 'in test_beamio IFLAG: ',IFLAG
           write(*,*) 'in test_beamio IERR: ',IERR
           write(*,*) 'aRay(1,5): ',aRay(1,5)
           write(*,*) '---------------------------------'
         ENDIF
 
!! read with rbeamA18

         CALL    RBEAMANALYZE (FILEIN,NCOL1,NP,IFLAG,IERR)
         IF ((IFLAG.ne.0).OR.(IERR.ne.0)) THEN
            print *,'test_beamio: rBeamAnalyze: Error in file: ',FILEIN
         ELSE
 
            !
            ! allocae arrays
            !
            if (ALLOCATED(aRay18)) DEALLOCATE(aRay18)
            if (.not. ALLOCATED(aRay18)) then
               ALLOCATE(aRay18(18,NP),stat=ierr)
               if (ierr /= 0) then
                  print *,"TEST_BEAMIO: Error allocating aray" ; stop 4
               end if
            end if
            CALL    RBEAM18(FILEIN,aRay18,iErr)
 
            write(*,*) '---------------------------------'
            write(*,*) '   Read begin.dat (using rBeam18): '
            write(*,*) 'in test_beamio NCOL: ',NCOL1
            write(*,*) 'in test_beamio NP: ',NP
            write(*,*) 'in test_beamio IFLAG: ',IFLAG
            write(*,*) 'in test_beamio IERR: ',IERR
            write(*,*) 'in test_beamio aRay18(1,5): ',aRay18(1,5)
            write(*,*) 'in test_beamio RAY18(17,50): ',aRay18(17,50)
            write(*,*) '---------------------------------'
       ENDIF

!! write with write_off

         fileout = 'junk.dat'
         print *, "...calling WRITE_OFF"
         CALL WRITE_OFF (FILEOUT,aRay,aPhase,aAp,NCOL1,NP,IFLAG,IFORM,IERR)

         write(*,*) '---------------------------------'
         write(*,*) '   Write junk.dat (using write_off): '
         write(*,*) 'FILEOUT: ',FILEOUT
         write(*,*) 'NCOL: ',NCOL1
         write(*,*) 'NP: ',NP
         write(*,*) 'IFLAG: ',IFLAG
         write(*,*) 'IFORM: ',IFORM
         write(*,*) 'IERR: ',IERR
         write(*,*) '---------------------------------'
         write(*,*) ' '


!! write with write_off

         fileout = 'junk18.dat'
         print *, "...calling WRITE_OFF18"
         CALL    WRITE_OFF18 (FILEOUT,aRay18,IERR)

         write(*,*) '---------------------------------'
         write(*,*) '   Write junka18.dat (using write_off18): '
         write(*,*) 'FILEOUT: ',FILEOUT
         write(*,*) 'NCOL: ',NCOL1
         write(*,*) 'NP: ',NP
         write(*,*) 'IFLAG: ',IFLAG
         write(*,*) 'IFORM: ',IFORM
         write(*,*) 'IERR: ',IERR
         write(*,*) '---------------------------------'
         write(*,*) ' '


         end program test_beamio
