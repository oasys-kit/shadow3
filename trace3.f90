! C+++
! C	PROGRAM		TRACE3
! C
! C	PURPOSE		Main program. Simplified version of trace
! C			that works only for menu mode (i.e., with fixed
! C			file names: 
! C			begin.dat for source
! C			systemfile.dat for system 
! C
! C    LIMITATIONS      Only work for Geometrical Sources
! C---
Program	Trace3

  use shadow_globaldefinitions
  use stringio,                   only: fname
  use shadow_beamio,              only: beamGetDim, beamLoad
  use shadow_variables
  use shadow_kernel


  implicit none
  
  type (poolSource)                         :: src
  type (poolOE)                             :: oei
  integer(kind=ski)                         :: iCount, numarg
  integer(kind=ski)                         :: eof,noe, lun=37
  integer(kind=ski)                         :: nCol1,nPoint1,iFlag,iErr
  character(len=sklen),dimension(100)       :: infile
  real(kind=skr),dimension(:,:),allocatable :: ray18
  character(len=2)                          :: opt
  logical                                   :: use_src, use_trc
  
  use_src = .false.
  use_trc = .false.
  opt = "no"
  numarg = iargc()
  if(numarg.ne.0) call getarg(1,opt)
  if(opt=="-s") use_src=.true.
  if(opt=="-t") use_trc=.true.
  if(opt=="-a") then 
    use_src=.true. 
    use_trc=.true.
  end if
  if(.not.use_src) then
    if(.not.use_trc) then
      write(*,*) "trace needs one argument:"
      write(*,*) " -s for generating source only following start.00"
      write(*,*) " -t for tracing only following systemfile.dat, it reads ray from begin.dat"
      write(*,*) " -a to generate and trace all toghether"
      write(*,*) ""
      stop
    end if
  end if
      
  if(use_src) then   
    call PoolSourceLoad(src,"start.00")
    allocate( ray18(18,src%nPoint) )
    call sourceGeom(src,ray18,src%nPoint)        
    call beamWrite(ray18, iErr, src%nCol, src%nPoint, "begin.dat")
    nCol1 = src%nCol
    nPoint1 = src%nPoint
  end if
  
  if(use_trc.and.(.not.use_src)) then
    call beamGetDim ('begin.dat',nCol1,nPoint1,iFlag,iErr)
    allocate( ray18(18,nPoint1) )
    ray18=0.0d0
    call beamLoad(ray18, iErr, nCol1, nPoint1, 'begin.dat')
    nCol = nCol1
    nPoint = nPoint1
  end if
  

!
! reads systemfile.dat
!
  if(use_trc) then
    open (unit=lun, file='systemfile.dat', status='old', iostat=eof)
    if (eof /= 0 ) then
      print *,'eof=',eof
      print *,"trace3: Problems reading file: systemfile.dat "
      stop
    else
    !
    ! loops over start.xx files contained in systemfile.dat
    !
      iCount = 0
      do while (eof==0)  ! enters in an infinite loop over oe's
        iCount = iCount+1
        read (unit=lun, fmt='(A)', iostat=eof) infile(iCount)
      end do
      noe = iCount-1
    endif
    close (unit=lun)

    !
    ! loop over oe's
    !
    do iCount=1,noe   ! enters in an infinite loop over oe's

      ! reads start.xx into pool0i

      write(6,*)'Tracing optical element # ',iCount
      call PoolOELoad(oei,infile(iCount)) 
      call PoolOEWrite(oei,"pippo.00")
      call TraceOE(oei,ray18,nPoint1,iCount)

      ! write end.xx
      call fName(ffile, 'end', iCount, izero)
      call PoolOEWrite(oei,ffile)

    end do
  end if
end Program trace3


