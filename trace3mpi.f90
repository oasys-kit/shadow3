! C+++
! C	PROGRAM		TRACE3MPI
! C
! C	PURPOSE		Main program. Simplified version of trace
! C			that works only for menu mode (i.e., with fixed
! C			file names: 
! C			begin.dat for source
! C			systemfile.dat for system 
! C
! C     MODIFICATIONS   MPI version by Dimitris Karkoulis
! C
! C
! C---
Program	trace3mpi

  use shadow_globaldefinitions
  use stringio,                   only: fname
  use shadow_beamio,              only: beamGetDim, beamLoad
  use shadow_variables
  use shadow_kernel
!  use mpi

  implicit none
  
  type (poolSource)                         :: src
  type (poolOE)                             :: oei
  integer(kind=ski)                         :: iCount, numarg
  integer(kind=ski)                         :: eof,noe, lun
  integer(kind=ski)                         :: nCol1,nPoint1,iFlag,iErr
  character(len=sklen),dimension(100)       :: infile
  real(kind=skr),dimension(:,:),allocatable :: ray18
  character(len=2)                          :: opt
  logical                                   :: use_src, use_trc
  integer(kind=ski)                         :: mpiid,mpinum,mpierr
  character(100)                            :: mpinames
  integer(kind=ski),dimension(8)            :: s_prof_time,e_prof_time
  real(kind=skr)                            :: start_time, finish_time

  include "mpif.h"
  
  ! initialize MPI
  call date_and_time(values=s_prof_time)
  call MPI_Init(mpierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,mpiid,mpierr)
  call MPI_Comm_size(MPI_COMM_WORLD,mpinum,mpierr)
  call date_and_time(values=e_prof_time)
  ! set timing 
  start_time =s_prof_time(5) * 3600 + s_prof_time(6) * 60 &
           + s_prof_time(7) + 0.001 * s_prof_time(8)
  finish_time=e_prof_time(5) * 3600 + e_prof_time(6) * 60 &
           + e_prof_time(7) + 0.001 * e_prof_time(8)
           write(*,*)'Elapsed time on MPI init:',finish_time-start_time,' myid:',mpiid

  ! check arguments
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
      write(*,*) "trace3mpi needs one argument:"
      write(*,*) " -s for generating source only following start.00"
      write(*,*) " -t for tracing only following systemfile.dat, it reads ray from begin.dat-(n)"
      write(*,*) " -a to generate and trace all toghether (it does not dump begin.dat file)"
      write(*,*) ""
      stop
    end if
  end if

  if(mpiid .EQ. 1) then
    write (*,*) 'OpenMPI configuration:                 ',mpinum,' process(es)'
  end if
      

  ! creates source
  if(use_src) then
    call date_and_time(values=s_prof_time)
    call PoolSourceLoad(src,"start.00")
    src%istar1 = src%istar1 + mpiid

    allocate( ray18(18,src%nPoint) )
    call sourceGeom(src,ray18,src%nPoint)        

    ! write begin.dat if argument is -s
    if(.not.use_trc) then
      write(mpinames,'(I2)') mpiid+1
      mpinames = 'begin.dat-'//trim(adjustl(mpinames))
      call beamWrite(ray18, iErr, src%nCol, src%nPoint, mpinames)
    endif

    nCol1 = src%nCol
    nPoint1 = src%nPoint

    call date_and_time(values=e_prof_time)
    start_time =s_prof_time(5) * 3600 + s_prof_time(6) * 60 &
            + s_prof_time(7) + 0.001 * s_prof_time(8)
    finish_time=e_prof_time(5) * 3600 + e_prof_time(6) * 60 &
            + e_prof_time(7) + 0.001 * e_prof_time(8)
    write(*,*)'Elapsed time on use_src:',finish_time-start_time,' myid:',mpiid
  end if
  


  ! reads begin.dat if arguments is -t
  if(use_trc.and.(.not.use_src)) then

    call date_and_time(values=s_prof_time)
    write(mpinames,'(I2)') mpiid+1
    mpinames = 'begin.dat-'//trim(adjustl(mpinames))

    call beamGetDim (mpinames,nCol1,nPoint1,iFlag,iErr)
    allocate( ray18(18,nPoint1) )

    ray18=0.0d0
    call beamLoad(ray18, iErr, nCol1, nPoint1, mpinames)

    nCol = nCol1
    nPoint = nPoint1

    call date_and_time(values=e_prof_time)
    start_time =s_prof_time(5) * 3600 + s_prof_time(6) * 60 &
             + s_prof_time(7) + 0.001 * s_prof_time(8)
    finish_time=e_prof_time(5) * 3600 + e_prof_time(6) * 60 &
             + e_prof_time(7) + 0.001 * e_prof_time(8)
    write(*,*)'Elapsed time on use_trc + !use_src:',finish_time-start_time,' myid:',mpiid
  end if


!
! reads systemfile.dat
!
  if(use_trc) then
    call date_and_time(values=s_prof_time)
    open (unit=lun, file='systemfile.dat', status='old', iostat=eof)
    if (eof /= 0 ) then
      print *,"trace3mpi: File not found: systemfile.dat "
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

      write(*,*) mpiid,' Load pool'
      call PoolOELoad(oei,infile(iCount)) 
      !call PoolOEWrite(oei,"pippo.00")
      write(*,*) mpiid,' Initiate TraceOE'
      oei%fwrite=3
      call TraceOE(oei,ray18,nPoint1,iCount)


      ! write star.xx
      write(mpinames,'(I2)') mpiid+1
      mpinames = 'star.01-' // trim(adjustl(mpinames))
      call beamWrite(ray18, iErr, 18, nPoint1, mpinames)

      ! write end.xx

      call fName(ffile, 'end', iCount, izero)
      write(mpinames,'(I2)') mpiid
      ffile = trim(adjustl(ffile)) // "-" // trim(adjustl(mpinames))
      write(*,*) mpiid,' Store Pool'
      call PoolOEWrite(oei,ffile)

    end do
    call date_and_time(values=e_prof_time)
    start_time =s_prof_time(5) * 3600 + s_prof_time(6) * 60 &
             + s_prof_time(7) + 0.001 * s_prof_time(8)
    finish_time=e_prof_time(5) * 3600 + e_prof_time(6) * 60 &
             + e_prof_time(7) + 0.001 * e_prof_time(8)
     write(*,*)'Elapsed time on use_trc:',finish_time-start_time,' myid:',mpiid
   end if

   call MPI_BARRIER(MPI_COMM_WORLD,mpierr)
   call MPI_FINALIZE(mpierr)

end Program trace3mpi


