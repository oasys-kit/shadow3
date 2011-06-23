PROGRAM  Fig3

use shadow_globaldefinitions    
!use stringio
use shadow_beamio
use shadow_variables
use shadow_kernel
use shadow_synchrotron

implicit none
  
type (poolSource)  :: pool00
type (poolOE)      :: pool0i
real(kind=skr), allocatable, dimension(:,:) :: ray
integer(kind=ski) :: ierr
character(len=sklen) :: file

! load variables from start.00
file="start.00"
CALL PoolSourceLoad(pool00,file) 
print *,'Number of rays: ',pool00%npoint
pool00%npoint=10000
print *,'Number of rays (modified): ',pool00%npoint

! allocate ray 
ALLOCATE( ray(18,pool00%npoint) )

! calculate source
CALL  SourceSync (pool00,ray,pool00%npoint)

!! ! write file begin.dat
!! file = "begin.dat"
!! CALL beamWrite(ray,ierr,18,pool00%npoint,file)
!! ! write end.00 file
!! CALL GlobalToPoolSource(pool00)
!! file = "end.00"
!! CALL PoolSourceWrite(pool00,file)

! reads start.01 into pool0i
file = "start.01"
call PoolOELoad(pool0i,file)
! traces OE1
call TraceOE(pool0i,ray,pool00%npoint,1)

! write file star.01
file = "star.01"
CALL beamWrite(ray,ierr,18,pool00%npoint,file)
!! ! write end.01
!! file = "end.01"
!! CALL PoolOEWrite(pool0i,file)

END PROGRAM Fig3
