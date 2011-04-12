PROGRAM example02

! similar to  example01 (i.e., reads source and oe 1 pars from
! start files)  but runs into a loop and accumulates the histogram 
use shadow_globaldefinitions
use shadow_beamio
use shadow_variables
use shadow_kernel
use shadow_synchrotron
use shadow_postprocessors

implicit none
type (poolSource)                          :: src
type (poolOE)                              :: oe1
real(kind=skr), allocatable, dimension(:,:):: ray
integer(kind=ski)                          :: ierr,i,j,nRuns,nRays
! parameters for histo1
real(kind=skr), allocatable, dimension(:)  :: xArray,yArray
real(kind=skr)                             :: center,width
integer(kind=ski)                          :: nBins, col
integer(kind=ski)                          :: ilost,inorm,irefl,iener

! inputs
nRuns=5
nRays=10000

! inputs histo1
nBins=11
center = 0.0D0
width  = 0.0D0
col=1   ! col to analyze
iener=0 ! if col=11, units: 0=A, 1=eV, 2=cm-1
ilost=0 ! lost ray flag 0=Good, 1=All, 2=LostOnly
inorm=0 ! normalize to: 0=None, 1=MaxHisto, 2=Area
irefl=0 ! if 1, weigth rays with reflectivity A**2

! allocate and initialize histo1 output
ALLOCATE( xArray(nBins) )
ALLOCATE( yArray(nBins) )
xArray=0.0D0
yArray=0.0D0

! load variables from start.00 and start.01
CALL PoolSourceLoad(src,"start.00")
CALL PoolOELoad(oe1,"start.01")

src%npoint=nRays ! redefine the number of rays to be used
oe1%fwrite=3     ! avoid to write binary files mirr.01 star.01 and info 
                 ! files effic.01 optax.01

! allocate and initialize ray
ALLOCATE( ray(18,src%npoint) )
ray = 0.0D0

! SHADOW loop
DO i=1,nRuns
  
  print *,'>>> Running iteration ',i,' out of ',nRuns
  
  ! calculate source
  src%istar1= src%istar1+213421 ! change the source seed
  print *,'  using seed: ',src%istar1
  CALL SourceSync(src,ray,src%npoint)
  ! traces OE1
  call TraceOE(oe1,ray,src%npoint,1)
  
  !compute histogram
  !
  ! note that center and width are set to zero for i=1, so they 
  ! are computed and stored for i>1
  ! Also, yarray is zero for i=1, but for i>1 it accumulates the counts. 


  ! full call 
  ! call histo1_calc(ray,src%npoint,18, &
  !                 xarray,yarray,nBins,&
  !                 col,iener,center,width,&
  !                 ilost,inorm,irefl)
  ! easy call
  call histo1_calc_easy( & 
                   ! arguments (mandatory)
                   ray,col,nBins,xarray,yarray, & 
                   ! keyword optional arguments 
                   center=center,width=width,   &
                   iener=iener,ilost=ilost,inorm=inorm,irefl=irefl)
END DO
  
! show results
print *,'Number of runs: ',nRuns
print *,'Number of rays per run: ',src%npoint
print *,'Resulting histogram:'
do j=1,nBins
  print *,' ',j,xarray(j),yarray(j)
end do

! clean and end
DEALLOCATE(ray)
DEALLOCATE(xArray,yArray)
!DEALLOCATE(yArray)
STOP
END PROGRAM example02
