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
type (poolSource)                          :: src,srcTmp
type (poolOE)                              :: oe1,oe1Tmp
real(kind=skr), allocatable, dimension(:,:):: ray
integer(kind=ski)                          :: ierr,i,j,nMaxRuns,nRays

! parameters for histo1
real(kind=skr), allocatable, dimension(:)  :: xArray,yArray,y2Array
real(kind=skr)                             :: center,width
integer(kind=ski)                          :: nBins, col
integer(kind=ski)                          :: ilost,inorm,irefl,iener
real(kind=skr)                             :: invn,sigma,pc,itot
!character(len=80) :: file
!character(len=1) :: ci

! inputs
nMaxRuns=200 ! max number of runs
nRays=1000

! inputs histo1
nBins=11
center = 0.0D0
width  = 0.0
col=1   ! col to analyze
iener=0 ! if col=11, units: 0=A, 1=eV, 2=cm-1
ilost=0 ! lost ray flag 0=Good, 1=All, 2=LostOnly
inorm=0 ! normalize to: 0=None, 1=MaxHisto, 2=Area
irefl=1 ! if 1, weigth rays with reflectivity A**2

! allocate and initialize histo1 output
ALLOCATE( xArray(nBins),yArray(nBins),y2Array(nBins) )
xArray=0.0D0
yArray=0.0D0
y2Array=0.0D0

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
DO i=1,nMaxRuns
  
  print *,'>>> Running iteration ',i,' out of ',nMaxRuns
  
  ! calculate source
  ! src%istar1= src%istar1+2134213453845 ! change the source seed
  !src%istar1= 5 ! change the source seed
  !print *,'  using seed: ',src%istar1
  srcTmp = src ! copy type for forther use (SourceSyc modifies it!)
  CALL SourceSync(srcTmp,ray,src%npoint)
  !i1=0.0D0
  !i2=0.0D0
  !call intens_calc(ray,src%npoint,18,0,i1,i2)
  !print *,'>>>>>> INTENSITY SOURCE: ',i1
  ! traces OE1
  oe1Tmp = oe1
  call TraceOE(oe1Tmp,ray,src%npoint,1)
  !i1=0.0D0
  !i2=0.0D0
  !call intens_calc(ray,src%npoint,18,0,i1,i2)
  !print *,'>>>>>> INTENSITY: ',i1
  !write(ci,'(I1)') i
  !file="star.01-"//trim(adjustl(ci))
  !print *,'>>>>>>>>>>>>>>>>>>>'//trim(file)//'<<<<<<<<<<<<<<<'
  !CALL beamWrite(ray,ierr,18,src%npoint,file )
  
  !compute histogram
  !
  ! note that center and width are set to zero for i=1, so they 
  ! are computed and stored for i>1
  ! Also, yarray and y2Array are zero for i=1, but for i>1 it accumulates the counts. 


  ! full call 
  ! call histo1_calc(ray,src%npoint,18, &
  !                 xarray,yarray,y2Array,nBins,&
  !                 col,iener,center,width,&
  !                 ilost,inorm,irefl)
  ! easy call
   call histo1_calc_easy( & 
                    ! arguments (mandatory)
                    ray,col,nBins,xarray,yarray,y2Array, & 
                    ! keyword optional arguments 
                    center=center,width=width,   &
                    iener=iener,ilost=ilost,inorm=inorm,irefl=irefl)
  !print *,'AFTER center,width,counts, squared-counts: ',center,width,yarray(nBins/2),y2Array(nBins/2)

  ! exit if error at central bin is less that 2%
  invn = 1.0d0/(src%npoint*i)
  sigma = sqrt(y2Array(nBins/2) - (yarray(nBins/2)**2)*invn )
  pc = 100*sigma/yarray(nBins/2)
  print *,'run, sigma, %error: ',i,sigma,pc
  write(28,*) i,sigma,pc
  IF (pc .LE. 2) EXIT
END DO

! show results
print *,'Maximum number of runs: ',nMaxRuns
print *,'Number of rays per run: ',src%npoint
print *,'Total rays: ',1.0d0/invn
print *,'Resulting histogram:'
print *,' j   xarray    yarray   sigma   100*sigma/yarray '
itot=0.0d0
do j=1,nBins
  sigma = sqrt(y2Array(j) - (yarray(j)**2)*invn )
  !sigma = sqrt(max(sigma,0.0d0))
  pc = 100*sigma/yarray(j)
  IF (abs(sigma) .LE. 1d-13) pc = 0
  print *,' ',j,xarray(j),yarray(j),sigma,100*sigma/yarray(i)
  write(27,*) xarray(j),yarray(j),sigma
  itot=itot+yarray(j)
end do

print *,'Total counts: ',itot
! clean and end
DEALLOCATE(ray,xArray,yArray,y2Array)
STOP
END PROGRAM example02
