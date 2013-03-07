PROGRAM example01

    use shadow_globaldefinitions
    use shadow_beamio
    use shadow_variables
    use shadow_kernel
    use shadow_synchrotron

    implicit none
    type (poolSource)   :: src
    type (poolOE)       :: oe1
    real(kind=skr), allocatable, dimension(:,:) :: ray
    integer(kind=ski)   :: ierr,itmp

    ! load variables from start.00
    CALL PoolSourceLoad(src,"start.00")
    print *,'Number of rays: ',src%npoint
    src%npoint=100000
    print *,'Number of rays (modified): ',src%npoint

    ! allocate ray
    itmp=18
    ALLOCATE( ray(itmp,src%npoint) )

    ! calculate source
    CALL SourceSync(src,ray,src%npoint)
    call beamWrite(ray,ierr,itmp,src%npoint,"begin.dat")
    ! reads start.01 into oe1
    call PoolOELoad(oe1,"start.01")
    ! traces OE1
    itmp=1
    call TraceOE(oe1,ray,src%npoint,itmp)

    ! write file star.01
    itmp=18
    CALL beamWrite(ray,ierr,itmp,src%npoint,"star.01")
    DEALLOCATE(ray)

END PROGRAM example01
