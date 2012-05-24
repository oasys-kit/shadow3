
!
!
! This program is an example of a system that contains one single 
! (spherical) mirror. The ray tracing is performed "by hand", meaning that it 
! makes propagation, reflection, etc in this main code, and it does not
! use the generic SHADOW procedures for that. 
!
! The example corresponds to an spherical mirror, but it can be easily 
! adaptet to any conic via the definition of the ccc coefficients. 
!
! It is intended to be an example for building specific ray-tracers using
! SHADOW library.
!
!
! Author: M. Sanchez del Rio
!
!

PROGRAM example_standalone_mirror

    use shadow_globaldefinitions
    use shadow_beamio
    use shadow_math, only: rotvector
    use shadow_variables
    use shadow_kernel
    use shadow_synchrotron
    use shadow_postprocessors, only: retrace, shrot, shtranslation

        implicit none
        type (poolSource)   :: src
        type (poolOE)       :: oe1
        real(kind=skr), allocatable, dimension(:,:) :: ray
        integer(kind=ski)   :: ierr,i,source_from
        integer(kind=ski)   :: ncol1,npoint1,iFlag
        real(kind=skr)      :: alpha1,p,q,radius,tmp
        real(kind=skr)      :: grazing1,grazing2,theta1,theta2
        real(kind=skr),dimension(3)  :: tmpv,tmp_pos,tmp_vel,n1,n11,ntmp
        real(kind=skr),dimension(3),parameter  :: vx=(/1.0,0.0,0.0/)
        real(kind=skr),dimension(3),parameter  :: vy=(/0.0,1.0,0.0/)
        real(kind=skr),dimension(3),parameter  :: vz=(/0.0,0.0,1.0/)
        !real(kind=skr),dimension(10) :: ccc

        !
        ! inputs 
        p= 1000.00     ! source to crystal
        q= 300.00     ! source to crystal
        alpha1 = 0.0    ! mirror orientation angle
        theta1 = 89.713521 ! incident angle in deg
        grazing1 = (90.0-theta1)*pi/180.0 !grazing angle in rad
        grazing2 = (90.0-theta1)*pi/180.0 !grazing reflection angle in rad
        radius=2D0*p*q/(p+q)/sin(grazing1)

        print *," ------------  Input parameters: "
        print *," p: ",p
        print *," q: ",q
        print *," incidence angle [deg]: ",theta1
        print *," ------------  "
        print *," Radius of curvature of spherical mirror: ",radius

        ! Source: two possibilities: 
        !   source_from=0 calculate from start.00, 
        !   source_from=1 read begin.dat
        source_from = 0

        ! Source i)
        if (source_from .eq. 0) then
        ! load variables from start.00
               CALL PoolSourceLoad(src,"start.00")
               !print *,'Number of rays: ',src%npoint
               !src%npoint=100000
               !print *,'Number of rays (modified): ',src%npoint
        ! allocate ray
               ALLOCATE( ray(18,src%npoint) )
        ! calculate source
               CALL SourceSync(src,ray,src%npoint)
               !call write_off18(ray,ierr,18,src%npoint,"begin.dat")
               npoint1 = src%npoint
               nCol1 = 18
               call beamWrite(ray, iErr, nCol1, nPoint1, "begin.dat")
        else
          !call RBeamAnalyze ('begin.dat',nCol1,nPoint1,iFlag,iErr)
          call beamGetDim ("begin.dat",nCol1,nPoint1,iFlag,iErr)
          allocate( ray(18,nPoint1) )
          ray=0.0d0
          !call RBeam18(ray, iErr, nCol1, nPoint1, 'begin.dat')
          call beamLoad(ray,iErr,nCol1,nPoint1,'begin.dat')
          ! put these variables in global
          ! to do: try to mark the error in ncol
          nCol = nCol1
          nPoint = nPoint1
        endif

        print *,"Source read from file: begin.dat"
        print *," Number of rays: ",nPoint1
        print *," Number of columns: ",nCol1

! trace start.01
!	! reads start.01 into oe1
!	call PoolOELoad(oe1,"start.01")
!	! traces OE1
!	call TraceOE(oe1,ray,npoint1,1)
!	! write file star.01
!	CALL Write_off18(ray,ierr,18,npoint1,"star.01")

      ! puts source in the mirror reference system
      call shrot(ray,nPoint1,alpha1,2)
      call shrot(ray,nPoint1,grazing1,1)
      tmpv = (/0.0D0,-p*cos(grazing1),p*sin(grazing1)/)
      call shtranslation(ray,nPoint1,tmpv)

      ! compute intersection

      ! this part uses shadow_kernel with its own variables 
      ! We need to define ccc, and t_source, two variables used
      ! in intercept and global in shadow_kernel
      ccc=0.0
      ccc(1:3)=1.0
      ccc(9) = -2*radius !concave
      !ccc(9) =  2*radius !convex
      print *,"Using a phere of radius: ",radius
      print *,"Conic coefficients i,ccc(i) : "
      do i=1,10 
          print *,"   ",i,ccc(i)
      end do

      t_source = p ! used by intercept

      do i=1,npoint1
        tmp_pos = ray(1:3,i)
        tmp_vel = ray(4:6,i)
        iFlag = 1
        call intercept(tmp_pos,tmp_vel,tmp,iFlag)
        ! skip the ray if it misses the target.
        if (iFlag.eq.-1) then
          ray(10,i)=-1000 ! set flag to bad
          cycle
        end if
        tmp_pos = tmp_pos + tmp*tmp_vel

        ! calculate the normal
        n1(1)=2*ccc(1)*tmp_pos(1) +ccc(4)*tmp_pos(2) +ccc(6)*tmp_pos(3) +ccc(7)
        n1(2)=2*ccc(2)*tmp_pos(2) +ccc(4)*tmp_pos(1) +ccc(5)*tmp_pos(3) +ccc(8)
        n1(3)=2*ccc(3)*tmp_pos(3) +ccc(5)*tmp_pos(2) +ccc(6)*tmp_pos(1) +ccc(9)
        tmp = sqrt(n1(1)*n1(1) + n1(2)*n1(2) + n1(3)*n1(3))
        n1 = n1 / tmp

        !calculates the reflection direction using specular reflection
        ! v2 = v1 -2 (v1.normal) normal
        ! being . the dot product
        tmpv = tmp_vel*n1             ! 
        tmp = tmpv(1)+tmpv(2)+tmpv(3) !(v1.normal)
        tmpv = -2.0*tmp*n1            !-2 (v1.normal) normal
        tmp_vel = tmp_vel + tmpv
        tmp = sqrt(tmp_vel(1)*tmp_vel(1) + tmp_vel(2)*tmp_vel(2) + tmp_vel(3)*tmp_vel(3))
        tmp_vel = tmp_vel/tmp
        ray(1:3,i) = tmp_pos
        ray(4:6,i) = tmp_vel
      end do
      call beamWrite(ray, iErr, nCol1, nPoint1, "mirrT.01")

      ! calculates the image
      call shrot(ray,nPoint1,grazing2,1)
      call retrace(ray,nPoint1,q,1)
      call beamWrite(ray, iErr, nCol1, nPoint1, "starT.01")
      
      print *,"Succesful run. Output files: mirrT.01 and starT.01"

    DEALLOCATE(ray)

END PROGRAM example_standalone_mirror
