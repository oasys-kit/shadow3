!----
!----
!---- MODULE:  shadow_beamio
!----
!---- i/o a file with a shadow beam (begin.dat, star.xx, mirr.xx, screen.xx)
!----
!---- Module grouping the adapted f77 routines and new ones: 
!----           beamGetDim (new), rbeam, beamLoad (old rbeam18),
!----           write_off, beamWrite  (old write_off18)
!----
!----     srio@esrf.eu 2009-09-18
!----
!----
!---- Example of usage: see test_beamio.f95
!----
!----

module shadow_beamio

    use, intrinsic :: ISO_C_BINDING
    use shadow_globaldefinitions, only : ski, skr, skc

    implicit none

!---- Everything is private unless explicitly made public ----!
    private

    public :: beamGetDim, rbeam, beamLoad, write_off, beamWrite 


contains

!!	beamGetDim read the first three integers of the input binary file.
!!      Analyze the binary file where beam's rays are written.
!!	Rays are written in form of a array of 2two dimensions.
!!	The file format is the next:
!!      Three integers giving the information about the shape of the array.
!!      the rays'array itself.

    subroutine beamGetDim (fname, ncol, npoint, iFlag, iErr) 

        character(len=*), intent(in)      :: fname   !< Input file name.
        ! Number of colomn of the matrix, each rays can have 12, 13, 18 colomns.
        integer(kind=ski), intent(out)    :: ncol
        !< Number of rays.
        integer(kind=ski), intent(out)    :: npoint
        !< don't know please srio@esrf help !!!
        integer(kind=ski), intent(out)    :: iFlag
        !< Error flag.
        integer(kind=ski), intent(out)    :: iErr
        !  Internal variable: unit number.
        integer(kind=ski)                 :: lun
    
        ierr = 0
        ncol = 0
        npoint = 0
        iflag = 0
    
        lun = 21
    
        open(unit=lun, file=fname, status="old", action="read", form="unformatted", iostat=iErr)
        if (iErr /= 0 ) then
            print *,"beamGetDim: Error opening file: : "//trim(fName)
           iErr = 1
           close (unit=lun)
           return
        end if
    
        read(unit=lun, iostat=iErr) NCOL, NPOINT, IFLAG
        if (iErr /= 0) then
            print *, "beamGetDim: Error reading header in file: "//trim(fname)
            iErr = 2
            close (unit=lun)
            return
        end if
    
        close (unit=lun)
        return
    end subroutine beamGetDim



    !!+++
    !! SUBROUTINE RBEAM (FNAME,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IERR)
    !!
    !! purpose
    !!          to the calling program. This is an intermediate
    !!          subroutine that will be easily modified to suit
    !!          the format of SHADOW.
    !!
    !! Input : FNAME (character*(*)) Input file name
    !! 
    !! Output  : RAY  Array which contains the basic 12 columns that
    !!                defined each ray
    !!                It must be allocated as input
    !!          PHASE Array which contains the OPD and the 2 phase 
    !!                angles of the A vector
    !!                It must be allocated as input
    !!          AP    Array which contains the Ap vector for each ray
    !!                            It must be allocated as input
    !! IERR = 0, normal return
    !!      = 1, error opening file
    !!      = 2, error reading file
    !!      = 3, array dimensions do not agree with file dimensions
    !!      = 4, wrong number of columns
    !!
    !!---

!> reads in a BEAM file from SHADOW and returns it. OLD version imported, DEPRECATED.
!! the file format is the next:
!! Three integers giving the information about the shape of the matrix.
!! the rays'matrix itself.
        subroutine rbeam (fname, ray, phase, ap, iErr)
        character(len=*),            intent(in) :: fname !< Input file name
        real(kind=skr),dimension(:,:), intent(in out) :: ray !< Array containing the basic 12 columns which define each ray
        real(kind=skr),dimension(:,:), intent(in out) :: phase !< Array containing the OPD and the 2 phase angles of the A vector
        real(kind=skr),dimension(:,:), intent(in out) :: ap !< Array which contains the Ap vector for each ray
        integer(kind=ski),             intent(out) :: iErr !< Error Flag.

        integer(kind=ski)     ::    lun, i, j, k, l
        integer(kind=ski)     ::    ncol, npoint
        integer(kind=ski)     ::    ncol1, npoint1, iFlag1, iErr1


        ncol = size(ray,1)
        npoint = size(ray,2)

        lun = 21

        open(unit=lun, file=fname, status="old", action="read", form="unformatted", iostat=iErr)

        if (iErr /= 0 ) then
            print *,"RBEAM Error opening file: "//trim(fName)
            iErr=1
            return
        end if

        read(unit=lun, iostat=iErr) ncol1, npoint1, iflag1

        if (iErr /= 0) then
            print *, "RBEAM Error reading file: "//trim(fname)
            close (unit=lun)
            iErr = 2
            return
        end if

        if (iFlag1 /= 0) then
            print *, "RBEAM Error (Flag /=0) in file: "//trim(fname)
            close (unit=lun)
            iErr = 2
            return
        end if

        if (ncol /= ncol1) then
            print *, "RBEAM Error with number of columns"
            print *, "      in input variables: ", ncol
            print *, "      in file: ", ncol1
            close (unit=lun)
            iErr = 3
            return
        end if

        if (npoint /= npoint1) then
            print *, "RBEAM Error with number of rays"
            print *, "   in input variables: ", npoint
            print *, "   in file: ", npoint1
            close (unit=lun)
            iErr = 3
            return
        end if

        select case (ncol)
            case (12)
                do i = 1, npoint
                    read (unit=lun, iostat=iErr) (ray(j,i), j=1, 12)
                    if (iErr /= 0) then
                        print *, "RBEAM Error reading file: "//trim(fname)
                        close (unit=lun)
                        iErr = 2
                        return
                    end if
                end do
           
            case (13)
                do i=1,NPOINT
                    read (unit=lun, iostat=iErr) (ray(j,i), j=1,12), phase(1,i)
                
                    if (iErr /= 0) then
                        iErr = 2
                        print *, "RBEAM Error reading file: "//trim(fname)
                        exit
                    end if
                end do
                
            case (18)
                do i = 1, npoint
                    read (unit=lun, iostat=iErr) (ray(j,i), j=1,12), (phase(k,i), k=1,3), (ap(l,i), l=1,3)
                    if (iErr /= 0) then
                        iErr = 2
                        print *, "RBEAM Error reading file: "//trim(fname)
                        exit
                    end if
                end do
            case default
                iErr = 4
                print *, "RBEAM Invalid number of columns: ", ncol
        end select

        close(unit=lun)
        return
    end subroutine rbeam

    !
    !
    !


    !!+++
    !!  SUBROUTINE      beamLoad      (FNAME,RAY,IERR)
    !!
    !! purpose          The same as RBEAM, except return everything in
    !!                  one single array RAY(18,N_DIM) instead of three
    !!                  separate arrays.
    !!
    !! Input     :      FNAME  Input file name
    !!
    !! Output  :	RAY	Array which contains the 18 columns that
    !!				defined each ray
    !!                            It must be dimensionated as input.
    !!			NCOL	Number of columns for each ray 
    !!			NPOINT	Number of rays
    !!			IFLAG	A flag which is stored on the input file
    !!			IERR = 0, normal return
    !!			     = 1, error opening file
    !!			     = 2, error reading file
    !!  		     = 3, array dimensions do not agree with 
    !!                            file dimensions
    !! 			     = 4, wrong number of columns
    !!
    !!
    !!---

!>	The same as RBEAM, but returns everything in one single array RAY(18,ncol) instead of three separate arrays.
!!	NEW version, ENCOURAGED.
        subroutine beamLoad (ray,iErr,ncol1,npoint,FNAME) !bind(C,NAME="beamLoad")

        integer(kind=ski),intent(in)    :: npoint   !< number of rays
        character(len=*),intent(in)     :: fname    !< Input file name.

        real(kind=skr), dimension(18,npoint), intent(inout)   :: ray !< Array containing all ray datas.

        integer(kind=ski),intent(out)    :: ncol1    !< 1st dimension of ray.
        integer(kind=ski),intent(out)   :: iErr     !< Error flag.

        integer(kind=ski)     ::    lun, i, j, k, l
        integer(kind=ski)     ::    npoint1, iflag1, ierr1

        lun = 21
        open(unit=lun,file=fname,status="old",action="read",form="unformatted", iostat=iErr)

        if (iErr /= 0 ) then
           print *,"beamLoad Error opening file: "//trim(fName)
           iErr=1
           !stop 'Program failure in beamLoad'
           return
        end if


        read(unit=lun,iostat=iErr) ncol1, npoint1, iflag1

        if (iErr /= 0) then
            print *, "beamLoad Error reading header in file: "//trim(fname)
            close (unit=lun)
            !iErr = 2
            !stop 'Program failure in beamLoad'
            print *, 'Program failure in beamLoad'
            return
        end if

        if (iFlag1 /= 0) then
            print *, "beamLoad Error (Flag /=0) in file: "//trim(fname)
            close (unit=lun)
            iErr = 2
            !stop 'Program failure in beamLoad'
            print *, 'Program failure in beamLoad'
            return
        end if

        if (npoint /= npoint1) then
            print *, "beamLoad Error with number of rays"
            print *, "   in input variables: ", npoint
            print *, "   in file: ", npoint1
           close (unit=lun)
            !stop 'Program failure in beamLoad'
            print *, 'Program failure in beamLoad'
            !iErr = 3
            return
        end if

        if (ncol1.ne.12.and.ncol1.ne.13.and.ncol1.ne.18) then
            print *, "beamLoad Error with number of columns in file: ", ncol1
            close (unit=lun)
            !stop 'Program failure in beamLoad'
            print *, 'Program failure in beamLoad'
            !iErr = 4
            return
        end if


        do I=1, npoint
            read (unit=lun, iostat=iErr) (RAY(j,i), j=1, ncol1)
            if (iErr /= 0) then
                !iErr = 2
                print *, "beamLoad Error reading ray file: "//trim(fname)
                print *, "        ray number, iErr: ",i,iErr
                !stop 'Program failure in beamLoad'
                print *, 'Program failure in beamLoad'
                !exit
                return
            end if
        end do

        close (unit=lun)
        return
    end subroutine beamLoad

    !
    !
    !

!!+++
!!	SUBROUTINE	WRITE_OFF (FNAME,RAY,PHASE,AP,NCOL,NPOINT,IFLAG,IERR)
!!
!!	Input	:	FNAME	(character*(*))	Output file name
!!			RAY	Array which contains the basic 12 columns that
!!				defined each ray
!!			PHASE	Array which contains the OPD and the 2 phase 
!!				angles of the A vector
!!			AP	Array which contains the Ap vector for each ray
!!			NCOL	Number of columns for each ray 
!!				(valid NCOL = 12, 13, or 18)
!!			NPOINT	Number of rays
!!			IFLAG	A flag which is written to the output file.
!!			IFORM = 0, Binary output
!!			      = 1, Formatted output
!!	
!!	Output	:	IERR = 0, normal return
!!			     = 1, error opening file
!!			     = 2, error writing file
!!
!!---
subroutine write_off (fname, ray, phase, ap, ncol, npoint, iFlag, iForm, iErr)

implicit none
character(len=*),              intent(in)       :: fname
real(kind=skr), dimension(:,:),intent(in)       :: ray
real(kind=skr), dimension(:,:),intent(in)       :: phase
real(kind=skr), dimension(:,:),intent(in)       :: ap
integer(kind=ski),             intent(inout)    :: ncol
integer(kind=ski),             intent(inout)    :: npoint
integer(kind=ski),             intent(in)       :: iFlag, iForm
integer(kind=ski),             intent(out)      :: iErr

integer(kind=ski), parameter  :: iounit=20
character(len=80)             :: fformat
integer(kind=ski)             :: i, j, k, l
integer(kind=ski)             :: write_only_good_rays,npointOut

! todo: generalize this local flag
write_only_good_rays = 0
!!
!! Check for valid argument of NCOL.
!!
if (ncol .ne. 12 .and. ncol .ne. 13 .and. ncol .ne. 18) then
    print *, "BEAMIO-WRITE_OFF Warning: Invalid argument of NCOL: ", ncol
    ncol = size(ray,1)
    print *, "BEAMIO-WRITE_OFF Using NCOL=Size(ray,1)=", ncol
    print *, "BEAMIO-WRITE_OFF FileName: "//trim(fname)
    print *, " "
end if

!!
!! Check for valid argument of NPOINT.
!!
if (npoint .le. 0) then
    print *, "BEAMIO-WRITE_OFF Warning: Invalid argument of NPOINT: ", npoint
    npoint = size(ray,2)
    print *, "BEAMIO-WRITE_OFF Using NPOINT=Size(ray,2)=", npoint
    print *, "BEAMIO-WRITE_OFF FileName: "//trim(fname)
    print *, " "
end if

!!
!! Decide if output is formatted or unformatted.
!!

if (iForm .eq. 0) then
    fformat = 'UNFORMATTED'
else if (iForm .eq. 1) then
    fformat = 'FORMATTED'
else
    print *, "BEAMIO-WRITE_OFF Error Invalid argument of IO Format: ", iForm
    !stop
    return
end if

!!
!! Now do the dirty deed. Also, Remove the INITIALSIZE hint used 
!! for the VMS version.
!!

open (unit=iounit, file=fname, status='unknown', form=fformat, iostat=iErr)

if (iErr /= 0 ) then
    print *,"BEAMIO-WRITE_OFF Error opening file: "//trim(fName)
    iErr=1
    return
end if

REWIND (iounit)

npointOut = npoint
if (write_only_good_rays .gt. 0) then
   npointOut = 0
   do i=1, npoint
      if (ray(10,i) .gt. 0) npointOut = npointOut + 1
   end do

end if

if (iForm .EQ. 0) then
    write (iounit, err=20) ncol, npointOut, iFlag
else
    write (iounit, *, err=20) ncol, npointOut, iFlag
end if

if (write_only_good_rays .eq. 0) then
    if (ncol .eq. 12) then
        if (IFORM .EQ. 0) then
            do i=1, npoint
                write (iounit, err=20) (ray(j,i), j=1,12)
            end do
        else
            do i=1, npoint
                write (iounit, *, err=20) (ray(j,i), j=1,12)
            end do
        end if
    else if (ncol .eq. 13) then
        if (iForm .eq. 0) then
            do i=1, npoint
                write (iounit, err=20) (ray(j,i), j=1,12), phase(1,i)
            end do
        else
            do i=1, npoint
                write (iounit, *, err=20) (ray(j,i), j=1,12), phase(1,I)
            end do
        end if
    else if (ncol .eq. 18) then
        if (iForm .eq. 0) then
            do i=1, npoint
                write (20, err=20) (ray(j,i), j=1,12), (phase(k,i), K=1,3), (ap(l,i), l=1,3)
            end do
        else
            do i=1, npoint
                write (20, *, err=20) (ray(j,i), j=1,12), (phase(k,i), k=1,3), (ap(l,i), l=1,3)
            end do
        end if
    end if
else ! write only good rays
    if (ncol .eq. 12) then
        if (IFORM .EQ. 0) then
            do i=1, npoint
                write (iounit, err=20) (ray(j,i), j=1,12)
            end do
        else
            do i=1, npoint
                write (iounit, *, err=20) (ray(j,i), j=1,12)
            end do
        end if
    else if (ncol .eq. 13) then
        if (iForm .eq. 0) then
            do i=1, npoint
                if (ray(10,i) .gt. 0) write (iounit, err=20) (ray(j,i), j=1,12), phase(1,i)
            end do
        else
            do i=1, npoint
                if (ray(10,i) .gt. 0) write (iounit, *, err=20) (ray(j,i), j=1,12), phase(1,I)
            end do
        end if
    else if (ncol .eq. 18) then
        if (iForm .eq. 0) then
            do i=1, npoint
                if (ray(10,i) .gt. 0) write (20, err=20) (ray(j,i), j=1,12), (phase(k,i), K=1,3), (ap(l,i), l=1,3)
            end do
        else
            do i=1, npoint
                if (ray(10,i) .gt. 0) write (20, *, err=20) (ray(j,i), j=1,12), (phase(k,i), k=1,3), (ap(l,i), l=1,3)
            end do
        end if
    end if
end if

!!
!! Close file.
!!
close (unit=iounit)

!! successful end
iErr = 0
return
!! unsuccessful end
20    iErr    =  2
return
end subroutine write_off

!
!
!

!! 
!! 	SUBROUTINE	beamWrite (FNAME,RAY,IERR)
!! 
!! 	Input	:	FNAME	Output file name
!! 			RAY	Array which contains the basic 12,13 or 18 
!!                          columns that defined each ray
!! 	
!! 	Output	:	IERR = 0, normal return
!! 			     = 1, error opening file
!! 			     = 2, error writing file
!! 
!! 

subroutine beamWrite (ray, iErr, ncol, npoint, fname) !bind(C, name="beamWrite")
implicit none
character(len=*), intent(in)    :: fname
integer(kind=ski),intent(in)    :: ncol
integer(kind=ski),intent(in)    :: npoint
real(kind=skr), dimension(18,npoint),    intent(in)    :: ray
integer(kind=ski),intent(out)   :: iErr

integer(kind=ski), parameter    :: iounit=20
integer(kind=ski)               :: i, j, k, l
integer(kind=ski)               :: iform, iflag
character(len=80)               :: fformat


iflag=0


!! just a warning...
if (ncol.ne.12.and.ncol.ne.13.and.ncol.ne.18) then
    print *, "beamWrite Warning: number of columns: ",ncol
end if

iForm=0
if (iForm .eq. 0) then
    fformat = 'UNFORMATTED'
else
    fformat = 'FORMATTED'
end if

open (unit=iounit, file=fname, status='unknown', form=fformat, iostat=iErr)

if (iErr /= 0 ) then
    print *,"beamWrite Error opening file: "//trim(fName)
    iErr=1
    return
end if

if (iForm .eq. 0) then
    write (iounit, err=20) ncol, npoint, iFlag
else
    write (iounit, *, err=20) ncol, npoint, iFlag
end if

if (iForm .eq. 0) then
    do i=1, npoint
        write (iounit, err=20) (ray(j,i), j=1,ncol)
    end do
else
    do i=1, npoint
        write (iounit, *, err=20) (ray(j,i), j=1,ncol)
    end do
end if

close (unit=iounit)

!! succesful end
iErr = 0
return
!! unsuccesful end
20      iErr = 2
return
end subroutine beamWrite
!
!
!

end module shadow_beamio

