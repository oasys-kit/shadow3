












































module shadow_bind_f
!
!
! This module define the Fortran Routines that will be seen by C and other
! bindings, plus other C<->Fortran compatibility tools.
!
! 
!
! Author: N. Canestrari
!
!
    use ISO_C_BINDING
    use shadow_globaldefinitions
    use shadow_beamio
    use shadow_variables
    use shadow_kernel
    use shadow_synchrotron
    use shadow_postprocessors

    implicit none

    type, public, bind(C) :: pixel
        integer(kind=C_INT)   :: np
        real(kind=C_DOUBLE)   :: dn
        real(kind=C_DOUBLE)   :: up
    end type pixel

    private :: CstringToFstring

    public  :: BindShadowPoolSourceLoad, BindShadowPoolSourceWrite
    public  :: BindShadowPoolOELoad, BindShadowPoolOEWrite
    public  :: BindShadowSourceGeom, BindShadowSourceSync, BindShadowTraceOE
    public  :: BindShadowBeamWrite, BindShadowBeamgetDim, BindShadowBeamLoad
    public  :: BindShadowFFresnel2d

contains

    subroutine CstringToFstring(cstr,fstr,length)
        integer(kind=C_INT), value, intent(in)           :: length
        character(kind=C_CHAR), intent(in)               :: cstr(length)
        character(kind=C_CHAR, len=length), intent(out)  :: fstr

        character(kind=C_CHAR,len=9)                     :: fmt

        write(fmt,'(A,I6,2A)') '(', length, 'A)'
        write(fstr,fmt) cstr
    end subroutine CstringToFstring


    subroutine BindShadowPoolSourceLoad(src, file, length) bind (C,name="BindShadowPoolSourceLoad")
        type(poolSource), intent(out)                    :: src
        character(kind=C_CHAR), intent(in)               :: file(*)
        integer(kind=C_INT), value, intent(in)           :: length

        character(kind=C_CHAR, len=length)               :: fname

        call CstringToFstring(file,fname, length)        
        call PoolSourceLoad(src,fname)
    end subroutine BindShadowPoolSourceLoad


	subroutine BindShadowPoolSourceWrite(src, file, length) bind (C,name="BindShadowPoolSourceWrite")
        type (poolSource), intent(inout)                 :: src
        character(kind=C_CHAR), intent(in)               :: file(*)
        integer(kind=C_INT), value, intent(in)           :: length

        character(kind=C_CHAR, len=length)               :: fname

        call CstringToFstring(file,fname, length)
        call PoolSourceWrite(src,fname)
	end subroutine BindShadowPoolSourceWrite


	subroutine BindShadowPoolOELoad(oe, file, length) bind (C,name="BindShadowPoolOELoad")
        type (poolOE), intent(out)                       :: oe
        character(kind=C_CHAR), intent(in)               :: file(*)
        integer(kind=C_INT), value, intent(in)           :: length

        character(kind=C_CHAR, len=length)                     :: fname

        call CstringToFstring(file,fname, length)
        call PoolOELoad(oe,fname)
	end subroutine BindShadowPoolOELoad


	subroutine BindShadowPoolOEWrite(oe, file, length) bind (C,name="BindShadowPoolOEWrite")
        type (poolOE), intent(inout)                        :: oe
        character(kind=C_CHAR), intent(in)                     :: file(*)
        integer(kind=C_INT), value, intent(in)                :: length

        character(kind=C_CHAR, len=length)                     :: fname

        call CstringToFstring(file,fname, length)
        call PoolOEWrite(oe,fname)
	end subroutine BindShadowPoolOEWrite


	subroutine BindShadowSourceGeom(src, ray, nPoint) bind (C,name="BindShadowSourceGeom")
        type (poolSource), intent(inout)                    :: src
        integer(kind=C_INT), intent(in)                       :: nPoint
        real(kind=C_DOUBLE), dimension(18,nPoint), intent(out)   :: ray

        call sourceGeom(src, ray, nPoint)
	end subroutine BindShadowSourceGeom


    subroutine BindShadowSourceSync(src, ray, nPoint) bind (C,name="BindShadowSourceSync")
        type (poolSource), intent(inout)                    :: src
        integer(kind=C_INT), intent(in)                       :: nPoint
        real(kind=C_DOUBLE), dimension(18,nPoint), intent(out)   :: ray

        call SourceSync(src, ray, nPoint)
    end subroutine BindShadowSourceSync


	subroutine BindShadowTraceOE(oe, ray, nPoint, iCount) bind (C,name="BindShadowTraceOE")
        type (poolOE), intent(inout)                        :: oe
        integer(kind=C_INT), intent(in)                       :: nPoint, iCount
        real(kind=C_DOUBLE), dimension(18,nPoint), intent(inout) :: ray

        call TraceOE(oe, ray, npoint, iCount)
	end subroutine BindShadowTraceOE


	subroutine BindShadowBeamWrite(ray, nPoint, nCol, file, length) bind (C,name="BindShadowBeamWrite")
        integer(kind=C_INT), intent(in)                       :: nPoint, nCol
        real(kind=C_DOUBLE), dimension(18,nPoint), intent(inout) :: ray
        character(kind=C_CHAR), intent(in)                     :: file(*)
        integer(kind=C_INT), value, intent(in)                :: length

        character(kind=C_CHAR, len=length)                     :: fname
        integer(kind=C_INT)                                   :: i, iErr

        call CstringToFstring(file,fname, length)
        call beamWrite(ray, iErr, nCol, nPoint, fname)
	end subroutine BindShadowBeamWrite


	subroutine BindShadowBeamgetDim(file, length, nPoint, nCol) bind (C,name="BindShadowBeamgetDim")
        character(kind=C_CHAR), intent(in)                     :: file(*)
        integer(kind=C_INT), intent(out)                      :: nPoint, nCol
        integer(kind=C_INT), value, intent(in)                :: length

        character(kind=C_CHAR, len=length)                     :: fname
        integer(kind=C_INT)                                   :: i, iErr, iFlag

        call CstringToFstring(file,fname, length)
        call beamGetDim(fname, nCol, nPoint, iFlag, iErr)
	end subroutine BindShadowBeamgetDim


	subroutine BindShadowBeamLoad(ray, nPoint1, nCol1, file, length) bind (C,name="BindShadowBeamLoad")
        character(kind=C_CHAR), intent(in)                     :: file(*)
        integer(kind=C_INT), intent(inout)                    :: nPoint1, nCol1
!	type(C_PTR), intent(in)                                :: ray
        real(kind=C_DOUBLE), dimension(18,nPoint1), intent(inout)   :: ray
        integer(kind=C_INT), value, intent(in)                :: length

	!real(kind=8), pointer ::  f_ray(:,:)

        character(kind=C_CHAR, len=length)                     :: fname
        integer(kind=C_INT)                                   :: i, iErr

	!call c_f_pointer(ray,f_ray,[18,npoint1])

        call CstringToFstring(file,fname, length)
        call beamLoad(ray, iErr, nCol1, nPoint1, fname)
	end subroutine BindShadowBeamLoad


	subroutine BindShadowFFresnel2D(ray, nPoint, dist, EField, px, pz) bind (C,name="BindShadowFFresnel2D")
        real(kind=C_DOUBLE), dimension(18,nPoint), intent(in)    :: ray
        integer(kind=C_INT), intent(in)                       :: nPoint
        real(kind=C_DOUBLE), intent(in)                          :: dist
        type(pixel), intent(in)                             :: px, pz
        complex(kind=C_DOUBLE_COMPLEX), dimension(3,px%np,pz%np),intent(inout) :: EField

        call FFresnel2D(ray,nPoint,dist,EField,px%np,px%up,px%dn,pz%np,pz%up,pz%dn)
	end subroutine BindShadowFFresnel2D

end module shadow_bind_f
