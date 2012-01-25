Module shadow_crl !compound refractive lenses

  Use, intrinsic :: ISO_C_BINDING

  Use shadow_globaldefinitions
  Use gfile
  Use shadow_variables
  Use shadow_kernel

  Implicit None

  Public :: precrl , runcrl
  Private :: TraceCRL, ReadCRL
  
  Contains


!----
!----  SUbroutine precrl
!----
!----  Preprocessor for COmpound Refractive Lenses
!----
!----  create a crl.01 file with parameters for a compound refractive lens
!---- 
!----  srio@esrf.eu 2011-09-27
!---- 
!
Subroutine precrl
    !---- Use Modules ----!
    !use GFile
    !use stringio

    !---- Variables ----!
    implicit none

    Type(GfType)           :: g1
    integer(kind=ski)      :: ns,i,j,iShape,fCyl,changeConvexity,itmp
    real(kind=skr)         :: r0,ref0,ref1,pp,qq,pp0,qq0,ddIn,ddV
    character(len=sklen)   :: fileName
    character(len=3)       :: stmp,stmp2
    Logical                :: arggiven=.false., esta, iOut
    real(kind=skr)         :: cil_ang, sin_cil, cos_cil, ref_in, ref_out
    real(kind=skr),dimension(10)  :: ccc, cccTmp
    real(kind=skr), parameter :: torad = 0.017453292519943295769237
    ! variables for sphere
    real(kind=skr)         :: f, R, delta
    ! variables for ellipsoid/hyperboloid
    real(kind=skr)         :: axmaj, axmin, afoci, ee, eccent, ycen, zcen
    real(kind=skr)         :: a,b,c
    real(kind=skr),dimension(3) :: rncen,rtcen
    ! variables for paraboloid
    real(kind=skr)         :: costhe, sinthe, param, fact
    ! variables for hyperboloid
    real(kind=skr)         :: cc, branch
    
    ccc=0.0D0

    print *,'Lens interface shape: '
    print *,   '1 - Sphere'
    print *,   '2 - Ellipsoid'
    !print *,   'Toroidal-3'
    print *,   '4 - Paraboloid'
    print *,   '5 - Plane'
    !print *,   '6 Codling slit'
    print *,   '7 - Hyperboloid'
    !print *,   'Cone-8??'
    !print *,   'Polynomial-9??
    iShape = irint(' <?> ')

    fCyl = irint('Cylindical [0=No,1=Yes] ? ')

    CIL_ANG = 0.0D0
    IF (fCyl.eq.1) THEN 
      CIL_ANG = RNUMBER(' Angle (degrees) of cylinder axis from X axis: ')
    ENDIF

    ns = irint(' Number of interfaces NS (Number of lenses=NS/2) ? ')

    ref0 = RNUMBER(' Refraction index of the medium containing source ?')
    ref1 = RNUMBER(' Refraction index of the next medium?')

    pp0 = RNUMBER(' p0: physical focal source-crl distance ')
    qq0 = RNUMBER(' q0: physical focal crl-image distance ')

    pp = RNUMBER(' p: focal source-crl distance ')
    qq = RNUMBER(' q: focal crl-image distance ')

    r0  = RNUMBER(' lens semiaperture [-1 for infinity]? ')

    ddIn  = 0.0D0
    ddV  = 0.0D0
    IF (ns.gt.1) THEN
      ddIn  = RNUMBER('lens thickness [along optical axis, in material] ? ')
      ddV  = RNUMBER('lens length [along optical axis, in vacuum] ? ')
    ENDIF

    print *,''
    print *,''
    

    !
    ! set values of g1
    !
    DO i=1,ns
      Write( stmp, '(i3)' ) i
      stmp = adjustl(stmp)
      itmp=10
      iOut = GfForceSetValue(g1,"FMIRR("//trim(stmp)//")",itmp)
      itmp=2
      IF (mod(i,itmp).EQ.0) THEN
      ! even surfaces
        iOut = GfForceSetValue(g1,"R_IND_OBJ("//trim(stmp)//")",ref1)
        iOut = GfForceSetValue(g1,"R_IND_IMA("//trim(stmp)//")",ref0)
        iOut = GfForceSetValue(g1,"T_SOURCE("//trim(stmp)//")",ddIn*0.5D0)
        iOut = GfForceSetValue(g1,"T_IMAGE("//trim(stmp)//")",ddV*0.5D0)
        ref_in = ref1
        ref_out = ref0
        changeConvexity=1
      ELSE
      ! odd surfaces
        iOut = GfForceSetValue(g1,"R_IND_OBJ("//trim(stmp)//")",ref0)
        iOut = GfForceSetValue(g1,"R_IND_IMA("//trim(stmp)//")",ref1)
        iOut = GfForceSetValue(g1,"T_SOURCE("//trim(stmp)//")",ddV*0.5D0)
        iOut = GfForceSetValue(g1,"T_IMAGE("//trim(stmp)//")",ddIn*0.5D0)
        ref_in = ref0
        ref_out = ref1
        changeConvexity=0
      ENDIF
      select case (iShape)
        ! now everything is based on 
        ! F_surface = R_surface/(2*delta) delta = (n1/n2) - 1
        case (1) !spherical case need only lens semiaperture and lens length
          !
          ! pre-calculations 
          !
          delta = 1.0-(ref1/ref0)
          !! f = 1.0/(1.0/qq+1.0/pp)*ns
          !! print *, "focal distance ", f    
          !! R = f*delta
          IF (ns.eq.1) THEN
            f = 1.0/(1.0/qq+1.0/pp)
            R = (ref1-ref0)/( ref1/qq + ref0/pp  )
          ELSE
            f = 1.0/(1.0/qq+1.0/pp)*ns
            R = f*delta
          ENDIF
          print *, "Focal distance is ", f
          print *, "R is ", R

          ccc(1) = 1.0D0
          ccc(2) = 1.0D0
          ccc(3) = 1.0D0
          ccc(9) = 2.0*R
        
        !case (2) !ellipsoid 
        !case (4) !paraboloid 
        !case (5) !plane
        !case (7) !hyperboloid

        case (2) !ellipsoid 
!
! test: use focal distance instead
!
!qq = (ref0/pp+ref1/qq)
!qq=1.0d0/qq
            print *,'Ellipsoid: Object is at Infinity'
            IF (ref0.gt.ref1) THEN
              print *,'******* Warning: n1>n2 & object at infinity: '
              print *,'******* Should you consider a hyperboloid instead of an ellipsoid?'
            ENDIF

            AXMAJ  =  qq*ref1/(ref0+ref1)
            print *,'Rev Ellipsoid a:   ',axmaj
            AXMIN = qq*SQRT(abs(ref1-ref0)/(ref0+ref1))
            print *,'Rev Ellipsoid b:   ',axmin
            AFOCI =  SQRT( AXMAJ**2-AXMIN**2 )
            print *,'Rev Ellipsoid c=sqrt(a^2-b^2): ',afoci
            print *,'Rev Ellipsoid focal discance c^2:   ',afoci**2
            ECCENT = AFOCI/AXMAJ
            print *,'Rev Ellipsoid excentricity:    ',eccent
ee = ref1/ref0
IF (ee.GT.1) ee=1.0d0/ee
print *,'Rev Ellipsoid excentricity OLD:    ',ee
            !C
            !C The center is computed on the basis of the object 
            !C and image positions
            !C
            YCEN  = -AXMAJ
            ZCEN  = 0D0
            !C
            !C Computes now the normal in the mirror center.
            !C
            !rncen=DblArr(3)
            RNCEN(1)  = .0D0
            RNCEN(2)  = -1D0
            RNCEN(3)  = 0.0D0
            !C
            !C Computes the tangent versor in the mirror center.
            !C
            !rtcen=DblArr(3)
            RTCEN(1)  =  .0D0
            RTCEN(2)  =   RNCEN(3)
            RTCEN(3)  = - RNCEN(2)
            !C Computes now the quadric coefficient with the mirror center
            !C located at (0,0,0) and normal along (0,0,1)
            !C
            !print *,'p[cm]:  ',pp
            !print *,'q[cm]:  ',qq
            !print *,'refraction index n0:  ',ref0
            !print *,'refraction index n1:  ',ref1
            print *,'Lens center at: ',0.0,ycen,zcen
            print *,'Lens normal: ',rncen 
            print *,'Lens tangent:',rtcen

            A  =  1/AXMIN**2
            B  =  1/AXMAJ**2
            C  =  A



            CCC(1) =  A
            CCC(2) =  B*RTCEN(2)**2 + C*RTCEN(3)**2
            CCC(3) =  B*RNCEN(2)**2 + C*RNCEN(3)**2
            CCC(4) =  .0D0
            CCC(5) =  2*(B*RNCEN(2)*RTCEN(2)+C*RNCEN(3)*RTCEN(3))
            CCC(6) =  .0D0
            CCC(7) =  .0D0
            CCC(8) =  .0D0
            CCC(9) =  2*(B*YCEN*RNCEN(2)+C*ZCEN*RNCEN(3))
            CCC(10) =  .0D0

!            END
!         3: BEGIN ; Toroidal ?????????????????
!                 R_MAJ        =   SSOUR*SIMAG*2/COSTHE/(SSOUR + SIMAG)
!                 R_MIN        =   SSOUR*SIMAG*2*COSTHE/(SSOUR + SIMAG)
!                 txt = [txt, 'Toroid is not a conic!', $
!                        String('Major radius: ', $
!                        R_MAJ, Format='(A40,G20.15)'), $
!                        String('Minor radius: ', $
!                        R_MIN, Format='(A40,G20.15)')]
!                 ;C
!                 ;C NOTE : The major radius is the in reality the radius of the torus
!                 ;C max. circle. The true major radius is then
!                 ;C
!                 R_MAJ        =   R_MAJ - R_MIN
!            END
        case (4) !paraboloid
            ! calculate the tangent radius (as for the sphere)
            R = 1D0/(ref0/pp+ref1/qq)
            R = R*(ref1-ref0)
            PARAM = -R
            COSTHE = 1.0D0
            SINTHE = 0.0D0
            IF (pp.LT.qq) THEN 
              YCEN        = - pp*SINTHE**2
              ZCEN        = - 2*pp*SINTHE*COSTHE
              fact = -1.0D0
            ELSE
              YCEN        = - qq*SINTHE**2
              ZCEN        = - 2*qq*SINTHE*COSTHE
              fact = 1.0D0
            ENDIF
            print *,'Paraboloid p parameter (=-R): ',PARAM
            print *,'Lens center at: ',0.0,ycen,zcen
            CCC(1)        =   1.0D0
            CCC(2)        =   COSTHE**2
            CCC(3)        =   SINTHE**2
            CCC(4)        =   0.0D0
            CCC(5)        =   2*fact*COSTHE*SINTHE
            CCC(6)        =   0.0D0
            CCC(7)        =   0.0D0
            CCC(8)        =   0.0D0
            CCC(9)        =   2*ZCEN*SINTHE - 2*PARAM*COSTHE
            CCC(10) =    .0D0

        case (5) !plane
            CCC(10) =    .0D0
            !C
            !C The sign of CCC(9) is <0 to keep consistency with the other cases
            !C normal.
            !C
            CCC(9) = - 1.0D0

        case (7) !hyperboloid

            print *,'Hyperboloid: Object is at Infinity'
            IF (ref0.lt.ref1) THEN
              print *,'******* Warning: n1<n2 & object at infinity: '
              print *,'******* Should you consider an ellipsoid instead of an ellipsoid?'
            ENDIF

            AXMAJ  =  qq*ref1/(ref0+ref1)
            print *,'Rev Hyperboloid a:   ',axmaj
            AXMIN = qq*SQRT(abs(ref1-ref0)/(ref0+ref1))
            print *,'Rev Hyperboloid b:   ',axmin
            AFOCI =  SQRT( AXMAJ**2+AXMIN**2 )
            print *,'Rev Hyperboloid c=sqrt(a^2+b^2): ',afoci
            print *,'Rev Hyperboloid focal discance c^2:   ',afoci**2
            ECCENT = AFOCI/AXMAJ
            print *,'Rev Ellipsoid excentricity:    ',eccent
EE = ref0/ref1
IF (ee.LT.1) ee=1.0D0/ee
print *,'Rev Ellipsoid excentricity OLD:    ',ee
!AXMAJ         =  qq/(1D0+EE)
!CC = AXMAJ*EE
!AXMIN         =  sqrt(CC*CC-AXMAJ*AXMAJ)

            ! 
            BRANCH=1.0D0 ! branch=+1,-1
            ! 
            YCEN  = -AXMAJ
            ZCEN  = 0.0D0
            RNCEN (1) = 0.0D0
            RNCEN (2) = -1.0
            RNCEN (3) = 0.0D0
            !         
            !C
            !C Computes the tangent in the same RF
            !C
            RTCEN (1) =   0.0D0
            RTCEN (2) = - RNCEN(3)   ! > 0 
            RTCEN (3) =   RNCEN(2)   ! > 0
            ! 
            ! 
            print *,'Hyperboloid a: ', AXMAJ
            print *,'Hyperboloid b: ', AXMIN
            print *,'Hyperboloid c: ', AFOCI
            print *,'Hyperboloid focal discance c^2: ', AFOCI**2
            print *,'Hyperboloid excentricity: ', ECCENT
            print *,'Hyperbola BRANCH: ',branch
            print *,'Lens center at: ',0.0D0,YCEN,ZCEN
            print *,'Lens normal: ',RNCEN
            print *,'Lens tangent: ',RTCEN
            !C
            !C Coefficients of the canonical form
            !C
            A   = - 1/AXMIN**2
            B   =   1/AXMAJ**2
            C   =   A
            !C
            !C Rotate now in the mirror RF. The equations are the same as for 
            !C the ellipse case.
            !C
            CCC(1)  =  A
            CCC(2)  =  B*RTCEN(2)**2 + C*RTCEN(3)**2
            CCC(3)  =  B*RNCEN(2)**2 + C*RNCEN(3)**2
            CCC(4)  =  0.0D0
            CCC(5)  =  2*(B*RNCEN(2)*RTCEN(2)+C*RNCEN(3)*RTCEN(3))
            CCC(6)  =  0.0D0
            CCC(7)  =  0.0D0
            CCC(8)  =  0.0D0
            CCC(9)  =  2*(B*YCEN*RNCEN(2)+C*ZCEN*RNCEN(3))
            CCC(10) =  0.0D0

            !         8: BEGIN ; Ice-cream cone
            !                 CCC(1-1)        =  1.0D0
            !                 CCC(2-1)        =  1.0D0
            !                 ;CCC(3-1)        =  -(TAN (!dpi/180*CONE_A))^2
            !                 CCC(3-1)        =  -(TAN (THETA))^2
            !                 txt = [txt, 'Cone is an Ice-cream cone']
            !            END
            !         9:
            !         else: BEGIN
            !            print,'Optical surface type not found: '+oeType[iType]
            !            END
            ! 

        
        case default !not yet implemented
        
      end select
      !
      ! is cylindrical?
      !
      IF (fCyl.eq.1) THEN 
       ! C
       ! C Set to zero the coeff. involving X for the cylinder case, after
       ! C projecting the surface on the desired plane.
       ! C
        CIL_ANG = TORAD*CIL_ANG
        COS_CIL = COS(CIL_ANG)
        SIN_CIL = SIN(CIL_ANG)
        cccTmp = CCC  ! vector copy
        CCC(1) = cccTmp(1)*SIN_CIL**4 + cccTmp(2)*COS_CIL**2*SIN_CIL**2 - & !X^2
           cccTmp(4)*COS_CIL*SIN_CIL**3
        CCC(2) = cccTmp(2)*COS_CIL**4 + cccTmp(1)*COS_CIL**2*SIN_CIL**2 - & !Y^2
           cccTmp(4)*COS_CIL**3*SIN_CIL
        CCC(3) = cccTmp(3) !Z^2
        CCC(4) = - 2*cccTmp(1)*COS_CIL* SIN_CIL**3 - &
             2*cccTmp(2)*COS_CIL**3*SIN_CIL + &
             2*cccTmp(4)*COS_CIL**2*SIN_CIL**2 !X Y
        CCC(5) = cccTmp(5)*COS_CIL**2 - cccTmp(6)*COS_CIL*SIN_CIL !Y Z
        CCC(6) = cccTmp(6)*SIN_CIL**2 - cccTmp(5)*COS_CIL*SIN_CIL !X Z
        CCC(7) = cccTmp(7)*SIN_CIL**2 - cccTmp(8)*COS_CIL*SIN_CIL !X
        CCC(8) = cccTmp(8)*COS_CIL**2 - cccTmp(7)*COS_CIL*SIN_CIL !Y
        CCC(9) = cccTmp(9) !Z
        CCC(10)= cccTmp(10)
      ENDIF
      ! C
      ! C Set the correct mirror convexity.
      ! C
      !IF (ref_in.le.ref_out) THEN
      IF (changeConvexity.eq.1) THEN
        CCC(5) = - CCC(5)
        CCC(6) = - CCC(6)
        CCC(9) = - CCC(9)
      ELSE
      END IF

      DO j=1,10
        Write( stmp2, '(i2)' ) j
        stmp2 = adjustl(stmp2)
        iOut = GfForceSetValue(g1,"ccc("//trim(stmp2)//","//trim(stmp)//")",ccc(j))
      ENDDO
    ENDDO
    iOut = GfForceSetValue(g1,"fCyl",fCyl)
    iOut = GfForceSetValue(g1,"cil_ang",cil_ang)


    ! define pp0,qq0
    iOut = GfSetValue(g1,"T_SOURCE(1)",pp0)
    iOut = GfSetValue(g1,"T_IMAGE("//trim(stmp)//")",qq0)


    ! for memo, store the inputs [not to be used]
    iOut = GfForceSetValue(g1,"iShape",iShape)
    iOut = GfForceSetValue(g1,"ns",ns)
    iOut = GfForceSetValue(g1,"r0",r0)
    iOut = GfForceSetValue(g1,"ref0",ref0)
    iOut = GfForceSetValue(g1,"ref1",ref1)
    iOut = GfForceSetValue(g1,"pp",pp)
    iOut = GfForceSetValue(g1,"qq",qq)
    iOut = GfForceSetValue(g1,"pp0",pp0)
    iOut = GfForceSetValue(g1,"qq0",qq0)
    iOut = GfForceSetValue(g1,"r0",r0)
    iOut = GfForceSetValue(g1,"ddIn",ddIn)
    iOut = GfForceSetValue(g1,"ddV",ddV)
!
! print all stored variables on screen
!
!     iOut = GfTypePrint(g1)
!     if (.not. iOut) return

!
! dump the GfType into a new file
!

     fileName="crl.01"
     iOut = GfFileWrite(g1,fileName)
     if (iOut) then
        print *,'File written to disk: '//trim(fileName)
     else
        print *,'FAILED TO WRITE FILE: '//trim(fileName)
     end if

    print *,'------------ ccc ---------------'
          write(*,'(a,f20.10)') ,'ccc(1) = ',ccc(1)
          write(*,'(a,f20.10)') ,'ccc(2) = ',ccc(2)
          write(*,'(a,f20.10)') ,'ccc(3) = ',ccc(3)
          write(*,'(a,f20.10)') ,'ccc(4) = ',ccc(4)
          write(*,'(a,f20.10)') ,'ccc(5) = ',ccc(5)
          write(*,'(a,f20.10)') ,'ccc(6) = ',ccc(6)
          write(*,'(a,f20.10)') ,'ccc(7) = ',ccc(7)
          write(*,'(a,f20.10)') ,'ccc(8) = ',ccc(8)
          write(*,'(a,f20.10)') ,'ccc(9) = ',ccc(9)
          write(*,'(a,f20.10)') ,'ccc(10) = ',ccc(10)
    print *,'--------------------------------'

End Subroutine precrl

!
!
!
subroutine runcrl
    !use shadow_globaldefinitions
    !use shadow_beamio
    !use shadow_variables
    !use shadow_crl
    
    implicit none
    
    type (poolSource)  :: src
    type (poolOE), allocatable, dimension(:) :: arrOE
    Real (kind=skr), allocatable, dimension(:,:) :: ray
    
    Integer (kind=ski) :: i, ncol1, npoint1, ns, iOut, iErr, iFlag, iCount
    Character(len=3) :: stmp
    
    print *,'***********************************************************************'
    print *,'  CRL: using source from begin.dat and CRL definition from crl.01'
    print *,'        output file: final.01 '
    print *,'***********************************************************************'
    call beamGetDim("begin.dat", ncol1, npoint1, iFlag, iErr)
    allocate( ray(18,npoint1) )
    call beamLoad(ray,iErr,ncol1,npoint1,"begin.dat")
    ncol = ncol1
    print *, ncol1
    npoint = npoint1
    print *,'runcrl: npoint,ncol: ',npoint1,ncol1
    print *,'runcrl: reading crl.01...'
    call ReadCRL("crl.01",arrOE)
    print *,'runcrl: Done reading crl.01'
    
    ns = size(arrOE)
    do i=1,ns
      Write( stmp, '(i3)' ) i
      stmp = adjustl(stmp)
      call PoolOEWrite(arrOE(i),"start."//stmp)
      print *,'runcrl: File written to disk: start.'//stmp
    end do
    iCount = 1
    call TraceCRL(arrOE, ray, iCount)
    
    call beamWrite(ray, iErr, ncol1, npoint1, "final.01")
    print *,'runcrl: File written to disk: final.01' 

    return
End SUbroutine runcrl

  !
  ! Private 
  !
  Subroutine TraceCRL(arrOE,ray18,icount)
    Type(poolOE), dimension(:), intent(inout) :: arrOE
    Real(kind=skr), dimension(:,:), intent(in out) :: ray18
    Integer(kind=ski), intent(inout) :: icount

    Integer(kind=ski) :: nOE, nPoint

    Integer(kind=ski) :: i

    nOE = size(arrOE)
    nPoint = size(ray18,2)
    Do i=1, nOE
      Call TraceOE (arrOE(i),ray18,nPoint,iCount)
      iCount = iCount+1
    End Do
  End Subroutine TraceCRL



  Subroutine ReadCRL(fname,arrOE)
    Character(kind=skc, len=*), intent(in) :: fname
    Type(poolOE), dimension(:), allocatable, intent(out) :: arrOE
    
    Integer(kind=ski) :: ns
    type(GfType) :: gf
    
    Logical :: iOut
    Integer(kind=ski) :: i, j
    Character(len=3) :: stmp, stmp2
    
    If(.not.GfFileLoad(gf,fname)) print *, "unable to load all file"
    iOut = GfGetValue(gf,"ns",ns)
    Allocate( arrOE(ns) )
    Do i=1, ns
      Call PoolOEDefault(arrOE(i))
      Write( stmp, '(i3)' ) i
      stmp = adjustl(stmp)
      iOut = GfGetValue(gf,"FMIRR("//trim(stmp)//")",arrOE(i)%FMIRR).and.iOut
      iOut = GfGetValue(gf,"R_IND_OBJ("//trim(stmp)//")",arrOE(i)%R_IND_OBJ).and.iOut
      iOut = GfGetValue(gf,"R_IND_IMA("//trim(stmp)//")",arrOE(i)%R_IND_IMA).and.iOut
      iOut = GfGetValue(gf,"T_SOURCE("//trim(stmp)//")",arrOE(i)%T_SOURCE).and.iOut
      iOut = GfGetValue(gf,"T_IMAGE("//trim(stmp)//")",arrOE(i)%T_IMAGE).and.iOut
      arrOE(i)%F_REFRAC = 1
      arrOE(i)%FWRITE = 0
      arrOE(i)%T_INCIDENCE = 0.0D0
      arrOE(i)%T_REFLECTION = 180.0D0
!       arrOE(i)%F_EXT = 1
      arrOE(i)%F_DEFAULT = 0
      arrOE(i)%F_ANGLE = 1
      Do j=1,10
        Write( stmp2, '(i2)' ) j
        stmp2 = adjustl(stmp2)
        iOut = GfGetValue(gf,"ccc("//trim(stmp2)//","//trim(stmp)//")",arrOE(i)%ccc(j)).and.iOut
      End Do
    End Do
  End Subroutine ReadCRL

  
  
  

End Module shadow_crl
