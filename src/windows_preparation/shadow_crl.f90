












































Module shadow_crl !compound refractive lenses

! todo: integrate this module into shadow_preprocessors and shadow_kernel

  Use, intrinsic :: ISO_C_BINDING

  Use shadow_globaldefinitions
  Use gfile
  Use shadow_variables
  Use shadow_kernel

  Implicit None

  Public :: precrl , runcrl, pretransfocator
  Private :: TraceCRL, ReadCRL
  
  Contains


!----
!----  Subroutine precrl
!----
!----  Preprocessor for Compound Refractive Lenses
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
    integer(kind=ski)      :: ns,i,j,ishape_local,fcyl_local
    integer(kind=ski)      :: changeConvexity,itmp,ftmp
    integer(kind=ski)      :: fwrite_local,fhit_c_local,fshape_local,f_r_ind_local
    !integer(kind=ski)      :: n_oe_local
    integer(kind=ski)      :: f_ext_local
    real(kind=skr)         :: rwidx1_local,rwidx2_local,rlen1_local,rlen2_local
    real(kind=skr)         :: ref0,ref1,att0,att1
    real(kind=skr)         :: pp,qq,pp0,qq0,ddIn,ddV,qqtmp
    character(len=sklen)   :: fileName,file0,file1
    character(len=3)       :: stmp,stmp2
    !Logical                :: arggiven=.false., esta, iOut
    Logical                :: iOut
    real(kind=skr)         :: cil_ang_local, sin_cil, cos_cil, ref_in, ref_out
    real(kind=skr),dimension(10)  :: ccc, cccTmp
    !real(kind=skr), parameter :: torad = 0.017453292519943295769237
    ! variables for sphere
    real(kind=skr)         :: f, R, delta
    ! variables for ellipsoid/hyperboloid
    real(kind=skr)         :: axmaj, axmin, afoci, ee, eccent, ycen, zcen
    real(kind=skr)         :: a,b,c
    real(kind=skr),dimension(3) :: rncen,rtcen
    ! variables for paraboloid
    real(kind=skr)         :: costhe, sinthe, param, fact
    ! variables for hyperboloid
    real(kind=skr)         :: cc, branch,crl_length,tmp1,tmp2
    
    ccc=0.0D0
    file0="none_specified"
    file1="none_specified"
    ref0=1d0
    ref1=1d0
    att0=0d0
    att1=0d0

    !n_oe_local = irint('What is the number of this optical element [default=1]? ')
    !IF (n_oe_local .eq. 0) n_oe_local = 1

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
    ishape_local = irint('?> ')

    fcyl_local = irint('Cylindical [0=No,1=Yes] ? ')

    cil_ang_local = 0.0D0
    IF (fcyl_local.eq.1) THEN 
      cil_ang_local = RNUMBER(' Angle (degrees) of cylinder axis from X axis: ')
    ENDIF

    ns = irint(' Number of interfaces NS (Number of lenses=NS/2) ? ')

    print *,' You must enter refraction index in the two media:'
    print *,'    first, for the medium containing the source (OBJECT for the first interface)'
    print *,'    then, for the next medium (IMAGE for the first interface)'
    print *,'    '
    print *,' Refraction index (optical constants) from: '
    print *,'     0: constant: keyword in both media'
    print *,'     1: file(from prerefl) in first medium keyboard in next medium'
    print *,'     2: keyboard in first medium and file (from prerefl) next medium'
    print *,'     3: file(from prerefl) in both media'
    print *,'     '
    f_r_ind_local = irint('?>')

    select case (f_r_ind_local)
        case (0) 
          ref0 = RNUMBER(' Refraction index of the medium containing source ?')
          ref1 = RNUMBER(' Refraction index of the next medium ?')
          att0 = RNUMBER(' Attenuation coeff [UserLength^-1] of the medium containing source ?')
          att1 = RNUMBER(' Attenuation coeff [UserLength^-1] of the next medium ?')
        case (1) 
          file0 = rstring("file (from prerefl) for medium containing source: ")
          ref1 = RNUMBER(' Refraction index of the next medium ? ')
          att1 = RNUMBER(' Attenuation coeff [UserLength^-1] of the next medium ? ')
        case (2) 
          ref0 = RNUMBER(' Refraction index of the medium containing source ?')
          att0 = RNUMBER(' Attenuation coeff [UserLength^-1] of the medium containing source ?')
          file1 = rstring("file (from prerefl) for the next medium ? ")
        case (3) 
          file0 = rstring("file (from prerefl) for medium containing source ? ")
          file1 = rstring("file (from prerefl) for the next medium ? ")
    end select 





    pp0 = RNUMBER(' p0: physical distance from source plane (or continuation plane) to CRLs first interface ')
    qq0 = RNUMBER(' q0: physical distance from last interface of the CRL to the image plane (or continuation plane) ')


    print *,'CRL parameters: '
    print *,'   0:  internal/calculated (from focal distances)'
    print *,'   1:  external/user defined'
    f_ext_local = irint('?>')


    if (f_ext_local.eq.0) then
      pp = RNUMBER(' pp: focal source-crl distance ')
      qq = RNUMBER(' qq: focal crl-image distance ')
    else
      if (ishape_local.eq.1) then
        R = RNUMBER(' Radius of the sphere [UserLength] ? ')
      elseif(ishape_local.eq.4) then
        R = RNUMBER(' Radius of curvature of the paraboloid at the tip ? ')
      elseif(ishape_local.eq.2.or.ishape_local.eq.7) then
        AXMAJ = RNUMBER(' Major Axis ? ')
        AXMIN = RNUMBER(' Minor Axis ? ')
        R = 0.5*(AXMAJ + AXMIN) ! to avoid R undefined
      end if
    endif

    !
    ! lens dimension
    !
    fhit_c_local = IYES ('Lens dimensions finite [ Y/N ] ?')
    IF (fhit_c_local.EQ.1) THEN
       !IF (IVERB.EQ.1) THEN
        WRITE(6,*)'Lens shape. Options:'
        WRITE(6,*)'         rectangular :    1'
        WRITE(6,*)'   full  elliptical  :    2'
        !WRITE(6,*)'  "hole" elliptical  :    3'
       !END IF
       fshape_local = IRINT ('Shape: [ 1, 2] ?')
       IF (fshape_local.EQ.1) THEN
         rwidx1_local = RNUMBER ('Lens half-width x(+) ? ')
         rwidx2_local = RNUMBER ('Lens half-width x(-) ? ')
         rlen1_local = RNUMBER ('Lens half-length y(+) ? ')
         rlen2_local = RNUMBER ('Lens half-length y(-) ? ')
       ELSE
         rwidx1_local = 0D0 ! just define it
         rlen1_local = 0D0  ! just define it
         rwidx2_local = RNUMBER ('External Outline Major axis ( x ) ? ')
         rlen2_local = RNUMBER ('External Outline Minor axis ( y ) ? ')
         !IF (fshape_local.EQ.3) THEN
         !  rwidx1_local = RNUMBER ('Internal Outline Major axis ( x ) ? ')
         !  rlen1_local = RNUMBER ('Internal Outline Minor axis ( y ) ? ')
         !END IF
       END IF
    ELSE
         fshape_local = 0 ! just define it
         rwidx1_local = 0D0 ! just define it
         rlen1_local  = 0D0  ! just define it
         rwidx2_local = 0D0 ! just define it
         rlen2_local  = 0D0 ! just define it
    END IF

    ddIn  = 0.0D0
    ddV  = 0.0D0
    IF (ns.gt.1) THEN
      ddIn  = RNUMBER('lens thickness [along optical axis, in material] (=ddIn) ? ')
      ddV  = RNUMBER('lens length [along optical axis, in vacuum] (=ddV) ? ')
    ENDIF

    print *,'Verbose-Shadow-Run? '
    print *,'    files to save option: '
    print *,'      0: all files (mirr.xx, star.xx)'
    print *,'      1: mirror file only -- mirr.xx'
    print *,'      2: image file only -- star.xx'
    print *,'      3: none'
    fwrite_local = irint('?> ')
    print *,''

    filename = RSTRING('Name of output file (default: crl.01): ')
    if (trim(filename).eq."") filename = "crl.01"
    


    print *," "
    print *,"Please wait whilst creating input file... "
    print *," "
    print *," "
    print *," "
    ! end of input section

    !
    ! set values of g1
    !
    DO i=1,ns
      Write( stmp, '(i3)' ) i
      stmp = adjustl(stmp)
      ! set variables that are constat for every lens
      itmp=10
      iOut = GfForceSetValue(g1,"FMIRR("//trim(stmp)//")",itmp)
      iOut = GfForceSetValue(g1,"FWRITE("//trim(stmp)//")",fwrite_local)
      ! finite dimensions now
      iOut = GfForceSetValue(g1,"FHIT_C("//trim(stmp)//")",fhit_c_local)
      !if (fhit_c_local.eq.1) then 
      iOut = GfForceSetValue(g1,"FSHAPE("//trim(stmp)//")",fshape_local)
      iOut = GfForceSetValue(g1,"RWIDX1("//trim(stmp)//")",rwidx1_local)
      iOut = GfForceSetValue(g1,"RWIDX2("//trim(stmp)//")",rwidx2_local)
      iOut = GfForceSetValue(g1,"RLEN1("//trim(stmp)//")",rlen1_local)
      iOut = GfForceSetValue(g1,"RLEN2("//trim(stmp)//")",rlen2_local)
      !endif

      ! set variables that are change with lens number
      itmp=2
      IF (mod(i,itmp).EQ.0) THEN
         select case(f_r_ind_local)
            case (1)
              ftmp = 2
            case (2)
              ftmp = 1
            case default
              ftmp = f_r_ind_local
         end select
      ! even surfaces
        iOut = GfForceSetValue(g1,"F_R_IND("//trim(stmp)//")",ftmp)
        iOut = GfForceSetValue(g1,"R_IND_OBJ("//trim(stmp)//")",ref1)
        iOut = GfForceSetValue(g1,"R_IND_IMA("//trim(stmp)//")",ref0)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_OBJ("//trim(stmp)//")",att1)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_IMA("//trim(stmp)//")",att0)
        iOut = GfForceSetValue(g1,"FILE_R_IND_OBJ("//trim(stmp)//")",file1)
        iOut = GfForceSetValue(g1,"FILE_R_IND_IMA("//trim(stmp)//")",file0)
        iOut = GfForceSetValue(g1,"T_SOURCE("//trim(stmp)//")",ddIn*0.5D0)
        iOut = GfForceSetValue(g1,"T_IMAGE("//trim(stmp)//")",ddV*0.5D0)
        ref_in = ref1
        ref_out = ref0
        changeConvexity=1
      ELSE
      ! odd surfaces
        iOut = GfForceSetValue(g1,"F_R_IND("//trim(stmp)//")",f_r_ind_local)
        iOut = GfForceSetValue(g1,"R_IND_OBJ("//trim(stmp)//")",ref0)
        iOut = GfForceSetValue(g1,"R_IND_IMA("//trim(stmp)//")",ref1)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_OBJ("//trim(stmp)//")",att0)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_IMA("//trim(stmp)//")",att1)
        iOut = GfForceSetValue(g1,"FILE_R_IND_OBJ("//trim(stmp)//")",file0)
        iOut = GfForceSetValue(g1,"FILE_R_IND_IMA("//trim(stmp)//")",file1)
        iOut = GfForceSetValue(g1,"T_SOURCE("//trim(stmp)//")",ddV*0.5D0)
        iOut = GfForceSetValue(g1,"T_IMAGE("//trim(stmp)//")",ddIn*0.5D0)
        ref_in = ref0
        ref_out = ref1
        changeConvexity=0 ! first surface is concave, R>0, (as SHADOW's default)
      ENDIF

      select case (ishape_local)
        ! now everything is based on 
        ! F_surface = R_surface/(2*delta) delta = (n1/n2) - 1
        case (1) !spherical case need only lens semiaperture and lens length
          !
          ! pre-calculations 
          !
          delta = 1.0-(ref1/ref0)
          if (f_ext_local.eq.0) then   ! internal/calculated
            IF (ns.eq.1) THEN
              f = 1.0/(1.0/qq+1.0/pp)
              R = (ref1-ref0)/( ref1/qq + ref0/pp  )
            ELSE
              f = 1.0/(1.0/qq+1.0/pp)
              R = ns*f*delta
              !debugging srio@esrf.eu 20130515 R>0 in shadow means concave
              !R = -R   ! change sign: first interface must be concave
            ENDIF
          else ! external/user defined
              f = R/delta/ns ! TODO: calculate for ellipse/hyperbola
          endif

          if (i.eq.1) then
            print *, "Surface       : ", i
            print *, "Focal distance (full CRL)      : ", f
            print *, "Focal distance (this interface): ", f*ns
            print *, "R             : ", R
            print *, "changeConvexity     : ", changeConvexity
          endif

          ccc(1) = 1.0D0
          ccc(2) = 1.0D0
          ccc(3) = 1.0D0
          ccc(9) = -2.0*R
        
        case (2) !ellipsoid 
            if (f_ext_local.eq.0) then ! internal/calculated
              qqtmp = qq
              ! for CRL
              IF (ns.GT.1) THEN 
                ! use focal distance instead
                qqtmp = (ref0/pp+ref1/qq)
                qqtmp=1.0d0/qqtmp
                qqtmp=qqtmp*ns
              END IF
              print *,'Ellipsoid: Object is at Infinity'
              IF (ref0.gt.ref1) THEN
                print *,'******* Warning: n1>n2 & object at infinity: '
                print *,'******* Should you consider a hyperboloid instead of an ellipsoid?'
              ENDIF
  
              AXMAJ = qqtmp*ref1/(ref0+ref1)
              AXMIN = qqtmp*SQRT(abs(ref1-ref0)/(ref0+ref1))
            endif



            AFOCI =  SQRT( AXMAJ**2-AXMIN**2 )
            ECCENT = AFOCI/AXMAJ
            ee = ref1/ref0
            IF (ee.GT.1) ee=1.0d0/ee
            if (i.eq.1) then
              print *,'Surface: ',i
              print *,'Rev Ellipsoid a:   ',axmaj
              print *,'Rev Ellipsoid b:   ',axmin
              print *,'Rev Ellipsoid c=sqrt(a^2-b^2): ',afoci
              print *,'Rev Ellipsoid focal discance c^2:   ',afoci**2
              print *,'Rev Ellipsoid excentricity:    ',eccent
              print *,'Rev Ellipsoid excentricity OLD:    ',ee
            endif
            !C
            !C The center is computed on the basis of the object 
            !C and image positions
            !C
            YCEN  = -AXMAJ
            ZCEN  = 0D0
            !C
            !C Computes now the normal in the mirror center.
            !C
            RNCEN(1)  = .0D0
            RNCEN(2)  = -1D0
            RNCEN(3)  = 0.0D0
            !C
            !C Computes the tangent versor in the mirror center.
            !C
            RTCEN(1)  =  .0D0
            RTCEN(2)  =   RNCEN(3)
            RTCEN(3)  = - RNCEN(2)
            !C Computes now the quadric coefficient with the mirror center
            !C located at (0,0,0) and normal along (0,0,1)
            !C
            if (i.eq.1) then
              print *,'Lens center at: ',0.0,ycen,zcen
              print *,'Lens normal: ',rncen 
              print *,'Lens tangent:',rtcen
            endif

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


            ! 
            ! for the parabola we need the radius of the tangent circle, 
            ! so this part is the same as for the sphere:
            !
            delta = 1.0-(ref1/ref0)

            if (f_ext_local.eq.0) then
              IF (ns.eq.1) THEN
                ! for single lens
                f = 1.0/(1.0/qq+1.0/pp)
                R = (ref1-ref0)/( ref1/qq + ref0/pp  )
                R = abs(R)
              ELSE
                f = 1.0/(1.0/qq+1.0/pp)
                R = ns*f*delta
              ENDIF
            else 
                f = R/delta/ns
            endif


            ! calculate the tangent radius (as for the sphere)
            !IF (ns.GT.1) THEN 
            !  ! for CRL
            !  R = R*ns
            !  R = -R
            !ELSE 
            !  R = 1D0/(ref0/pp+ref1/qq)
            !  R = R*(ref1-ref0)
            !END IF
            PARAM = R
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

            if (i.eq.1) then
              print *, "Surface       : ", i
              print *, "R             : ", R
              print *, "changeConvexity     : ", changeConvexity
              print *,'Paraboloid p parameter (=R): ',PARAM
              print *,'Lens center at: ',0.0,ycen,zcen
            endif
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

            if (f_ext_local.eq.0) then
              AXMAJ  =  qq*ref1/(ref0+ref1)
              AXMIN = qq*SQRT(abs(ref1-ref0)/(ref0+ref1))
            endif

            AFOCI =  SQRT( AXMAJ**2+AXMIN**2 )
            ECCENT = AFOCI/AXMAJ
            EE = ref0/ref1
            IF (ee.LT.1) ee=1.0D0/ee

            if (i.eq.1) then
              print *,'Rev Hyperboloid a:   ',axmaj
              print *,'Rev Hyperboloid b:   ',axmin
              print *,'Rev Hyperboloid c=sqrt(a^2+b^2): ',afoci
              print *,'Rev Hyperboloid focal distance c^2:   ',afoci**2
              print *,'Rev Ellipsoid excentricity:    ',eccent
              print *,'Rev Ellipsoid excentricity OLD:    ',ee
            endif
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
            if (i.eq.1) then
              print *,'Hyperboloid a: ', AXMAJ
              print *,'Hyperboloid b: ', AXMIN
              print *,'Hyperboloid c: ', AFOCI
              print *,'Hyperboloid focal discance c^2: ', AFOCI**2
              print *,'Hyperboloid excentricity: ', ECCENT
              print *,'Hyperbola BRANCH: ',branch
              print *,'Lens center at: ',0.0D0,YCEN,ZCEN
              print *,'Lens normal: ',RNCEN
              print *,'Lens tangent: ',RTCEN
            endif
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
      IF (fcyl_local.eq.1) THEN 
       ! C
       ! C Set to zero the coeff. involving X for the cylinder case, after
       ! C projecting the surface on the desired plane.
       ! C
        cil_ang_local = TORAD*cil_ang_local
        COS_CIL = COS(cil_ang_local)
        SIN_CIL = SIN(cil_ang_local)
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


    ! set pp0,qq0
    iOut = GfSetValue(g1,"T_SOURCE(1)",pp0)
    iOut = GfSetValue(g1,"T_IMAGE("//trim(stmp)//")",qq0)


    ! 
    ! calculate CRL length
    !

    select case (ns) 
      case(1) 
        crl_length = 0.0
      case(2) 
        iOut = GfGetValue(g1,"T_IMAGE(1)",tmp1) 
        iOut = GfGetValue(g1,"T_SOURCE(2)",tmp2)
        crl_length = tmp1 + tmp2
      case default
        iOut = GfGetValue(g1,"T_IMAGE(1)",tmp1) 
        iOut = GfGetValue(g1,"T_SOURCE(2)",tmp2)
        crl_length = tmp1 + tmp2


        do i=1,ns-1 
          write( stmp2, '(i2)' ) i
          stmp2 = adjustl(stmp2)
          iOut = GfGetValue(g1,"T_IMAGE("//trim(stmp2)//")",tmp1) 
          write( stmp2, '(i2)' ) i+1
          stmp2 = adjustl(stmp2)
          iOut = GfGetValue(g1,"T_SOURCE("//trim(stmp2)//")",tmp2)
          crl_length = crl_length + tmp1 + tmp2
        enddo
    end select 
    print *,'CLR length is: ',crl_length
    ! for memo, store the some input variables [not needed]
    iOut = GfForceSetValue(g1,"ishape_local",ishape_local)
    iOut = GfForceSetValue(g1,"fwrite_local",fwrite_local)
    iOut = GfForceSetValue(g1,"fcyl_local",fcyl_local)
    iOut = GfForceSetValue(g1,"cil_ang_local",cil_ang_local)
    iOut = GfForceSetValue(g1,"fhit_c_local",fhit_c_local)
    iOut = GfForceSetValue(g1,"fshape_local",fshape_local)
    iOut = GfForceSetValue(g1,"rwidx1_local",rwidx1_local)
    iOut = GfForceSetValue(g1,"rwidx2_local",rwidx2_local)
    iOut = GfForceSetValue(g1,"rlen1_local",rlen1_local)
    iOut = GfForceSetValue(g1,"rlen2_local",rlen2_local)
    iOut = GfForceSetValue(g1,"ns",ns)
    iOut = GfForceSetValue(g1,"ref0",ref0)
    iOut = GfForceSetValue(g1,"ref1",ref1)
    iOut = GfForceSetValue(g1,"att0",att0)
    iOut = GfForceSetValue(g1,"att1",att1)
    iOut = GfForceSetValue(g1,"pp",pp)
    iOut = GfForceSetValue(g1,"qq",qq)
    iOut = GfForceSetValue(g1,"pp0",pp0)
    iOut = GfForceSetValue(g1,"qq0",qq0)
    iOut = GfForceSetValue(g1,"ddIn",ddIn)
    iOut = GfForceSetValue(g1,"ddV",ddV)
    iOut = GfForceSetValue(g1,"R",R)
    iOut = GfForceSetValue(g1,"crl_length",crl_length)

!
! print all stored variables on screen
!
!     iOut = GfTypePrint(g1)
!     if (.not. iOut) return

!
! dump the GfType into a new file
!

     !call FNAME(fileName,"crl",n_oe_local,itwo)
     !fileName="crl.01"
     iOut = GfFileWrite(g1,fileName)
     if (iOut) then
        print *,'File written to disk: '//trim(fileName)
     else
        print *,'FAILED TO WRITE FILE: '//trim(fileName)
     end if

    print *,'------------ ccc (last interface) ---------------'
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
    print *,'-------------------------------------------------'

    print *,' '
    print *,'Use shadow3 command "runcrl" to run shadow'
End Subroutine precrl

!
!
!
subroutine runcrl(idefault)
    
    implicit none
    
    Integer (kind=ski),intent(in) :: idefault
    type (poolSource)  :: src
    type (poolOE), allocatable, dimension(:) :: arrOE
    Real (kind=skr), allocatable, dimension(:,:) :: ray
    character(len=sklen)   :: filesource,filecrl,fileout
    
    Integer (kind=ski)    :: i, ncol1, npoint1, ns, iOut, iErr, iFlag, iCount
    !Character(len=3) :: stmp
    character(len=sklen)  :: stmp
    
    if (idefault.eq.0) then
       filesource = RSTRING("File with source (default: begin.dat): ")
       if (trim(filesource).eq."") filesource="begin.dat"
       filecrl = RSTRING("File with CRL definition (default: crl.01): ")
       if (trim(filecrl).eq."") filecrl="crl.01"
       fileout = RSTRING("File final image (star.xx type, default: final.01): ")
       if (trim(fileout).eq."") fileout="final.01"
    else
       filesource="begin.dat"
       filecrl="crl.01"
       fileout="final.01"
    endif


    print *,'***********************************************************************'
    print *,'  CRL: using source from '//trim(filesource)//' and CRL definition from '//trim(filecrl)
    print *,'        output file: '//trim(fileout)
    print *,'***********************************************************************'
    call beamGetDim(filesource, ncol1, npoint1, iFlag, iErr)
    allocate( ray(18,npoint1) )
    call beamLoad(ray,iErr,ncol1,npoint1,filesource)
    ncol = ncol1
    print *, ncol1
    npoint = npoint1
    print *,'runcrl: npoint,ncol: ',npoint1,ncol1
    print *,'runcrl: reading '//trim(filecrl)//'...'
    call ReadCRL(filecrl,arrOE)
    print *,'runcrl: Done reading '//trim(filecrl)
    
    ns = size(arrOE)
    ! 
    ! write shadow files (not needed)
    ! 
    IF ( arrOE(1)%FWRITE.LT.3) THEN 
      OPEN (UNIT=23,FILE='systemfile.dat',STATUS='UNKNOWN')
      do i=1,ns
        CALL FNAME (stmp, 'start', i, izero)
        call PoolOEWrite(arrOE(i),stmp)
        !Write( stmp, '(i3)' ) i
        !stmp = adjustl(stmp)
        !call PoolOEWrite(arrOE(i),"start."//stmp)
        print *,'runcrl: File written to disk: '//trim(stmp)
        write(23,'(a)') trim(stmp)
      end do
      CLOSE(23)
      print *,'runcrl: File written to disk: systemfile.dat'
    END IF


    print *,'runcrl: Running trace for all interfaces'
    iCount = 1
    call TraceCRL(arrOE, ray, iCount)
    
    call beamWrite(ray, iErr, ncol1, npoint1, fileout)
    print *,'runcrl: File written to disk: '//fileout 

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
      print *,'>> TraceCRL: tracing surface ',iCount
      Call TraceOE (arrOE(i),ray18,nPoint,iCount)
      iCount = iCount+1
    End Do
  End Subroutine TraceCRL



  ! this routine reads the CRL definition in crl.01 and 
  ! creates an OE array of types with each individual lens

  Subroutine ReadCRL(fname,arrOE)
    Character(kind=skc, len=*), intent(in) :: fname
    Type(poolOE), dimension(:), allocatable, intent(out) :: arrOE
    
    Integer(kind=ski) :: ns
    type(GfType) :: gf
    
    Logical :: iOut, iOut2
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
      iOut = GfGetValue(gf,"F_R_IND("//trim(stmp)//")",arrOE(i)%F_R_IND).and.iOut
      iOut = GfGetValue(gf,"R_IND_OBJ("//trim(stmp)//")",arrOE(i)%R_IND_OBJ).and.iOut
      iOut = GfGetValue(gf,"R_IND_IMA("//trim(stmp)//")",arrOE(i)%R_IND_IMA).and.iOut

      iOut = GfGetValue(gf,"R_ATTENUATION_OBJ("//trim(stmp)//")",arrOE(i)%R_ATTENUATION_OBJ).and.iOut
      iOut = GfGetValue(gf,"R_ATTENUATION_IMA("//trim(stmp)//")",arrOE(i)%R_ATTENUATION_IMA).and.iOut
      iOut = GfGetValue(gf,"FILE_R_IND_OBJ("//trim(stmp)//")",arrOE(i)%FILE_R_IND_OBJ).and.iOut
      iOut = GfGetValue(gf,"FILE_R_IND_IMA("//trim(stmp)//")",arrOE(i)%FILE_R_IND_IMA).and.iOut

      iOut = GfGetValue(gf,"T_SOURCE("//trim(stmp)//")",arrOE(i)%T_SOURCE).and.iOut
      iOut = GfGetValue(gf,"T_IMAGE("//trim(stmp)//")",arrOE(i)%T_IMAGE).and.iOut
      iOut = GfGetValue(gf,"FWRITE("//trim(stmp)//")",arrOE(i)%FWRITE).and.iOut
      arrOE(i)%F_REFRAC = 1
!      arrOE(i)%FWRITE = 0
      arrOE(i)%T_INCIDENCE = 0.0D0
      arrOE(i)%T_REFLECTION = 180.0D0
!       arrOE(i)%F_EXT = 1
      arrOE(i)%F_DEFAULT = 0
      arrOE(i)%F_ANGLE = 0
      Do j=1,10
        Write( stmp2, '(i2)' ) j
        stmp2 = adjustl(stmp2)
        iOut = GfGetValue(gf,"ccc("//trim(stmp2)//","//trim(stmp)//")",arrOE(i)%ccc(j)).and.iOut
      End Do
      ! CRL finite dimensions
      iOut = GfGetValue(gf,"FHIT_C("//trim(stmp)//")",arrOE(i)%FHIT_C).and.iOut
      iOut = GfGetValue(gf,"FSHAPE("//trim(stmp)//")",arrOE(i)%FSHAPE).and.iOut
      iOut = GfGetValue(gf,"RWIDX1("//trim(stmp)//")",arrOE(i)%RWIDX1).and.iOut
      iOut = GfGetValue(gf,"RWIDX2("//trim(stmp)//")",arrOE(i)%RWIDX2).and.iOut
      iOut = GfGetValue(gf,"RLEN1("//trim(stmp)//")",arrOE(i)%RLEN1).and.iOut
      iOut = GfGetValue(gf,"RLEN2("//trim(stmp)//")",arrOE(i)%RLEN2).and.iOut

      ! CRL movements (optional)
      iOut2 = GfIsDefined(gf,"F_MOVE("//trim(stmp)//")")
      if (iOut2 .eqv. .true.) then 
         iOut = GfGetValue(gf,"F_MOVE("//trim(stmp)//")",arrOE(i)%F_MOVE)
         print *,"CRL movements defined for oe:  ",stmp

         iOut = GfIsDefined(gf,"X_ROT("//trim(stmp)//")")
         if (iOut .eqv. .true.) then 
            iOut = GfGetValue(gf,"X_ROT("//trim(stmp)//")",arrOE(i)%X_ROT)
            print *,"Found X_ROT: ",arrOE(i)%X_ROT
         end if

         iOut = GfIsDefined(gf,"Y_ROT("//trim(stmp)//")")
         if (iOut .eqv. .true.) then 
            iOut = GfGetValue(gf,"Y_ROT("//trim(stmp)//")",arrOE(i)%Y_ROT)
            print *,"Found Y_ROT: ",arrOE(i)%Y_ROT
         end if

         iOut = GfIsDefined(gf,"Z_ROT("//trim(stmp)//")")
         if (iOut .eqv. .true.) then 
            iOut = GfGetValue(gf,"Z_ROT("//trim(stmp)//")",arrOE(i)%Z_ROT)
            print *,"Found Z_ROT: ",arrOE(i)%Z_ROT
         end if

         iOut = GfIsDefined(gf,"OFFX("//trim(stmp)//")")
         if (iOut .eqv. .true.) then 
            iOut = GfGetValue(gf,"OFFX("//trim(stmp)//")",arrOE(i)%OFFX)
            print *,"Found OFFX: ",arrOE(i)%OFFX
         end if

         iOut = GfIsDefined(gf,"OFFY("//trim(stmp)//")")
         if (iOut .eqv. .true.) then 
            iOut = GfGetValue(gf,"OFFY("//trim(stmp)//")",arrOE(i)%OFFY)
            print *,"Found OFFY: ",arrOE(i)%OFFY
         end if

         iOut = GfIsDefined(gf,"OFFZ("//trim(stmp)//")")
         if (iOut .eqv. .true.) then 
            iOut = GfGetValue(gf,"OFFZ("//trim(stmp)//")",arrOE(i)%OFFZ)
            print *,"Found OFFZ: ",arrOE(i)%OFFZ
         end if

      end if
    End Do
  End Subroutine ReadCRL

!----
!----  Subroutine pretransfocator
!----
!----  Preprocessor for Transfocator
!----
!----  create a crl.01 file with parameters for a compound refractive lens
!---- 
!----  srio@esrf.eu 2013-04-09
!---- 
!

Subroutine pretransfocator

    !---- Variables ----!
    implicit none

    Type(GfType)           :: g1
    integer(kind=ski)      :: ns,i,j,ishape_local,fcyl_local
    integer(kind=ski)      :: changeConvexity,itmp,ftmp
    integer(kind=ski)      :: fwrite_local,fhit_c_local,fshape_local,f_r_ind_local
    integer(kind=ski)      :: f_ext_local
    real(kind=skr)         :: rwidx1_local,rwidx2_local,rlen1_local,rlen2_local
    real(kind=skr)         :: ref0,ref1,att0,att1
    real(kind=skr)         :: pp,qq,pp0,qq0,ddIn,ddV,qqtmp
    character(len=sklen)   :: fileName,file0,file1
    character(len=3)       :: stmp,stmp2
    Logical                :: iOut
    real(kind=skr)         :: cil_ang_local, sin_cil, cos_cil, ref_in, ref_out
    real(kind=skr),dimension(10)  :: ccc, cccTmp
    ! variables for sphere
    real(kind=skr)         :: f, R, delta
    ! variables for ellipsoid/hyperboloid
    real(kind=skr)         :: axmaj, axmin, afoci, ee, eccent, ycen, zcen
    real(kind=skr)         :: a,b,c
    real(kind=skr),dimension(3) :: rncen,rtcen
    ! variables for paraboloid
    real(kind=skr)         :: costhe, sinthe, param, fact
    ! variables for hyperboloid
    real(kind=skr)         :: cc, branch,crl_length,tmp0,tmp1,tmp2

    !transfocator 
    real(kind=skr)         :: crl_diameter, crl_R, crl_R2, crl_interThickness
    real(kind=skr)         :: crl_thickness, crl_fs_before, crl_fs_after
    integer(kind=ski)      :: crl_nlenses, crl_shape, crl_cylindrical
    integer(kind=ski)      :: ierr, iaccumulated, icrl
    character(len=sklen)   :: crl_file
    character(len=1)       :: ctmp="#"
    
    ccc=0.0D0
    file0="none_specified"
    file1="none_specified"
    ref0=1d0
    ref1=1d0
    att0=0d0
    att1=0d0

    !bug fixed srio@esrf.eu 20140627, vonstett@esrf.fr wrote (20130513): 
    !I had edited mytransfocator.dat and re-run precrl and runcrl several
    !times while playing around, but then I got the weird effect that o.e.'s
    !up to 102 were created for a 11-lens CRL, and the result didn't make
    !sense any more.
    !So apparently some values didn't get reset properly, as the created
    !crl.* file starts like this:
    !FMIRR(81) = 10
    !FWRITE(81) = 3
    !FHIT_C(81) = 1 
    iaccumulated=0
    icrl=0

    print *,' '
    print *,'The transfocator stack of CRLs must be defined in "transfocator definition" file: '
    print *,   '0 - Create a "transfocator definition" template file'
    print *,   '1 - Load a "transfocator definition" input file'
    itmp  = irint('?>')
    if (itmp .eq. 0) then 
      crl_file  = rstring('File name to dump template (default: mytransfocator.dat): ')
      if (trim(crl_file).eq."") crl_file = "mytransfocator.dat"
      OPEN (UNIT=25,FILE=crl_file,STATUS='UNKNOWN')
      write(25,'(a)') "# comment lines start with # and can only be at the beginning of the file"
      write(25,'(a)') "#"
      write(25,'(a)') "# This file describes a transfocator"
      write(25,'(a)') "# Each line describes a transfocator block (a CRL) with:"
      write(25,'(a)') "# crl_Nlenses        : number of lenses"
      write(25,'(a)') "# crl_shape          : shape code: 1=Sphere"
      write(25,'(a)') "#                                  2=Ellipsoid"
      write(25,'(a)') "#                                  4=Paraboloid"
      write(25,'(a)') "#                                  5=Plane"
      write(25,'(a)') "#                                  6=Hyperboloid"
      write(25,'(a)') "# crl_cylindrical    : flag for cilyndrical: 0=2D focusing, 1(2)=Focus in Y(X)"
      write(25,'(a)') "# crl_R              : radius (at tip for parabolas) or major axis for ell/hyp"
      write(25,'(a)') "# crl_R2             : minor axis for ell/hyp, not used for sphere/paraboloid"
      write(25,'(a)') "# crl_diameter       : lens physical aperture"
      write(25,'(a)') "# crl_interThickness : thickness between two interfaces (in material)"
      write(25,'(a)') "# crl_Thickness      : total thickness of a single lens"
      write(25,'(a)') "# crl_fs_before      : free space before the first lens"
      write(25,'(a)') "# crl_fs_after       : free space after the last lens"
      write(25,'(a)') "# crl_file           : material file (from prerefl preprocessor)"
      write(25,'(a)') "#"
      write(25,'(a)') "#  Eg: a block of two lenses: "
      write(25,'(a)') "#                     .                  .                                 "
      write(25,'(a)') "#              |      .*********+++++++++.       |          "
      write(25,'(a)') "#              |      . *******  +++++++ .       |            "
      write(25,'(a)') "#              |      .  *****    +++++  .       |            "
      write(25,'(a)') "#              |      .   ***      +++   .       |            "
      write(25,'(a)') "#              |      .   ***      +++   .       |            "
      write(25,'(a)') "#              |      .   ***      +++   .       |            "
      write(25,'(a)') "#              |      .  *****    +++++  .       |            "
      write(25,'(a)') "#              |      . *******  +++++++ .       |           "
      write(25,'(a)') "#              |      .*********+++++++++.       |            "
      write(25,'(a)') "#                     .   <->"
      write(25,'(a)') "#                     . crl_interThickness"
      write(25,'(a)') "#                     .<-------><------->."
      write(25,'(a)') "#                     .          crl_thickness"
      write(25,'(a)') "#              |<---->.                  .<----->|"
      write(25,'(a)') "#       crl_fs_before .                  .crl_fs_after"
      write(25,'(a)') "#"
      write(25,'(a)') "#"
      write(25,'(a)') "# Data for ESRF-ID11 (e.g. Baltser et al. http://dx.doi.org/10.1117/12.893343 )"
      write(25,'(a)') "16  4 0 200e-4  -1.0 1e-1 50e-4 1e-1  3150  0     Be.dat"
      write(25,'(a)') "21  4 0 200e-4  -1.0 2e-1 50e-4 1e-1  0     1000  Al.dat"

      CLOSE (UNIT=25)
      print *,' "transfocator definition" file written to disk: '//trim(crl_file)
      print *,' modify it and re-run "precrl"'
      return
    endif

    crl_file  = rstring('Input "transfocator definition" file s (default: mytransfocator.dat) : ')
    if (trim(crl_file).eq."") crl_file = "mytransfocator.dat"

    fileName = RSTRING('Name of output file (default: crl.01): ')
    if (trim(fileName).eq."") fileName = "crl.01"

    print *,'Verbose-Shadow-Run? '
    print *,'    files to save option (suggested: 3): '
    print *,'      0: all files'
    print *,'      1: mirror file only -- mirr.xx'
    print *,'      2: image file only -- star.xx'
    print *,'      3: none'
    fwrite_local = irint('?> ')
    print *,''



    OPEN (UNIT=25,FILE=crl_file,STATUS='OLD')

    ! count the number of lines in the header
    itmp = 0
    do while (ctmp .eq. "#")
      read(25,'(a1)', iostat=ierr) ctmp
      if (iErr /= 0 ) then
                print *,"Error reading the file: "//trim(crl_file)
                !stop
                return
      end if
      itmp = itmp+1
    end do
    itmp = itmp-1

    rewind(25)
    ! jump the header
    if (itmp .gt. 0) then
        do i=1,itmp
          read(25,'(a1)', iostat=ierr) ctmp
        end do
    end if
    
10000 continue

    !read a line with a transfocator block (a CRL)
    read(25,*, iostat=ierr) crl_nlenses,  crl_shape, crl_cylindrical, &
                            crl_R, crl_R2, crl_diameter, & 
                            crl_interThickness, crl_Thickness, &
                            crl_fs_before, crl_fs_after, crl_file

    if (iErr /= 0 ) then
        !print *,"End of file",iErr
        close(25)
        goto 20000
    end if
     
    icrl = icrl+1
    print *,'>>>> PROCESSING CRL number: ',icrl,' <<<<'
    print *,'     crl_nlenses: ',crl_nlenses
    print *,'     crl_shape: ',crl_shape
    print *,'     crl_cylindrical: ',crl_cylindrical
    print *,'     crl_R: ',crl_R
    print *,'     crl_R2: ',crl_R2
    print *,'     crl_diameter: ',crl_diameter
    print *,'     crl_interThickness: ',crl_interThickness
    print *,'     crl_Thickness: ',crl_Thickness
    print *,'     crl_fs_before: ',crl_fs_before
    print *,'     crl_fs_after: ',crl_fs_after
    print *,'     crl_file: ',trim(crl_file)


    !'1 - Sphere'
    !'2 - Ellipsoid'
    !'Toroidal-3'
    !'4 - Paraboloid'
    !'5 - Plane'
    !'6 Codling slit'
    !'7 - Hyperboloid'
    !'Cone-8??'
    !'Polynomial-9??
    ishape_local = crl_shape

    !Cylindical 
    fcyl_local = crl_cylindrical

    ! Number of interfaces NS (Number of lenses=NS/2) 
    ns = crl_nlenses*2

    ! 2: keyboard in first medium and file (from prerefl) next medium'
    f_r_ind_local = 2

    ! Refraction index of the medium containing source 
    ref0 = 1.0d0
    ! Attenuation coeff [UserLength^-1] of the medium containing source 
    att0 = 0.0d0 
    ! file (from prerefl) for the next medium 
    !file1 = "Be.dat"
    file1 = crl_file


    ! pp0: physical focal source-crl distance
    !pp0 = 3150.0D0
    pp0 = crl_fs_before
    ! qq0: physical focal crl-image distance
    !qq0 = 1021.93D0
    qq0 = crl_fs_after

    !'CRL parameters: '
    !'   1:  external/user defined'
    f_ext_local = 1

      if (ishape_local.eq.1) then
        ! Radius of the sphere [UserLength]
        !R = 200d-4
        R = crl_R
      elseif(ishape_local.eq.4) then
        ! Radius of curvature of the paraboloid at the tip 
        !R = 200d-4
        R = crl_R
      elseif(ishape_local.eq.2.or.ishape_local.eq.7) then
        !AXMAJ = RNUMBER(' Major Axis ? ')
        !AXMIN = RNUMBER(' Minor Axis ? ')
        !AXMAJ = 200d-4
        !AXMIN = 200d-4
        AXMAJ = crl_r
        AXMIN = crl_r2
        R = 0.5*(AXMAJ + AXMIN) ! to avoid R undefined
      end if
    !endif

    !
    ! lens dimension
    !
    !'Lens dimensions finite 
    fhit_c_local = 1
    !   '   full  elliptical  :    2'
    !   fshape_local = IRINT ('Shape: [ 1, 2] ?')
    fshape_local = 2
    rwidx1_local = 0D0 ! initialize
    rlen1_local  = 0D0 ! initialize
    !rwidx2_local = RNUMBER ('External Outline Major axis ( x ) ? ')
    rwidx2_local = 5D-2
    rwidx2_local = crl_diameter/2
    !rlen2_local = RNUMBER ('External Outline Minor axis ( y ) ? ')
    !rlen2_local = 5D-2
    rlen2_local = crl_diameter/2

    ! lens thickness [along optical axis, in material] (=ddIn) 
    ddIn  = crl_interThickness
    ! lens length [along optical axis, in vacuum] (=ddV) 
    ddV  = crl_thickness - crl_interThickness

    


    print *," "
    ! end of input section

    !
    ! set values of g1
    !
    print *,'Please wait...'
DO i=1+iaccumulated,ns+iaccumulated
      Write( stmp, '(i3)' ) i
      stmp = adjustl(stmp)
      ! set variables that are constat for every lens
      itmp=10
      iOut = GfForceSetValue(g1,"FMIRR("//trim(stmp)//")",itmp)
      iOut = GfForceSetValue(g1,"FWRITE("//trim(stmp)//")",fwrite_local)
      ! finite dimensions now
      iOut = GfForceSetValue(g1,"FHIT_C("//trim(stmp)//")",fhit_c_local)
      !if (fhit_c_local.eq.1) then 
      iOut = GfForceSetValue(g1,"FSHAPE("//trim(stmp)//")",fshape_local)
      iOut = GfForceSetValue(g1,"RWIDX1("//trim(stmp)//")",rwidx1_local)
      iOut = GfForceSetValue(g1,"RWIDX2("//trim(stmp)//")",rwidx2_local)
      iOut = GfForceSetValue(g1,"RLEN1("//trim(stmp)//")",rlen1_local)
      iOut = GfForceSetValue(g1,"RLEN2("//trim(stmp)//")",rlen2_local)
      !endif

      ! set variables that are change with lens number
      itmp=2
      IF (mod(i,itmp).EQ.0) THEN
         select case(f_r_ind_local)
            case (1)
              ftmp = 2
            case (2)
              ftmp = 1
            case default
              ftmp = f_r_ind_local
         end select
      ! even surfaces
        iOut = GfForceSetValue(g1,"F_R_IND("//trim(stmp)//")",ftmp)
        iOut = GfForceSetValue(g1,"R_IND_OBJ("//trim(stmp)//")",ref1)
        iOut = GfForceSetValue(g1,"R_IND_IMA("//trim(stmp)//")",ref0)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_OBJ("//trim(stmp)//")",att1)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_IMA("//trim(stmp)//")",att0)
        iOut = GfForceSetValue(g1,"FILE_R_IND_OBJ("//trim(stmp)//")",file1)
        iOut = GfForceSetValue(g1,"FILE_R_IND_IMA("//trim(stmp)//")",file0)
        iOut = GfForceSetValue(g1,"T_SOURCE("//trim(stmp)//")",ddIn*0.5D0)
        iOut = GfForceSetValue(g1,"T_IMAGE("//trim(stmp)//")",ddV*0.5D0)
        ref_in = ref1
        ref_out = ref0
        changeConvexity=1
      ELSE
      ! odd surfaces
        iOut = GfForceSetValue(g1,"F_R_IND("//trim(stmp)//")",f_r_ind_local)
        iOut = GfForceSetValue(g1,"R_IND_OBJ("//trim(stmp)//")",ref0)
        iOut = GfForceSetValue(g1,"R_IND_IMA("//trim(stmp)//")",ref1)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_OBJ("//trim(stmp)//")",att0)
        iOut = GfForceSetValue(g1,"R_ATTENUATION_IMA("//trim(stmp)//")",att1)
        iOut = GfForceSetValue(g1,"FILE_R_IND_OBJ("//trim(stmp)//")",file0)
        iOut = GfForceSetValue(g1,"FILE_R_IND_IMA("//trim(stmp)//")",file1)
        iOut = GfForceSetValue(g1,"T_SOURCE("//trim(stmp)//")",ddV*0.5D0)
        iOut = GfForceSetValue(g1,"T_IMAGE("//trim(stmp)//")",ddIn*0.5D0)
        ref_in = ref0
        ref_out = ref1
        changeConvexity=0 ! first surface is concave, R>0, (as SHADOW's default)
      ENDIF

      select case (ishape_local)

        case (1) !spherical case need only lens semiaperture and lens length
          ccc(1) = 1.0D0
          ccc(2) = 1.0D0
          ccc(3) = 1.0D0
          !fixed minus sign srio@esrf.eu 20130524
          ccc(9) = -2.0*R
        
        case (2) !ellipsoid 
            AFOCI =  SQRT( AXMAJ**2-AXMIN**2 )
            ECCENT = AFOCI/AXMAJ
            ee = ref1/ref0
            IF (ee.GT.1) ee=1.0d0/ee
            if (i.eq.1) then
              print *,'Surface: ',i
              print *,'Rev Ellipsoid a:   ',axmaj
              print *,'Rev Ellipsoid b:   ',axmin
              print *,'Rev Ellipsoid c=sqrt(a^2-b^2): ',afoci
              print *,'Rev Ellipsoid focal discance c^2:   ',afoci**2
              print *,'Rev Ellipsoid excentricity:    ',eccent
              print *,'Rev Ellipsoid excentricity OLD:    ',ee
              print *,' '
            endif
            !C
            !C The center is computed on the basis of the object 
            !C and image positions
            !C
            YCEN  = -AXMAJ
            ZCEN  = 0D0
            !C
            !C Computes now the normal in the mirror center.
            !C
            RNCEN(1)  = .0D0
            RNCEN(2)  = -1D0
            RNCEN(3)  = 0.0D0
            !C
            !C Computes the tangent versor in the mirror center.
            !C
            RTCEN(1)  =  .0D0
            RTCEN(2)  =   RNCEN(3)
            RTCEN(3)  = - RNCEN(2)
            !C Computes now the quadric coefficient with the mirror center
            !C located at (0,0,0) and normal along (0,0,1)
            !C
            if (i.eq.1) then
              print *,'Lens center at: ',0.0,ycen,zcen
              print *,'Lens normal: ',rncen 
              print *,'Lens tangent:',rtcen
              print *,' '
            endif

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

!         3: BEGIN ; Toroidal ?????????????????
        case (4) !paraboloid
            PARAM = R
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

            if (i.eq.1) then
              print *, "Surface       : ", i
              print *, "R             : ", R
              print *, "changeConvexity     : ", changeConvexity
              print *,'Paraboloid p parameter (=R): ',PARAM
              print *,'Lens center at: ',0.0,ycen,zcen
              print *,' '
            endif
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
            AFOCI =  SQRT( AXMAJ**2+AXMIN**2 )
            ECCENT = AFOCI/AXMAJ
            EE = ref0/ref1
            IF (ee.LT.1) ee=1.0D0/ee

            if (i.eq.1) then
              print *,'Rev Hyperboloid a:   ',axmaj
              print *,'Rev Hyperboloid b:   ',axmin
              print *,'Rev Hyperboloid c=sqrt(a^2+b^2): ',afoci
              print *,'Rev Hyperboloid focal distance c^2:   ',afoci**2
              print *,'Rev Ellipsoid excentricity:    ',eccent
              print *,'Rev Ellipsoid excentricity OLD:    ',ee
              print *,' '
            endif
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
            if (i.eq.1) then
              print *,'Hyperboloid a: ', AXMAJ
              print *,'Hyperboloid b: ', AXMIN
              print *,'Hyperboloid c: ', AFOCI
              print *,'Hyperboloid focal discance c^2: ', AFOCI**2
              print *,'Hyperboloid excentricity: ', ECCENT
              print *,'Hyperbola BRANCH: ',branch
              print *,'Lens center at: ',0.0D0,YCEN,ZCEN
              print *,'Lens normal: ',RNCEN
              print *,'Lens tangent: ',RTCEN
              print *,' '
            endif
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

        case default !not yet implemented
            print *,'PRETRANSFOCATOR: Surface shape index not implemented: ' &
                   ,ishape_local
        
      end select
      !
      ! is cylindrical?
      !
      IF (fcyl_local.ge.1) THEN 
        cil_ang_local = 0.0D0
        if (fcyl_local .eq. 2) cil_ang_local=90.0
       ! C
       ! C Set to zero the coeff. involving X for the cylinder case, after
       ! C projecting the surface on the desired plane.
       ! C
        cil_ang_local = TORAD*cil_ang_local
        COS_CIL = COS(cil_ang_local)
        SIN_CIL = SIN(cil_ang_local)
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

! overwrite pp0,qq0
Write( stmp2, '(i2)' ) 1+iaccumulated
stmp2 = adjustl(stmp2)
iOut = GfSetValue(g1,"T_SOURCE("//trim(stmp2)//")", &
                  pp0+0.5*(crl_thickness-crl_interThickness) )

iOut = GfSetValue(g1,"T_IMAGE("//trim(stmp)//")", & 
                  qq0+0.5*(crl_thickness-crl_interThickness) )

iaccumulated = iaccumulated + ns

goto 10000

20000 continue

print *,'Total number of interfaces: ',iaccumulated
!---------------------------
    ! 
    ! calculate TRANSFOCATOR length
    !

    select case (iaccumulated) 
      case(1) 
        crl_length = 0.0
      case(2) 
        iOut = GfGetValue(g1,"T_IMAGE(1)",tmp1) 
        iOut = GfGetValue(g1,"T_SOURCE(2)",tmp2)
        crl_length = tmp1 + tmp2
      case default
        !t_source(1) without the free space is:
        iOut = GfGetValue(g1,"T_IMAGE(2)",tmp0) 

        iOut = GfGetValue(g1,"T_IMAGE(1)",tmp1) 
        iOut = GfGetValue(g1,"T_SOURCE(2)",tmp2)
        crl_length = tmp0 + tmp1 + tmp2


        do i=2,iaccumulated-1 
          write( stmp2, '(i2)' ) i
          stmp2 = adjustl(stmp2)
          iOut = GfGetValue(g1,"T_IMAGE("//trim(stmp2)//")",tmp1) 
          write( stmp2, '(i2)' ) i+1
          stmp2 = adjustl(stmp2)
          iOut = GfGetValue(g1,"T_SOURCE("//trim(stmp2)//")",tmp2)
          crl_length = crl_length + tmp1 + tmp2
        enddo
        !t_image(last) without the free space is:
        write( stmp2, '(i2)' ) iaccumulated-1
        stmp2 = adjustl(stmp2)
        iOut = GfGetValue(g1,"T_SOURCE("//trim(stmp2)//")",tmp0)
        crl_length = crl_length + tmp0
        
    end select 
    print *,'Transfocator length is: ',crl_length
    iOut = GfForceSetValue(g1,"ns",iaccumulated)
    iOut = GfForceSetValue(g1,"crl_length",crl_length)

!
! dump the GfType into a new file
!
    iOut = GfFileWrite(g1,fileName)
    if (iOut) then
        print *,'File written to disk: '//trim(fileName)
    else
        print *,'FAILED TO WRITE FILE: '//trim(fileName)
    end if

    print *,'Use shadow3 command "runcrl" to run shadow'

End Subroutine pretransfocator

  
End Module shadow_crl
