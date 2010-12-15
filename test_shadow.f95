!----
!----  Program test_shadow
!----
!----  testing the module: shadow
!---- 
!---- 
!---- 
!----  This is a test of some of the routines in shadow.f95 module
!---- 
!---- It is NOT a test of the full shadow nor the main programs
!---- 
!---- 
!---- 
!----  srio@esrf.eu 2009-09-23
!---- 
!
Program test_shadow

    !---- Use Modules ----!
    use shadow

    !---- Variables ----!
    implicit none

    character(len=512)     :: fileName, flag
    integer(kind=4)                :: iErr



    ! 
    ! test rwname
    !
    flag = "R_SOUR"
    fileName = 'start.00'
    print *,">> Calling rwname with ",trim(fileName)," ",trim(flag)
    call rwname(fileName,flag,iErr)
    print *,">> Back from rwname with "//trim(flag)

    flag = "W_SOUR"
    fileName = 'end.00'
    print *,">> Calling rwname with ",trim(fileName)," ",trim(flag)
    call rwname(fileName,flag,iErr)
    print *,">> Back from rwname with "//trim(flag)

    ! flag = "R_OE"
    ! fileName = 'start.01'
    ! print *,">> Calling rwname with ",trim(fileName)," ",trim(flag)
    ! call rwname(fileName,flag,iErr)
    ! print *,">> Back from rwname with "//trim(flag)

    ! flag = "W_OE"
    ! fileName = 'end.01'
    ! print *,">> Calling rwname with ",trim(fileName)," ",trim(flag)
    ! call rwname(fileName,flag,iErr)
    ! print *,">> Back from rwname with "//trim(flag)

End Program test_shadow



