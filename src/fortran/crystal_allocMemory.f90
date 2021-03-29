! X.J. Yu, slsyxj@nus.edu.sg, allocate memory
#define VALIDMEM(s)       if (allocated(s)) deallocate(s)
#define ALLOCAMEM(s,x)      allocate(s(x),stat=iErr)
#define ALLOCAMEM2(s,x1,x2)      allocate(s(x1,x2),stat=iErr)
!
    VALIDMEM(ENERGY)
    ALLOCAMEM(ENERGY,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
        end if
    
    VALIDMEM(FP_A)
    ALLOCAMEM(FP_A,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
        end if
    
    VALIDMEM(FPP_A)
    ALLOCAMEM(FPP_A,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
        end if

    VALIDMEM(FP_B)
    ALLOCAMEM(FP_B,NREFL)
    if (iErr /= 0 ) then
       CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(FPP_B)
    ALLOCAMEM(FPP_B,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(FP_1)
    ALLOCAMEM2(FP_1,N_BATOM,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(FPP_2)
    ALLOCAMEM2(FPP_2,N_BATOM,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(FCOL)
    ALLOCAMEM(FCOL,N_ZCOL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(CC)
    ALLOCAMEM2(CC,N_BATOM,3)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(F0)
    ALLOCAMEM(F0,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(F1)
    ALLOCAMEM(F1,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(F2)
    ALLOCAMEM(F2,NREFL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(GG)
    ALLOCAMEM(GG,N_BATOM)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(GG_BAR)
    ALLOCAMEM(GG_BAR,N_BATOM)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(FF)
    ALLOCAMEM(FF,N_BATOM)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(F_ATNUM)
    ALLOCAMEM(F_ATNUM,N_BATOM)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if

    VALIDMEM(I_ZCOL)
    ALLOCAMEM(I_ZCOL,N_ZCOL)
    if (iErr /= 0 ) then
        CALL LEAVE ('CRYSTAL','Error in allocate memory',iErr)
    end if
    
!--------------------------------------------------------------
