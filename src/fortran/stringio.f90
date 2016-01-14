!----
!---- MODULE:  stringIO
!----
!---- SHADOW utilities for string Input/Output
!---- This module groups the SHADOW routines (F=function, S=subroutine):
!----     ** these are for error io:
!----     S: mssg.F 
!----     S: leave.F 
!----     ** these are for entering variables from prompt
!----     F: iyes.F 
!----     F: rnumber.F 
!----     F: irint.F 
!----     F: rstring.F 
!----     ** these are utils for strings
!----     F: iblank.F 
!----     F: fname.F
!----     S: despace.F
!----     ** these are C-like utils for strings written in fortran
!----        but not used externally. They are kept as private. 
!----     S: fstrtrim
!----     S: fstrtrim_l
!----     S: fstrtrim_r
!----     S: fstrupcase
!----     S: fstrlocase
!----     S: fstrfill
!----     S: fstrchr
!----     ** these are for cleaning terminal window
!----     clscreen.F
!----    
!----
!----
!---- Example of usage: 
!
!

Module stringIO
      !---- Use Modules ----!
      use shadow_globaldefinitions, only: ski, skr, skx, skc, sklen, OS, OS_DS
      !---- Variables ----!
      implicit none

      !---- Everything is private unless explicitly made public ----!
      private 

      !---- List of public functions ----!
 
      !TODO: all routines are public by now.
      !Some routines cam be made private?
      ! private :: blablaF1
      !---- List of public overloaded functions ----!
      !---- List of public functions ----!
      public ::  iyes, rnumber, rstring, iblank, fname, u_case
      public ::  myConcat
      !---- List of public subroutines ----!
      public ::  mssg,leave, irint, despace, clscreen, datapath
      public ::  fstrlocase, fstrupcase


     !---- List of private functions ----!
     ! private :: blablaF2
     !---- List of private subroutines ----!
     private :: fstrtrim, fstrtrim_l, fstrtrim_r, fstrchr
     private :: fstrfill
 
     !---- Definitions ----!

     !---- Interfaces ----!

     Contains
     !
     !---- Public Functions and Subroutines ----!
     !

     ! C+++
     ! C SUBROUTINE MSSG (TEXT1,TEXT2,IFLAG)
     ! C
     ! C PURPOSE  
     ! C     Outputs an error message to the screen and to a file
     ! C
     ! C ARGUEMNTS 
     ! C     TEXT1 : Tipically, the calling routine name
     ! C     TEXT2 : A file name, or a message
     ! C     IFLAG : A flag or an IOSTAT value.
     ! C
     ! C---
    
     SUBROUTINE MSSG (T1, T2, IFLAG)
       CHARACTER *(*) T1, T2
!     	integer(kind=ski) iblank,iflag
       integer(kind=ski) iflag
       WRITE(6,*)'SHADOW-E-Error: '
       WRITE(6,*)'Module     : ', T1
       WRITE(6,*)'Message    : ', T2
       WRITE(6,*)'Error flag : ', IFLAG
       WRITE (33, 100) T1(1:IBLANK(T1))
       WRITE (33, 100) T2(1:IBLANK(T2))
       WRITE (33, 110) IFLAG
100    FORMAT (1X,'>',A)
110    FORMAT (1X,I5)
       RETURN
     END SUBROUTINE MSSG

     !
     !
     !

     ! C+++
     ! C	SUBROUTINE	LEAVE
     ! C
     ! C	PURPOSE		To print out a message, then signal the VAX 
     ! C			condition handler to abort the command procedure.
     ! C---

  SUBROUTINE LEAVE (TEXT1,TEXT2,IFLAG)
    CHARACTER*(*) TEXT1,TEXT2
    integer(kind=ski) SS_ABORT
    integer(kind=ski) iflag
    PARAMETER (SS_ABORT = 44)
    CALL MSSG (TEXT1,TEXT2,IFLAG)
    ! C	STOP		SS_ABORT
    ! C
    ! C HACK/FIXME/TODO
    ! C *update* when 6000 compiler improves to include calls to EXIT.
    ! C
    ! #if rs6000
    ! 	STOP		1
    ! #else
    CALL EXIT (SS_ABORT)
    ! #endif
  END SUBROUTINE LEAVE



    !
    !
    !


    ! C+++
    ! C	FUNCTION	IYES	(prompt)
    ! C
    ! C	PURPOSE		Decodes an answer line to a prompt.
    ! C
    ! C			If IN = Y or y or 1, then IYES = 1
    ! C			else		 	  IYES = 0
    ! C---
  integer(kind=ski) FUNCTION IYES (PROMPT)
    CHARACTER *(*) PROMPT
    CHARACTER *1 IN
    LOGICAL TEST
    IYES = 1
10  WRITE (6,1000,ADVANCE='NO') PROMPT
1000	FORMAT	(1X,A,2x)
    ! #ifdef MICROSOFT
    !      	READ	(5,*,ERR=20) IN
    ! #else
     	READ	(5,1010,ERR=20) IN
    1010	FORMAT (1A1)
    ! #endif
     	TEST = (IN(1:1).EQ.'Y').OR.(IN(1:1).EQ.'y').OR.(IN(1:1).EQ.'1')
     	IYES	= 1
     	IF (TEST) RETURN
     	IYES	= 0
     	RETURN
    20     	WRITE(6,*)'What ?'
     	GO TO 10
     	END FUNCTION IYES


    !
    !
    !

    ! C+++
    ! C	FUNCTION	RNUMBER
    ! C
    ! C	Purpose		Error-proof read-in routine for a real*8 number
    ! C
    ! C
    ! C---
     	!DOUBLE PRECISION FUNCTION	RNUMBER	( PROMPT )
     	REAL(KIND=skr) FUNCTION	RNUMBER	( PROMPT )
        IMPLICIT INTEGER(kind=ski) (I-N)
     	CHARACTER*(*)	PROMPT
     	! REAL*8		VALUE
     	REAL(KIND=skr)  VALUE
    ! C
    ! C The error iteration limit is 10 right now.
    ! C
	ICOUNT	= 0
    10	WRITE 	(6, '(1X,A,2X)',ADVANCE='NO')	PROMPT
    ! #ifdef MICROSOFT
    !      	READ	(5, *, IOSTAT=IRET)	VALUE
    ! #else
     	READ	(5, '(F21.0)', IOSTAT=IRET)	VALUE
    ! #endif
     	IF (IRET.NE.0) THEN
     	  WRITE (6,*)	'What ? [ Program expects real number input ]'
	  ICOUNT	= ICOUNT + 1
	  IF (ICOUNT.GT.10) CALL LEAVE ('RNUMBER : ','Exceed error iteration limit.',IRET)
     	  GO TO 10
     	ELSE
     	  RNUMBER = VALUE
     	  RETURN
     	END IF
    END FUNCTION RNUMBER

    !
    !
    !

    
    ! C+++
    ! C	FUNCTION	IRINT
    ! C
    ! C	Purpose		Error-proof read-in routine for an I*4 number
    ! C
    ! C
    ! C---
     	integer(kind=ski)  FUNCTION IRINT	(PROMPT)
	IMPLICIT INTEGER(kind=ski) (i-n)
     	CHARACTER*(*)	PROMPT
	
    ! C
    ! C The error iteration limit is 10 right now.
    ! C
	ICOUNT	= 0
    10	WRITE 	(6, '(1X,A,2X)',ADVANCE='NO')	PROMPT
    ! #ifdef MICROSOFT
    !      	READ	(5, *, IOSTAT=IRET)	IVAL
    ! #else
     	READ	(5, '(I10.0)', IOSTAT=IRET)	IVAL
    ! #endif
     	IF (IRET.NE.0) THEN
     	  WRITE (6,*) 'What ? [ Program expects integer number input ]'
	  ICOUNT = ICOUNT + 1
	  IF (ICOUNT.GT.10) CALL LEAVE ('IRINT : ','Exceed error iteration limit.',IRET)
     	  GO TO 10
     	ELSE
     	  IRINT = IVAL
     	  RETURN
     	END IF
     END FUNCTION IRINT


    !
    !
    !

    ! 
    ! C+++
    ! C	FUNCTION	RSTRING
    ! C
    ! C	PURPOSE		Reads in an ASCII string
    ! C
    ! C---
     	FUNCTION	RSTRING ( ARG )
        implicit integer(kind=ski) (i-n)
     	CHARACTER(len=sklen) :: RSTRING
        CHARACTER *(*)	ARG
    ! C
    ! C The error iteration limit is 10 right now.
    ! C
	ICOUNT = 0
    1     	WRITE (6,1000,ADVANCE='NO')  ARG
     	READ  (5, 1010, END=20, ERR=10)	RSTRING
     	RETURN
    10	WRITE (6,1000,ADVANCE='NO')  'I/O-%-ERR: What ?? Please try again.'
	ICOUNT	= ICOUNT + 1
	IF (ICOUNT.GT.10) THEN
          ITMP=0
	  CALL 	LEAVE ('RSTRING : ','Exceed error iteration limit.',ITMP)
	ELSE
     	  GO TO 1
	END IF
    20	continue
        !RSTRING (1:2) = '^Z'
        RSTRING = "EXIT"
     	RETURN
    1000	FORMAT (1X,A)
    1010	FORMAT (A)
    END FUNCTION RSTRING


    !
    !
    !


    ! C +++
    ! C 	integer(kind=ski) 	function 	iblank
    ! C
    ! C	purpose		Returns the last non-white spot in the string.
    ! C
    ! C	input		a fortran character string.
    ! C	output		Index of the last non-white char in the string.
    ! C			Empty strings return 1.
    ! C	hacked by	Mumit Khan
    ! C ---
    !!	integer function iblank (str)
	integer(kind=ski) function iblank (str)
	implicit 	none
	character*(*) 	str
	integer(kind=ski) 	ilen, i
	integer(kind=ski)		ASCII_TAB
	parameter	(ASCII_TAB = 9)
	character*1	tabchar
    ! C
    ! C start at the back and work your way back the string until a non-white 
    ! C character is hit or beginning of string (== empty string).
    ! C
	tabchar = char(ASCII_TAB)
	ilen = len(str)
	do 10 i = ilen, 1, -1
	  if (str(i:i).NE.' ' .AND. str(i:i).NE.tabchar) then
	    goto 20
	  endif
     10	continue
    ! C
     20	iblank = i
	return
    end function iblank


    !
    !
    !

    !! copied from crysFML lib (J. R. Carvajal et al.)

    !!----
    !!---- Character Function U_Case(Text) Result (Mtext)
    !!----    character (len=*), intent(in) :: text   !  In -> String:"Input Line"
    !!----    character (len=len(text))     :: mtext  ! Out -> String:"INPUT LINE"
    !!----
    !!----    Conversion to upper case, text is not modified
    !!----
    !!---- Update: February - 2005
    !!
    Function U_Case(Text) Result (Mtext)
       !---- Argument ----!
       character (len=*), intent(in) :: text
       character (len=len(text))     :: mtext

       !---- Local variables ----!
       integer(KIND=ski), parameter :: inc = ICHAR("A") - ICHAR("a")
       integer(KIND=ski)            :: leng, pos

       mtext=text
       leng=len_trim(mtext)
       do pos=1,leng
          if (mtext(pos:pos) >= "a" .and. mtext(pos:pos) <= "z")           &
              mtext(pos:pos) = CHAR ( ICHAR(mtext(pos:pos)) + inc )
       end do

       return
    End Function U_Case

    !
    !
    !

    ! C+++
    ! C   SUBROUTINE   FNAME
    ! C
    ! C   PURPOSE      To append an integer number to a string. This
    ! C         will be used as a file-name to discriminate 
    ! C         between the element of SHADOW
    ! C
    ! C   INPUT      ALPHA    String (file root)
    ! C              INDEX    Numeric value for the extension index
    ! C              LENGTH   Numeric value for the number of digits in the
    ! C                       extension. 
    ! C                       if LENGTH=0, the program calculates automatically
    ! C                       the number of characters in the extention (allways
    ! C                       even, e.g., root.02, root.99, root.0101, root.9999 )
    ! C
    ! C   OUTPUT     NAME     (e.g.,  root.0101)
    ! C
    ! C---
SUBROUTINE FNAME (NAME1, ALPHA, INDEX1, LENGTH)

   implicit none
   character(len=*), intent(in)  :: alpha
   integer(kind=ski), intent(in) :: INDEX1,length
   character(len=*), intent(out) :: NAME1

   integer(kind=ski)  :: iword,i,itot,iformat,length2,itmp
   character(len=80)  :: TEMP,MYFORMAT,SLENGTH

   ! empty strings, just in case...
   DO I=1,LEN(NAME1)
          NAME1 (I:I) = ' '
   END DO
   DO I=1,LEN(SLENGTH)
          SLENGTH (I:I) = ' '
   END DO
   DO I=1,LEN(MYFORMAT)
          MYFORMAT (I:I) = ' '
   END DO

   LENGTH2 = LENGTH
   IF (LENGTH2.EQ.0) THEN
     length2 = 1+int(log10(real(index1)))
     IF (index1.LE.0) length2=1 
     itmp=2
     IF (MOD(length2,itmp).NE.0) length2=length2+1
   END IF

   CALL   DESPACE (ALPHA, TEMP, IWORD)
   NAME1 (:IWORD) = TEMP (:IWORD)
   IWORD   = IWORD + 1
   NAME1 (IWORD:IWORD) = '.'
   ITOT   =  IWORD + LENGTH2

   write(slength,fmt=*) length2
   !print *,'>>>>>>> length: '//trim(adjustl(slength))//'<<<<'
   MYFORMAT='(I'//trim(adjustl(slength))//'.'//trim(adjustl(slength))//')'
   !print *,'>>>>>>> format: '//trim(myformat)//'<<<<'
   !print *,'>>>>>>> iword+1: ',iword+1
   !print *,'>>>>>>> itot: ',itot
   !print *,'>>>>>>> index1: ',index1
   WRITE (NAME1(IWORD+1:ITOT),MYFORMAT,ERR=100) INDEX1
   RETURN
100   NAME1   =   'NAMERR'
   RETURN
END SUBROUTINE FNAME


    !
    !
    !


    ! C+++
    ! C	subroutine	despace
    ! C
    ! C	purpose		to remove leading and trailing spaces from a string
    ! C
    ! C	input		inp(*) the string to be despaced
    ! C
    ! C	out		out(*) the string with all l/t spaces removed
    ! C
    ! C			klen   the length of the new string. 0 if empty
    ! C
    ! C---
     	subroutine	despace	( inp, out, klen)
	implicit none
     	!character *(*)	inp, out
     	character(len=*)  ::  inp, out
        integer(kind=ski)   ::  klen, istart, iend
    ! C
    ! C find the leading and trailing indices first. istart and iend are both 0
    ! C for blank input string.
    ! C
	call fstrtrim (inp, istart, iend)
    ! C
    ! C return klen = 0 and an emtpy string when the input was empty as well.
    ! C
	if (istart.eq.0 .or. iend.eq.0) then
	  out = ' '
	  klen = 0
	  return
	endif

    ! C
    ! C FIXME/CHECK: should check for enough space in "out" or not to hold new
    ! C string.
    ! C
	klen = iend - istart + 1
	out(1:klen) = inp(istart:iend)
     	return
     	end subroutine despace

    !
    !
    !

    ! C+++
    ! C	SUBROUTINE	CLSCREEN
    ! C
    ! C	PURPOSE		To clear the display using the appropriate
    ! C			escape charcters instead of system dependent
    ! C			calls.
    ! C
    ! C---
     	SUBROUTINE	CLSCREEN
    ! #if defined(vms)
    !         CALL LIB$ERASE_PAGE(1,1)
    ! #else
    	CHARACTER	*8	CLEAR
	CLEAR(1:1) 	= char(27)
	CLEAR(2:4)	= '[;H'
	CLEAR(5:5)	= char(27)
	CLEAR(6:8)	= '[2J'
     	WRITE (6,*)	CLEAR
    ! #endif
     	RETURN
    END SUBROUTINE CLSCREEN

    !
    !
    !


    !
    !---- Private Subroutines ----!
    !

    !
    !
    !


    ! C
    ! C Currently implemented routines:
    ! C
    ! C  fstrtrim, fstrtrim_l, fstrtrim_r: White space trimmers
    ! C  fstrchr: C strchr like routine
    ! C  fstrupcase: Upcase a string
    ! C  fstrlocase: Upcase a string
    ! C  fstrfill: Fills a string with whatever character
    ! C  

	subroutine fstrtrim (string, index1, index2)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C	subroutine fstrtrim (string, index1, index2)
    ! C
    ! C  Returns the length of the string less any leading & trailing blanks
    ! C
    ! C	string		- input/string
    ! C       index1, index2  - first/last non-white character
    ! C                         -1 if error
    ! C			  if string is blank, returns 0 for both
    ! C
    ! C--
	character*(*) string
    ! C
	call fstrtrim_l(string, index1)
	call fstrtrim_r(string, index2)
	return
    end subroutine fstrtrim

    !
    !
    !

    ! C
    ! C
    ! C
	subroutine fstrtrim_l (string, index)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C	subroutine fstrtrim_l (string, index)
    ! C
    ! C  Returns the length of the string less any leading blanks
    ! C
    ! C	string		- input/string
    ! C       index           - index of first non-white character
    ! C                         -1 if error
    ! C			  if string is blank, returns 0
    ! C
    ! C--
	character*(*) string
	character*(1) tabchar
    ! C
	tabchar = char(ASCII_TAB)
    ! C
	l = len(string)
    ! C
	if (l .eq. 0) then
	    i = 0
	    goto 99
	endif
    ! C
	istart = 1
	iend = l
	incr = 1
    ! C
	do 10 i = istart, iend, incr
	    if (string(i:i) .ne. ' ' .and. string(i:i) .ne. tabchar) goto 99
     10     continue
    ! C
    ! C  string is either blank or no leading space
    ! C
	i = 0
    ! C
    ! C  return string length
    ! C
     99	continue
	index = i
	return
    end subroutine fstrtrim_l

    !
    !
    !



	subroutine fstrtrim_r (string, index)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C	subroutine fstrtrim_r (string, index)
    ! C
    ! C  Returns the length of the string less any trailing blanks
    ! C
    ! C	string		- input/string
    ! C       index           - index of last non-white character
    ! C                         -1 if error
    ! C			  if string is blank, returns 0
    ! C
    ! C--
	character*(*) string
	character*(1) tabchar
    ! C
	tabchar = char(ASCII_TAB)
    ! C
	l = len(string)
    ! C
	if (l .eq. 0) then
	    i = 0
	    goto 99
	endif
    ! C
	istart = l
	iend = 1
	incr = -1
    ! C
	do 10 i = istart, iend, incr
	    if (string(i:i) .ne. ' ' .and. string(i:i) .ne. tabchar) goto 99
     10     continue
    ! C
    ! C  string is either blank or no leading space
    ! C
	i = 0
    ! C
    ! C  return string length
    ! C
     99	continue
	index = i
	return
    end subroutine fstrtrim_r

    !
    !
    !

	subroutine fstrupcase (string)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C	subroutine fstrupcase (string)
    ! C
    ! C  Convert string to upper case
    ! C  Warning: Blows up if passed string in read-only memory (literal)
    ! C
    ! C	string		- transput/string
    ! C			  string is converted to uppercase
    ! C--
	character*(*) string
    ! C
	do 101 i = 1, len(string)
	    ival = ichar(string(i:i))
	    if (ival .ge. ichar('a') .and. ival .le. ichar('z')) then
		ival = ival - (ichar('a')-ichar('A'))
		string(i:i) = char(ival)
	    endif
     101	continue
    ! C
	return
    end subroutine fstrupcase

    !
    !
    !


	subroutine fstrlocase (string)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C	subroutine fstrlocase (string)
    ! C
    ! C  Convert string to lower case
    ! C  Warning: Blows up if passed string in read-only memory (literal)
    ! C
    ! C	string		- transput/string
    ! C			  string is converted to lowercase
    ! C--
	character*(*) string
    ! C
	do 101 i = 1, len(string)
	    ival = ichar(string(i:i))
	    if (ival .ge. ichar('A') .and. ival .le. ichar('Z')) then
		ival = ival + (ichar('a')-ichar('A'))
		string(i:i) = char(ival)
	    endif
     101	continue
    ! C
	return
    end subroutine fstrlocase

    !
    !
    !
	subroutine fstrfill (string, what, iflag)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C fstrfill -- fill a string with the given character
    ! C
    ! C inputs:
    ! C    string -- a character string 
    ! C    what   -- the character to search for (character(1))
    ! C
    ! C outputs:
    ! C    iflag: 0 AOK
    ! C	   -1 if any input is misbehaved!
    ! C
    ! C--
	character*(*) string, what
    ! C
	if (len(what) .ne. 1) then
	    iflag = -1
	    goto 99
	endif
    ! C
	do 10 i = 1, len(string)
	    string(i:i) = what
 10	continue
	iflag = 0
	goto 99
    ! C
     99	continue
 	return
    end subroutine fstrfill

    !
    !
    !


    ! C
    ! C
    ! C
	subroutine fstrchr (string, what, iflag)
	implicit integer(kind=ski) (a-z)
    ! C++
    ! C
    ! C
    ! C fstrchr -- a mixture of C version of strchr
    ! C
    ! C inputs:
    ! C    string -- a character string 
    ! C    what   -- the character to search for (character(1))
    ! C
    ! C outputs:
    ! C    iflag: 0 error, ie., not found
    ! C	   -1 if any input is misbehaved!
    ! C	    1 ... len(string) position of the "what" string
    ! C
    ! C--
	character *(*) string, what
    ! C
	len1 = len(string)
	len2 = len(what)
	if (len2 .ne. 1) then
	    iflag = -1
	    goto 99
	endif
    ! C
	do 1 i = 1, len1
 	    if (string(i:i) .eq. what(1:1)) goto 3
     1	continue
 	if (string(i:i) .ne. what(1:1)) then
	    iflag = 0
	    goto 99
	endif
    ! C
 3	continue
 	iflag = i
	goto 99
    ! C
     99	continue
 	return
    end subroutine fstrchr
    !
    !
    !

!C +++
!C
!C datapath.F: get pathname of a SHADOW data file
!C
!C Author: Mumit Khan <khan@xraylith.wisc.edu>
!C Copyright(c) 1996 Mumit Khan
!c             completely rewritten by srio@esrf.eu   2010
!c             file must exists, otherwise exits with error
!C
!   The file is searched in the following directory path (in order): 
!      1) .                    !current directory
!      2) the directory where shadow3 binary sits
!      3) the folder "data" under the directory where shadow3 binary sits
!      4) $SHADOW3_HOME        ! env variable for user shadow3 installation
!      5) $SHADOW3_HOME/data   
!      6) $XOP_HOME/extensions/shadowvui/shadow3/  
!                           !standard shadow3 for xop/shadowvui
!      7) $XOP_HOME/extensions/shadowvui/shadow3/data
!      8) $SHADOW_DATA_FILE    ! data folder for shadow2
!      9) $SHADOW_ROOT/data    ! standard location for data folder for shadow2
!C 
!C ---
!
subroutine datapath (file, path, iflag)

!c++
!c
!c datapath -- gets a full pathname for a SHADOW data file
!c
!c input:
!c    file:   the file name to find
!c    iflag:  0 don't check for existence
!c	     1 do check for existence
!c 
!c returns: 
!c    path:   the returned full path
!c    iflag:  0 if ok
!c            1 does not exist
!c
!c--

        implicit none

        character(len=*),intent(in)       ::  file
        integer(kind=ski),   intent(in out) ::  iFlag
        character(len=1024),intent(out)    ::  path
        character(len=1024)                ::  dataDir
        ! ATTENTION: this is the DEFAULT integer, thus
        !            platform dependent!!!!
        !integer(kind=ski)                   ::  nStr
        integer                              ::  nStr

        !srio danger todo : OS-dependent
        !character(len=1)                  :: path_sep='/'
        logical                           :: lExists

        iFlag=0  !OK
        !
        ! 1) if file exists in the current directry, use it
        !
        INQUIRE (file = file, exist = lExists)
        IF (lExists) THEN
           path = file
           RETURN
        END IF

        !
        ! 2) file in the directory where the "shadow3" binary is sitting
        !
        call get_command_argument(0,dataDir)
        nStr = 7 ! number of characters in shadow3
        dataDir = dataDir(1:len(trim(dataDir))-nStr)
        path = TRIM(datadir)//OS_DS//TRIM(file)
        INQUIRE (file = path, exist = lExists)
        IF (lExists) RETURN

        !
        ! 3) if file exists in the directory "data" under where the "shadow3" 
        ! binary is sitting
        !
        call get_command_argument(0,dataDir)
        nStr = 7 ! number of characters in shadow3
        dataDir = dataDir(1:len(trim(dataDir))-nStr)
        path = TRIM(datadir)//OS_DS//'data'//OS_DS//TRIM(file)
        INQUIRE (file = path, exist = lExists)
        IF (lExists) RETURN


        ! 4) checks if file is in $SHADOW3_HOME
        CALL GET_ENVIRONMENT_VARIABLE ('SHADOW3_HOME', datadir, nStr)
        IF (nStr .gt. 0) THEN
          path = TRIM(datadir)//OS_DS//TRIM(file)
          INQUIRE (file = path, exist = lExists)
          IF (lExists) RETURN
        END IF
        
        ! 5) checks if file is in $SHADOW3_HOME/data
        CALL GET_ENVIRONMENT_VARIABLE ('SHADOW3_HOME', datadir, nStr)
        IF (nStr .gt. 0) THEN
          path = TRIM(datadir)//OS_DS//'data'//OS_DS//TRIM(file)
          INQUIRE (file = path, exist = lExists)
          IF (lExists) RETURN
        END IF
        
        ! 6) checks if file is in $XOP_HOME/extensions/shadowvui/shadow3
        CALL GET_ENVIRONMENT_VARIABLE ('XOP_HOME', datadir, nStr)
        IF (nStr .gt. 0) THEN
          path = TRIM(datadir)//OS_DS//'extensions'//OS_DS//'shadowvui'//OS_DS//'shadow3'//OS_DS//TRIM(file)
          INQUIRE (file = path, exist = lExists)
          IF (lExists) RETURN
        END IF

        ! 7) checks if file is in $XOP_HOME/extensions/shadowvui/shadow3/data
        CALL GET_ENVIRONMENT_VARIABLE ('XOP_HOME', datadir, nStr)
        IF (nStr .gt. 0) THEN
          path = TRIM(datadir)//OS_DS//'extensions'//OS_DS//'shadowvui'//OS_DS//'shadow3'//OS_DS//'data'//OS_DS//TRIM(file)
          INQUIRE (file = path, exist = lExists)
          IF (lExists) RETURN
        END IF

        ! 8) checks if file is in $SHADOW_DATA_DIR
        CALL GET_ENVIRONMENT_VARIABLE ('SHADOW_DATA_DIR', datadir, nStr)
        IF (nStr .gt. 0) THEN
          path = TRIM(datadir)//OS_DS//TRIM(file)
          INQUIRE (file = path, exist = lExists)
          IF (lExists) RETURN
        END IF
        
        ! 9) checks if file is in $SHADOW_ROOT/data
        CALL GET_ENVIRONMENT_VARIABLE ('SHADOW_ROOT', datadir, nStr)
        IF (nStr .gt. 0) THEN
          path = TRIM(datadir)//OS_DS//'data'//OS_DS//TRIM(file)
          INQUIRE (file = path, exist = lExists)
          IF (lExists) RETURN
        END IF

        ! file not found
        iFlag=1
!        print *,"Searched in: . $SHADOW_DATA_DIR and $SHADOW_ROOT/data"
!        CALL Leave("DATAPATH","File not found: "//TRIM(file),iflag)

End Subroutine datapath

!
!
!

  character(kind=skc, len=1024) function myConcat(str1, str2)
    character(kind=skc,len=*), intent(in) :: str1
    character(kind=skc,len=*), intent(in) :: str2

    myConcat = str1//"("//trim(adjustl(str2))//")"

  end function myConcat






End Module stringio

