!!
!!    Test integers **attention to compatibility 32/64 bits machines**
!!
!!
!!

!! in kukulcan (32 bits)
!!  selected_int_kind(18):  8
!!  selected_int_kind(9):  4
!! selected_int_kind(4):  2
!!  selected_int_kind(2):  1
!!  selected_real_kind(6,37):  4
!!  selected_real_kind(15,307):  8
!!  kind(1):  4  <<<<<<<<<<<<
!! kind(1.):  4
!! kind(1.0D0):  8
!! kind(tmp):  8
!! kind(itmp):  4
!! kind(ii):  4

!!
!! in coral (64 bits)
!! selected_int_kind(18):  8
!! selected_int_kind(9):  4
!! selected_int_kind(4):  2
!! selected_int_kind(2):  1
!! selected_real_kind(6,37):  4
!! selected_real_kind(15,307):  8
!! kind(1):  8  <<<<<<<<<<<<
!! kind(1.):  4
!! kind(1.0D0):  8
!! kind(tmp):  8
!! kind(itmp):  4
!! kind(ii):  8 <<<<<<<<<<<<<<




PROGRAM test_integers

implicit none 

real(kind=kind(1.0d0))         :: TMP
integer                        :: ITMP
integer                        :: II
integer(kind=4)                :: ITMP4
integer(kind=8)                :: ITMP8

!!  Kind types for 64-, 32-, 16-, and 8-bit signed integers
!!  Kind types for 8-, 4-, 2-, and 1-byte signed integers
print *,'selected_int_kind(18): ',selected_int_kind(18)
print *,'selected_int_kind(9): ',selected_int_kind(9)
print *,'selected_int_kind(4): ',selected_int_kind(4)
print *,'selected_int_kind(2): ',selected_int_kind(2)


print *,'selected_real_kind(6,37): ',selected_real_kind(6,37)
print *,'selected_real_kind(15,307): ',selected_real_kind(15,307)

print *,'kind(1): ',kind(1)
print *,'kind(1_2): ',kind(1_2)
print *,'kind(1_4): ',kind(1_4)
print *,'kind(1_8): ',kind(1_8)
print *,'kind(1.): ',kind(1.)
print *,'kind(1.0D0): ',kind(1.0D0)
print *,'kind(kind(1.0D0)): ',kind(kind(1.0D0))

tmp = 1.0
print *,'kind(tmp=1.0): ',kind(tmp)

!! if undefined, kind(itmp)=4 in 32 bit machines and 8 in 64 bits

print *,'kind(itmp=2): ',kind(itmp)

itmp4 = 2
print *,'kind(itmp4=2): ',kind(itmp4)

itmp8 = 2
print *,'kind(itmp8=2): ',kind(itmp8)

print *,'kind(ii): ',kind(ii)

itmp = 2

END PROGRAM TEST_INTEGERS
