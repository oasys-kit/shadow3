rem #
rem # dos script to compile shadow3
rem #


rem 
rem 
rem Usage:  compile  compiler compiler_with_path_using_double_backslash
rem
rem Example:  compile  gfortran c:\\MinGW\\bin\\gfortran.exe
rem 
@echo off
setlocal
echo Creating shadow_version.f90...


rem COMPILER is obtained from the first command line parameter if present
if not (%1) == () (set COMPILER=%1) else if (%COMPILER%) == () (set COMPILER="gfortran")
echo  COMPILER is: %COMPILER%


rem COMPILER PATH  is obtained from the second command line parameter if present
if not (%2) == () (set COMPILERWITHPATH=%2) else if (%COMPILERPATH%) == () (set COMPILERWITHPATH=%COMPILER%)
echo  COMPILERWITHPATH is: %COMPILERWITHPATH%

echo  Calling shadow_version.bat to create shadow_version.f90...
call shadow_version.bat %COMPILER% %COMPILERWITHPATH%
echo  DONE Creating shadow_version.f90.

del shadow3.exe *.o *.mod
echo Compiling...
@echo on
call compile_%COMPILER%.bat %COMPILERWITHPATH%
@echo off
echo DONE Compiling.

echo Cleaning...
del *.o *.mod
echo ALL DONE.

