rem #
rem # dos script to compile shadow3
rem #
@echo off
setlocal
echo Creating shadow_version.f90...

rem Set COMPILER
rem COMPILER is obtained from the first command line parameter if present
rem set COMPILER=gfortran
rem set COMPILER=g95
if not (%1) == () (set COMPILER=%1) else if (%COMPILER%) == () (set COMPILER=gfortran)


call shadow_version.bat %COMPILER%
echo DONE Creating shadow_version.f90.

del shadow3.exe *.o *.mod
echo Compiling...
@echo on
call compile_%COMPILER%.bat
@echo off
echo DONE Compiling.

echo Cleaning...
del *.o *.mod
echo ALL DONE.

