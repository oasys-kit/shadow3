@echo off
setlocal
rem Set COMPILER
rem COMPILER is obtained from the first command line parameter if present
set COMPILER=gfortran
if not (%1) == () (set COMPILER=%1) else if (%COMPILER%) == () (set COMPILER=gfortran)
echo shadow_version.bat: Using compiler is %COMPILER% 

del /F version.txt shadow_version.f90
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ > version.txt
echo                            compilation settings                                 >> version.txt           
echo                          Compiled on %date% at %time% >> version.txt
echo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ >> version.txt
echo ver                                                                             >> version.txt
ver   >> version.txt


echo %COMPILER% --version  >> version.txt
%COMPILER% --version       >> version.txt

echo type compile_%COMPILER%.bat                                                                >> version.txt
type compile_%COMPILER%.bat    >> version.txt

rem using sed (4.07)  from http://www.pement.org/sed/ 
echo shadow_version.bat: Running sed... 
sed.exe -e "s/^/print *,\"/"  version.txt -i                                                                   
sed.exe -e "s/$/\"/" version.txt -i   

echo !---- MODULE:  shadow_version       >  shadow_version.f90 
echo Module shadow_version               >> shadow_version.f90
echo     implicit none                   >> shadow_version.f90 
echo     public :: shadow_version_info   >> shadow_version.f90 
echo Contains                            >> shadow_version.f90 
echo SUBROUTINE shadow_version_info      >> shadow_version.f90 
type version.txt                         >> shadow_version.f90
echo !                                   >> shadow_version.f90 
echo END SUBROUTINE shadow_version_info  >> shadow_version.f90 
echo End Module shadow_version           >> shadow_version.f90 
echo shadow_version.bat: Cleaning... 
del /F version.txt
echo shadow_version.bat: DONE.