del *.f90 *.o *.mod
copy ..\windows_preparation\*.f90 *

echo  Calling shadow_version.bat to create shadow_version.f90...
call shadow_version.bat %COMPILER% %COMPILERWITHPATH%
echo  DONE Creating shadow_version.f90.


c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_globaldefinitions.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c stringio.f90         
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c gfile.f90            
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_beamio.f90      
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none     -c shadow_math.f90             
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_variables.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_roughness.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_kernel.f90    
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_synchrotron.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_pre_sync.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_pre_sync_urgent.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_preprocessors.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_postprocessors.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_version.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow_crl.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -c shadow3.f90
c:\progra~1\gfortran\bin\gfortran.exe -static -ffree-line-length-none -O2 -o shadow3.exe *.o
