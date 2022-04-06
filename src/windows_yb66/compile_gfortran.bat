del *.o *.mod *.dll *.exe

REM
REM  compile shadow sources (including fortran binding)
REM
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_version.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_globaldefinitions.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/stringio.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/gfile.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_beamio.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_math.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_variables.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_roughness.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_kernel.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_synchrotron.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_pre_sync.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_pre_sync_urgent.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_preprocessors.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_postprocessors.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_version.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_crl.f90
gfortran -I ../def -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow_bind_f.f90


gfortran -shared -lm -fPIC -o libshadow3.dll *.o

REM  create shadow3.exe - not needed for python
gfortran -static -cpp -D_COMPILE4WIN -ffree-line-length-none -O2 -c ../fortran/shadow3.f90
gfortran -static -ffree-line-length-none -O2 -o shadow3.exe shadow3.o libshadow3.dll



gcc -c -I ../def ../c/shadow_bind_c.c
gcc -shared -lm  -fPIC  -o libshadow3c.dll shadow_bind_c.o -L. -lshadow3

REM
REM  copy created binary and libraries to main dir
REM
copy libshadow3.dll ..\..
copy libshadow3c.dll ..\..
copy shadow3.exe ..\..
