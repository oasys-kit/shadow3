del *.o *.mod *.dll *.exe

REM
REM  compile shadow sources (including fortran binding)
REM
gfortran -Idef -D_COMPILE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_globaldefinitions.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\stringio.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\gfile.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_beamio.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none     -c fortran\shadow_math.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_variables.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_roughness.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_kernel.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_synchrotron.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_pre_sync.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_pre_sync_urgent.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_preprocessors.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_postprocessors.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_version.F90
gfortran -Idef -D_COMPLE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_crl.F90
gfortran -Idef -D_COMPILE4WIN -cpp -static -ffree-line-length-none -O2 -c fortran\shadow_bind_f.F90


gfortran -shared -lm -fPIC -o libshadow3.dll *.o

REM  create shadow3.exe - not needed for python
REM  gfortran -static -ffree-line-length-none -O2 -c shadow3.f90
REM  gfortran -static -ffree-line-length-none -O2 -o shadow3.exe shadow3.o libshadow3.dll

gcc -c -Idef c/shadow_bind_c.c
gcc -shared -lm  -fPIC  -o libshadow3c.dll shadow_bind_c.o -L. -lshadow3

REM
REM  copy created libraries to main dir
REM
copy libshadow3.dll ..
copy libshadow3c.dll ..
