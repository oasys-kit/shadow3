del *.o *.mod *.dll *.exe

gfortran -static -ffree-line-length-none -O2 -c shadow_globaldefinitions.f90
gfortran -static -ffree-line-length-none -O2 -c stringio.f90
gfortran -static -ffree-line-length-none -O2 -c gfile.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_beamio.f90
gfortran -static -ffree-line-length-none     -c shadow_math.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_variables.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_roughness.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_kernel.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_synchrotron.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_pre_sync.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_pre_sync_urgent.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_preprocessors.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_postprocessors.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_version.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_crl.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_bind_f.f90


gfortran -shared -lm -fPIC -o libshadow3.dll *.o

gfortran -static -ffree-line-length-none -O2 -c shadow3.f90
gfortran -static -ffree-line-length-none -O2 -o shadow3.exe shadow3.o libshadow3.dll

gcc -c shadow_bind_c.c
gcc -shared -lm  -fPIC  -o libshadow3c.dll shadow_bind_c.o -L. -lshadow3

copy libshadow3.dll ..\..
copy libshadow3c.dll ..\..
