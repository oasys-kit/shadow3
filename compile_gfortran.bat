gfortran -static -ffree-line-length-none -O2 -c shadow_globaldefinitions.f90
gfortran -static -ffree-line-length-none -O2 -c stringio.f90         
gfortran -static -ffree-line-length-none -O2 -c gfile.f90            
gfortran -static -ffree-line-length-none -O2 -c shadow_beamio.f90           
gfortran -static -ffree-line-length-none -O2 -c shadow_math.f90             
gfortran -static -ffree-line-length-none -O2 -c shadow_variables.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_kernel.f90    
gfortran -static -ffree-line-length-none -O2 -c shadow_synchrotron.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_preprocessors.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_pre_sync.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_postprocessors.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_version.f90
gfortran -static -ffree-line-length-none -O2 -c shadow_crl.f90
gfortran -static -ffree-line-length-none -O2 -c shadow3.f90
gfortran -static -ffree-line-length-none -O2 -o shadow3.exe *.o