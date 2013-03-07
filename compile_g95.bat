set PATH = C:\g95\bin          
set G95_LIBRARY_PATH=C:\g95\lib
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_globaldefinitions.f90      
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c stringio.f90         
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c gfile.f90            
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_beamio.f90           
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_math.f90               
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_variables.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_kernel.f90    
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_synchrotron.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_preprocessors.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_pre_sync.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_postprocessors.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_version.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow_crl.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -c shadow3.f90
g95 -static -ffree-line-length-huge -O2 -fomit-frame-pointer -o shadow3.exe *.o