/bin/rm *.f90 *.F90 *.o *.mod *.def
# cp /scisoft/data/srio/GIT_REPOSITORIES/EPN-CAMPUS/shadow3/Makefile  .

cp ../def/* .

cp ../fortran/gfile.f90			            gfile.F90			                  
cp ../fortran/shadow_preprocessors.f90         shadow_preprocessors.F90      
cp ../fortran/shadow3.f90			            shadow3.F90			                
cp ../fortran/shadow_pre_sync.f90              shadow_pre_sync.F90           
cp ../fortran/shadow_beamio.f90		        shadow_beamio.F90		           
cp ../fortran/shadow_pre_sync_urgent.f90       shadow_pre_sync_urgent.F90    
cp ../fortran/shadow_bind_f.f90		        shadow_bind_f.F90		           
cp ../fortran/shadow_roughness.f90             shadow_roughness.F90          
cp ../fortran/shadow_crl.f90		            shadow_crl.F90		              
cp ../fortran/shadow_synchrotron.f90           shadow_synchrotron.F90        
cp ../fortran/shadow_globaldefinitions.f90     shadow_globaldefinitions.F90  
cp ../fortran/shadow_variables.f90             shadow_variables.F90          
cp ../fortran/shadow_kernel.f90		        shadow_kernel.F90		           
cp ../fortran/shadow_math.f90		            shadow_math.F90		             
cp ../fortran/stringio.f90                     stringio.F90                  
cp ../fortran/shadow_postprocessors.f90        shadow_postprocessors.F90     

#echo "" > shadow_version.F90
cp ../fortran/shadow_version.f90  .

make -f Makefile-preprocessor preprocess

/bin/rm *.F90 *.o *.mod *.def

#make




