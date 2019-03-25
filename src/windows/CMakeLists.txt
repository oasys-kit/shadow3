cmake_minimum_required (VERSION 3.10)
project(shadow3 Fortran)

#find_package(HDF5 REQUIRED COMPONENTS Fortran Fortran_HL)
set(_COMPILE_Fortran " /fpp")
add_definitions(-D_COMPILE4WIN)
SET (DEF_INCLUDE_DIR "../def") 
SET (LIB_TYPE STATIC)
#SET (LIB_TYPE SHARED)

ADD_LIBRARY(shadow3 ${LIB_TYPE} shadow_globaldefinitions.f90 stringio.f90 gfile.f90 shadow_beamio.f90 shadow_math.f90 shadow_variables.f90 shadow_roughness.f90 shadow_kernel.f90 shadow_synchrotron.f90 shadow_pre_sync.f90 shadow_pre_sync_urgent.f90 shadow_preprocessors.f90 shadow_postprocessors.f90 shadow_version.f90 shadow_crl.f90
)
target_include_directories(shadow3 PRIVATE ${DEF_INCLUDE_DIR})
message(${DEF_INCLUDE_DIR})

add_executable(shadow3_exe shadow3.f90)
target_link_libraries(shadow3_exe PUBLIC "shadow3")
#ADD_LIBRARY(shadow3 ${LIB_TYPE} stringio.f90) 

#add_executable(shadow3 shadow3.f90)
#target_include_directories(myprog PRIVATE ${HDF5_INCLUDE_DIRS})
##target_include_directories(shadow3 PUBLIC "C:/Program Files/HDF_Group/HDF5/1.10.5/include/shared")
#message(${HDF5_INCLUDE_DIRS})
#target_link_libraries(myprog PRIVATE ${HDF5_Fortran_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
