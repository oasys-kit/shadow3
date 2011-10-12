#
# makefile for shadow3
#
# srio@esrf.eu    05-02-2010  first version
# srio@esrf.eu    17-12-2010  version using preprocessor
#
#in ESRF/NICE: 
#   source /scisoft/ESRF_sw/opteron2/set_environment.tcsh
#   use  FC=gfortran
#
#in ESRF/kukulcan: 
#    setenv LD_LIBRARY_PATH .
#    use  FC=g95

#FC = g95
FC = gfortran
MPIFC = mpif90
CC = gcc
#CC = gcc-mp-4.4
CCP = g++
#CCP = g++-mp-4.4
PY = python

FFLAGS = -fPIC 
#-fopenmp -g
CFLAGS = -fPIC  
#-fopenmp -g

LIBFLAGS = -shared -lm 
#-lpthread

#do not change this. Meant to be command line argument
MPI=


FMODULES = \
	shadow_globaldefinitions.f90 \
	stringio.f90 \
	gfile.f90 \
	shadow_beamio.f90 \
	shadow_math.f90 \
	shadow_variables.f90 \
	shadow_kernel.f90 \
	shadow_synchrotron.f90 \
	shadow_pre_sync.f90 \
	shadow_preprocessors.f90 \
	shadow_postprocessors.f90 \
	shadow_bind_f.f90
#	cdf_z.f


OBJFMODULES =  ${FMODULES:.f90=.o}
OBJTESTS   =  ${FTESTS:.f90=.o}


#
# targets
#


shadow3:  $(OBJFMODULES) shadow3.o
	$(FC) $(FFLAGS) -o shadow3 shadow3.o $(OBJFMODULES) 


examples: 
	$(FC) $(FFLAGS) -c gen_source.f90 
	$(FC) $(FFLAGS) -o gen_source gen_source.o -L. -lshadow

	$(FC) $(FFLAGS) -c trace.f90
	$(FC) $(FFLAGS) -o trace trace.o -L. -lshadow

	$(FC) $(FFLAGS) -c trace3.f90
	$(FC) $(FFLAGS) -o trace3 trace3.o -L. -lshadow

	$(CC) $(CFLAGS) -c trace3_c.c
	$(CC) $(CFLAGS) -o trace3_c trace3_c.o -L. -lshadow -lshadowc

	$(CCP) $(CFLAGS) -c trace3_cpp.cpp
	$(CCP) $(CFLAGS) -o trace3_cpp trace3_cpp.o -L. -lshadow -lshadowc -lshadowc++

	$(FC) $(FFLAGS) -c fig3.f90
	$(FC) $(FFLAGS) -o fig3 fig3.o -L. -lshadow

	$(FC) $(FFLAGS) -c example01_f95.f90 -o example01_f95.o
	$(FC) $(FFLAGS) -o example01_f95 example01_f95.o -L. -lshadow

	$(CC) -I. $(CFLAGS) -c example01_c.c -o example01_c.o
	$(CC) $(CFLAGS) -o example01_c example01_c.o -L. -lshadowc

	$(CCP) -I. $(CFLAGS) -c example01_cpp.cpp -o example01_cpp.o
	$(CCP) $(CFLAGS) -o example01_cpp example01_cpp.o -L. -lshadowc++

ifeq ($(MPI),1)
	$(MPIFC) $(FFLAGS) -c trace3mpi.f90
	$(MPIFC) $(FFLAGS) -o trace3mpi trace3mpi.o -L. -lshadow -lmpi_f90
endif

lib: $(OBJFMODULES) shadow_bind_c.o shadow_bind_cpp.o
	$(FC) $(LIBFLAGS) -o libshadow.so $(OBJFMODULES)
	$(CC) $(LIBFLAGS) -o libshadowc.so -L. -lshadow shadow_bind_c.o
	$(CCP) $(LIBFLAGS) -o libshadowc++.so -L. -lshadow -lshadowc shadow_bind_cpp.o

idl: shadow_bind_idl.c shadow_bind_idl_loader.c shadow_bind_idl_loader.h idl_export.h shadow_bind_idl.dlm
	$(CC) $(CFLAGS) -c shadow_bind_idl_loader.c
	$(CC) $(CFLAGS) -c shadow_bind_idl.c
	$(CC) $(LIBFLAGS) -o shadow_bind_idl.so -L. -lshadowc shadow_bind_idl_loader.o shadow_bind_idl.o


python: setup.py 
	$(PY) setup.py build

#cp library to main level
#	/bin/cp build/lib.linux-x86_64-2.6/Shadow.so .
#	/bin/cp build/lib.macosx-10.5-i386-2.6/Shadow.so .
	/bin/cp build/*/Shadow.so .



all: shadow3 lib examples python idl

shadow_variables.f90: shadow_variables_precpp.F90
# The sed commands have been put into a separate script because
# the sed in MacOS do not accept \n
	./Makefile_use_precompiler shadow_variables

shadow_kernel.f90: shadow_kernel_precpp.F90
# The sed commands have been put into a separate script because
# the sed in MacOS do not accept \n
	./Makefile_use_precompiler shadow_kernel


shadow_bind_c.o: shadow_bind_c.c
	$(CC) -I. $(CFLAGS) -c shadow_bind_c.c -o shadow_bind_c.o

shadow_bind_cpp.o: shadow_bind_cpp.cpp
	$(CCP) -I. $(CFLAGS) -c shadow_bind_cpp.cpp -o shadow_bind_cpp.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean: 
#objects and libraries
	/bin/rm -f *.o *.mod *.so
	/bin/rm -f ../lib/*

# binaries
	/bin/rm -f gen_source trace trace3 trace3mpi trace3_c trace3_cpp shadow3 fig3
	/bin/rm -f example01_f95 example01_c example01_cpp
	/bin/rm -f ../bin/*

# files created by the preprocessor
#	/bin/rm -f tmp1.f90 tmp2.f90 tmp3.f90 tmp4.f90
	/bin/rm -f tmp1_shadow_variables.f90 tmp2_shadow_variables.f90 
	/bin/rm -f tmp1_shadow_kernel.f90 tmp2_shadow_kernel.f90 
	/bin/rm -f shadow_variables.f90 shadow_kernel.f90

# files created by python
	/bin/rm -rf build

purge: clean
#shadow runs
	/bin/rm -f start.* end.* begin.dat star.* mirr.* screen.* \
                   systemfile.* effic.* angle.* optax.*
	/bin/rm -f plotxy* histo1* shadow3.inp pippo* xshwig.*

install:
	/bin/cp shadow3 /scisoft/xop2.3/extensions/shadowvui/shadow-2.3.2m-linux/bin/shadow3
	#mv *.o ../obj
	#/bin/cp  shadow3 ../DISTR/
	#/bin/cp  gen_source ../DISTR/
	#/bin/cp  trace3 ../DISTR/
	#/bin/cp  trace ../DISTR/
	#/bin/cp libshado*.so ../lib/
	#/bin/cp build/lib.linux-x86_64-2.6/Shadow.so ../lib
	#/bin/cp  shadow3 ../bin/


