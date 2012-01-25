#
# Makefile for shadow3
#
#
#
# Usage:
#       make  (or make shadow3): builds shadow3 binary 
#       make lib        : bulds libraries libshadow3*
#       make examples   : compiles examples and tools using libshadow3*
#       make python     : creates python bind
#       make idl        : creates idl bind
#       make all        : make shadow3 lib examples python idl
#
#       make clean      : cleans files created when running make
#       make purge      : clean + removes files created by shadow3
#
#
# srio@esrf.eu    05-02-2010  first version
# srio@esrf.eu    17-12-2010  version using preprocessor
#
# HINTS:
#
#   Recommended compiler: gfortran
#
#   in ESRF/NICE: 
#     source /scisoft/ESRF_sw/opteron2/set_environment.tcsh
#     use  FC=gfortran
#
#   in ESRF/kukulcan: 
#     setenv LD_LIBRARY_PATH .
#     use  FC=g95
#

#-------------------------------------------------------------------------------
# customize compiler and flags
FC = gfortran
FFLAGS = -fPIC -ffree-line-length-none
#FC = g95
#FFLAGS = -fPIC -ffree-line-length-huge

MPIFC = mpif90
CC = gcc
#CC = gcc-mp-4.4
CCP = g++
#CCP = g++-mp-4.4
PY = python

#-fopenmp -g
CFLAGS = -fPIC  
#-fopenmp -g

LIBFLAGS = -shared -lm 
#-lpthread
#-------------------------------------------------------------------------------

#do not change this. Meant to be command line argument
MPI=


FMODULES = \
	shadow_version.f90 \
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
	shadow_bind_f.f90 \
	shadow_crl.f90
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
	$(FC) $(FFLAGS) -o gen_source gen_source.o -L. -lshadow3

	$(FC) $(FFLAGS) -c trace.f90
	$(FC) $(FFLAGS) -o trace trace.o -L. -lshadow3

	$(FC) $(FFLAGS) -c trace3.f90
	$(FC) $(FFLAGS) -o trace3 trace3.o -L. -lshadow3

	$(CC) $(CFLAGS) -c trace3_c.c
	$(CC) $(CFLAGS) -o trace3_c trace3_c.o -L. -lshadow3 -lshadow3c

	$(CC) $(CFLAGS) -o example_shadow_format example_shadow_format.c

	$(CCP) $(CFLAGS) -c trace3_cpp.cpp
	$(CCP) $(CFLAGS) -o trace3_cpp trace3_cpp.o -L. -lshadow3 -lshadow3c -lshadow3c++

	$(FC) $(FFLAGS) -c fig3.f90
	$(FC) $(FFLAGS) -o fig3 fig3.o -L. -lshadow3

	$(FC) $(FFLAGS) -c example01_f95.f90 -o example01_f95.o
	$(FC) $(FFLAGS) -o example01_f95 example01_f95.o -L. -lshadow3

	$(CC) -I. $(CFLAGS) -c example01_c.c -o example01_c.o
	$(CC) $(CFLAGS) -o example01_c example01_c.o -L. -lshadow3c

	$(CCP) -I. $(CFLAGS) -c example01_cpp.cpp -o example01_cpp.o
	$(CCP) $(CFLAGS) -o example01_cpp example01_cpp.o -L. -lshadow3c++

ifeq ($(MPI),1)
	$(MPIFC) $(FFLAGS) -c trace3mpi.f90
	$(MPIFC) $(FFLAGS) -o trace3mpi trace3mpi.o -L. -lshadow3 -lmpi_f90
endif

lib: $(OBJFMODULES) shadow_bind_c.o shadow_bind_cpp.o
	$(FC) $(LIBFLAGS) -o libshadow3.so $(OBJFMODULES)
	$(CC) $(LIBFLAGS) -o libshadow3c.so -L. -lshadow3 shadow_bind_c.o
	$(CCP) $(LIBFLAGS) -o libshadow3c++.so -L. -lshadow3 -lshadow3c shadow_bind_cpp.o


idl: shadow_bind_idl.c shadow_bind_idl_loader.c shadow_bind_idl_loader.h idl_export.h shadow_bind_idl.dlm
	$(CC) $(CFLAGS) -c shadow_bind_idl_loader.c
	$(CC) $(CFLAGS) -c shadow_bind_idl.c
	$(CC) $(LIBFLAGS) -o shadow_bind_idl.so -L. -lshadow3c shadow_bind_idl_loader.o shadow_bind_idl.o


python: setup.py 
	$(PY) setup.py build

#cp library to main level
#	/bin/cp build/lib.linux-x86_64-2.6/Shadow.so .
#	/bin/cp build/lib.macosx-10.5-i386-2.6/Shadow.so .
	/bin/cp build/*/Shadow.so .



all: shadow3 lib examples python idl

shadow_version.f90: shadow_version_precpp.F90 
# shadow_version.sh creates shadow_version.h
	./shadow_version.sh $(FC)
	./Makefile_use_precompiler shadow_version

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
	/bin/rm -f version.txt

# binaries
	/bin/rm -f gen_source trace trace3 trace3mpi trace3_c trace3_cpp
	/bin/rm -f shadow3 fig3 example_shadow_format
	/bin/rm -f example01_f95 example01_c example01_cpp
	/bin/rm -f ../bin/*

# files created by the preprocessor
#	/bin/rm -f tmp1.f90 tmp2.f90 tmp3.f90 tmp4.f90
	/bin/rm -f tmp1_shadow_variables.f90 tmp2_shadow_variables.f90 
	/bin/rm -f tmp1_shadow_kernel.f90 tmp2_shadow_kernel.f90 
	/bin/rm -f tmp1_shadow_version.f90 tmp2_shadow_version.f90 
	/bin/rm -f shadow_variables.f90 shadow_kernel.f90 shadow_version.f90
	/bin/rm -f shadow_version.h

# files created by python
	/bin/rm -rf build

purge: clean
#shadow runs
	/bin/rm -f start.* end.* begin.dat star.* mirr.* screen.* \
                   systemfile.* effic.* angle.* optax.* focus focnew*
	/bin/rm -f plotxy* histo1* shadow3.inp pippo* xshwig.*
	/bin/rm -f SRANG SRDISTR SRSPEC epath.nml 
	/bin/rm -f F12LIB.INDEX F12LIB.FULL
#examples runs
	/bin/rm -f testio.00

# customize depending on where you want to install
install:
	/bin/cp shadow3 /scisoft/xop2.3/extensions/shadowvui/shadow3/shadow3
	/bin/cp shadow3 /scisoft/xop2.3/extensions/shadowvui/shadow-2.3.2m-linux/bin/shadow3
#	$(PY) setup.py install
#	/bin/cp libshado*.so /usr/lib/
#	/bin/cp shadow3 /usr/bin/

	


