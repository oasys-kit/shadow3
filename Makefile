#
# Makefile for shadow3
#
#
#
# Usage:
#       make  (or make shadow3): builds shadow3 binary 
#       make lib        : builds shared libraries libshadow3*.so
#       make libstatic  : builds static library libshadow3.a
#       make examples   : compiles examples and tools using libraries:
#                         examplesFortran: fortran examples statically linked
#                         examplesMore: C and C++ examples dymamically linked
#       make python     : creates python bind (uses shared libraries)
#       make idl        : creates idl bind
#       make preprocess : creates *.f90 files (to be exported to other platforms)
#       make all        : make shadow3 examples python idl
#
#       make clean      : cleans files created when running make
#       make purge      : clean + removes files created by shadow3
#
#       make install    : to be adapted to user needs
#
# srio@esrf.eu    05-02-2010  first version
# srio@esrf.eu    17-12-2010  version using preprocessor
# srio@esrf.eu    03-01-2013  minimize the use of scripts
#
# HINTS:
#
#
#   customize next section (only) 
#
#
#   Recommended compiler: gfortran
#
#   in ESRF/NICE: 
#     source /scisoft/ESRF_sw/opteron2/set_environment.tcsh
#     use  FC=gfortran
#
#


##-------------------------------------------------------------------------------
##                   THIS SECTION CAN BE CUSTOMIZED 
##-------------------------------------------------------------------------------

#for 32 bits compilation, set this
#(in Ubuntu needs: sudo apt-get install gfortran-multilib )
#32BITS=-m32

#setings for linux
SO=.so
EXE=
COMPILEOPT=-D_COMPILE4NIX

#settings for mac
#SO=.dylib
#EXE=
#COMPILEOPT=-D_COMPILE4MAX

#settings for windows
#SO=.dll
#EXE=.exe
#COMPILEOPT=-D_COMPILE4WIN


#
# customize compiler and flags
#
FC = gfortran
STATIC = -static
#STATIC = -static -static-libgfortran -static-libgcc
FFLAGS = -cpp -fPIC -ffree-line-length-none $(32BITS) $(STATIC) -O2 -fomit-frame-pointer $(COMPILEOPT)
LINKFLAGS = $(32BITS) $(STATIC)

# NOTE:
# For Ubuntu using g95, I must define
# export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu
# otherwise I get an error:  ld: cannot find crti.o: No such file or directory
#FC = g95
#STATIC=
#FFLAGS = -cpp -fPIC -ffree-line-length-huge $(STATIC) -O2 -fomit-frame-pointer $(COMPILEOPT)
#LINKFLAGS = $(32BITS) $(STATIC)

#FC = x86_64-w64-mingw32-gfortran
#STATIC=-static -static-libgfortran -static-libgcc
#FFLAGS = -cpp -fPIC -ffree-line-length-none $(32BITS) $(STATIC) -O2 -fomit-frame-pointer $(COMPILEOPT)

#FC = i686-w64-mingw32-gfortran
#STATIC=-static -static-libgfortran -static-libgcc
#FFLAGS = -cpp -fPIC -ffree-line-length-none $(32BITS) $(STATIC) -O2 -fomit-frame-pointer $(COMPILEOPT)


CC = gcc
#CC = x86_64-w64-mingw32-gcc
#CC = i686-w64-mingw32-gcc

CXX = g++
#CXX = x86_64-w64-mingw32-g++
#CXX = i686-w64-mingw32-g++

PY = python
#PY = python3

#-fopenmp -g
CFLAGS = -fPIC $(32BITS)
#-fopenmp -g

LIBFLAGS = -shared -lm 
#-lpthread

CPP = cpp -traditional

#-- MPI stuff

#do not change this. Meant to be command line argument
MPI=
MPIFC = mpif90
#overwrite compilers
#CC = gcc-mp-4.4
#CXX = g++-mp-4.4

##-------------------------------------------------------------------------------
##                   END OF CUSTOMIZABLE SECTION
##
## (you may also want to customize the install target, if you want to use it)
##-------------------------------------------------------------------------------


#
# LIST OF SOURCE FILES
#
# note that the order is important...
#
# note that all fortran sources have now .F90 extension, and all are passed
#      by the preprocessor, even if only a few ones have preprocessor 
#      instructions, namely: 
#                            shadow_version.F90  
#                            shadow_globaldefinitions.F90  
#                            shadow_variables.F90  
#                            shadow_kernel.F90  
#

FMODULES = \
	shadow_version.F90 \
	shadow_globaldefinitions.F90 \
	stringio.F90 \
	gfile.F90 \
	shadow_beamio.F90 \
	shadow_math.F90 \
	shadow_variables.F90 \
	shadow_roughness.F90 \
	shadow_kernel.F90 \
	shadow_synchrotron.F90 \
	shadow_pre_sync.F90 \
	shadow_pre_sync_urgent.F90 \
	shadow_preprocessors.F90 \
	shadow_postprocessors.F90 \
	shadow_bind_f.F90 \
	shadow_crl.F90


OBJFMODULES = ${FMODULES:.F90=.o}
FMODULESPRE = ${FMODULES:.F90=.f90}
OBJTESTS    = ${FTESTS:.F90=.o}


#
# targets
#


shadow3: $(OBJFMODULES) shadow3.o  
	$(FC) $(LINKFLAGS) -o shadow3$(EXE) shadow3.o $(OBJFMODULES) 

#create .f90 files (see rule) and clean (remove lines starting with #) them using sed
preprocess: $(FMODULESPRE) shadow3.f90
	for myfile in $(FMODULESPRE); do \
		echo "Removing lines starting with # in: " $$myfile ; \
		sed -i '/^#/d' $$myfile ; \
	done
	echo "Removing lines starting with # in: shadow3.f90"
	sed -i '/^#/d' shadow3.f90 

examples: examplesFortran  examplesMore

#examples in Fortran, linked statically
examplesFortran: libstatic gen_source.o trace.o trace3.o example01_f95.o example02_f95.o example_standalone_mirror.o
	$(FC) $(LINKFLAGS) -o gen_source gen_source.o -L. -lshadow3
	$(FC) $(LINKFLAGS) -o trace trace.o -L. -lshadow3
	$(FC) $(LINKFLAGS) -o trace3 trace3.o -L. -lshadow3
	$(FC) $(LINKFLAGS) -o example01_f95 example01_f95.o -L. -lshadow3
	$(FC) $(LINKFLAGS) -o example02_f95 example02_f95.o -L. -lshadow3
	$(FC) $(LINKFLAGS) -o example_standalone_mirror example_standalone_mirror.o -L. -lshadow3

#examples in C and C++, linked dynamically
#TODO: link statically
examplesMore: lib  libcpp
	$(CC) $(CFLAGS) -c trace3_c.c
	$(CC) $(CFLAGS) -o trace3_c trace3_c.o -L. -lshadow3c -lshadow3

	$(CC) $(CFLAGS) -c example_shadow_format.c
	$(CC) $(CFLAGS) -o example_shadow_format example_shadow_format.o

	$(CC) -I. $(CFLAGS) -c example01_c.c 
	$(CC) $(CFLAGS) -o example01_c example01_c.o -L. -lshadow3c -lshadow3

	$(CXX) -I. $(CFLAGS) -c trace3_cpp.cpp
	$(CXX) $(CFLAGS) -o trace3_cpp trace3_cpp.o -L. -lshadow3c++ -lshadow3c -lshadow3
	$(CXX) -I. $(CFLAGS) -c example01_cpp.cpp -o example01_cpp.o
	$(CXX) $(CFLAGS) -o example01_cpp example01_cpp.o -L. -lshadow3c++ -lshadow3c -lshadow3

ifeq ($(MPI),1)
	$(MPIFC) $(FFLAGS) -c trace3mpi.F90
	$(MPIFC) $(FFLAGS) -o trace3mpi trace3mpi.o -L. -lshadow3 -lmpi_F90
endif

lib: $(OBJFMODULES) shadow_bind_c.o 
	$(FC) $(LIBFLAGS) $(CFLAGS) -o libshadow3$(SO) $(OBJFMODULES)
	$(CC) $(LIBFLAGS) $(CFLAGS) -o libshadow3c$(SO) shadow_bind_c.o -L. -lshadow3 #$(OBJFMODULES)


libcpp: $(OBJFMODULES) shadow_bind_c.o shadow_bind_cpp.o
	$(CXX) $(LIBFLAGS) $(CFLAGS) -o libshadow3c++$(SO) shadow_bind_c.o shadow_bind_cpp.cpp -L. -lshadow3 -lshadow3c #$(OBJFMODULES)

libstatic: $(OBJFMODULES) shadow_bind_c.o shadow_bind_cpp.o
	ar cr libshadow3.a $(OBJFMODULES)
	#ar cr libshadow3c.a shadow_bind_c.o $(OBJFMODULES)
	#ar cr libshadow3c++.a shadow_bind_c.o shadow_bind_cpp.o $(OBJFMODULES)

idl: lib shadow_bind_idl.c shadow_bind_idl_loader.c shadow_bind_idl_loader.h idl_export.h shadow_bind_idl.dlm
	$(CC) $(CFLAGS) -c shadow_bind_idl_loader.c
	$(CC) $(CFLAGS) -c shadow_bind_idl.c
	$(CC) $(LIBFLAGS) -o shadow_bind_idl$(SO) -L. -lshadow3c shadow_bind_idl_loader.o shadow_bind_idl.o


python: lib setup.py 
	$(PY) setup.py build

all: shadow3 examples python idl

# 
# dependencies and rules
#

shadow_version.F90: shadow_version.h 

shadow_version.h:
# shadow_version.sh creates shadow_version.h including compilation info for current system
	./shadow_version.sh $(FC)

%.f90: %.F90
	$(CPP) $(COMPILEOPT) $< -o $@

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

#
# cleaning 
#
clean:  
	/bin/rm -f *.o *.mod *$(SO) *.a *.dylib
	/bin/rm -f version.txt
	/bin/rm -f gen_source trace trace3 trace3mpi trace3_c trace3_cpp
	/bin/rm -f shadow3$(EXE) example_shadow_format
	/bin/rm -f example01_f95 example01_c example01_cpp example02_f95
	/bin/rm -f example_standalone_mirror
	/bin/rm -f shadow_version.h
	/bin/rm -rf build *.pyc
	/bin/rm -f *.f90 

#shadow runs
#examples runs
purge: clean
	/bin/rm -f start.* end.* begin.dat star.* mirr.* screen.* \
                   systemfile.* effic.* angle.* optax.* focus focnew*
	/bin/rm -f plotxy* histo1* shadow3.inp xshwig.* 
	/bin/rm -f SRANG SRDISTR SRSPEC epath.nml 
	/bin/rm -f F12LIB.INDEX F12LIB.FULL 
	/bin/rm -f testio.00 crl.01 final.01
# customize depending on where you want to install
install:
	install libshado*$(SO) /usr/lib/
	$(PY) setup.py install
	install shadow3 /usr/bin/
#	/bin/cp shadow3 /opt/scisoft/xop2.3/extensions/shadowvui/shadow3/shadow3
#	/bin/cp shadow3 /scisoft/xop2.3/extensions/shadowvui/shadow-2.3.2m-linux/bin/shadow3

	


