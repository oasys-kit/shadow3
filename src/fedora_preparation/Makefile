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
#       make idl        : creates idl bind
#       make preprocess : creates *.f90 files (to be exported to other platforms)
#
#       make softclean  : cleans files created when running make 
#                         except shadow3 binary
#       make clean      : cleans files created when running make
#       make purge      : clean + removes files created by shadow3
#
#       make install    : to be adapted to user needs
#
# HINTS:
#
#   customize next section (only) 
#
#   Recommended compiler: gfortran
#


##-------------------------------------------------------------------------------
##                   THIS SECTION CAN BE CUSTOMIZED 
##-------------------------------------------------------------------------------

SO=.so
EXE=
COMPILEOPT=
STATIC =

ifeq ($(OS),Windows_NT)
    uname_S := Windows
else
    uname_S := $(shell uname -s)
endif

ifeq ($(uname_S), Windows)
    SO=.dll
    EXE=.exe
    COMPILEOPT=-D_COMPILE4WIN
    STATIC = -static
endif

ifeq ($(uname_S), Linux)
    SO=.so
    EXE=
    COMPILEOPT=-D_COMPILE4NIX
    STATIC = 
    #STATIC = -static
    #STATIC = -static -static-libgfortran -static-libgcc
endif
ifeq ($(uname_S), Darwin)
    #needed for gfortran in macos 10.10
    export MACOSX_DEPLOYMENT_TARGET=10.9
    SO=.so
    EXE=
    COMPILEOPT=-D_COMPILE4MAX
    STATIC = 
endif

#for 32 bits compilation, set this
#(in Ubuntu needs: sudo apt-get install gfortran-multilib )
#32BITS=-m32

#
# customize compiler and flags
#
FC = gfortran
FFLAGS = -I ./ -cpp -fPIC -ffree-line-length-none $(32BITS) $(STATIC) -O2 -fomit-frame-pointer $(COMPILEOPT)
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

#-fopenmp -g
CFLAGS = -I ./def/ -fPIC $(32BITS)
#-fopenmp -g

LIBFLAGS = -shared -lm 
#-lpthread

CPP = cpp -traditional

#-- MPI stuff

#do not change this. Meant to be command line argument
MPI=
MPIFC = mpif90

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
# note that all fortran sources are passed
#      by the preprocessor, even if only a few ones have preprocessor 
#      instructions, namely: 
#                            shadow_version.F90  
#                            shadow_globaldefinitions.F90  
#                            shadow_variables.F90  
#                            shadow_kernel.F90  
#

OBJFMODULES = \
	shadow_version.o \
	shadow_globaldefinitions.o \
	stringio.o \
	gfile.o \
	shadow_beamio.o \
	shadow_math.o \
	shadow_variables.o \
	shadow_roughness.o \
	shadow_kernel.o \
	shadow_synchrotron.o \
	shadow_pre_sync.o \
	shadow_pre_sync_urgent.o \
	shadow_preprocessors.o \
	shadow_postprocessors.o \
	shadow_bind_f.o \
	shadow_crl.o

FMODULESPRE = ${FMODULES:.F90=.f90}

#
# targets
#
all: shadow3 

#
# shadow3 target ----------------
#
shadow3: $(OBJFMODULES) shadow3.o
	$(FC) $(LINKFLAGS) -o shadow3$(EXE) shadow3.o $(OBJFMODULES) 
	echo 'Note that binary is created in the root directory (./..) '

shadow3.o: shadow3.f90
	$(FC) $(FFLAGS) -c shadow3.f90 

#
#objects ingredients
#

shadow_version.o: shadow_version.h shadow_version.f90
	$(FC) $(FFLAGS) -c shadow_version.f90 

# shadow_version.sh creates shadow_version.h including compilation info 
# for current system
shadow_version.h:
	./shadow_version.sh $(FC)

shadow_globaldefinitions.o: shadow_globaldefinitions.f90
	$(FC) $(FFLAGS) -c shadow_globaldefinitions.f90 

stringio.o: stringio.f90
	$(FC) $(FFLAGS) -c stringio.f90 

gfile.o: gfile.f90
	$(FC) $(FFLAGS) -c gfile.f90 

shadow_beamio.o: shadow_beamio.f90
	$(FC) $(FFLAGS) -c shadow_beamio.f90 

shadow_math.o: shadow_math.f90
	$(FC) $(FFLAGS) -c shadow_math.f90 

shadow_variables.o: shadow_variables.f90
	$(FC) $(FFLAGS) -c shadow_variables.f90 

shadow_roughness.o: shadow_roughness.f90
	$(FC) $(FFLAGS) -c shadow_roughness.f90 

shadow_kernel.o: shadow_kernel.f90
	$(FC) $(FFLAGS) -c shadow_kernel.f90 

shadow_synchrotron.o: shadow_synchrotron.f90
	$(FC) $(FFLAGS) -c shadow_synchrotron.f90 

shadow_pre_sync.o: shadow_pre_sync.f90
	$(FC) $(FFLAGS) -c shadow_pre_sync.f90 

shadow_pre_sync_urgent.o: shadow_pre_sync_urgent.f90
	$(FC) $(FFLAGS) -c shadow_pre_sync_urgent.f90 

shadow_preprocessors.o: shadow_preprocessors.f90
	$(FC) $(FFLAGS) -c shadow_preprocessors.f90 

shadow_postprocessors.o: shadow_postprocessors.f90
	$(FC) $(FFLAGS) -c shadow_postprocessors.f90 

shadow_bind_f.o: shadow_bind_f.f90
	$(FC) $(FFLAGS) -c shadow_bind_f.f90 

shadow_crl.o: shadow_crl.f90
	$(FC) $(FFLAGS) -c shadow_crl.f90 

#
# lib target ----------------
#
lib: $(OBJFMODULES) shadow_bind_c.o shadow_bind_cpp.o
	$(FC) $(LIBFLAGS) $(CFLAGS) -o libshadow3$(SO) $(OBJFMODULES)
	$(CC) $(LIBFLAGS) $(CFLAGS) -o libshadow3c$(SO) shadow_bind_c.o -L. -lshadow3 #$(OBJFMODULES)
	$(CXX) $(LIBFLAGS) $(CFLAGS) -o libshadow3c++$(SO) shadow_bind_cpp.o -L. -lshadow3 #$(OBJFMODULES)

shadow_bind_c.o: c/shadow_bind_c.c
	$(CC) $(CFLAGS) -c c/shadow_bind_c.c

shadow_bind_cpp.o: c/shadow_bind_cpp.cpp
	$(CXX) $(CFLAGS) -c c/shadow_bind_cpp.cpp

libstatic: $(OBJFMODULES) shadow_bind_c.o shadow_bind_cpp.o
	ar cr libshadow3.a $(OBJFMODULES) 
	ar cr libshadow3c.a shadow_bind_c.o $(OBJFMODULES)
	ar cr libshadow3c++.a shadow_bind_c.o shadow_bind_cpp.o $(OBJFMODULES)

#
# examples target -----------------
#
examples: examples_fortran examples_c

examples_fortran: libstatic examples/example01_f95.F90  examples/example_standalone_mirror.F90  examples/trace3.F90 examples/trace.F90 examples/example02_f95.F90 examples/gen_source.F90 examples/trace3mpi.F90
	$(FC) $(FFLAGS) -c examples/gen_source.F90
	$(FC) $(LINKFLAGS) -o gen_source gen_source.o -L. -lshadow3

	$(FC) $(FFLAGS) -c examples/trace.F90
	$(FC) $(LINKFLAGS) -o trace trace.o -L. -lshadow3

	$(FC) $(FFLAGS) -c examples/trace3.F90
	$(FC) $(LINKFLAGS) -o trace3 trace3.o -L. -lshadow3

	$(FC) $(FFLAGS) -c examples/example01_f95.F90
	$(FC) $(LINKFLAGS) -o example01_f95 example01_f95.o -L. -lshadow3

	$(FC) $(FFLAGS) -c examples/example02_f95.F90
	$(FC) $(LINKFLAGS) -o example02_f95 example02_f95.o -L. -lshadow3

	$(FC) $(FFLAGS) -c examples/example_standalone_mirror.F90
	$(FC) $(LINKFLAGS) -o example_standalone_mirror example_standalone_mirror.o -L. -lshadow3

examples_c: lib examples/trace3_c.c examples/example01_c.c examples/example_shadow_format.c
	$(CC) $(CFLAGS) -I ./c/ -c examples/trace3_c.c
	$(CC) $(CFLAGS) -o trace3_c trace3_c.o -L. -lshadow3c -lshadow3

	$(CC) $(CFLAGS) -c -I ./c/ examples/example01_c.c
	$(CC) $(CFLAGS) -o example01_c example01_c.o -L. -lshadow3c -lshadow3

	$(CC) $(CFLAGS) -c -I ./c/ examples/example_shadow_format.c
	$(CC) $(CFLAGS) -o example_shadow_format example_shadow_format.o -L. -lshadow3c -lshadow3

examples_cpp: lib examples/trace3_cpp.cpp examples/example01_cpp.cpp
	$(CXX) -I. $(CFLAGS) -I ./c/ -c examples/trace3_cpp.cpp
	$(CXX) $(CFLAGS) -o trace3_cpp trace3_cpp.o -L. -lshadow3c++ -lshadow3c -lshadow3

#	$(CXX) -I. $(CFLAGS) -I ./c/ c example01_cpp.cpp -o example01_cpp.o
#	$(CXX) $(CFLAGS) -o example01_cpp example01_cpp.o -L. -lshadow3c++ -lshadow3c -lshadow3

#
#TODO: not yet working
#

ifeq ($(MPI),1)
	$(MPIFC) $(FFLAGS) -c examples/trace3mpi.F90
	$(MPIFC) $(FFLAGS) -o trace3mpi trace3mpi.o -L. -lshadow3 -lmpi_F90
endif

preprocess: $(FMODULESPRE) shadow3.f90
	for myfile in $(FMODULESPRE); do \
		echo "Removing lines starting with # in: " $$myfile ; \
		sed -i '/^#/d' $$myfile ; \
	done
	echo "Removing lines starting with # in: shadow3.f90"
	sed -i '/^#/d' shadow3.f90 

idl: lib shadow_bind_idl.c shadow_bind_idl_loader.c shadow_bind_idl_loader.h idl_export.h shadow_bind_idl.dlm
	$(CC) $(CFLAGS) -c shadow_bind_idl_loader.c
	$(CC) $(CFLAGS) -c shadow_bind_idl.c
	$(CC) $(LIBFLAGS) -o shadow_bind_idl$(SO) -L. -lshadow3c shadow_bind_idl_loader.o shadow_bind_idl.o



#===============================================================================
#===============================================================================

python: 
	echo "Not supported: please use directly python setup.py "

clean: softclean
	/bin/rm ../shadow3$(EXE) 

softclean:  
	/bin/rm -f *.o *.mod *$(SO) *.a *.dylib
	/bin/rm -f version.txt
	/bin/rm -f gen_source trace trace3 trace3mpi trace3_c trace3_cpp
	/bin/rm -f example_shadow_format
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
	install ../shadow3 /usr/bin/
	#install libshado*$(SO) /usr/lib/

	


