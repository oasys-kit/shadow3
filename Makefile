#
# makefile for shadow3
#
# srio@esrf.eu    05-02-2010  first version
# srio@esrf.eu    17-12-2010  version using preprocessor
#
FC = g95
#in coral, source /scisoft/ESRF_sw/opteron2/set_environment.tcsh
#FC = gfortran
CC = gcc
CCP = g++
FFLAGS = -fPIC
CFLAGS = -fPIC

LIBFLAGS = -shared -lm

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
	shadow_bind.f90
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

	$(FC) $(FFLAGS) -c fig3.f90
	$(FC) $(FFLAGS) -o fig3 fig3.o -L. -lshadow

	$(FC) $(FFLAGS) -c example01_f95.f90 -o example01_f95.o
	$(FC) $(FFLAGS) -o example01_f95 example01_f95.o -L. -lshadow

	$(CC) -I. $(CFLAGS) -c example01_c.c -o example01_c.o
	$(CC) $(CFLAGS) -o example01_c example01_c.o -L. -lshadowc

	$(CC) -I. $(CFLAGS) -c example01_cpp.cpp -o example01_cpp.o
	$(CC) $(CFLAGS) -o example01_cpp example01_cpp.o -L. -lshadowc++

#TODO
#	$(CC) -I. $(CFLAGS) -c trace3_c.c -o trace3_c.o
#	$(CC) $(CFLAGS) -o trace3_c trace3_c.o -L. -lshadowc
#
#	$(CC) -I. $(CFLAGS) -c trace3_cpp.c -o trace3_cpp.o
#	$(CC) $(CFLAGS) -o trace3_cpp trace3_cpp.o -L. -lshadowc++


lib: $(OBJFMODULES) ShadowMask_c.o ShadowMask_cpp.o
	$(FC) $(LIBFLAGS) -o libshadow.so $(OBJFMODULES)
	$(CC) $(LIBFLAGS) -o libshadowc.so -L. -lshadow ShadowMask_c.o
	$(CCP) $(LIBFLAGS) -o libshadowc++.so -L. -lshadow -lshadowc ShadowMask_cpp.o

python: setup.py 
	python setup.py build
#cp library to main level
	/bin/cp build/lib.linux-x86_64-2.6/Shadow.so .

all: shadow3 lib examples python

shadow_variables.f90: shadow_variables_precpp.F90
	cpp -w -C -I. shadow_variables_precpp.F90 -o tmp1.f90  
	sed 's/newline/\n/g' <tmp1.f90 > tmp2.f90
	sed 's/^#/!#/' < tmp2.f90 > shadow_variables.f90

shadow_kernel.f90: shadow_kernel_precpp.F90
	cpp -w -C -I. shadow_kernel_precpp.F90 -o tmp3.f90  
	sed 's/newline/\n/g' <tmp3.f90 > tmp4.f90
	sed 's/^#/!#/' < tmp4.f90 > shadow_kernel.f90

ShadowMask_c.o: ShadowMask.c
	$(CC) -I. $(CFLAGS) -c ShadowMask.c -o ShadowMask_c.o

ShadowMask_cpp.o: ShadowMask.cpp
	$(CCP) -I. $(CFLAGS) -c ShadowMask.cpp -o ShadowMask_cpp.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean: 
#objects and libraries
	/bin/rm -f *.o *.mod *.so
	/bin/rm -f ../lib/*

#shadow runs
	/bin/rm -f start.* end.* begin.dat star.* mirr.* screen.* \
                   systemfile.* effic.* angle.* optax.*
# binaries
	/bin/rm -f gen_source trace trace3 shadow3 fig3
	/bin/rm -f example01_f95 example01_c example01_cpp
	/bin/rm -f ../bin/*

# files created by the preprocessor
	/bin/rm -f tmp1.f90 tmp2.f90 tmp3.f90 tmp4.f90
	/bin/rm -f shadow_variables.f90 shadow_kernel.f90

# files created by python
	/bin/rm -rf build

install:
	/bin/cp shadow3 /scisoft/xop2.3/extensions/shadowvui/shadow-2.3.2m-linux/bin/shadow3
	/bin/cp  shadow3 ../DISTR/
	/bin/cp  trace3 ../DISTR/
	/bin/cp  trace ../DISTR/
	/bin/cp  gen_source ../DISTR/
	#mv *.o ../obj
	#/bin/cp  shadow3 ../bin/
	#/bin/cp  gen_source ../bin/
	#/bin/cp  trace3 ../bin/
	#/bin/cp  trace ../bin/
	#/bin/cp libshado*.so ../lib/
	#/bin/cp build/lib.linux-x86_64-2.6/Shadow.so ../lib
	#/bin/cp  shadow3 ../bin/


