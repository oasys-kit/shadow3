#
# makefile for shadow3
#
# srio@esrf.eu    05-02-2010  first version
# srio@esrf.eu    17-12-2010  version using preprocessor
#
FC = g95
#in coral, source /scisoft/ESRF_sw/opteron2/set_environment.tcsh
#FC = gfortran
FFLAGS = 

FMODULES =                    \
	shadow_globaldefinitions.f90          \
	stringio.f90          \
	gfile.f90             \
	shadow_beamio.f90            \
	shadow_math.f90       \
	shadow_variables.f90  \
	shadow_kernel.f90     \
	shadow_synchrotron.f90 \
	shadow_pre_sync.f90     \
	shadow_preprocessors.f90        \
	shadow_postprocessors.f90
#	cdf_z.f


FMAINS=                       \
       gen_source.f90         \
       trace.f90              \
       trace3.f90             \
       shadow3.f90            \
       fig3.f90

FTESTS =                        \
	test_stringio.f90       \
	test_gfile.f90          \
	test_beamio.f90         \
	test_math.f90           \
	test_integers.f90       \
	test_shadow_kernel.f90


OBJMODULES =  ${FMODULES:.f90=.o}
OBJMAINS   =  ${FMAINS:.f90=.o}
OBJTESTS   =  ${FTESTS:.f90=.o}


all: $(OBJMODULES) $(OBJMAINS)
#	gcc -c wranc.c
#	g95 -c cdf_z.f
#	$(FC) $(FFLAGS) -o gen_source gen_source.o $(OBJMODULES) wranc.o 
#	$(FC) $(FFLAGS) -o trace trace.o $(OBJMODULES) wranc.o
#	$(FC) $(FFLAGS) -o trace3 trace3.o $(OBJMODULES) wranc.o
#	$(FC) $(FFLAGS) -o fig3 fig3.o $(OBJMODULES) wranc.o 
#	$(FC) $(FFLAGS) -o shadow3 shadow3.o $(OBJMODULES) wranc.o cdf_z.o
	$(FC) $(FFLAGS) -o shadow3 shadow3.o $(OBJMODULES) 

shadow_variables.f90: shadow_variables_precpp.F90
	cpp -w -C shadow_variables_precpp.F90 -o tmp1.f90  
	sed 's/newline/\n/g' <tmp1.f90 > tmp2.f90
	sed 's/^#/!#/' < tmp2.f90 > shadow_variables.f90

shadow_kernel.f90: shadow_kernel_precpp.F90
	cpp -w -C shadow_kernel_precpp.F90 -o tmp3.f90  
	sed 's/newline/\n/g' <tmp3.f90 > tmp4.f90
	sed 's/^#/!#/' < tmp4.f90 > shadow_kernel.f90

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean: 
#objects
	/bin/rm -f *.o *.mod
#shadow runs
	/bin/rm -f start.* end.* begin.dat star.* mirr.* screen.* \
                   systemfile.* effic.* angle.* optax.*
# binaries
	/bin/rm -f gen_source trace trace3 shadow3 fig3

# files created by the preprocessor
	/bin/rm -f tmp1.f90 tmp2.f90 tmp3.f90 tmp4.f90
	/bin/rm -f shadow_variables.f90 shadow_kernel.f90

install:
	/bin/cp shadow3 /scisoft/xop2.3/extensions/shadowvui/shadow-2.3.2m-linux/bin/shadow3


