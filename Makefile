#
# makefile for shadow3
# srio@esrf.eu    05-02-2010  first version
#
FC = g95
#FC = /scisoft/ESRF_sw/opteron2/bin/gfortran
FFLAGS = 

FMODULES =                    \
	shadow_kind.f90          \
	stringio.f90          \
	gfile.f90             \
	beamio.f90            \
	math.f90              \
	math_imsl.f90         \
	shadow_variables.f90  \
	shadow_kernel.f90     \
	shadow_sourcesync.f90 \
	shadow_pre_id.f90     \
	shadow_pre.f90        \
	shadow_post.f90       \
	cdf_z.f
#        shadow_sourcesync.f90

FMAINS=                       \
       gen_source.f90         \
       trace.f90              \
       trace3.f90             \
       shadow3.f90
#       input_source.f90       \
#       translate.f90          \
#       srcdf.f90              \
#       test_sync.f90          \
#       test_beamio.f90

FTESTS =                        \
	test_stringio.f90       \
	test_gfile.f90          \
	test_beamio.f90         \
	test_math.f90           \
	test_math_imsl.f90      \
	test_integers.f90       \
	test_shadow_kernel.f90


OBJMODULES =  ${FMODULES:.f90=.o}
OBJMAINS   =  ${FMAINS:.f90=.o}
OBJTESTS   =  ${FTESTS:.f90=.o}

all: $(OBJMODULES) $(OBJMAINS)
	gcc -c wranc.c
#	g95 -c cdf_z.f
#	$(FC) $(FFLAGS) -o input_source input_source.o $(OBJMODULES) 
	$(FC) $(FFLAGS) -o gen_source gen_source.o $(OBJMODULES) wranc.o 
	$(FC) $(FFLAGS) -o trace trace.o $(OBJMODULES) wranc.o
	$(FC) $(FFLAGS) -o trace3 trace3.o $(OBJMODULES) wranc.o
#	$(FC) $(FFLAGS) -o shadow3 shadow3.o $(OBJMODULES) wranc.o cdf_z.o
	$(FC) $(FFLAGS) -o shadow3 shadow3.o $(OBJMODULES) wranc.o 
#	$(FC) $(FFLAGS) -o translate translate.o stringio.o beamio.o
#	$(FC) $(FFLAGS) -o srcdf srcdf.o $(OBJMODULES) wranc.o 
#	$(FC) $(FFLAGS) -o test_sync test_sync.o  $(OBJMODULES)
#	$(FC) $(FFLAGS) -o test_beamio test_beamio.o  $(OBJMODULES)

tests: $(OBJMODULES) $(OBJTESTS)
	$(FC) $(FFLAGS) -o test_integers test_integers.o  
	$(FC) $(FFLAGS) -o test_stringio test_stringio.o  stringio.o
	$(FC) $(FFLAGS) -o test_gfile test_gfile.o gfile.o stringio.o 
	$(FC) $(FFLAGS) -o test_beamio test_beamio.o  beamio.o
	$(FC) $(FFLAGS) -o test_math test_math.o  math.o
	$(FC) $(FFLAGS) -o test_math_imsl test_math_imsl.o  math_imsl.o
	$(FC) $(FFLAGS) -o test_shadow_kernel test_shadow_kernel.o $(OBJMODULES)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<


clean: 
	/bin/rm -f *.o *.mod
	/bin/rm -f test_integers test_stringio test_gfile \
                   test_beamio test_math test_math_imsl test_shadow_kernel
	/bin/rm -f start.* end.* begin.dat star.* mirr.* screen.* \
                   systemfile.* effic.* angle.* optax.*
	/bin/rm -f input_source gen_source trace trace3 translate shadow3

install:


