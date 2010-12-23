#
#

run:
	../shadow3 < shadow3.inp

all:  run python

python:
	/bin/cp ../Shadow.so .
	/bin/cp ../libshadowc.so .
	/bin/cp ../libshadow.so .
	python test.py

clean: 
#shadow runs
	/bin/rm -f end.* begin.dat star.* mirr.* screen.* \
                   effic.* angle.* optax.*
	/bin/rm -f starPy.04
	/bin/rm -f SRANG SRDISTR SRSPEC  
	/bin/rm -f plotxy_scatter.dat plotxy_histtop.dat plotxy_histside.dat
	/bin/rm -f plotxy.gpl 
	/bin/rm -f rmir.* ccc.out
	/bin/rm -f *.so fort.*


