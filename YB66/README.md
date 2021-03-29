#change source file for complex crystal calculation in SHADOW3

src/fortran/shadow_kernel.f90

#new source file:
src/fortran/crystal_allocMemory.f90

#YB66 subdir:

YB66/JUMBO_YB66.ows		#SHADOWOUV examples workspace for DCM of YB66 crystals
YB66/Plane crystal_2.png	#tracing result for YB66 crystall @2080 eV
YB66/YB66_004_1300_1500.dat	#YB66 new type crystal file centre at 1385.6 eV
YB66/YB66_006_2.dat		#YB66 new type crystal file centre at 2080 eV

YB66/Paper Fig.7.ows		#SHADOWOUV examples workspace for create Fig.7 in manuscript
YB66/InSb(111)_New.dat		#New format crystal file for Paper Fig.7.ows
YB66/InSb(111)_Old.dat		#Old format crystal file for Paper Fig.7.ows
YB66/Beryl(100).dat		#New format crystal file for Paper Fig.7.ows

#xraylib compiled files
YB66/_xraylib.cp38-win_amd64.pyd #xraylib-3.3.0 compiled dll, need to put in python/Lib/site-packages
YB66/cygxrl-7.dll		 #xraylib-3.3.0 compiled dll, can put in     python/




