--------------------------------------------------------------------------------
                           SHADOW 3.0 SOURCE DISTRIBUTION
--------------------------------------------------------------------------------


Contents:


1 What is SHADOW
2 Download
3 Source files
4 Building SHADOW
5 Other info
6 Contact

--------------------------------------------------------------------------------
1 What is SHADOW
----------------

SHADOW is an open source ray tracing code for modeling optical systems. 

Targeted to synchrotron radiation beamlines, it has unique features for 
designing X-ray optical systems. 

For more info, please read this paper (open access):

SHADOW3: a new version of the synchrotron X-ray optics modelling package
M. Sanchez del Rio, N. Canestrari, F. Jiang and F. Cerrina
Journal of Synchrotron Radiation Volume 18, Part 5 (September 2011)
http://dx.doi.org/10.1107/S0909049511026306

2 Download
----------

SHADOW3 sources are downloaded using git: 

  since 2015-10-16 the master shadow3 git repository is at github.com:

  git clone https://github.com/srio/shadow3

  however, the original repository at the EPN-campus is maintained, and
  will be kept updated, from time to time: 

  git clone git://git.epn-campus.eu/repositories/shadow3

Other SHADOW web resources: 

  shadow3 source repository: 
    http://github.com/srio/shadow3   

  shadow3 binary distributions (not updated):
    shadow3: ftp://ftp.esrf.fr/pub/scisoft/shadow3/  
    shadow2: ftp://ftp.esrf.fr/pub/scisoft/shadow/  ( - obsoleted - )

  Documentation:
    Shadow Primer (for using shadow3 in command mode)
      http://forge.epn-campus.eu/attachments/996/Shadow3Primer.pdf  

  SHADOW user interfaces:
    ShadowOui: 
      Pretty fancy and new interface under python/oasys:
      https://www.elettra.trieste.it/lightsources/labs-and-services/hard-x-ray-techniques-laboratory/oasys.html
      http://ftp.esrf.eu/pub/scisoft/Oasys/ 

    ShadowVui:
      The traditional IDL-based interface working under XOP 2.4 and 2.3
      xop2.4: https://www1.aps.anl.gov/Science/Scientific-Software/XOP
      xop2.3: ftp://ftp.esrf.fr/pub/scisoft/xop2.3/  

  Official reference:
    http://dx.doi.org/10.1107/S0909049511026306 


3 Source files
--------------

After using "git clone https://github.com/shadow3"
a single shadow3 directory is created. It contains all sources and resources tu build shadow3. The basic files are

Tools:
  Makefile:  makefile 

Source files: 
	shadow_version.F90 
	shadow_globaldefinitions.F90 
	stringio.F90 
	gfile.F90 
	shadow_beamio.F90 
	shadow_math.F90 
	shadow_variables.F90 
	shadow_kernel.F90 
	shadow_synchrotron.F90 
	shadow_pre_sync.F90 
	shadow_preprocessors.F90 
	shadow_postprocessors.F90 
	shadow_bind_f.F90 
	shadow_crl.F90

        cdf_z.f
        wranc.c

Source file main shadow3 program:
  shadow3.F90		      

Source file optional main programs (examples):
  gen_source.F90		      
  trace3.F90
  trace3mpi.F90
  trace.F90
  example01_f95.F90	      
  example02_f95.F90	      

Source file API (C/C++/PYTHON/IDL)
  shadow_bind_idl.c	   
  shadow_bind_idl_loader.c  
  shadow_bind_c.c		 
  shadow_bind_python.c
  setup.py	       
  ShadowTools.py
  ShadowToolsPrivate.py  
  setpythonpath.sh

Source file examples API (C/C++/PYTHON/IDL)
  trace3_c.c
  trace3_py.py
  example01_c.c		 
  example_shadow_format.c  
  example_tolerances.py  
  shadow_bind_idl_test.pro

README files:
  README.txt (this file)
  README_API.txt 
  README_CRL.txt 
  README_MPI.txt 
  README_PYTHON.txt
  source.nml   (description of the source variables, as in start.00)
  oe.nml       (description of the optical element variables, as in start.01)

Data files: 
  PRELIB1.DAT and PRELIB2.DAT (data file containg the oprical library)

Directories:
  PRIMER_INPUT_FILES  (script to run all the examples discussed in the Primer)

4 Building SHADOW
----------------
  Check (edit) the Makefile if you need to redefine compilers etc.

  The use of Makefile is documented in its header. Basically:
  make           : builds shadow3 the main program
  make python    : builds shadow3 python API
  make all       : builds shadow3, libraries, examples and APIs


5 Other info
------------

6 Contact
---------

SHADOW is currently maintained by Manuel Sanchez del Rio (srio@esrf.eu)



