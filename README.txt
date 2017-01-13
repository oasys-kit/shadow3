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

If you are (or want to be) a SHADOW user, it is recommended that you use
a user interface. We strongly recommend OASYS (https://www.elettra.eu/oasys.html).

2 Download
----------

SHADOW3 sources are downloaded using git: 

  since 2015-10-16 the master shadow3 git repository is at github.com:

  git clone https://github.com/srio/shadow3

  the original repository at the EPN-campus (git clone git://git.epn-campus.eu/repositories/shadow3) is now obsolete. 

  

Other SHADOW web resources: 

  shadow3 binary distributions (not updated):
    shadow3: ftp://ftp.esrf.fr/pub/scisoft/shadow3/  
    shadow2: ftp://ftp.esrf.fr/pub/scisoft/shadow/  ( - obsoleted - )

  Documentation:
    Shadow Primer (for using shadow3 in command mode)
      http://ftp.esrf.eu/pub/scisoft/shadow3/Shadow3Primer.pdf

  SHADOW user interfaces:
    ShadowOui: 
      Pretty fancy and new interface under python/oasys:
      https://www.elettra.eu/oasys.html 

    ShadowVui:
      The traditional IDL-based interface working under XOP 2.4 and 2.3
      xop2.4: https://www1.aps.anl.gov/Science/Scientific-Software/XOP
      xop2.3: http://ftp.esrf.fr/pub/scisoft/xop2.3/  

  Official reference:
    http://dx.doi.org/10.1107/S0909049511026306 


3 Important files
-----------------

in shadow3/src: fortran, C and other sources
in Shadow: python sources

in docs: README files:
  README_API.txt 
  README_CRL.txt 
  README_MPI.txt 
  README_PYTHON.txt
  source.nml   (description of the source variables, as in start.00)
  oe.nml       (description of the optical element variables, as in start.01)

in data: data files: 
  PRELIB1.DAT and PRELIB2.DAT (data file containg the oprical library)

4 Building SHADOW
----------------
  binary:
  cd src
  Check (edit) the Makefile if you need to redefine compilers etc.
  The use of Makefile is documented in its header. Basically:
  make           : builds shadow3 the main program
  make all       : builds shadow3, libraries, etc
  make install   : installation (see Makefile)

  python API:
  in shadow3 directory, use the standard python setup:
  
  python3 setup.py build
  sudo python3 setup.py install


5 Other info
------------

6 Contact
---------

SHADOW is currently maintained by Manuel Sanchez del Rio (srio@esrf.eu)



