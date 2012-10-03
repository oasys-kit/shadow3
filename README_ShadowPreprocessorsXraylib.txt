
SHADOW preprocessors using xraylib in python
============================================


This is a first use of xraylib for creating the preprocessor files needed by SHADOW. 

Memorandum:
         xraylib is a library for X-ray matter interaction cross sections for X-ray applications
         Repository: https://github.com/tschoonj/xraylib
         ESRF site: http://ftp.esrf.eu/pub/scisoft/xraylib/readme.html
         Reference: http://dx.doi.org/10.1016/j.sab.2011.09.011  

Motivation: The optical library in SHADOW is a bit obsolete, and should be updated in the future. 
         One of the solutions we are studying is the use of xraylib. 
         This is an experiment to rewrite the SHADOW preprocessors for creating the reflectivity
         information for mirrors (prerefl), multilayers (pre_mlayer), and crystals (bragg). 

Limitations: bragg() preprocessor is limited to ZincBlende cubic crystallographic structures. In 
       the future all the SHADOW crystal code will be upgraded to include any crystallographic 
       system, so this utility will also need revision. 

Usage: 
      You need python, and xraylib (see instructions in its site).
      Define the environment (for me this one):
        export LD_LIBRARY_PATH="/users/srio/xraylib-2.16.0/src/.libs" 
        export DYLD_LIBRARY_PATH="/users/srio/xraylib-2.16.0/src/.libs" 
        export PYTHONPATH="/users/srio/xraylib-2.16.0//python" 

      Then: 
      python ShadowPreprocessorsXraylib.py
      or, alternatively: 
      ipython
      In [1]: from ShadowPreprocessorsXraylib import *
      In [2]: bragg() # or pre_mlayer() or prerefl()
      bragg: SHADOW preprocessor for crystals - python+xraylib version  etc etc.


Bugs (in xraylib): 
     It has been found that xraylib create values of alpha and gamma vs q (in prerefl) 
     that are not smooth but present some "stepping". This is due to a problem with dealing
     with single precission numbers passed to python. To solve this issue, change 
     float->double in the head or the functions inside
     ~/xraylib-2.16.0/src/refractive_indices.c :
         double Refractive_Index_Re(const char compound[], float E, float density) {
         double Refractive_Index_Im(const char compound[], float E, float density) {
     and also in ~/xraylib-2.16.0/include/xraylib.h : 
         /* Refractive indices functions */
         double Refractive_Index_Re(const char compound[], float E, float density);
         double Refractive_Index_Im(const char compound[], float E, float density);


srio@esrf.eu  2012/10/03
