--------------------------------------------------------------------------------
                     SHADOW under PYTHON: A start-up mini-guide
--------------------------------------------------------------------------------

Contents:

1 Generalities
2 Build shadow3-python
3 Testing if it works
4 Running SHADOW
5 Access to data
6 using ShadowTools (histo1, plotxy, etc.)
7 using ShadowPreprocessorsXraylib
8 using SHADOW with SRW

--------------------------------------------------------------------------------

1) Generalities
---------------

SHADOW is interfaced to python with the idea of using the enormous potential 
of python scripting in combination with the SHADOW kernel and additional 
tools written in python. It permits to run SHADOW from the python prompt, 
create sophisticated runs using scripts, and apply new tools and preprocessors 
written in python. In the future, we plan to develope a completely new GUI 
using PyQt. 

The python tools are under continuous development, and this file has the 
aim to describe the implementation and give clues for programmers 
wanting to explore SHADOW's potential under python. 

Python use the following modules for working with SHADOW: 
   1) numpy (mandatory)
   2) matplotlib (optional)
   3) xraylib (optional)
   4) SRW (optional)

Python tools are scattered in several files in the shadow3 distribution: 
   $ ls  *.py
   example_tolerances.py  ShadowMain.py                  ShadowTools.py
   __init__.py            ShadowPreprocessorsXraylib.py  SRW2SHADOW_Example01.py
   setup.py               ShadowSrw.py                   srwlib_fake.py
   ShadowLibExtensions.py ShadowToolsPrivate.py          trace3_py.py


2) Build shadow3-python
-----------------------

   For building the necessary structure needed to run shadow3 in python
   you need to "make python" which essentially creates the needed 
   runtime libraries "make lib" and then runs "python setup.py build". 
   The latter will compile the necessary C code for the binding 
   (shadow_bind_python.c) and define the needed libraries shadow3c 
   (C binding) and shadow3 (kernel).   
   
   Using the standard python building strategy, the results are placed
   in a directory called "build". In my case (linux) I have: 

   $ ls build/lib.linux-x86_64-2.7/Shadow/
      example_tolerances.py  ShadowPreprocessorsXraylib.py  srwlib_fake.py
      __init__.py	       ShadowSrw.py		      trace3_py.py
      ShadowLib.so	       ShadowToolsPrivate.py
      ShadowLibExtensions.py   ShadowTools.py

   Please note that we have not used any installation feature.
   The installation in central directories is let to the users. 

   The python stuff has been developed under linux/ubuntu, and partially
   tested under MacOSX, but not yet under Windows.
   
3) Testing if it works
----------------------

   Because initialize shadow3-python via the __init__.py file, it is not 
   possible to run SHADOW stuff from the same build directory. 

   We must cd to another directory, and define two environment variables 
   pointing to the directory containg the shadow3 stuff and the directory 
   containing the required libraries. In my case I do: 

   export  LD_LIBRARY_PATH=/users/srio/GIT/shadow3/ 
   export  PYTHONPATH=/users/srio/GIT/shadow3/build/lib.linux-x86_64-2.7

   and now I call python and import SUCCESFULLY shadow3 stuff called "Shadow":

   $ python
   Python 2.7.3 (default, Aug  1 2012, 05:14:39) 
   [GCC 4.6.3] on linux2
   Type "help", "copyright", "credits" or "license" for more information.
   >>> import Shadow
   >>> 
   
   To make life easier two things may ba considered: 
   1) The file setpythonpath.sh is provided (must be customized) for defining
      the environment :

      $ source  setpythonpath.sh 

   2) It is recommended to use the ipython interface, which allows easy 
      editing and completion of the command:

      $ ipython
      In [1]: import Shadow
      In [2]: 


4) Running SHADOW
-----------------

     What to do after a succesful "import Shadow"? 
     Read and run this piece of code (you may copy and paste in your python 
     session):

#--- START python example 1 ---
#
# a minimalist shadow3-python run
#
import os      # for commodity
import Shadow 
#
# create a "beam"
#
beam = Shadow.Beam()
#
#source
#
src = Shadow.Source()  # define a container for the source parameters
# loads values in start.00, if exists
if os.path.isfile('start.00'):
   src.load('start.00')   

beam.genSource(src)      # runs shadow/source
#beam.load('begin.dat')  # alternatively reads source from file begin.dat

#
#oe
#
oe1 = Shadow.OE()        # define a contained for the first optical element
if os.path.isfile('start.01'):
    oe1.load('start.01')

beam.traceOE(oe1,1)
#--- END python example 1 ---

An edditional example (trace3_py.py) runs SHADOW from parameter files 
(start.xx, systemfile.dat). Usage: 
  python trace3_py.py [-s] [-t] [-a]

4) Access to data
-----------------
   after doing: 

import Shadow
beam = Shadow.Beam()
src = Shadow.Source()  
oe1 = Shadow.OE()

all SHADOW variables (capitalized) are available in python for reading 
and writing: 
 src.XXX  with XXX the variables in start.00, e.g., print (src.NPOINT)
 oe1.XXX  with XXX the variables in start.01, etc. , e.g., print (oe1.T_INCIDENCE)
 after "filling" beam (e.g., running source by doing beam.genSource(src) ) 
 you can access to the SHADOW beam ray matrix (npoints,18 columns): 
    In [30]: beam.rays[100,17]
    Out[30]: -0.00012925520265964806
 Just remember that python indices start by zero so index [100,17] corresponds
    to the 18th "column" of the 101st "ray" (in SHADOW's terminology).
 

6) using ShadowTools (histo1, plotxy, etc.)
-------------------------------------------

ShadowTools is a python module containing graphic applications (plotxy and 
histo1) implemented in python with matplotlib

It needs python (we reccomend ipython) with matplotlib installed.

First run SHADOW to get some files ready. For example (a different one is 
also possible) start ShadowVUI and run the tutorial file ex17a.ws 

Launch "ipython" (default matplotlib backend) or "ipython -pylab qt" 
(with matplotlib and qt ready). 

Then you may run this bunch of examples

#--- START python example 2 ---
#
import Shadow.ShadowTools as st
#
#
# HISTO1 examples
#
st.histo1("begin.dat",1)
#
st.histo1("begin.dat",1,nbins=80,xrange=[-.15,.15]) 
#
st.histo1("begin.dat",1,nbins=80,yrange=[0.0,300.0])
#
st.histo1("begin.dat",1,nbins=80,nolost=2)
#
st.histo1("begin.dat",1,nbins=80,ref=23)
#
# this example will also write the file HISTO1 containing the histogram
st.histo1("begin.dat",1,write=1)
#
st.histo1("begin.dat",1,nbins=100,title="Hello",xtitle="Hello1",ytitle="Hello2")
#
st.histo1("begin.dat",11,noplot=1)
st.plt.show() 
#
p = st.histo1("begin.dat",11,noplot=1)
#p.<TAB> to see output
p.figure.show()
#
st.histo1("star.01",11,ref=1)
#
# PLOTXY examples
#
help(st.plotxy)
#
st.plotxy("begin.dat",1,4,xrange=[-0.25,0.2])
#
st.plotxy("begin.dat",1,4,yrange=[-.002,0.002])
#
st.plotxy("begin.dat",1,3,nolost=1)
st.plotxy("begin.dat",1,3,nolost=2)
#
st.plotxy("star.01",1,2,xtitle="Hello",ytitle="hello2",title="Hello")
#
st.plotxy("star.01",1,2,noplot=1)
st.plt.show()
#
st.plotxy("star.01",1,2,calfwhm=1)
#
o= st.plotxy("star.01",1,2,calfwhm=1)
#
st.plotxy("star.02",1,3,contour=5)  
#
st.plotxy("star.02",1,3,contour=6)
#
# OTHER examples
#
a=st.getshcol("star.01",(1,3))
#
a=st.getshcol("star.01",1) # also: a=st.getshonecol("star.01",1) )
#--- END python example 2 ---

The file example_tolerances.py contains an example of a script for calculating 
the tolerances of a mirror. It uses ShadowTools to compute histograms and 
calculate their width, etc. It outputs the results to a file. To run it
(you must prepare before the start.xx files): 

python example_tolerance.py

Some common questions using ShadowTools:

i)   cannot use TAB in ipython: set correct ~/.ipython files
ii)  how to print the plot?  Save to a file, and print it
iii) how to clean MATPLOTLIB memory (note that clicking "x" in the window 
     does not clean memory): st.plt.close("all")

7) using ShadowPreprocessorsXraylib
-----------------------------------

This is a first use of xraylib for creating the preprocessor files needed 
by SHADOW. 

What is xraylib?
    xraylib is an open source library for X-ray matter interaction cross 
    sections for X-ray applications
         Repository: https://github.com/tschoonj/xraylib
         ESRF site: http://ftp.esrf.eu/pub/scisoft/xraylib/readme.html
         Reference: http://dx.doi.org/10.1016/j.sab.2011.09.011  

Why using xraylib?
    The optical library in SHADOW is a bit obsolete, and should be updated 
    in the future. 
    One of the solutions we are studying is the use of xraylib. 


ShadowPreprocessorsXraylib is a python module that implements the SHADOW 
preprocessors for creating the reflectivity information for mirrors (prerefl), 
multilayers (pre_mlayer), and crystals (bragg). 

Limitation: bragg() preprocessor is limited to ZincBlende cubic 
crystallographic structures. In the future all the SHADOW crystal code will be 
upgraded to include any crystallographic system, so this utility will also 
need a revision. 

Usage: 
      You need xraylib installed (see instructions in its site).
      Define the environment (see file setpythonpath.sh)
        export LD_LIBRARY_PATH="/users/srio/xraylib-2.16.0/src/.libs" 
        export DYLD_LIBRARY_PATH="/users/srio/xraylib-2.16.0/src/.libs" 
        export PYTHONPATH="/users/srio/xraylib-2.16.0//python" 

      Then: 
      python ShadowPreprocessorsXraylib.py
      or, alternatively: 
      ipython
      In [1]: from Shadow.ShadowPreprocessorsXraylib import *
      In [2]: bragg() # or pre_mlayer() or prerefl()
      bragg: SHADOW preprocessor for crystals - python+xraylib version  etc etc.


Bugs (in xraylib): 
     It has been found that xraylib create values of alpha and gamma vs q (in 
     prerefl) that are not smooth but present some "stepping". This is due to 
     a problem with dealing with single precission numbers passed to python. 
     To solve this issue, change 
     float->double in the head or the functions inside
     ~/xraylib-2.16.0/src/refractive_indices.c :
      double Refractive_Index_Re(const char compound[], float E, float density) {
      double Refractive_Index_Im(const char compound[], float E, float density) {
     and also in ~/xraylib-2.16.0/include/xraylib.h : 
      /* Refractive indices functions */
      double Refractive_Index_Re(const char compound[], float E, float density);
      double Refractive_Index_Im(const char compound[], float E, float density);
(srio@esrf.eu  2012/10/03)



8) using SHADOW with SRW
------------------------

Python is an ideal tool for scripting and thus combining different packages
under the same interface. The potential of SHADOW can be extended using the 
abilities of SRW to simulate very accurately synchrotron and FEL sources, and
to trace a beamline using wave optics. We have the intention to use in the 
SHADOW and SRW packages under python, and to create a common user interface 
that will permit to define a beamline in a unique way, and trace it either with SHADOW or SWR, or a combination of both. 

As an example, we include a scripts that implement a beamline, where the
undulator source is created with SRW. SHADOW's source is sampled from the 
probability distributions calculated by SRW and stored in hdf5 files. 

python SRW2SHADOW_Example01.py help


