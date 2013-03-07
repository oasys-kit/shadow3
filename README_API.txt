
--------------------------------------------------------------------------------
                     SHADOW Application Programming Interface 
--------------------------------------------------------------------------------

Contents:

1 Generalities
2 Fortran kernel
3 Fortran layer
4 C layer
5 C++ layer
6 Python Layer
7 IDL layer

--------------------------------------------------------------------------------

1) Generalities
---------------

SHADOW3 has been built with the idea of allowing programmers to develop new tools
based on its kernel. The SHADOW kernel is written in pure Fortran 90. 

An API has been written in top of the Fortran Kernel to be able to use SHADOW
from C, C++, python and IDL, and may be other languages. 

The layer structure is summarised in this README_API.txt file. It consists a basic
C layer on top of the Fortran kernel that serves as a common base for the other layers
(C++, python and IDL): 



|------------------------------------------|
|  C++ layer |  python layer  |  IDL layer |
|------------------------------------------|
|                  C layer                 |
|------------------------------------------|
|              Fortran layer               |
|------------------------------------------|
|              Fortran Kernel              |
|------------------------------------------|


2) Fortran Kernel
-----------------

The SHADOW kernel consists of several Fortran modules:

(i)   shadow_globaldefinitions: basic definitions used everywhere.
(ii)  stringio: some string manipulation tools.
(iii) gfile: a new Fortran module to manipulate files with list of input/output 
      variables (called g-files in SHADOW).
(iv)  shadow_beamio: routines to access to binary beam files (start:xx, mirr:xx, 
      screen:xxyy).
(v)   shadow_math: mathematical tools.
(vi)  shadow_variables: definition of variables and Fortran types used by the kernel, 
      plus the routines to manipulate them.
(vii) shadow_kernel: contains the global variables and the routines in the SHADOW kernel.
(viii)shadow_version: created at compiletion time, contains the compilation commands.


In addition to the kernel, SHADOW3 contains the following modules:

(i)   shadow_synchrotron: synchrotron sources (bending magnets, wiggler and undulators).
(ii)  shadow_preprocessors: pre-processors, like prefefl or bragg.
(iii) shadow_pre_sync: pre-processors for shadow synchrotron.
(iv)  shadow_postprocessors: post-processors, like histo1 or ffresnel.
(v)   shadow_crl: compound refractive lenses (under development).

(v) Main programs: all SHADOW3 is included into a single command line executable: 
    shadow3. All pre- and post-processors are included in this executable. 
    In addition other main programs are provided as examples ("make examples"),
    or for being 100% compatible with the old versions ( gen_source and trace ).


3) Fortran Layer (file shadow_bind_f.F90)
-----------------------------------------

The Fortran layer define the Fortran Routines that will be seen by C and other
bindings, plus other C<->Fortran compatibility tools.

exposed variables:
 - (i)   Type PoolSource          (in module: shadow_variables)
 - (ii)  Type PoolOE              (in module: shadow_variables)
 - (iii) real(kind=8) Beam        (at main level)
 - (iv)  complex(kind=16) EField  (in module: shadow_postprocessors)


(i,ii) PoolSource (PoolOE)
  This Type holds all the variables necessary to define the Source (Optical Element). 
  As an example take a look at the file start.00 (start.0X, X=1,2,...)
  Names and kinds are defined once for all in the file shadow_source.def.def 
  (shadow_oe*.def), through a set of MACRO functions, one for each variable. 
  Each MACRO function is then reused to define, to read, to write and to and 
  for whatever operation involving all the variable in poolSource (poolOE).
  This tecnique was adopted to avoid annoying repetitions.

(iii) Beam description
  SHADOW stores the "beam" in an array the array seen as (18,NPOINT) in Fortran and 
  [NPOINT][18] in C, holding 18 real values for each ray.  In SHADOW's terminoloty
  the 18 values per ray are called "columns" (starting from 1). These values are:
  
  1  - position x in user units
  2  - position y in user units
  3  - position z in user units
  4  - director cosine x (approx angle x' in rad)
  5  - director cosine y 
  6  - director cosine z (approx angle z' in rad)
  7  - electric field s-polarized Es_x
  8  - electric field s-polarized Es_y
  9  - electric field s-polarized Es_z
  10 - flag: 1.0 the ray is good, <1 the ray is lost
  11 - wavevector modulus: 2 pi / lambda
  12 - ray index (starting from 1.0)
  13 - optical path, in user units
  14 - s-polarized phase Es_phi
  15 - p-polarized phase Ep_phi
  16 - electric field p-polarized Ep_x
  17 - electric field p-polarized Ep_y
  18 - electric field p-polarized Ep_z

(iv) EField
  complex Electric Field components used in subroutine FFresnel2D (in module 
  shadow_postprocessors) the size of the matrix is defined by the number of 
  pixels: seen as (3,nzpixels,nxpixels) in fortran and [nxpixels][nzpixels][3] 
  in C it's obvious but for each couple of nx,nz one has 3 values corresponding 
  to the components:
   - Electric Field E_x
   - Electric Field E_y
   - Electric Field E_z

Exposed functions: 
    BindShadowPoolSourceLoad, BindShadowPoolSourceWrite
    BindShadowPoolOELoad, BindShadowPoolOEWrite
    BindShadowSourceGeom, BindShadowSourceSync, BindShadowTraceOE
    BindShadowBeamWrite, BindShadowBeamgetDim, BindShadowBeamLoad
    BindShadowFFresnel2d


4) C Layer (files shadow_bind_c.h, shadow_bind_c.c)
-----------------------------------------------------

exposed variables:
 - typedef struct poolSource
 - typedef struct poolOE
 - typedef struct pixel
 - typedef struct dComplex

exposed functions:
 - double* CShadowAllocateBeamFromPool ( poolSource* src, double* ray);
 - double* CShadowAllocateBeam ( int, double* );
 - void CShadowPoolSourceLoad ( poolSource* src, char* filename);
 - void CShadowPoolSourceWrite ( poolSource* src, char* filename);
 - void CShadowPoolOELoad ( poolOE* oe, char* filename);
 - void CShadowPoolOEWrite ( poolOE* oe, char* filename);
 - void CShadowBeamGetDim ( int* nCol, int* nPoint, char* filename);
 - void CShadowBeamLoad ( double* ray, int nCol, int nPoint, char* filename);
 - void CShadowBeamWrite ( double* ray, int nCol, int nPoint, char* filename);
 - void CShadowSourceGeom ( poolSource* src, double* ray);
 - void CShadowSourceSync ( poolSource* src, double* ray);
 - void CShadowTraceOE ( poolOE* oe, double* ray, int nPoint, int iCount);
 - void CShadowFFresnel2D ( double* ray, int nPoint, double dist, dComplex* E, pixel* xp, pixel* zp);
 - void CShadowSetupDefaultSource ( poolSource* src);
 - void CShadowSetupDefaultOE ( poolOE* oe);

all defined in file: shadow_bind_c.h

C Structures defined:

(i)   poolSource poolOE: defined using MACRO as for Fortran

(ii)  pixel: struct containing:
 - int         np : number of pixels
 - double      up : max measure of image along this axis
 - double      dn : min measure of image along this axis

(iii) dComplex: struct containing:
 - double      real
 - double      imag

C functions defined: 

(i)   CShadowAllocateBeamFromPool
      purpose:
        Ensure the Allocation of the array ray where SHADOW store the physical quantities.
        It allocate ray using realloc.
      input
       . poolSource *src   
       . double     *beam    : always pointer to the contigous bank of memory hosting the matrix.
      output
       . double     *beam

(ii)  CShadowAllocateBeam
      purpose:
        Ensure the Allocation of the array ray where SHADOW store the physical quantities.
        It allocate ray using realloc.
      input
       . int         nPoint : number of rays.
       . double     *beam    : always pointer to the contigous bank of memory hosting the matrix.
      output
       . double     *beam

(iii) CShadowPoolSourceLoad (CShadowPoolSourceWrite)
      purpose:
        It reads (writes) poolSource from (to) the file called filename.
      input
       . poolSource *src
       . char       *filename : name of the file where to read (write). It is similar to start.00
      output
       . none

(iv)  CShadowPoolOELoad (CShadowPoolOEWrite)
      purpose:
        It reads (writes) poolOE from (to) the file called filename.
      input
       . poolOE     *oe
       . char       *filename : name of the file where to read (write). It is similar to start.0X, where X=1,2,...
      output
       . none

(v)   CShadowSetupDefaultSource (CShadowSetupDefaultOE)
      purpose:
        It sets default values for poolSource (poolOE): fill the memory with '\0'
      input
       . poolSource *src
         or
       . poolOE     *oe
      output
       . none
      
(vi)  CShadowBeamGetDim
      purpose: 
        It gets the dimension of the ray, written in the file called "filename". Although nCol is always fixed to 18 we keep it for back compatibility reason.
        It is used to Allocate ray before reading it. (see next).
      input
       . int        *nCol     : nCol in the new version is always 18, but for back-compatibility reason here is mantained.
       . int        *nPoint
       . char       *filename
      output
       . none direct, but nCol and nPoint will be changed, and will survive after the function since one passes the pointers
      
(vii) CShadowBeamLoad (CShadowBeamWrite)
      purpose:
        It reads (writes) ray from (to) the file called filename.
      input
       . double     *ray
       . int         nCol
       . int         nPoint
       . char       *filename
      output
       . none
      
(viii) CShadowSourceGeom
       purpose:
         It generates ray according to the geometrical source src.
       input
        . poolSource *src : it must describe a geometrical source
        . double     *ray
       output
        . none, but ray is changed.

(ix)   CShadowSourceSync
       purpose:
         It generates ray according to the synchrotron source src.
       input
        . poolSource *src : it must describe a synchrotron source
        . double     *ray
       output
        . none, but ray is changed.
       
(x)    CShadowTraceOE
       purpose
         It propagates ray from the input state to final according to the optical element oe.
       input
        . poolOE     *oe
        . double     *ray
        . int         nPoint
        . int         iCount : counter of optical element mantained for back-compatibility reason. old writing to file strategy.
       output
        . none, but ray is changed.
       
(xi)   CShadowFFresnel2D
       purpose
         It generates the 2 dimensional matrix describing the complex electric field components, on an image plane at a given distance from the actual position.
       input
        . double     *ray
        . int         nPoint
        . double      distance
        . dComplex   *E : in C complex is not defined, so we use a compatible struct dComplex.
        . pixel      *xp
        . pixel      *zp
       output
        . none, but E is changed.
 
 
5) C++ Layer (files shadow_bind_cpp.hpp shadow_bind_cpp.cpp)
------------------------------------------------------------

exposed classes:

 - Source
 - OE
 - Ray

Source
It hiers members from struct poolSource.
constructors and functions
 . Source()                 : sets default values, using CShadowSetupDefaultSource.
 . Source(char* filename)   : sets values from the file filename, using CShadowSourceLoad
 . Source(const Source& src): copy-constructor
 . read(char* filename)     : read values from the file filename, using CShadowSourceLoad
 . write(char* filename)    : write values to file filename, using CShadowSourceWrite


OE
It hiers members from struct poolOE.
constructors and functions
 . OE()                     : sets default values, using CShadowSetupDefaultOE.
 . OE(char* filename)       : sets values from the file filename, using CShadowOELoad
 . OE(const OE& oe)         : copy-constructor
 . read(char* filename)     : read values from the file filename, using CShadowOELoad
 . write(char* filename)    : write values to file filename, using CShadowOEWrite


Beam members
 . int         nCol         : protected, for back-compatibility reasons.
 . int         nPoint       : protected, number of rays
 . double     *rays         : public, pointer the array of data.
constructors, destructor and functions
 . Beam()                    : simple constructor
 . ~Beam()                   : destructor, use free(Beam)
 . Beam(const Beam& r)        : copy-constructor
 . init(int col, int point) : <-- I want to make it protected
 . read(char* filename)     : read values from the file filename, using CShadowBeamGetDim and CShadowBeamLoad
 . write(char* filename)    : write values to file filename, using
CShadowBeamWrite
 . genSource(Source* src)   : generates Beam according to src (doesn't matter if src is geometrical or synchrotron), using CShadowSourceGeom or CShadowSourceSync
 . trace(OE* oe, int count) : propagates Beam according to oe, using
CShadowTraceOE
 . ffresnel2D(double dist, dComplex* E, pixel* xp, pixel* zp) : use CShadowFFresnel2D to generate E. E must be setup outside.


6) Python Layer (files shadow_bind_python.h, shadow_bind_python.c)
------------------------------------------------------------------

module Shadow:
 - class Source
 - class OE
 - class Beam
 - class Image

all declared in shadow_bind_python.h and defined in shadow_bind_python.c


Source
members
listed thank to the MACRO in shadow_source.def
methods
 . read                     : input = (string f), output = (). It reads the source from file named f.
 . write                    : input = (string f), output = (). It writes the source to file named f.
 . getters and setters      : used, the members type cannot be modified, a cast is always done if the wrong type is passed.
 

OE
members
listed thank to the MACRO in shadow_oe.def
methods
 . read                     : input = (string f), output = (). It reads the source from file named f.
 . write                    : input = (string f), output = (). It writes the source to file named f.
 . getters and setters      : used, the members type cannot be modified, a cast is always done if the wrong type is passed.
                              if the member is an array, here it will appear as a numpy array. 

Beam members
 . rays                     : numpy array holding the data.
methods
 . read                     : input = (string f), output = (). It reads the rays from file named f.
 . write                    : input = (string f), output = (). It writes the rays to file named f.
 . genSource                : input = (Source s), output = (). It generates the rays according to Source s.
 . traceOE                    : input = (OE oe, int32 i), output = (). It traces rays according to OE oe. i is the OE number.
 . getters and setters      : not used.


Image members
 . xmax                     : float64. horizontal max of the image.
 . xmin                     : float64. horizontal min of the image.
 . zmax                     : float64. vertical max of the image.
 . zmin                     : float64. vertical min of the image.
 . distance                 : float64. distance of the image.
 . npixel_x                 : int32. number of pixels along along the horizontal axis
 . npixel_z                 : int32. number of pixels along along the vertical axis
 . image                    : numpy array. matrix with the electric field data. see ffresnel2d in C-API 
 . intensity                : numpy array. matrix |E|^2.
methods
 . fresnel2d                : input = (Ray r) output = (). It runs the subroutine ffresnel2d.

For using SHADOW with python, see file README_PYTHON.txt

7) IDL Layer 
------------

files 
idl_export.h	   shadow_bind_idl.dlm	     shadow_bind_idl_loader.h
shadow_bind_idl.c  shadow_bind_idl_loader.c  shadow_bind_idl_test.pro

procedures
 . PRINTSOURCE              : input = (IDL_Structures s, IDL_string f) write the structure equivalent to Source into the file f.
 . PRINTOE                  : input = (IDL_Structures o, IDL_string f) write the structure equivalent to OE into the file f.
functions                   
 . GENSOURCEGEOMETRIC       : input = (IDL_Structures s) output = (IDL_array ray) 
                              generate the ray from the structure s equivalent to a geometric Source
 . GENSOURCESYNCHROTRON     : input = (IDL_Structures s) output = (IDL_array ray) 
                              generate the ray from the structure s equivalent to a synchrotron Source
 . TRACEOE                  : input = (IDL_Structures o, IDL_array ray, IDL_LONG i) output = (ray) 
                              trace ray according to the structures o equivalent to an OE 

