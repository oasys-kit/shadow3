SHADOW API

F95 - KERNEL

exposed variables:
 - Type PoolSource          (in file: shadow_variables.f90)
 - Type PoolOE              (in file: shadow_variables.f90)
 - real(kind=8) Ray         (at main level)
 - complex(kind=16) EField  (in file: shadow_postprocessors.f90)

binding in file: shadow_bind.f90


PoolSource (PoolOE)
This Type holds all the variables necessary to define the Source (Optical Element). 
As an example take a look at the file start.00 (start.0X, X=1,2,...)
Names and kinds are defined once for all in the file ShadowMaskSource.def (ShadowMaskOE.def), 
through a set of MACRO functions, one for each variable. 
Each MACRO function is then reused to define, to read, to write and to and for whatever 
operation involving all the variable in poolSource (poolOE).
This tecnique was adopted to avoid annoing repetitions.


Ray
the array seen as (18,NPOINT) in Fortran and [NPOINT][18] in C, holds 18 real values for each ray.
these values are:
 - position x in cm
 - position y in cm
 - position z in cm
 - angle x' in rad
 - angle y' in rad
 - angle z' in rad
 - electric field s-polarized Es_x
 - electric field s-polarized Es_y
 - electric field s-polarized Es_z
 - flag: 1.0 the ray contributes, 0.0 it doesn't
 - angular frequency: 2 pi / lambda
 - flag index (fortran way, starting from 0.0)
 - optical path, in cm
 - electric field s-polarized phase Es_phi
 - electric field p-polarized phase Ep_phi
 - electric field p-polarized Ep_x
 - electric field p-polarized Ep_y
 - electric field p-polarized Ep_z


EField
complex Electric Field components used in subroutine FFresnel2D (in module shadow_postprocessors)
the size of the matrix is defined by the number of pixels: seen as (3,nzpixels,nxpixels) in fortran and [nxpixels][nzpixels][3] in C
it's obvious but for each couple of nx,nz one has 3 values corresponding to the components:
 - Electric Field E_x
 - Electric Field E_y
 - Electric Field E_z


C   - LAYER (see ShadowMask.h, ShadowMask.c)

exposed variables:
 - typedef struct poolSource
 - typedef struct poolOE
 - typedef struct pixel
 - typedef struct dComplex

exposed functions:
 - double* CShadowAllocateRay ( poolSource* src, double* ray);
 - double* CShadowAllocateRayInt ( int, double* );
 - void CShadowPoolSourceLoad ( poolSource* src, char* filename);
 - void CShadowPoolSourceWrite ( poolSource* src, char* filename);
 - void CShadowPoolOELoad ( poolOE* oe, char* filename);
 - void CShadowPoolOEWrite ( poolOE* oe, char* filename);
 - void CShadowSetupDefaultSource ( poolSource* src);
 - void CShadowSetupDefaultOE ( poolOE* oe);
 - void CShadowGetDimRay ( int* nCol, int* nPoint, char* filename);
 - void CShadowReadRay ( double* ray, int nCol, int nPoint, char* filename);
 - void CShadowWriteRay ( double* ray, int nCol, int nPoint, char* filename);
 - void CShadowSourceGeom ( poolSource* src, double* ray);
 - void CShadowSourceSync ( poolSource* src, double* ray);
 - void CShadowTrace ( poolOE* oe, double* ray, int nPoint, int iCount);
 - void CShadowFFresnel2D ( double* ray, int nPoint, double dist, dComplex* E, pixel* xp, pixel* zp);

all defined in file: ShadowMask.h


poolSource poolOE
difined using MACRO as for Fortran


pixel
struct containing:
 - int         np : number of pixels
 - double      up : max measure of image along this axis
 - double      dn : min measure of image along this axis


dComplex
struct containing:
 - double      real
 - double      imag


CShadowAllocateRay
purpose:
  Ensure the Allocation of the array ray where SHADOW store the physical quantities.
  It allocate ray using realloc.
input
 . poolSource *src   
 . double     *ray    : always pointer to the contigous bank of memory hosting the matrix.
output
 . double     *ray


CShadowAllocateRayInt
purpose:
  Ensure the Allocation of the array ray where SHADOW store the physical quantities.
  It allocate ray using realloc.
input
 . int         nPoint : number of rays.
 . double     *ray    : always pointer to the contigous bank of memory hosting the matrix.
output
 . double     *ray


CShadowPoolSourceLoad (CShadowPoolSourceWrite)
purpose:
  It reads (writes) poolSource from (to) the file called filename.
input
 . poolSource *src
 . char       *filename : name of the file where to read (write). It is similar to start.00
output
 . none


CShadowPoolOELoad (CShadowPoolOEWrite)
purpose:
  It reads (writes) poolOE from (to) the file called filename.
input
 . poolOE     *oe
 . char       *filename : name of the file where to read (write). It is similar to start.0X, where X=1,2,...
output
 . none


CShadowSetupDefaultSource (CShadowSetupDefaultOE)
purpose:
  It sets default values for poolSource (poolOE): fill the memory with '\0'
input
 . poolSource *src
   or
 . poolOE     *oe
output
 . none

CShadowGetDimRay
purpose: 
  It gets the dimension of the ray, written in the file called "filename". Although nCol is always fixed to 18 we keep it for back compatibility reason.
  It is used to Allocate ray before reading it. (see next).
input
 . int        *nCol     : nCol in the new version is always 18, but for back-compatibility reason here is mantained.
 . int        *nPoint
 . char       *filename
output
 . none direct, but nCol and nPoint will be changed, and will survive after the function since one passes the pointers


CShadowReadRay (CShadowWriteRay)
purpose:
  It reads (writes) ray from (to) the file called filename.
input
 . double     *ray
 . int         nCol
 . int         nPoint
 . char       *filename
output
 . none


CShadowSourceGeom
purpose:
  It generates ray according to the geometrical source src.
input
 . poolSource *src : it must describe a geometrical source
 . double     *ray
output
 . none, but ray is changed.


CShadowSourceSync
purpose:
  It generates ray according to the synchrotron source src.
input
 . poolSource *src : it must describe a synchrotron source
 . double     *ray
output
 . none, but ray is changed.


CShadowTrace
purpose
  It propagates ray from the input state to final according to the optical element oe.
input
 . poolOE     *oe
 . double     *ray
 . int         nPoint
 . int         iCount : counter of optical element mantained for back-compatibility reason. old writing to file strategy.
output
 . none, but ray is changed.


CShadowFFresnel2D
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
 
 
C++ - LAYER (see ShadowMask.hpp, ShadowMask.cpp)

exposed classes:

 - Source
 - OE
 - Ray

all defined in file: ShadowMask.hpp


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


Ray
members
 . int         nCol         : protected, for back-compatibility reasons.
 . int         nPoint       : protected, number of rays
 . double     *rays         : public, pointer the array of data.
constructors, destructor and functions
 . Ray()                    : simple constructor
 . ~Ray()                   : destructor, use free(rays)
 . Ray(const Ray& r)        : copy-constructor
 . init(int col, int point) : <-- I want to make it protected
 . read(char* filename)     : read values from the file filename, using CShadowGetDimRay and CShadowReadRay
 . write(char* filename)    : write values to file filename, using CShadowWriteRay
 . genSource(Source* src)   : generates rays according to src (doesn't matter if src is geometrical or synchrotron), using CShadowSourceGeom or CShadowSourceSync
 . trace(OE* oe, int count) : propagates rays according to oe, using CShadowTrace
 . ffresnel2D(double dist, dComplex* E, pixel* xp, pixel* zp) : use CShadowFFresnel2D to generate E. E must be setup outside.


python - LAYER (see shadow_python.h, shadow_python.c)

module Shadow:
 - class Source
 - class OE
 - class Ray
 - class Image

all declared in shadow_python.h and defined in shadow_python.c


Source
members
listed thank to the MACRO in ShadowMaskSource.def
methods
 . read                     : input = (string f), output = (). It reads the source from file named f.
 . write                    : input = (string f), output = (). It writes the source to file named f.
 . getters and setters      : used, the members type cannot be modified, a cast is always done if the wrong type is passed.
 

OE
members
listed thank to the MACRO in ShadowMaskOE.def
methods
 . read                     : input = (string f), output = (). It reads the source from file named f.
 . write                    : input = (string f), output = (). It writes the source to file named f.
 . getters and setters      : used, the members type cannot be modified, a cast is always done if the wrong type is passed.
                              if the member is an array, here it will appear as a numpy array. 

Ray
members
 . rays                     : numpy array holding the data.
methods
 . read                     : input = (string f), output = (). It reads the rays from file named f.
 . write                    : input = (string f), output = (). It writes the rays to file named f.
 . genSource                : input = (Source s), output = (). It generates the rays according to Source s.
 . trace                    : input = (OE oe, int32 i), output = (). It traces rays according to OE oe. i is the OE number.
 . getters and setters      : not used.


Image
members
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


IDL - Layer (see IDL_Shadow.c, IDL_ShadowLoader.h, IDL_ShadowLoader.c and IDL_Shadow.dlm)
procedures
 . PRINTSOURCE              : input = (IDL_Structures s, IDL_string f) write the structure equivalent to Source into the file f.
 . PRINTOE                  : input = (IDL_Structures o, IDL_string f) write the structure equivalent to OE into the file f.
functions                   
 . GENSOURCEGEOMETRIC       : input = (IDL_Structures s) output = (IDL_array ray) 
                              generate the ray from the structure s equivalent to a geometric Source
 . GENSOURCESYNCHROTRON     : input = (IDL_Structures s) output = (IDL_array ray) 
                              generate the ray from the structure s equivalent to a synchrotron Source
 . TRACE                    : input = (IDL_Structures o, IDL_array ray, IDL_LONG i) output = (ray) 
                              trace ray according to the structures o equivalent to an OE 
