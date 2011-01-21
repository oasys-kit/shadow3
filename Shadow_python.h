
#ifndef __SHADOW_PYTHON__
#define __SHADOW_PYTHON__

/*  Python dependencies  */
#include <Python.h>
#include <structmember.h>
#include <numpy/arrayobject.h>

/*  C-Layer dependencies  */
#include "ShadowMask.h"

/*  Standard library included  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ShadowRay_CheckExact(op) (Py_TYPE(op) == &ShadowRayType)
#define ShadowSource_CheckExact(op) (Py_TYPE(op) == &ShadowSourceType)
#define ShadowOE_CheckExact(op) (Py_TYPE(op) == &ShadowOEType)

typedef struct {
  PyObject_HEAD
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ctype name;
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) PyObject* name;
#include "ShadowMaskSource.def"
} Shadow_Source;

/*  Definition of the functions  */
void PySourceToSource ( Shadow_Source*, poolSource* );
void SourceToPySource ( poolSource*, Shadow_Source* );
static void Source_dealloc ( Shadow_Source* );
static PyObject* Source_new ( PyTypeObject*, PyObject*, PyObject* );
static int Source_init ( Shadow_Source*, PyObject*, PyObject* );
static PyObject* Source_read ( Shadow_Source*, PyObject* );
static PyObject* Source_write ( Shadow_Source*, PyObject* );
static PyObject* Source_SpacePoint ( Shadow_Source* );
static PyObject* Source_SpaceRectangle ( Shadow_Source*, PyObject* );
static PyObject* Source_SpaceEllipse ( Shadow_Source*, PyObject* );
static PyObject* Source_SpaceGaussian ( Shadow_Source*, PyObject* );
static PyObject* Source_SpaceDepthNone ( Shadow_Source* );
static PyObject* Source_SpaceDepthUniform ( Shadow_Source*, PyObject* );
static PyObject* Source_SpaceDepthGaussian ( Shadow_Source*, PyObject* );
static PyObject* Source_AngleFlat ( Shadow_Source*, PyObject* );
static PyObject* Source_AngleUniform ( Shadow_Source*, PyObject* );
static PyObject* Source_AngleGaussian ( Shadow_Source*, PyObject* );
static PyObject* Source_AngleConical ( Shadow_Source*, PyObject* );
static PyObject* Source_EnergySingle ( Shadow_Source*, PyObject* );
static PyObject* Source_EnergySeveral ( Shadow_Source*, PyObject* );
static PyObject* Source_EnergyUniform ( Shadow_Source*, PyObject* );
static PyObject* Source_EnergyRelative ( Shadow_Source*, PyObject* );

#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* Source_get_##name(Shadow_Source*, void*);
#include "ShadowMaskSource.def"
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int Source_set_##name(Shadow_Source*, PyObject*, void*);
#include "ShadowMaskSource.def"


typedef struct {
  PyObject_HEAD
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ctype name;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) PyObject* name;
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) PyObject* name;
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) PyObject* name;
#include "ShadowMaskOE.def"
} Shadow_OE;

void PyOEToOE ( Shadow_OE*, poolOE* );
void OEToPyOE ( poolOE*, Shadow_OE* );
static void OE_dealloc ( Shadow_OE* );
static PyObject* OE_new ( PyTypeObject*, PyObject*, PyObject* );
static int OE_init ( Shadow_OE*, PyObject*, PyObject* );
static PyObject* OE_read ( Shadow_OE*, PyObject* );
static PyObject* OE_write ( Shadow_OE*, PyObject* );
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* OE_get_##name(Shadow_OE*, void*);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue)
#include "ShadowMaskOE.def"
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int OE_set_##name(Shadow_OE*, PyObject*, void*);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue)
#include "ShadowMaskOE.def"


typedef struct {
  PyObject_HEAD
  PyArrayObject* rays;
} Shadow_Ray;

static void Ray_dealloc ( Shadow_Ray* );
static PyObject* Ray_new ( PyTypeObject*, PyObject*, PyObject* );
static int Ray_init ( Shadow_Ray*, PyObject*, PyObject* );
static PyObject* Ray_read ( Shadow_Ray*, PyObject* );
static PyObject* Ray_write ( Shadow_Ray*, PyObject* );
static PyObject* Ray_genSource ( Shadow_Ray*, PyObject* );
static PyObject* Ray_trace ( Shadow_Ray*, PyObject* );

char RayRays_doc[2000] = "";

const char Ray_doc[2000] = "";

typedef struct {
  PyObject_HEAD
  double xmax;
  double xmin;
  double zmax;
  double zmin;
  double distance;
  int npixel_x;
  int npixel_z;
  PyArrayObject* image;
  PyArrayObject* intensity;
} Shadow_Image;

static void Image_dealloc ( Shadow_Image* );
static PyObject* Image_new ( PyTypeObject*, PyObject*, PyObject* );
static int Image_init ( Shadow_Image*, PyObject*, PyObject* );
static PyObject* Image_fresnel2D ( Shadow_Image*, PyObject* );

char ImageXmax_doc[2000] = "Horizontal maximum position of image (cm)";
char ImageXmin_doc[2000] = "Horizontal minimum position of image (cm)";
char ImageZmax_doc[2000] = "Vertical maximum position of image (cm)";
char ImageZmin_doc[2000] = "Vertical minimum position of image (cm)";
char ImageDistance_doc[2000] = " distance of image (cm)";
char ImageNpixelX_doc[2000] = "number of pixels along the horizontal edge of the image";
char ImageNpixelZ_doc[2000] = "number of pixels along the vertical edge of the image";
char ImageImage_doc[2000] = "Electrical field component registered in the image.\n\tE(npixel_x, npixel_z, 3)";
char ImageIntensity_doc[2000] = "Intensity not normalized of diffracted beam on the image I(npixel_x, npixel_z)";
const char Image_doc[2000] = "Shadow.Image class helps the user in performing optic calculations with Shadow3.";


static PyObject* saveRay ( PyObject*, PyObject* );




#endif
