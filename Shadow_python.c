
#include "Shadow_python.h"
#include "ShadowMask.h"

/***************************************************************************
 *         Shadow_Source Python Object
 *
 *
 *
 *
 *
 ***************************************************************************/

void PySourceToSource ( Shadow_Source* pySrc, poolSource* src )
{
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) src->name = pySrc->name;
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) strncpy(src->name, PyString_AsString(pySrc->name), STRLEN);
#include "ShadowMaskSource.def"
}

void SourceToPySource ( poolSource* src, Shadow_Source* pySrc )
{
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) pySrc->name = src->name;
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) pySrc->name = PyString_FromString(src->name);
#include "ShadowMaskSource.def"
}

static void Source_dealloc ( Shadow_Source* self )
{
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) if(self->name!=NULL) Py_DECREF(self->name);
#include "ShadowMaskSource.def"
  self->ob_type->tp_free ( ( PyObject* ) self );
}

static PyObject* Source_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  Shadow_Source* self;
  self = ( Shadow_Source* ) type->tp_alloc ( type, 0 );
  if ( self != NULL ) {
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) self->name=defvalue;
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) self->name = NULL;
#include "ShadowMaskSource.def"
  }
  return ( PyObject* ) self;
}

static int Source_init ( Shadow_Source* self, PyObject* args, PyObject* kwds )
{
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) self->name = PyString_FromString(defvalue);
#include "ShadowMaskSource.def"
  return 0;
}

static PyObject* Source_read ( Shadow_Source* self, PyObject* args )
{
  poolSource* src = NULL;
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  src = ( poolSource* ) malloc ( sizeof ( poolSource ) );
  CShadowPoolSourceLoad ( src, ( char* ) FileName );
  SourceToPySource ( src, self );
  free ( src );

  Py_RETURN_NONE;
}

static PyObject* Source_write ( Shadow_Source* self, PyObject* args )
{
  poolSource* src;
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  src = ( poolSource* ) malloc ( sizeof ( poolSource ) );
  PySourceToSource ( self, src );
  CShadowPoolSourceWrite ( src, ( char* ) FileName );
  free ( src );

  Py_RETURN_NONE;
}

//different point of view

//instead spatial has to be
static PyObject* Source_SpacePoint ( Shadow_Source* self )
{
  self->FDISTR = 0;

  Py_RETURN_NONE;
}

static PyObject* Source_SpaceRectangle ( Shadow_Source* self, PyObject* args )
{
  double x, z;
  if ( !PyArg_ParseTuple ( args, "dd", &x, &z ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be two doubles!" );
    return NULL;
  }
  self->FDISTR = 1;
  self->WXSOU = x;
  self->WZSOU = z;

  Py_RETURN_NONE;
}

static PyObject* Source_SpaceEllipse ( Shadow_Source* self, PyObject* args )
{
  double x, z;
  if ( !PyArg_ParseTuple ( args, "dd", &x, &z ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be two doubles!" );
    return NULL;
  }
  self->FDISTR = 2;
  self->WXSOU = x;
  self->WZSOU = z;

  Py_RETURN_NONE;
}

static PyObject* Source_SpaceGaussian ( Shadow_Source* self, PyObject* args )
{
  double x, z;
  if ( !PyArg_ParseTuple ( args, "dd", &x, &z ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be two doubles!" );
    return NULL;
  }
  self->FDISTR = 3;
  self->SIGMAX = x;
  self->SIGMAZ = z;

  Py_RETURN_NONE;
}

static PyObject* Source_SpaceDepthNone ( Shadow_Source* self )
{
  self->FSOURCE_DEPTH = 1;

  Py_RETURN_NONE;
}

static PyObject* Source_SpaceDepthUniform ( Shadow_Source* self, PyObject* args )
{
  double y;
  if ( !PyArg_ParseTuple ( args, "d", &y ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a double!" );
    return NULL;
  }
  self->FSOURCE_DEPTH = 2;
  self->WYSOU = y;

  Py_RETURN_NONE;
}

static PyObject* Source_SpaceDepthGaussian ( Shadow_Source* self, PyObject* args )
{
  double y;
  if ( !PyArg_ParseTuple ( args, "d", &y ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a double!" );
    return NULL;
  }
  self->FSOURCE_DEPTH = 3;
  self->SIGMAY = y;
  Py_RETURN_NONE;
}

static PyObject* Source_AngleFlat ( Shadow_Source* self, PyObject* args )
{
  double x1, x2, z1, z2;
  if ( !PyArg_ParseTuple ( args, "dddd", &x1, &x2, &z1, &z2 ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be four doubles!" );
    return NULL;
  }
  self->FDISTR = 1;
  self->HDIV1 = x1;
  self->HDIV2 = z1;
  self->VDIV1 = x2;
  self->VDIV2 = z2;

  Py_RETURN_NONE;
}

static PyObject* Source_AngleUniform ( Shadow_Source* self, PyObject* args )
{
  double x1, x2, z1, z2;
  if ( !PyArg_ParseTuple ( args, "dddd", &x1, &x2, &z1, &z2 ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be four doubles!" );
    return NULL;
  }
  self->FDISTR = 2;
  self->HDIV1 = x1;
  self->HDIV2 = z1;
  self->VDIV1 = x2;
  self->VDIV2 = z2;

  Py_RETURN_NONE;
}

static PyObject* Source_AngleGaussian ( Shadow_Source* self, PyObject* args )
{
  double x1, x2, x3, z1, z2, z3;
  if ( !PyArg_ParseTuple ( args, "dddddd", &x1, &x2, &x3, &z1, &z2, &z3 ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be six doubles!" );
    return NULL;
  }
  self->FDISTR = 3;
  self->HDIV1 = x1;
  self->HDIV2 = z1;
  self->VDIV1 = x2;
  self->VDIV2 = z2;
  self->SIGDIX = x3;
  self->SIGDIZ = z3;

  Py_RETURN_NONE;
}

static PyObject* Source_AngleConical ( Shadow_Source* self, PyObject* args )
{
  double x, z;
  if ( !PyArg_ParseTuple ( args, "dd", &x, &z ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be two doubles!" );
    return NULL;
  }
  self->FDISTR = 4;
  self->CONE_MAX = x;
  self->CONE_MIN = z;

  Py_RETURN_NONE;
}

static PyObject* Source_EnergySingle ( Shadow_Source* self, PyObject* args )
{
  double e1;
  if ( !PyArg_ParseTuple ( args, "d", &e1 ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a double!" );
    return NULL;
  }
  self->F_COLOR = 1;
  self->N_COLOR = 1;
  self->PH1 = e1;
  Py_RETURN_NONE;
}

static PyObject* Source_EnergySeveral ( Shadow_Source* self, PyObject* args )
{
  int ne;
  double e1, e2, e3, e4, e5, e6, e7, e8, e9, e10;
  if ( !PyArg_ParseTuple ( args, "idd|dddddddd", &ne, &e1, &e2, &e3, &e4, &e5, &e6, &e7, &e8, &e9, &e10 ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be an integer and two or more (up to ten) doubles!" );
    return NULL;
  }
  if ( ne<2||ne>10 ) {
    PyErr_SetString ( PyExc_TypeError, "integer argument should be between two and ten!" );
    return NULL;
  }
  self->F_COLOR = 2;
  self->N_COLOR = ne;
  if ( 0<ne ) {
    self->PH1  = e1 ;
  }
  if ( 1<ne ) {
    self->PH2  = e2 ;
  }
  if ( 2<ne ) {
    self->PH3  = e3 ;
  }
  if ( 3<ne ) {
    self->PH4  = e4 ;
  }
  if ( 4<ne ) {
    self->PH5  = e5 ;
  }
  if ( 5<ne ) {
    self->PH6  = e6 ;
  }
  if ( 6<ne ) {
    self->PH7  = e7 ;
  }
  if ( 7<ne ) {
    self->PH8  = e8 ;
  }
  if ( 8<ne ) {
    self->PH9  = e9 ;
  }
  if ( 9<ne ) {
    self->PH10 = e10;
  }
  Py_RETURN_NONE;
}

static PyObject* Source_EnergyUniform ( Shadow_Source* self, PyObject* args )
{
  double e1, e2;
  if ( !PyArg_ParseTuple ( args, "dd", &e1, &e2 ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a double!" );
    return NULL;
  }
  self->F_COLOR = 3;
  self->N_COLOR = 2;
  self->PH1 = e1;
  self->PH2 = e2;
  Py_RETURN_NONE;
}

static PyObject* Source_EnergyRelative ( Shadow_Source* self, PyObject* args )
{
  int ne;
  double e1, e2, e3, e4, e5, e6, e7, e8, e9, e10;
  double r1, r2, r3, r4, r5, r6, r7, r8, r9, r10;
  if ( !PyArg_ParseTuple ( args, "idddd|dddddddddddddddd", &ne,
                                                                                                     &e1, &r1, &e2, &r2, &e3, &r3, &e4, &r4, &e5, &r5,
                                                                                                     &e6, &r6, &e7, &r7, &e8, &r8, &e9, &r9, &e10, &r10 ) ) {
    PyErr_SetString ( PyExc_TypeError, "arguments should be an integer and two or more (up to ten) doubles!" );
    return NULL;
  }
  if ( ne<2||ne>10 ) {
    PyErr_SetString ( PyExc_TypeError, "integer argument should be between two and ten!" );
    return NULL;
  }
  self->F_COLOR = 4;
  self->N_COLOR = ne;
  if ( 0<ne ) {
    self->PH1  = e1 ;
    self->RL1  = r1 ;
  }
  if ( 1<ne ) {
    self->PH2  = e2 ;
    self->RL1  = r2 ;
  }
  if ( 2<ne ) {
    self->PH3  = e3 ;
    self->RL1  = r3 ;
  }
  if ( 3<ne ) {
    self->PH4  = e4 ;
    self->RL1  = r4 ;
  }
  if ( 4<ne ) {
    self->PH5  = e5 ;
    self->RL1  = r5 ;
  }
  if ( 5<ne ) {
    self->PH6  = e6 ;
    self->RL1  = r6 ;
  }
  if ( 6<ne ) {
    self->PH7  = e7 ;
    self->RL1  = r7 ;
  }
  if ( 7<ne ) {
    self->PH8  = e8 ;
    self->RL1  = r8 ;
  }
  if ( 8<ne ) {
    self->PH9  = e9 ;
    self->RL1  = r9 ;
  }
  if ( 9<ne ) {
    self->PH10 = e10;
    self->RL1  = r10;
  }
  Py_RETURN_NONE;
}



#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* Source_get_##name(Shadow_Source* self, void* closure) \
{ \
  Py_INCREF(self->name); \
  return self->name; \
}
#include "ShadowMaskSource.def"

#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int Source_set_##name(Shadow_Source* self, PyObject* value, void* closure) \
{ \
  if(value == NULL) { \
    PyErr_SetString(PyExc_TypeError, "Cannot delete attribute"); \
    return -1; \
  } \
  if(! PyString_Check(value)) { \
    PyErr_SetString(PyExc_TypeError, "The value passed must be a string"); \
    return -1; \
  } \
  if(PyString_Size(value)>=1024) { \
    PyErr_SetString(PyExc_TypeError, "the string value is too long"); \
    return -1; \
  } \
  Py_DECREF(self->name); \
  Py_INCREF(value); \
  self->name=value; \
  return 0; \
}
#include "ShadowMaskSource.def"

static PyMemberDef Source_members[] = {
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) {#name,pytype,offsetof(Shadow_Source,name),0,#name},
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue)
#include "ShadowMaskSource.def"
  {NULL}                                             /* Sentinel          */
};

static PyMethodDef Source_methods[] = {
  {"read" , ( PyCFunction ) Source_read , METH_VARARGS, "read Shadow.Source from a file"},
  {"write", ( PyCFunction ) Source_write, METH_VARARGS, "write Shadow.Source on a file" },
  {"spacePoint", ( PyCFunction ) Source_SpacePoint, METH_NOARGS, "define Source spacial distribution to be a point"},
  {"spaceRectangle", ( PyCFunction ) Source_SpaceRectangle, METH_VARARGS, "define Source spacial distribution to be in a rectangle"},
  {"spaceEllipse", ( PyCFunction ) Source_SpaceEllipse, METH_VARARGS, "define Source spacial distribution to be in an ellipse"},
  {"spaceGaussian", ( PyCFunction ) Source_SpaceGaussian, METH_VARARGS, "define Source spacial distribution to be a gaussian"},
  {"spaceDepthNone", ( PyCFunction ) Source_SpaceDepthNone, METH_NOARGS, "define Source spacial distribution to not have depth"},
  {"spaceDepthUniform", ( PyCFunction ) Source_SpaceDepthUniform, METH_VARARGS, "define Source spacial distribution to have a uniform depth"},
  {"spaceDepthGaussian", ( PyCFunction ) Source_SpaceDepthGaussian, METH_VARARGS, "define Source spacial distribution to have a gaussian depth"},
  {"angleFlat", ( PyCFunction ) Source_AngleFlat, METH_VARARGS, "define Source angular distribution to be flat"},
  {"angleUniform", ( PyCFunction ) Source_AngleUniform, METH_VARARGS, "define Source angular distribution to be uniform"},
  {"angleGaussian", ( PyCFunction ) Source_AngleGaussian, METH_VARARGS, "define Source angular distribution to be gaussian"},
  {"angleConical", ( PyCFunction ) Source_AngleConical, METH_VARARGS, "define Source angular distribution to be in a Cone"},
  {"energySingle", ( PyCFunction ) Source_EnergySingle, METH_VARARGS, "define Source energy distribution to take a single energy value"},
  {"energySeveral", ( PyCFunction ) Source_EnergySeveral, METH_VARARGS, "define Source energy distribution to take several energy values (up to ten)"},
  {"energyUniform", ( PyCFunction ) Source_EnergyUniform, METH_VARARGS, "define Source energy distribution to be uniform"},
  {"energyRelative", ( PyCFunction ) Source_EnergyRelative, METH_VARARGS, "define Source energy distribution to take several energy values (up to ten) with aside the values of their weight"},
  {NULL}                                             /* Sentinel          */
};

static PyGetSetDef Source_getseters[] = {
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
  {#name, (getter) Source_get_##name, (setter) Source_set_##name, #name, NULL},
#include "ShadowMaskSource.def"
  {NULL}                                             /* Sentinel          */
};

static PyTypeObject ShadowSourceType = {
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
  "Shadow.Source",                                   /* tp_name           */
  sizeof ( Shadow_Source ),                          /* tp_basicsize      */
  0,                                                 /* tp_itemsize       */
  ( destructor ) Source_dealloc,                     /* tp_dealloc        */
  0,                                                 /* tp_print          */
  0,                                                 /* tp_getattr        */
  0,                                                 /* tp_setattr        */
  0,                                                 /* tp_compare        */
  0,                                                 /* tp_repr           */
  0,                                                 /* tp_as_number      */
  0,                                                 /* tp_as_sequence    */
  0,                                                 /* tp_as_mapping     */
  0,                                                 /* tp_hash           */
  0,                                                 /* tp_call           */
  0,                                                 /* tp_str            */
  0,                                                 /* tp_getattro       */
  0,                                                 /* tp_setattro       */
  0,                                                 /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,          /* tp_flags          */
  "Source object",                                   /* tp_doc            */
  0,                                                 /* tp_traverse       */
  0,                                                 /* tp_clear          */
  0,                                                 /* tp_richcompare    */
  0,                                                 /* tp_weaklistoffset */
  0,                                                 /* tp_iter           */
  0,                                                 /* tp_iternext       */
  Source_methods,                                    /* tp_methods        */
  Source_members,                                    /* tp_members        */
  Source_getseters,                                  /* tp_getset         */
  0,                                                 /* tp_base           */
  0,                                                 /* tp_dict           */
  0,                                                 /* tp_descr_get      */
  0,                                                 /* tp_descr_set      */
  0,                                                 /* tp_dictoffset     */
  ( initproc ) Source_init,                          /* tp_init           */
  0,                                                 /* tp_alloc          */
  Source_new,                                        /* tp_new            */
};



/***************************************************************************
 *         Shadow_OE Python Object
 *
 *
 *
 *
 *
 ***************************************************************************/

void PyOEToOE ( Shadow_OE* pyOe, poolOE* oe )
{
  int i;
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) oe->name = pyOe->name;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) strncpy(oe->name, PyString_AsString(pyOe->name), STRLEN);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
  for(i=0;i<ADIM;i++) oe->name[i] = *( (ctype*) ( PyArray_GETPTR1(pyOe->name, i) ) );
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
  for(i=0;i<ADIM;i++) strncpy(oe->name[i], (char*) ( PyArray_GETPTR1(pyOe->name, i) ), STRLEN);
#include "ShadowMaskOE.def"
}

void OEToPyOE ( poolOE* oe, Shadow_OE* pyOe )
{
  int i;
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) pyOe->name = oe->name;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) pyOe->name = PyString_FromString(oe->name);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
  for(i=0;i<ADIM;i++) *( (ctype*) ( PyArray_GETPTR1(pyOe->name, i) ) ) = oe->name[i];
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
  for(i=0;i<ADIM;i++) strncpy((char*) ( PyArray_GETPTR1(pyOe->name, i) ), oe->name[i], STRLEN);
#include "ShadowMaskOE.def"
}


static void OE_dealloc ( Shadow_OE* self )
{
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) if(self->name!=NULL) Py_DECREF(self->name);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) if(self->name!=NULL) Py_DECREF(self->name);
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) if(self->name!=NULL) Py_DECREF(self->name);
#include "ShadowMaskOE.def"
  self->ob_type->tp_free ( ( PyObject* ) self );
}

static PyObject* OE_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  Shadow_OE* self;
  self = ( Shadow_OE* ) type->tp_alloc ( type, 0 );
  if ( self != NULL ) {
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) self->name = defvalue;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) self->name = NULL;
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) self->name = NULL;
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) self->name = NULL;
#include "ShadowMaskOE.def"
  }
  return ( PyObject* ) self;
}

static int OE_init ( Shadow_OE* self, PyObject* args, PyObject* kwds )
{
  int i;
  int nd = 1;
  npy_intp dims[nd];
  npy_intp strides_int[nd];
  npy_intp strides_double[nd];
  npy_intp strides_char[nd];
  dims[0] = 10;
  strides_int[0] = sizeof ( int );
  strides_double[0] = sizeof ( double );
  strides_char[0] = 1024*sizeof ( char );
#define ski NPY_INT32
#define skr NPY_FLOAT64
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) self->name = PyString_FromString(defvalue);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
  self->name = PyArray_New(&PyArray_Type, nd, dims, fkind, strides_##ctype, NULL, sizeof(ctype), NPY_CARRAY|NPY_OWNDATA, NULL); \
  for(i=0;i<ADIM;i++) *( (ctype*)( PyArray_GETPTR1(self->name,i) ) ) = defvalue;
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
  self->name = PyArray_New(&PyArray_Type, nd, dims, NPY_STRING, strides_##ctype, NULL, 1024*sizeof(ctype), NPY_CARRAY|NPY_OWNDATA, NULL); \
  for(i=0;i<ADIM;i++) strncpy( (ctype*)( PyArray_GETPTR1(self->name,i) ) , defvalue, STRLEN);
#include "ShadowMaskOE.def"
#undef ski
#undef skr
  return 0;
}

static PyObject* OE_read ( Shadow_OE* self, PyObject* args )
{
  poolOE* oe;
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  oe = ( poolOE* ) malloc ( sizeof ( poolOE ) );
  CShadowPoolOELoad ( oe, ( char* ) FileName );
  OEToPyOE ( oe, self );

  free ( oe );

  Py_RETURN_NONE;
}

static PyObject* OE_write ( Shadow_OE* self, PyObject* args )
{
  poolOE* oe;
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  oe = ( poolOE* ) malloc ( sizeof ( poolOE ) );
  PyOEToOE ( self, oe );
  CShadowPoolOEWrite ( oe, ( char* ) FileName );

  free ( oe );

  Py_RETURN_NONE;
}


#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void* closure) \
{ \
  Py_INCREF(self->name); \
  return self->name; \
}
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue)
#include "ShadowMaskOE.def"


#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  if(value == NULL) { \
    PyErr_SetString(PyExc_TypeError, "Cannot delete attribute"); \
    return -1; \
  } \
  if(! PyString_Check(value)) { \
    PyErr_SetString(PyExc_TypeError, "The value passed must be a string"); \
    return -1; \
  } \
  if(PyString_Size(value)>=1024) { \
    PyErr_SetString(PyExc_TypeError, "the string value is too long"); \
    return -1; \
  } \
  Py_DECREF(self->name); \
  Py_INCREF(value); \
  self->name=value; \
  return 0; \
}
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue)
#include "ShadowMaskOE.def"

static PyMemberDef OE_members[] = {
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) {#name,pytype,offsetof(Shadow_OE,name),0,#name},
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue)
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) {#name,T_OBJECT_EX,offsetof(Shadow_OE,name),0,#name},
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) {#name,T_OBJECT_EX,offsetof(Shadow_OE,name),0,#name},
#include "ShadowMaskOE.def"
  {NULL}                                             /* Sentinel          */
};

static PyMethodDef OE_methods[] = {
  {"read" , ( PyCFunction ) OE_read , METH_VARARGS, "read Shadow.OE from a file"},
  {"write", ( PyCFunction ) OE_write, METH_VARARGS, "write Shadow.OE on a file" },
  {NULL}                                             /* Sentinel          */
};

static PyGetSetDef OE_getseters[] = {
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
  {#name, (getter) OE_get_##name, (setter) OE_set_##name, #name, NULL},
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue)
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue)
#include "ShadowMaskOE.def"
  {NULL}                                             /* Sentinel          */
};

static PyTypeObject ShadowOEType = {
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
  "Shadow.OE",                                       /* tp_name           */
  sizeof ( Shadow_OE ),                              /* tp_basicsize      */
  0,                                                 /* tp_itemsize       */
  ( destructor ) OE_dealloc,                         /* tp_dealloc        */
  0,                                                 /* tp_print          */
  0,                                                 /* tp_getattr        */
  0,                                                 /* tp_setattr        */
  0,                                                 /* tp_compare        */
  0,                                                 /* tp_repr           */
  0,                                                 /* tp_as_number      */
  0,                                                 /* tp_as_sequence    */
  0,                                                 /* tp_as_mapping     */
  0,                                                 /* tp_hash           */
  0,                                                 /* tp_call           */
  0,                                                 /* tp_str            */
  0,                                                 /* tp_getattro       */
  0,                                                 /* tp_setattro       */
  0,                                                 /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,          /* tp_flags          */
  "OE object",                                       /* tp_doc            */
  0,                                                 /* tp_traverse       */
  0,                                                 /* tp_clear          */
  0,                                                 /* tp_richcompare    */
  0,                                                 /* tp_weaklistoffset */
  0,                                                 /* tp_iter           */
  0,                                                 /* tp_iternext       */
  OE_methods,                                        /* tp_methods        */
  OE_members,                                        /* tp_members        */
  OE_getseters,                                      /* tp_getset         */
  0,                                                 /* tp_base           */
  0,                                                 /* tp_dict           */
  0,                                                 /* tp_descr_get      */
  0,                                                 /* tp_descr_set      */
  0,                                                 /* tp_dictoffset     */
  ( initproc ) OE_init,                              /* tp_init           */
  0,                                                 /* tp_alloc          */
  OE_new,                                            /* tp_new            */
};

/***************************************************************************
 *         Shadow_Ray Python Object
 *
 *
 *
 *
 *
 ***************************************************************************/

static void Ray_dealloc ( Shadow_Ray* self )
{
  if ( self->rays!=NULL )
    Py_DECREF ( self->rays );
  self->ob_type->tp_free ( ( PyObject* ) self );
}

static PyObject* Ray_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  Shadow_Ray* self;
  self = ( Shadow_Ray* ) type->tp_alloc ( type, 0 );
  self->rays = NULL;

  return ( PyObject* ) self;
}

static int Ray_init ( Shadow_Ray* self, PyObject* args, PyObject* kwds )
{
  return 0;
}

static PyObject* Ray_read ( Shadow_Ray* self, PyObject* args )
{
  int nCol, nPoint;
  int nd = 2;
  npy_intp dims[nd];
  npy_intp strides[nd];
  const char *FileName;

  FILE* TestFile;

  dims[1] = 18;
  strides[0] = 18*sizeof ( double );
  strides[1] = sizeof ( double );


  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }

  TestFile = fopen ( ( char* ) FileName,"r" );
  if ( TestFile==NULL ) {
    PyErr_SetString ( PyExc_TypeError, "file cannot be opened!" );
    return NULL;
  }
  fclose ( TestFile );

  // file is conform test?
  CShadowGetDimRay ( &nCol, &nPoint, ( char* ) FileName );

  dims[0] = nPoint;

  if ( self->rays!=NULL )
    Py_DECREF ( self->rays );
  self->rays = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, nd, dims, NPY_FLOAT64, strides, NULL, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );
  Py_INCREF ( self->rays );
  CShadowReadRay ( ( double* ) ( self->rays->data ), nCol, nPoint, ( char* ) FileName );

  Py_RETURN_NONE;
}

static PyObject* Ray_write ( Shadow_Ray* self, PyObject* args )
{
  int nPoint, nCol;
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  // file already exist warning?

  if ( self->rays==NULL ) {
    PyErr_SetString ( PyExc_TypeError, "rays is not set yet" );
    return NULL;
  }

  nPoint = self->rays->dimensions[0];
  nCol = 18;
  CShadowWriteRay ( ( double* ) ( self->rays->data ), nCol, nPoint, ( char* ) FileName );

  Py_RETURN_NONE;
}

static PyObject* Ray_genSource ( Shadow_Ray* self, PyObject* args )
{
  poolSource* src;
  Shadow_Source* pySrc = NULL;
  int nd = 2;
  npy_intp dims[nd];
  npy_intp strides[nd];

  if ( !PyArg_ParseTuple ( args, "O", &pySrc ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    return NULL;
  }
  if ( !ShadowSource_CheckExact ( pySrc ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.Source instance" );
    return NULL;
  }

  src = ( poolSource* ) malloc ( sizeof ( poolSource ) );
  dims[1] = 18;
  strides[0] = 18*sizeof ( double );
  strides[1] = sizeof ( double );
  dims[0] = pySrc->NPOINT;

  if ( self->rays!=NULL )
    Py_DECREF ( self->rays );
  self->rays = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, nd, dims, NPY_FLOAT64, strides, NULL, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );

  PySourceToSource ( pySrc,src );
  if ( ( pySrc->FDISTR==4 ) || ( pySrc->FSOURCE_DEPTH==4 ) || ( pySrc->F_WIGGLER>0 ) ) {
    CShadowSourceSync ( src, ( double* ) ( self->rays->data ) );
  }
  else {
    CShadowSourceGeom ( src, ( double* ) ( self->rays->data ) );
  }

  free ( src );

  Py_RETURN_NONE;
}

static PyObject* Ray_trace ( Shadow_Ray* self, PyObject* args )
{
  int nPoint;
  int nCol;
  int iCount;
  poolOE* oe1;
  Shadow_OE* pyOe = NULL;


  if ( !PyArg_ParseTuple ( args, "Oi", &pyOe, &iCount ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    return NULL;
  }
  if ( !ShadowOE_CheckExact ( pyOe ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.OE instance" );
    return NULL;
  }

  if ( self->rays==NULL ) {
    PyErr_SetString ( PyExc_TypeError, "rays is empty" );
    return NULL;
  }

  oe1 = ( poolOE* ) malloc ( sizeof ( poolOE ) );
  nPoint = self->rays->dimensions[0];
  nCol = 18;
  PyOEToOE ( pyOe,oe1 );
  CShadowTrace ( oe1, ( double* ) ( self->rays->data ), nPoint, iCount );

  free ( oe1 );

  Py_RETURN_NONE;
}

static PyMemberDef Ray_members[] = {
  {"rays",T_OBJECT_EX,offsetof ( Shadow_Ray,rays ),0,"rays"},
  {NULL}
};

static PyMethodDef Ray_methods[] = {
  {"read" , ( PyCFunction ) Ray_read , METH_VARARGS, "read Shadow.Ray from a file"},
  {"write", ( PyCFunction ) Ray_write, METH_VARARGS, "write Shadow.Ray on a file" },
  {"genSource", ( PyCFunction ) Ray_genSource, METH_VARARGS, "generate rays from Source"},
  {"trace", ( PyCFunction ) Ray_trace, METH_VARARGS, "trace rays according to a given OE"},

  {NULL}                                             /* Sentinel          */
};

static PyTypeObject ShadowRayType = {
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
  "Shadow.Ray",                                      /* tp_name           */
  sizeof ( Shadow_Ray ),                             /* tp_basicsize      */
  0,                                                 /* tp_itemsize       */
  ( destructor ) Ray_dealloc,                        /* tp_dealloc        */
  0,                                                 /* tp_print          */
  0,                                                 /* tp_getattr        */
  0,                                                 /* tp_setattr        */
  0,                                                 /* tp_compare        */
  0,                                                 /* tp_repr           */
  0,                                                 /* tp_as_number      */
  0,                                                 /* tp_as_sequence    */
  0,                                                 /* tp_as_mapping     */
  0,                                                 /* tp_hash           */
  0,                                                 /* tp_call           */
  0,                                                 /* tp_str            */
  0,                                                 /* tp_getattro       */
  0,                                                 /* tp_setattro       */
  0,                                                 /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,          /* tp_flags          */
  "Ray object",                                      /* tp_doc            */
  0,                                                 /* tp_traverse       */
  0,                                                 /* tp_clear          */
  0,                                                 /* tp_richcompare    */
  0,                                                 /* tp_weaklistoffset */
  0,                                                 /* tp_iter           */
  0,                                                 /* tp_iternext       */
  Ray_methods,                                       /* tp_methods        */
  Ray_members,                                       /* tp_members        */
  0,                                                 /* tp_getset         */
  0,                                                 /* tp_base           */
  0,                                                 /* tp_dict           */
  0,                                                 /* tp_descr_get      */
  0,                                                 /* tp_descr_set      */
  0,                                                 /* tp_dictoffset     */
  ( initproc ) Ray_init,                             /* tp_init           */
  0,                                                 /* tp_alloc          */
  Ray_new,                                           /* tp_new            */
};

/***************************************************************************
 *         Shadow_Image Python Object
 *
 *
 *
 *
 *
 ***************************************************************************/

static void Image_dealloc ( Shadow_Image* self )
{
  if ( self->image!=NULL )
    Py_DECREF ( self->image );
  if ( self->intensity!=NULL )
    Py_DECREF ( self->intensity );
  self->ob_type->tp_free ( ( PyObject* ) self );
}

static PyObject* Image_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  Shadow_Image* self;
  self = ( Shadow_Image* ) type->tp_alloc ( type, 0 );
  self->xmax = 5.0;
  self->xmin = 0.0;
  self->zmax = 5.0;
  self->zmin = 0.0;
  self->distance = 100.0;
  self->npixel_x = 128;
  self->npixel_z = 128;
  self->image = NULL;
  self->intensity = NULL;
  return ( PyObject* ) self;
}

static int Image_init ( Shadow_Image* self, PyObject* args, PyObject* kwds )
{
  return 0;
}

static PyObject* Image_fresnel2D ( Shadow_Image* self, PyObject* args )
{
  Shadow_Ray* r;
  int nPoint;
  int i,j,k;

  int nd = 3;
  pixel *x;
  pixel *z;


  npy_intp dims[nd];
  npy_intp strides[nd];

  if ( !PyArg_ParseTuple ( args, "O", &r ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    return NULL;
  }

  if ( !ShadowRay_CheckExact ( r ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.Ray instance" );
    return NULL;
  }

  if ( r->rays == NULL ) {
    PyErr_SetString ( PyExc_TypeError, "rays field of Shadow.Ray instance is empty" );
    return NULL;
  }

  dims[0] = self->npixel_z;
  dims[1] = self->npixel_x;
  dims[2] = 3;

  strides[0] = dims[2]*dims[1]*2*sizeof ( double );
  strides[1] = dims[2]*2*sizeof ( double );
  strides[2] = 2*sizeof ( double );

  if ( self->image!=NULL )
    Py_DECREF ( self->image );
  self->image = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, nd, dims, NPY_COMPLEX128, strides, NULL, 2*sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );

  nd = 2;

  strides[0] = dims[1]*sizeof ( double );
  strides[1] = sizeof ( double );

  if ( self->intensity!=NULL )
    Py_DECREF ( self->intensity );
  self->intensity = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, nd, dims, NPY_FLOAT64, strides, NULL, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );

  nPoint = r->rays->dimensions[0];

  x = ( pixel* ) malloc ( sizeof ( pixel ) );
  z = ( pixel* ) malloc ( sizeof ( pixel ) );

  x->np = self->npixel_x;
  x->up = self->xmax;
  x->dn = self->xmin;

  z->np = self->npixel_z;
  z->up = self->zmax;
  z->dn = self->zmin;

  CShadowFFresnel2D ( ( double* ) ( r->rays->data ), nPoint, self->distance, ( dComplex* ) ( self->image->data ), x, z );

  for ( i=0;i<self->npixel_z;i++ ) {
    for ( j=0;j<self->npixel_x;j++ ) {
      * ( ( double* ) ( PyArray_GETPTR2 ( self->intensity, i, j ) ) ) = 0.0;
      for ( k=0;k<3;k++ ) {
        * ( ( double* ) ( PyArray_GETPTR2 ( self->intensity, i, j ) ) ) +=
          ( ( dComplex* ) ( PyArray_GETPTR3 ( self->image, i, j, k ) ) )->real *
          ( ( dComplex* ) ( PyArray_GETPTR3 ( self->image, i, j, k ) ) )->real +
          ( ( dComplex* ) ( PyArray_GETPTR3 ( self->image, i, j, k ) ) )->imag *
          ( ( dComplex* ) ( PyArray_GETPTR3 ( self->image, i, j, k ) ) )->imag ;
      }
    }
  }

  free ( x );
  free ( z );

  Py_RETURN_NONE;
}

static PyMemberDef Image_members[] = {
  {"xmax",T_DOUBLE,offsetof ( Shadow_Image,xmax ),0,ImageXmax_doc},
  {"xmin",T_DOUBLE,offsetof ( Shadow_Image,xmin ),0,ImageXmin_doc},
  {"zmax",T_DOUBLE,offsetof ( Shadow_Image,zmax ),0,ImageZmax_doc},
  {"zmin",T_DOUBLE,offsetof ( Shadow_Image,zmin ),0,ImageZmin_doc},
  {"distance",T_DOUBLE,offsetof ( Shadow_Image,distance ),0,ImageDistance_doc},
  {"npixel_x",T_INT,offsetof ( Shadow_Image,npixel_x ),0,ImageNpixelX_doc},
  {"npixel_z",T_INT,offsetof ( Shadow_Image,npixel_z ),0,ImageNpixelZ_doc},
  {"image",T_OBJECT_EX,offsetof ( Shadow_Image,image ),0,ImageImage_doc},
  {"intensity",T_OBJECT_EX,offsetof ( Shadow_Image,intensity ),0,ImageIntensity_doc},
  {NULL}
};


static PyMethodDef Image_methods[] = {
  {"fresnel2D" , ( PyCFunction ) Image_fresnel2D , METH_VARARGS, "create a diffraction diffraction image using a Shadow.Ray"},
  {NULL}                                             /* Sentinel          */
};


static PyTypeObject ShadowImageType = {
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
  "Shadow.Image",                                    /* tp_name           */
  sizeof ( Shadow_Image ),                           /* tp_basicsize      */
  0,                                                 /* tp_itemsize       */
  ( destructor ) Image_dealloc,                      /* tp_dealloc        */
  0,                                                 /* tp_print          */
  0,                                                 /* tp_getattr        */
  0,                                                 /* tp_setattr        */
  0,                                                 /* tp_compare        */
  0,                                                 /* tp_repr           */
  0,                                                 /* tp_as_number      */
  0,                                                 /* tp_as_sequence    */
  0,                                                 /* tp_as_mapping     */
  0,                                                 /* tp_hash           */
  0,                                                 /* tp_call           */
  0,                                                 /* tp_str            */
  0,                                                 /* tp_getattro       */
  0,                                                 /* tp_setattro       */
  0,                                                 /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,          /* tp_flags          */
  Image_doc,                                         /* tp_doc            */
  0,                                                 /* tp_traverse       */
  0,                                                 /* tp_clear          */
  0,                                                 /* tp_richcompare    */
  0,                                                 /* tp_weaklistoffset */
  0,                                                 /* tp_iter           */
  0,                                                 /* tp_iternext       */
  Image_methods,                                     /* tp_methods        */
  Image_members,                                     /* tp_members        */
  0,                                                 /* tp_getset         */
  0,                                                 /* tp_base           */
  0,                                                 /* tp_dict           */
  0,                                                 /* tp_descr_get      */
  0,                                                 /* tp_descr_set      */
  0,                                                 /* tp_dictoffset     */
  ( initproc ) Image_init,                           /* tp_init           */
  0,                                                 /* tp_alloc          */
  Image_new,                                         /* tp_new            */
};

/***************************************************************************
 *         Shadow general methods
 *
 *
 *
 *
 *
 ***************************************************************************/

static PyObject* saveRay ( PyObject* self, PyObject* args )
{
  PyObject* newRay;
  PyObject* oldRay;
  PyArrayObject* tmp;
  double* newdata;
  int i;

  if ( !PyArg_ParseTuple ( args, "O", &oldRay ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    return NULL;
  }

  if ( !ShadowRay_CheckExact ( oldRay ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.Ray instance" );
    return NULL;
  }

  newRay = Ray_new ( &ShadowRayType, NULL, NULL );

  if ( ( ( Shadow_Ray* ) ( oldRay ) )->rays!=NULL ) {
    tmp = ( ( Shadow_Ray* ) ( oldRay ) )->rays;
    newdata = ( double* ) malloc ( tmp->dimensions[0]*tmp->dimensions[1]*sizeof ( double ) );
    for ( i=0;i<tmp->dimensions[0]*tmp->dimensions[1];i++ )
      newdata[i] = ( ( double* ) ( tmp->data ) ) [i];
    ( ( Shadow_Ray* ) ( newRay ) )->rays = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, tmp->nd, tmp->dimensions, NPY_FLOAT64, tmp->strides,
                                                                            newdata, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );
  }

  return newRay;
}

/*  Shadow methods none  */

static PyMethodDef Shadow_methods[] = {
  {"saveRay" , ( PyCFunction ) saveRay , METH_VARARGS, "save Ray in a new instance Shadow.Ray"},
  {NULL}                                             /* Sentinel          */
};

/*  module init function  */

#ifndef PyMODINIT_FUNC /* declarations for DLL import/export */
#define PyMODINIT_FUNC void
#endif
PyMODINIT_FUNC
initShadow ( void )
{
  _import_array();
  PyObject* m;
  ShadowSourceType.ob_type = &PyType_Type;
  if ( PyType_Ready ( &ShadowSourceType ) < 0 )
    return;
  ShadowOEType.ob_type = &PyType_Type;
  if ( PyType_Ready ( &ShadowOEType ) < 0 )
    return;
  ShadowRayType.ob_type = &PyType_Type;
  if ( PyType_Ready ( &ShadowRayType ) < 0 )
    return;
  ShadowImageType.ob_type = &PyType_Type;
  if ( PyType_Ready ( &ShadowImageType ) < 0 )
    return;

  m = Py_InitModule3 ( "Shadow", Shadow_methods, "Extension Module for Ray Tracing Sofware SHADOW" );
  Py_INCREF ( &ShadowSourceType );
  PyModule_AddObject ( m, "Source", ( PyObject * ) &ShadowSourceType );
  Py_INCREF ( &ShadowOEType );
  PyModule_AddObject ( m, "OE", ( PyObject * ) &ShadowOEType );
  Py_INCREF ( &ShadowRayType );
  PyModule_AddObject ( m, "Ray", ( PyObject * ) &ShadowRayType );
  Py_INCREF ( &ShadowImageType );
  PyModule_AddObject ( m, "Image", ( PyObject * ) &ShadowImageType );
}
