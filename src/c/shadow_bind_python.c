#include "shadow_bind_python.h"
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <emmintrin.h>
#include <string.h>
//#include <intrin.h>

/***************************************************************************
 *         Shadow_Source Python Object
 *
 *
 *
 *
 *
 ***************************************************************************/

static void Source_dealloc ( Shadow_Source* self )
{
  Py_TYPE(self)->tp_free ( ( PyObject* ) self );
}

static PyObject* Source_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  return type->tp_alloc ( type, 0 );
}

static int Source_init ( Shadow_Source* self, PyObject* args, PyObject* kwds )
{
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) self->pl.name=defvalue;
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) strcpy(self->pl.name,defvalue);
#include "shadow_source.def"
  return 0;
}

static PyObject* Source_load ( Shadow_Source* self, PyObject* args )
{
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  CShadowPoolSourceLoad ( &(self->pl), ( char* ) FileName );

  Py_RETURN_NONE; //TODO do we want to output self???
}

static PyObject* Source_write ( Shadow_Source* self, PyObject* args )
{
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  CShadowPoolSourceWrite ( &(self->pl), ( char* ) FileName );

  Py_RETURN_NONE; //TODO do we want to output self???
}

static PyObject* Source_genBeam ( Shadow_Source* self )
{
  Shadow_Beam *bm = (Shadow_Beam*) PyObject_CallObject((PyObject *) &ShadowBeamType, NULL);
  npy_intp dims[2];
  npy_intp strides[2];
  strides[0] = 18*sizeof ( double );
  strides[1] = sizeof( double );
  dims[0] = self->pl.NPOINT;
  dims[1] = 18;
  if ( bm->rays!=NULL )
    Py_DECREF ( bm->rays );
  bm->rays = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, 2, dims, NPY_FLOAT64, strides, NULL, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );
  if ( ( self->pl.FDISTR==4 ) || ( self->pl.FSOURCE_DEPTH==4 ) || ( self->pl.F_WIGGLER>0 ) ) {
    CShadowSourceSync ( &(self->pl), ( double* ) ( bm->rays->data ) );
  }
  else {
    CShadowSourceGeom ( &(self->pl), ( double* ) ( bm->rays->data ) );
  }
  return (PyObject*) bm;
}

#if PY_MAJOR_VERSION >= 3
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* Source_get_##name(Shadow_Source* self, void* closure) \
{ \
  return PyBytes_FromString(trim(self->pl.name)); \
}
#else
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* Source_get_##name(Shadow_Source* self, void* closure) \
{ \
  return PyString_FromString(trim(self->pl.name)); \
}
#endif

#include "shadow_source.def"


#if PY_MAJOR_VERSION >= 3
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int Source_set_##name(Shadow_Source* self, PyObject* value, void* closure) \
{ \
  if( value!=NULL && PyBytes_Check(value) ) { \
    strcpy(self->pl.name, PyBytes_AsString(value)); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a string."); \
    return -1; \
  } \
  return 0; \
}
#else
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int Source_set_##name(Shadow_Source* self, PyObject* value, void* closure) \
{ \
  if( value!=NULL && PyString_Check(value) ) { \
    strcpy(self->pl.name, PyString_AsString(PyString_AsEncodedObject(value,"utf8","replace"))); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a string."); \
    return -1; \
  } \
  return 0; \
}
#endif

#include "shadow_source.def"


static PyMemberDef Source_members[] = {
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) {#name,pytype,offsetof(Shadow_Source,pl.name),0,#name},
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue)
#include "shadow_source.def"
  {NULL}                                             /* Sentinel          */
};

static PyMethodDef Source_methods[] = {
  {"load" ,   ( PyCFunction ) Source_load ,   METH_VARARGS, "load Shadow.Source from a file"},
  {"write",   ( PyCFunction ) Source_write,   METH_VARARGS, "write Shadow.Source on a file" },
  {"genBeam", ( PyCFunction ) Source_genBeam, METH_NOARGS,  "generate and return beam object"},
  {NULL}                                             /* Sentinel          */
};

static PyGetSetDef Source_getseters[] = {
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
  {#name, (getter) Source_get_##name, (setter) Source_set_##name, #name, NULL},
#include "shadow_source.def"
  {NULL}                                             /* Sentinel          */
};

static PyTypeObject ShadowSourceType = {
#if PY_MAJOR_VERSION < 3
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
#else
  PyVarObject_HEAD_INIT( NULL, 0 )
#endif
  "Source",                                          /* tp_name           */
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

static void OE_dealloc ( Shadow_OE* self )
{
  Py_TYPE(self)->tp_free ( ( PyObject* ) self );
}

static PyObject* OE_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  Shadow_OE *self;
  int i;
  self = (Shadow_OE*) type->tp_alloc ( type, 0 );
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) self->pl.name = defvalue;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) strcpy(self->pl.name, defvalue);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) for(i=0;i<arrdim;i++) self->pl.name[i]=defvalue;
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) memset(&(self->pl.name[0][0]), 0, arrdim*length);
#include "shadow_oe.def"
  return (PyObject*) self;
}

static int OE_init ( Shadow_OE* self, PyObject* args, PyObject* kwds )
{
  int i;
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) self->pl.name = defvalue;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) strcpy(self->pl.name, defvalue);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) for(i=0;i<arrdim;i++) self->pl.name[i]=defvalue;
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) memset(self->pl.name, 0, arrdim*length);
#include "shadow_oe.def"
  return 0;
}

static PyObject* OE_load ( Shadow_OE* self, PyObject* args )
{
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  CShadowPoolOELoad ( &(self->pl), ( char* ) FileName );

  Py_RETURN_NONE;
}

static PyObject* OE_write ( Shadow_OE* self, PyObject* args )
{
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }
  CShadowPoolOEWrite ( &(self->pl), ( char* ) FileName );

  Py_RETURN_NONE;
}

static PyObject* OE_trace ( Shadow_OE* self, PyObject* args )
{
  Shadow_Beam * bm = NULL;
  int nPoint;
  int iCount;
  if ( !PyArg_ParseTuple ( args, "Oi", &bm, &iCount ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a python object!" );
    return NULL;
  }
  if ( !PyObject_TypeCheck ( bm, &ShadowBeamType ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.Beam instance" );
    Py_RETURN_NONE;
  }
  if ( bm->rays==NULL ) {
    PyErr_SetString ( PyExc_TypeError, "rays is empty" );
    Py_RETURN_NONE;
  }
  nPoint = bm->rays->dimensions[0];
  CShadowTraceOE ( &(self->pl), ( double* ) ( bm->rays->data ), nPoint, iCount );
  return (PyObject*) bm;
}

#define ski NPY_INT32
#define skr NPY_FLOAT64
#define skc NPY_STRING


#if PY_MAJOR_VERSION>=3
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void* closure) \
{ \
  return PyBytes_FromString(trim(self->pl.name)); \
}
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void *closure){ \
  int ndims = 1; \
  npy_intp dims[1] = {arrdim}; \
  return PyArray_SimpleNewFromData(ndims,dims,fkind,self->pl.name); \
}
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void *closure){ \
  PyObject *res; \
  int nd = 1; \
  npy_intp strides[1] = {length*sizeof(char)}; \
  npy_intp dims[1] = {arrdim}; \
  res = PyArray_New(&PyArray_Type,nd,dims,fkind,strides,self->pl.name,length,0,NULL); \
  return res; \
}
#else
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void* closure) \
{ \
  return PyString_FromString(trim(self->pl.name)); \
}
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void *closure){ \
  int ndims = 1; \
  npy_intp dims[1] = {arrdim}; \
  return PyArray_SimpleNewFromData(ndims,dims,fkind,self->pl.name); \
}
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
static PyObject* OE_get_##name(Shadow_OE* self, void *closure){ \
  PyObject *res; \
  int nd = 1; \
  npy_intp strides[1] = {length*sizeof(char)}; \
  npy_intp dims[1] = {arrdim}; \
  res = PyArray_New(&PyArray_Type,nd,dims,fkind,strides,self->pl.name,length,0,NULL); \
  return res; \
}
#endif
#include "shadow_oe.def"


#if PY_MAJOR_VERSION>=3
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  if( value!=NULL && PyBytes_Check(value) ) { \
    strcpy(self->pl.name, PyBytes_AsString(value)); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a string."); \
    return -1; \
  } \
  return 0; \
}
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  if(PyArray_Check(value) && PyArray_ISCONTIGUOUS(value) && PyArray_NDIM(value)==1 && PyArray_Size(value)==arrdim && PyArray_ISNUMBER(value)){ \
    memcpy((void*)self->pl.name, PyArray_DATA( (PyArrayObject*) PyArray_Cast( (PyArrayObject*)value,fkind) ),sizeof(ctype)*arrdim); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a conform array."); \
    return -1; \
  } \
  return 0; \
}
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  PyObject *res; \
  int nd = 1; \
  npy_intp strides[1] = {length*sizeof(char)}; \
  npy_intp dims[1] = {arrdim}; \
  if(PyArray_Check(value) && PyArray_ISCONTIGUOUS(value) && PyArray_NDIM(value)==1 && PyArray_Size(value)==arrdim && PyArray_ISSTRING(value)){ \
    res = PyArray_New(&PyArray_Type,nd,dims,fkind,strides,self->pl.name,length,NPY_WRITEABLE,NULL); \
    PyArray_CastTo((PyArrayObject*)res,(PyArrayObject*)value); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a conform array of string."); \
    return -1; \
  } \
  return 0; \
}
#else
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  if( value!=NULL && PyString_Check(value) ) { \
    strcpy(self->pl.name, PyString_AsString(PyString_AsEncodedObject(value,"utf8","replace"))); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a string."); \
    return -1; \
  } \
  return 0; \
}
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  if(PyArray_Check(value) && PyArray_ISCONTIGUOUS(value) && PyArray_NDIM(value)==1 && PyArray_Size(value)==arrdim && PyArray_ISNUMBER(value)){ \
    memcpy((void*)self->pl.name, PyArray_DATA( (PyArrayObject*) PyArray_Cast( (PyArrayObject*)value,fkind) ),sizeof(ctype)*arrdim); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a conform array."); \
    return -1; \
  } \
  return 0; \
}
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
static int OE_set_##name(Shadow_OE* self, PyObject* value, void* closure) \
{ \
  PyObject *res; \
  int nd = 1; \
  npy_intp strides[1] = {length*sizeof(char)}; \
  npy_intp dims[1] = {arrdim}; \
  if(PyArray_Check(value) && PyArray_ISCONTIGUOUS(value) && PyArray_NDIM(value)==1 && PyArray_Size(value)==arrdim && PyArray_ISSTRING(value)){ \
    res = PyArray_New(&PyArray_Type,nd,dims,fkind,strides,self->pl.name,length,NPY_WRITEABLE,NULL); \
    PyArray_CastTo((PyArrayObject*)res,(PyArrayObject*)value); \
  } \
  else{ \
    PyErr_SetString(PyExc_TypeError, "not a conform array of string."); \
    return -1; \
  } \
  return 0; \
}
#endif
#include "shadow_oe.def"


static PyMemberDef OE_members[] = {
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) {#name,pytype,offsetof(Shadow_OE,pl.name),0,#name},
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue)
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) 
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) 
#include "shadow_oe.def"
  {NULL}                                             /* Sentinel          */
};

static PyMethodDef OE_methods[] = {
  {"load" , ( PyCFunction ) OE_load , METH_VARARGS, "load Shadow.OE from a file"},
  {"write", ( PyCFunction ) OE_write, METH_VARARGS, "write Shadow.OE on a file" },
  {"trace", ( PyCFunction ) OE_trace, METH_VARARGS, "trace Shadow.Beam through the opticacl element"},
  {NULL}                                             /* Sentinel          */
};

static PyGetSetDef OE_getseters[] = {
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue)
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
  {#name, (getter) OE_get_##name, (setter) OE_set_##name, #name, NULL},
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
  {#name, (getter) OE_get_##name, (setter) OE_set_##name, #name, NULL},
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
  {#name, (getter) OE_get_##name, (setter) OE_set_##name, #name, NULL},
#include "shadow_oe.def"
  {NULL}                                             /* Sentinel          */
};

static PyTypeObject ShadowOEType = {
#if PY_MAJOR_VERSION < 3
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
#else
  PyVarObject_HEAD_INIT( NULL, 0 )
#endif
  "OE",                                              /* tp_name           */
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
 *         Shadow_Beam Python Object
 *
 *
 *
 *
 *
 ***************************************************************************/

static void Beam_dealloc ( Shadow_Beam* self )
{
  Py_XDECREF ( self->rays );
  Py_TYPE(self)->tp_free ( ( PyObject* ) self );
}

static PyObject* Beam_new ( PyTypeObject* type, PyObject* args, PyObject* kwds )
{
  return type->tp_alloc ( type, 0 );
}

static int Beam_init ( Shadow_Beam* self, PyObject* args, PyObject* kwds )
{
  self->rays = NULL;
  return 0;
}

static PyObject* Beam_load ( Shadow_Beam* self, PyObject* args )
{
  int nCol, nPoint;
  npy_intp dims[2];
  const char *FileName;
  FILE* TestFile;

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
  CShadowBeamGetDim ( &nCol, &nPoint, ( char* ) FileName );

  dims[0] = nPoint;
  dims[1] = 18;

  if ( self->rays!=NULL )
    Py_DECREF ( self->rays );
  self->rays = ( PyArrayObject* ) PyArray_ZEROS(2, dims, NPY_FLOAT64, 0);
  CShadowBeamLoad ( ( double* ) ( self->rays->data ), nCol, nPoint, ( char* ) FileName );
  Py_RETURN_NONE;
}

static PyObject* beam_SetRayZeros(Shadow_Beam* self, PyObject* args)
{
  npy_int NRays;
  npy_intp dims[2];
  if(!PyArg_ParseTuple ( args, "i", &NRays )) { 
    printf("rays not initialized\n"); 
    Py_RETURN_NONE; 
  }
  dims[0] = NRays;
  dims[1] = 18;
  self->rays = ( PyArrayObject* ) PyArray_ZEROS(2, dims, NPY_FLOAT64, 0);
  Py_RETURN_NONE;
}


static PyObject* Beam_write ( Shadow_Beam* self, PyObject* args )
{
  int nPoint, nCol;
  const char* FileName;
  if ( !PyArg_ParseTuple ( args, "s", &FileName ) ) {
    PyErr_SetString ( PyExc_TypeError, "argument should be a string!" );
    return NULL;
  }

  if ( self->rays==NULL ) {
    PyErr_SetString ( PyExc_TypeError, "rays is not set yet" );
    Py_RETURN_NONE;
  }

  nPoint = self->rays->dimensions[0];
  nCol = 18;
  CShadowBeamWrite ( ( double* ) ( self->rays->data ), nCol, nPoint, ( char* ) FileName );

  Py_RETURN_NONE;
}

static PyObject* Beam_genSource ( Shadow_Beam* self, PyObject* args )
{
  Shadow_Source* pySrc = NULL;
  npy_intp dims[2];
  npy_intp strides[2];

  if ( !PyArg_ParseTuple ( args, "O", &pySrc ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    return NULL;
  }
  if ( !PyObject_TypeCheck ( pySrc, &ShadowSourceType ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.Source instance" );
    return NULL;
  }

  strides[0] = 18*sizeof ( double ); 
  strides[1] = sizeof( double );
  dims[0] = pySrc->pl.NPOINT; 
  dims[1] = 18;

  if ( self->rays!=NULL )
    Py_DECREF ( self->rays );
  self->rays = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, 2, dims, NPY_FLOAT64, strides, NULL, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );

  if ( ( pySrc->pl.FDISTR==4 ) || ( pySrc->pl.FSOURCE_DEPTH==4 ) || ( pySrc->pl.F_WIGGLER>0 ) ) {
    CShadowSourceSync ( &(pySrc->pl), ( double* ) ( self->rays->data ) );
  }
  else {
    CShadowSourceGeom ( &(pySrc->pl), ( double* ) ( self->rays->data ) );
  }

  Py_RETURN_NONE;
}

static PyObject* Beam_traceOE ( Shadow_Beam* self, PyObject* args )
{
  int nPoint;
  int iCount;
  Shadow_OE* pyOe = NULL;

  if ( !PyArg_ParseTuple ( args, "Oi", &pyOe, &iCount ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    Py_RETURN_NONE;
  }
  if ( !PyObject_TypeCheck ( pyOe, &ShadowOEType ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.OE instance" );
    Py_RETURN_NONE;
  }

  if ( self->rays==NULL ) {
    PyErr_SetString ( PyExc_TypeError, "rays is empty" );
    Py_RETURN_NONE;
  }
  nPoint = self->rays->dimensions[0];
  CShadowTraceOE ( &(pyOe->pl), ( double* ) ( self->rays->data ), nPoint, iCount );

  Py_RETURN_NONE;
}


static PyMemberDef Beam_members[] = {
  {"rays",T_OBJECT_EX,offsetof ( Shadow_Beam,rays ),0,"rays"},
  {NULL}
};

static PyMethodDef Beam_methods[] = {
  {"load" , ( PyCFunction ) Beam_load , METH_VARARGS, "load Shadow.Beam from a file"},
  {"write", ( PyCFunction ) Beam_write, METH_VARARGS, "write Shadow.Beam on a file" },
  {"genSource", ( PyCFunction ) Beam_genSource, METH_VARARGS, "generate rays from Source"},
  {"traceOE", ( PyCFunction ) Beam_traceOE, METH_VARARGS, "trace rays according to a given OE"},
  {"SetRayZeros", ( PyCFunction ) beam_SetRayZeros, METH_VARARGS, "set member rays to zeros"},
  {NULL}                                             /* Sentinel          */
};

static PyTypeObject ShadowBeamType = {
#if PY_MAJOR_VERSION < 3
  PyObject_HEAD_INIT ( NULL )
  0,                                                 /* ob_size           */
#else
  PyVarObject_HEAD_INIT( NULL, 0 )
#endif
  "Beam",                                            /* tp_name           */
  sizeof ( Shadow_Beam ),                            /* tp_basicsize      */
  0,                                                 /* tp_itemsize       */
  ( destructor ) Beam_dealloc,                       /* tp_dealloc        */
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
  "Beam object",                                     /* tp_doc            */
  0,                                                 /* tp_traverse       */
  0,                                                 /* tp_clear          */
  0,                                                 /* tp_richcompare    */
  0,                                                 /* tp_weaklistoffset */
  0,                                                 /* tp_iter           */
  0,                                                 /* tp_iternext       */
  Beam_methods,                                      /* tp_methods        */
  Beam_members,                                      /* tp_members        */
  0,                                                 /* tp_getset         */
  0,                                                 /* tp_base           */
  0,                                                 /* tp_dict           */
  0,                                                 /* tp_descr_get      */
  0,                                                 /* tp_descr_set      */
  0,                                                 /* tp_dictoffset     */
  ( initproc ) Beam_init,                            /* tp_init           */
  0,                                                 /* tp_alloc          */
  Beam_new,                                          /* tp_new            */
};










































static PyObject* saveBeam ( PyObject* self, PyObject* args )
{
  PyObject* newBeam;
  PyObject* oldBeam;
  PyArrayObject* tmp;
  double* newdata;
  int i;

  if ( !PyArg_ParseTuple ( args, "O", &oldBeam ) ) {
    PyErr_SetString ( PyExc_TypeError, "Error passing argument" );
    return NULL;
  }

  if ( !PyObject_TypeCheck ( oldBeam, &ShadowBeamType ) ) {
    PyErr_SetString ( PyExc_TypeError, "the argument has to be a Shadow.Beam instance" );
    return NULL;
  }

  newBeam = Beam_new ( &ShadowBeamType, NULL, NULL );

  if ( ( ( Shadow_Beam* ) ( oldBeam ) )->rays!=NULL ) {
    tmp = ( ( Shadow_Beam* ) ( oldBeam ) )->rays;
    newdata = ( double* ) malloc ( tmp->dimensions[0]*tmp->dimensions[1]*sizeof ( double ) );
    for ( i=0;i<tmp->dimensions[0]*tmp->dimensions[1];i++ )
      newdata[i] = ( ( double* ) ( tmp->data ) ) [i];
    ( ( Shadow_Beam* ) ( newBeam ) )->rays = ( PyArrayObject* ) PyArray_New ( &PyArray_Type, tmp->nd, tmp->dimensions, NPY_FLOAT64, tmp->strides,
                                                                            newdata, sizeof ( double ), NPY_CARRAY|NPY_OWNDATA, NULL );
  }

  return newBeam;
}

#define DOUBLE_TO_INT(in,out) out=_mm_cvttsd_si32(_mm_load_sd(&(in)));

#define FLOAT_TO_INT(in,out)  out=_mm_cvttss_si32(_mm_load_ss(&(in)));

static npy_int BinarySearch(npy_double x, npy_double *wl, npy_int n){
  npy_int low, high;		/* range of elements to consider */
  npy_int k;			/* middle element between low and high */
  low = 0;
  high = n-1;

  while (high - low > 1) {
    k = (low + high) / 2;
    if (x < wl[k]) { high = k; } 
    else if (x > wl[k+1]) { low = k; } 
    else {
      high = k;
      low = k;
    }
  }
  return low;
}


static void Rotate(npy_double *ux, npy_double *uy, npy_double *uz, const npy_double vx, const npy_double vy, const npy_double vz) { 
  npy_double sinT, IsinT, cosT, I_cosT, nx, nz;
  npy_double Rxx, Rxy, Rxz, Ryx, Ryy, Ryz, Rzx, Rzy, Rzz;
  npy_double tmpx, tmpy, tmpz;
// vector u to rotate using the rotation R such that dot(R,e2) = v
  nx =   vz;
  nz = - vx;
  sinT   = sqrt(nx*nx + nz*nz);
  IsinT  = 1.0/sinT;
  nx    *= IsinT;
  nz    *= IsinT;
  cosT   = vy;
  I_cosT = (1.0-cosT);

  Rxx =   cosT + nx*nx*I_cosT;
  Rxy = - nz*sinT;
  Rxz =   nx*nz*I_cosT;

  Ryx =   nz*sinT;
  Ryy =   cosT;
  Ryz = - nx*sinT;

  Rzx =   nx*nz*I_cosT;
  Rzy =   nx*sinT;
  Rzz =   cosT + nz*nz*I_cosT;

  tmpx = Rxx*(*ux) + Rxy*(*uy) + Rxz*(*uz);
  tmpy = Ryx*(*ux) + Ryy*(*uy) + Ryz*(*uz);
  tmpz = Rzx*(*ux) + Rzy*(*uy) + Rzz*(*uz);
  
  *ux = tmpx;
  *uy = tmpy;
  *uz = tmpz; 
}


static PyObject *
vecRotate(PyObject *self, PyObject *args){
  PyArrayObject *PyUx, *PyUy, *PyUz, *PyVx, *PyVy, *PyVz;
  npy_intp      *dims;//, Rdims[3];
  npy_int        size;
  npy_double    *ux, *uy, *uz, *vx, *vy, *vz;//, *R;
  int i;
  if(!PyArg_ParseTuple(args, "O!O!O!O!O!O!", &PyArray_Type, &PyUx, &PyArray_Type, &PyUy, &PyArray_Type, &PyUz,
                                             &PyArray_Type, &PyVx, &PyArray_Type, &PyVy, &PyArray_Type, &PyVz)) return NULL;
  dims = PyArray_DIMS(PyUx); //actually it is better to check if all the dimensions are ok...
  size = dims[0];

  ux = PyArray_DATA(PyUx); uy = PyArray_DATA(PyUy); uz = PyArray_DATA(PyUz);
  vx = PyArray_DATA(PyVx); vy = PyArray_DATA(PyVy); vz = PyArray_DATA(PyVz);

  for(i=0;i<size;i++){
    Rotate(&ux[i], &uy[i], &uz[i], vx[i], vy[i], vz[i]);
  }
  return Py_None;
}


static PyObject* FastCDFfromZeroIndex(PyObject *self, PyObject *args){
/* python related variable (input - output) */
  PyArrayObject *arry, *x;
  PyObject *arrR;
  npy_intp *ydims, *xdims;
  npy_int size, NE; 
  npy_double *dataR;
/* C internal variables */  
  int i;
  npy_int    x_in;
  npy_double x_lo, x_up, y_lo, *tmpx, *tmpR;
  if (!PyArg_ParseTuple(args, "O!O!", &PyArray_Type, &arry, 
                                      &PyArray_Type, &x)) return NULL;
/* set up dimensions  */
  xdims = PyArray_DIMS(x);
  ydims = PyArray_DIMS(arry); 
  size  = xdims[0];
  NE    = ydims[0];
/* set up pointers    */
  dataR = (npy_double*) malloc(size*sizeof(npy_double));
/* build arrR */
  arrR  = PyArray_SimpleNewFromData(1, xdims, NPY_FLOAT64, (void*) dataR);
  PyArray_FLAGS(arrR) |= NPY_OWNDATA;
/* cycle */
  for(i=0;i<size;i++){
    tmpx  = (npy_double*) PyArray_GETPTR1(x,i);
    tmpR  = (npy_double*) PyArray_GETPTR1(arrR,i);
    x_in  = BinarySearch(*tmpx, (npy_double*) PyArray_GETPTR1(arry,0), NE);
    x_lo  = *( (npy_double*) PyArray_GETPTR1(arry,x_in) );
    x_up  = *( (npy_double*) PyArray_GETPTR1(arry,x_in+1) );
    y_lo  = (npy_double) x_in;
   *tmpR  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);
  }
  return arrR;
}


static PyObject* FastCDFfromOneIndex(PyObject *self, PyObject *args){
/* python related variable (input - output) */
  PyArrayObject *arry, *index, *x;
  PyObject *arrR;
  npy_intp *ydims, *xdims;
  npy_int size, NY; 
  npy_double *dataR;
/* C internal variables */  
  int i;
  npy_int    x_in, indx;
  npy_double x_lo, x_up, y_lo, *tmpx, *tmpF, *tmpR; 
  npy_double tmp1, tmp2, len1, len2;
  if (!PyArg_ParseTuple(args, "O!O!O!", &PyArray_Type, &arry, 
                                        &PyArray_Type, &index, 
                                        &PyArray_Type, &x)) return NULL;
/* set up dimensions  */
  xdims = PyArray_DIMS(x);
  ydims = PyArray_DIMS(arry); 
  size  = xdims[0];
  NY    = ydims[1];
/* set up pointers    */
  dataR = (npy_double*) malloc(size*sizeof(npy_double));
/* build arrR */
  arrR  = PyArray_SimpleNewFromData(1, xdims, NPY_FLOAT64, (void*) dataR); 
  PyArray_FLAGS(arrR) |= NPY_OWNDATA;
/* cycle */
  for(i=0;i<size;i++){
    tmpF  = (npy_double*) PyArray_GETPTR1(index,i);
    tmpx  = (npy_double*) PyArray_GETPTR1(x,i);
    tmpR  = (npy_double*) PyArray_GETPTR1(arrR,i);

    DOUBLE_TO_INT(*tmpF,indx);

    x_in  = BinarySearch(*tmpx,(npy_double*) PyArray_GETPTR2(arry,indx,0),NY);
    x_lo  = *( (npy_double*) PyArray_GETPTR2(arry,indx,x_in) );
    x_up  = *( (npy_double*) PyArray_GETPTR2(arry,indx,x_in+1) );
    y_lo  = (npy_double) x_in;
    tmp1  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);

    x_in  = BinarySearch(*tmpx,(npy_double*) PyArray_GETPTR2(arry,indx+1,0),NY);
    x_lo  = *( (npy_double*) PyArray_GETPTR2(arry,indx+1,x_in) );
    x_up  = *( (npy_double*) PyArray_GETPTR2(arry,indx+1,x_in+1) );
    y_lo  = (npy_double) x_in;
    tmp2  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);

    len1  = (*tmpF) - (npy_double) indx;
    len2  = len1 - 1.0;
    if(len1 < 1.0e-16) {
      len1 = 1.0;
      len2 = 0.0;
    }
    else{
      len1  = 1.0/len1/len1;
      len2  = 1.0/len2/len2;
    }

   *tmpR  = (tmp1*len1 + tmp2*len2) / (len1+len2);
  }
  return arrR;
}

static PyObject* FastCDFfromTwoIndex(PyObject *self, PyObject *args){
/* python related variable (input - output) */
  PyArrayObject *arry, *index1, *index2, *x;
  PyObject *arrR;
  npy_intp *ydims, *xdims;
  npy_int size, NX; 
  npy_double *dataR;
/* C internal variables */  
  int i;
  npy_int    x_in, ind1, ind2;
  npy_double x_lo, x_up, y_lo, *tmpx, *tmpF1, *tmpF2, *tmpR;
  npy_double tmp1, tmp2, tmp3, tmp4, len1, len2, len3, len4;
  if (!PyArg_ParseTuple(args, "O!O!O!O!", &PyArray_Type, &arry, 
                                          &PyArray_Type, &index1,
                                          &PyArray_Type, &index2,
                                          &PyArray_Type, &x)) return NULL;
/* set up dimensions  */
  xdims = PyArray_DIMS(x);
  ydims = PyArray_DIMS(arry); 
  size  = xdims[0];
  NX    = ydims[2];
  
/* set up pointers    */
  dataR = (npy_double*) malloc(size*sizeof(npy_double));
/* build arrR */
  arrR  = PyArray_SimpleNewFromData(1, xdims, NPY_FLOAT64, (void*) dataR);
  PyArray_FLAGS(arrR) |= NPY_OWNDATA;
/* cycle */
  for(i=0;i<size;i++){
    tmpF1 = (npy_double*) PyArray_GETPTR1(index1,i);
    tmpF2 = (npy_double*) PyArray_GETPTR1(index2,i);
    tmpx  = (npy_double*) PyArray_GETPTR1(x,i);
    tmpR  = (npy_double*) PyArray_GETPTR1(arrR,i);

    DOUBLE_TO_INT(*tmpF1, ind1);
    DOUBLE_TO_INT(*tmpF2, ind2);

    x_in  = BinarySearch(*tmpx,(npy_double*) PyArray_GETPTR3(arry,ind1,ind2,0),NX);
    x_lo  = *((npy_double*) PyArray_GETPTR3(arry,ind1,ind2,x_in));    
    x_up  = *((npy_double*) PyArray_GETPTR3(arry,ind1,ind2,x_in+1));
    y_lo  = (npy_double) x_in;
    tmp1  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);

    x_in  = BinarySearch(*tmpx,(npy_double*) PyArray_GETPTR3(arry,ind1,ind2+1,0),NX);
    x_lo  = *((npy_double*) PyArray_GETPTR3(arry,ind1,ind2+1,x_in));
    x_up  = *((npy_double*) PyArray_GETPTR3(arry,ind1,ind2+1,x_in+1));
    y_lo  = (npy_double) x_in;
    tmp2  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);

    x_in  = BinarySearch(*tmpx,(npy_double*) PyArray_GETPTR3(arry,ind1+1,ind2,0),NX);
    x_lo  = *((npy_double*) PyArray_GETPTR3(arry,ind1+1,ind2,x_in));
    x_up  = *((npy_double*) PyArray_GETPTR3(arry,ind1+1,ind2,x_in+1));
    y_lo  = (npy_double) x_in;
    tmp3  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);    

    x_in  = BinarySearch(*tmpx,(npy_double*) PyArray_GETPTR3(arry,ind1+1,ind2+1,0),NX);
    x_lo  = *((npy_double*) PyArray_GETPTR3(arry,ind1+1,ind2+1,x_in));
    x_up  = *((npy_double*) PyArray_GETPTR3(arry,ind1+1,ind2+1,x_in+1));
    y_lo  = (npy_double) x_in;
    tmp4  = y_lo + (*tmpx - x_lo) / (x_up - x_lo);    

    len1  = (*tmpF1) - (npy_double) ind1;
    len2  = (*tmpF2) - (npy_double) ind2;
    len3  = len1 - 1.0;
    len4  = len2 - 1.0;
    if(len1 < FLT_EPSILON) {
      len1 = 1.0;
      len3 = 0.0;
    }
    else{
      len1  = 1.0/len1/len1;
      len3  = 1.0/len3/len3;
    }
    if(len2 < FLT_EPSILON){
      len2 = 1.0;
      len4 = 0.0;
    }
    else{
      len2  = 1.0/len2/len2;
      len4  = 1.0/len4/len4;
    }

   *tmpR  = (tmp1*len1*len2 + tmp2*len1*len4 + tmp3*len3*len2 + tmp4*len3*len4) / (len1*len2 + len1*len4 + len3*len2 + len3*len4);
  }
  return arrR;
}






/*  Shadow methods none  */

static PyMethodDef Shadow_methods[] = {
  {"saveBeam" ,            ( PyCFunction ) saveBeam,             METH_VARARGS, "save Beam in a new instance Shadow.Beam"},
  {"vecRotate",            ( PyCFunction ) vecRotate,            METH_VARARGS, NULL},
  {"FastCDFfromZeroIndex", ( PyCFunction ) FastCDFfromZeroIndex, METH_VARARGS, NULL},
  {"FastCDFfromOneIndex",  ( PyCFunction ) FastCDFfromOneIndex,  METH_VARARGS, NULL},
  {"FastCDFfromTwoIndex",  ( PyCFunction ) FastCDFfromTwoIndex,  METH_VARARGS, NULL},
  {NULL, NULL, 0, NULL}                              /* Sentinel          */
};
/*  module init function  */

#if PY_MAJOR_VERSION >= 3
static struct PyModuleDef shadowModule = {
    PyModuleDef_HEAD_INIT,
    "Shadow.ShadowLib",
    "ShadowLib module is the Python binding for the SHADOW3 Library",
    -1,
    Shadow_methods,
    NULL,
    NULL,
    NULL,
    NULL
};

PyMODINIT_FUNC
PyInit_ShadowLib( void ){
  PyObject* m;
  _import_array();//???

//  Py_TYPE(ShadowSourceType) = PyType_Type;
//  Py_TYPE(ShadowOEType) = PyType_Type;
//  Py_TYPE(ShadowBeamType) = PyType_Type;
  if ( PyType_Ready ( &ShadowSourceType ) < 0 ){ printf("failed to load Source"); return NULL; }
  if ( PyType_Ready ( &ShadowOEType ) < 0 ){ printf("failed to load OE"); return NULL; }
  if ( PyType_Ready ( &ShadowBeamType ) < 0 ){ printf("failed to load Beam"); return NULL; }
  m = PyModule_Create(&shadowModule);
  Py_INCREF ( &ShadowSourceType );
  PyModule_AddObject ( m, "Source", ( PyObject * ) &ShadowSourceType );
  Py_INCREF ( &ShadowOEType );
  PyModule_AddObject ( m, "OE", ( PyObject * ) &ShadowOEType );
  Py_INCREF ( &ShadowBeamType );
  PyModule_AddObject ( m, "Beam", ( PyObject * ) &ShadowBeamType );
  return m;
}

#else

PyMODINIT_FUNC
initShadowLib ( void )
{
  _import_array();
  PyObject* m;

  ShadowSourceType.ob_type = &PyType_Type;
  ShadowOEType.ob_type = &PyType_Type;
  ShadowBeamType.ob_type = &PyType_Type;
  if ( PyType_Ready ( &ShadowSourceType ) < 0 ){ printf("failed to load Source"); return; }
  if ( PyType_Ready ( &ShadowOEType ) < 0 ){ printf("failed to load OE"); return; }
  if ( PyType_Ready ( &ShadowBeamType ) < 0 ){ printf("failed to load Beam"); return; }

  m = Py_InitModule3 ( "Shadow.ShadowLib", Shadow_methods, "Extension Module for Ray Tracing Sofware SHADOW" );

  Py_INCREF ( &ShadowSourceType );
  PyModule_AddObject ( m, "Source", ( PyObject * ) &ShadowSourceType );
  Py_INCREF ( &ShadowOEType );
  PyModule_AddObject ( m, "OE", ( PyObject * ) &ShadowOEType );
  Py_INCREF ( &ShadowBeamType );
  PyModule_AddObject ( m, "Beam", ( PyObject * ) &ShadowBeamType );
}

#endif
