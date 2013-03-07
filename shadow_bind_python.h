/*
 * =====================================================================================
 *
 *       Filename:  shadow_bind_python.h
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  11/27/2012 11:39:01 AM
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  YOUR NAME (), 
 *   Organization:  
 *
 * =====================================================================================
 */

#ifndef __SHADOW_BIND_PYTHON__
#define __SHADOW_BIND_PYTHON__

/*  Python dependencies  */
#include <Python.h>
#include <structmember.h>
#include <numpy/arrayobject.h>

/*  C-Layer dependencies  */
#include "shadow_bind_c.h"

/*  Standard library included  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/********************************************
 *  
 * source class declaration
 *
 ********************************************/

/*  Definition of struct for python class  */
typedef struct {
  PyObject_HEAD
  poolSource pl;
} Shadow_Source;


/********************************************
 *  
 * oe class declaration
 *
 ********************************************/

/*  Definition of struct for python class  */
typedef struct {
  PyObject_HEAD
  poolOE pl;
} Shadow_OE;


/********************************************
 *  
 * beam class declaration
 *
 ********************************************/

typedef struct {
  PyObject_HEAD
  PyArrayObject* rays;
} Shadow_Beam;


/********************************************
 *  
 * Type declaration
 *
 ********************************************/

static PyTypeObject ShadowSourceType;
static PyTypeObject ShadowOEType;
static PyTypeObject ShadowBeamType;

#endif
