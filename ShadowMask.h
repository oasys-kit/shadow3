
#include <stdlib.h>
#include <stdio.h>

#ifndef __SHADOWMASK_H__
#define __SHADOWMASK_H__

typedef struct {
  int np;
  double up;
  double dn;
} pixel;

typedef struct {
  double real;
  double imag;
} dComplex;

typedef struct {
#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ctype name;
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ctype name[length];
#include "ShadowMaskSource.def"
} poolSource;

typedef union {
  poolSource Src;
  char poolSourceBit[ sizeof ( poolSource ) ];
} poolSourceUnion;


typedef struct {
#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) ctype name;
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) ctype name[length];
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) ctype name[arrdim];
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) ctype name[arrdim][length];
#include "ShadowMaskOE.def"
} poolOE;

typedef union {
  poolOE OE;
  char poolOEBit[ sizeof ( poolOE ) ];
} poolOEUnion;

//INTERFACE libshadow
extern void BindShadowPoolSourceLoad ( poolSource*, char*, int );
extern void BindShadowPoolSourceWrite ( poolSource*, char*, int );
extern void BindShadowPoolOELoad ( poolOE*, char*, int );
extern void BindShadowPoolOEWrite ( poolOE*, char*, int );
extern void BindShadowSourceGeom ( poolSource*, double*, int* );
extern void BindShadowSourceSync ( poolSource*, double*, int* );
extern void BindShadowTraceOE ( poolOE*, double*, int*, int* );
extern void BindShadowWriteRay ( double*, int*, int*, char*, int );
extern void BindShadowGetDimRay ( char*, int, int*, int* );
extern void BindShadowReadRay ( double*, int*, int*, char*, int );
extern void BindShadowFFresnel2D ( double*, int*, double*, dComplex*, pixel*, pixel* );
//END INTERFACE libshadow


double* CShadowAllocateRay ( poolSource*, double* );
double* CShadowAllocateRayInt ( int, double* );
void CShadowPoolSourceLoad ( poolSource*, char* );
void CShadowPoolSourceWrite ( poolSource*, char* );
void CShadowPoolOELoad ( poolOE*, char* );
void CShadowPoolOEWrite ( poolOE*, char* );
void CShadowGetDimRay ( int*, int*, char* );
void CShadowReadRay ( double*, int, int, char* );
void CShadowWriteRay ( double*, int, int, char* );
void CShadowSourceGeom ( poolSource*, double* );
void CShadowSourceSync ( poolSource*, double* );
void CShadowTrace ( poolOE*, double*, int, int );
void CShadowFFresnel2D ( double*, int, double, dComplex*, pixel*, pixel* );
void CShadowSetupDefaultSource ( poolSource* );
void CShadowSetupDefaultOE ( poolOE* );

#endif

