#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "idl_export.h"
#include "shadow_bind_idl_loader.h"
#include "shadow_bind_c.h"

#define STRLEN 1024
#define IDL_IS_LONG(v)   ((v)->type==IDL_TYP_LONG)
#define IDL_IS_LONG64(v) ((v)->type==IDL_TYP_LONG64)
#define IDL_IS_FLOAT(v)  ((v)->type==IDL_TYP_FLOAT)
#define IDL_IS_DOUBLE(v) ((v)->type==IDL_TYP_DOUBLE)
#define IDL_IS_STRING(v) ((v)->type==IDL_TYP_STRING)
#define IDL_IS_ARRAY(v)  ((v)->flags & IDL_V_ARR)

/*globals here*/
static char statusBuffer[256] ;

/* function protos */
extern IDL_VPTR IDL_CDECL IDL_ShadowSourceGeometric ( int argc, IDL_VPTR argv[], char *argk );
extern IDL_VPTR IDL_CDECL IDL_ShadowSourceSynchrotron ( int argc, IDL_VPTR argv[], char *argk );
extern IDL_VPTR IDL_CDECL IDL_ShadowTrace ( int argc, IDL_VPTR argv[], char *argk );
extern void IDL_CDECL IDL_ShadowPrintSource ( int argc, IDL_VPTR argv[], char *argk );
extern void IDL_CDECL IDL_ShadowPrintOE ( int argc, IDL_VPTR argv[], char *argk );
//extern void IDL_CDECL IDL_ShadowDEBUGPrintOE(int argc, IDL_VPTR argv[], char *argk);
/* define the functions */
static IDL_SYSFUN_DEF2 IDL_ShadowFunctions[] =
{
  {IDL_ShadowSourceGeometric, "GENSOURCEGEOM", 1, 1, 0, 0},
  {IDL_ShadowSourceSynchrotron, "GENSOURCESYNC", 1, 1, 0, 0},
  {IDL_ShadowTrace, "TRACEOE", 3, 3, 0, 0},
  /*
   * here the others functions we want to share
   */
};

static IDL_SYSFUN_DEF2 IDL_ShadowProcedures[] = {
  { ( IDL_FUN_RET ) IDL_ShadowPrintSource, "PRINTSOURCE", 2, 2, 0, 0},
  { ( IDL_FUN_RET ) IDL_ShadowPrintOE, "PRINTOE", 2, 2, 0, 0},
//    {(IDL_FUN_RET) IDL_ShadowDEBUGPrintOE, "DEBUGPRINTOE", 1, 1, 0, 0},
};

/* Check which system we are one */
#if defined(WIN32)
#include <windows.h>
#endif

int IDL_ShadowStartup ( void )
{
  if ( !IDL_SysRtnAdd ( IDL_ShadowFunctions, TRUE, ARRLEN ( IDL_ShadowFunctions ) ) )
  {
    return IDL_FALSE;
  }
  if ( !IDL_SysRtnAdd ( IDL_ShadowProcedures, FALSE, ARRLEN ( IDL_ShadowProcedures ) ) )
  {
    return IDL_FALSE;
  }
  /*Register the exit handler*/
  IDL_ExitRegister ( IDL_Shadow_exit_handler );

  return ( IDL_TRUE );
}

/* called when IDL is shutdown */
void IDL_Shadow_exit_handler ( void ) { /*nothing special to do in this case */ }


/*
 *  Handle Shadow Structures (Both Source and OE)
 */

void PassPoolSourceFromIDL ( poolSource *src, void *ptr_in, void *ptr_def ) {
  IDL_MEMINT offset;
  IDL_VPTR      var;
  int len = 1024;

#define EXPAND_SOURCE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) \
  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
  src->name = *((ctype*) (ptr_in + offset) );
#define EXPAND_SOURCE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
  strncpy(src->name, IDL_STRING_STR( (IDL_STRING*) (ptr_in + offset) ), len);
#include "shadow_source.def"
}

void PassPoolOEFromIDL ( poolOE *oe, void *ptr_in, void *ptr_def ) {
  IDL_MEMINT    offset;
  void         *ptr_tmp;
  int           i;
  IDL_VPTR      var;
  int dim = 10;
  int len = 1024;

#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) \
  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
  oe->name = *((ctype*) (ptr_in + offset) );
#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
  strncpy(oe->name, IDL_STRING_STR( (IDL_STRING*) (ptr_in + offset) ), len);
#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
  for(i=0;i<dim;i++){ \
    oe->name[i] = *((ctype*) (ptr_in+offset+i));\
  }
#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
  for(i=0;i<dim;i++){ \
    if ( ((IDL_STRING*)(ptr_in+offset))[i].slen!=0 ) {\
      strncpy(oe->name[i], ((IDL_STRING*) (ptr_in+offset))[i].s, len); \
    } \
    else{ strncpy(oe->name[i], "", len); } \
  }
#include "shadow_oe.def"
}


IDL_VPTR IDL_CDECL IDL_ShadowSourceGeometric ( int argc, IDL_VPTR argv[], char *argk )
{
  /*
   *  Called in IDL as
   *  ray = genSourceGeomtric(IDL_poolSource)
   */

  poolSource   *src;
  double       *ray;
  IDL_VPTR      ivray;
  IDL_MEMINT    dim[2];

  //void         *ptr_out;
  void         *ptr_in;
  void         *ptr_def;

  IDL_ENSURE_STRUCTURE ( argv[0] );
  IDL_EXCLUDE_EXPR ( argv[0] );
  src = ( poolSource* ) malloc ( sizeof ( poolSource ) );
  //ptr_out = (void*) &ps;
  ptr_in  = ( void* ) argv[0]->value.s.arr->data;
  ptr_def = ( void* ) argv[0]->value.s.sdef;
  PassPoolSourceFromIDL ( src, ptr_in, ptr_def );

  dim[0]=18;
  dim[1]=src->NPOINT;
  ray = ( double* ) IDL_MakeTempArray ( ( int ) IDL_TYP_DOUBLE, 2, dim, IDL_ARR_INI_ZERO, &ivray );
  CShadowSourceGeom ( src, ray );
  free ( src );
  return ivray;
}

IDL_VPTR IDL_CDECL IDL_ShadowSourceSynchrotron ( int argc, IDL_VPTR argv[], char *argk )
{
  /*
   *  Called in IDL as
   *  ray = genSourceGeomtric(IDL_poolSource)
   */

  poolSource   *src;
  double       *ray;
  IDL_VPTR      ivray;
  IDL_MEMINT    dim[2];

  //void         *ptr_out;
  void         *ptr_in;
  void         *ptr_def;

  IDL_ENSURE_STRUCTURE ( argv[0] );
  IDL_EXCLUDE_EXPR ( argv[0] );
  src = ( poolSource* ) malloc ( sizeof ( poolSource ) );
  //ptr_out = (void*) &ps;
  ptr_in  = ( void* ) argv[0]->value.s.arr->data;
  ptr_def = ( void* ) argv[0]->value.s.sdef;

  PassPoolSourceFromIDL ( src, ptr_in, ptr_def );

  dim[0]=18;
  dim[1]=src->NPOINT;
  ray = ( double* ) IDL_MakeTempArray ( ( int ) IDL_TYP_DOUBLE, 2, dim, IDL_ARR_INI_ZERO, &ivray );
  CShadowSourceSync ( src, ray );
  free ( src );
  return ivray;
}



IDL_VPTR IDL_CDECL IDL_ShadowTrace ( int argc, IDL_VPTR argv[], char *argk )
{
  /*
   *  Called in IDL as
   *  ray = genSourceGeomtric(IDL_poolSource)
   */

  poolOE       *oe1;
  double       *ray;
  IDL_VPTR      ivray;

  int           iCount;
  void         *ptr_in;
  void         *ptr_def;

  IDL_ENSURE_STRUCTURE ( argv[0] );
  IDL_EXCLUDE_EXPR ( argv[0] );
  IDL_ENSURE_ARRAY ( argv[1] );
  IDL_ENSURE_SCALAR ( argv[2] );

  oe1 = ( poolOE* ) malloc ( sizeof ( poolOE ) );
  //ptr_out = (void*) &ps;
  ptr_in  = ( void* ) argv[0]->value.s.arr->data;
  ptr_def = ( void* ) argv[0]->value.s.sdef;

  switch ( argv[2]->type ) {
    case IDL_TYP_LONG:
      iCount = ( int ) argv[2]->value.l;
      break;
    case IDL_TYP_LONG64:
      iCount = ( int ) argv[2]->value.l64;
      break;
    default:
      printf ( "3rd arg must be integer!\n" );
  }

  PassPoolOEFromIDL ( oe1, ptr_in, ptr_def );
  ivray = argv[1];
  ray = ( double* ) argv[1]->value.arr->data; // or (IDL_DOUBLE*) !!!!!


  CShadowTraceOE ( oe1, ray, ivray->value.arr->dim[1],iCount );
  free ( oe1 );
  return ivray;
}



void IDL_CDECL IDL_ShadowPrintSource ( int argc, IDL_VPTR argv[], char *argk )
{
  /*
   *  Called in IDL as
   *  PrintSource(poolSource,IDL_STRING)
   */

  poolSource   *src;
  char         *file;

  //void         *ptr_out;
  void         *ptr_in;
  void         *ptr_def;

  IDL_ENSURE_STRUCTURE ( argv[0] );
  IDL_EXCLUDE_EXPR ( argv[0] );
  IDL_ENSURE_STRING ( argv[1] );

  //ptr_out = (void*) &ps;
  ptr_in  = ( void* ) argv[0]->value.s.arr->data;
  ptr_def = ( void* ) argv[0]->value.s.sdef;

  src = ( poolSource* ) malloc ( sizeof ( poolSource ) );

  PassPoolSourceFromIDL ( src, ptr_in, ptr_def );

  file = IDL_VarGetString ( argv[1] );

  CShadowPoolSourceWrite ( src,file );
  free ( src );
}



void IDL_CDECL IDL_ShadowPrintOE ( int argc, IDL_VPTR argv[], char *argk )
{
  /*
   *  Called in IDL as
   *  PrintSource(poolSource,IDL_STRING)
   */

  poolOE       *pOE;
  char         *file;

  //void         *ptr_out;
  void         *ptr_in;
  void         *ptr_def;

  IDL_ENSURE_STRUCTURE ( argv[0] );
  IDL_EXCLUDE_EXPR ( argv[0] );
  IDL_ENSURE_STRING ( argv[1] );

  //ptr_out = (void*) &pOE;
  ptr_in  = ( void* ) argv[0]->value.s.arr->data;
  ptr_def = ( void* ) argv[0]->value.s.sdef;

  pOE = ( poolOE* ) malloc ( sizeof ( poolOE ) );

  PassPoolOEFromIDL ( pOE, ptr_in, ptr_def );

  file = IDL_VarGetString ( argv[1] );

  CShadowPoolOEWrite ( pOE,file );
  free ( pOE );
}


//void IDL_CDECL IDL_ShadowDEBUGPrintOE(int argc, IDL_VPTR argv[], char *argk)
//{
//  /*
//   *  Called in IDL as
//   *  PrintSource(poolSource)
//   */
//  long          i;
//  void         *ptr_in;
//  void         *ptr_def;
//  void         *ptr_arr;
//  IDL_MEMINT    offset;
//  IDL_VPTR      var;
//
//  char         *strs;
//
//  IDL_ENSURE_STRUCTURE(argv[0]);
//  IDL_EXCLUDE_EXPR(argv[0]);
//
//  ptr_in  = (void*) argv[0]->value.s.arr->data;
//  ptr_def = (void*) argv[0]->value.s.sdef;
////  offset = IDL_StructTagInfoByName(ptr_def, "FILE_ABS", IDL_MSG_LONGJMP, &var);
////  strs = (char*) malloc(64*sizeof(char));
////  memcpy(strs,((IDL_STRING*)(ptr_in+offset))->s,64);
////  printf("try0 %s\n",strs);
////  printf("try1 %s\n",strs+15);
////  printf("try2 %s\n",strs+17);
//  printf("%d %d\n", (int) sizeof(IDL_STRING), (int) sizeof(IDL_STRING*));
//  printf("%d %d\n", (int) sizeof(double), (int) sizeof(double*));
//  getchar();
//
//#define EXPAND_OE_SCALAR(ctype,ftype,fkind,pytype,name,cformat,fformat,defvalue) \
//  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
//  printf("%s: ",#name); printf(cformat,*((ctype*) (ptr_in+offset) )); printf("\n");
//#define EXPAND_OE_STRING(ctype,ftype,fkind,pytype,name,cformat,fformat,length,defvalue) \
//  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
//  printf("%s: ",#name); printf("%s\n", IDL_STRING_STR( (IDL_STRING*) (ptr_in+offset) ));
//#define EXPAND_OE_ARRAYS(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,defvalue) \
//  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
//  for(i=0;i<10;i++){\
//    printf("%s: ",#name); printf(cformat, *((ctype*) (ptr_in+offset+i) )), printf("\n");\
//  }
//#define EXPAND_OE_ARRSTR(ctype,ftype,fkind,pytype,name,cformat,fformat,arrdim,length,defvalue) \
//  offset = IDL_StructTagInfoByName(ptr_def, #name, IDL_MSG_LONGJMP, &var); \
//  for(i=0;i<10;i++){\
//    if ( ((IDL_STRING*)(ptr_in+offset))[i].slen!=0 ) \
//      printf("%s: ",#name); printf("%s\n", ((IDL_STRING*) (ptr_in+offset))[i].s );\
//  }
//
//#include "ShadowMaskOE.def"
//}
//
