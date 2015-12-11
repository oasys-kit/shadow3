#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "idl_export.h"
#include "shadow_bind_idl_loader.h"

/*
 * Define message codes and their corresponding printf(3) format
 * strings. Note that message codes start at zero and each one is
 * one less that the previous one. Codes must be monotonic and
 * contiguous. Must match the corresponding entries in IDLtoC.h
 */
static IDL_MSG_DEF msg_arr[] =
{
  {  "IDL_SHADOW_ERROR",                      "%NError: %s." },
  {  "IDL_SHADOW_NOSTRINGARRAY",              "%NString arrays not allowed %s"},
};

/*
 * The load function fills in this message block handle with the
 * opaque handle to the message block used for this module. The other
 * routines can then use it to throw errors from this block.
 */
IDL_MSG_BLOCK msg_block;

int IDL_Load ( void )
{
  if ( ! ( msg_block = IDL_MessageDefineBlock ( "IDLtoC", ARRLEN ( msg_arr ),msg_arr ) ) )
    return IDL_FALSE;
  if ( !IDL_ShadowStartup() )
    IDL_MessageFromBlock ( msg_block, IDL_SHADOW_ERROR, IDL_MSG_RET, "Unable to initialize Shadow" );

  return IDL_TRUE;
}

