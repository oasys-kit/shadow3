#ifndef IDL_C_H
#define IDL_C_H

/* These MUST match the corresponding entries in IDLtoC.c */
/* message numbers... */
#define IDL_SHADOW_ERROR                0
#define IDL_SHADOW_NOSTRINGARRAY        -1

/* Handy macro */
#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

extern IDL_MSG_BLOCK msg_block;

/*Define the startup function that adds the C functions to IDL and the */
/*Exit handler.*/

/* IDLtoCarrayExamples.c */
extern void IDL_Shadow_exit_handler(void);
extern int IDL_ShadowStartup(void);

#endif

