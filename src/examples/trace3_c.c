
#include <stdio.h>
#include <string.h>
#include "shadow_bind_c.h"

#ifdef __OPENMP
#include <omp.h>
#endif


int count_lines ( FILE *f ) {
  char ch = '\0';
  int  c  = 0;
  while ( ch!=EOF ) {
    ch=fgetc ( f );
    if ( ch=='\n' )
      c++;
  }
  return c;
}

void inputManager ( int argc, char** argv, int *trc, int *src ) {
  if ( argc==1 ) {
    * ( trc ) = 1;
  }
  else {
    if ( !strcmp ( argv[1],"-s" ) )
      * ( src ) = 1;
    if ( !strcmp ( argv[1],"-a" ) )
      * ( src ) = 1, * ( trc ) = 1;
  }
}

int main ( int argc, char** argv )
{
  int i;
  int use_trc = 0;
  int use_src = 0;
  // manage input args
//  inputManager(argc, argv, &use_trc, &use_src);

  if ( argc==1 ) {
    use_trc = 1;
  }
  else {
    if ( !strcmp ( argv[1],"-s" ) )
      use_src = 1;
    if ( !strcmp ( argv[1],"-t" ) )
      use_trc = 1;    
    if ( !strcmp ( argv[1],"-a" ) )
      use_src = 1, use_trc = 1;
  }
  if( use_src==0 && use_trc==0 ){
    printf("trace needs one argument:\n");
    printf(" -s for generating source only following start.00\n");
    printf(" -t for tracing only following systemfile.dat, it reads ray from begin.dat\n");
    printf(" -a to generate and trace all toghether\n\n");
  }
  /*
  for(i=0;i<argc;i++)
    printf("%s ",argv[i]);
  printf("\n");
  */
  poolSource src;
  poolOE     oe;

  double    *ray=NULL;

  char       in_0X[10][1024] ;
  FILE      *sysFile;

  int        iCount;
  char       str_i[7];
  int        nCol, nPoint;


  if ( use_src ) {
    CShadowPoolSourceLoad ( &src, "start.00" );    
    ray = CShadowAllocateBeamFromPool ( &src, ray );
    CShadowSourceGeom ( &src, ray );
    CShadowBeamWrite ( ray, src.NCOL, src.NPOINT, "begin.dat" );
    nPoint = src.NPOINT;
    nCol   = src.NCOL;
  }

  if ( use_trc && !use_src ) {    
    CShadowBeamGetDim ( &nCol, &nPoint, "begin.dat" );
    ray = CShadowAllocateBeam (nPoint, ray);
    CShadowBeamLoad ( ray, nCol, nPoint, "begin.dat" );
    CShadowBeamWrite( ray, nCol, nPoint, "debug.dat" );
  }
  if ( use_trc ) {
    sysFile = fopen ( "systemfile.dat","r" );
    iCount = count_lines ( sysFile );

    rewind ( sysFile );
    for ( i=0;i<iCount;i++ )
      fscanf ( sysFile, "%s", in_0X[i] );

    fclose ( sysFile );
    for ( i=0;i<iCount;i++ ) {
      CShadowPoolOELoad ( &oe, in_0X[i] );

#ifdef __OPENMP          
      int slice,tid,nthreads;
      //omp_set_num_threads(2);

#pragma omp parallel shared(oe,ray,nPoint,i) private(slice,tid)
      {
        printf("thread %d / %d active\n",tid=omp_get_thread_num(),nthreads=omp_get_num_threads());
#pragma        omp barrier
        slice=nPoint/nthreads;
        if(tid!=(nthreads-1)){
          CShadowTraceOE ( &oe, &(ray[slice * 18 * tid]), slice, i+1 );
      }
        else{
          CShadowTraceOE ( &oe, &(ray[slice * tid * 18]), slice+ nPoint%nthreads, i+1 );

        }
      }
#else
          CShadowTraceOE ( &oe, ray,nPoint, i+1 );
#endif      
      sprintf ( str_i,"star.0%d",i+1 );      
      CShadowBeamWrite ( ray, nCol, nPoint, str_i );
    }
  }

  return EXIT_SUCCESS;
}
