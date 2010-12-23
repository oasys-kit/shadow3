#include <stdio.h>
#include <string.h>
#include <ShadowMask.h>



int count_lines(FILE *f){
  char ch = "\0";
  int  c  = 0;
  while(ch!=EOF){
    ch=fgetc(f);
    if(ch=='\n') c++;
  }
  return c;
}

int main(int argc, char** argv)
{
  int i;
  int use_trc = 0;
  int use_src = 0;
  // manage input args

  if( argc==1 ) { use_trc = 1; }
  else{
    if ( !strcmp(argv[1],"-s") ) use_src = 1; 
    if ( !strcmp(argv[1],"-a") ) use_src = 1, use_trc = 1; 
  }
  /*
  for(i=0;i<argc;i++)
    printf("%s ",argv[i]);
  printf("\n");
  */
  poolSource src;
  poolOE     oe;

  double    *ray;
  
  char      *in_00   = ( char* ) "start.00";
  char      *out_00  = ( char* ) "begin.dat";
  char       in_0X[10][1024] ;
  FILE      *sysFile;
  
  int        iCount;
  char       str_i[7];
  int        nCol, nPoint;


  if(use_src){
    CShadowPoolSourceLoad(&src, in_00);
    ray = CShadowAllocateRay(&src, ray);
    CShadowSource1B(&src, ray);
    CShadowWriteRay(src.NCOL, src.NPOINT, ray, out_00);
    nPoint = src.NPOINT;
    nCol   = src.NCOL;
  }
  if(use_trc && !use_src){
    ray = CShadowReadRay(ray, &nCol, &nPoint, out_00);
  }
  if(use_trc){
    sysFile = fopen("systemfile.dat","r");
    iCount = count_lines(sysFile);
    
    rewind(sysFile);
    for (i=0;i<iCount;i++)
      fscanf(sysFile, "%s", in_0X[i]);

    fclose(sysFile);
    for (i=0;i<iCount;i++){      
      CShadowPoolOELoad(&oe, in_0X[i]);
      CShadowTrace(&oe, ray, nPoint, nCol, i+1);
      sprintf(str_i,"star.0%d",i+1);
      CShadowWriteRay(nCol, nPoint, ray, str_i);
    }  
  }

  return EXIT_SUCCESS;
}
