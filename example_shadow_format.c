//
//
//  Simple C program to write a SHADOW binary file.
//
//  Notes: 
//      reclen is the size of bytes of the array to be written
//      as in FORTRAN, it is written before and after each record.
//
//      the file is filled with values that correspond to the column
//      number starting from zero(to be changed by the user)
//
//  Authors: M. Sanchez del Rio and D. Karkoulis
//
//  Date: 2011, November 17
//

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main()
{
  double *ray=NULL;
  double ray_vec[18];
  int i,j,ierr; 
  int npoint ; 
  int reclen;
  int ncol=18 ; 
  int iflag=1 ; 


  npoint=11000;
  printf(" Number of rays npoint: %d\n", npoint);
  printf(" Number of cols ncol: %d\n", ncol);
  printf(" iflag: %d\n", iflag);

  // allocate ray
  ray = ( double* ) realloc ( ray, 18*npoint*sizeof(double) );
  memset(ray,0,sizeof(double)*18*npoint);
  // fill array with column number
  for(i=0;i<npoint;i++){
	for(j=0;j<ncol;j++){
		ray[i + npoint * j]=j;
	}
  }
  // open file
  FILE *fp = fopen("testio.00", "w");

  // write header
        reclen = 3*sizeof(int);
        fwrite(&reclen, sizeof(int), 1, fp);
            fwrite(&ncol, sizeof(int), 1, fp);
            fwrite(&npoint, sizeof(int), 1, fp);
            fwrite(&iflag, sizeof(int), 1, fp);
        fwrite(&reclen, sizeof(int), 1, fp);
        reclen = ncol * sizeof(double);
        // you can close the file if you want (to be reopened in append
        // mode).
        //fclose(fp);

  // loop over rays
  for (i = 0; i < npoint; ++i) {
       // reopen file if closed before
       //fp=fopen("testio.00","a");
       fwrite(&reclen, sizeof(int), 1, fp);
            //cp from matrix to vector
            for (j = 0; j < 18; j++) {	
		ray_vec[j] = ray[i + npoint * j];	
            }
            fwrite(ray_vec, sizeof(double), 18, fp);
       fwrite(&reclen, sizeof(int), 1, fp);
       // close file (if wanted)
       //fclose(fp);
  }


  // close and exit
  fclose(fp);
  free(ray);
  printf(" File written to disk: testio.00 \n");
  return EXIT_SUCCESS;
}
