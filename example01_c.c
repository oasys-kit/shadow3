#include <stdio.h>
#include <string.h>
#include <ShadowMask.h>

int main()
{
  poolSource src;
  poolOE     oe1;
  double    *ray;

  // load variables from start.00
  CShadowPoolSourceLoad(&src, "start.00");

  printf(" Number of rays: %d\n", src.NPOINT);
  src.NPOINT=100000;
  printf(" Number of rays (modified): %d\n", src.NPOINT);

  // allocate ray
  ray = CShadowAllocateRay(&src,ray);

  // calculate source
  CShadowSourceSync(&src, ray);
  CShadowWriteRay(18,src.NPOINT,ray,"begin.dat");
  // reads start.01 into oe1
  CShadowPoolOELoad(&oe1,"start.01");
  // traces OE1
  CShadowTrace(&oe1,ray,src.NPOINT,1);

  // write file star.01
  CShadowWriteRay(18,src.NPOINT,ray,"star.01");

  free(ray);
  return EXIT_SUCCESS;
}
