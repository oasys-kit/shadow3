#include <iostream>
#include <ShadowMask.hpp>

using namespace std;

int main()
{
  Source src;
  OE     oe1;
  Ray    ray;

  // load variables from start.00
  src.read( (char*) "start.00");

  cout << " Number of rays: " << src.NPOINT << endl;
  src.NPOINT=100000;
  cout << " Number of rays (modified): " << src.NPOINT << endl;

  // calculate source
  ray.genSource(&src);
  ray.write( (char*) "begin.dat");
  // reads start.01 into oe1
  oe1.read( (char*) "start.01");
  // traces OE1
  ray.trace(&oe1,1);

  // write file star.01
  ray.write( (char*) "star.01");

  return EXIT_SUCCESS;
}
