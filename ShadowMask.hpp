
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
//#include <complex>

#ifndef __SHADOWMASK_HPP__
#define __SHADOWMASK_HPP__


extern "C" {
#include "ShadowMask.h"
}

/*
 *  Source Class here
 */

using namespace std;

class Source : public poolSource
{
public:
  Source();
  Source(char*);
  Source(const Source&);
  void write(char*);
  void read(char*);
};


/*
 *  Optical Element Class here
 */

class OE : public poolOE
{
public:
  OE();
  OE(char*);
  OE(const OE&);
  void write(char*);
  void read(char*);
};

/*
 *  Ray Class here
 */
class Ray
{
protected:
  int nCol;
  int nPoint;

public:
  double *rays;
  Ray();
  ~Ray();
  Ray(const Ray&);
  void init(int, int);
  void write(char*);
  void read(char*);
  void genSource(Source*);
  void trace(OE*,int);
  void ffresnel2D(double, dComplex*, pixel*, pixel*);
};


#endif


