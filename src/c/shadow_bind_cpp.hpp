
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
//#include <complex>

#ifndef __SHADOWMASK_HPP__
#define __SHADOWMASK_HPP__


extern "C" {
#include "shadow_bind_c.h"
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
  void load(char*);
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
  void load(char*);
};

/*
 *  Beam Class here
 */
class Beam
{
protected:
  int nCol;
  int nPoint;

public:
  double *rays;
  Beam();
  ~Beam();
  Beam(const Beam&);
  void init(int, int);
  void write(char*);
  void load(char*);
  void genSource(Source*);
  void traceOE(OE*,int);
  void ffresnel2D(double, dComplex*, pixel*, pixel*);
};


#endif


