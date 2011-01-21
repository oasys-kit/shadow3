
//#include <complex.h>
#include "ShadowMask.hpp"
/*
 *  Ray Class methods here
 */

Ray::Ray() { 
  this->rays=NULL;
}

Ray::~Ray() {
  free ( this->rays );
}

Ray::Ray ( const Ray &r )
{
  this->rays=NULL;
  init ( 18, r.nPoint );
  for ( int i=0; i<nCol*nPoint ; i++ )
    this->rays[i] = r.rays[i];
}

void Ray::init ( int col, int point )
{
  this->rays = ( double* ) realloc ( ( void* ) this->rays, 18*point*sizeof ( double ) );
  this->nCol = col;
  this->nPoint = point;
}

void Ray::genSource ( Source *Src )
{
  init ( 18,Src->NPOINT );
  if ( ( Src->FDISTR==4 ) || ( Src->FSOURCE_DEPTH==4 ) || ( Src->F_WIGGLER>0 ) ) {
    CShadowSourceSync ( Src, this->rays );
  }
  else {
    CShadowSourceGeom ( Src, this->rays );
  }
}

void Ray::trace ( OE *Oe, int iCount )
{
  CShadowTrace ( Oe, this->rays, this->nPoint, iCount );
}

void Ray::write ( char *FileDat )
{
  CShadowWriteRay ( this->rays, this->nCol, this->nPoint, FileDat );
}

void Ray::read ( char *FileDat )
{
  CShadowGetDimRay ( & ( this->nCol ), & ( this->nPoint ), FileDat );
  this->rays = ( double* ) realloc ( ( void* ) this->rays, 18*this->nPoint*sizeof ( double ) );
  CShadowReadRay ( this->rays, this->nCol, this->nPoint, FileDat );
}

void Ray::ffresnel2D ( double dist, dComplex *image, pixel *x, pixel *z ) {
  CShadowFFresnel2D ( this->rays, this->nPoint, dist, image, x, z );
}

/*
 *  Source Class methods here
 */
Source::Source()
{
  CShadowSetupDefaultSource ( ( poolSource* ) this );
}

Source::Source ( char *FileStart00 )
{
  CShadowSetupDefaultSource ( ( poolSource* ) this );
  read ( FileStart00 );
}

Source::Source ( const Source &Src )
{
  poolSourceUnion *tmp1, *tmp2;
  tmp1 = ( poolSourceUnion* ) &Src;
  tmp2 = ( poolSourceUnion* ) this;
  for ( int i=0;i<sizeof ( poolSource );i++ )
    tmp2->poolSourceBit[i]=tmp1->poolSourceBit[i];
}

void Source::write ( char *FileStart00 )
{
  CShadowPoolSourceWrite ( this, FileStart00 );
}

void Source::read ( char *FileStart00 )
{
  CShadowPoolSourceLoad ( this, FileStart00 );
}

/*
 *  Optical Element Class methods here
 */

OE::OE()
{
  CShadowSetupDefaultOE ( ( poolOE* ) this );
}
OE::OE ( char *FileStart0X )
{
  CShadowSetupDefaultOE ( ( poolOE* ) this );
  read ( FileStart0X );
}

OE::OE ( const OE &oe )
{
  poolOEUnion *tmp1, *tmp2;
  tmp1 = ( poolOEUnion* ) &oe;
  tmp2 = ( poolOEUnion* ) this;
  for ( int i=0;i<sizeof ( poolOE );i++ )
    tmp2->poolOEBit[i]=tmp1->poolOEBit[i];
}

void OE::write ( char *FileStart0X )
{
  CShadowPoolOEWrite ( this, FileStart0X );
}

void OE::read ( char *FileStart0X )
{
  CShadowPoolOELoad ( this, FileStart0X );
}

//void Source::init()
//{
//  CShadowSetupDefaultSource(this);
//}





