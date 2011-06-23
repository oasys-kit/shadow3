
//#include <complex.h>
#include "shadow_bind_cpp.hpp"
/*
 *  Beam Class methods here
 */

Beam::Beam() { 
  this->rays=NULL;
}

Beam::~Beam() {
  free ( this->rays );
}

Beam::Beam ( const Beam &r )
{
  this->rays=NULL;
  init ( 18, r.nPoint );
  for ( int i=0; i<nCol*nPoint ; i++ )
    this->rays[i] = r.rays[i];
}

void Beam::init ( int col, int point )
{
  this->rays = ( double* ) realloc ( ( void* ) this->rays, 18*point*sizeof ( double ) );
  this->nCol = col;
  this->nPoint = point;
}

void Beam::genSource ( Source *Src )
{
  init ( 18,Src->NPOINT );
  if ( ( Src->FDISTR==4 ) || ( Src->FSOURCE_DEPTH==4 ) || ( Src->F_WIGGLER>0 ) ) {
    CShadowSourceSync ( Src, this->rays );
  }
  else {
    CShadowSourceGeom ( Src, this->rays );
  }
}

void Beam::traceOE ( OE *Oe, int iCount )
{
  CShadowTraceOE ( Oe, this->rays, this->nPoint, iCount );
}

void Beam::write ( char *FileDat )
{
  CShadowBeamWrite ( this->rays, this->nCol, this->nPoint, FileDat );
}

void Beam::load ( char *FileDat )
{
  CShadowBeamGetDim ( & ( this->nCol ), & ( this->nPoint ), FileDat );
  this->rays = ( double* ) realloc ( ( void* ) this->rays, 18*this->nPoint*sizeof ( double ) );
  CShadowBeamLoad ( this->rays, this->nCol, this->nPoint, FileDat );
}

void Beam::ffresnel2D ( double dist, dComplex *image, pixel *x, pixel *z ) {
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
  load ( FileStart00 );
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

void Source::load ( char *FileStart00 )
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
  load ( FileStart0X );
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

void OE::load ( char *FileStart0X )
{
  CShadowPoolOELoad ( this, FileStart0X );
}

//void Source::init()
//{
//  CShadowSetupDefaultSource(this);
//}





