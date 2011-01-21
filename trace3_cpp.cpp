#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cstdio>
#include <cstring>
#include "ShadowMask.hpp"

using namespace std;

int main ( int argc, char** argv )
{
  bool              use_trc = false;
  bool              use_src = false;

  if ( argc==1 ) {
    use_trc = true;
  }
  else {
    if ( !strcmp ( argv[1],"-s" ) )
      use_src = true;
    if ( !strcmp ( argv[1],"-t" ) )
      use_trc = true;
    if ( !strcmp ( argv[1],"-a" ) ) {
      use_src = true;
      use_trc = true;
    }
  }


  ifstream          sysFile;
  vector <string>   files;
  string            tmp1;
  stringstream      tmp2;
  Source            src;
  OE                oe;
  Ray               ray;


  if ( use_src ) {
    src.read ( ( char* ) "start.00" );
    ray.genSource ( &src );
    ray.write ( ( char* ) "begin.dat" );
  }
  if ( use_trc && !use_src ) {
    ray.read ( ( char* ) "begin.dat" );
  }
  if ( use_trc ) {
    sysFile.open ( "systemfile.dat" );
    while ( !sysFile.eof() ) {
      getline ( sysFile,tmp1 );
      if ( !tmp1.empty() )
        files.push_back ( tmp1 );
    }
    sysFile.close();
    for ( int i=0 ; i<files.size() ; i++ ) {
      oe.read ( ( char* ) files[i].c_str() );
      ray.trace ( &oe,i+1 );
      tmp2 << "star.0" << i+1;
      tmp1 = tmp2.str();
      ray.write ( ( char* ) tmp1.c_str() );
    }
  }
  return EXIT_SUCCESS;
}

