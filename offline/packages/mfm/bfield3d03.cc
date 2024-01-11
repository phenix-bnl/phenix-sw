// $Id: bfield3d03.cc,v 1.4 2008/07/26 17:34:13 hpereira Exp $

/*!
   \file bfield3d03.cc
   \brief fortran wrapper to PHBFieldMap
   \author Hugo Pereira Da Costa
   \version $Revision: 1.4 $
   \date $Date: 2008/07/26 17:34:13 $
*/

#include <iostream>
#include <string>
#include <cstdlib>
#include <unistd.h>
#include <cmath>

#include "PHBFieldMap.h"

using namespace std;

//_____________________________________________________________
//! initialize the magnetic field at first call. Stores the magnetic field at point \a xyz in \a bxyz
/*! this method is called internally in GEANT via gufld, reimplemented by PISA */
extern "C" void bfield3d03_( float* xyz, float* bxyz, int* code, char* filename = 0, int* size = 0 )
{
    
  static PHBFieldMap field_map;
  if( !field_map.initialized() )
  {
    
    // storage for hard-coded magnetic field name (depending on code value)
    string localname;
    switch(*code) {

      case 0:
      localname = "Sim3D03.root";
      break;

      case 1:
      localname = "Sim3D++.root";
      break;

      case 2:

      // check file
      if( !(filename && size ) )
      {
        cout << "bfield3d03_ - illegal arguments for code " << *code << endl;
        exit(1);
      }
      
      // assign filename
      localname = string( filename, *size-1 );
      break;
      
      case 3:
      localname = "Sim3D+-.root";
      break;

      default:
      cout << "bfield3dO3v2_ - illegal code: " << *code << endl;
      exit(1);
    }    

    // storage for afs magnetic field
    string afsname( string( "/afs/rhic.bnl.gov/phenix/software/simulation/" ) + localname );
      
    // decide wether local of afs name should be used
    string fileName( localname );
    if( access( fileName.c_str(), R_OK ) != 0 )
    {
      cout << "bfield3d03_ - Local file version " << fileName << " is not present." << endl;
      cout << "bfield3d03_ - Will use standard version " << afsname << "  " << endl;
      fileName = afsname;
    
    }
    
    // try load afs file
    if( access( fileName.c_str(), R_OK ) != 0 )
    {
      cout << "bfield3d03_ - Unable to find " << fileName << endl;
      cout << "bfield3d03_ - Exiting " << endl;
      exit(1);    
    }
   
    // do the initialization
    field_map.initialize( fileName );
    field_map.set_interpolation_mode( PHBFieldMap::LINEAR );

  }
 
  // compute magnetic field
  const PHBFieldMap::BField& field( field_map.get_field( xyz[0], xyz[1], xyz[2] ) );
  
  // update values
  bxyz[0] = field.bx;
  bxyz[1] = field.by;
  bxyz[2] = field.bz;
    
  return;
  
}
