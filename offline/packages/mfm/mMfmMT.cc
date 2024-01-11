#define mfmMAIN

// $Id: mMfmMT.cc,v 1.22 2009/08/20 03:50:24 pinkenbu Exp $


/*!
   \file mMfmMT.cc
   \brief static magnetic field initialization 
   \author Sean Kelly, Hugo Pereira Da Costa
   \version $Revision: 1.22 $
   \date $Date: 2009/08/20 03:50:24 $
*/

#include <mMfmMT.h>

#include <cstdlib>
#include <iostream>

// interface to fortran methods

//! forward definition of fortran subroutine used to set the magnetic field scale
extern "C" int mfm_setfscale_(float *fscale);

//! forward definition of fortran subroutine used to retrieve the magnetic field scale
extern "C" int mfm_getfscale_(float *fscale);

//! forward definition of fortran subroutine used to initialize the magnetic field
/*! 
this fortran interface is doing the real job. mMfmMT is only a wrapper class used by the offline reconstruction. 
This separation is required for PISA to be able to use this common interface
*/
extern "C" int mfm_initialize_(int *nmaps, int *interp, float *fscale);

using namespace std;

//______________________________________________________
// default values
float mMfmMT::mapFileScale = 1.0;
int mMfmMT::mapFileFlag = mMfmMT::UNKNOWN;

//______________________________________________________
int mMfmMT::initialize(void)
{
    int nmaps = 1;
    int interp = 2;
    float fscale = getMapFileScale();
    int mapFile = getMapFileFlag();

    cout << "mMfmMT::initialize - mapFile = " << getMapFileFlagName(mapFile) << " (" << mapFile << ")" << endl;
    switch (mapFile)
    {
      
      case MAP_2D_2001:
      cout <<"mMfmMT::initialize - 2001 two dimensional map file" << endl;
      interp = 20;
      break;
      
      case MAP_3D_2001:
      cout <<"mMfmMT::initialize - 2001 three dimensional map file" << endl;
      interp = 30;
      break;

      case MAP_3D_2003:
      cout <<"mMfmMT::initialize - 2003 single coil, three dimensional map file Sim3D03.root" << endl;
      interp = 33;
      break;

      case MAP_3D_PLUS_PLUS:
      cout <<"mMfmMT::initialize - 2004 dual coil, three dimensional map file Sim3D++.root" << endl;
      interp = 34;
      break;
 
      case MAP_3D_PLUS_MINUS:
      cout <<"mMfmMT::initialize - 2007 dual coil, three dimensional map file Sim3D+-.root" << endl;
      interp = 35;
      break;
    
      default:
      cout << "mMfmMT::initialize - unrecognized mapfile flag: " << mapFile << endl;
      cout << "mMfmMT::initialize - use mMfmMT::setMapFileFlag(...) to set a valid index." << endl;
      cout << "mMfmMT::initialize - using mapFileFlag = MAP_3D_PLUS_PLUS in the meanwhile." << endl;
      setMapFileFlag( (mapFile = MAP_3D_PLUS_PLUS) );
      interp = 34;
      break;
    }
    
    mfm_initialize_(&nmaps, &interp, &fscale);

    return 0;

}

//______________________________________________________
void mMfmMT::setMapFileScale(float mapScale) 
{

  cout << "mMfmMT::setMapFileScale - mapFileScale was at " << getMapFileScale() << endl;
  
  mfm_setfscale_(&mapScale);  // this sets the scale factor in the FORTRAN common block
  mapFileScale = mapScale;    // this sets the scale factor in the C++ global area

  cout << "mMfmMT::setMapFileScale - mapFileScale has been reset to " << mapFileScale << endl;
}

//______________________________________________________
float mMfmMT::getMapFileScale() 
{
  
  /* 
    at first the fortran scale is set to the default value
    otherwise it gets overriden by the default fortran value, 
    which is 0
  */
  static bool first( true );
  if( first ) {
    first = false;
    mfm_setfscale_( &mapFileScale );
  } 

  // retrieves the value from fortran
  mfm_getfscale_(&mapFileScale);  
  return mapFileScale;
}

//______________________________________________________
void mMfmMT::setMapFileFlag(int mapFile) 
{

  cout << "mMfmMT::setMapFileFlag - mapFile = " << getMapFileFlagName(mapFile) << " (" << mapFile << ")" << endl;
  switch(mapFile) 
  {
    
    case MAP_2D_1997:
    cout << "mMfmMT::setMapFileFlag - 1997 two dimensional map file" << endl;
    cout << "mMfmMT::setMapFileFlag - CAUTION: This map file has the wrong polarity in the Muon Arm" << endl;
    break;

    case MAP_2D_2001:
    cout << "mMfmMT::setMapFileFlag - 2001 two dimensional map file" << endl;
    cout << "mMfmMT::setMapFileFlag - The map file has the correct polarity in the Muon Arm" << endl;
    break;
    
    case MAP_3D_2001:
    cout << "mMfmMT::setMapFileFlag - 2001 three dimensional map file" << endl;
    cout << "mMfmMT::setMapFileFlag - The map file is based on the analysis of the field map measurements" << endl;
    cout << "mMfmMT::setMapFileFlag - The map file DOES NOT INCLUDE the North Muon Arm" << endl;
    break;

    case MAP_3D_2003:
    cout << "mMfmMT::setMapFileFlag - 2003 single coil, three dimensional map file" << endl;
    cout << "mMfmMT::setMapFileFlag - The map file is based on the analysis of the field map measurements" << endl;
    break;

    case MAP_3D_PLUS_PLUS:
    cout << "mMfmMT::setMapFileFlag - 2004 dual coil, three dimensional map file" << endl;
    cout << "mMfmMT::setMapFileFlag - The map file is based on the analysis of the field map measurements" << endl;
    break;

    case MAP_3D_PLUS_MINUS:
    cout << "mMfmMT::setMapFileFlag - 2007 dual coil, three dimensional map file" << endl;
    cout << "mMfmMT::setMapFileFlag - +- polarity" << endl;
    break;

    default:  // error condition
    cerr << "mMfmMT::setMapFileFlag - ERROR: unrecognized mapFile choice " << mapFile << endl;
    exit(1);

  } 
  
  mapFileFlag = mapFile;

}

//______________________________________________________
int mMfmMT::getMapFileFlag() 
{ return mapFileFlag; }
