#include <iostream>
#include "PdbMutGeom.hh"

using namespace std;

//__________________________________________
PdbMutGeom::PdbMutGeom()
{
  setGlobalPosition(0,0,0);
  setGlobalVector(0,0,0);
}

//__________________________________________
PdbMutGeom::PdbMutGeom( const PdbMutGeom& base_ref ):
  _id               ( base_ref._id               ),
  ArmNum            ( base_ref.ArmNum            ),
  StationNum        ( base_ref.StationNum        ),
  OctantNum         ( base_ref.OctantNum         ),
  fGlobalPosition_x ( base_ref.fGlobalPosition_x ),
  fGlobalPosition_y ( base_ref.fGlobalPosition_y ),
  fGlobalPosition_z ( base_ref.fGlobalPosition_z ),
  fGlobalVector_x   ( base_ref.fGlobalVector_x   ),
  fGlobalVector_y   ( base_ref.fGlobalVector_y   ),
  fGlobalVector_z   ( base_ref.fGlobalVector_z   )
{}

//__________________________________________
PdbMutGeom& PdbMutGeom::operator = ( const PdbMutGeom& base_ref )
{
  _id               = base_ref._id;
  ArmNum            = base_ref.ArmNum;
  StationNum        = base_ref.StationNum;
  OctantNum         = base_ref.OctantNum;
  fGlobalPosition_x = base_ref.fGlobalPosition_x;
  fGlobalPosition_y = base_ref.fGlobalPosition_y;
  fGlobalPosition_z = base_ref.fGlobalPosition_z;
  fGlobalVector_x   = base_ref.fGlobalVector_x;
  fGlobalVector_y   = base_ref.fGlobalVector_y;
  fGlobalVector_z   = base_ref.fGlobalVector_z;

  return *this;
}

//__________________________________________
PdbMutGeom::~PdbMutGeom()
{}

//__________________________________________
void PdbMutGeom::print() const
{
  cout << "PdbMutGeom object" << endl;
  cout << "index=(" << _id << "," << ArmNum << "," << StationNum << "," << OctantNum << ")";
  cout << " position=("
    << fGlobalPosition_x << ","
    << fGlobalPosition_y << ","
    << fGlobalPosition_z << ")";
    
  cout << " orientation vector=(" 
    << fGlobalVector_x << ","
    << fGlobalVector_y << ","
    << fGlobalVector_z << ")"
    << endl;
}

