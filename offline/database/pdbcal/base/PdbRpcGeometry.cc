//  Declaration of class PdbRpcGeometry
//  Purpose: Stores Geometry information for RPCs
//  Author: Richard Hollis (rhollis@ucr.edu)

#include <PdbRpcGeometry.hh>

#include <phool.h> // for PHWHERE

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>

using namespace std;

//______________________________________________________
PdbRpcGeometry::PdbRpcGeometry()
{
  //This is the default constructor.
  //We nullify everything to make sure there are no slip-ups
  fArmNum        = -1;
  fStationNum    = -1;
  fOctantNum     = -1;
  fHalfOctantNum = -1;
  fRadSegment    = -1;
  
  zero();
}

//______________________________________________________
PdbRpcGeometry::PdbRpcGeometry(
  const int iarm, const int istation, const int ioctant,
  const int ihalfoctant, const int iradsegment)
{
  //This is the constructor.
  fArmNum        = iarm;
  fStationNum    = istation;
  fOctantNum     = ioctant;
  fHalfOctantNum = ihalfoctant;
  fRadSegment    = iradsegment;
  
  zero();
}

//______________________________________________________
PdbRpcGeometry::PdbRpcGeometry( const PdbRpcGeometry& ref )
{
  //This is the copy constructor.
  fArmNum        = ref.getArm();
  fStationNum    = ref.getStation();
  fOctantNum     = ref.getOctant();
  fHalfOctantNum = ref.getHalfOctant();
  fRadSegment    = ref.getRadSegment();
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    fInvalid[istrip]   = ref.get_Invalid(istrip);
    fPosXBegin[istrip] = ref.get_PosXBegin(istrip);
    fPosYBegin[istrip] = ref.get_PosYBegin(istrip);
    fPosZBegin[istrip] = ref.get_PosZBegin(istrip);
    fPosXEnd[istrip]   = ref.get_PosXEnd(istrip);
    fPosYEnd[istrip]   = ref.get_PosYEnd(istrip);
    fPosZEnd[istrip]   = ref.get_PosZEnd(istrip);
    fStripArea[istrip] = ref.get_StripArea(istrip); }
  fPacketOffset = ref.get_PacketOffset();
}

//_________________________________________________________________
PdbRpcGeometry& PdbRpcGeometry::operator = (const PdbRpcGeometry& ref )
{
  //Operator "=" definition.
  fArmNum        = ref.getArm();
  fStationNum    = ref.getStation();
  fOctantNum     = ref.getOctant();
  fHalfOctantNum = ref.getHalfOctant();
  fRadSegment    = ref.getRadSegment();
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    fInvalid[istrip]   = ref.get_Invalid(istrip);
    fPosXBegin[istrip] = ref.get_PosXBegin(istrip);
    fPosYBegin[istrip] = ref.get_PosYBegin(istrip);
    fPosZBegin[istrip] = ref.get_PosZBegin(istrip);
    fPosXEnd[istrip]   = ref.get_PosXEnd(istrip);
    fPosYEnd[istrip]   = ref.get_PosYEnd(istrip);
    fPosZEnd[istrip]   = ref.get_PosZEnd(istrip);
    fStripArea[istrip] = ref.get_StripArea(istrip); }
  fPacketOffset = ref.get_PacketOffset();
  
  return *this;
}

//______________________________________________________
void PdbRpcGeometry::zero() 
{
  //Set all variables to zero by default.
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    fInvalid[istrip]   = 0;
    fPosXBegin[istrip] = 0.;
    fPosYBegin[istrip] = 0.;
    fPosZBegin[istrip] = 0.;
    fPosXEnd[istrip]   = 0.;
    fPosYEnd[istrip]   = 0.;
    fPosZEnd[istrip]   = 0.;
    fStripArea[istrip] = 0.; }
  fPacketOffset = 0;
}

//______________________________________________________
void PdbRpcGeometry::print() const 
{
  //Output some information
  write(cout); 
}

//______________________________________________________
void PdbRpcGeometry::write(ostream& os) const 
{
  //Output some information
  os << setiosflags(ios::fixed) 
     << setw(1); // one digit only for the first numbers 
  os << fArmNum << " " 
     << fStationNum << " " 
     << fOctantNum << " " 
     << fHalfOctantNum << " " 
     << fRadSegment << " " ;
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    os << fInvalid[istrip] << " "
       << setprecision(3) // three decimal places
       << fPosXBegin[istrip] << " " << fPosYBegin[istrip] << " " << fPosZBegin[istrip] << " "
       << fPosXEnd[istrip]   << " " << fPosYEnd[istrip]   << " " << fPosZEnd[istrip]   << " "
       << fStripArea[istrip] << " "; }
  os << fPacketOffset;

  os << endl << endl;  
}

//______________________________________________________
void PdbRpcGeometry::read(istream& is) 
{
  //Read in the geometry
  is >> fArmNum  
     >> fStationNum  
     >> fOctantNum  
     >> fHalfOctantNum  
     >> fRadSegment;
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    is >> fInvalid[istrip];
    is >> fPosXBegin[istrip];
    is >> fPosYBegin[istrip];
    is >> fPosZBegin[istrip];
    is >> fPosXEnd[istrip];
    is >> fPosYEnd[istrip];
    is >> fPosZEnd[istrip];
    is >> fStripArea[istrip]; }
  is >> fPacketOffset;
}

//______________________________________________________
int PdbRpcGeometry::getUniqueId() const
{
  int id = fRadSegment;//2 bits
  id += fHalfOctantNum<<2;//1 bit
  id += fOctantNum<<3;//3 bits
  id += fStationNum<<6;//2 bits
  id += fArmNum<<8;//1 bit
  
  return id;
}
