//  Declaration of class PdbRpcDeadNoise
//  Purpose: Stores dead channel, noisy channel, and cluster size information for RPCs
//  Author: Richard Hollis (rhollis@ucr.edu)

#include <PdbRpcDeadNoise.hh>

#include <phool.h> // for PHWHERE

#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>

using namespace std;

//______________________________________________________
PdbRpcDeadNoise::PdbRpcDeadNoise()
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
PdbRpcDeadNoise::PdbRpcDeadNoise(
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
PdbRpcDeadNoise::PdbRpcDeadNoise( const PdbRpcDeadNoise& ref )
{
  //This is the copy constructor.
  fArmNum        = ref.getArm();
  fStationNum    = ref.getStation();
  fOctantNum     = ref.getOctant();
  fHalfOctantNum = ref.getHalfOctant();
  fRadSegment    = ref.getRadSegment();
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    fDead[istrip]        = ref.get_Dead(istrip);
    fNoise[istrip]       = ref.get_Noise(istrip);
    fClusterSize[istrip] = ref.get_ClusterSize(istrip); }
}

//_________________________________________________________________
PdbRpcDeadNoise& PdbRpcDeadNoise::operator = (const PdbRpcDeadNoise& ref )
{
  //Operator "=" definition.
  fArmNum        = ref.getArm();
  fStationNum    = ref.getStation();
  fOctantNum     = ref.getOctant();
  fHalfOctantNum = ref.getHalfOctant();
  fRadSegment    = ref.getRadSegment();
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    fDead[istrip]        = ref.get_Dead(istrip);
    fNoise[istrip]       = ref.get_Noise(istrip);
    fClusterSize[istrip] = ref.get_ClusterSize(istrip); }
  
  return *this;
}

//______________________________________________________
void PdbRpcDeadNoise::zero() 
{
  //Set all variables to zero by default.
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    fDead[istrip]        = 0;
    fNoise[istrip]       = 0.;
    fClusterSize[istrip] = 0.; }
}

//______________________________________________________
void PdbRpcDeadNoise::print() const 
{
  //Output some information
  write(cout); 
}

//______________________________________________________
void PdbRpcDeadNoise::write(ostream& os) const 
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
    os << fDead[istrip] << " "
       << setprecision(3) // three decimal places
       << fNoise[istrip] << " " << fClusterSize[istrip] << " "; }

  os << endl << endl;  
}

//______________________________________________________
void PdbRpcDeadNoise::read(istream& is) 
{
  //Read in the geometry
  is >> fArmNum  
     >> fStationNum  
     >> fOctantNum  
     >> fHalfOctantNum  
     >> fRadSegment;
  
  for(int istrip=0 ; istrip<NRPCSTRIPS ; istrip++) {
    is >> fDead[istrip];
    is >> fNoise[istrip];
    is >> fClusterSize[istrip]; }
}

//______________________________________________________
int PdbRpcDeadNoise::getUniqueId() const
{
  int id = fRadSegment;//2 bits
  id += fHalfOctantNum<<2;//1 bit
  id += fOctantNum<<3;//3 bits
  id += fStationNum<<6;//2 bits
  id += fArmNum<<8;//1 bit
  
  return id;
}
