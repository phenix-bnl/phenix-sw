#include <iostream>
#include "PdbMutDCMMap.hh"

using namespace std;

PdbMutDCMMap::PdbMutDCMMap(){}

PdbMutDCMMap::~PdbMutDCMMap(){}

void PdbMutDCMMap::setAllIdentifiers(int newarm, int newsta, int newoct, 
				     int newhalfoct, int newgap, int newplane)
{
  arm = newarm;
  station = newsta;
  oct = newoct;
  halfOct = newhalfoct;
  gap = newgap;
  plane = newplane;
}

void PdbMutDCMMap::print() const
{
  cout<<"MuTr DCM Channel Map for Arm "<<arm<<", Station "
      <<station<<", Octant "<<oct<<", halfOct "<<halfOct<<endl;
  cout<<" gap "<<gap<<", plane"<<plane<<endl;
  cout<<"strip /t packetID /t DCM channel /n";
  for(int strip=0;strip<MaxStrips;strip++){
    cout<<strip<<"/t"<<PacketID[strip]<<"/t"<<DCMChannel[strip]<<endl;
  }
}
