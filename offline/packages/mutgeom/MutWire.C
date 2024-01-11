#include <iostream>
#include "MutWire.h"

using namespace std;

//_______________________________________
MutWire::MutWire(const MutArm* Arm,
           const MutStation* Station,
           const MutOctant* Octant,
           const MutHalfOctant* HalfOctant,
           const MutGap* Gap,
           const MutPlane* Plane,
           const int& WireNum)
  : f_pArm(Arm),
    f_pStation(Station),
    f_pOctant(Octant),
    f_pHalfOctant(HalfOctant),
    f_pGap(Gap),
    f_pPlane(Plane),
    fWireNum(WireNum)
{
  name = "Wire";
  ChannelDead = False;
}

//_______________________________________
MutWire::~MutWire()
{}


//_______________________________________
void MutWire::print() const
{
  cout << name <<" "<<fWireNum<<"\n";
  cout << " Begin point = ";
  fGlobalPositionBegin.print();
  cout << " End point = "; 
  fGlobalPositionEnd.print();
  if(ChannelDead) cout << "This wire is disabled.\n";
  else cout << "This wire is active.\n";
}


//_______________________________________
double MutWire::getAngle()
{
  return atan2((fGlobalPositionEnd.getY() - fGlobalPositionBegin.getY()),
                 (fGlobalPositionEnd.getX() - fGlobalPositionBegin.getX()));
}

//_______________________________________
void MutWire::translate(const PHPoint &translation)
{
  fGlobalPositionBegin = fGlobalPositionBegin + translation;
  fGlobalPositionEnd = fGlobalPositionEnd + translation;
}
