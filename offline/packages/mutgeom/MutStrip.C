// $Id: MutStrip.C,v 1.17 2008/06/24 13:02:17 hpereira Exp $

/*!
  \file MutStrip.C
  \brief Describes a Strip of the muon tracker system. 
  \author Douglas Fields, Nicki Bruner

*/

#include <iostream>
#include "MutStrip.h"

using namespace std;

//______________________________________________________________
MutStrip::MutStrip(const MutArm* Arm,
           const MutStation* Station,
           const MutOctant* Octant,
           const MutHalfOctant* HalfOctant,
           const MutGap* Gap,
           const MutPlane* Plane,
           const int& StripNum)
  : f_pArm(Arm),
    f_pStation(Station),
    f_pOctant(Octant),
    f_pHalfOctant(HalfOctant),
    f_pGap(Gap),
    f_pPlane(Plane),
    fStripNum(StripNum),
    fGlobalPositionBegin(0,0,0),
    fGlobalPositionEnd(0,1,0)
{
  name = "Strip";
  ChannelDead = False;
  IsStripAttenuated=False;
  DCMChannel=-1;
  packet_ID=-1;
}

//______________________________________________________________
MutStrip::~MutStrip()
{ }

//______________________________________________________________
void MutStrip::printLocation() const
{
  cout << "MutStrip::printLocation - ["
    << getPacket_ID() << "," 
    << getDCMChannel() << "] "
    << getArm() << " "
    << getStation() << " "
    << getOctant() << " "
    << getHalfOctant() << " "
    << getGap() << " "
    << getPlane() << " "
    << getStrip() 
    << endl;
}

//______________________________________________________________
void MutStrip::print() const
{
  cout << name <<" "<<fStripNum<<"\n";
  cout << " Begin point = ";
  fGlobalPositionBegin.print();
  cout << " End point = "; 
  fGlobalPositionEnd.print();
  if(ChannelDead) cout << "This strip is inactive.\n";
  else cout << "This strip is active.\n";
}

//______________________________________________________________
double MutStrip::getAngle() const
{
  return atan2(
    (fGlobalPositionEnd.getY() - fGlobalPositionBegin.getY()),
    (fGlobalPositionEnd.getX() - fGlobalPositionBegin.getX()));
}

//______________________________________________________________
void MutStrip::translate(const PHPoint &translation)
{
  fGlobalPositionBegin = fGlobalPositionBegin + translation;
  fGlobalPositionEnd = fGlobalPositionEnd + translation;
}
