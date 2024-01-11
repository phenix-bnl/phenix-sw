// ====================================================================
// IMPLEMENTATIONS OF MutGap METHODS.
// ====================================================================
/**
 * Describes a Gap of the muon tracker system.
 *
 * @author Douglas Fields \URL{mailto:fields@unm.edu}
 * @author Nicki Bruner \URL{mailto:bruner@glueball.phys.unm.edu}
 *
 */

#include <vector>
#include <iostream>
#include "MutPlane.h"

using namespace std;
using namespace MUTGEOM;

//_______________________________________
MutGap::MutGap()
  : f_pArm(0),
    f_pStation(0),
    f_pOctant(0),
    f_pHalfOctant(0),
    fGapNum(Gap1),
    f_landauOffset(30),
    f_landauScale(15)
{
  SetGlobalGeom(0,0,0,0,0,1);
}

//_______________________________________
MutGap::MutGap(const MutArm* Arm,
       const MutStation* Station,
       const MutOctant* Octant,
       const MutHalfOctant* HalfOctant,
       const GapNumber& GapNum)
  : f_pArm(Arm),
    f_pStation(Station),
    f_pOctant(Octant),
    f_pHalfOctant(HalfOctant),
    fGapNum(GapNum),
    f_landauOffset(30),
    f_landauScale(15)
{
  /*
  The Gap's position and orientation are derived from its
  half-octant's position/orientation plus the gap's offset
  perpendicular to the half-octant's plane.
  */

 switch(fGapNum) {
 case Gap1:
   name = "Gap 1";
   break;
 case Gap2:
   name = "Gap 2";
   break;
 case Gap3:
   name = "Gap 3";
   break;
 }

 fGlobalVector=f_pHalfOctant->getGlobalVector();

 /* The gap's position is the location of the survey reference point (pin1)
    plus the perpendicular distance (offset) from the upstream frame to the
    anode (wire) plane of that gap.
    6.35 mm is the nominal gap thickness for all three stations.
    This value increases for Stations 1 and 3 from the glueing process.
 */
 gapThickness = 0.6440;  //in cm
 double frameThickness = 0.635;  //dummy initialization
 double panelThickness = 1.402;  //dummy initialization

 switch(getStation()) {
 case Station1:
   panelThickness = 1.402; // in cm
   frameThickness = 0.635;  //in cm
   break;
 case Station2:
   panelThickness = 0.794;
   frameThickness = 2.3645;//Actual frame is 2.841 cm, but station 2
                           //has a separation *between* the gaps that is
                           //folded into these numbers.
                           //distance from frame to first anode plane=3.476cm
                           //distance from first anode to second = 1.429cm
   gapThickness = 0.6350;  //This number is exact since it is used in MutPlane
   break;
 case Station3:
   panelThickness = 2.165;
   frameThickness = 0.635;  //in cm
   break;
 }

  gapOffset = fGlobalVector * (frameThickness +
      (fGapNum+1)*(gapThickness+panelThickness) - gapThickness/2)* -1.0;

  fGlobalPosition=f_pHalfOctant->getGlobalPosition();
  fGlobalPosition=fGlobalPosition + gapOffset;

  // Create Plane Objects and store pointers to them
  f_pMutPlanes = vector<MutPlane*>(NumberOfPlanes);
  for (int j=0; j<NumberOfPlanes; j++)
  f_pMutPlanes[j] = new MutPlane(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,this,PlaneNumber(j));;

}

//_______________________________________
MutGap::~MutGap()
{

  for( unsigned int i=0; i<f_pMutPlanes.size(); i++ )
  delete f_pMutPlanes[i];
  f_pMutPlanes.clear();

}

//_______________________________________
void MutGap::RefreshPlane(const PlaneNumber& PlaneNum)
{

  if (f_pMutPlanes[PlaneNum]) {

    // The Plane object exists - delete it and create a new one.
    delete f_pMutPlanes[PlaneNum];
    f_pMutPlanes[PlaneNum] =
    new MutPlane(f_pArm,f_pStation,f_pOctant,f_pHalfOctant,this,PlaneNum);

  } else cout << "Cannot refresh non-existant Plane." << "\n";
}

//_______________________________________
void MutGap::translate(const PHPoint &translation)
{

  fGlobalPosition = fGlobalPosition + translation;
  for (int i=0; i<NumberOfPlanes; i++) {
    if(f_pMutPlanes[i]) f_pMutPlanes[i]->translate(translation);
  }
}

//_______________________________________
void MutGap::rotate(float angle, char axisLabel)
{
  rotateThis(angle, axisLabel);
  for (int i=0; i<NumberOfPlanes; i++)
    if(f_pMutPlanes[i]) f_pMutPlanes[i]->rotate(angle, axisLabel);
}

//_______________________________________
void MutGap::XYExpansion(float expansionPercentage)
{
  for (int i=0; i<NumberOfPlanes; i++)
    if(f_pMutPlanes[i]) f_pMutPlanes[i]->XYExpansion(expansionPercentage);
}

//_______________________________________
void MutGap::transformToNewFrame(PHFrame oldFrame, PHFrame newFrame)
{
  fGlobalPosition = rotateAndTranslate(oldFrame, fGlobalPosition, newFrame);
  fGlobalVector = rotateAndTranslate(oldFrame, fGlobalVector, newFrame);
  for (int i=0; i<NumberOfPlanes; i++) {
    if(f_pMutPlanes[i]) f_pMutPlanes[i]->transformToNewFrame(oldFrame,newFrame);
  }
}
