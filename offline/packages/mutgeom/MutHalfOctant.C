// $Id: MutHalfOctant.C,v 1.13 2006/12/21 14:59:57 hpereira Exp $

/*!
  \file MutHalfOctant.C
  \brief Describes a HalfOctant of the muon tracker system. 

  \author Douglas Fields, Nicki Bruner

*/

#include "PHGeometry.h"
#include "MutStrip.h"
#include "MutWire.h"

#include <vector>
#include <iostream>

using namespace std;
using namespace PHGeometry;
using namespace MUTGEOM;

//____________________________________________
MutHalfOctant::MutHalfOctant(const MutArm* Arm,
               const MutStation* Station,
               const MutOctant* Octant,
               const HalfOctantNumber& HalfOctantNum)
  : f_pArm(Arm),
    f_pStation(Station),
    f_pOctant(Octant),
    fHalfOctantNum(HalfOctantNum)
{
  switch(fHalfOctantNum) 
  {
    case HalfOctant1:
      name = "HalfOctant 1";
       break;
     case HalfOctant2:
       name = "HalfOctant 2";
       break;
   }

  fGlobalPosition=f_pOctant->getGlobalPosition();
  fGlobalVector=f_pOctant->getGlobalVector();

  // Create Gap Objects and store pointers to them
  f_pMutGaps = vector<MutGap*>(f_pStation->getNumberOfGaps());
  for (int j=0; j<getNumberOfGaps(); j++) 
  f_pMutGaps[j] = new MutGap(f_pArm,f_pStation,f_pOctant,this,GapNumber(j));

}

//____________________________________________
MutHalfOctant::~MutHalfOctant()
{
  
  for( unsigned int i=0; i<f_pMutGaps.size(); i++) 
  delete f_pMutGaps[i];
  f_pMutGaps.clear();

}

//____________________________________________
void MutHalfOctant::RefreshGap(const GapNumber& GapNum)
{

  if (f_pMutGaps[GapNum]) {
    // The Gap object exists - delete it and create a new one.
    delete f_pMutGaps[GapNum];
    f_pMutGaps[GapNum] = 
    new MutGap(f_pArm,f_pStation,f_pOctant,this,GapNum);
  }
  else {
    cout << "Cannot refresh non-existant Gap." << "\n";
  }
}

//____________________________________________
void MutHalfOctant::translate(const PHPoint &translation)
{
  //Translate position of half-octant and its gaps.

  fGlobalPosition = fGlobalPosition + translation;
  for (int i=0; i<getNumberOfGaps(); i++) {
    if(f_pMutGaps[i]) f_pMutGaps[i]->translate(translation);
  }
}


//____________________________________________
void MutHalfOctant::rotate(float angle, char axisLabel)
{
  rotateThis(angle, axisLabel);
  for (int i=0; i<getNumberOfGaps(); i++) 
    if(f_pMutGaps[i]) f_pMutGaps[i]->rotate(angle, axisLabel);
}


//____________________________________________
void MutHalfOctant::XYExpansion(float expansionPercentage)
{
  /*  
      We want to be able to adjust for thermal expansions in the plane 
      of the strip/wires.  For this, adjusting as a percentage of 
      current size seems reasonable.  This percentage is passed as a 
      parameter.
      The adjustment will be determined for each strip relative to the 
      centerline of the octant.  Since the strips for station 1 aren't 
      uniformly spaced, the adjustment must be determined for each strip
      based on its original distance from the strip closest to the 
      centerline.
  */

  for (int i=0; i<getNumberOfGaps(); i++) 
    if(f_pMutGaps[i]) f_pMutGaps[i]->XYExpansion(expansionPercentage);
}


//____________________________________________
PHFrame MutHalfOctant::getBodyCenteredFrame()
{
  /* Return a coordinate system centered on the chamber with y axis 
     defined as the centerline of the chamber, x axis as lying in the 
     plane of the chamber, and z axis pointing normal to the planes, 
     fGlobalVector, as usual.
     Use Gap2 anode to define the center of the chamber.  This will be 
     imperfect for Station 3, but close enough.
     Use the straight strips to define the y-axis.
  */

  PlaneNumber pla = Cathode2;         //plane containing straight strips
  if(getStation()==Station2) pla = Cathode1;

  int centerStrip = f_pMutGaps[Gap2]->f_pMutPlanes[pla]->getNumElements()/2;
  int centerWire = f_pMutGaps[Gap2]->f_pMutPlanes[Wire]->getNumElements()/2;
  PHLine centerStripLine(f_pMutGaps[Gap2]->f_pMutPlanes[pla]->
       f_pMutStrips[centerStrip]->getGlobalPositionBegin(),
       f_pMutGaps[Gap2]->f_pMutPlanes[pla]->
       f_pMutStrips[centerStrip]->getGlobalPositionEnd());
  PHLine centerWireLine(f_pMutGaps[Gap2]->f_pMutPlanes[Wire]->
       f_pMutWires[centerWire]->getGlobalPositionBegin(),
       f_pMutGaps[Gap2]->f_pMutPlanes[Wire]->
       f_pMutWires[centerWire]->getGlobalPositionEnd());

  PHVector yAxis = f_pMutGaps[Gap2]->f_pMutPlanes[pla]->
       f_pMutStrips[centerStrip]->getGlobalPositionEnd() -
       f_pMutGaps[Gap2]->f_pMutPlanes[pla]->
       f_pMutStrips[centerStrip]->getGlobalPositionBegin();
  PHVector xAxis = yAxis.cross(fGlobalVector);
  PHVector zAxis = xAxis.cross(yAxis);


  PHPoint frameOrigin = closestApproachLineLine(centerStripLine,centerWireLine);
  PHFrame frame(frameOrigin,xAxis,yAxis,zAxis);

  return frame;
}

//____________________________________________
void MutHalfOctant::transformToNewFrame(PHFrame oldFrame, PHFrame newFrame)
{
  fGlobalPosition = rotateAndTranslate(oldFrame, fGlobalPosition, newFrame);
  fGlobalVector = rotateAndTranslate(oldFrame, fGlobalVector, newFrame);
  for (int j=0; j<getNumberOfGaps(); j++) {
    if(f_pMutGaps[j]) 
    f_pMutGaps[j]->transformToNewFrame(oldFrame,newFrame);
  }
}
