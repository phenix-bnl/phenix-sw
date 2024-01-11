// --------------------------------------------------------
// PHENIX Drift Chamber Software: Stony Brook Tue Jul  7 19:10:51 1998 
// 
// Source file of the Class: DchSimPar 
//
// Created by: Axel Drees
// Upgraded by: Federica Ceretto
 
#include <cmath>
#include <iostream>
#include "DchSimPar.h" 

DchSimPar::DchSimPar(TABLE_HEAD_ST *header, DDCHFASTSIMPAR_ST *data) 
{
   randomSeed = data[0].randseed;
   wireEfficiency = data[0].wire_eff;
   backWireInefficiency = data[0].back_eff;
   driftRegionResolution = data[0].rphires;
   propRegionResolution  = data[0].rphiprop;
   mergeTrackFlag = True;
   correctToF  = True;
   aveDriftVelocity = 0.005;  // cm/msec
   hitWidthInDriftRegionX1 = 40.*aveDriftVelocity*0.822;
   hitWidthInDriftRegionX2 = 38.*aveDriftVelocity*0.822;
   hitWidthInDriftRegionUV = 38.5*aveDriftVelocity*0.822;
   hitWidthInPropRegion  = 61.3*aveDriftVelocity*0.822;
   hitSigmaInDriftRegionX1 = 13*aveDriftVelocity;
   hitSigmaInDriftRegionX2 = 13*aveDriftVelocity;
   hitSigmaInDriftRegionUV = 13.2*aveDriftVelocity;
   hitSigmaInPropRegion  = 17.8*aveDriftVelocity;
   widthCut = 16.*aveDriftVelocity*0.822 ;        // in cm
   t0 = 10;
}

DchSimPar::~DchSimPar() 
{
}
float DchSimPar::getDriftResolution(float driftDistance )
{

  float driftResolution;
  if(driftDistance<0.2694) driftResolution = 199.2 - 300.5 * driftDistance;
  else driftResolution = sqrt( (2*4333*driftDistance)+109*109);
  driftResolution = driftResolution * 0.0001;
  return driftResolution;
}

float DchSimPar::getWidthOfHit(int plane, float driftDistance, float startOfDriftRegion)
{
   float  widthOfHit;
   float  driftRegionWidth;

   if (plane < 12 )  driftRegionWidth = hitWidthInDriftRegionX1;
   else if(19 < plane && plane < 32 )  driftRegionWidth = hitWidthInDriftRegionX2;
   else driftRegionWidth = hitWidthInDriftRegionUV;

   if(fabs(driftDistance) > startOfDriftRegion) {
       widthOfHit = driftRegionWidth;
       return  widthOfHit;
   }else{
       widthOfHit = hitWidthInPropRegion + ((driftRegionWidth - hitWidthInPropRegion)/
	            startOfDriftRegion) *fabs(driftDistance);
      return widthOfHit;
   }
}

float 
DchSimPar::getSigmaWidthOfHit(int plane, float driftDistance, float startOfDriftRegion)
{
   float sigmaWidthOfHit;
   float  driftRegionSigma;

   if (plane < 12 )  driftRegionSigma = hitSigmaInDriftRegionX1;
   else if(19 < plane && plane < 32 )  driftRegionSigma = hitSigmaInDriftRegionX2;
   else driftRegionSigma = hitSigmaInDriftRegionUV;
   
   if (fabs(driftDistance) > startOfDriftRegion) {
       sigmaWidthOfHit = driftRegionSigma;
       return sigmaWidthOfHit;
   }else {
     sigmaWidthOfHit = driftRegionSigma;
       return sigmaWidthOfHit;
   }
       
}

std::ostream& 
operator<<(std::ostream& os, DchSimPar& a)
{
    return (os << "----------------" << "  random Seed  : " << a.getRandomSeed()
	    << "  wire Efficiency: " << a.getWireEfficiency()); 
}

