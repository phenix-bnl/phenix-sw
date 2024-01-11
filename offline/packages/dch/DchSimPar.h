// Purpose: Class corresponding to dc fastsim parameter table
//--------------------------------------------------- 
// PHENIX Drift Chamber Software: Stony Brook Tue Jul  7 19:10:51 1998    
// 
// Implementation of the Class: DchSimPar 
// 
// Created by: Axel Drees
// Upgraded by :Federica Ceretto  at Tue Jul  7 19:10:51 1998
// 
// Description: A class corresponding to the drift chamber fast
// simulator parameter table.

#ifndef __DCHSIMPAR_H
#define __DCHSIMPAR_H

#include <iostream>
#include "phool.h"
#include "dDchFastSimPar.h"
#include "table_header.h"

class DchSimPar {
public: 
  DchSimPar(TABLE_HEAD_ST *header,DDCHFASTSIMPAR_ST *data);  
  virtual ~DchSimPar(); 

public:
    float  getWidthOfHit(int, float, float);
    float  getWidthOfHit(float, float) { std::cout << "DchSimPar: WARNING, old code used, width set to 10000"<< std::endl; return 10000;}
    float  getSigmaWidthOfHit(float, float) { std::cout << "DchSimPar: WARNING, old code used, sigma set to 10000"<< std::endl; return 10000;}
    float  getSigmaWidthOfHit(int, float, float);
  friend std::ostream&   operator<<(std::ostream&, const DchSimPar);

    int   getRandomSeed()             { return randomSeed;}
    float getWidthCut()               { return widthCut;}
    float getWireEfficiency()         { return wireEfficiency;}
    float getBackWireInefficiency()   { return backWireInefficiency;}
    float getDriftRegionResolution()  { return driftRegionResolution;
    std::cout << "warning using DriftRegionResolution: old code!!!" << std::endl;}
    float getPropRegionResolution()   { return propRegionResolution;
    std::cout << "warning using PropRegionResolution: old code!!!" << std::endl;}
    float getDriftResolution(float driftDistance);
    float getAveDriftVelocity()       { return aveDriftVelocity;}
    float getT0()                     { return t0;}
    float getHitWidthInPropRegion()   { return hitWidthInPropRegion;}
    float getHitSigmaInPropRegion()   { return hitSigmaInPropRegion;}
    PHBoolean getCorrectToF()         { return correctToF;}
    PHBoolean getMergeTrackFlag()         { return mergeTrackFlag;}

    void setRandomSeed(int val)               { randomSeed = val;}
    void setWidthCut(float val)               { widthCut = val;}
    void setWireEfficiency(float val)         { wireEfficiency = val;}
    void setBackWireInefficiency(float val)   { backWireInefficiency = val;}
    void setAveDriftVelocity(float val)       { aveDriftVelocity = val;}
    void setT0(float val)                     { t0 = val;}
    void setHitSigmaInPropRegion(float val)   { hitSigmaInPropRegion = val*aveDriftVelocity;}
    
private:
    
    int randomSeed;
    float widthCut;
    float wireEfficiency;
    float backWireInefficiency;
    float driftRegionResolution;
    float propRegionResolution;
    float aveDriftVelocity;
    float t0;
    float hitWidthInDriftRegionX1;        // cm, width of hit in drift region
    float hitWidthInDriftRegionX2;        // cm, width of hit in drift region
    float hitWidthInDriftRegionUV;        // cm, width of hit in drift region
    float hitWidthInPropRegion;         // same for proportional region
    float hitSigmaInDriftRegionX1;      // sigma of width 
    float hitSigmaInDriftRegionX2;      // sigma of width 
    float hitSigmaInDriftRegionUV;      // sigma of width 
    float hitSigmaInPropRegion;
    PHBoolean mergeTrackFlag;
    PHBoolean correctToF; // extended name was tofc

}; 

#endif /* __DCHSIMPAR_H */ 
