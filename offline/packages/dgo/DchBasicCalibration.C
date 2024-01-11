// Implementation of class file: DchBasicCalibration.h                       
// Created by: Federica Ceretto at Wed Feb 17 15:19:41 1999

#include <iostream>
#include "DchBasicCalibration.h"

using namespace std;

DchBasicCalibration::DchBasicCalibration()
{
  t0             = 0;          //  time offset
  driftVelocity  = 0;          //  cm/nsec
  binSize        = 0;          //  4XRHIC clock freq = 0.038 GHz
}   

DchBasicCalibration::DchBasicCalibration(float t, float v)
{
  t0             = t;                    //  time offset
  driftVelocity  = v;                    //  cm/nsec
  binSize        = ((1.0/0.038)/32.0);   //  4XRHIC clock freq = 0.038 GHz
}

DchBasicCalibration::DchBasicCalibration(float t, float v, float b)
{
  t0             = t;                     
  driftVelocity  = v;       
  binSize        = b; 
}

DchBasicCalibration::~DchBasicCalibration()
{  
}

DchBasicCalibration::DchBasicCalibration(const DchBasicCalibration &rhs)
{
  t0            = rhs.t0;
  driftVelocity = rhs.driftVelocity;
  binSize       = rhs.binSize;
}

DchBasicCalibration& DchBasicCalibration::operator = (const DchBasicCalibration &rhs)
{
  t0            = rhs.t0;
  driftVelocity = rhs.driftVelocity;
  binSize       = rhs.binSize;
  return *this;
}

long DchBasicCalibration::transformDistanceToTime (const float&   distance, const short& edge)    
{
   long time;
   float tmpTime = (distance/driftVelocity + t0);
   time = (long) tmpTime; 
   return time;
} 


float DchBasicCalibration::transformTimeToDistance(const long&  time)
{
   float distance; 
   float tmpTime = (time+.5);
   distance = (tmpTime-t0)*driftVelocity; 
   return distance;
} 
 
void DchBasicCalibration::print()
{
   cout << " DriftVelocity (cm/bins): " << driftVelocity << endl;
   cout << " DriftVelocity (cm/ns)  : " << driftVelocity/binSize << endl;
   cout << " T0      (bins)         : " << t0            << endl;
   cout << " T0      (ns)           : " << t0*binSize    << endl;
   cout << " BinSize (nsec)         : " << binSize       << endl;
   cout << " BinSize (micron)       : " << binSize*driftVelocity*10000. << endl;
}


