// Implementation of class file: DchBasicNoise.h                       
// Created by: Federica Ceretto at Mon Dec 27 15:19:41 1999

#include <iostream>
#include "DchBasicNoise.h"

using namespace std;

DchBasicNoise::DchBasicNoise()
{
  status          =   1;
  countsPerEvent  =   0;
  globalIndex     =  -1;
}   

DchBasicNoise::DchBasicNoise(int index, float ce, PHBoolean f)
{
  globalIndex = index;
  status = f;
  countsPerEvent = ce;
}
DchBasicNoise::DchBasicNoise(int index, float ce, short f)
{
  globalIndex = index;
  status = f;
  countsPerEvent = ce;
}


DchBasicNoise::~DchBasicNoise()
{  
}

void 
DchBasicNoise::print()
{
  cout << "Channel Global Index : " << globalIndex << endl;
  cout << "Counts/Events        : " << countsPerEvent << endl;
  if (status == 1)
    {
      cout << "GOOD " << endl;
    }
  else
    {
      cout << "NOISY/DEAD " << endl;
    }
}


