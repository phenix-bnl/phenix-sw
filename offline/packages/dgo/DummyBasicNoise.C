//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: DummyBasicNoise.h                       
//                                                                
// Created by: Federica Ceretto at Mon Dec 27 15:19:41 1999                                      
//                                                                              
//                                                                
// Last update: Mon Dec 27 15:19:41 1999                                              
//                                                                
//----------------------------------------------------------------

#include "iostream.h"
#include "DummyBasicNoise.h"

DummyBasicNoise::DummyBasicNoise()
{
  noisy           =  0;
  countsPerEvent  =  0;
  globalIndex     = -1;
}   

DummyBasicNoise::DummyBasicNoise(int index, float ce, PHBoolean f)
{
  globalIndex = index;
  noisy = f;
  countsPerEvent = ce;
}
DummyBasicNoise::~DummyBasicNoise()
{  
}

void DummyBasicNoise::print()
{
  cout << "Channel Global Index : " << globalIndex << endl; 
  cout << "Counts/Events        : " << countsPerEvent << endl;
  if (noisy == 0) {
    cout << "Noisy         :  NO " << endl;
  }else {
    cout << "Noisy         : YES " << endl;
  }
}


