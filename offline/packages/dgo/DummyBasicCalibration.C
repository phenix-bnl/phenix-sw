//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: DummyBasicCalibration.h                       
//                                                                
// Created by: Federica Ceretto                               
//                                                                              
//                                                                
// Last update:                                      
//                                                                
//----------------------------------------------------------------

#include "iostream.h"
#include "DummyBasicCalibration.h"

DummyBasicCalibration::DummyBasicCalibration()
{
  gain    = 1;          // gain
  pedestal = 0;
}   


DummyBasicCalibration::~DummyBasicCalibration()
{  
}

DummyBasicCalibration::DummyBasicCalibration(const DummyBasicCalibration &rhs)
{
  gain   = rhs.gain;
  pedestal = rhs.pedestal;

}

DummyBasicCalibration& DummyBasicCalibration::operator = (const DummyBasicCalibration &rhs)
{
  gain  = rhs.gain;
  pedestal = rhs.pedestal;
  return *this;
}


float DummyBasicCalibration::calculateAmplitude(float amp)
{
   return (amp-pedestal)*(gain);
}
 
void DummyBasicCalibration::print()
{
   cout << " Gain : " << gain << endl;
   cout << " Pedestal: " << pedestal << endl;
}


