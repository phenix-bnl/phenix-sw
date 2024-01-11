#ifndef DUMMYBASICCALIBRATION_H
#define DUMMYBASICCALIBRATION_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: DummyBasicCalibration.h                       
//                                                                
// Created by: Federica Ceretto at Wed Feb 17 15:19:41 1999                                      
//                                                                
// Description:
//
// Purpose: The calibration Object for the Drift Chamber 
//
// Last update: Wed Feb 17 15:19:41 1999                                              
//                                                                
//----------------------------------------------------------------
 
class DummyBasicCalibration {
  
public:
  
  DummyBasicCalibration();
  virtual ~DummyBasicCalibration(); 

  DummyBasicCalibration(const DummyBasicCalibration &); 
  DummyBasicCalibration& operator = (const DummyBasicCalibration &); 

  float getGain() { return gain;}
  void setGain(float value)       { gain = value;}

  float getPedestal() { return pedestal;}
  void  setPedestal(float value)       { pedestal = value;}

  float calculateAmplitude(float amp);

  void  print();
  
private:
  
  float gain;
  float pedestal;
}; 

#endif /* DUMMYBASICCALIBRATIONON_H */ 
