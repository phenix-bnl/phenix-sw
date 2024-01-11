#ifndef DCHBASICCALIBRATION_H
#define DCHBASICCALIBRATION_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: DchBasicCalibration.h                       
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
 
class DchBasicCalibration {
  
public:
  
  DchBasicCalibration();
  DchBasicCalibration(float t0, float vdrift); 
  DchBasicCalibration(float t0, float vdrift, float binsize); 
  virtual ~DchBasicCalibration(); 

  DchBasicCalibration(const DchBasicCalibration &); 
  DchBasicCalibration& operator = (const DchBasicCalibration &); 

  float getDriftVelocity() { return driftVelocity;}
  float getT0()            { return t0;}
  float getBinSize()       { return binSize;}

  void setDriftVelocity(float value) { driftVelocity = value;}
  void setT0(float value)            { t0      = value;}
  void setBinSize(float value)       { binSize = value;}


  float transformTimeToDistance(const long& time);
  long  transformDistanceToTime(const float& distance, const short& edge);
  void  print();
  
private:
  
  float t0;
  float driftVelocity;    
  float binSize;      // time bin size of TD conversion
    
}; 

#endif /* DCHBASICCALIBRATIONON_H */ 
