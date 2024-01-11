//-----------------------------------------------------------------------------
//  Declaration of class TofCalibObject
//
//  Purpose: Calibration parameters organization for Time-of-Flight
//  Description: TOF Calibration Object
//
//  Author: Tatsuya Chujo (Univ. of Tsukuba)
//
//  History: 07/08/00  T.Chujo  First Version
//           08/23/00  A.Kiyomichi add Mip Peak info
//-----------------------------------------------------------------------------

#ifndef __TOFCALIBOBJECT_HH__
#define __TOFCALIBOBJECT_HH__

#include "Tof.hh"
#include "TofBasicObject.hh"

#include "PHTimeStamp.h"

class TofAddressObject;

class TofCalibObject : public TofBasicObject{
public:
  // Constructor
  TofCalibObject();
  // Destructor
  virtual ~TofCalibObject();
  
  // Get Name
  PHString getName() {return "TOF Calibration Object";} 
  
  // Get Calibration parameters
  float getTvcPede(int lu, int slatid);
  float getQvcPede(int lu, int slatid);
  float getTvcConv(int lu, int slatid);
  float getQvcConv(int lu, int slatid);
  float getSlewPar_a(int lu, int slatid);
  float getSlewPar_b(int lu, int slatid);
  float getToffset(int slatid);
  float getYoffset(int slatid);
  float getVelocity(int slatid);
  float getElossConv(int slatid);
  float getGlobalT();

  // Set Calibration parameters
  void setTvcPede(int lu, int slatid, float tvcpede);
  void setQvcPede(int lu, int slatid, float qvcpede);
  void setTvcConv(int lu, int slatid, float tvcconv);
  void setQvcConv(int lu, int slatid, float qvcconv);
  void setSlewPar_a(int lu, int slatid, float slewpar_a);
  void setSlewPar_b(int lu, int slatid, float slewpar_b);
  void setToffset(int slatid, float toffset);
  void setYoffset(int slatid, float yoffset);
  void setVelocity(int slatid, float velocity);
  void setElossConv(int slatid, float elossconv);
  void setGlobalT(float globalt);
  
  // Mip Peak info.  [NO standard parameter]
  void  setMipPeak(int lu, int slatid, float mippeak, float mipsigma);
  float getMipPeak(int lu, int slatid);
  float getMipSigma(int lu, int slatid);

  // Fetch calibration parameter from a Database 
  PHBoolean fetch();
  PHBoolean fetchTvcPede();
  PHBoolean fetchQvcPede();
  PHBoolean fetchTvcConv();
  PHBoolean fetchQvcConv();
  PHBoolean fetchSlewPar();
  PHBoolean fetchToffset();
  PHBoolean fetchYoffset();
  PHBoolean fetchVelocity();
  PHBoolean fetchElossConv();
  PHBoolean fetchGlobalT();
  PHBoolean fetchMipPeak();

  // fetch parameter from run number
  PHBoolean fetch(const int run);
  PHBoolean fetchTvcPede(const int run);
  PHBoolean fetchQvcPede(const int run);
  PHBoolean fetchTvcConv(const int run);
  PHBoolean fetchQvcConv(const int run);
  PHBoolean fetchSlewPar(const int run);
  PHBoolean fetchToffset(const int run);
  PHBoolean fetchYoffset(const int run);
  PHBoolean fetchVelocity(const int run);
  PHBoolean fetchElossConv(const int run);
  PHBoolean fetchGlobalT(const int run);
  PHBoolean fetchMipPeak(const int run);
  
  // Fetch calibration parameters from ASCII file "filename"
  PHBoolean fetchPedestalFromFile(const char *filename, TofAddressObject *address);
  PHBoolean fetchTvcConvFromFile(const char *filename, TofAddressObject *address);
  PHBoolean fetchQvcConvFromFile(const char *filename, TofAddressObject *address);
  PHBoolean fetchSlewParFromFile(const char *filename);
  PHBoolean fetchToffsetFromFile(const char *filename);
  PHBoolean fetchYoffsetFromFile(const char *filename);
  PHBoolean fetchVelocityFromFile(const char *filename);
  PHBoolean fetchElossConvFromFile(const char *filename);
  PHBoolean fetchGlobalTFromFile(const char *filename);
  
  // Update calibration parameters from current TofCalib 
  // object in memory 
  PHBoolean updateTvcPede(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateQvcPede(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateTvcConv(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateQvcConv(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateSlewPar(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateToffset(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateYoffset(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateVelocity(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateElossConv(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateGlobalT(PHTimeStamp TStart, PHTimeStamp TStop);
  PHBoolean updateMipPeak(PHTimeStamp TStart, PHTimeStamp TStop);

  // use run number
  PHBoolean updateTvcPede(const int beginrun, const int endrun=-1);
  PHBoolean updateQvcPede(const int beginrun, const int endrun=-1);
  PHBoolean updateTvcConv(const int beginrun, const int endrun=-1);
  PHBoolean updateQvcConv(const int beginrun, const int endrun=-1);
  PHBoolean updateSlewPar(const int beginrun, const int endrun=-1);
  PHBoolean updateToffset(const int beginrun, const int endrun=-1);
  PHBoolean updateYoffset(const int beginrun, const int endrun=-1);
  PHBoolean updateVelocity(const int beginrun, const int endrun=-1);
  PHBoolean updateElossConv(const int beginrun, const int endrun=-1);
  PHBoolean updateGlobalT(const int beginrun, const int endrun=-1);
  PHBoolean updateMipPeak(const int beginrun, const int endrun=-1);

  // Write to ASCII file  
  PHBoolean writePedestalToFile(const char* filename,TofAddressObject *address);
  PHBoolean writeTvcConvToFile(const char *filename,TofAddressObject *address);
  PHBoolean writeQvcConvToFile(const char *filename,TofAddressObject *address);
  PHBoolean writeSlewParToFile(const char *filename);
  PHBoolean writeToffsetToFile(const char *filename);
  PHBoolean writeYoffsetToFile(const char *filename);
  PHBoolean writeVelocityToFile(const char *filename);
  PHBoolean writeElossConvToFile(const char *filename);
  PHBoolean writeGlobalTToFile(const char *filename);
  
  // Mip Peak info.  [NO standard parameter]
  PHBoolean fetchMipPeakFromFile(const char *filename);
  PHBoolean writeMipPeakToFile(const char *filename);
  // Print
  void print(int slatid);
  
private:
  // TOF Calibration Parameters
  float  TvcPede[TOF_NSLAT][2];    // Tvc Pedestal value [ch]
  float  QvcPede[TOF_NSLAT][2];    // Qvc Pedestal value [ch]
  float  TvcConv[TOF_NSLAT][2];    // Tvc Conversion factor [ns/ch]
  float  QvcConv[TOF_NSLAT][2];    // Qvc Conversion factor [MeV/ch]
  float  SlewPar_a[TOF_NSLAT][2];  // Slewing Parameter (a) [ns]
  float  SlewPar_b[TOF_NSLAT][2];  // Slewing Parameter (b) [ns ch**0.5]
  float  Toffset[TOF_NSLAT];       // Timing offset [ns]      
  float  Yoffset[TOF_NSLAT];       // Y offset [cm]
  float  Velocity[TOF_NSLAT];      // Light velocity in scint.[cm/sec]
  float  ElossConv[TOF_NSLAT];     // Energy loss conversion factor [GeV/ch]
  float  GlobalT[1];               // Global Time offset [ns]

  // Mip Peak info.  [NO standard parameter]
  float  MipPeak[TOF_NSLAT][2];       // Mip. peak [Landau distribution mean]
  float  MipSigma[TOF_NSLAT][2];      // Mip. peak [Landau distribution sigma]

};

#endif /* __TOFCALIBOBJECT_HH__ */
