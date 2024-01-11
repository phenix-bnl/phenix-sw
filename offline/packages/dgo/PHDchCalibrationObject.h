#ifndef __PHDCHCALIBRATIONOBJECT_H__
#define __PHDCHCALIBRATIONOBJECT_H__

// Implementation of class PHDchCalibrationObject.h 
//
// Created by: Federica Ceretto at Wed Feb 17 15:19:41 1999
//
// Retrieves (and writes) Calibration info from the Database for the
// Drift Chamber
//
// Modified by TKH as follows:
//    1)  The drift distance shall be a PHVector to
//        allow for mis-shaped drift corridors 
//        (e.g. stereo wires).
//    2)  Parameters of the calibration are held in
//        private data members of this object since
//        the now MANY parameters have differing 
//        dimensionality.
//    3)  Eliminate DchBasicCalibration, consistent with 
//        item #2.
//    4)  Attach drift method index to allow implementation
//        of competing drift models at differing levels of
//        complexity.
//                                    11-7-2001
//--------------------------------------------------------------

#include <PHDchAddressObject.h>
#include <PHCalibrationObject.h>
#include <PHVector.h>
#include <DchDgoPar.h>
#include <PHArray.hh>

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

enum DchCalibMethod { kDchLinearCalib, kDchInnerStereoCalib,
                      kDchOuterStereoCalib, kDchRiabovCalib };

class PHDchCalibrationObject : public PHCalibrationObject { 
  
public:
  
  PHDchCalibrationObject(PHDchAddressObject *add); 
  virtual ~PHDchCalibrationObject();

  // Methods to handle ASCII files
  PHBoolean fetchFromFile();
  PHBoolean fetchSlewFromFile();
  PHBoolean fetchLocalFromFile();
  PHBoolean fetchStereoFromFile();
  PHBoolean setFileName(const char* cal,
			const char* slew   = "DchCalibrationSlew.Real",
			const char* loc    = "DchCalibrationLocal.Real",  
			const char* stereo = "DchCalibrationStereo.Real");

  // Methods to handle DATABASE
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);
  virtual PHBoolean update(PHTimeStamp&,PHTimeStamp&,const char *,PdbBankID, const char * );
  virtual PHBoolean updateValidityTimeForLastBank(PHTimeStamp&, PHTimeStamp&, PHTimeStamp&,
						  const char *, PdbBankID, int force = 0);

  // Methods to apply forward (data) and reverse (MC) calibrations.
  PHVector transformTimeToDistance(const PdbIndex* index, const float& time, 
                                   const DchCalibMethod& m=kDchLinearCalib,
				   const float ddt=0., const float ddv=1.);
  float    transformNominalTimeToDistance(const long& time);
  long     transformDistanceToNominalTime(const float& distance, const short& edge,
					  const float ddt=0., const float ddv=1.); // MC only...uses Nominal Calib

  // Utility and Debugging...
  void  printCalibration(PdbIndex* index);
  void  printStereoCalibration();
  
  // Sets 'n gets...
  void  setCommittingFlag(int val) { committingFlag = val;}
  void  setRunNumber(int v)        { runNumber = v;}
  void  setBbcMean(float v)        { bbcMean   = v;}
  void  setZdcMean(float v)        { zdcMean   = v;}
  void  setBbcCounts(int v)        { bbcCounts = v;}
  void  setZdcCounts(int v)        { zdcCounts = v;}
  void  setBinSize(float v)        { binSize   = v;}
  void  setNominalT0(short arm, float t)            { nominalT0[arm]=t;}
  void  setNominalDriftVelocity(short arm, float v) { nominalDriftVelocity[arm]=v;}
  void  setExplicitCalibration  (PdbIndex *,float t, float v);
  void  setExplicitT0           (PdbIndex *,float t);
  void  setExplicitDriftVelocity(PdbIndex *,float v);
  void  setExplicitT0(int iarm, int iside, int iplane, int icell, float t);
  void  setExplicitDriftVelocity(int iarm, int iside, int iplane, int icell, float v);  

  int   getRunNumber() { return runNumber;}
  float getBbcMean()   { return bbcMean;}
  float getZdcMean()   { return zdcMean;}
  int   getBbcCounts() { return bbcCounts;}
  int   getZdcCounts() { return zdcCounts;}
  float getBinSize()   { return binSize;} 
  int   getSlew_npar() { return sle_npar;}
  float getNominalT0(int arm)            { return nominalT0[arm];}
  float getNominalDriftVelocity(int arm) { return nominalDriftVelocity[arm];}
  float getExplicitT0(PdbIndex *);
  float getExplicitDriftVelocity(PdbIndex *);
  float getExplicitT0(int iarm, int iside, int iplane, int icell);
  float getExplicitDriftVelocity(int iarm, int iside, int iplane, int icell);
  PH3DimArray<float> slewingArray;

protected:

  void initialize();
  // These "actual" calibration routines are hidden from the user.
  // They are called by the "switchyard" in accordance with the
  // DchCalibMethod requested by the user.
  PHVector transformTTDLinear     (const PdbIndex* index, const float& time, const float ddt, const float ddv);
  PHVector transformTTDInnerStereo(const PdbIndex* index, const float& time, const float ddt, const float ddv);
  PHVector transformTTDOuterStereo(const PdbIndex* index, const float& time, const float ddt, const float ddv);
  PHVector transformTTDRiabov     (const PdbIndex* index, const float& time, const float ddt, const float ddv);
  
private:
  PdbCalBank* slewBank;
  PdbCalBank* localBank;
  PdbCalBank* stereoBank;
  
  const char* calFile;
  const char* slewFile;
  const char* calLocalFile;
  const char* stereoFile;

  int   nChannels;
  int   channelParameters;
  float bbcTimeZeroOffset;
  int   committingFlag;
  PHDchAddressObject* dchaddress;

  //--------
  // Calibrations separated into groups by which
  // Database Bank they come from (and in order).

  //  ***calibrationBank***
    int   headerParameters,runNumber;
    float bbcMean,zdcMean;
    int   bbcCounts,zdcCounts;
    //    numberOfArms; (declared globally)
    float binSize;
    float nominalT0[numberOfArms], nominalDriftVelocity[numberOfArms];
  
  //  ***slewBank***
    int sle_head;
    int sle_ncha;
    int sle_npar;
    //  slewingArray;  (declared publicly)

  //  ***localBank***
    float tZero[numberOfArms][numberOfSides][numberOfPlanes][numberOfCells];
    float driftVelocity[numberOfArms][numberOfSides][numberOfPlanes][numberOfCells];

  //  ***stereoBank***
    float kinkLocation;
    float deltaRad[numberOfArms];
    float deltaDist[numberOfArms][numberOfPlanes][numberOfSides];
  //--------

}; 

#endif /* __PHDCHCALIBRATIONOBJECT_H__ */
