#ifndef __MNEWDCHDRIFTVELOCITYCALIBRATOR_H__
#define __MNEWDCHDRIFTVELOCITYCALIBRATOR_H__

#include "phool.h"
#include "PHCompositeNode.h"
#include "PHPointerList.h"
#include "DchAnaPar.h"
#include "PHTimeStamp.h"

#include "table_header.h"
#include "DchRawTable.hh"
#include "DchHitLineTable.hh"

#include "DchTrackInfo.hh"
#include "PdbBankID.hh"

class TF1;
class PHDchHistogrammer;
class PHDchCalibrationObject;
class PHDchAddressObject;
class PHDchGeometryObject;
class PHDchNoiseObject;

class mNewDchDriftVelocityCalibrator
{
public:
  mNewDchDriftVelocityCalibrator();
  virtual ~mNewDchDriftVelocityCalibrator();
  PHBoolean event(PHCompositeNode *);
  
public:  
  const PHDchAddressObject*     getDAO() const { return dchAddressObject;}
  const PHDchGeometryObject*    getDGO() const { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const { return dchCalibrationObject;}
  const PHDchNoiseObject*       getDNO() const { return dchNoiseObject;}

  PHBoolean fillHistogramms();
  void setWidthOfProportionalRegion(float val) { propWidth = val;}
  PHBoolean integrate();
  PHBoolean fitTimeDistanceRelation(int minTime=160,int maxTime=550,double T0 = 0,double Vd=0.004);
  PHBoolean writeCalibrationInDatabase(const char* name, int runStart = 0, int runStop=0);
  PHBoolean calculateCalibrationFromEdges(short arm, int minl, int maxl, int mint, int maxt, short mod = 0);
  void      zeroArray();
  float     getGuessT0(int arm) { return guessT0[arm];}
  float     getFittetT0(int arm)    { return fittedT0[arm];}
  float     getFittedDrift(int arm) { return fittedDrift[arm];}
  float     getMeanTimeBBC() {return meanTimeBBC;}
  float     getMeanTimeZDC() {return meanTimeZDC;}
  int       getZDCEntries() {return zdcEntries;}
  int       getBBCEntries() {return bbcEntries;}
  void      setVerbose(int val) {verbose = val;}
  int       getWireType(int pl);
  int       getCalibrationStage() {return calibrationStage;}
  void      setCalibrationStage(int val) {calibrationStage = val;}
  float     getBbcTimeZeroOffset();
  // for separate calibration of different wireTypes :
  // 0 all wire types
  // 1-6 (X1,U1,V1,X2,U2,V2)
  void      printCalibration(short arm, short mod);
  void      initializeStartValues();
  void      writeFile();
  int       getEventRunNumber() {return EventRunNumber;}

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
  PHBoolean timeDistributionOnHits();
  PHBoolean timeDistributionOnTracks();
  PHBoolean generateDistanceDistribution();
  PHBoolean getBBCandZDCInfo();

private:
  int calibrationStage;
  int verbose;
  int EventRunNumber;

  PHCompositeNode*        topNode;
  PHDchGeometryObject*    dchGeometryObject;
  PHDchAddressObject*     dchAddressObject;
  PHDchCalibrationObject* dchCalibrationObject;
  PHDchNoiseObject*       dchNoiseObject;
  PHDchHistogrammer*      dchHistogrammer;
  DchRawTable*            rawTable;
  DchHitLineTable*        hitLineTable;

  TF1 *trailingRaw,      *trailingTrack;
  TF1 *leadingRaw,       *leadingTrack;

  float propWidth;  // width of the proprotional region for Ref Distribution
  PHPointerList<DchTrackInfo>* trackList;

  float intTimeBBC;
  float intTimeZDC;
  float meanTimeBBC;
  float meanTimeZDC;
  int   zdcEntries;
  int   bbcEntries;
  
  double fittedDrift[2];
  double fittedT0[2];
  float  binSize;
 
  float timeArrayTrack0[numberOfTimeBins];  
  float timeArrayTrackScaled0[numberOfTimeBins];
  float timeArrayTrack1[numberOfTimeBins];  
  float timeArrayTrackScaled1[numberOfTimeBins];

  /*
  float timeArray0[numberOfTimeBins];  
  float timeArrayScaled0[numberOfTimeBins];  
  float timeArray1[numberOfTimeBins];  
  float timeArrayScaled1[numberOfTimeBins];  

  */

  float distArrayScaled[numberOfTimeBins];

  float integralTimeScaled0[numberOfTimeBins];
  float integralTimeTrackScaled0[numberOfTimeBins];
  float integralTimeScaled1[numberOfTimeBins];
  float integralTimeTrackScaled1[numberOfTimeBins];
  float integralDistanceScaled[numberOfTimeBins];

  float timeDistance1[numberOfTimeBins];  // time is in Bins
  float timeDistanceTrack1[numberOfTimeBins]; // time is in Bins
  float timeDistance0[numberOfTimeBins];  // time is in Bins
  float timeDistanceTrack0[numberOfTimeBins]; // time is in Bins

  // module calibration
  float timeArray0[7][numberOfTimeBins];
  float timeArrayScaled0[7][numberOfTimeBins];
  float timeArray1[7][numberOfTimeBins];
  float timeArrayScaled1[7][numberOfTimeBins];

  float startT0[7][2];
  float startVD[7][2];
  float finalT0[7][2];
  float finalVD[7][2];

  float slopel[7][2];
  float noisel[7][2];
  float ampl[7][2];
  float halfampl[7][2];

  float slopet[7][2];
  float noiset[7][2];
  float ampt[7][2];
  float halfampt[7][2];

  float guessT0[2];
  float guessVD[2];

};
#endif /*__MNEWDCHDRIFTCALIBRATOR_H__*/














