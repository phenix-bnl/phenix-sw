/********************************************************/
/*                                                      */
/* base class for mNewDchMcAnalyzer & mNewDchCosAnalyer */
/*                                                      */
/********************************************************/

#ifndef __MNEWDCHNOISEANALYZER_H__
#define __MNEWDCHNOISEANALYZER_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

#include "DchRawTable.hh"

class PHDchGeometryObject; //DGO
class PHDchAddressObject;  //DAO
class PHDchNoiseObject;    //DNO
class PHDchCalibrationObject; //DCO
class PHDchHistogrammer; 
class PHCompositeNode;

class mNewDchNoiseAnalyzer
{
public:
  mNewDchNoiseAnalyzer(int flagBBC =1, float noiseThreshold = 1.); // 
  virtual ~mNewDchNoiseAnalyzer(){}
  PHBoolean event(PHCompositeNode* topNode);

  const PHDchAddressObject* getDAO() const {return pDchAddress;}
  const PHDchGeometryObject* getDGO() const {return pDchGeometry;}
  const PHDchNoiseObject* getDNO() const {return pDchNoise;}
  const PHDchCalibrationObject* getDCO() const {return pDchCalibration;}

protected:

  PHBoolean fillNoiseField();
  PHBoolean callPAM(PHPointerList<PHNode>& pList);

public:

  void setHistogrammer(PHDchHistogrammer* val) { pDchHistogrammer = val;}
  PHDchHistogrammer* getHistogrammer() { return pDchHistogrammer ;}
  
  int  getEventCounter() { return eventCounter;}
  int  getNoiseEdge0At(int arm, int side, int plane,int cell)
     { return noiseFieldEdge0[arm][side][plane][cell];}
  int  getNoiseEdge1At(int arm, int side, int plane,int cell)
     { return noiseFieldEdge1[arm][side][plane][cell];}

  PHBoolean analyzeReferenceChannel();
  PHBoolean analyzeTimeZeroBBC();
  PHBoolean fillNoiseNtuple();
  PHBoolean writeFile(float threshold = 1);
  int getReferenceTime() { return referenceTime;}
  int getCalibrationStage() { return calibrationStage;}
  void setCalibrationStage(int val) {calibrationStage = val;}
  PHBoolean writeCalibrationInDatabase(const char* name, int runStart = 0, int runStop=0, float threshold = 1);

private:
  
  PHCompositeNode*        pTopNode;
  PHDchAddressObject*     pDchAddress;
  PHDchGeometryObject*    pDchGeometry;
  PHDchNoiseObject*       pDchNoise;
  PHDchCalibrationObject* pDchCalibration;
  PHDchHistogrammer*      pDchHistogrammer;
  DchRawTable*            rawTable;

  short flagMC, flagBBC;
  int   referenceTime;
  float timeZeroBBC;
  int calibrationStage;
  int EventRunNumber;

  PHBoolean foundReferenceTime;
  int eventCounter;
  float noiseThreshold;
  int noiseFieldEdge0[2][2][40][80];
  int noiseFieldEdge1[2][2][40][80];

};
  
#endif /*__MNEWDCHNOISEANALYZER_H__*/
