#ifndef __MNEWDCHINITIALIZER_H__
#define __MNEWDCHINITIALIZER_H__

#include <phool.h>
#include <PHCompositeNode.h>
#include <PHPointerList.h>
#include <PHTimeStamp.h>
#include <DchCandidate.hh>
#include <DchTrackCandidate.hh>
#include <DchTrackInfo.hh>
#include <Event.h>

class PHDchAddressObject;
class PHDchGeometryObject;
class PHDchCalibrationObject;
class PHDchNoiseObject;
class PHDchHistogrammer;
class DchHitLineLists;
class DchPc1HitLists;

class mNewDchInitializer
{
public:
  mNewDchInitializer(short mcFlag = 1, short histoFlag = 0, short dbAccess = 0, int runnumber = -1);
  virtual ~mNewDchInitializer(){}
  PHBoolean event(PHCompositeNode *);

private:

  PHBoolean constructPhoolTree(PHCompositeNode* root);  // put in the tree all what is needed
  void  setFileNames();   // set default file names 

public:
  PHBoolean checkDetectorObjectValidity();              // check validity 
 
public:

  void setGeometryFileNames(const char* info, const char* wire, const char* frame);
  void setCalibrationFileName(const char* cal,const char* slew="DchCalibrationSlew.Real", const char* local="DchCalibrationLocal.Real", const char* stereo="DchCalibrationStereo.Real");
  void setNoiseFileName(const char* noise,const char* effi);
  
  PHBoolean updateNoise(PHTimeStamp& ,PHTimeStamp& , const char* descrip);
  PHBoolean updateCalibration(PHTimeStamp& ,PHTimeStamp& , const char* descrip, int flag = 0);
  PHBoolean updateGeometry(PHTimeStamp& ,PHTimeStamp& , const char* descrip, int flag = 0);
  PHBoolean updateAddress(PHTimeStamp& ,PHTimeStamp& , const char* descrip);
  
  const PHDchAddressObject*     getDAO() const   { return dchAddressObject;}
  PHDchGeometryObject*    getDGO() const   { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const   { return dchCalibrationObject;}
  const PHDchNoiseObject*       getDNO() const   { return dchNoiseObject;}
  PHDchHistogrammer* getHistogrammer()  const { return dchHistogrammer;}
  
  PHTimeStamp getTsearch() { return Tsearch;}
  short getFlagMC() const {return flagMC;}  
  void setFlagMC(const short val) {flagMC = val;}
  void setIniHisto(const short val) {iniHisto = val;}
  short getReadFromDB() const {return readFromDB ;}

  const char* getAddressLocation()     { return addressNameDB;}
  const char* getGeometryLocation()    { return geometryNameDB;}
  const char* getCalibrationLocation() { return calibNameDB;}
  const char* getNoiseLocation()       { return noiseNameDB;}
  
private:
  PHDchGeometryObject    *dchGeometryObject;
  PHDchAddressObject     *dchAddressObject;
  PHDchCalibrationObject *dchCalibrationObject;
  PHDchNoiseObject       *dchNoiseObject;
  PHDchHistogrammer      *dchHistogrammer;

  DchHitLineLists             *hitLineLists;
  DchPc1HitLists              *pc1HitLists;
  
  PHPointerList<DchCandidate> *candidateList;
  PHPointerList<DchTrackCandidate> *trackCandidateList;
  PHPointerList<DchTrackCandidate> *perfTrackCandidateList;
  PHPointerList<DchRawInfo>   *rawTableList;
  PHPointerList<DchTrackInfo> *trackTableList;
  PHPointerList<DchHitInfo>   *hitTableList;
  PHPointerList<DchTrackInfo> *trackInfoListBest;
  
  PHBoolean isTree;
  short iniHisto;
  short flagMC;
  short readFromDB;
  PHTimeStamp Tsearch;
  int RunNumber;

  const char* geoInfoFile;
  const char* geoFrameFile;
  const char* geoWireFile;
  const char* noiseFile;
  const char* calFile;
  const char* slewFile;
  const char* localFile;
  const char* stereoFile;
  const char* effiFile;
  
  const char* addressNameDB;
  const char* geometryNameDB;
  const char* calibNameDB;
  const char* noiseNameDB;
};
#endif /*__MNEWDCHINITIALIZER_H__*/






