/********************************************************/
/*                                                      */
/* base class for mNewDchMcAnalyzer & mNewDchCosAnalyer */
/*                                                      */
/********************************************************/

#ifndef __MNEWDCHANALYZER_H__
#define __MNEWDCHANALYZER_H__

#include "phool.h"
#include "PHCompositeNode.h"
#include "PHPointerList.h"

#include "DchCandidate.hh"
#include "DchTrackInfo.hh"

#include "DchRawTable.hh"
#include "DchHitLineTable.hh"
#include "DchTrack.h"

class PHDchGeometryObject; //DGO
class PHDchAddressObject;  //DAO
class PHDchNoiseObject;    //DNO
class PHDchCalibrationObject; //DCO
class PHDchHistogrammer;

class mNewDchAnalyzer
{
public:
  mNewDchAnalyzer();
 virtual ~mNewDchAnalyzer();
  PHBoolean event(PHCompositeNode* topNode);

  const PHDchAddressObject* getDAO() const {return pDchAddress;}
  const PHDchGeometryObject* getDGO() const {return pDchGeometry;}
  const PHDchNoiseObject* getDNO() const {return pDchNoise;}
  const PHDchCalibrationObject* getDCO() const {return pDchCalibration;}
  const PHDchHistogrammer* getHistogrammer() const { return pDchHistogrammer;}

  void setHistogrammer(PHDchHistogrammer* histo) {pDchHistogrammer = histo;}
  
  DchTrackInfo* getTrackBest(size_t i) {return (*trackInfoListBest)[i];}
  DchTrackInfo* getTrack(size_t i) { return (*trackTableList)[i];}
  DchHitInfo*   getHit(size_t i)   { return (*hitTableList)[i];}
  DchRawInfo*   getRaw(size_t i)   { return (*rawTableList)[i];}
  
  size_t numberOfBestTracks() {return trackInfoListBest->length();}
  size_t numberOfTracks() {return trackTableList->length();}
  size_t numberOfRaws() { return rawTableList->length();}
  size_t numberOfHits() { return hitTableList->length();}

  size_t numberOfCandidates() { return candidateList->length();}
  DchCandidate* getCandidate(size_t i) { return (*candidateList)[i];} 
  
  void  setResidualWin(float win){ resWin = win;} 
  void  setDriftWin(float win){ driftWin = win;} 
  float getResidualWin(){ return resWin;}
  float getDriftWin(){ return driftWin;}

protected:

  PHBoolean callPAM(PHPointerList<PHNode>& pList);
 
  int isCrossingWire(int i,int a, int c, int&p);
  PHLine  getTrackLine(int trackCounter);
  PHPlane getDriftPlane(PHPoint punchPoint,int arm,int plane,int cell,PHLine wire);
  void    getBestResidual(int arm,int side,int plane,int cell,
			  float twDist,float& bestResidual, int& bestHitCntr); 
  void    getResidual(int arm,int side,int plane,int cell, int trackID,
		      float twDist,float& bestResidual, int& bestHitCntr);

  float   getTimeResidual(int p1,int t1,int p2, int t2, int p3,int t3);
  PHBoolean copyTrackInfo(DchTrackInfo*, int);
  PHBoolean copyHitInfo(DchHitInfo*, int);
  PHBoolean copyRawInfo(DchRawInfo*,int);
  void fillRawTableList();
  void fillHitTableList();
  void fillTrackTableList();
  void completeHitTableList();
  void completeTrackCandidateList();
  
  float discontinuity(int trk, int arm, int cell, int pc, PHBoolean cc, PHBoolean cw,int signOfCrossingCell);
  int   countX1(DchTrackInfo* );
  int   countX2(DchTrackInfo* );
  float fitX1WCLine(DchTrackInfo*, int);
  float fitX2WCLine(DchTrackInfo*, int);
  float fitXCCLine(DchTrackInfo*, int, int);

public:
  
  void calculateEasyEfficiencyDistribution();
  void calculateEfficiencyDistribution();
  void calculateTimeResolutionOnTracks();
  void calculateHitAssociatedResiduals();
  void calculateHitAssociatedResidualsOn2DPlane();
  void calculateResiduals();
  void calculateEfficiency();
  void calculateBestTrackEfficiency();
  void calculateTrackResolution();
   
  PHBoolean fillTrackInfo();
  PHBoolean fillHitInfo();
  PHBoolean fillTrackInfoBest();
  
private:
  PHCompositeNode*        pTopNode;
  
  PHDchAddressObject*     pDchAddress;
  PHDchGeometryObject*    pDchGeometry;
  PHDchNoiseObject*       pDchNoise;
  PHDchCalibrationObject* pDchCalibration;
  PHDchHistogrammer*      pDchHistogrammer;

  DchRawTable     *rawTable;
  DchHitLineTable *hitLineTable;
  DchTrack   *trackTable;

  //-------------------
  PHPointerList<DchTrackInfo>* trackInfoListBest;

  PHPointerList<DchRawInfo>*   rawTableList;
  PHPointerList<DchHitInfo>*   hitTableList;
  PHPointerList<DchTrackInfo>* trackTableList;

  PHPointerList<DchCandidate>* candidateList;

  //--------------------
  float resWin;
  float driftWin;

};
  
#endif /*__MNEWDCHANALYZER_H__*/
