#ifndef __DCHHITASSOCIATOR_HH__
#define __DCHHITASSOCIATOR_HH__

#include <cmath>
#include "dDchHit.h"
#include "DchAnaPar.h"
#include "PHPointerList.h"
#include "DchHitLineLists.hh"
#include "DchPc1HitLists.hh"
#include "DchTrackCandidate.hh"
#include "PHDchGeometryObject.h"
#include "DchHitLineTable.hh"

#include "table_header.h"

class DchHitAssociator{ 
  
public:  
  DchHitAssociator(PHDchGeometryObject*);
  DchHitAssociator(PHDchGeometryObject*, DchHitLineLists*, PHPointerList<DchTrackCandidate>*);
  virtual ~DchHitAssociator();
 
  void associateXHits(int, int share = 0);  // int = 0 -> X1&X2; 1 -> X1; 2 -> X2
  void associateUVHits();
  void associatePc1Hits();
  void associateUVHitsWithUVPlanes();
  void associateX1AndX2HitListForCandidates(int opt);

  void clearAssociation();
  void markAssociatedHitsAsUsed();
  void markAssociatedHitsAsUnused();
  void moveHitsToCandidates(TABLE_HEAD_ST *dDchHit_h, DDCHHIT_ST *dDchHit, DchHitLineLists* hitLines, DchHitLineTable* hitLineTable);

  void attachCandidateList(PHPointerList<DchTrackCandidate>* candiList) {candidates = candiList;}
  void attachHitLineLists(DchHitLineLists* list) { hitList = list;}
  void attachPc1HitLists(DchPc1HitLists* list) { pc1List = list;}
 
  short getCellDifferenceCut(){return pCellDifferenceCut;}
  float getMaxXDistance(){return pMaxXDistance;}
  float getReferenceRadius(){return pReferenceRadius;}
  float getMaxUVDistance(){return pMaxUVDistance;}
  float getMaxPc1Distance(){return pMaxPc1Distance;}
  float getZMax(){return pZMax;}
  float getZMin(){return pZMin;}
  float getZAvg(){return pZAvg;}
 
  void  setCellDifferenceCut(short v){pCellDifferenceCut = v;}
  void  setReferenceRadius(float v){pReferenceRadius = v;}
  void  setMaxXDistance(float v){pMaxXDistance = v;}
  void  setMaxUVDistance(float v){pMaxUVDistance = v;}
  void  setMaxPc1Distance(float v){pMaxPc1Distance = v;}
  void  setZMax(float v){pZMax = v;}
  void setZMin(float v){pZMin = v;}
  void setZAvg(float v){pZAvg = v;}

  int  getVerbose() {return verbose;}
  void setVerbose(int v) {verbose = v;}

  void setVertex(PHPoint v) {vertex = v;}
  PHPoint getVertex() {return vertex;}

private: 
  PHDchGeometryObject *geometry;
  DchHitLineLists* hitList;
  DchPc1HitLists* pc1List;
  PHPointerList<DchTrackCandidate> *candidates;

  int verbose;
  // parameters to calculate alpha and phi
  short pCellDifferenceCut;     
  float pReferenceRadius;

  // cut parameters
  float pMaxXDistance;
  float pMaxUVDistance;
  float pMaxPc1Distance;

  // parameters to limit fill region of UV feature space
  float pZMin;  //cm
  float pZAvg;
  float pZMax;

  PHPoint vertex;
};

#endif /* __DCHHITASSOCIATOR_HH__ */
