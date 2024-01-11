#ifndef __DCHLINETRACKER_H
#define __DCHLINETRACKER_H

#include "DchHitLineLists.hh"
#include "DchTrackCandidate.hh"
#include <PHPointerList.h>

class PHDchGeometryObject;
class DchHitLineLists;
class PHPoint;
class PHVector;

class DchLineTracker
{ 
public:  
  DchLineTracker(PHDchGeometryObject*);
  DchLineTracker(PHDchGeometryObject*, DchHitLineLists*, PHPointerList<DchTrackCandidate>*);
  virtual ~DchLineTracker(){}
 
  void fitLineToXHits();
  void fitLineToAllHits();
  int intersectionWithReferenceRadius(short arm,PHPoint& globalp, PHVector& globalv,
                                               PHPoint& hit, PHPoint& localp, PHVector& localv);
  void removeDuplicateCandidates();
  
  float getTukeysWeight(){return pTukeysWeight;}
  float getDeltaAlpha(){return pDeltaAlpha;}
  float getDeltaPhi(){return pDeltaPhi;}
  int   getNumberOfItterations(){return pNumberOfItterations;}
 
  void  setTukeysWeight(float v) {pTukeysWeight = v;}
  void  setDeltaAlpha(float v) {pDeltaAlpha = v;}
  void  setDeltaPhi(float v) {pDeltaPhi = v;}
  void  setNumberOfItterations(int i){pNumberOfItterations = i;}

  int  getVerbose() {return verbose;}
  void setVerbose(int v) {verbose = v;}

private:
  PHDchGeometryObject *geometry;
  DchHitLineLists* hitList;
  PHPointerList<DchTrackCandidate> *candidates;

  int verbose;

  // cut parameters
  float pTukeysWeight;            // Tukeys constant typically ~ 3 
  int   pNumberOfItterations;     // number of itterations in robust fit
  float pDeltaAlpha;              // alpha cut to remove dublicate candidates
  float pDeltaPhi;                // phi cut correlated with alpha cut  
};

#endif /* __DCHLINETRACKER_H */
