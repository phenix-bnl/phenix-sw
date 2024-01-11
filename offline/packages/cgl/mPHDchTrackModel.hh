#ifndef __MPHDCHTRACKMODEL_H__
#define __MPHDCHTRACKMODEL_H__

#include "phool.h"
#include "PHPointerList.h"
#include "PHPoint.h"
#include "PHLine.h"

class cglDetectorGeo;
class dDchTracksWrapper;
class dPHDchTrackWrapper;
class dDchHitWrapper;
class VtxOut;
class PHCompositeNode;
class PHDchTrack;

class mPHDchTrackModel 
{
public:

  mPHDchTrackModel();
  virtual ~mPHDchTrackModel();
  PHBoolean event(PHCompositeNode *);

public:
  void echoVerboseLevels();
  void flattenTheProjections();
  void turnOffRobust() { robust = 0;}
  void setMaxIterations(int &i) { iterations = i;}
  int getIterations() { return successfulIterations;} 
  void setVerbose(short &level)      { verbose = level; }
  short getVerbose()                 {return verbose;} 
  void setTrackQualityThresh(int &quality) { qualityThresh = quality;}
  int getTrackQualityThresh() { return qualityThresh;}

  void check(PHCompositeNode *);

  int ResetEvent(PHCompositeNode *topNode);

protected:
  PHBoolean getVertexFromVtxOutClass(PHCompositeNode *dstNode);
  PHPoint getVertex() const {return vertex;}
  void print(long i);
  void fitTracks();
  void getAllHits();
  PHBoolean getX1andX2Hits(long &trkIndex);

private:
  cglDetectorGeo *PHcglDetGeo;

  VtxOut* vtxout;

  PHPoint vertex,vertexErr;

  PHPointerList<PHLine> *hits;
  PHPointerList<PHDchTrack> *tracks;

  dDchHitWrapper* hitWrapper;
  dDchTracksWrapper* trkWrapper;
  dPHDchTrackWrapper* projWrapper;

  int verbose;
  int iterations;
  int successfulIterations;
  int robust;
  int qualityThresh;

  // for debugging purposes
  int failedCount;

};

#endif /*__MPHDCHTRACKMODEL_H__*/
