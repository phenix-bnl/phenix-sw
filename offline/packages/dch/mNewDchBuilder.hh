#ifndef __MNEWDCHBUILDER_H__
#define __MNEWDCHBUILDER_H__

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

class mNewDchBuilder 
{
public:
  mNewDchBuilder();
 virtual ~mNewDchBuilder();
  PHBoolean event(PHCompositeNode* topNode);

  DchTrackInfo* getTrackBest(size_t i) {return (*trackInfoListBest)[i];}
  DchTrackInfo* getTrack(size_t i) { return (*trackTableList)[i];}
  DchHitInfo*   getHit(size_t i)   { return (*hitTableList)[i];}
  DchRawInfo*   getRaw(size_t i)   { return (*rawTableList)[i];}
  DchCandidate* getCandidate(size_t i) { return (*candidateList)[i];} 
  
  size_t numberOfBestTracks() {return trackInfoListBest->length();}
  size_t numberOfTracks() {return trackTableList->length();}
  size_t numberOfRaws() { return rawTableList->length();}
  size_t numberOfHits() { return hitTableList->length();}
  size_t numberOfCandidates() { return candidateList->length();}
  
protected:

  PHBoolean callPAM(PHPointerList<PHNode>& pList);
  
  PHBoolean copyTrackInfo(DchTrackInfo*, int);
  PHBoolean copyHitInfo(DchHitInfo*, int);
  PHBoolean copyRawInfo(DchRawInfo*,int);
  void fillRawTableList();
  void fillHitTableList();
  void fillTrackTableList();
  void completeHitTableList();
  void completeTrackCandidateList();
    
private:
  
  DchRawTable*            rawTable;
  DchHitLineTable*        hitLineTable;
  DchTrack*          trackTable;

  PHCompositeNode*        pTopNode;
  
  PHDchAddressObject*     dchAddress;
  PHDchGeometryObject*    dchGeometry;
  PHDchNoiseObject*       dchNoise;
  PHDchCalibrationObject* dchCalibration;
  PHDchHistogrammer*      dchHistogrammer;

  PHPointerList<DchTrackInfo>* trackInfoListBest;
  PHPointerList<DchRawInfo>*   rawTableList;
  PHPointerList<DchHitInfo>*   hitTableList;
  PHPointerList<DchTrackInfo>* trackTableList;
  PHPointerList<DchCandidate>* candidateList;
 
};
  
#endif /*__MNEWDCHBUILDER_H__*/
