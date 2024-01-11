#ifndef __MNEWDCHPERFECTTRACKER_H__
#define __MNEWDCHPERFECTTRACKER_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include "DchTrackCandidate.hh"
#include "dcghitWrapper.h"
#include "PHPoint.h"

#include <deque>

class PHCompositeNode;
class PHDchGeometryObject;

class mNewDchPerfectTracker
{
public:
  mNewDchPerfectTracker();
  virtual ~mNewDchPerfectTracker();
  PHBoolean event(PHCompositeNode *);
  void CopyWrapper(PHCompositeNode *topNode);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
  PHBoolean fillTrackCandidateInfo(DchTrackCandidate*,int,DCGHIT_ST *);
  void fillDchTracksExtTable(PHCompositeNode *root);

private:
 PHDchGeometryObject* dchGeometryObject;
 PHPointerList<DchTrackCandidate>* perfectCandidates;
 
};

struct IDPoint
{
  int id;
  PHPoint point;
};

struct SimpleTrack
{
  std::deque<IDPoint> HitList;
  int TrackLocationInArray;
};

#endif /*__MNEWDCHPERFECTTRACKER_H__*/





