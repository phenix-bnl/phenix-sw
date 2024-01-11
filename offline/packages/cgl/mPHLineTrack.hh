#ifndef __MPHLINETRACK_HH__
#define __MPHLINETRACK_HH__

#include <phool.h>
#include <PHPointerList.h>

class cglDetectorGeo;
class dDchTracksWrapper;
class PHCompositeNode;
class PHLineTrack;

class mPHLineTrack 
{
public:
  mPHLineTrack();
  virtual ~mPHLineTrack() {}
  PHBoolean event(PHCompositeNode *);

protected:
  void callLineTrack(PHCompositeNode *topNode);

private:
  cglDetectorGeo *PHcglDetGeo;
  PHPointerList<PHLineTrack> *tracks;
  dDchTracksWrapper *trkWrapper;
};

#endif /* __MPHLINETRACK_HH__ */
