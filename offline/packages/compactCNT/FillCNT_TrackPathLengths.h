#ifndef __FILLCNT_TRACKPATHLENGTHS_H__
#define __FILLCNT_TRACKPATHLENGTHS_H__

#include <SubsysReco.h>

class PHCentralTrack;

class FillCNT_TrackPathLengths: public SubsysReco
{
 public:
  FillCNT_TrackPathLengths(const std::string &name = "FILLCNT_TRACKPATHLENGTHS");
  virtual ~FillCNT_TrackPathLengths() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:

};

#endif
