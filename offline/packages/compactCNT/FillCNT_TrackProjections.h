#ifndef __FILLCNT_TRACKPROJECTIONS_H__
#define __FILLCNT_TRACKPROJECTIONS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>

class PHCentralTrack;

class FillCNT_TrackProjections: public SubsysReco
{
 public:
  FillCNT_TrackProjections(const std::string &name = "FILLCNT_TRACKPROJECTIONS");
  virtual ~FillCNT_TrackProjections() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  float flipandslide(const float z) const;

#ifdef DUMP
  std::ofstream dumprecover;
#endif

};

#endif
