#ifndef __TOFWRECO_H_
#define __TOFWRECO_H_

#include "SubsysReco.h"

class TofwEvent;
class TofwGeometry;
class TofwCalib;

class TofwReco : public SubsysReco
{

 public:
  TofwReco(const std::string &name = "TOFW");
  virtual ~TofwReco();

  int Init(PHCompositeNode* topNode);
  int InitRun(PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);
  int ResetEvent(PHCompositeNode* topNode);

 protected:
  int CreateNodeTree(PHCompositeNode* topNode);

  TofwEvent* d_tofw;
  TofwGeometry* d_geom;
  TofwCalib* d_calib;
};

#endif
