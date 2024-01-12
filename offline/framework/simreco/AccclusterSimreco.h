#ifndef __ACCCLUSTERSIMRECO_H_
#define __ACCCLUSTERSIMRECO_H_

#include "SubsysReco.h"

class AccProj;

class AccclusterSimreco : public SubsysReco
{
 public:
  AccclusterSimreco(const char * name = "AccclusterSimreco", const int ver=1);
  virtual ~AccclusterSimreco() {}

  int Init         (PHCompositeNode* topNode);
  int process_event(PHCompositeNode* topNode);
  int ResetEvent   (PHCompositeNode* topNode);

 protected:
  int version;

  AccProj* d_accproj;

};

#endif
