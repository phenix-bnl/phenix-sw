#ifndef __CREATECNT_H__
#define __CREATECNT_H__

#include <SubsysReco.h>

class PHCentralTrack;

class CreateCNT: public SubsysReco
{
 public:
  CreateCNT(const std::string &name = "CREATECNT");
  virtual ~CreateCNT() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:

  PHCentralTrack *phcnt;
};

#endif
