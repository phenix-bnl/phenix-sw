#ifndef __FILLCNT_DCHHITS_H__
#define __FILLCNT_DCHHITS_H__

#include <SubsysReco.h>

class PHCentralTrack;

class FillCNT_DchHits: public SubsysReco
{
 public:
  FillCNT_DchHits(const std::string &name = "FILLCNT_DCHHITS");
  virtual ~FillCNT_DchHits() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:



};

#endif
