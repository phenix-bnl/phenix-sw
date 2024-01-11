#ifndef __FILLCNT_EMCPC3_H__
#define __FILLCNT_EMCPC3_H__

#include <SubsysReco.h>

class PHCentralPad;

class FillCNT_EmcPc3: public SubsysReco
{
 public:
  FillCNT_EmcPc3(const std::string &name = "FILLCNT_EMCPC3");
  virtual ~FillCNT_EmcPc3() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:


};

#endif
