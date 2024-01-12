#ifndef __ZDCSIMRECO_H__
#define __ZDCSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;

class ZdcEvent   ;
class ZdcResponse;


class ZdcSimreco: public SubsysReco
{
 public:
  ZdcSimreco(const std::string &name = "ZDC");
  virtual ~ZdcSimreco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

 protected:

  ZdcEvent*    mZdcEvent    ;
  ZdcResponse* mZdcResponse ;
};

#endif /* __ZDCSIMRECO_H__ */
