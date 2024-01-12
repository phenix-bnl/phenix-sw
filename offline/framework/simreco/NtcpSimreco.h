#ifndef __NTCPSIMRECO_H__
#define __NTCPSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;
class NtcpEvent;

class NtcpSimreco: public SubsysReco
{
 public:
  NtcpSimreco(const char *name = "NTCP");
  virtual ~NtcpSimreco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const char *what) const {return;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  NtcpEvent *mNtcpEvent ;

};

#endif /* __NTCPSIMRECO_H__ */
