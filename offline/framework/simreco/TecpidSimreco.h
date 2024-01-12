#ifndef __TECPIDSIMRECO_H__
#define __TECPIDSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;
class mTecPIDModule         ;

class TecpidSimreco: public SubsysReco
{
 public:
  TecpidSimreco(const char *name = "TECPID");
  virtual ~TecpidSimreco() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  mTecPIDModule         * mTecPID        ;

};

#endif /* __TECPIDSIMRECO_H__ */
