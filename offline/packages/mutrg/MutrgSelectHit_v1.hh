#ifndef __MUTRGSELECTHIT_V1__
#define __MUTRGSELECTHIT_V1__

#include "MutrgSelectHit.hh"

#include "MutrgPar.hh"

class MutrgSelectHit_v1 : public MutrgSelectHit{
public:
  MutrgSelectHit_v1(bool init_flag=true);
  virtual ~MutrgSelectHit_v1(void);
  void CreateObject(void);

  int Init(PHCompositeNode *node,bool flag_reg=false);
  int InitRun(PHCompositeNode *node,bool flag_reg=false);
  int ProcessEvent(PHCompositeNode *node);

  virtual int MultiplicityCut(void);
  virtual int ClusterSizeCut(void);

protected:
};

#endif /* __MUTRGSELECTHIT_V1__ */
