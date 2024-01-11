#ifndef __MUTRGPROCESSHIT_V2__
#define __MUTRGPROCESSHIT_V2__

#include "MutrgProcessHit.hh"

class TMutHit;
class TMutHitMap;
class MutrgUnpack;
class MutrgDecode;

class MutrgProcessHit_v2 : public MutrgProcessHit{
public:
  MutrgProcessHit_v2(bool init_flag=true);
  virtual ~MutrgProcessHit_v2(void);
  void CreateObject(void);

  int Init(PHCompositeNode *node,bool flag_reg=false);
  int InitRun(PHCompositeNode *node,bool flag_reg=false);
  int ProcessEvent(PHCompositeNode *node);
  int FillHit(Event *evt);

protected:
  MutrgUnpack *mutrg_unpack;
  MutrgDecode *mutrg_decode;
};

#endif /* __MUTRGPROCESSHIT_V2_ */
