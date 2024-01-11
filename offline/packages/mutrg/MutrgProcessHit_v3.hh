#ifndef __MUTRGPROCESSHIT_V3__
#define __MUTRGPROCESSHIT_V3__

#include "MutrgProcessHit.hh"

class TMutHit;
class TMutHitMap;
class MutrgUnpack;
class MutrgDecode;

class MutrgProcessHit_v3 : public MutrgProcessHit{
public:
  MutrgProcessHit_v3(bool init_flag=true);
  virtual ~MutrgProcessHit_v3(void);
  void CreateObject(void);

  int Init(PHCompositeNode *node,bool flag_reg=false);
  int InitRun(PHCompositeNode *node,bool flag_reg=false);
  int ProcessEvent(PHCompositeNode *node);
  int FillHit(Event *evt);
  int Associate(TMutHitMap *mut_hitmap);

protected:
  MutrgUnpack *mutrg_unpack;
  MutrgDecode *mutrg_decode;
};

#endif /* __MUTRGPROCESSHIT_V3_ */
