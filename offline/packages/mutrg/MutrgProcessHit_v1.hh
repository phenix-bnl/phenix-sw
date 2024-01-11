#ifndef __MUTRGPROCESSHIT_V1__
#define __MUTRGPROCESSHIT_V1__

#include "MutrgProcessHit.hh"

class TMutHit;
class TMutHitMap;
class MutrgUnpack;
class MutrgDecode;

class MutrgProcessHit_v1 : public MutrgProcessHit{
public:
  MutrgProcessHit_v1(bool init_flag=true);
  virtual ~MutrgProcessHit_v1(void);
  void CreateObject(void);

  int Init(PHCompositeNode *node,bool flag_reg=false);
  int InitRun(PHCompositeNode *node,bool flag_reg=false);
  int ProcessEvent(PHCompositeNode *node);
  int FillHit(Event *evt);
  int Associate(TMutHitMap *mut_hitmap);
  int CalcPeakValue(TMutHit *hit,double &peak,double &ptime,double &slope);

protected:
  MutrgUnpack *mutrg_unpack;
  MutrgDecode *mutrg_decode;
};

#endif /* __MUTRGPROCESSHIT_V1_ */
