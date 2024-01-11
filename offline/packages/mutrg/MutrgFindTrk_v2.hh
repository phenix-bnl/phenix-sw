#ifndef __MUTRGFINDTRK_V2__
#define __MUTRGFINDTRK_V2__

#include "MutrgFindTrk.hh"
#include "MutrgPar.hh"
#include <vector>

class TMutTrkMap;
class MutrgHit;
class MutrgHitArray;
class MutrgTrkArray;
class MutrgTrkMapping_v1;

class MutrgFindTrk_v2 : public MutrgFindTrk{
public:
  MutrgFindTrk_v2(bool init_flag=true);
  virtual ~MutrgFindTrk_v2(void);
  void CreateObject(void);
  int SetMapFile(const char *filename);
  int SetMapDB(int run=-9999,MutrgPar::TrkDBVersion ver=
               MutrgPar::TRKDV_RUN_MUTRG_RPC_0);
  int SetMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver=
               MutrgPar::TRKDV_RUN_MUTRG_RPC_0);

  int Init(PHCompositeNode *node,bool flag_reg=false);
  int InitRun(PHCompositeNode *node,bool flag_reg=false);
  int ProcessEvent(PHCompositeNode *node);

  int FindTrk(MutrgHitArray *mutrg_hits);
  int Associate(TMutTrkMap *mut_trkmap);

protected:
  bool flag_set_mapping;

  MutrgTrkMapping_v1 *mutrg_mapping;
};

#endif /* __MUTRGFINDTRK_V2__ */
