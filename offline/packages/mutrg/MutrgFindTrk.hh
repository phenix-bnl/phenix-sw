#ifndef __MUTRGFINDTRK__
#define __MUTRGFINDTRK__

#include <string>

#include "PHTimeStamp.h"
#include "MutrgPar.hh"

class PHCompositeNode;
class TMutTrkMap;
class MutrgHitArray;
class MutrgTrk;
class MutrgTrkArray;

class MutrgFindTrk{
public:
  MutrgFindTrk(bool init_flag=true);
  virtual ~MutrgFindTrk(void);
  virtual void CreateObject(void);

  virtual int SetMutrgTrkArray(MutrgTrkArray *trks,bool flag_delete=false);
  virtual MutrgTrkArray* GetMutrgTrkArray(void){return mutrg_trks;}
  virtual MutrgTrkArray* RegMutrgTrkArray(PHCompositeNode *node,
					  const char *name,
					  const char *rename="");

  virtual int Init(PHCompositeNode *node,bool flag_reg=false);
  virtual int InitRun(PHCompositeNode *node,bool flag_reg=false);
  virtual int ProcessEvent(PHCompositeNode *node);

  virtual int FindTrk(PHCompositeNode *node);
  virtual int FindTrk(MutrgHitArray *mutrg_hits);
  virtual int Associate(TMutTrkMap *mut_trkmap);

  virtual int SetMapFile(const char *filename);
  virtual int SetMapDB(int run=-9999,MutrgPar::TrkDBVersion ver=MutrgPar::TRKDV_RUN_MUTRG_RPC_0);
  virtual int SetMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver=MutrgPar::TRKDV_RUN_MUTRG_RPC_0);
  virtual void SetHitNameForTrk(const char *name){mutrg_hits_name=name;}
  virtual void SetAllowMutrStripDiff(int diff){allow_mutr_strip_diff=diff;}

  virtual const char* GetHitNameForTrk(void){return mutrg_hits_name.c_str();}
  virtual int GetAllowMutrStripDiff(int diff){return allow_mutr_strip_diff;}

  const char *ClassName(void) const{return class_name.c_str();}

protected:
  std::string class_name;

  std::string mutrg_hits_name;

  MutrgTrkArray *mutrg_trks;

  int allow_mutr_strip_diff;
};

#endif /* __MUTRGFINDTRK__ */
