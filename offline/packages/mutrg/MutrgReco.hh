#ifndef __MUTRGRECO__
#define __MUTRGRECO__

#include "SubsysReco.h"
#include "PHTimeStamp.h"
#include "MutrgPar.hh"

class TMutHitMap;
class MutrgProcessHit;
class MutrgSelectHit;
class MutrgFindTrk;

class MutrgReco : public SubsysReco{
public:
  enum INPUT_MODE {INPUT_PRDF,INPUT_DST};

  MutrgReco(const char *name="MutrgReco");
  virtual ~MutrgReco();

  virtual int Init(PHCompositeNode *top_node);
  virtual int InitRun(PHCompositeNode *top_node);
  virtual int End(PHCompositeNode *top_node);
  virtual int process_event(PHCompositeNode *top_node);

  virtual void CreateModule(void);

  virtual void SetInputMode(INPUT_MODE mode){input_mode=mode;}
  virtual int SetTrkMapFile(const char *filename);
  virtual int SetTrkMapDB(int run=-9999,MutrgPar::TrkDBVersion ver=MutrgPar::TRKDV_RUN_MUTRG_RPC_0);
  virtual int SetTrkMapDB(PHTimeStamp ts,MutrgPar::TrkDBVersion ver=MutrgPar::TRKDV_RUN_MUTRG_RPC_0);
  virtual void SetHitNameForTrk(const char *name);
  virtual void SetAllowMutrStripDiff(int diff);

  virtual void DoProcessHit(bool flag){flag_prohit=flag;}
  virtual void DoSelectHit(bool flag){flag_selhit=flag;}
  virtual void DoFindTrk(bool flag){flag_findtrk=flag;}
  virtual void DoProcessAll(bool flag);

  virtual void DoExtendHitClock(int clk_ext,int clk_shift);
  virtual void DoMultiplicityCut(int threshold);
  virtual void DoMultiplicityCut(int thre_s0,int thre_s1,int thre_s2,
				 int thre_n0,int thre_n1,int thre_n2);
  virtual void DoClusterSizeCut(int threshold);
  virtual void DoClusterSizeCut(int thre_s0,int thre_s1,int thre_s2,
				int thre_n0,int thre_n1,int thre_n2);
  virtual void DoClustering(bool flag,int max_size=320);

  virtual const char* ClassName(void){return Name();}

protected:
  INPUT_MODE input_mode;
  bool flag_prohit;
  bool flag_selhit;
  bool flag_findtrk;

  MutrgProcessHit *mutrg_prohit;
  MutrgSelectHit *mutrg_selhit;
  MutrgFindTrk *mutrg_findtrk;
};

#endif /* __MUTRGRECO__ */
