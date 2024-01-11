#ifndef __MUTRGSELECTHIT__
#define __MUTRGSELECTHIT__

#include <string>
#include <iostream>
#include "MutrgPar.hh"

class PHCompositeNode;
class MutrgHitArray;

class MutrgSelectHit{
public:
  MutrgSelectHit(bool init_flag=true);
  virtual ~MutrgSelectHit(void);
  virtual void CreateObject(void);

  virtual int SetMutrgSelHitArray(MutrgHitArray *hits,bool flag_delete=false);
  virtual MutrgHitArray* GetMutrgSelHitArray(void){return mutrg_hits_sel;}
  virtual MutrgHitArray* RegMutrgSelHitArray(PHCompositeNode *node,
					     const char *name,
					     const char *rename="");

  virtual int Init(PHCompositeNode *node,bool flag_reg=false);
  virtual int InitRun(PHCompositeNode *node,bool flag_reg=false);
  virtual int ProcessEvent(PHCompositeNode *node);

  virtual void DoExtendHitClock(int clk_ext,int clk_shift);
  virtual void DoMultiplicityCut(int threshold);
  virtual void DoMultiplicityCut(int thre_s0,int thre_s1,int thre_s2,
				 int thre_n0,int thre_n1,int thre_n2);
  virtual void DoClusterSizeCut(int threshold);
  virtual void DoClusterSizeCut(int thre_s0,int thre_s1,int thre_s2,
				int thre_n0,int thre_n1,int thre_n2);
  virtual void DoClustering(bool flag,int max_size=320);

  const char *ClassName(void) const{return class_name.c_str();}

  virtual void PrintParameters(std::ostream &os=std::cout) const;

protected:
  std::string class_name;

  int hit_clk_ext;
  int hit_clk_shift;

  bool do_multiplicity_cut;
  int multiplicity_threshold[MutrgPar::NARM][MutrgPar::NSTATION];

  bool do_clsize_cut;
  int clsize_threshold[MutrgPar::NARM][MutrgPar::NSTATION];

  bool do_clustering;
  int max_cluster_size;

  MutrgHitArray *mutrg_hits_sel; // hit array after selection
};

#endif /*  __MUTRGSELECTHIT__ */
