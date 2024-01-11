#ifndef __MMPCEXMPCSHOWERMATCH_HH__
#define __MMPCEXMPCSHOWERMATCH_HH__

#include <memory>
#include <vector>
#include <SubsysReco.h>

class SubsysReco;
class PHCompositeNode;
class mpcClusterContainer;
class mpcTowerContainer;
class MpcMap;
class TMpcExHitContainer;
class TMpcExHit;
class TMpcExShowerContainer; 
class TMpcExShower; 
class BbcOut;
class PHGlobal;

class mMpcExMPCShowerMatch: public SubsysReco
{
 public:

  mMpcExMPCShowerMatch( const char* name="MMPCEXMPCSHOWERMATCH");
  virtual ~mMpcExMPCShowerMatch();
  
  struct PairIndices{
    unsigned int ClusterA;
    unsigned int ClusterB;
  };

  virtual int Init(PHCompositeNode*);
  virtual int InitRun(PHCompositeNode*);
  virtual int process_event(PHCompositeNode*);
  virtual int End(PHCompositeNode *topNode);

  void SetVerbosity(bool b) { verbosity = b; }
  void SetCleanShowerListFlag(bool b) { CleanShowerList = b; }

  void MatchMPC_HoughSpace(TMpcExShower *AShower);
  void MatchMPCTower_HoughSpace(TMpcExShower *AShower);
  void CalcShowerPosition(TMpcExShower *AShower);
  void CalcShowerHitInfo(TMpcExShower *AShower);
  void FindMPCclosest(TMpcExShowerContainer *SomeShowers);
  void MergeShowers(TMpcExShower *main, TMpcExShower *splinter);

  double CombinedHitEnergy(TMpcExHit *Ahit);

#ifndef __CINT__
  std::vector<PairIndices> CleanShowers(TMpcExShowerContainer *SomeShowers);
#endif 

  void set_interface_ptrs(PHCompositeNode* top_node);

 private:

  bool verbosity;
  bool CleanShowerList;

  float Econv;
  float ShowerR;
  float EMax_high;
  float hlslope;

  double get_vertex();

  mpcClusterContainer *_fmpcClusters;
  mpcTowerContainer *_fmpcTowers;
  MpcMap *_fmpc_map;
  TMpcExHitContainer *_fHits;
  BbcOut *_fBbcout;
  PHGlobal *_fGlobal;
};

#endif /* __MMPCEXMPCSHOWERMATCH_H__ */ 
