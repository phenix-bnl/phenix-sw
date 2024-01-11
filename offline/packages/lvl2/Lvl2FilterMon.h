#ifndef __LVL2FILTERMON_H__
#define __LVL2FILTERMON_H__

#include <iostream>
#include <SubsysReco.h>

class PHCompositeNode;
class TFile;
class TH1F;
class TH2F;
class TTree;
class TBranch;
class TLeaf;

class Lvl2FilterMon: public SubsysReco
{

 public:

  Lvl2FilterMon(const char *name = "ATPLVL2");
  virtual ~Lvl2FilterMon() {};

  int process_event(PHCompositeNode *topNode);
  int Init(PHCompositeNode *topNode);
  void identify(std::ostream& out = std::cout) const;
  int BeginRun(const int runno);
  int EndRun(const int runno);

  void SetSequenceNumber(int inesq);
 
private:

  float Lvl1Scaled[32];
  float Lvl2ExecutedLvl1Scaled[32][32];
  float Lvl2FiredLvl1Scaled[32][32];
  float Lvl2ErrorLvl1Scaled[32][32];
  const char *Lvl1Name[64];
  const char *Lvl2Name[64];

  unsigned int nevt;
  unsigned int ntrigrun;
  int esq;
  unsigned int runnumber;
  int init_done;
  int done;
  int got_names;
  int real_data_flag;

  char lvl2decisionnodename[100];
  char lvl2trigrunnodename[100];
  char lvl2outarraynodename[100];

  TFile *hfile;
  TH1F *rejectplot;
  TH1F *zvertplot;
  TH2F *momvsenergy;
  TH2F *pc3yvsz_east;
  TH2F *pc3yvsz_west;
  TH2F *massvspt;
  TH2F *eoverpvsnpe;
  TH2F *eoverpvsp;
  TH2F *richyvsz_east;
  TH2F *richyvsz_west;
  TH2F *pc1yvsz_east;
  TH2F *pc1yvsz_west;
  TH2F *emcyvsz_east;
  TH2F *emcyvsz_west;
  TH1F *trkringdist;

  //MuidTrackPrim
  TH1F *muidN_depth;
  TH1F *muidN_slope;
  TH1F *muidN_panel;
  TH1F *muidN_trkpevt;
  TH1F *muidN_hitpevt;
  TH1F *muidN_hid;
  TH1F *muidN_vid;
  TH1F *muidN_pmin;
  TH1F *muidS_depth;
  TH1F *muidS_slope;
  TH1F *muidS_panel;
  TH1F *muidS_trkpevt;
  TH1F *muidS_hitpevt;
  TH1F *muidS_hid;
  TH1F *muidS_vid;
  TH1F *muidS_pmin;

  //MuidPairPrim
  TH1F *muidN_openangle;
  TH1F *muidN_pairpevt;
  TH1F *muidN_mass;
  TH1F *muidS_openangle;
  TH1F *muidS_pairpevt;
  TH1F *muidS_mass;

  //MutrTrackPrim
  TH1F *mutrN_trkpevt; 
  TH1F *mutrN_depth;  
  TH1F *mutrN_sign;  
  TH1F *mutrN_E;  
  TH1F *mutrN_ptot;
  TH1F *mutrN_theta_muid;
  TH1F *mutrN_theta_st3;
  TH1F *mutrN_theta_st2; 
  TH1F *mutrN_phi_muid;
  TH1F *mutrN_phi_st3;
  TH1F *mutrN_phi_st2; 
  TH1F *mutrS_trkpevt; 
  TH1F *mutrS_depth;
  TH1F *mutrS_sign;
  TH1F *mutrS_E;
  TH1F *mutrS_ptot;
  TH1F *mutrS_theta_muid;
  TH1F *mutrS_theta_st3;
  TH1F *mutrS_theta_st2;
  TH1F *mutrS_phi_muid;
  TH1F *mutrS_phi_st3;
  TH1F *mutrS_phi_st2;

  //MutrPairPrim
  TH1F *mutrN_pairpevt;
  TH1F *mutrN_mass;
  TH1F *mutrN_openangle;
  TH1F *mutrS_pairpevt;
  TH1F *mutrS_mass;
  TH1F *mutrS_openangle; 

  //MuonTrigFilter
  TH1F *L2MuonTrig_bbc_charge_sum;
  TH1F *L2MuonTrig_muid_nhitsS;
  TH1F *L2MuonTrig_muid_nhitsN;
  TH1F *L2MuonTrig_accepted;
  TH2F *L2MuonTrig_nhitsS_bbc;
  TH2F *L2MuonTrig_nhitsN_bbc;
  TH2F *L2MuonTrig_nhitsNS_bbc;

  //Tree Setup
  TTree *trgstats;
  TBranch *b;
  int RunNum;
  int FirstEvtNum;
  int nEvtsTotal;
  int nEvtsRawRej;
  int nEvtsNotData;
  int nEvtsGood;
  float nLL1Trg[32];
  //nL2EmcHighPtTile[iLL1Trg][nL2Exec,nL2Acc]
  float nL2EmcHighPtTile[32][2];
  float nL2AuAuDiElectron[32][2];
  float nL2MutrDimuonNorth[32][2];
  float nL2MutrDimuonSouth[32][2];
  float nL2EMu[32][2];  
};

#endif /*__LVL2FILTERMON_H__ */  
