#ifndef __MPCPI0CAL_H__
#define __MPCPI0CAL_H__

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <SubsysReco.h>
#include <getClass.h>
#include <EventHeader.h>


#include <PHGlobal.h>
#include <phool.h>
#include <vector>

#include <TrigLvl1.h>

#include "MpcPi0Mass.h"
#include "MpcPi0MassContainer.h"
#include "pi0base.h"
#include "pi0mixer.h"

#include <TROOT.h>
#include <TObjArray.h>
#include <TFile.h>
#include <TF1.h>
#include <TMath.h>
#include <TMatrixD.h>
#include <TVector3.h>
#include <TLorentzVector.h>
#include <TChain.h>
#include <TString.h>
#include <TStyle.h>

class PHCompositeNode;

class MpcPi0Mass;
class pi0mixer;
class pi0base;

class mpcTowerContainer;
class mpcClusterContainer;
class mpcClusterContent;
class EventHeader;
class PHGlobal;
class PHPythiaContainer;
class MpcMap;
class RunHeader;
class Fun4AllServer;

class TTree;
class TH1D;
class TH1F;
class TH2;
class TH2F;

class MpcPi0Cal: public SubsysReco
{
public:
  MpcPi0Cal(char *out_root_file, int iteration = 0, int run = 0);
  virtual ~MpcPi0Cal() {}
  
  //  For this analysis we only use Init, process_event, and End;
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);// {std::cout << "new run\n"; return 0;}
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode) {return 0;}
  int Reset(PHCompositeNode *topNode) {return 0;}
  int EndRun(const int runnumber);
  int End(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

  void GetNodes(PHCompositeNode *topNode); 

  int init_files();
  void init_groups();
  int get_group(int myrun);
  int get_status();
  int add_histos();
  void write_status();

  void set_iteration(int num) {fIteration = num;}
  void set_fname_coefficient();
  void SetCoefficient(Int_t as, Int_t x, Int_t y, Float_t coef) {coefficient[as][x][y] = coef;}
  Float_t GetCoefficient(Int_t as, Int_t x, Int_t y) {return coefficient[as][x][y];}
  void ReadCoefficients();	// read in current gain coefficients

  //Int_t CutDeadTower(int numx, int numy, int runnumber);
  Int_t CutEnergyAsym(Float_t e1, Float_t e2);

  void InitPi0Mass();
  void DumpCalibResults();
  void DumpCoefficient();
  Int_t MakeSummaryTower();
  Int_t MakeSummaryCoefficient();

  //additions for mixing

  static int passedMpcAsymmetry(const pi0base* ph1, const pi0base* ph2);
  static int passedMpcPhotonCuts(pi0base *ph);
  static int passedMpcPionCuts(const pi0base *ph1, const pi0base *ph2);
  int SetMpcPhoton(pi0base *ph, mpcClusterContent* clus, float zvertex, int run, int event);
  int SetMpcPion(pi0base *pi, pi0base *ph1, pi0base *ph2);
  

private:

  bool daflag;
  float centrality;
  int mpcWarnMap[2][18][18];
  static const float mpcAssym = 0.6;
  static const float mpcPtmin = 0.4;
  static const float mpcPtmax = 0.75;
  static const float mpcEmin = 8.0;
  static const float mpcEmax = 12.0;
  static const float mpcMaxRadiusCut = 50;  
  static const float mpcMinRadius = 3.5;  

  pi0mixer* mix[20][2]; //Beau: These are what I added for mixing
  pi0base* ph1; //Beau: mixing
  pi0base* ph2; //Beau: mixing
  MpcPi0MassContainer* mc; //Beau: mixing

  Int_t ncalls;  
  Int_t fIteration;

  // Declaration of leave types
  Int_t           runnum;
  Int_t           ntow;
  Int_t           nclus;
  Float_t         vertex;
  Int_t         zbin;
  enum{ STACK_CLUSTER = 500 };
  Int_t         tarm[STACK_CLUSTER];   //[nclus]
  Float_t         te[STACK_CLUSTER];   //[nclus]
  Float_t         te9[STACK_CLUSTER];   //[nclus]
  Float_t         tecore[STACK_CLUSTER];   //[nclus]
  Int_t           tixpos[STACK_CLUSTER];   //[nclus]
  Int_t           tiypos[STACK_CLUSTER];   //[nclus]
  Float_t         tx[STACK_CLUSTER];   //[nclus]
  Float_t         ty[STACK_CLUSTER];   //[nclus]
  Float_t         tz[STACK_CLUSTER];   //[nclus]

  Double_t e_1;
  Double_t e_2;
  Double_t e_pi0;
  Double_t mass_pi0;
  Double_t pt_pi0;
  Double_t p_pi0;

  TH1D *henergy1;
  TH1D *henergy2;
  TH1D *hptPi0;
  TH1D *henergyPi0;
  TH1D *hmassPi0;
  TH2F *hmass_vs_e_Pi0;

  TVector3 d3vec1;
  TVector3 d3vec2;
  TLorentzVector d4vec1;
  TLorentzVector d4vec2;
  TLorentzVector d4vecPi0;

  TString armname[2];  // south, north
  TString fname_coefficient;

  TStyle *m_style;

  std::string rootfile;
  TFile *hfile;
  TTree *tree;
  Int_t treeflag;	// whether to create tree

  mpcClusterContent *mpccluster;

  MpcMap *mpcmap;

  enum{ MPC_ARMNUM = 2 };
  enum{ MPC_X = 18 };
  enum{ MPC_Y = 18 };
  MpcPi0Mass* pi0mass_tower[MPC_ARMNUM][MPC_X][MPC_Y];
  Float_t coefficient[MPC_ARMNUM][MPC_X][MPC_Y];
  //Double_t leakage_correction[MPC_ARMNUM][MPC_X][MPC_Y];
  TH2 *leakage_mean_offset[MPC_ARMNUM];

  //DataNodes...
  mpcTowerContainer   *mpctowercont;
  mpcClusterContainer *mpcclustercont;
  EventHeader         *eheader;
  PHGlobal            *phglobal;
  PHPythiaContainer   *phpythia;
  Fun4AllServer* se;

  RunHeader           *runheader;
};

#endif /* __MPCPI0CAL_H__ */
