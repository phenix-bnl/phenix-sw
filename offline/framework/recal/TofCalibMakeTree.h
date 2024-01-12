#ifndef __TOFRECALCREATETREE_H__
#define __TOFRECALCREATETREE_H__

#include "SubsysReco.h"
#include "Tof.hh"
#include "TFile.h"
#include "TTree.h"
#include "gsl/gsl_const_cgs.h"

class PHCompositeNode;
class TofCalibObject;

class TofCalibMakeTree: public SubsysReco
{
 public:
  TofCalibMakeTree(const char *OutFile);
  virtual ~TofCalibMakeTree() {}
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode) {return 0;}
  int Reset(PHCompositeNode *topNode) {return 0;}
  int End(PHCompositeNode *topNode);
  void Print(const char *what) const {return;}
  bool IsRun4_200GeV();
  bool IsRun4_62GeV();
  bool IsRun5_200GeV();
  bool IsRun5_62GeV();
  bool IsRun5_22_5GeV();
  bool IsRun5_pp_200GeV();
  bool IsRun6_pp_200GeV();
  bool IsRun6_pp_62GeV();
  bool IsRun7_AuAu_200GeV();
  bool IsRun8_dAu_200GeV();
  bool IsRun8_pp_200GeV();
  bool IsRun9_pp_500GeV();
  bool IsRun9_pp_200GeV();
  bool IsRun10_AuAu_200GeV();
  bool IsRun10_AuAu_62GeV();
  bool IsRun10_AuAu_39GeV();
  bool IsRun10_AuAu_7GeV();
  bool IsRun11_pp_500GeV();

 protected:
  std::string OutFileName;

 private:
  static const float c      = GSL_CONST_CGS_SPEED_OF_LIGHT/1.0e9; //[cm/ns]
  static const float m_pion = 0.13956995;                         // [GeV/c]^2

  TFile* hfile;
  TTree* tree;

// event parameters
  unsigned int RunNumber;
  float ZdcEnergyN;
  float ZdcEnergyS;
  int   BbcMultN;
  int   BbcMultS;
  float BbcChargeN;
  float BbcChargeS;
  float BbcZVertex;
  float BbcTimeZero;
//  float BbcPercentile;
  short NumberTofHits;
  float Centrality;

// track parameters
  static const int MAX_TRK = 300;
  int   ntrack;
  unsigned int npart;
  short charge      [MAX_TRK];
  short quality     [MAX_TRK];
  float zed         [MAX_TRK];
  float alpha       [MAX_TRK];
  float phi         [MAX_TRK];
  float phi0        [MAX_TRK];
  float the0        [MAX_TRK];
  float mom         [MAX_TRK];
  float ptofx       [MAX_TRK];
  float ptofy       [MAX_TRK];
  float ptofz       [MAX_TRK];
  float pltof       [MAX_TRK];
  float tofdphi     [MAX_TRK];
  float tofdz       [MAX_TRK];
  float tofsdphi    [MAX_TRK];
  float tofsdz      [MAX_TRK];
  int   slat        [MAX_TRK];
  float ttof        [MAX_TRK]; 
  float etof        [MAX_TRK];
  float tofph1      [MAX_TRK];
  float tofph2      [MAX_TRK];
  float toftdc1     [MAX_TRK];
  float toftdc2     [MAX_TRK];
  short dcarm       [MAX_TRK];



};

#endif /* __TOFRECALCREATETREE_H__ */



