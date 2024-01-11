#ifndef __UTIREACTIONPLANE_H__
#define __UTIREACTIONPLANE_H__

#include <cmath>
#include "TProfile.h"
#include <iostream>
#include <fstream>
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TSystem.h>
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TFile.h"
#include "TGraphErrors.h"
#include "TPaveText.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TTree.h"

class TriggerHelper;
class BbcRaw;
class DchTrack;
class PHCompositeNode;
class utiReactionPlane {
  static utiReactionPlane* _instance;
  utiReactionPlane(); 
 public:
  static utiReactionPlane* instance();
  virtual ~utiReactionPlane();

  bool CheckByOneZ;
  bool CheckByOneCent;
  
  int ncent;
  int nzps;
  int ndet;
  int nhar;
  int runset;

  ////////// useful variables
  int cent;
  float z;
  bool minbias;
  float chargesum;
  int ntrack;
  ///////////////////

  void  readTable(int run, int DB=0);
  void  writeTable(int run, int DB=0);
  void  calculateReac(PHCompositeNode *topNode,int pass);
  void  finishpass(int pass);
  float getReacPlane(int idet, int ihar);
  float getObsReacPlane(int idet, int ihar);
  float GetWeightedSummandX(int idet, int ihar);
  float GetWeightedSummandY(int idet, int ihar);
  void  InitializeWeights();
  void  SetParamTable(int runs, int zbins, int centb, int dets, int hars);
  void  CalcWeightedSummandBBC(int icent,int izps, BbcRaw *bbcraw);
  void  CalcWeightedSumandCNT(int icent,int izps, DchTrack * dchtrack);
  float Flattened(float phi, int idet,int ihar,int icent,int izps);
  float OffsetCorrected(float y, float x, float cy, float cx, int har);
  void  FillFlatteningHistograms(float psiObs,int idet, int ihar, int icent,int izps);
  int   GetNCent();
  int   GetCent();
  int   GetNZps();
  void  setincalibname(const char*name);
  void  setoutcalibname(const char*name);
  void  setOffByOneZ(bool yesno);
  void  setOffByOneCent(bool yesno);
  int   GetCentralityBin(int cent);
  int   GetZBin(float z);
  float GetZ();
  float GetCharge();
  bool  GetMinBias();
  int   GetNtrack();
  void  makeHisto();
  void  savetofile(const char*name="histo.root");
  float GetFactor(int izps, int icent, int ipmt);
  float GetCharge(int izps, int icent, int ipmt);
  void  FillEPass1(float marray[400]);
private:
  static const int nDet=8;
  static const int nHar=4;
  static const int MaxCentBins=50;
  static const int nZps=30;
  static const int nOrd=32;

  char incalibname[200];
  char outcalibname[200];
  // Some Variable Constants
  
  void  deleteHisto();
  // CalFile info
  float     pede[128],oflw[128],slw1[128],slw2[128];  
  float     gain[128],tgai[128],qgai[128];
  
  // Geo File info
  float     xpos[128],ypos[128],zpos[128],phipos[128];


  int izps,icent;

  // CNT weights
  float     sumx[nDet][nHar];
  float     sumy[nDet][nHar];
  float     sumw[nDet][nHar];

  float     charge[128];
  float     ccharge[128];

  float      psi[nDet][nHar];
  float      psiobs[nDet][nHar];//observed reaction plane

  float      costable[nDet][nHar][MaxCentBins][nZps][nOrd]; ///aaa
  float      sintable[nDet][nHar][MaxCentBins][nZps][nOrd]; ///bbb

  float     ofsx[nDet][nHar][MaxCentBins][nZps];
  float     ofsy[nDet][nHar][MaxCentBins][nZps];
  int       irad[128];

  float     fac[MaxCentBins][nZps][128];
  
 


  // Filled in CalhitPlan
  TProfile *Hsum[nDet][nHar][MaxCentBins][nZps];
  TH1F     *Hphi[nDet][nHar][MaxCentBins][nZps];
  TProfile *Hprc[nDet][nHar][MaxCentBins][nZps];
  TProfile *Hprs[nDet][nHar][MaxCentBins][nZps];
  TH1F     *Hphj[nDet][nHar][MaxCentBins][nZps];

  // Filled during calbbc
  TProfile *Hpmt[MaxCentBins][nZps];
  TProfile *Hrad[MaxCentBins][nZps];


  TH2F     *Hcor[MaxCentBins][nZps];
  TH2F     *Hrsp[MaxCentBins];
  TProfile *Hrsc[MaxCentBins];
  TProfile *Hrss[MaxCentBins];
  //  TFile    *hfile;

};
#endif  /* __UTIREACTIONPLANE_H__ */


