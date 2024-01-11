#include <iostream>
#include <fstream>
#include <stdlib.h>

using namespace std;

#include "TROOT.h"
#include "TTree.h"
#include "TFile.h"
#include "TH2.h"
#include "TGraph.h"

#include "CrkGeometryObject.hh"
          

class PHTrackOut;
class CglTrack;
class DchTrack;
class CrkHit;
class PadCluster;
class EmcClusterLocalExt;

class dCrkHitWrapper;
class dPadClusterWrapper;
class dPadClusterWrapper;
class dEmcClusterLocalExtWrapper;
class emcClusterContainer;
class emcClusterContent;

class PHCompositeNode;
class PHNodeIOManager;


class RICH_Alignment{      //++CINT
 public:
  RICH_Alignment();
  RICH_Alignment(char *dstin);
  virtual ~RICH_Alignment(){}

  Int_t SetDST(char *dstin="DST.root");
  void SetAlignmentFile(char *alignmentdat="alignment.dat")
    {strcpy(c_alignmentf,alignmentdat);}
  void SetEval(char *evalout="Evalout.root"){
    i_Eval=1;
    tf_eval = new TFile(evalout,"RECREATE");
  }

  void SetMaxent(int maxent=-1){i_MaxEnt=maxent;}
  void SetEventOffset(int offset=0){i_Offset=offset;}

  void SetDSTLvl(int isudst=1){i_DSTLvl=isudst;}
  void UsePAD(int usepad=1){i_UsePAD=usepad;}
  void UseSurvey(int usesurvey=1){i_UseSurvey=usesurvey;}
  void VerifByPhi(int verifphi=1){i_VerifByPhi=verifphi;}
  void ImportHotPMTList(char *hotlistf="rich_hotPMTlist.txt");

  void SetVerbosity(int verb=0){i_Verb=verb;}

  void ProcessTracks();
  void ProcessRings(Int_t iloop);
  void Calc(Int_t fixed=10, Int_t mean=5);

  void Write();
  void Write(char *align);
  void Verify();

  void Process(Int_t fixed=10, Int_t mean=5);


  Float_t Getdz(Int_t arm,Int_t side,Int_t panel,Int_t iloop)
    {return f_dz[arm][side][panel][iloop];}
  Float_t Getdphi(Int_t arm,Int_t side,Int_t panel,Int_t iloop)
    {return f_dphi[arm][side][panel][iloop];}

  Float_t GetdzFinal(Int_t arm,Int_t side,Int_t panel,Int_t iloop)
    {return f_dzfinal[arm][side][panel];}
  Float_t GetdphiFinal(Int_t arm,Int_t side,Int_t panel,Int_t iloop)
    {return f_dphifinal[arm][side][panel];}

  void ImportOutput(char *evalf="Evalout.root",char *alignf="alignment.dat");

 private:
  
  Float_t f_dz[2][2][24][20];
  Float_t f_dphi[2][2][24][20];

  Float_t f_dzfinal[2][2][24];
  Float_t f_dphifinal[2][2][24];

  Float_t f_errdz[2][2][24];
  Float_t f_errdphi[2][2][24];


  TFile *tf_dst;
  TFile *tf_eval;

  char c_dstname[200];
  char c_alignmentf[200];

  Int_t i_DSTLvl;      // 0:DST 1:UDST 2:NDST
  Int_t i_UsePAD;      // 0:Projection to PAD from DC track  1:PAD hit
  Int_t i_UseSurvey;   
  Int_t i_Eval;
  Int_t i_VerifByPhi;  // 0:Verify by Y axis (not recommended)

  Int_t i_MaxEnt;      // Max entry to proceed verify (-1:unlimited)
  Int_t i_Offset;      // Max entry to proceed verify (-1:unlimited)

  Int_t i_Verb;        // Verbosity level

  Int_t i_nhotPMT;       // N of Hot PMT's
  Int_t i_hotPMT[1000];  // List of Hot PMT's

  TTree *T;
  TTree *tracks;

  CrkGeometryObject *cgo;

  PHTrackOut *phtrk;
  CglTrack *cgl;
  DchTrack *dch;

  dEmcClusterLocalExtWrapper* demc;

  CrkHit *dchit;
  PadCluster* dpc1;
  PadCluster* dpc3;

  emcClusterContainer *emcClusters;
  emcClusterContent *emccontent;

  TBranch *b_phtrk;
  TBranch *b_cgl;
  TBranch *b_dch;

  TBranch *b_chit;
  TBranch *b_pc1;
  TBranch *b_pc3;
  TBranch *b_emc;

  TBranch *b_cnt;

  void CalcError(TH2* hist,int arm,int side,int panel); 
  void SetupCGO()
    { cgo = new CrkGeometryObject(); if(i_UseSurvey==1) {cgo->UseSurvey();}}
  void SetBranches();

  TGraph* FindCent(TH2* hist);

  PHCompositeNode* topNode;
  PHCompositeNode* dstNode;
  PHNodeIOManager *iman;

  ClassDef(RICH_Alignment,0)  // ROOT Class Implementation
};

