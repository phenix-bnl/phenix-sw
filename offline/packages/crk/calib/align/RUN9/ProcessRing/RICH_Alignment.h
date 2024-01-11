#include <iostream>
#include <fstream>
#include <stdlib.h>

#include "math.h"

#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TChain.h"
#include "TH2.h"
#include "TH1.h"
#include "TGraph.h"
#include "TCanvas.h"

#include "PHCompositeNode.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"
#include "PHGeometry.h"
#include "PHSphereSection.h"
#include "PHPoint.h"
#include "PHLine.h"

using namespace std;
using namespace PHGeometry;

class CrkGeometryObject_new;

class RICH_Alignment
{      //++CINT
 public:
  RICH_Alignment();
  RICH_Alignment(char *infile);
  virtual ~RICH_Alignment(){}
    
  void ImportHotPMTList(char *hotlistf="rich_hotPMTlist.txt"); 
  void Init(char *infile);
  void ProcessRings(int iloop);
  void Calc(int fixed=10, int mean=5);
  void Write();
  void Write(char *align);
  void Verify();
  void Process(int fixed=10, int mean=5);
  void ImportOutput(char *evalf="Evalout.root", char *alignf="alignment.dat");
  void Init_alignment_parameters(char *infile);
  void Init_alignment_parameters_0th(char *infile);
  void set_dr(float dr){Dr = dr;}

  void SetAlignmentFile(char *alignmentdat="alignment.dat")
    { strcpy(c_alignmentf,alignmentdat); }

  void SetEval(char *evalout="Evalout.root");
  void SetMaxent(int maxent=-1)
    { i_MaxEnt=maxent; }
  void SetEventOffset(int offset=0)
    { i_Offset=offset; }
  void VerifByPhi(int verifphi=1)
    { i_VerifByPhi=verifphi; }
  void SetVerbosity(int verb=0)
    { i_Verb=verb; }
  void Set_EndPoint(int det=0)
    { i_end_point = det;}
  void Use_Hit(int det=0)
    { i_use_hit = det;}

  float Getdz(int arm, int side, int panel, int iloop)
    { return f_dz[arm][side][panel][iloop]; }
  float Getdphi(int arm, int side, int panel, int iloop)
    { return f_dphi[arm][side][panel][iloop]; }
  float GetdzFinal(int arm, int side, int panel, int iloop)
    { return f_dzfinal[arm][side][panel]; }
  float GetdphiFinal(int arm,int side,int panel,int iloop)
    { return f_dphifinal[arm][side][panel]; }

  void SetMirrorX(float x){
    offset_x = x; 
  }
  void SetMirrorY(float y){
    offset_y = y; 
  }
  void SetMirrorZ(float z){
    offset_z = z; 
  }

 private:
  float v_ref[3];
  float b_ref[3];
  float posx[1000];
  float posy[1000];
  float posz[1000];
  float posr[1000];
  float posphi[1000];
  int this_npmt;
  short pmt[1000];
  float npe[1000];
  float tcrk[1000];

  float sv_ref[3];
  float sb_ref[3];
  float sposx[1000];
  float sposy[1000];
  float sposz[1000];
  float sposr[1000];
  float sposphi[1000];
  int  this_snpmt;
  short spmt[1000];
  float snpe[1000];
  float stcrk[1000];

  float ppc1pos[3];
  float ppc2pos[3];
  float ppc3pos[3];



  float f_dz[2][2][24][20];
  float f_dphi[2][2][24][20];
  float f_dzfinal[2][2][24];
  float f_dphifinal[2][2][24];
  float f_dzstart[2][2][24];
  float f_dphistart[2][2][24];
  
  float f_errdz[2][2][24];
  float f_errdphi[2][2][24];
  
  int ntrk, arm, side, panel;
  int sside, spanel;
  int  n0, npmt0, npmt1;  
  int  sn0;  
  float npe0, npe1, chi2, ecore, mom;
  float pc2dphi;
  float pc2dz;
  float pc3dphi;
  float pc3dz;
  float emcdphi;
  float emcdz;
  float alpha;
  float beta;
  float zed;
  float bbcz;
  float phi;
  int flag;
  int tmp_ntrk;
  float bbcq;
  float zdce;
  int run;

  float start[3];
  float end[3];


  int i_DSTLvl;      // 0:DST 1:UDST 2:NDST
  int i_UsePAD;      // 0:Projection to PAD from DC track  1:PAD hit
  int i_UseSurvey;   
  int i_Eval;
  int i_VerifByPhi;  // 0:Verify by Y axis (not recommended)
  int i_end_point ; // 0-pc2, 1-pc3, 
  int i_use_hit; //0 - use hit 1- use projection
  int i_MaxEnt;      // Max entry to proceed verify (-1:unlimited)
  int i_Offset;      // Max entry to proceed verify (-1:unlimited)

  int i_Verb;        // Verbosity level

  int i_nhotPMT;       // N of Hot PMT's
  int i_hotPMT[1000];  // List of Hot PMT's

  char c_dstname[200];
  char c_alignmentf[200];
  
  CrkGeometryObject_new *cgo;

  TFile *fin;
  TFile *tf_dst;
  TFile *tf_eval;

  TTree *trk;


  PHPoint cross_to_crk;
  PHPoint pstart, pend;
  PHPoint pmt_pos;
  PHLine ref;

  //  TTree *tree;
  int d_run;
  int d_ntrk;
  float d_bbcz;
  float d_bbcq;
  float d_zdce;
  int d_arm, d_side, d_panel, d_npmt;
  int d_sside, d_spanel, d_snpmt;
  float d_vref[3], d_bref[3];
  float d_svref[3], d_sbref[3];

  float d_mom, d_ecore;
  int d_n0;
  int d_sn0;
  int d_n0_new;
  int d_pmtid[1000];
  float d_posx[1000];
  float d_posy[1000];
  float d_posz[1000];
  float d_posr[1000];
  float d_R[1000];
  float d_X[1000];
  float d_Y[1000];
  float d_npe[1000];
  float d_tcrk[1000];

  int d_spmtid[1000];
  float d_sposx[1000];
  float d_sposy[1000];
  float d_sposz[1000];
  float d_sposr[1000];
  float d_sR[1000];
  float d_sX[1000];
  float d_sY[1000];
  float d_snpe[1000];
  float d_stcrk[1000];


  float offset_x;
  float offset_y;
  float offset_z;

  TGraph* FindCent(TH2* hist);
  void CalcError(TH2* hist, int arm, int side, int panel); 
  void SetBranches();

  float Dr;
  

  //  ClassDef(RICH_Alignment,0)  // ROOT Class Implementation
};

ClassImp(RICH_Alignment)   // ROOT Class Implementation
