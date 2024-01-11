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

#include "CrkGeometryObject.hh"
#include "PHCompositeNode.h"
#include "PHNodeIOManager.h"
#include "PHIODataNode.h"
#include "PHGeometry.h"
#include "PHSphereSection.h"
#include "PHPoint.h"
#include "PHLine.h"

using namespace std;
using namespace PHGeometry;

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

  void SetAlignmentFile(char *alignmentdat="alignment.dat")
    { strcpy(c_alignmentf,alignmentdat); }
  void SetEval(char *evalout="Evalout.root")
    {
      i_Eval=1;
      tf_eval = new TFile(evalout,"RECREATE");
    }
  void SetMaxent(int maxent=-1)
    { i_MaxEnt=maxent; }
  void SetEventOffset(int offset=0)
    { i_Offset=offset; }
  void VerifByPhi(int verifphi=1)
    { i_VerifByPhi=verifphi; }
  void SetVerbosity(int verb=0)
    { i_Verb=verb; }


  float Getdz(int arm, int side, int panel, int iloop)
    { return f_dz[arm][side][panel][iloop]; }
  float Getdphi(int arm, int side, int panel, int iloop)
    { return f_dphi[arm][side][panel][iloop]; }
  float GetdzFinal(int arm, int side, int panel, int iloop)
    { return f_dzfinal[arm][side][panel]; }
  float GetdphiFinal(int arm,int side,int panel,int iloop)
    { return f_dphifinal[arm][side][panel]; }

 private:
  float *v_ref;
  float *b_ref;
  float *posx;
  float *posy;
  float *posz;
  float *posr;
  float *posphi;
  float *this_npe;

  float f_dz[2][2][24][20];
  float f_dphi[2][2][24][20];
  float f_dzfinal[2][2][24];
  float f_dphifinal[2][2][24];
  float f_dzstart[2][2][24];
  float f_dphistart[2][2][24];
  
  float f_errdz[2][2][24];
  float f_errdphi[2][2][24];
  
  int ntrk, arm, side, panel, this_npmt, n0;  

  int i_DSTLvl;      // 0:DST 1:UDST 2:NDST
  int i_UsePAD;      // 0:Projection to PAD from DC track  1:PAD hit
  int i_UseSurvey;   
  int i_Eval;
  int i_VerifByPhi;  // 0:Verify by Y axis (not recommended)

  int i_MaxEnt;      // Max entry to proceed verify (-1:unlimited)
  int i_Offset;      // Max entry to proceed verify (-1:unlimited)

  int i_Verb;        // Verbosity level

  int i_nhotPMT;       // N of Hot PMT's
  int i_hotPMT[1000];  // List of Hot PMT's

  char c_dstname[200];
  char c_alignmentf[200];
  
  CrkGeometryObject *cgo;

  TFile *fin;
  TFile *tf_dst;
  TFile *tf_eval;

  TTree *trk;

  TGraph* FindCent(TH2* hist);
  void CalcError(TH2* hist, int arm, int side, int panel); 
  void SetBranches();

  ClassDef(RICH_Alignment,0)  // ROOT Class Implementation
};

ClassImp(RICH_Alignment)   // ROOT Class Implementation
