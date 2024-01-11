#ifndef __MPCPI0TTREE_H__
#define __MPCPI0TTREE_H__

/* Beau Meredith 11-11-2010 
This is a class adapted from Aaron Veicht
who adapted it from Mickey Chiu.  It writes out information for two
clusters (including partesum info) which pass a minimal set of cuts
The output is then used in the iterative pi0 calibration.

The basic cuts are as follows on both clusters:

E > 2 GeV
chi2/dof < 3
E1+E2 > 7
Easymm = fabs(E1 - E2)/(E1+E2) < 0.6
mass < 1 GeV/c^2


*/


#include "SubsysReco.h"
#include <iostream>
#include <fstream>
#include <string>
#include <MpcMap.h>
#include <BbcOut.h>
#include <TString.h>

class Fun4AllHistoManager;
class PHCompositeNode;
class TH1;
class TH2;
class TCanvas;
class PHGlobal;
class EventHeader;
class RunHeader;
class TriggerHelper;
//class MpcCalib;
//class MpcGeom;
//class BbcOut;
//class BbcGeo;
class TNtuple;
class TFile;
class TTree;
class TVector3;
class TLorentzVector;
//class TMath;
class MpcMip;
class mpcClusterContainer;
class mpcPi0TTree: public SubsysReco
{
 public:
  mpcPi0TTree(const char* outfile = "pi0.root", const char* inDeadTowerList="");
  virtual ~mpcPi0TTree();
  
  //  For this analysis we only use Init, process_event;
  int Init         (PHCompositeNode *topNode);
  int InitRun      (PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End          (PHCompositeNode *topNode);
  
  int Reset        (PHCompositeNode *topNode) { return 1; }
  
  void SetSimFlag(int sim_or_not) { simulation_flag = sim_or_not; }

 protected:
  std::string OutFileName;
  Fun4AllHistoManager *HistoManager;
  
  TCanvas *DisplayCanvas;
  TH1 *hPi;
  mpcClusterContainer *mpcclus;
  EventHeader *evtheader; 
  RunHeader *runheader;
  TTree *ttree; 
  TTree *stree; 
  TFile *pi0file;
  TriggerHelper *trighelp;
  int event; 
  int run;
  int simulation_flag;	// whether running on sims(=1) or not (=0)
  float zvtx;
  float cent;


  float etow[220];
  int ixtow[220];
  int iytow[220];
  float chtow[220];
  int ntow_tot;

  float pi0_z;
  float x1;  
  float y1;
  float z1;
  float x2;
  float y2;
  float z2;
  float px1;  
  float py1;
  float pz1;
  float px2;
  float py2;
  float pz2;  
  float energy1;
  float energy2;	
  float pi0_px; 
  float pi0_py; 
  float pi0_pz;
  float pi0_energy;
  float mass;
  float pt;
  int arm1;
  int arm2;
  PHGlobal *global;
  TString DeadTowerList;
  int tower[2][18][18];
  MpcMap* mpcmap;
  int driver1;
  int driver2;
  int fee1;
  int fee2;

   float etot1;
  float etot2;
  float cdisp;
  float disp;
  
  Int_t ixpos;
  Int_t iypos;
  float ldisp;
  float lcdisp;
  float corrdispx1;
  float corrdispy1;
  float corrdispx2;
  float corrdispy2;
  float logcorrdispx1;
  float logcorrdispy1;
  float logcorrdispx2;
  float logcorrdispy2;
  float dispx1;
  float dispy1;
  float dispx2;
  float dispy2;
  float logdispx1;
  float logdispy1;
  float logdispx2;
  float logdispy2;

  float phi;
  float theta;
  float eta;

  float chi1;
  float chi2;
  float pi0_chi;


  float etow1[220];
  float ch1[220];
  float etow2[220];
  float ch2[220];
  int ntow1;
  int ntow2;
  

  //BbcOut* bbcout;
  //  EventHeader *evtheader;
};

#endif /* __MPCPI0TTREE_H__ */



