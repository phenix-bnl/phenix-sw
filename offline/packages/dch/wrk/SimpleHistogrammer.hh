#ifndef _SIMPLEHISTOGRAMMER_HH
#define _SIMPLEHISTOGRAMMER_HH
/*ROOT Object header files*/
#include <TCanvas.h>
#include <TFrame.h>
#include <TNtuple.h>
#include <TF1.h>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <TString.h>
#include <TFile.h>
#include "PHCompositeNode.h"
#define MAXN 4000
class  DchTrack;
class  dPadClusterWrapper;
class  dTofReconstructedWrapper;
class  dEmcClusterLocalExtWrapper;
class  CglTrack;
class  PHTrackOut;
class  McRecoSingleMicro_v1;
class  McEvalSingleList_v1;
class  primaryWrapper;
class  utiMatch;
class  utiHadPid;
class  BbcOut;
class SimpleHistogrammer {
private:
  static SimpleHistogrammer* _instance;
  SimpleHistogrammer();
public:
   ~SimpleHistogrammer();
  static SimpleHistogrammer* instance();
  bool setFileName(TString name);
  TString     filename;
  short        flagInit;
  TFile        *Datafile;
  int          verbose;

  //my stuff
  TString     highptdsteast;
  TString     highptdstwest;

protected:
  TList *     ntupleList;
  TList *     histoList;
public:
  int count;
  TFile*   getFile()          {return Datafile;}
  int      getVerbose()        {return verbose;}
  void     setVerbose(int val) {verbose = val;}
public:
  int      initializeFile(char* name=0);
  int      saveToFile(int option=0);
  int      addNtuple(TNtuple* val);
  int      addHistogram(TH1*histo);
  void     flush();
  //my stuff
  void     fillCompleteEvaluation(PHCompositeNode* topNode);



  void     setup(PHCompositeNode* topNode);
  void     Fill();
  TNtuple *single;
  TNtuple *primary;
  utiMatch *mat;
  utiHadPid * Pid;
  DchTrack* d_dctrk;
  dPadClusterWrapper*d_pc2,*d_pc3;
  dTofReconstructedWrapper*d_tof;
  dEmcClusterLocalExtWrapper*d_emc;
  CglTrack* d_cgl;
  PHTrackOut* d_proj;
  McEvalSingleList_v1       * mcsingle;
  primaryWrapper* d_primary;
  BbcOut*  d_bbc; 
  int run;
  int seq;
  int dowest,doeast;
  int changematch;
  float momcut;
  //storage
  float         bbcz;
  Short_t       McTrk_EVENTID[MAXN];             
  Short_t       McTrk_NRECO[MAXN];               
  Short_t       McTrk_RECOID1[MAXN];             
  Short_t       McTrk_RECOID2[MAXN];             
  Short_t       McTrk_RECOID3[MAXN];             
  Short_t       McTrk_MCTRACKID[MAXN];           
  Short_t       McTrk_GENERATION[MAXN];          
  Short_t       McTrk_PARTICLEID[MAXN];          
  Short_t       McTrk_PARENTID[MAXN];            
  Short_t       McTrk_PRIMARYID[MAXN];           
  Float_t       McTrk_VERTEXX[MAXN];             
  Float_t       McTrk_PARENTVERTEXX[MAXN];       
  Float_t       McTrk_PRIMARYVERTEXX[MAXN];      
  Float_t       McTrk_VERTEXY[MAXN];             
  Float_t       McTrk_PARENTVERTEXY[MAXN];       
  Float_t       McTrk_PRIMARYVERTEXY[MAXN];      
  Float_t       McTrk_VERTEXZ[MAXN];             
  Float_t       McTrk_PARENTVERTEXZ[MAXN];       
  Float_t       McTrk_PRIMARYVERTEXZ[MAXN];      
  Float_t       McTrk_MOMENTUMX[MAXN];           
  Float_t       McTrk_PARENTMOMENTUMX[MAXN];     
  Float_t       McTrk_PRIMARYMOMENTUMX[MAXN];    
  Float_t       McTrk_MOMENTUMY[MAXN];           
  Float_t       McTrk_PARENTMOMENTUMY[MAXN];     
  Float_t       McTrk_PRIMARYMOMENTUMY[MAXN];    
  Float_t       McTrk_MOMENTUMZ[MAXN];           
  Float_t       McTrk_PARENTMOMENTUMZ[MAXN];     
  Float_t       McTrk_PRIMARYMOMENTUMZ[MAXN];    
  Short_t       McTrk_QUALITY[MAXN];             
  Float_t       McTrk_THETA0[MAXN];              
  Float_t       McTrk_PHI0[MAXN];                
  Float_t       McTrk_PHI[MAXN];                 
  Float_t       McTrk_ALPHA[MAXN];               
  Float_t       McTrk_ZED[MAXN];                 
  Float_t       McTrk_BETA[MAXN];                   
  

  Short_t         McRecoTrk_EVENTID[MAXN];
  Short_t         McRecoTrk_MCINDEX[MAXN];
  Short_t         McRecoTrk_RECOID[MAXN]; 
  Short_t         McRecoTrk_QUALITY[MAXN];
  Float_t         McRecoTrk_MOMENTUM[MAXN];
  Float_t         McRecoTrk_THETA0[MAXN]; 
  Float_t         McRecoTrk_PHI0[MAXN];   
  Float_t         McRecoTrk_PHI[MAXN];    
  Float_t         McRecoTrk_ALPHA[MAXN];  
  Float_t         McRecoTrk_ZED[MAXN];    
  Float_t         McRecoTrk_BETA[MAXN];   
  Float_t         McRecoTrk_AVERAGETIME[MAXN];
  Short_t         McRecoTrk_XHITS[MAXN];  
  Short_t         McRecoTrk_UVHITS[MAXN]; 
  Short_t         McRecoTrk_MULMAIN[MAXN];
  Short_t         McRecoTrk_MULXMAIN[MAXN];
  Short_t         McRecoTrk_MULUVMAIN[MAXN];
  Short_t         McRecoTrk_MAIN[MAXN];   
  Short_t         McRecoTrk_XMAIN[MAXN];  
  Short_t         McRecoTrk_UVMAIN[MAXN]; 
  Short_t         McRecoTrk_AMBIGUITY[MAXN];
  Float_t         McRecoTrk_PURITY[MAXN]; 
  Float_t         McRecoTrk_XPURITY[MAXN];
  Float_t         McRecoTrk_UVPURITY[MAXN];
  Short_t         McRecoTrk_PC1CLUSID[MAXN];
  Short_t         McRecoTrk_PC2CLUSID[MAXN];
  Short_t         McRecoTrk_PC3CLUSID[MAXN];
  Short_t         McRecoTrk_PC1CLUSIDTRUE[MAXN];
  Short_t         McRecoTrk_PC2CLUSIDTRUE[MAXN];
  Short_t         McRecoTrk_PC3CLUSIDTRUE[MAXN];
  Short_t         McRecoTrk_PC1CLUSIDG[MAXN];
  Short_t         McRecoTrk_PC2CLUSIDG[MAXN];
  Short_t         McRecoTrk_PC3CLUSIDG[MAXN];
  Float_t         McRecoTrk_PC1POINTXG[MAXN];
  Float_t         McRecoTrk_PC2POINTXG[MAXN];
  Float_t         McRecoTrk_PC3POINTXG[MAXN];
  Float_t         McRecoTrk_PC1POINTYG[MAXN];
  Float_t         McRecoTrk_PC2POINTYG[MAXN];
  Float_t         McRecoTrk_PC3POINTYG[MAXN];
  Float_t         McRecoTrk_PC1POINTZG[MAXN];
  Float_t         McRecoTrk_PC2POINTZG[MAXN];
  Float_t         McRecoTrk_PC3POINTZG[MAXN];
  Short_t         McRecoTrk_TOFID[MAXN];  
  Short_t         McRecoTrk_TOFIDTRUE[MAXN];
  Short_t         McRecoTrk_TOFIDG[MAXN]; 
  Float_t         McRecoTrk_TOFPOINTXG[MAXN];
  Float_t         McRecoTrk_TOFPOINTYG[MAXN];
  Float_t         McRecoTrk_TOFPOINTZG[MAXN];
  Float_t         McRecoTrk_TOFG[MAXN];   
  Float_t         McRecoTrk_TOFELOSSG[MAXN];
  Short_t         McRecoTrk_EMCCLUSID[MAXN];
  Short_t         McRecoTrk_EMCCLUSIDTRUE[MAXN];
  Short_t         McRecoTrk_EMCCLUSIDG[MAXN];
  Short_t         McRecoTrk_EMCANCTRK0[MAXN];
  Short_t         McRecoTrk_EMCANCTRK1[MAXN];
  Short_t         McRecoTrk_EMCANCTRK2[MAXN];
  Short_t         McRecoTrk_EMCANCTWRHIT0[MAXN];
  Short_t         McRecoTrk_EMCANCTWRHIT1[MAXN];
  Short_t         McRecoTrk_EMCANCTWRHIT2[MAXN];
  Short_t         McRecoTrk_EMCANCPID0[MAXN];
  Short_t         McRecoTrk_EMCANCPID1[MAXN];
  Short_t         McRecoTrk_EMCANCPID2[MAXN];
  Float_t         McRecoTrk_EMCANCEDEP0[MAXN];
  Float_t         McRecoTrk_EMCANCEDEP1[MAXN];
  Float_t         McRecoTrk_EMCANCEDEP2[MAXN];
  Float_t         McRecoTrk_EMCANCPTOT0[MAXN];
  Float_t         McRecoTrk_EMCANCPTOT1[MAXN];
  Float_t         McRecoTrk_EMCANCPTOT2[MAXN];
  Float_t         McRecoTrk_EMCPOINTXG[MAXN];
  Float_t         McRecoTrk_EMCPOINTYG[MAXN];
  Float_t         McRecoTrk_EMCPOINTZG[MAXN];
  Float_t         McRecoTrk_EMCEFRACG[MAXN];
  Float_t         McRecoTrk_EMCECOREG[MAXN];
  Float_t         McRecoTrk_EMCMEASEG[MAXN];
  Float_t         McRecoTrk_EMCTOFG[MAXN];
  Short_t         McRecoTrk_CRKACC[MAXN]; 
  Short_t         McRecoTrk_CRKNPMT0[MAXN];
  Short_t         McRecoTrk_CRKNPMT1[MAXN];
  Short_t         McRecoTrk_CRKNPMT3[MAXN];
  Float_t         McRecoTrk_CRKNPE0[MAXN];
  Float_t         McRecoTrk_CRKNPE1[MAXN];
  Float_t         McRecoTrk_CRKNPE3[MAXN];
  Float_t         McRecoTrk_CRKCHI2[MAXN];
  Float_t         McRecoTrk_CRKDISP[MAXN];
  Float_t         McRecoTrk_CRKPATH[MAXN];              


  short  Central_dcarm[MAXN];   
  short  Central_charge[MAXN];   
  short  Central_quality[MAXN];  
  float  Central_zed[MAXN];      
  float  Central_phi[MAXN];      
  float  Central_alpha[MAXN];    
  float  Central_alpha1[MAXN];   
  float  Central_alpha2[MAXN];   
  float  Central_beta[MAXN];     
  float  Central_phi0[MAXN];     
  float  Central_the0[MAXN];   
  float  Central_momentum[MAXN]; 
  short  Central_nx1hits[MAXN];  
  short  Central_nx2hits[MAXN];  
  float  Central_dist1[MAXN];    
  float  Central_dist2[MAXN];    
  float  Central_ppc2x[MAXN];    
  float  Central_ppc2y[MAXN];    
  float  Central_ppc2z[MAXN];    
  float  Central_ppc3x[MAXN];    
  float  Central_ppc3y[MAXN];    
  float  Central_ppc3z[MAXN];    
  float  Central_pemcx[MAXN];    
  float  Central_pemcy[MAXN];    
  float  Central_pemcz[MAXN];   
  float  Central_ptofx[MAXN];    
  float  Central_ptofy[MAXN];    
  float  Central_ptofz[MAXN];    
  short  Central_sect[MAXN];
  float  Central_temc[MAXN];
  float  Central_ecorr[MAXN];
  float  Central_ecore[MAXN];
  float  Central_prob[MAXN];
  float  Central_emcchi2[MAXN]; 
  short  Central_n0[MAXN];   
  float  Central_npe0[MAXN]; 
  short  Central_n1[MAXN];   
  float  Central_npe1[MAXN]; 
  float  Central_tcrk[MAXN]; 
  float  Central_disp[MAXN]; 
  float  Central_pc2sdphi[MAXN];
  float  Central_pc2sdz[MAXN];
  float  Central_pc3sdphi[MAXN];
  float  Central_pc3sdz[MAXN];
  float  Central_emcsdphi[MAXN];
  float  Central_emcsdz[MAXN];
  float  Central_tofsdphi[MAXN];
  float  Central_tofsdz[MAXN];
  float  Central_toftof[MAXN];
  float  Central_m2[MAXN];
  float  Central_emcsdphi_e[MAXN];
  float  Central_emcsdz_e[MAXN];
  float  Central_plemc[MAXN];
  float  Central_pltof[MAXN];
  float  Central_eloss[MAXN];
  float  Central_tofz[MAXN];
  float  Central_tofphi[MAXN];
  float  Central_slat[MAXN];
  float  Central_isPi[MAXN];
  float  Central_isK[MAXN];
  float  Central_isP[MAXN];
};
#endif
