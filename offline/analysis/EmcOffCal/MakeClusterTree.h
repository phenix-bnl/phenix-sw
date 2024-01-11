#ifndef __CALIBTREE_H__
#define __CALIBTREE_H__

#include "EmcAnaCommon.h"

#include <SubsysReco.h>

class TObject;
class PHCompositeNode;
class TTree;
class TFile;
class TString;
class TBuffer;
class Warnmap;

class MakeClusterTree: public SubsysReco
{
  ////
  //// variables for tree
  ////
  static const Int_t max_n_photon = 1000;
  static const Int_t max_multiplicity_total = 10000;
  static const Int_t max_n_track = 1000;  

  //// PHGlobal
  Int_t d_run;
  Int_t d_evt;
  Int_t d_cross;
  Float_t d_bbcz;
  Float_t d_bbct0;

  Int_t d_trigscaled;
  Int_t d_trig_MPC_2x2;
  Int_t d_trig_4x4c_bbc_narrow;
  Int_t d_trig_4x4a;
  Int_t d_trig_4x4b;
  Int_t d_trig_bbc;
  Int_t d_trig_bbc_narrow;
  Int_t d_trig_novertex;

  //// emcClusterContainer
  //photon
  Int_t d_n_photon;
  Int_t d_armsect[max_n_photon];
  Int_t d_ypos[max_n_photon];
  Int_t d_zpos[max_n_photon];
  Int_t d_multiplicity[max_n_photon];
  Int_t d_multiplicity_total;
  Int_t d_towerid[max_multiplicity_total];
  Float_t d_partesum[max_multiplicity_total];
  Float_t d_e[max_n_photon];
  Float_t d_ecore[max_n_photon];
  Float_t d_ecent[max_n_photon];
  Float_t d_prob[max_n_photon];
  Float_t d_chi2[max_n_photon];
  Float_t d_x[max_n_photon];
  Float_t d_y[max_n_photon];
  Float_t d_z[max_n_photon];
  Float_t d_pc3dphi[max_n_photon];
  Float_t d_pc3dz[max_n_photon];
  Float_t d_tof[max_n_photon];
  Float_t d_rawtdc[max_n_photon];

  //tracked particle
  /*Int_t t_n_trkpart;*/

  /* Int_t t_dcarm[max_n_track]; */
/*   Int_t t_sect[max_n_track]; */
/*   Int_t t_ysect[max_n_track]; */
/*   Int_t t_zsect[max_n_track]; */

/*   Int_t t_erthit[max_n_track]; */

/*   Float_t t_pemcx[max_n_track]; */
/*   Float_t t_pemcy[max_n_track]; */
/*   Float_t t_pemcz[max_n_track]; */

/*   Float_t t_dcphi[max_n_track]; */
/*   Float_t t_zed[max_n_track]; */

/*   Float_t t_ppc1x[max_n_track]; */
/*   Float_t t_ppc1y[max_n_track]; */
/*   Float_t t_ppc1z[max_n_track]; */

/*   Float_t t_ppc3x[max_n_track]; */
/*   Float_t t_ppc3y[max_n_track]; */
/*   Float_t t_ppc3z[max_n_track]; */

/*   Float_t t_mom[max_n_track]; */
/*   Float_t t_ecore[max_n_track]; */
/*   Float_t t_pT[max_n_track]; */
/*   Float_t t_alpha[max_n_track]; */
/*   Float_t t_the0[max_n_track]; */
/*   Float_t t_phi0[max_n_track]; */
/*   Float_t t_beta[max_n_track]; */
  
/*   Int_t t_charge[max_n_track]; */
/*   Int_t t_quality[max_n_track]; */
/*   Float_t t_n0[max_n_track]; */
/*   Float_t t_npe0[max_n_track]; */
/*   Float_t t_n1[max_n_track]; */
/*   Float_t t_chi2[max_n_track]; */
/*   Float_t t_dep[max_n_track]; */
/*   Float_t t_disp[max_n_track]; */
/*   Float_t t_prob[max_n_track]; */

/*   Float_t t_emcdphi[max_n_track]; */
/*   Float_t t_emcdz[max_n_track]; */

/*   Float_t t_pc3dphi[max_n_track]; */
/*   Float_t t_pc3dz[max_n_track]; */

  //Float_t t_emcsdphi[max_n_track];
  //Float_t t_emcsdz[max_n_track];
  //Float_t t_emcsdphi_e[max_n_track];
  //Float_t t_emcsdz_e[max_n_track];

  //Int_t t_flag[max_n_track];
  
  int    m_bl_apply_warnmap;
  int    m_bl_with_partesum_etc;
  int    m_run_target;
  double m_mom_cut;
  double MIN_CUT;
  float  m_nevt_max;
  float  m_nevt_processed;

 public:
  MakeClusterTree(
     const int run, const char* ofilename, const char* fname_warnmap,
     const float nevt_max, const double mom_cut, const double trk_cut,
     const int bl_apply_warnmap, const int bl_with_partesum_etc,
     const char *name = "CALIBTREE");
  virtual ~MakeClusterTree() {;}

  int End(PHCompositeNode *topNode); // called at EndRun
  int Init(PHCompositeNode *topNode); // Initialization at startup - create histos here
  int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
  int process_event(PHCompositeNode *topNode); // your analysis code goes here
  int Reset(PHCompositeNode *topNode); // called when new run opened (experimental)
  int ResetEvent(PHCompositeNode *topNode); // called at end of each event (for cleanup)
  void Print(const std::string&) const { return; }

  int SectToSM(int dcarm, int sect, int ysect, int zsect);

 protected:
  TTree* m_tree;
  //TTree* trk_tree;
  TFile* m_file;
  TString m_ofilename;

  Warnmap* m_warnmap;

};

#endif
