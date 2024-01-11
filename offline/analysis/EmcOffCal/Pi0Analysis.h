#ifndef PI0ANALYSIS_H__
#define PI0ANALYSIS_H__

#include "EmcAnaCommon.h"

#include <fstream>
#include <map>

class TFile;
class TH1D;
class TH2D;
class TFile;
class TTree;
class TChain;
class TH1F;

class Warnmap;
class RecalEcore;

class UncalibTowerList;

class Pi0Analysis 
{
   static const double PT_MIN_PI0    =  0;
   static const double PT_MAX_PI0    = 10; // used as upper edge of histo
   static const int    N_BINS_PT_PI0 = 20;

   static const int    N_BINS_MASS = 200;
   static const double MASS_MIN    = 0.0;
   static const double MASS_MAX    = 0.6;

   UncalibTowerList* uncalib_list;

   Int_t m_bl_ene_corr;

   Double_t m_nev;
   Double_t m_nev_4x4a;
   Double_t m_nev_4x4b;
   Double_t m_nev_4x4c_bbc_narrow;
   Double_t m_nev_bbc;
   Double_t m_nev_bbc_narrow;
   Double_t m_nev_MPC_2x2;
   Double_t m_nev_novertex;//HG
   Double_t m_nev_4x4b_pure;//HG
   Double_t m_nev_4x4b_e_narrow;//HG

   Double_t m_npi0;
   Double_t m_npi0_4x4a;
   Double_t m_npi0_4x4b;
   Double_t m_npi0_4x4c_bbc_narrow;
   Double_t m_npi0_bbc;
   Double_t m_npi0_bbc_narrow;
   Double_t m_npi0_MPC_2x2;
   Double_t m_npi0_novertex;//HG
   Double_t m_npi0_4x4b_pure;//HG
   Double_t m_npi0_4x4b_e_narrow;//HG

   
   Int_t m_type_by_type;
   Int_t m_sector_by_sector;
   Int_t m_tower_by_tower;
   Int_t m_supermod_by_supermod;

   TFile* m_ofile;
   
   TH1D* m_h1_pi0mass_PbGl[3]; //!
   TH1D* m_h1_pi0mass_PbSc[3]; //!
   TH2D* m_h2_pi0mass_PbGl[3]; //!
   TH2D* m_h2_pi0mass_PbSc[3]; //!
   
   TH1D* m_h1_tof_as[EmcAnaCommon::N_ARMSECT][3]; //!

   TH1D* m_h1_pi0mass_as[EmcAnaCommon::N_ARMSECT][3]; //!
   TH2D* m_h2_pi0mass_as[EmcAnaCommon::N_ARMSECT][3]; //!
   
   TH1D* m_h1_pi0mass_asns[EmcAnaCommon::N_ARMSECT][2][3]; //!

   TH1D* m_h1_pi0mass_tower[EmcAnaCommon::N_ARMSECT][EmcAnaCommon::N_YPOS_PBGL][EmcAnaCommon::N_ZPOS_PBGL][3]; //!

   TH1D* m_h1_pi0mass_supermod[EmcAnaCommon::N_ARMSECT][EmcAnaCommon::N_SUPERMOD][3]; //!

   TH1F* hBBCC_NS;

   //// run-by-run histo
   std::map<int, TH1D*> m_h1_pi0mass_PbGl_run[3]; //!
   std::map<int, TH1D*> m_h1_pi0mass_PbSc_run[3]; //!
   std::map<int, TH1D*> m_h1_pi0mass_as_run[EmcAnaCommon::N_ARMSECT][3]; //!

   Warnmap* m_warnmap; //!
   RecalEcore* m_recal_ecore; //!
   
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain     
   
   static const Int_t N_PHOTON = 1000;
   static const Int_t MULTIPLICITY_TOTAL = 10000;
   
   Int_t   m_run;
//   Int_t   m_evt;
//   Float_t m_bbcz;
//   Float_t m_bbct0;
   
   Int_t m_trig_MPC_2x2;
   Int_t m_trig_4x4c_bbc_narrow;
   Int_t m_trig_4x4a;
   Int_t m_trig_4x4b;
   Int_t m_trig_bbc;
   Int_t m_trig_bbc_narrow;
   Int_t m_trig_novertex;
   Int_t m_trig_4x4b_pure;//HG
   Int_t m_trig_4x4b_e_narrow;//HG

   Float_t m_bbcz;
   Float_t m_bbct0;
   Float_t m_bbccn;
   Float_t m_bbccs;
   
   Int_t   m_n_photon;
   Int_t   m_armsect[N_PHOTON];
   Int_t   m_ypos[N_PHOTON];
   Int_t   m_zpos[N_PHOTON];
   Int_t   m_multiplicity[N_PHOTON];
   Int_t   m_multiplicity_total;
   Int_t   m_towerid[MULTIPLICITY_TOTAL];
   Float_t m_partesum[MULTIPLICITY_TOTAL];
   Float_t m_e[N_PHOTON];
   Float_t m_ecore[N_PHOTON];
   Float_t m_x[N_PHOTON];
   Float_t m_y[N_PHOTON];
   Float_t m_z[N_PHOTON];
   Float_t m_tof[N_PHOTON];
   Float_t m_chi2[N_PHOTON];
      

 public:
   Pi0Analysis(Int_t bl_ene_corr, const char* fname_warnmap, const char* fname_coef, const char* fname_coef_supermod, const char* fname_uncalib_list);
   virtual ~Pi0Analysis();
 protected:
   Int_t SetTree(TTree* tree);
   Int_t GetEntry(Long64_t entry);
   Long64_t LoadTree(Long64_t entry);
   void ProcessTree(TTree* tree, double mom_min_target_pbsc, double mom_min_target_pbgl, double mom_min_pair_pbsc, double mom_min_pair_pbgl, double pT_min_pair_pbsc, double pT_min_pair_pbgl, double frac_energy_in_target, double tof_min, double tof_max,double bbc_max_chargesum);
 public:
   void InitMassHist(const char* fname_root_for_hist, Int_t type_by_type, Int_t sector_by_sector, Int_t tower_by_tower);
   void MakeMassHist(const char* fname, double mom_min_target_pbsc, double mom_min_target_pbgl, double mom_min_pair_pbsc, double mom_min_pair_pbgl, double pT_min_pair_pbsc, double pt_min_pair_pbgl, double frac_energy_in_target, double tof_min, double tof_max,double bbc_max_chargesum);
  void AnalysisRunByRun();
  void Analysis(const char* fname_table_type, const char* fname_table_sector, const char* fname_table_tower);

};
      
#endif // PI0ANALYSIS_H__
