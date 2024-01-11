#ifndef __TOWERHISTCHECKER_H__
#define __TOWERHISTCHECKER_H__

#include <cstdio>
#include <fstream>
#include <map>
#include <TROOT.h>
#include "EmcAnaCommon.h"

class TObject;
class TFile;
class TH1;
class TFile;
class TTree;
class TChain;

class Pi0MassFitter;
class Coefficient;
class UncalibTowerList;

class ResultParam {
 public:
   Double_t m_height;
   Double_t m_height_err;
   Double_t m_mean;
   Double_t m_mean_err;
   Double_t m_sigma;
   Double_t m_sigma_err;
   Double_t m_chi2;
   Double_t m_signal;
   Double_t m_bg;
   Int_t m_NDF;
   Int_t m_poln;

   ResultParam() {;}
   ~ResultParam() {;}
};

class TowerHistChecker
{
   static const double PT_MIN_PI0    =  0;
   static const double PT_MAX_PI0    = 10; // used as upper edge of histo
   static const int    N_BINS_PT_PI0 = 20;

   static const int    N_BINS_MASS = 200;
   static const double MASS_MIN    = 0.0;
   static const double MASS_MAX    = 0.6;

   static const int GIVE_UP_ZERO_STAT = 1;
   static const int GIVE_UP_FEW_STAT  = 2;
   static const int GIVE_UP_NOISY     = 3;
   static const int GIVE_UP_NO_PEAK   = 4;

   Int_t m_niter;
   Pi0MassFitter* m_fitter;
   Coefficient* m_coef_in;
   Coefficient* m_coef_out;
   UncalibTowerList* m_uncalib_list;
   ResultParam* m_res_par;


   int m_bl_no_eye_check;

   bool omit_uncalib;
   //// variables to hold fit results
   Int_t m_tid;
   Int_t m_as; 
   Int_t m_sm;
   Double_t m_height;
   Double_t m_height_err;
   Double_t m_mean;
   Double_t m_mean_err;
   Double_t m_sigma;
   Double_t m_sigma_err;
   Double_t m_chi2;
   Double_t m_signal;
   Double_t m_bg;
   Int_t m_NDF;
   Int_t m_poln;
   Double_t m_coef;
   Double_t m_new_coef;
   Double_t m_new_coef_err;
   Double_t m_cont_max;

 public:
   TowerHistChecker(Int_t niter);
   virtual ~TowerHistChecker();

 protected:
   void ExecFit(TH1* h1_pi0mass, int bl_by_hand = false);
   void SaveResultOfCurrentAsYZ();
   void SaveResultOfCurrentAsSm();
   void WriteResultTable(char* fname_table_tower);

 public:
   void CheckCalibResult(
      Double_t height_give_up, Double_t height_low, Double_t height_high, 
      Double_t mean_low, Double_t mean_high, 
      Double_t sigma_low, Double_t sigma_high, 
      Double_t rchi2_low, Double_t rchi2_high,
      char* fname_root_for_hist, char* fname_table_tower,
      char* fname_uncalib_list_in, char* fname_uncalib_list_out, 
      char* fname_coef_in, char* fname_coef_out, 
      char* fname_coef_supermod_in, char* fname_coef_supermod_out
      );
   void SetFlagNoEyeCheck(int bl_no_eye_check) { m_bl_no_eye_check = bl_no_eye_check; }
   
};
      
#endif // __TOWERHISTCHECKER_H__
