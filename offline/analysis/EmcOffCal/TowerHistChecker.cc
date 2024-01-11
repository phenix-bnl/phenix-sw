#include "TowerHistChecker.h"
#include "Coefficient.h"
#include "Pi0MassFitter.h"
#include "UncalibTowerList.h"

#include <TFile.h>
#include <TH1D.h>
#include <TH2D.h>
#include <TTree.h>
#include <TChain.h>
#include <TCanvas.h>

#include <TSystem.h>
#include <TStyle.h>
#include <TVector3.h>

#include <cassert>
#include <cstdio>
#include <iostream>
#include <fstream>

using namespace std;
using namespace EmcAnaCommon;

TowerHistChecker::TowerHistChecker(Int_t niter)
{
   m_niter = niter;
   if(niter<2){
     omit_uncalib=false;
   }else{
     omit_uncalib=true;
   }

   m_fitter = new Pi0MassFitter();
   m_coef_in  = new Coefficient();
   m_coef_out = new Coefficient();
   m_uncalib_list = new UncalibTowerList();
   m_res_par = new ResultParam[N_TOWER];

   m_bl_no_eye_check = false;

   cout<<"Created TowerHistChecker."<<endl;
}

TowerHistChecker::~TowerHistChecker()
{
   delete m_fitter;
   delete m_coef_in;
   delete m_coef_out;
   delete m_uncalib_list;
   delete [] m_res_par;
}

void TowerHistChecker::CheckCalibResult(
   Double_t cont_max_give_up, Double_t height_low, Double_t height_high,
   Double_t mean_low, Double_t mean_high, 
   Double_t sigma_low, Double_t sigma_high, 
   Double_t rchi2_low, Double_t rchi2_high,
   char* fname_root_for_hist, char* fname_table_tower,
   char* fname_uncalib_list_in, char* fname_uncalib_list_out, 
   char* fname_coef_in, char* fname_coef_out,
   char* fname_coef_supermod_in, char* fname_coef_supermod_out
   )
{
   TFile* file_hist = new TFile(fname_root_for_hist, "UPDATE");

   m_coef_in->ReadCoef(fname_coef_in, fname_coef_supermod_in);
   m_uncalib_list->ReadUncalibList(fname_uncalib_list_in);

   TCanvas* c_hist = new TCanvas("c_hist");
   extern TStyle* gStyle;
   gStyle->SetOptFit(kTRUE);
   ////////supermod
   for (int i_as = 0; i_as < N_ARMSECT; i_as++) {
     for(int i_sm = 0; i_sm < N_SUPERMOD; i_sm++){
       if(!IsValidSM(i_as,i_sm)){continue;}
       m_as = i_as;
       m_sm = i_sm;
       m_coef = m_coef_in->GetCoefSm(i_as, i_sm);
       
       TH1D* h1_pi0mass = 0;
       char hname[128];
       sprintf(hname, "h1_pi0mass_as%ism%.2i_v2", i_as, i_sm);
       h1_pi0mass = (TH1D*)file_hist->Get(hname);
       
       m_fitter->SetHist(h1_pi0mass);
       m_fitter->UseResultsStoredInHist();
       m_fitter->GetResults(m_height, m_height_err, m_mean, m_mean_err, 
			    m_sigma, m_sigma_err, 
			    m_chi2, m_NDF, m_poln, m_signal, m_bg);
       m_cont_max = m_fitter->GetContentMax();
       m_new_coef = CalcNewCoef(m_coef, m_mean);
       m_new_coef_err = CalcNewCoefErr(m_new_coef, m_mean, m_mean_err);
       
       
       // give up
       if (m_NDF <= 0 ) {
         m_new_coef = 1;
	 m_new_coef_err = 0.0;
         cout << endl << "Sector " << i_as << " supermod " << i_sm << " ... give up due to few stat (no NDF)." << endl;
	 
         SaveResultOfCurrentAsSm();
         continue;
       }
       
       // height check and rebin if needed
       const int N_rebin = 3;
       int i_rebin = 0;
       while ((m_cont_max < cont_max_give_up || m_height <= height_low) && 
	      i_rebin < N_rebin) {
         h1_pi0mass->Rebin();
         ExecFit(h1_pi0mass);
         i_rebin++;
       }
       if (m_cont_max < cont_max_give_up) {
         m_new_coef = 1;
	 m_new_coef_err = 0.0;
	 
         cout << endl << "Sector " << i_as << "  supermod " << i_sm << " ... give up due to few stat." << endl;
         SaveResultOfCurrentAsSm();
         continue;
       }
       
       int bl_need_check = 0;
       if (m_height <= height_low) {
	 bl_need_check |= 0x8;
       }
       if (m_mean <= mean_low || m_mean >= mean_high)
         bl_need_check |= 0x4;
       if (m_sigma <= sigma_low || m_sigma >= sigma_high)
         bl_need_check |= 0x2;
       if (m_chi2/m_NDF <= rchi2_low || m_chi2/m_NDF >= rchi2_high)
         bl_need_check |= 0x1;
       
       if(bl_need_check){
	 m_new_coef = 1;
	 m_new_coef_err = 0.0;	
       }
       
       // eye check
       if ((!m_bl_no_eye_check) && bl_need_check) {
         h1_pi0mass->Draw();
         c_hist->Update();
         cout << endl << "Sector " << i_as << " supermod " << i_sm << ": coef = " << m_coef 
              << ", check status = 0x" << hex << bl_need_check << dec << endl;
	 
         Int_t bl_repeat = true;
         //Int_t bl_repeat = false;
	 cout << "SM needs eyecheck +1" <<endl;
         while (bl_repeat) {
	   cout << "Action? ";
	   string line;
	   getline(cin, line);
	   switch (line[0]) {
            case 'o': // ok
	      bl_repeat = false;
	      break;
	   case 'r':
	     h1_pi0mass->Rebin();
	     ExecFit(h1_pi0mass);
	     c_hist->Update();
	     break;
	   case 'f': // fit by hand
	     ExecFit(h1_pi0mass, true);
	     c_hist->Update();
	     break;
	   case 's': // give up due to few stat.
	     m_new_coef = 1;
	     m_new_coef_err = 0.0;
	     bl_repeat = false;
	     break;
	   case 'n': // give up due to noisiness
	     m_new_coef = 1;
	     m_new_coef_err = 0.0;
	     bl_repeat = false;
	     break;
	   case 'p': // give up due to no peak
	     m_new_coef = 1;
	     m_new_coef_err = 0.0;
	     bl_repeat = false;
	     break;
	   case 'u': // update canvas
	     c_hist->Update();
	     break;
	   case '\n':
	     break;
	   default:
	     cout << "INPUT AGAIN" << endl 
		  << "  o = OK" << endl
		  << "  r = rebin and fit" << endl
		  << "  f = fit by hand" << endl
		  << "  s = give up due to few stat" << endl
		  << "  n = give up due to nois" << endl
		  << "  p = give up due to no peak" << endl
		  << "  u = update canvas" << endl;
	   }
         }
       }
       SaveResultOfCurrentAsSm();
     }
   }

   ////////supermod
   ////tower

   TFile* fTestHeight = new TFile("testheight.root","RECREATE");
   TH1F* hHeightWide = new TH1F("hHeightWide","hHeightWide",20000,(double)0.,(double)10000000.);
   TH1F* hHeightZoom = new TH1F("hHeightZoom","hHeightZoom",20000,(double)0.,(double)500000.);
   int ntowerstocheck=0;
   for (m_tid = 0; m_tid < N_TOWER; m_tid++) {
     int as, y, z;
     TowerID2AsYZ(m_tid, as, y, z);
     m_coef = m_coef_in->GetCoef(as, y, z);
     
     TH1D* h1_pi0mass = 0;
     char hname[128];
      sprintf(hname, "h1_pi0mass_as%iy%.2iz%.2i_v2", as, y, z);
      h1_pi0mass = (TH1D*)file_hist->Get(hname);

      m_fitter->SetHist(h1_pi0mass);
      m_fitter->UseResultsStoredInHist();
      m_fitter->GetResults(m_height, m_height_err, m_mean, m_mean_err, 
                           m_sigma, m_sigma_err, 
                           m_chi2, m_NDF, m_poln, m_signal, m_bg);
      m_cont_max = m_fitter->GetContentMax();
      m_new_coef = CalcNewCoef(m_coef, m_mean);
      m_new_coef_err = CalcNewCoefErr(m_new_coef, m_mean, m_mean_err);

      // omit edge towers and uncalib towers
      if (IsEdgePos(as, y, z) || 
          (m_uncalib_list->IsInUncalibList(AsYZ2TowerID(as, y, z))&&omit_uncalib)) {
         m_new_coef = -999.0;
	 m_new_coef_err = 0.0;
         SaveResultOfCurrentAsYZ();
         continue;
      }

      // give up
      if (m_NDF <= 0 ) {
         m_new_coef = -999.0;
	 m_new_coef_err = 0.0;
         m_uncalib_list->AddUncalibList(AsYZ2TowerID(as, y, z), 
                                        UncalibTowerList::ZERO_STAT);
         cout << endl << "Tower as" << as << " y" << y << " z" << z 
              << " ... give up due to few stat (no NDF)." << endl;

         SaveResultOfCurrentAsYZ();
         continue;
      }

	hHeightWide->Fill(m_cont_max);
	hHeightZoom->Fill(m_cont_max);
      // height check and rebin if needed
      const int N_rebin = 3;
      int i_rebin = 0;
      while ((m_cont_max < cont_max_give_up || m_height <= height_low) && 
             i_rebin < N_rebin) {
         h1_pi0mass->Rebin();
         ExecFit(h1_pi0mass);
         i_rebin++;
      }
      if (m_cont_max < cont_max_give_up) {
         m_new_coef = -999.0;
	 m_new_coef_err = 0.0;
         m_uncalib_list->AddUncalibList(as, y, z, 
                                        UncalibTowerList::FEW_STAT);
         cout << endl << "Tower as" << as << " y" << y << " z" << z 
              << " ... give up due to few stat." << endl;
         SaveResultOfCurrentAsYZ();
         continue;
      }

      int bl_need_check = 0;
      if (m_height <= height_low || m_height >= height_high) {
         if (m_height / m_cont_max < 0.6 || m_height / m_cont_max > 1.0)
            bl_need_check |= 0x8;
      }
      if (m_mean <= mean_low || m_mean >= mean_high)
         bl_need_check |= 0x4;
      if (m_sigma <= sigma_low || m_sigma >= sigma_high)
         bl_need_check |= 0x2;
      if (m_chi2/m_NDF <= rchi2_low || m_chi2/m_NDF >= rchi2_high)
         bl_need_check |= 0x1;

      //      if(bl_need_check){
      //m_new_coef = -999.0;
      //m_new_coef_err = 0.0;
      //m_uncalib_list->AddUncalibList(as, y, z, 
      //			       UncalibTowerList::NO_EYE);
      //}

      // eye check
      if ((!m_bl_no_eye_check) && bl_need_check) {
         h1_pi0mass->Draw();
         c_hist->Update();
         cout << endl << "Tower as" << as << " y" << y << " z" << z
              << ": coef = " << m_coef 
              << ", check status = 0x" << hex << bl_need_check << dec << endl;

         Int_t bl_repeat = true;
         //Int_t bl_repeat = false;
	 cout << "Tower needs eyecheck +1" <<endl;
   	 ntowerstocheck++;
         while (bl_repeat) {
            cout << "Action? ";
            string line;
            getline(cin, line);
            switch (line[0]) {
            case 'o': // ok
               bl_repeat = false;
               break;
            case 'r':
               h1_pi0mass->Rebin();
               ExecFit(h1_pi0mass);
               c_hist->Update();
               break;
            case 'f': // fit by hand
               ExecFit(h1_pi0mass, true);
               c_hist->Update();
               break;
            case 's': // give up due to few stat.
               m_new_coef = -999.0;
	       m_new_coef_err = 0.0;
               m_uncalib_list->AddUncalibList(as, y, z, 
                                              UncalibTowerList::FEW_STAT);
               bl_repeat = false;
               break;
            case 'n': // give up due to noisiness
               m_new_coef = -999.0;
	       m_new_coef_err = 0.0;
               m_uncalib_list->AddUncalibList(as, y, z, 
                                              UncalibTowerList::NOISY);
               bl_repeat = false;
               break;
            case 'p': // give up due to no peak
               m_new_coef = -999.0;
	       m_new_coef_err = 0.0;
               m_uncalib_list->AddUncalibList(as, y, z, 
                                              UncalibTowerList::NO_PEAK);
               bl_repeat = false;
               break;
	    case 'w': // add to warn map
               m_new_coef = -999.0;
	       m_new_coef_err = 0.0;
               m_uncalib_list->AddUncalibList(as, y, z, 
                                              UncalibTowerList::WARNED);
               bl_repeat = false;
               break;
            case 'u': // update canvas
               c_hist->Update();
               break;
            case '\n':
               break;
            default:
               cout << "INPUT AGAIN" << endl 
                    << "  o = OK" << endl
                    << "  r = rebin and fit" << endl
                    << "  f = fit by hand" << endl
                    << "  s = give up due to few stat" << endl
                    << "  n = give up due to nois" << endl
                    << "  p = give up due to no peak" << endl
		    << "  w = add to warn map" <<endl
                    << "  u = update canvas" << endl;
            }
         }
      }
      SaveResultOfCurrentAsYZ();
   }
   hHeightWide->Write();
   hHeightZoom->Write();
   fTestHeight->Close();
   cout << "Towers to be inspected manually: " <<ntowerstocheck << endl;

   m_coef_out->WriteCoef(fname_coef_out,fname_coef_supermod_out);
   WriteResultTable(fname_table_tower);

   delete c_hist;

   file_hist->Close();

   m_uncalib_list->WriteUncalibList(fname_uncalib_list_out);
}

void TowerHistChecker::ExecFit(TH1* h1_pi0mass, int bl_by_hand)
{
   m_fitter->SetHist(h1_pi0mass);
   if (bl_by_hand) {
     m_fitter->FitMassByHand();
   } else{
     if(m_niter==0){
       m_fitter->FitMass(true);
     } else {
       m_fitter->FitMass(false);
     }
   }
   m_fitter->GetResults(m_height, m_height_err, m_mean, m_mean_err, 
                  m_sigma, m_sigma_err, m_chi2, m_NDF, m_poln, m_signal, m_bg);
   m_cont_max = m_fitter->GetContentMax();
   m_new_coef = CalcNewCoef(m_coef, m_mean);
   m_new_coef_err = CalcNewCoefErr(m_new_coef, m_mean, m_mean_err);

}

void TowerHistChecker::SaveResultOfCurrentAsYZ()
{
   m_res_par[m_tid].m_height     = m_height;
   m_res_par[m_tid].m_height_err = m_height_err;
   m_res_par[m_tid].m_mean       = m_mean;
   m_res_par[m_tid].m_mean_err   = m_mean_err;
   m_res_par[m_tid].m_sigma      = m_sigma;
   m_res_par[m_tid].m_sigma_err  = m_sigma_err;
   m_res_par[m_tid].m_chi2       = m_chi2;
   m_res_par[m_tid].m_signal     = m_signal;
   m_res_par[m_tid].m_bg         = m_bg;
   m_res_par[m_tid].m_NDF        = m_NDF;
   m_res_par[m_tid].m_poln       = m_poln;

   m_coef_out->SetCoef(m_tid, m_new_coef, m_new_coef_err);
}

void TowerHistChecker::SaveResultOfCurrentAsSm()
{
  m_coef_out->SetCoefSm(m_as, m_sm, m_new_coef, m_new_coef_err);
}

void TowerHistChecker::WriteResultTable(char* fname_table_tower)
{
   FILE* of_table = fopen(fname_table_tower, "w");
   assert(of_table);
   for (int i = 0; i < N_TOWER; i++) {
      int as, y, z;
      TowerID2AsYZ(i, as, y, z);
      fprintf(of_table,
              "%i %2i %2i  %f %f %f  %f %f  %f %f  %f %f  %f %2i  %i  %f  %f\n",
              as, y, z, m_coef_in->GetCoef(i), m_coef_out->GetCoef(i), m_coef_out->GetCoefErr(i),
              m_res_par[i].m_height,
              m_res_par[i].m_height_err,
              m_res_par[i].m_mean,
              m_res_par[i].m_mean_err,
              m_res_par[i].m_sigma,
              m_res_par[i].m_sigma_err,
              m_res_par[i].m_chi2,
              m_res_par[i].m_NDF,
              m_res_par[i].m_poln,
              m_res_par[i].m_signal,
              m_res_par[i].m_bg
         );
   }
   fclose(of_table);
}
