#include "Pi0MassFitter.h"
#include "ProcessNonLinTree.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <TFile.h>
#include <TH2D.h>

using namespace std;
using namespace EmcAnaCommon;

void ProcessNonLinTree::Process(const char* fn_iflist, const char* ofilename)
{
   m_ofile = new TFile(ofilename, "RECREATE");

   ReadHistos(fn_iflist);

   for (int ias = 0; ias < N_ARMSECT; ias++) {
      m_h2_pi0mass[ias]->RebinX(2);
//      m_h2_pi0mass[ias]->RebinY(2);
   }

   ostringstream oss;

   int n_mom      = m_h2_pi0mass[0]->GetNbinsX();
   double mom_min = m_h2_pi0mass[0]->GetXaxis()->GetXmin();
   double mom_max = m_h2_pi0mass[0]->GetXaxis()->GetXmax();
//   double mom_step = (mom_max - mom_min) / n_mom;

   TH1* h1_mass_vs_mom[N_ARMSECT];
   for (int ias = 0; ias < N_ARMSECT; ias++) {
      oss.str("");
      oss << "h1_mass_vs_mom_as" << ias;
      m_ofile->cd();
      h1_mass_vs_mom[ias] = new TH1D(oss.str().c_str(), "", n_mom, mom_min, mom_max);
      h1_mass_vs_mom[ias]->Sumw2();
   }

   m_fitter = new Pi0MassFitter();
   m_fitter->SetFitRange(0.08, 0.18);
   for (int ias = 0; ias < N_ARMSECT; ias++) {
      for (int imom = 0; imom < n_mom; imom++) {
         double mom_cent = m_h2_pi0mass[ias]->GetXaxis()->GetBinCenter(imom+1);
         oss.str("");
         oss << "h1_pi0mass_as" << ias << "_mom" << imom;

         TH1* h1 = m_h2_pi0mass[ias]->ProjectionY(oss.str().c_str(), imom+1, imom+1);
         m_fitter->SetHist(h1);
         m_fitter->FitMass();
         Double_t height, height_err;
         Double_t mean, mean_err;
         Double_t sigma, sigma_err;
         Double_t chi2;
         Int_t NDF, poln;
         Double_t signal, bg;
         m_fitter->GetResults(height, height_err, mean, mean_err, 
                              sigma, sigma_err, chi2, NDF, poln, signal, bg);
         if (0.5 < mom_cent && mom_cent < 9) {
//         if (NDF > 0 && mean_err < 0.01 && signal > bg / 5) {
            h1_mass_vs_mom[ias]->SetBinContent(imom+1, mean);
            h1_mass_vs_mom[ias]->SetBinError  (imom+1, mean_err);
         }
      }
   }

   m_ofile->cd();
   m_ofile->Write();
   m_ofile->Close();
}

void ProcessNonLinTree::ReadHistos(const char* fn_iflist)
{
   ostringstream oss;

   for (int ias = 0; ias < N_ARMSECT; ias++) m_h2_pi0mass[ias] = 0;

   ifstream if_treelist(fn_iflist);
   string fn_tree;
   while (if_treelist >> fn_tree) {
      cout << "Reading histos from " << fn_tree << endl;
      TFile* ifile = new TFile(fn_tree.c_str());

      for (int ias = 0; ias < N_ARMSECT; ias++) {
         oss.str("");
         oss << "h2_pi0mass_as" << ias;
         TH2* h2 = (TH2*)ifile->Get(oss.str().c_str());
         if (! m_h2_pi0mass[ias]) {
            m_ofile->cd();
            m_h2_pi0mass[ias] = (TH2*)h2->Clone(oss.str().c_str());
         } else {
            m_h2_pi0mass[ias]->Add(h2);
         }
      }

      delete ifile;
   }
   if_treelist.close();
}
