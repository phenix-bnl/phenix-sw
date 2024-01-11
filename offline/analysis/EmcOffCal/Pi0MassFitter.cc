#include "Pi0MassFitter.h"

#include <cstdio>
#include <iostream>
#include <TH1D.h>
#include <TH2D.h>
#include <TF1.h>

using namespace std;

Pi0MassFitter::Pi0MassFitter()
{
   m_h1_mass = 0;
   m_bl_own_hist = false;

   m_sigma_lower = 0.005;
   m_sigma_upper = 0.1;

   m_fitrange_lower = 0.075;
   m_fitrange_upper = 0.25;
}

Pi0MassFitter::~Pi0MassFitter()
{
   if (m_bl_own_hist && m_h1_mass) delete m_h1_mass;
}

void Pi0MassFitter::SetHist(TH1* h1)
{
   if (m_bl_own_hist && m_h1_mass) delete m_h1_mass;
   m_h1_mass = h1;
   m_bl_own_hist = false;

   m_M_Nbin    = m_h1_mass->GetNbinsX();
   m_Mmin_hist = m_h1_mass->GetXaxis()->GetXmin();
   m_Mmax_hist = m_h1_mass->GetXaxis()->GetXmax();
}

void Pi0MassFitter::SetHist(TH2* h2, int bin_low, int bin_high)
{
   // assume that Xaxis = mass and Yaxis = pT
   m_M_Nbin    = h2->GetNbinsX();
   m_Mmin_hist = h2->GetXaxis()->GetXmin();
   m_Mmax_hist = h2->GetXaxis()->GetXmax();
   
   if (m_bl_own_hist && m_h1_mass) delete m_h1_mass;
   m_h1_mass = new TH1D("h1_mass_own", "", m_M_Nbin, m_Mmin_hist, m_Mmax_hist);
   h2->ProjectionX(m_h1_mass->GetName(), bin_low, bin_high);
   m_bl_own_hist = true;
}

void Pi0MassFitter::FitMass(bool itr_0)
{
   m_h1_mass->SetAxisRange(m_fitrange_lower, m_fitrange_upper);
   m_content_max = m_h1_mass->GetMaximum();
   m_bin_content_max = m_h1_mass->GetMaximumBin();
//     double bincent_content_max = m_h1_mass->GetBinCenter(m_bin_content_max);
   double content_min = m_h1_mass->GetMinimum();
   double content_diff = m_content_max - content_min;
   m_h1_mass->SetAxisRange(m_Mmin_hist, m_Mmax_hist);
   
   if (content_min < 5) m_poln = 1;
   else if (5 <= content_min && content_min < 15) m_poln = 2;
   else m_poln = 3;
   
   char func_form[128];
   sprintf(func_form, "gaus(0) + pol%i(3)", m_poln);
   m_func = new TF1("fitfunc", func_form, m_fitrange_lower, m_fitrange_upper);
   if(itr_0){
     m_func->SetParameters(content_diff, m_MASS_0, m_SIGMA, content_min);
   } else {
     m_func->SetParameters(content_diff, m_MASS, m_SIGMA, content_min);
   }
   m_func->SetParLimits(0, 0.0, 1.2 * content_diff);
   m_func->SetParLimits(1, m_fitrange_lower, m_fitrange_upper);
   m_func->SetParLimits(2, m_sigma_lower, m_sigma_upper);
   
   m_h1_mass->Fit(m_func, "RBQ");
   SetFitResults();
   delete m_func;
   m_func = 0;
}

void Pi0MassFitter::FitMassByHand()
{
   Double_t con, mean, sigma, p0, p1, fit_lower, fit_upper;
   cout << "Initial parameters (const mean sigma p0 p1 range_lower range_upper)" << endl;
   while (true) {
      cout << "Param? ";
      Char_t buffer[1024];
      fgets(buffer, 1024, stdin);
      Int_t n_read = sscanf(buffer, "%lf%lf%lf%lf%lf%lf%lf", &con, &mean, &sigma, &p0, &p1, &fit_lower, &fit_upper);
      if (3 <= n_read && n_read <= 7) {
         if (n_read < 4) p0        = -1;
         if (n_read < 5) p1        =  0;
         if (n_read < 6) fit_lower = m_fitrange_lower;
         if (n_read < 7) fit_upper = m_fitrange_upper;
         break;
      } else {
         cout << "INPUT AGAIN." << endl;
      }
   }

   m_h1_mass->SetAxisRange(fit_lower, fit_upper);
   m_content_max = m_h1_mass->GetMaximum();
   m_bin_content_max = m_h1_mass->GetMaximumBin();
   double content_min = m_h1_mass->GetMinimum();
   m_h1_mass->SetAxisRange(m_Mmin_hist, m_Mmax_hist);
   
   if (p0 < 0) p0 = content_min;

   m_poln = 1;
   m_func = new TF1("fitfunc", "gaus(0) + pol1(3)", fit_lower, fit_upper);
   m_func->SetParameters(con, mean, sigma, p0, p1);
   
   m_h1_mass->Fit(m_func, "RBQ");
   SetFitResults();
   delete m_func;
   m_func = 0;
}

int Pi0MassFitter::UseResultsStoredInHist()
{
   m_func = m_h1_mass->GetFunction("fitfunc");
   if (! m_func) return false;
   SetFitResults();
   return true;
}

void Pi0MassFitter::SetFitResults()
{
   m_content_max     = m_h1_mass->GetMaximum();
   m_bin_content_max = m_h1_mass->GetMaximumBin();

   m_height     = m_func->GetParameter(0);
   m_height_err = m_func->GetParError(0);
   m_mean       = m_func->GetParameter(1);
   m_mean_err   = m_func->GetParError(1);
   m_sigma      = m_func->GetParameter(2);
   m_sigma_err  = m_func->GetParError(2);
   m_chi2       = m_func->GetChisquare();
   m_NDF        = m_func->GetNDF();
   m_poln       = m_func->GetNumberFreeParameters() - 3;

   char func_form[128];
   sprintf(func_form, "pol%i", m_poln);
   TF1* func_bg = new TF1("func_bg", func_form, 
                          m_fitrange_lower, m_fitrange_upper);
   func_bg->SetParameters( m_func->GetParameters() + 3 );
   
   int bin_low  = m_h1_mass->FindBin(m_mean - 3 * m_sigma);
   int bin_high = m_h1_mass->FindBin(m_mean + 3 * m_sigma);
   double m_low  = m_h1_mass->GetBinLowEdge(bin_low);
   double m_high = m_h1_mass->GetBinLowEdge(bin_high + 1);
   m_signal = m_h1_mass->Integral(bin_low, bin_high);
   m_bg = func_bg->Integral(m_low, m_high) / m_h1_mass->GetBinWidth(bin_low);
   m_signal -= m_bg;

   delete func_bg;
}
