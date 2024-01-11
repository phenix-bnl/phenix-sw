#include "MpcPi0Mass.h"
#include <iostream>
#include <TF1.h>

ClassImp(MpcPi0Mass)

using namespace std;

MpcPi0Mass::MpcPi0Mass()
{
}

MpcPi0Mass::MpcPi0Mass(Char_t* name, Int_t with_ntuple)
{
  SetName(name);
  m_with_ntuple = with_ntuple;

  TString hname;
  if (m_with_ntuple)
    {
      hname = "h1_"; hname += name;
      m_h1_mass = new TH1D(hname, hname, M_Nbin, Mmin_hist, Mmax_hist);
      hname = "nt_"; hname += name;
      m_nt_mass = new TNtuple(hname, hname, "mass:pT:p");
    }
  else
    {
      hname = "h1_"; hname += name;
      m_h1_mass = new TH1D(hname, hname, M_Nbin, Mmin_hist, Mmax_hist);
      hname = "h1_bkgd_"; hname += name;
      m_h1_massbkgd = new TH1D(hname, hname, M_Nbin, Mmin_hist, Mmax_hist);
      hname = "h1_yield_"; hname += name;
      m_h1_massyield = new TH1D(hname, hname, M_Nbin, Mmin_hist, Mmax_hist);
      m_nt_mass = 0;
    }

  // Various parameters for the fit
  m_mean_lower = 0.02;		// lower bound for mass
  m_mean_upper = 0.25;		// upper bound for mass

  m_sigma_lower = 0.005;	// lower bound for sigma
  m_sigma_upper = 0.2;		// upper bound for sigma
  //  m_sigma_upper = 0.1;

  //m_mass_fit_lower = 0.05;
  m_mass_fit_lower = -0.1;	// lower bound of fit range
  m_mass_fit_upper = 0.30;	// upper bound of fit range

  normlo = 0.25;  //integration range for normalization of signal and background
  normhi = 0.4;

  fFitStatus = FIT_FAIL;	// status of fit
}

MpcPi0Mass::~MpcPi0Mass()
{
  if (m_h1_mass) delete m_h1_mass;
  if (m_h1_massbkgd) delete m_h1_massbkgd;
  if (m_h1_massyield) delete m_h1_massyield;
  if (m_nt_mass) delete m_nt_mass;
}

void MpcPi0Mass::FillMass(Double_t mass, Double_t pT, Double_t p, Double_t gamma_e)
{
  if (m_with_ntuple)
    {
      if (pT == 0.0) cerr << GetName() << ": pT (2nd arg) = 0.0 !!\n";
      if (p  == 0.0) cerr << GetName() << ": p (3rd arg) = 0.0 !!\n";
      m_nt_mass->Fill(mass, pT, p);
    }
  else
    {
      if ( p>0. )
        {
          Double_t weight = 1;
	  //weight = gamma_e/p;
	  
          m_h1_mass->Fill(mass,weight);
        }
    }
}


void MpcPi0Mass::FillMassBkgd(Double_t mass, Double_t pT, Double_t p, Double_t gamma_e)
{
  if (m_with_ntuple)
    {
    }
  else
    {
      if ( p>0. )
        {
          Double_t weight = 1;
	  //weight = gamma_e/p;
	  
          m_h1_massbkgd->Fill(mass,weight);
        }
    }
}

void MpcPi0Mass::GetYield()
{
  int lobin = m_h1_mass->FindBin(normlo);
  int hibin = m_h1_mass->FindBin(normhi);
  
  float signal_int = m_h1_mass->Integral(lobin,hibin);
  float bkgd_int = m_h1_massbkgd->Integral(lobin,hibin);
  //std::cout << "signal counts in integration range" << signal_int
  //    << "\nbkgd counts in integration range" << bkgd_int << "\n";
  if(bkgd_int >0 && signal_int >0){
    m_h1_massbkgd->Scale(signal_int/bkgd_int);
    m_h1_massyield->Add(m_h1_mass,1);
    m_h1_massyield->Add(m_h1_massbkgd,-1);
  }
  else{
    //if(bkgd_int == 0)
     // cout << "bkgd counts = 0 in integration range\n";
    //if(signal_int == 0)
    // cout << "bkgd counts = 0 in integration range\n";
  }
}

void MpcPi0Mass::FitMass(Double_t pT_lower, Double_t pT_upper, Double_t p_lower, Double_t p_upper)
{
  if (m_with_ntuple)
    {
      char selection[2048];
      sprintf(selection, "%f < pT && pT < %f && %f < p && p < %f", pT_lower, pT_upper, p_lower, p_upper);
      m_h1_mass->Reset();
      m_nt_mass->Project(m_h1_mass->GetName(), "mass", selection);
    }

  double Mmin_hist = m_h1_mass->GetXaxis()->GetXmin();
  double Mmax_hist = m_h1_mass->GetXaxis()->GetXmax();
  m_h1_mass->SetAxisRange(m_mass_fit_lower, m_mass_fit_upper);
  double content_max = m_h1_mass->GetMaximum();
  double content_min = m_h1_mass->GetMinimum();
  m_h1_mass->SetAxisRange(Mmin_hist, Mmax_hist);

  // ****  should be changed  ****
  if (content_min < 80) fPoln = 1;
  else if (80 <= content_min && content_min < 400) fPoln = 2;
  else fPoln = 3;
  // *****************************

  char func_form[1024];
  //sprintf(func_form, "gaus(0) + pol%i(3)", fPoln);
  sprintf(func_form, "gaus(0) + expo(3)");

  TF1* func = new TF1("fitfunc", func_form, m_mass_fit_lower, m_mass_fit_upper);
  for (float lowest_mass=m_mass_fit_lower; lowest_mass<0.05; lowest_mass+=0.01)
    {
      func->SetRange(lowest_mass,m_mass_fit_upper);
      func->SetParameters(content_max - content_min,PI0_MASS, m_sigma_typical,1,1,1);
      func->SetParLimits(0, 0.0, 1.2 * content_max);
      func->SetParLimits(1, m_mean_lower, m_mean_upper);
      func->SetParLimits(2, m_sigma_lower, m_sigma_upper);
     
      m_h1_mass->Fit(func, "RBQ");

      fChi2       = m_h1_mass->GetFunction("fitfunc")->GetChisquare();
      fNDF        = m_h1_mass->GetFunction("fitfunc")->GetNDF();
      if ( fChi2/fNDF < 3 ) break;
    }
     
  fMean      = m_h1_mass->GetFunction("fitfunc")->GetParameter(1);
  fMeanErr   = m_h1_mass->GetFunction("fitfunc")->GetParError(1);
  fSigma     = m_h1_mass->GetFunction("fitfunc")->GetParameter(2);
  fSigmaErr  = m_h1_mass->GetFunction("fitfunc")->GetParError(2);
  fChi2      = m_h1_mass->GetFunction("fitfunc")->GetChisquare();
  fNDF       = m_h1_mass->GetFunction("fitfunc")->GetNDF();
  Int_t bin_lower = m_h1_mass->FindBin(m_mass_fit_lower);
  Int_t bin_upper = m_h1_mass->FindBin(m_mass_fit_upper);
  fNpairInBin  = m_h1_mass->Integral(bin_lower, bin_upper) / (bin_upper - bin_lower);

  delete func;

  //if (fNpairInBin < 10 || fNDF <= 0)
  if (fNpairInBin < 5 || fNDF <= 0)
    {
      fFitStatus = FIT_FAIL;
    }
  //else if (fChi2 / fNDF > 10)
  else if (fChi2 / fNDF > 30)
    {
      fFitStatus = FIT_FAIR;
    }
  else
    {
      fFitStatus = FIT_GOOD;
    }
}



void MpcPi0Mass::FitMass_new(Double_t pT_lower, Double_t pT_upper, Double_t p_lower, Double_t p_upper)
{
  if (m_with_ntuple)
    {
      char selection[2048];
      sprintf(selection, "%f < pT && pT < %f && %f < p && p < %f", pT_lower, pT_upper, p_lower, p_upper);
      m_h1_mass->Reset();
      m_nt_mass->Project(m_h1_mass->GetName(), "mass", selection);
    }

  double Mmin_hist = m_h1_mass->GetXaxis()->GetXmin();
  double Mmax_hist = m_h1_mass->GetXaxis()->GetXmax();
  m_h1_mass->SetAxisRange(m_mass_fit_lower, m_mass_fit_upper);
  m_h1_massyield->SetAxisRange(m_mass_fit_lower, m_mass_fit_upper);
  double content_max = m_h1_massyield->GetMaximum();
  double content_min = m_h1_massyield->GetMinimum();
  m_h1_mass->SetAxisRange(Mmin_hist, Mmax_hist+0.01);
  m_h1_massyield->SetAxisRange(Mmin_hist, Mmax_hist+0.01);

  // ****  should be changed  ****
  //if (content_min < 80) fPoln = 1;
  //else if (80 <= content_min && content_min < 400) fPoln = 2;
  //else fPoln = 3;
  // *****************************

  char func_form[1024];
  //sprintf(func_form, "gaus(0) + pol%i(3)", fPoln);
  sprintf(func_form, "gaus(0)");

  TF1* func = new TF1("fitfunc", func_form, m_mass_fit_lower, m_mass_fit_upper);
  for (float lowest_mass=m_mass_fit_lower; lowest_mass<0.05; lowest_mass+=0.01)
    {
      func->SetRange(lowest_mass,m_mass_fit_upper);
      func->SetParameters(content_max - content_min,PI0_MASS, m_sigma_typical);
      func->SetParLimits(0, 0.0, 1.2 * content_max);
      func->SetParLimits(1, m_mean_lower, m_mean_upper);
      func->SetParLimits(2, m_sigma_lower, m_sigma_upper);
     
      m_h1_massyield->Fit(func, "RBQ");

      fChi2       = m_h1_massyield->GetFunction("fitfunc")->GetChisquare();
      fNDF        = m_h1_massyield->GetFunction("fitfunc")->GetNDF();
      if ( fChi2/fNDF < 3 ) break;
    }
     
  fMean      = m_h1_massyield->GetFunction("fitfunc")->GetParameter(1);
  fMeanErr   = m_h1_massyield->GetFunction("fitfunc")->GetParError(1);
  fSigma     = m_h1_massyield->GetFunction("fitfunc")->GetParameter(2);
  fSigmaErr  = m_h1_massyield->GetFunction("fitfunc")->GetParError(2);
  fChi2      = m_h1_massyield->GetFunction("fitfunc")->GetChisquare();
  fNDF       = m_h1_massyield->GetFunction("fitfunc")->GetNDF();
  Int_t bin_lower = m_h1_massyield->FindBin(m_mass_fit_lower);
  Int_t bin_upper = m_h1_massyield->FindBin(m_mass_fit_upper);
  fNpairInBin  = m_h1_massyield->Integral(bin_lower, bin_upper) / (bin_upper - bin_lower);

  delete func;

  //if (fNpairInBin < 10 || fNDF <= 0)
  if (fNpairInBin < 5 || fNDF <= 0)
    {
      fFitStatus = FIT_FAIL;
    }
  //else if (fChi2 / fNDF > 10)
  else if (fChi2 / fNDF > 30)
    {
      fFitStatus = FIT_FAIR;
    }
  else
    {
      fFitStatus = FIT_GOOD;
    }
}


