#ifndef __PI0MASSFITTER_H__
#define __PI0MASSFITTER_H__

#include "EmcAnaCommon.h"

class TH1;
class TH2;
class TF1;

class Pi0MassFitter
{
   TH1* m_h1_mass; //!
   TF1* m_func; //!
   int m_bl_own_hist;

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
   
   Double_t m_content_max;
   Int_t m_bin_content_max;

   //static const Double_t m_MASS_0 = 0.155; //don't have to refit so many, typical of first iteration
   //New Val for AuAUrun11
   // static const Double_t m_MASS_0 = 0.115; //don't have to refit so many, typical of first iteration
//    static const Double_t m_MASS = 0.134;
//    static const Double_t m_SIGMA = 0.015; // typical value

  //Run12 UU&CuAu
  static const Double_t m_MASS_0 = 0.135; //don't have to refit so many, first iteration
  static const Double_t m_MASS = 0.135;
  static const Double_t m_SIGMA = 0.012;
  Double_t m_sigma_lower;
  Double_t m_sigma_upper;
  Double_t m_fitrange_lower;
  Double_t m_fitrange_upper;
  
  //// binning info
  Int_t    m_M_Nbin;
  Double_t m_Mmin_hist;
  Double_t m_Mmax_hist;
  
public:
  Pi0MassFitter();
  virtual ~Pi0MassFitter();
  void SetHist(TH1* h1);
  void SetHist(TH2* h2, int bin_low, int bin_high);
  
  void FitMass(bool itr_0 = false);
  void FitMassByHand();
  int UseResultsStoredInHist();
  
  void GetFitRange(Double_t& lower, Double_t& upper);
  void GetSigmaRange(Double_t& lower, Double_t& upper);
  void SetFitRange(Double_t lower, Double_t upper);
  void GetResults(Double_t& height, Double_t& height_err, 
		  Double_t& mean, Double_t& mean_err, 
		  Double_t& sigma, Double_t& sigma_err, 
		  Double_t& chi2, Int_t& NDF,
		  Int_t & poln, Double_t& signal, Double_t& bg);
  
  Double_t GetContentMax() { return m_content_max; }
  Int_t GetBinContentMax() { return m_bin_content_max; }
  Double_t GetMeanLowerFitLimit()  { return m_fitrange_lower; }
  Double_t GetSigmaLowerFitLimit() { return m_sigma_lower; }
  
protected:
  void SetFitResults();
  
};

inline void Pi0MassFitter::GetFitRange(Double_t& lower, Double_t& upper)
{
  lower = m_fitrange_lower;
  upper = m_fitrange_upper;
}

inline void Pi0MassFitter::GetSigmaRange(Double_t& lower, Double_t& upper)
{
  lower = m_sigma_lower;
  upper = m_sigma_upper;
}

inline void Pi0MassFitter::SetFitRange(Double_t lower, Double_t upper)
{
  m_fitrange_lower = lower;
  m_fitrange_upper = upper;
}

inline void Pi0MassFitter::GetResults(Double_t& height, Double_t& height_err, 
				      Double_t& mean, Double_t& mean_err, 
				      Double_t& sigma, Double_t& sigma_err, 
				      Double_t& chi2, Int_t& NDF,
				      Int_t & poln, Double_t& signal, Double_t& bg)
{
  height     = m_height;
  height_err = m_height_err;
  mean       = m_mean;
  mean_err   = m_mean_err;
  sigma      = m_sigma;
  sigma_err  = m_sigma_err;
  chi2       = m_chi2;
  NDF        = m_NDF;
  poln       = m_poln;
  signal     = m_signal;
  bg         = m_bg;
}

#endif // __TPI0MASSFITTER_H__

