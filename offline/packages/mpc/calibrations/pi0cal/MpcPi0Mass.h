#ifndef __MPCPI0MASS_H__
#define __MPCPI0MASS_H__

#include <TROOT.h>
#include <TH1.h>
#include <TNtuple.h>

class MpcPi0Mass : public TNamed
{
public:
     MpcPi0Mass();
     MpcPi0Mass(Char_t* name, Int_t with_ntuple = false);
     virtual ~MpcPi0Mass();
     void FillMass(Double_t mass, Double_t pT = 0.0, Double_t p = 0.0, Double_t p1 = 0.0);
     void FillMassBkgd(Double_t mass, Double_t pT = 0.0, Double_t p = 0.0, Double_t p1 = 0.0);//Beau:
     void FitMass(Double_t pT_lower = 0.0, Double_t pT_upper = 1e10, 
		  Double_t p_lower = 0.0, Double_t p_upper = 1e10);
     void FitMass_new(Double_t pT_lower = 0.0, Double_t pT_upper = 1e10, //Beau: gets yeilds--take a look
		  Double_t p_lower = 0.0, Double_t p_upper = 1e10);
     void SetFitRange(Double_t lower, Double_t upper);
     void SetFitLimits(Double_t mean_lower, Double_t mean_upper, 
		       Double_t sigma_lower, Double_t sigma_upper);
     void GetResults(Double_t& mean, Double_t& mean_err, 
		     Double_t& sigma, Double_t& sigma_err, 
		     Double_t& chi2, Int_t& NDF,
		     Int_t & poln, Double_t& npair_in_bin);

     Int_t HasNtuple();
     Int_t FitIsGood();
     Int_t FitIsFair();
     Int_t FitIsFail();
     TH1D* GetHist();
     TH1D* GetHistBkgd();
     void SetHist( TH1D* h ); //Beau: Added for mixing
     TNtuple* GetNtuple();
     Double_t GetCoefficient();

     void GetYield();

private:
     TH1D* m_h1_mass;
     TH1D* m_h1_massbkgd; //Beau:
     TH1D* m_h1_massyield;//Beau:
     
     TNtuple* m_nt_mass;

     Int_t m_with_ntuple;

     Double_t fMean;		// mass of pi0 fit
     Double_t fMeanErr;
     Double_t fSigma;		// sigma of pi0 fit
     Double_t fSigmaErr;
     Double_t fChi2;		// chi2 of pi0 fit
     Int_t fNDF;		// NDF of pi0 fit
     Int_t fPoln;		// parameter of poln fit
     Double_t fNpairInBin;	// number of pi0's in bin

     Int_t fFitStatus;		// status of pi0 fit

     static const Double_t PI0_MASS = 0.1349766;
     static const Double_t m_sigma_typical = 0.05;
     //     static const Double_t m_sigma_typical = 0.02;
     Double_t m_mean_lower;
     Double_t m_mean_upper;
     Double_t m_sigma_lower;
     Double_t m_sigma_upper;
     Double_t m_mass_fit_lower;
     Double_t m_mass_fit_upper;

     Double_t normlo;
     Double_t normhi;

     static const Int_t M_Nbin = 130;
     static const Double_t Mmin_hist = -0.1;
     static const Double_t Mmax_hist = 1.2;

     static const Int_t FIT_GOOD = 0x0;
     static const Int_t FIT_FAIR = 0x1;
     static const Int_t FIT_FAIL = 0x2;

     ClassDef(MpcPi0Mass, 1)
};

inline void MpcPi0Mass::SetFitRange(Double_t lower, Double_t upper)
{
     m_mass_fit_lower = lower;
     m_mass_fit_upper = upper;
}

inline void MpcPi0Mass::SetFitLimits(Double_t mean_lower, Double_t mean_upper, 
				   Double_t sigma_lower, Double_t sigma_upper)
{
     m_mean_lower = mean_lower;
     m_mean_upper = mean_upper;
     m_sigma_lower = sigma_lower;
     m_sigma_upper = sigma_upper;
}

inline void MpcPi0Mass::GetResults(Double_t& mean, Double_t& mean_err, 
				 Double_t& sigma, Double_t& sigma_err, 
				 Double_t& chi2, Int_t& NDF,
				 Int_t & poln, Double_t& npair_in_bin)
{
     mean = fMean;
     mean_err = fMeanErr;
     sigma = fSigma;
     sigma_err = fSigmaErr;
     chi2 = fChi2;
     NDF = fNDF;
     poln = fPoln;
     npair_in_bin = fNpairInBin;
}

inline Int_t MpcPi0Mass::HasNtuple()
{
     return m_with_ntuple;
}

inline Int_t MpcPi0Mass::FitIsGood()
{ 
     return fFitStatus & FIT_GOOD; 
}

inline Int_t MpcPi0Mass::FitIsFair() 
{
     return fFitStatus & FIT_FAIR; 
}

inline Int_t MpcPi0Mass::FitIsFail()
{
     return fFitStatus & FIT_FAIL; 
}

inline TH1D* MpcPi0Mass::GetHist()
{
     return m_h1_mass; 
}

inline TH1D* MpcPi0Mass::GetHistBkgd()
{
     return m_h1_massbkgd; 
}

inline void MpcPi0Mass::SetHist(TH1D* h)
{
  m_h1_mass = new TH1D(*h); 
}

inline TNtuple* MpcPi0Mass::GetNtuple()
{
     return m_nt_mass; 
}

inline Double_t MpcPi0Mass::GetCoefficient()
{
     return PI0_MASS / fMean;
}

#endif // __MPCPI0MASS_H__
