#ifndef __ncfclProbDist_h__
#define __ncfclProbDist_h__

#include "TMath.h"
#include "TH1.h"

namespace NcfclA
{

   void setHandler();
   
   void my_gsl_error_handler(const char *reason, const char *file, int line, int gsl_errno);
   
  //Normalization factor for Normal Distribution > 0
  Double_t norm0(Double_t* xa, Double_t* par);

  Double_t norm(Double_t* x, Double_t* par);

  Double_t normal(Double_t* x, Double_t* par);
  
  Double_t gausexp(Double_t* x, Double_t* par);
  
  Double_t Gamma(Double_t *x, Double_t *par);

  Double_t GammaDist(Double_t *x, Double_t *par);

  // Analytic solution (Mathematica) for Gamma Distribution with underlying
  // gaussian smearing.  Only valid for x > width^2 * gamma_p
  Double_t SmearedGammaDist(Double_t *x, Double_t *par);

  // N successes in M tries, each try Probability p
  Double_t Binomial(Int_t N, Int_t M, Double_t p);

  Double_t myBinomial(Double_t *x, Double_t* par);

  Double_t myNegBinomial(Double_t *x, Double_t* par);
  
  // Number of ways to choose N objects given M objects
  Double_t Choose(Double_t M, Double_t N);

  Double_t fNgNbNcol(Double_t* x, Double_t* par);

  Double_t fNgNbNcolRenorm(Double_t *x, Double_t *par);

  Double_t fNBlack(Double_t *x, Double_t *par);
  
  // Conditional Probability of observing Efcal energy in the forward
  // calorimeter when Ngrey grey protons are within the forward
  // calorimeter acceptance.  For now (10/27/2003), Ron is using a
  // Gamma function to describe the Fcal energy distribution to Ngrey grey
  // protons.  Analyzing E910 experimental tracks in a PISA
  // simulation, the mean energy is proportional to the number of grey
  // protons.
  Double_t fPEfcalNgrey(Double_t *x, Double_t *par);

  // Mean Ngrey is assumed to follow second order polonomial in Ncoll
  Double_t MeanNgrey(Double_t Ncoll, Double_t c0, Double_t c1,
		     Double_t c2);

  // Conditional Probability Distribution for observing Ngrey grey protons
  // particles when Ncoll binary collisions occurred in a nucleus of
  // Z_Target. Mean Ngrey dependence assumed to be a 2nd order polonomial

  Double_t fPNgreyNcol(Double_t *x, Double_t *par);
  Double_t fPNgreyNcol2(Double_t *x, Double_t *par);


  //Probability of Ngreys in Fcal Acceptance given Ngreys total
  //Assume Binomial Distribution
  Double_t fPNgfcalNg(Double_t *x, Double_t *par);

  // Conditional Probability Distribution for observing Efcal energy in the 
  // forward calorimeter when Ncol binary collisions occur.  Distribution
  // is made by summing over Ngrey for the product of fPNgreyNcol* fPEfcalNgrey
  // distributions.
  Double_t fPEfcalNcol(Double_t *x, Double_t *par);

  Double_t fPEfcalNcolRenorm(Double_t *x, Double_t *par);

  Double_t fPGlauber(Double_t *x, Double_t *par);

  Double_t fPGlauberEfcalNcol(Double_t *x, Double_t *par);

  Double_t fPEfcalSumNcol(Double_t *x, Double_t *par);

  Double_t fGlauberNgreyNcol(Double_t *x, Double_t *par);

  Double_t fNgreySumNcol(Double_t *x, Double_t *par);

  Double_t fPSimpleEfcalNcol(Double_t *x, Double_t *par);

  Double_t fPSimpleGlauberEfcalNcol(Double_t *x, Double_t *par);

  Double_t fPSimpleGlauberEfcalSumNcol(Double_t *x, Double_t *par);

  Double_t fPZdcEBlack(Double_t* x, Double_t* par);

  Double_t fPZdcBlackNcol(Double_t* x, Double_t* par);

  Double_t fPZdcNcolSumNblack(Double_t* x, Double_t* par);

  Double_t fPZdcSumNcol(Double_t* x, Double_t* par);

  Double_t fPZNumBlackSumNcol(Double_t* x, Double_t* par);

  Double_t fPGlauberEfcalGammaNcol(Double_t* x, Double_t* par);

  Double_t fGlauberNgNbSumNcol(Double_t* x, Double_t* par);

  enum locConsts {dMax  = 2, AuMax = 50, maxNcolSum = 36};

  // Local Arrays (needed by non-class member fit routines)
  //  static double    glauber[AuMax][dMax];
  extern "C" TH1* hGlauberDist;
  extern "C" TF1* fMeanNgrey;
  
};



#endif // __ncfclProbDist_h__
