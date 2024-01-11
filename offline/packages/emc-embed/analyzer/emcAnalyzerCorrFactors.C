// $Header
//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2002-2003
//
//  Author: D. d'Enterria
//
//  Nevis Labs. Columbia University
//
//-----------------------------------------------------------------------------


#include <cmath>
#include <cstring>
#include <iostream>

#include "TFormula.h"
#include "TString.h"
#include "TGraphErrors.h"

#include "emcAnalyzerUtils.h"
#include "emcAnalyzerCorrFactors.h"

// input pi0 Efficiencies
//#include "emcEfficiency_8percent_counts.h"
//#include "emcEfficiency_6percent_counts.h"
//#include "emcEfficiency_7percent_counts.h" // Final pi0 efficiencies for PPG014
//#include "emcEfficiency_pi0_4percentsmear_2pass_counts.h" // Dec.'03 pi0 repass, 4% smearing, elec. corr.
#include "emcEfficiency_pi0_4percentsmear_3pass_counts.h" // Dec.'03 pi0 repass, 4% smearing, elec. corr.

// input eta Efficiencies
//#include "emcEfficiency_eta_7percentsmear_counts.h" // July/2003, 7% smearing, pol-log fit
//#include "emcEfficiency_eta_5percentsmear_counts.h" // Oct/2003, 5% smearing, pol4 fit
//#include "emcEfficiency_eta_5percentsmear_2pass_counts.h" // Oct/2003, 5% smearing, pol4 fit
//#include "emcEfficiency_eta_5percentsmear_2pass_counts_newextraloss.h" // Oct/2003, 5% smearing, pol4 fit
//#include "emcEfficiency_eta_3percentsmear_2pass_counts.h" // Nov/2003, 3% smearing, pol4 fit
//#include "emcEfficiency_eta_2percentsmear_3pass_counts.h" // Dec.'03, 2% smearing, pol6 fit
//#include "emcEfficiency_eta_3percentsmear_1pass_counts.h" // Final eta, 3% smearing, log+log^2 fit
//#include "emcEfficiency_eta_3percentsmear_2pass_counts.h" // Final eta, 3% smearing, 2nd iteration, log+log^2 fit
#include "emcEfficiency_eta_3percentsmear_3pass_counts.h" // Final eta, 3% smearing, 2nd iteration, log+log^2 fit

using namespace std;

using namespace emcAnalyzerUtils;
using namespace TMath;

namespace emcAnalyzerCorrFactors
{

//_____________________________________________________________________________
double 
acceptance(const double pT, const char *type, const char* partic )
{

  double Accep = 0.;  
  double Accep_error = 0.;

  if (strcasecmp(partic,"pi0") == 0) 
    {

      // ================================================================================
      // Pi0 PbSC acceptance
      // ================================================================================
      // Parametrization PHENIX Analysis Note AN115 + update AN170
      // Accep = ([0]+[1]*x)*(1.0-exp([2]-[3]*x))
      // Accep_error = A_error/A *Accep  1
      
      // New PbSc acceptance fit using fast MC results
      //  FCN=119.591 FROM MIGRAD    STATUS=CONVERGED     166 CALLS         167 TOTAL
      //                      EDM=7.88888e-17    STRATEGY= 1  ERROR MATRIX UNCERTAINTY   0.8 per cent
      //   EXT PARAMETER                                   STEP         FIRST   
      //   NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
      //    1  p0           2.37499e-01   7.92532e-04  -1.29857e-10  -6.68643e-05
      //    2  p1           2.88449e-03   9.35308e-05   2.08646e-11  -5.47195e-04
      //    3  p2           1.06248e-01   1.28257e-02  -9.41850e-10   2.14084e-06
      //    4  p3           1.04224e+00   1.67829e-02   1.30360e-09  -1.34320e-06
      
      
      double A = 0.237499  ;  double A_error = 0.000792532;
      double B = 0.00288449 ; double B_error = 9.35308e-05;
      double a = 0.106248  ;  double a_error = 0.0128257;
      double b = 1.04224   ;  double b_error = 0.0167829;
      
      Accep = (A + B*pT) * (1.0 - Exp(a-b*pT));
      
      // ================================================================================
      // Dec.'03 pi0 PbSC acceptance
      // ================================================================================
      //
      // fast MC with fine binning, input power-law pT spectrum, and 5-parameter fit
      //
      // Acceptance_pi0_PbSc_Run2.root
      // ================================================================================
      // ([0]+[1]*x+[2]*x*x)*(1.0-exp([3]-[4]*x))
      // [0] = 0.248815 +/- 0.00140156  Relative Error = 0.563294%
      // [1] = 0.00338359 +/- 0.00025908  Relative Error = 7.65696%
      // [2] = -0.000102529 +/- 1.07306e-05  Relative Error = 10.4659%
      // [3] = 0.0200231 +/- 0.0432673  Relative Error = 216.087%
      // [4] = 1.3173 +/- 0.0509205  Relative Error = 3.86551%
      
//       double p0 = 0.248815;     double ep0 = 0.00140156;
//       double p1 = 0.00338359;   double ep1 = 0.00025908;
//       double p2 = -0.000102529; double ep2 = 1.07306e-05; // though small, it reproduces better data in pT=2-5 GeV/c
//       double p3 = 0.0200231;    double ep3 = 0.0432673;
//       double p4 = 1.3173;       double ep4 = 0.0509205;
      
//       Accep = (p0 + p1*pT + p2*pT*pT) * (1.0 - Exp(p3 - p4*pT));

      // This below is the error coming from the acceptance fit
	  
      // It amounts to ~3.8% at low pT and ~1.8% at high pT
      double term1 = Accep/(A+B*pT);
      double term2 = ( A+B*pT - Accep );
      Accep_error = term1*term1*( A_error*A_error + pT*pT*B_error*B_error ) +
	term2*term2*( a_error*a_error + pT*pT*b_error*b_error );
      
      // FIXME: we should compute Accep_error for new acceptance

    }
  else if (strcasecmp(partic,"eta") == 0) 
    {

      //
      //       // ================================================================================
      //       // Eta acceptance as of Oct.03
      //       // ================================================================================
      //       // /afs/run2_eta/run2_eta_acceptance/Acceptance_eta_PbSc_Run2.root
      //       // ================================================================================
      //       // Fit to 4-degree polinomial
      //       //
      //       //   1  p0          -3.03784e-02   6.03324e-04   2.88304e-07   4.62104e-06
      //       //   2  p1           3.71808e-02   5.75195e-04   4.34431e-08   7.21397e-06
      //       //   3  p2          -4.97931e-03   1.58637e-04   4.12875e-09  -5.68597e-05
      //       //   4  p3           3.18322e-04   1.64939e-05   3.52046e-10  -1.26391e-03
      //       //   5  p4          -7.72499e-06   5.72931e-07   2.86494e-11   3.72020e-03
      
      double p0 = -3.03784e-02  ; double ep0 = 6.03324e-04; 
      double p1 =  3.71808e-02  ; double ep1 = 5.75195e-04;
      double p2 = -4.97931e-03  ; double ep2 = 1.58637e-04;
      double p3 =  3.18322e-04  ; double ep3 = 1.64939e-05;
      double p4 = -7.72499e-06  ; double ep4 = 5.72931e-07;
      
      //       Accep = p0 + p1*pT + p2 * pT*pT + p3 * pT*pT*pT + p4 * pT*pT*pT*pT;


      // We use Baldo's acceptance (so that acc. exactly cancels in R_AA)
      // ([0]+[1]*x)*(1.0-exp([2]-[3]*x)) with the parameters:

      double BRgg = 0.3943;
      p0 = 1.97466e-01;//   2.79783e-01
      p1 = 3.09929e-03;//   2.11112e-02
      p2 = 4.34005e-01;//   1.33475e+00
      p3 = 4.25437e-01;//   9.96158e-01

      Accep = BRgg * (p0 + p1*pT ) * (1.0 - Exp(p2 - p3*pT));
      
      // FIXME: we should compute Accep_error for new acceptance

    }

  Accep_error = Sqrt( Accep_error );
  
  //cout << " <I> Acc. relat. error:  " << Accep_error/Accep << endl;
  
  if (strcasecmp(type,"error") == 0) 
    {
      return Accep*0.03; // fixme: fixed error
      //return Accep_error;
    }
  else 
    {     
      //cout << " <I> Acc.:  " << pT << " " << Accep << endl;
      return Accep;
    }
  
}

//_____________________________________________________________________________
double
acceptance_old(const double pT,const char *type)
{

  // Acceptance Parametrization PHENIX Analysis Note 115
  // Accep = A * (1.0 - exp(-a-b*pT))
  // Accep_error = A_error/A *Accep

  double Accep;     double Accep_error;
  double A=0.255;   double A_error=0.002;
  double a=0.46;    double a_error=0.07;
  double b=0.49;    double b_error=0.04;

  Accep = A * (1.0 - Exp(-a-b*pT));

  if (strcasecmp(type,"error") == 0) 
    { 
      double abpT_error= Sqrt(a_error*a_error+b_error*b_error*pT*pT);
      Accep_error = Sqrt( Power(A_error/A*Accep,2) +
				 Power(A*Exp(-a-b*pT)*b*abpT_error,2) );
      return Accep_error;
    }
  else 
    { 
      return Accep;
    }

}

//_____________________________________________________________________________
double
offVertex(const char *type)
{

  // Not used now anymore

  if (strcasecmp(type,"error") == 0)
    {
      return 0.0;
    }
  else
    {
      return 1.0; 
    }
}

//_____________________________________________________________________________
double 
efficiency(const double pT, 
	   const int CentClass, 
	   const char* cut,
	   const char* type, 
	   const char* partic)
{

  double eff = efficiencyForCut(pT,CentClass,cut,"value",partic);

  if ( strcasecmp(type,"error")==0 ) 
    {

      //--- Systematic Error Calculation:
      //
      // [CCC] = "Centrality correlated error": Errors that cancel in ratios of centralities.
      // [PTC] = "pT correlated error": Errors that move consistently up&down all the spectrum.
      
      // 1. Raw pi0 extraction ~ 10%  [non CCC] [non PTC]

      double ExtractionRelatError = 0.10 ; // pT-dependence (?), CC-dependence (?)

      if (strcasecmp(partic,"eta") == 0)
	{
	  ExtractionRelatError = 0.0; // taken into account in emcAnalyzer::plot_spectrum directly (included in "ecounts")
	}

      // 2. Efficiency fit <2% (? fixme ...)    [non CCC] [non PTC]

      double EfffitRelatError = efficiencyForCut(pT,CentClass,cut,"error",partic);
      EfffitRelatError /= eff;
      
      // 3. Energy scale, (pT smearing, power-law weights). [CCC] [basically PTC]

      double EnScaleRelatError = getEnScaleRelError(pT);

      // 4. Fiducial + DeadWarn + Acceptance cuts  [CCC] [PTC]
      // Based on the difference between real and merged dead-map application
      // Difference between PISA and Objectivity geometries + Acceptance fit

      double FiduDeadWarnPlusAccRelatError = 0.05 ;
      
      // 5. PID (Chi2*TOF) cut (**)   [non CCC] [non PTC]

      double PIDRelatError = 0.08; // constant with pT and centrality
           
      // INFO: For completeness ...
      // Other errors outside efficiency that add up in the final errors:
      // 
      // 6. General acceptance (fit): See acceptance() function below ---> FIXME: Already in 4. ?
      // 7. Off-vtx (now null ...)
      // 8. Statistical (Npi0 & Nevts)
      // 9. pT bin error: 50 MeV/c ---> This is already included in 3.
      
      double effRelatError = quadRelatError( ExtractionRelatError,1.,
					     EfffitRelatError,1.,
					     EnScaleRelatError,1.,
					     FiduDeadWarnPlusAccRelatError,1.,
					     PIDRelatError,1.);
      
      return effRelatError*eff;
    }
  else
    {
      return eff;
    }

}

//_____________________________________________________________________________
double 
efficiencyForCut( double pT, 
		  int CentClass, 
		  const char* cut,
		  const char* type, 
		  const char* partic)
{

  double* p0;
  double* p0err;
  double* p1;
  double* p1err;
  double* p2;
  double* p2err;
  double* p3;
  double* p3err;
  double* p4;
  double* p4err;
  double* p5;
  double* p5err;
  double* p6;
  double* p6err;

  if ( CentClass == 17 ) // 70-92% or 60-92%
    {
      int c1 = 0; int c2 = 0;
      getCentralityClassLimits( CentClass, c1, c2);
      //cout << " <W> Efficiency correction factors for class " << c1 << "-" << c2 
      //	   << "% not computed by default. Using those of 70-80% " << endl;
    }
  if ( CentClass == 18 ) // 60-80%
    {
      int c1 = 0; int c2 = 0;
      getCentralityClassLimits( CentClass, c1, c2);
      //cout << " <W> Efficiency correction factors for class " << c1 << "-" << c2 
      //	   << "% not computed by default. Using those of 60-70%" << endl;
    }

  TFormula *EffFunc = 0;

  if (strcasecmp(partic,"pi0") == 0)
    {
      EffFunc = new TFormula("EffFunc","pol6");

      //EffFunc = new TFormula("EffFunc","[0]+[1]*x+[2]*x*x+[3]*log(x)+[4]*log(x)*log(x)"); // PPG014
      //cout << "using pi0 pol-log effic. fit" << endl;

      //// EffFunc = new TFormula("EffFunc","([0]*x*x+[1]*x+[2])*(1.0-exp([3]*x+[4]))"); // QM'02 (?)

      getParamsPointers(cut,p0,p0err,p1,p1err,p2,p2err,p3,p3err,p4,p4err,p5,p5err,p6,p6err,partic);
      
      if ( !p0 ) 
	{
	  cerr << " <E> Unknown efficiencies for cut: " << cut << endl;
	  return -1.;
	}

      EffFunc->SetParameters(p0[CentClass],p1[CentClass],p2[CentClass],p3[CentClass],
			     p4[CentClass],p5[CentClass],p6[CentClass]);

      //EffFunc->SetParameters(p0[CentClass],p1[CentClass],p2[CentClass],p3[CentClass],p4[CentClass]);
    }
  else
    {

      //EffFunc = new TFormula("EffFunc","pol6");
      //EffFunc = new TFormula("EffFunc","pol4");
      //getParamsPointers(cut,p0,p0err,p1,p1err,p2,p2err,p3,p3err,p4,p4err,partic);

      EffFunc = new TFormula("EffFunc","[0]+[1]*x+[2]*x*x+[3]*log(x)+[4]*log(x)*log(x)");
      //cout << "using eta pol-log effic. fit" << endl;
      
      getParamsPointers(cut,p0,p0err,p1,p1err,p2,p2err,p3,p3err,p4,p4err,p5,p5err,p6,p6err,partic);

      if ( !p0 ) 
	{
	  cerr << " <E> Unknown efficiencies for cut: " << cut << endl;
	  return -1.;
	}

      //EffFunc->SetParameters(p0[CentClass],p1[CentClass],p2[CentClass],p3[CentClass],
      //			     p4[CentClass],p5[CentClass],p6[CentClass]);
      EffFunc->SetParameters(p0[CentClass],p1[CentClass],p2[CentClass],p3[CentClass],
			     p4[CentClass]);
    }

  double effic = EffFunc->Eval(pT); 
  
  // This below is the error coming from the efficiency fit
  // It amounts to ~3.8% at low pT and ~1.8% at high pT

  if ( strcasecmp(type,"error")==0 ) 
    {

//       TFormula eEff1("eEff1","pow( [3]/([0]*x*x+[1]*x+[2]), 2.)");
//       eEff1.SetParameters(p0[CentClass],p1[CentClass],p2[CentClass],effic);

//       TFormula eEff2("eEff2","pow( ([0]*x*x+[1]*x+[2]-[3]), 2.)");
//       eEff2.SetParameters(p0[CentClass],p1[CentClass],p2[CentClass],effic);

//       double effic_error = eEff1.Eval(pT)*( Power(p2err[CentClass],2.) + 
// 					    Power(pT*p1err[CentClass],2.) +
// 					    Power(pT*pT*p0err[CentClass],2.)  ); 

//       effic_error += eEff2.Eval(pT)*( Power(p4err[CentClass],2.) + 
// 				      Power(pT*p3err[CentClass],2.) );

//       effic_error = Sqrt( effic_error );

      //cout << " <I> Effic. relat error = " << effic_error/effic << endl;

      //return effic_error;  // FIXME: As of now this error is not realistic

      return 0.02*effic;  // FIXME: 2.5% fixed error in the efficiency fit
    }
  else
    {      
      return effic;
    }

}

//_____________________________________________________________________________
void
getParamsPointers( const char* cut,
		   double*& p0,
		   double*& p0err,
		   double*& p1,
		   double*& p1err,
		   double*& p2,
		   double*& p2err,
		   double*& p3,
		   double*& p3err,
		   double*& p4,
		   double*& p4err, 
		   double*& p5,
		   double*& p5err, 
		   double*& p6,
		   double*& p6err, 
		   const char* partic)
{

  TString cut_str = cut;
  if (cut_str.Contains("trig",TString::kIgnoreCase) ) // high pT triggered yields
    {
      cut_str.Remove(cut_str.Capacity()-4,cut_str.Capacity()); // "chop" the "trig" prefix
    }
  if (cut_str.Contains("r012",TString::kIgnoreCase) || // reaction-plane binned yields
      cut_str.Contains("r345",TString::kIgnoreCase) ||
      cut_str.Contains("r678",TString::kIgnoreCase) )
    {
      cut_str = "noPID";
    }

  char cutname[100];
  sprintf(cutname,cut_str.Data());

  //_____________________________________________________________________________
  if (strcasecmp(partic,"pi0") == 0)
  {
  if (strcasecmp(cutname,"noPID") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1Cut_counts;

      p0 = P0_BasicCuts3x3Asym1Cut_counts_pi0;
      p1 = P1_BasicCuts3x3Asym1Cut_counts_pi0;
      p2 = P2_BasicCuts3x3Asym1Cut_counts_pi0;
      p3 = P3_BasicCuts3x3Asym1Cut_counts_pi0;
      p4 = P4_BasicCuts3x3Asym1Cut_counts_pi0;
      p5 = P5_BasicCuts3x3Asym1Cut_counts_pi0;
      p6 = P6_BasicCuts3x3Asym1Cut_counts_pi0;

      p0err = P0ERR_BasicCuts3x3Asym1Cut_counts_pi0;
      p1err = P1ERR_BasicCuts3x3Asym1Cut_counts_pi0;
      p2err = P2ERR_BasicCuts3x3Asym1Cut_counts_pi0;
      p3err = P3ERR_BasicCuts3x3Asym1Cut_counts_pi0;
      p4err = P4ERR_BasicCuts3x3Asym1Cut_counts_pi0;
      p5err = P5ERR_BasicCuts3x3Asym1Cut_counts_pi0;
      p6err = P6ERR_BasicCuts3x3Asym1Cut_counts_pi0;

//       p0 = P0_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p1 = P1_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p2 = P2_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p3 = P3_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p4 = P4_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p5 = P5_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p6 = P6_NoW3EnergyCosAsym1Cut_counts_pi0;

//       p0err = P0ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p1err = P1ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p2err = P2ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p3err = P3ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p4err = P4ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p5err = P5ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
//       p6err = P6ERR_NoW3EnergyCosAsym1Cut_counts_pi0;
    }
  else if (strcasecmp(cutname,"chisq1") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ChiSq1Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq1Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq1Cut_counts;
    }
  else if (strcasecmp(cutname,"chisq2") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ChiSq2Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq2Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq2Cut_counts;
    }
  else if (strcasecmp(cutname,"tof1chisq1") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq1ToF1Cut_counts;
    }
  else if (strcasecmp(cutname,"tof1chisq2") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq2ToF1Cut_counts;
    }
  else if (strcasecmp(cutname,"tof1") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ToF1Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ToF1Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ToF1Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ToF1Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ToF1Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ToF1Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ToF1Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ToF1Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ToF1Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ToF1Cut_counts;
    }
  else if (strcasecmp(cutname,"tof2chisq1") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts;
    }
  else if (strcasecmp(cutname,"tof2chisq2") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq2ToF2Cut_counts;

      p0 = P0_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p1 = P1_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p2 = P2_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p3 = P3_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p4 = P4_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p5 = P5_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p6 = P6_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;

      p0err = P0ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p1err = P1ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p2err = P2ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p3err = P3ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p4err = P4ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p5err = P5ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;
      p6err = P6ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_pi0;

//       p0 = P0_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p1 = P1_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p2 = P2_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p3 = P3_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p4 = P4_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p5 = P5_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p6 = P6_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;

//       p0err = P0ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p1err = P1ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p2err = P2ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p3err = P3ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p4err = P4ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p5err = P5ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;
//       p6err = P6ERR_NoW3EnergyCosAsym1ChiSq2ToF2Cut_counts_pi0;

    }
  else if (strcasecmp(cutname,"tof2") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ToF2Cut_counts;
//       p1 = P1_BasicCuts5x5Asym1ToF2Cut_counts;
//       p2 = P2_BasicCuts5x5Asym1ToF2Cut_counts;
//       p3 = P3_BasicCuts5x5Asym1ToF2Cut_counts;
//       p4 = P4_BasicCuts5x5Asym1ToF2Cut_counts;

//       p0err = P0ERR_BasicCuts5x5Asym1ToF2Cut_counts;
//       p1err = P1ERR_BasicCuts5x5Asym1ToF2Cut_counts;
//       p2err = P2ERR_BasicCuts5x5Asym1ToF2Cut_counts;
//       p3err = P3ERR_BasicCuts5x5Asym1ToF2Cut_counts;
//       p4err = P4ERR_BasicCuts5x5Asym1ToF2Cut_counts;
    }
  else if (strcasecmp(cutname,"asym2_chisq2") == 0)
    {
    }
  else if (strcasecmp(cutname,"asym2_noPID") == 0)
    {
    }
  else if (strcasecmp(cutname,"asym2_tof1chisq2") == 0)
    {
    }
  else if (strcasecmp(cutname,"asym2_tof1") == 0)
    {
    }
  else
    {
      cout << " <W> No efficiency parameters for cut: " << cutname << " !!" << endl;

      p0=0;
      p0err=0;
      p1=0;
      p1err=0;
      p2=0;
      p2err=0;
      p3=0;
      p3err=0;
      p4=0;
      p4err=0;
      p5=0;
      p5err=0;
      p6=0;
      p6err=0;
    }
  } // pi0

  //_____________________________________________________________________________

  else if (strcasecmp(partic,"eta") == 0)
  {
  if (strcasecmp(cutname,"3x3noPID") == 0)
    {
//       p0 = P0_BasicCuts3x3Asym1Cut_counts_eta;
//       p1 = P1_BasicCuts3x3Asym1Cut_counts_eta;
//       p2 = P2_BasicCuts3x3Asym1Cut_counts_eta;
//       p3 = P3_BasicCuts3x3Asym1Cut_counts_eta;
//       p4 = P4_BasicCuts3x3Asym1Cut_counts_eta;

//       p0err = P0ERR_BasicCuts3x3Asym1Cut_counts_eta;
//       p1err = P1ERR_BasicCuts3x3Asym1Cut_counts_eta;
//       p2err = P2ERR_BasicCuts3x3Asym1Cut_counts_eta;
//       p3err = P3ERR_BasicCuts3x3Asym1Cut_counts_eta;
//       p4err = P4ERR_BasicCuts3x3Asym1Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"3x3tof1chisq1") == 0)
    {
//       p0 = P0_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p1 = P1_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p2 = P2_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p3 = P3_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p4 = P4_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;

//       p0err = P0ERR_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p1err = P1ERR_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p2err = P2ERR_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p3err = P3ERR_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
//       p4err = P4ERR_BasicCuts3x3Asym1ChiSq1ToF1Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"3x3tof2chisq1") == 0)
    {
//       p0 = P0_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p1 = P1_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p2 = P2_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p3 = P3_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p4 = P4_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;

//       p0err = P0ERR_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p1err = P1ERR_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p2err = P2ERR_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p3err = P3ERR_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
//       p4err = P4ERR_BasicCuts3x3Asym1ChiSq1ToF2Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"5x5tof2chisq1") == 0)
    {
//       p0 = P0_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p1 = P1_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p2 = P2_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p3 = P3_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p4 = P4_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;

//       p0err = P0ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p1err = P1ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p2err = P2ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p3err = P3ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
//       p4err = P4ERR_BasicCuts5x5Asym1ChiSq1ToF2Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"chisq1") == 0)
    {
//       p0 = P0_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p1 = P1_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p2 = P2_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p3 = P3_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p4 = P4_BasicCutsAsym1ChiSq1Cut_counts_eta;

//       p0err = P0ERR_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p1err = P1ERR_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p2err = P2ERR_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p3err = P3ERR_BasicCutsAsym1ChiSq1Cut_counts_eta;
//       p4err = P4ERR_BasicCutsAsym1ChiSq1Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"chisq2") == 0)
    {
//       p0 = P0_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p1 = P1_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p2 = P2_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p3 = P3_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p4 = P4_BasicCutsAsym1ChiSq2Cut_counts_eta;

//       p0err = P0ERR_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p1err = P1ERR_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p2err = P2ERR_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p3err = P3ERR_BasicCutsAsym1ChiSq2Cut_counts_eta;
//       p4err = P4ERR_BasicCutsAsym1ChiSq2Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"tof1chisq2") == 0)
    {
//       p0 = P0_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p1 = P1_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p2 = P2_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p3 = P3_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p4 = P4_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;

//       p0err = P0ERR_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p1err = P1ERR_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p2err = P2ERR_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p3err = P3ERR_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
//       p4err = P4ERR_BasicCutsAsym1ChiSq2ToF1Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"tof1") == 0)
    {
//       p0 = P0_BasicCutsAsym1ToF1Cut_counts_eta;
//       p1 = P1_BasicCutsAsym1ToF1Cut_counts_eta;
//       p2 = P2_BasicCutsAsym1ToF1Cut_counts_eta;
//       p3 = P3_BasicCutsAsym1ToF1Cut_counts_eta;
//       p4 = P4_BasicCutsAsym1ToF1Cut_counts_eta;

//       p0err = P0ERR_BasicCutsAsym1ToF1Cut_counts_eta;
//       p1err = P1ERR_BasicCutsAsym1ToF1Cut_counts_eta;
//       p2err = P2ERR_BasicCutsAsym1ToF1Cut_counts_eta;
//       p3err = P3ERR_BasicCutsAsym1ToF1Cut_counts_eta;
//       p4err = P4ERR_BasicCutsAsym1ToF1Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"tof2chisq2") == 0)
    {
      p0 = P0_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p1 = P1_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p2 = P2_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p3 = P3_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p4 = P4_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p5 = 0;
      p6 = 0;
      //p5 = P5_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      //p6 = P6_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;

      p0err = P0ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p1err = P1ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p2err = P2ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p3err = P3ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p4err = P4ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      p5err = 0;
      p6err = 0;
      //p5err = P5ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
      //p6err = P6ERR_BasicCuts3x3Asym1ChiSq2ToF2Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"tof2") == 0)
    {
//       p0 = P0_BasicCutsAsym1ToF2Cut_counts_eta;
//       p1 = P1_BasicCutsAsym1ToF2Cut_counts_eta;
//       p2 = P2_BasicCutsAsym1ToF2Cut_counts_eta;
//       p3 = P3_BasicCutsAsym1ToF2Cut_counts_eta;
//       p4 = P4_BasicCutsAsym1ToF2Cut_counts_eta;

//       p0err = P0ERR_BasicCutsAsym1ToF2Cut_counts_eta;
//       p1err = P1ERR_BasicCutsAsym1ToF2Cut_counts_eta;
//       p2err = P2ERR_BasicCutsAsym1ToF2Cut_counts_eta;
//       p3err = P3ERR_BasicCutsAsym1ToF2Cut_counts_eta;
//       p4err = P4ERR_BasicCutsAsym1ToF2Cut_counts_eta;
    }
  else if (strcasecmp(cutname,"asym2_chisq2") == 0)
    {
    }
  else if (strcasecmp(cutname,"asym2_noPID") == 0)
    {
    }
  else if (strcasecmp(cutname,"3x3asym0tof1chisq1") == 0)
    {
//       p0 = P0_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p1 = P1_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p2 = P2_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p3 = P3_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p4 = P4_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;

//       p0err = P0ERR_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p1err = P1ERR_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p2err = P2ERR_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p3err = P3ERR_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;
//       p4err = P4ERR_BasicCuts3x3Asym2ChiSq1ToF1Cut_counts_eta;

    }
  else if (strcasecmp(cutname,"asym2_tof1") == 0)
    {
    }
  else
    {
      cout << " <W> No efficiency parameters for cut: " << cutname << " !!" << endl;

      p0=0;
      p0err=0;
      p1=0;
      p1err=0;
      p2=0;
      p2err=0;
      p3=0;
      p3err=0;
      p4=0;
      p4err=0;
    }
  }

}

//_____________________________________________________________________________
double
getEnScaleRelError( const double pT )
{
  
  // 3. Energy scale, (pT smearing, power-law weights). [CCC] [basically PTC]
  // From studies with 6%,7%,8% constant term:
  // double EnScaleRelatError = 0.02; // (only accounts for differences in effic.) 
  // From Sasha Bazilevsky studies:
  
  //double EnScaleRelatError = 0.001+2.906e-2*pT-3.886e-3*pT*pT+2.302e-4*pT*pT*pT-4.906e-6*pT*pT*pT*pT;
  
  // New Sasha's parameterization: EnScaleError = (new EnScaleError)**2+(EnLinearityError)**2
  
  // Energy global scale error
  
  double EnScaleRelatError = 0.0077+1.52056e-02*pT-1.21385e-03*pT*pT+3.19973e-05*pT*pT*pT;
  
  // Energy linearity error
  
  double EnLinearityRelatError = -0.030165+1.44612e-02*pT-3.09512e-04*pT*pT;
  
  EnScaleRelatError = quadRelatError( EnScaleRelatError,1.,
				      EnLinearityRelatError,1.);
    
  return EnScaleRelatError;

}

//_____________________________________________________________________________
double
getCentCorrelatRelError( const double pT )
{

  // Errors that cancel in ratios of centralities:
  // [CCC] = "Centrality Class Correlated error"

  // 3. Energy scale, (pT smearing, power-law weights). [CCC] [basically PTC]

  double EnScaleRelatError = getEnScaleRelError(pT);
  
  // 4. Fiducial + DeadWarn + Acceptance cuts  [CCC] [PTC]
  // Based on the difference between real and merged dead-map application
  // Difference between PISA and Objectivity geometries + Acceptance fit

  double FiduDeadWarnPlusAccRelatError = 0.05;

  // 6. General acceptance (fit): ridiculously small now

  double AccfitRelatError = acceptance(pT,"error")/acceptance(pT,"value");

  double CCCRelatError = quadRelatError( EnScaleRelatError,1.,
					 FiduDeadWarnPlusAccRelatError,1.,
					 AccfitRelatError, 1.);
  
  return CCCRelatError;
}

//_____________________________________________________________________________
double
getpTCorrelatRelError( const double pT )
{

  // Errors that move consistently up&down all the spectrum.
  // [PTC] = "pT correlated error"
  
  // 3. Energy scale, (pT smearing, power-law weights). [CCC] [basically PTC]

  double EnScaleRelatError = getEnScaleRelError(pT);

  // 4. Fiducial + DeadWarn + Acceptance cuts  [CCC] [PTC]
  // Based on the difference between real and merged dead-map application
  // Difference between PISA and Objectivity geometries + Acceptance fit

  double FiduDeadWarnPlusAccRelatError = 0.05 ;  
  
  // 5. PID (Chi2*TOF) cut (we consider this for eta only !)   [non CCC] [PTC]
  double PIDRelatError = 0.08; // constant with pT and centrality

  double PTCRelatError = quadRelatError( EnScaleRelatError,1.,
					 FiduDeadWarnPlusAccRelatError,1.);
  
  return PTCRelatError;

}

//_____________________________________________________________________________
void
AddOrSubstractRelatErrors( TGraphErrors *g,
			   TString error_type,
			   TString option,
			   const int verbose )
{

  if (!g)
    {
      cout << " <W> AddOrSubstractRelatErrors: null pointer to TGraphErrors" << endl;
      return;
    }

  if ( !error_type.Contains("pT") && !error_type.Contains("CC") && 
       !error_type.Contains("RAA") && !error_type.Contains("pp") || error_type.Sizeof()>4 )
    {
      cout << " <E> AddOrSubstractRelatErrors: Don't know this error type  \"" 
	   << error_type << "\"" << endl;
      cout << "     Valid error types: \"pT\" : pT correlated errors" << endl;
      cout << "                        \"CC\" : CC dependent errors" << endl;
      cout << "                        \"RAA\": AuAu/pp cancelling errors" << endl;
      return;
    }

  if ( !option.Contains("+") && !option.Contains("-") )
    {
      cout << " <E> AddOrSubstractRelatErrors: Don't know this option  " << option << endl;
      cout << "     Valid options: \"+-\" : Add/subtract error" << endl;
      return;
    }

  if (verbose) cout << endl;
  if (verbose>1) g->Print();

  double x,y;
  double ex,ey;
  double eAddOrSubRelat;
  double eyrelat = 0.;
  int N = g->GetN();

  double control = 0.;
  double FiduDeadWarnPlusAccRelatError = 0.05;
  double Acceptance_pp_error = 0.038;

  for ( int i = 0; i < N; i++ ) 
    {
      g->GetPoint(i,x,y);
      ex = g->GetErrorX(i); 
      ey = g->GetErrorY(i);
      eyrelat = ey/y;

      double e0 = eyrelat;
      eAddOrSubRelat = 0.;

      if ( error_type.Contains("CC") ) //  CC correlated errors
	{
	  eAddOrSubRelat = getCentCorrelatRelError(x);

	  //eAddOrSubRelat = getEnScaleRelError(x);
	  //eAddOrSubRelat *= eAddOrSubRelat ;
	  //eAddOrSubRelat += FiduDeadWarnPlusAccRelatError*FiduDeadWarnPlusAccRelatError;
	  //eAddOrSubRelat = Sqrt(eAddOrSubRelat);
	}
      else if ( error_type.Contains("RAA") ) // reduced case (for AuAu/pp ratio): 
	{
	  //eAddOrSubRelat = getpTCorrelatRelError(x);
	  //eAddOrSubRelat = getEnScaleRelError(x); //energy scale error alone
	  eAddOrSubRelat = FiduDeadWarnPlusAccRelatError; // only 5% FiduDeadWarnPlusAccRelatError 
	  //eAddOrSubRelat = 0.; // none subtracted ...
	}
      else if ( error_type.Contains("pT") ) // pT correlated errors
	{
	  eAddOrSubRelat = getpTCorrelatRelError(x);
	}

      else if ( error_type.Contains("pp") ) // reduced case (for AuAu/pp ratio): 
	{
	  eAddOrSubRelat = Acceptance_pp_error; // 
	}

      control += abs(eAddOrSubRelat);

      if ( option.Contains("-") ) // we substract the error
	{
	  eyrelat = eyrelat*eyrelat - eAddOrSubRelat*eAddOrSubRelat;
	  if (eyrelat<0)
	    {
	      cout << " <E>  AddOrSubstractRelatErrors: New error negative ?!?!" << endl;
	    }
	  else
	    {
	      eyrelat = Sqrt(eyrelat);
	    }
	}
      if ( option.Contains("+") ) // we add the error
	{
	  eyrelat = quadRelatError(eyrelat,1.,eAddOrSubRelat,1.);
	}

      if (verbose)
	{
	  printf(" <I> pT = %.2f Error modified (%s)  from: ey = %.2f%%  to ---> %.2f%%\n",
		 x,option.Data(),e0*100.,eyrelat*100.);
	}

      eyrelat *= y;

      g->SetPointError(i,ex,eyrelat);
    }

  if (verbose>1) g->Print();

  if (verbose)
    {
      cout << " <I> AddOrSubstractRelatErrors for graph: " << g->GetName()
	   << " -- Average added/substracted " << error_type << " relat error = " 
	   << option << 100.*control/N << "%" << endl;
    }
}

//_____________________________________________________________________________
void
dump(const char* cut,const char* partic)
{

  double* p0;
  double* p0err;
  double* p1;
  double* p1err;
  double* p2;
  double* p2err;
  double* p3;
  double* p3err;
  double* p4;
  double* p4err;
  double* p5;
  double* p5err;
  double* p6;
  double* p6err;

  cout << " <I> Correction Factors for cut: " << cut << endl;
  getParamsPointers(cut,p0,p0err,p1,p1err,p2,p2err,p3,p3err,p4,p4err,p5,p5err,p6,p6err,partic);
  
  if ( !p0 ) 
    {
      cerr << " <E> No Correction Factors known for this cut" << endl;      
    }
  else
    {
      for ( size_t i=0; i < 11; ++i ) // FIXME: 11 ??
	{
	  printf(" <I> Cent=%2d p0=%7.4f +- %7.4f p1=%7.4f +- %7.4f p2=%7.4f +- %7.4f ",
		     i,    p0[i],p0err[i],   p1[i],p1err[i],   p2[i],p2err[i]);
	  printf("p3=%7.4f +- %7.4f p4=%7.4f +- %7.4f p5=%7.4f +- %7.4f p6=%7.4f +- %7.4f \n",
		  p3[i],p3err[i],   p4[i],p4err[i],
		  p5[i],p5err[i],   p6[i],p6err[i]);
	}
    }

}

//_____________________________________________________________________________
double
corrFactor( const double pT, 
	    const int CentClass, 
	    const char *cut,
	    const char *type,
	    const char *partic)
{
  if (CentClass == -1) return -1;

  double err_corr_factor = 0;

  if (strcasecmp(type,"error") == 0) 
    {
      err_corr_factor = 
	Power(acceptance(pT,"error")*efficiency(pT,CentClass,cut,"value",partic)*offVertex("value"),2);
      err_corr_factor += 
	Power(acceptance(pT,"value")*efficiency(pT,CentClass,cut,"error",partic)*offVertex("value"),2);
      err_corr_factor += 
	Power(acceptance(pT,"value")*efficiency(pT,CentClass,cut,"value",partic)*offVertex("error"),2);
      err_corr_factor = Sqrt(err_corr_factor);
      return err_corr_factor;
    }
  else
    {
      return acceptance(pT,"value")*efficiency(pT,CentClass,cut,"value",partic)*offVertex("value");
    }
}

} // end of namespace
