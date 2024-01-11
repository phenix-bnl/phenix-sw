#include <iostream>
#include <iomanip>
#include <fstream>
#include "TMath.h"
#include "TF1.h"
#include "TH2.h"
#include "TClonesArray.h"
#include "TStyle.h"
#include "TString.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TLatex.h"
#include "TLegend.h"
#include "TPaveText.h"

using namespace std;

// Important macro includes

static const Int_t CentClasses = 20;

//#include "Efficiency_8percent_counts.h" // <--- Input Efficiencies
//#include "Efficiency_6percent_counts.h" // <--- Input Efficiencies
#include "Efficiency_7percent_counts.h" // <--- Input Efficiencies

#include "Ncollisions.h"
#include "pi0_RAA.h"


static const Double_t ADDITIONAL_DEADWARN_EFF_LOSS = 1.; // This is now taken into account in the Efficiency factors
//static const Double_t ADDITIONAL_DEADWARN_EFF_LOSS = 0.60/0.75; // -20% additional eff. loss for deadwarn cut wrongly computed

static const Int_t MaxPTbins = 22;//18;

static const Double_t Events[CentClasses] = { 3148148./2., 3148148./2.,  //  0-10
					      3177042./2., 3177042./2.,  // 10-20
					      3129548./2., 3129548./2.,  // 20-30
					      3139715./2., 3139715./2.,  // 30-40
					      3181711./2., 3181711./2.,  // 40-50
					      3158935./2., 3158935./2.,  // 50-60
					      3181306./2., 3181306./2.,  // 60-70
					      3118054./2., 3118054./2.,  // 70-80
					      3547648.  ,                // 80-92,
					      3181306.+3118054.+3547648.,// 60-92
					      //3181306.+3118054.,         // 60-80
					      3181306.,         // 60-80
					      28781108.};                // min.bias

//const Double_t mean_pt_corr = 0.04;
const Double_t mean_pt_corr[MaxPTbins] = { 1.18, 1.69, 2.20, 2.70, 3.21, 3.71, 4.22, 4.72, 5.22, 
					   5.72, 6.22, 6.73, 7.23, 7.73, 8.23, 8.73, 9.23, 9.73,
					   10.23, 10.73, 11.23, 11.73};

const Double_t twoPi = 2.*TMath::Pi();
const Double_t pt_binwidth = 0.5 ;

// Global Hagedorn parameters

Double_t nHag[CentClasses] ;
Double_t AHag[CentClasses] ;

// Transverse energies
static const Double_t ET[CentClasses]     = { 574., 467.,
					      382., 312.,
					      254., 206.,
					      165., 131.,
					      102.,  78.,
					      0,0,0,0,0,0,0,0,0,0};

static const Double_t eET[CentClasses]    = { 26., 22.,
					      18., 16.,
					      14., 12.,
					      11., 10.,
					       8.,  7.,
					      0,0,0,0,0,0,0,0,0,0};

//const char* data_tables_location = "/home/enterria/wrk/CORRECTED_RUN2_FINAL/tables_new/";
//const char* data_tables_location = "/home/enterria/wrk/CORRECTED_RUN2_FINAL/tables/";
//const char* data_tables_location = "/afs/rhic/phenix/users/enterria/CORRECTED_RUN2/tables/";
const char* data_tables_location = "/afs/rhic/phenix/users/enterria/tables_new/";

//_____________________________________________________________________________
Int_t getCentralityClass(const Int_t Cent1, const Int_t Cent2)
{
  //CentClass =  0 :  0- 5%
  //CentClass =  1 :  5-10%
  //CentClass =  2 : 10-15%
  //CentClass =  3 : 15-20%
  //CentClass =  4 : 20-25%
  //CentClass =  5 : 25-30%
  //CentClass =  6 : 30-35%
  //CentClass =  7 : 35-40%
  //CentClass =  8 : 40-45%
  //CentClass =  9 : 45-50%
  //CentClass = 10 : 50-55%
  //CentClass = 11 : 55-60%
  //CentClass = 12 : 60-65%
  //CentClass = 13 : 65-70%
  //CentClass = 14 : 70-75%
  //CentClass = 15 : 75-80%
  //CentClass = 16 : 80-92%    <-- special case
  //CentClass = 17 : 60-92%    <-- special case
  //CentClass = 18 : 60-80%    <-- special case
  //CentClass = 19 : min.bias  <-- special case

  Int_t CC = Cent1/5;

  if ( (CC>20) || (CC<0) || (Cent1>Cent2) )
    {
      cout << " <E> I don't know this Centrality Class :" << Cent1 << " - " << Cent2 << "%" << endl;
      return -1;
    }
  if ( (Cent1==100) || (Cent2==100) ) // min. bias (let's consider that's what the user want ...)
    {
      //cout << " <W> I will assume you are interested in min.bias centrality ..." << endl;
      return 19;
    }
  if ( (Cent1==60) && (Cent2==80) ) // 60-80%
    {
      return 18;
    }
  if ( (Cent1==60) && (Cent2==92) ) // 60-92%
    {
      return 17;
    }
  if ( (Cent1==80) && (Cent2==92) ) // 80-92%
    {
      return 16;
    }
  if ( (Cent1!=Cent2-5) && (Cent1!=Cent2-10) )
    {
      cout << " <E> I don't know this Centrality Class :" << Cent1 << " - " << Cent2 << "%" << endl;
      return -1;
    }

  return CC;

}

//_____________________________________________________________________________
Double_t getEvents(const Int_t Cent1, const Int_t Cent2)
{
  Int_t CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  if ( (Cent1 == Cent2-5) || (CC == 16) || (CC == 17) || 
                             (CC == 18) || (CC == 19) ) // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
    {
      return Events[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      return Events[CC]+Events[CC+1];
    }
  else return -1.;
}

//_____________________________________________________________________________
Double_t getEvents_relat_error(const Int_t Cent1, const Int_t Cent2)
{
  Int_t CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  if ( (Cent1 == Cent2-5) || (CC == 16) || (CC == 17) || 
                             (CC == 18) || (CC == 19) ) // case 0-5, 15-20 ... or special cases: 60-80, min. bias
    {
      return 1./TMath::Sqrt(Events[CC]);
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      return 1./TMath::Sqrt(Events[CC]+Events[CC+1]);
    }
  else return -1.;
}

//_____________________________________________________________________________
Double_t getNcoll(const Int_t Cent1, const Int_t Cent2, const char *value="Ncoll")
{
  Int_t CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  if ( (Cent1 == Cent2-5) || (CC == 16) || (CC == 17) || 
                             (CC == 18) || (CC == 19) ) // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
    {
      if (strcmp(value,"Npart") == 0) return Npart[CC];
      return Ncoll[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (strcmp(value,"Npart") == 0) return (Npart[CC]+Npart[CC+1])/2;
      return (Ncoll[CC]+Ncoll[CC+1])/2;
    }

  else return -1.;
}

//_____________________________________________________________________________
// the original errors are percentual (%) !

Double_t get_relat_errNcoll(const Int_t Cent1, const Int_t Cent2, const char *value="Ncoll")
{
  Int_t CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  if ( (Cent1 == Cent2-5) || (CC == 16) || (CC == 17) || 
                             (CC == 18) || (CC == 19) ) // case 0-5, 15-20 ... or special cases: 60-80, min. bias
    {
      if (strcmp(value,"Npart") == 0) return eNpart_relat[CC]/100.; // (%) --> relat. error
      return eNcoll_relat[CC]/100.; // (%) --> relat. error
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (strcmp(value,"Npart") == 0) return (eNpart_relat[CC]+eNpart_relat[CC+1])/200.; // (%) --> relat. error
      return (eNcoll_relat[CC]+eNcoll_relat[CC+1])/200.; // (%) --> relat. error
    }
  else return -1.;
}

//_____________________________________________________________________________
Double_t Efficiency(const Double_t pT, 
		    const Int_t CentClass, 
		    const char* cut="noPID",
		    const char* Value="value")
{

  TFormula* EffFit = new TFormula("EffFit","([0]*x*x+[1]*x+[2])*(1.0-exp([3]*x+[4]))");

  if (strcmp(cut,"noPID") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1Cut_counts[CentClass],
			     P1_BasicCutsAsym1Cut_counts[CentClass],
			     P2_BasicCutsAsym1Cut_counts[CentClass],
			     P3_BasicCutsAsym1Cut_counts[CentClass],
			     P4_BasicCutsAsym1Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"chisq1") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ChiSq1Cut_counts[CentClass],
			     P1_BasicCutsAsym1ChiSq1Cut_counts[CentClass],
			     P2_BasicCutsAsym1ChiSq1Cut_counts[CentClass],
			     P3_BasicCutsAsym1ChiSq1Cut_counts[CentClass],
			     P4_BasicCutsAsym1ChiSq1Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"chisq2") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ChiSq2Cut_counts[CentClass],
			     P1_BasicCutsAsym1ChiSq2Cut_counts[CentClass],
			     P2_BasicCutsAsym1ChiSq2Cut_counts[CentClass],
			     P3_BasicCutsAsym1ChiSq2Cut_counts[CentClass],
			     P4_BasicCutsAsym1ChiSq2Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"tof1chisq1") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ChiSq1ToF1Cut_counts[CentClass],
			     P1_BasicCutsAsym1ChiSq1ToF1Cut_counts[CentClass],
			     P2_BasicCutsAsym1ChiSq1ToF1Cut_counts[CentClass],
			     P3_BasicCutsAsym1ChiSq1ToF1Cut_counts[CentClass],
			     P4_BasicCutsAsym1ChiSq1ToF1Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"tof1chisq2") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ChiSq2ToF1Cut_counts[CentClass],
			     P1_BasicCutsAsym1ChiSq2ToF1Cut_counts[CentClass],
			     P2_BasicCutsAsym1ChiSq2ToF1Cut_counts[CentClass],
			     P3_BasicCutsAsym1ChiSq2ToF1Cut_counts[CentClass],
			     P4_BasicCutsAsym1ChiSq2ToF1Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"tof1") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ToF1Cut_counts[CentClass],
			     P1_BasicCutsAsym1ToF1Cut_counts[CentClass],
			     P2_BasicCutsAsym1ToF1Cut_counts[CentClass],
			     P3_BasicCutsAsym1ToF1Cut_counts[CentClass],
			     P4_BasicCutsAsym1ToF1Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"tof2chisq1") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ChiSq1ToF2Cut_counts[CentClass],
			     P1_BasicCutsAsym1ChiSq1ToF2Cut_counts[CentClass],
			     P2_BasicCutsAsym1ChiSq1ToF2Cut_counts[CentClass],
			     P3_BasicCutsAsym1ChiSq1ToF2Cut_counts[CentClass],
			     P4_BasicCutsAsym1ChiSq1ToF2Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"tof2chisq2") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ChiSq2ToF2Cut_counts[CentClass],
			     P1_BasicCutsAsym1ChiSq2ToF2Cut_counts[CentClass],
			     P2_BasicCutsAsym1ChiSq2ToF2Cut_counts[CentClass],
			     P3_BasicCutsAsym1ChiSq2ToF2Cut_counts[CentClass],
			     P4_BasicCutsAsym1ChiSq2ToF2Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"tof2") == 0)
    {
      EffFit->SetParameters( P0_BasicCutsAsym1ToF2Cut_counts[CentClass],
			     P1_BasicCutsAsym1ToF2Cut_counts[CentClass],
			     P2_BasicCutsAsym1ToF2Cut_counts[CentClass],
			     P3_BasicCutsAsym1ToF2Cut_counts[CentClass],
			     P4_BasicCutsAsym1ToF2Cut_counts[CentClass]);
    }
  else if (strcmp(cut,"asym2_chisq2") == 0)
    {
    }
  else if (strcmp(cut,"asym2_noPID") == 0)
    {
    }
  else if (strcmp(cut,"asym2_tof1chisq2") == 0)
    {
    }
  else if (strcmp(cut,"asym2_tof1") == 0)
    {
    }

//   TFormula* ErrEffFit = new TFormula("ErrEffFit","([0]*x*x+[1]*x+[2])*(1.0-exp([3]*x+[4]))");

//   ErrEffFit->SetParameters( P0ERR_BasicCuts_counts[CentClass],P1ERR_BasicCuts_counts[CentClass],
// 			    P2ERR_BasicCuts_counts[CentClass],
// 			    P3ERR_BasicCuts_counts[CentClass],P4ERR_BasicCuts_counts[CentClass]);

  Double_t Effic = EffFit->Eval(pT); 
  if (pT>7. && (strcmp(Value,"value")==0) ) 
    { 
      cout << "<I> Effic. for pT = " << pT << " is = " << Effic;
    }

  Double_t Effic_abs_error = 0;   //ErrEffFit->Eval(pT);

  // Systematic Error Calculation
  // -1. Raw pi0 extraction procedure = 10% (according to Saskia)
  Double_t ExtractRaw_relat_error = 0.15 ;
  // 0. The error that comes straight from the efficiency calculation is just the extraction window one

  //  Double_t ExtractWindow_relat_error = Effic_abs_error/Effic ;
  Double_t ExtractWindow_relat_error = 0.06 ;

  //cout << "Relat error: " << ExtractWindow_relat_error << endl;

  // 1. pT smearing, power-law weights, ...: 
  //Double_t pTsmearing_Syst_relat_error = 0.5*( (1.159+0.01398*pT)-1.);  // From Laurent studies with 8%+-4% constant term
  // 2. LowEnergy and Asymmetry Cuts
  //Double_t LowEnergy_Asymetry_Syst_relat_error = 0.03 ;  // More work to be done from data after QM02
  // 3. Fiducial and DeadWarn cuts
  //Double_t Fiducial_DeadWarn_Syst_relat_error = 0.05 ;  // Based on the difference between DeadMap at Subatech and BNL
  // 4. Chi2 cut 
  //Double_t Chi2_Syst_relat_error = 0.05; // More work to be done from data after QM02. This upper limit is good enough 
  // 5. TOF cut
  //Double_t TOF_Syst_relat_error = 0.10; // According to Klaus TOF-cut on/off studies
  // 6. Acceptance Systematic Error
  //Double_t Acceptance_Syst_relat_error = 0.04; // 4% difference between PISA and Objectivity geometries

  // For info: Other errors outside efficiency that add up in the correction factors:
  // 7. General acceptance (fit): See Acceptance() function below
  // 8. Off-vtx contribution : See NonVertexCorrection() function below

  Double_t Syst_relat_error = TMath::Sqrt(ExtractRaw_relat_error*ExtractRaw_relat_error                     +                    
					  ExtractWindow_relat_error*ExtractWindow_relat_error);

					  //pTsmearing_Syst_relat_error*pTsmearing_Syst_relat_error                 +
					  //LowEnergy_Asymetry_Syst_relat_error*LowEnergy_Asymetry_Syst_relat_error +
					  //Fiducial_DeadWarn_Syst_relat_error*Fiducial_DeadWarn_Syst_relat_error   +
					  //Chi2_Syst_relat_error*Chi2_Syst_relat_error                             + 
					  //TOF_Syst_relat_error*TOF_Syst_relat_error                               +
					  //Acceptance_Syst_relat_error*Acceptance_Syst_relat_error                 );

  Effic_abs_error = Syst_relat_error*Effic;

  if (strcmp(Value,"error") == 0) 
    return Effic_abs_error;
  else
    return Effic;
}

//_____________________________________________________________________________
Double_t get_centrality_indep_relat_errors( const Double_t pT )
{

  return 0;
  // Error calculations due to cancelation effects
  // 2. LowEnergy and Asymmetry Cuts
  Double_t LowEnergy_Asymetry_Syst_relat_error = 0.03 ;  // More work to be done from data after QM02
  // 3. Fiducial and DeadWarn cuts
  Double_t Fiducial_DeadWarn_Syst_relat_error = 0.05 ;  // Based on the difference between DeadMap at Subatech and BNL
  // 6. Acceptance Systematic Error
  Double_t Acceptance_Syst_relat_error = 0.04; // 4% difference between PISA and Objectivity geometries
  // 5. TOF cut
  Double_t TOF_Syst_relat_error = 0.10; // According to Klaus TOF-cut on/off studies

  Double_t Cancelled_Syst_relat_error = TMath::Sqrt(LowEnergy_Asymetry_Syst_relat_error*LowEnergy_Asymetry_Syst_relat_error +
						   Fiducial_DeadWarn_Syst_relat_error*Fiducial_DeadWarn_Syst_relat_error   +
						   Acceptance_Syst_relat_error*Acceptance_Syst_relat_error                 +
						   TOF_Syst_relat_error*TOF_Syst_relat_error                                );
  
  return Cancelled_Syst_relat_error;
}

//_____________________________________________________________________________
Double_t Acceptance(const Double_t pT, const char *Value="value")
{
  // Acceptance Parametrization PHENIX Analysis Note an115
  // Accep = (A+BpT) * (1.0 - exp(-a-b*pT))
  // Accep_error = A_error/A *Accep  1
  //    1  p0           2.31982e-01   2.72676e-03  -0.00000e+00  -5.28800e-04
  //    2  p1           2.53766e-03   3.15332e-04   0.00000e+00  -4.90148e-03
  //    3  p2          -4.40588e-02   3.92561e-02  -0.00000e+00   2.75295e-05
  //    4  p3           9.57877e-01   5.20808e-02   0.00000e+00   1.11556e-05

  Double_t Accep = 0.;  
  Double_t A = 0.232;   
  Double_t B = 0.00254;
  Double_t a = -0.044;  
  Double_t b = 0.958;   

  Double_t Accep_error = 0.;
  Double_t A_error = 0.0027;
  Double_t a_error = 0.039;
  Double_t b_error = 0.04;
  Double_t abpT_error = 0.;

  Accep = (A +B*pT)* (1.0 - TMath::Exp(-a-b*pT));

  if (strcmp(Value,"error") == 0) 
    {
      abpT_error  = TMath::Sqrt(a_error*a_error+b_error*b_error*pT*pT);
      Accep_error = TMath::Sqrt( TMath::Power(A_error/A*Accep,2) +
				 TMath::Power(A*TMath::Exp(-a-b*pT)*b*abpT_error,2) );
      return Accep_error;
    }
  else 
    {
      return Accep;
    }
  
}

//_____________________________________________________________________________
Double_t Acceptance_old(const Double_t pT,const char *Value="value")
{
  // Acceptance Parametrization PHENIX Analysis Note 115
  // Accep = A * (1.0 - exp(-a-b*pT))
  // Accep_error = A_error/A *Accep 
  Double_t Accep;     Double_t Accep_error;
  Double_t A=0.255;   Double_t A_error=0.002;
  Double_t a=0.46;    Double_t a_error=0.07;
  Double_t b=0.49;    Double_t b_error=0.04;
  Accep = A * (1.0 - TMath::Exp(-a-b*pT));

  if (strcmp(Value,"error") == 0) 
    { 
      Double_t abpT_error= TMath::Sqrt(a_error*a_error+b_error*b_error*pT*pT);
      Accep_error = TMath::Sqrt( TMath::Power(A_error/A*Accep,2) +
				 TMath::Power(A*TMath::Exp(-a-b*pT)*b*abpT_error,2) );
      return Accep_error;
    }
  else 
    { 
      return Accep;
    }

}

//_____________________________________________________________________________
//Double_t NonVertexCorrection(const Double_t pT, Int_t const CentClass, const char *Value="value")
Double_t NonVertexCorrection(const char *Value="value")
{
  if (strcmp(Value,"error") == 0) 
    return 0.03; // off-vtx pions are +3% - +6%
  else 
    return 0.94; // off-vtx pions are +3% - +6% . Let's take the upper value
}


//_____________________________________________________________________________
Double_t get_pTcorr_relat_errors( const Double_t pT )
{

  return 0;

  // Error calculations due to cancelation effects
  // 2. LowEnergy and Asymmetry Cuts
  Double_t LowEnergy_Asymetry_Syst_relat_error = 0.03 ;  // More work to be done from data after QM02
  // 3. Fiducial and DeadWarn cuts
  Double_t Fiducial_DeadWarn_Syst_relat_error = 0.05 ;  // Based on the difference between DeadMap at Subatech and BNL
  // 4. Chi2 cut 
  Double_t Chi2_Syst_relat_error = 0.05; // More work to be done from data after QM02. This upper limit is good enough 
  // 6. Acceptance Systematic Error
  Double_t Acceptance_Syst_relat_error = 0.04; // 4% difference between PISA and Objectivity geometries

  // 7. General acceptance (fit):
  Double_t Acceptance_fit_relat_error = Acceptance(pT,"error");

  // 8. Off-vtx contribution : See NonVertexCorrection() function below
  Double_t OffVtxPi0_relat_error = NonVertexCorrection("error");
  
  Double_t pTcorr_relat_error = TMath::Sqrt(LowEnergy_Asymetry_Syst_relat_error*LowEnergy_Asymetry_Syst_relat_error +
					   Fiducial_DeadWarn_Syst_relat_error*Fiducial_DeadWarn_Syst_relat_error   +
					   Chi2_Syst_relat_error*Chi2_Syst_relat_error                             +
					   Acceptance_Syst_relat_error*Acceptance_Syst_relat_error                 +
					   Acceptance_fit_relat_error*Acceptance_fit_relat_error                   +
					   OffVtxPi0_relat_error*OffVtxPi0_relat_error                              );
  
  return pTcorr_relat_error;
}


//_____________________________________________________________________________
Double_t CorrectionFactor(const Double_t pT, const Int_t CentClass, 
			  const char *cut="noPID",
			  const char *Value="value")
{
  if (CentClass == -1) return -1;

  Double_t err_corr_factor = 0;

  if (strcmp(Value,"error") == 0) 
    {
      err_corr_factor = 
	TMath::Power(Acceptance(pT,"error")*Efficiency(pT,CentClass,cut,"value")*NonVertexCorrection("value"),2);
      err_corr_factor += 
	TMath::Power(Acceptance(pT,"value")*Efficiency(pT,CentClass,cut,"error")*NonVertexCorrection("value"),2);
      err_corr_factor += 
	TMath::Power(Acceptance(pT,"value")*Efficiency(pT,CentClass,cut,"value")*NonVertexCorrection("error"),2);
      err_corr_factor = TMath::Sqrt(err_corr_factor);
      return err_corr_factor;
    }
  else
    {
      return Acceptance(pT,"value")*Efficiency(pT,CentClass,cut,"value")*NonVertexCorrection("value");
    }
}

//_____________________________________________________________________________
Int_t plot_Correction_Factors( const Int_t Cent1 = 0, const Int_t Cent2 = 10,
			       const char* cut = "noPID" )
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TCanvas *c0 = 0 ;
  char titleglobal[300];
  if (Cent1<100) sprintf(titleglobal,"Centrality Class %d_%d",Cent1,Cent2);
  else sprintf(titleglobal,"Centrality Class: min. bias");

  Double_t corr_factor[MaxPTbins];
  Double_t err_corr_factor[MaxPTbins];
  Double_t err_corr_factor_min[MaxPTbins];
  Double_t err_corr_factor_max[MaxPTbins];
  Double_t pT[MaxPTbins];
  Double_t epT[MaxPTbins];

  Int_t CC = getCentralityClass(Cent1,Cent2);

  if ( CC == -1) return -1;

  Int_t NumPTbins[CentClasses] = { 10, 13, //  0- 5,  5-10 // 14:  0-10
				   16, 14, // 10-15, 15-20 // 14: 10-20
				   14, 14, // 20-30
				   14, 14, // 30-40
				   14, 14, // 40-50
				   12, 12, // 50-60
				   11, 11, // 60-70
				   11, 11, // 70-80
				    9,  // 80-92, 
				   14,  // 60-92, 
				   14,  // 60-80
				   18}; // min. bias

  cout << "Plotting Correction Factors for CentClass: " << CC << endl; // No difference for X-(X+10)% or X-(X+5)% classes

  Int_t NpTbins = NumPTbins[CC]; 
  if ( (Cent1 == 0 || Cent1 == 10 ) && (Cent2 == 10 || Cent2 == 20 ) ) 
    {
      NpTbins = 14; // Those 2 mixed classes have longer ranges
    }

  Double_t pTstep = 0.75;

  for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
    { 
      //pTstep += pt_binwidth - mean_pt_corr;
      pTstep = mean_pt_corr[pTbin];
      pT[pTbin] = pTstep;
      epT[pTbin] = 0.050 ; // const pT bin width = 0.05 GeV/c
      corr_factor[pTbin] = 1/(CorrectionFactor(pTstep,CC,cut,"value"));
      err_corr_factor[pTbin] = (CorrectionFactor(pTstep,CC,cut,"error"))*corr_factor[pTbin]*corr_factor[pTbin];
      err_corr_factor_min[pTbin] = corr_factor[pTbin]-err_corr_factor[pTbin];
      err_corr_factor_max[pTbin] = corr_factor[pTbin]+err_corr_factor[pTbin];
      cout << pTstep << " " << corr_factor[pTbin] << endl;
    }

  char title0[100];
  //sprintf(title0," Full Correction Factors for Cent. Class %i_%i",Cent1,Cent2);
  sprintf(title0,"Efficiency#times Acceptance Correction Factors");
  char title[100];
  sprintf(title,"corr_factors_centclass_%i_%i",Cent1,Cent2);
  c0 = new TCanvas(title,title,600,600);

  //TGraphErrors *CorrFactPi0 = new TGraphErrors( NpTbins, pT, corr_factor, epT, err_corr_factor);
  TGraph *CorrFactPi0 = new TGraph( NpTbins, pT, corr_factor);
  TGraph *CorrFactPi0_min = new TGraph( NpTbins, pT, err_corr_factor_min);
  TGraph *CorrFactPi0_max = new TGraph( NpTbins, pT, err_corr_factor_max);

  TF1 *fitmin = new TF1("fitmin","pol7", 1.0, pTstep+0.2);
  CorrFactPi0_min->Fit("fitmin","QR","");
  TF1 *fitmax = new TF1("fitmax","pol7", 1.0, pTstep+0.2);
  CorrFactPi0_max->Fit("fitmax","QR","");

  Double_t width_frame = TMath::Abs(1.2*pTstep);
  Int_t width_frame2 = (Int_t)(2*width_frame);
  //Double_t height_frame = TMath::Abs(1.2*err_corr_factor_max[0]);
  //TH2F *frame = new TH2F("frame","frame",width_frame2,0.,width_frame,height_frame,5,height_frame);
  TH2F *frame = new TH2F("frame","frame",width_frame2,0.,width_frame,25,5,30);
  frame->SetStats(0);
  frame->SetTitle(titleglobal);
  frame->SetYTitle(title0);
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->Draw();
  
  TLegend *le = new TLegend(4.4,18.85,11.37,28.8," #pi^{0} spectra correction factors:","brNDC");
  le->AddEntry(frame,"<Acceptance> #sim 1/23.0%","P");
  le->AddEntry(frame,"<Efficiency> #sim 1/20.0% (low p_{T}) - 1/35.0% (high p_{T} ","P");
  le->AddEntry(frame,"<Off-vertex #pi^{0}: #sim -6.0%","P");
  le->SetFillColor(kWhite);
  le->Draw();
  fitmax->SetFillColor(2);
  fitmax->SetFillStyle(3006);
  fitmax->SetLineColor(28);
  fitmax->SetLineWidth(3);
  fitmax->Draw("FCsame");

  fitmin->SetFillColor(10);
  fitmin->SetLineColor(28);
  fitmin->SetLineWidth(3);
  fitmin->Draw("FCsame");
  frame->Draw("sameaxis");
  CorrFactPi0->SetMarkerStyle(8);
  CorrFactPi0->Draw("P");
  //c0->SetLogy();
  c0->Update();

  return 0;
}

//_____________________________________________________________________________
// OPTIONS:
//          "raw"    = raw spectra (no correction)
//          "acc"    = acceptance-corrected spectra
//          "acceff" = acceptance & efficiency -corrected spectra
//          "full"   = fully corrected spectra
//
//          cut = "noPID"
//          cut = "chisq1"
//          cut = "chisq2"
//          cut = "tof1chisq1"
//          cut = "tof1chisq2"
//          cut = "tof1"
//          cut = "tof2chisq1"
//          cut = "tof2chisq2"
//          cut = "tof2"


TGraphErrors* plot_pi0_spectra_run2( const Int_t Cent1 = 0, const Int_t Cent2 = 10, 
				     const char *cut="tof1chisq2", 
				     const char *spectra="full", 
				     const char *plot="yes")
{

  Int_t long_output = 0;

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Int_t CC = getCentralityClass(Cent1,Cent2);
  Double_t eventsCC = getEvents(Cent1,Cent2);
  Double_t eventsCC_relat_error = getEvents_relat_error(Cent1,Cent2);

  TGraphErrors *problem = 0;
  if ( CC == -1) return problem;

  cout << " Calculating " << spectra << "-corrected pi0 spectrum for centrality class: " << CC 
       << " (" << Cent1 << " - " << Cent2 << "%)" << endl;
  cout << " Total # of events in centrality class: " << eventsCC << " +/- " << eventsCC_relat_error*eventsCC << endl;

  char title[300];
  sprintf(title,"Centrality Class %d_%d",Cent1,Cent2);
  //if (Cent1==100) sprintf(title,"Centrality Class: min. bias");

  //_____________________________________________________________________________
  
  char ffile[300];
  char name[300]; 
  char name1[300]; 
  char name2[300]; 
  char name3[300]; 

  sprintf(name,"yields_%d_%d_%s",Cent1,Cent2,cut);

  sprintf(name1,"yield_raw_pure_cent%d_%d_%s",Cent1,Cent2,cut);
  sprintf(name2,"yield_raw_acc_corr_cent%d_%d_%s",Cent1,Cent2,cut);
  sprintf(name3,"yield_raw_acceff_corr_cent%d_%d_%s",Cent1,Cent2,cut);

  sprintf(ffile,"%s%s.txt",data_tables_location,name);
  ifstream ifiledata(ffile);
  if (!ifiledata)
    {
      cout << " Can't open file: " << ffile << endl;
      cout << " Are you sure that the centrality class: " << Cent1 << " - " << Cent2 << "% exists ?" << endl;
      return 0;
    }
  else cout << " Reading " << ffile  << " ..." << endl;

  Double_t pTpizero[MaxPTbins];
  Double_t epTpizero[MaxPTbins];
  Double_t countspizero[MaxPTbins];
  Double_t ecountspizero[MaxPTbins];

  Int_t i = 0;
  // Read pT, countsPi0 from the file
  while ( ifiledata >> 
	  pTpizero[i] >> 
	  countspizero[i] >> 
	  ecountspizero[i]
	  ){ 
    cout << "  " << pTpizero[i]
 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
    i++;
  }
  ifiledata.close();

  const Int_t NpTbins = i;
   
  Double_t raw_yield[NpTbins];
  Double_t acc_corr_yield[NpTbins];
  Double_t acceff_corr_yield[NpTbins];
  Double_t fully_corr_yield[NpTbins];

  Double_t abs_error_raw_yield[NpTbins];
  Double_t abs_error_acc_corr_yield[NpTbins];
  Double_t abs_error_acceff_corr_yield[NpTbins];
  Double_t abs_error_fully_corr_yield[NpTbins];

  Double_t full_cross_sect_corr = 1.0; // 0.92 ; //only for min.-bias.

  Double_t tot_corr = 0.;
  Double_t pt_mean = 0.;
  Double_t efficiency = 0.;
  Double_t acceptance = 0.;
  Double_t nonvtx_corr = 0. ;

  Double_t abs_error_statistical = 0.;
  Double_t abs_error_efficiency = 0.;
  Double_t abs_error_acceptance = 0.;
  Double_t abs_error_nonvtx_corr = 0. ;
  Double_t abs_error_noncentrality_indep = 0;
  Double_t abs_error_centrality_indep = 0;

  cout << " Output format is: pT(GeV/c)  pTerror(GeV/c)  Npi0  Npi0_tot_error (Relat_error%) ";
  cout << "Stat_error CentClass_indep_error pT_correlated_error" << endl;

  for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
    { 

      pt_mean = mean_pt_corr[pTbin];
      pTpizero[pTbin] = pt_mean;
      epTpizero[pTbin] = 0.05 ; // const pT bin width = 50 MeV/c

      acceptance  = Acceptance(pt_mean,"value");
      efficiency  = Efficiency(pt_mean,CC,cut,"value");
      nonvtx_corr = NonVertexCorrection("value");

      abs_error_acceptance  = Acceptance(pt_mean,"error");
      abs_error_efficiency  = Efficiency(pt_mean,CC,cut,"error");
      abs_error_nonvtx_corr = NonVertexCorrection("error");

      // Raw spectrum (normalized) 
      raw_yield[pTbin] = countspizero[pTbin]/(eventsCC*pt_binwidth);
      abs_error_raw_yield[pTbin] = TMath::Sqrt( TMath::Power(ecountspizero[pTbin]/(eventsCC*pt_binwidth),2) +
						TMath::Power(eventsCC_relat_error*raw_yield[pTbin],2) ); // really small error but anyway ...

      // Acceptance-corrected raw spectrum
      acc_corr_yield[pTbin] = raw_yield[pTbin]/(acceptance*twoPi);
      abs_error_acc_corr_yield[pTbin] = TMath::Sqrt( TMath::Power(abs_error_raw_yield[pTbin]/(acceptance*twoPi),2) +
						     TMath::Power(abs_error_acceptance/acceptance*acc_corr_yield[pTbin],2) );
      // Acceptance+Efficiency-corrected raw spectrum
      acceff_corr_yield[pTbin] = acc_corr_yield[pTbin]/efficiency;
      abs_error_acceff_corr_yield[pTbin] = TMath::Sqrt( TMath::Power(abs_error_acc_corr_yield[pTbin]/efficiency,2) +
							TMath::Power(abs_error_efficiency/efficiency*acceff_corr_yield[pTbin],2) );
      // Full-corrected spectrum
      tot_corr = nonvtx_corr/pt_mean/full_cross_sect_corr;
      fully_corr_yield[pTbin] = acceff_corr_yield[pTbin]*tot_corr;
      abs_error_fully_corr_yield[pTbin] = TMath::Sqrt( TMath::Power(abs_error_acceff_corr_yield[pTbin]*tot_corr,2) +
						       // really small error but anyway ...
						       TMath::Power(epTpizero[pTbin]/pt_mean*fully_corr_yield[pTbin],2) + 
						       TMath::Power(abs_error_nonvtx_corr/nonvtx_corr*fully_corr_yield[pTbin],2) );

      cout << "  " << setprecision(4) << pt_mean << " " << epTpizero[pTbin] ;
      if (long_output) cout << " [" << setprecision(4) << countspizero[pTbin] << " +/- " << ecountspizero[pTbin];

      if (strcmp(spectra,"raw") == 0)  // raw
	{
	  cout << " --> " << setprecision(4) << raw_yield[pTbin] 
	       << " +/- " << abs_error_raw_yield[pTbin] << endl;
	}
      else
	{
	  if (long_output) cout << " Acc:" << setprecision(4) << acceptance;
	  if (strcmp(spectra,"acc") == 0) // acc
	    { 
	      cout << " --> " << setprecision(4) << acc_corr_yield[pTbin] 
		   << " +/- " << abs_error_acc_corr_yield[pTbin]  << endl;
	    }
	  else
	    {
	      if (long_output) cout << ", Eff:" << efficiency ;
	      if (long_output) cout << ", Acc*Eff: x" << 1/(acceptance*efficiency) << "]"; 
	      if (strcmp(spectra,"full") == 0) // full 
		{
		  float erelat = abs_error_fully_corr_yield[pTbin]/fully_corr_yield[pTbin];

		  abs_error_statistical = (ecountspizero[pTbin]/countspizero[pTbin])*fully_corr_yield[pTbin];

		  abs_error_noncentrality_indep = TMath::Sqrt( (erelat*erelat) - 
							       TMath::Power(get_centrality_indep_relat_errors(pTbin),2));
		  abs_error_noncentrality_indep = abs_error_noncentrality_indep*fully_corr_yield[pTbin] ;
		  abs_error_noncentrality_indep = 0;
		  abs_error_centrality_indep    = get_centrality_indep_relat_errors(pTbin)*fully_corr_yield[pTbin];

		  //float test = TMath::Sqrt(abs_error_noncentrality_indep*abs_error_noncentrality_indep +
		  //			   abs_error_centrality_indep*abs_error_centrality_indep);

		  float pTcorr_error =  get_pTcorr_relat_errors(pTbin)*fully_corr_yield[pTbin];

		  cout << " " << setprecision(4) << fully_corr_yield[pTbin] << " " << abs_error_fully_corr_yield[pTbin] 
		       << " " << erelat*100. << " "
		    //<< " Errors: Stat.: " 
		       << abs_error_statistical << " " 
		    //<< "  CentClass-indep.: " 
		       << abs_error_noncentrality_indep << " " 
		    //<< " Cancel.: " << abs_error_centrality_indep 
		    //<< "  pT-correlated: " 
		       << pTcorr_error <<  endl;
		    //<< " (" << (pTcorr_error/test)*100. << "%) " <<  endl;
		    //<< " Quad. sum: " << test <<  endl;

		}
	      else if  (strcmp(spectra,"acceff") == 0) // acceff
		{
		  cout << " --> " << setprecision(4) << acceff_corr_yield[pTbin] 
		       << " +/- " << abs_error_acceff_corr_yield[pTbin]  << endl;
		}
	    }
	}

    }
  
  //_____________________________________________________________________________

  TCanvas *c1 = 0; TGraphErrors *RawPi0 = 0;
  TCanvas *c2 = 0; TGraphErrors *AccCorrPi0 = 0;
  TCanvas *c3 = 0; TGraphErrors *AccEffCorrPi0 = 0;
  TCanvas *c4 = 0; TGraphErrors *FullCorrPi0 = 0;

  if (strcmp(spectra,"raw") == 0) 
    {
      c1 = new TCanvas(name1,name1,600,600);
      Double_t max_yield4 = 10.*raw_yield[0];
      Double_t min_yield4 = 0.1*raw_yield[NpTbins-1];
      
      RawPi0 = new TGraphErrors( NpTbins, pTpizero, raw_yield, epTpizero, abs_error_raw_yield);
      RawPi0->SetMaximum(max_yield4);
      RawPi0->SetMinimum(min_yield4);
      RawPi0->SetMarkerStyle(20);
      RawPi0->SetMarkerColor(2);
      //RawPi0->SetMarkerStyle(8);
      RawPi0->Draw("AP");
      RawPi0->SetTitle(title);
      RawPi0->GetYaxis()->SetTitle("NORMALIZED RAW dN^{#pi^{0}}/dp_{T}");
      RawPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
      c1->SetLogy();
      c1->Update();
    }

  //_____________________________________________________________________________
  
  if (strcmp(spectra,"acc") == 0) 
    {
      c2 = new TCanvas(name2,name2,600,600);
      AccCorrPi0 = new TGraphErrors( NpTbins, pTpizero, acc_corr_yield, epTpizero, abs_error_acc_corr_yield);
      Double_t max_yield2 = 10.*acc_corr_yield[0];
      Double_t min_yield2 = 0.1*acc_corr_yield[NpTbins-1];

      AccCorrPi0->SetMaximum(max_yield2);
      AccCorrPi0->SetMinimum(min_yield2);
      AccCorrPi0->SetMarkerStyle(20);
      AccCorrPi0->SetMarkerColor(2);
      AccCorrPi0->Draw("AP");
      AccCorrPi0->SetTitle(title);
      AccCorrPi0->GetYaxis()->SetTitleOffset(1.4);
      AccCorrPi0->GetYaxis()->SetTitle("ACCEPT. CORRECTED dN^{#pi^{0}}/dp_{T}");
      AccCorrPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
      c2->SetLogy();
      c2->Update();
    }

  //_____________________________________________________________________________
  
  if (strcmp(spectra,"acceff") == 0) 
    {
      c3 = new TCanvas(name3,name3,600,600);
      Double_t max_yield3 = 10.*acceff_corr_yield[0];
      Double_t min_yield3 = 0.1*acceff_corr_yield[NpTbins-1];
      
      AccEffCorrPi0 = new TGraphErrors( NpTbins, pTpizero, acceff_corr_yield, epTpizero, abs_error_acceff_corr_yield);
      AccEffCorrPi0->SetMaximum(max_yield3);
      AccEffCorrPi0->SetMinimum(min_yield3);
      AccEffCorrPi0->SetMarkerStyle(20);
      AccEffCorrPi0->SetMarkerColor(2);
      AccEffCorrPi0->Draw("AP");
      AccEffCorrPi0->GetYaxis()->SetTitleOffset(1.4);
      AccEffCorrPi0->GetYaxis()->SetTitle("ACC.+EFF. CORRECTED dN^{#pi^{0}}/dp_{T}");
      AccEffCorrPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
      AccEffCorrPi0->SetTitle(title);
      c3->SetLogy();
      c3->Update();
    }

  //_____________________________________________________________________________
  
  if (strcmp(spectra,"full") == 0) 
    {
      c4 = new TCanvas(name,name,600,600);
      FullCorrPi0 = new TGraphErrors( NpTbins, pTpizero, fully_corr_yield, epTpizero, abs_error_fully_corr_yield);
      Double_t max_yield = 10.*fully_corr_yield[0];
      Double_t min_yield = 0.1*fully_corr_yield[NpTbins-1];

      FullCorrPi0->SetMaximum(max_yield);
      FullCorrPi0->SetMinimum(min_yield);
      FullCorrPi0->SetMarkerStyle(20);
      FullCorrPi0->SetMarkerColor(2);
      FullCorrPi0->Draw("AP");
      FullCorrPi0->SetTitle(title);
      FullCorrPi0->GetYaxis()->SetTitleOffset(1.4);
      FullCorrPi0->GetYaxis()->SetTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
      FullCorrPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
      c4->SetLogy();
      c4->Update();
    }
 //_____________________________________________________________________________
  
  Double_t ptmin = 2.0;
  Double_t ptmax = pt_mean-0.5; // <-- modify this if no fit
  Double_t p0 = 1.72;
  Double_t n_exp = 10.5;
  TF1 *hagedorn = new TF1("hagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn->SetParameters(10000., p0, n_exp);
  hagedorn->SetParLimits(0, 1, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  //hagedorn->SetParLimits(1, 1.9, 3.36);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);
  //hagedorn->SetParLimits(2, 9.408,9.408); 
  hagedorn->SetParNames("A Hag","p_o", "n");
  hagedorn->SetLineWidth(1);
  hagedorn->Draw("same");

  // Fitting to Hagedorn
  if (strcmp(spectra,"raw") == 0) 
    {   
      c1->Update();
      RawPi0->Fit("hagedorn","QR","");
    }
  if (strcmp(spectra,"acc") == 0)
    {
      c2->Update();
      AccCorrPi0->Fit("hagedorn","QR","");
    }
  if (strcmp(spectra,"acceff") == 0)
    {
      c3->Update();
      AccEffCorrPi0->Fit("hagedorn","QR","");
    }
  if (strcmp(spectra,"full") == 0)
    {
      c4->Update();
      FullCorrPi0->Fit("hagedorn","QR");//,"");
    }

  cout << "Fit result (pT = " << ptmin << " - " << ptmax << " GeV/c)" << endl;
  AHag[CC] = hagedorn->GetParameter(0) ;   cout << "AHag =" << AHag[CC] << endl ;
  p0       = hagedorn->GetParameter(1) ;   cout << "p0   =" << p0 << endl;
  nHag[CC] = hagedorn->GetParameter(2) ;   cout << "n    =" << nHag[CC] << endl;
  cout << "Chi2/Ndf  =" << hagedorn->GetChisquare()/hagedorn->GetNumberFitPoints() << endl;
  hagedorn->Draw("same");

  if (strcmp(spectra,"raw") == 0) 
    {   
      c1->Update();
      if (strcmp(plot,"no") == 0) c1->Close();
      return RawPi0;
    }
  if (strcmp(spectra,"acc") == 0)
    {
      c2->Update();
      if (strcmp(plot,"no") == 0) c2->Close();
      return AccCorrPi0;
    }
  if (strcmp(spectra,"acceff") == 0)
    {
      c3->Update();
      if (strcmp(plot,"no") == 0) c3->Close();
      return AccCorrPi0;
    }
  if (strcmp(spectra,"full") == 0)
    {
      c4->Update();
      if (strcmp(plot,"no") == 0) c4->Close();
      return FullCorrPi0;
    }


//   c1->SetLogy();
//   FullCorrPi0->Draw("A");
  
//    cout << "p_0/n (expo slope )= " << p_0/n << " GeV/c " << endl;
//    cout << "1/n  (power slope )= " << 1/n << " GeV/c " << endl;
//    cout << "<pT> = " << 2*p_0/(n-3) << "  GeV/c "  << endl;


//    TF1 *hagedorn2 = new TF1("gamma",gamma, ptmin, ptmax,4);
// // Sets initial values and parameter names
//    hagedorn2->SetParameters(2/(n-1),p_0,A_Hag,n);
//    //hagedorn2->Draw("Same");

   // Try the same for pure power-law

//    TF1 *powlaw = new TF1("powlaw","[0]/x^[1]", ptmin, ptmax);
//    powlaw->SetParameters(200.0, 7.0);
//    powlaw->SetParLimits(1, 1.0,20.);
//    powlaw->SetParNames("A","expo");

//    FullCorrPi0->Fit("powlaw","QR","");
//    Double_t A, expo ; 
//    cout << "Fit result: " << endl;
//    A   = powlaw->GetParameter(0) ;   cout << "A powlaw      =" << A << endl ;
//    expo= powlaw->GetParameter(1) ;   cout << "expo powlaw   =" << expo << endl;
//    cout << "Chi2/Ndf  =" << powlaw->GetChisquare()/powlaw->GetNumberFitPoints() << endl;
//    powlaw->SetLineColor(2);
//    powlaw->Draw("same");

  return 0;
} 

//_____________________________________________________________________________
// OPTIONS: "wa98_pi0_cen.dat" = WA98 central PbPb->pi0
//          "yields_pp_sasha.txt"   = pp->pi0 PHENIX (preliminary)
//          "yields_pp_hisa_tbl_c22mbc_tot.txt"   = pp->pi0 PHENIX (high-pT trigger)
//          "yields_pp_hisa_tbl_mbc_tot.txt"   = pp->pi0 PHENIX (high-pT trigger)
//          "pi0_ms_pbgl_minb.dat"  = AuAu->pi0 PHENIX PbGl
//          "pi0_ms_pbgl_00-10.dat"  = AuAu->pi0 PHENIX PbGl
//          "pi0_ms_pbgl_70-80.dat" = AuAu->pi0 PHENIX PbGl
//          "picharged.txt" = AuAu->pi+- PHENIX 
//          "PizeroAuAu130_cent.dat" = AuAu->pi0 130 GeV
//          "pi0_pbsc_XX_YY.txt" = AuAu->pi0 200 GeV
//
//          type: "pT" (default) or "xT"
//

TGraphErrors* plot_xx_pi0_spectra(const char *spectra = "yields_pp_hisa_tbl_c22mbc_tot.txt", 
				  const char *type = "pT", const Double_t scale = 1. )
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  cout << " Calculating full-corrected ("<< spectra << ") " << type << " pi0 spectrum " << endl;

  TString spectra_string = spectra;
  TString type_string = type;

  //_____________________________________________________________________________

  static const Int_t MaxLocalPTbins = 50;
  Double_t pTpizero[MaxLocalPTbins];
  Double_t epTpizero[MaxLocalPTbins];
  Double_t countspizero[MaxLocalPTbins];
  Double_t ecountspizero[MaxLocalPTbins];
  Double_t esyspizero[MaxLocalPTbins];
  Double_t bidon[MaxLocalPTbins];

  Double_t sqrt_s = 0;

  Int_t i = 0;

  char ffile[300];
  sprintf(ffile,"%s%s",data_tables_location,spectra);
  ifstream ifiledata(ffile);
  if (!ifiledata)
    {
      cout << " Can't open file: " << ffile << endl;
      return 0;
    }
  else cout << " Reading " << ffile  << " ..." << endl;

  // Read pT, countsPi0 from the file
  char name[300]; 
  char title[300];
  if (spectra_string.Contains("pi0_pbsc",TString::kExact)) // PHENIX Run-2
    {
      // Let's skip the first 4 lines
      for (Int_t l=0;l<4;l++) ifiledata.ignore(1000,'\n'); // Skip 1000 characters or until end-of-line 
      sprintf(name,spectra);
      sprintf(title,"PHENIX Run-2 AuAu #rightarrow #pi^{0} 200 GeV");
      sqrt_s = 200;

      if (spectra_string.Contains("pi0_pbsc_comb",TString::kExact)) // PHENIX Run-2
	{
	  while ( ifiledata >> 
		  pTpizero[i] >> 
		  epTpizero[i] >> 
		  countspizero[i] >> 
		  ecountspizero[i] >>
		  bidon[i] >>
		  bidon[i] >>
		  bidon[i] 
		  ){i++;}
	}
      else
	{
	  while ( ifiledata >> 
		  pTpizero[i] >> 
		  epTpizero[i] >> 
		  countspizero[i] >> 
		  ecountspizero[i] >>
		  bidon[i] >>
		  bidon[i] >>
		  bidon[i] >>
		  bidon[i] 
		  ){ 
	    //cout << "  " << pTpizero[ii] //<< endl;
	    //    << "  " << fully_corr_yield[ii][i] << "  %error: " << efully_corr_yield_relat[ii][i] << endl;
	    i++;
	  }
	}
    }

  if (spectra_string.Contains("wa98",TString::kExact)) // WA98
    {
      sprintf(name,"yields_wa98");
      sprintf(title,"WA98 PbPb #rightarrow #pi^{0} 17 GeV");
      sqrt_s = 17.4;
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      countspizero[i] >> 
	      epTpizero[i] >>
	      ecountspizero[i]
	      ){ 
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	i++;
      }
    }
  else if ( (spectra_string.Contains("sasha",TString::kExact)) || // PHENIX pp sasha
	    (spectra_string.Contains("charged",TString::kExact)) ) // PHENIX piminus
    {
      sprintf(name,"yields_pp");
      sprintf(title,"PHENIX pp #rightarrow #pi^{#pm} 200 GeV");
      sqrt_s = 200.;
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      countspizero[i] >> 
	      ecountspizero[i]
	      ){ 
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	epTpizero[i] = 0.05 ; // const pT bin error
	i++;
     }
    }
  else if (spectra_string.Contains("130",TString::kExact)) // PHENIX AuAu 130
    {
      sprintf(name,"AuAu_130");
      sprintf(title,"PHENIX AuAu #rightarrow #pi^{0} 130 GeV");
      sqrt_s = 130.;
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      epTpizero[i] >>
	      countspizero[i] >> 
	      ecountspizero[i]
	      ){ 
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	epTpizero[i] = 0.05 ; // const pT bin error
	i++;
     }
   }
  else if (spectra_string.Contains("hisa",TString::kExact)) // PHENIX pp hisa
    {
      //Double_t pp_luminosity_relat_error = 0.20; // Additional 20% error on pp yield (luminosity)
      sprintf(name,"pp_200");
      sprintf(title,"PHENIX pp #rightarrow #pi^{0} 200 GeV");
      sqrt_s = 200.;
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      bidon[i] >>
	      countspizero[i] >> 
	      ecountspizero[i] >>
	      esyspizero[i]
	      ){ 
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	epTpizero[i] = 0.05 ; // const pT bin error
	ecountspizero[i] = TMath::Sqrt(//TMath::Power(pp_luminosity_relat_error*countspizero[i],2)+
				       TMath::Power(ecountspizero[i],2)+
				       TMath::Power(esyspizero[i],2));
	i++;
      }
   }
  else if (spectra_string.Contains("pbgl",TString::kExact)) // PHENIX PbGl
    {
      sprintf(name,"pbgl");
      sprintf(title,"PHENIX AuAu #rightarrow #pi^{0} 200 GeV (PbGl)");
      sqrt_s = 200.;
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      //epTpizero[i] >>
	      countspizero[i] >> 
	      ecountspizero[i]
	      ){ 
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	epTpizero[i] = 0.05 ; // const pT bin error
	i++;
     }
//   else // AuAu/pp PHENIX 200
//     {
//       sprintf(name,"ratio");
//       sprintf(title,"PHENIX #pi^{0} AuAu/pp 200 GeV");
//       sqrt_s = 200.;
//       while ( ifiledata >> 
// 	      pTpizero[i] >> 
// 	      //epTpizero[i] >>
// 	      countspizero[i] >> 
// 	      ecountspizero[i]
// 	      ){ 
// 	//     cout << "  " << pTpizero[i]
// 	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
// 	epTpizero[i] = 0.05 ; // const pT bin error
// 	i++;
//      }
    }

  ifiledata.close();

  const Int_t NpTbins = i;
   
  if (type_string.Contains("xT",TString::kExact)) // xT axis instead of pT
    {
      for (Int_t i = 0; i < NpTbins ; i++) 
	{
	  pTpizero[i] = 2.*pTpizero[i]/sqrt_s;
	  epTpizero[i] = 2.*epTpizero[i]/sqrt_s;
	}
    }

  Double_t corr_yield[NpTbins];
  Double_t ecorr_yield[NpTbins];

  for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
    { 
      // Full corrected pp --> pi0 or WA98 pi0
      corr_yield[pTbin]  = scale*TMath::Abs(countspizero[pTbin]);
      ecorr_yield[pTbin] = scale*TMath::Abs(ecountspizero[pTbin]); 

      if (pTpizero[pTbin]==0) pTpizero[pTbin] = 0.1; 
      if (corr_yield[pTbin]==0) corr_yield[pTbin] = 1.;

      cout << "  " << setprecision(4) << pTpizero[pTbin] << "+/-" << epTpizero[pTbin] ;
      cout << "  " << setprecision(4) << countspizero[pTbin] << " +/- " << ecountspizero[pTbin];
      cout << " --> " << setprecision(4) << corr_yield[pTbin] << " +/- " <<  ecorr_yield[pTbin] << endl;

    }
  
  //_____________________________________________________________________________

  TCanvas *c1 = 0; 
  TGraphErrors *xxPi0 = 0;
  c1 = new TCanvas(name,name,600,600);
  Double_t max_yield4 = 10.*TMath::Max(corr_yield[0],corr_yield[1]);
  Double_t min_yield4 = 0.1*corr_yield[NpTbins-1];
  
  xxPi0 = new TGraphErrors( NpTbins, pTpizero, corr_yield, epTpizero, ecorr_yield);
  xxPi0->SetMaximum(max_yield4);
  xxPi0->SetMinimum(min_yield4);
  xxPi0->SetMarkerStyle(20);
  xxPi0->SetMarkerColor(2);
  //xxPi0->SetMarkerStyle(8);
  xxPi0->Draw("AP");
  xxPi0->SetTitle(title);
  xxPi0->GetYaxis()->SetTitleOffset(1.25);
  xxPi0->GetYaxis()->SetTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  xxPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
  c1->SetLogy();
  if (type_string.Contains("xT",TString::kExact))
    {
      c1->SetLogx();
      xxPi0->GetYaxis()->SetTitle("Invariant #pi^{0} yield");// (GeV/c)^{-2}");
      xxPi0->GetXaxis()->SetTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
      xxPi0->GetXaxis()->SetRangeUser(0.01,1.); 
    }
  c1->Update();

 //_____________________________________________________________________________
  
  //  return xxPi0;

  Double_t ptmin = 2.0;
  Double_t ptmax;
  if (spectra_string.Contains("wa98",TString::kExact)) 
    {
      ptmax = pTpizero[NpTbins-3];
    }
  else
    {
      ptmax = pTpizero[NpTbins]; // play with this value in case of no fit ...
    }
  ptmax = 10.;
  cout << "Fitting spectrum to Hagedorn between " << type << "= " << ptmin << " - " << ptmax << " GeV/c" << endl;

  Double_t p0 = 1.72;
  Double_t n_exp = 10.5;
  TF1 *hagedorn = new TF1("hagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn->SetParameters(10000., p0, n_exp);
  hagedorn->SetParLimits(0, 0.0001, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  //hagedorn->SetParLimits(1, 1.9, 3.36);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);
  //hagedorn->SetParLimits(2, 9.408,9.408); 
  hagedorn->SetParNames("A Hag","p_o", "n");
  hagedorn->SetLineWidth(1);
  hagedorn->Draw("same");

  c1->Update();
  xxPi0->Fit("hagedorn","QR","");

  cout << "Fit result: " << endl;
  AHag[18] = hagedorn->GetParameter(0) ;   cout << "AHag =" << AHag[18] << endl ;
  p0       = hagedorn->GetParameter(1) ;   cout << "p0   =" << p0 << endl;
  nHag[18] = hagedorn->GetParameter(2) ;   cout << "n    =" << nHag[18] << endl;
  cout << "Chi2/Ndf  =" << hagedorn->GetChisquare()/hagedorn->GetNumberFitPoints() << endl;
  hagedorn->Draw("same");
  c1->Update();

  return xxPi0;

} 

//_____________________________________________________________________________
// Returns Axel Drees interpolated pp spectrum at 130 GeV 
// 
TH1 *plot_interpolated_pp_spectrum_130( const char *type = "pT", const Double_t scale = 1.)
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TString type_string = type;
  TCanvas *c4 = 0; 
  TF1 *haged = 0;

  Double_t ptmin = 1.;
  Double_t ptmax = 5.;
  Double_t A = scale*330; // mb/(GeV/c)^2
  Double_t p0 = 1.72;
  Double_t n = 12.4;
  Double_t sqrt_s = 130.;
  Double_t pt2xt = 2./sqrt_s;

  c4 = new TCanvas("pp_130","pp_130",600,600);
  c4->SetLogy();

  haged = new TF1("haged","[0]/((1+(x/[1]))^[2])", ptmin, ptmax);
  haged->SetParameters(A, p0, n);
  haged->SetLineWidth(1);  
  haged->Draw("");

  if (type_string.Contains("xT",TString::kExact)) // xT axis instead of pT
    {
      haged->GetHistogram()->GetXaxis()->SetLimits(pt2xt*ptmin, pt2xt*ptmax);  
      haged->GetHistogram()->Draw("");
      c4->SetLogx();
    }

  c4->Update();

  //  if (strcmp(plot,"no") == 0) c4->Close();

  return haged->GetHistogram();

}

//_____________________________________________________________________________
Int_t suppress_vs_centrality()
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Int_t logy = 0 ; // Log-y scale yes/no

  Int_t MaxCent = 9;
  Double_t max_RAA = 1.4;
  if (logy) max_RAA = 2.5;
  Double_t min_RAA = 0.;
  if (logy) min_RAA = 0.1;

  Double_t CentClass[MaxCent];
  Double_t eCentClass[MaxCent];

  Double_t RAA_pT3[MaxCent];
  Double_t eRAA_pT3[MaxCent];
  Double_t RAA_pT5[MaxCent];
  Double_t eRAA_pT5[MaxCent];
  Double_t RAA_pT7[MaxCent];
  Double_t eRAA_pT7[MaxCent];

  TCanvas *c1 = 0;
  TGraphErrors *Suppr_vs_Cent_pT3 = 0;
  TGraphErrors *Suppr_vs_Cent_pT5 = 0;
  TGraphErrors *Suppr_vs_Cent_pT7 = 0;

  char name1[50];
  char name2[50];
  char name3[50];
  sprintf(name1,"suppr_vs_centClass_pT3");
  sprintf(name2,"suppr_vs_centClass_pT5");
  sprintf(name3,"suppr_vs_centClass_pT7");

  static const Int_t MaxLocalPTbins = 25;
  Double_t pTpizero[MaxLocalPTbins];
  Double_t pizero_ratio[MaxLocalPTbins];
  Double_t epizero_ratio[MaxLocalPTbins];
  Double_t eNcollpizero_ratio[MaxLocalPTbins];

  // Separated Ncoll errors drawn as functions
  Double_t eNcoll_min3[MaxCent];
  Double_t eNcoll_max3[MaxCent];
  Double_t eNcoll_min5[MaxCent];
  Double_t eNcoll_max5[MaxCent];
  Double_t eNcoll_min7[MaxCent];
  Double_t eNcoll_max7[MaxCent];

  
  Int_t Cent1=0;
  Int_t Cent2=10;
  Int_t CCprime = 0;

  for (Int_t CC = 0; CC<MaxCent*2; CC=CC+2)
    {

      cout << " Calculating ratio AuAu/pp vs. CentClass for centrality class: " << CC 
	   << " (" << Cent1 << " - " << Cent2 << "%)" << endl;

      char title[300];
      char ffile[300];
      char name[300]; 
      sprintf(title,"Ratio AuAu/pp for centrality Class %d_%d",Cent1,Cent2);
      sprintf(name,"R_AA_%d_%d",Cent1,Cent2);

      // suppression vs. Npart
      //sprintf(name,"R_AA_%d_%d_part",Cent1,Cent2); Int_t Ncoll_pp = 2;

      sprintf(ffile,"%s%s.txt",data_tables_location,name);
      
      ifstream ifiledata(ffile);
      if (!ifiledata)
	{
	  cout << " Can't open file: " << ffile << endl;
	  return 0;
	}
      else cout << " Reading " << ffile  << " ..." << endl;
      
      Int_t i = 0;
      // Read pT, countsPi0 from the file
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      //epTpizero[i] >>
	      pizero_ratio[i] >> 
	      epizero_ratio[i] >>
	      eNcollpizero_ratio[i]
	      ){
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	pizero_ratio[i] *= Ncoll_pp;
	epizero_ratio[i] *= Ncoll_pp;
	eNcollpizero_ratio[i] *= Ncoll_pp;
	i++;
      }

      CentClass[CCprime] = 95-Cent1;
      eCentClass[CCprime] = 0.;//5.;

      // suppression factor      
      RAA_pT3[CCprime] = pizero_ratio[2]; // pT = 3 GeV/c
      //if (RAA_pT3[CCprime]>1.5) RAA_pT3[CCprime]=1.4;
      cout << "Suppression at " <<  pTpizero[2] << " GeV/c: " << RAA_pT3[CCprime] << endl;
      eRAA_pT3[CCprime] = epizero_ratio[2];
      eNcoll_min3[CCprime] = RAA_pT3[CCprime] - eNcollpizero_ratio[2]; // pT = 3 GeV/c
      eNcoll_max3[CCprime] = RAA_pT3[CCprime] + eNcollpizero_ratio[2]; // pT = 3 GeV/c
      if (eNcoll_max3[CCprime]>max_RAA) eNcoll_max3[CCprime] = max_RAA;

      RAA_pT5[CCprime] = pizero_ratio[6]; // pT = 5 GeV/c
      cout << "Suppression at " << pTpizero[6] << "GeV/c: " << RAA_pT5[CCprime] << endl;
      eRAA_pT5[CCprime] = epizero_ratio[6];
      eNcoll_min5[CCprime] = RAA_pT5[CCprime] - eNcollpizero_ratio[6]; // pT = 5 GeV/c
      eNcoll_max5[CCprime] = RAA_pT5[CCprime] + eNcollpizero_ratio[6]; // pT = 5 GeV/c
      if (eNcoll_max5[CCprime]>max_RAA) eNcoll_max5[CCprime] = max_RAA;

      Int_t max_pTbin = 10;
      RAA_pT7[CCprime] = pizero_ratio[max_pTbin]; // pT = 7 GeV/c
      cout << "Suppression at " << pTpizero[10] << " GeV/c: " << RAA_pT7[CCprime] << endl;
      eRAA_pT7[CCprime] = epizero_ratio[max_pTbin];
      eNcoll_min7[CCprime] = RAA_pT7[CCprime] - eNcollpizero_ratio[max_pTbin]; // pT = 7 GeV/c
      eNcoll_max7[CCprime] = RAA_pT7[CCprime] + eNcollpizero_ratio[max_pTbin]; // pT = 7 GeV/c
      if (eNcoll_max7[CCprime]>max_RAA) eNcoll_max5[CCprime] = max_RAA;

//       Int_t max_pTbin = 12;
//       if (max_pTbin<=i) 
// 	{
// 	  RAA_pT7[CCprime] = pizero_ratio[max_pTbin]; // pT = 7 GeV/c
// 	  cout << "Suppression at " << pTpizero[i] << " GeV/c: " << RAA_pT7[CCprime] << endl;
// 	  eRAA_pT7[CCprime] = epizero_ratio[max_pTbin];
// 	  eNcoll_min7[CCprime] = RAA_pT7[CCprime] - eNcollpizero_ratio[max_pTbin]; // pT = 7 GeV/c
// 	  eNcoll_max7[CCprime] = RAA_pT7[CCprime] + eNcollpizero_ratio[max_pTbin]; // pT = 7 GeV/c
// 	}

      Cent1 = Cent1+10;
      Cent2 = Cent1+10; if (Cent2 == 90) Cent2 = 92;
      cout << "CCprime = " << CCprime << endl;
      CCprime++;
    }

  Suppr_vs_Cent_pT3 = new TGraphErrors( MaxCent, CentClass, RAA_pT3, eCentClass, eRAA_pT3);
  Suppr_vs_Cent_pT5 = new TGraphErrors( MaxCent, CentClass, RAA_pT5, eCentClass, eRAA_pT5);
  Suppr_vs_Cent_pT7 = new TGraphErrors( MaxCent, CentClass, RAA_pT7, eCentClass, eRAA_pT7);

  c1 = new TCanvas(name1,name1,620,600);
  c1->ToggleEventStatus();
  c1->Range(-11.2,-0.16,100.567,1.5);
  c1->SetFillColor(0);
  c1->SetBorderMode(0);
  c1->SetBorderSize(0);
  c1->SetRightMargin(0.00503356);
  c1->SetTopMargin(0.012);
  c1->SetFrameFillColor(0);
  c1->SetFrameBorderMode(0);
  if (logy) c1->SetLogy();

  TH2F *frame = new TH2F(name1,name1,20,0.,100.,10,min_RAA,max_RAA);
  //frame->LabelsOption(">","X");
  frame->SetStats(0);
  frame->SetTitle(name1);
  frame->SetYTitle("R_{AA} Nuclear modification factor");
  //frame->SetYTitle("R_{AA} Suppression factor (N_{part} scaling)");
  frame->GetYaxis()->SetTitleOffset(1.);
  frame->GetYaxis()->SetTitleSize(0.05);
  frame->SetXTitle("Centrality Class (%)");
  frame->GetXaxis()->SetTitleOffset(1.);
  frame->GetXaxis()->SetTitleSize(0.05);
  //frame->GetXaxis()->SetDrawOption("<");
  //frame->GetXaxis()->LabelsOption(">");
  frame->Draw();

  TBox *pave = new TBox(30.2587,0.00763156,50.1312,1.09513);//,0,"br");
  pave->SetFillColor(10);
  pave->SetFillStyle(3005);
  pave->Draw();

  // decreasing labels
  frame->GetXaxis()->SetLabelOffset(6);
  Double_t ypos = -0.0525773;
  if (logy) ypos -= 1;
  TLatex *tex = new TLatex(10.6742,ypos,"80-92  70-80  60-70  50-60  40-50  30-40  20-30  10-20   0-10");
  tex->SetTextSize(0.032);
  tex->SetLineWidth(2);
  tex->Draw();

  c1->cd();

  TString label = "R_{AA} versus centrality for : ";
  TLegend *legend = new TLegend(0.568,0.788,0.984,0.974,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(4);
  legend->SetTextSize(0.042);
  legend->AddEntry(Suppr_vs_Cent_pT3, "p_{T} = 2.2 GeV/c", "P");
  legend->AddEntry(Suppr_vs_Cent_pT5, "p_{T} = 4.2 GeV/c", "P");
  legend->AddEntry(Suppr_vs_Cent_pT7, "p_{T} = 6.2 GeV/c", "P");
  legend->Draw();

  Suppr_vs_Cent_pT3->SetMarkerStyle(20);
  Suppr_vs_Cent_pT3->SetMarkerSize(1.7);
  Suppr_vs_Cent_pT3->SetMarkerColor(1);
  //Suppr_vs_Cent_pT3->Draw("PL"); 
  Suppr_vs_Cent_pT3->Draw("P"); 

  Suppr_vs_Cent_pT5->SetMarkerStyle(21);
  Suppr_vs_Cent_pT5->SetMarkerSize(1.7);
  Suppr_vs_Cent_pT5->SetMarkerColor(2);
  //Suppr_vs_Cent_pT5->Draw("PL"); 
  Suppr_vs_Cent_pT5->Draw("P"); 

  Suppr_vs_Cent_pT7->SetMarkerStyle(29);
  Suppr_vs_Cent_pT7->SetMarkerSize(2.5);
  Suppr_vs_Cent_pT7->SetMarkerColor(4);
  //Suppr_vs_Cent_pT7->Draw("PL"); 
  Suppr_vs_Cent_pT7->Draw("P"); 

  TLine *line = new TLine(0.,1.0,100.,1.0);
  if (logy) line = new TLine(0.,TMath::Log(1.0),100.,TMath::Log(1.0));
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(4);
  line->Draw("same");

  // Separated Ncoll errors draw sys. errors as boxes
  TBox *b3 = 0;
  TBox *b5 = 0;
  TBox *b7 = 0;
  for (int i=0; i<MaxCent; i++) 
    {
      b3 = new TBox(CentClass[i]-eCentClass[i],eNcoll_min3[i],
		    CentClass[i]+eCentClass[i],eNcoll_max3[i]);
      //cout << "Centmin & eNcoll_min = " << CentClass[i]-eCentClass[i] << " " << eNcoll_min3[i] << endl;
      //cout << "Centmax & eNcoll_max = " << CentClass[i]+eCentClass[i] << " " << eNcoll_max3[i] << endl;
      
      if (i==5 || i==6)
	{
	  b3->SetFillStyle(3013);
	}
//       else
// 	{
// 	  b3->SetFillStyle(3013);
// 	}
      b3->SetFillColor(kYellow);
      //b3->Draw();
      b5 = new TBox(CentClass[i]-eCentClass[i],eNcoll_min5[i],
		    CentClass[i]+eCentClass[i],eNcoll_max5[i]);
      //cout << "Centmin & eNcoll_min = " << CentClass[i]-eCentClass[i] << " " << eNcoll_min5[i] << endl;
      //cout << "Centmax & eNcoll_max = " << CentClass[i]+eCentClass[i] << " " << eNcoll_max5[i] << endl;
      
      if (i==5 || i==6)
	{
	  b5->SetFillStyle(3013);
	}
      else
	{
	  //b3->SetFillStyle(3013);
	}
      b5->SetFillColor(kYellow);
      //b5->Draw();
      b7 = new TBox(CentClass[i]-eCentClass[i],eNcoll_min7[i],
		    CentClass[i]+eCentClass[i],eNcoll_max7[i]);
      //cout << "Centmin & eNcoll_min = " << CentClass[i]-eCentClass[i] << " " << eNcoll_min7[i] << endl;
      //cout << "Centmax & eNcoll_max = " << CentClass[i]+eCentClass[i] << " " << eNcoll_max7[i] << endl;
      
      if (i==5 || i==6)
	{
	  b7->SetFillStyle(3013);
	}
      else
	{
	  //b3->SetFillStyle(3013);
	}
      b7->SetFillColor(kYellow);
      //b7->Draw(); 
      //cout << "CCprime' = " << i << endl;
  }

  Suppr_vs_Cent_pT3->Draw("P"); 
  //pave->Draw();
  Suppr_vs_Cent_pT5->Draw("P"); 
  Suppr_vs_Cent_pT7->Draw("P"); 
  line->Draw("same");
  legend->Draw();
  //c1->RedrawAxis();

  c1->Update();

  return 1;
}

//_____________________________________________________________________________
Int_t suppress_vs_ET()
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t piR2_scale = 148./Aoverlap[0]; // fm
  Double_t eta_to_y = 1.2 ;
  Double_t tau0 = 1.; // fm/c

  Int_t MaxCent = 5; // up to 40-50% class (no ET above, no pT=7 GeV/c above)
  Double_t EBjorken[MaxCent];
  Double_t eEBjorken[MaxCent];

  Double_t Etrans[MaxCent];
  Double_t eEtrans[MaxCent];

  Double_t RAA_pT3[MaxCent];
  Double_t eRAA_pT3[MaxCent];
  Double_t RAA_pT5[MaxCent];
  Double_t eRAA_pT5[MaxCent];
  Double_t RAA_pT7[MaxCent];
  Double_t eRAA_pT7[MaxCent];

  TCanvas *c1 = 0;
  TGraphErrors *Suppr_vs_EBjor_pT3 = 0;
  TGraphErrors *Suppr_vs_EBjor_pT5 = 0;
  TGraphErrors *Suppr_vs_EBjor_pT7 = 0;

  TCanvas *c2 = 0;
  TGraphErrors *Suppr_vs_ET_pT3 = 0;
  TGraphErrors *Suppr_vs_ET_pT5 = 0;
  TGraphErrors *Suppr_vs_ET_pT7 = 0;

  char name1[50];
  char name2[50];
  char name3[50];
  sprintf(name1,"suppr_vs_bjorkEn_pT3");
  sprintf(name2,"suppr_vs_bjorkEn_pT5");
  sprintf(name3,"suppr_vs_bjorkEn_pT7");

  static const Int_t MaxLocalPTbins = 25;
  Double_t pTpizero[MaxLocalPTbins];
  Double_t pizero_ratio[MaxLocalPTbins];
  Double_t epizero_ratio[MaxLocalPTbins];
  Double_t eNcollpizero_ratio[MaxLocalPTbins];

  // Separated Ncoll errors drawn as functions
  Double_t eNcoll_min3[MaxCent];
  Double_t eNcoll_max3[MaxCent];
  Double_t eNcoll_min5[MaxCent];
  Double_t eNcoll_max5[MaxCent];
  Double_t eNcoll_min7[MaxCent];
  Double_t eNcoll_max7[MaxCent];

  Int_t Cent1=0;
  Int_t Cent2=10;
  Int_t CCprime = 0;

  for (Int_t CC = 0; CC<MaxCent*2; CC=CC+2)
    {

      //      Int_t CC = getCentralityClass(Cent1,Cent2);
      cout << " Calculating ratio AuAu/pp vs. EBjorken for centrality class: " << CC 
	   << " (" << Cent1 << " - " << Cent2 << "%)" << endl;

      char title[300];
      char ffile[300];
      char name[300]; 
      sprintf(title,"Ratio AuAu/pp for centrality Class %d_%d",Cent1,Cent2);
      sprintf(name,"R_AA_%d_%d",Cent1,Cent2);
      //sprintf(name,"R_AA_%d_%d_part",Cent1,Cent2);

      sprintf(ffile,"%s%s.txt",data_tables_location,name);
      
      ifstream ifiledata(ffile);
      if (!ifiledata)
	{
	  cout << " Can't open file: " << ffile << endl;
	  return 0;
	}
      else cout << " Reading " << ffile  << " ..." << endl;
      
      Int_t i = 0;
      // Read pT, countsPi0 from the file
      while ( ifiledata >> 
	      pTpizero[i] >> 
	      //epTpizero[i] >>
	      pizero_ratio[i] >> 
	      epizero_ratio[i] >>
	      eNcollpizero_ratio[i]
	      ){
	//     cout << "  " << pTpizero[i]
	// 	 << "  " << countspizero[i] << " +/- " << ecountspizero[i] << endl;
	i++;
      }
      
      //if (i>9) EBjorken[i] = eta_to_y*ET[9]/(piR2*tau0);
      //else 
      Etrans[CCprime] = (ET[CC]+ET[CC+1])/2;
      eEtrans[CCprime] = TMath::Sqrt( (eET[CC]/ET[CC])*(eET[CC]/ET[CC])+
				      (eET[CC+1]/ET[CC+1])*(eET[CC+1]/ET[CC+1]));
      eEtrans[CCprime] *= Etrans[CCprime];
      //eEtrans[CCprime] = ET[CC]-ET[CC+1];

      EBjorken[CCprime] = eta_to_y*Etrans[CCprime]/(piR2_scale*((Aoverlap[CC]+Aoverlap[CC+1])/2.)*tau0);
      eEBjorken[CCprime] = TMath::Sqrt( (eET[CC]/ET[CC])*(eET[CC]/ET[CC])+
					(eET[CC+1]/ET[CC+1])*(eET[CC+1]/ET[CC+1])+ //);
					(eAoverlap[CC]/Aoverlap[CC])*(eAoverlap[CC]/Aoverlap[CC]) +
					(eAoverlap[CC+1]/Aoverlap[CC+1])*(eAoverlap[CC+1]/Aoverlap[CC+1]) );

      eEBjorken[CCprime] *= EBjorken[CCprime];
      cout << "ET      : " << Etrans[CCprime] << " +/- " << eEtrans[CCprime] << endl;
      cout << "EBjorken: " << EBjorken[CCprime] << " +/- " << eEBjorken[CCprime] << endl;

      // suppression factor      
      RAA_pT3[CCprime] = pizero_ratio[2]; // pT = 3 GeV/c
      cout << "Suppression at 3 GeV/c: " << RAA_pT3[CCprime] << endl;
      eRAA_pT3[CCprime] = epizero_ratio[2];
      eNcoll_min3[CCprime] = RAA_pT3[CCprime] - eNcollpizero_ratio[2]; // pT = 3 GeV/c
      eNcoll_max3[CCprime] = RAA_pT3[CCprime] + eNcollpizero_ratio[2]; // pT = 3 GeV/c

      RAA_pT5[CCprime] = pizero_ratio[6]; // pT = 5 GeV/c
      cout << "Suppression at 5 GeV/c: " << RAA_pT5[CCprime] << endl;
      eRAA_pT5[CCprime] = epizero_ratio[6];
      eNcoll_min5[CCprime] = RAA_pT5[CCprime] - eNcollpizero_ratio[6]; // pT = 3 GeV/c
      eNcoll_max5[CCprime] = RAA_pT5[CCprime] + eNcollpizero_ratio[6]; // pT = 3 GeV/c

      RAA_pT7[CCprime] = pizero_ratio[10]; // pT = 7 GeV/c
      cout << "Suppression at 7 GeV/c: " << RAA_pT7[CCprime] << endl;
      eRAA_pT7[CCprime] = epizero_ratio[10];
      eNcoll_min7[CCprime] = RAA_pT7[CCprime] - eNcollpizero_ratio[10]; // pT = 3 GeV/c
      eNcoll_max7[CCprime] = RAA_pT7[CCprime] + eNcollpizero_ratio[10]; // pT = 3 GeV/c

      Cent1 = Cent1+10;
      Cent2 = Cent1+10; if (Cent2 == 90) Cent2 = 92;
      cout << "CCprime = " << CCprime << endl;
      CCprime++;
    }

  Suppr_vs_EBjor_pT3 = new TGraphErrors( MaxCent, EBjorken, RAA_pT3, eEBjorken, eRAA_pT3);
  Suppr_vs_EBjor_pT5 = new TGraphErrors( MaxCent, EBjorken, RAA_pT5, eEBjorken, eRAA_pT5);
  Suppr_vs_EBjor_pT7 = new TGraphErrors( MaxCent, EBjorken, RAA_pT7, eEBjorken, eRAA_pT7);

  c1 = new TCanvas(name1,name1,600,600);

  TH2F *frame = new TH2F(name1,name1,10,0.,5.,10,0,1.5);
  frame->SetStats(0);
  frame->SetTitle(name1);
  frame->SetYTitle("R_{AA} Suppression factor");
  frame->SetXTitle("Bjorken Energy Density (GeV/fm3)");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();

  c1->cd();

  //TString label = "R_{AA} as a function of #epsilon for : ";
  TString label = "R_{AA} as a function of energy density for : ";
  TLegend *legend = new TLegend(0.59,0.56,0.90,0.88,label,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.042);
  legend->AddEntry(Suppr_vs_EBjor_pT3, "p_{T} = 2.2 GeV/c", "P");
  legend->AddEntry(Suppr_vs_EBjor_pT5, "p_{T} = 4.2 GeV/c", "P");
  legend->AddEntry(Suppr_vs_EBjor_pT7, "p_{T} = 6.2 GeV/c", "P");
  legend->Draw();

  Suppr_vs_EBjor_pT3->SetMarkerStyle(20);
  Suppr_vs_EBjor_pT3->SetMarkerSize(1.2);
  Suppr_vs_EBjor_pT3->SetMarkerColor(1);
  //Suppr_vs_EBjor_pT3->Draw("PL"); 
  Suppr_vs_EBjor_pT3->Draw("P"); 

  Suppr_vs_EBjor_pT5->SetMarkerStyle(20);
  Suppr_vs_EBjor_pT5->SetMarkerSize(1.2);
  Suppr_vs_EBjor_pT5->SetMarkerColor(2);
  //Suppr_vs_EBjor_pT5->Draw("PL"); 
  Suppr_vs_EBjor_pT5->Draw("P"); 

  Suppr_vs_EBjor_pT7->SetMarkerStyle(20);
  Suppr_vs_EBjor_pT7->SetMarkerSize(1.2);
  Suppr_vs_EBjor_pT7->SetMarkerColor(4);
  //Suppr_vs_EBjor_pT7->Draw("PL"); 
  Suppr_vs_EBjor_pT7->Draw("P"); 

  TLine *line = new TLine(0.,1.0,5.,1.0);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(4);
  line->Draw("same");

  // Separated Ncoll errors draw sys. errors as boxes
  TBox *b3 = 0;
  TBox *b5 = 0;
  TBox *b7 = 0;
  for (int i=0; i<MaxCent; i++) 
    {
      b3 = new TBox(EBjorken[i]-eEBjorken[i],eNcoll_min3[i],
		    EBjorken[i]+eEBjorken[i],eNcoll_max3[i]);
      //cout << "EBjormin & eNcoll_min = " << EBjorken[i]-eEBjorken[i] << " " << eNcoll_min3[i] << endl;
      //cout << "EBjormax & eNcoll_max = " << EBjorken[i]+eEBjorken[i] << " " << eNcoll_max3[i] << endl;
      
      b3->SetFillColor(kYellow);
      b3->Draw();
      b5 = new TBox(EBjorken[i]-eEBjorken[i],eNcoll_min5[i],
		    EBjorken[i]+eEBjorken[i],eNcoll_max5[i]);
      //cout << "EBjormin & eNcoll_min = " << EBjorken[i]-eEBjorken[i] << " " << eNcoll_min5[i] << endl;
      //cout << "EBjormax & eNcoll_max = " << EBjorken[i]+eEBjorken[i] << " " << eNcoll_max5[i] << endl;
      
      b5->SetFillColor(kYellow);
      b5->Draw();

      b7 = new TBox(EBjorken[i]-eEBjorken[i],eNcoll_min7[i],
		    EBjorken[i]+eEBjorken[i],eNcoll_max7[i]);
      //cout << "EBjormin & eNcoll_min = " << EBjorken[i]-eEBjorken[i] << " " << eNcoll_min7[i] << endl;
      //cout << "EBjormax & eNcoll_max = " << EBjorken[i]+eEBjorken[i] << " " << eNcoll_max7[i] << endl;
      
      b7->SetFillColor(kYellow);
      b7->Draw(); 
      //cout << "CCprime' = " << i << endl;
  }

  Suppr_vs_EBjor_pT3->Draw("P"); 
  Suppr_vs_EBjor_pT5->Draw("P"); 
  Suppr_vs_EBjor_pT7->Draw("P"); 

  c1->Update();

  //_____________________________________________________________________________

  Suppr_vs_ET_pT3 = new TGraphErrors( MaxCent, Etrans, RAA_pT3, eEtrans, eRAA_pT3);
  Suppr_vs_ET_pT5 = new TGraphErrors( MaxCent, Etrans, RAA_pT5, eEtrans, eRAA_pT5);
  Suppr_vs_ET_pT7 = new TGraphErrors( MaxCent, Etrans, RAA_pT7, eEtrans, eRAA_pT7);

  sprintf(name1,"suppr_vs_ET_pT3");
  c2 = new TCanvas(name1,name1,600,600);

  TH2F *frame2 = new TH2F(name1,name1,65,0.,650.,10,0,1.5);
  frame2->SetStats(0);
  frame2->SetTitle(name1);
  frame2->SetYTitle("R_{AA} Suppression factor");
  frame2->SetXTitle("E_{T} (GeV)");
  frame2->GetYaxis()->SetTitleOffset(1.2);
  frame2->GetXaxis()->SetTitleOffset(1.2);
  frame2->Draw();

  c2->cd();

  TString label2 = "R_{AA} as a function of E_{T} for : ";
  TLegend *legend2 = new TLegend(0.59,0.56,0.90,0.88,label2,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend2->SetFillStyle(0);
  legend2->SetBorderSize(0);
  legend2->SetTextSize(0.042);
  legend2->AddEntry(Suppr_vs_ET_pT3, "p_{T} = 2.2 GeV/c", "P");
  legend2->AddEntry(Suppr_vs_ET_pT5, "p_{T} = 4.2 GeV/c", "P");
  legend2->AddEntry(Suppr_vs_ET_pT7, "p_{T} = 6.2 GeV/c", "P");
  legend2->Draw();

  Suppr_vs_ET_pT3->SetMarkerStyle(20);
  Suppr_vs_ET_pT3->SetMarkerSize(1.2);
  Suppr_vs_ET_pT3->SetMarkerColor(1);
  //Suppr_vs_ET_pT3->Draw("PL"); 
  Suppr_vs_ET_pT3->Draw("P"); 

  Suppr_vs_ET_pT5->SetMarkerStyle(20);
  Suppr_vs_ET_pT5->SetMarkerSize(1.2);
  Suppr_vs_ET_pT5->SetMarkerColor(2);
  //Suppr_vs_ET_pT5->Draw("PL"); 
  Suppr_vs_ET_pT5->Draw("P"); 

  Suppr_vs_ET_pT7->SetMarkerStyle(20);
  Suppr_vs_ET_pT7->SetMarkerSize(1.2);
  Suppr_vs_ET_pT7->SetMarkerColor(4);
  //Suppr_vs_ET_pT7->Draw("PL"); 
  Suppr_vs_ET_pT7->Draw("P"); 

  line = new TLine(0.,1.0,650.,1.0);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(4);
  line->Draw("same");

  // Separated Ncoll errors draw sys. errors as boxes
  for (int i=0; i<MaxCent; i++) 
    {
      b3 = new TBox(Etrans[i]-eEtrans[i],eNcoll_min3[i],
		    Etrans[i]+eEtrans[i],eNcoll_max3[i]);
      //cout << "Etransmin & eNcoll_min = " << Etrans[i]-eEtrans[i] << " " << eNcoll_min3[i] << endl;
      //cout << "Etransmax & eNcoll_max = " << Etrans[i]+eEtrans[i] << " " << eNcoll_max3[i] << endl;
      
      b3->SetFillColor(kYellow);
      b3->Draw();
      b5 = new TBox(Etrans[i]-eEtrans[i],eNcoll_min5[i],
		    Etrans[i]+eEtrans[i],eNcoll_max5[i]);
      //cout << "Etransmin & eNcoll_min = " << Etrans[i]-eEtrans[i] << " " << eNcoll_min5[i] << endl;
      //cout << "Etransmax & eNcoll_max = " << Etrans[i]+eEtrans[i] << " " << eNcoll_max5[i] << endl;
      
      b5->SetFillColor(kYellow);
      b5->Draw();

      b7 = new TBox(Etrans[i]-eEtrans[i],eNcoll_min7[i],
		    Etrans[i]+eEtrans[i],eNcoll_max7[i]);
      //cout << "Etransmin & eNcoll_min = " << Etrans[i]-eEtrans[i] << " " << eNcoll_min7[i] << endl;
      //cout << "Etransmax & eNcoll_max = " << Etrans[i]+eEtrans[i] << " " << eNcoll_max7[i] << endl;
      
      b7->SetFillColor(kYellow);
      b7->Draw(); 
      //cout << "CCprime' = " << i << endl;
  }

  Suppr_vs_ET_pT3->Draw("P"); 
  Suppr_vs_ET_pT5->Draw("P"); 
  Suppr_vs_ET_pT7->Draw("P"); 


  c2->Update();

  return 1;

}

//_____________________________________________________________________________
// type_of_plot = "singles" or "global"

void plot_all_pi0_spectra_run2(const char *cut="noPID", 
			       const char *spectra="raw", 
			       const char *type_of_plot="singles")
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Int_t Cent1 = 0;
  Int_t Cent2 = 10;

  //Double_t ptmax = 8.5; // <-- change this for min.bias
  Double_t ptmax = 10.5;
  Int_t ptbins = (Int_t)ptmax*2;

  char title[50];
  sprintf(title,"pi0_spectra_all_cent");
  TCanvas *c10 = 0;
  if (strcmp(type_of_plot,"global") == 0) 
    {
      c10 = new TCanvas(title,title,900,700);
      TH2F *frame = new TH2F(title,title,ptbins,0.,ptmax,100,5E-08,15);
      frame->SetStats(0);
      frame->SetTitle(title);
      frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
      frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
      frame->GetYaxis()->SetTitleOffset(1.4);
      frame->GetXaxis()->SetTitleOffset(1.2);
      frame->Draw();
      c10->SetLogy();
      c10->Update();
    }
  //gStyle->SetOptStat(11111111);

  TString label = "PbSc #pi^{0} spectra: ";
  TLegend *legend = new TLegend(0.59,0.56,0.90,0.88,label,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.042);
  legend->Draw();

  char sublabel[100];
  Int_t j = 0;

  TGraph *temp_spec_[CentClasses] ;

  for (Int_t i=0; i<=CentClasses; i+=2)
    {
      if (i==20) // min. bias
	{
	  continue;
	  if (strcmp(type_of_plot,"global") == 0) 
	    {
	      temp_spec_[i] = (TGraphErrors*)plot_pi0_spectra_run2(100,100,cut,spectra,"no");
	      temp_spec_[i]->SetMarkerStyle(20);
	      temp_spec_[i]->SetMarkerSize(1.2);
	      temp_spec_[i]->SetMarkerColor(1);
	      c10->cd();
	      temp_spec_[i]->Draw("P"); // <--- min. bias plot
	      sprintf(sublabel,"min.bias");
	      legend->AddEntry(temp_spec_[i],sublabel,"P");
	      legend->Draw();
	      c10->Update();
	    }
	  else
	    {
	      temp_spec_[i] = (TGraphErrors*)plot_pi0_spectra_run2(100,100,cut,spectra,"yes");
	    }
	}
      else if (Cent1 == 80) // 80-92%
	{ 
	  continue;
	  if (strcmp(type_of_plot,"global") == 0) 
	    {
	      temp_spec_[i] = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,92,cut,spectra,"no");
	      temp_spec_[i]->SetMarkerStyle(21);
	      temp_spec_[i]->SetMarkerSize(1.1);
	      temp_spec_[i]->SetMarkerColor(3);
	      c10->cd();
	      temp_spec_[i]->Draw("P");
	      sprintf(sublabel,"80-92%%");
	      legend->AddEntry(temp_spec_[i], sublabel, "P");
	      c10->Update();
	    }
	  else  // "singles"
	    {
	      temp_spec_[i] = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,92,cut,spectra,"yes");
	    }
	}
      //else if (Cent1 == 0) // 0-10% ... 70-80%
      else if (Cent1 != 90) // 0-10% ... 70-80%
	{ 
	  if (Cent1 == 60) Cent2 = 80;
	  if (Cent1 == 70) continue;
	  if (strcmp(type_of_plot,"global") == 0) 
	    {
	      int col = (i<10) ? i+2 : i-8 ; if (col==0) col=4;
	      temp_spec_[i] = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut,spectra,"no");
	      temp_spec_[i]->SetMarkerStyle(20+j); j++;
	      temp_spec_[i]->SetMarkerSize(1.2);
	      temp_spec_[i]->SetMarkerColor(col);
	      c10->cd();
	      temp_spec_[i]->Draw("P");
	      sprintf(sublabel,"%i-%i%%",Cent1,Cent2);
	      legend->AddEntry(temp_spec_[i], sublabel, "P");
	      c10->Update();
	    }
	  else // "singles"
	    {
	      temp_spec_[i] = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut,spectra,"yes");
	    }
	}
      Cent1 +=10;
      Cent2 = Cent1+10;
    }

  // let's replot the most central
  if (strcmp(type_of_plot,"global") == 0) 
    {
      temp_spec_[0]->SetMarkerStyle(20);
      temp_spec_[0]->SetMarkerSize(1.2);
      temp_spec_[0]->SetMarkerColor(2);
      c10->cd();
      temp_spec_[0]->Draw("P");
      c10->Update();
    }
  c10->Update();
  
  //suppress_vs_ET();
}

//_____________________________________________________________________________
// OPTIONS: "central" , "minbias", "periph"
//          "all"

void plot_comparison_spectra_pbsc_pbgl(const char* type="central")
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TCanvas *c20 = 0;
  TGraph *pbsc = 0 ;
  TGraph *pbgl = 0 ;

  char title[50];
  sprintf(title,"pi0_spectra_pbsc_pbgl_comparison");
  c20 = new TCanvas(title,title,800,700);
  TH2F *frame = new TH2F(title,title,21,0.,10.5,100,5E-08,35);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  c20->SetLogy();
  c20->Update();

  TString label = "EMCal #pi^{0} spectra: ";
  TLegend *legend = new TLegend(0.59,0.56,0.90,0.88,label,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.042);
  legend->Draw();
  char sublabel[50];

  if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // central   
    {
      pbsc = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","full","no");
      pbsc->SetMarkerStyle(20);
      pbsc->SetMarkerSize(1.2);
      pbsc->SetMarkerColor(4);
      c20->cd();
      pbsc->Draw("P");
      sprintf(sublabel,"PbSc 0-10%%");
      legend->AddEntry(pbsc, sublabel, "P");
      c20->Update();
      pbgl = (TGraphErrors*)plot_xx_pi0_spectra("pi0_ms_pbgl_00-10.dat");
      //pbgl = (TGraphErrors*)plot_xx_pi0_spectra("pi0_ms_pbsc_00-10.dat");
      pbgl->SetMarkerStyle(20);
      pbgl->SetMarkerSize(1.2);
      pbgl->SetMarkerColor(2);
      c20->cd();
      pbgl->Draw("P");
      sprintf(sublabel,"PbGl 0-10%%");
      legend->AddEntry(pbgl, sublabel, "P");
      c20->Update();
    }
  if (strcmp(type,"minbias") == 0 || strcmp(type,"all") == 0) // minbias
    {
      pbsc = (TGraphErrors*)plot_pi0_spectra_run2(100,100,"noPID","full","no");
      pbsc->SetMarkerStyle(25);
      pbsc->SetMarkerSize(1.2);
      pbsc->SetMarkerColor(4);
      c20->cd();
      pbsc->Draw("P");
      sprintf(sublabel,"PbSc min.bias");
      legend->AddEntry(pbgl, sublabel, "P");
      c20->Update();
      pbgl = (TGraphErrors*)plot_xx_pi0_spectra("pi0_ms_pbgl_minb.dat");
      //pbgl = (TGraphErrors*)plot_xx_pi0_spectra("pi0_ms_pbsc_minb.dat");
      pbgl->SetMarkerStyle(25);
      pbgl->SetMarkerSize(1.2);
      pbgl->SetMarkerColor(2);
      c20->cd();
      pbgl->Draw("P");
      sprintf(sublabel,"PbGl min.bias");
      legend->AddEntry(pbgl, sublabel, "P");
      c20->Update();
    }
  if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // Periph 
    {
      pbsc = (TGraphErrors*)plot_pi0_spectra_run2(70,80,"noPID","full","no");
      pbsc->SetMarkerStyle(29);
      pbsc->SetMarkerSize(1.7);
      pbsc->SetMarkerColor(4);
      c20->cd();
      pbsc->Draw("P");
      sprintf(sublabel,"PbSc 70-80%%");
      legend->AddEntry(pbsc, sublabel, "P");
      c20->Update();
      pbgl = (TGraphErrors*)plot_xx_pi0_spectra("pi0_ms_pbgl_70-80.dat");
      //pbgl = (TGraphErrors*)plot_xx_pi0_spectra("pi0_ms_pbsc_70-80.dat");
      pbgl->SetMarkerStyle(29);
      pbgl->SetMarkerSize(1.7);
      pbgl->SetMarkerColor(2);
      c20->cd();
      pbgl->Draw("P");
      sprintf(sublabel,"PbGl 70-80%%");
      legend->AddEntry(pbgl, sublabel, "P");
      c20->Update();
    }

  c20->Update();

  TH1F *h_pbsc = new TH1F("h_pbsc","h_pbsc",100,0,10);
  TH1F *h_pbgl = new TH1F("h_pbgl","h_pbgl",100,0,10);
  Int_t pbsc_points = pbsc->GetN();
  Int_t pbgl_points = pbgl->GetN();
  Double_t pbsc_pT[pbsc_points], pbscYield[pbsc_points], epbscYield[pbsc_points];
  Double_t pbgl_pT[pbgl_points], pbglYield[pbgl_points], epbglYield[pbgl_points];
  Int_t binpT = 0;

  for (Int_t i=0; i<pbsc_points;i++)
    {
      pbsc->GetPoint(i,pbsc_pT[i],pbscYield[i]);
      //epTpp[i] = ppPi0->GetErrorX(i);
      epbscYield[i] = pbsc->GetErrorY(i);
      binpT = h_pbsc->GetXaxis()->FindBin( pbsc_pT[i] );
      h_pbsc->SetBinContent(binpT,pbscYield[i]);
      h_pbsc->SetBinError(binpT,epbscYield[i]);
      //      cout << "("<< pTpp[i] << " ," << ScaledppYield[i] << ")" << endl ;
    }
  for (Int_t i=0; i<pbgl_points;i++)
    {
      pbgl->GetPoint(i,pbgl_pT[i],pbglYield[i]);
      //epTpp[i] = ppPi0->GetErrorX(i);
      epbglYield[i] = pbgl->GetErrorY(i);
      //      cout << "("<< pTpp[i] << " ," << ScaledppYield[i] << ")" << endl ;
      binpT = h_pbgl->GetXaxis()->FindBin( pbgl_pT[i] );
      h_pbgl->SetBinContent(binpT,pbglYield[i]);
      h_pbgl->SetBinError(binpT,epbglYield[i]);
    }

//   c20->cd();
//   h_pbsc->Draw("same");
//   h_pbgl->Draw("same");
//   c20->Update();

//   char name2[50];
//   sprintf(name2,"ratio_pbsc_pbgl_%s",type);
//   TCanvas *c19 = new TCanvas(name2,name2,600,600);

//   TH1F *ratio = 0;
//   ratio = (TH1F*)h_pbsc->Clone();   
//   ratio->SetTitle("ratio_pbsc_pbgl");
//   ratio->Divide(h_pbgl);
//   frame->SetYTitle("Ratio PbSc/PbGl");
//   frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
//   frame->GetYaxis()->SetTitleOffset(1.4);
//   frame->GetXaxis()->SetTitleOffset(1.2);
//   ratio->Draw("e");
//   c19->Update();

}
//_____________________________________________________________________________
// OPTIONS: "central" , "minbias", "periph"
//          "all"

void plot_comparison_pi0_piminus(const char* type="central")
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TCanvas *c20 = 0;
  TGraph *pbsc = 0;
  TGraph *piminus = 0;

  char title[50];
  sprintf(title,"pi0_piminus_comparison_piminus");
  c20 = new TCanvas(title,title,800,700);
  TH2F *frame = new TH2F(title,title,23,0.,11.5,100,5E-09,35);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  c20->SetLogy();
  c20->Update();

  if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // central 
    {
      pbsc = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","full","no");
      pbsc->SetMarkerStyle(20);
      pbsc->SetMarkerSize(1.2);
      pbsc->SetMarkerColor(4);
      c20->cd();
      pbsc->Draw("P");
      c20->Update();
      piminus = (TGraphErrors*)plot_xx_pi0_spectra("picharged_cent.txt");
      piminus->SetMarkerStyle(20);
      piminus->SetMarkerSize(1.2);
      piminus->SetMarkerColor(2);
      c20->cd();
      piminus->Draw("P");
      c20->Update();
    }
  if (strcmp(type,"minbias") == 0 || strcmp(type,"all") == 0) // minbias
    {
      pbsc = (TGraphErrors*)plot_pi0_spectra_run2(100,100,"noPID","full","no");
      pbsc->SetMarkerStyle(20);
      pbsc->SetMarkerSize(1.2);
      pbsc->SetMarkerColor(4);
      c20->cd();
      pbsc->Draw("P");
      c20->Update();
      piminus = (TGraphErrors*)plot_xx_pi0_spectra("picharged_minb.txt");
      piminus->SetMarkerStyle(20);
      piminus->SetMarkerSize(1.2);
      piminus->SetMarkerColor(2);
      c20->cd();
      piminus->Draw("P");
      c20->Update();
    }
  if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // Periph 
    {
      pbsc = (TGraphErrors*)plot_pi0_spectra_run2(70,80,"noPID","full","no");
      pbsc->SetMarkerStyle(20);
      pbsc->SetMarkerSize(1.2);
      pbsc->SetMarkerColor(4);
      c20->cd();
      pbsc->Draw("P");
      c20->Update();
      piminus = (TGraphErrors*)plot_xx_pi0_spectra("picharged_periph.dat");
      piminus->SetMarkerStyle(20);
      piminus->SetMarkerSize(1.2);
      piminus->SetMarkerColor(2);
      c20->cd();
      piminus->Draw("P");
      c20->Update();
    }

  char comment[50];
  if ( strcmp(type,"all") == 0) 
    {
      TLatex *tex = new TLatex(3.35,1.02177,"Blue: PbSc #pi^{0} (Central-Min.Bias-Periph)");
      tex->SetTextColor(4);
      tex->SetTextSize(0.0398773);
      tex->Draw();
      tex = new TLatex(3.35,0.61479,"Red: #pi^{#pm} (Central-Min.Bias-Periph)");
      tex->SetTextColor(2);
      tex->SetTextSize(0.0398773);
      tex->Draw();
    }
  else
    {
      sprintf(comment,"Blue: PbSc #pi^{0} (%s)",type);
      TLatex *tex = new TLatex(6,-0.1,comment);
      tex->SetTextColor(4);
      tex->SetTextSize(0.04);
      tex->SetLineWidth(1);
      tex->Draw();
      sprintf(comment,"Red: #pi^{#pm} (%s)",type);
      tex = new TLatex(6.,-0.5,comment);
      tex->SetTextColor(2);
      tex->SetTextSize(0.04);
      tex->SetLineWidth(1);
      tex->Draw();
    }
  c20->Update();

}

//_____________________________________________________________________________
// OPTIONS: "central" , "minbias", "periph"
//          "all"

void plot_comparison_spectra_17_130_200(const char* type="central")
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TCanvas *c21 = 0;
  TGraph *pbsc_200 = 0;
  TGraph *emcal_130 = 0;
  TGraph *wa98_17 = 0;

  char title[50];
  sprintf(title,"pi0_comparison_spectra_pbsc_200_emcal_130_wa98_17");
  c21 = new TCanvas(title,title,800,700);
  c21->SetLeftMargin(0.120603);
  c21->SetRightMargin(0.0690955);
  c21->SetTopMargin(0.0812883);
  TH2F *frame = new TH2F(title,title,21,0.,10.5,100,5E-08,35);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  c21->SetLogy();
  c21->Update();

  TString label = "#pi^{0} spectra SPS #leftrightarrow RHIC: ";
  TLegend *legend = new TLegend(0.409548,0.708589,0.90,0.90,label,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.034);
  legend->Draw();
  char sublabel[50];

  if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // central 
    {
      pbsc_200 = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","full","no"); // 0-10%
      pbsc_200->SetMarkerStyle(20);
      pbsc_200->SetMarkerSize(1.3);
      pbsc_200->SetMarkerColor(4);
      c21->cd();
      pbsc_200->Draw("P");
      sprintf(sublabel,"PbSc, AuAu @ 200 GeV [0-10%%]");
      legend->AddEntry(pbsc_200, sublabel, "P");
      c21->Update();
      emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_cent.dat"); // 0-10%
      emcal_130->SetMarkerStyle(20);
      emcal_130->SetMarkerSize(1.3);
      emcal_130->SetMarkerColor(2);
      c21->cd();
      emcal_130->Draw("P");
      sprintf(sublabel,"EMCal, AuAu @ 130 GeV [0-10%%]");
      legend->AddEntry(emcal_130, sublabel, "P");
      c21->Update();
      wa98_17 = (TGraphErrors*)plot_xx_pi0_spectra("wa98_pi0_cen.dat");  // 0-12.7%
      wa98_17->SetMarkerStyle(20);
      wa98_17->SetMarkerSize(1.2);
      wa98_17->SetMarkerColor(8);
      c21->cd();
      wa98_17->Draw("P");
      sprintf(sublabel,"WA98, PbPb @ 17 GeV [0-13%%]");
      legend->AddEntry(wa98_17, sublabel, "P");
      c21->Update();
    }
  if (strcmp(type,"minbias") == 0 || strcmp(type,"all") == 0) // minbias
    {
      pbsc_200 = (TGraphErrors*)plot_pi0_spectra_run2(100,100,"noPID","full","no");
      pbsc_200->SetMarkerStyle(20);
      pbsc_200->SetMarkerSize(1.3);
      pbsc_200->SetMarkerColor(4);
      c21->cd();
      pbsc_200->Draw("P");
      c21->Update();
      emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_minb.dat"); // is this guy correct ?
      emcal_130->SetMarkerStyle(20);
      emcal_130->SetMarkerSize(1.3);
      emcal_130->SetMarkerColor(2);
      c21->cd();
      emcal_130->Draw("P");
      c21->Update();
      wa98_17 = (TGraphErrors*)plot_xx_pi0_spectra("wa98_pi0_minb.dat"); // doesn't exist so far
      wa98_17->SetMarkerStyle(20);
      wa98_17->SetMarkerSize(1.2);
      wa98_17->SetMarkerColor(8);
      c21->cd();
      wa98_17->Draw("P");
      c21->Update();
    }
  if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // Periph 
    {
      pbsc_200 = (TGraphErrors*)plot_pi0_spectra_run2(70,80,"noPID","full","no"); // 60-80% ! to compare with 130A GeV
      pbsc_200->SetMarkerStyle(20);
      pbsc_200->SetMarkerSize(1.3);
      pbsc_200->SetMarkerColor(4);
      c21->cd();
      pbsc_200->Draw("P");
      c21->Update();
      emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_periph.dat"); // 60-80%
      emcal_130->SetMarkerStyle(20);
      emcal_130->SetMarkerSize(1.3);
      emcal_130->SetMarkerColor(2);
      c21->cd();
      emcal_130->Draw("P");
      c21->Update();
      wa98_17 = (TGraphErrors*)plot_xx_pi0_spectra("wa98_pi0_per.dat"); // 67-82.8% !!
      wa98_17->SetMarkerStyle(20);
      wa98_17->SetMarkerSize(1.2);
      wa98_17->SetMarkerColor(8);
      c21->cd();
      wa98_17->Draw("P");
      c21->Update();
    }

  // Delete low-pT WA98 points
  for (Int_t i=0; i<9;i++)
    {
      wa98_17->SetPoint(i,-1.,-1);
    }

  //
  TPaveText *pave = new TPaveText(6.25116,-4.12289,10.2884,-1.09057,"br"); 
  pave->SetFillColor(20);
  pave->SetTextColor(9);  
  TText *text = pave->AddText("Modified power-law fit:");
  text = pave->AddText("200 GeV:");
  text = pave->AddText("A=1.46e+06, p_{0}=1.72, n = 12.36"); 
  text = pave->AddText("130 GeV:");
  text = pave->AddText("A=1.48e+06, p_{0}=1.72, n = 12.84"); 
  text = pave->AddText(" 17 GeV:");
  text = pave->AddText("A=3.65e+06, p_{0}=1.72, n = 14.50"); 
  pave->Draw();

//   char comment[50];
//   if ( strcmp(type,"all") == 0) 
//     {
//       TLatex *tex = new TLatex(3.35,1.02177,"Blue: Pbsc@200 (Central-Min.Bias-Periph)");
//       tex->SetTextColor(4);
//       tex->SetTextSize(0.0398773);
//       tex->Draw();
//       tex = new TLatex(3.35,0.61479,"Red: EMCal@130 (Central-Min.Bias-Periph)");
//       tex->SetTextColor(2);
//       tex->SetTextSize(0.0398773);
//       tex->Draw();
//     }
//   else
//     {
//     }
  c21->Update();

}

//_____________________________________________________________________________
// OPTIONS: "central" , "minbias", "periph"
//          

void plot_comparison_xT_spectra_17_130_200(const char* type="central")
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TCanvas *c21 = 0;
  TGraph *pbsc_200 = 0;
  TGraph *emcal_130 = 0;
  TGraph *wa98_17 = 0;
  TGraph *pp_200 = 0;

  char title[50];
  sprintf(title,"pi0_comparison_xT_%s_spectra_pbsc_200_emcal_130_wa98_17",type);
  c21 = new TCanvas(title,title,800,700);
  //c21 = new TCanvas(title,title,165,6,650,700);
  c21->Range(-2.63635,-8.62882,0.14404,4.6899);
  c21->SetLeftMargin(0.12);
  c21->SetRightMargin(0.05);
  c21->SetTopMargin(0.236);
  TH2F *frame = new TH2F(title,title,100,0.005,1.01,100,5E-08,35);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("Normalized  #pi^{0} yield");
  //frame->SetYTitle("1/[2#pi x_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dx_{T}d#eta");// (GeV/c)^{-2}");
  frame->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  c21->SetLogy();
  c21->SetLogx();
  c21->Update();

  TString label = "  Central #pi^{0} spectra";;//= "#pi^{0} spectra SPS #leftrightarrow RHIC: ";
  //TLegend *legend = new TLegend(0.409548,0.708589,0.90,0.90,label,"brNDC");
  TLegend *legend = new TLegend(0.409548,0.783742,0.944724,0.95092,NULL,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(3);
  legend->SetTextSize(0.034);
  legend->Draw();
  char sublabel[50];

  if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // central 
    {
      pbsc_200 = (TGraphErrors*)plot_xx_pi0_spectra("pi0_pbsc_0_10.txt","xT"); // 0-10%
      pbsc_200->SetMarkerStyle(20);
      pbsc_200->SetMarkerSize(1.3);
      pbsc_200->SetMarkerColor(4);
      c21->cd();
      pbsc_200->Draw("P");
      sprintf(sublabel,"PbSc, AuAu @ 200 GeV [0-10%%]");
      legend->AddEntry(pbsc_200, sublabel, "P");
      c21->Update();
      emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_cent.dat","xT"); // 0-10%
      emcal_130->SetMarkerStyle(20);
      emcal_130->SetMarkerSize(1.3);
      emcal_130->SetMarkerColor(2);
      c21->cd();
      emcal_130->Draw("P");
      sprintf(sublabel,"EMCal, AuAu @ 130 GeV [0-10%%]");
      legend->AddEntry(emcal_130, sublabel, "P");
      c21->Update();
      wa98_17 = (TGraphErrors*)plot_xx_pi0_spectra("wa98_pi0_cen.dat","xT");  // 0-12.7%
      wa98_17->SetMarkerStyle(20);
      wa98_17->SetMarkerSize(1.2);
      wa98_17->SetMarkerColor(8);
      c21->cd();
      wa98_17->Draw("P");
      sprintf(sublabel,"WA98, PbPb @ 17 GeV [0-13%%]");
      legend->AddEntry(wa98_17, sublabel, "P");
      c21->Update();
      pp_200 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt","xT"); // pp
      pp_200->SetMarkerStyle(20);
      pp_200->SetMarkerSize(1.2);
      pp_200->SetMarkerColor(1);
      c21->cd();
      pp_200->Draw("P");
      sprintf(sublabel,"pp @ 200 GeV");
      legend->AddEntry(pp_200, sublabel, "P");
      c21->Update();
    }
  if (strcmp(type,"minbias") == 0 || strcmp(type,"all") == 0) // minbias
    {
      pbsc_200 = (TGraphErrors*)plot_xx_pi0_spectra("pi0_pbsc_minbias.txt","xT");
      pbsc_200->SetMarkerStyle(20);
      pbsc_200->SetMarkerSize(1.3);
      pbsc_200->SetMarkerColor(4);
      c21->cd();
      pbsc_200->Draw("P");
      c21->Update();
      emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_minb.dat","xT"); // is this guy correct ?
      emcal_130->SetMarkerStyle(20);
      emcal_130->SetMarkerSize(1.3);
      emcal_130->SetMarkerColor(2);
      c21->cd();
      emcal_130->Draw("P");
      c21->Update();
      wa98_17 = (TGraphErrors*)plot_xx_pi0_spectra("wa98_pi0_minb.dat","xT"); // doesn't exist so far
      wa98_17->SetMarkerStyle(20);
      wa98_17->SetMarkerSize(1.2);
      wa98_17->SetMarkerColor(8);
      c21->cd();
      wa98_17->Draw("P");
      c21->Update();
    }
  if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // Periph 
    {
      pbsc_200 = (TGraphErrors*)plot_xx_pi0_spectra("pi0_pbsc_60_80.txt","xT"); // 60-80% 
      pbsc_200->SetMarkerStyle(20);
      pbsc_200->SetMarkerSize(1.3);
      pbsc_200->SetMarkerColor(4);
      c21->cd();
      pbsc_200->Draw("P");
      sprintf(sublabel,"PbSc, AuAu @ 200 GeV [60-80%%]");
      legend->AddEntry(pbsc_200, sublabel, "P");
      c21->Update();
      emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_periph.dat","xT"); // 60-80%
      emcal_130->SetMarkerStyle(20);
      emcal_130->SetMarkerSize(1.3);
      emcal_130->SetMarkerColor(2);
      c21->cd();
      emcal_130->Draw("P");
      sprintf(sublabel,"EMCal, AuAu @ 130 GeV [60-80%%]");
      legend->AddEntry(emcal_130, sublabel, "P");
      c21->Update();
      wa98_17 = (TGraphErrors*)plot_xx_pi0_spectra("wa98_pi0_per.dat","xT"); // 67-82.8% !!
      wa98_17->SetMarkerStyle(20);
      wa98_17->SetMarkerSize(1.2);
      wa98_17->SetMarkerColor(8);
      c21->cd();
      wa98_17->Draw("P");
      sprintf(sublabel,"WA98, PbPb @ 17 GeV [67-83%%]");
      legend->AddEntry(wa98_17, sublabel, "P");
      c21->Update();
      pp_200 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt","xT"); // pp
      pp_200->SetMarkerStyle(20);
      pp_200->SetMarkerSize(1.2);
      pp_200->SetMarkerColor(1);
      c21->cd();
      pp_200->Draw("P");
      sprintf(sublabel,"pp @ 200 GeV");
      legend->AddEntry(pp_200, sublabel, "P");
      c21->Update();
    }

  // Delete low-pT WA98 points
  for (Int_t i=0; i<9;i++)
    {
      wa98_17->SetPoint(i,-1.,-1);
    }

//   //
//   TPaveText *pave = new TPaveText(6.25116,-4.12289,10.2884,-1.09057,"br"); 
//   pave->SetFillColor(20);
//   pave->SetTextColor(9);  
//   TText *text = pave->AddText("Modified power-law fit:");
//   text = pave->AddText("200 GeV:");
//   text = pave->AddText("A=1.46e+06, p_{0}=1.72, n = 12.36"); 
//   text = pave->AddText("130 GeV:");
//   text = pave->AddText("A=1.48e+06, p_{0}=1.72, n = 12.84"); 
//   text = pave->AddText(" 17 GeV:");
//   text = pave->AddText("A=3.65e+06, p_{0}=1.72, n = 14.50"); 
//   pave->Draw();

//   char comment[50];
//   if ( strcmp(type,"all") == 0) 
//     {
//       TLatex *tex = new TLatex(3.35,1.02177,"Blue: Pbsc@200 (Central-Min.Bias-Periph)");
//       tex->SetTextColor(4);
//       tex->SetTextSize(0.0398773);
//       tex->Draw();
//       tex = new TLatex(3.35,0.61479,"Red: EMCal@130 (Central-Min.Bias-Periph)");
//       tex->SetTextColor(2);
//       tex->SetTextSize(0.0398773);
//       tex->Draw();
//     }
//   else
//     {
//     }
  c21->Update();

}

void plot_xT_scaling_130_200()
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TCanvas *c21 = 0;
  TCanvas *c22 = 0;

  TGraph *pbsc_200 = 0;
  TGraph *emcal_130 = 0;
  TGraph *pp_200 = 0;

  char title[50];
  sprintf(title,"pi0_xT_scaling_200_130");
  c21 = new TCanvas(title,title,700,700);
  c21->Range(-2.21568,-8.29572,-0.807725,1.68179);
  c21->SetLeftMargin(0.12);
  c21->SetRightMargin(0.012);
  c21->SetTopMargin(0.014);

  TH2F *frame = new TH2F(title,title,100,0.009,0.15,100,5E-08,35);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("(#sqrt{s_{NN}}/200)^{n} d^{2}N^{#pi^{0}}/2#pi dp_{T}d#eta");
  //frame->SetYTitle("1/[2#pi x_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dx_{T}d#eta");// (GeV/c)^{-2}");
  frame->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  c21->SetLogy();
  c21->SetLogx();
  c21->Update();

  TString label = "  Central #pi^{0} spectra";
  //TLegend *legend = new TLegend(0.409548,0.708589,0.90,0.90,label,"brNDC");
  TLegend *legend = new TLegend(0.366379,0.797546,0.979885,0.976994,NULL,"brNDC");
  //TLegend *legend = new TLegend(0.59,0.51,0.90,0.88,label,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(3);
  legend->SetTextSize(0.034);
  legend->Draw();
  char sublabel[50];

  // CENTRAL
  pp_200 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt","xT",1./2.); // pp
  pp_200->SetMarkerStyle(29);
  pp_200->SetMarkerSize(1.);
  pp_200->SetMarkerColor(1);
  c21->cd();
  pp_200->Draw("P");
  c21->Update();
  pbsc_200 = (TGraphErrors*)plot_xx_pi0_spectra("pi0_pbsc_0_10.txt","xT"); // 0-10%
  pbsc_200->SetMarkerStyle(21);
  pbsc_200->SetMarkerSize(1.3);
  pbsc_200->SetMarkerColor(4);
  c21->cd();
  pbsc_200->Draw("P");
  sprintf(sublabel,"PbSc, AuAu @ 200 GeV [0-10%%]");
  legend->AddEntry(pbsc_200, sublabel, "P");
  c21->Update();
  emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_cent.dat","xT",1./20.); // 0-10%
  emcal_130->SetMarkerStyle(21);
  emcal_130->SetMarkerSize(1.3);
  emcal_130->SetMarkerColor(2);
  c21->cd();
  emcal_130->Draw("P");
  sprintf(sublabel,"EMCal, AuAu @ 130 GeV [0-10%%]");
  legend->AddEntry(emcal_130, sublabel, "P");
  c21->Update();

  // Ratios now ...
  TH1F *h_xT_130_cent = new TH1F("h_xT_130_cent","h_xT_130_cent",100,0,1);
  TH1F *h_xT_200_cent = new TH1F("h_xT_200_cent","h_xT_200_cent",100,0,1);
  Int_t points_130_cent = emcal_130->GetN();
  Int_t points_200_cent = pbsc_200->GetN();
  Double_t xT_130_cent_xT[points_130_cent], xT_130_centYield[points_130_cent], exT_130_centYield[points_130_cent];
  Double_t xT_200_cent_xT[points_200_cent], xT_200_centYield[points_200_cent], exT_200_centYield[points_200_cent];
  Int_t binxT = 0;

  for (Int_t i=0; i<points_130_cent;i++)
    {
      emcal_130->GetPoint(i,xT_130_cent_xT[i],xT_130_centYield[i]);
      exT_130_centYield[i] = emcal_130->GetErrorY(i);
      binxT = emcal_130->GetXaxis()->FindBin( xT_130_cent_xT[i] );
      h_xT_130_cent->SetBinContent(binxT,xT_130_centYield[i]);
      h_xT_130_cent->SetBinError(binxT,exT_130_centYield[i]);
    }
  for (Int_t i=0; i<points_200_cent;i++)
    {
      pbsc_200->GetPoint(i,xT_200_cent_xT[i],xT_200_centYield[i]);
      exT_200_centYield[i] = pbsc_200->GetErrorY(i);
      binxT = pbsc_200->GetXaxis()->FindBin( xT_200_cent_xT[i] );
      h_xT_200_cent->SetBinContent(binxT,xT_200_centYield[i]);
      h_xT_200_cent->SetBinError(binxT,exT_200_centYield[i]);
    }

  // PERIPH
  pp_200 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt","xT",5.); // pp
  pp_200->SetMarkerStyle(29);
  pp_200->SetMarkerSize(1.);
  pp_200->SetMarkerColor(1);
  c21->cd();
  pp_200->Draw("P");
  c21->Update();
  pbsc_200 = (TGraphErrors*)plot_xx_pi0_spectra("pi0_pbsc_60_80.txt","xT"); // 60-80% 
  pbsc_200->SetMarkerStyle(20);
  pbsc_200->SetMarkerSize(1.3);
  pbsc_200->SetMarkerColor(4);
  c21->cd();
  pbsc_200->Draw("P");
  sprintf(sublabel,"PbSc, AuAu @ 200 GeV [60-80%%]");
  legend->AddEntry(pbsc_200, sublabel, "P");
  c21->Update();
  emcal_130 = (TGraphErrors*)plot_xx_pi0_spectra("PizeroAuAu130_periph.dat","xT",1./14.); // 60-80%
  emcal_130->SetMarkerStyle(20);
  emcal_130->SetMarkerSize(1.3);
  emcal_130->SetMarkerColor(2);
  c21->cd();
  emcal_130->Draw("P");
  sprintf(sublabel,"EMCal, AuAu @ 130 GeV [60-80%%]");
  legend->AddEntry(emcal_130, sublabel, "P");

  sprintf(sublabel,"pp @ 200 GeV");
  legend->AddEntry(pp_200, sublabel, "P");
  c21->Update();

  // Ratios now ...

  TH1F *h_xT_130_periph = new TH1F("h_xT_130_periph","h_xT_130_periph",100,0.009,0.15);
  TH1F *h_xT_200_periph = new TH1F("h_xT_200_periph","h_xT_200_periph",100,0.009,0.15);
  Int_t points_130_periph = emcal_130->GetN();
  Int_t points_200_periph = pbsc_200->GetN();
  Double_t xT_130_periph_xT[points_130_periph], xT_130_periphYield[points_130_periph], exT_130_periphYield[points_130_periph];
  Double_t xT_200_periph_xT[points_200_periph], xT_200_periphYield[points_200_periph], exT_200_periphYield[points_200_periph];

  for (Int_t i=0; i<points_130_periph;i++)
    {
      emcal_130->GetPoint(i,xT_130_periph_xT[i],xT_130_periphYield[i]);
      exT_130_periphYield[i] = emcal_130->GetErrorY(i);
      binxT = emcal_130->GetXaxis()->FindBin( xT_130_periph_xT[i] );
      h_xT_130_periph->SetBinContent(binxT,xT_130_periphYield[i]);
      h_xT_130_periph->SetBinError(binxT,exT_130_periphYield[i]);
      cout << "130: " << binxT << " " << xT_130_periphYield[i] << endl;
    }
  for (Int_t i=0; i<points_200_periph;i++)
    {
      pbsc_200->GetPoint(i,xT_200_periph_xT[i],xT_200_periphYield[i]);
      exT_200_periphYield[i] = pbsc_200->GetErrorY(i);
      binxT = pbsc_200->GetXaxis()->FindBin( xT_200_periph_xT[i] );
      h_xT_200_periph->SetBinContent(binxT,xT_200_periphYield[i]);
      h_xT_200_periph->SetBinError(binxT,exT_200_periphYield[i]);
      cout << "200: " << binxT << " " << xT_200_periphYield[i] << endl;
    }

  char title2[50];
  sprintf(title2,"ratio_xT");
  c22 = new TCanvas(title2,title2,700,700);
  c22->cd();
  TH2F *frame2 = new TH2F(title2,title2,100,0.009,0.15,100,0,10);
  frame2->SetStats(0);
  frame2->SetTitle(title2);
  //frame2->SetYTitle("(#sqrt{s_{NN}}/200)^{n} d^{2}N^{#pi^{0}}/2#pi dp_{T}d#eta");
  frame2->SetYTitle("Ratio 200/130");
  frame2->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  frame2->GetYaxis()->SetTitleOffset(1.4);
  frame2->GetXaxis()->SetTitleOffset(1.2);
  frame2->Draw();
  c22->SetLogx();
  c22->Update();

  TH1F *ratio = 0;
  ratio = (TH1F*)h_xT_200_periph->Clone();   
  ratio->SetTitle("ratio_xT_200_130");
  ratio->Divide(h_xT_130_periph);
  ratio->Draw("e");
  c22->Update();

//   h_xT_200_periph->Draw("same");
//   h_xT_130_periph->Draw("same");
//   c22->Update();

}

//_____________________________________________________________________________
// OPTIONS: "central" , "minbias", "periph" (60-80%), "periph70" (70-80%)
//          "central_130_wa98" (central + NuclModifFactor RHIC200, RHIC130, WA98
//          "central_130" (central + NuclModifFactor RHIC200, RHIC130)
//          "all"

void plot_comparison_pp_AuAu_run2(const char* cut="noPID", const char* type="central_130_wa98")
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TString type_string = type;

  TCanvas *c22 = 0;
  TGraph *AuAuPi0 = 0;
  TGraph *ppPi0 = 0;
  TGraph *ppPi0Scaled = 0;

  TBox *pT_corr_errors = 0;

  char title[50];
  sprintf(title,"pi0_comparison_spectra_pp_AuAu");
  c22 = new TCanvas(title,title,600,700);
  c22->Range(-1.23529,-9.51842,9.28235,2.69298);
  c22->SetFillColor(0);
  c22->SetBorderMode(0);
  c22->SetBorderSize(0);
  c22->SetLeftMargin(0.11745);
  c22->SetRightMargin(0.0268456);
  c22->SetTopMargin(0.0567485);
  c22->SetFrameFillColor(0);
  c22->SetFrameBorderMode(0);                                                                            

  TH2F *frame = 0;

  if ( (type_string.Contains("central",TString::kExact)) || strcmp(type,"all") == 0) // any 0-10% central_*
    //  if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // 0-10% central
    {
      frame = new TH2F(title,title,21,0.,10.5,100,5E-07,100);
    }
  else if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // 60-80% Periph 
    {
      frame = new TH2F(title,title,18,0.,9.,100,5E-09,20);
    }
  else if (strcmp(type,"periph70") == 0 || strcmp(type,"all") == 0) // 70-80% Periph 
    {
      frame = new TH2F(title,title,18,0.,9.,100,5E-09,20);
    }
  else
    {
      frame = new TH2F(title,title,18,0.,9.,100,5E-09,100);
    }

  frame->SetStats(0);
  frame->SetTitle(title);
  //frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N /dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.4);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  c22->SetLogy();
  c22->Update();

  // pp pi0 are in mb/(GeV/c2)^{-2}
  Double_t pp_sigma = 42.;

  Double_t NCollCent  = getNcoll(0,10); // 0-10%
  Double_t eNCollCent = get_relat_errNcoll(0,10);

  Double_t NCollPeriph  = getNcoll(60,80); // 60-80%
  Double_t eNCollPeriph = get_relat_errNcoll(60,80);

  Double_t NCollPeriph70  = getNcoll(70,80); // 70-80%
  Double_t eNCollPeriph70 = get_relat_errNcoll(70,80);

  Double_t NColl30_40  = getNcoll(30,40); // 30-40%
  Double_t eNColl30_40 = get_relat_errNcoll(30,40);

  Double_t NCollMinBias  = getNcoll(0,100); // min.bias
  Double_t eNCollMinBias = get_relat_errNcoll(0,100);

  Double_t pp_luminosity_relat_error = 0.30; // Additional 20% error on pp yield (luminosity)

  ppPi0 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt");
  //ppPi0 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_sasha.txt");

  Int_t ppPoints = ppPi0->GetN();
  Double_t pTpp[ppPoints];
  Double_t epTpp[ppPoints];
  Double_t ppYield[ppPoints];
  Double_t ScaledppYield[ppPoints];
  Double_t eScaledppYield[ppPoints]; // pure pp scaled propagated error from pp spectrum
  Double_t eNcoll[ppPoints]; // separated Ncoll error on pp scaled spectrum

  //TString label = "#pi^{0} spectra pp #leftrightarrow AuAu: ";
  TLegend *legend = new TLegend(0.2685,0.8574,0.619,0.934,NULL,"brNDC");
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);
  legend->SetTextSize(0.037);
  c22->cd();
  legend->Draw();
  char sublabel[50];

  // Let's get the scaled pp spectrum
  for (Int_t i=0; i<ppPoints;i++)
    {
      ppPi0->GetPoint(i,pTpp[i],ppYield[i]);
      epTpp[i] = ppPi0->GetErrorX(i);
      eScaledppYield[i] = (ppPi0->GetErrorY(i))/ppYield[i];
      ScaledppYield[i] = ppYield[i]/pp_sigma;

      if ( (type_string.Contains("central",TString::kExact)) || strcmp(type,"all") == 0) // any 0-10% central_*
      //if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // 0-10% central
	{
	  cout << "Scaling pp point " << i << " by " << NCollCent 
	       << " i.e. from (" << pTpp[i] << " ," << ppYield[i] << ") to " ;
	  ScaledppYield[i] *= NCollCent;
// 	  eScaledppYield[i] = TMath::Sqrt(TMath::Power(eScaledppYield[i],2)+TMath::Power(eNCollCent,2));
	  eScaledppYield[i] *= ScaledppYield[i]; 
	  eNcoll[i] = TMath::Sqrt(TMath::Power(pp_luminosity_relat_error,2)+TMath::Power(eNCollCent,2));
	  eNcoll[i] *= ScaledppYield[i];
	  //cout << "eNcoll = " <<  eNcoll[i] << endl;
	}
      else if (strcmp(type,"minbias") == 0 || strcmp(type,"all") == 0) // minbias
	{
	  cout << "Scaling pp point " << i << " by " << NCollMinBias 
	       << " i.e. from (" << pTpp[i] << " ," << ppYield[i] << ") to " ;
	  ScaledppYield[i] *= NCollMinBias;
// 	  eScaledppYield[i] = TMath::Sqrt(TMath::Power(eScaledppYield[i],2)+TMath::Power(eNCollMinBias,2));
	  eScaledppYield[i] *= ScaledppYield[i];
	  eNcoll[i] = TMath::Sqrt(TMath::Power(pp_luminosity_relat_error,2)+TMath::Power(eNCollMinBias,2));
	  eNcoll[i] *= ScaledppYield[i];
	}
      else if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // 60-80% Periph 
	{
	  cout << "Scaling pp point " << i << " by " << NCollPeriph 
	       << " i.e. from (" << pTpp[i] << " ," << ppYield[i] << ") to " ;
	  ScaledppYield[i] *= NCollPeriph;
// 	  eScaledppYield[i] = TMath::Sqrt(TMath::Power(eScaledppYield[i],2)+TMath::Power(eNCollPeriph,2));
	  eScaledppYield[i] *= ScaledppYield[i];
	  eNcoll[i] = TMath::Sqrt(TMath::Power(pp_luminosity_relat_error,2)+TMath::Power(eNCollPeriph,2));
	  eNcoll[i] *= ScaledppYield[i];
	}
      else if (strcmp(type,"periph70") == 0 || strcmp(type,"all") == 0) // 70-80% Periph 
	{
	  cout << "Scaling pp point " << i << " by " << NCollPeriph70 
	       << " i.e. from (" << pTpp[i] << " ," << ppYield[i] << ") to " ;
	  ScaledppYield[i] *= NCollPeriph70;
// 	  eScaledppYield[i] = TMath::Sqrt(TMath::Power(eScaledppYield[i],2)+TMath::Power(eNCollPeriph70,2));
	  eScaledppYield[i] *= ScaledppYield[i];
	  eNcoll[i] = TMath::Sqrt(TMath::Power(pp_luminosity_relat_error,2)+TMath::Power(eNCollPeriph70,2));
	  eNcoll[i] *= ScaledppYield[i];
	}
      else if (strcmp(type,"30_40") == 0 || strcmp(type,"all") == 0) // 30-40% Periph 
	{
	  cout << "Scaling pp point " << i << " by " << NColl30_40
	       << " i.e. from (" << pTpp[i] << " ," << ppYield[i] << ") to " ;
	  ScaledppYield[i] *= NColl30_40;
// 	  eScaledppYield[i] = TMath::Sqrt(TMath::Power(eScaledppYield[i],2)+TMath::Power(eNCollPeriph,2));
	  eScaledppYield[i] *= ScaledppYield[i];
	  eNcoll[i] = TMath::Sqrt(TMath::Power(pp_luminosity_relat_error,2)+TMath::Power(eNColl30_40,2));
	  eNcoll[i] *= ScaledppYield[i];
	}
      cout << "("<< pTpp[i] << " ," << ScaledppYield[i] << ")" << endl ;
    }

  ppPi0Scaled = new TGraphErrors( ppPoints, pTpp, ScaledppYield, epTpp, eScaledppYield);
  ppPi0Scaled->SetMarkerStyle(29);
  ppPi0Scaled->SetMarkerSize(1.9);
  ppPi0Scaled->SetMarkerColor(9);

//   // Separated Ncoll errors drawn as boxes
//   for (int i=0; i<ppPoints; i++) 
//     {
//       Double_t logNcollerrSup = TMath::Log(ScaledppYield[i]+eNcoll[i]); // TBoxes don't like log scales
//       Double_t logNcollerrInf = TMath::Log(ScaledppYield[i]-eNcoll[i]); // TBoxes don't like log scales
//       pT_corr_errors = new TBox(pTpp[i]-0.25,logNcollerrInf,
// 				pTpp[i]+0.25,logNcollerrSup);
//       pT_corr_errors->SetFillColor(kYellow);
//       pT_corr_errors->Draw();
//   }
    
  // hagedorn fit to the scaled pp data
  Double_t ptmin = 6.00;
  Double_t ptmax = 13.0;
  Double_t p0 = 1.72;
  Double_t n_exp = 10.5;
  TF1 *hagedorn2 = new TF1("hagedorn2","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn2->SetParameters(10000., p0, n_exp);
  hagedorn2->SetParLimits(0, 0.0001, 20000000);
  hagedorn2->SetParLimits(1, p0, p0);
  hagedorn2->SetParLimits(2, n_exp-4, n_exp+4);
  hagedorn2->SetParNames("A Hag","p_o", "n");
  hagedorn2->SetLineWidth(1);
  //hagedorn2->Draw("same");

  // Separated Ncoll+ pp-luminosity errors drawn as functions
  Double_t ppNcoll_min[ppPoints];
  Double_t ppNcoll_max[ppPoints];

  for (int i=0; i<ppPoints; i++) 
    {
      ppNcoll_min[i] = ScaledppYield[i]+eNcoll[i];
      ppNcoll_max[i] = ScaledppYield[i]-eNcoll[i];
    }

  TGraph *ppNcoll_error_max = new TGraph(ppPoints, pTpp, ppNcoll_max);
  ppNcoll_error_max->SetLineColor(50);
  ppNcoll_error_max->SetFillColor(50);
  ppNcoll_error_max->Draw("FC");

  TGraph *ppNcoll_error_min = new TGraph(ppPoints, pTpp, ppNcoll_min);
  ppNcoll_error_min->SetLineColor(50);
  ppNcoll_error_min->SetFillColor(0);
  ppNcoll_error_min->Draw("FC");

//   TF1 *fitmin = new TF1("fitmin","[0]*([1]/(x+[1])^[2]", 1.0, pTpp[ppPoints]);
//   fitmin->SetParameters(10000., p0, n_exp);
//   fitmin->SetParLimits(0, 0.0001, 20000000);
//   fitmin->SetParLimits(1, p0, p0);
//   fitmin->SetParLimits(2, n_exp-4, n_exp+4);
//   fitmin->SetParNames("A Hag","p_o", "n");
//   fitmin->SetLineWidth(1);
//   ppNcoll_error_min->Fit("fitmax","QR","");

//   TF1 *fitmax = new TF1("fitmax","[0]*([1]/(x+[1])^[2]", 1.0, pTpp[ppPoints]);
//   fitmax->SetParameters(10000., p0, n_exp);
//   fitmax->SetParLimits(0, 0.0001, 20000000);
//   fitmax->SetParLimits(1, p0, p0);
//   fitmax->SetParLimits(2, n_exp-4, n_exp+4);
//   fitmax->SetParNames("A Hag","p_o", "n");
//   fitmax->SetLineWidth(1);
//   ppNcoll_error_max->Fit("fitmin","QR","");

  //
  char title2[50];
  char title3[50];

  if ( (type_string.Contains("central",TString::kExact)) || strcmp(type,"all") == 0) // any central_*
    //if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // 0-10% central 
    {
      AuAuPi0 = (TGraphErrors*)plot_pi0_spectra_run2(0,10,cut,"full","no");
      AuAuPi0->SetMarkerStyle(20);
      AuAuPi0->SetMarkerSize(1.2);
      AuAuPi0->SetMarkerColor(2);
      c22->cd();
      AuAuPi0->Draw("P");
      sprintf(sublabel,"#pi^{0} AuAu @ 200 GeV [0-10%%]");
      legend->AddEntry(AuAuPi0, sublabel, "P");
      legend->SetTextColor(2);
      c22->cd();
      ppPi0Scaled->Draw("P");
      ppPi0Scaled->Fit("hagedorn2","QR","");
      sprintf(sublabel,"#pi^{0} pp @ 200 GeV [N_{coll} (0-10%%) scaled]");
      legend->AddEntry(ppPi0Scaled, sublabel, "P");
      legend->SetTextColor(9);
      c22->Update();
      //sprintf(title3,"R_{AA}= Yield_{AuAu}(0-10%%)/[N_{coll 0-10%%}Yield_{pp}]");
      sprintf(title2,"NuclModifFactorCentral");
      sprintf(title3,"R_{AA}= Yield_{AuAu}/[N_{coll}Yield_{pp}]");
    }
  if (strcmp(type,"minbias") == 0 || strcmp(type,"all") == 0) // minbias
    {
      AuAuPi0 = (TGraphErrors*)plot_pi0_spectra_run2(100,100,cut,"full","no");
      AuAuPi0->SetMarkerStyle(20);
      AuAuPi0->SetMarkerSize(1.2);
      AuAuPi0->SetMarkerColor(2);
      c22->cd();
      AuAuPi0->Draw("P");
      sprintf(sublabel,"#pi^{0} AuAu @ 200 GeV [min.bias]");
      legend->AddEntry(AuAuPi0, sublabel, "P");
      legend->SetTextColor(2);
      c22->cd();
      ppPi0Scaled->Draw("P");
      ppPi0Scaled->Fit("hagedorn2","QR","");
      sprintf(sublabel,"#pi^{0} pp @ 200 GeV [N_{coll} (min.bias) scaled]");
      legend->AddEntry(ppPi0Scaled, sublabel, "P");
      legend->SetTextColor(9);
      c22->Update();
      //sprintf(title3,"R_{AA}= Yield_{AuAu}/[N_{coll min.bias}Yield_{pp}]");
      sprintf(title2,"NuclModifFactorMinBias");
      sprintf(title3,"R_{AA}= Yield_{AuAu}/[N_{coll}Yield_{pp}]");
    }
  if (strcmp(type,"periph") == 0 || strcmp(type,"all") == 0) // 60-80% Periph 
    {
      AuAuPi0 = (TGraphErrors*)plot_pi0_spectra_run2(60,80,cut,"full","no");
      AuAuPi0->SetMarkerStyle(20);
      AuAuPi0->SetMarkerSize(1.2);
      AuAuPi0->SetMarkerColor(2);
      c22->cd();
      AuAuPi0->Draw("P");
      sprintf(sublabel,"#pi^{0} @ AuAu 200 GeV [60-80%%]");
      legend->AddEntry(AuAuPi0, sublabel, "P");
      legend->SetTextColor(2);
      c22->cd();
      ppPi0Scaled->Draw("P");
      ppPi0Scaled->Fit("hagedorn2","QR","");
      sprintf(sublabel,"#pi^{0} pp @ 200 GeV [N_{coll} (60-80%%) scaled]");
      legend->AddEntry(ppPi0Scaled, sublabel, "P");
      legend->SetTextColor(9);
      c22->Update();
      //sprintf(title3,"R_{AA}= Yield_{AuAu}(60-80%%)/[N_{coll 60-80%%}Yield_{pp}]");
      sprintf(title2,"NuclModifFactorPeriph");
      sprintf(title3,"R_{AA}= Yield_{AuAu}/[N_{coll}Yield_{pp}]");
    }
  if (strcmp(type,"periph70") == 0 || strcmp(type,"all") == 0) // 70-80% Periph 
    {
      AuAuPi0 = (TGraphErrors*)plot_pi0_spectra_run2(70,80,cut,"full","no");
      AuAuPi0->SetMarkerStyle(20);
      AuAuPi0->SetMarkerSize(1.2);
      AuAuPi0->SetMarkerColor(2);
      c22->cd();
      AuAuPi0->Draw("P");
      sprintf(sublabel,"#pi^{0} @ AuAu 200 GeV [70-80%%]");
      legend->AddEntry(AuAuPi0, sublabel, "P");
      legend->SetTextColor(2);
      c22->cd();
      ppPi0Scaled->Draw("P");
      ppPi0Scaled->Fit("hagedorn2","QR","");
      sprintf(sublabel,"#pi^{0} pp @ 200 GeV [N_{coll} (70-80%%) scaled]");
      legend->AddEntry(ppPi0Scaled, sublabel, "P");
      legend->SetTextColor(9);
      c22->Update();
      //sprintf(title3,"R_{AA}= Yield_{AuAu}(70-80%%)/[N_{coll 70-80%%}Yield_{pp}]");
      sprintf(title2,"NuclModifFactorPeriph");
      sprintf(title3,"R_{AA}= Yield_{AuAu}/[N_{coll}Yield_{pp}]");
    }
  if (strcmp(type,"30_40") == 0 || strcmp(type,"all") == 0) // 30-40% Periph 
    {
      AuAuPi0 = (TGraphErrors*)plot_pi0_spectra_run2(30,40,cut,"full","no");
      AuAuPi0->SetMarkerStyle(20);
      AuAuPi0->SetMarkerSize(1.2);
      AuAuPi0->SetMarkerColor(2);
      c22->cd();
      AuAuPi0->Draw("P");
      sprintf(sublabel,"#pi^{0} @ AuAu 200 GeV [30-40%%]");
      legend->AddEntry(AuAuPi0, sublabel, "P");
      legend->SetTextColor(2);
      c22->cd();
      ppPi0Scaled->Draw("P");
      ppPi0Scaled->Fit("hagedorn2","QR","");
      sprintf(sublabel,"#pi^{0} pp @ 200 GeV [Ncoll(30-40%%) scaled]");
      legend->AddEntry(ppPi0Scaled, sublabel, "P");
      legend->SetTextColor(9);
      c22->Update();
      //sprintf(title3,"R_{AA}= Yield_{AuAu}(70-80%%)/[N_{coll 70-80%%}Yield_{pp}]");
      sprintf(title2,"NuclModifFactor30_40");
      sprintf(title3,"R_{AA}= Yield_{AuAu}/[N_{coll}Yield_{pp}]");
    }

  c22->cd();
  ppPi0Scaled->Draw("P");
  AuAuPi0->Draw("P");
  sprintf(sublabel,"Uncertainty in N_{coll} scaling");
  legend->AddEntry(ppNcoll_error_max, sublabel, "P");
  legend->SetTextColor(50);
//   fitmax->SetLineColor(1);
//   fitmax->Draw("same");
//   fitmin->SetLineColor(1);
//   fitmin->Draw("same");

  c22->Update();

  //_____________________________________________________________________________
  // nuclear modification factor plots

  TCanvas *c23 = new TCanvas(title2,title2,375,75,600,600);
  c23->Range(-1.10095,-0.124486,8.54857,1.1249);
  c23->SetFillColor(0);
  c23->SetBorderMode(0);
  c23->SetBorderSize(0);
  c23->SetLeftMargin(0.114094);
  c23->SetRightMargin(0.005);
  c23->SetTopMargin(0.02);
  c23->SetFrameFillColor(0);
  c23->SetFrameBorderMode(0);

  TGraph *RatioAuAuPi0 = 0;

  Double_t upper_RAA = 1.5;
  Double_t upper_pT = 10.5;

  if (strcmp(type,"central_130") == 0 || strcmp(type,"all") == 0) upper_RAA = 1.1;
  if (strcmp(type,"central_130_wa98") == 0 || strcmp(type,"all") == 0) upper_RAA = 2.8;
  if (strcmp(type,"minbias") == 0 ) upper_pT = 10.;

  TH2F *frame2 = new TH2F(title2,title2,(Int_t)upper_pT*2,0.,upper_pT,10,0.,upper_RAA);
  frame2->SetStats(0);
  frame2->SetTitle(title2);
  frame2->SetYTitle(title3);
  frame2->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame2->GetYaxis()->SetTitleOffset(1.1);
  frame2->GetYaxis()->SetTitleSize(0.05);
  frame2->GetXaxis()->SetTitleOffset(1.0);
  frame2->GetXaxis()->SetTitleSize(0.05);

  //_____________________________________________________________________________
  // Nuclear modification factors: Ratio AuAu over Scaled pp
  Int_t Npoints = TMath::Min(ppPoints,AuAuPi0->GetN());//+1;
  Double_t pT[Npoints];
  Double_t epT[Npoints];
  Double_t yieldAuAuPi0[Npoints];
  Double_t ratioAuAu_pp_Pi0[Npoints];
  Double_t eratioAuAu_pp_Pi0[Npoints] ;
  Double_t eratioNcoll[Npoints] ;
  Double_t fitted_pp_scaled_yield = 0;
  Double_t eyieldAuAuPi0 = 0;

  Int_t ii;
  for( ii=0; ii<Npoints; ii++) 
    {
      AuAuPi0->GetPoint(ii,pT[ii],yieldAuAuPi0[ii]);
      fitted_pp_scaled_yield = hagedorn2->Eval(pT[ii]);

      //if (ScaledppYield[ii]!=0) 
      //{
      // the highest pT points where binning is different:
      if (pT[ii] > 7.0 )// && fitted_pp_scaled_yield!=0)
	{
	  ratioAuAu_pp_Pi0[ii] = yieldAuAuPi0[ii]/fitted_pp_scaled_yield; // <--- Denom. is hagedorn fitted pp-scaled
	}
      else
	{
	  ratioAuAu_pp_Pi0[ii] = yieldAuAuPi0[ii]/ScaledppYield[ii]; // <--- Denom. is straightforw. pp-scaled bin content 
	}
      eyieldAuAuPi0 = AuAuPi0->GetErrorY(ii)/yieldAuAuPi0[ii] ;
      cout << "%error in AuAu yield: " << eyieldAuAuPi0 << endl;
      eratioAuAu_pp_Pi0[ii] = TMath::Sqrt(TMath::Power(eScaledppYield[ii]/ScaledppYield[ii],2)+
					  TMath::Power(eyieldAuAuPi0,2));
      //eratioNcoll[ii] = eNCollCent*ratioAuAu_pp_Pi0[ii];
      eratioNcoll[ii] = eNcoll[ii]/ScaledppYield[ii];
      //eratioNcoll[ii] = TMath::Sqrt(TMath::Power(eNCollCent,2)+TMath::Power(pp_luminosity_relat_error,2));
      epT[ii] = AuAuPi0->GetErrorX(ii);
      //}
      eratioAuAu_pp_Pi0[ii] *= ratioAuAu_pp_Pi0[ii];
      eratioNcoll[ii] *= ratioAuAu_pp_Pi0[ii];

      cout << "pT= " << pT[ii] << " AuAu/pp ratio = " << ratioAuAu_pp_Pi0[ii]
	   << " ratio bin-to-bin = "  << yieldAuAuPi0[ii]/ScaledppYield[ii];
      cout << "[ pp scaled = " << ScaledppYield[ii] << " hagedorn pp scaled = " << fitted_pp_scaled_yield << " ]";
      cout << " errs: " << eratioAuAu_pp_Pi0[ii] << " " << eratioNcoll[ii] << endl;
    }

//   if (strcmp(type,"central") == 0 || strcmp(type,"30_40") == 0) // 0-10% central and 30-40%
//     {
//       // two highest pT points where binning is different:
//       float ratio_at_high_pt = yieldAuAuPi0[Npoints-1]/fitted_pp_scaled_yield;
//       ratioAuAu_pp_Pi0[Npoints-1] = ratio_at_high_pt;
//       ratio_at_high_pt = yieldAuAuPi0[Npoints]/fitted_pp_scaled_yield;
//       ratioAuAu_pp_Pi0[Npoints] = ratio_at_high_pt;
//       //ratio_at_high_pt = yieldAuAuPi0[Npoints-2]/fitted_pp_scaled_yield;
//       //ratioAuAu_pp_Pi0[Npoints-2] = ratio_at_high_pt;
//     }
      
  RatioAuAuPi0 = new TGraphErrors(Npoints,pT,ratioAuAu_pp_Pi0,epT,eratioAuAu_pp_Pi0);
  RatioAuAuPi0->SetMarkerStyle(20);
  RatioAuAuPi0->SetMarkerSize(1.2);
  RatioAuAuPi0->SetMarkerColor(4);
  RatioAuAuPi0->Print();
  c23->cd();
  frame2->Draw();
  
  TLine *line = new TLine(0.25,1.0,8.5,1.0);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(4);
  line->Draw("same");

  RatioAuAuPi0->Draw("P");

  //_____________________________________________________________________________
  // Comparison to WA98 and PHENIX 130 Central
  if (strcmp(type,"central_130") == 0) upper_RAA = 1.4;
  if (strcmp(type,"central_130_wa98") == 0 || strcmp(type,"all") == 0) upper_RAA = 2.8;

  if ( (type_string.Contains("central",TString::kExact)) || strcmp(type,"all") == 0) // any central_*
    //if (strcmp(type,"central") == 0 || strcmp(type,"all") == 0) // 0-10% central 
    {

      TGraphErrors *wa98_RAA = 0;
      if (strcmp(type,"central_130_wa98") == 0) wa98_RAA = (TGraphErrors*)get_pi0_WA98_RAA();
      TGraphErrors *emcal130_RAA = (TGraphErrors*)get_pi0_130_RAA();
      c23->cd();
      
      TLegend *le = new TLegend(0.43,0.70,0.95,0.956,"  R_{AA} #pi^{0} 0-10% Central:","brNDC");
      le->AddEntry(RatioAuAuPi0,"Au+Au #sqrt{s_{NN}}=200 GeV ","P");
      le->AddEntry(emcal130_RAA,"Au+Au #sqrt{s_{NN}}=130 GeV ","P");
      if (strcmp(type,"central_130_wa98") == 0) le->AddEntry(wa98_RAA,"Pb+Pb #sqrt{s_{NN}}=17.3 GeV ","P");
      le->SetFillColor(kWhite);
      le->Draw();

      // Draw PHENIX 130 errors
      TBox *box = 0;
      TClonesArray *array_of_boxes = 0;
      array_of_boxes = (TClonesArray*)plot_pi0_130_errors();
      Int_t boxes = array_of_boxes->GetLast()+1;
      for (Int_t j=0; j<boxes;j++)
	{
	  box = ((TBox *) (*array_of_boxes)[j]);
	  c23->cd(); 
	  box->Draw();
	}

      if (strcmp(type,"central_130_wa98") == 0) 
	{
	  // Draw WA98 errors
	  TLine *lu = 0; TLine *lul = 0 ; TLine *lur = 0 ; 
	  TLine *ll = 0; TLine *lll = 0 ; TLine *llr = 0 ;
	  TClonesArray *array_of_lines = 0;
	  array_of_lines = (TClonesArray*)plot_WA98_RAA_errors();
	  Int_t lines = array_of_lines->GetLast()+1;
	  for (Int_t j=0; j<(lines-6);j++)
	    {
	      lu  = ((TLine *) (*array_of_lines)[j++]);
	      lul = ((TLine *) (*array_of_lines)[j++]);
	      lur = ((TLine *) (*array_of_lines)[j++]);
	      ll  = ((TLine *) (*array_of_lines)[j++]);
	      lll = ((TLine *) (*array_of_lines)[j++]);
	      llr = ((TLine *) (*array_of_lines)[j]);
	      
	      c23->cd();
	      lu->Draw(); lul->Draw(); lur->Draw();
	      ll->Draw(); lll->Draw(); llr->Draw();
	    }
	}

      // draw separated pT-corr errors as boxes
      //pT_corr_errors->Delete();
      for (int i=0; i<Npoints; i++) 
	{
	  pT_corr_errors = new TBox(pT[i]-0.25,ratioAuAu_pp_Pi0[i]-eratioNcoll[i],
				    pT[i]+0.25,ratioAuAu_pp_Pi0[i]+eratioNcoll[i]);
	  if (strcmp(type,"central_130") == 0) pT_corr_errors->SetFillColor(50);
	  else pT_corr_errors->SetFillColor(kYellow);
	  pT_corr_errors->Draw();
	}

      if (strcmp(type,"central_130_wa98") == 0) wa98_RAA->Draw("P");
      emcal130_RAA->SetMarkerStyle(29);
      emcal130_RAA->SetMarkerSize(2.0);
      emcal130_RAA->Draw("P");
      RatioAuAuPi0->SetMarkerSize(2.0);
      RatioAuAuPi0->SetMarkerColor(2);
      RatioAuAuPi0->Draw("P");
      c23->Update();

    } // any central
  else
    {
      for (int i=0; i<Npoints; i++) 
	{
	  pT_corr_errors = new TBox(pT[i]-0.25,ratioAuAu_pp_Pi0[i]-eratioNcoll[i],
				    pT[i]+0.25,ratioAuAu_pp_Pi0[i]+eratioNcoll[i]);
	  pT_corr_errors->SetFillColor(kYellow);
	  pT_corr_errors->Draw();
	}
      
      RatioAuAuPi0->SetMarkerColor(2);
      RatioAuAuPi0->Draw("P");

    }

  c23->Update();
}

//_____________________________________________________________________________

void plot_all_correction_factors()
{
  Int_t Cent1 = 0;
  Int_t Cent2 = 10;

  for (Int_t i=0; i<=CentClasses; i+=2)
    {
      if (i==20) 
	{
	  plot_Correction_Factors(100,100);
	}
      else if (Cent1 == 80)
	{ 
	  plot_Correction_Factors(Cent1,92);
	}
      else if (Cent1 != 90)
	{ 
	  plot_Correction_Factors(Cent1,Cent2);
	}
      Cent1 +=10;
      Cent2 = Cent1+10;
    }
}

//_____________________________________________________________________________
TGraphErrors *plot_ratios_centralities(const Int_t Cent1 =  0, const Int_t Cent2 = 10, 
				       const Int_t CentRef1 = 60, const Int_t CentRef2 = 80,
				       const char* cut="noPID",
				       const char* plot="yes", const char* readfromraw="yes")
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TGraphErrors *ReferenceSpectrum = 0;
  TGraphErrors *DenominatorSpectrum = 0;

  char fileRef[300];
  char fileDen[300];

  if (strcmp(readfromraw,"no") == 0) // read from corrected data files on disk
    {
      sprintf(fileDen,"pi0_pbsc_%d_%d.txt",Cent1,Cent2);
      DenominatorSpectrum = (TGraphErrors*)plot_xx_pi0_spectra(fileDen);
      if ( !DenominatorSpectrum ) 
	{
	  sprintf(fileDen,"pi0_pbsc_comb_%d_%d.txt",Cent1,Cent2);
	  DenominatorSpectrum = (TGraphErrors*)plot_xx_pi0_spectra(fileDen);
	}

      sprintf(fileRef,"pi0_pbsc_%d_%d.txt",CentRef1,CentRef2);      
      ReferenceSpectrum = (TGraphErrors*)plot_xx_pi0_spectra(fileRef);
      if ( !ReferenceSpectrum ) 
	{
	  sprintf(fileRef,"pi0_pbsc_comb_%d_%d.txt",Cent1,Cent2);
	  ReferenceSpectrum = (TGraphErrors*)plot_xx_pi0_spectra(fileRef);
	}
    }
  else // reconstruct the spectra
    {
      ReferenceSpectrum   = (TGraphErrors*)plot_pi0_spectra_run2(CentRef1,CentRef2,cut,"full",plot);
      DenominatorSpectrum = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut,"full",plot);
    }

  Int_t NpTbins = TMath::Min(ReferenceSpectrum->GetN(),DenominatorSpectrum->GetN());//+1;

  Double_t pT[NpTbins];
  Double_t e_pT[NpTbins];
  Double_t Ratio[NpTbins];
  Double_t e_Ratio[NpTbins];
  Double_t eNcoll_Ratio[NpTbins];
  Double_t x,yRef,yDen, e_yRef, e_yDen;

  Int_t CC = getCentralityClass(Cent1, Cent2);
  Int_t CCref = getCentralityClass(CentRef1, CentRef2);

  TGraphErrors *problem = 0;
  if ( (CC == -1) || (CCref == 1) ) return problem;

  Double_t NcollRef = getNcoll(CentRef1,CentRef2);
  Double_t NcollDen = getNcoll(Cent1,Cent2);
  Double_t eNcollRef = get_relat_errNcoll(CentRef1,CentRef2);
  Double_t eNcollDen = get_relat_errNcoll(Cent1,Cent2);

  cout << "CC (ref):" << CCref <<" Ncoll is " << NcollRef << " %err: " << eNcollRef << endl;
  cout << "CC      :" << CC <<" Ncoll is " << NcollDen << " %err: " << eNcollDen << endl;

  char title[50]; 
  char name[50];
  sprintf(title,"#pi^{0} Yield(%d-%d%%) / Yield(%d-%d%%)",Cent1,Cent2,CentRef1,CentRef2);
  sprintf(name,"ratio_%d_%d_to_%d_%d",Cent1,Cent2,CentRef1,CentRef2);

  Int_t ii;
  cout << title << endl;
  cout << " Output format is: pT(GeV/c)  pTerror(GeV/c)   Ratio   Ratio_error(propag_cent_indep_error) ";
  cout << " pT_correlated_error(Ncoll_error)" << endl;

  for(ii=0;ii<NpTbins;ii++) 
    {
      ReferenceSpectrum->GetPoint(ii,x,yRef);
      DenominatorSpectrum->GetPoint(ii,x,yDen);
      pT[ii]    = x;
      e_pT[ii]  = ReferenceSpectrum->GetErrorX(ii);
      //Ratio[ii] = yRef/NcollRef/yDen*NcollDen;
      Ratio[ii] = (yDen/NcollDen)/(yRef/NcollRef);

      Double_t Cancelled_Syst_error = get_centrality_indep_relat_errors(ii) ;

      e_yRef = TMath::Sqrt( TMath::Power(ReferenceSpectrum->GetErrorY(ii),2) -
			    TMath::Power(Cancelled_Syst_error*yRef,2)            );

      e_yDen = TMath::Sqrt( TMath::Power(DenominatorSpectrum->GetErrorY(ii),2) -
			    TMath::Power(Cancelled_Syst_error*yDen,2)            );

//       e_Ratio[ii] = TMath::Sqrt( TMath::Power(NcollDen/NcollRef*e_yRef/yDen,2)+
// 				 TMath::Power(NcollDen/NcollRef*e_yDen*yRef/yDen/yDen,2) +
// 				 TMath::Power(eNcollRef*Ratio[ii]/100.,2)                +
// 				 TMath::Power(eNcollDen*Ratio[ii]/100.,2)        );

      e_Ratio[ii] = TMath::Sqrt( TMath::Power(NcollRef/NcollDen*e_yDen/yRef,2)+
				 TMath::Power(NcollRef/NcollDen*e_yRef*yDen/yRef/yRef,2) );

      eNcoll_Ratio[ii] = TMath::Sqrt( TMath::Power(eNcollDen*Ratio[ii],2) +
				      TMath::Power(eNcollRef*Ratio[ii],2)  );

      cout << "  " << setprecision(4) << pT[ii] << " " << e_pT[ii] ;
      cout << "  " << setprecision(4) << Ratio[ii] << " " << e_Ratio[ii];
      cout << "  " << setprecision(4) << eNcoll_Ratio[ii];
      cout << endl;
    }

  TCanvas *c5 = new TCanvas(name,name,600,600);
  //c5->Range(-0.12,-0.43,7.25,3.86);
  TGraphErrors *RatioPi0 = new TGraphErrors( NpTbins, pT, Ratio, e_pT, e_Ratio);
  Double_t max_yield = 1.5 ;//3.*Ratio[0];
  Double_t min_yield = 0. ;//*Ratio[NpTbins-1];
  
  RatioPi0->SetMaximum(max_yield);
  RatioPi0->SetMinimum(min_yield);
  RatioPi0->SetMarkerStyle(20);
  RatioPi0->SetMarkerColor(4);
  RatioPi0->SetTitle(title);
  RatioPi0->Draw("AP");
  RatioPi0->GetYaxis()->SetTitleOffset(1.0);
  RatioPi0->GetYaxis()->SetTitleSize(0.050);
  RatioPi0->GetYaxis()->SetTitle(title);
  RatioPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
  RatioPi0->GetXaxis()->SetTitleSize(0.05);

//   TLegend *le = new TLegend(0.437919,0.63587,0.862416,0.871377,"","brNDC");
//   le->AddEntry(RatioPi0,title);
//   le->SetFillColor(kWhite);
//   le->Draw();

  // Separated Ncoll errors drawn as functions
  Double_t eNcoll_min[NpTbins];
  Double_t eNcoll_max[NpTbins];

  for (int i=0; i<NpTbins; i++) 
    {
      eNcoll_min[i] = Ratio[i]+eNcoll_Ratio[i];
      eNcoll_max[i] = Ratio[i]-eNcoll_Ratio[i];
    }

  TGraph *eNcoll_error_max = new TGraph(NpTbins, pT, eNcoll_max);
  eNcoll_error_max->SetLineColor(50);
  eNcoll_error_max->SetFillColor(50);

  TGraph *eNcoll_error_min = new TGraph(NpTbins, pT, eNcoll_min);
  eNcoll_error_min->SetLineColor(50);
  eNcoll_error_min->SetFillColor(0);
  //  eNcoll_error_max->Draw("L");
  //eNcoll_error_min->Draw("L");

  // Separated Ncoll errors draw sys. errors as boxes
  TBox *b = 0;
  for (int i=0; i<NpTbins; i++) 
    {
      b = new TBox(pT[i]-0.25,eNcoll_min[i],
		   pT[i]+0.25,eNcoll_max[i]);
      
      b->SetFillColor(kYellow);
      b->Draw();
  }

  RatioPi0->Draw("P");

  TLine *line = new TLine(0.80,1.0,6.8,1.0);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(4);
  line->Draw("same");

  c5->Update();

  if (strcmp(plot,"no") == 0) c5->Close();

  return RatioPi0;
  
}

//_____________________________________________________________________________
void plot_all_ratios()
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  char title1[50];
  char title2[50];
  sprintf(title1,"pi0_centrality_ratios_set1");
  sprintf(title2,"pi0_centrality_ratios_set2");

  TCanvas *c10 = new TCanvas(title1,title1,900,700);
  //gStyle->SetOptStat(11111111);
  c10->Divide(2,2,0,0);

  TCanvas *c10b = new TCanvas(title2,title2,900,700);
  //gStyle->SetOptStat(11111111);
  c10b->Divide(2,2,0,0);

  Int_t Cent1 = 0;
  Int_t Cent2 = 10;

  TGraphErrors *bidon = 0;

  for (Int_t i=0; i<=CentClasses; i+=2)
    {
      Int_t subpad = i/2;
      if (subpad>4) subpad -= 4;
      if (Cent1 == 80)
	{ 
	  bidon = (TGraphErrors*)plot_ratios_centralities(80,92,60,80,"no");
	  c10b->cd(subpad);
	  bidon->Draw("AP");
	  c10b->Update();
	}
      else if (Cent1 == 100)
	{ 
	  bidon = (TGraphErrors*)plot_ratios_centralities(100,100,60,80,"no");
	  c10b->cd(subpad);
	  bidon->Draw("AP");
	  c10b->Update();
	}
      else if ( (Cent1 != 0) && (Cent2 != 10) && (Cent1 != 90) ) // no need to do the ratio 0-10/0-10
	{ 
	  bidon = (TGraphErrors*)plot_ratios_centralities(Cent1,Cent2,60,80,"no");
	  if (Cent1<50) c10->cd(subpad);
	  else c10b->cd(subpad);
	  bidon->Draw("AP");
	  c10->Update();
	  c10b->Update();
	}
      Cent1 +=10;
      Cent2 = Cent1+10;
    }
  c10->Update();
  c10b->Update();
}

//_____________________________________________________________________________

// ratios_spectra_centralities(const Int_t CC1, const Int_t CC2)
// {
//   gROOT->SetStyle("Plain");
//   gStyle->SetOptTitle(0);
//   gStyle->SetPalette(1);

//   Int_t CC = getCentralityClass(Cent1,Cent2);
//   Double_t eventsCC = getEvents(Cent1,Cent2);
//   cout << " Calculating " << spectra << " pi0 spectrum for centrality class: " << CC 
//        << " (" << Cent1 << " - " << Cent2 << "%)" << endl;

//   char title[300];
//   sprintf(title,"Centrality Class %d_%d",Cent1,Cent2);


// }

// //_____________________________________________________________________________
// void calculate_pT_mean()
// {

//   Double_t ptmin = 1.0;
//   Double_t ptmax = 7.0;
//   Double_t p0 = 1.72;
//   Double_t n_exp = 10.5;

//     TF1 *localhagedorn = new TF1("localhagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
//     localhagedorn->SetParameters(10000., p0, n_exp,0.);
//     localhagedorn->SetParLimits(0, 1, 20000000);
//     localhagedorn->SetParLimits(1, p0, p0);
//     localhagedorn->SetParLimits(2, n_exp-4, n_exp+4);
//     localhagedorn->SetParNames("A Hag","p_o", "n");
//     localhagedorn->Draw("same");

//     TF1 *xhagedorn  = new TF1("xhagedorn", "[0]*x*([1]/(x+[1])^[2])", ptmin, ptmax);
//     xhagedorn->SetParameter(0,A_Hag);
//     xhagedorn->SetParameter(1,p_0);
//     xhagedorn->SetParameter(2,n);
//     xhagedorn->SetLineColor(2);
//     TF1 *xhagedorn1  = new TF1("xhagedorn1", "[0]*x*([1]/(x+[1])^[2])", ptmin, ptmax);
//     xhagedorn1->SetParameter(0,A_Hag);
//     xhagedorn1->SetParameter(1,p_0);
//     xhagedorn1->SetParameter(2,n+hagedorn->GetParError(2));
//     xhagedorn1->SetLineColor(2);
//     TF1 *xhagedorn2  = new TF1("xhagedorn2", "[0]*x*([1]/(x+[1])^[2])", ptmin, ptmax);
//     xhagedorn2->SetParameter(0,A_Hag);
//     xhagedorn2->SetParameter(1,p_0);
//     xhagedorn2->SetParameter(2,n-hagedorn->GetParError(2));
//     xhagedorn2->SetLineColor(2);
//     //    xhagedorn->Draw("same");
//     TF1 *hagedorn1  = new TF1("hagedorn1", "[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
//     hagedorn1->SetParameter(0,A_Hag);
//     hagedorn1->SetParameter(1,p_0);
//     hagedorn1->SetParameter(2,n+hagedorn->GetParError(2));
//     hagedorn1->SetLineColor(2);
//     hagedorn1->Draw("same");
//     TF1 *hagedorn2  = new TF1("hagedorn2", "[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
//     hagedorn2->SetParameter(0,A_Hag);
//     hagedorn2->SetParameter(1,p_0);
//     hagedorn2->SetParameter(2,n-hagedorn->GetParError(2));
//     hagedorn2->SetLineColor(4);
//     hagedorn2->Draw("same");

//     Double_t pT;

//     for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
//       {
// 	cout << "pTbin " << pTbin << " pT " << pTpizero[pTbin] << " Mean is " << 
// 	  xhagedorn->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25)/hagedorn->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25) 
// 	   << " "  <<
// 	xhagedorn1->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25)/hagedorn1->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25) 
// 	   << " "  <<
// 	xhagedorn2->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25)/hagedorn2->Integral(pTpizero[pTbin]-0.25,pTpizero[pTbin]+0.25) <<
// endl;
//     }
      
//   }

// }

//_____________________________________________________________________________
TGraphErrors *plot_ratios_centralities_pp(const Int_t Cent1 = 0, const Int_t Cent2 = 10,
					  const char* cut="noPID",
					  const char* plot="yes")
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  TGraphErrors *NumeratorSpectrum   = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut,"full","no");
  TGraphErrors *DenominatorSpectrum = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt");
  //TGraphErrors *DenominatorSpectrum = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_sasha.txt");
  TH2F *frame = new TH2F("frame","frame",10,0.,8.,10,0.0000001,20.);
  TCanvas *c6 = new TCanvas("pp_yield","pp_yield");
  c6->cd();
  frame->Draw();
  DenominatorSpectrum->SetMinimum(1e-09);
  DenominatorSpectrum->Draw("P");
  //Fiting by a hagedorn
  Double_t ptmin = 6.0;
  Double_t ptmax = 12.0; // <-- modify this if no fit
  Double_t p0 = 1.72;
  Double_t n_exp = 10.5;
  TF1 *hagedorn = new TF1("hagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn->SetParameters(10000., p0, n_exp);
  hagedorn->SetParLimits(0, 1, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  //hagedorn->SetParLimits(1, 1.9, 3.36);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);
  //hagedorn->SetParLimits(2, 9.408,9.408); 
  hagedorn->SetParNames("A Hag","p_o", "n");
  hagedorn->SetLineWidth(1);
  hagedorn->Draw("same");
  DenominatorSpectrum->Fit("hagedorn","QR","");
  //hagedorn->SetLineColor(4);
  //hagedorn->Draw("same");
  c6->SetLogy();
  c6->Update();
  // pp pi0 are in mb/(GeV/c2)^{-2}
  Double_t pp_sigma = 42.;
  c6->Close();

  Double_t pp_luminosity_relat_error = 0.30; // Additional 20% error on pp yield (luminosity)

  Int_t NpTbins = TMath::Min(NumeratorSpectrum->GetN(),DenominatorSpectrum->GetN());//+1;

  Double_t pT[NpTbins];
  Double_t e_pT[NpTbins];
  Double_t Ratio[NpTbins];
  Double_t e_Ratio[NpTbins];
  Double_t eNcoll_Ratio[NpTbins];
  Double_t x,yNum,yDen, e_yNum, e_yDen;

  Int_t CC = getCentralityClass(Cent1, Cent2);

  TGraphErrors *problem = 0;
  if ( CC == -1) return problem;

  Double_t NcollNum  = getNcoll(Cent1,Cent2);
  Double_t eNcollNum = get_relat_errNcoll(Cent1,Cent2);
  Double_t NcollDen = Ncoll_pp; // Npart_pp
  Double_t eNcollDen = 0;

  cout << "CC: " << CC <<" Ncoll is " << NcollNum << " %err: " << eNcollNum << endl;
  cout << "pp ref.: NcolDen is " << NcollDen << " %err: " <<  eNcollDen << endl;
		  
  Int_t ii;
  for(ii=0;ii<NpTbins;ii++) 
    {
      DenominatorSpectrum->GetPoint(ii,x,yDen);
      NumeratorSpectrum->GetPoint(ii,x,yNum);
      
      pT[ii]    = x;
      e_pT[ii]  = NumeratorSpectrum->GetErrorX(ii);

      Ratio[ii] = (yNum/NcollNum)/(yDen/NcollDen)*pp_sigma; // <--- bin-to-bin
      //Ratio[ii] = yNum/NcollNum/hagedorn->Eval(x)*NcollDen*pp_sigma; // Hagedorn fit pp scaled

      // Error calculations due to cancelation effects
      // 1. pT smearing, power-law weights, ...: 

      e_yNum = NumeratorSpectrum->GetErrorY(ii)/yNum;
      e_yDen = DenominatorSpectrum->GetErrorY(ii)/yDen;               
      e_Ratio[ii] = TMath::Sqrt( TMath::Power(e_yNum,2)+ TMath::Power(e_yDen,2) );

//       e_Ratio[ii] = TMath::Sqrt( TMath::Power(NcollDen/NcollNum*e_yNum/hagedorn->Eval(x)*pp_sigma,2)+
// 				 TMath::Power(NcollDen/NcollNum*e_yDen*yNum/hagedorn->Eval(x)/hagedorn->Eval(x)*pp_sigma,2) +
// 				 TMath::Power(eNcollDen*Ratio[ii]/100.,2)                         +
// 				 TMath::Power(eNcollNum*Ratio[ii]/100.,2)                           );

      eNcoll_Ratio[ii] = TMath::Sqrt( TMath::Power(eNcollDen,2) +
				      TMath::Power(eNcollNum,2) + 
				      TMath::Power(pp_luminosity_relat_error,2) );

      // two highest pT points where binning is different:
      if (x > 7.)
	{
	  //cout << "Ratio (bin-to-bin) was: " << Ratio[ii] << " New (hagedorn) ratio is: " ;
	  Ratio[ii] =  yNum/NcollNum/hagedorn->Eval(x)*NcollDen*pp_sigma; // Hagedorn fit pp scaled
	  //cout <<  Ratio[ii] << endl;
	}

      e_Ratio[ii] *= Ratio[ii];
      eNcoll_Ratio[ii] *= Ratio[ii];

      printf("%f  %f  %f  %f \n",pT[ii],Ratio[ii],e_Ratio[ii],eNcoll_Ratio[ii]);

     }

  char title[50]; 
  char name[50];
 
  sprintf(title,"#pi^{0} Yield(%d-%d)/Yield(pp)",Cent1,Cent2);
  sprintf(name,"ratio_%d_%d_to_pp",Cent1,Cent2); 

  TCanvas *c5 = new TCanvas(name,name,600,600);
  TGraphErrors *RatioPi0 = new TGraphErrors( NpTbins, pT, Ratio, e_pT, e_Ratio);
  Double_t max_yield = 2.5 ;//3.*Ratio[0];
  Double_t min_yield = 0. ;//*Ratio[NpTbins-1];
  
  RatioPi0->SetMaximum(max_yield);
  RatioPi0->SetMinimum(min_yield);
  RatioPi0->SetMarkerStyle(20);
  RatioPi0->SetMarkerColor(2);
  RatioPi0->Draw("AP");
  RatioPi0->GetYaxis()->SetTitleOffset(1.0);
  RatioPi0->GetYaxis()->SetTitleSize(0.05);
  RatioPi0->GetYaxis()->SetTitle("R_{AA} nuclear modif. factor (N_{part} scaling)");
  RatioPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
  RatioPi0->GetXaxis()->SetTitleSize(0.05);

  TLegend *le = new TLegend(0.437919,0.63587,0.862416,0.871377,title,"brNDC");
  le->SetFillColor(kWhite);
  le->Draw();

  TLine *line = new TLine(0.27307,1.0,10.5132,1.0);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(4);
  line->Draw("same");

  // Separated Ncoll errors draw sys. errors as boxes
  TBox *b = 0;
  for (int i=0; i<NpTbins; i++) 
    {
      b = new TBox(pT[i]-0.25,Ratio[i]-eNcoll_Ratio[i],
		   pT[i]+0.25,Ratio[i]+eNcoll_Ratio[i]);
      
      b->SetFillColor(kYellow);
      b->Draw();
  }

  RatioPi0->Draw("P");

  c5->Update();

  if (strcmp(plot,"no") == 0) c5->Close();

  return RatioPi0;
  
}


//_____________________________________________________________________________
// DESCRIPTION: Reads the fully corrected data tables for N centrality classes
//              and combines them into a single data table.
//              Plots the resulting spectrum and/or returns the TGraph
//              N = (Cent2 - Cent1)/10  
//              e.g. Cent1=0 (0-10%) and Cent2=20 (10-20%) will give: 0-20%

TGraphErrors *combine_pi0_spectra_run2( const Int_t Cent1_min = 0, const Int_t Cent2_max = 20, const char *plot="yes")
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  cout << " Calculating fully-corrected pi0 spectrum for the centrality class: "
       << " (" << Cent1_min << " - " << Cent2_max << "%)" << endl;

  Int_t offset = 10;
  Int_t num_of_cent_class = (Cent2_max - Cent1_min)/offset;
  if (num_of_cent_class == 1) 
    {
      cout << "Spectrum for centrality class " << Cent1_min << " - " << Cent2_max 
	   << "%) already exists in principle. Let's try combining 5% tables ..." << endl;
      num_of_cent_class = 2;
      offset = 5; 
    }

  char title[300];
  sprintf(title,"Centrality Class %d_%d",Cent1_min,Cent2_max);
  char name0[300];
  sprintf(name0,"pi0_combined_spectra_%d_%d",Cent1_min,Cent2_max);

  char ffile[300];
  char name[300];

  // Input data file format is: 
  // pT(GeV/c)  pTerror(GeV/c)  Npi0  Npi0_tot_error (relat_error) stat_error CentClass_indep_error pT_corelated_error

  Double_t pTpizero[MaxPTbins];
  Double_t epTpizero[MaxPTbins];
  Double_t fully_corr_yield[MaxPTbins][CentClasses];
  Double_t efully_corr_yield[MaxPTbins][CentClasses];
  Double_t efully_corr_yield_relat[MaxPTbins][CentClasses];
  Double_t abs_error_stat[MaxPTbins][CentClasses];
  Double_t abs_error_cent_indep[MaxPTbins][CentClasses];
  Double_t abs_error_pt_correl[MaxPTbins][CentClasses];

  Double_t eventsCC[CentClasses];
  Double_t eventsCC_error[CentClasses];
  Double_t accum_events = 0;
  Double_t accum_events_error = 0;

  Int_t NpTbins_min = 100; // initialized arbitrarily large
  Int_t CentClass = Cent1_min;

  for (Int_t i=0; i<num_of_cent_class; i++)
    {
      if (CentClass==80) sprintf(name,"pi0_pbsc_%d_%d",CentClass,CentClass+12);
      else sprintf(name,"pi0_pbsc_%d_%d",CentClass,CentClass+offset);      
      sprintf(ffile,"%s%s.txt",data_tables_location,name);
      ifstream ifiledata(ffile);
      if (!ifiledata)
	{
	  cout << " Can't open file: " << ffile << endl;
	  cout << " Are you sure that the centrality class: " << CentClass << " - " << CentClass+offset << "% exists ?" << endl;
	  return 0;
	}
      else cout << " Reading " << ffile  << " ..." << endl;

      // Let's skip the first 3 lines
      for (Int_t l=0;l<4;l++) ifiledata.ignore(1000,'\n'); // Skip 1000 characters or until end-of-line 

      Int_t ii = 0;
      // Read pT, countsPi0 from the file
      while ( ifiledata >> 
	      pTpizero[ii] >> 
	      epTpizero[ii] >> 
	      fully_corr_yield[ii][i] >> 
	      efully_corr_yield[ii][i] >>
	      efully_corr_yield_relat[ii][i] >>
	      abs_error_stat[ii][i] >>
	      abs_error_cent_indep[ii][i] >>
	      abs_error_pt_correl[ii][i] 
	      ){ 
	cout << "  " << pTpizero[ii] //<< endl;
	     << "  " << fully_corr_yield[ii][i] << "  error: " << efully_corr_yield[ii][i] 
	     << "  " << abs_error_cent_indep[ii][i] << endl;
	ii++;
      }
      ifiledata.close();

      NpTbins_min = (NpTbins_min<ii) ? NpTbins_min : ii; // store mininum value of max. # of pT bins per CentClass
      eventsCC[i] = getEvents(CentClass,CentClass+offset);
      eventsCC_error[i] = eventsCC[i]*getEvents_relat_error(CentClass,CentClass+offset);
      accum_events+=eventsCC[i];
      accum_events_error+=TMath::Power(eventsCC_error[i],2);

      CentClass+=offset;
    }

  accum_events_error = TMath::Sqrt(accum_events_error);

  const Int_t NpTbins = NpTbins_min;

  Double_t combin_fully_corr_yield[NpTbins];
  Double_t ecombin_fully_corr_yield[NpTbins];
  Double_t combin_abs_error_stat[NpTbins];
  Double_t combin_abs_error_cent_indep[NpTbins];
  Double_t combin_abs_error_pt_correl[NpTbins];

  cout << " Output format is: pT(GeV/c)  pTerror(GeV/c)  Npi0  Npi0_tot_error (Relat_error%) ";
  cout << "CentClass_indep_error pT_correlated_error" << endl;

  for (Int_t pTbin = 0; pTbin < NpTbins ; pTbin++) 
    { 
      combin_fully_corr_yield[pTbin] = 0;
      ecombin_fully_corr_yield[pTbin] = 0;
      combin_abs_error_stat[pTbin] = 0;
      combin_abs_error_cent_indep[pTbin] = 0;
      combin_abs_error_pt_correl[pTbin] = 0;

      // Simple weighted average: 1/Nevt*[Nevt_1*Npi0_1+ ...+ Nevt_n*Npi0_n]
      for (Int_t i=0; i<num_of_cent_class ; i++)
	{
	  combin_fully_corr_yield[pTbin]+=fully_corr_yield[pTbin][i]*eventsCC[i];
	  // errors are basically 1/Nevt*[Nevt_1*errNpi0_1+ ...+ errNevt_n*Npi0_n] + other minor stat errors in Nevt's
	  ecombin_fully_corr_yield[pTbin]+= ( TMath::Power(eventsCC[i]*efully_corr_yield[pTbin][i],2) +
					      TMath::Power(fully_corr_yield[pTbin][i]*eventsCC_error[i],2) ); // pretty small 
 	  combin_abs_error_cent_indep[pTbin]+= ( TMath::Power(eventsCC[i]*abs_error_cent_indep[pTbin][i],2) +
						 TMath::Power(fully_corr_yield[pTbin][i]*eventsCC_error[i],2));
 	  combin_abs_error_pt_correl[pTbin]+= (TMath::Power(eventsCC[i]*abs_error_pt_correl[pTbin][i],2) +
					       TMath::Power(fully_corr_yield[pTbin][i]*eventsCC_error[i],2));
	}
      combin_fully_corr_yield[pTbin]/= accum_events;
      ecombin_fully_corr_yield[pTbin]+= TMath::Power(combin_fully_corr_yield[pTbin]*accum_events_error,2); // pretty small 
      ecombin_fully_corr_yield[pTbin] = TMath::Sqrt(ecombin_fully_corr_yield[pTbin]);
      ecombin_fully_corr_yield[pTbin]/= accum_events;
      float erelat = ecombin_fully_corr_yield[pTbin]/combin_fully_corr_yield[pTbin];

      combin_abs_error_cent_indep[pTbin] = TMath::Sqrt(combin_abs_error_cent_indep[pTbin]);
      combin_abs_error_cent_indep[pTbin]/= accum_events;
      combin_abs_error_pt_correl[pTbin] = TMath::Sqrt(combin_abs_error_pt_correl[pTbin]);
      combin_abs_error_pt_correl[pTbin]/= accum_events;

      cout << "  " << setprecision(4) << pTpizero[pTbin] << " " << epTpizero[pTbin] ;
      cout << "  " << setprecision(4) << combin_fully_corr_yield[pTbin] << " " << ecombin_fully_corr_yield[pTbin];
      cout << "  " << setprecision(4) << 100.*erelat;
      cout << "  " << setprecision(4) << combin_abs_error_cent_indep[pTbin];
      cout << "  " << setprecision(4) << combin_abs_error_pt_correl[pTbin];
      cout << endl;

    }
  //_____________________________________________________________________________

  TCanvas *c4 = 0; 
  TGraphErrors *FullCorrPi0 = 0;

  c4 = new TCanvas(name0,name0,600,600);
  FullCorrPi0 = new TGraphErrors( NpTbins, pTpizero, combin_fully_corr_yield, epTpizero, ecombin_fully_corr_yield);
  Double_t max_yield = 10.*combin_fully_corr_yield[0];
  Double_t min_yield = 0.1*combin_fully_corr_yield[NpTbins-1];
  
  FullCorrPi0->SetMaximum(max_yield);
  FullCorrPi0->SetMinimum(min_yield);
  FullCorrPi0->SetMarkerStyle(20);
  FullCorrPi0->SetMarkerColor(2);
  FullCorrPi0->Draw("AP");
  FullCorrPi0->SetTitle(title);
  FullCorrPi0->GetYaxis()->SetTitleOffset(1.2);
  FullCorrPi0->GetYaxis()->SetTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  FullCorrPi0->GetXaxis()->SetTitle("#pi^{0} p_{T} (GeV/c)");
  c4->SetLogy();
  c4->Update();

 //_____________________________________________________________________________
  
  Double_t ptmin = 2.0;
  Double_t ptmax = pTpizero[NpTbins]-1.; // <-- modify this if no fit
  Double_t p0 = 1.72;
  Double_t n_exp = 10.5;
  TF1 *hagedorn = new TF1("hagedorn","[0]*([1]/(x+[1])^[2])", ptmin, ptmax);
  hagedorn->SetParameters(10000., p0, n_exp);
  hagedorn->SetParLimits(0, 1, 20000000);
  hagedorn->SetParLimits(1, p0, p0);
  //hagedorn->SetParLimits(1, 1.9, 3.36);
  hagedorn->SetParLimits(2, n_exp-4, n_exp+4);
  //hagedorn->SetParLimits(2, 9.408,9.408); 
  hagedorn->SetParNames("A Hag","p_o", "n");
  hagedorn->SetLineWidth(1);
  hagedorn->Draw("same");

  // Fitting to Hagedorn
  c4->Update();
  FullCorrPi0->Fit("hagedorn","QR","");
  
//   cout << "Fit result: " << endl;
//   AHag[CC] = hagedorn->GetParameter(0) ;   cout << "AHag =" << AHag[CC] << endl ;
//   p0       = hagedorn->GetParameter(1) ;   cout << "p0   =" << p0 << endl;
//   nHag[CC] = hagedorn->GetParameter(2) ;   cout << "n    =" << nHag[CC] << endl;
//   cout << "Chi2/Ndf  =" << hagedorn->GetChisquare()/hagedorn->GetNumberFitPoints() << endl;
//   hagedorn->Draw("same");

  if (strcmp(plot,"no") == 0) c4->Close();
  return FullCorrPi0;

}

//_____________________________________________________________________________

void xT_scaling_pp()
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t ptmin = 0.5;
  Double_t ptmax = 8.5;
  Int_t ptbins = (Int_t)(ptmax-ptmin)*2;
  Double_t sqrt_s = 130.;
  Double_t pt2xt = 2./sqrt_s;

  char title[50];
  sprintf(title,"pp_pi0_spectra_130_200");
  TCanvas *c10 = new TCanvas(title,title,600,600);
  TH2F *frame = new TH2F(title,title,ptbins,ptmin,ptmax,100,5E-08,15);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("1/[2#pi p_{T}] d^{2}#sigma^{#pi^{0}}/dp_{T}d#eta (mb/(GeV^{2})");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.5);
  frame->GetXaxis()->SetTitleOffset(1.2);

  frame->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  frame->GetXaxis()->SetLimits(pt2xt*ptmin,pt2xt*ptmax);
  frame->Draw();
  c10->SetLogy();
  c10->SetLogx();
  c10->Update();

  TH1F *pp_130 = 0;
  TGraphErrors *pp_200 = 0;

 //Double_t pp2AuAu_Ncoll = 974.;
  // pp pi0 are in mb/(GeV/c2)^{-2}
  //Double_t pp_sigma = 42.;
  Double_t sqrt_s_scaling_130 = 0.63*TMath::Power(130./200.,6.3); // 0.63 is h/pi ratio !
  Double_t sqrt_s_scaling_200 = 1.; //TMath::Power(200.,-6.5);

  pp_130 = (TH1F*)plot_interpolated_pp_spectrum_130("xT",sqrt_s_scaling_130);//pp2AuAu_Ncoll);
  pp_130->SetMarkerStyle(8);
  pp_130->SetMarkerSize(1.2);
  pp_130->SetMarkerColor(3);
  //pp_130->SetLineSize(2);
  //pp_130->SetLineColor(3);
  c10->cd();
  pp_130->Draw("same");
  c10->Update();

  c10->cd();
  pp_200 = (TGraphErrors*)plot_xx_pi0_spectra("yields_pp_hisa_tbl_c22mbc_tot.txt","xT",sqrt_s_scaling_200); // pp @ 200 GeV
  pp_200->SetMarkerStyle(20);
  pp_200->SetMarkerSize(1.2);
  pp_200->SetMarkerColor(2);
  c10->cd();
  pp_200->Draw("P");
  c10->Update();

  TLatex *tex = new TLatex(-2.08281,0.57812,"Blue curve: 130 GeV interpolation scaled by (130/200)^{6.3}");
  tex->SetTextColor(9);
  tex->SetTextSize(0.0416667);
  tex->SetLineWidth(2);
  tex->Draw();
  tex = new TLatex(-2.0765,0.138435,"Red points: pp #rightarrow #pi^{0} data @ 200 GeV");
  tex->SetTextColor(2);
  tex->SetTextSize(0.0416667);
  tex->SetLineWidth(2);
  tex->Draw();
  c10->Update();

  // Now the ratio
  Int_t Npoints = pp_200->GetN();
  Double_t xT[Npoints];
  Double_t exT[Npoints];
  Double_t yieldpp_200[Npoints];
  Double_t ratio_pp_130_200[Npoints];
  Double_t eratio_pp_130_200[Npoints] ;
  Double_t yieldpp_130 = 0;
  Double_t eyieldpp_200 = 0;
  Double_t e_interpolation = 0;
  Double_t e_luminosity = 0.2;

  Int_t ii;
  for( ii=0; ii<Npoints; ii++) 
    {
      pp_200->GetPoint(ii,xT[ii],yieldpp_200[ii]);
      yieldpp_130 = pp_130->GetBinContent(pp_130->GetXaxis()->FindBin(xT[ii]));

      ratio_pp_130_200[ii] = yieldpp_200[ii]/yieldpp_130;
      eyieldpp_200 = pp_200->GetErrorY(ii)/yieldpp_200[ii];

      e_interpolation = 0.16+2.6*xT[ii]; // 20% at 1 GeV/c --> 35% at 5 GeV/c
      //cout << "%error in pp_130 yield: " << eyieldpp_200 << endl;
      eratio_pp_130_200[ii] = TMath::Sqrt(TMath::Power(e_interpolation,2)+ TMath::Power(e_luminosity,2)+ TMath::Power(eyieldpp_200,2));
      exT[ii] = pp_200->GetErrorX(ii);
      eratio_pp_130_200[ii] *= ratio_pp_130_200[ii];
      cout << "xT= " << xT[ii] << " pp_130/pp_200 ratio = " << ratio_pp_130_200[ii];
      cout << " err: " << eratio_pp_130_200[ii] << endl;
    }

  TGraphErrors *Ratio_pp_200 = new TGraphErrors(Npoints,xT,ratio_pp_130_200,exT,eratio_pp_130_200);
  Ratio_pp_200->SetMarkerStyle(20);
  Ratio_pp_200->SetMarkerSize(1.2);
  Ratio_pp_200->SetMarkerColor(4);
  //Ratio_pp_200->Print();
  TCanvas *c23 = new TCanvas("ratio_pp_130_200","ratio_pp_130_200",600,600);
  c23->cd();

  TH2F *frame2 = 0;
  frame2 = (TH2F*)frame->Clone();
  frame2->Draw();
  frame2->SetYTitle("(200./130.)^{6.3} #sigma^{200}(data)/#sigma^{130}(interpolation) (pp #rightarrow #pi^{0}) ");
  TLine *line = new TLine(0.01,1.0,0.1,1.0);
  line->Draw();
  Ratio_pp_200->Draw("P");


//   TH1F *ratio = 0;
//   ratio = (TH1F*)pp_130->Clone();   
//   ratio->SetTitle("ratio_xT_200_130");
//   ratio->Divide(h_xT_130_periph);
//   ratio->Draw("e");
//   c22->Update();

}
//_____________________________________________________________________________

void afew_in_one()
{
  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t ptmin = 0.5;
  Double_t ptmax = 8.5;
  Int_t ptbins = (Int_t)(ptmax-ptmin)*2;
  //Double_t sqrt_s = 130.;
  //Double_t pt2xt = 2./sqrt_s;

  char title[50];
  sprintf(title,"pi0_spectra");
  TCanvas *c10 = new TCanvas(title,title,600,600);
  TH2F *frame = new TH2F(title,title,ptbins,ptmin,ptmax,100,5E-08,15);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.5);
  frame->GetXaxis()->SetTitleOffset(1.2);

  //frame->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  //frame->GetXaxis()->SetLimits(pt2xt*ptmin,pt2xt*ptmax);
  frame->Draw();
  c10->SetLogy();
  //c10->SetLogx();
  c10->Update();

  TGraphErrors *one = 0;
  TGraphErrors *two = 0;
  TGraphErrors *three = 0;
  TGraphErrors *four = 0;

  one = (TGraphErrors*)plot_pi0_spectra_run2();
  //one = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","raw","no");
  one->SetMarkerStyle(20);
  one->SetMarkerSize(1.2);
  one->SetMarkerColor(1);
  c10->cd();
  one->Draw("P");
  c10->cd();
  two = (TGraphErrors*)plot_xx_pi0_spectra("pi0_pbsc_0_10_QM02.txt");
  //two = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","acc","no");
  //two = (TGraphErrors*)combine_pi0_spectra_run2(60,80,"no");
  two->SetMarkerStyle(20);
  two->SetMarkerSize(1.2);
  two->SetMarkerColor(4);
  c10->cd();
  two->Draw("P");
//   three = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","acceff","no");
//   three->SetMarkerStyle(20);
//   three->SetMarkerSize(1.2);
//   three->SetMarkerColor(8);
//   c10->cd();
//   three->Draw("P");
//   c10->cd();
//   four = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","full","no");
//   four->SetMarkerStyle(20);
//   four->SetMarkerSize(1.2);
//   four->SetMarkerColor(2);
//   c10->cd();
//   four->Draw("P");
//   c10->cd();

  c10->Update();

}

//_____________________________________________________________________________

TGraphErrors *ratio_tgraphs( TGraphErrors *tgraph1, TGraphErrors *tgraph2)
{

  Int_t Npoints1 = tgraph1->GetN();
  Int_t Npoints2 = tgraph2->GetN();
  Int_t Npoints = TMath::Min(Npoints1,Npoints2);//+1;

  Double_t pT[Npoints];
  Double_t epT[Npoints];

  Double_t yield_tgraph1[Npoints];
  Double_t yield_tgraph2[Npoints];

  Double_t ratio_graphs[Npoints];
  Double_t eratio_graphs[Npoints] ;

  Int_t ii;
  for( ii=0; ii<Npoints; ii++) 
    {
      tgraph1->GetPoint(ii,pT[ii],yield_tgraph1[ii]);
      tgraph2->GetPoint(ii,pT[ii],yield_tgraph2[ii]);
      ratio_graphs[ii] = yield_tgraph1[ii]/yield_tgraph2[ii];

      double e_tgraph1 = tgraph1->GetErrorY(ii)/yield_tgraph1[ii];
      double e_tgraph2 = tgraph2->GetErrorY(ii)/yield_tgraph2[ii];

      eratio_graphs[ii] = TMath::Sqrt(TMath::Power(e_tgraph1,2)+TMath::Power(e_tgraph2,2));
      eratio_graphs[ii] *= ratio_graphs[ii];

      epT[ii] = tgraph1->GetErrorX(ii); // single error from first TGraph for 1st axis !

      cout << "pT= " << pT[ii] << " tgraph2/tgraph1 ratio = " << ratio_graphs[ii];
      cout << " err: " << eratio_graphs[ii] << endl;
    }

  TGraphErrors *ratio_tgraph1_tgraph2 = new TGraphErrors(Npoints,pT,ratio_graphs,epT,eratio_graphs);
  ratio_tgraph1_tgraph2->SetMarkerStyle(20);
  ratio_tgraph1_tgraph2->SetMarkerSize(1.2);
  ratio_tgraph1_tgraph2->SetMarkerColor(4);
  //ratio_tgraph1_tgraph2->Print();

  return ratio_tgraph1_tgraph2;

}

//_____________________________________________________________________________

TGraphErrors *cut_comparison(const Int_t Cent1 = 0 , const Int_t Cent2 = 10, 
			     const char* cut1 = "noPID", 
			     const char* cut2 = "tof1")
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t ptmin = 0.5;
  Double_t ptmax = 10.5;
  Int_t ptbins = (Int_t)(ptmax-ptmin)*2;

  char title[100];
  sprintf(title,"cut_comparison_%s_%s_%d_%d",cut1,cut2,Cent1,Cent2);

  TCanvas *c10 = new TCanvas(title,title,600,600);
  TH2F *frame = new TH2F(title,title,ptbins,ptmin,ptmax,100,5E-08,15);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.5);
  frame->GetXaxis()->SetTitleOffset(1.2);

  frame->Draw();
  c10->SetLogy();
  c10->Update();

  TGraphErrors *gcut1 = 0;
  TGraphErrors *gcut2 = 0;

  // cut 1
  gcut1 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut1,"full","no");
  gcut1->SetMarkerStyle(20);
  gcut1->SetMarkerSize(1.2);
  gcut1->SetMarkerColor(1);
  c10->cd();
  gcut1->Draw("P");

  // cut 2
  gcut2 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut2,"full","no");
  gcut2->SetMarkerStyle(20);
  gcut2->SetMarkerSize(1.2);
  gcut2->SetMarkerColor(4);
  c10->cd();
  gcut2->Draw("P");

  // ratio
  TLegend *legend = new TLegend(0.295302,0.778986,0.899329,0.947464,"Ratio of cuts:","brNDC");
  legend->SetTextSize(0.042);
  legend->SetLineColor(1);
  legend->SetLineStyle(1);
  legend->SetLineWidth(1);
  legend->SetFillColor(19);
  legend->SetFillStyle(0);
  legend->SetBorderSize(0);

  TGraphErrors *ratio_cut1_cut2 = 0;
  ratio_cut1_cut2 = (TGraphErrors*)ratio_tgraphs(gcut1, gcut2);
  char title2[50];
  sprintf(title2,"%s/%s (%d-%d%%)",cut1,cut2,Cent1,Cent2);
  legend->AddEntry(ratio_cut1_cut2,title2,"P");    
  c10->Update();

  TCanvas *c23 = new TCanvas(title2,title2,600,600);
  c23->Range(-0.40566,-0.225873,8.59057,2.04107);
  c23->SetFillColor(0);
  c23->SetBorderMode(0);
  c23->SetBorderSize(2);
  c23->SetRightMargin(0.0100671);
  c23->SetTopMargin(0.0181159);
  c23->SetFrameBorderMode(0);
  c23->SetFrameBorderMode(0);
  c23->cd();
  
  TH2F *frame2 = new TH2F(title2,title2,ptbins,ptmin,ptmax,20,0.,2.0);
  frame2->SetStats(0);
  frame2->SetTitle(title);
  frame2->SetYTitle("Ratio of spectra (diff. cuts)");
  frame2->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame2->GetYaxis()->SetTitleOffset(1.3);
  frame2->GetXaxis()->SetTitleOffset(1.2);
  frame2->Draw();

  TLine *line = new TLine(0.515094,1.,8.51509,1.);
  line->SetLineColor(50);
  line->SetLineStyle(2);
  line->Draw();

  ratio_cut1_cut2->SetMarkerStyle(29);
  ratio_cut1_cut2->SetMarkerSize(1.7);
  ratio_cut1_cut2->SetMarkerColor(4);
  ratio_cut1_cut2->SetLineColor(4);
  ratio_cut1_cut2->Draw("P");

  legend->Draw();

  c23->Update();

  return  ratio_cut1_cut2;
} 

//_____________________________________________________________________________
void all_cuts_comparison(const Int_t Cent1 = 0 , const Int_t Cent2 = 10,
			 double ptmax = 10.5)
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t ptmin = 0.5;

  Int_t ptbins = (Int_t)(ptmax-ptmin)*2;

  char title0[50];
  sprintf(title0,"cuts_comparison_%d_%d",Cent1,Cent2);
  TCanvas *c10 = new TCanvas(title0,title0,600,600);
  char title[50];
  sprintf(title,"ratio #pi^{0} yields diff. cuts (%d-%d%%)",Cent1,Cent2);
  TH2F *frame = new TH2F(title,title,ptbins,ptmin,ptmax,20,0.,2.);
  frame->SetStats(0);
  frame->SetTitle(title);
  frame->SetYTitle(title);
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.5);
  frame->GetXaxis()->SetTitleOffset(1.2);
  frame->Draw();
  TLine *line = new TLine(ptmin,1.,ptmax,1.);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

  TGraphErrors *one = 0;
  TGraphErrors *two = 0;
  TGraphErrors *three = 0;
  TGraphErrors *four = 0;
  TGraphErrors *five = 0;

  one = (TGraphErrors *)cut_comparison(Cent1,Cent2,"noPID","tof1chisq2");
  two = (TGraphErrors *)cut_comparison(Cent1,Cent2,"chisq1","tof1chisq2");
  three = (TGraphErrors *)cut_comparison(Cent1,Cent2,"chisq2","tof1chisq2");
  four = (TGraphErrors *)cut_comparison(Cent1,Cent2,"tof1","tof1chisq2");
  five = (TGraphErrors *)cut_comparison(Cent1,Cent2,"tof1chisq1","tof1chisq2");

  c10->cd();
  one->SetLineColor(1);
  one->SetLineStyle(20);
  one->Draw("");
  two->SetLineColor(2);
  two->SetLineStyle(22);
  two->Draw("");
  three->SetLineColor(4);
  three->SetLineStyle(29);
  three->Draw("");
  four->SetLineColor(3);
  four->SetLineStyle(23);
  four->Draw("");
  five->SetLineColor(6);
  five->SetLineStyle(8);
  five->Draw("");

  c10->Update();

}
//_____________________________________________________________________________

void ratio_qm_new(int Cent1=0,int Cent2=10,  const char* cut1 = "noPID", double ptmax = 10.5)
{

  gROOT->SetStyle("Plain");
  gStyle->SetOptTitle(0);
  gStyle->SetPalette(1);

  Double_t ptmin = 0.5;

  Int_t ptbins = (Int_t)(ptmax-ptmin)*2;

  char title0[50];
  sprintf(title0,"ratio_new_QM_%d_%d",Cent1,Cent2);
  TCanvas *c10 = new TCanvas(title0,title0,600,600);
  char title[50];
  sprintf(title,"ratio #pi^{0} new/QM (%d-%d%%)",Cent1,Cent2);
  TH2F *frame = new TH2F(title,title,ptbins,ptmin,ptmax,20,0.,2.);
  frame->SetStats(0);
  frame->SetTitle(title);
  //frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
  frame->SetYTitle(title);
  frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
  frame->GetYaxis()->SetTitleOffset(1.5);
  frame->GetXaxis()->SetTitleOffset(1.2);

  //frame->SetXTitle("x_{T}=2p_{T}/#sqrt{s_{NN}}");
  //frame->GetXaxis()->SetLimits(pt2xt*ptmin,pt2xt*ptmax);
  frame->Draw();
  TLine *line = new TLine(ptmin,1.,ptmax,1.);
  line->SetLineColor(2);
  line->SetLineStyle(2);
  line->SetLineWidth(3);
  line->Draw();

  TGraphErrors *one = 0;
  TGraphErrors *two = 0;
  TGraphErrors *ratio = 0;

  one = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof1chisq2","full","yes");
  //one = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","raw","no");
  //one->SetMarkerStyle(20);
  //one->SetMarkerSize(1.2);
  //one->SetMarkerColor(1);

  char file2[200];
  sprintf(file2,"pi0_pbsc_%d_%d_QM02.txt",Cent1,Cent2);

  two = (TGraphErrors*)plot_xx_pi0_spectra(file2);
  //two = (TGraphErrors*)plot_pi0_spectra_run2(0,10,"noPID","acc","no");
  //two = (TGraphErrors*)combine_pi0_spectra_run2(60,80,"no");
  //two->SetMarkerStyle(20);
  //two->SetMarkerSize(1.2);
  //two->SetMarkerColor(4);

  ratio = (TGraphErrors*)ratio_tgraphs(one,two);
  c10->cd();
  ratio->Draw("*");

  c10->Update();

}


// //_____________________________________________________________________________

// void cut_comparison(const Int_t Cent1 = 0 , const Int_t Cent2 = 10, 
// 		    const char* cut1 = "noPID", 
// 		    const char* cut2="tof")
// {

//   gROOT->SetStyle("Plain");
//   gStyle->SetOptTitle(0);
//   gStyle->SetPalette(1);

//   Double_t ptmin = 0.5;
//   Double_t ptmax = 8.5;
//   Int_t ptbins = (Int_t)(ptmax-ptmin)*2;

//   char title[100];
//   sprintf(title,"cut_comparison_%s_%s_%d_%d",cut1,cut2,Cent1,Cent2);

//   TCanvas *c10 = new TCanvas(title,title,600,600);
//   TH2F *frame = new TH2F(title,title,ptbins,ptmin,ptmax,100,5E-08,15);
//   frame->SetStats(0);
//   frame->SetTitle(title);
//   frame->SetYTitle("1/[2#pi p_{T}N_{evt} ] d^{2}N^{#pi^{0}}/dp_{T}d#eta (GeV/c)^{-2}");
//   frame->SetXTitle("#pi^{0} p_{T} (GeV/c)");
//   frame->GetYaxis()->SetTitleOffset(1.5);
//   frame->GetXaxis()->SetTitleOffset(1.2);

//   frame->Draw();
//   c10->SetLogy();
//   c10->Update();

//   TGraphErrors *gcut1 = 0;
//   TGraphErrors *gchisq1 = 0;
//   TGraphErrors *gchisq2 = 0;
//   TGraphErrors *gtof1 = 0;
//   TGraphErrors *gtof2 = 0;
//   TGraphErrors *gtof1chisq1 = 0;
//   TGraphErrors *gtof1chisq2 = 0;
//   TGraphErrors *gtof2chisq1 = 0;
//   TGraphErrors *gtof2chisq2 = 0;

//   gcut1 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,cut1,"full","no");
//   gcut1->SetMarkerStyle(20);
//   gcut1->SetMarkerSize(1.2);
//   gcut1->SetMarkerColor(1);
//   c10->cd();
//   gcut1->Draw("P");

//   TLegend *legend = new TLegend(0.295302,0.778986,0.899329,0.947464,"Ratio of cuts:","brNDC");
//   legend->SetTextSize(0.042);
//   legend->SetLineColor(1);
//   legend->SetLineStyle(1);
//   legend->SetLineWidth(1);
//   legend->SetFillColor(19);
//   legend->SetFillStyle(0);
//   legend->SetBorderSize(0);

//   TGraphErrors *ratio_cut1_cut1 = 0;
//   TGraphErrors *ratio_cut1_cut2 = 0;

//   if (strcmp(cut2,"chisq") == 0)
//     {
//       c10->cd();
//       gchisq1 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"chisq1","full","no");
//       gchisq1->SetMarkerStyle(20);
//       gchisq1->SetMarkerSize(1.2);
//       gchisq1->SetMarkerColor(4);
//       c10->cd();
//       gchisq1->Draw("P");
      
//       c10->cd();
//       gchisq2 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"chisq2","full","no");
//       gchisq2->SetMarkerStyle(20);
//       gchisq2->SetMarkerSize(1.2);
//       gchisq2->SetMarkerColor(8);
//       c10->cd();
//       gchisq2->Draw("P");

//       ratio_cut1_cut1 = (TGraphErrors*)ratio_tgraphs(cut1, chisq1);
//       ratio_cut1_cut2 = (TGraphErrors*)ratio_tgraphs(cut1, chisq2);

//       legend->AddEntry(ratio_cut1_cut1,"chisq1/noPID", "P");
//       legend->AddEntry(ratio_cut1_cut2,"chisq2/noPID", "P");
//     }  
//   else if (strcmp(cut2,"tof") == 0)
//     {
//       c10->cd();
//       gtof1 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof1","full","no");
//       gtof1->SetMarkerStyle(20);
//       gtof1->SetMarkerSize(1.2);
//       gtof1->SetMarkerColor(2);
//       c10->cd();
//       gtof1->Draw("P");
      
//       c10->cd();
//       gtof2 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof2","full","no");
//       gtof2->SetMarkerStyle(20);
//       gtof2->SetMarkerSize(1.2);
//       gtof2->SetMarkerColor(2);
//       c10->cd();
//       gtof2->Draw("P");

//       ratio_cut1_cut1 = (TGraphErrors*)ratio_tgraphs(cut1, tof1);
//       ratio_cut1_cut2 = (TGraphErrors*)ratio_tgraphs(cut1, tof2);

//       legend->AddEntry(ratio_cut1_cut1,"tof1/noPID", "P");
//       legend->AddEntry(ratio_cut1_cut2,"tof2/noPID", "P");
//     }
//   else if (strcmp(cut2,"tofchisq1") == 0)
//     {
//       c10->cd();
//       gtof1chisq1 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof1chisq1","full","no");
//       gtof1chisq1->SetMarkerStyle(20);
//       gtof1chisq1->SetMarkerSize(1.2);
//       gtof1chisq1->SetMarkerColor(2);
//       c10->cd();
//       gtof1chisq1->Draw("P");

//       c10->cd();
//       gtof2chisq1 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof2chisq1","full","no");
//       gtof2chisq1->SetMarkerStyle(20);
//       gtof2chisq1->SetMarkerSize(1.2);
//       gtof2chisq1->SetMarkerColor(2);
//       c10->cd();
//       gtof2chisq1->Draw("P");

//       ratio_cut1_cut1 = (TGraphErrors*)ratio_tgraphs(cut1, tof1chisq1);
//       ratio_cut1_cut2 = (TGraphErrors*)ratio_tgraphs(cut1, tof2chisq1);

//       //legend->AddEntry(ratio_cut1_cut1,"tof1chisq1/noPID", "P");
//       //legend->AddEntry(ratio_cut1_cut2,"tof2chisq1/noPID", "P");
//       legend->AddEntry(ratio_cut1_cut1,"tof1chisq1/chisq1", "P");
//       legend->AddEntry(ratio_cut1_cut2,"tof2chisq1/chisq1", "P");
//     }
//   else if (strcmp(cut2,"tofchisq2") == 0)
//     {
//       c10->cd();
//       tof1chisq2 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof1chisq2","full","no");
//       tof1chisq2->SetMarkerStyle(20);
//       tof1chisq2->SetMarkerSize(1.2);
//       tof1chisq2->SetMarkerColor(2);
//       c10->cd();
//       tof1chisq2->Draw("P");

//       c10->cd();
//       tof2chisq2 = (TGraphErrors*)plot_pi0_spectra_run2(Cent1,Cent2,"tof2chisq2","full","no");
//       tof2chisq2->SetMarkerStyle(20);
//       tof2chisq2->SetMarkerSize(1.2);
//       tof2chisq2->SetMarkerColor(2);
//       c10->cd();
//       tof2chisq2->Draw("P");

//       ratio_cut1_cut1 = (TGraphErrors*)ratio_tgraphs(cut1, tof1chisq2);
//       ratio_cut1_cut2 = (TGraphErrors*)ratio_tgraphs(cut1, tof2chisq2);

//       //legend->AddEntry(ratio_cut1_cut1,"tof1chisq2/noPID", "P");
//       //legend->AddEntry(ratio_cut1_cut2,"tof2chisq2/noPID", "P");
//       legend->AddEntry(ratio_cut1_cut1,"tof1chisq2/chisq2", "P");
//       legend->AddEntry(ratio_cut1_cut2,"tof2chisq2/chisq2", "P");
//     }
    
//   c10->Update();

//   TCanvas *c23 = new TCanvas("ratio_cuts","ratio_cuts",600,600);
//   c23->Range(-0.40566,-0.225873,8.59057,2.04107);
//   c23->SetFillColor(0);
//   c23->SetBorderMode(0);
//   c23->SetBorderSize(2);
//   c23->SetRightMargin(0.0100671);
//   c23->SetTopMargin(0.0181159);
//   c23->SetFrameBorderMode(0);
//   c23->SetFrameBorderMode(0);
//   c23->cd();
  
//   char title2[50];
//   sprintf(title2,"ratio_of_pi0_spectra_cuts");
//   TH2F *frame2 = new TH2F(title2,title2,ptbins,ptmin,ptmax,20,0.,2.0);
//   frame2->SetStats(0);
//   frame2->SetTitle(title);
//   frame2->SetYTitle("Ratio of spectra (diff. cuts)");
//   frame2->SetXTitle("#pi^{0} p_{T} (GeV/c)");
//   frame2->GetYaxis()->SetTitleOffset(1.3);
//   frame2->GetXaxis()->SetTitleOffset(1.2);
//   frame2->Draw();

//   TLine *line = new TLine(0.515094,1.,8.51509,1.);
//   line->SetLineColor(50);
//   line->SetLineStyle(2);
//   line->Draw();

//   ratio_cut1_cut1->SetMarkerStyle(20);
//   ratio_cut1_cut1->SetMarkerSize(1.2);
//   ratio_cut1_cut1->SetMarkerColor(2);
//   ratio_cut1_cut1->SetLineColor(2);
//   ratio_cut1_cut1->Draw("P");

//   ratio_cut1_cut2->SetMarkerStyle(29);
//   ratio_cut1_cut2->SetMarkerSize(1.7);
//   ratio_cut1_cut2->SetMarkerColor(4);
//   ratio_cut1_cut2->SetLineColor(4);
//   ratio_cut1_cut2->Draw("P");

//   legend->Draw();

//   c23->Update();

// }
