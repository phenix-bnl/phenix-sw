#include "corrections.h"
#include <cmath>
#include <cstring>
#include <iostream>

//----------------- Asym1 ToF1

static double P0_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut[] = { 
          0.274508,
          0.276017,
          0.287278,
          0.292872,
          0.305886,
          0.304365,
          0.303592,
          0.306328,
          0.316178,
          0.000000,
          0.294010
};

static double P0ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut[] = { 
          0.008540,
          0.005461,
          0.004741,
          0.005030,
          0.005536,
          0.005909,
          0.005215,
          0.003172,
          0.003407,
          1.414214,
          0.005713
};

static double P1_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut[] = { 
          0.007702,
          0.010185,
          0.012745,
          0.011132,
          0.012523,
          0.015780,
          0.016342,
          0.012820,
          0.014124,
          0.000000,
          0.014017
};

static double P1ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut[] = { 
          0.001795,
          0.001491,
          0.001299,
          0.001091,
          0.001437,
          0.001552,
          0.001411,
          0.000803,
          0.001093,
          1.414214,
          0.001449
};

//---------------- Asym1 ToF2

double P0_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut[] = { 
          0.256044,
          0.270395,
          0.279451,
          0.278446,
          0.297421,
          0.310342,
          0.290846,
          0.299839,
          0.304129,
          0,
          0.285761
};
double P0ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut[] = { 
          0.00622325,
          0.00450402,
          0.00419646,
          0.00524945,
          0.00547403,
          0.00492584,
          0.00457393,
          0.00284616,
          0.00351287,
          1.41421,
          0.00551662
};
double P1_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut[] = { 
          0.00571282,
          0.00524434,
          0.00814513,
          0.00828433,
          0.00772076,
          0.00581256,
          0.0123805,
          0.00762069,
          0.00992525,
          0,
          0.00915316
};
double P1ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut[] = { 
          0.00139774,
          0.00116327,
          0.00114657,
          0.00120167,
          0.00145584,
          0.00109868,
          0.00125932,
          0.000742342,
          0.00118257,
          1.41421,
          0.00142539
};

//---------------- Asym2 ToF1

double P0_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut[] = { 
          0.215895,
          0.216151,
          0.224139,
          0.224334,
          0.241826,
          0.231798,
          0.239873,
          0.234639,
          0.244252,
          0,
          0.229251
};
double P0ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut[] = { 
          0.00714642,
          0.00349153,
          0.00297549,
          0.00429109,
          0.00432299,
          0.00352536,
          0.00294122,
          0.00379265,
          0.00342793,
          1.41421,
          0.00488914
};
double P1_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut[] = { 
          0.00409985,
          0.00591869,
          0.00853463,
          0.00807333,
          0.00705711,
          0.012556,
          0.0110228,
          0.00900818,
          0.0103299,
          0,
          0.00969333
};
double P1ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut[] = { 
          0.00156708,
          0.00102835,
          0.000451846,
          0.000883456,
          0.00100712,
          0.00105677,
          0.000760352,
          0.000887706,
          0.00111683,
          1.41421,
          0.00120235
};

//----------------- Asym2 ToF2

double P0_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut[] = { 
          0.197363,
          0.208210,
          0.221102,
          0.213358,
          0.231594,
          0.226394,
          0.230600,
          0.229244,
          0.236242,
          0.000000,
          0.222634
};
double P0ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut[] = { 
          0.005027,
          0.003005,
          0.002705,
          0.004641,
          0.004335,
          0.003378,
          0.002642,
          0.003427,
          0.003380,
          1.414214,
          0.004697
};
double P1_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut[] = { 
          0.003975,
          0.003332,
          0.004081,
          0.005693,
          0.005402,
          0.007847,
          0.007580,
          0.005710,
          0.006809,
          0.000000,
          0.006139
};
double P1ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut[] = { 
          0.001161,
          0.000801,
          0.000557,
          0.000998,
          0.001102,
          0.000870,
          0.000748,
          0.000794,
          0.001113,
          1.414214,
          0.001204
};


//_____________________________________________________________________________
double 
corrections::acceptance(double pT, const char* type)
{
  // Acceptance Parametrization PHENIX Analysis Note an115
  // Accep = (A+BpT) * (1.0 - exp(-a-b*pT))
  // Accep_error = A_error/A *Accep  1
  //    1  p0           2.31982e-01   2.72676e-03  -0.00000e+00  -5.28800e-04
  //    2  p1           2.53766e-03   3.15332e-04   0.00000e+00  -4.90148e-03
  //    3  p2          -4.40588e-02   3.92561e-02  -0.00000e+00   2.75295e-05
  //    4  p3           9.57877e-01   5.20808e-02   0.00000e+00   1.11556e-05

  double Accep;     
  double Accep_error;

  double A=0.232;   
  double A_error=0.0027;

  double B=0.00254;

  double a=-0.044;  
  double a_error=0.039;

  double b=0.958;   
  double b_error=0.04;

  Accep = (A +B*pT)* (1.0 - exp(-a-b*pT));

  if (strcmp(type,"error") == 0) 
    {
      double abpT_error= sqrt(a_error*a_error+b_error*b_error*pT*pT);
      Accep_error = sqrt( pow(A_error/A*Accep,2) +
			  pow(A*exp(-a-b*pT)*b*abpT_error,2) );
      return Accep_error;
    }
  else 
    {
      return Accep;
    }
}

//_____________________________________________________________________________
double 
corrections::efficiency(double pT, int centClass, const char* cut,
			const char* type)
{
  double rv = 1.0;

  double effic = cutEfficiency(pT,centClass,cut,"value");

  // this is only the extraction window related error
  double ExtractWindow_error = cutEfficiency(pT,centClass,cut,"error");

  //--- Systematic Error Calculation

  // Raw pi0 extraction procedure = 10% (according to Saskia)
  double ExtractRaw_error = 0.10 ;
  
  // pT smearing, power-law weights, ...: 
  // From Laurent studies with 8%+-4% constant term
  double pTsmearing_Syst_error = 0.5*( (1.159+0.01398*pT)-1.);  

  // 2. LowEnergy and Asymmetry Cuts
// More work to be done from data after QM02
  double LowEnergy_Asymetry_Syst_error = 0.03 ;
  
  // 3. Fiducial and DeadWarn cuts
  // Based on the difference between DeadMap at Subatech and BNL
  double Fiducial_DeadWarn_Syst_error = 0.05 ;  

  // 4. Chi2 cut 
  //  More work to be done from data after QM02.
  double Chi2_Syst_error = 0.05; 

  // 5. TOF cut
// According to Klaus TOF-cut on/off studies
  double TOF_Syst_error = 0.10; 

  // 6. Acceptance Systematic Error
  // 4% difference between PISA and Objectivity geometries
  double Acceptance_Syst_error = 0.04; 

  double Syst_error = TMath::Sqrt 
    (
     ExtractRaw_error*ExtractRaw_error +
     pTsmearing_Syst_error*pTsmearing_Syst_error                 +
     LowEnergy_Asymetry_Syst_error*LowEnergy_Asymetry_Syst_error +
     Fiducial_DeadWarn_Syst_error*Fiducial_DeadWarn_Syst_error   +
     Chi2_Syst_error*Chi2_Syst_error                             + 
     TOF_Syst_error*TOF_Syst_error                               +
     Acceptance_Syst_error*Acceptance_Syst_error                 +
     ExtractWindow_error*ExtractWindow_error                      
       );
  
  double effic_error = Syst_error*effic;

  if ( strcmp(type,"value")==0 ) 
    {
      rv = effic;
    }
  else if ( strcmp(type,"error")==0 ) 
    {
      rv = effic_error;
    }
  else
    {
      std::cerr << "Unknown type " << type << endl;
    }
  return rv;
}

//_____________________________________________________________________________
double 
corrections::cutEfficiency(double pt, int centClass, const char* cut, 
			   const char* type)
{
  double* p0;
  double* p0err;
  double* p1;
  double* p1err;

  getCorrectionPointers(cut,p0,p0err,p1,p1err);

  double rv=1.0;
  
  if ( !p0 ) 
    {
      std::cerr << "Unknown efficiencies for cut " << cut << endl;
      return rv;
    }

  double effic =  p0[centClass] + p1[centClass]*pt;

  double effic_error = ( p0err[centClass] + p1err[centClass]*pt);

   if ( strcmp(type,"value")==0 ) 
    {
      rv = effic;
    }
  else if ( strcmp(type,"error")==0 ) 
    {
      rv = effic_error;
    }
  else
    {
      std::cerr << "Unknown type " << type << endl;
    }

   return rv;
}

//_____________________________________________________________________________
void
corrections::dumpCorrections(const char* cut)
{

  double* p0;
  double* p0err;
  double* p1;
  double* p1err;

  std::cout << "Corrections for cut " << cut << endl;
  getCorrectionPointers(cut,p0,p0err,p1,p1err);
  
  if ( !p0 ) 
    {
      std::cerr << "No corrections known for this cut" << std::endl;      
    }
  else
    {
      for ( size_t i=0; i < 11; ++i ) 
	{
	  printf("Cent=%2d p0=%7.4f +- %7.4f p1=%7.4f +- %7.4f\n",
		 i,p0[i],p0err[i],p1[i],p1err[i]);
	}
    }

}

//_____________________________________________________________________________
void
corrections::getCorrectionPointers(const char* cut,
				   double*& p0,
				   double*& p0err,
				   double*& p1,
				   double*& p1err)
{
  if ( strcmp(cut,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut") == 0 )
    {
      p0 = P0_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut;
      p0err = P0ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut;
      p1 = P1_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut;
      p1err = P1ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF1Cut;
    }
  else if ( strcmp(cut,"FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut") == 0 )
    {
      p0 = P0_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut;
      p0err = P0ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut;
      p1 = P1_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut;
      p1err = P1ERR_FiduNoW3DeadWarnEnergyAsym1Chi2ToF2Cut;
    }
  else if ( strcmp(cut,"FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut") == 0 )
    {
      p0 = P0_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut;
      p0err = P0ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut;
      p1 = P1_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut;
      p1err = P1ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF1Cut;
    }
  else if ( strcmp(cut,"FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut") == 0 )
    {
      p0 = P0_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut;
      p0err = P0ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut;
      p1 = P1_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut;
      p1err = P1ERR_FiduNoW3DeadWarnEnergyAsym2Chi2ToF2Cut;
    }
  else
    {
      p0=0;
      p0err=0;
      p1=0;
      p1err=0;
    }
}

//_____________________________________________________________________________
double
corrections::nonVertex(const char* type)
{
  if (strcmp(type,"error") == 0) 
    {
      return 0.03; // off-vtx pions are +3% - +6
    }
  else 
    {
      return 0.94; // off-vtx pions are +3% - +6% . Let's take the upper value
    }
}
