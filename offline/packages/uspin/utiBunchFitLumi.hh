////////////////////////////////////////////////////////////
// Physics asymmetry extraction using the bunch fit method (Tanida method)
//
//  Author  : Kiyoshi Tanida (original)
//  Created : 2003-04-22
////////////////////////////////////////////////////////////
#ifndef utiBunchFitLumi_h
#define utiBunchFitLumi_h

//
// C libraries
//
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

//
// C++ libraries
//
#include <string> // Use string class in the STL
#include <iostream> // Use iostream 
#include <vector>

////////////////////////////////////////////////////////////
// utiBunchFitLumi class
////////////////////////////////////////////////////////////
class utiBunchFitLumi{
 private:

  int verbosity;
  //
  // Constants for simulation
  //
  int SEED; // Seed of randomization
  double CT; // Simulated detector asymmetry
  double ALLT; // Simulated blue beam physics asymmetry
  double STAT; // Total statistics  
  int NTRY; // Number of simulation trials

  //
  // Constants for fitting
  //
  double EPS; // Calculation convergence constant

  //
  // Constants for both simulation and fitting
  //
  int NBUNCH; // Number of bunches

  //
  // Inputs for bunch fitting
  //
  std::vector<double> Ratio; // Number of counts in left
  std::vector<double> ErrRatio; // Number of counts in right
  std::vector<double> PolBlue; // Polarization of blue beam 
  std::vector<double> PolYell; // Polarization of Yellow beam 
  std::vector<double> BunchStat; // Fraction of total number of counts

  //
  // Outputs from bunch fitting
  //
  std::string FitTypeName;
  float CRatio,CRatioErr;
  float A_LL,A_LLErr;
  float Chisq,DOF,RedChisq;
 public:

  //
  // Constructor
  //
  utiBunchFitLumi(){
    verbosity = 0;
    //
    // Set default values for simulation
    //
    SetNBUNCH();
    SetSimOptions();
    SetEPS();
  }

  utiBunchFitLumi(int iNBUNCH,double ratio[],double dratio[],
		  double polb[],double poly[])
  {  
    verbosity = 0;
    //
    // Set default values for simulation
    //
    SetNBUNCH();
    SetSimOptions();
    SetEPS();
    //
    // Set bunchs and polarizations
    //
    SetRatio(iNBUNCH,ratio,dratio);
    SetPol(iNBUNCH,polb,poly);
  }

  void Verbosity(const int i) {verbosity = i;}

  //
  // Accessor
  //
  double GetCRatio(){return CRatio;};
  double GetCRatioErr(){return CRatioErr;}
  double GetA_LL(){return A_LL;};
  double GetA_LLErr(){return A_LLErr;}
  double GetChisq(){return Chisq;};
  double GetDOF(){return DOF;};
  double GetRedChisq(){return RedChisq;}
  void PrintPol()
    {
      for(int i=0;i<GetNBUNCH();i++){
	std::cout <<" " << i;
	std::cout <<" " << PolBlue[i];
	std::cout <<" " << PolYell[i];
	std::cout << std::endl;
      }
    }
  void GetPol(int &iNBUNCH,double polb[],double poly[])
    {
      iNBUNCH=GetNBUNCH();
      for(int i=0;i<GetNBUNCH();i++){
	polb[i]=PolBlue[i];poly[i]=PolYell[i];
      }
    }

  void GetRatio(int &iNBUNCH,double ratio[],double dratio[])
    {
      iNBUNCH=GetNBUNCH();
      for(int i=0;i<GetNBUNCH();i++){
	ratio[i]=Ratio[i];
	dratio[i]=ErrRatio[i];
      }
    }

  int GetNBUNCH(){return NBUNCH;}
  double GetEPS(){return EPS;}

  void SetPol(int iNBUNCH,double polb[],double poly[]){
    if(SetNBUNCH(iNBUNCH)) exit(1);
    for(int i=0; i<GetNBUNCH(); i++){
      PolBlue[i]=polb[i];PolYell[i]=poly[i];
    }
  }

  void SetRatio(int iNBUNCH,double ratio[],double dratio[]){
      if(SetNBUNCH(iNBUNCH)) exit(1);
      for(int i=0;i<GetNBUNCH();i++){
	Ratio[i]=ratio[i];
	ErrRatio[i]=dratio[i];
      }
    }
  void SetBunchStat(int iNBUNCH,double bstat[]){
    if(SetNBUNCH(iNBUNCH)) exit(1);
    for(int i=0; i<GetNBUNCH(); i++){BunchStat[i]=bstat[i];}
  }

  void SetCRatio(double val){CRatio=val;};
  void SetCRatioErr(double val){CRatioErr=val;};
  void SetA_LL(double val){A_LL=val;};
  void SetA_LLErr(double val){A_LLErr=val;};
  void SetChisq(double val){Chisq=val;};
  void SetDOF(double val){DOF=val;};
  void SetRedChisq(double val){RedChisq=val;}
  void SetFitParams(double dCRatio, double dCRatioErr,
		    double dA_LL, double dA_LLErr,
		    double dChisq, double dDOF,double dRedChisq);
  int SetNBUNCH(int iNBUNCH=120){
    NBUNCH=iNBUNCH;
    Ratio.resize(NBUNCH);
    ErrRatio.resize(NBUNCH,0.0);
    PolBlue.resize(NBUNCH,0.0);
    PolYell.resize(NBUNCH,0.0);
    BunchStat.resize(NBUNCH,0.0);
    return 0;
  }

  void SetEPS(double def_val=1.0e-15){EPS=def_val;}
  void SetSimOptions(double dCT=0.1,
		     double dALLT=0.2,
		     double dSTAT=1.0e10,
		     int iSEED=51)
    {
      CT=dCT; // Simulated detector asymmetry
      ALLT=dALLT; // Simulated blue beam physics asymmetry
      STAT=dSTAT; // Total statistics
      SEED=iSEED; // Seed of randomization
    }

  //
  // Tools
  //
  void PrintFitParams(); // Print fit param data values
  void PrintFitOptions(); // Print fit options
  void PrintSimOptions(); // Print simulation options

  //
  // Mathematical functions
  //
  int inverse(double *input, double *inverted); // Calculate inverse matrix
  double frand(); // Give a random number
  double grand(); // Give a random number

  //
  // Simulation of polarized collision measurement
  //
  void siminit(); // Generate a simulated bunch pattern
  void siminit(double polb[],double poly[],double bstat[]); 
  void siminit(std::vector<double> &polb,std::vector<double> &poly,
	       std::vector<double> &bstat);
  void SimPolCollMea(); // Measurement
  void SimPolCollMea(double ratio[],double dratio[]); // Measurement
  void SimPolCollMea(double ratio[],double dratio[],
		     double polb[],double poly[],double bstat[]);
  void SimPolCollMea(std::vector<double> &ratio,std::vector<double> &dratio,
		     std::vector<double> polb,std::vector<double> poly,
		     std::vector<double> bstat);
    
  //
  // Asymmetry Analysis
  //
  void FullFit();
  void FullFit(double ratio[],double dratio[]);
  void FullFit(double ratio[],double dratio[],double polb[],double poly[]);
  void FullFit(std::vector<double> ratio,std::vector<double> dratio,
	       std::vector<double> polb,std::vector<double> poly);

  //
  // Applications
  //
  void SimAndFit();
};

#endif // utiBunchFitLumi_h
