////////////////////////////////////////////////////////////
// Physics asymmetry extraction using the bunch fit method (Tanida method)
//
//  Author  : Hideyuki Kobayahsi (C to C++ translation) 
//          : Kiyoshi Tanida (original)
//  Created : 2003-03-10
//  Modified: 2003-04-22 by K. Tanida 
//            skip bunches with |polarization| > 1
//            re-calculate chi-square and degree-of-freedom correctly 
//  Modified: 2004-04-14 by Y. Fukao
//            minor change in if statement (3 place),
//            "nl[i]+nr[i]<=0.0" -> "nl[i]<=0.0 || nr[i]<=0.0"
////////////////////////////////////////////////////////////
#ifndef utiBunchFitAsym_h
#define utiBunchFitAsym_h

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

////////////////////////////////////////////////////////////
// utiBunchFitAsym class
////////////////////////////////////////////////////////////
class utiBunchFitAsym {
 private:
  //
  // Global constants
  //
  static const int MaxNBUNCH=250; // Maximum number of bunches
  int verbosity;

  //
  // Constants for simulation
  //
  int SEED; // Seed of randomization
  double DT; // Simulated detector asymmetry
  double ABT; // Simulated blue beam physics asymmetry
  double AYT; // Simulated yellow beam physics asymmetry
  double STAT; // Total statistics  
  int NTRY; // Number of simulation trials

  //
  // Constants for fitting
  //
  double EPS; // Calculation convergence constant
  int NITER; // Number of iteration for Newton-Rapson method

  //
  // Constants for both simulation and fitting
  //
  int NBUNCH; // Number of bunches

  //
  // Inputs for bunch fitting
  //
  double NumL[MaxNBUNCH]; // Number of counts in left
  double NumR[MaxNBUNCH]; // Number of counts in right
  double PolBlue[MaxNBUNCH]; // Polarization of blue beam 
  double PolYell[MaxNBUNCH]; // Polarization of Yellow beam 
  double BunchStat[MaxNBUNCH]; // Fraction of total number of counts

  //
  // Outputs from bunch fitting
  //
  std::string FitTypeName;
  float DetAsym,DetAsymErr;
  float BlueAsym,BlueAsymErr;
  float YellAsym,YellAsymErr;
  float Chisq,DOF,RedChisq;

  double alr[MaxNBUNCH];
  double dalr[MaxNBUNCH];

 public:

  //
  // Constructor
  //
  utiBunchFitAsym(){
    verbosity = 0;
    //
    // Set default values for simulation
    //
    SetNBUNCH();
    SetSimOptions();
    SetFitOptions();

    // Initialize asymmetry array
    for(int i=0;i<MaxNBUNCH;i++){
      alr[i]=0; // Asymmetry of left right
      dalr[i]=0; // Error of Asymmetry of left right
    }
  }

  utiBunchFitAsym(int iNBUNCH,double numl[],double numr[],
		  double polb[],double poly[])
  {  
    verbosity = 0;
    //
    // Set default values for simulation
    //
    SetNBUNCH();
    SetSimOptions();
    SetFitOptions();
    //
    // Set bunchs and polarizations
    //
    SetNum(iNBUNCH,numl,numr);
    SetPol(iNBUNCH,polb,poly);

    // Initialize asymmetry array
    for(int i=0;i<MaxNBUNCH;i++){
      alr[i]=0; // Asymmetry of left right
      dalr[i]=0; // Error of Asymmetry of left right
    }
  }

  void Verbosity(const int i) {verbosity = i;}

  //
  // Accessor
  //
  std::string GetFitTypeName(){return FitTypeName;};
  double GetDetAsym(){return DetAsym;};
  double GetDetAsymErr(){return DetAsymErr;}
  double GetBlueAsym(){return BlueAsym;};
  double GetBlueAsymErr(){return BlueAsymErr;}
  double GetYellAsym(){return YellAsym;};
  double GetYellAsymErr(){return YellAsymErr;}
  double GetChisq(){return Chisq;};
  double GetDOF(){return DOF;};
  double GetRedChisq(){return RedChisq;}
  double GetAsymBunch(int ibunch) { return alr[ibunch]; }
  double GetAsymEBunch(int ibunch){ return dalr[ibunch]; }

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

  void GetNum(int &iNBUNCH,double numl[],double numr[])
    {
      iNBUNCH=GetNBUNCH();
      for(int i=0;i<GetNBUNCH();i++){
	numl[i]=NumL[i];numr[i]=NumR[i];
      }
    }

  void SetPol(int iNBUNCH,double polb[],double poly[])
    {
      if(SetNBUNCH(iNBUNCH)) exit(1);
      for(int i=0;i<GetNBUNCH();i++){
	PolBlue[i]=polb[i];PolYell[i]=poly[i];
      }
    }
  void SetNum(int iNBUNCH,double numl[],double numr[])
    {
      if(SetNBUNCH(iNBUNCH)) exit(1);
      for(int i=0;i<GetNBUNCH();i++){
	NumL[i]=numl[i];NumR[i]=numr[i];
      }
    }
  void SetFitTypeName(std::string str){FitTypeName=str;};
  void SetDetAsym(double val){DetAsym=val;};
  void SetDetAsymErr(double val){DetAsymErr=val;}
  void SetBlueAsym(double val){BlueAsym=val;};
  void SetBlueAsymErr(double val){BlueAsymErr=val;}
  // Calculate blue and yellow asymmetries the same way, then reverse 
  // the sign on yellow at the end, since the beams are in opposite directions.
  void SetYellAsym(double val){YellAsym=-val;}; 
  void SetYellAsymErr(double val){YellAsymErr=val;}
  void SetChisq(double val){Chisq=val;};
  void SetDOF(double val){DOF=val;};
  void SetRedChisq(double val){RedChisq=val;}
  void SetFitParams(double dDetAsym, double dDetAsymErr,
		    double dBlueAsym, double dBlueAsymErr,
		    double dYellAsym, double dYellAsymErr,
		    double dChisq, double dDOF,double dRedChisq, 
		    std::string sFitTypeName);
  void SetFitOptions(double dEPS=1.0e-6,
		     int iNITER=10)
    {
      EPS=dEPS; // Calculation convergence constant
      NITER=iNITER; // Max number of iteration for Newton-Raphson method
    }
  int SetNBUNCH(int iNBUNCH=120)
    {
      if(iNBUNCH>MaxNBUNCH){
	std::cout << "utiBunchFitAsym::SetNBUNCH(): NBUNCH="<<NBUNCH;
	std::cout << " violates boundary of ";
	std::cout << "MaxNBUNCH="<<MaxNBUNCH;
	std::cout << std::endl;
	return 1;
      }
      NBUNCH=iNBUNCH;
      return 0;
    }
  int GetNBUNCH(){return NBUNCH;}
  void SetSimOptions(double dDT=0.1,
		     double dABT=0.2,
		     double dAYT=-0.05,
		     double dSTAT=1.0e10,
		     int iSEED=51)
    {
      DT=dDT; // Simulated detector asymmetry
      ABT=dABT; // Simulated blue beam physics asymmetry
      AYT=dAYT; // Simulated yellow beam physics asymmetry
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
  void init(); // Generate a simulated bunch pattern
  void init(double polb[],double poly[],double bstat[]); 
  void SimPolCollMea(); // Measurement
  void SimPolCollMea(double nl[],double nr[]); // Measurement
  void SimPolCollMea(double nl[],double nr[],
		     double polb[],double poly[],double bstat[]);
    
  //
  // Asymmetry Analysis
  //
  void BunchPairAna();
  void BunchPairAna(double nl[],double nr[]);
  void BunchPairAna(double nl[],double nr[],double polb[],double poly[]);
  void LinApproxFit();
  void LinApproxFit(double nl[],double nr[]);
  void LinApproxFit(double nl[],double nr[],double polb[],double poly[]);
  void FullFit();
  void FullFit(double nl[],double nr[]);
  void FullFit(double nl[],double nr[],double polb[],double poly[]);

  //
  // Applications
  //
  void SimAndFit();
};

#endif // utiBunchFitAsym_h
