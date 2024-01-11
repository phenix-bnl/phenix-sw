#ifndef __CINT__
#include "utiBunchFitAsym.hh"
#include <iostream>
#endif // __CINT__

//
// An example to demonstrate utiBunchFitAsym class
//
//  Author : Hideyuki Kobayashi (translation from C to C++)
//         : Kiyoshi Tanida (original)
//  Date   : 2003-03-11
//
// How to run in root
//  root [] gSystem->Load("../.libs/libuspin.so");
//  root [] .x mainBunchFit.C
//
//------------------------------- simulation program ------------------------
// 11/10/2002: sqrt formula and linear approximated formula. 
// 12/28/2002: try to implement full formula with Newton-Raphson method 
// 03/09/2003: Translation of the code from C to C++
// 


const int MaxNBUNCH=250;

//
// Main						    
//
int mainBunchFit()
{
  double nl[MaxNBUNCH],nr[MaxNBUNCH]; // Number of counts in left and right
  double polb[MaxNBUNCH],poly[MaxNBUNCH]; // Polarization in blue and yellow beams
  int i;

  //
  // Customizable values for simulation
  //
  int SEED=51; // Seed of randomization
  double DT=0.6; // Simulated detector asymmetry
  double ABT=0.2; // Simulated blue beam physics asymmetry
  double AYT=-0.05; // Simulated yellow beam physics asymmetry
  double STAT=1.0e10; // Total statistics

  //
  // Customizable values for fitting
  //
  double EPS=1.0e-6; // Calculation termination error
  int NITER=10; // Number of iteration for Newton-Rapson method

  //
  // Customizable values for simulation and fitting
  //
  const int NBUNCH=20; // Number of bunches (constant)
  int iNBUNCH=20; // Number of bunches used in the fitting (variable)

  //
  // Create an instans for bunch fitting
  //
  utiBunchFitAsym bfit;

  //
  // Customize number of bunches
  //
  bfit.SetNBUNCH(NBUNCH);


  ////////////////////////////////////////////////////////////
  // Simulation
  ////////////////////////////////////////////////////////////
  cout << "\n";
  cout << "Simulation ...\n";
  cout << "\n";
  //
  // Customize simulation setting
  //
  bfit.SetSimOptions(DT,ABT,AYT,STAT,SEED);

  //
  // Print out simulation options
  //
  bfit.PrintSimOptions();
  
  //
  // Do the simulation
  //
  bfit.init(); // Creates model bunch pattern
  bfit.SimPolCollMea(); // Fill values in nl[] and nr[]

  //
  // An example to get simulated number of counts and polarization
  //
  bfit.GetNum(iNBUNCH,nl,nr);
  bfit.GetPol(iNBUNCH,polb,poly);
  for(i=0;i<iNBUNCH;i++){
    cout <<" " <<  i;
    cout <<" " <<  nl[i];
    cout <<" " <<  nr[i];
    cout <<" " <<  polb[i];
    cout <<" " <<  poly[i];
    cout << endl;
  }
  //bfit.PrintPol();


  utiBunchFitAsym bbfit(iNBUNCH,nl,nr,polb,poly);
  bbfit.PrintFitOptions();
  //
  // Full fitting using the Newton Lapson method
  //
  bbfit.LinApproxFit();
  bbfit.FullFit();
  bbfit.PrintFitParams(); // Print out results
  bfit.FullFit();
  bfit.PrintFitParams(); // Print out results

  /*
  ////////////////////////////////////////////////////////////
  // Fitting
  ////////////////////////////////////////////////////////////
  cout << "\n";
  cout << "Fitting ...\n";
  cout << "\n";
  //
  // Set number of counts and polarization
  //
  bfit.SetNum(iNBUNCH,nl,nr);
  bfit.SetPol(iNBUNCH,polb,poly);

  //
  // Printout fitting options
  //
  bfit.SetFitOptions(EPS,NITER);
  bfit.PrintFitOptions();

  //
  // Bunch pair analysis -- Sqrt formula
  //
  bfit.BunchPairAna();
    
  //
  // Linear approximation fitting
  //
  bfit.LinApproxFit();
  bfit.PrintFitParams(); // Print out results

  //
  // Full fitting using the Newton Lapson method
  //
  bfit.FullFit();
  bfit.PrintFitParams(); // Print out results
  */

  return 0;
} // End of main()



