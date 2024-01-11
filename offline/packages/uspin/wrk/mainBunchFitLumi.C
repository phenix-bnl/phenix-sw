#ifndef __CINT__
#include "utiBunchFitLumi.hh"
#include <iostream>
#endif // __CINT__

//
// An example to demonstrate utiBunchFitLumi class
//
//  Author : Kiyoshi Tanida
//  Date   : 2003-04-22
//
// How to run in root
//  root [] gSystem->Load("../.libs/libuspin.so");
//  root [] .x BunchFitLumi.C
//

const int MaxNBUNCH=500;

//
// Main						    
//
int mainBunchFitLumi()
{
  double ratio[MaxNBUNCH],dratio[MaxNBUNCH]; 
  // Number of counts in left and right
  double polb[MaxNBUNCH],poly[MaxNBUNCH]; 
  // Polarization in blue and yellow beams
  int i;

  //
  // Customizable values for simulation
  //
  int SEED=51; // Seed of randomization
  double CT=0.2; // Simulated CRatio
  double ALLT=0.02; // Simulated A_LL
  double STAT=1.0e10; // Total statistics

  //
  // Customizable values for simulation and fitting
  //
  const int NBUNCH=20; // Number of bunches (constant)
  int iNBUNCH=20; // Number of bunches used in the fitting (variable)

  //
  // Create an instance for bunch fitting
  //
  //  utiBunchFitLumi    bfit;
  utiBunchFitLumi bfit;
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
  bfit.SetSimOptions(CT,ALLT,STAT,SEED);

  //
  // Print out simulation options
  //
  bfit.PrintSimOptions();
  
  //
  // Do the simulation
  //
  bfit.siminit(); // Creates model bunch pattern
  bfit.SimPolCollMea(); // Fill values in ratio[] and dratio[]

  //
  // An example to get simulated number of counts and polarization
  //
  bfit.GetRatio(iNBUNCH,ratio,dratio);
  bfit.GetPol(iNBUNCH,polb,poly);
  for(i=0;i<iNBUNCH;i++){
    cout <<" " <<  i;
    cout <<" " <<  ratio[i];
    cout <<" " <<  dratio[i];
    cout <<" " <<  polb[i];
    cout <<" " <<  poly[i];
    cout << endl;
  }
  //bfit.PrintPol();

  bfit.FullFit();
  bfit.PrintFitParams(); // Print out results

  return 0;
} // End of main()



