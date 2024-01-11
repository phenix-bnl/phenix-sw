
// Derived from TMVA::TSpline1 by J. Lajoie - 8/26/2016

// @(#)root/tmva $Id: MpcExTSpline1.C,v 1.1 2016/09/01 12:27:58 lajoie Exp $
// Author: Andreas Hoecker, Joerg Stelzer, Helge Voss, Kai Voss 

//_______________________________________________________________________
//                                                                      
// Linear interpolation of TGraph
//_______________________________________________________________________

#include <TMath.h>
#include <TGraph.h>
#include "MpcExTSpline1.h"


//_______________________________________________________________________
MpcExTSpline1::MpcExTSpline1( TGraph* theGraph )
   : fGraph( theGraph )
{

}

//_______________________________________________________________________
MpcExTSpline1::~MpcExTSpline1( void )
{
   // destructor
   if (fGraph) delete fGraph; 
}

//_______________________________________________________________________
Double_t MpcExTSpline1::Eval( Double_t x ) const
{  
   // returns linearly interpolated TGraph entry around x
   Int_t ibin = TMath::BinarySearch( fGraph->GetN(),
                                     fGraph->GetX(),
                                     x );
   Int_t nbin = fGraph->GetN();

   // sanity checks
   if (ibin < 0    ) ibin = 0;
   if (ibin >= nbin) ibin = nbin - 1;

   Int_t nextbin = ibin;
   if ((x > fGraph->GetX()[ibin] && ibin != nbin-1) || ibin == 0) 
      nextbin++;
   else
      nextbin--;  

   // linear interpolation
   Double_t dx = fGraph->GetX()[ibin] - fGraph->GetX()[nextbin];
   Double_t dy = fGraph->GetY()[ibin] - fGraph->GetY()[nextbin];
   return fGraph->GetY()[ibin] + (x - fGraph->GetX()[ibin]) * dy/dx;
}


